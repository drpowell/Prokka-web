{-# LANGUAGE OverloadedStrings #-}
module Jobs
    ( Job(..), Params(..), JobID(..), JobStatus(..)
    , allJobIds, nextJob, allJobs, jobsForUser
    , deleteJob, createJob, infoJob
    , createTmpOut, jobDone, getStatusFile, getDataFile
    , getActualFile, ActualFile(..), zippedOutput

    , jobIP, jobTime
    ) where

import Imports
import System.IO
import System.IO.Error (isDoesNotExistError)
import Control.Exception
import Control.Monad (filterM,mzero)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import System.Directory (removeFile,removeDirectoryRecursive,doesDirectoryExist,getDirectoryContents
                        ,createDirectory,renameDirectory,doesFileExist)
import Data.Aeson
import System.Posix.Types (ProcessID)
import System.Posix.Files (getFileStatus, modificationTime)
import System.FilePath.Posix (takeBaseName)
import System.Time (getClockTime)
import System.Process (rawSystem)
import Utils (newRandFile)


fileDir :: FilePath
fileDir = "uploads"

type JobOutput = Text
type JobID = Text
type UserID = Text
type Params = Map Text Text

data JobStatus = JobWaiting
               | JobRunning ProcessID
               | JobComplete JobOutput
                 deriving (Show,Eq)

data Job = Job { jobParams :: Params
               , jobUser :: UserID
               , jobId :: JobID
               , jobMisc :: Map Text Text
               , jobStatus :: JobStatus
               }

instance FromJSON Job where
   parseJSON (Object v) = Job <$>
                            v .: "params" <*>
                            v .: "user"   <*>
                            v .: "id"     <*>
                            v .: "misc"   <*>
                            (return JobWaiting)

-- A non-Object value is of the wrong type, so use mzero to fail.
   parseJSON _          = mzero

instance ToJSON Job where
    toJSON job = Data.Aeson.object
                      ["params" .= jobParams job
                      ,"user"   .= jobUser job
                      ,"id"     .= jobId job
                      ,"misc"   .= jobMisc job
                      ]

jobBasename :: JobID -> FilePath
jobBasename jobId = fileDir ++ "/" ++ T.unpack jobId

fnameToJobId :: FilePath -> JobID
fnameToJobId fname = fromString (takeBaseName fname)

okFile :: FilePath -> Bool
okFile fname = fileDir `isPrefixOf` fname && not (".." `isInfixOf` fname)

enforceOkJob :: JobID -> IO ()
enforceOkJob jobId = if okFile (getInfoName jobId)
                       then return ()
                       else error $ "Bad job id : "++(T.unpack jobId)

getInfoName   jobId = jobBasename jobId ++ ".info"
getTmpOutName jobId = jobBasename jobId ++ ".tmpout"
getOutDirName jobId = jobBasename jobId ++ ".output"
getStatusFile jobId = jobBasename jobId ++ ".running"
getDataFile   jobId = jobBasename jobId ++ ".file"
getZipOutName jobId = jobBasename jobId ++ ".zip"

createTmpOut :: JobID -> IO FilePath
createTmpOut jobId = do
  let d = getTmpOutName jobId
  createDirectory d
  return d

jobDone :: JobID -> IO ()
jobDone jobId = do
  renameDirectory (getTmpOutName jobId) (getOutDirName jobId)

jobsForUser :: UserID -> IO [Job]
jobsForUser uid = filter ((uid ==) . jobUser) <$> allJobs

allJobIds :: IO [JobID]
allJobIds = do
  files <- getDirectoryContents fileDir
  let jobFiles = filter (\f -> (not $ "." `isInfixOf` f)) files
  return $ map fnameToJobId jobFiles

allJobs :: IO [Job]
allJobs =  catMaybes <$> (allJobIds >>= mapM infoJob)

nextJob :: IO (Maybe JobID)
nextJob = do
  allJobs <- allJobIds
  waiting <- filterM (\j -> checkJobStatus j >>= return . (==JobWaiting)) allJobs
  withTimes <- mapM (\j -> do status <- getFileStatus $ getInfoName j
                              return (modificationTime status, j)
                    ) waiting
  return $ case withTimes of
             [] -> Nothing
             ls -> Just $ snd $ minimum ls

infoJob :: JobID -> IO (Maybe Job)
infoJob jobId = do
  enforceOkJob jobId
  let fname = getInfoName jobId
  if okFile fname
    then do json <- decode <$> BS.readFile fname
            case json of
              Just x -> do status <- checkJobStatus jobId
                           return . Just $ x { jobStatus = status }
              Nothing -> do putStrLn "Failed to parse json"
                            return Nothing
    else do putStrLn "Bad fname"
            return Nothing

tryReadFile :: FilePath -> IO (Maybe Text)
tryReadFile fname = do
  Control.Exception.catch (T.readFile fname >>= return . Just)
                          ((\_ -> return Nothing) :: IOException -> IO (Maybe Text))

checkJobStatus :: JobID -> IO JobStatus
checkJobStatus jobId = do
  enforceOkJob jobId
  haveOutDir <- doesDirectoryExist (getOutDirName jobId)
  if haveOutDir then return $ JobComplete "done"
    else do running <- tryReadFile (getStatusFile jobId)
            case running of
              Just x -> return $ JobRunning (read $ T.unpack x)
              Nothing -> return JobWaiting

deleteJob :: JobID -> IO ()
deleteJob jobId = do
    enforceOkJob jobId
    mapM_ (\f -> rmFile $ f jobId) [getInfoName, getStatusFile, getDataFile, getZipOutName]
    mapM_ (\f -> removeDirectoryRecursive $ f jobId) [getOutDirName, getTmpOutName]
  where
    rmFile f = Control.Exception.catch
                  (removeFile f)
                  (\e -> if isDoesNotExistError e then return () else throw e)

createJob :: UserID -> Text -> Params -> FileInfo -> IO Job
createJob userId ip params fileInfo = do
  (fname, hndl) <- newRandFile fileDir
  hClose hndl
  putStrLn $ "Writing to : "++fname

  now <- getClockTime
  let jobId = fnameToJobId fname
  let job = Job { jobParams = params
                , jobId = jobId
                , jobUser = userId
                , jobStatus = JobWaiting
                , jobMisc = fromList $ [("fileName", fileName fileInfo)
                                       ,("fileContentType", fileContentType fileInfo)
                                       ,("ip", ip)
                                       ,("submittedAt",fromString $ show now)
                                       ]
                }

  hndl <- openFile (getInfoName jobId) WriteMode
  BS.hPut hndl (encode job)
  hClose hndl

  fileMove fileInfo (getDataFile jobId)

  return job

data ActualFile = InvalidPath | Directory [FilePath] | File FilePath

getActualFile :: JobID -> [Text] -> IO ActualFile
getActualFile jobId subPathT
    | not (okFile $ getInfoName jobId) = return InvalidPath
    | not (checkValidPath subPath)     = return InvalidPath
    | otherwise = do
        haveOutDir <- doesDirectoryExist (getOutDirName jobId)
        if not haveOutDir
            then return InvalidPath
            else do isFile <- doesFileExist actualDir
                    if isFile
                       then return $ File actualDir
                       else do files <- getDirectoryContents actualDir
                               return . Directory $ filter (\f -> not $ "." `isPrefixOf` f) files
  where
    subPath = map T.unpack subPathT
    actualDir = getOutDirName jobId ++ case subPath of
                                         [] -> ""
                                         _ -> "/" ++ intercalate "/" subPath
    checkValidPath path = null $ filter (\f -> "." `isPrefixOf` f) path

zippedOutput :: JobID -> IO FilePath
zippedOutput jobId = do
    enforceOkJob jobId
    let zipFile = getZipOutName jobId
    exists <- doesFileExist zipFile
    if exists
       then return zipFile
       else do mkZipFile jobId
               return zipFile
  where
    stripFileDir p = fromJust $ stripPrefix (fileDir++"/") p
    mkZipFile jobId = rawSystem "./zip-output.sh" [fileDir  -- cd to here
                                                  , stripFileDir $ getOutDirName jobId  -- What to zip
                                                  , stripFileDir $ getZipOutName jobId  -- Where to put it
                                                  ]

jobIP :: Job -> Text
jobIP job = fromMaybe "" (lookup "ip" . toList . jobMisc $ job)

jobTime :: Job -> Text
jobTime job = fromMaybe "" (lookup "submittedAt" . toList . jobMisc $ job)