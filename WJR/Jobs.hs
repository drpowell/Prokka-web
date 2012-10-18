{-# LANGUAGE OverloadedStrings,TupleSections #-}
module WJR.Jobs
    ( Job(..), Params, JobID, JobStatus(..), JobOutput(..), isNullUserID
    , allJobIds, nextJob, allJobs, jobsForUser
    , deleteJob, createJob, infoJob
    , createTmpOut, jobDone, getStatusFile, getSuccessFile, getDataFile
    , getActualFile, ActualFile(..), zippedOutput

    , jobIP, jobTime
    ) where

import WJR.Imports
import WJR.Utils (newRandFile)

import System.IO
import Control.Exception
import Control.Monad (filterM,mzero)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import System.Directory (doesDirectoryExist,getDirectoryContents
                        ,createDirectory,renameDirectory,doesFileExist)
import Data.Aeson
import System.Posix.Types (ProcessID)
import System.Posix.Files (getFileStatus, modificationTime)
import System.FilePath.Posix as FP (takeBaseName)
import System.Time (getClockTime)
import System.Process (rawSystem)


uploadDir,outputDir,statusDir :: FilePath
prefixDir = "user-files"
uploadDir = prefixDir ++ "/uploads"
outputDir = prefixDir ++ "/output"
statusDir = prefixDir ++ "/status"

type JobID = Text
type UserID = Text
type Params = Map Text Text

nullUserID :: UserID
nullUserID = ""
isNullUserID :: UserID -> Bool
isNullUserID userId = T.null userId

data JobOutput = JobSuccess | JobFailure deriving (Eq, Show)

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

jobBasename :: JobID -> FilePath -> String -> FilePath
jobBasename jobId dir ext = dir ++ "/" ++ T.unpack jobId ++ ext

fnameToJobId :: FilePath -> JobID
fnameToJobId fname = fromString (takeBaseName fname)

okFile :: FilePath -> Bool
okFile fname = prefixDir `isPrefixOf` fname && not (".." `isInfixOf` fname)

enforceOkJob :: JobID -> IO ()
enforceOkJob jobId = if okFile (getInfoName jobId)
                       then return ()
                       else error $ "Bad job id : "++(T.unpack jobId)

getInfoName   jobId = jobBasename jobId statusDir ".info"
getTmpOutName jobId = jobBasename jobId statusDir ".tmpout"
getOutDirName jobId = jobBasename jobId outputDir ".output"
getStatusFile jobId = jobBasename jobId statusDir ".running"
getSuccessFile jobId = jobBasename jobId statusDir ".success"
getDataFile   jobId = jobBasename jobId uploadDir ".file"
getZipOutName jobId = jobBasename jobId outputDir ".zip"
getStemName   jobId = jobBasename jobId statusDir ""

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
  files <- getDirectoryContents statusDir
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
    then handle ((\_ -> return Nothing) :: IOException -> IO (Maybe Job)) $
           do json <- decode <$> BS.readFile fname
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
  success <- doesFileExist (getSuccessFile jobId)
  if haveOutDir then return $ JobComplete $ if success then JobSuccess else JobFailure
    else do running <- tryReadFile (getStatusFile jobId)
            case running of
              Just x -> return $ JobRunning (read $ T.unpack x)
              Nothing -> return JobWaiting

deleteJob :: JobID -> IO ()
deleteJob jobId = do
    enforceOkJob jobId
    rawSystem "rm" $ ["-rf"] ++ map ($jobId) [ getStemName, getInfoName, getStatusFile, getSuccessFile, getDataFile
                                             , getZipOutName
                                             , getOutDirName, getTmpOutName]
    return ()

createJob :: Maybe UserID -> Text -> Params -> FileInfo -> IO Job
createJob mUserId ip params fileInfo = do
  (fname, hndl) <- newRandFile statusDir
  hClose hndl
  putStrLn $ "Writing to : "++fname

  now <- getClockTime
  let jobId = fnameToJobId fname
  let job = Job { jobParams = params
                , jobId = jobId
                , jobUser = fromMaybe nullUserID mUserId
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

data ActualFile = InvalidPath | Directory [(Bool, FilePath)] | File FilePath

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
                       else do isDir <- doesDirectoryExist actualDir
                               if not isDir
                                  then return InvalidPath
                                  else dirContents
  where
    join ls = intercalate "/" ls
    remDotFiles files = filter (\f -> not $ "." `isPrefixOf` f) files
    relativePath f = join $ subPath ++ [f]
    subPath = map T.unpack subPathT
    actualDir = join $ [getOutDirName jobId] ++ subPath
    checkValidPath path = null $ filter (\f -> "." `isPrefixOf` f) path

    dirContents = do files <- remDotFiles <$> getDirectoryContents actualDir
                     isDirs <- mapM (\f -> (,relativePath f) <$> doesDirectoryExist (join [actualDir,f])) files
                     return $ Directory isDirs


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
    stripFileDir p = fromJust $ stripPrefix (outputDir++"/") p
    mkZipFile jobId = rawSystem "./zip-output.sh" [outputDir  -- cd to here
                                                  , stripFileDir $ getOutDirName jobId  -- What to zip
                                                  , stripFileDir $ getZipOutName jobId  -- Where to put it
                                                  ]

jobIP :: Job -> Text
jobIP job = fromMaybe "" (lookup "ip" . toList . jobMisc $ job)

jobTime :: Job -> Text
jobTime job = fromMaybe "" (lookup "submittedAt" . toList . jobMisc $ job)