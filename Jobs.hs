{-# LANGUAGE OverloadedStrings #-}
module Jobs
    ( Job(..), Params(..), JobID(..), JobStatus(..)
    , allJobIds, nextJob, allJobs, jobsForUser
    , deleteJob, createJob, infoJob
    , createTmpOut, jobDone, getStatusFile
    ) where

import Imports
import System.IO
import Control.Exception
import Control.Monad (filterM,mzero)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import System.Directory (removeFile,doesDirectoryExist,getDirectoryContents,createDirectory,renameDirectory)
import Data.Aeson
import System.Posix.Types (ProcessID)
import System.Posix.Files (getFileStatus, modificationTime)
import System.FilePath.Posix (takeBaseName)
import System.Time (getClockTime)
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

getInfoName   jobId = jobBasename jobId ++ ".info"
getTmpOutName jobId = jobBasename jobId ++ ".tmpout"
getOutDirName jobId = jobBasename jobId ++ ".output"
getStatusFile jobId = jobBasename jobId ++ ".running"
getDataFile   jobId = jobBasename jobId ++ ".file"

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
  haveOutDir <- doesDirectoryExist (getOutDirName jobId)
  if haveOutDir then return $ JobComplete "done"
    else do running <- tryReadFile (getStatusFile jobId)
            case running of
              Just x -> return $ JobRunning (read $ T.unpack x)
              Nothing -> return JobWaiting

deleteJob :: JobID -> IO ()
deleteJob jobId = do
  let fname = getInfoName jobId
  if okFile fname
     then do removeFile fname
             removeFile (jobBasename jobId)
             removeFile (getDataFile jobId)
             -- TODO : remove status file and output (or tmpoutput) if they exist
     else return ()

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

  hndl <- openFile (getDataFile jobId) WriteMode
  BS.hPut hndl (fileContent fileInfo)
  hClose hndl
  return job
