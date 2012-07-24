{-# LANGUAGE OverloadedStrings #-}
module Jobs
    ( Job(..), Params(..), JobID(..), JobStatus(..)
    , paramsAdd
    , allJobIds, nextJob
    , deleteJob, createJob, infoJob
    , createTmpOut, jobDone, getStatusFile
    ) where

import Imports
import System.IO
import Control.Exception
import Control.Monad (filterM)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import System.Directory (removeFile,doesDirectoryExist,getDirectoryContents,createDirectory,renameDirectory)
import Data.Aeson
import System.Posix.Types (ProcessID)
import System.Posix.Files (getFileStatus, modificationTime)

type JobOutput = Text
type JobID = Text

data JobStatus = JobWaiting
               | JobRunning ProcessID
               | JobComplete JobOutput
                 deriving (Show,Eq)

data Job = Job { jobParams :: [(Text,Text)]
               , jobId :: JobID
               , jobStatus :: JobStatus
               }

data Params = Params { paramMap :: Map Text Text
                     , fileInfo :: FileInfo
                     }
               deriving (Show)


paramsAdd :: [(Text,Text)] -> Params -> Params
paramsAdd lst params = params { paramMap = fromList $ toList (paramMap params) ++ lst }

fileDir :: FilePath
fileDir = "uploads"

jobFname :: JobID -> FilePath
jobFname jobId = fileDir ++ "/" ++ T.unpack jobId

okFile :: FilePath -> Bool
okFile fname = fileDir `isPrefixOf` fname && not (".." `isInfixOf` fname)

getTmpOutName jobId = jobFname jobId ++ ".tmpout"
getOutDirName jobId = jobFname jobId ++ ".output"
getStatusFile jobId = jobFname jobId ++ ".running"
getDataFile jobId   = jobFname jobId ++ ".file"

createTmpOut :: JobID -> IO FilePath
createTmpOut jobId = do
  let d = getTmpOutName jobId
  createDirectory d
  return d

jobDone :: JobID -> IO ()
jobDone jobId = do
  renameDirectory (getTmpOutName jobId) (getOutDirName jobId)


allJobIds :: IO [JobID]
allJobIds = do
  files <- getDirectoryContents fileDir
  let jobFiles = filter (\f -> (not $ "." `isInfixOf` f)) files
  return $ map fromString jobFiles

nextJob :: IO (Maybe JobID)
nextJob = do
  allJobs <- allJobIds
  waiting <- filterM (\j -> checkJobStatus j >>= return . (==JobWaiting)) allJobs
  withTimes <- mapM (\j -> do status <- getFileStatus $ jobFname j
                              return (modificationTime status, j)
                    ) waiting
  return $ case withTimes of
             [] -> Nothing
             ls -> Just $ snd $ minimum ls

infoJob :: JobID -> IO (Maybe Job)
infoJob jobId = do
  let fname = jobFname jobId
  if okFile fname
    then do json <- decode <$> BS.readFile fname
            case json of
              Just x -> do status <- checkJobStatus jobId
                           return . Just $ Job { jobParams = toList x
                                               , jobId = jobId
                                               , jobStatus = status
                                               }
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
  let fname = jobFname jobId
  if okFile fname
     then do removeFile fname
             removeFile (getDataFile jobId)
     else return ()

createJob :: Params -> IO Job
createJob params = do
  let mp = paramsAdd [("fileName", fileName $ fileInfo params)
                     ,("fileContentType", fileContentType $ fileInfo params)
                     ] params
  (fname, hndl) <- openTempFile fileDir "job"
  let jobId = fromString fname
  BS.hPut hndl (encode $ paramMap mp)
  hClose hndl
  putStrLn $ "Writing to : "++fname
  hndl <- openFile (getDataFile jobId) WriteMode
  BS.hPut hndl (fileContent $ fileInfo params)
  hClose hndl
  return $ Job { jobParams = toList $ paramMap mp
               , jobId = jobId
               , jobStatus = JobWaiting
               }
