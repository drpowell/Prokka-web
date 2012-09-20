#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings,DeriveDataTypeable #-}

import WJR.Jobs
import WJR.ParamDefs

import Data.String
import System.Exit
import System.Environment
import System.Process
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (canonicalizePath)
import System.Time (getClockTime, diffClockTimes, timeDiffToString)
import System.Posix.Types (CPid)
import System.Process.Internals (ProcessHandle__(..),ProcessHandle(..),PHANDLE)
import System.Console.CmdArgs hiding (name)
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (withMVar, newMVar, modifyMVar_, readMVar)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.List (isInfixOf, stripPrefix, isPrefixOf, tails)
import Text.Regex.Posix

maxLoad :: Double
maxLoad = 4.0

cmdLocation opts | debug opts = "/bin/echo"
                 | otherwise = "/bio/sw/vbc/prokka/bin/prokka"

safeRead :: (Read a) => String -> Maybe a
safeRead s = case reads s of
               [(x,"")] -> Just x
               _ -> Nothing

getLoad :: IO (Maybe Double)
getLoad = do uptime <- readProcess "/usr/bin/uptime" [] ""
             let regexp = "load average (([[:digit:]]|\\.)+)" :: String
             return $ case uptime =~ regexp of
                        [(_:s:_)] -> safeRead s
                        _ -> Nothing

canRun :: IO Bool
canRun = do mLoad <-getLoad
            case mLoad of
              Nothing -> putStrLn "Unable to read load" >> return False
              Just load -> return $ load <= maxLoad

exCode ExitSuccess = 0
exCode (ExitFailure n) = n

getPID :: ProcessHandle -> IO CPid
getPID (ProcessHandle p) =
    withMVar p $ \pp -> return $ case pp of
                                   (OpenHandle pp) -> toPID pp
                                   _ -> 0
  where
    toPID :: PHANDLE -> CPid
    toPID ph = toEnum $ fromEnum ph

logM msg = putStrLn msg

buildCmdLine :: Options -> Job -> IO [String]
buildCmdLine opts job = do
    dataFile <- canonicalizePath $ getDataFile (jobId job)
    return $ params ++ fastOpt ++ [dataFile]
  where
    fastOpt = if fast opts then ["--fast"] else []
    params = map T.unpack $ concatMap paramToOption paramDefs
    paramToOption p = case M.lookup (name p) (jobParams job) of
                        Nothing -> []
                        Just val -> toRun p p val

runJobId :: Options -> JobID -> IO ()
runJobId opts jobId = do
  mJob <- infoJob jobId
  case mJob of
    Nothing -> logM $ "Bad jobId : "++show jobId
    Just job -> runJob opts job

runJob :: Options -> Job -> IO ()
runJob opts job = do
  outDir <- createTmpOut (jobId job)
  let statusFile = outDir ++ "/job-exit-status"
  cmdLine <- buildCmdLine opts job
  writeFile statusFile $ "Running : "++cmdLocation opts++" : "++show cmdLine++"\n\n"

  inH <- openFile "/dev/null" ReadMode
  outH <- openFile (outDir ++ "/job-stdout") WriteMode
  errH <- openFile (outDir ++ "/job-stderr") WriteMode
  t1 <- getClockTime
  hndl <- runProcess (cmdLocation opts) cmdLine
                     (Just outDir) Nothing (Just inH) (Just outH) (Just errH)
  pid <- getPID hndl
  logM $ "  Running pid="++show pid
  writeFile (getStatusFile $ jobId job) (show pid)
  code <- waitForProcess hndl
  mapM_ hClose [inH, outH, errH]
  t2 <- getClockTime
  appendFile statusFile
                ("Process exited with code : "++show (exCode code)++
                 "\nElapsed time : "++timeDiffToString (diffClockTimes t2 t1)++"\n"
                )
  jobDone $ jobId job


runLoop opts prSleeping = do
  next <- nextJob
  case next of
    Nothing -> do when prSleeping (logM "No jobs.  Sleeping...")
                  threadDelay $ 1000*1000
                  runLoop opts False
    Just jobId -> do logM $ "Running : "++show jobId
                     runJobId opts jobId
                     runLoop opts True

data Options = Options { debug :: Bool
                       , fast :: Bool
                       } deriving (Show, Data, Typeable)

options = Options
 {debug = False &= help "Run in debug mode - no commands will be actually run"
 ,fast  = False &= help "Run the jobs with a '--fast' option"
 } &= summary "WJR runner.  Daemon to run tasks from the wjr web page"

main = do
  opts <- cmdArgs options
  putStrLn $ "Running with options : "++show opts
  runLoop options True
