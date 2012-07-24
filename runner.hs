#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}

import Jobs
import Shelly
import Data.String
import System.Exit
import System.Process
import System.IO
import System.Time (getClockTime, diffClockTimes, timeDiffToString)
import Control.Concurrent (threadDelay)
import System.Posix.Types (CPid)
import System.Process.Internals (ProcessHandle__(..),ProcessHandle(..),PHANDLE)
import Control.Concurrent.MVar (withMVar)


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

runJob :: JobID -> IO ()
runJob jobId = do
  outDir <- createTmpOut jobId
  inH <- openFile "/dev/null" ReadMode
  outH <- openFile (outDir ++ "/job-stdout") WriteMode
  errH <- openFile (outDir ++ "/job-stderr") WriteMode
  t1 <- getClockTime
  hndl <- runProcess "../../tempScript" [] (Just outDir) Nothing (Just inH) (Just outH) (Just errH)
  pid <- getPID hndl
  logM $ "  Running pid="++show pid
  writeFile (getStatusFile jobId) (show pid)
  code <- waitForProcess hndl
  mapM_ hClose [inH, outH, errH]
  t2 <- getClockTime
  writeFile (outDir ++ "/job-exit-status")
                ("Process exited with code : "++show (exCode code)++
                 "\nElapsed time : "++timeDiffToString (diffClockTimes t2 t1)++"\n"
                )
  jobDone jobId


runLoop prSleeping = do
  next <- nextJob
  case next of
    Nothing -> do when prSleeping (logM "No jobs.  Sleeping...")
                  threadDelay $ 1000*1000
                  runLoop False
    Just job -> do logM $ "Running : "++show job
                   runJob job
                   runLoop True

main = runLoop True
