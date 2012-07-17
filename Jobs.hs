{-# LANGUAGE OverloadedStrings #-}
module Jobs
    where

import Imports
import System.IO
import qualified Data.ByteString.Lazy as BS
import Data.List
import System.Directory (removeFile)
import Data.Aeson

data Params = Params { paramMap :: Map Text Text
                     , fileInfo :: FileInfo
                     }
               deriving (Show)

fileDir = "uploads"

okFile fname = fileDir `isPrefixOf` fname && not (".." `isInfixOf` fname)

infoJob :: FilePath -> IO [(Text,Text)]
infoJob fname = do
  if okFile fname
    then do json <- decode <$> BS.readFile fname
            case json of
              Just x -> return $ toList x
              Nothing -> do putStrLn "Failed to parse json"
                            return []
    else do putStrLn "Bad fname"
            return []

deleteJob fname = do
  if okFile fname
     then do removeFile fname
             removeFile (fname ++ ".file")
     else return ()

createJob :: Params -> IO Text
createJob params = do
  let mp = fromList $ toList (paramMap params) ++
             [("fileName", fileName $ fileInfo params)
             ,("fileContentType", fileContentType $ fileInfo params)
             ]
  (fname, hndl) <- openTempFile fileDir "job"
  BS.hPut hndl (encode mp)
  hClose hndl
  putStrLn $ "Writing to : "++fname
  hndl <- openFile (fname++".file") WriteMode
  BS.hPut hndl (fileContent $ fileInfo params)
  hClose hndl
  return $ fromString fname
