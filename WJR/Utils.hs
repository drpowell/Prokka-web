module WJR.Utils
    ( newRandFile
    ) where

import System.IO (Handle)
import qualified Data.ByteString.Lazy as BS
import Data.Digest.Pure.MD5 (md5)
import System.Random (randomIO)
import Data.Char (ord)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.IO (openFd, fdToHandle, OpenMode(..), defaultFileFlags, OpenFileFlags(..))
import Control.Exception (catch,throw)

newRandFile :: FilePath -> IO (FilePath, Handle)
newRandFile dir = do
  n <- randomIO :: IO Int
  let str = show . md5 . BS.pack . map (fromIntegral . ord) . show $ n
      fname = dir ++ "/" ++ str
  mfd <- Control.Exception.catch
          (openFd fname WriteOnly (Just 0o600) defaultFileFlags {exclusive=True} >>= return . Just)
          (\e -> if isAlreadyExistsError e then return Nothing else throw e)
  case mfd of
    Nothing -> newRandFile dir
    Just fd -> fdToHandle fd >>= \h -> return (fname, h)

