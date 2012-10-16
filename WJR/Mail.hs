{-# LANGUAGE OverloadedStrings #-}
module WJR.Mail
  ( jobDoneEmail
  ) where

import WJR.Jobs
import Network.Mail.Mime
import Data.String (fromString)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT


-- | Helper function to convert a name and email address into a proper 'Address'
mkAddress :: String -> String -> Address
mkAddress name email = Address (Just $ fromString name) $ fromString email

jobDoneEmail job
  | M.lookup "email" (jobParams job) == Just "yes" = doSend
  | otherwise = return ()
  where
  subject = "Job Finished!"
  to = mkAddress (T.unpack $ jobUser job) (T.unpack $ jobUser job)
  from = mkAddress "Prokka web service" "noreply@vicbioinformatics.com"
  plainBody = LT.pack $ "Your job has finished! "++show (jobId job)
  doSend = renderSendMail $ Mail {
                   mailFrom = from
                 , mailTo   = [to]
                 , mailCc   = []
                 , mailBcc  = []
                 , mailHeaders = [ ("Subject",  subject) ]
                 , mailParts =
                     [[ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
                     $ LT.encodeUtf8 plainBody
                     ]]
                 }

