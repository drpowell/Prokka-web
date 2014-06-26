{-# LANGUAGE OverloadedStrings,QuasiQuotes #-}
module WJR.Mail
  ( jobDoneEmail
  ) where

import WJR.Jobs
import WJR.Application
import WJR.Settings

import Text.Hamlet (hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Yesod.Core (renderRoute,joinPath)
import Blaze.ByteString.Builder (toByteString)

import Network.Mail.Mime
import Data.String (fromString)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

-- | Helper function to convert a name and email address into a proper 'Address'
mkAddress :: String -> String -> Address
mkAddress name email = Address (Just $ fromString name) $ fromString email

emailBody :: Job -> LT.Text
emailBody job = LT.pack . renderHtml $ body myRender
  where
    body = [hamlet|Your prokka job has #{status}! Check the results here : @{JobR $ jobId job}|]
    status = jobStatusText job
    app = error "No Yesod set in mail sending" :: App
    myRender a _ = let (x,y) = renderRoute a
                   in T.decodeUtf8 $ Blaze.ByteString.Builder.toByteString $ joinPath app (T.pack approotSetting) x y

jobDoneEmail :: Job -> IO ()
jobDoneEmail job
  | isNullUserID (jobUser job) = return ()
  | M.lookup "email" (jobParams job) == Just "yes" = doSend
  | otherwise = return ()
  where
    subject = "Job Finished!"
    to = mkAddress (T.unpack $ jobUser job) (T.unpack $ jobUser job)
    from = mkAddress "Prokka web service" "noreply@vicbioinformatics.com"
    plainBody = emailBody job
    doSend = renderSendMail $ Mail
               { mailFrom = from
               , mailTo   = [to]
               , mailCc   = []
               , mailBcc  = []
               , mailHeaders = [ ("Subject",  subject) ]
               , mailParts =
                   [[ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
                    $ LT.encodeUtf8 plainBody
                    ]]
               }
