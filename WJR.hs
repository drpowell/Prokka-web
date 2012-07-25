#!/usr/bin/env runghc
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Imports
import Yesod.Static (Static, Route(..), static)
import Text.Hamlet (hamletFile,shamlet)
import qualified Data.Text as T
import Data.Traversable (sequenceA)
import Data.List (delete)
import Settings
import Jobs
import Network.Wai

import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Network.HTTP.Conduit (Manager,newManager,def)

bootstrap_css :: Route Static
bootstrap_css = StaticRoute ["bootstrap.css"]    []

data App = App { getStatic :: Static
               , httpManager :: Manager
               }

mkYesod "App" [parseRoutes|
/ RootR GET
/auth AuthR Auth getAuth
/static StaticR Static getStatic
/new NewR
/job/#Text JobR GET
/deleteJob/#Text DeleteJobR GET
|]

instance Yesod App where
    approot = ApprootStatic "http://dna.med.monash.edu.au:3000"
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR bootstrap_css
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _     = RootR
    logoutDest _    = RootR
    authPlugins _   = [authBrowserId, authGoogleEmail]
    authHttpManager = httpManager

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


main = do
  st <- static "static"
  man <- newManager def
  warpDebug 3000 (App st man)

data ParamForm = ParamForm { paramMap :: Map Text Text
                           , fileInfo :: FileInfo
                           }
                 deriving (Show)

paramsAForm :: AForm App App ParamForm
paramsAForm =
    let a = [freq "Name" textField
            ,fopt "Email" textField
            ,fopt "pulldown" (selectFieldList ([("One","One"),("Two","Two"),("Cee","Cee")] :: [(Text,Text)]))
                 ]
        p = fromList . mapMaybe maybeSnd <$> (sequenceA a)
        fileForm = fileAFormReq "FastA file"
    in ParamForm <$> p <*> fileForm
  where
    maybeSnd :: (a, Maybe b) -> Maybe (a,b)
    maybeSnd (x,Just y) = Just (x,y)
    maybeSnd _ = Nothing

fopt :: String -> Field App App a -> AForm App App (Text, Maybe a)
fopt n f = (\x -> (fromString n,x)) <$> aopt f (fromString n) Nothing

freq :: String -> Field App App a -> AForm App App (Text, Maybe a)
freq n f = (\x -> (fromString n,Just x)) <$> areq f (fromString n) Nothing

paramsForm :: Html -> MForm App App (FormResult ParamForm, Widget)
paramsForm = renderTable paramsAForm

myJobs :: Handler [Job]
myJobs = do
  maid <- maybeAuthId
  case maid of
    Nothing -> return []
    Just uid -> liftIO $ jobsForUser uid

getRootR :: Handler RepHtml
getRootR = do
  jobs <- myJobs
  maid <- maybeAuthId
  defaultLayout $(widgetFile "home")

checkAccess Nothing = notFound
checkAccess (Just job) = do
  maid <- maybeAuthId
  when (maybe True (jobUser job /=) maid) $
       notFound

getJobR :: JobID -> Handler RepHtml
getJobR jobId = do
  job <- liftIO $ infoJob jobId
  checkAccess job
  case job of
    Nothing -> notFound
    Just job -> let params = toList $ jobParams job
                    status :: Text
                    status = case jobStatus job of
                               JobWaiting -> "Waiting"
                               JobRunning _ -> "Running..."
                               JobComplete _ -> "Finished"
                in defaultLayout $(widgetFile "job")

getDeleteJobR :: JobID -> Handler RepHtml
getDeleteJobR jobId = do
  job <- liftIO $ infoJob jobId
  checkAccess job
  liftIO $ deleteJob jobId
  setMessage $ msgLabel "Job Deleted"
  redirect RootR

msgLabel :: Text -> Html
msgLabel msg = [shamlet|<span class="label label-success">#{msg}</span>|]

requestIP :: Handler Text
requestIP = fmap (fromString . show . remoteHost . reqWaiRequest) getRequest

handleNewR :: Handler RepHtml
handleNewR = do
    Just aid <- maybeAuthId
    ((result, widget), enctype) <- runFormPost paramsForm
    case result of
        FormSuccess params -> do ip <- requestIP
                                 job <- liftIO $ createJob aid ip (paramMap params) (fileInfo params)
                                 setMessage $ msgLabel "Job created."
                                 redirect RootR
        _ ->  defaultLayout $(widgetFile "new-job")

