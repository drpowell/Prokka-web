{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, TupleSections #-}

module WJR.Application
    where

import WJR.Imports

import Yesod.Static (Static, Route(..), static)
import Text.Hamlet (hamletFile,shamlet)
import qualified Data.Text as T
import Network.Wai
-- import Network.Wai.Handler.CGI as CGI

import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import qualified Web.ClientSession as CS
import Network.HTTP.Conduit (Manager,newManager,def)
import Network.Gravatar as G

import WJR.AdminUsers (adminUser)
import WJR.Settings
import WJR.Jobs
import WJR.ParamDefs

bootstrap_css, bootstrap_js, jquery_js :: Route Static
bootstrap_css = StaticRoute ["bootstrap.css"]        []
bootstrap_js  = StaticRoute ["bootstrap-tooltip.js"] []
jquery_js     = StaticRoute ["jquery-1.7.2.min.js"]  []

data App = App { getStatic :: Static
               , httpManager :: Manager
               }

mkYesod "App" [parseRoutes|
/ RootR GET
/auth AuthR Auth getAuth
/static StaticR Static getStatic

/about AboutR GET
/queue QueueR GET
/new NewR
/job/#Text JobR GET
/job-delete/#Text JobDeleteR GET
/job-output/*OutputFile JobOutputR GET
/all-jobs AllJobsR GET
|]

-- | Authorization for the various routes.  TODO, use this to authorize access to jobs?
routeAuthorized r _write
    | okRoutes r       = return Authorized
    | loggedInRoutes r = loggedIn
    | adminRoutes r    = isAdmin
    | otherwise        = return $ Unauthorized "Not allowed"
  where
    okRoutes RootR          = True
    okRoutes (AuthR _)      = True
    okRoutes (StaticR _)    = True
    okRoutes NewR           = True
    okRoutes (JobR _)       = True
    okRoutes (JobDeleteR _) = True
    okRoutes (JobOutputR _) = True
    okRoutes AboutR         = True
    okRoutes _              = False
    loggedInRoutes QueueR = True
    loggedInRoutes _      = False
    adminRoutes AllJobsR = True
    adminRoutes _        = False
    loggedIn = do user <- maybeAuthId
                  return $ case user of
                             Nothing -> AuthenticationRequired
                             Just _ -> Authorized
    isAdmin = do mUser <- maybeAuthId
                 return $ case mUser of
                    Nothing -> AuthenticationRequired
                    Just user -> if adminUser user then Authorized else undefined Unauthorized "Admin only"

instance Yesod App where
    approot = ApprootStatic $ T.pack approotSetting
    authRoute _ = Just $ AuthR LoginR

    isAuthorized = routeAuthorized

    -- | Create the session backend.  Overriden here to increase session timeout to 2 weeks
    makeSessionBackend _ = do
        key <- CS.getKey CS.defaultKeyFile
        return $ Just $ clientSessionBackend key (14 * 24 * 60)

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
            addScript $ StaticR jquery_js
            addScript $ StaticR bootstrap_js
            $(widgetFile "default-layout")

        authId <- maybeAuthId
        let mGravatar = authId >>= return . G.gravatar defGravatar
        let isAdmin = maybe False adminUser authId
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- Allow big uploads for new jobs : TODO, more efficient upload handling : http://stackoverflow.com/questions/10880105/efficient-large-file-upload-with-yesod
    maximumContentLength _ (Just NewR) = 2 * 1024 * 1024 * 1024 -- 2 gigabytes
    maximumContentLength _ _ = 2 * 1024 * 1024 -- 2 megabytes

defGravatar = G.defaultConfig { gSize = Just $ Size 30
                              , gDefault = Just MM
                              }

instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _     = QueueR
    logoutDest _    = RootR
    authPlugins _   = [authBrowserId, authGoogleEmail]
    authHttpManager = httpManager

    loginHandler = defaultLayout $ do
      tm <- lift getRouteToMaster
      let browserId   = apLogin authBrowserId tm
          googleEmail = apLogin authGoogleEmail tm
      $(widgetFile "login")

    -- | What to show on the login page.
    -- loginHandler :: GHandler Auth m RepHtml
    -- loginHandler = defaultLayout $ do
    --     tm <- lift getRouteToMaster
    --     master <- lift getYesod
    --     mapM_ (flip apLogin tm) (authPlugins master)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage



defaultMain = do
  st <- static "static"
  man <- newManager def
  let app = App st man
  runWarp (fromIntegral listenPort) app
  -- toWaiApp app >>= run

----------------------------------------------------------------------


-- | Convert a "Field" that uses a Bool, to use a Text yes/no instead
fieldBoolToText :: RenderMessage m FormMessage => Field s m Bool -> Field s m Text
fieldBoolToText (Field parser viewer) = Field parser' viewer'
    where
      showB True = "yes"
      showB False = "no"
      readB "yes" = True
      readB _ = False
      parser' x = do r <- parser x
                     return $ case r of
                       Right (Just b) -> Right . Just . showB $ b
                       Right Nothing -> Right Nothing
                       Left m -> Left m
      viewer' id name attrs valOrErr req =
          let toB = case valOrErr of {Left x -> Left x; Right b -> Right (readB b)}
          in viewer id name attrs toB req


data ParamForm = ParamForm { fileInfo :: FileInfo
                           , paramMap :: Map Text Text
                           }

-- Params for the jobs.  TODO - separate this out (how?)
paramsForm :: Html -> MForm App App (FormResult ParamForm, Widget)
paramsForm fragment = do
    (fileR, fileV) <- aFormToForm $ fileAFormReq $
                         FieldSettings "Contigs file (FastA): "
                                       (Just "FastA file containing your contigs to annotate")
                                       Nothing (Just "contigs") [("required","")]

    -- results :: [(Text, FormResult Text)]
    -- views :: [FieldView]
    (results, paramViews) <- unzip <$> sequenceA (map fs paramDefs)
    let views = fileV paramViews
    let widget = $(widgetFile "param-form")

    let resMap = fromList . mapMaybe maybeSnd <$> sequenceA (map pairA results)
    let retParamForm = ParamForm <$> fileR <*> resMap
    return (retParamForm, widget)
  where
    pairA (a,b) = (\x -> (a,x)) <$> b
    maybeSnd :: (a, Maybe b) -> Maybe (a,b)
    maybeSnd (x,Just y) = Just (x,y)
    maybeSnd _ = Nothing

    -- fs :: ParamField -> MForm s m ((Text,FormResult Text), FieldView s m)
    fs (ParamField name label tip fld def _) =
        let s = FieldSettings (fromString label) (Just $ fromString tip) Nothing (Just name) []
        in do (r,v) <- mopt (paramFieldToField fld) s (Just def)
              return ((name,r), v)

    paramFieldToField TextField         = textField
    paramFieldToField CheckBoxField     = fieldBoolToText checkBoxField
    paramFieldToField (ListField pairs) = selectFieldList pairs
----------------------------------------------------------------------

myJobs :: Handler [Job]
myJobs = do
  maid <- maybeAuthId
  case maid of
    Nothing -> return []
    Just uid -> liftIO $ jobsForUser uid

getRootR :: Handler RepHtml
getRootR = defaultLayout $(widgetFile "home")

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $(widgetFile "about")

getQueueR :: Handler RepHtml
getQueueR = do
  jobs <- myJobs
  defaultLayout $(widgetFile "queue")

getAllJobsR :: Handler RepHtml
getAllJobsR = do
  jobs <- liftIO $ allJobs
  defaultLayout $(widgetFile "all-queue")

checkAccess Nothing = notFound
checkAccess (Just job)
    | isNullUserID (jobUser job) = return ()   -- Job submitted by anonymous user, only need the jobId to access it
    | otherwise = do
  maid <- maybeAuthId
  case maid of
    Nothing -> if isNullUserID (jobUser job)   -- User not logged in, check the job was submitted by such a user
               then return ()
               else notFound
    Just user -> if adminUser user || user == jobUser job
                 then return ()
                 else notFound

jobStatusText :: Job -> Text
jobStatusText job = case jobStatus job of
                      JobWaiting -> "Waiting"
                      JobRunning _ -> "Running..."
                      JobComplete _ -> "Finished"

getJobR :: JobID -> Handler RepHtml
getJobR jobId = do
  mAuthId <- maybeAuthId
  job <- liftIO $ infoJob jobId
  checkAccess job
  case job of
    Nothing -> notFound
    Just job -> let params = toList $ jobParams job
                    status = jobStatusText job
                    isAdmin = maybe False adminUser mAuthId
                    finished = case jobStatus job of { JobComplete _ -> True; _ -> False}
                in defaultLayout $(widgetFile "job")

getJobDeleteR :: JobID -> Handler RepHtml
getJobDeleteR jobId = do
  job <- liftIO $ infoJob jobId
  checkAccess job
  liftIO $ deleteJob jobId
  setMessage $ msgLabel "Job Deleted"
  maid <- maybeAuthId
  if isNothing maid
    then redirect RootR
    else redirect QueueR

msgLabel :: Text -> Html
msgLabel msg = [shamlet|<div class="alert alert-success">#{msg}</div>|]

requestIP :: Handler Text
requestIP = fmap (fromString . show . remoteHost . reqWaiRequest) getRequest

handleNewR :: Handler RepHtml
handleNewR = do
    maid <- maybeAuthId
    ((result, widget), enctype) <- runFormPost paramsForm
    case result of
        FormSuccess params -> do ip <- requestIP
                                 job <- liftIO $ createJob maid ip (paramMap params) (fileInfo params)
                                 setMessage $ msgLabel "Job created."
                                 redirect $ JobR (jobId job)
        _ ->  defaultLayout $(widgetFile "new-job")


data OutputFile = OutputFile JobID [Text]
                | OutputZipped JobID
                  deriving (Show,Eq,Read)
instance PathMultiPiece OutputFile where
    toPathMultiPiece (OutputFile j lst) = j : lst
    toPathMultiPiece (OutputZipped j) = [j `T.append` ".zip"]

    fromPathMultiPiece [j] =  case T.stripSuffix ".zip" j of
                                   Nothing -> Just $ OutputFile j []
                                   Just jId -> Just $ OutputZipped jId
    fromPathMultiPiece (j:lst) =  Just $ OutputFile j lst
    fromPathMultiPiece _ = Nothing

fileName (OutputFile _ lst) = last lst
fileName (OutputZipped _) = ""

jobOutputLink, jobOutputZippedLink :: JobID -> Route App
jobOutputLink jobId = JobOutputR $ OutputFile jobId []
jobOutputZippedLink jobId = JobOutputR $ OutputZipped jobId

getJobOutputR :: OutputFile -> Handler RepHtml
getJobOutputR (OutputZipped jobId) = do
  mJob <- liftIO $ infoJob jobId
  checkAccess mJob
  file <- liftIO $ zippedOutput jobId
  sendFile typeOctet file
getJobOutputR (OutputFile jobId path) = do
    checkValidPath path
    mJob <- liftIO $ infoJob jobId
    checkAccess mJob
    case mJob of
      Nothing -> notFound
      Just _ -> sendFileOrListing
  where
    checkValidPath path = case filter (\f -> "." `T.isPrefixOf` f) path of
                            [] -> return ()
                            _ -> error "Invalid"
    sendFileOrListing = do
        file <- liftIO $ getActualFile jobId path
        case file of
          InvalidPath -> notFound
          File filename -> sendFile typePlain filename
          Directory filePaths -> let files = map (\f -> OutputFile jobId (path ++ [fromString f])) filePaths
                                 in defaultLayout $(widgetFile "output-list")

