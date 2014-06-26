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
import System.FilePath (splitDirectories)
-- import Network.Wai.Handler.CGI as CGI

import Text.Julius
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import qualified Web.ClientSession as CS
import Network.HTTP.Conduit (Manager,newManager)
import Network.Gravatar as G

import WJR.AdminUsers (adminUser)
import WJR.Settings
import WJR.Jobs
import WJR.ParamDefs

bootstrap_css, bootstrap_js, jquery_js, jfileTree_css, jfileTree_js :: Route Static
bootstrap_css = StaticRoute ["bootstrap.css"]        []
bootstrap_js  = StaticRoute ["bootstrap-tooltip.js"] []
jquery_js     = StaticRoute ["jquery-1.7.2.min.js"]  []
jfileTree_css = StaticRoute ["jqueryFileTree","jqueryFileTree.css"] []
jfileTree_js  = StaticRoute ["jqueryFileTree","jqueryFileTree.js"] []

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
/job-output/#Text JobOutputR GET
/job-files/#Text/*Texts JobFilesAjaxR POST GET
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
    okRoutes (JobFilesAjaxR _ _) = True
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

defGravatar :: GravatarOptions
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


defaultMain :: IO ()
defaultMain = do
  st <- static "static"
  man <- newManager def
  let app = App st man
  runWarp (fromIntegral listenPort) app
  -- toWaiApp app >>= run

----------------------------------------------------------------------


-- | Convert a "Field" that uses a Bool, to use a Text yes/no instead
-- fieldBoolToText :: RenderMessage m FormMessage => Field s m Bool -> Field s m Text
fieldBoolToText :: Field sub master Bool -> Field sub master Text
fieldBoolToText fld = fld {fieldParse=parser', fieldView=viewer' }
    where
      showB True = "yes"
      showB False = "no"
      readB "yes" = True
      readB _ = False
      parser = fieldParse fld
      viewer = fieldView fld
      parser' x y = do r <- parser x y
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

-- checkAccess :: Maybe Job -> App
checkAccess Nothing = notFound >> return Nothing
checkAccess (Just job)
    | isNullUserID (jobUser job) = return $ Just job  -- Job submitted by anonymous user, only need the jobId to access it
    | otherwise = do
  maid <- maybeAuthId
  case maid of
    Nothing -> -- User not logged in, and already checked it is a non-anonymous job
               notFound >> return Nothing
    Just user -> if adminUser user || user == jobUser job
                 then return $ Just job
                 else notFound >> return Nothing

jobStatusText :: Job -> Text
jobStatusText job = case jobStatus job of
                      JobWaiting -> "Waiting"
                      JobRunning _ -> "Running..."
                      JobComplete JobSuccess -> "Finished"
                      JobComplete JobFailure -> "FAILED"

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
                    jsPoll = rawJS (if finished then "false" else "true" :: String)
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
msgLabel msg = [shamlet|<div class="alert alert-success">#{msg}|]

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


----------------------------------------------------------------------
-- Output file handling

jobOutputLink, jobOutputZippedLink :: JobID -> Route App
jobOutputLink jobId = JobOutputR $ jobId
jobOutputZippedLink jobId = JobOutputR $ T.append jobId ".zip"

getJobOutputR :: Text -> Handler RepHtml
getJobOutputR pJobId
    | ".zip" `T.isSuffixOf` pJobId = let Just jobId = T.stripSuffix ".zip" pJobId
                                     in do Just job <- checkAccess =<< liftIO (infoJob jobId)
                                           sendZippedOutput job
    | otherwise = do
        Just job <- checkAccess =<< liftIO (infoJob pJobId)
        root <- rawJS <$> firstSubDir
        let jobId = pJobId
        let empList = []   -- Can't seem to have this in the julius template!
        defaultLayout $ fileListWidget >> $(widgetFile "output-browser")
  where
    sendZippedOutput job = do file <- liftIO $ zippedOutput (jobId job)
                              sendFile typeOctet file

    fileListWidget = do
      addStylesheet $ StaticR jfileTree_css
      addScript $ StaticR jfileTree_js

    firstSubDir     = do actual <- liftIO $ getActualFile pJobId ["/"]
                         let root = "/"
                         return $ case actual of
                                    Directory files -> case filter fst files of
                                                         ((True,f):_) -> f
                                                         _ -> root
                                    _ -> root

outputFileAccess :: JobID -> [Text] -> Handler ActualFile
outputFileAccess jobId dir = do
    checkAccess =<< liftIO (infoJob jobId)
    file <- liftIO $ getActualFile jobId dir
    return file

getJobFilesAjaxR :: Text -> [Text] -> Handler RepHtml
getJobFilesAjaxR jobId dir = do
  file <- outputFileAccess jobId dir
  case file of
    InvalidPath -> notFound
    File filename -> sendFile typePlain filename
    Directory _ -> notFound

postJobFilesAjaxR :: Text -> [Text] -> Handler RepHtml
postJobFilesAjaxR jobId _ = do
  mDir <- lookupPostParam "dir"
  case mDir of
    Nothing -> notFound
    Just dir -> do file <- outputFileAccess jobId (splitDir dir)
                   case file of
                     InvalidPath -> notFound
                     File filename -> sendFile typePlain filename
                     Directory filePaths -> let files = map toView filePaths
                                            in hamletToRepHtml $(hamletFile "templates/output-files.hamlet")
  where
    splitDir dir = map T.pack $ splitDirectories $ T.unpack dir
    toView (isDir, path) = let dirs = splitDirectories path
                           in (isDir, path, last dirs, JobFilesAjaxR jobId (map T.pack dirs))
