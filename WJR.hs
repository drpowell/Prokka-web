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

import AdminUsers (adminUser)
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

/queue QueueR GET
/new NewR
/job/#Text JobR GET
/deleteJob/#Text DeleteJobR GET
/all-jobs AllJobsR GET
|]

-- | Authorization for the various routes.  TODO, use this to authorize access to jobs?
routeAuthorized r _write
    | okRoutes r       = return Authorized
    | loggedInRoutes r = loggedIn
    | adminRoutes r    = isAdmin
    | otherwise        = return $ Unauthorized "Not allowed"
  where
    okRoutes RootR       = True
    okRoutes (AuthR _)   = True
    okRoutes (StaticR _) = True
    okRoutes _           = False
    loggedInRoutes QueueR         = True
    loggedInRoutes NewR           = True
    loggedInRoutes (JobR _)       = True
    loggedInRoutes (DeleteJobR _) = True
    loggedInRoutes _              = False
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
    approot = ApprootStatic "http://dna.med.monash.edu.au:3000"
    authRoute _ = Just $ AuthR LoginR

    isAuthorized = routeAuthorized

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
        authId <- maybeAuthId
        let isAdmin = maybe False adminUser authId
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- Allow big uploads for new jobs : TODO, more efficient upload handling : http://stackoverflow.com/questions/10880105/efficient-large-file-upload-with-yesod
    maximumContentLength _ (Just NewR) = 2 * 1024 * 1024 * 1024 -- 2 gigabytes
    maximumContentLength _ _ = 2 * 1024 * 1024 -- 2 megabytes


instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _     = QueueR
    logoutDest _    = RootR
    authPlugins _   = [authBrowserId, authGoogleEmail]
    authHttpManager = httpManager

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


main = do
  st <- static "static"
  man <- newManager def
  warpDebug 3000 (App st man)

data ParamForm = ParamForm { fileInfo :: FileInfo
                           , paramMap :: Map Text Text
                           }
                 deriving (Show)

pairs = map (\x -> (x,x))

-- Params for the jobs.  TODO - separate this out (how?)
paramsAForm :: AForm App App ParamForm
paramsAForm =
    let fileForm = fileAFormReq "Contigs file (FastA): "
        a = [freq "Kingdom" (selectFieldList (pairs ["Bacteria","Archaea","Viruses"]))
            ]
        p = fromList . mapMaybe maybeSnd <$> (sequenceA a)
    in ParamForm <$> fileForm <*> p
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
getRootR = defaultLayout $(widgetFile "home")

getQueueR :: Handler RepHtml
getQueueR = do
  jobs <- myJobs
  defaultLayout $(widgetFile "queue")

getAllJobsR :: Handler RepHtml
getAllJobsR = do
  jobs <- liftIO $ allJobs
  defaultLayout $(widgetFile "all-queue")

checkAccess Nothing = notFound
checkAccess (Just job) = do
  maid <- maybeAuthId
  when (maybe True (jobUser job /=) maid) $
       notFound

jobStatusText :: Job -> Text
jobStatusText job = case jobStatus job of
                      JobWaiting -> "Waiting"
                      JobRunning _ -> "Running..."
                      JobComplete _ -> "Finished"

getJobR :: JobID -> Handler RepHtml
getJobR jobId = do
  job <- liftIO $ infoJob jobId
  checkAccess job
  case job of
    Nothing -> notFound
    Just job -> let params = toList $ jobParams job
                    status = jobStatusText job
                in defaultLayout $(widgetFile "job")

getDeleteJobR :: JobID -> Handler RepHtml
getDeleteJobR jobId = do
  job <- liftIO $ infoJob jobId
  checkAccess job
  liftIO $ deleteJob jobId
  setMessage $ msgLabel "Job Deleted"
  redirect QueueR

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
                                 redirect QueueR
        _ ->  defaultLayout $(widgetFile "new-job")

