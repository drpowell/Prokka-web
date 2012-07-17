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


bootstrap_css :: Route Static
bootstrap_css = StaticRoute ["bootstrap.css"]    []

data App = App { getStatic :: Static }

mkYesod "App" [parseRoutes|
/static StaticR Static getStatic
/ RootR GET
/new NewR
/job/#Text JobR GET
/deleteJob/#Text DeleteJobR GET
|]

instance Yesod App where
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

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


main = static "static" >>= \s -> warpDebug 3000 (App s)

paramsAForm :: AForm App App Params
paramsAForm =
    let a = [freq "Name" textField
            ,fopt "Email" textField
            ,fopt "pulldown" (selectFieldList ([("One","One"),("Two","Two"),("Cee","Cee")] :: [(Text,Text)]))
                 ]
        p = fromList . mapMaybe maybeSnd <$> (sequenceA a)
        fileForm = fileAFormReq "FastA file"
    in Params <$> p <*> fileForm
  where
    maybeSnd :: (a, Maybe b) -> Maybe (a,b)
    maybeSnd (x,Just y) = Just (x,y)
    maybeSnd _ = Nothing

fopt :: String -> Field App App a -> AForm App App (Text, Maybe a)
fopt n f = (\x -> (fromString n,x)) <$> aopt f (fromString n) Nothing

freq :: String -> Field App App a -> AForm App App (Text, Maybe a)
freq n f = (\x -> (fromString n,Just x)) <$> areq f (fromString n) Nothing

paramsForm :: Html -> MForm App App (FormResult Params, Widget)
paramsForm = renderTable paramsAForm

jobsSession = "jobs"
getSessionList tag = do
  sess <- T.splitOn " " . fromMaybe "" <$> lookupSession tag
  return $ filter (not . T.null) sess
setSessionList tag lst = setSession tag (T.intercalate " " lst)
addToSessionList tag val = do
  sess' <- (val:) <$> getSessionList tag
  setSessionList tag sess'
  return sess'

getRootR :: Handler RepHtml
getRootR = do
  sess <- getSessionList jobsSession
  defaultLayout [whamlet|
                  <h1>Jobs!
                  $if null sess
                  $else
                    <p>Your Jobs
                    <ul>
                      $forall job <- sess
                        <li>
                          <a href=@{JobR job}>#{job}
                  <p>
                    <a href=@{NewR}>Create New Job
                 |]

getJobR :: Text -> Handler RepHtml
getJobR fname = do
  sess <- getSessionList jobsSession
  if fname `notElem` sess
     then notFound
     else do params <- liftIO $ infoJob (T.unpack fname)
             defaultLayout [whamlet|<p>Job : #{fname}
                                    <p>Parameters
                                    $forall (key,val) <- params
                                       <li><b>#{key}</b>: #{val}
                                    <p>
                                      <a href=@{DeleteJobR fname}>Delete Job
                            |]

getDeleteJobR :: Text -> Handler RepHtml
getDeleteJobR fname = do
  sess <- getSessionList jobsSession
  if fname `notElem` sess
     then notFound
     else do setSessionList jobsSession (Data.List.delete fname sess)
             liftIO $ deleteJob (T.unpack fname)
             setMessage $ msgLabel "Job Deleted"
             redirect RootR

msgLabel :: Text -> Html
msgLabel msg = [shamlet|<span class="label label-success">#{msg}</span>|]

handleNewR :: Handler RepHtml
handleNewR = do
    ((result, widget), enctype) <- runFormPost paramsForm
    case result of
        FormSuccess params -> do fname <- liftIO $ createJob params
                                 sess <- addToSessionList jobsSession fname
                                 setMessage $ msgLabel "Job created."
                                 redirect RootR
        _ ->  defaultLayout [whamlet|
<h1>Create New Job
<form method=post action=@{NewR} enctype=#{enctype}>
    <table>
        ^{widget}
    <input type=submit>
|]


