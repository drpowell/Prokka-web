{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{- Copied from Yesod Default_Util, solely for the purpose of supporting coffeescript. Remove once yesod does this -}

-- | Various utilities used in the scaffolded site.
module Default_Util
    ( globFile
    , widgetFileNoReload
    , widgetFileReload
    , widgetFileJsCss
    ) where

import Yesod.Core -- purposely using complete import so that Haddock will see addStaticContent
import Control.Monad (when)
import System.Directory (doesFileExist)
import Language.Haskell.TH.Syntax
import Text.Lucius (luciusFile, luciusFileReload)
import Text.Julius (juliusFile, juliusFileReload)
import Text.Cassius (cassiusFile, cassiusFileReload)
import Text.Coffee (coffeeFile, coffeeFileReload)
import Data.Maybe (catMaybes)
import Prelude

-- | expects a file extension for each type, e.g: hamlet lucius julius
globFile :: String -> String -> FilePath
globFile kind x = "templates/" ++ x ++ "." ++ kind

widgetFileNoReload :: FilePath -> Q Exp
widgetFileNoReload x = combine "widgetFileNoReload" x
    [ whenExists x False "hamlet"  whamletFile
    , whenExists x True  "cassius" cassiusFile
    , whenExists x True  "julius"  juliusFile
    , whenExists x True  "lucius"  luciusFile
    , whenExists x True  "coffee"  coffeeFile
    ]

widgetFileReload :: FilePath -> Q Exp
widgetFileReload x = combine "widgetFileReload" x
    [ whenExists x False "hamlet"  whamletFile
    , whenExists x True  "cassius" cassiusFileReload
    , whenExists x True  "julius"  juliusFileReload
    , whenExists x True  "lucius"  luciusFileReload
    , whenExists x True  "coffee"  coffeeFileReload
    ]

widgetFileJsCss :: (String, FilePath -> Q Exp) -- ^ JavaScript file extenstion and loading function. example: ("julius", juliusFileReload)
                -> (String, FilePath -> Q Exp) -- ^ Css file extenstion and loading function. example: ("cassius", cassiusFileReload)
                -> FilePath -> Q Exp
widgetFileJsCss (jsExt, jsLoad) (csExt, csLoad) x = combine "widgetFileJsCss" x
    [ whenExists x False "hamlet"  whamletFile
    , whenExists x True  csExt csLoad
    , whenExists x True  jsExt jsLoad
    ]

combine :: String -> String -> [Q (Maybe Exp)] -> Q Exp
combine func file qmexps = do
    mexps <- sequence qmexps
    case catMaybes mexps of
        [] -> error $ concat
            [ "Called "
            , func
            , " on "
            , show file
            , ", but no template were found."
            ]
        exps -> return $ DoE $ map NoBindS exps

whenExists :: String
           -> Bool -- ^ requires toWidget wrap
           -> String -> (FilePath -> Q Exp) -> Q (Maybe Exp)
whenExists = warnUnlessExists False

warnUnlessExists :: Bool
                 -> String
                 -> Bool -- ^ requires toWidget wrap
                 -> String -> (FilePath -> Q Exp) -> Q (Maybe Exp)
warnUnlessExists shouldWarn x wrap glob f = do
    let fn = globFile glob x
    e <- qRunIO $ doesFileExist fn
    when (shouldWarn && not e) $ qRunIO $ putStrLn $ "widget file not found: " ++ fn
    if e
        then do
            ex <- f fn
            if wrap
                then do
                    tw <- [|toWidget|]
                    return $ Just $ tw `AppE` ex
                else return $ Just ex
        else return Nothing
