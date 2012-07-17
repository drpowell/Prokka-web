module Settings
  where

import Default_Util
import Language.Haskell.TH.Syntax

development = True

widgetFile :: String -> Q Exp
widgetFile = if development then Default_Util.widgetFileReload
                            else Default_Util.widgetFileNoReload

