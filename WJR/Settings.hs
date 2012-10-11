module WJR.Settings
  where

import Yesod.Default.Util
import Data.Default (def)
import Text.Hamlet

-- import Default_Util
import Language.Haskell.TH.Syntax

-- approotSetting = "http://127.0.0.1:3000"
approotSetting = "http://dna.med.monash.edu.au/prokka"

development = True


-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload
             ) widgetFileSettings

