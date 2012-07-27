{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ParamDefs
    ( ParamField(..), FieldType(..), paramDefs
    ) where

import GeneticCodes
import Data.Text (Text)
import qualified Data.Text as T
import Data.String

data FieldType = TextField
               | CheckBoxField
               | ListField [(Text,Text)]

data ParamField s m = ParamField { name :: Text
                                 , label :: String
                                 , tip :: String
                                 , field :: FieldType
                                 , defVal :: Maybe Text

                                 , toRun :: ParamField s m -> Text -> [Text]
                                 }

pairs = map (\x -> (x,x))

paramDefs :: [ParamField s m]
paramDefs =
    let gcodes = map (\(a,b) -> (fromString $ b ++ " : " ++ a, fromString b)) geneticCodes
        grams = pairs ["Ignore","+ve","-ve"]
    in [ParamField "name" "Project Name" "A name for you to identifying this job by" TextField Nothing runNo
       ,ParamField "locustag" "Locus tag prefix" "Locus tag prefix" TextField (Just "PROKKA") runId
       ,ParamField "genus" "Genus" "Genus name - will be used to aid annotation" TextField (Just "Genus") runId
       ,ParamField "species" "Species" "Species name" TextField Nothing runId
       ,ParamField "strain" "Strain" "Strain name" TextField Nothing runId
       ,ParamField "gcode" "Genetic Code" "Genetic Code / Translation table" (ListField gcodes) (Just "11") runId
       ,ParamField "gram" "Gram" "Gram +ve or Gram -ve" (ListField grams) Nothing runGram
       ,ParamField "addgenes" "Add genes" "Add 'gene' features for each 'CDS' feature" CheckBoxField Nothing runBool
       ,ParamField "usegenus" "Use genus" "Use genus-specific BLAST databases (requires 'Genus' above to be defined)"  CheckBoxField Nothing runBool
       ]

runNo, runId, runBool, runGram :: ParamField a b -> Text -> [Text]

runNo _ _ = []

runId fld val = ["--" `T.append` name fld, val]

runBool fld val
    | val=="yes" = ["--" `T.append` name fld]
    | otherwise  = []

runGram fld val
    | val=="Ignore" = []
    | otherwise = runId fld val
