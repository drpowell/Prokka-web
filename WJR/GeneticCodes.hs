{-# LANGUAGE OverloadedStrings #-}

module WJR.GeneticCodes
    where

import Data.String

geneticCodes :: (IsString a, IsString b) => [(a,b)]
geneticCodes = map swap
               [("1", "The Standard Code")
               ,("2", "The Vertebrate Mitochondrial Code")
               ,("3", "The Yeast Mitochondrial Code")
               ,("4", "The Mold, Protozoan, and Coelenterate Mitochondrial Code and the Mycoplasma/Spiroplasma Code")
               ,("5", "The Invertebrate Mitochondrial Code ")
               ,("6", "The Ciliate, Dasycladacean and Hexamita Nuclear Code")
               ,("9", "The Echinoderm and Flatworm Mitochondrial Code")
               ,("10", "The Euplotid Nuclear Code")
               ,("11", "The Bacterial, Archaeal and Plant Plastid Code")
               ,("12", "The Alternative Yeast Nuclear Code")
               ,("13", "The Ascidian Mitochondrial Code")
               ,("14", "The Alternative Flatworm Mitochondrial Code")
               ,("15", "Blepharisma Nuclear Code")
               ,("16", "Chlorophycean Mitochondrial Code")
               ,("21", "Trematode Mitochondrial Code")
               ,("22", "Scenedesmus Obliquus Mitochondrial Code")
               ,("23", "Thraustochytrium Mitochondrial Code")
               ,("24", "Pterobranchia Mitochondrial Code")
               ]
  where swap (a,b) = (b,a)