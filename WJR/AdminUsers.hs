{-# LANGUAGE OverloadedStrings #-}
module WJR.AdminUsers
    ( adminUser
    ) where

adminUser "david@drp.id.au" = True
adminUser "torsten.seemann@gmail.com" = True
adminUser _ = False

