{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.File where
import Control.Applicative
import Control.DateTime.Moment
import Text.Fragment
import Text.Render
import {-# SOURCE #-} Data.Body
import {-# SOURCE #-} Data.Directory

data Key = Key { id      :: String
               , name    :: String
               , tags    :: [String]
               , matches :: String -> Maybe Bool }

data File = File Key Body

pinpoint :: [String] -> File -> Operation Moment
pinpoint _ _ = return $ Unknown "Can't pinpoint events yet"

link :: [String] -> File -> String
link _ _ = "Can't href to events yet"

title :: File -> String
title (File k _) = name k

instance Fragment File where
    resolve (File k b) = document (name k) <$> header <*> resolve b
        where header = return "Yeah we don't do headers yet"
