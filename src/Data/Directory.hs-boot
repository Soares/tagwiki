{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Directory
    ( Directory
    , File
    , Momentable
    , offset
    , pinpoint
    , location
    ) where
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Dangerous
import {-# SOURCE #-} Control.DateTime.Moment ( Direction, Moment )
import {-# SOURCE #-} Text.Pinpoint
import {-# SOURCE #-} Data.Trail


class ( Applicative a
      , Errorable a
      , MonadReader Directory a
      , MonadState Trail a
      ) => Momentable a

data Directory

data File
instance Eq File
instance Show File

offset :: (Momentable m) => String -> m (Maybe (Direction, Moment))
pinpoint :: (Momentable m) => Pinpoint -> m Moment
location :: (Momentable m) => Pinpoint -> m String
