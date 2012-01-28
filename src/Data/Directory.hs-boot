{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Directory
    ( Directory
    , Momentable
    , Operable
    , offset
    , pinpoint
    , location
    ) where
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Dangerous
import {-# SOURCE #-} Control.DateTime.Moment ( Direction, Moment )
import {-# SOURCE #-} Text.Point
import {-# SOURCE #-} Text.Pin


class ( Applicative a
      , Errorable a
      , MonadReader Directory a
      ) => Operable a

class ( Operable a
      , MonadState [String] a
      ) => Momentable a

class ( Operable a
      , MonadState [File] a
      ) => Restricted a

instance (Operable a) => Operable (StateT [File] a)
instance (Operable a) => Operable (StateT [String] a)
instance (Operable a) => Restricted (StateT [File] a)

data Directory
data File

offset :: (Momentable m) => String -> m (Maybe (Direction, Moment))
pinpoint :: (Momentable m) => Pin -> Maybe Point -> m Moment
location :: (Operable m) => Pin -> Maybe Point -> m String
