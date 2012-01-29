{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Directory
    ( Directory
    , Momentable
    , safeRecurseEra
    , offset
    , pinpoint
    , location
    ) where
import Control.Applicative
import Control.Monad.Reader
import Control.Dangerous
import Text.Pinpoint
import {-# SOURCE #-} Control.DateTime.Moment ( Direction, Moment )

class Stated a
class ( Applicative a
      , Errorable a
      , MonadReader Directory a
      , Stated a
      ) => Momentable a

data Directory

offset :: (Momentable m) => String -> m (Maybe (Direction, Moment))
pinpoint :: (Momentable m) => Pinpoint -> m Moment
location :: (Momentable m) => Pinpoint -> m String
safeRecurseEra :: (Momentable m) => String -> (String -> m String) -> m String
