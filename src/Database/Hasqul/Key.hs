{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Hasqul.Key
    ( Key(..)
    , coerceKey
    ) where

import Data.Aeson
import Data.Default.Class
import Data.Int                   (Int64)
import Database.Hasqul.Valuable
import GHC.Generics

-- Type for primary and foreign key
newtype Key a = Key { unKey :: Int64 }
    deriving stock (Generic, Show, Eq, Ord)
    deriving newtype (Valuable, Default, FromJSON, ToJSON)

coerceKey :: Key a -> Key b
coerceKey (Key a) = Key a
