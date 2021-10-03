{-# LANGUAGE DeriveGeneric #-}

module Database.Hasqul.Key
    ( Key(..)
    ) where

import Data.Default.Class
import Data.Functor.Contravariant ((>$<))
import Data.Int                   (Int64)
import Database.Hasqul.Valuable
import GHC.Generics

import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc

-- Type for primary and foreign key
newtype Key a = Key { unKey :: Int64 } deriving (Generic, Show, Eq, Ord)

instance Valuable (Key a) where
    valueDec = Key <$> Dec.int8
    valueEnc = unKey >$< Enc.int8

instance Default (Key a) where
    def = Key 0
