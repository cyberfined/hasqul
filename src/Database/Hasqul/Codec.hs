{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Hasqul.Codec
    ( Codec(..)
    , Table(..)
    ) where

import Data.Default.Class
import Data.Functor.Contravariant
import Data.Proxy
import Database.Hasqul.GCodec
import Database.Hasqul.Key
import Database.Hasqul.Valuable
import GHC.Generics

import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc

type family UnTable (c :: *) :: * where
    UnTable (Table xs c) = c
    UnTable c            = c

class Codec c where
    decode :: Dec.Row (UnTable c)
    encode :: Enc.Params (UnTable c)

instance (Generic a, Default a, GCodec (Rep a) (KnowNullable (Rep a) xs))
  => Codec (Table xs a) where
    decode = fmap to $ gDecode prx $ from (def @a)
      where prx = Proxy @(KnowNullable (Rep a) xs)
    encode = from >$< (gEncode prx $ from (def @a))
      where prx = Proxy @(KnowNullable (Rep a) xs)

-- Table is used for deriving Codec with deriving via mechanism
newtype Table (a :: [*]) b = Table { unTable :: b }

instance Codec (Key a) where
    decode = (Dec.column . Dec.nonNullable) (valueDec @(Key a))
    encode = (Enc.param . Enc.nonNullable) (valueEnc @(Key a))
