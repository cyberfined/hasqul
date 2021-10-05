{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database.Hasqul.Codec
    ( Codec(..)
    , Table(..)
    , decode
    ) where

import Data.Functor.Contravariant
import Data.Proxy
import Data.Vector                (Vector)
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
    decodeRow :: Dec.Row (UnTable c)
    encode    :: Enc.Params (UnTable c)

instance (Generic a, GCodec (Rep a p) (KnowNullable (Rep a) xs))
  => Codec (Table xs a) where
    decodeRow = fmap to $ gDecode nullPrx repPrx
      where nullPrx = Proxy @(KnowNullable (Rep a) xs)
            repPrx = Proxy @(Rep a p)
    encode = from >$< (gEncode nullPrx repPrx)
      where nullPrx = Proxy @(KnowNullable (Rep a) xs)
            repPrx = Proxy @(Rep a p)

-- Table is used for deriving Codec with deriving via mechanism
newtype Table (a :: [*]) b = Table { unTable :: b }

instance Codec (Key a) where
    decodeRow = (Dec.column . Dec.nonNullable) (valueDec @(Key a))
    encode    = (Enc.param . Enc.nonNullable) (valueEnc @(Key a))

instance Codec () where
    decodeRow = pure ()
    encode    = Enc.noParams

type family IsSingle (a :: *) :: Bool where
    IsSingle [a]        = 'False
    IsSingle (Vector a) = 'False
    IsSingle _          = 'True

class DecodeFromRow a (b :: Bool) where
    decode' :: Proxy b -> Dec.Result a

instance (UnTable a ~ a, Codec a) => DecodeFromRow a 'True where
    decode' _ = Dec.singleRow $ decodeRow @a

instance (UnTable a ~ a, Codec a) => DecodeFromRow [a] 'False where
    decode' _ = Dec.rowList $ decodeRow @a

instance (UnTable a ~ a, Codec a) => DecodeFromRow (Vector a) 'False where
    decode' _ = Dec.rowVector $ decodeRow @a

decode :: forall a. (UnTable a ~ a, DecodeFromRow a (IsSingle a)) => Dec.Result a
decode = decode' (Proxy @(IsSingle a))
