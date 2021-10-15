{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Hasqul.Codec
    ( Codec(..)
    , Encoder(..)
    , decode
    ) where

import Data.ByteString            (ByteString)
import Data.Functor.Contravariant
import Data.Int                   (Int64, Int32, Int16)
import Data.Proxy
import Data.Text                  (Text)
import Data.Time                  (Day, LocalTime, UTCTime, TimeOfDay, DiffTime)
import Data.Scientific            (Scientific)
import Data.UUID                  (UUID)
import Data.Vector                (Vector)
import Database.Hasqul.GCodec
import Database.Hasqul.Key
import Database.Hasqul.Valuable
import GHC.Generics

import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc

type family UnEncoder (c :: *) :: * where
    UnEncoder (Encoder xs c) = c
    UnEncoder c            = c

class Codec c where
    decodeRow :: Dec.Row (UnEncoder c)
    encode    :: Enc.Params (UnEncoder c)

instance (Generic a, GCodec (Rep a p) (KnowNullable (Rep a) xs))
  => Codec (Encoder xs a) where
    decodeRow = to <$> gDecode nullPrx repPrx
      where nullPrx = Proxy @(KnowNullable (Rep a) xs)
            repPrx = Proxy @(Rep a p)
    encode = from >$< gEncode nullPrx repPrx
      where nullPrx = Proxy @(KnowNullable (Rep a) xs)
            repPrx = Proxy @(Rep a p)

-- Encoder is used for deriving Codec with deriving via mechanism
newtype Encoder (a :: [*]) b = Encoder { unEncoder :: b }

instance Valuable a => Codec (Maybe a) where
    decodeRow = (Dec.column . Dec.nullable) (valueDec @a)
    encode    = (Enc.param . Enc.nullable) (valueEnc @a)

instance Codec (Key a) where
    decodeRow = (Dec.column . Dec.nonNullable) (Key <$> Dec.int8)
    encode    = (Enc.param . Enc.nonNullable) (unKey >$< Enc.int8)

instance Codec Bool where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.bool
    encode    = (Enc.param . Enc.nonNullable) Enc.bool

instance Codec Int16 where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.int2
    encode    = (Enc.param . Enc.nonNullable) Enc.int2

instance Codec Int32 where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.int4
    encode    = (Enc.param . Enc.nonNullable) Enc.int4

instance Codec Int64 where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.int8
    encode    = (Enc.param . Enc.nonNullable) Enc.int8

instance Codec Float where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.float4
    encode    = (Enc.param . Enc.nonNullable) Enc.float4

instance Codec Double where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.float8
    encode    = (Enc.param . Enc.nonNullable) Enc.float8

instance Codec Scientific where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.numeric
    encode    = (Enc.param . Enc.nonNullable) Enc.numeric

instance Codec Char where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.char
    encode    = (Enc.param . Enc.nonNullable) Enc.char

instance Codec Text where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.text
    encode    = (Enc.param . Enc.nonNullable) Enc.text

instance Codec ByteString where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.bytea
    encode    = (Enc.param . Enc.nonNullable) Enc.bytea

instance Codec Day where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.date
    encode    = (Enc.param . Enc.nonNullable) Enc.date

instance Codec LocalTime where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.timestamp
    encode    = (Enc.param . Enc.nonNullable) Enc.timestamp

instance Codec UTCTime where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.timestamptz
    encode    = (Enc.param . Enc.nonNullable) Enc.timestamptz

instance Codec TimeOfDay where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.time
    encode    = (Enc.param . Enc.nonNullable) Enc.time

instance Codec DiffTime where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.interval
    encode    = (Enc.param . Enc.nonNullable) Enc.interval

instance Codec UUID where
    decodeRow = (Dec.column . Dec.nonNullable) Dec.uuid
    encode    = (Enc.param . Enc.nonNullable) Enc.uuid

instance Codec () where
    decodeRow = pure ()
    encode    = Enc.noParams

instance (UnEncoder a ~ a, Codec a, UnEncoder b ~ b, Codec b) => Codec (a, b) where
    decodeRow = (,) <$> decodeRow @a <*> decodeRow @b
    encode    = (fst >$< encode @a) <> (snd >$< encode @b)

instance (UnEncoder a ~ a, Codec a, UnEncoder b ~ b, Codec b, UnEncoder c ~ c, Codec c)
  => Codec (a, b, c) where
    decodeRow = (,,) <$> decodeRow @a <*> decodeRow @b <*> decodeRow @c
    encode    =  ((\(a,_,_) -> a) >$< encode @a)
              <> ((\(_,b,_) -> b) >$< encode @b)
              <> ((\(_,_,c) -> c) >$< encode @c)

instance ( UnEncoder a ~ a, Codec a
         , UnEncoder b ~ b, Codec b
         , UnEncoder c ~ c, Codec c
         , UnEncoder d ~ d, Codec d
         ) => Codec (a, b, c, d) where
    decodeRow = (,,,) <$> decodeRow @a <*> decodeRow @b <*> decodeRow @c <*> decodeRow @d
    encode    =  ((\(a,_,_,_) -> a) >$< encode @a)
              <> ((\(_,b,_,_) -> b) >$< encode @b)
              <> ((\(_,_,c,_) -> c) >$< encode @c)
              <> ((\(_,_,_,d) -> d) >$< encode @d)

instance ( UnEncoder a ~ a, Codec a
         , UnEncoder b ~ b, Codec b
         , UnEncoder c ~ c, Codec c
         , UnEncoder d ~ d, Codec d
         , UnEncoder e ~ e, Codec e
         ) => Codec (a, b, c, d, e) where
    decodeRow =  (,,,,)
             <$> decodeRow @a
             <*> decodeRow @b
             <*> decodeRow @c
             <*> decodeRow @d
             <*> decodeRow @e
    encode    =  ((\(a,_,_,_,_) -> a) >$< encode @a)
              <> ((\(_,b,_,_,_) -> b) >$< encode @b)
              <> ((\(_,_,c,_,_) -> c) >$< encode @c)
              <> ((\(_,_,_,d,_) -> d) >$< encode @d)
              <> ((\(_,_,_,_,e) -> e) >$< encode @e)

instance ( UnEncoder a ~ a, Codec a
         , UnEncoder b ~ b, Codec b
         , UnEncoder c ~ c, Codec c
         , UnEncoder d ~ d, Codec d
         , UnEncoder e ~ e, Codec e
         , UnEncoder f ~ f, Codec f
         ) => Codec (a, b, c, d, e, f) where
    decodeRow =  (,,,,,)
             <$> decodeRow @a
             <*> decodeRow @b
             <*> decodeRow @c
             <*> decodeRow @d
             <*> decodeRow @e
             <*> decodeRow @f
    encode    =  ((\(a,_,_,_,_,_) -> a) >$< encode @a)
              <> ((\(_,b,_,_,_,_) -> b) >$< encode @b)
              <> ((\(_,_,c,_,_,_) -> c) >$< encode @c)
              <> ((\(_,_,_,d,_,_) -> d) >$< encode @d)
              <> ((\(_,_,_,_,e,_) -> e) >$< encode @e)
              <> ((\(_,_,_,_,_,f) -> f) >$< encode @f)

type family IsSingle (a :: *) :: Bool where
    IsSingle [a]        = 'False
    IsSingle (Vector a) = 'False
    IsSingle _          = 'True

class DecodeFromRow a (b :: Bool) where
    decode' :: Proxy b -> Dec.Result a

instance (UnEncoder a ~ a, Codec a) => DecodeFromRow a 'True where
    decode' _ = Dec.singleRow $ decodeRow @a

instance (UnEncoder a ~ a, Codec a) => DecodeFromRow [a] 'False where
    decode' _ = Dec.rowList $ decodeRow @a

instance (UnEncoder a ~ a, Codec a) => DecodeFromRow (Vector a) 'False where
    decode' _ = Dec.rowVector $ decodeRow @a

decode :: forall a. (UnEncoder a ~ a, DecodeFromRow a (IsSingle a)) => Dec.Result a
decode = decode' (Proxy @(IsSingle a))
