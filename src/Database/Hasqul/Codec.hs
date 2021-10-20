{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Hasqul.Codec
    ( Codec(..)
    , Encoder(..)
    , ValueCodec(..)
    , decode
    ) where

import Data.ByteString            (ByteString)
import Data.Functor.Contravariant
import Data.Int                   (Int64, Int32, Int16)
import Data.Kind
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

type family UnEncoder (c :: Type) :: Type where
    UnEncoder (Encoder xs c) = c
    UnEncoder (ValueCodec c) = c
    UnEncoder c              = c

-- Encoder is used for deriving Codec with deriving via mechanism
newtype Encoder (a :: [Type]) b = Encoder { unEncoder :: b }

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

newtype ValueCodec a = ValueCodec { unValueCodec :: a }

instance Valuable a => Codec (ValueCodec a) where
    decodeRow = (Dec.column . Dec.nonNullable) (valueDec @a)
    encode    = (Enc.param . Enc.nonNullable) (valueEnc @a)

deriving via ValueCodec (Key a) instance Codec (Key a)
deriving via ValueCodec Bool instance Codec Bool
deriving via ValueCodec Int16 instance Codec Int16
deriving via ValueCodec Int32 instance Codec Int32
deriving via ValueCodec Int64 instance Codec Int64
deriving via ValueCodec Float instance Codec Float
deriving via ValueCodec Double instance Codec Double
deriving via ValueCodec Scientific instance Codec Scientific
deriving via ValueCodec Char instance Codec Char
deriving via ValueCodec Text instance Codec Text
deriving via ValueCodec ByteString instance Codec ByteString
deriving via ValueCodec Day instance Codec Day
deriving via ValueCodec LocalTime instance Codec LocalTime
deriving via ValueCodec UTCTime instance Codec UTCTime
deriving via ValueCodec TimeOfDay instance Codec TimeOfDay
deriving via ValueCodec DiffTime instance Codec DiffTime
deriving via ValueCodec UUID instance Codec UUID

instance Valuable a => Codec [a] where
    decodeRow = (Dec.column . Dec.nonNullable) (valueDec @[a])
    encode = (Enc.param . Enc.nonNullable) (valueEnc @[a])

instance Valuable a => Codec (Vector a) where
    decodeRow = (Dec.column . Dec.nonNullable) (valueDec @(Vector a))
    encode = (Enc.param . Enc.nonNullable) (valueEnc @(Vector a))

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

type family IsSimple (a :: Type) :: Bool where
    IsSimple [a]        = 'False
    IsSimple (Vector a) = 'False
    IsSimple (Maybe a)  = 'False
    IsSimple _          = 'True

class DecodeFromRow a (b :: Bool) where
    decode' :: Proxy b -> Dec.Result a

instance (UnEncoder a ~ a, Codec a) => DecodeFromRow a 'True where
    decode' _ = Dec.singleRow $ decodeRow @a

instance (UnEncoder a ~ a, Codec a) => DecodeFromRow [a] 'False where
    decode' _ = Dec.rowList $ decodeRow @a

instance (UnEncoder a ~ a, Codec a) => DecodeFromRow (Vector a) 'False where
    decode' _ = Dec.rowVector $ decodeRow @a

instance (UnEncoder a ~ a, Codec a) => DecodeFromRow (Maybe a) 'False where
    decode' _ = Dec.rowMaybe $ decodeRow @a

decode :: forall a. (UnEncoder a ~ a, DecodeFromRow a (IsSimple a)) => Dec.Result a
decode = decode' (Proxy @(IsSimple a))
