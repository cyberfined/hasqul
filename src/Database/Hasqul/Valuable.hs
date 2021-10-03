{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Database.Hasqul.Valuable
    ( Valuable(..)
    , TextEnum
    , IntEnum
    ) where

import Data.Functor.Contravariant (contramap)
import Data.Int                   (Int64, Int32, Int16)
import Data.ByteString            (ByteString)
import Data.Text                  (Text)
import Data.Time                  (Day, LocalTime, UTCTime, TimeOfDay, DiffTime)
import Data.Scientific            (Scientific)
import Data.UUID                  (UUID)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified Hasql.Decoders      as Dec
import qualified Hasql.Encoders      as Enc

class Valuable a where
    valueDec :: Dec.Value a
    valueEnc :: Enc.Value a

instance Valuable Bool where
    valueDec = Dec.bool
    valueEnc = Enc.bool

instance Valuable Int16 where
    valueDec = Dec.int2
    valueEnc = Enc.int2

instance Valuable Int32 where
    valueDec = Dec.int4
    valueEnc = Enc.int4

instance Valuable Int64 where
    valueDec = Dec.int8
    valueEnc = Enc.int8

instance Valuable Float where
    valueDec = Dec.float4
    valueEnc = Enc.float4

instance Valuable Double where
    valueDec = Dec.float8
    valueEnc = Enc.float8

instance Valuable Scientific where
    valueDec = Dec.numeric
    valueEnc = Enc.numeric

instance Valuable Char where
    valueDec = Dec.char
    valueEnc = Enc.char

instance Valuable Text where
    valueDec = Dec.text
    valueEnc = Enc.text

instance Valuable ByteString where
    valueDec = Dec.bytea
    valueEnc = Enc.bytea

instance Valuable Day where
    valueDec = Dec.date
    valueEnc = Enc.date

instance Valuable LocalTime where
    valueDec = Dec.timestamp
    valueEnc = Enc.timestamp

instance Valuable UTCTime where
    valueDec = Dec.timestamptz
    valueEnc = Enc.timestamptz

instance Valuable TimeOfDay where
    valueDec = Dec.time
    valueEnc = Enc.time

instance Valuable DiffTime where
    valueDec = Dec.interval
    valueEnc = Enc.interval

instance Valuable UUID where
    valueDec = Dec.uuid
    valueEnc = Enc.uuid

-- TextEnum is used for deriving Valuable with deriving via mechanism
-- for text represented enums
newtype TextEnum a = TextEnum { unTextEnum :: a }

instance (Show a, Bounded a, Enum a) => Valuable (TextEnum a) where
    valueDec = Dec.enum (flip HashMap.lookup inverseMap)
      where inverseMap = HashMap.fromList $ map (toMap . toEnum) [minInt..maxInt]
            toMap :: a -> (Text, TextEnum a)
            toMap x = (Text.pack $ show $ x, TextEnum x)
            minInt = fromEnum (minBound @a)
            maxInt = fromEnum (maxBound @a)
    valueEnc = Enc.enum (Text.pack . show . unTextEnum)

-- IntEnum is used for deriving Valuable with deriving via mechanism
-- for int represented enums
newtype IntEnum a b = IntEnum { unIntEnum :: a }

instance (Enum a, Integral b, Valuable b) => Valuable (IntEnum a b) where
    valueDec = fmap (IntEnum . toEnum . fromIntegral) (valueDec @b)
    valueEnc = contramap (fromIntegral . fromEnum . unIntEnum) (valueEnc @b)
