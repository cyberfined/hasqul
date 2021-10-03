{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Database.Hasqul.GCodec
    ( GCodec(..)
    , KnowNullable
    , IgnoreField
    , NoEncodeField
    , EncodeId
    ) where

import Database.Hasqul.Key
import Database.Hasqul.Valuable
import Data.Default.Class
import Data.Functor.Contravariant           ((>$<))
import Data.Functor.Contravariant.Divisible (divide)
import Data.Proxy                           (Proxy(..))
import GHC.Generics
import GHC.TypeLits

import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc

data Nullable
    = Nullable
    | NonNullable
    | NoEncDec
    | DecNoEnc
    | Prod !Nullable !Nullable

type family KnowNullable (grecord :: * -> *) (settings :: [*]) :: Nullable where
    KnowNullable (K1 _ (Maybe c))                    _  = 'Nullable
    KnowNullable (K1 _ c)                            _  = 'NonNullable
    KnowNullable (M1 S _ (K1 _ (Key c)))             xs = IsIdEncoded xs
    KnowNullable (M1 S ('MetaSel ('Just f) _ _ _) x) xs = IsIgnored f x xs
    KnowNullable (M1 _ _ x)                          xs = KnowNullable x xs
    KnowNullable (l :*: r)                           xs = 'Prod (KnowNullable l xs)
                                                                (KnowNullable r xs)

type family IsIgnored (x :: Symbol) (grecord :: * -> *) (settings :: [*]) :: Nullable where
    IsIgnored f _ (IgnoreField f ': xs)   = 'NoEncDec
    IsIgnored f _ (NoEncodeField f ': xs) = 'DecNoEnc
    IsIgnored f x (_ ': xs)               = IsIgnored f x xs
    IsIgnored _ x xs                      = KnowNullable x xs

type family IsIdEncoded (settings :: [*]) :: Nullable where
    IsIdEncoded (EncodeId ': _) = 'NonNullable
    IsIdEncoded (_ ': xs)       = IsIdEncoded xs
    IsIdEncoded _               = 'DecNoEnc

class GCodec grecord (null :: Nullable) where
    gDecode :: Proxy null -> grecord p -> Dec.Row (grecord p)
    gEncode :: Proxy null -> grecord p -> Enc.Params (grecord p)

instance Valuable c => GCodec (K1 i (Maybe c)) 'Nullable where
    gDecode _ _ = K1 <$> (Dec.column . Dec.nullable) (valueDec @c)
    gEncode _ _ = unK1 >$< (Enc.param . Enc.nullable) (valueEnc @c)

instance Valuable c => GCodec (K1 i c) 'NonNullable where
    gDecode _ _ = K1 <$> (Dec.column . Dec.nonNullable) (valueDec @c)
    gEncode _ _ = unK1 >$< (Enc.param . Enc.nonNullable) (valueEnc @c)

instance Default c => GCodec (K1 i c) 'NoEncDec where
    gDecode _ _ = pure (K1 def)
    gEncode _ _ = const (()) >$< Enc.noParams

instance Valuable c => GCodec (K1 i c) 'DecNoEnc where
    gDecode _ _ = K1 <$> (Dec.column . Dec.nonNullable) (valueDec @c)
    gEncode _ _ = const (()) >$< Enc.noParams

instance (GCodec l lNull, GCodec r rNull) => GCodec (l :*: r) ('Prod lNull rNull) where
    gDecode _ (l :*: r) =  (:*:)
                       <$> gDecode (Proxy @lNull) l
                       <*> gDecode (Proxy @rNull) r
    gEncode _ (l :*: r) = divide (\(x :*: y) -> (x, y)) leftEnc rightEnc
      where leftEnc = gEncode (Proxy @lNull) l
            rightEnc = gEncode (Proxy @rNull) r

instance GCodec x null => GCodec (M1 i t x) null where
    gDecode prx (M1 x) = M1 <$> gDecode prx x
    gEncode prx (M1 x) = unM1 >$< gEncode prx x

-- Settings

-- Field is not encoded and decoded.  Default value is used on decoding
data IgnoreField (a :: Symbol)

-- Field is not encoded, but decoding is performed
data NoEncodeField (a :: Symbol)

-- Id by default is not encoded. If you want to insert with id, set this option
data EncodeId
