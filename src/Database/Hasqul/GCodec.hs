{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Hasqul.GCodec
    ( GCodec(..)
    , KnowNullable
    , IdType
    ) where

import Database.Hasqul.Key
import Database.Hasqul.Options
import Database.Hasqul.Valuable
import Data.Default.Class
import Data.Functor.Contravariant           ((>$<))
import Data.Functor.Contravariant.Divisible (divide)
import Data.Kind
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


type family KnowNullable (grecord :: Type -> Type) (settings :: [Type]) :: Nullable where
    KnowNullable rec xs = Fst (KnowNullableSettings rec xs)

type family KnowNullableSettings (grecord :: Type -> Type) (settings :: [Type])
    :: (Nullable, [Type]) where
    KnowNullableSettings (K1 _ (Maybe _)) xs = '( 'Nullable, xs)
    KnowNullableSettings (K1 _ _)         xs = '( 'NonNullable, xs)
    KnowNullableSettings (M1 S _ (K1 _ (Key k))) xs = IsIdEncoded k xs
    KnowNullableSettings (M1 S ('MetaSel ('Just f) _ _ _) x) xs = '( IsIgnored f x xs, xs)
    KnowNullableSettings (M1 _ _ x) xs = KnowNullableSettings x xs
    KnowNullableSettings (l :*: r) xs = KnowNullableProd (KnowNullableSettings l xs) r

type family KnowNullableProd (xs :: (Nullable, [Type])) (grecord :: Type -> Type)
    :: (Nullable, [Type]) where
    KnowNullableProd '(n, xs) r = ConsProd n (KnowNullableSettings r xs)

type family ConsProd (n :: Nullable) (xs :: (Nullable, [Type]))
    :: (Nullable, [Type]) where
    ConsProd l '(r, xs) = '( 'Prod l r, xs)

type family IsIgnored (x :: Symbol) (grecord :: Type -> Type) (settings :: [Type])
    :: Nullable where
    IsIgnored f _ (IgnoreField f ': xs)   = 'NoEncDec
    IsIgnored f _ (NoEncodeField f ': xs) = 'DecNoEnc
    IsIgnored f x (_ ': xs)               = IsIgnored f x xs
    IsIgnored _ x xs                      = KnowNullable x xs

type family IsIdEncoded (k :: Type) (settings :: [Type]) :: (Nullable, [Type]) where
    IsIdEncoded _ (EncodeId ': xs) = '( 'NonNullable, EncodeId ': xs)
    IsIdEncoded k (IdType k ': xs) = '(HasEncodeId xs, xs)
    IsIdEncoded k (x ': xs)        = InsertSettings x (IsIdEncoded k xs)
    IsIdEncoded _ _                = '( 'NonNullable, '[])

type family HasEncodeId (settings :: [Type]) :: Nullable where
    HasEncodeId (EncodeId ': _) = 'NonNullable
    HasEncodeId (_ ': xs)       = HasEncodeId xs
    HasEncodeId _               = 'DecNoEnc

type family Fst (tuple :: (Nullable, [Type])) :: Nullable where
    Fst '(x, _) = x

type family InsertSettings (x :: Type) (xs :: (Nullable, [Type]))
    :: (Nullable, [Type]) where
    InsertSettings x '(n, xs) = '(n, x ': xs)

data IdType a

class GCodec grecord (null :: Nullable) where
    gDecode :: Proxy null -> Proxy grecord -> Dec.Row grecord
    gEncode :: Proxy null -> Proxy grecord -> Enc.Params grecord

instance Valuable c => GCodec (K1 i (Maybe c) p) 'Nullable where
    gDecode _ _ = K1 <$> (Dec.column . Dec.nullable) (valueDec @c)
    gEncode _ _ = unK1 >$< (Enc.param . Enc.nullable) (valueEnc @c)

instance Valuable c => GCodec (K1 i c p) 'NonNullable where
    gDecode _ _ = K1 <$> (Dec.column . Dec.nonNullable) (valueDec @c)
    gEncode _ _ = unK1 >$< (Enc.param . Enc.nonNullable) (valueEnc @c)

instance Default c => GCodec (K1 i c p) 'NoEncDec where
    gDecode _ _ = pure (K1 def)
    gEncode _ _ = const () >$< Enc.noParams

instance Valuable c => GCodec (K1 i c p) 'DecNoEnc where
    gDecode _ _ = K1 <$> (Dec.column . Dec.nonNullable) (valueDec @c)
    gEncode _ _ = const () >$< Enc.noParams

instance (GCodec (l p) lNull, GCodec (r p) rNull)
  => GCodec ((l :*: r) p) ('Prod lNull rNull) where
    gDecode _ _  =  (:*:)
                <$> gDecode (Proxy @lNull) (Proxy @(l p))
                <*> gDecode (Proxy @rNull) (Proxy @(r p))
    gEncode _ _ = divide (\(l :*: r) -> (l, r)) leftEnc rightEnc
      where leftEnc = gEncode (Proxy @lNull) (Proxy @(l p))
            rightEnc = gEncode (Proxy @rNull) (Proxy @(r p))

instance GCodec (x p) null => GCodec (M1 i t x p) null where
    gDecode prx _ = M1 <$> gDecode prx (Proxy @(x p))
    gEncode prx _ = unM1 >$< gEncode prx (Proxy @(x p))
