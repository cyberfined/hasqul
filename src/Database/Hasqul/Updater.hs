{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Hasqul.Updater
    ( FromStatement(..)
    , Updater(..)
    , Updatable(..)
    ) where

import Database.Hasqul.Codec                (encode)
import Database.Hasqul.Key
import Database.Hasqul.Options
import Database.Hasqul.Valuable
import Data.ByteString                      (ByteString)
import ByteString.StrictBuilder
import Data.Char
import Data.Functor.Contravariant           ((>$<))
import Data.Functor.Contravariant.Divisible (choose, divide)
import Data.List                            (foldl')
import Data.Maybe                           (fromMaybe, isNothing)
import Data.Proxy
import Data.Tuple                           (swap)
import GHC.Generics
import GHC.TypeLits
import Hasql.Decoders                       (noResult)
import Hasql.Encoders
import Hasql.Session                        (Session)
import Hasql.Transaction                    (Transaction)
import Hasql.Statement                      (Statement(..))

import qualified Data.ByteString.Char8 as BS
import qualified Hasql.Session         as Session
import qualified Hasql.Transaction     as Transaction

newtype Updater (a :: [*]) b = Updater { unUpdater :: b }

class FromStatement a b where
    update :: Key a -> a -> b ()

instance Updatable a => FromStatement a Session where
    update kx x = case updateStatement x of
        Nothing -> pure ()
        Just st -> Session.statement (kx, x) st

instance Updatable a => FromStatement a Transaction where
    update kx x = case updateStatement x of
        Nothing -> pure ()
        Just st -> Transaction.statement (kx, x) st

class Updatable a where
    updateStatement :: a -> Maybe (Statement (Key a, a) ())

instance (ToSettings xs, Generic a, GUpdatable (Rep a) (KnowUpdateType (Rep a) xs)) =>
    Updatable (Updater xs a) where
    updateStatement upd = statement $ updInfo { updEncoder = enc }
      where updInfo = gUpdateInfo sets updPrx (from $ unUpdater upd)
            enc = from . unUpdater >$< updEncoder updInfo
            sets = toSettings (Proxy @xs)
            updPrx = Proxy @(KnowUpdateType (Rep a) xs)

data UpdateType
    = NonNullable
    | Nullable
    | Always
    | Ignored
    | Prod !UpdateType !UpdateType

type family KnowUpdateType (grecord :: * -> *) (settings :: [*]) :: UpdateType where
    KnowUpdateType (K1 _ (Maybe (Maybe _)))            _  = 'Nullable
    KnowUpdateType (K1 _ (Maybe _))                    _  = 'NonNullable
    KnowUpdateType (K1 _ _)                            _  = 'Always
    KnowUpdateType (M1 S ('MetaSel ('Just f) _ _ _) x) xs = IsIgnored f x xs
    KnowUpdateType (M1 _ _ x)                          xs = KnowUpdateType x xs
    KnowUpdateType (l :*: r)                           xs = 'Prod (KnowUpdateType l xs)
                                                                  (KnowUpdateType r xs)

type family IsIgnored (x :: Symbol) (grecord :: * -> *) (settings :: [*]) :: UpdateType
  where
    IsIgnored f _ (IgnoreField f ': xs) = 'Ignored
    IsIgnored f x (_ ': xs)             = IsIgnored f x xs
    IsIgnored _ x xs                    = KnowUpdateType x xs

class GUpdatable grecord (upd :: UpdateType) where
    gUpdateInfo :: Settings -> Proxy upd -> grecord p -> UpdateInfo (grecord p)

instance (GUpdatable l lUpd, GUpdatable r rUpd)
  => GUpdatable (l :*: r) ('Prod lUpd rUpd) where
    gUpdateInfo sets _ (l :*: r) = (combine updL updR) { updEncoder = enc }
      where updL = gUpdateInfo sets (Proxy @lUpd) l
            updR = gUpdateInfo sets (Proxy @rUpd) r
            enc = divide (\(x :*: y) -> (x,y)) (updEncoder updL) (updEncoder updR)
            combine u1 u2 = u1 { updColumns = updColumns u1 <> updColumns u2 }

instance (Selector s, Valuable x) =>
    GUpdatable (M1 S s (K1 i (Maybe x))) 'NonNullable where
    gUpdateInfo sets _ meta@(M1 (K1 x))
      | isNothing x = UpdateInfo "" [] $ const () >$< noParams
      | otherwise   = UpdateInfo "" [field] enc
      where field = convertField sets $ BS.pack $ selName meta
            enc = choose toEither noParams (param $ nonNullable $ valueEnc @x)
            toEither = maybe (Left ()) Right . unK1 . unM1

instance (Selector s, Valuable x) =>
    GUpdatable (M1 S s (K1 i (Maybe (Maybe x)))) 'Nullable where
    gUpdateInfo sets _ meta@(M1 (K1 x))
      | isNothing x = UpdateInfo "" [] $ const () >$< noParams
      | otherwise   = UpdateInfo "" [field] enc
      where field = convertField sets $ BS.pack $ selName meta
            enc = choose toEither noParams (param $ nullable $ valueEnc @x)
            toEither = maybe (Left ()) Right . unK1 . unM1

instance (Selector s, Valuable x) => GUpdatable (M1 S s (K1 i x)) 'Always where
    gUpdateInfo sets _ meta = UpdateInfo "" [field] enc
      where field = convertField sets $ BS.pack $ selName meta
            enc = unK1 . unM1 >$< (param $ nonNullable $ valueEnc @x)

instance GUpdatable (M1 S s (K1 i x)) 'Ignored where
    gUpdateInfo _ _ _ = UpdateInfo "" [] $ const () >$< noParams

instance (Constructor c, GUpdatable x upd) => GUpdatable (M1 C c x) upd where
      gUpdateInfo sets updPrx meta@(M1 x) = upd { updTable = table, updEncoder = enc }
        where symVal = BS.pack $ conName meta
              table = case setTable sets of
                  Nothing -> toCase (setFieldsCase sets) symVal <> "s"
                  Just t  -> t
              newStripPrefix = toCase CamelCase symVal
              upd = gUpdateInfo sets' updPrx x
              enc = unM1 >$< updEncoder upd
              sets' = case setStripPrefix sets of
                  Just{}  -> sets
                  Nothing -> sets { setStripPrefix = Just newStripPrefix }

instance GUpdatable x upd => GUpdatable (M1 D d x) upd where
    gUpdateInfo sets updPrx (M1 x) = upd { updEncoder = enc }
      where upd = gUpdateInfo sets updPrx x
            enc = unM1 >$< updEncoder upd

data UpdateInfo a = UpdateInfo
    { updTable   :: !ByteString
    , updColumns :: ![ByteString]
    , updEncoder :: !(Params a)
    }

statement :: forall a. UpdateInfo a -> Maybe (Statement (Key a, a) ())
statement upd
  | null (updColumns upd) = Nothing
  | otherwise             = Just $ Statement (builderBytes query) enc dec False
  where query =  "UPDATE " <> bytes (updTable upd)
              <> " SET " <> queryUpds
              <> " WHERE " <> idColumn <> " = $" <> asciiIntegral idVar
        (idVar, queryUpds) = foldl' addColumn (1, "") (updColumns upd)
        idColumn = bytes (updTable upd) <> ".id"
        enc = divide swap (updEncoder upd) (encode @(Key a))
        dec = noResult

        addColumn :: (Int, Builder) -> ByteString -> (Int, Builder)
        addColumn (var, q) col
          | var == 1  = (var + 1, updQ)
          | otherwise = (var + 1, q <> ", " <> updQ)
          where updQ = bytes col <> " = $" <> asciiIntegral var

class ToSettings (xs :: [*]) where
    toSettings :: Proxy xs -> Settings

instance (KnownSymbol t, ToSettings xs) => ToSettings (TableName t ': xs) where
    toSettings _ = (toSettings $ Proxy @xs) { setTable = Just table }
      where table = BS.pack $ symbolVal (Proxy @t)

instance (KnownSymbol p, ToSettings xs) => ToSettings (StripPrefix p ': xs) where
    toSettings _ = (toSettings $ Proxy @xs) { setStripPrefix = Just prefix }
      where prefix = BS.pack $ symbolVal (Proxy @p)

instance ToSettings xs => ToSettings (IgnoreField f ': xs) where
    toSettings _ = toSettings (Proxy @xs)

instance ToSettings xs => ToSettings (CamelCaseFields ': xs) where
    toSettings _ = (toSettings $ Proxy @xs) { setFieldsCase = CamelCase }

instance ToSettings xs => ToSettings (SnakeCaseFields ': xs) where
    toSettings _ = (toSettings $ Proxy @xs) { setFieldsCase = SnakeCase }

instance ToSettings '[] where
    toSettings _ = Settings Nothing SnakeCase Nothing

data Settings = Settings
    { setTable         :: !(Maybe ByteString)
    , setFieldsCase    :: !FieldsCase
    , setStripPrefix   :: !(Maybe ByteString)
    }

data FieldsCase
    = CamelCase
    | SnakeCase

convertField :: Settings -> ByteString -> ByteString
convertField sets field = toCase (setFieldsCase sets) stripped
  where stripped = case setStripPrefix sets of
            Just prefix -> fromMaybe field $ BS.stripPrefix prefix field
            Nothing     -> field

toCase :: FieldsCase -> ByteString -> ByteString
toCase fcase string
  | BS.null string = string
  | first == '_'   = toCase' $ BS.tail string
  | isUpper first  = BS.cons (toLower first) (toCase' $ BS.tail string)
  | otherwise      = toCase' string
  where first = BS.head string
        toCase' = case fcase of
            CamelCase -> toCamelCase
            SnakeCase -> toSnakeCase

        toCamelCase :: ByteString -> ByteString
        toCamelCase str
          | BS.null after = str
          | otherwise     = before <> after'
          where (before, after) = BS.span (/='_') str
                afterTail = BS.tail after
                after' = BS.cons (toUpper $ BS.head afterTail)
                    (toCamelCase $ BS.tail afterTail)

        toSnakeCase :: ByteString -> ByteString
        toSnakeCase str
          | BS.null after = str
          | otherwise     = before <> after'
          where (before, after) = BS.span (\x -> isLower x || x == '_') str
                prefix = BS.pack ['_', toLower $ BS.head after]
                after' = prefix <> (toSnakeCase $ BS.tail after)
