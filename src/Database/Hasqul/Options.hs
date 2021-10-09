module Database.Hasqul.Options
    ( IgnoreField
    , NoEncodeField
    , EncodeId
    , TableName
    , StripPrefix
    , CamelCaseFields
    , SnakeCaseFields
    ) where

import GHC.TypeLits

-- Field is not encoded and decoded.  Default value is used on decoding
data IgnoreField (a :: Symbol)

-- Field is not encoded, but decoding is performed
data NoEncodeField (a :: Symbol)

-- Id by default is not encoded. If you want to insert with id, set this option
data EncodeId

data TableName (a :: Symbol)

data StripPrefix (a :: Symbol)

data CamelCaseFields

data SnakeCaseFields
