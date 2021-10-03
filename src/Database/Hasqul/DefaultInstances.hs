{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Hasqul.DefaultInstances () where

import Data.ByteString    (ByteString)
import Data.Default.Class
import Data.Text          (Text)

instance Default ByteString where
    def = mempty

instance Default Text where
    def = mempty
