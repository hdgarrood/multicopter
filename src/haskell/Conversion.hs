module Conversion (convert) where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Blaze.ByteString.Builder   as Builder

import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT

class ConvertibleTo a where
    toIntermediate :: a -> BSL.ByteString

class ConvertibleFrom a where
    fromIntermediate :: BSL.ByteString -> a

convert :: (ConvertibleFrom b, ConvertibleTo a) => a -> b
convert = fromIntermediate . toIntermediate

-- Strict bytestrings
strictToLazy :: BS.ByteString -> BSL.ByteString
strictToLazy = BSL.fromChunks . (: [])

lazyToStrict :: BSL.ByteString -> BS.ByteString
lazyToStrict = BS.concat . BSL.toChunks

instance ConvertibleTo BS.ByteString where
    toIntermediate = strictToLazy

instance ConvertibleFrom BS.ByteString where
    fromIntermediate = lazyToStrict

-- Lazy bytestrings
instance ConvertibleTo BSL.ByteString where
    toIntermediate = id

instance ConvertibleFrom BSL.ByteString where
    fromIntermediate = id

-- Lazy text
instance ConvertibleTo LT.Text where
    toIntermediate = LT.encodeUtf8

instance ConvertibleFrom LT.Text where
    fromIntermediate = LT.decodeUtf8

-- ByteString Builders
instance ConvertibleTo Builder.Builder where
    toIntermediate = Builder.toLazyByteString
