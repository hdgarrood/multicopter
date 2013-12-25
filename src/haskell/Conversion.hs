module Conversion (convert) where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Blaze.ByteString.Builder   as Builder

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT

import Data.Char

class ConvertTo a where
    fromIntermediate :: BSL.ByteString -> a

class ConvertFrom a where
    toIntermediate :: a -> BSL.ByteString

convert :: (ConvertFrom a, ConvertTo b) => a -> b
convert = fromIntermediate . toIntermediate

-- Strict bytestrings
strictToLazy :: BS.ByteString -> BSL.ByteString
strictToLazy = BSL.fromChunks . (: [])

lazyToStrict :: BSL.ByteString -> BS.ByteString
lazyToStrict = BS.concat . BSL.toChunks

instance ConvertTo BS.ByteString where
    fromIntermediate = lazyToStrict

instance ConvertFrom BS.ByteString where
    toIntermediate = strictToLazy

-- Lazy bytestrings
instance ConvertTo BSL.ByteString where
    fromIntermediate = id

instance ConvertFrom BSL.ByteString where
    toIntermediate = id

-- Strict text
instance ConvertFrom T.Text where
    toIntermediate = strictToLazy . T.encodeUtf8

instance ConvertTo T.Text where
    fromIntermediate = T.decodeUtf8 . lazyToStrict

-- Lazy text
instance ConvertTo LT.Text where
    fromIntermediate = LT.decodeUtf8

instance ConvertFrom LT.Text where
    toIntermediate = LT.encodeUtf8

-- ByteString Builders
instance ConvertFrom Builder.Builder where
    toIntermediate = Builder.toLazyByteString

-- String
instance ConvertTo String where
    fromIntermediate = map (chr . fromIntegral) . BSL.unpack

instance ConvertFrom String where
    toIntermediate = BSL.pack . map (fromIntegral . ord)
