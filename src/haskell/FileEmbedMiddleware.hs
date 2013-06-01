{-# LANGUAGE OverloadedStrings #-}

module FileEmbedMiddleware (fileEmbed) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Conduit (ResourceT)
import System.FilePath

import Network.Wai
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (fromByteString)

fileEmbed :: [(FilePath, B.ByteString)] -> Middleware
fileEmbed embeddedFiles app req =
    case getEmbeddedFile embeddedFiles (pathInfo req) of
        Just x  -> respond x
        Nothing -> app req

respond :: B.ByteString -> ResourceT IO Response
respond bs =
    return $
        ResponseBuilder
            status200
            [("Content-Type", "text/plain")] 
            (fromByteString bs)

getEmbeddedFile ::
    [(FilePath, B.ByteString)] ->
    [T.Text] ->
    Maybe B.ByteString
getEmbeddedFile embeddedFiles pathInfo =
    getEmbedPath pathInfo >>= (\p -> lookup p embeddedFiles)

getEmbedPath :: [T.Text] -> Maybe String
getEmbedPath pathInfo =
    case pathInfo of
        "static":xs -> Just $ T.unpack $ T.intercalate "/" xs
        _           -> Nothing
