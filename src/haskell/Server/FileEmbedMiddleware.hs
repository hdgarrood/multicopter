module Server.FileEmbedMiddleware where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Map as M
import System.FilePath

import Network.Wai
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (fromByteString)

fileEmbed :: [(FilePath, B.ByteString)] -> Middleware
fileEmbed embeddedFiles app req =
    case getEmbeddedFile embeddedFiles (pathInfo req) of
        Just x  -> respond x
        Nothing -> app req

respond :: (B.ByteString, B.ByteString) -> IO Response
respond (contents, mimeType) =
    return $
        responseBuilder
            status200
            [("Content-Type", mimeType)] 
            (fromByteString contents)

-- Given a list of embedded files, and a request's pathinfo, return a
-- (contents, mimeType) pair, or Nothing
getEmbeddedFile ::
    [(FilePath, B.ByteString)] ->
    [T.Text] ->
    Maybe (B.ByteString, B.ByteString)
getEmbeddedFile embeddedFiles path = do
    contents <- getEmbedPath path >>= (\p -> lookup p embeddedFiles)
    let mimeType = getMime $ T.unpack $ last path
    return (contents, mimeType)

getEmbedPath :: [T.Text] -> Maybe String
getEmbedPath path =
    case path of
        "static":xs -> Just $ T.unpack $ T.intercalate "/" xs
        _           -> Nothing

getMime :: FilePath -> B.ByteString
getMime fp =
    let (_, ext) = splitExtension fp
    in  M.findWithDefault defaultMimeType ext mimeMap

defaultMimeType :: B.ByteString
defaultMimeType = "application/octet-stream"
    
-- A mapping of file extensions to MIME types
mimeMap :: M.Map String B.ByteString
mimeMap = M.fromList $
    [ (".html", "text/html")
    , (".js", "text/javascript")
    , (".css", "text/css")
    ]
