module SafeStaticDataFileMiddleware (safeStaticDataFiles) where

import System.Directory
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)

import Network.Wai
import Network.Wai.Middleware.Static
import Network.HTTP.Types               (status200)

import Paths_multicopter                (getDataFileName)

-- | Serve static data files (from Cabal)
safeStaticDataFiles :: Middleware
safeStaticDataFiles app req =
    let p = noDots
    in  maybe (app req)
              (\fp -> do datafp <- liftIO $ getDataFileName fp
                         exists <- liftIO $ doesFileExist datafp
                         if exists
                             then return $ ResponseFile status200
                                           [("Content-Type", getMimeType fp)]
                                           fp
                                           Nothing
                             else app req)
              (tryPolicy p $ T.unpack $ T.intercalate "/" $ pathInfo req)

type Ascii = B.ByteString

getMimeType :: FilePath -> Ascii
getMimeType = go . extensions
    where go [] = defaultMimeType
          go (ext:exts) = fromMaybe (go exts) $ M.lookup ext defaultMimeTypes

extensions :: FilePath -> [String]
extensions [] = []
extensions fp = case dropWhile (/= '.') fp of
                    [] -> []
                    s -> let ext = tail s
                         in ext : extensions ext

defaultMimeType :: Ascii
defaultMimeType = "application/octet-stream"

-- This list taken from snap-core's Snap.Util.FileServe
defaultMimeTypes :: M.Map String Ascii
defaultMimeTypes = M.fromList [
  ( "asc"     , "text/plain"                        ),
  ( "asf"     , "video/x-ms-asf"                    ),
  ( "asx"     , "video/x-ms-asf"                    ),
  ( "avi"     , "video/x-msvideo"                   ),
  ( "bz2"     , "application/x-bzip"                ),
  ( "c"       , "text/plain"                        ),
  ( "class"   , "application/octet-stream"          ),
  ( "conf"    , "text/plain"                        ),
  ( "cpp"     , "text/plain"                        ),
  ( "css"     , "text/css"                          ),
  ( "cxx"     , "text/plain"                        ),
  ( "dtd"     , "text/xml"                          ),
  ( "dvi"     , "application/x-dvi"                 ),
  ( "gif"     , "image/gif"                         ),
  ( "gz"      , "application/x-gzip"                ),
  ( "hs"      , "text/plain"                        ),
  ( "htm"     , "text/html"                         ),
  ( "html"    , "text/html"                         ),
  ( "jar"     , "application/x-java-archive"        ),
  ( "jpeg"    , "image/jpeg"                        ),
  ( "jpg"     , "image/jpeg"                        ),
  ( "js"      , "text/javascript"                   ),
  ( "json"    , "application/json"                  ),
  ( "log"     , "text/plain"                        ),
  ( "m3u"     , "audio/x-mpegurl"                   ),
  ( "mov"     , "video/quicktime"                   ),
  ( "mp3"     , "audio/mpeg"                        ),
  ( "mpeg"    , "video/mpeg"                        ),
  ( "mpg"     , "video/mpeg"                        ),
  ( "ogg"     , "application/ogg"                   ),
  ( "pac"     , "application/x-ns-proxy-autoconfig" ),
  ( "pdf"     , "application/pdf"                   ),
  ( "png"     , "image/png"                         ),
  ( "ps"      , "application/postscript"            ),
  ( "qt"      , "video/quicktime"                   ),
  ( "sig"     , "application/pgp-signature"         ),
  ( "spl"     , "application/futuresplash"          ),
  ( "svg"     , "image/svg+xml"                     ),
  ( "swf"     , "application/x-shockwave-flash"     ),
  ( "tar"     , "application/x-tar"                 ),
  ( "tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( "tar.gz"  , "application/x-tgz"                 ),
  ( "tbz"     , "application/x-bzip-compressed-tar" ),
  ( "text"    , "text/plain"                        ),
  ( "tgz"     , "application/x-tgz"                 ),
  ( "torrent" , "application/x-bittorrent"          ),
  ( "ttf"     , "application/x-font-truetype"       ),
  ( "txt"     , "text/plain"                        ),
  ( "wav"     , "audio/x-wav"                       ),
  ( "wax"     , "audio/x-ms-wax"                    ),
  ( "wma"     , "audio/x-ms-wma"                    ),
  ( "wmv"     , "video/x-ms-wmv"                    ),
  ( "xbm"     , "image/x-xbitmap"                   ),
  ( "xml"     , "text/xml"                          ),
  ( "xpm"     , "image/x-xpixmap"                   ),
  ( "xwd"     , "image/x-xwindowdump"               ),
  ( "zip"     , "application/zip"                   ) ]
