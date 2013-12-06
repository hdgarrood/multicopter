module Server.ToMarkupInstances where

import           Text.Blaze.Html5
import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Conversion
import Server.Types
import Server.Player

instance ToMarkup BS.ByteString where
    toMarkup = toMarkup . (convert :: BS.ByteString -> T.Text)

instance ToMarkup PlayerId where
    toMarkup = toMarkup . unPlayerId
