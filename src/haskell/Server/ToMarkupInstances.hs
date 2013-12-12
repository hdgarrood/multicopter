{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server.ToMarkupInstances where

import           Text.Blaze.Html5
import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Conversion
import Game.Types
import Server.Types

instance ToMarkup BS.ByteString where
    toMarkup = toMarkup . (convert :: BS.ByteString -> T.Text)

instance ToMarkup PlayerId where
    toMarkup = toMarkup . unPlayerId

instance ToMarkup Game where
    toMarkup game = toMarkup $
                        ("Game #" `T.append` (T.pack (show (gameId game))))
