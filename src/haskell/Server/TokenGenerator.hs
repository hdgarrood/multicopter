module Server.TokenGenerator where

import           System.Random
import           Control.Monad
import           Control.Monad.Random
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (singleton)

import Server.Types

-- arbitrarily chosen
tokenLength :: Int
tokenLength = 50

mkTokenGenerator :: Int -> TokenGenerator
mkTokenGenerator = TokenGenerator . mkStdGen

getTokenGenerator :: Rand StdGen TokenGenerator
getTokenGenerator = fmap TokenGenerator getSplit

nextToken :: TokenGenerator -> (ByteString, TokenGenerator)
nextToken (TokenGenerator gen) =
    let (token, gen') = runRand generateToken gen
    in  (token, TokenGenerator gen')

generateToken :: Rand StdGen ByteString
generateToken = do
    chars <- replicateM tokenLength generateTokenChar
    return $ foldl1 B.append chars

generateTokenChar :: Rand StdGen ByteString
generateTokenChar =
    fmap singleton $ getRandomR ('a', 'z')
