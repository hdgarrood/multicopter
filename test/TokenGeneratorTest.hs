module TokenGeneratorTest where

-- Checking the frequency of duplicated characters in a token

import           System.Random
import           TokenGenerator
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)

pairs :: ByteString -> [ByteString]
pairs str
    | B.length str < 2 = []
    | otherwise        =
        let firstTwo = B.take 2 str
            tail     = B.tail str
        in firstTwo : consecutivePairs tail

consecutivePairs :: ByteString -> [ByteString]
consecutivePairs =
    filter (\str -> B.index str 0 == B.index str 1) . pairs

genNTokens :: TokenGenerator -> Int -> [ByteString]
genNTokens tokGen n =
    helper tokGen n []
    where
        helper tokGen 0 toks = toks
        helper tokGen n toks =
            let (tok, tokGen') = nextToken tokGen
            in  helper tokGen' (n-1) $ tok : toks

testDuplicatePairFrequency :: IO ()
testDuplicatePairFrequency = do
    gen <- getStdGen
    let tokGen = TokenGenerator { randomGen = gen }
    let tokensToGenerate = 1000
    let toks = genNTokens tokGen tokensToGenerate

    let actual = sum $ map (length . consecutivePairs) toks
    let expected = tokensToGenerate * tokenLength `div` 26
    putStrLn $ "Expected about this many dupes: " ++ show expected
    putStrLn $ "Got this many: " ++ show actual

