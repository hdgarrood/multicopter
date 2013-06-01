{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import QQStr
import qualified Data.Text as T

multiline :: T.Text
multiline = [str|hello!
I'm a multiline string!
|]

main = putStr multiline
