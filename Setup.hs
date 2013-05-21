{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Text.Lazy                  as LT
import Shelly
import Distribution.Simple
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Prelude                         hiding (FilePath)

default (LT.Text)

-- Ensure that the 'coffee' executable is present.
checkForCoffee args flags = do
    shellyNoDir $ errExit False $ silently $ do
        run_ "which" ["coffee"]
        code <- lastExitCode
        if code == 1
            then fail "The CoffeeScript executable `coffee` is required."
            else return ()
    return emptyHookedBuildInfo

-- Compile all the CoffeeScript into JavaScript, and place it into static/, so
-- that Cabal picks it up as data files.
buildCoffee args flags = do
    putStrLn "Compiling CoffeeScript..."
    shellyNoDir $ escaping False $ do
        run_ "coffee" ["--compile",
                       "--output",  "static/from-coffee",
                       "coffee/*.coffee"]
    return emptyHookedBuildInfo

main :: IO ()
main = defaultMainWithHooks $
    simpleUserHooks { preConf  = checkForCoffee
                    , preBuild = buildCoffee
                    }
