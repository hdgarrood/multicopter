{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Text.Lazy                as LT
import Shelly
import Distribution.Simple
import Distribution.Simple.Configure (configure)
import Prelude                       hiding (FilePath)

default (LT.Text)

-- buildHaskell :: ShIO ()
-- buildHaskell =
--     run_ "ghc" ["--make",   "haskell/Main.hs",
--                 "-o",       "site/multicopter",
--                 "-ihaskell",
--                 "-hidir",   "haskell/interfaces",
--                 "-odir",    "haskell/objs"]


-- Ensure that the 'coffee' executable is present.
checkForCoffee args flags = do
    shellyNoDir $ errExit False $ do
        run_ "which" ["coffee"]
        code <- lastExitCode
        if code == 1
            then fail "The CoffeeScript executable `coffee` is required."
            else return ()
    configure args flags

-- Compile all the CoffeeScript into JavaScript, and place it into static/, so
-- that Cabal picks it up as data files.
-- buildCoffee pkg_descr localbuildinfo hooks flags = do
    -- shellyNoDir $ do
    --     run_ "coffee" ["--compile",
    --                    "--output",  "static/",
    --                    "coffee/*.coffee"]
    -- defaultBuildHook pkg_descr localbuildinfo hooks flags

main :: IO ()
main = defaultMainWithHooks $
    simpleUserHooks { confHook = checkForCoffee
                    -- , preBuild = buildCoffee
                    }
