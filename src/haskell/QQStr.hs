module QQStr (str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

str = QuasiQuoter { quoteExp  = stringE
                  -- HACK: None of these is used, it's just to stop GHC giving
                  -- us warnings about uninitialized fields
                  , quotePat  = fail
                  , quoteType = fail
                  , quoteDec  = fail
                  }
    where
        fail = error
            "something went wrong while QuasiQuoting a multiline string"

textE :: T.Text -> ExpQ
textE x =
    -- convert the text to a string literal
    -- and wrap it with T.pack
    appE (varE 'T.pack) $ litE $ StringL $ T.unpack x
