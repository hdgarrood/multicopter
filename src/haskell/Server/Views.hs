module Server.Views where

import           Prelude hiding (div)
import           Data.Text.Lazy (Text)
import           Data.Text.Format as TF
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text
import qualified Web.Scotty.Trans (ActionT, html)

import           Server.WebM
import           Server.Player
import           Server.ToMarkupInstances()
import           Conversion()

withinMainLayout :: Html -> Html
withinMainLayout content = docTypeHtml $ do
    H.head $ do
        meta ! A.charset "utf-8"
    body $ do
        h1 "multicopter"
        content
        H.div ! A.id "alert" ! A.style "display: none;" $ do
            H.div ! A.id "alert-heading" $ ""
            H.div ! A.id "alert-details" $ ""

render :: Html -> Web.Scotty.Trans.ActionT WebM ()
render = Web.Scotty.Trans.html . renderHtml . withinMainLayout

loginNotice :: Text -> Html
loginNotice name = div $ do
    toMarkup $ (TF.format "hooray! you're logged in as {}. " (Only name))
    a ! A.href "/registered-players" $ "registered players"

registrationForm :: Html
registrationForm =
    form ! A.action "/register" ! A.method "post" $ do
        input ! A.type_ "hidden" ! A.name "back_path" ! A.value "/"
        label ! A.for "name" $ "Your name:"
        input ! A.type_ "text" ! A.name "name" ! A.autofocus "autofocus"
        input ! A.type_ "submit" ! A.value "Submit"

registrationFormWithError :: Text -> Html
registrationFormWithError err = do
    div ! A.class_ "error" $ toMarkup err
    registrationForm

registeredPlayers :: [Player] -> Html
registeredPlayers players = do
    h2 "Registered players:"
    table $ do
        thead $ do
            tr $ do
                th "id"
                th "name"
                th "token"
        tbody $ mapM_ (\pl ->
            tr $ do
                td' $ playerId pl
                td' $ playerName pl
                td' $ playerToken pl) players
    where
        td' :: ToMarkup a => a -> Html
        td' = td . toHtml
