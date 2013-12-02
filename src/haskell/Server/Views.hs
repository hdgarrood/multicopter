module Server.Views where

import           Data.Text.Lazy (Text)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text
import qualified Web.Scotty.Trans (ActionT, html)

import           Server.WebM
import           Server.Player
import           Server.ToMarkupInstances
import           Conversion

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
render = Web.Scotty.Trans.html . renderHtml

registrationForm :: Html
registrationForm = withinMainLayout $
    form ! A.action "/register" ! A.method "post" $ do
        input ! A.type_ "hidden" ! A.name "back_path" ! A.value "/"
        label ! A.for "name" $ "Your name:"
        input ! A.type_ "text" ! A.name "name" ! A.autofocus "autofocus"
        input ! A.type_ "submit" ! A.value "Submit"

registeredPlayers :: [Player] -> Html
registeredPlayers players = withinMainLayout $ do
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
