module Server.Views where

import           Prelude hiding (div, head, id)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Format as TF
import           Text.Blaze.Html5 hiding (style)
import           Text.Blaze.Html5.Attributes hiding (form, label, content)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as Scotty

import           Server.View
import           Server.Routing
import           Server.Types
import           Game.Types
import           Server.WebM
import           Server.ToMarkupInstances()
import           Conversion()

-- Rendering
render :: View -> ActionT WebM ()
render = renderWithLayout defaultLayout

renderWithLayout :: Layout -> View -> ActionT WebM ()
renderWithLayout l v = Scotty.html $ renderHtml $ renderView l v

defaultLayout :: Layout
defaultLayout headContent content =
    docTypeHtml $ do
        head $ do
            meta ! charset "utf-8"
            headContent
        body $ do
            h1 "multicopter"
            content
            div ! id "alert" ! style "display: none;" $ do
                div ! id "alert-heading" $ ""
                div ! id "alert-details" $ ""

loginNotice :: Text -> View
loginNotice pName = view $
    div $ do
        toMarkup $ TF.format "hooray! you're logged in as {}. " (Only pName)
        a ! href "/registered-players" $ "registered players"

registrationForm :: View
registrationForm = view $
    form ! action "/register" ! method "post" $ do
        input ! type_ "hidden" ! name "back_path" ! value "/"
        label ! for "name" $ "Your name:"
        input ! type_ "text" ! name "name" ! autofocus "autofocus"
        input ! type_ "submit" ! value "Submit"

registrationFormWithError :: Text -> View
registrationFormWithError err = view $ do
    div ! class_ "error" $ toMarkup err
    renderPartial registrationForm

registeredPlayers :: [Player] -> View
registeredPlayers players = view $ do
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

gameNotFound :: View
gameNotFound = view $ do
    h2 "Game not found. soz lol"

startGame :: Game -> View
startGame game = viewWithHead headContent bodyContent
    where
        headContent = script ! type_ "text/javascript" $ toMarkup js
        js = T.unlines $
                [ "$(document).ready(function() {"
                , "  var websocket_url = ["
                , "    'ws://',"
                , "    window.location.host,"
                , TF.format "'{}'," (Only (pathForGame game))
                , "    '?auth_token=',"
                , "    $.cookie('auth_token'),"
                , "  ].join('')"
                , "  startGame(websocket_url)"
                , "})"
                ]
        bodyContent = do
            button ! id "start-game" $ "Start the game"
            div ! id "canvas-container" $ ""
