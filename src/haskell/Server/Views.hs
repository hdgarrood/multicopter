module Server.Views where

import           Prelude hiding (div, head, id)
import           Control.Monad
import           Data.Text.Lazy (Text)
import           Data.Text.Format as TF
import           Text.Blaze.Html5 hiding (style)
import           Text.Blaze.Html5.Attributes hiding (form, label, content, title)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as Scotty

import           Server.View
import           Server.Routing
import           Server.Types
import           Game.Types
import           Game.Constants
import           Server.ToMarkupInstances()
import           Conversion()

-- Rendering
render :: View -> Action' ()
render = renderWithLayout defaultLayout

renderWithLayout :: Layout -> View -> ActionT Text WebM ()
renderWithLayout l v = Scotty.html $ renderHtml $ renderView l v

defaultLayout :: Layout
defaultLayout headContent content =
    docTypeHtml $ do
        head $ do
            meta ! charset "utf-8"
            scriptTag "/static/jquery-1.10.2.js"
            scriptTag "/static/underscore.js"
            styleTag "/static/style.css"
            title "multicopter"
            headContent
        body $ do
            h1 "multicopter"
            content
            div ! id "alert" ! style "display: none;" $ do
                div ! id "alert-heading" $ ""
                div ! id "alert-details" $ ""

homePage :: Text -> [Game] -> View
homePage pName games = view $ do
    div $ do
        toMarkup $ TF.format "hooray! you're logged in as {}. " (Only pName)
        a ! href "/registered-players" $ "registered players"
    renderPartial createGameButton
    renderPartial (gameList games)

createGameButton :: View
createGameButton = view $
    form ! action "/games" ! method "post" $ do
        input ! type_ "submit" ! value "Start a new game"

gameList :: [Game] -> View
gameList games = view $ do
    h2 "games"
    ul $ do
        forM_ games $ \game ->
            li $
                a ! href (toValue $ pathForGame game) $ toMarkup game

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
        headContent = do
            scriptTag "/static/game.js"

        bodyContent = do
            button ! id "start-game" $ "Start the game"
            div ! id "canvas-container" ! dataAttribute "websocket-path" webSocketPath $
                canvas ! id "canvas" ! width (toValue c_worldWidth) ! height (toValue c_worldHeight) $ ""

        webSocketPath = toValue . wsPathForGame $ game

scriptTag :: AttributeValue -> Html
scriptTag val = script ! type_ "text/javascript" ! src val $ ""

styleTag :: AttributeValue -> Html
styleTag val = link ! rel "stylesheet" ! type_ "text/css" ! href val
