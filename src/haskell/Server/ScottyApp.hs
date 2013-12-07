module Server.ScottyApp where

import Control.Concurrent.STM
import Control.Monad (join)
import Data.Maybe
import Data.FileEmbed
import Data.Text.Lazy
import qualified Data.ByteString as BS
import Web.Scotty.Trans
import Web.Cookie
import Network.Wai
import Network.HTTP.Types

import Server.Types
import Server.FileEmbedMiddleware
import Game.Types
import Server.PlayerRepository
import Server.GameRepository
import Server.Views
import Server.WebM
import Server.Routing
import Conversion

makeCookie :: Text -> Text -> SetCookie
makeCookie n v = def { setCookieName = n', setCookieValue = v' }
    where
        n' = convert n
        v' = convert v

renderSetCookie' :: SetCookie -> Text
renderSetCookie' = convert . renderSetCookie

setCookie :: Text -> Text -> ActionT WebM ()
setCookie n v = setHeader "Set-Cookie" (renderSetCookie' (makeCookie n v))

ensureAuthenticated :: ActionT WebM ()
ensureAuthenticated = do
    authed <- isAuthenticated
    if authed
        then next
        else do
            path <- param "path" :: ActionT WebM Text
            case path of
                "/register" -> handleRegistration
                _           -> redirect "/register"

getAuthToken :: ActionT WebM (Maybe BS.ByteString)
getAuthToken = do
    cookieHeader <- reqHeader "Cookie"
    return $ extract cookieHeader
    where
        extract :: Maybe Text -> Maybe BS.ByteString
        extract = join . fmap (lookup "auth_token" . parseCookies . convert)

getCurrentPlayer :: ActionT WebM (Maybe Player)
getCurrentPlayer = do
    maybeToken <- getAuthToken
    case maybeToken of
        Nothing    -> return Nothing
        Just token -> webM $ gets (getPlayerBy (Token token) . serverPlayers)

-- The call to 'fromJust' is mostly acceptable because any code that calls this
-- will only be executed after we've checked that someone's logged in.
getCurrentPlayer' :: ActionT WebM Player
getCurrentPlayer' = fmap fromJust getCurrentPlayer

-- TODO: Retrieve the player associated with an auth token once per request and
-- save the result somewhere.
isAuthenticated :: ActionT WebM Bool
isAuthenticated = fmap isJust getCurrentPlayer

handleRegistration :: ActionT WebM ()
handleRegistration = do
    req <- request
    case requestMethod req of
        "GET"  -> render registrationForm
        "POST" -> signUp `rescue` (\_ -> redirect "/register")
        _      -> status methodNotAllowed405
    where
        signUp = do
            name <- param "name"
            result <- webM $ modifyPlayersWith (addPlayer name)
            case result of
                Left err     -> render $ registrationFormWithError err
                Right player -> do
                    setCookie "auth_token" (convert $ playerToken player)
                    redirect "/"

-- Specify something that should always happen before each request. The given
-- ActionT WebM() should probably contain a 'next' so that the rest of the
-- application can be reached.
beforehand :: ActionT WebM () -> ScottyT WebM ()
beforehand = matchAny (function
    (\req -> Just [("path", convert $ rawPathInfo req)]))

startScottyApp :: TVar ServerState -> IO ()
startScottyApp tvar =
    scottyWebM 3000 tvar $ do
        middleware $ fileEmbed $(embedDir "src/static")

        beforehand ensureAuthenticated

        get "/register" $ do
            -- by this point, we've already logged in
            redirect "/"

        get "/" $ do
            player <- getCurrentPlayer'
            games  <- webM $ gets (getAllGames' . serverGames)
            render $ homePage (playerName player) games

        get "/registered-players" $ do
            players <- webM $ gets (getAllPlayers . serverPlayers)
            render (registeredPlayers players)

        get "/games/:id" $ do
            gId  <- fmap GameId $ param "id"
            game <- webM $ gets (getGameById gId . serverGames)
            case game of
                Just (x,_) -> render $ startGame x
                Nothing    -> do status notFound404
                                 render gameNotFound

        post "/games" $ do
            game <- webM $ modifyGamesWith addGame
            redirect (pathForGame game)
