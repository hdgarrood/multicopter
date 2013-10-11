module Views where

import           Data.Text.Lazy (Text)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text
import qualified Web.Scotty.Trans (ActionT, html)

import           WebM

data View = RegistrationForm

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

renderViewToHtml :: View -> Html
renderViewToHtml RegistrationForm = withinMainLayout registrationForm

renderViewToText :: View -> Text
renderViewToText = renderHtml . renderViewToHtml

render :: View -> Web.Scotty.Trans.ActionT WebM ()
render = Web.Scotty.Trans.html . renderViewToText

registrationForm :: Html
registrationForm =
    form ! A.action "/register" ! A.method "post" $ do
        input ! A.type_ "hidden" ! A.name "back_path" ! A.value "/"
        label ! A.for "name" $ "Your name:"
        input ! A.type_ "text" ! A.name "name" ! A.autofocus "autofocus"
        input ! A.type_ "submit" ! A.value "Submit"
