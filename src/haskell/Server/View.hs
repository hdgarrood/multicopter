module Server.View (
    Layout,
    View,
    renderPartial,
    renderView,
    view,
    viewWithHead,
) where

import Text.Blaze.Html5 (Html)

-- a Layout takes some extra content to go in the <head> (which may be empty),
-- some content to go in the <body>, and returns a full HTML page.
type Layout = (Html -> Html -> Html)

data View = BasicView Html
          | ViewWithHead Html Html

-- Render the view on its own with no layout.
renderPartial :: View -> Html
renderPartial (BasicView content) = content
renderPartial (ViewWithHead _ content) = content

-- Render a view with a layout.
renderView :: Layout -> View -> Html
renderView l (BasicView content) = l (return ()) content
renderView l (ViewWithHead headContent content) = l headContent content

view :: Html -> View
view = BasicView

viewWithHead :: Html -> Html -> View
viewWithHead = ViewWithHead
