module MessageList (
  Props(),
  component
) where

import Moment
import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Html.Attributes.Unsafe as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

type Props = { messages :: [MessageListItem.Props] }

render :: T.Render _ Unit Props Unit
render _ _ props _ = content
  where
  content :: T.Html _
  content =
    T.div (A.style style)
      (item <$> props.messages)

  item a = T.component MessageListItem.component a []

  style = { display: "flex", flexFlow: "column nowrap", padding: 0 }

spec :: T.Spec _ Unit Props Unit
spec = T.simpleSpec unit performAction render

component = T.createClass spec

performAction :: T.PerformAction _ Unit Props Unit
performAction _ _ = return unit

