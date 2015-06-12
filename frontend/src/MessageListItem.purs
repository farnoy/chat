module MessageListItem (
  Author(),
  Props(),
  component
) where

import Data.Date
import Data.Maybe
import Moment
import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Html.Attributes.Unsafe as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

type Author = { login :: String }
type Props = { body :: String, author :: Author,  timestamp :: String}

render :: T.Render _ Unit Props Unit
render _ _ props _ = content
  where
  content :: T.Html _
  content =
    T.div (A.style style)
      [ T.div' [
          T.strong' [T.text props.author.login]
        , T.text " - "
        , T.code' [T.text $ moment props.timestamp "hh:mm"]
        ]
      , T.div' [T.text props.body]
      ]

  style = { flex: "1 0", marginBottom: "15px" }

spec :: T.Spec _ Unit Props Unit
spec = T.simpleSpec unit performAction render

component = T.createClass spec

performAction :: T.PerformAction _ Unit Props Unit
performAction _ _ = return unit

