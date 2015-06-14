module ChannelList (
  Channel(),
  Props(),
  component
) where

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Html.Attributes.Unsafe as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

type Channel = { name :: String, id :: Number }
type Props = { channels :: [Channel], active :: String }

foreign import wrapFlux """
  function wrapFlux(id) {
    return function(children) {
      return React.createElement.apply(
        this,
        [
          require("flummox/component"),
          {key: id}
        ].concat(children));
    };
  }
  """ :: forall eff. Number -> [T.Html eff] -> T.Html eff

render :: T.Render _ Unit Props Unit
render ctx _ props _ = content
  where
  content :: T.Html _
  content =
    T.ol (A.style listStyle)
      (item <$> props.channels)

  item c = wrapFlux c.id [ T.component ChannelListItem.component { name: c.name, active: c.name == props.active } [] ]

  listStyle = { listStyleType: "none", padding: "10px" }

spec :: T.Spec _ Unit Props Unit
spec = T.simpleSpec unit performAction render

component = T.createClass spec

performAction :: T.PerformAction _ Unit Props Unit
performAction _ _ = return unit

