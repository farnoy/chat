module ChannelListItem (
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
import Debug.Trace

foreign import link """
  function link(name) {
    return function(children) {
      var Link = require("react-router/lib/components/Link");
      return React.createElement(Link, {to: "channel", params: {channel: name}}, children);
    };
  }
  """ :: forall eff. String -> [T.Html eff] -> T.Html eff

foreign import onDelete """
  function onDelete(ctx) {
    return function(event) {
      event.preventDefault();
      ctx.props.flux.getActions("channels").delete(ctx.props.name);
    };
  }
  """ :: forall ev. T.Context Unit Unit -> ev -> Unit

type Props = { name :: String, active :: Boolean }

render :: T.Render (trace :: Trace) Unit Props Unit
render ctx _ props _ = content
  where
  content :: T.Html _
  content =
    T.li (A.style itemStyle)
      [ link props.name [ T.text "# ", T.text props.name ]
      , T.a (T.onClick ctx (onDelete ctx) <> A.href "#") [ T.text "✗" ]
      ]

  itemStyle = { fontWeight: if props.active then "bold" else "inherit"
              , display: "flex"
              , justifyContent: "space-between"
              }

spec :: T.Spec _ Unit Props Unit
spec = T.simpleSpec unit performAction render

component = T.createClass spec

performAction :: T.PerformAction _ Unit Props Unit
performAction _ _ = return unit

