module ChannelForm (
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

type State = String

data Action = SetValue String | Identity

foreign import preventDefault """
  function preventDefault(e) {
    return function(ret) {
      e.preventDefault();
      return ret;
    }
  } """ :: forall a b. a -> b -> b

foreign import eventValue """
  function eventValue(e) {
    return e.target.value;
  } """ :: forall a. a -> String

foreign import onSubmitImpl """
  function onSubmitImpl(ctx) {
    return function(ret) {
      return function(ev) {
        ev.preventDefault();
        ctx.props.flux.getActions("channels").createChannel({
          name: ctx.state.value,
          id: Math.random() * 10000
        }).then(function() {
          ctx.setState({value: ""});
        });
        return ret;
      };
    };
  } """ :: forall a. T.Context State Action -> a -> T.FormEvent -> a

render :: T.Render _ State Unit Action
render ctx val _ _ =
    T.form (A.style style <> T.onSubmit ctx (onSubmitImpl ctx Identity))
      [ T.input (
             A.value val
          <> A._type "text"
          <> T.onChange ctx onChange) []
      , T.input (
           A._type "submit"
        <> A.value "Create") []
      ]

  where
  onChange ev = preventDefault ev $ SetValue (eventValue ev)

  item a = T.component MessageListItem.component a []

  style = { display: "flex", flexFlow: "column nowrap" }

spec :: T.Spec _ State Unit Action
spec = T.simpleSpec "" performAction render

component = T.createClass spec

performAction :: T.PerformAction _ State Unit Action
performAction _ (SetValue s) = T.modifyState (const s)
performAction _ Identity = return unit

