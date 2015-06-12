module MessageForm (
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

type State = String

data Action = SetValue String

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
        ctx.props.flux.getActions("messages").createMessage({
          channel: ctx.props.activeChannel,
          message: {
            body: ctx.state.value
          }
        }).then(function() {
          ctx.setState({value: ""});
        });
        return ret;
      };
    };
  } """ :: forall a. T.Context State Action -> a -> T.FormEvent -> a

render :: T.Render _ State Unit Action
render ctx val _ _ =
    T.form (T.onSubmit ctx (onSubmitImpl ctx (SetValue "")))
      [ T.input (
             A.value val
          <> A.placeholder "Message..."
          <> A._type "text"
          <> T.onChange ctx onChange
          <> A.style style) []
      ]

  where
  onChange ev = preventDefault ev $ SetValue (eventValue ev)

  item a = T.component MessageListItem.component a []

  style = { borderRadius: "10px", border: "1px solid #ccc", padding: "5px", width: "100%" }

spec :: T.Spec _ State Unit Action
spec = T.simpleSpec "" performAction render

component = T.createClass spec

performAction :: T.PerformAction _ State Unit Action
performAction _ (SetValue s) = T.modifyState (const s)

