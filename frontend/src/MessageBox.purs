module MessageBox (
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

type Props = { messages :: [MessageListItem.Props], activeChannel :: String }

foreign import wrapFlux """
  function wrapFlux(children) {
    return React.createElement(
      require("flummox/component"),
      {connectToStores: "channels"},
      children);
  }
  """ :: forall eff. T.Html eff -> T.Html eff

{-
  TODO later on
  port this logic to purescript

  componentWillUpdate: (nextProps, nextState) ->
    node = React.findDOMNode(@refs.messageContainer)
    @wasScrolled = node.scrollHeight - node.scrollTop - node.offsetHeight < 5

  componentDidUpdate: ->
    if @wasScrolled
      node = React.findDOMNode(@refs.messageContainer)
      node.scrollTop = node.scrollHeight
-}

render :: T.Render _ Unit Props Unit
render _ _ props _ = content
  where
  content :: T.Html _
  content =
    T.div (A.style containerStyle)
      [ T.div (A.style messageContainerStyle)
        [ T.component MessageList.component { messages: props.messages } [] ]
      , T.div (A.style formContainerStyle)
        [ wrapFlux $ T.component MessageForm.component { activeChannel: props.activeChannel } [] ]
      ]

  style = { flex: "1 0", marginBottom: "15px" }

  containerStyle = { flex: 1, display: "flex", flexFlow: "column nowrap",
                     justifyContent: "space-between" }

  messageContainerStyle = { flex: "1 1 200px", overflow: "auto" }

  formContainerStyle = { flex: "0 0 50px", marginTop: "20px" }

spec :: T.Spec _ Unit Props Unit
spec = T.simpleSpec unit performAction render

component = T.createClass spec

performAction :: T.PerformAction _ Unit Props Unit
performAction _ _ = return unit

