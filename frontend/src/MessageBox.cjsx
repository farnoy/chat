React = require("react")
FluxComponent = require("flummox/component")
MessageList = require("./MessageList").component
MessageForm = require("./MessageForm")

module.exports = React.createClass
  render: ->
    <div style={@styles.container}>
      <div style={@styles.messageContainer}
           ref="messageContainer">
        <MessageList messages={@props.messages.toJS()} />
      </div>
      <div style={@styles.formContainer}>
        <FluxComponent connectToStores={"channels"}>
          <MessageForm activeChannel={@props.activeChannel} />
        </FluxComponent>
      </div>
    </div>

  styles:
    container:
      flex: 1
      display: "flex"
      flexFlow: "column nowrap"
      justifyContent: "space-between"
    messageContainer:
      flex: "1 1 200px"
      overflow: "auto"
    formContainer:
      flex: "0 0 50px"
      marginTop: "20px"

  componentWillUpdate: (nextProps, nextState) ->
    node = React.findDOMNode(@refs.messageContainer)
    @wasScrolled = node.scrollHeight - node.scrollTop - node.offsetHeight < 5

  componentDidUpdate: ->
    if @wasScrolled
      node = React.findDOMNode(@refs.messageContainer)
      node.scrollTop = node.scrollHeight

