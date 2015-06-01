React = require("react")
MessageListItem = require("./MessageListItem")

module.exports = React.createClass
  render: ->
    <div style={@style}>
      {@props.messages.map (message) ->
        <MessageListItem key={message.id} {...message} />
      }
    </div>

  style:
    padding: 0
    display: "flex"
    flexFlow: "column nowrap"
