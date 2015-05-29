React = require("react")
MessageListItem = require("./MessageListItem")

module.exports = React.createClass
  render: ->
    <ul style={@style}>
      <p>Message Count: {@props.messages.count()}</p>
      {@props.messages.map (message) ->
        <MessageListItem key={message.id} {...message} />
      }
    </ul>

  style:
    padding: 0

