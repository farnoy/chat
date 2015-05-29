React = require("react")
MessageListItem = require("./MessageListItem")

module.exports = React.createClass
  render: ->
    <div style={@style}>
      <p>Message Count: {@props.messages.count()}</p>
      {@props.messages.map (message) ->
        <MessageListItem key={message.id} {...message} />
      }
    </div>

  style:
    padding: 0
    display: "flex"
    flexFlow: "column nowrap"
