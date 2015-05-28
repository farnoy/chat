React = require("react")
moment = require("moment")

MessageList = React.createClass
  render: ->
    <ul style={@style}>
      {_.map @props.messages, (message) ->
        <MessageListItem key={message.id} {...message} />
      }
    </ul>

  style:
    padding: 0

MessageListItem = React.createClass
  render: ->
    d = new Date(@props.timestamp)
    <li>{@props.author.login}: <code>{moment(d).format("hh:mm")}</code> {@props.body}</li>

MessageForm = React.createClass
  getInitialState: -> {value: ""}

  render: ->
    <form onSubmit={@onSubmit}>
      <input style={@inputStyle} placeholder="Message..." type="text" value={@state.value} onChange={@onChange} />
    </form>

  inputStyle:
    borderRadius: "10px"
    border: "1px solid #ccc"
    padding: "5px"
    width: "100%"

  onChange: (event) ->
    @setState({value: event.target.value})

  onSubmit: (event) ->
    fetch "/api/channels/#{@props.activeChannel}/messages",
      method: "post",
      headers: {"Content-Type": "application/json"}
      body: JSON.stringify({body: @state.value})
      credentials: "include"
    .then (response) =>
      if response.ok
        # before WS we used to fetch this
        # @props.modifiedCallback(@props.activeChannel)
        @setState(value: "")

    event.preventDefault()

module.exports =
  MessageList: MessageList
  MessageListItem: MessageListItem
  MessageForm: MessageForm
