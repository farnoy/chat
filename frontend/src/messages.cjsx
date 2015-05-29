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

  contextTypes:
    flux: React.PropTypes.object.isRequired

  inputStyle:
    borderRadius: "10px"
    border: "1px solid #ccc"
    padding: "5px"
    width: "100%"

  onChange: (event) ->
    @setState({value: event.target.value})

  onSubmit: (event) ->
    @context.flux.getActions("messages").createMessage
      channel: @props.activeChannel
      message:
        body: @state.value
    .then => @setState(value: "")

    event.preventDefault()

module.exports =
  MessageList: MessageList
  MessageListItem: MessageListItem
  MessageForm: MessageForm
