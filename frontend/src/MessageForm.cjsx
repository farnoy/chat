React = require("react")

module.exports  = React.createClass
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
    @props.flux.getActions("messages").createMessage
      channel: @props.activeChannel
      message:
        body: @state.value
    .then => @setState(value: "")

    event.preventDefault()
