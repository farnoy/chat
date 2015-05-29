React = require("react")

module.exports = React.createClass
  getInitialState: -> {value: ""}

  render: ->
    <form onSubmit={@onSubmit}>
      <input type="text" value={@state.value} onChange={@onChange} />
      <input type="submit" value="Create" />
    </form>

  onChange: (event) ->
    @setState({value: event.target.value})

  onSubmit: (event) ->
    @props.flux.getActions("channels")
      .createChannel(name: @state.value, id: Math.random() * 10000)
    .then (json) => @setState(value: "")

    event.preventDefault()
