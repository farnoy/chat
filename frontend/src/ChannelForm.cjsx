React = require("react")

module.exports = React.createClass
  getInitialState: -> {value: ""}

  render: ->
    <form onSubmit={@onSubmit} style={@styles().form}>
      <input type="text" value={@state.value} onChange={@onChange} />
      <input type="submit" value="Create" />
    </form>

  styles: ->
    form:
      display: "flex"
      flexFlow: "column nowrap"

  onChange: (event) ->
    @setState({value: event.target.value})

  onSubmit: (event) ->
    @props.flux.getActions("channels")
      .createChannel(name: @state.value, id: Math.random() * 10000)
    .then (json) => @setState(value: "")

    event.preventDefault()
