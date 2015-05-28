React = require("react")
Link = require("react-router/lib/components/Link")

ChannelList = React.createClass
  render: ->
    <ol style={@listStyle}>
      {_.map(@props.channels, (channel) =>
        <ChannelListItem key={channel.id}
                         name={channel.name}
                         active={channel.name == @props.active}
                         activate={@props.activate} />)
      }
    </ol>

  listStyle:
    listStyleType: "none"
    padding: "10px"

ChannelListItem = React.createClass
  render: ->
    <li
        style={if @props.active then @activeStyle}>
        <Link to="channel" params={channel: @props.name}># {@props.name}</Link>
    </li>

  activeStyle:
    fontWeight: "bold"

  onClick: (event) ->
    event.preventDefault()
    @props.activate(@props.name)

ChannelForm = React.createClass
  getInitialState: -> {value: ""}

  render: ->
    <form onSubmit={@onSubmit}>
      <input type="text" value={@state.value} onChange={@onChange} />
      <input type="submit" value="Create" />
    </form>

  onChange: (event) ->
    @setState({value: event.target.value})

  onSubmit: (event) ->
    fetch "/api/channels",
      method: "post",
      headers: {"Content-Type": "application/json"}
      body: JSON.stringify({name: @state.value})
      credentials: "include"
    .then (response) =>
      if response.ok
        @props.modifiedCallback()

    event.preventDefault()

module.exports = {
  ChannelForm: ChannelForm,
  ChannelListItem: ChannelListItem,
  ChannelList: ChannelList
}
