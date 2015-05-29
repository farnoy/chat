React = require("react")
Link = require("react-router/lib/components/Link")

ChannelList = React.createClass
  render: ->
    <ol style={@listStyle}>
      {_.map(@props.channels, (channel) =>
        <ChannelListItem key={channel.id}
                         {...channel}
                         active={channel.name == @props.active} />)
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

  contextTypes:
    flux: React.PropTypes.object.isRequired

  activeStyle:
    fontWeight: "bold"

  onClick: (event) ->
    event.preventDefault()
    @context.flux.getActions("channels").setActive(@props.name)

ChannelForm = React.createClass
  getInitialState: -> {value: ""}

  contextTypes:
    flux: React.PropTypes.object.isRequired

  render: ->
    <form onSubmit={@onSubmit}>
      <input type="text" value={@state.value} onChange={@onChange} />
      <input type="submit" value="Create" />
    </form>

  onChange: (event) ->
    @setState({value: event.target.value})

  onSubmit: (event) ->
    @context.flux.getActions("channels")
      .createChannel(name: @state.value, id: Math.random() * 10000)
    .then (json) => @setState(value: "")

    event.preventDefault()

module.exports = {
  ChannelForm: ChannelForm,
  ChannelListItem: ChannelListItem,
  ChannelList: ChannelList
}
