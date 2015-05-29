React = require("react")
Link = require("react-router/lib/components/Link")

module.exports = React.createClass
  render: ->
    <li
        style={if @props.active then @activeStyle}>
        <Link to="channel" params={channel: @props.name}># {@props.name}</Link>
    </li>

  activeStyle:
    fontWeight: "bold"

  onClick: (event) ->
    event.preventDefault()
    @props.flux.getActions("channels").setActive(@props.name)
