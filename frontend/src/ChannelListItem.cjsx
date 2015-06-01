React = require("react")
Link = require("react-router/lib/components/Link")

module.exports = React.createClass
  render: ->
    <li
        style={@styles().itemStyle}>
        <Link to="channel" params={channel: @props.name}># {@props.name}</Link>
        <a onClick={@onDelete} href="#">&#x2717;</a>
    </li>

  styles: ->
    itemStyle:
      fontWeight: if @props.active then "bold" else "inherit"
      display: "flex"
      justifyContent: "space-between"

  onClick: (event) ->
    event.preventDefault()
    @props.flux.getActions("channels").setActive(@props.name)

  onDelete: (event) ->
    @props.flux.getActions("channels").delete(@props.name)
    event.preventDefault()
