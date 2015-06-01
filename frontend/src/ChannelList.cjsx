React = require("react")
ChannelListItem = require("./ChannelListItem")
FluxComponent = require("flummox/component")

module.exports = React.createClass
  render: ->
    <ol style={@listStyle}>
      {@props.channels.map (channel) =>
        <FluxComponent key={channel.id}>
          <ChannelListItem {...channel}
                           active={channel.name == @props.active} />
        </FluxComponent>
      }
    </ol>

  listStyle:
    listStyleType: "none"
    padding: "10px"
