React = require("react")
ChannelListItem = require("./ChannelListItem")

module.exports = React.createClass
  render: ->
    <ol style={@listStyle}>
      {@props.channels.map (channel) =>
        <ChannelListItem key={channel.id}
                         {...channel}
                         active={channel.name == @props.active} />
      }
    </ol>

  listStyle:
    listStyleType: "none"
    padding: "10px"
