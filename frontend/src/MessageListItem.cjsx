React = require("react")
moment = require("moment")

module.exports = React.createClass
  render: ->
    d = new Date(@props.timestamp)
    <div style={@styles.container}>
      <div>
        <strong>{@props.author.login}</strong> - <code>{moment(d).format("hh:mm")}</code>
      </div>
      <div>{@props.body}</div>
    </div>

  styles:
    container:
      flex: "1 0"
      marginBottom: "15px"
