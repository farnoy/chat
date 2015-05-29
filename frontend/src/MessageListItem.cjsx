React = require("react")
moment = require("moment")

module.exports = React.createClass
  render: ->
    d = new Date(@props.timestamp)
    <li>{@props.author.login}: <code>{moment(d).format("hh:mm")}</code> {@props.body}</li>
