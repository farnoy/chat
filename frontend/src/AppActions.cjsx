Flummox = require("flummox")
Promise = require("es6-promise").Promise

module.exports = class AppActions extends Flummox.Actions
  constructor: (@flux) -> super()

  setupWebSocket: ->
    websocket = new WebSocket("ws://localhost:8082")
    websocket.onmessage = (msg) =>
      payload = JSON.parse(msg.data)
      chan = @flux.getStore("channels").getChannelById(payload.channel_id)
      @flux.getActions("messages").addMessage(channel: chan.name, message: payload)
    websocket

  setActiveChannel: (channel) ->
    channel
