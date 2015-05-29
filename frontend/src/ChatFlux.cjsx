Flummox = require("flummox")
Immutable = require("immutable")
ChannelActions = require("./ChannelActions")
ChannelStore = require("./ChannelStore")
MessageActions = require("./MessageActions")
MessageStore = require("./MessageStore")

module.exports = class ChatFlux extends Flummox.Flux
  constructor: ->
    super()

    @createActions("channels", ChannelActions)
    @createActions("messages", MessageActions, @)

    @createStore("channels", ChannelStore, @)
    @createStore("messages", MessageStore, @)
