Flummox = require("flummox")
Immutable = require("immutable")
ChannelActions = require("./ChannelActions")
ChannelStore = require("./ChannelStore")
MessageActions = require("./MessageActions")
MessageStore = require("./MessageStore")
AppActions = require("./AppActions")
AppStore = require("./AppStore")

module.exports = class ChatFlux extends Flummox.Flux
  constructor: ->
    super()

    @createActions("channels", ChannelActions)
    @createActions("messages", MessageActions, @)
    @createActions("app", AppActions, @)

    @createStore("channels", ChannelStore, @)
    @createStore("messages", MessageStore, @)
    @createStore("app", AppStore, @)
