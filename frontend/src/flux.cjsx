Flummox = require("flummox")
Immutable = require("immutable")

class ChannelActions extends Flummox.Actions
  createChannel: ({name}) ->
    new Promise (resolve, reject) ->
      fetch "/api/channels",
        method: "post",
        headers: {"Content-Type": "application/json"}
        body: JSON.stringify({name: name})
        credentials: "include"
      .then (response) ->
        if response.status >= 200 && response.status < 300
          response.json()
        else
          err = Error("Cannot create the channel")
          err.channel = name
          throw err
      .then (response) =>
        if response.ok
          fetch "/api/channels",
            headers: {"Content-Type": "application/json"}
            credentials: "include"
          .then (response) ->
            if response.status >= 200 && response.status < 300
              response.json()
            else
              err = Error("Cannot fetch the channels")
              err.channel = name
              throw err
          .then (json) ->
            resolve(json)

  setChannels: (channels) -> channels

  setActive: (channel) -> channel

  reloadChannels: ->
    new Promise (resolve, reject) =>
      fetch("/api/channels", credentials: "include").then (response) ->
        response.json()
      .then (json) =>
        resolve(@setChannels(json))

class MessageActions extends Flummox.Actions
  constructor: (@flux) ->
    super()

  preload: (channel) ->
    unless @flux.getStore("messages").hasMessages(channel)
      @loadMessages(channel)

  loadMessages: (channel) ->
    new Promise (resolve, reject) ->
      fetch "/api/channels/#{channel}/messages",
        credentials: "include"
      .then (response) ->
        response.json()
      .then (json) ->
        resolve(channel: channel, messages: json)

  createMessage: ({channel, message}) ->
    new Promise (resolve, reject) ->
      fetch "/api/channels/#{channel}/messages",
        method: "post",
        headers: {"Content-Type": "application/json"}
        body: JSON.stringify(message)
        credentials: "include"
      .then (response) ->
        if response.ok
          resolve()

  addMessage: ({channel, message}) -> {channel, message}

class MessageStore extends Flummox.Store
  constructor: (flux) ->
    super(flux)

    message = flux.getActionIds("messages")

    @registerAsync(message.loadMessages, null, @handleSetMessages)
    @register(message.addMessage, @handleAddMessage)

    @state = {messagesByChannel: Immutable.Map()}

  getMessages: (channel) ->
    @state.messagesByChannel.get(channel, Immutable.List())

  hasMessages: (channel) ->
    @state.messagesByChannel.has(channel)

  handleSetMessages: ({channel, messages}) ->
    messages = Immutable.List(messages).sortBy((chan) -> new Date(chan.timestamp).getTime())
    state = @state.messagesByChannel.set(channel, messages)
    @setState(messagesByChannel: Immutable.Map(state))
    console.log "handled set messages", state

  handleAddMessage: ({channel, message}) ->
    state = @state.messagesByChannel.update(channel, Immutable.List(), (list) -> list.push(message))
    @setState(messagesByChannel: Immutable.Map(state))

class ChannelStore extends Flummox.Store
  constructor: (flux) ->
    super(flux)

    channel = flux.getActionIds("channels")
    @registerAsync(channel.createChannel, @handleNewChannel, @handleSetChannels, @handleFailCreate)
    @register(channel.setChannels, @handleSetChannels)
    @register(channel.setActive, @handleSetActive)

    @state = {channels: Immutable.List(), active: null}

  getChannelById: (id) -> @state.channels.find((c) -> c.id == id)

  getActiveChannel: -> @state.active

  handleNewChannel: (channel) ->
    channel.temporary = true
    @setState(channels: @state.channels.push(channel))

  handleSetChannels: (channels) ->
    @setState(channels: Immutable.List(channels))

  handleFailCreate: (error) ->
    @setState
      channels: @state.channels.filter (chan) ->
        chan.name != error.channel || not chan?.temporary

  handleSetActive: (channel) ->
    @setState(active: channel)

class AppFlux extends Flummox.Flux
  constructor: ->
    super()

    @createActions("channels", ChannelActions)
    @createActions("messages", MessageActions, @)

    @createStore("channels", ChannelStore, @)
    @createStore("messages", MessageStore, @)

module.exports = AppFlux
