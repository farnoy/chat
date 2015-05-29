Flummox = require("flummox")

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

    @state = {messagesByChannel: {}}

  getMessages: (channel) ->
    @state.messagesByChannel[channel] || []

  hasMessages: (channel) ->
    @state.messagesByChannel[channel]?

  handleSetMessages: ({channel, messages}) ->
    state = _.cloneDeep(@state.messagesByChannel)
    state[channel] = messages
    @setState(messagesByChannel: state)

  handleAddMessage: ({channel, message}) ->
    state = _.cloneDeep(@state.messagesByChannel)
    state[channel] ||= []
    state[channel].push(message)
    @setState(messagesByChannel: state)
    console.log "handled add message", state


class ChannelStore extends Flummox.Store
  constructor: (flux) ->
    super(flux)

    channel = flux.getActionIds("channels")
    @registerAsync(channel.createChannel, @handleNewChannel, @handleSetChannels, @handleFailCreate)
    @register(channel.setChannels, @handleSetChannels)
    @register(channel.setActive, @handleSetActive)

    @state = {channels: [], active: null}

  getChannelById: (id) -> _.find(@state.channels, "id", id)

  getActiveChannel: -> @state.active

  handleNewChannel: (channel) ->
    @setState(channels: @state.channels.concat([_.assign(channel, {temporary: true})]))

  handleSetChannels: (channels) ->
    @setState(channels: channels)

  handleFailCreate: (error) ->
    @setState
      channels: _.filter @state.channels, (chan) ->
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
