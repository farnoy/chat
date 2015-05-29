Flummox = require("flummox")
Immutable = require("immutable")

module.exports = class ChannelStore extends Flummox.Store
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
