Flummox = require("flummox")
Immutable = require("immutable")

module.exports = class ChannelStore extends Flummox.Store
  constructor: (flux) ->
    super(flux)

    channel = flux.getActionIds("channels")
    @registerAsync(channel.createChannel, @handleNewChannel, @handleSetChannels, @handleFailCreate)
    @register(channel.setChannels, @handleSetChannels)
    @registerAsync(channel.delete, null, @handleRemoveChannel)

    @state = {channels: Immutable.List()}

  getChannelById: (id) -> @state.channels.find((c) -> c.id == id)

  handleNewChannel: (channel) ->
    channel.temporary = true
    @setState(channels: @state.channels.push(channel))

  handleSetChannels: (channels) ->
    @setState(channels: Immutable.List(channels))

  handleFailCreate: (error) ->
    @setState
      channels: @state.channels.filter (chan) ->
        chan.name != error.channel || not chan?.temporary

  handleRemoveChannel: (channel) ->
    @setState
      channels: @state.channels.filter (chan) ->
        chan.name != channel
