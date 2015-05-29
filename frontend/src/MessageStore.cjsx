Flummox = require("flummox")
Immutable = require("immutable")

module.exports = class MessageStore extends Flummox.Store
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
