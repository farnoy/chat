React = require("react")
FluxComponent = require("flummox/component")
ChannelList = require("./ChannelList")
ChannelForm = require("./ChannelForm")
MessageList = require("./MessageList")
MessageForm = require("./MessageForm")

module.exports = React.createClass
  render: ->
    <div style={@containerStyle}>
      <div style={@sidebarStyle}>
        <FluxComponent connectToStores={["channels"]}>
          <ChannelList />
        </FluxComponent>
        <ChannelForm />
      </div>

      <div style={@mainStyle}>
        { if @context.flux.getStore("channels").getActiveChannel()
          <p>Channel is: {@props.params.channel}</p>
          <div style={@messagesBoxStyle}>
            <div style={@messageListStyle} ref="scrollBox">
              <FluxComponent connectToStores={["channels", "messages"]}
                             stateGetter={([channelStore, messageStore]) ->
                               messages = messageStore.getMessages(channelStore.getActiveChannel())
                               {messages: messages.slice(-100)}}>
                <MessageList />
              </FluxComponent>
            </div>
            <div style={@messageFormStyle}>
              <FluxComponent connectToStores={"channels"}
                             stateGetter={(channelStore) ->
                               {activeChannel: channelStore.getActiveChannel()} }>
                <MessageForm />
              </FluxComponent>
            </div>
          </div>
        }
      </div>
    </div>

  containerStyle:
    display: "flex"
    height: "100%"
    width: "100%"

  sidebarStyle:
    flex: "0 0 200px"

  mainStyle:
    flex: "1 1 400px"
    display: "flex"

  messagesBoxStyle:
    flex: 1
    display: "flex"
    flexFlow: "column nowrap"
    justifyContent: "space-between"

  messageListStyle:
    flex: "1 1 200px"
    overflow: "auto"

  messageFormStyle:
    flex: "0 0 50px"
    marginTop: "20px"

  getInitialState: -> {websocket: null}

  componentWillUpdate: (nextProps, nextState) ->
    # @shouldScrollTheBox = false
    # if @state?.activeChannel == nextState?.activeChannel
      # prevLength = @state?.messageData[@state?.activeChannel]?.length
      # nextLength = nextState?.messageData[nextState?.activeChannel]?.length

      # if nextLength > prevLength
        # node = React.findDOMNode(@refs.scrollBox)
        # @shouldScrollTheBox = node.scrollHeight - node.scrollTop - node.offsetHeight < 5

  componentDidUpdate: ->
    # if @shouldScrollTheBox
      # node = React.findDOMNode(@refs.scrollBox)
      # node.scrollTop = node.scrollHeight

  componentWillMount: ->
    @state.websocket = new WebSocket("ws://localhost:8082")
    @state.websocket.onmessage = (msg) =>
      payload = JSON.parse(msg.data)
      chan = @context.flux.getStore("channels").getChannelById(payload.channel_id)
      @context.flux.getActions("messages").addMessage(channel: chan.name, message: payload)

    if @props.params?.channel?
      @setActiveChannel(@props.params.channel)

    @reloadChannelData()

  componentWillUnmount: ->
    @state.websocket.close()

  componentWillReceiveProps: (nextProps) ->
    @setActiveChannel(nextProps.params.channel)

  contextTypes:
    flux: React.PropTypes.object.isRequired

  reloadChannelData: ->
    @context.flux.getActions("channels").reloadChannels()

  setActiveChannel: (channel) ->
    @context.flux.getActions("channels").setActive(channel)
    @context.flux.getActions("messages").preload(channel)
