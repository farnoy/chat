React = require("react")
FluxComponent = require("flummox/component")
ChannelList = require("./ChannelList").component
ChannelForm = require("./ChannelForm")
MessageBox = require("./MessageBox").component

module.exports = React.createClass
  render: ->
    <div style={@containerStyle}>
      <div style={@sidebarStyle}>
        <FluxComponent connectToStores={"channels"} stateGetter={(channelStore) =>
              {channels: channelStore.getChannels().toJS()}}>
          <ChannelList active={@state.activeChannel} />
        </FluxComponent>

        <FluxComponent>
          <ChannelForm />
        </FluxComponent>
      </div>

      <div style={@mainStyle}>
        { if @state.activeChannel
          <p>Channel is: {@state.activeChannel}</p>
          <FluxComponent connectToStores={"messages"}
                         stateGetter={(messageStore) =>
                   messages = messageStore.getMessages(@state.activeChannel)
                   {messages: messages.slice(-100).toJS()}}>
            <MessageBox activeChannel={@state.activeChannel} />
          </FluxComponent>
        }
      </div>
    </div>

  containerStyle:
    display: "flex"
    height: "100%"
    width: "100%"

  sidebarStyle:
    flex: "1 0 100px"

  mainStyle:
    marginLeft: "20px"
    flex: "5 1 400px"
    display: "flex"

  getInitialState: ->
    websocket = new WebSocket("ws://localhost:8082")
    websocket.onmessage = (msg) =>
      payload = JSON.parse(msg.data)
      chan = @props.flux.getStore("channels").getChannelById(payload.channel_id)
      @props.flux.getActions("messages").addMessage(channel: chan.name, message: payload)

    {websocket: websocket, activeChannel: null}

  componentWillMount: ->
    if @props.params?.channel?
      @setActiveChannel(@props.params.channel)

    @reloadChannelData()

  componentWillReceiveProps: (nextProps) ->
    @setActiveChannel(nextProps.params.channel)

  reloadChannelData: ->
    @props.flux.getActions("channels").reloadChannels()

  setActiveChannel: (channel) ->
    @props.flux.getActions("messages").preload(channel)
    @setState(activeChannel: channel)
