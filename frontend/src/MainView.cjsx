React = require("react")
FluxComponent = require("flummox/component")
ChannelList = require("./ChannelList")
ChannelForm = require("./ChannelForm")
MessageBox = require("./MessageBox")

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
        { if @props.flux.getStore("channels").getActiveChannel()
          <p>Channel is: {@props.params.channel}</p>
          <FluxComponent connectToStores={["channels", "messages", "app"]}
                         stateGetter={([channelStore, messageStore, appStore]) ->
                   chan = channelStore.getActiveChannel()
                   messages = messageStore.getMessages(chan)
                   {messages: messages.slice(-100), activeChannel: chan}}>
            <MessageBox />
          </FluxComponent>
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

  getInitialState: -> {websocket: null}

  componentWillMount: ->
    @props.flux.getActions("app").setupWebSocket()

    if @props.params?.channel?
      @setActiveChannel(@props.params.channel)

    @reloadChannelData()

  componentWillReceiveProps: (nextProps) ->
    @setActiveChannel(nextProps.params.channel)

  reloadChannelData: ->
    @props.flux.getActions("channels").reloadChannels()

  setActiveChannel: (channel) ->
    @props.flux.getActions("channels").setActive(channel)
    @props.flux.getActions("messages").preload(channel)
