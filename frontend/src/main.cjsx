React = require("react")
_ = require("lodash")
Promise = require("es6-promise").Promise
Router = require("react-router/lib/runRouter")
Route = require("react-router/lib/Route")
Link = require("react-router/lib/components/Link")
DefaultRoute = require("react-router/lib/components/DefaultRoute")
RouteHandler = require("react-router/lib/components/RouteHandler")
Transition = require("react-router/lib/Transition")
Navigation = require("react-router/lib/Navigation")
{ChannelList, ChannelListItem, ChannelForm} = require("./channels")
{MessageList, MessageListItem, MessageForm} = require("./messages")
AppFlux = require("./flux")
FluxComponent = require("flummox/component")

MainView = React.createClass
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
                               {messages: messageStore.getMessages(channelStore.getActiveChannel())} }>
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
    @shouldScrollTheBox = false
    if @state?.activeChannel == nextState?.activeChannel
      prevLength = @state?.messageData[@state?.activeChannel]?.length
      nextLength = nextState?.messageData[nextState?.activeChannel]?.length

      if nextLength > prevLength
        node = React.findDOMNode(@refs.scrollBox)
        @shouldScrollTheBox = node.scrollHeight - node.scrollTop - node.offsetHeight < 5

  componentDidUpdate: ->
    if @shouldScrollTheBox
      node = React.findDOMNode(@refs.scrollBox)
      node.scrollTop = node.scrollHeight

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

App = React.createClass
  render: ->
    <div style={@styles.top}>
      <div style={@styles.container}>
        <ul>
          <li><Link to="root">Main</Link></li>
          <li><Link to="signup">Sign up</Link></li>
          <li><Link to="signin">Sign in</Link></li>
        </ul>
        <FluxComponent flux={@flux}>
          <RouteHandler />
        </FluxComponent>
      </div>
    </div>

  componentWillMount: ->
    @flux = new AppFlux()
    # for debugging
    # @flux.addListener "dispatch", (payload) ->
    # console.log("Dispatch: ", payload)

  styles:
    top:
      height: "100%"
      width: "100%"
      display: "flex"
      justifyContent: "center"
      alignItems: "center"

    container:
      backgroundColor: "white"
      padding: "30px"
      boxShadow: "0 0 20px black"
      flexFlow: "column nowrap"
      flex: "0 1 60%"
      display: "flex"
      height: "80%"

SigninView = React.createClass
  mixins: [Navigation]

  getInitialState: -> {login: "", password: "", validationFailed: false}

  render: ->
    <div>
      <p>Login</p>
      { if @state.validationFailed
          <strong>Incorrect</strong>
      }
      <form onSubmit={@onSubmit}>
        <label>Login <input type="text" name="login" onChange={@onLoginChanged} value={@state.login} /></label>
        <br />
        <label>Password <input type="password" name="password" onChange={@onPasswordChanged} value={@state.password} /></label>
        <br />
        <input type="submit" value="Login" />
      </form>
    </div>

  onLoginChanged: (event) -> @setState(login: event.target.value)

  onPasswordChanged: (event) -> @setState(password: event.target.value)

  onSubmit: (event) ->
    event.preventDefault()
    fetch "/api/signin",
      method: "post"
      headers: {"Content-Type": "application/json"}
      body: JSON.stringify(_.pick(@state, "login", "password"))
      credentials: "include"
    .then (response) ->
      if response.status >= 200 && response.status < 300
        response.json()
      else
        throw new Error(response.statusText)
    .then (json) =>
      if json.ok
        @transitionTo("root")
      else
        @setState(validationFailed: true)

SignupView = React.createClass
  mixins: [Navigation]

  getInitialState: -> {login: "", password: "", validationFailed: false}

  render: ->
    <div>
      <p>Signup</p>
      { if @state.validationFailed
          <strong>Incorrect</strong>
      }
      <form onSubmit={@onSubmit}>
        <label>Login <input type="text" name="login" onChange={@onLoginChanged} value={@state.login} /></label>
        <br />
        <label>Password <input type="password" name="password" onChange={@onPasswordChanged} value={@state.password} /></label>
        <br />
        <input type="submit" value="Login" />
      </form>
    </div>

  onLoginChanged: (event) -> @setState(login: event.target.value)

  onPasswordChanged: (event) -> @setState(password: event.target.value)

  onSubmit: (event) ->
    event.preventDefault()
    fetch "/api/signup",
      method: "post"
      headers: {"Content-Type": "application/json"}
      body: JSON.stringify(_.pick(@state, "login", "password"))
      credentials: "include"
    .then (response) ->
      if response.status >= 200 && response.status < 300
        response.json()
      else
        throw new Error(response.statusText)
    .then (json) =>
      if json.ok
        @transitionTo("root")
      else
        @setState(validationFailed: true)

Router <Route name="root" handler={App} path="/">
         <Route name="signin" handler={SigninView} />
         <Route name="signup" handler={SignupView} />
         <Route name="channel" path=":channel" handler={MainView} />
         <DefaultRoute handler={MainView} />
       </Route>, (Handler, state) ->
  React.render <Handler params={state.params} />, document.body
