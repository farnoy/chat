React = require("react")
ChatFlux = require("./ChatFlux")
FluxComponent = require("flummox/component")
RouteHandler = require("react-router/lib/components/RouteHandler")
Link = require("react-router/lib/components/Link")

module.exports = React.createClass
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
    @flux = new ChatFlux()
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
      flex: "0 1 800px"
      display: "flex"
      height: "80%"
      margin: "50px"
