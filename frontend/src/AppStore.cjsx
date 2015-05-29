Flummox = require("flummox")
Immutable = require("immutable")

module.exports = class AppStore extends Flummox.Store
  constructor: (flux) ->
    super(flux)

    app = flux.getActionIds("app")
    @register(app.setupWebSocket, @storeWebSocket)

    @state = {websocket: null}

  storeWebSocket: (websocket) ->
    @setState(websocket: websocket)
