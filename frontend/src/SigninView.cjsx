React = require("react")
Router = require("react-router/lib/runRouter")
Navigation = require("react-router/lib/Navigation")

module.exports = React.createClass
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
      body: JSON.stringify(login: @state.login, password: @state.password)
      credentials: "include"
    .then (response) ->
      response.json()
    .then (json) =>
      if json.ok
        @transitionTo("root")
      else
        @setState(validationFailed: true)
