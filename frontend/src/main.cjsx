React = require("react")
Router = require("react-router/lib/runRouter")
Route = require("react-router/lib/Route")
DefaultRoute = require("react-router/lib/components/DefaultRoute")
SignupView = require("./SignupView")
SigninView = require("./SigninView")
MainView = require("./MainView")
App = require("./App")

Router <Route name="root" handler={App} path="/">
         <Route name="signin" handler={SigninView} />
         <Route name="signup" handler={SignupView} />
         <Route name="channel" path=":channel" handler={MainView} />
         <DefaultRoute handler={MainView} />
       </Route>, (Handler, state) ->
  React.render <Handler params={state.params} />, document.body
