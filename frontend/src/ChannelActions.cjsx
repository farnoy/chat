Flummox = require("flummox")
Promise = require("es6-promise").Promise

module.exports = class ChannelActions extends Flummox.Actions
  createChannel: ({name}) ->
    new Promise (resolve, reject) ->
      fetch "/api/channels",
        method: "post",
        headers: {"Content-Type": "application/json"}
        body: JSON.stringify({name: name})
        credentials: "include"
      .then (response) ->
        if response.status >= 200 && response.status < 300
          response.json()
        else
          err = Error("Cannot create the channel")
          err.channel = name
          throw err
      .then (response) =>
        if response.ok
          fetch "/api/channels",
            headers: {"Content-Type": "application/json"}
            credentials: "include"
          .then (response) ->
            if response.status >= 200 && response.status < 300
              response.json()
            else
              err = Error("Cannot fetch the channels")
              err.channel = name
              throw err
          .then (json) ->
            resolve(json)

  setChannels: (channels) -> channels

  setActive: (channel) -> channel

  reloadChannels: ->
    new Promise (resolve, reject) =>
      fetch("/api/channels", credentials: "include").then (response) ->
        response.json()
      .then (json) =>
        resolve(@setChannels(json))

  delete: (channel) ->
    new Promise (resolve, reject) =>
      fetch "/api/channels/#{channel}",
        credentials: "include"
        method: "delete"
        headers: {"Content-Type": "application/json"}
      .then ->
        resolve(channel)

