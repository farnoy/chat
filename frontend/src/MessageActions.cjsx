Flummox = require("flummox")
Promise = require("es6-promise").Promise

module.exports = class MessageActions extends Flummox.Actions
  constructor: (@flux) ->
    super()

  preload: (channel) ->
    unless @flux.getStore("messages").hasMessages(channel)
      @loadMessages(channel)

  loadMessages: (channel) ->
    new Promise (resolve, reject) ->
      fetch "/api/channels/#{channel}/messages",
        credentials: "include"
      .then (response) ->
        response.json()
      .then (json) ->
        resolve(channel: channel, messages: json)

  createMessage: ({channel, message}) ->
    new Promise (resolve, reject) ->
      fetch "/api/channels/#{channel}/messages",
        method: "post",
        headers: {"Content-Type": "application/json"}
        body: JSON.stringify(message)
        credentials: "include"
      .then (response) ->
        if response.ok
          resolve()

  addMessage: ({channel, message}) -> {channel, message}

