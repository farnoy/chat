'use strict';
let React = require("react");
let moment = require("moment");

class MessageList extends React.Component {
  render() {
    return (<ul style={this.style()}>
      {_.map(this.props.messages, message =>
        <MessageListItem key={message.id} {...message} />)
      }
    </ul>);
  }

  style() {
    return {
      padding: 0
    };
  }
}

class MessageListItem extends React.Component {
  render() {
    let d = new Date(this.props.timestamp);
    return (
      <li>
        {this.props.author.login}: <code>{moment(d).format("hh:mm")}</code> {this.props.body}
      </li>);
  }
}

class MessageForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {value: ""};
  }

  render() {
    console.log(this.styles());
    return (
      <form onSubmit={this.onSubmit.bind(this)}>
        <input style={this.styles().input} placeholder="Message..."
               type="text" value={this.state.value} onChange={this.onChange.bind(this)} />
      </form>
    );
  }

  styles() {
    return {
      input: {
        borderRadius: "10px",
        border: "1px solid #ccc",
        padding: "5px",
        width: "100%"
      }
    };
  }

  onChange(event) {
    console.log(`onChange ${event.target.value}`);
    this.setState({value: event.target.value})
  }

  onSubmit(event) {
    fetch("/api/channels/${@props.activeChannel}/messages",
      {
        method: "post",
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({body: this.state.value}),
        credentials: "include"
      })
    .then(response => { if (response.ok) this.setState({value: ""}); })
        // before WS we used to fetch this
        // @props.modifiedCallback(@props.activeChannel)

    event.preventDefault()
  }

}

module.exports = {
  MessageList: MessageList,
  MessageListItem: MessageListItem,
  MessageForm: MessageForm
};
