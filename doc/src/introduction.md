# Introduction
 
This is the documentation for `irc`, an Erlang/OTP application that implements the IRC client protocol.
Internet Relay Chat (IRC) is an internet protocol for text-based instant messaging.

This application aims to be a complete IRC client that can be used in any project for connecting and
interacting with an IRC server.

## Prerequisites

It is assumed that the reader is familiar with:
- Erlang/OTP - https://www.erlang.org/doc/getting_started/users_guide
- The IRC client protocol - https://www.rfc-editor.org/rfc/rfc1459.html / https://modern.ircdocs.horse/

## Components

In this section we describe the most important concepts you need to know before you can start using
this application.

### Id
This IRC client application supports multiple simultaneous connections.
Each client instance requires a unique `Id`.

Th `Id` must be an Erlang atom. It is strongly recommended that this atom is *not* reused elsewhere
in your project to avoid name collisions and other issues (for example, in ETS and other Erlang
facilities).

### State
The IRC client maintains the connection state, including the nickname, user modes, channel information,
ISUPPORT, IRCv3 capabilities, and other details.

See the [State](state.md) page for more information.

### Messages
Incoming messages are parsed internally and processed by the client runtime before being passed to
the handler function.

For outgoing communication, the application provides facilities for both low-level and safe
high-level message construction. Message transmission can be immediate or time-shared.

See the [Messages](messages.md) page for further information.

### Configuration
IRC clients retrieve their configuration parameters from the `m:irc_config` gen server. You must
use this module to set the configuration before you can connect to an IRC server.

Learn more in the [Configuration](configuration.md) page.

### Handler
To receive and react to IRC events, you must implement a handler function.
The IRC client passes all incoming messages to this function.

This function is configured via the `handler` option in the `m:irc_config` module.

The IRC client calls the handler function synchronously for each message, blocking further processing
until the function returns.

> #### Asynchronous Handling {: .info}
> For asynchronous handling, the handler function should spawn new processes or forward messages
> to existing ones.

The handler function takes two arguements: the client `Id` and the parsed `Message` received from
the server.
