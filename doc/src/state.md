# State
The IRC application tracks and maintains the protocol state for each client
connection. This state is derived from incoming server messages and includes the
current nickname, user modes, channel users, channel and user modes, topics, and
server-advertised capabilities.

IRC message parsing and state updates are handled internally. Consumers of the
API can query this state directly, without needing to track or interpret protocol
messages themselves.

The state is exposed through a set of dedicated modules, each responsible for a
specific aspect of the connection.

## irc_state
The `m:irc_state` module provides functions for querying the state of the
current client connection.

Current configuration snapshot (see also `configuration.md`):
- `irc_state:get_config/1`

IRCv3 capability negotiation state:
- `irc_state:get_cap_list/1`
- `irc_state:has_cap_list/2`
- `irc_state:get_cap_ls/1`
- `irc_state:get_cap_end/1`

Client identity, authentication, and user mode information:
- `irc_state:get_nickname/1`
- `irc_state:get_user/1`
- `irc_state:get_host/1`
- `irc_state:get_account/1`
- `irc_state:get_auth/1`
- `irc_state:get_modes/1`

## irc_channel

## irc_isupport
