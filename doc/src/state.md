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
The `m:irc_channel` module provides information about IRC channels the client
has joined.

Common queries include the following; this list is *not* exhaustive.

List joined channels:
- `irc_channel:list/1`
- `irc_channel:is_joined/2`

Channel modes:
- `irc_channel:get_modes/2`

Users currently present along with their prefixes:
- `irc_channel:get_users/2`

## irc_isupport

> #### RPL_ISUPPORT (005) {: .neutral}
>
> *"IRC servers and networks implement many different IRC features, limits, and
> protocol options that clients should be aware of. The RPL_ISUPPORT (005)
> numeric is designed to advertise these features to clients on connection
> registration, providing a simple way for clients to change their behaviour
> based on what is implemented on the server."*
>
> — [modern.ircdocs.horse](https://modern.ircdocs.horse/#feature-advertisement)

The client tracks a selected subset of the values in the 005 ISUPPORT numeric
and makes them available via the `m:irc_isupport` module.
