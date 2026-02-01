# Configuration
IRC clients retrieve their settings from the `m:irc_config` gen_server. You must
define and store your configuration *before* attempting to connect to an IRC
server.

The API is split into *pure* functions for modifying the configuration maps and
*impure* function for persisting those maps to the `m:irc_config` gen_server:

| Scope                 | Functions        | Description                                   |
|:----------------------|:-----------------|:----------------------------------------------|
| **Global (Stateful)** | `get/1`, `set/2` | Retrieve or persist a full config map by Id.  |
| **Local (Pure)**      | `new/0`          | Create a default configuration map.           |
| **Local (Pure)**      | `get_*/1`        | Read a specific option from a local map.      |
| **Local (Pure)**      | `set_*/2`        | Update an option (returns a new map). |

## Creating a configuration
The configuration is represented using a map as defined by the
`t:irc_config:config/0` type. To prevent breaking changes, do not pattern match
on the map directly.

**Example:**
```erlang
%% 1. Initialize an empty configuration.
Config0 = irc_config:new(),

%% 2. Chain updates (returns new maps)
Config1 = irc_config:set_address(Config0, "irc.example.org"),
Config2 = irc_config:set_port(Config1, 6697).
%% ConfigN = ...

%% 3. Persist to the gen_server under a unique ID (atom)
ok = irc_config:set(my_server, ConfigN).
```

## Storing and Retrieving

- `irc_config:set(Id, Config)`
  Stores a config map under `Id` (an atom).
- **`irc_config:get(Id)`**
  Retrieves the configuration map currently associated with `Id`.

> #### Persistence Note {: .note}
>
> The configuration is stored in-memory within the `m:irc_config` process.
> It is **not** persisted to disk. If the Erlang node restarts, you must re-apply
> your configurations.

## Runtime behaviour
Configurations obtained via `irc_config:get/1` are only used to bootstrap the
client connection.

After an IRC client has started, its configuration is cached internally by its
process.

Changes made using `irc_config:set/2` while the client is running **do not**
affect the active connection. Updated values are applied only after the client is
restarted.

To inspect the configuration currently used by a **running** client, use
`irc_state:get_config/1`.

## Options

A brief summury of the options is provided on the table below:

| Option          | Explanation                               | Required | Default                                                  |
|-----------------|-------------------------------------------|----------|----------------------------------------------------------|
| address         | The hostname/IP of the IRC server         | yes      |                                                          |
| port            | The port used by the IRC server           |          | 6697                                                     |
| tls             | Whether to use TLS or not                 |          | true                                                     |
| server_password | The password used by the IRC server       |          | undefined                                                |
| packet_size     | The max number of bytes in an IRC message |          | 512                                                      |
| irc_send_mode   | See IRC message scheduling                |          | {shared, #{commands => 1, interval => 2000, size => 60}} |
| nickname        | The nickname to use                       | yes      |                                                          |
| user            | The username to use                       | yes      |                                                          |
| realname        | The realname to use                       | yes      |                                                          |
| nickserv        | See Nickserv                              |          | #{}                                                      |
| channels        | Channels to autojoin                      |          | #{}                                                      |
| sasl            | A list of SASL auth mechanisms to try     |          | []                                                       |
| cap_want        | A list of IRCv3 capabilities to enable    |          | [<<"sasl">>]                                             |
| handler         | The handler function                      | yes      |                                                          |


