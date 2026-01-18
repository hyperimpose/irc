# Messages

## Parsing

Messages received from the server are parsed using `irc_parser:message/1`. The resulting 
`t:irc_parser:message/0` value is then processed internally to update the IRC connection state and
handle protocol-level events. Once internal processing is complete, the message is passed to the
user-defined `handler` function.

The `m:irc_parser` module provides functions for extracting values from parsed messages.

IRC messages consist of one or more of the following parts:
- **Tags** - IRCv3 related. See [the spec](https://ircv3.net/specs/extensions/message-tags).
- **Prefix** - This refers to the origin (a user or a server) of the message.
- **Command** - The IRC command of the message. (Always given)
- **Params** - Any parameters of the Command.
  
You can access them with the following functions:
- `irc_parser:get_tags/1`
- `irc_parser:get_prefix/1`
- `irc_parser:get_prefix_nick/1`
- `irc_parser:get_prefix_user/1`
- `irc_parser:get_prefix_host/1`
- `irc_parser:get_command/1`
- `irc_parser:get_params/1`

Regarding the prefix functions, if no user or host is returned, the nick may actually represent a
servername. This is uncommon, and the distinction can usually be inferred from the command being
processed.
If no prefix is found (all `get_prefix*/1` functions return `undefined`), the message is assumed
to have originated from the server itself.

Instead of using `irc_parser:get_params/1` directly to handle a command, you can use one of the
specialized message parsers provided by the `m:irc_parser` module.
The `irc_parser:get_params/1` function is intended for cases where no specialized parser exists 
or when the existing parser does not cover the required use case.

_Example:_
```erlang
handler(Id, Message) ->
    case irc_parser:get_command(Message) of  % Get the IRC command
        <<"PRIVMSG">> ->  % Someone sent a message
            %% Get the prefix of the user that sent the message.
            {ok, Nick, User, Host} = irc_parser:get_prefix(Message),
            %% Get the parameters for the PRIVMSG command.
            %% Normally we would use irc_parser:privmsg/1, but we do it this
            %% way to showcase the get_params/1 function.
            [Receiver, Text] = irc_parser:get_params(Message),
            %% Print them to console. It would print something like:
            %% User drastikbot!drastik@drastik.org sent Hello! in #channel
            io:format("User ~p!~p@~p sent ~p in ~p~n",
                      [Nick, User, Host, Text, Receiver]);
        <<"JOIN">> ->  % Someone joined a channel
            %% Get the user's nickname
            Nickname = irc_parser:get_prefix_nick(Message),
            %% Use the specialized parser for the join command. It supports
            %% both normal IRC join and IRCv3 extended-join. In this case we
            %% check for both possibilities to ensure maximum compatibility
            %% between servers.
            case irc_parser:join(Message) of
                {ok, Channels} -> % Normal IRC Join. Channels is a list.
                    io:format("User ~p joined ~p", [Nickname, Channels]);
                {ok, Channels, _Account, _Realname} -> % IRCv3 extended join
                    io:format("User ~p joined ~p", [Nickname, Channels])
            end;
        _Command ->
            %% Ignore any other command
            void
    end.
```


## Sending

### irc_send
Sending messages to the IRC server is done using the `m:irc_send` module.

The following functions can be used to send messages:
- `irc_send:now/2`
- `irc_send:schedule/2`

They support both raw iolists and messages constructed using
`m:irc_make` and `m:irc_command`.

Two modes of operation are available:
- fifo
- shared

The mode can be selected with `m:irc_config`.
See the [Configuration](configuration.md) for details.

### irc_make
The `m:irc_make` module provides utilities for constructing IRC messages.
Sanitization, message splitting, and truncation is handled automatically.

The output produced by this module is compatible with the `m:irc_send` module.

**This is the preferred module to use when sending PRIVMSG and NOTICE messages.**

### irc_command
To construct other IRC protocol commands you can use the `m:irc_command` module.

It offers a structured alternative to manually concatenating command strings and
parameters. **No** sanitization or size enforcement is done.

Utilities from `m:irc_make` and `m:irc_text` may be used to handle sanitization
and size limits.

The output produced by this module is compatible with the `m:irc_send` module.


## Casemapping

Some parts of the IRC protocol, such as nicknames, are case-insensitive.
Converting between cases or comparing strings on IRC can be complicated,
as different servers may use different casemapping rules.

Proper case comparison and conversion are therefore essential to avoid
related errors and bugs.

The following functions are provided to handle IRC casemapping correctly:
- `irc_parser:casefold/2`
- `irc_parser:is_equal/3`
- `irc_parser:lowercase/2`
- `irc_parser:uppercase/2`

An example demonstrating the use of these functions is shown below:
```
%% You must be connected to an IRC server for the following to
%% work. Id must be set to the Id of the client.

example() ->
    Id = client1,  % The Id of the target IRC client
    A = "HELLO",
    B = "hello",

    %% Casefold strings so they are suitable for comparison
    case irc_parser:casefold(Id, A) == irc_parser:casefold(Id, B) of
        true  -> io:format("The strings are equal");
        false -> io:format("The strings are not equal")
    end,

    %% The above comparison can be simplified:
    case irc_parser:is_equal(Id, A, B) of
        true  -> io:format("The strings are equal");
        false -> io:format("The strings are not equal")
    end,

    %% You can convert strings to different cases:
    irc_parser:lowercase(Id, A),
    irc_parser:uppercase(Id, B).
```


## Client To Client Protocol (CTCP)

The CTCP is used for client side commands. It is embeded in PRIVMSG messages.

A common example is the /me command:
```text
Many IRC clients have a feature where users can type /me <any message here> and
the message will be shown as if it was an action done by the user:

<drastik> | This is a normal message
        * | drastik is here

To get the second message I sent /me is here. Behind the scenes this message is
sent as: PRIVMSG #channel :\x01ACTION is here\x01\r\n

The \x01ACTION is here\x01 part is the CTCP message. We know this because it
starts with the \x01 ASCII control code.
```

The `irc_parser:ctcp/1` function is provided to parse CTCP messages. In the
example below, we implement an echo bot that supports CTCP ACTION commands
and sends the text inside them:
```erlang
handler(Id, Message) ->
    case irc_parser:get_command(Message) of
        <<"PRIVMSG">> ->  % 1. Make sure you have a PRIVMSG message
            %% 2. Extract the text
            {ok, Recv, Text} = irc_parser:privmsg(Message),
            %% 3. Check if it is a CTCP message or not
            case irc_parser:ctcp(Text) of
                not_ctcp ->
                    %% 3.1. Not a CTCP message
                    irc_send:schedule(Id, irc_make:privmsg(Id, Recv, Text));
                {ok, Command, Params} ->
                    %% 3.2. This is a CTCP message. Check the CTCP command
                    case Command of
                        <<"ACTION">> ->
                            %% This is an ACTION command. Send the text back.
                            irc_send:schedule(Id, irc_make:privmsg(Id, Recv, Params));
                        _Unsupported ->
                            %% Ignore unknown commands
                            void
                    end
            end;
        _Command ->
            %% Ignore any other command
            void
    end.
```

Note that the CTCP has many different commands each with its own parameter
format, which may require further parsing. To support these commands in your
code you must read the relevant documentation and handle them yourself.

The `m:irc_make` and `m:irc_command` modules contain functions to help you
construct CTCP messages.


## Formatting

IRC messages may contain text formatting such as colors, typographical emphasis (bold, italics) etc.
Read the following to learn how to include text formatting in your messages:
- https://modern.ircdocs.horse/formatting : Detailed explanation
- https://gist.github.com/ion1/2791653 : Quick reference

### Stripping formatting
In many cases you will need to handle such messages in your programs. To do this you might need to
remove any formatting added. The function `irc_parser:formatting_strip/1` can be used for this.
