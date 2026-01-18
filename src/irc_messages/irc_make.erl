%%--------------------------------------------------------------------
%% Copyright 2023, 2026 hyperimpose.org
%%
%% This file is part of irc.
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published
%% by the Free Software Foundation, version 3.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%--------------------------------------------------------------------

-module(irc_make).

-moduledoc("""
High level IRC message construction.

The functions in this module will perform all the necessary sanitization,
splitting, truncation etc. on the given messages.
""").


-include_lib("kernel/include/logger.hrl").


-export([clean_up/1]).

-export([cap_req/1]).

-export([notice_max_size/2, notice/3, notice/4]).
-export([privmsg_max_size/2, privmsg/3, privmsg/4, ctcp_action/3]).


-define(CAP_REQ_LIMIT, 450).

-define(DEFAULT_TRUNC, <<"...">>).


%% Types
-type many() :: {many, [message()]}.
-type message() :: {message, #{command := atom(),
                               receiver := term(),
                               message := iodata()}}.


%%%===================================================================
%%% Helpers
%%%===================================================================

-doc("""
Remove unsafe bytes from the input text.

It  removes any  CRLF bytes  to make  sure that  the text  does not
contain a  second IRC command. This  can be used as  a mechanism to
clean user input and prevent the insertion of unwanted commands.

It also removes the NUL byte because it is not an allowed character
according to the RFCs.
""").
-spec clean_up(iodata()) -> binary().

clean_up(Text) ->
    case unicode:characters_to_binary(Text) of
        {Error, Bin, Rest} ->
            ?LOG_ERROR("[IRC] irc_make:clean_up - unicode ~p at: ~p"
                       " - Using the correct part: ~p", [Error, Rest, Bin]),
            clean_up(Bin, <<>>);
        Bin ->
            clean_up(Bin, <<>>)
    end.

clean_up(<<$\r, R/binary>>, Acc) -> clean_up(R, Acc);
clean_up(<<$\n, R/binary>>, Acc) -> clean_up(R, Acc);
clean_up(<<0,   R/binary>>, Acc) -> clean_up(R, Acc);
clean_up(<<C,   R/binary>>, Acc) -> clean_up(R, <<Acc/binary, C>>);
clean_up(<<>>,              Acc) -> Acc.


%% Clients SHOULD ensure that their list of requested capabilities is
%% not too long to be replied to with a single ACK or NAK message. If
%% a REQ’s final parameter gets sufficiently large (approaching the
%% 510 byte limit), clients SHOULD instead send multiple REQ
%% subcommands.

%% The IRC server will reply with: CAP * ACK :<caps>CRLF.  This leaves
%% at most 512 -  13 = 499 bytes for the caps.  We  use a Limit of 450
%% bytes.

-doc false.
cap_req(Caps) ->
    cap_req1(Caps, ?CAP_REQ_LIMIT, [], []).

cap_req1([H | R], Lim, Caps, Cmds) ->
    case iolist_size(H) of
        Size when Size > Lim ->
            Lim1 = ?CAP_REQ_LIMIT - Size,
            cap_req1(R, Lim1, [H], [irc_command:cap_req(Caps) | Cmds]);
        Size ->
            cap_req1(R, Lim - Size, [H | Caps], Cmds)
    end;
cap_req1([], _Lim, Caps, Cmds) ->
    [irc_command:cap_req(Caps) | Cmds].


-doc("""
Get the maximum text length allowed in a NOTICE message.

See: `irc_make:privmsg_max_size/2`.
""").
-spec notice_max_size(Id :: term(), Msgtarget :: binary()) -> integer().

notice_max_size(Id, Msgtarget) ->
    {Len, Nick, User, Host} = irc_state:group_length_prefix(Id),
    N = byte_size(Nick),
    U = byte_size(User),
    H = byte_size(Host),
    M = byte_size(Msgtarget),
    Len - N - U - H - M - 15.


-doc #{equiv => notice(Id, Recv, Text, #{mode => truncate})}.
notice(Id, Recv, Text) ->
    notice(Id, Recv, Text, #{mode => truncate}).


-doc("""
Make a NOTICE message.

See: `irc_make:privmsg/4`.
""").
-spec notice(Id, Recv, Text, Opts) -> message() | many() when
      Id   :: atom(),
      Recv :: iodata(),
      Text :: unicode:chardata(),
      Opts :: #{mode := truncate | fractional | divide}.

notice(Id, Recv, Text, #{mode := truncate} = Opts) ->
    Ellipsis = maps:get(ellipsis, Opts, ?DEFAULT_TRUNC),
    Max = notice_max_size(Id, Recv),
    Out = irc_text:truncate(clean_up(Text), Max, Ellipsis),
    {message, #{command  => notice,
                receiver => Recv,
                message  => irc_command:notice(Recv, Out)}};

notice(Id, Recv, Texts, #{mode := fractional} = Opts) ->
    Ellipsis = maps:get(ellipsis, Opts, ?DEFAULT_TRUNC),
    Max = notice_max_size(Id, Recv),
    Ts = [{F, clean_up(T)} || {F, T} <- Texts],
    Out = irc_text:fractional_truncate(Ts, Max, Ellipsis),
    {message, #{command  => notice,
                receiver => Recv,
                message  => irc_command:notice(Recv, Out)}};

notice(Id, Recv, Text, divide) ->
    MaxSize = notice_max_size(Id, Recv),
    TextList = irc_text:divide(clean_up(Text), MaxSize),
    F = fun (X) -> {message, #{command => notice,
                               receiver => Recv,
                               message => irc_command:notice(Recv, X)}}
        end,
    {many, lists:map(F, TextList)}.


-doc("""
Get the maximum text length allowed in a PRIVMSG message.

IRC messages are of a specific byte size. Usually 512 bytes.

This function will  calculate how many bytes are left  for the text
part of a PRIVMSG command.

A received PRIVMSG has the following format:
```text
:<nickname>!<user>@<hostmask> PRIVMSG <msgtarget> :<text><\r\n>
```

16 bytes are used for punctuation, the command itself and CRLF. The
rest is calculated dynamically.
""").
-spec privmsg_max_size(Id :: term(), Msgtarget :: binary()) -> integer().

privmsg_max_size(Id, Msgtarget) ->
    {Len, Nick, User, Host} = irc_state:group_length_prefix(Id),
    N = byte_size(Nick),
    U = byte_size(User),
    H = byte_size(Host),
    M = byte_size(Msgtarget),
    Len - N - U - H - M - 16.


-doc #{equiv => privmsg(Id, Recv, Text, #{mode => truncate})}.
privmsg(Id, Recv, Text) ->
    privmsg(Id, Recv, Text, #{mode => truncate}).


-doc("""
Make a PRIVMSG message.

This function constructs one or more PRIVMSG messages and performs all
necessary sanitization and size handling.

The behavior depends on the selected `mode`:

- `truncate`:
  The text is truncated to fit within the maximum allowed message size.
  If truncation occurs, an ellipsis is appended.

- `fractional`: The text is truncated using `irc_text:fractional_truncate/3`.

  _Example:_
  ```erlang
  Texts = [{1, "Not truncated"},
           {0.8, "80 percent truncated"},
           {0.2, "20 percent truncated"}],
  irc_make:privmsg(Id, Recv, Texts, #{mode => fractional}).
  ```

- `divide`:
  The text is split into multiple messages, each within the maximum allowed
  size. In this mode, the function returns a value of type `t:many/0`.

When `#{mode => truncate}` or `#{mode => fractional}` the `ellipsis` option
can be used to set an ellipsis other than the default.
""").
-spec privmsg(Id, Recv, Text, Opts) -> message() | many() when
      Id   :: atom(),
      Recv :: iodata(),
      Text :: unicode:chardata(),
      Opts :: #{mode := truncate | fractional | divide}.

privmsg(Id, Recv, Text, #{mode := truncate} = Opts) ->
    Ellipsis = maps:get(ellipsis, Opts, ?DEFAULT_TRUNC),
    Max = privmsg_max_size(Id, Recv),
    Out = irc_text:truncate(clean_up(Text), Max, Ellipsis),
    {message, #{command  => privmsg,
                receiver => Recv,
                message  => irc_command:privmsg(Recv, Out)}};

privmsg(Id, Recv, Texts, #{mode := fractional} = Opts) ->
    Ellipsis = maps:get(ellipsis, Opts, ?DEFAULT_TRUNC),
    Max = privmsg_max_size(Id, Recv),
    Ts = [{F, clean_up(T)} || {F, T} <- Texts],
    Out = irc_text:fractional_truncate(Ts, Max, Ellipsis),
    {message, #{command  => privmsg,
                receiver => Recv,
                message  => irc_command:privmsg(Recv, Out)}};

privmsg(Id, Recv, Text, #{mode := divide}) ->
    Max = privmsg_max_size(Id, Recv),
    TextList = irc_text:divide(clean_up(Text), Max),
    F = fun (X) -> {message, #{command => privmsg,
                               receiver => Recv,
                               message => irc_command:privmsg(Recv, X)}}
        end,
    {many, lists:map(F, TextList)}.


-doc("""
Create a CTCP ACTION message (known as /me).

The input text is sanitized and truncated as necessary to fit within the maximum
allowed message size. If truncation occurs, an ellipsis is appended.
""").
-spec ctcp_action(Id, Target, Text) -> message() when
      Id :: atom(),
      Target :: iodata(),
      Text :: unicode:chardata().

ctcp_action(Id, Target, Text) ->
    Max = privmsg_max_size(Id, Target) - 9,
    Out = irc_text:truncate(clean_up(Text), Max, ?DEFAULT_TRUNC),
    {message, #{command  => privmsg,
                receiver => Target,
                message  => irc_command:ctcp_action(Target, Out)}}.
