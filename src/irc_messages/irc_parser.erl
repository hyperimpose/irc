%%--------------------------------------------------------------------
%% Copyright (C) 2023 hyperimpose.org
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

%%%-------------------------------------------------------------------
%%% IRC protocol message parser.
%%%
%%% It parses IRC client protocol messages and provides an interface
%%% to access their contents.
%%%
%%% It also includes functions for:
%%% - Casefolding, equality checking, lowercase, uppercase (IRC aware)
%%% - Striping text formatting
%%%-------------------------------------------------------------------

-module(irc_parser).


-include_lib("kernel/include/logger.hrl").


-export([message/1]).

-export([get_tags/1, get_prefix/1, get_prefix_nick/1, get_prefix_user/1,
         get_prefix_host/1, get_command/1, get_params/1]).

-export([invite/1, join/1, kick/1, kill/1, mode/2, nick/1, notice/1, part/1,
         ping/1, pong/1, privmsg/1, privmsg/2, quit/1, topic/1, wallops/1]).

%%% Numeric replies
-export([rpl_welcome/1, rpl_yourhost/1, rpl_created/1, rpl_myinfo/1,
         rpl_isupport/1, rpl_umodeis/1, rpl_channelmodeis/2, rpl_creationtime/1,
         rpl_notopic/1, rpl_topic/1, rpl_topicwhotime/1,
         rpl_whoreply/1, rpl_namreply/2, rpl_endofnames/1, rpl_endofmotd/1,
         rpl_visiblehost/1]).

%%% IRCv3
-export([cap/1, account/1, away/1, batch/1, chghost/1, setname/1, fail/1,
         warn/1, note/1]).

%%% IRCv3: SASL
-export([authenticate/1,    rpl_loggedin/1,    rpl_loggedout/1,
         err_nicklocked/1,  rpl_saslsuccess/1, err_saslfail/1,
         err_sasltoolong/1, err_saslaborted/1, err_saslalready/1,
         rpl_saslmechs/1]).

-export([casefold/2, is_equal/3,
         lowercase/2, uppercase/2,
         lowercase_ascii/1, uppercase_ascii/1,
         lowercase_rfc1459/1, uppercase_rfc1459/1,
         lowercase_rfc1459_strict/1, uppercase_rfc1459_strict/1]).

-export([ctcp/1]).

-export([formatting_strip/1]).


%%%===================================================================
%%% Records
%%%===================================================================

-record(message, {tags = #{}, nick, user, host, command, params = []}).


%%%===================================================================
%%% Types
%%%===================================================================

-export_type([message/0]).

-type message() :: #message{}.


%%%===================================================================
%%% Macros
%%%===================================================================

-define(TAG(Tags, Key, Val), Tags#{Key => Val}).
-define(TAGS(S, Tags), S#message{tags = Tags}).

-define(NICK(S, Acc), S#message{nick = Acc}).
-define(USER(S, Acc), S#message{user = Acc}).
-define(HOST(S, Acc), S#message{host = Acc}).

-define(COMMAND(S, Acc), S#message{command = Acc}).

-define(PARAMS(S, Acc), S#message{params=lists:reverse(Acc)}).


-define(IS_DIGIT(B), B >= $0 andalso B =< $9).

%%%===================================================================
%%% IRC message parser (intended for clients only)
%%%===================================================================

%%--------------------------------------------------------------------
%% Parse an IRC message.  The message must be formatted according
%% to the  `RFC 1459, section  2.3.1' and `IRCv3 Message  Tags' specs.
%% The   parsing   is  done   as   loosely   as  possible   to   avoid
%% incompatibilities  with  the  various servers  deployed.
%%
%% No size limits  are enforced and the entire input  will be consumed
%% until a  LF character is  found or until  there is nothing  else to
%% consume.
%%
%% There is no error handling or error reporting. The given message is
%% consumed and it is upto the accessor functions to report any issues
%% with it. Invalid input may also crash the parser.
%%
%% Since the parser  will not return any information on  the amount of
%% data consumed,  messages should  be split by  the caller.   E.g. By
%% using the {packet, line} option in gen_tcp.
%%
%% The  data processed  are  not  decoded.  Bytes  come  in, bytes  go
%% out. This is  allows the bot to be encoding  agnostic.
%%
%% The parser  works entirely with  binaries.  This allows  for better
%% memory  efficiency despite  being  slower  than constructing  lists
%% (based on  measurements done in Erlang/OTP  23).  Addionally, being
%% encoding agnostic  may not be possible  if we move away  from using
%% binaries.
%%--------------------------------------------------------------------

-spec message(binary()) -> {ok, message()}.

message(<<$:, R/binary>>) -> nick(R, <<>>, #message{});
message(<<$@, R/binary>>) -> key(R, #{}, <<>>, #message{});
message(R)                -> command(R, <<>>, #message{}).


key(<<$\r, R/binary>>, T, Acc, S) -> key(R, T, Acc, S);
key(<<$\n, _/binary>>, T, Acc, S) -> {ok, ?TAGS(S, T#{Acc => <<>>})};
key(<<$\s, R/binary>>, T, Acc, S) -> tag_s(R, ?TAGS(S, T#{Acc => <<>>}));
key(<<$;,  R/binary>>, T, Acc, S) -> key(R, T#{Acc => <<>>}, <<>>, S);
key(<<$=,  R/binary>>, T, Acc, S) -> val(R, T, Acc, <<>>, S);
key(<<B,   R/binary>>, T, Acc, S) -> key(R, T, <<Acc/binary, B>>, S);
key(<<>>,              T, Acc, S) -> {ok, ?TAGS(S, T#{Acc => <<>>})}.

val(<<$\r,    R/binary>>, T, K, Acc, S) -> val(R, T, K, Acc, S);
val(<<$\n,    _/binary>>, T, K, Acc, S) -> {ok, ?TAGS(S, T#{K => Acc})};
val(<<$\s,    R/binary>>, T, K, Acc, S) -> tag_s(R, ?TAGS(S, T#{K => Acc}));
val(<<$;,     R/binary>>, T, K, Acc, S) -> key(R, T#{K => Acc}, <<>>, S);
val(<<"\\:",  R/binary>>, T, K, Acc, S) -> val(R, T, K, <<Acc/binary, $;>>, S);
val(<<"\\s",  R/binary>>, T, K, Acc, S) -> val(R, T, K, <<Acc/binary, $\s>>, S);
val(<<"\\\\", R/binary>>, T, K, Acc, S) -> val(R, T, K, <<Acc/binary, $\\>>, S);
val(<<"\\r",  R/binary>>, T, K, Acc, S) -> val(R, T, K, <<Acc/binary, $\r>>, S);
val(<<"\\n",  R/binary>>, T, K, Acc, S) -> val(R, T, K, <<Acc/binary, $\n>>, S);
val(<<"\\",   R/binary>>, T, K, Acc, S) -> val(R, T, K, Acc, S);
val(<<B,      R/binary>>, T, K, Acc, S) -> val(R, T, K, <<Acc/binary, B>>, S);
val(<<>>,                 T, K, Acc, S) -> {ok, ?TAGS(S, T#{K => Acc})}.

tag_s(<<$\s, R/binary>>, S) -> tag_s(R, S);
tag_s(<<$:,  R/binary>>, S) -> nick(R, <<>>, S);
tag_s(R, S)                 -> command(R, <<>>, S).


nick(<<$\r, R/binary>>, Acc, S) -> nick(R, Acc, S);
nick(<<$\n, _/binary>>, Acc, S) -> {ok, ?NICK(S, Acc)};
nick(<<$\s, R/binary>>, Acc, S) -> prefix_s(R, ?NICK(S, Acc));
nick(<<$!,  R/binary>>, Acc, S) -> user(R, <<>>, ?NICK(S, Acc));
nick(<<$@,  R/binary>>, Acc, S) -> host(R, <<>>, ?NICK(S, Acc));
nick(<<B,   R/binary>>, Acc, S) -> nick(R, <<Acc/binary, B>>, S);
nick(<<>>,              Acc, S) -> {ok, ?NICK(S, Acc)}.

user(<<$\r, R/binary>>, Acc, S) -> user(R, Acc, S);
user(<<$\n, _/binary>>, Acc, S) -> {ok, ?USER(S, Acc)};
user(<<$\s, R/binary>>, Acc, S) -> prefix_s(R, ?USER(S, Acc));
user(<<$@,  R/binary>>, Acc, S) -> host(R, <<>>, ?USER(S, Acc));
user(<<B,   R/binary>>, Acc, S) -> user(R, <<Acc/binary, B>>, S);
user(<<>>,              Acc, S) -> {ok, ?USER(S, Acc)}.

host(<<$\r, R/binary>>, Acc, S) -> host(R, Acc, S);
host(<<$\n, _/binary>>, Acc, S) -> {ok, ?HOST(S, Acc)};
host(<<$\s, R/binary>>, Acc, S) -> prefix_s(R, ?HOST(S, Acc));
host(<<B,   R/binary>>, Acc, S) -> host(R, <<Acc/binary, B>>, S);
host(<<>>,              Acc, S) -> {ok, ?HOST(S, Acc)}.

prefix_s(<<$\s, R/binary>>, S) -> prefix_s(R, S);
prefix_s(R, S)                 -> command(R, <<>>, S).


command(<<$\r, R/binary>>, Acc, S) -> command(R, Acc, S);
command(<<$\n, _/binary>>, Acc, S) -> {ok, ?COMMAND(S, Acc)};
command(<<$\s, R/binary>>, Acc, S) -> params(R, [], ?COMMAND(S, Acc));
command(<<B,   R/binary>>, Acc, S) -> command(R, <<Acc/binary, B>>, S);
command(<<>>,              Acc, S) -> {ok, ?COMMAND(S, Acc)}.


params(<<$\s, R/binary>>, Params, S) -> params(R, Params, S);
params(<<$:,  R/binary>>, Params, S) -> trailing(R, Params, <<>>, S);
params(R, Params, S) -> middle(R, Params, <<>>, S).

trailing(<<$\r, R/binary>>, P, Acc, S) -> trailing(R, P, Acc, S);
trailing(<<$\n, _/binary>>, P, Acc, S) -> {ok, ?PARAMS(S, [Acc | P])};
trailing(<<B,   R/binary>>, P, Acc, S) -> trailing(R, P, <<Acc/binary, B>>, S);
trailing(<<>>,              P, Acc, S) -> {ok, ?PARAMS(S, [Acc | P])}.

middle(<<$\r, R/binary>>, P, Acc,  S) -> middle(R, P, Acc, S);
middle(<<$\n, _/binary>>, P, <<>>, S) -> {ok, ?PARAMS(S, P)};
middle(<<$\n, _/binary>>, P, Acc,  S) -> {ok, ?PARAMS(S, [Acc | P])};
middle(<<$\s, R/binary>>, P, Acc,  S) -> params(R, [Acc | P], S);
middle(<<B,   R/binary>>, P, Acc,  S) -> middle(R, P, <<Acc/binary, B>>, S);
middle(<<>>,              P, <<>>, S) -> {ok, ?PARAMS(S, P)};
middle(<<>>,              P, Acc,  S) -> {ok, ?PARAMS(S, [Acc | P])}.


%%%===================================================================
%%% Parsed message getter functions
%%%===================================================================

-spec get_tags(message()) -> map().
get_tags(#message{tags = Tags}) -> Tags.

-spec get_prefix(message()) -> {ok, binary() | undefined,
                                    binary() | undefined,
                                    binary() | undefined}.
get_prefix(#message{nick = N, user = U, host = H}) -> {ok, N, U, H}.

-spec get_prefix_nick(message()) -> binary() | undefined.
get_prefix_nick(#message{nick = N}) -> N.

-spec get_prefix_user(message()) -> binary() | undefined.
get_prefix_user(#message{user = U}) -> U.

-spec get_prefix_host(message()) -> binary() | undefined.
get_prefix_host(#message{host = H}) -> H.

-spec get_command(message()) -> binary() | undefined.
get_command(#message{command = C}) -> C.

-spec get_params(message()) -> list().
get_params(#message{params = P}) -> P.


%%%===================================================================
%%% Message details
%%%===================================================================

%%% Functions for further parsing messages based on their command and
%%% accessing their parameters.
%%%
%%% Since this is a client library it is assumed that the messages
%%% passed to the functions below have been sent by an IRC server.
%%% This is important because the form of the parameters may be
%%% different to the one that would be used if a client had sent the
%%% message. Some client only commands have no equivalent server
%%% messages (they only have numerics).


%%--------------------------------------------------------------------
%% INVITE message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: INVITE
%% Parameters: <nickname> <channel>
%%--------------------------------------------------------------------

-spec invite(message()) -> {ok, binary(), binary()}.

invite(#message{params = [N, C]}) -> {ok, N, C}.

%%--------------------------------------------------------------------
%% JOIN message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: JOIN
%% Parameters: <channel>{,<channel>}
%%
%% IRCv3 `extended-join' Extension:
%%
%%    Command: JOIN
%% Parameters: <channel>{,<channel>} <accountname> <realname>
%%    Example: :nick!user@host JOIN #channel accountname :Real Name
%%--------------------------------------------------------------------

-spec join(message()) ->
          {ok, [binary()]} | {ok, [binary()], binary(), binary()}.

join(#message{params = [C]}) ->
    {ok, binary:split(C, <<$,>>, [global])};
join(#message{params = [C, AN, RN]}) ->
    {ok, binary:split(C, <<$,>>, [global]), AN, RN}.

%%--------------------------------------------------------------------
%% KICK message.
%%
%% RFC 1459:
%%
%%    Command: KICK
%% Parameters: <channel> <user> [<comment>]
%%
%% RFC 2812:
%%
%%    Command: KICK
%% Parameters: <channel> *( "," <channel> ) <user> *( "," <user> ) [<comment>]
%% Note:
%%
%% RFC 2812  specifies that when the  server sends a KICK  message the
%% RFC   1459   format   MUST    be   used   to   maintain   backwards
%% compatibility. We respect the RFC and  hope that IRC servers do the
%% same.
%%
%% If an IRC  server that breaks this parser is  found, the API should
%% be  extended   to  support   multiple  users  and/or   channels  AS
%% NEEDED. Backwards compatibility will be broken in such a case.
%%--------------------------------------------------------------------

-spec kick(message()) -> {ok, binary(), binary(), binary()}.

kick(#message{params = [C, U]})    -> {ok, C, U, <<>>};
kick(#message{params = [C, U, T]}) -> {ok, C, U, T}.

%%--------------------------------------------------------------------
%% KILL message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: KILL
%% Parameters: <nickname> <comment>
%%--------------------------------------------------------------------

-spec kill(message()) -> {ok, binary(), binary()}.

kill(#message{params = [N, C]}) -> {ok, N, C}.

%%--------------------------------------------------------------------
%% MODE message.
%%
%%    Command: MODE
%% Parameters: <target> [<modestring> [<mode arguments>...]]
%%--------------------------------------------------------------------

-spec mode(Id :: atom(), message()) ->
          {ok, binary()}
              | {ok, binary(), [{add | delete, char(), binary()}]}.

mode(_Id, #message{params = [T]}) ->
    {ok, T};
mode(Id, #message{params = [T, M | A]}) ->
    case irc_isupport:is_channel(Id, T) of
        true  -> mode_channel(Id, T, M, A);
        false -> mode_user(Id, T, M, A)
    end.

mode_channel(Id, Target, Modes, Args) ->
    P = [MP || {MP, _} <- irc_isupport:get_prefix(Id)],
    %% Inject the prefix modes
    Types = case irc_isupport:get_chanmodes(Id) of
                [A, B | R] -> [A,  [B, P] | R];
                [A]        -> [A,  P];
                []         -> [[], P]
            end,

    case Modes of
        <<$+, M/binary>> -> mode_add(M, Target, Args, Types, []);
        <<$-, M/binary>> -> mode_delete(M, Target, Args, Types, [])
    end.

mode_user(_Id, Target, Modes, Args) ->
    case Modes of
        <<$+, M/binary>> -> mode_add(M, Target, Args, user, []);
        <<$-, M/binary>> -> mode_delete(M, Target, Args, user, [])
    end.

mode_add(<<$+, R/binary>>, Target, Args, Types, Acc) ->
    mode_add(R, Target, Args, Types, Acc);
mode_add(<<$-, R/binary>>, Target, Args, Types, Acc) ->
    mode_delete(R, Target, Args, Types, Acc);
mode_add(<<M, R/binary>>, Target, Args, Types, Acc) ->
    case get_mode_type(Types, M) of
        1 ->  % Type A modes MUST have an arg when sent by the server
            [A | Args1] = Args,
            mode_add(R, Target, Args1, Types, [{add, a, M, A} | Acc]);
        2 ->  % Type B modes MUST have an arg
            [A | Args1] = Args,
            mode_add(R, Target, Args1, Types, [{add, b, M, A} | Acc]);
        3 ->  % Type C modes MUST have an arg when set
            [A | Args1] = Args,
            mode_add(R, Target, Args1, Types, [{add, c, M, A} | Acc]);
        4 ->  % Type D modes MUST NOT have args
            mode_add(R, Target, Args, Types, [{add, d, M, <<>>} | Acc]);
        _ ->  % Ignore other mode types
            mode_add(R, Target, Args, Types, Acc)
    end;
mode_add(<<>>, Target, _Args, _Types, Acc) ->
    {ok, Target, Acc}.

mode_delete(<<$+, R/binary>>, Target, Args, Types, Acc) ->
    mode_add(R, Target, Args, Types, Acc);
mode_delete(<<$-, R/binary>>, Target, Args, Types, Acc) ->
    mode_delete(R, Target, Args, Types, Acc);
mode_delete(<<M, R/binary>>, Target, Args, Types, Acc) ->
    case get_mode_type(Types, M) of
        1 ->  % Type A modes MUST have an arg when sent by the server
            [A | Args1] = Args,
            mode_add(R, Target, Args1, Types, [{delete, a, M, A} | Acc]);
        2 ->  % Type B modes MUST have an arg
            [A | Args1] = Args,
            mode_add(R, Target, Args1, Types, [{delete, b, M, A} | Acc]);
        3 ->  % Type C modes MUST NOT have an arg when unset
            mode_add(R, Target, Args, Types, [{delete, c, M, <<>>} | Acc]);
        4 ->  % Type D modes MUST NOT have args
            mode_add(R, Target, Args, Types, [{delete, d, M, <<>>} | Acc]);
        _ ->  % Ignore other mode types
            mode_add(R, Target, Args, Types, Acc)
    end;
mode_delete(<<>>, Target, _Args, _Types, Acc) ->
    {ok, Target, Acc}.


get_mode_type(user, _Mode) ->
    4;  % User modes have no arguments
get_mode_type(Types, Mode) ->
    get_mode_type(Types, Mode, 1).

get_mode_type([H | R], Mode, Type) ->
    case string:find(H, [Mode]) of
        nomatch -> get_mode_type(R, Mode, Type + 1);
        _Else   -> Type
    end;
get_mode_type([], _Mode, _Type)      ->
    0.

%%--------------------------------------------------------------------
%% NICK message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: NICK
%% Parameters: <nickname>
%%--------------------------------------------------------------------

-spec nick(message()) -> {ok, binary()}.

nick(#message{params = [N]}) -> {ok, N}.

%%--------------------------------------------------------------------
%% NOTICE message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: NOTICE
%% Parameters: <nickname> <text>
%% Note:
%%
%% See the note for PRIVMSG.
%%
%% It's  interesting  that  RFC  1459 defines  NOTICE  with  a  single
%% target. This further  confirms the decision to use  a single target
%% for parsing PRIVMSG and NOTICE.
%%
%% Despite this, some servers may  accept multiple targets in incoming
%% NOTICE messages. This is indicated by 005 RPL_ISUPPORT.
%%--------------------------------------------------------------------

-spec notice(message()) -> {ok, binary(), binary()}.

notice(#message{params = [R, T]}) -> {ok, R, T}.

%%--------------------------------------------------------------------
%% PART message.
%%
%% RFC 1459:
%%
%%    Command: PART
%% Parameters: <channel>{,<channel>}
%%
%% RFC 2812:
%%
%%    Command: PART
%% Parameters: <channel> *( "," <channel> ) [ <Part Message> ]
%%--------------------------------------------------------------------

-spec part(message()) -> {ok, [binary()], binary()}.

part(#message{params = [C]}) -> {ok, binary:split(C, <<$,>>, [global]), <<>>};
part(#message{params = [C, T]}) -> {ok, binary:split(C, <<$,>>, [global]), T}.

%%--------------------------------------------------------------------
%% PING message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: PING
%% Parameters: <server1> [<server2>]
%%--------------------------------------------------------------------

-spec ping(message()) -> {ok, binary()} | {ok, binary(), binary()}.

ping(#message{params = [S1]})     -> {ok, S1};
ping(#message{params = [S1, S2]}) -> {ok, S1, S2}.

%%--------------------------------------------------------------------
%% PONG message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: PONG
%% Parameters: <server1> [<server2>]
%%--------------------------------------------------------------------

-spec pong(message()) -> {ok, binary()} | {ok, binary(), binary()}.

pong(#message{params = [S1]})     -> {ok, S1};
pong(#message{params = [S1, S2]}) -> {ok, S1, S2}.

%%--------------------------------------------------------------------
%% PRIVMSG message.
%%
%% RFC 1459:
%%
%%    Command: PRIVMSG
%% Parameters: <receiver>{,<receiver>} <text to be sent>
%%
%% RFC 2812:
%%
%%    Command: PRIVMSG
%% Parameters: <msgtarget> <text to be sent>
%% Note:
%%
%% As is  obvious the two RFCs  disagree with each other.   For client
%% sent messages  the amount  of targets is  usually specified  by the
%% server  using  the  005  RPL_ISUPPORT numeric,  using  the  TARGMAX
%% parameter. For server sent messages  there is no known mechanism to
%% communicate  how  the  server  operates  or which  spec  is  to  be
%% followed.
%%
%% This implementation  follows RFC 2812  because that format  is more
%% logical  for  a  server  to   use,  should  have  better  backwards
%% compatibility and is easier to implement. Empirically a server that
%% sends multiple targetted PRIVMSG messages has never been observed.
%%
%% Also see the note for NOTICE
%%--------------------------------------------------------------------

-spec privmsg(message()) -> {ok, binary(), binary()}.

privmsg(#message{params = [R, T]}) -> {ok, R, T}.

%%--------------------------------------------------------------------
%% PRIVMSG message.
%%
%% Works like privmsg/1 but the receiver is changed to the nickname of
%% the user that sent the message  if the the receiver is the nickname
%% of the bot. This  is meant to be used when  receiving a message you
%% want to reply to.
%%
%% Note that only  the nickname is added as the  receiver. If you want
%% to be more specific, for  example messaging a password to services,
%% consider adding the the full prefix for safety reasons.
%%--------------------------------------------------------------------

-spec privmsg(Id :: term(), message()) -> {ok, binary(), binary()}.

privmsg(Id, #message{nick = N, params = [R, T]}) ->
    case irc_state:get_nickname(Id) of
        R -> {ok, N, T};
        _ -> {ok, R, T}
    end.

%%--------------------------------------------------------------------
%% QUIT message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: QUIT
%% Parameters: [<Quit message>]
%%--------------------------------------------------------------------

-spec quit(message()) -> {ok, binary()}.

quit(#message{params = []})  -> {ok, <<>>};
quit(#message{params = [T]}) -> {ok, T}.

%%--------------------------------------------------------------------
%% TOPIC message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: TOPIC
%% Parameters: <channel> [<topic>]
%%
%% Note: Normally servers do not send the topic command without
%% including `<topic>', because this format is meant to be used by
%% clients to request the topic from the server. If this happens we
%% assume that an empty topic was given.
%%--------------------------------------------------------------------

-spec topic(message()) -> {ok, binary(), binary()}.

topic(#message{params = [C]})    -> {ok, C, <<>>};
topic(#message{params = [C, T]}) -> {ok, C, T}.

%%--------------------------------------------------------------------
%% WALLOPS (Operwall) message.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: WALLOPS
%% Parameters: <text to send>
%%--------------------------------------------------------------------

-spec wallops(message()) -> {ok, binary()}.

wallops(#message{params = [T]}) -> {ok, T}.


%%%===================================================================
%%% IRCv3
%%%===================================================================

%%--------------------------------------------------------------------
%% IRCv3 Capability Negotiation:
%%
%% Spec: https://ircv3.net/specs/extensions/capability-negotiation
%%--------------------------------------------------------------------

-spec cap(message()) ->
          {cap_ls, binary(), [{binary(), binary()}]}
              | {cap_ls_more, binary(), [{binary(), binary()}]}
              | {cap_list, binary(), [binary()]}
              | {cap_list_more, binary(), [binary()]}
              | {cap_ack, binary(), [binary()]}
              | {cap_nak, binary(), [binary()]}
              | {cap_new, binary(), [{binary(), binary()}]}
              | {cap_del, binary(), [binary()]}.

cap(#message{params = [Id, <<"LS">>, L]}) ->
    {cap_ls, Id, cap_parse_kv(cap_parse_list(L), [])};
cap(#message{params = [Id, <<"LS">>, <<$*>>, L]}) ->
    {cap_ls_more, Id, cap_parse_kv(cap_parse_list(L), [])};
cap(#message{params = [Id, <<"LIST">>, L]}) ->
    {cap_list, Id, cap_parse_list(L)};
cap(#message{params = [Id, <<"LIST">>, <<$*>>, L]}) ->
    {cap_list_more, Id, cap_parse_list(L)};
cap(#message{params = [Id, <<"ACK">>, L]}) ->
    {cap_ack, Id, cap_parse_list(L)};
cap(#message{params = [Id, <<"NAK">>, L]}) ->
    {cap_nak, Id, cap_parse_list(L)};
cap(#message{params = [Id, <<"NEW">>, L]}) ->
    {cap_new, Id, cap_parse_kv(cap_parse_list(L), [])};
cap(#message{params = [Id, <<"DEL">>, L]}) ->
    {cap_del, Id, cap_parse_list(L)}.

cap_parse_list(ListBinary) ->
    %% Make sure to not include empty binaries. Can happen if ListBinary = <<>>
    %% or if there are multiple spaces in between keys.
    [X || X <- binary:split(ListBinary, <<$\s>>, [global]), X =/= <<>>].

cap_parse_kv([H | R], Acc) ->
    case binary:split(H, <<$=>>) of
        [K, V] ->
            cap_parse_kv(R, [{K, V} | Acc]);
        [K] ->
            cap_parse_kv(R, [{K, <<>>} | Acc])
    end;
cap_parse_kv([], Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% IRCv3 account-notify
%%
%% Spec: https://ircv3.net/specs/extensions/account-notify
%%--------------------------------------------------------------------

-spec account(message()) -> {ok, binary()}.

account(#message{params = [A]}) -> {ok, A}.

%%--------------------------------------------------------------------
%% IRCv3 away-notify
%%
%% Spec: https://ircv3.net/specs/extensions/away-notify
%%--------------------------------------------------------------------

-spec away(message()) -> not_away | {away, binary()}.

away(#message{params = []})  -> not_away;
away(#message{params = [T]}) -> {away, T}.

%%--------------------------------------------------------------------
%% IRCv3 batch
%%
%% Spec: https://ircv3.net/specs/extensions/batch
%%--------------------------------------------------------------------

-spec batch(message()) ->
          {ok, Ref :: binary(), Type :: binary(), Params :: [binary()]}
              | {ok, Ref :: binary()}.

batch(#message{params = [<<$+, R/binary>>, T | P]}) -> {ok, R, T, P};
batch(#message{params = [<<$-, R/binary>>]})        -> {ok, R}.

%%--------------------------------------------------------------------
%% IRCv3 chghost
%%
%% Spec: https://ircv3.net/specs/extensions/chghost
%%--------------------------------------------------------------------

-spec chghost(message()) -> {ok, User :: binary(), Host :: binary()}.

chghost(#message{params = [U, H]}) -> {ok, U, H}.


%%% IRCv3: SASL ======================================================

%%--------------------------------------------------------------------
%% AUTHENTICATE message
%%
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec authenticate(message()) -> empty | {ok, binary()} | {more, binary()}.

authenticate(#message{params = [<<"+">>]})                   -> empty;
authenticate(#message{params = [D]}) when byte_size(D) < 400 -> {ok, D};
authenticate(#message{params = [D]})                         -> {more, D}.


%%--------------------------------------------------------------------
%% RPL_LOGGEDIN numeric reply
%%
%% Numerics: 900
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec rpl_loggedin(message()) -> {ok, binary(), binary(), binary(), binary()}.

rpl_loggedin(#message{params = [N, P, A, T]}) -> {ok, N, P, A, T}.


%%--------------------------------------------------------------------
%% RPL_LOGGEDOUT numeric reply
%%
%% Numerics: 901
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec rpl_loggedout(message()) -> {ok, binary(), binary(), binary()}.

rpl_loggedout(#message{params = [N, P, T]}) -> {ok, N, P, T}.


%%--------------------------------------------------------------------
%% ERR_NICKLOCKED numeric reply
%%
%% Numerics: 902
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec err_nicklocked(message()) -> {ok, binary(), binary()}.

err_nicklocked(#message{params = [N, T]}) -> {ok, N, T}.


%%--------------------------------------------------------------------
%% RPL_SASLSUCCESS numeric reply
%%
%% Numerics: 903
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec rpl_saslsuccess(message()) -> {ok, binary(), binary()}.

rpl_saslsuccess(#message{params = [N, T]}) -> {ok, N, T}.


%%--------------------------------------------------------------------
%% ERR_SASLFAIL numeric reply
%%
%% Numerics: 904
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec err_saslfail(message()) -> {ok, binary(), binary()}.

err_saslfail(#message{params = [N, T]}) -> {ok, N, T}.


%%--------------------------------------------------------------------
%% ERR_SASLTOOLONG numeric reply
%%
%% Numerics: 905
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec err_sasltoolong(message()) -> {ok, binary(), binary()}.

err_sasltoolong(#message{params = [N, T]}) -> {ok, N, T}.


%%--------------------------------------------------------------------
%% ERR_SASLABORTED numeric reply
%%
%% Numerics: 906
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec err_saslaborted(message()) -> {ok, binary(), binary()}.

err_saslaborted(#message{params = [N, T]}) -> {ok, N, T}.


%%--------------------------------------------------------------------
%% ERR_SASLALREADY numeric reply
%%
%% Numerics: 907
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec err_saslalready(message()) -> {ok, binary(), binary()}.

err_saslalready(#message{params = [N, T]}) -> {ok, N, T}.


%%--------------------------------------------------------------------
%% RPL_SASLMECHS numeric reply
%%
%% Numerics: 908
%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%--------------------------------------------------------------------

-spec rpl_saslmechs(message()) -> {ok, binary(), binary(), binary()}.

rpl_saslmechs(#message{params = [N, M, T]}) -> {ok, N, M, T}.


%%% IRCv3: setname ===================================================

%%--------------------------------------------------------------------
%% IRCv3 setname
%%
%% Spec: https://ircv3.net/specs/extensions/setname
%%--------------------------------------------------------------------

-spec setname(message()) -> {ok, binary()}.

setname(#message{params = [T]}) -> {ok, T}.

%%--------------------------------------------------------------------
%% IRCv3 Standard Replies
%%
%% Spec: https://ircv3.net/specs/extensions/standard-replies
%%--------------------------------------------------------------------

-spec fail(message()) -> {ok, binary(), binary(), [binary()], binary()}.

fail(#message{params = [Cmd, Code | R]}) -> std_rpl(R, Cmd, Code, []).


-spec warn(message()) -> {ok, binary(), binary(), [binary()], binary()}.

warn(#message{params = [Cmd, Code | R]}) -> std_rpl(R, Cmd, Code, []).


-spec note(message()) -> {ok, binary(), binary(), [binary()], binary()}.

note(#message{params = [Cmd, Code | R]}) -> std_rpl(R, Cmd, Code, []).


std_rpl([H | []], Cmd, Code, Acc) -> {ok, Cmd, Code, lists:reverse(Acc), H};
std_rpl([H | R], Cmd, Code, Acc)  -> std_rpl(R, Cmd, Code, [H | Acc]).


%%%===================================================================
%%% Numeric replies
%%%===================================================================

%%% Functions for parsing the parameters of numeric replies.
%%%
%%% Since  it is  possible for  a numeric  to have  different meanings
%%% depending  on the  implementation of  the server  or for  multiple
%%% numerics to have the same meaning, the <command> is not checked.

%%--------------------------------------------------------------------
%% RPL_WELCOME numeric reply
%%
%%   Numerics: 001
%% Parameters: <client> :<text>
%%--------------------------------------------------------------------

-spec rpl_welcome(message()) -> {ok, binary(), binary()}.

rpl_welcome(#message{params = [Nick, Text]}) ->
    {ok, Nick, Text}.


%%--------------------------------------------------------------------
%% RPL_YOURHOST numeric reply
%%
%%   Numerics: 002
%% Parameters: <client> :<text>
%%--------------------------------------------------------------------

-spec rpl_yourhost(message()) -> {ok, binary(), binary()}.

rpl_yourhost(#message{params = [Nick, Text]}) ->
    {ok, Nick, Text}.


%%--------------------------------------------------------------------
%% RPL_CREATED numeric reply
%%
%%   Numerics: 003
%% Parameters: <client> :<text>
%%--------------------------------------------------------------------

-spec rpl_created(message()) -> {ok, binary(), binary()}.

rpl_created(#message{params = [Nick, Text]}) ->
    {ok, Nick, Text}.


%%--------------------------------------------------------------------
%% RPL_MYINFO numeric reply
%%
%%   Numerics: 004
%% Parameters: <client> <servername> <version> <user modes> <channel modes>
%%             [<channel modes with a parameter>]
%%--------------------------------------------------------------------

-spec rpl_myinfo(message()) ->
          {ok, #{nick => binary(),
                 servername => binary(),
                 version => binary(),
                 u_modes => binary(),
                 c_modes => binary(),
                 p_modes => binary()}}.

rpl_myinfo(#message{params = [Nick, SName, Version, UModes, CModes]}) ->
    {ok, #{nick => Nick, servername => SName, version => Version,
           u_modes => UModes, c_modes => CModes, p_modes => <<>>}};
rpl_myinfo(#message{params = [Nick, SName, Version, UModes, CModes, PModes]}) ->
    {ok, #{nick => Nick, servername => SName, version => Version,
           u_modes => UModes, c_modes => CModes, p_modes => PModes}}.


%%--------------------------------------------------------------------
%% RPL_ISUPPORT numeric reply
%%
%%   Numerics: 005
%% Parameters: <nick> <1-13 tokens> :are supported by this server
%% This function  may also be  used for RPL_REMOTEISUPPORT  105, which
%% uses the same format.
%%
%% This function makes no attempt to parse the values. This is because
%% parameter  values  have different  formats  and  defaults that  are
%% better handled by the parameter's handler.
%%
%% To aid dealing with faulty servers the tokens retain their original
%% order.  E.g.  A  server that sends the same parameter  twice in the
%% same message.
%%--------------------------------------------------------------------

-spec rpl_isupport(message()) ->
          {ok, binary(), Tokens :: [{binary(), binary()}], binary()}.

rpl_isupport(#message{params = [Nick | R]}) -> rpl_isupport(R, Nick, []).

rpl_isupport([H | []], Nick, Acc) ->
    {ok, Nick, lists:reverse(Acc), H};
rpl_isupport([H | R], Nick, Acc) ->
    case binary:split(H, <<$=>>) of
        [K, V] ->
            rpl_isupport(R, Nick, [{K, V} | Acc]);
        [K] ->
            rpl_isupport(R, Nick, [{K, <<>>} | Acc])
    end.


%%--------------------------------------------------------------------
%% RPL_UMODEIS numeric reply
%%
%%   Numerics: 221
%% Parameters: <client> <user modes>
%%--------------------------------------------------------------------

-spec rpl_umodeis(message()) -> {ok, binary(), [byte()]}.

rpl_umodeis(#message{params = [Nick, UModes]}) ->
    [$+ | Modes] = unicode:characters_to_list(UModes),
    {ok, Nick, Modes}.


%%--------------------------------------------------------------------
%% RPL_CHANNELMODEIS numeric reply
%%
%%   Numerics: 324
%% Parameters: <client> <channel> <modestring> <mode arguments>
%%--------------------------------------------------------------------

-spec rpl_channelmodeis(Id :: any(), message()) ->
          {ok, binary(), [{add | delete, char(), binary()}]}.

rpl_channelmodeis(Id, #message{params = [_Nick, Channel, Modes | Args]}) ->
    mode_channel(Id, Channel, Modes, Args).


%%--------------------------------------------------------------------
%% RPL_CREATIONTIME numeric reply
%%
%%   Numerics: 329
%% Parameters: <client> <channel> <creationtime>
%%--------------------------------------------------------------------

-spec rpl_creationtime(message()) -> {ok, binary(), binary(), binary()}.

rpl_creationtime(#message{params = [Nick, Channel, Creationtime]}) ->
    {ok, Nick, Channel, Creationtime}.


%%--------------------------------------------------------------------
%% RPL_NOTOPIC numeric reply
%%
%%   Numerics: 331
%% Parameters: <client> <channel> :No topic is set
%%--------------------------------------------------------------------

-spec rpl_notopic(message()) -> {ok, binary(), binary()}.

rpl_notopic(#message{params = [_Nick, Channel, Text]}) ->
    {ok, Channel, Text}.


%%--------------------------------------------------------------------
%% RPL_TOPIC numeric reply
%%
%%   Numerics: 332
%% Parameters: <client> <channel> :<topic>
%%--------------------------------------------------------------------

-spec rpl_topic(message()) -> {ok, binary(), binary()}.

rpl_topic(#message{params = [_Nick, Channel, Topic]}) ->
    {ok, Channel, Topic}.


%%--------------------------------------------------------------------
%% RPL_TOPICWHOTIME numeric reply
%%
%%   Numerics: 333
%% Parameters: <client> <channel> <nick> <setat>
%%--------------------------------------------------------------------

-spec rpl_topicwhotime(message()) -> {ok, binary(), binary()}.

rpl_topicwhotime(#message{params = [_Client, Channel, Nick, Timestamp]}) ->
    {ok, Channel, Nick, Timestamp}.


%%--------------------------------------------------------------------
%% RPL_WHOREPLY numeric reply
%%
%%   Numerics: 352
%% Parameters: <client> <channel> <user> <host> <server> <nick> <flags>
%%              :<hopcount> <realname>
%% Also known as RPL_YOURDISPLAYEDHOST or RPL_HOSTHIDDEN. The host can
%% also be in the form <user@host>.
%%--------------------------------------------------------------------

-spec rpl_whoreply(message()) ->
          {ok, #{client => binary(), channel => binary(), user => binary(),
                 host => binary(), server => binary(), nick => binary(),
                 flags => binary(), hopcount => binary(),
                 realname => binary()}}.

rpl_whoreply(#message{params = [Client, Channel, User, Host, Server, Nick,
                                Flags | Rest]}) ->
    [Hopcount, Realname | _] = string:split(Rest, " "),
    {ok, #{client => Client,
           channel => Channel,
           user => User,
           host => Host,
           server => Server,
           nick => Nick,
           flags => Flags,
           hopcount => Hopcount,
           realname => Realname}}.


%%--------------------------------------------------------------------
%% RPL_NAMREPLY numeric reply
%%
%%   Numerics: 353
%% Parameters: <client> <symbol> <channel> :[prefix]<nick>{ [prefix]<nick>}
%% This parser supports the `multi-prefix' IRCv3 extension.
%%--------------------------------------------------------------------

-spec rpl_namreply(Id :: term(), message()) ->
          {ok,
           Client  :: binary(),
           Symbol  :: binary(),
           Channel :: binary(),
           Users   :: #{binary() => [binary()]}}.

rpl_namreply(Id, #message{params = [Client, Symbol, Channel, Users]}) ->
    Ps = lists:map(fun ({_M, C}) -> C end,
                   irc_isupport:get_prefix(Id)),
    Us = lists:foldl(fun (X, Acc) ->
                              {U, P} = rpl_namreply_user(X, Ps, []),
                              Acc#{U => P}
                      end,
                      #{},
                      string:split(Users, " ", all)),
    {ok, Client, Symbol, Channel, Us}.

rpl_namreply_user(<<C, R/binary>> = Bin, Ps, Acc) ->
    %% Split the nickname from the prefix(es).
    case lists:any(fun (X) -> X == C end, Ps) of
        true  ->
            rpl_namreply_user(R, Ps, [C | Acc]);
        false ->
            {Bin, lists:reverse(Acc)}
    end.


%%--------------------------------------------------------------------
%% RPL_ENDOFNAMES numeric reply
%%
%%   Numerics: 366
%% Parameters: <client> <channel> :<text>
%%--------------------------------------------------------------------

-spec rpl_endofnames(message()) ->
          {ok, Client :: binary(), Channel :: binary(), Text :: binary()}.

rpl_endofnames(#message{params = [Client, Channel, Text]}) ->
    {ok, Client, Channel, Text}.


%%--------------------------------------------------------------------
%% RPL_ENDOFMOTD numeric reply
%%
%%   Numerics: 376
%% Parameters: <nick> :<text>
%%--------------------------------------------------------------------

-spec rpl_endofmotd(message()) -> {ok, Nick :: binary(), Text :: binary()}.

rpl_endofmotd(#message{params = [Nick, Text]}) -> {ok, Nick, Text}.


%%--------------------------------------------------------------------
%% RPL_VISIBLEHOST numeric reply
%%
%%   Numerics: 396
%% Parameters: <client> <host> :<text>
%% Also known as RPL_YOURDISPLAYEDHOST or RPL_HOSTHIDDEN. The host can
%% also be in the form <user@host>.
%%--------------------------------------------------------------------

-spec rpl_visiblehost(message()) ->
          {ok, Nick :: binary(), Host :: binary(), Text ::binary()}.

rpl_visiblehost(#message{params = [Nick, Host, Text]}) ->
    case string:split(Host, <<"@">>) of
        [_U, H] -> {ok, Nick, H, Text};
        [Host]  -> {ok, Nick, Host, Text}
    end.


%%%===================================================================
%%% Casemapping
%%%===================================================================

-spec casefold(Id :: term(), Input :: iodata()) -> binary().

casefold(Id, Input) -> lowercase(Id, Input).


-spec is_equal(Id :: term(), In1 :: iodata(), In2 :: iodata()) -> boolean().

is_equal(Id, In1, In2) -> casefold(Id, In1) == casefold(Id, In2).


-spec lowercase(Id :: term(), Input :: iodata()) -> binary().

lowercase(Id, Input) ->
    Bin = iolist_to_binary(Input),

    case irc_isupport:get_casemapping(Id) of
        <<"rfc1459">>        -> lowercase_rfc1459(Bin);
        <<"ascii">>          -> lowercase_ascii(Bin);
        <<"rfc1459-strict">> -> lowercase_rfc1459_strict(Bin);
        <<"strict-rfc1459">> -> lowercase_rfc1459_strict(Bin);
        Else ->
            ?LOG_EMERGENCY("[IRC:~p] Unsupported casemapping: ~p", [Id, Else]),
            %% Use ascii as a fallback, but this will probably break things
            lowercase_ascii(Bin)
    end.


-spec uppercase(Id :: term(), Input :: iodata()) -> binary().

uppercase(Id, Input) ->
    Bin = iolist_to_binary(Input),

    case irc_isupport:get_casemapping(Id) of
        <<"rfc1459">>        -> uppercase_rfc1459(Bin);
        <<"ascii">>          -> uppercase_ascii(Bin);
        <<"rfc1459-strict">> -> uppercase_rfc1459_strict(Bin);
        <<"strict-rfc1459">> -> uppercase_rfc1459_strict(Bin);
        Else ->
            ?LOG_EMERGENCY("[IRC:~p] Unsupported casemapping: ~p", [Id, Else]),
            %% Use ascii as a fallback, but this will probably break things
            uppercase_ascii(Bin)
    end.


-spec lowercase_ascii(binary()) -> binary().

lowercase_ascii(<<Bin/binary>>) -> lc_ascii(Bin, <<>>).

lc_ascii(<<B, R/binary>>, Acc) when B >= $A, B =< $Z ->
    lc_ascii(R, <<Acc/binary, (B + 16#20)>>);
lc_ascii(<<B, R/binary>>, Acc) ->
    lc_ascii(R, <<Acc/binary, B>>);
lc_ascii(<<>>, Acc) ->
    Acc.


-spec uppercase_ascii(binary()) -> binary().

uppercase_ascii(<<Bin/binary>>) -> uc_ascii(Bin, <<>>).

uc_ascii(<<B, R/binary>>, Acc) when B >= $a, B =< $z ->
    uc_ascii(R, <<Acc/binary, (B - 16#20)>>);
uc_ascii(<<B, R/binary>>, Acc) ->
    uc_ascii(R, <<Acc/binary, B>>);
uc_ascii(<<>>, Acc) ->
    Acc.


-spec lowercase_rfc1459(binary()) -> binary().

lowercase_rfc1459(<<Bin/binary>>) -> lc_rfc1459(Bin, <<>>).

lc_rfc1459(<<B, R/binary>>, Acc) when B >= $A, B =< $^ ->
    lc_rfc1459(R, <<Acc/binary, (B + 16#20)>>);
lc_rfc1459(<<B, R/binary>>, Acc) ->
    lc_rfc1459(R, <<Acc/binary, B>>);
lc_rfc1459(<<>>, Acc) ->
    Acc.


-spec uppercase_rfc1459(binary()) -> binary().

uppercase_rfc1459(<<Bin/binary>>) -> uc_rfc1459(Bin, <<>>).

uc_rfc1459(<<B, R/binary>>, Acc) when B >= $a, B =< $~ ->
    uc_rfc1459(R, <<Acc/binary, (B - 16#20)>>);
uc_rfc1459(<<B, R/binary>>, Acc) ->
    uc_rfc1459(R, <<Acc/binary, B>>);
uc_rfc1459(<<>>, Acc) ->
    Acc.


-spec lowercase_rfc1459_strict(binary()) -> binary().

lowercase_rfc1459_strict(<<Bin/binary>>) -> lc_rfc1459_strict(Bin, <<>>).

lc_rfc1459_strict(<<B, R/binary>>, Acc) when B >= $A, B =< $] ->
    lc_rfc1459_strict(R, <<Acc/binary, (B + 16#20)>>);
lc_rfc1459_strict(<<B, R/binary>>, Acc) ->
    lc_rfc1459_strict(R, <<Acc/binary, B>>);
lc_rfc1459_strict(<<>>, Acc) ->
    Acc.


-spec uppercase_rfc1459_strict(binary()) -> binary().

uppercase_rfc1459_strict(<<Bin/binary>>) -> uc_rfc1459_strict(Bin, <<>>).

uc_rfc1459_strict(<<B, R/binary>>, Acc) when B >= $a, B =< $} ->
    uc_rfc1459_strict(R, <<Acc/binary, (B - 16#20)>>);
uc_rfc1459_strict(<<B, R/binary>>, Acc) ->
    uc_rfc1459_strict(R, <<Acc/binary, B>>);
uc_rfc1459_strict(<<>>, Acc) ->
    Acc.


%%%===================================================================
%%% CTCP parser
%%%===================================================================

-spec ctcp(binary()) -> {ok, binary(), binary()} | not_ctcp.

ctcp(<<16#01, R/binary>>) -> ctcp_command(R, <<>>);
ctcp(_)                   -> not_ctcp.

ctcp_command(<<$\s, R/binary>>, Acc)   -> ctcp_params(R, Acc, <<>>);
ctcp_command(<<16#01, _/binary>>, Acc) -> {ok, Acc, <<>>};
ctcp_command(<<B, R/binary>>, Acc)     -> ctcp_command(R, <<Acc/binary, B>>);
ctcp_command(<<>>, Acc)                -> {ok, Acc, <<>>}.

ctcp_params(<<16#01, _/binary>>, C, Acc) ->
    {ok, C, Acc};
ctcp_params(<<B, R/binary>>, C, Acc) ->
    ctcp_params(R, C, <<Acc/binary, B>>);
ctcp_params(<<>>, C, Acc) ->
    {ok, C, Acc}.


%%%===================================================================
%%% Formatting
%%%===================================================================

-spec formatting_strip(binary()) -> binary().
formatting_strip(<<Input/binary>>) -> fstrip(Input, <<>>).

fstrip(<<16#02, R/binary>>, Acc) -> fstrip(R, Acc);  % Bold
fstrip(<<16#1D, R/binary>>, Acc) -> fstrip(R, Acc);  % Italic
fstrip(<<16#1F, R/binary>>, Acc) -> fstrip(R, Acc);  % Underline
fstrip(<<16#1E, R/binary>>, Acc) -> fstrip(R, Acc);  % Strikethrough
fstrip(<<16#11, R/binary>>, Acc) -> fstrip(R, Acc);  % Monospace
fstrip(<<16#16, R/binary>>, Acc) -> fstrip(R, Acc);  % Reverse Color
fstrip(<<16#0F, R/binary>>, Acc) -> fstrip(R, Acc);  % Reset
%% Color
fstrip(<<16#03, F1, F2, $,, B1, B2, R/binary>>, Acc)
  when ?IS_DIGIT(F1), ?IS_DIGIT(F2), ?IS_DIGIT(B1), ?IS_DIGIT(B2) ->
    fstrip(R, Acc);
fstrip(<<16#03, F1, F2, $,, B1, R/binary>>, Acc)
  when ?IS_DIGIT(F1), ?IS_DIGIT(F2), ?IS_DIGIT(B1) ->
    fstrip(R, Acc);
fstrip(<<16#03, F1, $,, B1, B2, R/binary>>, Acc)
  when ?IS_DIGIT(F1), ?IS_DIGIT(B1), ?IS_DIGIT(B2) ->
    fstrip(R, Acc);
fstrip(<<16#03, F1, $,, B1, R/binary>>, Acc)
  when ?IS_DIGIT(F1), ?IS_DIGIT(B1) ->
    fstrip(R, Acc);
fstrip(<<16#03, F1, F2, R/binary>>, Acc)
  when ?IS_DIGIT(F1), ?IS_DIGIT(F2) ->
    fstrip(R, Acc);
fstrip(<<16#03, F1, R/binary>>, Acc)
  when ?IS_DIGIT(F1) ->
    fstrip(R, Acc);
fstrip(<<16#03, R/binary>>, Acc) ->
    fstrip(R, Acc);
%% Hex Color
fstrip(<<16#03, F1, F2, F3, F4, F5, F6,
         $,, B1, B2, B3, B4, B5, B6, R/binary>>, Acc)
  when ?IS_DIGIT(F1), ?IS_DIGIT(F2), ?IS_DIGIT(F3),
       ?IS_DIGIT(F4), ?IS_DIGIT(F5), ?IS_DIGIT(F6),
       ?IS_DIGIT(B1), ?IS_DIGIT(B2), ?IS_DIGIT(B3),
       ?IS_DIGIT(B4), ?IS_DIGIT(B5), ?IS_DIGIT(B6) ->
    fstrip(R, Acc);
fstrip(<<16#03, F1, F2, F3, F4, F5, F6, R/binary>>, Acc)
  when ?IS_DIGIT(F1), ?IS_DIGIT(F2), ?IS_DIGIT(F3),
       ?IS_DIGIT(F4), ?IS_DIGIT(F5), ?IS_DIGIT(F6) ->
    fstrip(R, Acc);
fstrip(<<16#04,R/binary>>, Acc) ->
    fstrip(R, Acc);
fstrip(<<B, R/binary>>, Acc) -> fstrip(R, <<Acc/binary, B>>);
fstrip(<<>>, Acc) -> Acc.
