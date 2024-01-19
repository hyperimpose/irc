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
%%% Construct IRC protocol messages
%%%-------------------------------------------------------------------

-module(irc_command).

-export([admin/0,  admin/1,  away/0,   away/1,  connect/1,  connect/2,
         connect/3, die/0,  info/0, info/1, invite/2,  ison/1, join/1,
         kick/2,  kick/3, kill/2,  links/0, links/1,  links/2, list/0,
         list/1, list/2, lusers/0, lusers/1, lusers/2, mode/1, mode/2,
         motd/0, motd/1, names/0,  names/1, names/2, nick/1, notice/2,
         notice_many/2,  oper/2,   part/1,  part/2,   pass/1,  ping/1,
         ping/2,  pong/1, pong/2,  privmsg/2, privmsg_many/2,  quit/0,
         quit/1,  rehash/0,  restart/0,   squit/2,  stats/1,  stats/2,
         time/0, time/1,  topic/1, topic/2, trace/0,  trace/1, user/4,
         userhost/1,  users/0, users/1,  version/0, version/1,  who/0,
         who/1,   who/2,   whois/1,   whois/2,   whowas/1,   whowas/2,
         whowas/3]).

-export([cap_ls/0,   cap_ls/1,   cap_list/0,   cap_req/1,   cap_end/0,
         tagmsg/1,    batch_start/3,    batch_end/1,    monitor_add/1,
         monitor_remove/1,       monitor_clear/0,      monitor_list/0,
         monitor_status/0, authenticate/1, setname/1]).

-export([ctcp/2,     ctcp/3,     ctcp_action/2,     ctcp_clientinfo/1,
         ctcp_clientinfo/2, ctcp_ping/2, ctcp_source/1, ctcp_source/2,
         ctcp_time/1, ctcp_time/2, ctcp_version/1, ctcp_version/2]).

%%% This module implements the IRC commands described in RFC 1459, RFC
%%% 2812 and  IRCv3.
%%%
%%% Commands from other sources may also be implemented here.
%%%
%%% No effort to secure/sanitize user input is made. The caller has to
%%% make sure that the parameters passed do not inject any messages or
%%% parameters.   The  obvious  sanitization   that  can  be  done  is
%%% filtering out  any CRLF bytes,  but other (less  severe) injection
%%% methods are possible.
%%%
%%% No message size limit is enforced.  Make sure you will not go over
%%% the limit (usually 512 bytes).
%%%
%%% The functions  in this module will  construct the part of  the IRC
%%% message that contains the command and its parameters. The user may
%%% add a  `tags' or  a `prefix'  part to the  output of  the provided
%%% commands as needed.
%%%
%%% Because the functions include the parameters part in their output,
%%% which is the final part of an IRC message, the CRLF bytes are also
%%% added.


%%%===================================================================
%%% RFC defined commands
%%%===================================================================

%%--------------------------------------------------------------------
%% ADMIN command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: ADMIN
%% Parameters: [ <target> ]
%%--------------------------------------------------------------------

-spec admin() -> binary().

admin() -> <<"ADMIN\r\n">>.


-spec admin(Target :: iodata()) -> iolist().

admin(Server) -> [<<"ADMIN ">>, Server, <<"\r\n">>].

%%--------------------------------------------------------------------
%% AWAY command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: AWAY
%% Parameters: [ <text> ]
%%--------------------------------------------------------------------

-spec away() -> binary().

away() -> <<"AWAY\r\n">>.


-spec away(Text :: iodata()) -> iolist().

away(Text) -> [<<"AWAY :">>, Text, <<"\r\n">>].

%%--------------------------------------------------------------------
%% CONNECT command.
%%
%% RFC 1459:
%%
%%    Command: CONNECT
%% Parameters: <target server> [<port> [<remote server>]]
%%
%% RFC 2812:
%%
%%    Command: CONNECT
%% Parameters: <target server> <port> [ <remote server> ]
%%--------------------------------------------------------------------

-spec connect(Target :: iodata()) -> iolist().

connect(Target) -> [<<"CONNECT ">>, Target, <<"\r\n">>].


-spec connect(Target :: iodata(), Port :: iodata()) -> iolist().

connect(Target, Port) -> [<<"CONNECT ">>, Target, <<" ">>, Port, <<"\r\n">>].


-spec connect(Target :: iodata(), Port :: iodata(), Remote :: iodata())
             -> iolist().

connect(Target, Port, Remote) ->
    [<<"CONNECT ">>, Target, <<" ">>, Port, <<" ">>, Remote, <<"\r\n">>].

%%--------------------------------------------------------------------
%% DIE command.
%%
%% RFC 2812:
%%
%%    Command: DIE
%% Parameters:
%%--------------------------------------------------------------------

-spec die() -> binary().

die() -> <<"DIE\r\n">>.


%%--------------------------------------------------------------------
%% INFO command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: INFO
%% Parameters: [ <target> ]
%%--------------------------------------------------------------------

-spec info() -> binary().

info() -> <<"INFO\r\n">>.


-spec info(Target :: iodata()) -> iolist().

info(Target) -> [<<"INFO ">>, Target, <<"\r\n">>].

%%--------------------------------------------------------------------
%% INVITE command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: INVITE
%% Parameters: <nickname> <channel>
%%--------------------------------------------------------------------

-spec invite(Nick :: iodata(), Channel :: iodata()) -> iolist().

invite(Nick, Channel) -> [<<"INVITE ">>, Nick, <<" ">>, Channel, <<"\r\n">>].

%%--------------------------------------------------------------------
%% ISON command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: ISON
%% Parameters: <nickname>{<space><nickname>}
%%--------------------------------------------------------------------

-spec ison(Nicknames :: [iodata()]) -> iolist().

ison(Nicknames) -> [<<"ISON ">>, lists:join(<<" ">>, Nicknames), <<"\r\n">>].


%%--------------------------------------------------------------------
%% JOIN command.
%%
%% RFC 1459:
%%
%%    Command: JOIN
%% Parameters: <channel>{,<channel>} [<key>{,<key>}]
%%
%% RFC 2812:
%%
%%    Command: JOIN
%% Parameters: ( <channel> *( "," <channel> ) [ <key> *( "," <key> ) ] )
%%             / "0"
%%--------------------------------------------------------------------

-spec join(0 | #{iodata() => iodata()}) -> iolist().

join(0)        -> <<"JOIN 0\r\n">>;
join(Channels) -> join(maps:next(maps:iterator(Channels)), [], []).

join({C, K, I}, CAcc, KAcc) when K =:= ""; K =:= <<>> ->
    join(maps:next(I), CAcc ++ [C], KAcc);
join({C, K, I}, CAcc, KAcc) ->
    join(maps:next(I), [C | CAcc], [K | KAcc]);
join(none, CAcc, []) ->
    [<<"JOIN ">>, lists:join(<<",">>, CAcc), <<"\r\n">>];
join(none, CAcc, KAcc) ->
    [<<"JOIN ">>, lists:join(<<",">>, CAcc),
     <<" ">>, lists:join(<<",">>, KAcc), <<"\r\n">>].

%%--------------------------------------------------------------------
%% KICK command.
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
%%
%% RFC 2812 states that "For  the message to be syntactically correct,
%% there  MUST  be either  one  channel  parameter and  multiple  user
%% parameter,  or  as  many  channel  parameters  as  there  are  user
%% parameters.".
%%
%% We consider this syntax confusing  and pointless to implement. Only
%% support for multiple users on a single channel is provided.
%%--------------------------------------------------------------------

-spec kick(Channel :: iodata(), Users :: [iodata()]) -> iolist().

kick(Channel, Users) ->
    [<<"KICK ">>, Channel, <<" ">>, lists:join(<<",">>, Users), <<"\r\n">>].


-spec kick(Channel :: iodata(), Users :: [iodata()], Comment :: iodata())
          -> iolist().

kick(Channel, Users, Comment) ->
    U = lists:join(<<",">>, Users),
    [<<"KICK ">>, Channel, <<" ">>, U, <<" :">>, Comment, <<"\r\n">>].

%%--------------------------------------------------------------------
%% KILL command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: KILL
%% Parameters: <nickname> <comment>
%%--------------------------------------------------------------------

-spec kill(Nickname :: iodata(), Comment :: iodata()) -> iolist().

kill(Nickname, Comment) ->
    [<<"KILL ">>, Nickname, <<" :">>, Comment, <<"\r\n">>].


%%--------------------------------------------------------------------
%% LINKS command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: LINKS
%% Parameters: [[<remote server>] <server mask>]
%%--------------------------------------------------------------------

-spec links() -> binary().

links() -> <<"LINKS\r\n">>.


-spec links(ServerMask :: iodata()) -> iolist().

links(ServerMask) -> [<<"LINKS ">>, ServerMask, <<"\r\n">>].


-spec links(RemoteServer :: iodata(), ServerMask :: iodata()) -> iolist().

links(RemoteServer, ServerMask) ->
    [<<"LINKS ">>, RemoteServer, <<" ">>, ServerMask, <<"\r\n">>].

%%--------------------------------------------------------------------
%% LIST command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: LIST
%% Parameters: [<channel>{,<channel>} [<server>]]
%%--------------------------------------------------------------------

-spec list() -> binary().

list() -> <<"LIST\r\n">>.


-spec list(Channels :: [iodata()]) -> iolist().

list(Channels) -> [<<"LIST ">>, lists:join(<<",">>, Channels), <<"\r\n">>].


-spec list(Channels :: [iodata()], Server :: iodata()) -> iolist().

list(Channels, Server) ->
    [<<"LIST ">>, lists:join(<<" ">>, Channels), <<" ">>, Server, <<"\r\n">>].

%%--------------------------------------------------------------------
%% LUSERS command.
%%
%% RFC 2812:
%%
%%    Command: LUSERS
%% Parameters: [ <mask> [ <target> ] ]
%%--------------------------------------------------------------------

-spec lusers() -> binary().

lusers() -> <<"LUSERS\r\n">>.


-spec lusers(Mask :: iodata()) -> iolist().

lusers(Mask) -> [<<"LUSERS ">>, Mask, <<"\r\n">>].


-spec lusers(Mask :: iodata(), Target :: iodata()) -> iolist().

lusers(Mask, Target) -> [<<"LUSERS ">>, Mask, <<" ">>, Target, <<"\r\n">>].

%%--------------------------------------------------------------------
%% MODE command.
%%
%%    Command: MODE
%% Parameters: nickname *( ("-" / "+" ) modes)
%% Parameters: channel *( ("-" / "+" ) modes *arguments )
%%
%% The RFC definitions are fuzzy  and make assumptions about the modes
%% available. Above  is a  partial ABNF  representation of  the syntax
%% according to RFC 2812.
%%
%% The ISUPPORT numeric can be used  to learn which modes are provided
%% by the server.
%%--------------------------------------------------------------------

-spec mode(Target :: iodata()) -> iolist().

mode(Target) -> [<<"MODE ">>, Target, <<"\r\n">>].


-spec mode(Target :: iodata(), Modes :: iodata()) -> iolist().

mode(Target, Modes) -> [<<"MODE ">>, Target, <<" ">>, Modes, <<"\r\n">>].

%%--------------------------------------------------------------------
%% MOTD command.
%%
%% RFC 2812:
%%
%%    Command: MOTD
%% Parameters: [ <target> ]
%%--------------------------------------------------------------------

-spec motd() -> iolist().

motd() -> [<<"MOTD\r\n">>].


-spec motd(Target :: iodata()) -> iolist().

motd(Target) -> [<<"MOTD ">>, Target, <<"\r\n">>].

%%--------------------------------------------------------------------
%% NAMES command.
%%
%% RFC 1459:
%%
%%    Command: NAMES
%% Parameters: [<channel>{,<channel>}]
%%
%% RFC 2812:
%%
%%    Command: NAMES
%% Parameters: [ <channel> *( "," <channel> ) [ <target> ] ]
%%--------------------------------------------------------------------

-spec names() -> binary().

names() -> <<"NAMES\r\n">>.


-spec names(Channels :: [iodata()]) -> iolist().

names(Channels) -> [<<"NAMES ">>, lists:join(<<",">>, Channels), <<"\r\n">>].


-spec names(Channels :: [iodata()], Target :: iodata()) -> iolist().

names(Channels, Target) ->
    [<<"NAMES ">>, lists:join(<<",">>, Channels), <<" ">>, Target, <<"\r\n">>].

%%--------------------------------------------------------------------
%% NICK command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: NICK
%% Parameters: <nickname>
%%--------------------------------------------------------------------

-spec nick(Nickname :: iodata()) -> iolist().

nick(Nickname) -> [<<"NICK ">>, Nickname, <<"\r\n">>].

%%--------------------------------------------------------------------
%% NOTICE command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: NOTICE
%% Parameters: <msgtarget> <text>
%%
%% Some servers accept  multiple `Targets'.  This is  specified by the
%% `MAXTARGETS' parameter of the  ISUPPORT numeric. This functionality
%% is provided by the notice_many/2 function.
%%--------------------------------------------------------------------

-spec notice(Target :: iodata(), Text :: iodata()) -> iolist().

notice(Target, Text) -> [<<"NOTICE ">>, Target, <<" :">>, Text, <<"\r\n">>].


-spec notice_many(Targets :: [iodata()], Text :: iodata()) -> iolist().

notice_many(Targets, Text) ->
    [<<"NOTICE ">>, lists:join(<<",">>, Targets), <<" :">>, Text, <<"\r\n">>].

%%--------------------------------------------------------------------
%% OPER command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: OPER
%% Parameters: <user> <password>
%%
%%--------------------------------------------------------------------

-spec oper(User :: iodata(), Pass :: iodata()) -> iolist().

oper(User, Pass) -> [<<"OPER ">>, User, <<" ">>, Pass, <<"\r\n">>].

%%--------------------------------------------------------------------
%% PART command.
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

-spec part(Channels :: [iodata()]) -> iolist().

part(Channels) -> [<<"PART ">>, lists:join(<<",">>, Channels), <<"\r\n">>].


-spec part(Channels :: [iodata()], Message :: iodata()) -> iolist().

part(Channels, Message) ->
    [<<"PART ">>, lists:join(<<",">>, Channels), <<" :">>, Message, <<"\r\n">>].

%%--------------------------------------------------------------------
%% PASS command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: PASS
%% Parameters: <password>
%%--------------------------------------------------------------------

pass(Pass) -> [<<"PASS ">>, Pass, <<"\r\n">>].

%%--------------------------------------------------------------------
%% PING command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: PING
%% Parameters: <server1> [<server2>]
%%--------------------------------------------------------------------

-spec ping(Server :: iodata()) -> iolist().

ping(Server) -> [<<"PING ">>, Server, <<"\r\n">>].


-spec ping(Server :: iodata(), Server2 :: iodata()) -> iolist().

ping(Server, Server2) -> [<<"PING ">>, Server, <<" ">>, Server2, <<"\r\n">>].


%%--------------------------------------------------------------------
%% PONG command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: PONG
%% Parameters: <server> [<server2>]
%%--------------------------------------------------------------------

-spec pong(Server :: iodata()) -> iolist().

pong(Server) -> [<<"PONG ">>, Server, <<"\r\n">>].


-spec pong(Server :: iodata(), Server2 :: iodata()) -> iolist().

pong(Server, Server2) -> [<<"PONG ">>, Server, <<" ">>, Server2, <<"\r\n">>].

%%--------------------------------------------------------------------
%% PRIVMSG command.
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
%%
%% Support  for  multiple  receivers   is  usually  specified  by  the
%% `MAXTARGETS' parameter of the  ISUPPORT numeric. This functionality
%% is provided by the privmsg_many/2 function.
%%--------------------------------------------------------------------

-spec privmsg(Target :: iodata(), Text :: iodata()) -> iolist().

privmsg(Target, Text) -> [<<"PRIVMSG ">>, Target, <<" :">>, Text, <<"\r\n">>].


-spec privmsg_many(Targets :: iodata(), Text :: iodata()) -> iolist().

privmsg_many(Targets, Text) ->
    [<<"PRIVMSG ">>, lists:join(<<",">>, Targets), <<" :">>, Text, <<"\r\n">>].

%%--------------------------------------------------------------------
%% QUIT command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: QUIT
%% Parameters: [<Quit message>]
%%--------------------------------------------------------------------

-spec quit() -> binary().

quit() -> <<"QUIT\r\n">>.


-spec quit(Message :: iodata()) -> iolist().

quit(Message) -> [<<"QUIT :">>, Message, <<"\r\n">>].

%%--------------------------------------------------------------------
%% REHASH command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: REHASH
%% Parameters:
%%--------------------------------------------------------------------

-spec rehash() -> binary().

rehash() -> <<"REHASH\r\n">>.

%%--------------------------------------------------------------------
%% RESTART command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: RESTART
%% Parameters:
%%--------------------------------------------------------------------

-spec restart() -> binary().

restart() -> <<"RESTART\r\n">>.

%%--------------------------------------------------------------------
%% SQUIT command.
%%
%% RFC 2812:
%%
%%    Command: SQUIT
%% Parameters: <server> <comment>
%%--------------------------------------------------------------------

-spec squit(Server :: iodata(), Comment :: iodata()) -> binary().

squit(Server, Comment) -> [<<"SQUIT ">>, Server, <<" :">>, Comment, <<"\r\n">>].

%%--------------------------------------------------------------------
%% STATS command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: STATS
%% Parameters: [<query> [<server>]]
%%--------------------------------------------------------------------

-spec stats(Query :: iodata()) -> iolist().

stats(Query) -> [<<"STATS ">>, Query, <<"\r\n">>].


-spec stats(Query :: iodata(), Server :: iodata()) -> iolist().

stats(Query, Server) -> [<<"STATS ">>, Query, <<" ">>, Server, <<"\r\n">>].

%%--------------------------------------------------------------------
%% TIME command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: TIME
%% Parameters: [<server>]
%%--------------------------------------------------------------------

-spec time() -> binary().

time() -> <<"TIME\r\n">>.


-spec time(Server :: iodata()) -> iolist().

time(Server) -> [<<"TIME ">>, Server, <<"\r\n">>].

%%--------------------------------------------------------------------
%% TOPIC command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: TOPIC
%% Parameters: <channel> [<topic>]
%%--------------------------------------------------------------------

-spec topic(Channel :: iodata()) -> iolist().

topic(Channel) -> [<<"TOPIC ">>, Channel, <<"\r\n">>].


-spec topic(Channel :: iodata(), Topic :: iodata()) -> iolist().

topic(Channel, Topic) -> [<<"TOPIC ">>, Channel, <<" :">>, Topic, <<"\r\n">>].

%%--------------------------------------------------------------------
%% TRACE command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: TRACE
%% Parameters: [<server>]
%%--------------------------------------------------------------------

-spec trace() -> binary().

trace() -> <<"TRACE\r\n">>.


-spec trace(Server :: iodata()) -> iolist().

trace(Server) -> [<<"TRACE ">>, Server, <<"\r\n">>].

%%--------------------------------------------------------------------
%% USER command.
%%
%% RFC 1459:
%%
%%    Command: USER
%% Parameters: <username> <hostname> <servername> <realname>
%%
%% RFC 2812:
%%
%%    Command: USER
%% Parameters: <user> <mode> <unused> <realname>
%%
%% Because  the  second  and   third  options  differ  between  server
%% implementations (and  the RFCs) you  usually use `0' as  the second
%% and `*' as the third argument.
%%--------------------------------------------------------------------

-spec user(User :: iodata(), X :: iodata(), Y :: iodata(), Real :: iodata())
          -> iolist().

user(User, X, Y, Real) ->
    [<<"USER ">>, User, <<" ">>, X, <<" ">>, Y, <<" :">>, Real, <<"\r\n">>].

%%--------------------------------------------------------------------
%% USERHOST command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: USERHOST
%% Parameters: <nickname>{<space><nickname>}
%%--------------------------------------------------------------------

-spec userhost(Nicknames :: [iodata()]) -> iolist().

userhost(Nicknames) ->
    [<<"USERHOST ">>, lists:join(<<" ">>, Nicknames), <<"\r\n">>].

%%--------------------------------------------------------------------
%% USERS command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: USERS
%% Parameters: [ <target> ]
%%--------------------------------------------------------------------

-spec users() -> binary().

users() -> <<"USERS\r\n">>.


-spec users(Server :: iodata()) -> iolist().

users(Server) -> [<<"USERS ">>, Server, <<"\r\n">>].

%%--------------------------------------------------------------------
%% VERSION command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: VERSION
%% Parameters: [ <target> ]
%%--------------------------------------------------------------------

-spec version() -> binary().

version() -> <<"VERSION\r\n">>.


-spec version(Server :: iodata()) -> iolist().

version(Server) -> [<<"VERSION ">>, Server, <<"\r\n">>].

%%--------------------------------------------------------------------
%% WHO command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: WHO
%% Parameters: [ <mask> [ "o" ] ]
%%--------------------------------------------------------------------

-spec who() -> binary().

who() -> <<"WHO\r\n">>.


-spec who(Mask :: iodata()) -> iolist().

who(Mask) -> [<<"WHO ">>, Mask, <<"\r\n">>].


-spec who(Mask :: iodata(), o) -> iolist().

who(Mask, o) -> [<<"WHO ">>, Mask, <<" o\r\n">>].

%%--------------------------------------------------------------------
%% WHOIS command.
%%
%% RFC 1459 / RFC 2812:
%%
%%    Command: WHOIS
%% Parameters: [ <target> ] <mask> *( "," <mask> )
%%--------------------------------------------------------------------

-spec whois(Masks :: [iodata()]) -> iolist().

whois(Masks) -> [<<"WHOIS ">>, lists:join(<<",">>, Masks), <<"\r\n">>].


-spec whois(Target :: iodata(), Masks :: [iodata()]) -> iolist().

whois(Target, Masks) ->
    [<<"WHOIS ">>, Target, <<" ">>, lists:join(<<",">>, Masks), <<"\r\n">>].

%%--------------------------------------------------------------------
%% WHOWAS command.
%%
%% RFC 1459:
%%
%%    Command: WHOWAS
%% Parameters: <nickname> [<count> [<server>]]
%%
%% RFC 2812:
%%
%%    Command: WHOWAS
%% Parameters: <nickname> *( "," <nickname> ) [ <count> [ <target> ] ]
%%--------------------------------------------------------------------

-spec whowas(Nicknames :: [iodata()]) -> iolist().

whowas(Nicknames) ->
    [<<"WHOWAS ">>, lists:join(<<",">>, Nicknames), <<"\r\n">>].


-spec whowas(Nicknames :: [iodata()], Count :: iodata()) -> iolist().

whowas(Nicknames, Count) ->
    N = lists:join(<<",">>, Nicknames),
    [<<"WHOWAS ">>, N, <<" ">>, Count, <<"\r\n">>].


-spec whowas(Nicknames :: [iodata()], Count :: iodata(), Target :: iodata())
            -> iolist().

whowas(Nicknames, Count, Target) ->
    N = lists:join(<<",">>, Nicknames),
    [<<"WHOWAS ">>, N, <<" ">>, Count, <<" ">>, Target, <<"\r\n">>].


%%%===================================================================
%%% IRCv3 defined commands
%%%===================================================================

%%% Capability Negotiation
%%%
%%% Spec: https://ircv3.net/specs/extensions/capability-negotiation
%%%
%%% CAP LS [<version>]
%%% CAP LIST
%%% CAP REQ *( ["-"] <capability> )
%%% CAP END

-spec cap_ls() -> binary().

cap_ls() -> <<"CAP LS\r\n">>.


-spec cap_ls(Version :: iodata()) -> iolist().

cap_ls(Version) -> [<<"CAP LS ">>, Version, <<"\r\n">>].


-spec cap_list() -> binary().

cap_list() -> <<"CAP LIST\r\n">>.


-spec cap_req(Caps :: [iodata()]) -> iolist().

cap_req(Caps) -> [<<"CAP REQ :">>, lists:join(<<" ">>, Caps), <<"\r\n">>].


-spec cap_end() -> binary().

cap_end() -> <<"CAP END\r\n">>.

%%% Message Tags
%%%
%%% Spec: https://ircv3.net/specs/extensions/message-tags
%%%
%%% TAGMSG <msgtarget>

-spec tagmsg(Msgtarget :: iodata()) -> iolist().

tagmsg(Msgtarget) -> [<<"TAGMSG ">>, Msgtarget, <<"\r\n">>].

%%% batch
%%%
%%% Spec: https://ircv3.net/specs/extensions/batch
%%%
%%% BATCH "+" <reference-tag> <type> *( <parameter> )
%%% BATCH "-" <reference-tag>

-spec batch_start(Tag :: iodata(), Type :: iodata(), Params :: [iodata()])
                 -> iolist().

batch_start(Tag, Type, Params) ->
    P = lists:join(<<" ">>, Params),
    [<<"BATCH +">>, Tag, <<" ">>, Type, <<" ">>, P, <<"\r\n">>].


-spec batch_end(Tag :: iodata()) -> iolist().

batch_end(Tag) -> [<<"BATCH -">>, Tag, <<"\r\n">>].

%%% Monitor
%%%
%%% Spec: https://ircv3.net/specs/extensions/monitor
%%%
%%% MONITOR + target[,target2]*
%%% MONITOR - target[,target2]*
%%% MONITOR C
%%% MONITOR L
%%% MONITOR S

-spec monitor_add(Targets :: [iodata()]) -> iolist().

monitor_add(Targets) ->
    [<<"MONITOR + ">>, lists:join(<<",">>, Targets), <<"\r\n">>].


-spec monitor_remove(Targets :: [iodata()]) -> iolist().

monitor_remove(Targets) ->
    [<<"MONITOR - ">>, lists:join(<<",">>, Targets), <<"\r\n">>].


-spec monitor_clear() -> binary().

monitor_clear() -> <<"MONITOR C\r\n">>.


-spec monitor_list() -> binary().

monitor_list() -> <<"MONITOR L\r\n">>.


-spec monitor_status() -> binary().

monitor_status() -> <<"MONITOR S\r\n">>.

%%% IRCv3.1 SASL Authentication
%%%
%%% Spec: https://ircv3.net/specs/extensions/sasl-3.1
%%%
%%% AUTHENTICATE SP mechanism CRLF
%%% *(AUTHENTICATE SP 400BASE64 CRLF) AUTHENTICATE SP (1*399BASE64 / "+") CRLF
%%% AUTHENTICATE SP "*" CRLF

-spec authenticate(plain | external | scram_sha_1 | empty | abort | iodata())
                  -> iodata().

authenticate(plain)       -> <<"AUTHENTICATE PLAIN\r\n">>;
authenticate(external)    -> <<"AUTHENTICATE EXTERNAL\r\n">>;
authenticate(scram_sha_1) -> <<"AUTHENTICATE SCRAM-SHA-1\r\n">>;
authenticate(empty)       -> <<"AUTHENTICATE +\r\n">>;
authenticate(abort)       -> <<"AUTHENTICATE *\r\n">>;
authenticate(Data)        -> [<<"AUTHENTICATE ">>, Data, <<"\r\n">>].

%%% setname
%%%
%%% Spec: https://ircv3.net/specs/extensions/setname
%%%
%%% SETNAME :realname here

-spec setname(Realname :: iodata()) -> iolist().

setname(Realname) -> [<<"SETNAME :">>, Realname, <<"\r\n">>].


%%%===================================================================
%%% Client-to-Client Protocol (CTCP)
%%%===================================================================

%%% Spec: https://datatracker.ietf.org/doc/html/draft-oakley-irc-ctcp-02
%%%
%%% CTCP  depends  on the  PRIVMSG  command.   The provided  functions
%%% construct  a PRIVMSG  command that  includes a  CTCP query  in its
%%% trailing parameter.

-spec ctcp(Target :: iodata(), Command :: iodata()) -> iolist().

ctcp(Target, Command) -> privmsg(Target, [1, Command, 1]).


-spec ctcp(Target :: iodata(), Command :: iodata(), Params :: iodata())
          -> iolist().

ctcp(Target, Command, Params) ->
    privmsg(Target, [1, Command, <<" ">>, Params, 1]).

%%% Below are implementations of a few commonly implemented CTCP commands.

-spec ctcp_action(Target :: iodata(), Text :: iodata()) -> iolist().

ctcp_action(Target, Text) -> ctcp(Target, <<"ACTION">>, Text).


-spec ctcp_clientinfo(Target :: iodata()) -> iolist().

ctcp_clientinfo(Target) -> ctcp(Target, <<"CLIENTINFO">>).


-spec ctcp_clientinfo(Target :: iodata(), Tokens :: [iodata()]) -> iolist().

ctcp_clientinfo(Target, Tokens) ->
    ctcp(Target, <<"CLIENTINFO">>, lists:join(<<" ">>, Tokens)).


%% @todo DCC


-spec ctcp_ping(Target :: iodata(), Info :: iodata()) -> iolist().

ctcp_ping(Target, Info) -> ctcp(Target, <<"PING">>, Info).


-spec ctcp_source(Target :: iodata()) -> iolist().

ctcp_source(Target) -> ctcp(Target, <<"SOURCE">>).


-spec ctcp_source(Target :: iodata(), Info :: iodata()) -> iolist().

ctcp_source(Target, Info) -> ctcp(Target, <<"SOURCE">>, Info).


-spec ctcp_time(Target :: iodata()) -> iolist().

ctcp_time(Target) -> ctcp(Target, <<"TIME">>).


-spec ctcp_time(Target :: iodata(), Timestring :: iodata()) -> iolist().

ctcp_time(Target, Timestring) -> ctcp(Target, <<"TIME">>, Timestring).


-spec ctcp_version(Target :: iodata()) -> iolist().

ctcp_version(Target) -> ctcp(Target, <<"VERSION">>).


-spec ctcp_version(Target :: iodata(), Verstring :: iodata()) -> iolist().

ctcp_version(Target, Verstring) -> ctcp(Target, <<"VERSION">>, Verstring).
