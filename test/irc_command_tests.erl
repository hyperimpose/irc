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

-module(irc_command_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% RFC defined commands
%%%===================================================================

%%% ADMIN

admin_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:admin()), <<"ADMIN\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:admin([<<"te">>, "s", ["t"]])),
                   <<"ADMIN test\r\n">>)].

%%% AWAY

away1_test() ->
    M = irc_command:away(),
    B = iolist_to_binary(M),
    ?assertEqual(B, <<"AWAY\r\n">>).

away2_test() ->
    M = irc_command:away([<<"te">>, "s", ["t"]]),
    B = iolist_to_binary(M),
    ?assertEqual(B, <<"AWAY :test\r\n">>).

%%% CONNECT

connect_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:connect([<<"A">>])),
                   <<"CONNECT A\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:connect([<<"A">>], "B")),
                   <<"CONNECT A B\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:connect([<<"A">>], "B", "C")),
                   <<"CONNECT A B C\r\n">>)].

%%% DIE

die_test() ->
    M = irc_command:die(),
    B = iolist_to_binary(M),
    ?assertEqual(B, <<"DIE\r\n">>).

%%% INFO

info_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:info()), <<"INFO\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:info([<<"A">>])),
                   <<"INFO A\r\n">>)].

%%% INVITE

invite_test() ->
    ?assertEqual(iolist_to_binary(irc_command:invite("A", "B")),
                 <<"INVITE A B\r\n">>).

%%% ISON

ison_test() ->
    ?assertEqual(iolist_to_binary(irc_command:ison(["A", ["B"]])),
                 <<"ISON A B\r\n">>).

%%% JOIN

join_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:join(0)), <<"JOIN 0\r\n">>),
     %% C3 and C4 are intentionally positioned like that to test if the function
     %% will reposition them after the channels that have keys.
     ?_assertEqual(iolist_to_binary(irc_command:join(#{"C3" => "",
                                                       ["C1"] => "K1",
                                                       <<"C4">> => <<>>,
                                                       "C2" => <<"K2">>})),
                   <<"JOIN C1,C2,C3 K1,K2\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:join(#{"C1" => ""})),
                   <<"JOIN C1\r\n">>)].

%%% KICK

kick_test_() ->
     [?_assertEqual(iolist_to_binary(irc_command:kick("#chan", ["1", <<"2">>])),
                   <<"KICK #chan 1,2\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:kick(
                                      "#chan", "1", "comment")),
                   <<"KICK #chan 1 :comment\r\n">>)].

%%% KILL

kill_test() ->
    ?assertEqual(iolist_to_binary(irc_command:kill("Nick", ["comment"])),
                 <<"KILL Nick :comment\r\n">>).

%%% LINKS

links_test_() ->
     [?_assertEqual(iolist_to_binary(irc_command:links()), <<"LINKS\r\n">>),
      ?_assertEqual(iolist_to_binary(irc_command:links("Mask")),
                    <<"LINKS Mask\r\n">>),
      ?_assertEqual(iolist_to_binary(irc_command:links("Remote", "Mask")),
                    <<"LINKS Remote, Mask\r\n">>)].

%%% LIST

list_test_() ->
     [?_assertEqual(iolist_to_binary(irc_command:list()), <<"LIST\r\n">>),
      ?_assertEqual(iolist_to_binary(irc_command:list(["C1", ["C", "2"]])),
                    <<"LIST C1,C2\r\n">>),
      ?_assertEqual(iolist_to_binary(irc_command:list(["C1", ["C", "2"]], "S")),
                    <<"LIST C1,C2 S\r\n">>)].

%%% LUSERS

lusers_test_() ->
     [?_assertEqual(iolist_to_binary(irc_command:lusers()), <<"LUSERS\r\n">>),
      ?_assertEqual(iolist_to_binary(irc_command:lusers(["Mask"])),
                    <<"LUSERS Mask\r\n">>),
      ?_assertEqual(iolist_to_binary(irc_command:lusers(["Mask"], "Target")),
                    <<"LUSERS Mask Target\r\n">>)].

%%% MODE

mode_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:mode("test")),
                   <<"MODE test\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:mode(["t", "est"], "+o ok")),
                   <<"MODE test +o ok\r\n">>)].

%%% MOTD

motd_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:motd()), <<"MOTD\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:motd(["tar", "get"])),
                   <<"MOTD target\r\n">>)].

%%% NAMES

names_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:names()), <<"NAMES\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:names(["#a", "#b"])),
                   <<"NAMES #a,#b\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:names(["#a", "#b"], "target")),
                   <<"NAMES #a,#b target\r\n">>)].

%%% NICK

nick_test() ->
    ?assertEqual(iolist_to_binary(irc_command:nick("N")), <<"NICK N\r\n">>).

%%% NOTICE

notice_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:notice("#c", "text a b")),
                   <<"NOTICE #c :text a b\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:notice_many(
                                      ["#a", "#b"], "text a b")),
                   <<"NOTICE #a,#b :text a b\r\n">>)].

%%% OPER

oper_test() ->
    ?assertEqual(iolist_to_binary(irc_command:oper("User", ["12", "2"])),
                 <<"OPER User 122\r\n">>).

%%% PART

part_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:part(["#a", <<"#b">>])),
                   <<"PART #a,#b\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:part(["#a", <<"#b">>], "msg")),
                   <<"PART #a,#b :msg\r\n">>)].

%%% PASS

pass_test() ->
    ?assertEqual(iolist_to_binary(irc_command:pass(["12", "2"])),
                 <<"PASS 122\r\n">>).

%%% PING

ping_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:ping(["a"])),
                   <<"PING a\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:ping("a", [<<"b">>])),
                   <<"PING a b\r\n">>)].

%%% PONG

pong_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:pong(["a"])),
                   <<"PONG a\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:pong("a", [<<"b">>])),
                   <<"PONG a b\r\n">>)].

%%% PRIVMSG

privmsg_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:privmsg("#c", "text a b")),
                   <<"PRIVMSG #c :text a b\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:privmsg_many(
                                      ["#a", "#b"], "text a b")),
                   <<"PRIVMSG #a,#b :text a b\r\n">>)].

%%% QUIT

quit_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:quit()), <<"QUIT\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:quit(["m", "sg"])),
                   <<"QUIT :msg\r\n">>)].

%%% REHASH

rehash_test() ->
    ?assertEqual(iolist_to_binary(irc_command:rehash()), <<"REHASH\r\n">>).

%%% RESTART

restart_test() ->
    ?assertEqual(iolist_to_binary(irc_command:restart()), <<"RESTART\r\n">>).

%%% SQUIT

squit_test() ->
    ?assertEqual(iolist_to_binary(irc_command:squit("serv", "comment")),
                 <<"SQUIT serv :comment\r\n">>).

%%% STATS

stats_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:stats("q")), <<"STATS q\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:stats("q", "server")),
                   <<"STATS q server\r\n">>)].

%%% TIME

time_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:time()), <<"TIME\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:time("server")),
                   <<"TIME server\r\n">>)].

%%% TOPIC

topic_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:topic("#channel")),
                   <<"TOPIC #channel\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:topic("#channel", "text a")),
                   <<"TOPIC #channel :text a\r\n">>)].

%%% TRACE

trace_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:trace()), <<"TRACE\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:trace("server")),
                   <<"TRACE server\r\n">>)].

%%% USER

user_test_() ->
    ?assertEqual(iolist_to_binary(irc_command:user("user", "0", "*", "real")),
                 <<"USER user 0 * :real\r\n">>).

%%% USERHOST

userhost_test_() ->
    ?assertEqual(iolist_to_binary(irc_command:userhost(["n1", "n2"])),
                 <<"USERHOST n1 n2\r\n">>).

%%% USERS

users_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:users()), <<"USERS\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:users("server")),
                   <<"USERS server\r\n">>)].

%%% VERSION

version_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:version()), <<"VERSION\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:version("server")),
                   <<"VERSION server\r\n">>)].

%%% WHO

who_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:who()), <<"WHO\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:who("mask")),
                   <<"WHO mask\r\n">>),
    ?_assertEqual(iolist_to_binary(irc_command:who("mask", o)),
                   <<"WHO mask o\r\n">>)].

%%% WHOIS

whois_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:whois(["m1", "m2"])),
                   <<"WHOIS m1,m2\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:whois("target", ["m1", "m2"])),
                   <<"WHOIS target m1,m2\r\n">>)].

%%% WHOWAS

whowas_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:whowas(["n1", "n2"])),
                   <<"WHOWAS n1,n2\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:whowas(["n1", "n2"], "2")),
                   <<"WHOWAS n1,n2 2\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:whowas(["n1", "n2"], "2", "t")),
                   <<"WHOWAS n1,n2 2 t\r\n">>)].


%%%===================================================================
%%% IRCv3 defined commands
%%%===================================================================

cap_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:cap_ls()), <<"CAP LS\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:cap_ls("301")),
                   <<"CAP LS 301\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:cap_list()),
                   <<"CAP LIST\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:cap_req(["re1", <<"re2">>])),
                   <<"CAP REQ :re1 re2\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:cap_end()),
                   <<"CAP END\r\n">>)].

tagmsg_test() ->
    ?assertEqual(iolist_to_binary(irc_command:tagmsg("target")),
                 <<"TAGMSG target\r\n">>).

batch_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:batch_start("yXNAbvnRHTRBv",
                                                            "netsplit",
                                                            ["irc", "other"])),
                   <<"BATCH +yXNAbvnRHTRBv netsplit irc other\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:batch_end("yXNAbvnRHTRBv")),
                   <<"BATCH -yXNAbvnRHTRBv\r\n">>)].

monitor_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:monitor_add(["a", <<"b">>])),
                   <<"MONITOR + a,b\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:monitor_remove(["a", <<"b">>])),
                   <<"MONITOR - a,b\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:monitor_clear()),
                   <<"MONITOR C\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:monitor_list()),
                   <<"MONITOR L\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:monitor_status()),
                   <<"MONITOR S\r\n">>)].

authenticate_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:authenticate(plain)),
                   <<"AUTHENTICATE PLAIN\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:authenticate(external)),
                   <<"AUTHENTICATE EXTERNAL\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:authenticate(scram_sha_1)),
                   <<"AUTHENTICATE SCRAM-SHA-1\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:authenticate(empty)),
                   <<"AUTHENTICATE +\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:authenticate(abort)),
                   <<"AUTHENTICATE *\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:authenticate(["dat", <<"a">>])),
                   <<"AUTHENTICATE data\r\n">>)].

setname_test() ->
    ?assertEqual(iolist_to_binary(irc_command:setname("realname here")),
                 <<"SETNAME :realname here\r\n">>).

%%%===================================================================
%%% CTCP
%%%===================================================================

ctcp_test_() ->
    [?_assertEqual(iolist_to_binary(irc_command:ctcp("#chan", "cmd")),
                   <<"PRIVMSG #chan :", 1, "cmd", 1, "\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:ctcp("#chan", "cmd", "params")),
                   <<"PRIVMSG #chan :", 1, "cmd params", 1, "\r\n">>),
     %% The following should insert a SP after the command even if
     %% there are no parameters.
     ?_assertEqual(iolist_to_binary(irc_command:ctcp("#chan", "cmd", "")),
                   <<"PRIVMSG #chan :", 1, "cmd ", 1, "\r\n">>),
     %% ACTION
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_action("#chan", "msg")),
                   <<"PRIVMSG #chan :", 1, "ACTION msg", 1, "\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_action("#chan", "")),
                   <<"PRIVMSG #chan :", 1, "ACTION ", 1, "\r\n">>),
     %% CLIENTINFO
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_clientinfo("user")),
                   <<"PRIVMSG user :", 1, "CLIENTINFO", 1, "\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_clientinfo(
                                      "user", ["a", "b"])),
                   <<"PRIVMSG user :", 1, "CLIENTINFO a b", 1, "\r\n">>),
     %% PING
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_ping("user", ["a", "b"])),
                   <<"PRIVMSG user :", 1, "PING ab", 1, "\r\n">>),
     %% SOURCE
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_source("user")),
                   <<"PRIVMSG user :", 1, "SOURCE", 1, "\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_source("user", "ab")),
                   <<"PRIVMSG user :", 1, "SOURCE ab", 1, "\r\n">>),
     %% TIME
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_time("user")),
                   <<"PRIVMSG user :", 1, "TIME", 1, "\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_time("user", "ab")),
                   <<"PRIVMSG user :", 1, "TIME ab", 1, "\r\n">>),
     %% VERSION
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_version("user")),
                   <<"PRIVMSG user :", 1, "VERSION", 1, "\r\n">>),
     ?_assertEqual(iolist_to_binary(irc_command:ctcp_version("user", "ab")),
                   <<"PRIVMSG user :", 1, "VERSION ab", 1, "\r\n">>)].
