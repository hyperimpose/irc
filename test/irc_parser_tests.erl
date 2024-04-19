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

-module(irc_parser_tests).

-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Tests adapted from: https://github.com/ircdocs/parser-tests
%%%===================================================================

%%% msg-split.yaml
%%% PL: https://github.com/ircdocs/parser-tests/blob/
%%%     a1c8e14fb6699b3d34a14cc7672d6f34c917e412/tests/msg-split.yaml

t01_test() ->
    {ok, M} = irc_parser:message(<<"cmd bar baz asdf">>),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>),
    ?assertEqual(irc_parser:get_params(M), [<<"bar">>, <<"baz">>, <<"asdf">>]).

t02_test() ->
    {ok, M} = irc_parser:message(<<":nick cmd bar baz asdf">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"nick">>),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>),
    ?assertEqual(irc_parser:get_params(M), [<<"bar">>, <<"baz">>, <<"asdf">>]).

t03_test() ->
    {ok, M} = irc_parser:message(<<"cmd bar baz :asdf ghjk">>),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>),
    ?assertEqual(irc_parser:get_params(M),
                 [<<"bar">>, <<"baz">>, <<"asdf ghjk">>]).

t04_test() ->
    {ok, M} = irc_parser:message(<<"cmd bar baz :">>),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>),
    ?assertEqual(irc_parser:get_params(M), [<<"bar">>, <<"baz">>, <<>>]).

t05_test() ->
    {ok, M} = irc_parser:message(<<"cmd bar baz ::asdf">>),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>),
    ?assertEqual(irc_parser:get_params(M), [<<"bar">>, <<"baz">>, <<":asdf">>]).

t06_test() ->
    {ok, M} = irc_parser:message(<<":nick cmd bar baz :asdf ghjk">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"nick">>),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>),
    ?assertEqual(irc_parser:get_params(M),
                 [<<"bar">>, <<"baz">>, <<"asdf ghjk">>]).

t07_test() ->
    {ok, M} = irc_parser:message(<<":nick cmd bar baz :  asdf quux ">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"nick">>),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>),
    ?assertEqual(irc_parser:get_params(M),
                 [<<"bar">>, <<"baz">>, <<"  asdf quux ">>]).

t08_test() ->
    {ok, M} = irc_parser:message(<<":nick PRIVMSG bar :lol :) ">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"nick">>),
    ?assertEqual(irc_parser:get_command(M), <<"PRIVMSG">>),
    ?assertEqual(irc_parser:get_params(M), [<<"bar">>, <<"lol :) ">>]).

t09_test() ->
    {ok, M} = irc_parser:message(<<":nick cmd bar baz :">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"nick">>),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>),
    ?assertEqual(irc_parser:get_params(M), [<<"bar">>, <<"baz">>, <<>>]).

t10_test() ->
    {ok, M} = irc_parser:message(<<":nick cmd bar baz :  ">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"nick">>),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>),
    ?assertEqual(irc_parser:get_params(M), [<<"bar">>, <<"baz">>, <<"  ">>]).

t11_test() ->
    {ok, M} = irc_parser:message(<<"@a=b;c=32;k;rt=ql7;+a=b;+bb cmd">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"a">> => <<"b">>,
                                           <<"c">> => <<"32">>,
                                           <<"k">> => <<>>,
                                           <<"rt">> => <<"ql7">>,
                                           <<"+a">> => <<"b">>,
                                           <<"+bb">> => <<>>}),
    ?assertEqual(irc_parser:get_command(M), <<"cmd">>).

%% Escaped tags

t12_test() -> {ok, M} =
    irc_parser:message(<<"@a=b\\\\and\\nk;c=72\\s45;d=gh\\:764 cm">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"a">> => <<"b\\and\nk">>,
                                           <<"c">> => <<"72 45">>,
                                           <<"d">> => <<"gh;764">>}),
    ?assertEqual(irc_parser:get_command(M), <<"cm">>).

t13_test() ->
    {ok, M} = irc_parser:message(<<"@c;h=;a=b :nick ab cd">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"c">> => <<>>,
                                           <<"h">> => <<>>,
                                           <<"a">> => <<"b">>}),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"nick">>),
    ?assertEqual(irc_parser:get_command(M), <<"ab">>),
    ?assertEqual(irc_parser:get_params(M), [<<"cd">>]).

%% Different forms of last param

t14_test() ->
    {ok, M} = irc_parser:message(<<":src JOIN #chan">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"src">>),
    ?assertEqual(irc_parser:get_command(M), <<"JOIN">>),
    ?assertEqual(irc_parser:get_params(M), [<<"#chan">>]).

t15_test() ->
    {ok, M} = irc_parser:message(<<":src JOIN :#chan">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"src">>),
    ?assertEqual(irc_parser:get_command(M), <<"JOIN">>),
    ?assertEqual(irc_parser:get_params(M), [<<"#chan">>]).

t16_test() ->
    {ok, M} = irc_parser:message(<<":src AWAY">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"src">>),
    ?assertEqual(irc_parser:get_command(M), <<"AWAY">>).

t17_test() ->
    {ok, M} = irc_parser:message(<<":src AWAY ">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"src">>),
    ?assertEqual(irc_parser:get_command(M), <<"AWAY">>),
    ?assertEqual(irc_parser:get_params(M), []).

%% Ensure that TAB \t is not parsed as <SPACE>. According to the RFC
%% 1459, section 2.3.1 <SPACE> is defined to be one or more SPACE \s
%% (16#20) characters.

t18_test() ->
    {ok, M} = irc_parser:message(<<":cool\tguy foo bar baz">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"cool\tguy">>),
    ?assertEqual(irc_parser:get_command(M), <<"foo">>),
    ?assertEqual(irc_parser:get_params(M), [<<"bar">>, <<"baz">>]).

%% Control characters in the source

t19_test() ->
    {ok, M} = irc_parser:message(<<":coolguy!ag@net\x035w\x03ork.admin"
                                   " PRIVMSG foo :bar baz">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"coolguy">>),
    ?assertEqual(irc_parser:get_prefix_user(M), <<"ag">>),
    ?assertEqual(irc_parser:get_prefix_host(M), <<"net\x035w\x03ork.admin">>),
    ?assertEqual(irc_parser:get_command(M), <<"PRIVMSG">>),
    ?assertEqual(irc_parser:get_params(M), [<<"foo">>, <<"bar baz">>]).

t20_test() ->
    {ok, M} = irc_parser:message(<<":coolguy!~ag@n\x02et\x0305w\x0fork.admin"
                                   " PRIVMSG foo :bar baz">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"coolguy">>),
    ?assertEqual(irc_parser:get_prefix_user(M), <<"~ag">>),
    ?assertEqual(irc_parser:get_prefix_host(M),
                 <<"n\x02et\x0305w\x0fork.admin">>),
    ?assertEqual(irc_parser:get_command(M), <<"PRIVMSG">>),
    ?assertEqual(irc_parser:get_params(M), [<<"foo">>, <<"bar baz">>]).

t21_test() ->
    {ok, M} = irc_parser:message(
                <<"@tag1=value1;tag2;vendor1/tag3=value2;vendor2/tag4="
                  " :irc.example.com COMMAND param1 param2 :param3 param3">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"tag1">> => <<"value1">>,
                                           <<"tag2">> => <<>>,
                                           <<"vendor1/tag3">> => <<"value2">>,
                                           <<"vendor2/tag4">> => <<>>}),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"irc.example.com">>),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>),
    ?assertEqual(irc_parser:get_params(M),
                 [<<"param1">>, <<"param2">>, <<"param3 param3">>]).

t22_test() ->
    {ok, M} = irc_parser:message(<<":irc.example.com COMMAND"
                                   " param1 param2 :param3 param3">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"irc.example.com">>),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>),
    ?assertEqual(irc_parser:get_params(M),
                 [<<"param1">>, <<"param2">>, <<"param3 param3">>]).

t23_test() ->
    {ok, M} = irc_parser:message(
                <<"@tag1=value1;tag2;vendor1/tag3=value2;vendor2/tag4"
                  " COMMAND param1 param2 :param3 param3">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"tag1">> => <<"value1">>,
                                           <<"tag2">> => <<>>,
                                           <<"vendor1/tag3">> => <<"value2">>,
                                           <<"vendor2/tag4">> => <<>>}),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>),
    ?assertEqual(irc_parser:get_params(M),
                 [<<"param1">>, <<"param2">>, <<"param3 param3">>]).

t24_test() ->
    {ok, M} = irc_parser:message(<<"COMMAND">>),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>).

t25_test() ->
    {ok, M} = irc_parser:message(<<"@foo=\\\\\\\\\\:\\\\s\\s\\r\\n COMMAND">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"foo">> => <<"\\\\;\\s \r\n">>}),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>).

%% More than one SPACE (16#20) characters after the <command>.

t26_test() ->
    {ok, M} = irc_parser:message(
                <<":gravel.mozilla.org 432  #momo"
                  " :Erroneous Nickname: Illegal characters">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"gravel.mozilla.org">>),
    ?assertEqual(irc_parser:get_command(M), <<"432">>),
    ?assertEqual(irc_parser:get_params(M),
                 [<<"#momo">>, <<"Erroneous Nickname: Illegal characters">>]).

%% SPACE (16#20) after the last <middle> param. Ensure that no empty
%% param is created.

t27_test() ->
    {ok, M} = irc_parser:message(<<":gravel.mozilla.org MODE #tckk +n ">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"gravel.mozilla.org">>),
    ?assertEqual(irc_parser:get_command(M), <<"MODE">>),
    ?assertEqual(irc_parser:get_params(M), [<<"#tckk">>, <<"+n">>]).

%% Same as above but end with CRLF.

t28_test() ->
    {ok, M} = irc_parser:message(<<":gravel.mozilla.org MODE #tckk +n \r\n">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"gravel.mozilla.org">>),
    ?assertEqual(irc_parser:get_command(M), <<"MODE">>),
    ?assertEqual(irc_parser:get_params(M), [<<"#tckk">>, <<"+n">>]).

%% Same as above but with multiple SPACE (16#20) characters.

t29_test() ->
    {ok, M} = irc_parser:message(
                <<":services.esper.net MODE #foo-bar +o foobar  ">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"services.esper.net">>),
    ?assertEqual(irc_parser:get_command(M), <<"MODE">>),
    ?assertEqual(irc_parser:get_params(M),
                 [<<"#foo-bar">>, <<"+o">>, <<"foobar">>]).

t30_test() ->
    {ok, M} = irc_parser:message(<<"@tag1=value\\\\ntest COMMAND">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"tag1">> => <<"value\\ntest">>}),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>).

%% Ensure that invalid escape sequences in <tags> result in the parser
%% dropping the backslash \

t31_test() ->
    {ok, M} = irc_parser:message(<<"@tag1=value\\1 COMMAND">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"tag1">> => <<"value1">>}),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>).

%% Slashes at the end of a tag value should be dropped.

t32_test() ->
    {ok, M} = irc_parser:message(<<"@tag1=value1\\ COMMAND">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"tag1">> => <<"value1">>}),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>).

%% If there are duplicate tag keys, only the last occurence should be kept.

t33_test() ->
    {ok, M} = irc_parser:message(<<"@tag1=1;tag2=3;tag3=4;tag1=5 COMMAND">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"tag1">> => <<"5">>,
                                           <<"tag2">> => <<"3">>,
                                           <<"tag3">> => <<"4">>}),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>).

%% Vendor tags can have the same name as standard tags.

t34_test() ->
    {ok, M} = irc_parser:message(
                <<"@tag1=1;tag2=3;tag3=4;tag1=5;vendor/tag2=8 COMMAND">>),
    ?assertEqual(irc_parser:get_tags(M), #{<<"tag1">> => <<"5">>,
                                           <<"tag2">> => <<"3">>,
                                           <<"tag3">> => <<"4">>,
                                           <<"vendor/tag2">> => <<"8">>}),
    ?assertEqual(irc_parser:get_command(M), <<"COMMAND">>).

%% Ensure that the parser does not consider <middle> and <trailing> as
%% different <params> types.

t35_test() ->
    {ok, M} = irc_parser:message(<<":SomeOp MODE #channel :+i">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"SomeOp">>),
    ?assertEqual(irc_parser:get_command(M), <<"MODE">>),
    ?assertEqual(irc_parser:get_params(M), [<<"#channel">>, <<"+i">>]).

t36_test() ->
    {ok, M} = irc_parser:message(
                <<":SomeOp MODE #channel +oo SomeUser :AnotherUser">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"SomeOp">>),
    ?assertEqual(irc_parser:get_command(M), <<"MODE">>),
    ?assertEqual(irc_parser:get_params(M),
                 [<<"#channel">>, <<"+oo">>, <<"SomeUser">>,
                  <<"AnotherUser">>]).


%%% userhost-split.yaml
%%% PL: https://github.com/ircdocs/parser-tests/blob/
%%%     a1c8e14fb6699b3d34a14cc7672d6f34c917e412/tests/userhost-split.yaml

t37_test() ->
    {ok, M} = irc_parser:message(<<":coolguy CMD">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"coolguy">>).

t38_test() ->
    {ok, M} = irc_parser:message(<<":coolguy!ag@127.0.0.1 CMD">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"coolguy">>),
    ?assertEqual(irc_parser:get_prefix_user(M), <<"ag">>),
    ?assertEqual(irc_parser:get_prefix_host(M), <<"127.0.0.1">>).

t39_test() ->
    {ok, M} = irc_parser:message(<<":coolguy!~ag@localhost CMD">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"coolguy">>),
    ?assertEqual(irc_parser:get_prefix_user(M), <<"~ag">>),
    ?assertEqual(irc_parser:get_prefix_host(M), <<"localhost">>).

%% Missing user

t40_test() ->
    {ok, M} = irc_parser:message(<<":coolguy@127.0.0.1 CMD">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"coolguy">>),
    ?assertEqual(irc_parser:get_prefix_user(M), undefined),
    ?assertEqual(irc_parser:get_prefix_host(M), <<"127.0.0.1">>).

%% Missing host

t41_test() ->
    {ok, M} = irc_parser:message(<<":coolguy!ag CMD">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"coolguy">>),
    ?assertEqual(irc_parser:get_prefix_user(M), <<"ag">>),
    ?assertEqual(irc_parser:get_prefix_host(M), undefined).

%% With control codes

t42_test() ->
    {ok, M} = irc_parser:message(<<":coolguy!ag@net\x035w\x03ork.admin CMD">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"coolguy">>),
    ?assertEqual(irc_parser:get_prefix_user(M), <<"ag">>),
    ?assertEqual(irc_parser:get_prefix_host(M), <<"net\x035w\x03ork.admin">>).

t43_test() ->
    {ok, M} = irc_parser:message(
                <<":coolguy!~ag@n\x02et\x0305w\x0fork.admin CMD">>),
    ?assertEqual(irc_parser:get_prefix_nick(M), <<"coolguy">>),
    ?assertEqual(irc_parser:get_prefix_user(M), <<"~ag">>),
    ?assertEqual(irc_parser:get_prefix_host(M),
                 <<"n\x02et\x0305w\x0fork.admin">>).


%%%===================================================================
%%% Message details
%%%===================================================================

%%% INVITE

invite1_test() ->
    {ok, M} = irc_parser:message(<<":prefix INVITE nick #channel\r\n">>),
    ?assertEqual(irc_parser:invite(M), {ok, <<"nick">>, <<"#channel">>}).

%%% JOIN

join1_test() ->
    {ok, M} = irc_parser:message(<<":prefix JOIN #channel\r\n">>),
    ?assertEqual(irc_parser:join(M), {ok, [<<"#channel">>]}).

join2_test() ->
    {ok, M} = irc_parser:message(<<":prefix JOIN #chan1,#chan2,#chan3\r\n">>),
    ?assertEqual(irc_parser:join(M),
                 {ok, [<<"#chan1">>, <<"#chan2">>, <<"#chan3">>]}).

join3_test() ->
    {ok, M} = irc_parser:message(
                <<":prefix JOIN #chan1 accountname :Real Name\r\n">>),
    ?assertEqual(irc_parser:join(M),
                 {ok, [<<"#chan1">>], <<"accountname">>, <<"Real Name">>}).

join4_test() ->
    {ok, M} = irc_parser:message(
                <<":prefix JOIN #chan1,#chan2 accountname :Real Name\r\n">>),
    ?assertEqual(irc_parser:join(M),
                 {ok, [<<"#chan1">>, <<"#chan2">>],
                  <<"accountname">>, <<"Real Name">>}).

%%% KICK

kick1_test() ->
    {ok, M} = irc_parser:message(<<":prefix KICK #channel nick\r\n">>),
    ?assertEqual(irc_parser:kick(M), {ok, <<"#channel">>, <<"nick">>, <<>>}).

kick2_test() ->
    {ok, M} = irc_parser:message(<<":prefix KICK #channel nick :abc de\r\n">>),
    ?assertEqual(irc_parser:kick(M),
                 {ok, <<"#channel">>, <<"nick">>, <<"abc de">>}).

%%% KILL

kill1_test() ->
    {ok, M} = irc_parser:message(<<":prefix KILL nick :abc de\r\n">>),
    ?assertEqual(irc_parser:kill(M), {ok, <<"nick">>, <<"abc de">>}).

%%% MODE

%% todo

%%% NICK

nick1_test() ->
    {ok, M} = irc_parser:message(<<":prefix NICK nick\r\n">>),
    ?assertEqual(irc_parser:nick(M), {ok, <<"nick">>}).

%%% NOTICE

%% todo

%%% PART

part1_test() ->
    {ok, M} = irc_parser:message(<<":prefix PART #channel\r\n">>),
    ?assertEqual(irc_parser:part(M), {ok, [<<"#channel">>], <<>>}).

part2_test() ->
    {ok, M} = irc_parser:message(<<":prefix PART #channel1 :abc de\r\n">>),
    ?assertEqual(irc_parser:part(M),
                 {ok, [<<"#channel1">>], <<"abc de">>}).

part3_test() ->
    {ok, M} = irc_parser:message(<<":prefix PART #channel1,#channel2\r\n">>),
    ?assertEqual(irc_parser:part(M),
                 {ok, [<<"#channel1">>, <<"#channel2">>], <<>>}).

part4_test() ->
    {ok, M} = irc_parser:message(
                <<":prefix PART #channel1,#channel2 :abc de\r\n">>),
    ?assertEqual(irc_parser:part(M),
                 {ok, [<<"#channel1">>, <<"#channel2">>], <<"abc de">>}).

%%% PING

ping1_test() ->
    {ok, M} = irc_parser:message(<<"PING server\r\n">>),
    ?assertEqual(irc_parser:ping(M), {ok, <<"server">>}).

ping2_test() ->
    {ok, M} = irc_parser:message(<<"PING server1 server2\r\n">>),
    ?assertEqual(irc_parser:ping(M), {ok, <<"server1">>, <<"server2">>}).

%%% PONG

pong1_test() ->
    {ok, M} = irc_parser:message(<<"PONG server\r\n">>),
    ?assertEqual(irc_parser:pong(M), {ok, <<"server">>}).

pong2_test() ->
    {ok, M} = irc_parser:message(<<"PONG server1 server2\r\n">>),
    ?assertEqual(irc_parser:pong(M), {ok, <<"server1">>, <<"server2">>}).

%%% PRIVMSG

%% todo

%%% QUIT

quit1_test() ->
    {ok, M} = irc_parser:message(<<"QUIT\r\n">>),
    ?assertEqual(irc_parser:quit(M), {ok, <<>>}).

quit2_test() ->
    {ok, M} = irc_parser:message(<<"QUIT :abc de\r\n">>),
    ?assertEqual(irc_parser:quit(M), {ok, <<"abc de">>}).

%%% TOPIC

%% todo check if the server actually returns topic, because it may
%% return a numeric

%%% WALLOPS

wallops1_test() ->
    {ok, M} = irc_parser:message(<<":prefix WALLOPS :abc de\r\n">>),
    ?assertEqual(irc_parser:wallops(M), {ok, <<"abc de">>}).


%%% IRCv3 ============================================================

%%% CAP LS

%% No capabilities

cap_ls1_test_() ->
    {ok, M1} = irc_parser:message(<<"CAP * LS :">>),
    %% Faulty server response. Not handled.
    %% Included here in case it is needed in the future.
    %% {ok, M2} = irc_parser:message(<<"CAP * :LS">>),

    [?_assertEqual(irc_parser:cap(M1), {cap_ls, <<"*">>, []})
     %% , ?_assertEqual(irc_parser:cap(M3), {cap_ls, <<"*">>, []})
    ].

%% Capabilities without values (301 support only)

cap_ls2_test() ->
    {ok, M1} = irc_parser:message(<<"CAP * LS :multi-prefix sasl">>),
    ?assertEqual(irc_parser:cap(M1),
                 {cap_ls, <<"*">>, [{<<"multi-prefix">>, <<>>},
                                    {<<"sasl">>, <<>>}]}).

%% Capabilities with values (302 support)

cap_ls3_test() ->
    {ok, M1} = irc_parser:message(<<"CAP * LS :batch sasl=PLAIN,EXTERNAL">>),
    ?assertEqual(irc_parser:cap(M1),
                 {cap_ls, <<"*">>, [{<<"batch">>, <<>>},
                                    {<<"sasl">>, <<"PLAIN,EXTERNAL">>}]}).

%% Multiline capabilities

cap_ls4_test() ->
    {ok, M1} = irc_parser:message(<<"CAP * LS * :multi-prefix extended-join">>),
    ?assertEqual(irc_parser:cap(M1),
                 {cap_ls_more, <<"*">>, [{<<"multi-prefix">>, <<>>},
                                         {<<"extended-join">>, <<>>}]}).

%% Multiple SPACE chars between capabilities

cap_ls5_test() ->
    {ok, M1} = irc_parser:message(<<"CAP * LS :   multi-prefix   sasl  ">>),
    ?assertEqual(irc_parser:cap(M1),
                 {cap_ls, <<"*">>, [{<<"multi-prefix">>, <<>>},
                                    {<<"sasl">>, <<>>}]}).

%%% CAP LIST

%% No capabilities

cap_list1_test_() ->
    {ok, M1} = irc_parser:message(<<"CAP * LIST :">>),
    %% Faulty server response. Not handled.
    %% Included here in case it is needed in the future.
    %% {ok, M2} = irc_parser:message(<<"CAP * :LIST">>),

    [?_assertEqual(irc_parser:cap(M1), {cap_list, <<"*">>, []})
     %% , ?_assertEqual(irc_parser:cap(M3), {cap_list, <<"*">>, []})
    ].

%% With capabilities

cap_list2_test() ->
    {ok, M1} = irc_parser:message(<<"CAP * LIST :multi-prefix">>),
    ?assertEqual(irc_parser:cap(M1), {cap_list, <<"*">>, [<<"multi-prefix">>]}).

%% Multiline capabilities

cap_list3_test() ->
    {ok, M1} = irc_parser:message(<<"CAP * LIST * :multi-prefix sasl">>),
    ?assertEqual(irc_parser:cap(M1),
                 {cap_list_more, <<"*">>, [ <<"multi-prefix">>, <<"sasl">>]}).

%%% CAP ACK

cap_ack1_test() ->
    {ok, M1} = irc_parser:message(<<"CAP * ACK :multi-prefix -sasl">>),
    ?assertEqual(irc_parser:cap(M1), {cap_ack, <<"*">>, [<<"multi-prefix">>,
                                                         <<"-sasl">>]}).

%%% CAP NAK

cap_nak1_test() ->
    {ok, M1} = irc_parser:message(<<"CAP * NAK :multi-prefix -sasl">>),
    ?assertEqual(irc_parser:cap(M1), {cap_nak, <<"*">>, [<<"multi-prefix">>,
                                                         <<"-sasl">>]}).

%%% CAP NEW

cap_new1_test() ->
    {ok, M1} = irc_parser:message(<<":irc.example.com CAP nick NEW :batch"
                                    " sasl=PLAIN,EXTERNAL">>),
    ?assertEqual(irc_parser:cap(M1), {cap_new, <<"nick">>,
                                      [{<<"batch">>, <<>>},
                                       {<<"sasl">>, <<"PLAIN,EXTERNAL">>}]}).

%%% CAP DEL

cap_del1_test() ->
    {ok, M1} = irc_parser:message(
                 <<":irc.example.com CAP nick DEL :batch sasl">>),
    ?assertEqual(irc_parser:cap(M1),
                 {cap_del, <<"nick">>, [<<"batch">>, <<"sasl">>]}).

%%% ACCOUNT (from the account-notify spec)

account1_test() ->
    {ok, M1} = irc_parser:message(<<":nick!user@host ACCOUNT accountname">>),
    ?assertEqual(irc_parser:account(M1), {ok, <<"accountname">>}).

%%% AWAY (from the away-notify spec)

away1_test() ->
    {ok, M1} = irc_parser:message(<<":nick!user@host AWAY">>),
    ?assertEqual(irc_parser:away(M1), not_away).

away2_test() ->
    {ok, M1} = irc_parser:message(<<":nick!user@host AWAY message">>),
    ?assertEqual(irc_parser:away(M1), {away, <<"message">>}).

away3_test() ->
    {ok, M1} = irc_parser:message(<<":nick!user@host AWAY :message 123">>),
    ?assertEqual(irc_parser:away(M1), {away, <<"message 123">>}).

%%% BATCH (from the batch spec)

%% Start batch with params

batch1_test() ->
    {ok, M1} = irc_parser:message(
                 <<":irc.host BATCH +123 type param1 param2">>),
    ?assertEqual(irc_parser:batch(M1),
                 {ok, <<"123">>, <<"type">>, [<<"param1">>, <<"param2">>]}).

%% Start batch without params

batch2_test() ->
    {ok, M1} = irc_parser:message(
                 <<":irc.host BATCH +123 type">>),
    ?assertEqual(irc_parser:batch(M1), {ok, <<"123">>, <<"type">>, []}).

%% End batch

batch3_test() ->
    {ok, M1} = irc_parser:message(<<":irc.host BATCH -123">>),
    ?assertEqual(irc_parser:batch(M1), {ok, <<"123">>}).

%%% CHGHOST (from the chghost spec)

chghost1_test() ->
    {ok, M1} = irc_parser:message(
                 <<":nick!old_user@old_host CHGHOST new_user new_host">>),
    ?assertEqual(irc_parser:chghost(M1), {ok, <<"new_user">>, <<"new_host">>}).

%%% AUTHENTICATE (from the SASL spec)

%% Empty message reply

authenticate1_test_() ->
    {ok, M1} = irc_parser:message(<<"AUTHENTICATE +">>),
    {ok, M2} = irc_parser:message(<<"AUTHENTICATE :+">>),
    [?_assertEqual(irc_parser:authenticate(M1), empty),
     ?_assertEqual(irc_parser:authenticate(M2), empty)].

%% Normal and split message

authenticate2_test_() ->
    Sample100 = << <<$A>> || _ <- lists:seq(1, 100) >>,
    Sample400 = << <<$A>> || _ <- lists:seq(1, 400) >>,
    {ok, M1} = irc_parser:message(<<"AUTHENTICATE ", Sample400/binary>>),
    {ok, M2} = irc_parser:message(<<"AUTHENTICATE ", Sample100/binary>>),
    [?_assertEqual(irc_parser:authenticate(M1), {more, Sample400}),
     ?_assertEqual(irc_parser:authenticate(M2), {ok, Sample100})].

%%% SETNAME (from the setname spec)

setname1_test() ->
    {ok, M1} = irc_parser:message(<<":nick!user@host SETNAME :realname here">>),
    ?assertEqual(irc_parser:setname(M1), {ok, <<"realname here">>}).

%%% FAIL (from the Standard Replies spec)

fail1_test() ->
    {ok, M1} = irc_parser:message(<<"FAIL ACC REG_INVALID_CALLBACK REGISTER"
                                    " :Email address is not valid">>),
    ?assertEqual(irc_parser:fail(M1),
                 {ok, <<"ACC">>, <<"REG_INVALID_CALLBACK">>,
                 [<<"REGISTER">>], <<"Email address is not valid">>}).

%%% WARN (from the Standard Replies spec)

warn1_test() ->
    {ok, M1} = irc_parser:message(
                 <<"WARN REHASH CERTS_EXPIRED :Certificate has expired">>),
    ?assertEqual(irc_parser:fail(M1),
                 {ok, <<"REHASH">>, <<"CERTS_EXPIRED">>,
                 [], <<"Certificate has expired">>}).

%%% NOTE (from the Standard Replies spec)

note1_test() ->
    {ok, M1} = irc_parser:message(<<"NOTE * OPER_MESSAGE :Test message">>),
    ?assertEqual(irc_parser:fail(M1),
                 {ok, <<"*">>, <<"OPER_MESSAGE">>, [], <<"Test message">>}).


%%%===================================================================
%%% Numeric replies
%%%===================================================================

rpl_isupport1_test() ->
    {ok, M1} = irc_parser:message(
                 <<":irc.example.org 005 nick PREFIX=(ov)@+"
                   " CHANTYPES=#& :are supported by this server">>),
    ?assertEqual(irc_parser:rpl_isupport(M1),
                 {ok, <<"nick">>,
                  [{<<"PREFIX">>, <<"(ov)@+">>}, {<<"CHANTYPES">>,<<"#&">>}],
                  <<"are supported by this server">>}).


%%%===================================================================
%%% Casemapping
%%%===================================================================

casemapping_test_() ->
    [%% Range tests
     ?_assertEqual(irc_parser:lowercase_ascii(<<"@`aAzZ]}^~_\x{7F}">>),
                  <<"@`aazz]}^~_\x{7F}">>),
     ?_assertEqual(irc_parser:uppercase_ascii(<<"@`aAzZ]}^~_\x{7F}">>),
                  <<"@`AAZZ]}^~_\x{7F}">>),

     ?_assertEqual(irc_parser:lowercase_rfc1459(<<"@`aAzZ]}^~_\x{7F}">>),
                  <<"@`aazz}}~~_\x{7F}">>),
     ?_assertEqual(irc_parser:uppercase_rfc1459(<<"@`aAzZ]}^~_\x{7F}">>),
                  <<"@`AAZZ]]^^_\x{7F}">>),

     ?_assertEqual(irc_parser:lowercase_rfc1459_strict(<<"@`aAzZ]}^~_\x{7F}">>),
                  <<"@`aazz}}^~_\x{7F}">>),
     ?_assertEqual(irc_parser:uppercase_rfc1459_strict(<<"@`aAzZ]}^~_\x{7F}">>),
                  <<"@`AAZZ]]^~_\x{7F}">>),

     %% Character tests
     ?_assertEqual(irc_parser:lowercase_ascii(<<"aAzZ[]{}\\|^~">>),
                  <<"aazz[]{}\\|^~">>),
     ?_assertEqual(irc_parser:uppercase_ascii(<<"aAzZ[]{}\\|^~">>),
                  <<"AAZZ[]{}\\|^~">>),

     ?_assertEqual(irc_parser:lowercase_rfc1459(<<"aAzZ[]{}\\|^~">>),
                  <<"aazz{}{}||~~">>),
     ?_assertEqual(irc_parser:uppercase_rfc1459(<<"aAzZ[]{}\\|^~">>),
                  <<"AAZZ[][]\\\\^^">>),

     ?_assertEqual(irc_parser:lowercase_rfc1459_strict(<<"aAzZ[]{}\\|^~">>),
                  <<"aazz{}{}||^~">>),
     ?_assertEqual(irc_parser:uppercase_rfc1459_strict(<<"aAzZ[]{}\\|^~">>),
                  <<"AAZZ[][]\\\\^~">>)
    ].


%%%===================================================================
%%% CTCP parser tests
%%%===================================================================

%% With final delimiter

ctcp01_test() ->
    ?assertEqual(irc_parser:ctcp(<<16#01, "CMD PARAM", 16#01>>),
                 {ok, <<"CMD">>, <<"PARAM">>}).

%% Without final delimiter

ctcp02_test() ->
    ?assertEqual(irc_parser:ctcp(<<16#01, "CMD PARAM">>),
                 {ok, <<"CMD">>, <<"PARAM">>}).

%% Without parameters

ctcp03_test() ->
    ?assertEqual(irc_parser:ctcp(<<16#01, "CMD">>),
                 {ok, <<"CMD">>, <<>>}).

%% Without parameters and one trailing SPACE character

ctcp04_test() ->
    ?assertEqual(irc_parser:ctcp(<<16#01, "CMD ">>),
                 {ok, <<"CMD">>, <<>>}).

%% Without parameters and two trailing SPACE characters

ctcp05_test() ->
    ?assertEqual(irc_parser:ctcp(<<16#01, "CMD  ">>),
                 {ok, <<"CMD">>, <<" ">>}).

%% Various CTCP ACTION messages

ctcp06_test_() ->
    [?_assertEqual(irc_parser:ctcp(<<"\x{01}ACTION \x{01}">>),
                   {ok, <<"ACTION">>, <<>>}),
     ?_assertEqual(irc_parser:ctcp(<<"\x{01}ACTION\x{01}">>),
                   {ok, <<"ACTION">>, <<>>}),
     ?_assertEqual(irc_parser:ctcp(<<"\x{01}ACTION ">>),
                   {ok, <<"ACTION">>, <<>>}),
     ?_assertEqual(irc_parser:ctcp(<<"\x{01}ACTION">>),
                   {ok, <<"ACTION">>, <<>>})].


%%%===================================================================
%%% Formatting
%%%===================================================================
