%%--------------------------------------------------------------------
%% Copyright 2026 hyperimpose.org
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

-module(irc_channel_SUITE).

-export([suite/0, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([channels/1, topic/1, creationtime/1, modes/1, users/1]).

-include_lib("common_test/include/ct.hrl").


-define(ID, irc_channel_tests).


%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [{timetrap, {seconds, 10}}].


init_per_testcase(_TestCase, Config) ->
    ets:new(?ID, [set, protected, named_table, {keypos, 1}]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ets:delete(?ID).

%%--------------------------------------------------------------------

all() ->
    [channels, topic, creationtime, modes, users].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

channels(_Config) ->
    ok = irc_channel:add(?ID, [<<"#channel1">>]),
    ok = irc_channel:add(?ID, [<<"#channel2">>, <<"#channel3">>]),

    L0 = irc_channel:list(?ID),
    3 = length(L0),
    true = lists:member(<<"#channel2">>, L0),

    true = irc_channel:is_joined(?ID, "#channel2"),
    false = irc_channel:is_joined(?ID, <<"#channel4">>),

    irc_channel:delete(?ID, [<<"#channel2">>, "#channel3"]),

    L1 = irc_channel:list(?ID),
    1 = length(L1),
    true = lists:member(<<"#channel1">>, L1),

    true = irc_channel:is_joined(?ID, <<"#channel1">>),
    false = irc_channel:is_joined(?ID, "#channel2").


topic(_Config) ->
    C = <<"#channel">>,
    ok = irc_channel:add(?ID, [C]),

    undefined = irc_channel:get_topic(?ID, C),
    undefined = irc_channel:get_topic_nick(?ID, C),
    undefined = irc_channel:get_topic_timestamp(?ID, C),

    irc_channel:set_topic(?ID, C, <<"Sample Text">>),
    irc_channel:set_topic_nick(?ID, C, <<"User2">>),
    irc_channel:set_topic_timestamp(?ID, C, 1753760766),

    <<"Sample Text">> = irc_channel:get_topic(?ID, C),
    <<"User2">> = irc_channel:get_topic_nick(?ID, C),
    1753760766 = irc_channel:get_topic_timestamp(?ID, C),

    irc_channel:unset_topic(?ID, C),

    undefined = irc_channel:get_topic(?ID, C),
    undefined = irc_channel:get_topic_nick(?ID, C),
    undefined = irc_channel:get_topic_timestamp(?ID, C).


creationtime(_Config) ->
    C = <<"#channel">>,
    ok = irc_channel:add(?ID, [C]),

    undefined = irc_channel:get_creationtime(?ID, C),

    irc_channel:set_creationtime(?ID, C, 1759695595),
    1759695595 = irc_channel:get_creationtime(?ID, C).


modes(_Config) ->
    C = <<"#channel">>,
    ok = irc_channel:add(?ID, [C]),

    [] = irc_channel:get_modes(?ID, C),

    irc_channel:set_modes(?ID, C, [{$k, <<"password">>}, {$r, <<>>}]),
    [{$k, <<"password">>}, {$r, <<>>}] = irc_channel:get_modes(?ID, C).


users(_Config) ->
    users_get_users(),
    users_insert_user(),
    users_change_nick(),
    users_delete_user_3(),
    users_delete_user_2(),
    users_delete_users(),
    users_prefix().

users_get_users() ->
    C = <<"#channel">>,
    ok = irc_channel:add(?ID, [C]),

    Us = irc_channel:get_users(?ID, C),
    0 = maps:size(Us),

    irc_channel:delete(?ID, [C]).

users_insert_user() ->
    C = <<"#channel">>,
    ok = irc_channel:add(?ID, [C]),

    irc_channel:insert_user(?ID, [C], <<"USER1">>, []),
    #{<<"user1">> := U1} = irc_channel:get_users(?ID, C),
    #{name := <<"USER1">>, prefix := [], nickname := <<"user1">>} = U1,

    irc_channel:insert_user(?ID, [C], <<"USER2">>, [$+, $@]),
    #{<<"user1">> := U1, <<"user2">> := U2} = irc_channel:get_users(?ID, C),
    #{name := <<"USER1">>, prefix := [],   nickname := <<"user1">>} = U1,
    #{name := <<"USER2">>, prefix := "+@", nickname := <<"user2">>} = U2,

    irc_channel:delete(?ID, [C]).

users_change_nick() ->
    C = <<"#channel">>,
    ok = irc_channel:add(?ID, [C]),

    irc_channel:insert_user(?ID, [C], <<"USER1">>, []),
    irc_channel:insert_user(?ID, [C], <<"USER2">>, [$+, $@]),

    irc_channel:change_nick(?ID, "uSer1", "user3"),
    #{<<"user3">> := U} = Us = irc_channel:get_users(?ID, C),
    #{name := "user3", prefix := [], nickname := <<"user3">>} = U,
    2 = maps:size(Us),

    irc_channel:delete(?ID, [C]).

users_delete_user_3() ->
    C = <<"#channel">>,
    ok = irc_channel:add(?ID, [C]),

    irc_channel:insert_user(?ID, [C], <<"USER1">>, []),
    irc_channel:insert_user(?ID, [C], <<"USER2">>, []),
    irc_channel:insert_user(?ID, [C], <<"USER3">>, []),
    irc_channel:insert_user(?ID, [C], <<"USER4">>, []),
    irc_channel:insert_user(?ID, [C], <<"USER5">>, []),
    Us1 = irc_channel:get_users(?ID, C),
    5 = maps:size(Us1),

    irc_channel:delete_user(?ID, [C], "user1"),
    Us2 = irc_channel:get_users(?ID, C),
    4 = maps:size(Us2),
    #{<<"user2">> := _,
      <<"user3">> := _,
      <<"user4">> := _,
      <<"user5">> := _} = Us2,

    irc_channel:delete(?ID, [C]).

users_delete_user_2() ->
    C1 = "#channel",
    C2 = "#channel2",
    ok = irc_channel:add(?ID, [C1, C2]),
    2 = length(irc_channel:list(?ID)),

    irc_channel:insert_user(?ID, [C1, C2], <<"USER1">>, []),
    irc_channel:insert_user(?ID, [C1, C2], <<"USER2">>, []),
    irc_channel:insert_user(?ID, [C1, C2], <<"USER3">>, []),
    irc_channel:insert_user(?ID, [C1], <<"USER4">>, []),
    irc_channel:insert_user(?ID, [C2], <<"USER5">>, []),

    Us1 = irc_channel:get_users(?ID, C1),
    4 = maps:size(Us1),
    #{<<"user1">> := _,
      <<"user2">> := _,
      <<"user3">> := _,
      <<"user4">> := _} = Us1,

    Us2 = irc_channel:get_users(?ID, C2),
    4 = maps:size(Us2),
    #{<<"user1">> := _,
      <<"user2">> := _,
      <<"user3">> := _,
      <<"user5">> := _} = Us2,

    irc_channel:delete_user(?ID, "user1"),
    irc_channel:delete_user(?ID, "user5"),

    Us3 = irc_channel:get_users(?ID, C1),
    3 = maps:size(Us3),
    #{<<"user2">> := _,
      <<"user3">> := _,
      <<"user4">> := _} = Us3,

    Us4 = irc_channel:get_users(?ID, C2),
    2 = maps:size(Us4),
    #{<<"user2">> := _,
      <<"user3">> := _} = Us4,

    irc_channel:delete(?ID, [C1, C2]),
    0 = length(irc_channel:list(?ID)).

users_delete_users() ->
    C = <<"#channel">>,
    ok = irc_channel:add(?ID, [C]),

    irc_channel:insert_user(?ID, [C], <<"user1">>, []),
    irc_channel:insert_user(?ID, [C], <<"user2">>, []),
    irc_channel:insert_user(?ID, [C], <<"user3">>, []),

    3 = maps:size(irc_channel:get_users(?ID, C)),
    irc_channel:delete_users(?ID, C),
    0 = maps:size(irc_channel:get_users(?ID, C)),

    irc_channel:delete(?ID, [C]).

users_prefix() ->
    C = <<"#channel">>,
    ok = irc_channel:add(?ID, [C]),

    irc_channel:insert_user(?ID, [C], <<"user1">>, []),
    #{<<"user1">> := U1} = irc_channel:get_users(?ID, C),
    #{name := <<"user1">>, prefix := [], nickname := <<"user1">>} = U1,

    irc_channel:add_prefix(?ID, C, "user1", $+),
    #{<<"user1">> := U2} = irc_channel:get_users(?ID, C),
    #{name := <<"user1">>, prefix := [$+], nickname := <<"user1">>} = U2,

    irc_channel:add_prefix(?ID, C, "user1", $@),
    #{<<"user1">> := U3} = irc_channel:get_users(?ID, C),
    #{name := <<"user1">>, prefix := [$@, $+], nickname := <<"user1">>} = U3,

    irc_channel:delete_prefix(?ID, C, "user1", $@),
    #{<<"user1">> := U4} = irc_channel:get_users(?ID, C),
    #{name := <<"user1">>, prefix := [$+], nickname := <<"user1">>} = U4,

    irc_channel:delete_prefix(?ID, C, "user1", $+),
    #{<<"user1">> := U5} = irc_channel:get_users(?ID, C),
    #{name := <<"user1">>, prefix := [], nickname := <<"user1">>} = U5,

    irc_channel:delete(?ID, [C]).
