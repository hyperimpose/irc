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

-module(irc_runtime_SUITE).

-export([suite/0, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([rpl_channelmodeis/1]).

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
    [rpl_channelmodeis].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

rpl_channelmodeis(_Config) ->
    true = irc_isupport:set_chanmodes(?ID, <<"Xb,k,l,CMNRScimnprst">>),
    true = irc_isupport:set_prefix(?ID, <<"(ohvV)@%+-">>),
    ok = irc_channel:add(?ID, [<<"#channel">>]),

    M = {message,#{},<<"irc.lainchan.org">>,undefined,undefined,<<"324">>,
         [<<"nickname">>,<<"#channel">>,<<"+rktn">>,<<"key">>]},
    S = {state, ?ID,
         % Set enough values to fill the record. Only `id' is used.
         undefined, undefined, undefined, undefined, undefined, undefined},
    irc_runtime:message(M, S),

    [{$r, <<>>}, {$k, <<"key">>}, {$t, <<>>}, {$n, <<>>}] =
        irc_channel:get_modes(?ID, "#channel").
