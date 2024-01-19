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
%%% IRCv3 support module. It provides basic types and helper functions.
%%%-------------------------------------------------------------------

-module(ircv3).


-export([get_cap_req/1]).


-export_type([cap/0, cap_args/0,
              sasl/0, sasl_plain/0]).


-type cap() :: binary().
-type cap_args() :: any().


-type sasl_plain() :: {plain, User :: iodata(), Password :: iodata()}.

-type sasl() :: sasl_plain().



%%% cap_req ==========================================================

%%--------------------------------------------------------------------
%% Prepare a list of capabilities to send to a CAP REQ command.
%%
%% The list is intersection of the set of capabilities in the cap_want
%% config setting and the capabilities supported by the IRC server.
%%--------------------------------------------------------------------

get_cap_req(Id) ->
    CapWant = irc_config:get_cap_want(irc_state:get_config(Id)),
    Want_set = sets:from_list(CapWant),

    CapLs = irc_state:get_cap_ls(Id),
    Ls_set = sets:from_list(maps:keys(CapLs)),

    CapList = irc_state:get_cap_list(Id),
    List_set = sets:from_list(CapList),

    Del = lists:map(fun (X) -> <<$-, X/binary>> end,
                    sets:to_list(sets:subtract(List_set, Want_set))),
    Add = sets:to_list(sets:subtract(sets:intersection(Want_set, Ls_set),
                                     List_set)),
    Add ++ Del.
