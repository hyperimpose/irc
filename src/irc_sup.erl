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
%%% This is the top level supervisor of the application.
%%%
%%% It is responsible for starting (connect/1), stopping (disconnect/2)
%%% and supervising IRC client instances.
%%%-------------------------------------------------------------------

-module(irc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, connect/1, disconnect/1]).

%% Supervisor callbacks
-export([init/1]).

%% Macros
-define(NAME, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------

-spec start_link() -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.

start_link() ->
    supervisor:start_link({local, ?NAME}, ?MODULE, []).


%%--------------------------------------------------------------------
%% Connect to an IRC server by starting a new client instance
%%--------------------------------------------------------------------

-spec connect(Id :: atom()) ->
          supervisor:startchild_ret().

connect(Id) ->
    Spec = #{id       => Id,
             start    => {irc_client_sup, start_link, [Id]},
             restart  => transient,
             shutdown => infinity,
             type     => supervisor},
    supervisor:start_child(?NAME, Spec).


%%--------------------------------------------------------------------
%% Disconnect from an IRC server by stopping the client instance
%%--------------------------------------------------------------------

-spec disconnect(Id :: atom()) ->
          ok | {error, not_found | running | restarting}.

disconnect(Id) ->
    case supervisor:terminate_child(?NAME, Id) of
        {error, Error} -> {error, Error};
        ok             -> supervisor:delete_child(?NAME, Id)
    end.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}}.

init(_Args) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    IrcConfig = #{id       => irc_config,
                  start    => {irc_config, start_link, []},
                  restart  => permanent},

    {ok, {SupFlags, [IrcConfig]}}.
