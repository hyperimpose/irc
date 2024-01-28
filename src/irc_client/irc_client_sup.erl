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
%%% This supervisor essentially implements the IRC client. It starts
%%% the gen_servers needed to connect to an IRC server.
%%%-------------------------------------------------------------------

-module(irc_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Macros
-define(NAME(Id), {global, {?MODULE, Id}}).


%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(Id :: atom()) ->
          {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.

start_link(Id) ->
    supervisor:start_link(?NAME(Id), ?MODULE, Id).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(Id :: atom()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.

init(Id) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 1,
                 period    => 5},

    IrcSend = #{id       => irc_send,
                start    => {irc_send, start_link, [Id]},
                restart  => permanent,
                shutdown => 5000,
                type     => worker,
                modules  => [irc_send]},

    IrcClient = #{id       => irc_client,
                  start    => {irc_client, start_link, [Id]},
                  restart  => permanent,
                  shutdown => 5000,
                  type     => worker,
                  modules  => [irc_client]},

    {ok, {SupFlags, [IrcSend, IrcClient]}}.
