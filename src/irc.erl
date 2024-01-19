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
%%% Shortcut functions for managing IRC clients.
%%%
%%% This module provides a stable user facing API to the rest of the
%%% application.
%%%-------------------------------------------------------------------

-module(irc).


-export([connect/1, disconnect/1]).


%%--------------------------------------------------------------------

-spec connect(Id :: atom()) -> supervisor:startchild_ret().

connect(Id) ->
    irc_sup:connect(Id).

%%--------------------------------------------------------------------

-spec disconnect(Id :: atom()) ->
          ok | {error, not_found | running | restarting}.

disconnect(Id) ->
    irc_sup:disconnect(Id).
