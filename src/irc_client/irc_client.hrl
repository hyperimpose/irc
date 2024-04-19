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
%%% The state of the irc_client gen_server.
%%%
%%% It exists inside a header file because the record is used by other
%%% modules that are called by irc_client.
%%% For organizational reasons, various parts of irc_client are
%%% implemented in other modules.
%%%-------------------------------------------------------------------

-record(cap_state, {reset = false,
                    cap_ls = #{},
                    cap_req = [],
                    cap_ack = sets:new(),
                    cap_nak = sets:new(),
                    cap_list = {false, []},
                    sasl = false}).

-record(state, {id,  % The IRC client identifier.
                conf :: irc_config:config(),

                %% Networking
                socket,  % The socket used to connect to the IRC server.
                module :: gen_tcp | ssl | undefined,  % Socket handler module.
                wait = 0,  % How long to wait between reconnection attempts.

                %% Handler - A fun callback to which all IRC messages are sent.
                handler :: irc_config:handler() | undefined,

                %% Runtime
                cap :: #cap_state{} | undefined}).
