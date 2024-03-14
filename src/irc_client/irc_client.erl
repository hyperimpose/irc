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
%%% gen_server that manages:
%%% - the connection to the IRC server: socket, client registration
%%% - an ETS table with the state of the client instance
%%% - incoming messages
%%%
%%% This module implements the IRC protocol. Along with irc_send they
%%% implement an IRC client.
%%%
%%% It requires a user supplied module for configuration. See the
%%% irc_config module for details. This configuration is read once
%%% when the gen_server is initialized.
%%%
%%% It also pushes incoming messages to a user supplied handler.
%%%
%%% The ETS table is used for data that should be available to the
%%% users of the library. The table has protected access.
%%%-------------------------------------------------------------------

-module(irc_client).

-behaviour(gen_server).


-include_lib("kernel/include/logger.hrl").


%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


%% State
-include("irc_client.hrl").  % state, cap_state


%% Macros
-define(NAME(Id), {global, {?MODULE, Id}}).
-define(TIMEOUT, 120_000).        % 2 minutes in milliseconds for the gen_server
-define(SOCKET_TIMEOUT, 30_000).  % 30 seconds in milliseconds
-define(WAIT_LIMIT, 600_000).     % 10 minutes in milliseconds


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Id :: atom()) ->
          {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.

start_link(Id) ->
    gen_server:start_link(?NAME(Id), ?MODULE, Id, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Id :: atom()) -> {ok, State :: #state{}, Timeout :: timeout()}.

init(Id) ->
    process_flag(trap_exit, true),

    %% ETS table to hold the state of the IRC connection
    ets:new(Id, [set, protected, named_table, {keypos, 1}]),

    %% Configuration
    {ok, Conf} = irc_config:get(Id),

    State = #state{id = Id, conf = Conf},
    {ok, connect(State), ?TIMEOUT}.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({tcp, _, _} = S,       State) -> message(S, State);
handle_info({ssl, _, _} = S,       State) -> message(S, State);
handle_info({tcp_closed, _} = S,   State) -> closed(S, State);
handle_info({ssl_closed, _} = S,   State) -> closed(S, State);
handle_info({tcp_error, _, _} = S, State) -> socket_error(S, State);
handle_info({ssl_error, _, _} = S, State) -> socket_error(S, State);
handle_info(connect, State) ->  % Internal usage only. Used by connect/1.
    {noreply, connect(State), ?TIMEOUT};
handle_info(timeout, State) ->  % gen_server timeout message
    timeout(State);
handle_info(_Info, State) ->
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().

terminate(_Reason, _State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Initialize connection ============================================

connect(#state{id = Id, conf = Conf, wait = Wait} = State) ->
    Addr = irc_config:get_address(Conf),
    Port = irc_config:get_port(Conf),
    Size = irc_config:get_packet_size(Conf),
    Handler = irc_config:get_handler(Conf),

    State1 = State#state{handler = Handler},

    Opt = [binary, {active, once}, {packet, line}, {packet_size, Size}],

    case gen_tcp:connect(Addr, Port, Opt, ?SOCKET_TIMEOUT) of
        {ok, Socket}   ->
            State2 = wait_reset(State1),
            connect2(State2, Conf, Socket);
        {error, Error} ->
            ?LOG_ERROR("[IRC:~p] ~p", [Id, Error]),
            erlang:send_after(Wait, self(), connect),
            wait_increment(State1)
    end.

connect2(State, Conf, Socket) ->
    {S, M} = case irc_config:get_tls(Conf) of
                 true ->
                     ssl:start(),
                     Opt = [{verify, verify_none}],
                     {ok, TlsSocket} = ssl:connect(Socket, Opt, ?SOCKET_TIMEOUT),
                     {TlsSocket, ssl};
                 false ->
                     {Socket, gen_tcp}
             end,
    connect3(State#state{socket = S, module = M}, Conf).

connect3(#state{id = Id, socket = Socket, module = Module} = State, Conf) ->
    irc_send:set_mode(Id, irc_config:get_irc_send_mode(Conf)),
    irc_send:set_socket(Id, Socket, Module),  % Setup the send scheduler
    registration(State, Conf).


%% This function will send the appropriate messages to the IRC server
%% and reset the local runtime state.

registration(#state{id = Id} = State, Conf) ->
    Nick = irc_config:get_nickname(Conf),
    User = irc_config:get_user(Conf),
    Real = irc_config:get_realname(Conf),

    irc_send:schedule(Id, irc_command:cap_ls(<<"302">>)),
    irc_send:schedule(Id, irc_command:user(User, "0", "*", Real)),
    irc_send:schedule(Id, irc_command:nick(Nick)),

    ets:delete_all_objects(Id),        % Reset the state table
    irc_state:set_nickname(Id, Nick),  % Set the current nickname
    irc_state:set_config(Id, Conf),    % Set the current config

    State#state{cap = #cap_state{}}.   % Reset cap_state


wait_increment(#state{wait = 0} = S) ->
    S#state{wait = 10_000};  % 10 seconds
wait_increment(#state{wait = W} = S) when W >= ?WAIT_LIMIT ->
    S#state{wait = ?WAIT_LIMIT};
wait_increment(#state{wait = W} = S) ->
    S#state{wait = W * 2}.


wait_reset(S) -> S#state{wait = 0}.


%%% Socket handlers ==================================================

message({Mode, Socket, Data}, #state{id = Id, handler = Handler} = State) ->
    {ok, Message} = irc_parser:message(Data),
    S1 = irc_runtime:message(Message, State),
    Handler(Id, Message),  % Custom handler

    case Mode of
        ssl -> ok = ssl:setopts(Socket, [{active, once}]);
        tcp -> ok = inets:setopts(Socket, [{active, once}])
    end,

    {noreply, S1, ?TIMEOUT}.


closed({C, _Socket}, #state{id = Id} = State)
  when C == tcp_closed orelse C == ssl_closed ->
    ?LOG_DEBUG("[IRC:~p] Socket closed", [Id]),
    S1 = State#state{socket = undefined, module = undefined},
    {noreply, connect(S1), ?TIMEOUT}.


socket_error({C, _Socket, Reason}, #state{id = Id} = State)
  when C == tcp_error orelse C == ssl_error ->
    ?LOG_ERROR("[IRC:~p] Socket error: ~p", [Id, Reason]),
    S1 = State#state{socket = undefined, module = undefined},
    {noreply, connect(S1), ?TIMEOUT}.


timeout(#state{id = Id} = State) ->
    irc_send:now(Id, irc_command:ping(<<"drastikbot">>)),
    {noreply, State, ?TIMEOUT}.
