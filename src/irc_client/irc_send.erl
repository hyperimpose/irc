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
%%% A gen_server to schedule and send messages to the IRC server.
%%%
%%%
%%% Initialization / Configuration
%%% ------------------------------
%%%
%%% After starting the gen_server you  must also provide the necessary
%%% configuration parameters. These are  the socket, the socket module
%%% and the mode in which the schedule must be set.
%%%
%%% The configuration  is done  by calling the  functions set_socket/3
%%% and set_mode/2 after the gen_server has been started.
%%%
%%% Because of the obvious dependency to the caller of those functions
%%% you must use monitors or a  proper supervisor setup to ensure that
%%% reconfiguration will happen if the module crashes.
%%%
%%%
%%% Modes
%%% -----
%%%
%%% Because each server may specify different rate limiting mechanisms
%%% we use `modes'  to configure the message sending  behaviour of the
%%% bot.
%%%
%%% Available modes: shared, fifo
%%%
%%% Each mode  is documented  in detail  in the  section of  this file
%%% where it is implemented.
%%%
%%%
%%% Messages
%%% --------
%%%
%%% This module is designed to  work with messages formatted such that
%%% they conform to the message() type specification.
%%%-------------------------------------------------------------------

-module(irc_send).

-behaviour(gen_server).

%% API
-export([start_link/1, schedule/2, now/2, set_socket/3, set_mode/2, reset/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


%% Mode: shared
-record(shared_target, {receiver            :: binary() | server,
                        queue = queue:new() :: queue:queue(message()),
                        len = 0             :: integer()}).

-record(shared, {%% Config
                 commands   :: integer(),
                 interval   :: integer(),
                 maxsize    :: integer(),
                 %% Runtime
                 now        :: queue:queue(message()),
                 targets    :: irc_zipper:list_z(#shared_target{}),
                 dispatch   :: boolean(),
                 cmd_left   :: integer(),
                 reset_time :: integer()}).


%% State
-record(state, {id     :: atom(),
                socket :: gen_tcp:socket(),
                module :: module(),
                mode   :: mode()}).


%% Types
-type mode() :: fifo | #shared{}.

-type message() :: message_one() | message_many().

-type message_many() :: {many, [message()]}.

-type message_one() :: {message, #{receiver := term(), message := iodata()}}
                     | iodata().


%% Macros
-define(NAME(Id), {global, {irc_send, Id}}).


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


%%--------------------------------------------------------------------
%% Schedule a message for sending to the IRC server.
%%--------------------------------------------------------------------

-spec schedule(Id :: term(), message()) -> ok.

schedule(Id, Message) ->
    gen_server:call(?NAME(Id), {schedule, Message}).


%%--------------------------------------------------------------------
%% Send a message before any other scheduled messages.
%%
%% Avoid using this function.  It is meant for internal use by the bot
%% to send urgent protocol messages that should not be delayed.
%%
%% The actual mechanics of the function depend on the mode used.
%%
%% `fifo': The message is sent immediately to the server.  There is no
%% difference between now/2 and schedule/2 in this mode.
%%
%% `shared': The message is added in a special queue that is processed
%% before any of the other queues.
%%--------------------------------------------------------------------

-spec now(Id :: term(), message()) -> ok.

now(Id, Message) ->
    gen_server:call(?NAME(Id), {now, Message}).


%%--------------------------------------------------------------------
%% Set the socket and the module (gen_tcp / ssl) to use for sending
%% messages to the IRC server.
%%--------------------------------------------------------------------

-spec set_socket(Id :: term(), gen_tcp:socket(), module()) -> ok.

set_socket(Id, Socket, Module) ->
    gen_server:call(?NAME(Id), {set_socket, Socket, Module}).


%%--------------------------------------------------------------------
%% Set the message scheduling mode.
%%--------------------------------------------------------------------

-spec set_mode(Id :: term(), Mode :: mode()) -> ok.

set_mode(Id, Mode) ->
    gen_server:call(?NAME(Id), {set_mode, Mode}).


%%--------------------------------------------------------------------
%% Reset the mode record to clear the queues and reset the counters.
%%
%% This is useful when reconnecting, because we want the queues
%% emptied so that the old messages will not interfer with the
%% registration process.
%%
%% May also be useful for stopping unwanted output/flooding.
%%--------------------------------------------------------------------

-spec reset(Id :: term()) -> ok.

reset(Id) ->
    gen_server:call(?NAME(Id), reset).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Id :: atom()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.

init(Id) ->
    process_flag(trap_exit, true),
    {ok, #state{id = Id}}.

%%--------------------------------------------------------------------

handle_call({schedule, Message}, _From, State) -> call_schedule(Message, State);
handle_call({now, Message},      _From, State) -> call_now(Message, State);

handle_call({set_socket, Socket, Module}, _From, State) ->
    {reply, ok, State#state{socket = Socket, module = Module}};
handle_call({set_mode, Mode},             _From, State) ->
    {reply, ok, init_mode(Mode, State)};

handle_call(reset, _From, State) -> call_reset(State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(msg_process, State) -> info_msg_process(State);
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().

terminate(_Reason, _State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec init_mode(fifo | {shared, map()}, #state{}) -> #state{}.

init_mode(fifo, State) ->
    State#state{mode = fifo};
init_mode({shared, C}, State) ->
    Commands = maps:get(commands, C, 1),
    Interval = maps:get(interval, C, 1000),
    Size     = maps:get(size,     C, 60),
    State#state{mode = shared_init(Commands, Interval, Size)}.


-spec call_schedule(message(), #state{}) -> {reply, ok, #state{}}.

call_schedule(Message, #state{mode = Mode} = State) ->
    case Mode of
        #shared{} -> shared_schedule(Message, State);
        fifo      -> fifo_send(Message, State)
    end.


-spec call_now(message(), #state{}) -> {reply, ok, #state{}}.

call_now(Message, #state{mode = Mode} = State) ->
    case Mode of
        #shared{} -> shared_now(Message, State);
        fifo      -> fifo_send(Message, State)
    end.


-spec call_reset(#state{}) -> {reply, ok, #state{}}.

call_reset(#state{mode = Mode} = State) ->
    case Mode of
        #shared{} -> shared_reset(State);
        _         -> {reply, ok, State}  % Catch-all for modes without queues
    end.


-spec info_msg_process(#state{}) -> {noreply, #state{}}.

info_msg_process(#state{mode = Mode} = State) ->
    case Mode of
        #shared{} -> shared_process(State)
    end.


%%%===================================================================
%%% Mode: fifo
%%%===================================================================

%%% Implementation Documentation
%%% ----------------------------
%%%
%%% This mode emposes no limits  whatsoever to the bot's output.  Each
%%% message that arrives is immediately sent to the IRC server.
%%%
%%% There is  no reason to use  this.  Limiting the bot's  output rate
%%% prevents  the bot  from  getting disconnected  by  the server  for
%%% exceeding the  flood limits.

-spec fifo_send(message(), #state{}) -> {reply, ok, #state{}}.

fifo_send(Message, #state{socket = S, module = M} = State) ->
    fifo_send(Message, S, M),
    {reply, ok, State}.

fifo_send({many, Msgs},                 S, M) -> fifo_many(Msgs, S, M);
fifo_send({message, #{message := Msg}}, S, M) -> M:send(S, Msg);
fifo_send(Msg,                          S, M) -> M:send(S, Msg).

fifo_many([H | R], S, M) ->
    fifo_send(H, S, M),
    fifo_many(R, S, M);
fifo_many([], _S, _M) ->
    [].  % Do nothing


%%%===================================================================
%%% Mode: shared
%%%===================================================================

%%% Implementation Documentation
%%% ----------------------------
%%%
%%% - 1. Introduction
%%%
%%% In the  shared mode  the scheduler will  insert each  message into
%%% queues   based  on   the  receiver   (channel/nickname)  of   each
%%% message. There is a special  queue for server messages or messages
%%% for which  we do not know  the receiver.
%%%
%%% These queues exist in a list.  In the processing stage we pick the
%%% next queue from  the list, pop a  message from it, send  it to the
%%% server and move to the next queue in the list.
%%%
%%% Because  some messages  are urgent  and must  be sent  immediately
%%% there is an  extra queue whose messages are sent  before any other
%%% queue are processed.
%%%
%%% Switching between queues allows the bot to serve each channel/user
%%% in a  timely manner, by  spreading the commandrate allowed  by the
%%% server equally.
%%%
%%% This mode also implements rate limiting. X commands are sent every
%%% Y milliseconds.
%%%
%%%
%%% - 2. The #shared{} record
%%%
%%% The  shared mode  keeps its  state in  the #shared{}  record. That
%%% record is split in two sections: config, runtime.
%%%
%%% -- 2.1 Config
%%%
%%% The rate limiter and maximum queue size can be configured.
%%%
%%% For the rate limiter, the record entries `commands' and `interval'
%%% are  used.  The  algorithm  will send  `commands'  commands  every
%%% `interval' milliseconds.
%%%
%%% The `maxsize'  entry controls the  maximum allowed entries  in the
%%% queue. If  a queue has `maxsize'  entries already in it,  then any
%%% new messages will be dropped. When a message is dropped the caller
%%% is  NOT notified.  The  buffer  used for  urgent  messages is  not
%%% limited in size.
%%%
%%% -- 2.2 Runtime
%%%
%%% The following entries are used at runtime: now, targets, dispatch,
%%% cmd_left, reset_time.
%%%
%%% now: This  is a  queue that  contains messages  that must  be sent
%%%      immediately. No other  messages are sent until  this queue is
%%%      empty.
%%%
%%% targets: This is a list of queues. This is where the message
%%%          scheduling described earlier happens.
%%%
%%% dispatch: Boolean. This is used  by the functions `shared_now' and
%%%           `shared_schedule' to send a message to the gen_server so
%%%           that  it  initiates  the `shared_process'  function  and
%%%           starts sending messages to the server. It is set to true
%%%           when a  message is queued. It  is set to false  when all
%%%           the queues are empty.
%%%
%%% cmd_left: Used for  rate limiting. The number of  commands left to
%%%           send for this interval.
%%%
%%% reset_time: The  time when the  current interval will end  and the
%%%             `cmd_left' entry can be reset.


%%--------------------------------------------------------------------
%% Initialize and return the #shared{} record used by the mode.
%%--------------------------------------------------------------------

-spec shared_init(integer(), integer(), integer()) -> #shared{}.

shared_init(Commands, Interval, MaxSize) ->
    #shared{commands   = Commands,
            interval   = Interval,
            maxsize    = MaxSize,
            now        = queue:new(),
            targets    = irc_zipper:list(),
            dispatch   = false,
            cmd_left   = Commands,
            reset_time = erlang:monotonic_time(millisecond) + Interval}.


%%% reset [call] =====================================================

-spec shared_reset(#state{}) -> {reply, ok, #state{}}.

shared_reset(#state{mode = Mode} = State) ->
    #shared{commands = C, interval = I, maxsize = M} = Mode,
    {reply, ok, State#state{mode = shared_init(C, I, M)}}.


%%% now [call] =======================================================

-spec shared_now(message(), #state{}) -> {reply, ok, #state{}}.

shared_now({many, Messages}, State) ->
    shared_now_many(Messages, State);
shared_now({message, #{message := M}}, State) ->
    shared_now2(M, State);
shared_now(Message, State)                     ->
    shared_now2(Message, State).


shared_now_many([H | R], State) ->
    {reply, ok, S1} = shared_now(H, State),
    shared_now_many(R, S1);
shared_now_many([], State) ->
    {reply, ok, State}.


shared_now2(Message, #state{mode = Mode} = State) ->
    #shared{now = N, dispatch = Dsp} = Mode,

    case Dsp of
        false -> self() ! msg_process;
        true  -> []  % Do nothing
    end,

    N1 = queue:in(Message, N),
    S1 = State#state{mode = Mode#shared{now = N1, dispatch = true}},

    {reply, ok, S1}.


%%% schedule [call] ==================================================

-spec shared_schedule(message(), #state{}) -> {reply, ok, #state{}}.

shared_schedule({many, Messages}, State) ->
    shared_schedule_many(Messages, State);
shared_schedule({message, #{receiver := R, message := M}}, State) ->
    shared_schedule2(R, M, State);
shared_schedule(Message, State)                     ->
    shared_schedule2(server, Message, State).

shared_schedule_many([H | R], State) ->
    {reply, ok, S1} = shared_schedule(H, State),
    shared_schedule_many(R, S1);
shared_schedule_many([], State) ->
    {reply, ok, State}.


shared_schedule2(Receiver, Message, #state{mode = Mode} = State) ->
    #shared{targets = Ts, maxsize = MS} = Mode,

    T = #shared_target{len = L} = get_target(Ts, Receiver),

    %% Enforce the max queue size
    if
        L >= MS -> {reply, ok, State};
        true    -> shared_schedule3(Message, State, T)
    end.


shared_schedule3(Message, #state{mode = Mode} = State, T) ->
    #shared{targets = Ts, dispatch = Dsp} = Mode,
    #shared_target{queue = Q, len = L} = T,

    %% Start the dispatcher (the shared_process function)
    case Dsp of
        false -> self() ! msg_process;
        true  -> []  % Do nothing
    end,

    T1 = T#shared_target{queue = queue:in(Message, Q), len = L + 1},
    Mode1 = Mode#shared{targets = set_target(Ts, T1), dispatch = true},

    {reply, ok, State#state{mode = Mode1}}.


%%% msg_process [info] ===============================================

-spec shared_process(State :: #state{}) -> {noreply, #state{}}.

shared_process(#state{mode = Mode} = State) ->
    #shared{commands = Commands,
            interval = Interval,
            cmd_left = CmdLeft,
            reset_time = ResetTime} = Mode,

    Now = erlang:monotonic_time(millisecond),

    {CL1, RT1} = if
                     ResetTime =< Now -> {Commands, Now + Interval};
                     true             -> {CmdLeft, ResetTime}
                 end,

    S1 = State#state{mode = Mode#shared{cmd_left = CL1, reset_time = RT1}},

    if
        CL1 > 0 ->
            shared_process1(S1);
        true ->
            erlang:send_after(RT1 - Now, self(), msg_process),
            {noreply, S1}
    end.

shared_process1(State) ->
    #state{mode = Mode} = State,
    #shared{now = N} = Mode,

    %% Send the prioritized messages first.
    case queue:is_empty(N) of
        true  -> shared_process_target(State);
        false -> shared_process_now(State)
    end.


shared_process_now(State) ->
    #state{mode = Mode, socket = Socket, module = Module} = State,
    #shared{now = N, targets = Ts, cmd_left = CL} = Mode,

    Module:send(Socket, queue:head(N)),

    N1 = queue:tail(N),

    case shared_is_empty(N1, Ts) of
        false ->
            Mode1 = Mode#shared{now = N1, cmd_left = CL - 1},
            shared_process(State#state{mode = Mode1});
        true  ->
            Mode1 = Mode#shared{now = N1, cmd_left = CL - 1, dispatch = false},
            {noreply, State#state{mode = Mode1}}
    end.


shared_process_target(State)->
    #state{mode = M, socket = Socket, module = Module} = State,
    #shared{now = N, targets = Ts, cmd_left = CL} = M,

    Ts1 = irc_zipper:next_c(Ts),

    T = #shared_target{queue = Q, len = L} = irc_zipper:current(Ts1),

    Module:send(Socket, queue:head(Q)),

    Q1 = queue:tail(Q),
    Ts2 = case queue:is_empty(Q1) of
              true  -> irc_zipper:pop(Ts1);
              false -> set_target(Ts1, T#shared_target{queue = Q1, len = L - 1})
          end,

    case shared_is_empty(N, Ts2) of
        true  ->
            M1 = M#shared{targets = Ts2, cmd_left = CL - 1, dispatch = false},
            {noreply, State#state{mode = M1}};
        false ->
            M1 = M#shared{targets = Ts2, cmd_left = CL - 1},
            shared_process(State#state{mode = M1})
    end.


%%--------------------------------------------------------------------
%% Check if all the buffers are empty.
%%
%% @param Now The `now' entry from #shared{}
%% @param Targets The `targets' entry from #shared{}
%%--------------------------------------------------------------------

-spec shared_is_empty(Now     :: queue:queue(message()),
                      Targets :: irc_zipper:zipper_list(#shared_target{})) ->
          boolean().

shared_is_empty(Now, Targets) ->
    case queue:is_empty(Now) of
        false -> false;
        true  -> irc_zipper:is_empty(Targets)
    end.


%%%===================================================================
%%% Helpers: Shared Mode
%%%===================================================================

%%--------------------------------------------------------------------
%% Find the #shared_target{} record matching `Receiver' in `Ts' and
%% return it.
%%
%% If such a record does not exist, a new one is created and returned.
%%--------------------------------------------------------------------

-spec get_target(Ts       :: irc_zipper:zipper_list(#shared_target{}),
                 Receiver :: term()) ->
          #shared_target{}.

get_target(Ts, Receiver) ->
    Pred = fun (#shared_target{receiver = R}) when R =:= Receiver -> true;
               (_)                                                -> false
           end,

    case irc_zipper:find(Pred, Ts) of
        error -> #shared_target{receiver = Receiver};
        Else  -> Else
    end.


%%--------------------------------------------------------------------
%% Replace the records in `Ts' with `T' if they have the same
%% `receiver' entry.
%%
%% If `Ts' is empty, the record is added in `Ts'.
%%--------------------------------------------------------------------

-spec set_target(Ts :: irc_zipper:zipper_list(#shared_target{}),
                 T  :: #shared_target{}) ->
          irc_zipper:zipper_list(#shared_target{}).

set_target(Ts, #shared_target{receiver = R} = T) ->
    case irc_zipper:is_empty(Ts) of
        true  ->
            irc_zipper:set(Ts, T);
        false ->
            F = fun (#shared_target{receiver = R1}) when R1 =:= R -> true;
                    (_Else)                                       -> false
                end,

            case irc_zipper:update(Ts, F, T) of
                false -> irc_zipper:append(Ts, T);
                Else  -> Else
            end
    end.
