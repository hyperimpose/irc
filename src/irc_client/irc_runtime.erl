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
%%% Internal IRC runtime message dispatcher.
%%%
%%% It checks every incoming message and performs the necessary actions
%%% to keep the IRC connection in a valid state. This includes client
%%% registration upon connecting, pings, keeping track of state updates
%%% etc.
%%%-------------------------------------------------------------------

-module(irc_runtime).

-include_lib("kernel/include/logger.hrl").


-export([message/2]).


-include("irc_client.hrl").


message(Message, State) ->
    case irc_parser:get_command(Message) of
        %% Frequently used
        <<"JOIN">> -> join(Message, State);
        <<"PART">> -> part(Message, State);
        <<"PING">> -> ping(Message, State);
        <<"NICK">> -> nick(Message, State);
        <<"353">>  -> rpl_namreply(Message, State);
        <<"366">>  -> rpl_endofnames(Message, State);
        %% Commands
        <<"CAP">>   -> ircv3_cap:cap(Message, State);
        <<"KICK">>  -> kick(Message, State);
        <<"MODE">>  -> mode(Message, State);
        <<"TOPIC">> -> topic(Message, State);
        %% Numerics
        <<"001">> -> rpl_welcome(Message, State);
        <<"005">> -> rpl_isupport(Message, State);
        <<"221">> -> rpl_umodeis(Message, State);
        <<"324">> -> rpl_channelmodeis(Message, State);
        <<"329">> -> rpl_creationtime(Message, State);
        <<"331">> -> rpl_notopic(Message, State);
        <<"332">> -> rpl_topic(Message, State);
        <<"333">> -> rpl_topicwhotime(Message, State);
        <<"352">> -> rpl_whoreply(Message, State);
        <<"376">> -> rpl_endofmotd(Message, State);
        <<"396">> -> rpl_visiblehost(Message, State);
        <<"433">> -> err_nicknameinuse(Message, State);
        %% SASL
        <<"AUTHENTICATE">> -> sasl(<<"AUTHENTICATE">>, Message, State);
        <<"900">> = C      -> sasl(C, Message, State);
        <<"901">> = C      -> sasl(C, Message, State);
        <<"902">> = C      -> sasl(C, Message, State);
        <<"903">> = C      -> sasl(C, Message, State);
        <<"904">> = C      -> sasl(C, Message, State);
        <<"905">> = C      -> sasl(C, Message, State);
        <<"906">> = C      -> sasl(C, Message, State);
        <<"907">> = C      -> sasl(C, Message, State);
        <<"908">> = C      -> sasl(C, Message, State);
        _Else -> State
    end.


%%% JOIN =============================================================

join(Message, #state{id = Id} = State) ->
    Nick = irc_parser:get_prefix_nick(Message),
    Channels = case irc_parser:join(Message) of
                   {ok, Cs}           -> Cs;
                   {ok, Cs, _AN, _RN} -> Cs
               end,

    case irc_parser:is_equal(Id, Nick, irc_state:get_nickname(Id)) of
        true  ->  %% We joined a channel
            %% Request channel modes and creation time.
            [irc_send:schedule(Id, irc_command:mode(X)) || X <- Channels],
            irc_channel:add(Id, Channels);
        false ->  %% Someone joined a channel we are in
            irc_channel:insert_user(Id, Channels, Nick, [])
    end,
    State.


%%% KICK =============================================================

kick(Message, #state{id = Id} = State) ->
    {ok, Channel, Nick, _Text} = irc_parser:kick(Message),

    Nickname = irc_parser:casefold(Id, Nick),
    SelfNick = irc_parser:casefold(Id, irc_state:get_nickname(Id)),

    case Nickname == SelfNick of
        true  ->  % part self
            irc_channel:delete(Id, [Channel]);
        false ->  % part user
            irc_channel:delete_user(Id, [Channel], Nick)
    end,

    State.


%%% MODE =============================================================

mode(Message, #state{id = Id} = State) ->
    case irc_parser:mode(Id, Message) of
        {ok, Target, Modes} -> mode_set(State, Target, Modes);
        {ok, _Target}       ->
            %% This format should only be sent by clients to the server to
            %% request the current mode settings.
            ?LOG_WARNING("[IRC:~p] MODE - Invalid format received.", [Id]),
            State
    end.

mode_set(#state{id = Id} = State, Target, Modes) ->
    case irc_isupport:is_channel(Id, Target) of
        false -> mode_user(State, Target, Modes);
        true  -> mode_channel(State, Target, Modes)
    end.


mode_channel(#state{id = Id} = State, Target, Modes) ->
    Prefixes = irc_isupport:get_prefix(Id),

    F = fun ({add, Type, Mode, Arg})    ->
                case lists:keyfind(Mode, 1, Prefixes) of
                    false           ->  % Channel Mode
                        mode_channel_add(Id, Target, Type, Mode, Arg);
                    {Mode, Prefix}  ->  % Prefix Mode
                        irc_channel:add_prefix(Id, Target, Arg, Prefix)
                end;
            ({delete, Type, Mode, Arg}) ->
                case lists:keyfind(Mode, 1, Prefixes) of
                    false          ->  % Channel Mode
                        mode_channel_delete(Id, Target, Type, Mode, Arg);
                    {Mode, Prefix} ->  % Prefix Mode
                        mode_channel_prefix_delete(Id, Target, Arg, Prefix)
                end
        end,
    lists:foreach(F, Modes),

    State.

mode_channel_add(Id, Channel, Type, Mode, Arg) ->
    Modes = irc_channel:get_modes(Id, Channel),
    case Type of
        a ->
            irc_channel:set_modes(Id, Channel, [{Mode, Arg} | Modes]);
        b ->
            Modes1 = lists:keyreplace(Mode, 1, Modes, {Mode, Arg}),
            irc_channel:set_modes(Id, Channel, Modes1);
        c ->
            Modes1 = lists:keyreplace(Mode, 1, Modes, {Mode, Arg}),
            irc_channel:set_modes(Id, Channel, Modes1);
        d ->
            irc_channel:set_modes(Id, Channel, [{Mode, Arg} | Modes])
    end.

mode_channel_delete(Id, Channel, Type, Mode, Arg) ->
    Modes = irc_channel:get_modes(Id, Channel),
    case Type of
        a ->
            Modes1 = lists:delete({Mode, Arg}, Modes),
            irc_channel:set_modes(Id, Channel, Modes1);
        b ->
            Modes1 = lists:delete({Mode, Arg}, Modes),
            irc_channel:set_modes(Id, Channel, Modes1);
        c ->
            Modes1 = lists:keydelete(Mode, 1, Modes),
            irc_channel:set_modes(Id, Channel, Modes1);
        d ->
            Modes1 = lists:keydelete(Mode, 1, Modes),
            irc_channel:set_modes(Id, Channel, Modes1)
    end.

mode_channel_prefix_delete(Id, Channel, Nick, Prefix) ->
    irc_channel:delete_prefix(Id, Channel, Nick, Prefix),

    %% Base IRC does not let us know of other prefixes set
    %% for a user, so we have to send a NAMES command to
    %% learn if the user has any other prefixes.
    case irc_state:has_cap_list(Id, "multi-prefix") of
        true  -> [];  % do nothing
        false -> irc_send:now(Id, irc_command:names([Channel]))
    end.


mode_user(#state{id = Id} = State, Target, Modes) ->
    case irc_parser:is_equal(Id, Target, irc_state:get_nickname(Id)) of
        true  -> mode_user(State, Modes);
        false ->
            %% We should not be able to receive user modes for other users.
            ?LOG_WARNING("[IRC:~p] MODE - User mode for another user.", [Id])
    end.

mode_user(#state{id = Id} = State, Modes) ->
    F = fun ({add, d, Mode, _})    ->
                M1 = lists:uniq([Mode | irc_state:get_modes(Id)]),
                irc_state:set_modes(Id, M1);
            ({delete, d, Mode, _}) ->
                M1 = irc_state:get_modes(Id),
                M2 = lists:filter(fun (X) -> X =/= Mode end, M1),
                irc_state:set_modes(Id, M2);
            (Mode)                 ->
                %% User modes are always of Type D
                ?LOG_WARNING("[IRC:~p] MODE - Unsupported user mode type.",
                             [Id], #{irc_data => Mode})
        end,
    lists:foreach(F, Modes),

    State.


%%% NICK =============================================================

nick(Message, #state{id = Id} = State) ->
    Old = irc_parser:get_prefix_nick(Message),
    {ok, New} = irc_parser:nick(Message),

    case Old == irc_state:get_nickname(Id) of
        true  -> irc_state:set_nickname(Id, New);
        false -> []  % Another client changed nickname; do nothing
    end,

    irc_channel:change_nick(Id, Old, New),

    State.


%%% PART =============================================================

part(Message, #state{id = Id} = State) ->
    Nick = irc_parser:get_prefix_nick(Message),
    {ok, Channels, _Text} = irc_parser:part(Message),

    Nickname = irc_parser:casefold(Id, Nick),
    SelfNick = irc_parser:casefold(Id, irc_state:get_nickname(Id)),

    case Nickname == SelfNick of
        true  ->  % part self
            irc_channel:delete(Id, Channels);
        false ->  % part user
            irc_channel:delete_user(Id, Channels, Nick)
    end,

    State.


%%% PING =============================================================

ping(Message, #state{id = Id} = State) ->
    %% A server is supposed to provide a single argument.
    {ok, Server} = irc_parser:ping(Message),
    irc_send:now(Id, irc_command:pong(Server)),
    State.


%%% SASL =============================================================

sasl(Command, Message, #state{id = Id} = State) ->
    case Command of
        <<"AUTHENTICATE">> -> ircv3_sasl:authenticate(Id, Message);
        <<"900">>          -> ircv3_sasl:rpl_loggedin(Id, Message);
        <<"901">>          -> ircv3_sasl:rpl_loggedout(Id, Message);
        <<"902">>          -> ircv3_sasl:err_nicklocked(Id, Message);
        <<"903">>          -> ircv3_sasl:rpl_saslsuccess(Id, Message);
        <<"904">>          -> ircv3_sasl:err_saslfail(Id, Message);
        <<"905">>          -> ircv3_sasl:err_sasltoolong(Id, Message);
        <<"906">>          -> ircv3_sasl:err_saslaborted(Id, Message);
        <<"907">>          -> ircv3_sasl:err_saslalready(Id, Message);
        <<"908">>          -> ircv3_sasl:rpl_saslmechs(Id, Message)
    end,
    State.


%%% TOPIC ============================================================

topic(Message, #state{id = Id} = State) ->
    %% A TOPIC message is send to the users already joined in a channel
    %% when someone changes the topic.
    {ok, Channel, Topic} = irc_parser:topic(Message),
    case Topic of
        <<>> ->
            irc_channel:unset_topic(Id, Channel);
        _    ->
            Nick = irc_parser:get_prefix_nick(Message),
            Timestamp = os:system_time(second),
            irc_channel:set_topic(Id, Channel, Topic),
            irc_channel:set_topic_nick(Id, Channel, Nick),
            irc_channel:set_topic_timestamp(Id, Channel, Timestamp)
    end,
    State.


%%% 005 - RPL_WELCOME ================================================

rpl_welcome(Message, #state{id = Id} = State) ->
    {ok, Nick, _Text} = irc_parser:rpl_welcome(Message),
    irc_state:set_nickname(Id, Nick),
    State.


%%% 005 - RPL_ISUPPORT ===============================================

rpl_isupport(Message, #state{id = Id} = State) ->
    {ok, _Nick, Tokens, _Text} = irc_parser:rpl_isupport(Message),
    rpl_isupport1(Tokens, Id),
    State.

rpl_isupport1([H | R], Id) ->
    isupport(H, Id),
    rpl_isupport1(R, Id);
rpl_isupport1([], _Id) ->
    [].  % return


isupport({<<"AWAYLEN">>,      V}, Id) -> irc_isupport:set_awaylen(Id, V);
isupport({<<"-AWAYLEN">>,     _}, Id) -> irc_isupport:unset_awaylen(Id);

isupport({<<"CALLERID">>,     V}, Id) -> irc_isupport:set_callerid(Id, V);
isupport({<<"-CALLERID">>,    _}, Id) -> irc_isupport:unset_callerid(Id);

isupport({<<"CASEMAPPING">>,  V}, Id) -> irc_isupport:set_casemapping(Id, V);
isupport({<<"-CASEMAPPING">>, _}, Id) -> irc_isupport:unset_casemapping(Id);

isupport({<<"CHANLIMIT">>,    V}, Id) -> irc_isupport:set_chanlimit(Id, V);
isupport({<<"-CHANLIMIT">>,   _}, Id) -> irc_isupport:unset_chanlimit(Id);

isupport({<<"CHANMODES">>,    V}, Id) -> irc_isupport:set_chanmodes(Id, V);
isupport({<<"-CHANMODES">>,   _}, Id) -> irc_isupport:unset_chanmodes(Id);

isupport({<<"CHANNELLEN">>,   V}, Id) -> irc_isupport:set_channellen(Id, V);
isupport({<<"-CHANNELLEN">>,  _}, Id) -> irc_isupport:unset_channellen(Id);

isupport({<<"CHANTYPES">>,    V}, Id) -> irc_isupport:set_chantypes(Id, V);
isupport({<<"-CHANTYPES">>,   _}, Id) -> irc_isupport:unset_chantypes(Id);

isupport({<<"ELIST">>,        V}, Id) -> irc_isupport:set_elist(Id, V);
isupport({<<"-ELIST">>,       _}, Id) -> irc_isupport:unset_elist(Id);

isupport({<<"EXCEPTS">>,      V}, Id) -> irc_isupport:set_excepts(Id, V);
isupport({<<"-EXCEPTS">>,     _}, Id) -> irc_isupport:unset_excepts(Id);

isupport({<<"EXTBAN">>,       V}, Id) -> irc_isupport:set_extban(Id, V);
isupport({<<"-EXTBAN">>,      _}, Id) -> irc_isupport:unset_extban(Id);

isupport({<<"HOSTLEN">>,      V}, Id) -> irc_isupport:set_hostlen(Id, V);
isupport({<<"-HOSTLEN">>,     _}, Id) -> irc_isupport:unset_hostlen(Id);

isupport({<<"INVEX">>,        V}, Id) -> irc_isupport:set_invex(Id, V);
isupport({<<"-INVEX">>,       _}, Id) -> irc_isupport:unset_invex(Id);

isupport({<<"KICKLEN">>,      V}, Id) -> irc_isupport:set_kicklen(Id, V);
isupport({<<"-KICKLEN">>,     _}, Id) -> irc_isupport:unset_kicklen(Id);

isupport({<<"MAXLIST">>,      V}, Id) -> irc_isupport:set_maxlist(Id, V);
isupport({<<"-MAXLIST">>,     _}, Id) -> irc_isupport:unset_maxlist(Id);

isupport({<<"MAXTARGETS">>,   V}, Id) -> irc_isupport:set_maxtargets(Id, V);
isupport({<<"-MAXTARGETS">>,  _}, Id) -> irc_isupport:unset_maxtargets(Id);

isupport({<<"MODES">>,        V}, Id) -> irc_isupport:set_modes(Id, V);
isupport({<<"-MODES">>,       _}, Id) -> irc_isupport:unset_modes(Id);

isupport({<<"NETWORK">>,      V}, Id) -> irc_isupport:set_network(Id, V);
isupport({<<"-NETWORK">>,     _}, Id) -> irc_isupport:unset_network(Id);

isupport({<<"NICKLEN">>,      V}, Id) -> irc_isupport:set_nicklen(Id, V);
isupport({<<"-NICKLEN">>,     _}, Id) -> irc_isupport:unset_nicklen(Id);

isupport({<<"PREFIX">>,       V}, Id) -> irc_isupport:set_prefix(Id, V);
isupport({<<"-PREFIX">>,      _}, Id) -> irc_isupport:unset_prefix(Id);

isupport({<<"SAFELIST">>,     _}, Id) -> irc_isupport:set_safelist(Id);
isupport({<<"-SAFELIST">>,    _}, Id) -> irc_isupport:unset_safelist(Id);

isupport({<<"STATUSMSG">>,    V}, Id) -> irc_isupport:set_statusmsg(Id, V);
isupport({<<"-STATUSMSG">>,   _}, Id) -> irc_isupport:unset_statusmsg(Id);

isupport({<<"TARGMAX">>,      V}, Id) -> irc_isupport:set_targmax(Id, V);
isupport({<<"-TARGMAX">>,     _}, Id) -> irc_isupport:unset_targmax(Id);

isupport({<<"TOPICLEN">>,     V}, Id) -> irc_isupport:set_topiclen(Id, V);
isupport({<<"-TOPICLEN">>,    _}, Id) -> irc_isupport:unset_topiclen(Id);

isupport({<<"USERLEN">>,      V}, Id) -> irc_isupport:set_userlen(Id, V);
isupport({<<"-USERLEN">>,     _}, Id) -> irc_isupport:unset_userlen(Id);

isupport({<<"WHOX">>,         _}, Id) -> irc_isupport:set_whox(Id);
isupport({<<"-WHOX">>,        _}, Id) -> irc_isupport:unset_whox(Id);

isupport(Token,                   Id) ->
    ?LOG_WARNING("[IRC:~p] ISUPPORT - Unsupported token: ~p", [Id, Token]).


%%% 221 - RPL_UMODEIS ================================================

rpl_umodeis(Message, #state{id = Id} = State) ->
    {ok, _Nick, Modes} = irc_parser:rpl_umodeis(Message),
    irc_state:set_modes(Id, Modes),
    State.


%%% 324 - RPL_CHANNELMODEIS ==========================================

rpl_channelmodeis(Message, #state{id = Id} = State) ->
    {ok, Channel, Modes} = irc_parser:rpl_channelmodeis(Id, Message),
    mode_channel(State, Channel, Modes).


%%% 329 - RPL_CREATIONTIME ===========================================

rpl_creationtime(Message, #state{id = Id} = State) ->
    {ok, _Nick, Channel, Creationtime} = irc_parser:rpl_creationtime(Message),

    Ts = case string:to_integer(Creationtime) of
             {error, Reason} ->
                 ?LOG_ERROR("[IRC:~p] RPL_CREATIONTIME - timestamp:"
                            " Error converting string to integer: ~p",
                            [Id, Reason]),
                 undefined;
             {Int, _}        ->
                 Int
         end,

    irc_channel:set_creationtime(Id, Channel, Ts),
    State.


%%% 331 - RPL_NOTOPIC ================================================

rpl_notopic(Message, #state{id = Id} = State) ->
    {ok, Channel, _Text} = irc_parser:rpl_notopic(Message),
    irc_channel:unset_topic(Id, Channel),
    State.


%%% 332 - RPL_TOPIC ==================================================

rpl_topic(Message, #state{id = Id} = State) ->
    {ok, Channel, Topic} = irc_parser:rpl_topic(Message),
    irc_channel:unset_topic(Id, Channel),  % Reset all topic fields
    irc_channel:set_topic(Id, Channel, Topic),
    State.


%%% 333 - RPL_TOPICWHOTIME ===========================================

rpl_topicwhotime(Message, #state{id = Id} = State) ->
    {ok, Channel, Nick, Timestamp} = irc_parser:rpl_topicwhotime(Message),

    Ts = case string:to_integer(Timestamp) of
             {error, Reason} ->
                 ?LOG_ERROR("[IRC:~p] RPL_TOPICWHOTIME - timestamp:"
                            " Error converting string to integer: ~p",
                            [Id, Reason]),
                 undefined;
             {Int, _}        ->
                 Int
         end,

    irc_channel:set_topic_nick(Id, Channel, Nick),
    irc_channel:set_topic_timestamp(Id, Channel, Ts),

    State.


%%% 352 - RPL_WHOREPLY ===============================================

rpl_whoreply(Message, #state{id = Id} = State) ->
    {ok, Fields} = irc_parser:rpl_whoreply(Message),
    #{user := U, host := H, nick := N} = Fields,

    case N == irc_state:get_nickname(Id) of
        false ->
            [];  % do nothing
        true  ->
            irc_state:set_user(Id, U),
            irc_state:set_host(Id, H)
    end,
    State.


%%% 353 - RPL_NAMREPLY ===============================================

rpl_namreply(Message, #state{id = Id} = State) ->
    {ok, _C, _S, Channel, Users} = irc_parser:rpl_namreply(Id, Message),
    irc_channel:handle_rpl_namreply(Id, Channel, Users),
    State.


%%% 366 - RPL_ENDOFNAMES =============================================

rpl_endofnames(Message, #state{id = Id} = State) ->
    {ok, _C, Channel, _T} = irc_parser:rpl_endofnames(Message),
    irc_channel:handle_rpl_endofnames(Id, Channel),
    State.


%%% 376 - RPL_ENDOFMOTD ==============================================

%%--------------------------------------------------------------------
%% 376 RPL_ENDOFMOTD
%%
%% After the registration process is  completed, the bot can then send
%% any other command needed, such as JOIN, PRIVMSG etc.
%%
%% To mark the  end of the registration the server  will send a series
%% of messages containing information about the client and the server.
%%
%% Usually the  first such message  is 001 RPL_WELCOME, however  it is
%% not described in RFC 1459 and as  such the bot instead uses 376 and
%% 422 which are commmon  enough to be on both RFC  1459, RFC 2812 and
%% implemented by all  the servers known to the author  at the time of
%% writing.
%%
%% When this message appears the  bot will perform the following steps
%% to  learn  the necessary  runtime  information:
%%
%% - Get the actual nickname assigned to the bot (first PARAM)
%% - Send  a WHO  command for  the bot's nickname  and learn  what the
%% assigned hostname  is.  We do  this because we  want to be  able to
%% split messages properly and avoid going over the 512 byte limit.
%% - JOIN channels
%%--------------------------------------------------------------------

rpl_endofmotd(Message, #state{id = Id, conf = Conf} = State) ->
    {ok, Nick, _Text} = irc_parser:rpl_endofmotd(Message),
    irc_state:set_nickname(Id, Nick),

    %% If the  nickname we  ended up  with isn't the  same as  the one
    %% configured, try and recover it using nickserv.
    %% The response from NickServ is NOT checked.
    case irc_parser:is_equal(Id, Nick, irc_config:get_nickname(Conf)) of
        false -> try_nickserv_recover(Id, Conf);
        true  -> []  % do nothing
    end,

    %% Get the current client information such as <host>
    irc_send:now(Id, irc_command:who(Nick)),

    C1 = irc_config:get_channels(Conf),
    irc_send:schedule(Id, irc_command:join(C1)),

    State.


try_nickserv_recover(Id, Conf) ->
    case irc_config:get_nickserv(Conf) of
        #{prefix := NS, recover := R, recover_nick := N} ->
            ?LOG_INFO("[IRC:~p] Recovering nickname: ~p", [Id, N]),
            irc_send:schedule(Id, irc_command:privmsg(NS, R)),
            irc_state:set_nickname(Id, N);
        _Else                                            ->
            []  %% do nothing
   end.


%%% 396 - RPL_VISIBLEHOST ============================================

rpl_visiblehost(Message, #state{id = Id} = State) ->
    {ok, _N, Host, _T} = irc_parser:rpl_visiblehost(Message),
    irc_state:set_host(Id, Host),
    State.


%%% 433 - ERR_NICKNAMEINUSE ==========================================

err_nicknameinuse(Message, #state{id = Id} = State) ->
    Params = irc_parser:get_params(Message),
    Nick = irc_state:get_nickname(Id),
    ?LOG_INFO("[IRC:~p] {433} Nickname ~p | ~p", [Id, Nick, Params]),
    NextNick = <<Nick/binary, $_>>,
    ?LOG_INFO("[IRC:~p] {433} Trying nickname: ~p", [NextNick]),
    irc_send:now(Id, irc_command:nick(NextNick)),
    irc_state:set_nickname(Id, NextNick),
    State.
