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
%%% IRCv3 capability negotiation implementation.
%%%
%%% - Side module to irc_runtime, which forwards all CAP commands here
%%% - Will initiate SASL if configured and supported by the server
%%%-------------------------------------------------------------------

-module(ircv3_cap).

-include("irc_client.hrl").


-export([cap/2]).


cap(Message, State) ->
    case irc_parser:cap(Message) of
        {cap_ls,        _, _} = M -> cap_ls(M, State);
        {cap_ls_more,   _, _} = M -> cap_ls_more(M, State);
        {cap_list,      _, _} = M -> cap_list(M, State);
        {cap_list_more, _, _} = M -> cap_list_more(M, State);
        {cap_ack,       _, _} = M -> cap_ack(M, State);
        {cap_nak,       _, _} = M -> cap_nak(M, State);
        {cap_new,       _, _} = M -> cap_new(M, State);
        {cap_del,       _, _} = M -> cap_del(M, State)
    end.


%%--------------------------------------------------------------------
%% CAP LS
%%
%% Sent by the server as a response  to the initial CAP LS command. It
%% lists the supported capabilities.
%%
%% After all  the capabilities have  been received the  callback sends
%% the CAP REQ with the needed capabilities.
%%--------------------------------------------------------------------

cap_ls({cap_ls, _Cid, Caps}, #state{id = Id, cap = CS} = S) ->
    CS1 = #cap_state{cap_ls = CapLs} = add_cap_ls(CS, Caps),

    irc_state:set_cap_ls(Id, CapLs),

    Req = ircv3:get_cap_req(Id),
    CS2 = set_cap_req(CS1, Req),
    irc_send:schedule(Id, irc_make:cap_req(Req)),

    S#state{cap = CS2}.


cap_ls_more({cap_ls_more, _Cid, Caps}, #state{cap = CS} = S) ->
    S#state{cap = add_cap_ls(CS, Caps)}.


%%--------------------------------------------------------------------
%% CAP LIST
%%
%% When sent by the server it lists the enabled capabilities.
%%--------------------------------------------------------------------

cap_list({cap_list, _Cid, Caps}, #state{id = Id, cap = CS} = S) ->
    CS1 = handle_cap_list(CS, Caps, true),
    irc_state:set_cap_list(Id, get_cap_list(CS1)),
    S#state{cap = CS1}.


cap_list_more({cap_list_more, _Cid, Caps}, #state{cap = CS} = S) ->
    S#state{cap = handle_cap_list(CS, Caps, false)}.


try_cap_list(CS, Id) ->
    case has_pending_cap_req(CS) of
        false ->
            irc_send:schedule(Id, irc_command:cap_list()),
            true;
        true  ->
            false
    end.


%%--------------------------------------------------------------------
%% CAP ACK
%%
%% This is  sent by the server  to inform us that  the capabilities we
%% requested using CAP REQ were enabled.
%%
%% We add  the given list of  enabled capabilities to the  bot's state
%% for later use.
%%
%% Because  SASL authentication  must be  performed before  the client
%% registration is finished, this function will also initiate any SASL
%% authentication methods configured.  This will happen as soon as the
%% server enables the requested SASL capability.  The rest of the SASL
%% authentication is handled by the `irc_sasl' module.
%%--------------------------------------------------------------------

cap_ack({cap_ack, _Cid, Caps}, #state{id = Id, cap = CS} = S) ->
    CS1 = add_cap_ack(CS, Caps),
    try_cap_list(CS1, Id),
    case try_sasl(CS1, Id) of
        true  ->
            S#state{cap = CS1#cap_state{sasl = true}};
        false ->
            try_cap_end(CS1, Id),
            S#state{cap = CS1}
    end.


%%--------------------------------------------------------------------
%% CAP NAK
%%
%% This is  sent by the server  to inform us that  the capabilities we
%% requested using CAP REQ were denied.
%%
%% We add the given list of denied capabilities to the bot's state for
%% later use.
%%--------------------------------------------------------------------

cap_nak({cap_nak, _Cid, Caps}, #state{id = Id, cap = CS} = S) ->
    CS1 = add_cap_nak(CS, Caps),
    try_cap_list(CS1, Id),
    case try_sasl(CS1, Id) of
        true  ->
            S#state{cap = CS1#cap_state{sasl = true}};
        false ->
            try_cap_end(CS1, Id),
            S#state{cap = CS1}
    end.


cap_new({cap_new, _Cid, Caps}, #state{id = Id, cap = CS} = S) ->
    CS1 = #cap_state{cap_ls = CapLs} = add_cap_ls(CS, Caps),
    irc_state:set_cap_ls(Id, CapLs),
    Req = ircv3:get_cap_req(Id),
    CS2 = set_cap_req(CS1, Req),
    irc_send:schedule(Id, irc_make:cap_req(Req)),
    S#state{cap = CS2}.


cap_del({cap_del, _Cid, Caps}, #state{id = Id, cap = CS} = S) ->
    CS1 = #cap_state{cap_ls = CapLs} = del_cap_ls(CS, Caps),
    irc_state:set_cap_ls(Id, CapLs),
    Req = ircv3:get_cap_req(Id),
    CS2 = set_cap_req(CS1, Req),
    irc_send:schedule(Id, irc_command:cap_list()),
    S#state{cap = CS2}.


%%--------------------------------------------------------------------
%% Attempt to finish the capability negotiation.
%%
%% It will avoid sending CAP END  if any other following conditions is
%% met:
%%
%% - Authentication is in progress
%% - Not every requested capability has been accepted or rejected
%%
%% We do this  because in both cases sending CAP  END will prevent the
%% authentication process from completing or starting.
%%--------------------------------------------------------------------

-spec try_cap_end(CS :: #cap_state{}, Id :: any()) -> any().

try_cap_end(CS, Id) ->
    case irc_state:get_cap_end(Id) of
        true  -> false;
        false -> try_cap_end1(CS, Id)
    end.

try_cap_end1(CS, Id) ->
    case CS#cap_state.sasl of
        true  -> false;  % CAP END will be handled by the irc_sasl module
        false -> try_cap_end2(CS, Id)
    end.

try_cap_end2(CS, Id) ->
    case has_pending_cap_req(CS) of
        true  -> false;
        false -> irc_send:schedule(Id, irc_command:cap_end()),
                 irc_state:set_cap_end(Id, true),
                 true
    end.


%%--------------------------------------------------------------------
%% Attempt to authenticate using SASL.
%%
%% @returns  `true' if  a message  initiating SASL  authentication was
%% just sent, otherwise it returns `false'.
%%--------------------------------------------------------------------

-spec try_sasl(CS :: #cap_state{}, Id :: any()) -> boolean().

try_sasl(CS, Id) ->
    case CS#cap_state.sasl of
        true  -> false;  % SASL auth has already been started
        false -> try_sasl1(CS, Id)
    end.

try_sasl1(CS, Id) ->
    %% If the  initial capability negotiation  has ended, there  is no
    %% need to attempt SASL auth here.
    case irc_state:get_cap_end(Id) of
        true  -> false;
        false -> try_sasl2(CS, Id)
    end.

try_sasl2(CS, Id) ->
    case has_pending_cap_req(CS) of
        true  -> false;  % Wait until all the caps have been processed
        false -> try_sasl3(CS, Id)
    end.

try_sasl3(CS, Id) ->
    case has_cap_ack(CS, <<"sasl">>) of
        false -> false;
        true  -> try_sasl4(CS, Id)
    end.

try_sasl4(CS, Id) ->
    case CS#cap_state.cap_ls of
        #{<<"sasl">> := A} -> ircv3_sasl:init(Id, A, irc_state:get_auth(Id));
        _Else              -> false
    end.


%%% #cap_state{} Helpers =============================================

add_cap_ls(#cap_state{cap_ls = Ls} = CS, Caps) ->
    CS#cap_state{cap_ls = maps:merge(Ls, maps:from_list(Caps))}.


del_cap_ls(#cap_state{cap_ls = Ls} = CS, Caps) ->
    F = fun (X, Acc) -> maps:remove(X, Acc) end,
    CS#cap_state{cap_ls = lists:foldl(F, Ls, Caps)}.


set_cap_req(CS, Caps) ->
    CS#cap_state{cap_req = Caps, cap_ack = sets:new(), cap_nak = sets:new()}.


add_cap_ack(#cap_state{cap_ack = Ack} = CS, Caps) ->
    CS#cap_state{cap_ack = sets:union(Ack, sets:from_list(Caps))}.


has_cap_ack(#cap_state{cap_ack = Ack}, Cap) ->
    sets:is_element(Cap, Ack).


add_cap_nak(#cap_state{cap_nak = Nak} = CS, Caps) ->
    CS#cap_state{cap_nak = sets:union(Nak, sets:from_list(Caps))}.


has_pending_cap_req(#cap_state{cap_req = Req, cap_ack = Ack, cap_nak = Nak}) ->
    not sets:is_empty(sets:subtract(sets:from_list(Req), sets:union(Ack, Nak))).


get_cap_list(#cap_state{cap_list = {_, List}}) ->
    List.

handle_cap_list(#cap_state{cap_list = {true, _}} = CS, Caps, Final) ->
    CS#cap_state{cap_list = {Final, Caps}};
handle_cap_list(#cap_state{cap_list = {_,    C}} = CS, Caps, Final) ->
    CS#cap_state{cap_list = {Final, C ++ Caps}}.
