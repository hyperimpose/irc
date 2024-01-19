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
%%% IRCv3 SASL authentication implementation.
%%%
%%% - Side module to irc_runtime, which forwards all SASL related
%%%   commands and numerics here
%%%-------------------------------------------------------------------

-module(ircv3_sasl).

-include_lib("kernel/include/logger.hrl").


-export([init/1, init/3, authenticate/2, rpl_loggedin/2, rpl_loggedout/2,
         err_nicklocked/2, rpl_saslsuccess/2, err_saslfail/2, err_sasltoolong/2,
         err_saslaborted/2, err_saslalready/2, rpl_saslmechs/2]).


init(Id) ->
    case irc_state:has_cap_list(Id, <<"sasl">>) of
        false -> false;
        true  -> init1(Id)
    end.

init1(Id) ->
    case irc_state:get_cap_ls(Id) of
        #{<<"sasl">> := Args} ->
            init(Id, Args, irc_state:get_auth(Id));
        _Else                 ->
            false
    end.


%%--------------------------------------------------------------------
%% Initiate SASL Authentication.
%%
%% The  irc_config  module maintains  a  list  of SASL  authentication
%% methods that are tried in sequence. This function uses this list to
%% decide which method will be tried next.
%%
%% @param Args The mechanisms supported by the IRC server as they were
%%             returned during the CAP LS command. It can be the empty
%%             string, in which case checks for server support of a
%%             mechanism are skipped.
%% @param Sasl The previous SASL auth method that was used. It can be
%%             obtained through the function irc_state:get_auth(Id).
%%             When `false' the first method from the irc_config list
%%             will be used.
%% @returns `true' if SASL auth was initiated, else `false'.
%%--------------------------------------------------------------------

-spec init(term(), Args :: string(), Sasl :: {sasl, ircv3:sasl()} | false) ->
          boolean().

init(Id, Args, Sasl) ->
    case irc_config:next_sasl(irc_state:get_config(Id), Sasl) of
        {ok, S}   -> init1(S, Id, Args);
        undefined -> false;  % No auth config
        false     -> false;  % Proceed without auth
        empty     ->
            % All the auth methods failed:
            % stop the client process from connecting
            % show error message to the user using logs
            todo
    end.

init1(Sasl, Id, <<>>) -> init5(Sasl, Id);
init1(Sasl, Id, Args) -> init4(Sasl, Id, Args).

init4({plain, _User, _Pass} = Sasl, Id, Args) ->
    case string:find(Args, "PLAIN") of
        nomatch -> init(Id, Args, Sasl);  %% Try next auth method.
        _       -> init5(Sasl, Id)
    end;
init4(_Sasl, _Id, _Args) ->
    false.

init5({plain, _User, _Pass} = Sasl, Id) ->
    ?LOG_INFO("[IRC:~p] Trying SASL PLAIN auth", [Id]),
    irc_state:set_auth(Id, {sasl, Sasl}),
    irc_send:schedule(Id, irc_command:authenticate(plain)),
    true.


%%% AUTHENTICATE =====================================================

authenticate(Id, Message) ->
    case irc_parser:authenticate(Message) of
        empty      -> empty(Id);
        {ok, _D}   -> todo;
        {more, _D} -> todo
    end.

empty(Id) ->
    case irc_state:get_auth(Id) of
        {sasl, {plain, User, Pass}} ->
            B = base64:encode(<<User/binary, 0, User/binary, 0, Pass/binary>>),
            irc_send:schedule(Id, irc_command:authenticate(B));
        _Else                       ->
            todo
    end.


%%% 900 - RPL_LOGGEDIN ===============================================

rpl_loggedin(Id, Message) ->
    {ok, _N, _P, A, _T} = irc_parser:rpl_loggedin(Message),
    irc_state:set_account(Id, A).


%%% 901 - RPL_LOGGEDOUT ==============================================

rpl_loggedout(Id, _Message) ->
    irc_state:unset_account(Id).


%%% 902 - ERR_NICKLOCKED =============================================

err_nicklocked(_Id, _Message) ->
    %% log the error to tell the user
    %% depending on settings either apply an underscore to the nick
    %% or disconnect from the server.
    todo.


%%% 903 - RPL_SASLSUCCESS ============================================

rpl_saslsuccess(Id, _Message) ->
    ?LOG_INFO("[IRC:~p] Authenticated using SASL PLAIN", [Id]),
    irc_send:schedule(Id, irc_command:cap_end()),
    irc_state:set_cap_end(Id, true).


%%% 904 - ERR_SASLFAIL ===============================================

err_saslfail(Id, _Message) ->
    {_, Sasl} = irc_state:get_sasl(Id),
    ?LOG_ERROR("[IRC:~p] {904} SASL Failed: ~p", [Id, Sasl]),
    case init(Id) of  % Try the next auth method
        false -> irc_state:set_sasl(Id, undefined);
        _Else -> []
    end.


%%% 905 - ERR_SASLTOOLONG ============================================

err_sasltoolong(Id, Message) ->
    {ok, _N, T} = irc_parser:err_sasltoolong(Message),
    ?LOG_ERROR("[IRC:~p] {905} ~p - Please, report this error.", [Id, T]),
    case init(Id) of  % Try the next auth method
        false -> irc_state:set_sasl(Id, undefined);
        _Else -> []
    end.


%%% 906 - ERR_SASLABORTED ============================================

err_saslaborted(Id, Message) ->
    {ok, _N, T} = irc_parser:err_saslaborted(Message),
    ?LOG_NOTICE("[IRC:~p] {906} ~p", [Id, T]).


%%% 907 - ERR_SASLALREADY ============================================

err_saslalready(Id, Message) ->
    {ok, _N, T} = irc_parser:err_saslalready(Message),
    ?LOG_WARNING("[IRC:~p] {907} ~p", [Id, T]).


%%% 908 - RPL_SASLMECHS ==============================================

rpl_saslmechs(Id, Message) ->
    {ok, _N, M, _T} = irc_parser:rpl_saslmechs(Message),
    ?LOG_NOTICE("[IRC:~p] {908} Available SASL mechanisms: ~p", [Id, M]),
    case init(Id) of  % Try the next auth method
        false -> irc_state:set_sasl(Id, undefined);
        _Else -> []
    end.
