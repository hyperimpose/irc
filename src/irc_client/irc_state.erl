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
%%% Module for setting and getting the state of the IRC client.
%%%
%%% Each option is saved in the client instance ETS table. This module
%%% provides a CRUD interface for each option.
%%%
%%% The reader functions (prefixed with get_, has_) and the group API
%%% can be used by external, to this OTP application, code.
%%% Every other function is for internal use. The client instance will
%%% automatically update the state.
%%%-------------------------------------------------------------------

-module(irc_state).


-export([get_socket/1, set_socket/2]).

-export([get_cap_ls/1,   set_cap_ls/2,
         get_cap_list/1, set_cap_list/2, has_cap_list/2,
         get_cap_end/1,  set_cap_end/2]).

-export([get_account/1, set_account/2, unset_account/1,
         get_config/1, set_config/2,
         get_nickname/1, set_nickname/2,
         get_user/1, set_user/2,
         get_host/1, set_host/2,
         get_auth/1, set_auth/2,
         get_modes/1, set_modes/2]).

%% Group API
-export([group_prefix/1, group_length_prefix/1]).



%%% The `cap_ls' setting is a  map of the IRCv3 capabilities supported
%%% by the  server with their  arguments. If the server  doesnt report
%%% any arguments an empty binary <<>> is set as the value.

-type cap_ls() :: #{ircv3:cap() => ircv3:cap_args()}.


%%% The `auth' setting is the method the bot used for authentication.
%%%
%%% The `auth_conf' setting  is a list of  authentication methods used
%%% with  the  IRC   server.  The  methods  in  the   list  are  tried
%%% incrementally until one of them succeeds.
%%%
%%% If none of the methods in the  list succeed the bot will abort the
%%% connection with the IRC server. To proceed anyway, you may add the
%%% atom  `false' at  the end  of  the list,  which tells  the bot  to
%%% connect without authentication. An empty  list also means that the
%%% bot should not authenticate.
%%%
%%% If the  list starts  with a  SASL method the  bot will  attempt to
%%% authenticate during  IRCv3 capability negotiation  (before joining
%%% any channels).
%%%
%%% See  the auth()  type for  details on  the format  of the  various
%%% methods.

%%%===================================================================
%%% Connection API
%%%===================================================================

%%% socket ===========================================================

get_socket(Id) ->
    case ets:lookup(Id, socket) of
        [{socket, Socket}] -> Socket;
        []                 -> undefined
    end.

set_socket(Id, Socket) ->
    ets:insert(Id, {socket, Socket}).


%%%===================================================================
%%% Client API
%%%===================================================================

%%% config ===========================================================

get_config(Id) ->
    case ets:lookup(Id, config) of
        [{config, Config}] -> Config;
        []                 -> undefined
    end.


set_config(Id, Config) ->
    ets:insert(Id, {config, Config}).


%%% nickname =========================================================

get_nickname(Id) ->
    case ets:lookup(Id, nickname) of
        [{nickname, Nickname}] -> Nickname;
        []                     -> undefined
    end.


set_nickname(Id, Nickname) ->
    ets:insert(Id, {nickname, iolist_to_binary(Nickname)}).


%%% user =============================================================

get_user(Id) ->
    case ets:lookup(Id, user) of
        [{user, User}] -> User;
        []             -> undefined
    end.


set_user(Id, User) ->
    ets:insert(Id, {user, User}).


%%% host =============================================================

get_host(Id) ->
    case ets:lookup(Id, host) of
        [{host, Host}] -> Host;
        []             -> undefined
    end.


set_host(Id, Host) ->
    ets:insert(Id, {host, Host}).


%%% auth =============================================================

get_auth(Id) ->
    case ets:lookup(Id, auth) of
        [{auth, Auth}] -> Auth;
        []             -> false
    end.


set_auth(Id, Auth) ->
    ets:insert(Id, {auth, Auth}).


%%% modes ============================================================

get_modes(Id) ->
    case ets:lookup(Id, modes) of
        [{modes, Modes}] -> Modes;
        []               -> []
    end.


set_modes(Id, Modes) ->
    ets:insert(Id, {modes, Modes}).


%%%===================================================================
%%% IRCv3 API
%%%===================================================================

%%% account ==========================================================

get_account(Id) ->
    case ets:lookup(Id, account) of
        [{account, Account}] -> Account;
        []                   -> undefined
    end.


set_account(Id, Account) ->
    ets:insert(Id, {account, Account}).


unset_account(Id) ->
    ets:delete(Id, account).


%%% cap_ls ===========================================================

-spec get_cap_ls(Id :: any()) -> cap_ls().

get_cap_ls(Id) ->
    case ets:lookup(Id, cap_ls) of
        [{cap_ls, Caps}] -> Caps;
        []               -> #{}
    end.


-spec set_cap_ls(Id :: any(), Caps :: cap_ls()) -> true.

set_cap_ls(Id, Caps) ->
    ets:insert(Id, {cap_ls, Caps}).


%%% cap_list =========================================================

-spec get_cap_list(Id :: any()) -> [ircv3:cap()].

get_cap_list(Id) ->
    case ets:lookup(Id, cap_list) of
        [{cap_list, Caps}] -> Caps;
        []                 -> []
    end.


-spec set_cap_list(Id :: any(), Caps :: [ircv3:cap()]) -> true.

set_cap_list(Id, Caps) ->
    ets:insert(Id, {cap_list, Caps}).


-spec has_cap_list(Id :: any(), Cap :: unicode:chardata()) -> boolean().

has_cap_list(Id, Cap) ->
    lists:any(fun (X) -> string:equal(X, Cap) end, get_cap_list(Id)).


%%% cap_end ==========================================================

-spec get_cap_end(Id :: any()) -> boolean().

get_cap_end(Id) ->
    case ets:lookup(Id, cap_end) of
        [{cap_end, Value}] -> Value;
        []                 -> false
    end.


-spec set_cap_end(Id :: any(), Value :: boolean()) -> true.

set_cap_end(Id, Value) ->
    ets:insert(Id, {cap_end, Value}).


%%%===================================================================
%%% Group API (Functions that group data into a single call)
%%%===================================================================

-spec group_prefix(Id :: term()) ->
          {Nick :: binary(), User :: binary(), Host :: binary()}.

group_prefix(Id) ->
    {get_nickname(Id), get_user(Id), get_host(Id)}.


%%--------------------------------------------------------------------
%% Get  the current  prefix and  the  maximum allowed  length for  IRC
%% messages.
%%
%% This is meant to be used for calculating the maximum length of text
%% that can be added to a command like PRIVMSG.
%%
%% @todo  Currently  the max  length  is  fixed  to the  standard  512
%% bytes. Some  servers may  implement mechanisms for  increasing this
%% limit.
%%
%% The irc_config module has the  packet_size option. The intention is
%% to let the user insert the  message length supported by the server.
%% However  the  option is  not  used  anywhere  because if  a  server
%% supports  a non  standard length  it will  probably have  a way  to
%% negotiate this  for backwards compatibility. Additionally  the user
%% adding such  an option  does not mean  that the  server necessarily
%% supports this and issues might be caused.
%%
%% The  LINELEN option  from  the ISUPPORT  numeric  and a  capability
%% existed once, but they are both considered deprecated.
%%
%% Because of the  above non standard lengths will  not be implemented
%% until an  actual server  that uses  them is  encountered. Attention
%% must be paid to make it such that the implementation is not tied to
%% a single mechanism.
%%
%% @todo  Instead of  having the  caller calculate  the length  of the
%% Nick, User, Host dynamically every time  have them be stored in the
%% state and return the remaining length by the function.
%%--------------------------------------------------------------------

-spec group_length_prefix(Id :: term()) ->
          {integer(), Nick :: binary(), User :: binary(), Host :: binary()}.

group_length_prefix(Id) ->
    {512, get_nickname(Id), get_user(Id), get_host(Id)}.
