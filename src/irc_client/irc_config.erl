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
%%% Module to read config options from a settings module.
%%%
%%% To connect to an IRC server using this OTP application you must
%%% implement a settings module.
%%% The default name of this module is irc_settings. This name can be
%%% changed through the irc_config_module application env.
%%% This module must use -behaviour(irc_config) as defined below.
%%%
%%% You must make sure that the config returned is in the correct
%%% format according to the types provided.
%%%
%%% - A unique atom called the Id is used to discern different IRC
%%%   client instances. This Id is passed to the irc_settings:get/1
%%%   function which has to return the appropriate configuration.
%%% - After you get the configuration with get/1, pass the config to
%%%   the other functions to get access to the various options.
%%%-------------------------------------------------------------------

-module(irc_config).

-callback get(Id :: atom()) -> {ok, config()}.


-export([get/1, new/0,
         %% Connection
         get_address/1, set_address/2,
         get_port/1, set_port/2,
         get_tls/1, set_tls/2,
         get_server_password/1, set_server_password/2,
         get_packet_size/1, set_packet_size/2,
         get_irc_send_mode/1, set_irc_send_mode/2,
         %% Client
         get_nickname/1, set_nickname/2,
         get_user/1, set_user/2,
         get_realname/1, set_realname/2,
         get_nickserv/1, set_nickserv/2,
         get_channels/1, set_channels/2,
         %% IRCv3
         get_sasl/1, set_sasl/2, next_sasl/2,
         get_cap_want/1, set_cap_want/2,
         %% Handler
         get_handler/1, set_handler/2]).


-export_type([config/0, address/0, irc_send_mode/0, nickserv/0, channels/0,
              handler/0]).

-type address() :: inet:socket_address() | inet:hostname().
-type irc_send_mode() :: fifo | {shared, map()}.
-type nickserv() :: #{prefix := binary(), recover := binary()}.
-type channels() :: #{Channel :: binary() => Password :: binary()}.

-type handler() :: fun((Id :: atom(), Msg :: irc_parser:message()) -> any()).

-type config() :: #{%% Connection
                    address         => address(),
                    port            => inet:port_number(),
                    tls             => boolean(),
                    server_password => string() | undefined,
                    packet_size     => integer(),
                    irc_send_mode   => irc_send_mode(),
                    %% Client
                    nickname => string(),
                    user     => string(),
                    realname => string(),
                    nickserv => nickserv(),
                    channels => channels(),
                    %% IRCv3
                    sasl     => [ircv3:sasl()],
                    cap_want => [ircv3:cap()],
                    %% Metadata
                    network => atom(),
                    %% Handler
                    handler => handler()}.


%%%===================================================================
%%% API
%%%===================================================================

%%% Configuration ====================================================

-spec get(Id :: atom()) -> {ok, config()}.
get(Id) ->
    {ok, Module} = application:get_env(irc_config_module),
    Module:get(Id).

-spec new() -> #{}.
new() -> #{}.

%%% address ==========================================================

-spec get_address(config()) -> address().
get_address(#{address := Address}) -> Address.

-spec set_address(config(), address()) -> config().
set_address(Config, Address) -> Config#{address => Address}.

%%% port =============================================================

-spec get_port(config()) -> inet:port_number().
get_port(Config) -> maps:get(port, Config, 6697).

-spec set_port(config(), inet:port_number()) -> config().
set_port(Config, Port) -> Config#{port => Port}.

%%% tls ==============================================================

-spec get_tls(config()) -> boolean().
get_tls(Config) -> maps:get(tls, Config, true).

-spec set_tls(config(), boolean()) -> config().
set_tls(Config, Tls) -> Config#{tls => Tls}.

%%% server_password ==================================================

-spec get_server_password(config()) -> string() | undefined.
get_server_password(Config) -> maps:get(server_password, Config, undefined).

-spec set_server_password(config(), string() | undefined) -> config().
set_server_password(Config, Password) -> Config#{server_password => Password}.

%%% packet_size ======================================================

-spec get_packet_size(config()) -> integer().
get_packet_size(Config) -> maps:get(packet_size, Config, 512).

-spec set_packet_size(config(), integer()) -> config().
set_packet_size(Config, Size) -> Config#{packet_size => Size}.

%%% irc_send_mode ====================================================

-spec get_irc_send_mode(config()) -> irc_send_mode().
get_irc_send_mode(Config) ->
    Default = {shared, #{commands => 1, interval => 2000, size => 60}},
    maps:get(irc_send_mode, Config, Default).

-spec set_irc_send_mode(config(), irc_send_mode()) -> config().
set_irc_send_mode(Config, Mode) -> Config#{irc_send_mode => Mode}.

%%% nickname =========================================================

-spec get_nickname(config()) -> string().
get_nickname(#{nickname := Nickname}) -> Nickname.

-spec set_nickname(config(), string()) -> config().
set_nickname(Config, Nickname) -> Config#{nickname => Nickname}.

%%% user =============================================================

-spec get_user(config()) -> string().
get_user(#{user := User}) -> User.

-spec set_user(config(), string()) -> config().
set_user(Config, User) -> Config#{user => User}.

%%% realname =========================================================

-spec get_realname(config()) -> string().
get_realname(#{realname := Realname}) -> Realname.

-spec set_realname(config(), string()) -> config().
set_realname(Config, Realname) -> Config#{realname => Realname}.

%%% nickserv =========================================================

-spec get_nickserv(config()) -> nickserv().
get_nickserv(Config) -> maps:get(nickserv, Config, #{}).

-spec set_nickserv(config(), nickserv()) -> config().
set_nickserv(Config, Nickserv) -> Config#{nickserv => Nickserv}.

%%% channels =========================================================

-spec get_channels(config()) -> channels().
get_channels(Config) -> maps:get(channels, Config, #{}).

-spec set_channels(config(), channels()) -> config().
set_channels(Config, Channels) -> Config#{channels => Channels}.

%%% sasl =============================================================

-spec get_sasl(config()) -> [ircv3:sasl()].
get_sasl(Config) -> maps:get(sasl, Config, []).

-spec set_sasl(config(), [ircv3:sasl()]) -> config().
set_sasl(Config, Sasl) -> Config#{sasl => Sasl}.


-spec next_sasl(Config :: config(), Current :: ircv3:sasl() | false) ->
          {ok, ircv3:sasl()} | empty.

next_sasl(Config, SaslCurrent) -> nsasl(get_sasl(Config), SaslCurrent).

nsasl([S       | _], false)        -> {ok, S};
nsasl([Sasl, S | _], {sasl, Sasl}) -> {ok, S};
nsasl([_       | R], {sasl, Sasl}) -> nsasl(R, Sasl);
nsasl([],            _)            -> empty.

%%% cap_want =========================================================

%%% The `cap_want'  setting is a  list of IRCv3 capabilities  that the
%%% bot will enable if they are supported by the server.
%%%
%%% Only the capabilities in this list  will be enabled, so if you are
%%% going to change the defaults make sure to include the capabilities
%%% for any drastikbot features you want to use (eg. SASL).

-spec get_cap_want(config()) -> [ircv3:cap()].
get_cap_want(Config) -> maps:get(cap_want, Config, [<<"sasl">>]).

-spec set_cap_want(config(), [ircv3:cap()]) -> config().
set_cap_want(Config, CapWant) -> Config#{cap_want => CapWant}.

%%% handler ==========================================================

-spec get_handler(config()) -> handler().
get_handler(Config) -> maps:get(handler, Config).

-spec set_handler(config(), handler()) -> config().
set_handler(Config, Handler) -> Config#{handler => Handler}.
