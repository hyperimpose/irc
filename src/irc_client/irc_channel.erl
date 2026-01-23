%%--------------------------------------------------------------------
%% Copyright 2023-2026 hyperimpose.org
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
%%% Module for managing the IRC channels.
%%%
%%% For each channel joined we maintain its state inside the ETS table
%%% managed by the irc_client gen_server. This module provides a CRUD
%%% interface to the state of each channel.
%%%
%%% The reader functions (prefixed with get_ or has_) can be used by
%%% external, to this OTP application, code.
%%% Every other function is for internal use. The client instance will
%%% automatically update the state.
%%%-------------------------------------------------------------------

-module(irc_channel).

-include_lib("kernel/include/logger.hrl").


-export([add/2, delete/2, list/1, is_joined/2]).
-export([get_topic/2, set_topic/3,
         get_topic_nick/2, set_topic_nick/3,
         get_topic_timestamp/2, set_topic_timestamp/3,
         unset_topic/2]).
-export([get_creationtime/2, set_creationtime/3]).
-export([get_modes/2, set_modes/3]).
-export([insert_user/4, change_nick/3, delete_user/2, delete_user/3,
         delete_users/2, get_users/2, add_prefix/4, delete_prefix/4]).

%% IRC API
-export([handle_rpl_namreply/3, handle_rpl_endofnames/2]).




%%%===================================================================
%%% State API
%%%===================================================================

-doc """
* `nickname` - The `name` casefolded
* `name`     - The nickname as given by the server
* `prefix`   - The prefixes set for the user (for example: `[$@, $+]`)
""".
-type user() :: #{nickname := binary(),
                  name := iodata(),
                  prefix := [char()]}.

-record(channel, {channel      :: binary(),  % The name casefolded
                  name         :: iodata(),  % As given. Includes the type
                  topic        :: binary() | undefined,   % The topic itself
                  topic_nick   :: binary() | undefined,   % The nick that set it
                  topic_when   :: integer() | undefined,  % UNIX timestamp
                  creationtime :: integer() | undefined,  % UNIX timestamp
                  modes = []   :: [{char(), binary()}],
                  users = #{}  :: #{Nick :: binary() => user()},
                  %% State
                  rpl_endofnames = false :: boolean()}).

-define(KEY(Channel), {channel, Channel}).
-define(INSERT(Id, Channel, Record), ets:insert(Id, {?KEY(Channel), Record})).


%%% Helper functions =================================================

get(Id, Name) ->
    Channel = irc_parser:casefold(Id, Name),

    case ets:lookup(Id, ?KEY(Channel)) of
        [{_, R}] -> {ok, R};
        []  -> {error, not_joined}
    end.


update(Id, Name, Fun) ->
    Channel = irc_parser:casefold(Id, Name),
    case ets:lookup(Id, ?KEY(Channel)) of
        [{_, R}] -> ?INSERT(Id, Channel, Fun(R));
        _        -> {error, not_joined}
    end.


update_all(Id, Fun) ->
    lists:foreach(fun ([R]) -> ?INSERT(Id, R#channel.channel, Fun(R)) end,
                  ets:match(Id, {?KEY('_'), '$1'})).


update_many(Id, Names, Fun) ->
    lists:foreach(fun (Name) -> update(Id, Name, Fun) end, Names).


%%% channels =========================================================

-doc false.
-spec add(Id :: term(), Names :: [iodata()]) -> ok.

add(Id, Names) ->
    lists:foreach(fun (Name) ->
                          Channel = irc_parser:casefold(Id, Name),
                          R = #channel{channel = Channel, name = Name},
                          ?INSERT(Id, Channel, R)
                  end,
                  Names).


-doc false.
-spec delete(Id :: term(), Names :: [iodata()]) -> [true].

delete(Id, Names) ->
    lists:map(fun (Name) ->
                      Channel = irc_parser:casefold(Id, Name),
                      ets:delete(Id, ?KEY(Channel))
              end,
              Names).


-doc "List joined channels.".
-spec list(Id :: atom()) -> [binary()].

list(Id) ->
    lists:map(fun ([#channel{name = Name}]) -> Name end,
              ets:match(Id, {?KEY('_'), '$1'})).


-doc "Returns `true` if joined to the channel; otherwise, returns `false`".
-spec is_joined(Id :: atom(), Name :: iodata()) -> boolean().

is_joined(Id, Name) ->
    case get(Id, Name) of
        {ok, _}             -> true;
        {error, not_joined} -> false
    end.


%%% topic ============================================================

-doc """
Get the topic set for this channel.

If the channel has not been joined, a `badarg` exception is raised.
""".
-spec get_topic(Id :: atom(), Name :: iodata()) -> binary() | undefined.

get_topic(Id, Name) ->
    {ok, R} = get(Id, Name),
    R#channel.topic.


-doc false.
set_topic(Id, Name, Topic) ->
    update(Id, Name, fun (R) -> R#channel{topic = Topic} end).


-doc """
Get the nickname of the person that set the topic.

If the channel has not been joined, a `badarg` exception is raised.
""".
-spec get_topic_nick(Id :: atom(), Name :: iodata()) -> binary() | undefined.

get_topic_nick(Id, Name) ->
    {ok, R} = get(Id, Name),
    R#channel.topic_nick.


-doc false.
set_topic_nick(Id, Name, Nick) ->
    update(Id, Name, fun (R) -> R#channel{topic_nick = Nick} end).


-doc """
Get the topic set time as a UNIX timestamp.

If the channel has not been joined, a `badarg` exception is raised.
""".
-spec get_topic_timestamp(Id :: atom(), Name :: iodata())
                         -> binary() | undefined.

get_topic_timestamp(Id, Name) ->
    {ok, R} = get(Id, Name),
    R#channel.topic_when.


-doc false.
set_topic_timestamp(Id, Name, Timestamp) ->
    update(Id, Name, fun (R) -> R#channel{topic_when = Timestamp} end).


-doc false.
unset_topic(Id, Name) ->
    F = fun (R) ->
                R#channel{topic = undefined,
                          topic_nick = undefined,
                          topic_when = undefined}
        end,
    update(Id, Name, F).


%%% creationtime =====================================================

-doc """
Get the channel creation time as a UNIX timestamp.

If the channel has not been joined, a `badarg` exception is raised.
""".
-spec get_creationtime(Id :: atom(), Name :: iodata()) -> integer() | undefined.

get_creationtime(Id, Name) ->
    {ok, R} = get(Id, Name),
    R#channel.creationtime.


-doc false.
set_creationtime(Id, Name, Time) ->
    update(Id, Name, fun (R) -> R#channel{creationtime = Time} end).


%%% modes ============================================================

-doc """
Get the channel modes.

Each mode is represented as a `{Mode, Parameter}` tuple.

If the channel has not been joined, a `badarg` exception is raised.
""".
-spec get_modes(Id :: atom(), Name :: iodata())
               -> [{char(), binary()}] | undefined.

get_modes(Id, Name) ->
    {ok, R} = get(Id, Name),
    R#channel.modes.


-doc false.
set_modes(Id, Name, Modes) ->
    update(Id, Name, fun (R) -> R#channel{modes = Modes} end).


%%% users ============================================================

-doc false.
insert_user(Id, Channels, Nick, Prefix) ->
    Nickname = irc_parser:casefold(Id, Nick),

    F = fun (#channel{users = Us} = R) ->
                U = #{nickname => Nickname, name => Nick, prefix => Prefix},
                R#channel{users = Us#{Nickname => U}}
        end,
    update_many(Id, Channels, F).


-doc false.
change_nick(Id, OldNick, NewNick) ->
    Old = irc_parser:casefold(Id, OldNick),
    New = irc_parser:casefold(Id, NewNick),

    F = fun (#channel{users = Users} = R) ->
                case maps:get(Old, Users, undefined) of
                    undefined -> R;  % TODO This case should never happen
                    U         ->
                        U1 = U#{nickname := New, name := NewNick},
                        Us1 = maps:remove(Old, R#channel.users),
                        R#channel{users = Us1#{New => U1}}
                end
        end,
    update_all(Id, F).


-doc false.
delete_user(Id, Nick) ->
    delete_user(Id, list(Id), Nick).


-doc false.
delete_user(Id, Channels, Nick) ->
    Nickname = irc_parser:casefold(Id, Nick),

    F = fun (#channel{users = Users} = R) ->
                R#channel{users = maps:remove(Nickname, Users)}
        end,
    update_many(Id, Channels, F).


-doc false.
delete_users(Id, Name) ->
    F = fun (R) -> R#channel{users = #{}} end,
    update(Id, Name, F).


-doc """
Get the joined users.

The returned map is keyed by casefolded nicknames.
""".
-spec get_users(Id :: atom(), Name :: iodata()) -> #{Nick :: binary() => user()}.

get_users(Id, Name) ->
    {ok, R} = get(Id, Name),
    R#channel.users.


-doc false.
add_prefix(Id, Channel, Nick, Prefix) ->
    Nickname = irc_parser:casefold(Id, Nick),

    F = fun (#channel{users = Users} = R) ->
                case maps:get(Nickname, Users, undefined) of
                    undefined          -> R;  % TODO: should never happen
                    #{prefix := P} = U ->
                        U1 = U#{prefix := ins_prefix(Id, P, Prefix)},
                        R#channel{users = Users#{Nickname => U1}}
                end
        end,
    update(Id, Channel, F).

ins_prefix(Id, Enabled, New) ->
    case lists:any(fun (X) -> X =:= New end, Enabled) of
        true  -> Enabled;  % This prefix has already been set
        false -> ins_pfx(irc_isupport:get_prefix(Id), Enabled, New, [])
    end.

ins_pfx([{_, N} | _R], [E | ER], N, Acc) -> lists:reverse(Acc) ++ [N, E | ER];
ins_pfx([{_, N} | _R], [],       N, Acc) -> lists:reverse([N | Acc]);
ins_pfx([{_, A} | AR], [A | ER], N, Acc) -> ins_pfx(AR, ER, N, [A | Acc]);
ins_pfx([_      | AR], Enabled,  N, Acc) -> ins_pfx(AR, Enabled, N, Acc);
ins_pfx([]           , Enabled,  _, Acc) -> lists:reverse(Acc) ++ Enabled.


-doc false.
delete_prefix(Id, Channel, Nick, Prefix) ->
    Nickname = irc_parser:casefold(Id, Nick),

    F = fun (#channel{users = Users} = R) ->
                case maps:get(Nickname, Users, undefined) of
                    undefined          -> R;  % TODO: should never happen
                    #{prefix := P} = U ->
                        U1 = U#{prefix := lists:delete(Prefix, P)},
                        R#channel{users = Users#{Nickname => U1}}
                end
        end,
    update(Id, Channel, F).


%%%===================================================================
%%% IRC message handler API
%%%===================================================================

-doc false.
handle_rpl_namreply(Id, Name, Users) ->
    Us = maps:fold(fun (Nick, Pfx, Acc) ->
                           N = irc_parser:casefold(Id, Nick),
                           U = #{nickname => N, name => Nick, prefix => Pfx},
                           Acc#{N => U}
                   end,
                   #{},
                   Users),

    F = fun (#channel{rpl_endofnames = true} = R)                 ->
                R#channel{users = Us, rpl_endofnames = false};
            (#channel{rpl_endofnames = false, users = OldUs} = R) ->
                R#channel{users = maps:merge(OldUs, Us)}
        end,
    update(Id, Name, F).


-doc false.
handle_rpl_endofnames(Id, Name) ->
    F = fun (R) -> R#channel{rpl_endofnames = true} end,
    update(Id, Name, F).
