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
%%% Module for storing and reading the 005 ISUPPORT numeric options.
%%%
%%% Each option is saved in the client instance ETS table. This module
%%% provides a CRUD interface for each option.
%%%
%%% The reader functions (prefixed with get_, is_) can be used by external,
%%% to this OTP application, code.
%%% Every other function is for internal use. The client instance will
%%% automatically update the state.
%%%-------------------------------------------------------------------

-module(irc_isupport).


-export([get_awaylen/1, set_awaylen/2, unset_awaylen/1,
         get_callerid/1, set_callerid/2, unset_callerid/1,
         get_casemapping/1, set_casemapping/2, unset_casemapping/1,
         get_chanlimit/2, set_chanlimit/2, unset_chanlimit/1,
         get_chanmodes/1, get_chanmodes/2, set_chanmodes/2, unset_chanmodes/1,
         get_channellen/1, set_channellen/2, unset_channellen/1,
         get_chantypes/1, is_channel/2, set_chantypes/2, unset_chantypes/1,
         get_elist/1, has_elist/2, set_elist/2, unset_elist/1,
         get_excepts/1, set_excepts/2, unset_excepts/1,
         get_extban/1, set_extban/2, unset_extban/1,
         get_hostlen/1, set_hostlen/2, unset_hostlen/1,
         get_invex/1, set_invex/2, unset_invex/1,
         get_kicklen/1, set_kicklen/2, unset_kicklen/1,
         get_maxlist/1, set_maxlist/2, unset_maxlist/1,
         get_maxtargets/1, set_maxtargets/2, unset_maxtargets/1,
         get_modes/1, set_modes/2, unset_modes/1,
         get_network/1, set_network/2, unset_network/1,
         get_nicklen/1, set_nicklen/2, unset_nicklen/1,
         get_prefix/1, set_prefix/2, unset_prefix/1,
         get_safelist/1, set_safelist/1, unset_safelist/1,
         get_statusmsg/1, set_statusmsg/2, unset_statusmsg/1,
         get_targmax/1, set_targmax/2, unset_targmax/1,
         get_topiclen/1, set_topiclen/2, unset_topiclen/1,
         get_userlen/1, set_userlen/2, unset_userlen/1,
         get_whox/1, set_whox/1, unset_whox/1]).


-define(KEY(Key), {isupport, Key}).
-define(GET(Id, Key), ets:lookup(Id, ?KEY(Key))).
-define(GET(Id, Key, Default), case ets:lookup(Id, ?KEY(Key)) of
                                   [{?KEY(Key), V}] -> V;
                                   []               -> Default
                               end).
-define(SET(Id, Key, Value), ets:insert(Id, {?KEY(Key), Value})).


%%%===================================================================
%%% API
%%%===================================================================

%%% The set_* functions  of this API are designed such  that they take
%%% the  arguements as  sent  by  the server.  Any  processing of  the
%%% argument is done internally.


%%% awaylen ==========================================================

-spec get_awaylen(Id :: atom()) -> undefined | integer().

get_awaylen(Id) -> ?GET(Id, awaylen, undefined).


set_awaylen(Id, <<>>) -> ?SET(Id, awaylen, undefined);
set_awaylen(Id, L)    -> ?SET(Id, awaylen, binary_to_integer(L)).


unset_awaylen(Id) -> ?SET(Id, awaylen, undefined).


%%% callerid =========================================================

-spec get_callerid(Id :: atom()) -> undefined | char().

get_callerid(Id) -> ?GET(Id, callerid, undefined).


set_callerid(Id, <<>>)                 -> ?SET(Id, callerid, $g);
set_callerid(Id, <<Letter, _/binary>>) -> ?SET(Id, callerid, Letter).


unset_callerid(Id) -> ?SET(Id, callerid, undefined).


%%% casemapping ======================================================

%%% The default value was chosen based on:
%%% https://www.ietf.org/archive/id/draft-brocklesby-irc-isupport-03.txt

-spec get_casemapping(Id :: atom()) -> binary().

get_casemapping(Id) -> ?GET(Id, casemapping, <<"rfc1459">>).


set_casemapping(Id, Value) -> ?SET(Id, casemapping, Value).


unset_casemapping(Id) -> ?SET(Id, casemapping, <<"rfc1459">>).


%%% chanlimit ========================================================

-spec get_chanlimit(Id :: atom(), Type :: binary()) ->
          undefined | nolimit | integer().

get_chanlimit(Id, Type) ->
    case ?GET(Id, chanlimit, undefined) of
        #{Type := Limit} -> Limit;
        _Else            -> undefined
    end.


set_chanlimit(Id, Arg) -> ?SET(Id, chanlimit, set_chanlimit1(Arg)).

set_chanlimit1(Args) ->
    F = fun (X, Acc) -> case string:split(X, ":") of
                       [Type, Limit] -> Acc#{Type => binary_to_integer(Limit)};
                       [Type]        -> Acc#{Type => nolimit}
                   end
        end,
    lists:foldl(F, #{}, string:split(Args, <<",">>, all)).



unset_chanlimit(Id) -> ?SET(Id, chanlimit, #{}).


%%% chanmodes ========================================================

-spec get_chanmodes(Id :: atom()) -> [binary()].

get_chanmodes(Id) -> ?GET(Id, chanmodes, []).


-spec get_chanmodes(Id :: atom(), a | b | c | d) -> binary().

get_chanmodes(Id, a) ->
    case get_chanmodes(Id) of
        [A | _] -> A;
        _Else   -> <<>>
    end;
get_chanmodes(Id, b) ->
    case get_chanmodes(Id) of
        [_A, B | _] -> B;
        _Else       -> <<>>
    end;
get_chanmodes(Id, c) ->
    case get_chanmodes(Id) of
        [_A, _B, C | _] -> C;
        _Else           -> <<>>
    end;
get_chanmodes(Id, d) ->
    case get_chanmodes(Id) of
        [_A, _B, _C, D | _] -> D;
        _Else               -> <<>>
    end.


set_chanmodes(Id, Args) ->
    Modes = string:split(Args, <<",">>, all),
    ?SET(Id, chanmodes, Modes).


unset_chanmodes(Id) -> ?SET(Id, chanmodes, []).


%%% channellen =======================================================

%%% The default value (200) is defined in RFC 1459

-spec get_channellen (Id :: atom()) -> integer().

get_channellen(Id) -> ?GET(Id, channellen, 200).


set_channellen(Id, L) -> ?SET(Id, channellen, binary_to_integer(L)).


unset_channellen(Id) -> ?SET(Id, channellen, 200).


%%% chantypes ========================================================

-spec get_chantypes(Id :: atom()) -> binary().

get_chantypes(Id) -> ?GET(Id, chantypes, <<>>).


-spec is_channel(Id :: atom(), Channel :: iolist()) -> boolean().

is_channel(Id, Channel) ->
    case iolist_to_binary(Channel) of
        <<T, _/binary>> -> case string:find(get_chantypes(Id), [T]) of
                               nomatch -> false;
                               _Else   -> true
                           end;
        _Else           -> false
    end.


set_chantypes(Id, Types) -> ?SET(Id, chantypes, Types).


unset_chantypes(Id) -> ?SET(Id, chantypes, <<>>).


%%% elist ============================================================

-spec get_elist(Id :: atom()) -> undefined | binary().

get_elist(Id) -> ?GET(Id, elist, undefined).


-spec has_elist(Id :: atom(), Mode :: c | m | n | t | u | string()) ->
          boolean().

has_elist(Id, c)    -> has_elist(Id, <<"C">>);
has_elist(Id, m)    -> has_elist(Id, <<"M">>);
has_elist(Id, n)    -> has_elist(Id, <<"N">>);
has_elist(Id, t)    -> has_elist(Id, <<"T">>);
has_elist(Id, u)    -> has_elist(Id, <<"U">>);
has_elist(Id, Mode) -> case get_elist(Id) of
                           undefined -> false;
                           Elist     -> has_elist1(Elist, Mode)
                       end.

has_elist1(Elist, Mode) ->
    case string:find(string:casefold(Elist), string:casefold(Mode)) of
        nomatch -> false;
        _Else   -> true
    end.


set_elist(Id, <<>>) -> unset_elist(Id);
set_elist(Id, Args) -> ?SET(Id, elist, Args).

unset_elist(Id) -> ?SET(Id, elist, undefined).


%%% excepts ==========================================================

-spec get_excepts(Id :: atom()) -> undefined | char().

get_excepts(Id) -> ?GET(Id, excepts, undefined).


set_excepts(Id, <<Letter, _/binary>>) -> ?SET(Id, excepts, Letter);
set_excepts(Id, <<>>)                 -> ?SET(Id, excepts, $e).


unset_excepts(Id) -> ?SET(Id, excepts, undefined).


%%% extban ===========================================================

-spec get_extban(Id :: atom()) -> undefined | {char(), binary()}.

get_extban(Id) -> ?GET(Id, extban, undefined).


set_extban(Id, <<>>) ->  % Invalid args
    unset_extban(Id);
set_extban(Id, Args) ->
    [Prefix, Types] = string:split(Args, <<",">>),
    ?SET(Id, extban, {Prefix, Types}).


unset_extban(Id) -> ?SET(Id, extban, undefined).


%%% hostlen ==========================================================

%%% The default value (63) is defined in RFC 2812

-spec get_hostlen(Id :: atom()) -> integer().

get_hostlen(Id) -> ?GET(Id, hostlen, 63).


set_hostlen(Id, L) -> ?SET(Id, hostlen, binary_to_integer(L)).


unset_hostlen(Id) -> ?SET(Id, hostlen, 63).


%%% invex ============================================================

-spec get_invex(Id :: atom()) -> undefined | char().

get_invex(Id) -> ?GET(Id, invex, undefined).


set_invex(Id, <<Letter, _/binary>>) -> ?SET(Id, invex, Letter);
set_invex(Id, <<>>)                 -> ?SET(Id, invex, $I).


unset_invex(Id) -> ?SET(Id, invex, undefined).


%%% kicklen ==========================================================

-spec get_kicklen(Id :: atom()) -> undefined | integer().

get_kicklen(Id) -> ?GET(Id, kicklen, undefined).


set_kicklen(Id, <<>>) -> ?SET(Id, kicklen, undefined);
set_kicklen(Id, L)    -> ?SET(Id, kicklen, binary_to_integer(L)).


unset_kicklen(Id) -> ?SET(Id, kicklen, undefined).


%%% maxlist ==========================================================

-spec get_maxlist(Id :: atom()) -> [{binary(), integer()}].

get_maxlist(Id) -> ?GET(Id, maxlist, []).


set_maxlist(Id, Args)    ->
    F = fun (X, Acc) -> [Modes, Limit] = string:split(X, <<":">>),
                        [{Modes, binary_to_integer(Limit)} | Acc]
        end,
    L = lists:foldl(F, [], string:split(Args, <<",">>, all)),
    ?SET(Id, maxlist, L).


unset_maxlist(Id) -> ?SET(Id, maxlist, []).


%%% maxtargets =======================================================

-spec get_maxtargets(Id :: atom()) -> undefined | nolimit | integer().

get_maxtargets(Id) -> ?GET(Id, maxtargets, undefined).


set_maxtargets(Id, <<>>) -> ?SET(Id, maxtargets, nolimit);
set_maxtargets(Id, L)    -> ?SET(Id, maxtargets, binary_to_integer(L)).


unset_maxtargets(Id) -> ?SET(Id, maxtargets, undefined).


%%% modes ============================================================

-spec get_modes(Id :: atom()) -> undefined | nolimit | integer().

get_modes(Id) -> ?GET(Id, modes, undefined).


set_modes(Id, <<>>) -> ?SET(Id, modes, nolimit);
set_modes(Id, L)    -> ?SET(Id, modes, binary_to_integer(L)).


unset_modes(Id) -> ?SET(Id, modes, undefined).


%%% network ==========================================================

-spec get_network(Id :: atom()) -> undefined | binary().

get_network(Id) -> ?GET(Id, network, undefined).


set_network(Id, Network) -> ?SET(Id, network, Network).


unset_network(Id) -> ?SET(Id, network, undefined).


%%% nicklen ==========================================================

%%% The default value (9) is defined in RFC 1459

-spec get_nicklen(Id :: atom()) -> integer().

get_nicklen(Id) -> ?GET(Id, nicklen, 9).


set_nicklen(Id, L) -> ?SET(Id, nicklen, binary_to_integer(L)).


unset_nicklen(Id) -> ?SET(Id, nicklen, 9).


%%% prefix ===========================================================

%%% The default value (ov)@+ is based on RFC 1459

-spec get_prefix(Id :: atom()) -> [{char(), char()}].

get_prefix(Id) -> ?GET(Id, prefix, [{$o, $@}, {$v, $+}]).  % TODO: undefined is prolly empty in modern IRC


set_prefix(Id, Args) -> ?SET(Id, prefix, set_prefix1(Args, [])).

set_prefix1(<<$(, R/binary>>, Acc) -> set_prefix1(R, Acc);
set_prefix1(<<$), R/binary>>, Acc) -> set_prefix2(R, lists:reverse(Acc), []);
set_prefix1(<<C,  R/binary>>, Acc) -> set_prefix1(R, [C | Acc]).

set_prefix2(<<C, R/binary>>, [M | T], Acc) -> set_prefix2(R, T, [{M, C} | Acc]);
set_prefix2(<<>>, [], Acc) -> lists:reverse(Acc).


unset_prefix(Id) -> ?SET(Id, prefix, [{$o, $@}, {$v, $+}]).


%%% safelist =========================================================

-spec get_safelist(Id :: atom()) -> boolean().

get_safelist(Id) -> ?GET(Id, safelist, false).


set_safelist(Id) -> ?SET(Id, safelist, true).


unset_safelist(Id) -> ?SET(Id, safelist, false).


%%% statusmsg ========================================================

-spec get_statusmsg(Id :: atom()) -> undefined | binary().

get_statusmsg(Id) -> ?GET(Id, statusmsg, undefined).


set_statusmsg(Id, Args) -> ?SET(Id, statusmsg, Args).


unset_statusmsg(Id) -> ?SET(Id, statusmsg, undefined).


%%% targmax ==========================================================

-spec get_targmax(Id :: atom()) -> [{binary(), integer()}].

get_targmax(Id) -> ?GET(Id, targmax, []).


set_targmax(Id, Args) ->
    F = fun (X, Acc) -> case string:split(X, <<":">>) of
                            [Cmd, <<>>] -> [{Cmd, nolimit}];
                            [Cmd, Lim]  -> [{Cmd, binary_to_integer(Lim)} | Acc]
                        end
        end,
    L = lists:foldl(F, [], string:split(Args, <<",">>, all)),
    ?SET(Id, targmax, L).


unset_targmax(Id) -> ?SET(Id, targmax, []).


%%% topiclen =========================================================

-spec get_topiclen(Id :: atom()) -> undefined | integer().

get_topiclen(Id) -> ?GET(Id, topiclen, undefined).


set_topiclen(Id, <<>>) -> unset_topiclen(Id);
set_topiclen(Id, L)    -> ?SET(Id, topiclen, binary_to_integer(L)).


unset_topiclen(Id) -> ?SET(Id, topiclen, undefined).


%%% userlen ==========================================================

-spec get_userlen(Id :: atom()) -> undefined | integer().

get_userlen(Id) -> ?GET(Id, userlen, undefined).


set_userlen(Id, <<>>) -> unset_userlen(Id);
set_userlen(Id, L)    -> ?SET(Id, userlen, binary_to_integer(L)).


unset_userlen(Id) ->  ?SET(Id, userlen, undefined).


%%% whox =============================================================

-spec get_whox(Id :: atom()) -> boolean().

get_whox(Id) -> ?GET(Id, whox, false).


set_whox(Id) -> ?SET(Id, whox, true).


unset_whox(Id) -> ?SET(Id, whox, false).
