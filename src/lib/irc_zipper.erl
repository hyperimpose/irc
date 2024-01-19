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
%%% An implementation of the list data structure with a zipper.
%%%-------------------------------------------------------------------

-module(irc_zipper).


-export([current/1, find/2, from_list/1, is_empty/1, list/0, map/2,
         next/1, next_c/1, next_cr/1,
         pop/1,
         prev/1, prev_c/1, prev_cr/1,
         set/2, to_list/1,
         update/3, append/2]).


-type zipper_list() :: {[], []}.
-type zipper_list(Item) :: {[Item], [Item]}.

-export_type([zipper_list/0, zipper_list/1]).


current({_, [H | _]}) -> H;
current(_)            -> error.


find(Pred, {Left, Right}) ->
    case find1(Right, Pred) of
        error -> find1(Left, Pred);
        Else  -> Else
    end.

find1([H | R], Pred) ->
    case Pred(H) of
        true  -> H;
        _     -> find1(R, Pred)
    end;
find1([], _Pred) ->
    error.


from_list(List) -> {[], List}.


is_empty({[], []}) -> true;
is_empty(_)        -> false.


map(Fun, {Left, Right}) -> {[Fun(X) || X <- Left], [Fun(X) || X <- Right]}.


list() -> {[], []}.


next({_,    [_]} = L) -> L;
next({Left, [H | R]}) -> {[H | Left], R}.


next_c({Left, [H]}) -> {[], lists:reverse([H | Left])};
next_c(Else) -> next(Else).


next_cr({Left, [H]}) -> {[], [H | Left]};
next_cr(Else) -> next(Else).


pop({[],      []} = L)  -> L;
pop({[H | R], [_]})     -> {R, [H]};
pop({Left,    [_ | R]}) -> {Left, R}.


prev({[H | R], Right}) -> {R, [H | Right]};
prev(Else)             -> Else.


prev_c({[], Right}) -> {lists:reverse(Right), []};
prev_c(Else)        -> prev(Else).


prev_cr({[], Right}) -> {Right, []};
prev_cr(Else)        -> prev(Else).


set({Left, [_ | R]}, Item) -> {Left, [Item | R]};
set({[], []},        Item) -> {[], [Item]}.


to_list({Left, Right}) -> Right ++ lists:reverse(Left).


update({Left, Right}, Pred, New) ->
    case update(Right, Pred, New, []) of
        {ok, R1} -> {Left, R1};
        false    -> case update(Left, Pred, New, []) of
                        {ok, L1} -> {L1, Right};
                        false    -> false
                    end
    end.

update([H | R], Pred, New, Acc) ->
    case Pred(H) of
        true  -> {ok, lists:reverse(Acc) ++ [New | R]};
        false -> update(R, Pred, New, [H | Acc])
    end;
update([], _Pred, _New, _Acc) ->
    false.


append({Left, Right}, Item) -> {Left, Right ++ [Item]}.
