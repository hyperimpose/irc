%%--------------------------------------------------------------------
%% Copyright 2023 hyperimpose.org
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

-module(irc_text).

-export([divide/2,
         truncate/2, truncate/3,
         fractional_truncate/2, fractional_truncate/3]).


-doc "Divide `Text` into chunks that each fit within `Limit` bytes".
-spec divide(Text, Limit) -> Result when
      Text :: unicode:chardata(),
      Limit :: integer(),
      Result :: list(unicode:chardata()).

divide(Text, Limit) -> divide(string:to_graphemes(Text), Limit, 0, [], []).

divide([H | R], Limit, Size, TextAcc, OutAcc) ->
    G = unicode:characters_to_binary([H]),
    GS = byte_size(G),
    S = Size + GS,
    if
        Limit >= S ->
            divide(R, Limit, S, [G | TextAcc], OutAcc);
        true       ->
            T = lists:reverse(TextAcc),
            divide(R, Limit, GS, [G], [T | OutAcc])
    end;
divide([], _Limit, _Size, TextAcc, OutAcc) ->
    lists:reverse([lists:reverse(TextAcc) | OutAcc]).


-doc #{equiv => truncate(Texts, Limit, <<>>)}.
-spec truncate(Text, Limit) -> Result when
      Text     :: unicode:chardata(),
      Limit    :: integer(),
      Result   :: unicode:chardata().

truncate(Text, Limit) -> truncate(Text, Limit, <<>>).


-doc """
Truncate `Text` to fit within `Limit` bytes.
Append `Ellipsis` if truncation occurs.
""".
-spec truncate(Text, Limit, Ellipsis) -> Result when
      Text     :: unicode:chardata(),
      Limit    :: integer(),
      Ellipsis :: unicode:chardata(),
      Result   :: unicode:chardata().

truncate(Text, Limit, Ellipsis) ->
    E = unicode:characters_to_binary([Ellipsis]),
    truncate(Text, Limit - byte_size(E), E, 0, []).

truncate(Text, Limit, Ellipsis, Size, Acc) ->
    case string:next_grapheme(Text) of
        [H | R] ->
            G = unicode:characters_to_binary([H]),
            S = Size + byte_size(G),
            if
                Limit >= S ->
                    truncate(R, Limit, Ellipsis, S, [G | Acc]);
                true ->
                    lists:reverse([[Ellipsis] | Acc])
            end;
        [] ->
            lists:reverse(Acc);
        {error, _} ->
            lists:reverse([[Ellipsis] | Acc])
    end.


-doc #{equiv => fractional_truncate(Texts, Limit, <<>>)}.
-spec fractional_truncate(Texts, Limit) -> Result when
      Texts  :: [{number(), unicode:chardata()}],
      Limit  :: integer(),
      Result :: unicode:chardata().

fractional_truncate(Texts, Limit) -> fractional_truncate(Texts, Limit, <<>>).


-doc """
Fractionally truncate a list of strings.

The caller assigns to each string a fraction. This fraction describes how
much of the final output each string will take.

Assigning 1 instead of a fraction means that the string should not be
truncated. In that case the whole string will be used and its byte size will
be subtracted from the Limit available to fractional texts.

Each string from the list is truncated so that it doesn't use more than
(Fraction * Limit) bytes. All the fractions should add up to 1.

## Parameters

- `Texts` - A list of tuples in the following format: `[{0.0..1.0, iodata()}]`

  _Example:_
  ```erlang
  [{1,   "Text 1"},
   {2/3, "Text 2"},
   {1/3, "Text 3"}]
  ```

- `Limit` - The maximum number of bytes allowed in the result.
- `Ellipsis` - A string to append to any truncated text.
""".
-spec fractional_truncate(Texts, Limit, Ellipsis) -> Result when
      Texts    :: [{number(), unicode:chardata()}],
      Limit    :: integer(),
      Ellipsis :: unicode:chardata(),
      Result   :: unicode:chardata().

fractional_truncate(Texts, Limit, Ellipsis) ->
    Ones = unicode:characters_to_binary([T || {F, T} <- Texts, F =:= 1]),
    Avail = Limit - byte_size(Ones),

    F = fun ({1, T}, Acc) ->
                [T | Acc];
            ({F, T}, Acc) ->
                Max = case floor(Avail * F) of
                          Size when Size =< Avail -> Size;
                          _Else                   -> Avail
                      end,
                [truncate(T, Max, Ellipsis) | Acc]
        end,
    lists:reverse(lists:foldl(F, [], Texts)).
