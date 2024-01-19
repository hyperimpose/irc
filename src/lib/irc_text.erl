-module(irc_text).


-export([divide/2, truncate/2, truncate/3]).


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


truncate(Text, Limit) -> truncate(Text, Limit, <<>>).

truncate(Text, Limit, Ellipsis) ->
    E = unicode:characters_to_binary([Ellipsis]),
    truncate(Text, Limit + byte_size(E), E, 0, []).

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
