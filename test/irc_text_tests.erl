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

-module(irc_text_tests).

-include_lib("eunit/include/eunit.hrl").


%%% truncate/2, truncate/3

truncate_bounds_test_() ->
    L = fun (X) -> byte_size(unicode:characters_to_binary(X)) end,

    [%% Without Ellipsis
     ?_assert(11 >= L(irc_text:truncate("Hello, World!", 11))),
     ?_assert(26 >= L(irc_text:truncate("Γειά σου, Κόσμε!", 26))),
     %% With Ellipsis
     ?_assert(11 >= L(irc_text:truncate("Hello, World!", 11, <<"...">>))),
     ?_assert(26 >= L(irc_text:truncate("Γειά σου, Κόσμε!", 26, <<"...">>)))].
