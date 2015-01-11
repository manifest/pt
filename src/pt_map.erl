%% ------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014-2015 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ------------------------------------------------------------------

-module(pt_map).
-behaviour(pt_kvstore).

%% API
-export([
	keys/1,
	values/1,
	get/2,
	get/3,
	get_in/2,
	get_in/3,
	find/2,
	find_in/2,
	with/2,
	put/3,
	merge/2,
	remove/2,
	is_empty/1
]).

%% ==================================================================
%% API
%% ==================================================================

-spec keys(map()) -> list().
keys(M) ->
	maps:keys(M).

-spec values(map()) -> list().
values(M) ->
	maps:values(M).

-spec get(any(), map()) -> any().
get(Key, M) ->
	maps:get(Key, M).

-spec get(any(), map(), any()) -> any().
get(Key, M, Default) ->
	maps:get(Key, M, Default).

-spec get_in(list(), map()) -> any().
get_in(Keys, M) ->
	case find_in(Keys, M) of
		{ok, Val} -> Val;
		error     -> error(bad_key)
	end.

-spec get_in(list(), map(), any()) -> any().
get_in(Keys, M, Default) ->
	case find_in(Keys, M) of
		{ok, Val} -> Val;
		error     -> Default
	end.

-spec find(any(), map()) -> {ok, any()} | error.
find(Key, M) ->
	maps:find(Key, M).

-spec find_in(list(), map()) -> {ok, any()} | error.
find_in([H|T], M) when is_map(M) ->
	case find(H, M) of
		{ok, Val} -> find_in(T, Val);
		error     -> error
	end;
find_in([_], _)  -> error;
find_in([], Val) -> {ok, Val}.

-spec with(list(), map()) -> #{}.
with(Keys, M) ->
	maps:with(Keys, M).

-spec put(any(), any(), map()) -> map().
put(Key, Val, M) ->
	maps:put(Key, Val, M).

-spec merge(map(), map()) -> map().
merge(M1, M2) ->
	maps:merge(M1, M2).

-spec remove(any(), map()) -> map().
remove(Key, Map) ->
	maps:remove(Key, Map).

-spec is_empty(map()) -> boolean().
is_empty(M) ->
	maps:size(M) =:= 0.

%% ==================================================================
%% Tests 
%% ==================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

keys_test_() ->
	Test =
		[	{"map empty", #{},               []},
			{"map 2-val", #{a => 1, b => 2}, [a, b]} ],

	[{Desc, ?_assertEqual(Output, keys(M))} || {Desc, M, Output} <- Test].

values_test_() ->
	Test =
		[	{"map empty", #{},                []},
			{"map 2-val", #{a => 1, b => 2}, [1, 2]} ],

	[{Desc, ?_assertEqual(Output, values(M))} || {Desc, M, Output} <- Test].

get_test_() ->
	[	{"map empty",
			?_assertError(bad_key, ?MODULE:get(a, #{}))},
		{"map empty w/ default",
			?_assertEqual(default, ?MODULE:get(a, #{}, default))},
		{"key not exist",
			?_assertError(bad_key, ?MODULE:get(a, #{b => 2, c => 3}))},
		{"key not exist w/ default",
			?_assertEqual(default, ?MODULE:get(a, #{b => 2, c => 3}, default))},
		{"key exists",
			?_assertEqual(1, ?MODULE:get(a, #{a => 1, b => 2}))},
		{"key exist w/ default",
			?_assertEqual(1, ?MODULE:get(a, #{a => 1, b => 2}, default))} ].

get_in_test_() ->
	[	{"map empty",
			?_assertError(bad_key, get_in([a], #{}))},
		{"map empty w/ default",
			?_assertEqual(default, get_in([a], #{}, default))},
		{"map empty 2-key",
			?_assertError(bad_key, get_in([a, b], #{}))},
		{"map empty 2-key w/ default",
			?_assertEqual(default, get_in([a, b], #{}, default))},
		{"key not exist",
			?_assertError(bad_key, get_in([a], #{b => 2, c => 3}))},
		{"key not exist w/ default",
			?_assertEqual(default, get_in([a], #{b => 2, c => 3}, default))},
		{"key path not exist",
			?_assertError(bad_key, get_in([a,b], #{b => 2, c => 3}))},
		{"key path not exist w/default",
			?_assertEqual(default, get_in([a,b], #{b => 2, c => 3}, default))},
		{"key exists",
			?_assertEqual(1, get_in([a], #{a => 1, b => #{ba => 21, bb => 22}}))},
		{"key exist w/ default",
			?_assertEqual(1, get_in([a], #{a => 1, b => #{ba => 21, bb => 22}}, default))},
		{"key path exist",
			?_assertEqual(21, get_in([b, ba], #{a => 1, b => #{ba => 21, bb => 22}}))},
		{"key exist w/ default",
			?_assertEqual(21, get_in([b, ba], #{a => 1, b => #{ba => 21, bb => 22}}, default))},
		{"keys list empty",
			?_assertEqual(#{a => 1, b => 2}, get_in([],  #{a => 1, b => 2}))},
		{"keys list empty w/ default",
			?_assertEqual(#{a => 1, b => 2}, get_in([],  #{a => 1, b => 2}, default))} ].

find_test_() ->
	Test =
		[	{"map empty",     a, #{},               error},
			{"key not exist", a, #{b => 2, c => 3}, error},
			{"key exists",    a, #{a => 1, b => 2}, {ok, 1}} ],

	[{Desc, ?_assertEqual(Output, find(Key, M))} || {Desc, Key, M, Output} <- Test].

find_in_test_() ->
	Test =
		[	{"map empty",             [a],     #{},                                   error},
			{"map empty 2-key",       [a, b],  #{},                                   error},
			{"key not exist",         [a],     #{b => 2, c => 3},                     error},
			{"key path not exist",    [a, b],  #{b => 2, c => 3},                     error},
			{"key exists",            [a],     #{a => 1, b => #{ba => 21, bb => 22}}, {ok, 1}},
			{"key path exist",        [b, ba], #{a => 1, b => #{ba => 21, bb => 22}}, {ok, 21}},
			{"keys list empty",       [],      #{a => 1, b => 2},                     {ok, #{a => 1, b => 2}}} ],

	[{Desc, ?_assertEqual(Output, find_in(Keys, M))} || {Desc, Keys, M, Output} <- Test].

with_test_() ->
	Test =
		[	{"map empty",       [a, b], #{},               #{}},
			{"key not exist",   [a, b], #{b => 2, c => 3}, #{b => 2}},
			{"keys not exist",  [a, b], #{c => 3, d => 4}, #{}},
			{"keys exist",      [a, b], #{a => 1, b => 2}, #{a => 1, b => 2}},
			{"keys list empty", [],     #{a => 1, b => 2}, #{}} ],

	[{Desc, ?_assertEqual(Output, with(Keys, M))} || {Desc, Keys, M, Output} <- Test].

put_test_() ->
	Test =
		[	{"map empty",     a, 1, #{},               #{a => 1}},
			{"key not exist", a, 1, #{b => 2, c => 3}, #{a => 1, b => 2, c => 3}},
			{"key exists",    a, 1, #{a => 0, b => 2}, #{a => 1, b => 2}} ],

	[{Desc, ?_assertEqual(Output, ?MODULE:put(Key, Val, M))} || {Desc, Key, Val, M, Output} <- Test].

merge_test_() ->
	Test =
		[	{"first empty",  #{},               #{a => 1, b => 2}, #{a => 1, b => 2}},
			{"second empty", #{a => 1, b => 2}, #{},               #{a => 1, b => 2}},
			{"both empty",   #{},               #{},               #{}},
			{"same key",     #{a => 1, b => 2}, #{b => 0, c => 3}, #{a => 1, b => 0, c => 3}} ],

	[{Desc, ?_assertEqual(Output, merge(M1, M2))} || {Desc, M1, M2, Output} <- Test].

remove_test_() ->
	Test =
		[	{"map empty",     a, #{},               #{}},
			{"key not exist", a, #{b => 2, c => 3}, #{b => 2, c => 3}},
			{"key exists",    a, #{a => 1, b => 2}, #{b => 2}} ],

	[{Desc, ?_assertEqual(Output, remove(Key, M))} || {Desc, Key, M, Output} <- Test].

is_empty_test_() ->
	Test =
		[	{"map empty",     #{},       true},
			{"map not empty", #{a => 1}, false} ],

	[{Desc, ?_assertEqual(Output, is_empty(M))} || {Desc, M, Output} <- Test].

-endif.

