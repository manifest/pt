%% ------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014 Andrei Nesterov <ae.nesterov@gmail.com>
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

-module(pt_plist).
-behaviour(pt_kv_store).

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
	select_keys/2,
	put/3,
	merge/2,
	remove/2,
	is_empty/1
]).

%% Types
-type plist() :: [{any(), any()}].

-export_type([plist/0]).

%% ==================================================================
%% API
%% ==================================================================

-spec keys(plist()) -> [any()].
keys(L) ->
	lists:map(fun({Key, _}) -> Key end, L).

-spec values(plist()) -> [any()].
values(L) ->
	lists:map(fun({_, Val}) -> Val end, L).

-spec get(any(), plist()) -> any().
get(Key, L) ->
	case find(Key, L) of
		undefined -> erlang:error(bad_key);
		Val       -> Val
	end.

-spec get(any(), plist(), any()) -> any().
get(Key, L, Default) ->
	case lists:keyfind(Key, 1, L) of
		{Key, Val} -> Val;
		false      -> Default
	end.

-spec get_in([any()], plist()) -> any().
get_in(Keys, L) ->
	case find_in(Keys, L) of
		undefined -> erlang:error(bad_key);
		Val       -> Val 
	end.

-spec get_in([any()], plist(), any()) -> any().
get_in([], L, _) ->
	L;
get_in([Key|Keys], L, Default) ->
	Child = ?MODULE:get(Key, L, Default),
	case {Child =/= Default, Keys} of
		{_,    []} -> Child;
		{false, _} -> Child;
		{true,  _} -> get_in(Keys, Child, Default)
	end.

-spec find(any(), plist()) -> undefined | any().
find(Key, L) ->
	?MODULE:get(Key, L, undefined).

-spec find_in([any()], plist()) -> any().
find_in(Keys, L) ->
	get_in(Keys, L, undefined).

-spec select_keys([any()], plist()) -> plist().
select_keys(Keys, L) ->
	lists:foldl(
		fun({Key, Val}, Acc) ->
			case lists:member(Key, Keys) of
				true  -> [{Key, Val}|Acc];
				false -> Acc
			end
		end, [], L).

-spec put(any(), any(), plist()) -> plist().
put(Key, Val, L) ->
	L2 = remove(Key, L),
	[{Key, Val}|L2].

-spec merge(plist(), plist()) -> plist().
merge(L1, []) ->
	L1;
merge([], L2) ->
	L2;
merge(L1, L2) ->
	L2Keys = keys(L2),
	L11 = lists:filter(fun({Key, _}) -> not lists:member(Key, L2Keys) end, L1),
	lists:merge(L11, L2).

-spec remove(any(), plist()) -> plist().
remove(Key, L) ->
	lists:keydelete(Key, 1, L).

-spec is_empty(plist()) -> boolean().
is_empty(L) ->
	L =:= [].

%% ==================================================================
%% Tests 
%% ==================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

keys_test_() ->
	Test =
		[	{"list empty", [],               []},
			{"list 2-val", [{a, 1}, {b, 2}], [a, b]} ],

	[{Desc, ?_assertEqual(Output, keys(L))} || {Desc, L, Output} <- Test].

values_test_() ->
	Test =
		[	{"list empty", [],               []},
			{"list 2-val", [{a, 1}, {b, 2}], [1, 2]} ],

	[{Desc, ?_assertEqual(Output, values(L))} || {Desc, L, Output} <- Test].

get_test_() ->
	[	{"list empty",
			?_assertError(bad_key, ?MODULE:get(a, []))},
		{"list empty w/ default",
			?_assertEqual(default, ?MODULE:get(a, [], default))},
		{"key not exist",
			?_assertError(bad_key, ?MODULE:get(a, [{b, 2}, {c, 3}]))},
		{"key not exist w/ default",
			?_assertEqual(default, ?MODULE:get(a, [{b, 2}, {c, 3}], default))},
		{"key exists",
			?_assertEqual(1, ?MODULE:get(a, [{a, 1}, {b, 2}]))},
		{"key exist w/ default",
			?_assertEqual(1, ?MODULE:get(a, [{a, 1}, {b, 2}], default))} ].

find_test_() ->
	Test =
		[	{"list empty",    a, []},
			{"key not exist", a, [{b, 2}, {c, 3}]},
			{"key exists",    a, [{a, 1}, {b, 2}]} ],

	[{Desc, ?_assertEqual(?MODULE:get(Key, L, undefined), find(Key, L))} || {Desc, Key, L} <- Test].

get_in_test_() ->
	[	{"list empty",
			?_assertError(bad_key, get_in([a], []))},
		{"list empty w/ default",
			?_assertEqual(default, get_in([a], [], default))},
		{"list empty 2-key",
			?_assertError(bad_key, get_in([a, b], []))},
		{"list empty 2-key w/ default",
			?_assertEqual(default, get_in([a, b], [], default))},
		{"key not exist",
			?_assertError(bad_key, get_in([a], [{b, 2}, {c, 3}]))},
		{"key not exist w/ default",
			?_assertEqual(default, get_in([a], [{b, 2}, {c, 3}], default))},
		{"key path not exist",
			?_assertError(bad_key, get_in([a,b], [{b, 2}, {c, 3}]))},
		{"key path not exist w/default",
			?_assertEqual(default, get_in([a,b], [{b, 2}, {c, 3}], default))},
		{"key exists",
			?_assertEqual(1, get_in([a], [{a, 1}, {b, [{ba, 21}, {bb, 22}]}]))},
		{"key exist w/ default",
			?_assertEqual(1, get_in([a], [{a, 1}, {b, [{ba, 21}, {bb, 22}]}], default))},
		{"key path exist",
			?_assertEqual(21, get_in([b, ba], [{a, 1}, {b, [{ba, 21}, {bb, 22}]}]))},
		{"key exist w/ default",
			?_assertEqual(21, get_in([b, ba], [{a, 1}, {b, [{ba, 21}, {bb, 22}]}], default))},
		{"keys list empty",
			?_assertEqual([{a, 1}, {b, 2}], get_in([],  [{a, 1}, {b, 2}]))},
		{"keys list empty w/ default",
			?_assertEqual([{a, 1}, {b, 2}], get_in([],  [{a, 1}, {b, 2}], default))} ].

find_in_test_() ->
	Test =
		[	{"list empty",            [a],     [],                                   undefined},
			{"list empty 2-key",      [a, b],  [],                                   undefined},
			{"key not exist",         [a],     [{b, 2}, {c, 3}],                     undefined},
			{"key path not exist",    [a, b],  [{b, 2}, {c, 3}],                     undefined},
			{"key exists",            [a],     [{a, 1}, {b, [{ba, 21}, {bb, 22}]}],  1},
			{"key path exist",        [b, ba], [{a, 1}, {b, [{ba, 21}, {bb, 22}]}],  21},
			{"keys list empty",       [],      [{a, 1}, {b, 2}],                     [{a, 1}, {b, 2}]} ],

	[{Desc, ?_assertEqual(Output, find_in(Keys, L))} || {Desc, Keys, L, Output} <- Test].

select_keys_test_() ->
	Test =
		[	{"list empty",      [a, b], [],               []},
			{"key not exist",   [a, b], [{b, 2}, {c, 3}], [{b, 2}]},
			{"keys not exist",  [a, b], [{c, 3}, {d, 4}], []},
			{"keys exist",      [a, b], [{a, 1}, {b, 2}], [{b, 2}, {a, 1}]},
			{"keys list empty", [],     [{a, 1}, {b, 2}], []} ],

	[{Desc, ?_assertEqual(Output, select_keys(Keys, L))} || {Desc, Keys, L, Output} <- Test].

put_test_() ->
	Test =
		[	{"list empty",    a, 1, [],               [{a, 1}]},
			{"key not exist", a, 1, [{b, 2}, {c, 3}], [{a, 1}, {b, 2}, {c, 3}]},
			{"key exists",    a, 1, [{a, 0}, {b, 2}], [{a, 1}, {b, 2}]} ],

	[{Desc, ?_assertEqual(Output, ?MODULE:put(Key, Val, M))} || {Desc, Key, Val, M, Output} <- Test].

merge_test_() ->
	Test =
		[	{"first empty",  [],               [{a, 1}, {b, 2}], [{a, 1}, {b, 2}]},
			{"second empty", [{a, 1}, {b, 2}], [],               [{a, 1}, {b, 2}]},
			{"both empty",   [],               [],               []},
			{"same key",     [{a, 1}, {b, 2}], [{b, 0}, {c, 3}], [{a, 1}, {b, 0}, {c, 3}]} ],

	[{Desc, ?_assertEqual(Output, merge(L1, L2))} || {Desc, L1, L2, Output} <- Test].

remove_test_() ->
	Test =
		[	{"list empty",    a, [],               []},
			{"key not exist", a, [{b, 2}, {c, 3}], [{b, 2}, {c, 3}]},
			{"key exists",    a, [{a, 1}, {b, 2}], [{b, 2}]} ],

	[{Desc, ?_assertEqual(Output, remove(Key, M))} || {Desc, Key, M, Output} <- Test].

is_empty_test_() ->
	Test =
		[	{"map empty",     [],       true},
			{"map not empty", [{a, 1}], false} ],

	[{Desc, ?_assertEqual(Output, is_empty(M))} || {Desc, M, Output} <- Test].

-endif.

