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

-module(pt_kvterm).
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
	is_empty/1,
	to_map/1,
	to_list/1
]).

%% Definitions
-define(BRIDGE(Fun, T),
	(fun
		(M) when is_map(M) -> pt_map:Fun(M);
		(L)                -> pt_kvlist:Fun(L)
	end)(T)).

-define(BRIDGE(Fun, Arg1, T),
	(fun
		(M) when is_map(M) -> pt_map:Fun(Arg1, M);
		(L)                -> pt_kvlist:Fun(Arg1, L)
	end)(T)).

-define(BRIDGE(Fun, Arg1, Arg2, T),
	(fun
		(M) when is_map(M) -> pt_map:Fun(Arg1, Arg2, M);
		(L)                -> pt_kvlist:Fun(Arg1, Arg2, L)
	end)(T)).

-define(BRIDGE_D(Fun, Arg1, T, Default),
	(fun
		(M) when is_map(M) -> pt_map:Fun(Arg1, M, Default);
		(L)                -> pt_kvlist:Fun(Arg1, L, Default)
	end)(T)).

%% Types
-type kvterm() :: map() | pt_kvlist:kvlist().

-export_type([kvterm/0]).

%% ==================================================================
%% API
%% ==================================================================

-spec keys(kvterm()) -> list().
keys(T) ->
	?BRIDGE(keys, T).

-spec values(kvterm()) -> list().
values(T) ->
	?BRIDGE(values, T).

-spec get(any(), kvterm()) -> any().
get(Key, T) ->
	?BRIDGE(get, Key, T).

-spec get(any(), kvterm(), any()) -> any().
get(Key, T, Default) ->
	?BRIDGE_D(get, Key, T, Default).

-spec get_in(list(), kvterm()) -> any().
get_in(Keys, T) ->
	?BRIDGE(get_in, Keys, T).

-spec get_in(list(), kvterm(), any()) -> any().
get_in(Keys, T, Default) ->
	?BRIDGE_D(get_in, Keys, T, Default).

-spec find(any(), kvterm()) -> {ok, any()} | error.
find(Key, T) ->
	?BRIDGE(find, Key, T).

-spec find_in(list(), kvterm()) -> {ok, any()} | error.
find_in(Keys, T) ->
	?BRIDGE(find_in, Keys, T).

-spec with(list(), kvterm()) -> kvterm().
with(Keys, T) ->
	?BRIDGE(with, Keys, T).

-spec put(any(), any(), kvterm()) -> kvterm().
put(Key, Val, T) ->
	?BRIDGE(put, Key, Val, T).

-spec merge(kvterm(), kvterm()) -> kvterm().
merge(L, M) when not is_map(L), is_map(M) ->
	M1 = to_map(L),
	merge(M1, M);
merge(M, L) when not is_map(L), is_map(M) ->
	L1 = to_list(M),
	merge(L1, L);
merge(M1, M2) when is_map(M1) ->
	pt_map:merge(M1, M2);
merge(L1, L2) ->
	pt_kvlist:merge(L1, L2).

-spec remove(any(), kvterm()) -> kvterm().
remove(Key, T) ->
	?BRIDGE(remove, Key, T).

-spec is_empty(kvterm()) -> boolean().
is_empty(T) ->
	?BRIDGE(is_empty, T).

-spec to_map(kvterm()) -> map().
to_map(M) when is_map(M) -> M;
to_map(L)                -> maps:from_list(L).

-spec to_list(kvterm()) -> pt_kvlist:kvlist().
to_list(M) when is_map(M) -> maps:to_list(M);
to_list(L)                -> L.

