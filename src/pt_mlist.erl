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

-module(pt_mlist).
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
	is_empty/1,
	to_map/1,
	to_list/1
]).

%% Definitions
-define(BRIDGE(Fun, ML),
	(fun
		(M) when is_map(M) -> pt_map:Fun(M);
		(L)                -> pt_plist:Fun(L)
	end)(ML)).

-define(BRIDGE(Fun, Arg1, ML),
	(fun
		(M) when is_map(M) -> pt_map:Fun(Arg1, M);
		(L)                -> pt_plist:Fun(Arg1, L)
	end)(ML)).

-define(BRIDGE(Fun, Arg1, Arg2, ML),
	(fun
		(M) when is_map(M) -> pt_map:Fun(Arg1, Arg2, M);
		(L)                -> pt_plist:Fun(Arg1, Arg2, L)
	end)(ML)).

-define(BRIDGE_D(Fun, Arg1, ML, Default),
	(fun
		(M) when is_map(M) -> pt_map:Fun(Arg1, M, Default);
		(L)                -> pt_plist:Fun(Arg1, L, Default)
	end)(ML)).

%% Types
-type mlist() :: map() | pt_plist:plist().

-export_type([mlist/0]).

%% ==================================================================
%% API
%% ==================================================================

-spec keys(mlist()) -> [any()].
keys(ML) ->
	?BRIDGE(keys, ML).

-spec values(mlist()) -> [any()].
values(ML) ->
	?BRIDGE(values, ML).

-spec get(any(), mlist()) -> any().
get(Key, ML) ->
	?BRIDGE(get, Key, ML).

-spec get(any(), mlist(), any()) -> any().
get(Key, ML, Default) ->
	?BRIDGE_D(get, Key, ML, Default).

-spec get_in([any()], mlist()) -> any().
get_in(Keys, ML) ->
	?BRIDGE(get_in, Keys, ML).

-spec get_in([any()], mlist(), any()) -> any().
get_in(Keys, ML, Default) ->
	?BRIDGE_D(get_in, Keys, ML, Default).

-spec find(any(), mlist()) -> undefined | any().
find(Key, ML) ->
	?BRIDGE(find, Key, ML).

-spec find_in([any()], mlist()) -> undefined | any().
find_in(Keys, ML) ->
	?BRIDGE(find_in, Keys, ML).

-spec select_keys([any()], mlist()) -> mlist().
select_keys(Keys, ML) ->
	?BRIDGE(select_keys, Keys, ML).

-spec put(any(), any(), mlist()) -> mlist().
put(Key, Val, ML) ->
	?BRIDGE(put, Key, Val, ML).

-spec merge(mlist(), mlist()) -> mlist().
merge(L, M) when not is_map(L), is_map(M) ->
	M1 = to_map(L),
	merge(M1, M);
merge(M, L) when not is_map(L), is_map(M) ->
	L1 = to_list(M),
	merge(L1, L);
merge(M1, M2) when is_map(M1) ->
	pt_map:merge(M1, M2);
merge(L1, L2) ->
	pt_plist:merge(L1, L2).

-spec remove(any(), mlist()) -> mlist().
remove(Key, ML) ->
	?BRIDGE(remove, Key, ML).

-spec is_empty(mlist()) -> boolean().
is_empty(ML) ->
	?BRIDGE(is_empty, ML).

-spec to_map(mlist()) -> map().
to_map(M) when is_map(M) -> M;
to_map(L)                -> maps:from_list(L).

-spec to_list(mlist()) -> pt_plist:plist().
to_list(M) when is_map(M) -> maps:to_list(M);
to_list(L)                -> L.

