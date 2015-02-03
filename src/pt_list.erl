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

-module(pt_list).

%% API
-export([
	with/2,
	join/1,
	join/2,
	separate/2,
	deepmap/2
]).

%% ==================================================================
%% API
%% ==================================================================

-spec with(list(), list()) -> list().
with(Keys, L) ->
	lists:filter(fun(T) -> lists:member(T, Keys) end, L).

-spec deepmap(fun((any()) -> any()), list()) -> list().
deepmap(_, [])    -> [];
deepmap(F, [H|T]) -> [deepmap(F, H)|deepmap(F, T)];
deepmap(F, Val)   -> F(Val).

-spec join(list() | [list()]) -> list().
join(L) ->
	join(L, []).

-spec join(list() | [list()], list()) -> list().
join(L, Sep) when not is_list(Sep) -> join(L, [Sep]);
join([H|T], Sep) -> H ++ lists:append([Sep ++ X || X <- T]);
join([], _)      -> [].

-spec separate(list(), any()) -> list().
separate([], _)      -> [];
separate(L, Sep)     ->
	[H|T] = lists:reverse(L),
	lists:foldl(fun(Val, Acc) -> [Val,Sep|Acc] end, [H], T).

%% ==================================================================
%% Tests 
%% ==================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

with_test_() ->
	Test =
		[	{"list empty",      [a, b], [],     []},
			{"key not exist",   [a, b], [b, c], [b]},
			{"keys not exist",  [a, b], [c, d], []},
			{"keys exist",      [a, b], [a, b], [a, b]},
			{"keys list empty", [],     [a, b], []} ],

	[{Desc, ?_assertEqual(Output, with(Keys, L))} || {Desc, Keys, L, Output} <- Test].

join_test_() ->
	SepL =
		[	{" w/ 1-val sep",      $z},
			{" w/ 1-val list sep", [$z]},
			{" w/ 2-val list sep", [$z, $z]},
			{" w/ empty list sep", []} ],

	Test =
		[	{"list empty",  [],
				[	[],
					[],
					[],
					[] ]},
			{"list 1-list", [[$a, $a]],
				[	[$a, $a],
					[$a, $a],
					[$a, $a],
					[$a, $a] ]},
			{"list 2-list", [[$a, $a], [$b, $b]],
				[	[$a, $a, $z, $b, $b],
					[$a, $a, $z, $b, $b],
					[$a, $a, $z, $z, $b, $b],
					[$a, $a, $b, $b]]} ],

	[{InDesc ++ SepDesc, ?_assertEqual(lists:nth(N, Output), join(Input, Sep))}
		||	{{SepDesc, Sep}, N} <- lists:zip(SepL, lists:seq(1, length(SepL))),
				{InDesc, Input, Output} <- Test].

separate_test_() ->
	Test =
		[	{"list empty", [],     []},
			{"list 1-val", [a],    [a]},
			{"list 2-val", [a, a], [a, z, a]} ],

	[{Desc, ?_assertEqual(Output, separate(Input, z))} || {Desc, Input, Output} <- Test].

deepmap_test_() ->
	Fun = fun(Val) -> -Val end,
	Test =
		[	{"list empty",  [],                           []},
			{"list flat",   [1, 2, 3],                    [-1, -2, -3]},
			{"list nested", [1, [[2, 3], 4, [5], []], 7], [-1, [[-2, -3], -4, [-5], []], -7]} ],
	
	[{Desc, ?_assertEqual(Output, deepmap(Fun, L))} || {Desc, L, Output} <- Test].

-endif.

