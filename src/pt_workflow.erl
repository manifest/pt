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

-module(pt_workflow).

%% API
-export([
	decl/1,
	decl/2,
	decl/3,
	init/2,
	init/4,
	calll/3,
	callr/3,
	calll_sublist/3,
	callr_sublist/3,
	modules/1,
	state/1
]).

%% Types
-type declaration()
	:: {Base :: module(), Mod :: module(), DefaultOptions :: map()}
	 |                   {Mod :: module(), DefaultOptions :: map()}.

-record(wf, {
	mods   =  [] :: list(module()),
	state  = #{} :: map()
}).

-type workflow() :: #wf{}.

-export_type([declaration/0, workflow/0]).

%% ==================================================================
%% API
%% ==================================================================

-spec decl(module()) -> workflow().
decl(Mod) ->
	decl(decl, Mod).

-spec decl(atom(), module()) -> workflow().
decl(Decl, Mod) ->
	decl(Decl, Mod:Decl(), #wf{}).

-spec init(module(), map()) -> workflow().
init(Mod, State) ->
	init(decl, init, Mod, State).

-spec init(atom(), atom(), module(), map()) -> workflow().
init(Decl, Init, Mod, State) ->
	W1 = decl(Decl, Mod),
	W2 = W1#wf{state = maps:merge(W1#wf.state, State)},
	W2#wf{state = callr(Init, [], W2)}.

-spec calll(atom(), list(), workflow()) -> any().
calll(Fun, Args, #wf{mods = Mods, state = State}) ->
	pt_modlist:calll(Mods, Fun, Args ++ [State]).

-spec callr(atom(), list(), workflow()) -> any().
callr(Fun, Args, #wf{mods = Mods, state = State}) ->
	pt_modlist:callr(Mods, Fun, Args ++ [State]).

-spec calll_sublist(atom(), list(), workflow()) -> any().
calll_sublist(Fun, Args, #wf{mods = Mods, state = State}) ->
	pt_modlist:calll_sublist(Mods, Fun, Args ++ [State]).

-spec callr_sublist(atom(), list(), workflow()) -> any().
callr_sublist(Fun, Args, #wf{mods = Mods, state = State}) ->
	pt_modlist:callr_sublist(Mods, Fun, Args ++ [State]).

-spec state(workflow()) -> map().
state(#wf{state = State}) ->
	State.

-spec modules(workflow()) -> list(module()).
modules(#wf{mods = Mods}) ->
	Mods.

%% ==================================================================
%% Internal functions
%% ==================================================================

-spec decl(atom(), declaration(), workflow()) -> workflow().
decl(Decl, {Base, Mod, State}, W) ->
	decl(Decl, Base:Decl(), update(Mod, State, W));
decl(_, {Mod, State}, W) ->
	update(Mod, State, W).

-spec update(module(), map(), workflow()) -> workflow().
update(Mod, State, W) ->
	W#wf{
		mods = [Mod|W#wf.mods],
		state = maps:merge(State, W#wf.state)}.

