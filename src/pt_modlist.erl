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

-module(pt_modlist).

%% API
-export([
	calll/3,
	calll/4,
	callr/3,
	callr/4,
	calll_sublist/3,
	calll_sublist/4,
	callr_sublist/3,
	callr_sublist/4
]).

%% ==================================================================
%% API
%% ==================================================================

-spec calll(list(module()), atom(), list()) -> any().
calll(Mods, Fun, Args) ->
	calll_(Mods, Mods, Fun, Args, fun bad_call_error/3).

-spec calll(list(module()), atom(), list(), any()) -> any().
calll(Mods, Fun, Args, Default) ->
	calll_(Mods, Mods, Fun, Args, wrap_default_value(Default)).

-spec callr(list(module()), atom(), list()) -> any().
callr(Mods, Fun, Args) ->
	calll(lists:reverse(Mods), Fun, Args).

-spec callr(list(module()), atom(), list(), any()) -> any().
callr(Mods, Fun, Args, Default) ->
	calll(lists:reverse(Mods), Fun, Args, Default).

-spec calll_sublist(list(module()), atom(), list()) -> any().
calll_sublist(Mods, Fun, Args) ->
	Arity = length(Args) +1,
	calll_sublist_(Mods, Mods, Fun, Args, Arity, fun bad_call_error/3).

-spec calll_sublist(list(module()), atom(), list(), any()) -> any().
calll_sublist(Mods, Fun, Args, Default) ->
	Arity = length(Args) +1,
	calll_sublist_(Mods, Mods, Fun, Args, Arity, wrap_default_value(Default)).

-spec callr_sublist(list(module()), atom(), list()) -> any().
callr_sublist(Mods, Fun, Args) ->
	Arity = length(Args) +1,
	callr_sublist_(Mods, lists:reverse(Mods), Fun, Args, Arity, [], fun bad_call_error/3).

-spec callr_sublist(list(module()), atom(), list(), any()) -> any().
callr_sublist(Mods, Fun, Args, Default) ->
	Arity = length(Args) +1,
	callr_sublist_(Mods, lists:reverse(Mods), Fun, Args, Arity, [], wrap_default_value(Default)).

%% ==================================================================
%% Internal functions
%% ==================================================================

-spec calll_(list(module()), list(module()), atom(), list(), fun()) -> any().
calll_(InMods, [Mod|T], Fun, Args, Default) ->
	Arity = length(Args),
	case erlang:function_exported(Mod, Fun, Arity) of
		true  -> apply(Mod, Fun, Args);
		false -> calll_(InMods, T, Fun, Args, Default)
	end;
calll_(InMods, [], Fun, Args, Default) ->
	Default(InMods, Fun, length(Args)).

-spec calll_sublist_(list(module()), list(module()), atom(), list(), arity(), fun()) -> any().
calll_sublist_(InMods, [Mod|T] = Mods, Fun, Args, Arity, Default) ->
	case erlang:function_exported(Mod, Fun, Arity) of
		true  -> apply(Mod, Fun, [Mods|Args]);
		false -> calll_sublist_(InMods, T, Fun, Args, Arity, Default)
	end;
calll_sublist_(InMods, [], Fun, _, Arity, Default) ->
	Default(InMods, Fun, Arity).

-spec callr_sublist_(list(module()), list(module()), atom(), list(), arity(), list(module()), fun()) -> any().
callr_sublist_(InMods, [Mod|T], Fun, Args, Arity, Acc, Default) ->
	Acc2 = [Mod|Acc],
	case erlang:function_exported(Mod, Fun, Arity) of
		true  -> apply(Mod, Fun, [Acc2|Args]);
		false -> callr_sublist_(InMods, T, Fun, Args, Arity, Acc2, Default)
	end;
callr_sublist_(InMods, [], Fun, _, Arity, _, Default) ->
	Default(InMods, Fun, Arity).

-spec bad_call_error(list(module()), atom(), arity()) -> no_return().
bad_call_error(Mods, Fun, Arity) ->
	error({bad_call, Mods, Fun, Arity}).

-spec wrap_default_value(any()) -> fun((list(module()), atom(), arity()) -> any()).
wrap_default_value(Val) ->
	fun(_, _, _) -> Val end.

