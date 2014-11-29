# Pragmatic Toolset

[![Build Status][travis_img]][travis]

an experimental set of helper functions 

### Quick Look

#### Key-value collections with same behaviour

- `pt_map` is a thin wrapper around erlang map
- `pt_kvlist` is a list of key-value elements `[{key(), value()}]`
- `pt_kvterm` contains functions to work with both `pt_map` and `pt_kvlist`

For instance, `get/2` function always throws an error when key not found in the collection

```erlang
1> pt_map:get(a, #{}).
** exception error: bad_key
		 in function  maps:get/2
				called as maps:get(a,#{})
		 in call from pt_map:get/2 (src/pt_map.erl, line 35)
2> pt_kvlist:get(a, []). 
** exception error: bad_key
		 in function  pt_kvlist:get/2 (src/pt_kvlist.erl, line 41)
```

All modules implement an `get/3` function which takes an default value as third argument

```erlang
3> pt_map:get(a, #{}, undefined).
undefined
4> pt_kvlist:get(a, [], undefined).
undefined
```

`find/2` is the alias of the `get/3` function, default value is set to `undefined`
	
```erlang
5> pt_map:find(a, #{}).           
undefined
6> pt_kvlist:find(a, []).          
undefined
```

`put/3` inserts a new element

```erlang
7> pt_map:put(a, 1, #{b => 2, c => 3}). 
#{a => 1,b => 2,c => 3}
8> pt_kvlist:put(a, 1, [{b, 2}, {c, 3}]).
[{a,1},{b,2},{c,3}]
```

Use `pt_kvterm` to work with unknown data structures. No matter is it map or list.

```erlang
9> pt_kvterm:put(a, 1, #{b => 2, c => 3}).
#{a => 1,b => 2,c => 3}
10> pt_kvterm:put(a, 1, [{b, 2}, {c, 3}]). 
[{a,1},{b,2},{c,3}]
```

There are more helpful functions, see `pt_kvstore` behaviour for more information

```erlang
11> pt_map:get_in([b, ba], #{a => 1, b => #{ba => 21, bb => 22}}).
21
12> pt_kvlist:get_in([b, ba], [{a, 1}, {b, [{ba, 21}, {bb, 22}]}]).
21
```

#### Binary

Joining binary

```erlang
1> pt_binary:join([<<"Hello ">>, <<"world ">>, <<$!>>]).        
<<"Hello world !">>
2> pt_binary:join([<<"Hello">>, <<"world">>, <<$!>>], <<$\s>>). 
<<"Hello world !">>
```

#### Term

Convert any erlang term to a binary

```erlang
1> pt_term:to_binary(1).
<<"1">>
2> pt_term:to_binary(a).       
<<"a">>
3> pt_term:to_binary("list").
<<"list">>
```

Convert any erlang term to a list
	
```erlang
4> pt_term:to_list(1).              
"1"
5> pt_term:to_list(a).
"a"
6> pt_term:to_list(<<"binary">>).
"binary"
```

### License

Provided under the terms of [the MIT license][license].

[travis]:https://travis-ci.org/manifest/pt
[travis_img]:https://secure.travis-ci.org/manifest/pt.png
[license]:http://www.opensource.org/licenses/MIT

