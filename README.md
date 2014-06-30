# Pragmatic Toolset

an experimental set of helper functions 

### Quick Look

#### Key-value collections with same behaviour

- `pt_map` is a wraper around map
- `pt_plist` is some kind of property lists with unique keys
- `pt_mlist` wraps `pt_map` and `pt_plist`

For instance, `get/2` function always throws an error when key not found in the collection

	1> pt_map:get(a, #{}).
	** exception error: bad_key
			 in function  maps:get/2
					called as maps:get(a,#{})
			 in call from pt_map:get/2 (src/pt_map.erl, line 35)
	2> pt_plist:get(a, []). 
	** exception error: bad_key
			 in function  pt_plist:get/2 (src/pt_plist.erl, line 41)

All modules implement an `get/3` function which takes an default value as third argument
	
	3> pt_map:get(a, #{}, undefined).
	undefined
	4> pt_plist:get(a, [], undefined).
	undefined

`find/2` is the alias of the `get/3` function, works like previous example
	
	5> pt_map:find(a, #{}).           
	undefined
	6> pt_plist:find(a, []).          
	undefined

`put/3` inserts a new element

	7> pt_map:put(a, 1, #{b => 2, c => 3}). 
	#{a => 1,b => 2,c => 3}
	8> pt_plist:put(a, 1, [{b, 2}, {c, 3}]).
	[{a,1},{b,2},{c,3}]

`pt_mlist` module works properly with maps and tuples

	9> pt_mlist:put(a, 1, #{b => 2, c => 3}).
	#{a => 1,b => 2,c => 3}
	10> pt_mlist:put(a, 1, [{b, 2}, {c, 3}]). 
	[{a,1},{b,2},{c,3}]

There are more helpful functions, see `pt_kv_store` behaviour for more information

	11> pt_map:get_in([b, ba], #{a => 1, b => #{ba => 21, bb => 22}}).
	21
	12> pt_plist:get_in([b, ba], [{a, 1}, {b, [{ba, 21}, {bb, 22}]}]).
	21

#### Binary

Joining binary

	1> pt_binary:join([<<"Hello ">>, <<"world ">>, <<$!>>]).        
	<<"Hello world !">>
	2> pt_binary:join([<<"Hello">>, <<"world">>, <<$!>>], <<$\s>>). 
	<<"Hello world !">>

#### Term

Any to binary

	1> pt_term:to_binary(1).
	<<"1">>
	2> pt_term:to_binary(a).       
	<<"a">>
	3> pt_term:to_binary("list").
	<<"list">>

Any to list
	
	4> pt_term:to_list(1).              
	"1"
	5> pt_term:to_list(a).
	"a"
	6> pt_term:to_list(<<"binary">>).
	"binary"

### License

Provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT

