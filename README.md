# Pragmatic Toolset

[![Build Status][travis_img]][travis]

Small Erlang library for the big fight against routine data manipulations.

### How to use

Manipulations with Key-Value Lists

```erlang
1>  L = [{a, 1}, {b, 2}, {c, [{d, 3}]}].
[{a,1},{b,2},{c,[{d,3}]}]
2> pt_kvlist:keys(L).
[a,b,c]
3> pt_kvlist:values(L).
[1,2,[{d,3}]]
4> pt_kvlist:get(a, L).
1
5> pt_kvlist:get(z, L).
** exception error: bad_key
     in function  pt_kvlist:bad_key_error/0 (src/pt_kvlist.erl, line 141)
6> pt_kvlist:get(z, L, default).
default
7> pt_kvlist:find(z, L).          
error
8> pt_kvlist:find(a, L).
{ok, 1}
9> pt_kvlist:get_in([c, d], L).
3
10> pt_kvlist:select_keys([a, b], L).
[{b,2},{a,1}]
```

Manipulations with Maps

```erlang
1> M = #{a => 1, b => 2, c => #{d => 3}}.
#{a => 1,b => 2,c => #{d => 3}}
2> pt_map:keys(M).
[a,b,c]
3> pt_map:values(M).
[1,2,#{d => 3}]
4> pt_map:get(a, M).
1
5> pt_map:get(z, M).
** exception error: bad_key
     in function  maps:get/2
        called as maps:get(z,#{a => 1,b => 2,c => #{d => 3}})
     in call from pt_map:get/2 (src/pt_map.erl, line 59)
6> pt_map:get(z, M, default).
default
7> pt_map:find(z, M).          
error
8> pt_map:find(a, M).
{ok, 1}
9> pt_map:get_in([c, d], M).
3
10> pt_map:select_keys([a, b], M).
#{a => 1,b => 2}
```

In case you don't want to care about the type of container

```erlang
1> L = [{a, 1}, {b, 2}, {c, [{d, 3}]}].
[{a,1},{b,2},{c,[{d,3}]}]
2> M = #{a => 1, b => 2, c => #{d => 3}}.
#{a => 1,b => 2,c => #{d => 3}}
3> pt_kvterm:keys(L).
[a,b,c]
4> pt_kvterm:keys(M).
[a,b,c]
5> pt_kvterm:values(L).
[1,2,[{d,3}]]
6> pt_kvterm:values(M).
[1,2,#{d => 3}]
7> pt_kvterm:get(a, L).
1
8> pt_kvterm:get(a, M).
1
9> pt_kvterm:get_in([c, d], L).
3
10> pt_kvterm:get_in([c, d], M).
3
11> pt_kvterm:select_keys([a, b], L).
[{b,2},{a,1}]
12> pt_kvterm:select_keys([a, b], M).
#{a => 1,b => 2}
```

There are more helpful functions, see `pt_kvstore` behaviour for more information

### License

Provided under the terms of [the MIT license][license].

[travis]:https://travis-ci.org/manifest/pt
[travis_img]:https://secure.travis-ci.org/manifest/pt.png
[license]:http://www.opensource.org/licenses/MIT

