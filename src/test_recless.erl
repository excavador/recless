%% Test code for Recless.

-module(test_recless).
-author("Yariv Sadan (yarivvv@gmail.com, http://yarivsblog.com)").
-export([test/0]).
-compile({parse_transform, recless}).

-define(L(Val), io:format("~b ~p~n~n", [?LINE, Val])).

-record(bar, {x,y = 23,z}).
-record(foo, {a = 34,b = "bob",c = #bar{}}).
-record(baz, {d,e,f}).

test() ->
     1 = t1(),
    a = t2(#foo{a=a}),
    "b" = t3(#foo{b="b"}),
    foo = t5({#foo{a=foo}}),
    bar = t6({a,#foo{c=bar},b}),
    5 = t7([#foo{a=5, b=7}]),
    undefined = t8([#foo{a=4}, #foo{b=6}]),
    {4,6} = t9([#foo{a=4}, #foo{b=6}]),
    blah = t10(#foo{a=blah}),
    {4,6} = t11(#foo{a=4}, #foo{b=6}),
    {4,6} = t12(#foo{a=#foo{b=6}, b=4}),
    ok = t13(),
    ok = t14(),
    ok = t15(),
    ok = t16(),
    ok = t17(),
    ok = t19(),
    t25(),
    ok.

%% %% %% test patterns

t1() ->
    V1 = #foo{c=1},
    V3 = V1.c,
    V3.

t2(V1 = #foo{}) ->
    V1.a.

t3(V1) when is_record(V1, foo) ->
    R = V1.b,
    R.

t5({V1 = #foo{}}) ->
    V1.a.

t6({_,V1 = #foo{}, _}) ->
    V1.c.

t7([V = #foo{}]) ->
    V.a.

t8([V = #foo{} | _]) ->
    V.c.

t9([V = #foo{}, V1 = #foo{} | _]) ->
    A = V.a,
    B = V1.b,
    {A,B}.

t10(V1 = #foo{}) ->
    V1.a.

t11(V1 = #foo{}, V2 = #foo{}) ->
    A = V1.a,
    B = V2.b,
    {A, B}.

t12(V1 = #foo{a = A = #foo{}}) ->
    R1 = V1.b,
    R2 = A.b,
    {R1, R2}.

t13() ->
    A = #foo{a=3,b=4,c=5},
    {3} = {A.a},
    [3] = [A.a],
    [3,4,5] = [A.a, A.b, A.c],
    [B] = [#foo{c="bar"}],
    "bar" = B.c,
    {X,_Y,Z} = {#foo{a=4}, 7, #foo{c=3}},
    4 = X.a,
    3 = Z.c,
    ok.

t14() ->
    [A, {B,C, [D,E]}] = [#foo{a=5}, {#bar{x=10}, #bar{y="boo"},
    				     [#foo{b = <<"blah">>}, #bar{z=nada}]}],
    {5, 10, "boo", <<"blah">>, nada} =
    	{A.a, B.x, C.y, D.b, E.z},
    F = A,
    5 = F.a,
    {G, [H, I]} = {F, [D, E]},
    {5, <<"blah">>, nada} = {G.a, H.b, I.z},

     J = #foo{a = I.z},
     nada = J.a,
     K = #bar{y = #foo{a=17}},
     ok.

t15() ->
    A = #foo{a = #bar{x=8}, b=3, c=6},
    8 = (A#foo.a)#bar.x,
    8 = A.a.x,
    ok.

t16() ->
    A = #foo{a = #bar{x=8, y = #foo{ a= 17}}, b=3, c=6},
    9 = A#foo.b + A#foo.c,
    9 = A.b + A.c,
    -3 = -A.b,
    8 = A.a.x,
    14 = A.a.x + A.c,
    17 = A.a.y.a,
    ok.

t17() ->
    A = #foo{a = #bar{x=8, y = #foo{ a= 17}}, b=3, c=6},
    8 = (A#foo.a)#bar.x,
    3 = A.b,
    8 = A.a.x,
    17 = ((A#foo.a)#bar.y)#foo.a,
    17 = A.a.y.a,
    B = #foo{a = #bar{x = #foo{b = #bar{y = 19, z=26}}}, b=hello, c = "boo"},
    19 = B.a.x.b.y,
    26 = B.a.x.b.z,
    "boo" = B.c,

    C = B,
    hello = C.b,
    19 = C.a.x.b.y,
    26 = C.a.x.b.z,

    B#foo{c = "zbang"},
    B1 = B#foo{c = "zbang"},
    hello = B1.b,
    "zbang" = B1.c,
    19 = B1.a.x.b.y,
    26 = B1.a.x.b.z,

    B2 = B1#foo{a=#bar{x="boing"}},
    "boing" = B2.a.x,

    D = #foo{a = #bar{x=8, y = #baz{d = 37}}, b=3, c=6},
    37 = D.a.y.d,
    ok.


%% t18() ->
%%     A = #foo{a=123},
%%     %% the compiler warns that this is unsafe but we'll support this
%%     %% expression anyway for now
%%     catch B = A.a,
%%     123 = A.a,
%%     123 = B,
%%     ok.

t19() ->
    A = #bar{x = 9, y = "foo", z = #foo{a=3}},
    [9] = [B || B <- [A.x]],
    [9, "foo", 3] = [B || B <- [A.x, A.y, A.z.a]],
    [B || B <- [A.x]],
    ok.

t24() ->
    #foo{}.

t25() ->
    A = t24(),
    "bob" = A.b,
    B = A.b,
    "bob" = B,
    ok.

