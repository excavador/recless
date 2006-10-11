-module(test2).
-compile(export_all).

test() ->
    {ok, Tree} = epp:parse_file("test_recless.erl", [], []),
    recless:parse_transform(Tree, undefined),
    ok.
