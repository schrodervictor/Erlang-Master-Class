-module(utils).
-export([lookup/2]).

-spec lookup(atom(), structs:list_of_vars()) -> integer().
lookup(Var, [{Var, Num}|_]) ->
    Num;
lookup(Var, [_|Tail]) ->
    lookup(Var, Tail).
