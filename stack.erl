-module(stack).
-export([main/0]).

-type stack() :: [integer()].
-type instruction() :: {push, integer()}
                     | {fetch, atom()}
                     | {add2}
                     | {mul2}.

-type program() :: [instruction()].

-spec compile(structs:expr()) -> program().

compile({num, Num}) ->
    [{push, Num}];
compile({var, Var}) ->
    [{fetch, Var}];
compile({add, Expr1, Expr2}) ->
    compile(Expr1) ++ compile(Expr2) ++ [{add2}];
compile({mul, Expr1, Expr2}) ->
    compile(Expr1) ++ compile(Expr2) ++ [{mul2}].

-spec run(program(), strucs:list_of_vars(), stack()) ->
    integer().

run([{push, Num}|Continue], Env, Stack) ->
    run(Continue, Env, [Num|Stack]);
run([{fetch, Var}|Continue], Env, Stack) ->
    run(Continue, Env, [pretty_print:lookup(Var, Env)|Stack]);
run([{add2}|Continue], Env, [Num1, Num2 | Stack]) ->
    run(Continue, Env, [Num1+Num2|Stack]);
run([{mul2}|Continue], Env, [Num1, Num2 | Stack]) ->
    run(Continue, Env, [Num1*Num2|Stack]);
run([], _Env, [Result]) ->
    Result.

main() ->
    Expression = {add, {num, 2}, {mul, {num, 3}, {num, 4}}},
%    Program = [{push, 2}, {push, 3}, {push, 4}, {mul2}, {add2}],
    io:format("~b~n", [run(compile(Expression), [], [])]).
