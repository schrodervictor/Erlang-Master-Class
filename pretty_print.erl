-module(pretty_print).
-export([main/0]).

-spec print(strucs:expr()) -> string().
print({num, Num}) ->
    integer_to_list(Num);
print({var, Var}) ->
    atom_to_list(Var);
print({add, Expr1, Expr2}) ->
    "(" ++ print(Expr1) ++ "+" ++ print(Expr2) ++ ")";
print({mul, Expr1, Expr2}) ->
    "(" ++ print(Expr1) ++ "*" ++ print(Expr2) ++ ")".

-spec evaluate(strucs:list_of_vars(), strucs:expr()) -> integer().
evaluate(_Vars, {num, Num}) ->
    Num;
evaluate(Vars, {var, Var}) ->
    utils:lookup(Var, Vars);
evaluate(Vars, {add, Expr1, Expr2}) ->
    evaluate(Vars, Expr1) + evaluate(Vars, Expr2);
evaluate(Vars, {mul, Expr1, Expr2}) ->
    evaluate(Vars, Expr1) * evaluate(Vars, Expr2).

main() ->
    Expression = {add, {num, 2}, {mul, {num, 3}, {var, a}}},
    io:format("~s~n", [print(Expression)]),
    io:format("~b~n", [evaluate([{a,10}], Expression)]).
