-module(pretty_print).
-export([main/0]).
-export_type([expr/0, list_of_vars/0]).

-type expr() :: {num, integer()}
              | {var, atom()}
              | {add, expr()}
              | {mul, expr()}.

-type list_of_vars() :: [{atom(), integer()}].

-spec print(expr()) -> string().
print({num, Num}) ->
    integer_to_list(Num);
print({var, Var}) ->
    atom_to_list(Var);
print({add, Expr1, Expr2}) ->
    "(" ++ print(Expr1) ++ "+" ++ print(Expr2) ++ ")";
print({mul, Expr1, Expr2}) ->
    "(" ++ print(Expr1) ++ "*" ++ print(Expr2) ++ ")".

-spec evaluate(list_of_vars(), xpr()) -> integer().
evaluate(_Vars, {num, Num}) ->
    Num;
evaluate(Vars, {var, Var}) ->
    lookup(Var, Vars);
evaluate(Vars, {add, Expr1, Expr2}) ->
    evaluate(Vars, Expr1) + evaluate(Vars, Expr2);
evaluate(Vars, {mul, Expr1, Expr2}) ->
    evaluate(Vars, Expr1) * evaluate(Vars, Expr2).

-spec lookup(atom(), list_of_vars()) -> integer().
lookup(Var, [{Var, Num}|_]) ->
    Num;
lookup(Var, [_|Tail]) ->
    lookup(Var, Tail).

main() ->
    Expression = {add, {num, 2}, {mul, {num, 3}, {var, a}}},
    io:format("~s~n", [print(Expression)]),
    io:format("~b~n", [evaluate([{a,10}], Expression)]).