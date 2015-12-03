-module(simplify).
-export([main/0, simplify/1]).

mul_by_zero({mul, {num, 0}, _}) ->
    {num, 0};
mul_by_zero({mul, _, {num, 0}}) ->
    {num, 0};
mul_by_zero(Expr) -> Expr.

add_zero({add, {num, 0}, Expr}) ->
    Expr;
add_zero({add, Expr, {num, 0}}) ->
    Expr;
add_zero(Expr) -> Expr.

mul_by_one({mul, {num, 1}, Expr}) ->
    Expr;
mul_by_one({mul, Expr, {num, 1}}) ->
    Expr;
mul_by_one(Expr) ->
    Expr.

-spec simplify(pretty_print:expr()) -> pretty_print:expr().
simplify({add, Expr1, Expr2}) ->
    F = simplification_function(),
    F({add, F(Expr1), F(Expr2)});
simplify({mul, Expr1, Expr2}) ->
    F = simplification_function(),
    F({mul, F(Expr1), F(Expr2)});
simplify(Expr) ->
    Expr.

-spec strategies() -> [function()].
strategies() ->
    [fun mul_by_zero/1, fun add_zero/1, fun mul_by_one/1].

-spec simplification_function() -> function().
simplification_function() ->
    compose(strategies()).

-spec compose([function()]) -> function().
compose([Fun|[]]) -> Fun;
compose([Fun|Tail]) ->
    fun(E) -> Fun((compose(Tail))(E)) end.

main() ->
    Expression = {add, {num, 1}, {mul, {num, 3}, {num, 0}}},
    io:format("~p~n", [simplify(Expression)]).
