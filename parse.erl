-module(parse).
-export([main/0]).

-spec parse(string()) -> {pretty_print:expr(), string()}.
parse([$(|Rest]) ->
    {Expr1, Rest1} = parse(Rest),
    [Operator|Rest2] = Rest1,
    {Expr2, Rest3} = parse(Rest2),
    [$)|Rest4] = Rest3,
    case Operator of
        $+ ->
            Expr = {add, Expr1, Expr2};
        $* ->
            Expr = {mul, Expr1, Expr2}
    end,
    {Expr, Rest4};
parse([Char|Tail]) when $a =< Char andalso $z >= Char ->
    {Match, Rest} = get_while(fun is_alpha/1, Tail),
    Expr = {var, list_to_atom([Char|Match])},
    {Expr, Rest};
parse([Char|Tail]) when $0 =< Char andalso $9 >= Char ->
    {Match, Rest} = get_while(fun is_num/1, Tail),
    Expr = {num, list_to_integer([Char|Match])},
    {Expr, Rest}.

-spec is_alpha(char()) -> boolean().
is_alpha(Char) -> $a =< Char andalso $z >= Char.

-spec is_num(char()) -> boolean().
is_num(Char) -> $0 =< Char andalso $9 >= Char.

-spec get_while(fun((Type) -> boolean()), string()) -> {[Type], [_]}.
get_while(CheckFun, String) ->
    {Result, Rest} = get_while(CheckFun, String, []),
    {lists:reverse(Result), Rest}.

get_while(CheckFun, [First|String], Accumulator) ->
    case CheckFun(First) of
        true ->
            get_while(CheckFun, String, [First|Accumulator]);
        false ->
            {Accumulator, [First|String]}
    end;
get_while(_CheckFun, [], Accumulator) ->
    {Accumulator, []}.

main() ->
    String = "(2+(3*4))",
    {Expression, _} = parse(String),
    io:format("~p~n", [Expression]).


