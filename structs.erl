-module(structs).
-export_type([expr/0, list_of_vars/0]).

-type expr() :: {num, integer()}
              | {var, atom()}
              | {add, expr()}
              | {mul, expr()}.

-type list_of_vars() :: [{atom(), integer()}].
