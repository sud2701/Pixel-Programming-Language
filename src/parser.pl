:- table main_block/2.
program(program(CodeBlock)) --> [main], block(CodeBlock).

block(empty()) --> ['{'], ['}'].
block(code_block(CodeBlock)) --> ['{'], sub_block(CodeBlock),['}'].

sub_block(sub_block(Dec, Rest)) --> declaration(Dec), sub_block(Rest).
sub_block(sub_block(Cmd)) --> leftRecursionRemovedCommand(Cmd).
sub_block(sub_block(Dec)) --> declaration(Dec).
sub_block(sub_block(Cmd, Rest)) --> leftRecursionRemovedCommand(Cmd), sub_block(Rest).
sub_block([]) --> [].

declaration(declaration_node(Dec)) --> const_declaration(Dec).
declaration(declaration_node(Dec)) --> var_declaration(Dec).

const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;].
const_declaration(const_declaration(const, Dtype, Iden, =, Bool, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Bool), [;].
const_declaration(const_declaration(const, Dtype, Iden, =, Str, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], string_(Str), [;].
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], ternary_expression(Expr), [;].
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;], declaration(Rest).
const_declaration(const_declaration(const, Dtype, Iden, =, Bool, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Bool), [;], declaration(Rest).
const_declaration(const_declaration(const, Dtype, Iden, =, Str, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], string_(Str), [;], declaration(Rest).
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], ternary_expression(Expr), [;], declaration(Rest).


var_declaration(var_declaration(var, Dtype, Iden, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [;].
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;].
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Expr), [;].
var_declaration(var_declaration(var, Dtype, Iden, =, Str, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], string_(Str), [;].
var_declaration(var_declaration(var, Dtype, Iden, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [;], declaration(Rest).
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;], declaration(Rest).
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Expr), [;], declaration(Rest).
var_declaration(var_declaration(var, Dtype, Iden, =, Str, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], string_(Str), [;], declaration(Rest).

variable_datatype(datatype(int)) --> [int].
variable_datatype(datatype(bool)) --> [bool].
variable_datatype(datatype(str)) --> [str].


leftRecursionRemovedCommand(command_assign(T)) --> assignment(T), [;].
leftRecursionRemovedCommand(command_assign(T,Cmd)) --> assignment(T),[;],leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], arithmetic_assign(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], arithmetic_assign(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop_range(Iden, in, range, From, To, Jump, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(From), [','], simple_expression(To), [','], simple_expression(Jump), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop_range_single(Iden, in, range, To, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(To), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(if_else(if, Bool, Cmd, else, Cmd1)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), [else], leftRecursionRemovedCommand(Cmd1).
leftRecursionRemovedCommand(if_else_if(if, Bool, Cmd, Rest)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd),  else_if_ladder(Rest).
leftRecursionRemovedCommand(if(if, Bool, Cmd)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_parenthesis_left, variable(Iden), [=], booleanCondition(Bool), ['?'], commandForTernary(Expr1), [:], commandForTernary(Expr2), optional_parenthesis_right, [;], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(ternary_operator(Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_parenthesis_left, booleanCondition(Bool), ['?'], commandForTernary(Expr1), [:], commandForTernary(Expr2), optional_parenthesis_right, [;], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left, variable(Iden), [=], booleanCondition(Bool), ['?'], commandForTernary(Expr1), [:], commandForTernary(Expr2), optional_parenthesis_right, [;].
leftRecursionRemovedCommand(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left, booleanCondition(Bool), ['?'], commandForTernary(Expr1), [:], commandForTernary(Expr2), optional_parenthesis_right, [;].
leftRecursionRemovedCommand(while_loop(Boolean, Cmd)) --> [while], ['('], booleanCondition(Boolean), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(print(Arg)) --> [print], print_statement(Arg), [;].
leftRecursionRemovedCommand(print(Arg,Cmd)) --> [print], print_statement(Arg), [;],leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(increment(Iden, +, +)) --> variable(Iden), [+], [+], [;].
leftRecursionRemovedCommand(increment(Iden, +, +, Cmd)) --> variable(Iden), [+], [+], [;], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(decrement(Iden, -, -)) --> variable(Iden), [-], [-], [;].
leftRecursionRemovedCommand(decrement(Iden, -, -, Cmd)) --> variable(Iden), [-], [-], [;], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], simple_expression(Expr).
leftRecursionRemovedCommand(add_(Iden, +, =, Expr, Cmd)) --> variable(Iden), [+], [=], simple_expression(Expr), [;], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(sub_(Iden, -, =, Expr)) --> variable(Iden), [-], [=], simple_expression(Expr).
leftRecursionRemovedCommand(sub_(Iden, -, =, Expr, Cmd)) --> variable(Iden), [-], [=], simple_expression(Expr), [;], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(multiply_(Iden, *, =, Expr)) --> variable(Iden), [*], [=], simple_expression(Expr).
leftRecursionRemovedCommand(multiply_(Iden, *, =, Expr, Cmd)) --> variable(Iden), [*], [=], simple_expression(Expr), [;], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(divide_(Iden, /, =, Expr)) --> variable(Iden), [/], [=], simple_expression(Expr).
leftRecursionRemovedCommand(divide_(Iden, /, =, Expr, Cmd)) --> variable(Iden), [/], [=], simple_expression(Expr), [;], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(command_block(Blk)) --> block(Blk).

ternary_expression(Expr) --> simple_expression(Expr).
ternary_expression(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left(), variable(Iden), [=], booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right().
ternary_expression(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left(), booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right().
optional_parenthesis_left() --> ['('].
optional_parenthesis_left() --> [].

optional_parenthesis_right() --> [')'].
optional_parenthesis_right() --> [].

else_if_ladder(elif(elif, Bool, Cmd)) --> [else, if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd).
else_if_ladder(elif(elif, Bool, Cmd, Rest)) --> [else, if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), else_if_ladder(Rest).
else_if_ladder(elif(elif, Bool, Cmd, else, Cmd1)) --> [else, if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), [else], leftRecursionRemovedCommand(Cmd1).

commandForTernary(command_assign(T)) --> assignment(T).
commandForTernary(command_assign(T,Cmd)) --> assignment(T),[;], commandForTernary(Cmd).
commandForTernary(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], arithmetic_assign(Expr), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], arithmetic_assign(Expr), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop_range(Iden, in, range, From, To, Jump, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(From), [','], simple_expression(To), [','], simple_expression(Jump), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop_range_single(Iden, in, range, To, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(To), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(if_else(if, Bool, Cmd, else, Cmd1)) --> [if], ['('], booleanCondition(Bool), [')'], commandForTernary(Cmd), [else], leftRecursionRemovedCommand(Cmd1).
commandForTernary(if_else_if(if, Bool, Cmd, Rest)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd),  else_if_ladder(Rest).
commandForTernary(if(if, Bool, Cmd)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_parenthesis_left, variable(Iden), [=], booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right, [;], commandForTernary(Cmd).
commandForTernary(ternary_operator(Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_parenthesis_left, booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right, [;], commandForTernary(Cmd).
commandForTernary(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left, variable(Iden), [=], booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right.
commandForTernary(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left, booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right.
commandForTernary(while_loop(Boolean, Cmd)) --> [while], ['('], booleanCondition(Boolean), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(print(Arg)) --> [print], print_statement(Arg).
commandForTernary(print(Arg, Cmd)) --> [print], print_statement(Arg), [;], commandForTernary(Cmd).
commandForTernary(increment(Iden, +, +)) --> variable(Iden), [+], [+].
commandForTernary(increment(Iden, +, +, Cmd)) --> variable(Iden), [+], [+], [;], commandForTernary(Cmd).
commandForTernary(decrement(Iden, -, -)) --> variable(Iden), [-], [-].
commandForTernary(decrement(Iden, -, -, Cmd)) --> variable(Iden), [-], [-], [;], commandForTernary(Cmd).
commandForTernary(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], simple_expression(Expr).
commandForTernary(add_(Iden, +, =, Expr, Cmd)) --> variable(Iden), [+], [=], simple_expression(Expr), [;], commandForTernary(Cmd).
commandForTernary(sub_(Iden, -, =, Expr)) --> variable(Iden), [-], [=], simple_expression(Expr).
commandForTernary(sub_(Iden, -, =, Expr, Cmd)) --> variable(Iden), [-], [=], simple_expression(Expr), [;], commandForTernary(Cmd).
commandForTernary(multiply_(Iden, *, =, Expr)) --> variable(Iden), [*], [=], simple_expression(Expr).
commandForTernary(multiply_(Iden, *, =, Expr, Cmd)) --> variable(Iden), [*], [=], simple_expression(Expr), [;], commandForTernary(Cmd).
commandForTernary(divide_(Iden, /, =, Expr)) --> variable(Iden), [/], [=], simple_expression(Expr).
commandForTernary(divide_(Iden, /, =, Expr, Cmd)) --> variable(Iden), [/], [=], simple_expression(Expr), [;], commandForTernary(Cmd).

booleanCondition(boolean_true(true)) --> [true].
booleanCondition(boolean_false(false)) --> [false].
booleanCondition(boolean_equals(E, == , E1)) --> simple_expression(E), ['=', '='], simple_expression(E1).
booleanCondition(boolean_greater(E, > , E1)) --> simple_expression(E), ['>'], simple_expression(E1).
booleanCondition(boolean_less(E, < , E1)) --> simple_expression(E), ['<'], simple_expression(E1).
booleanCondition(boolean_greater_equal(E, >= , E1)) --> simple_expression(E), ['>', '='], simple_expression(E1).
booleanCondition(boolean_less_equal(E, <= , E1)) --> simple_expression(E), ['<', '='], simple_expression(E1).
booleanCondition(boolean_negation(not, B)) --> ['not'], booleanCondition(B).
booleanCondition(boolean_negation(!, B)) --> ['!'], booleanCondition(B).
booleanCondition(boolean_negation('!=', B)) --> ['!='], booleanCondition(B).


simple_expression(E0) --> e1(E1), e0(E1, E0).
simple_expression(parenthesis(E)) --> ['('], simple_expression(E), [')'].
simple_expression(increment(Iden, +, +)) --> variable(Iden), [+], [+].
simple_expression(decrement(Iden, -, -)) --> variable(Iden), [-], [-].

e0(EIn, EOut) --> [+], e1(E1), e0(add(EIn, E1), EOut).
e0(EIn, EOut) --> [-], e1(E1), e0(substract(EIn, E1), EOut).
e0(E, E) --> [].
e1(E1) --> e2(E2), e1_(E2, E1).
e1_(EIn, EOut) --> [*], e2(E2), e1_(multiply(EIn, E2), EOut).
e1_(EIn, EOut) --> [/], e2(E2), e1_(divide(EIn, E2), EOut).
e1_(E, E) --> [].
e2(I) --> variable(I).
e2(N) --> number(N).


print_statement(print_string(Value)) --> ['('], string_value(Value), [')'].
print_statement(print_string(Value)) --> ['('], variable(Value), [')'].
print_statement(print_boolean(true)) --> ['('], [true], [')'].
print_statement(print_boolean(false)) --> ['('], [false], [')'].
print_statement(print_expr(Expression)) --> ['('], simple_expression(Expression), [')'].


string_value(string_(Variable), [Variable | Tail], Tail) :- string(Variable).

variable(variable(Iden)) --> [Iden], {atom(Iden), not(number(Iden)), not(member(Iden, [int, float, bool, string, true, false, for,
    if, elif, else, while, range, and, or, not, in, range, <, >, <=, >=, ==,
    '!=', ++, --, +, -, *, /]))}.
number(number(Num)) --> [Num], { number(Num) }.
string_(string(String)) --> [String], {string(String)}.

assignment(assign(Iden,=,Expr)) --> variable(Iden),[=],simple_expression(Expr).
assignment(assign(Iden,=,Expr)) --> variable(Iden),[=],booleanCondition(Expr).
assignment(assign(Iden,=,Str)) --> variable(Iden),[=],string_value(Str).
arithmetic_assign(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], simple_expression(Expr).
arithmetic_assign(substract_(Iden, -, =, Expr)) --> variable(Iden), [-], [=], simple_expression(Expr).
arithmetic_assign(multiply_(Iden, *, =, Expr)) --> variable(Iden), [*], [=], simple_expression(Expr).
arithmetic_assign(divide_(Iden, /, =, Expr)) --> variable(Iden), [/], [=], simple_expression(Expr).


program_eval(Prog, Env) :-
    program_eval_(Prog, [], Env).

program_eval_(main(CodeBlock), PrevEnv, Env) :-
    block_evaluation(CodeBlock, PrevEnv, Env).

block_evaluation(code_block(SubBlock), PrevEnv, Env):-
    sub_block_evaluation(SubBlock, PrevEnv, Env).

block_evaluation(empty(), _, _).

sub_block_evaluation(sub_block(Dec, Rest), PrevEnv, Env) :- declaration_evaluation(Dec, PrevEnv, TempEnv), sub_block_evaluation(Rest, TempEnv, Env).
% sub_block_evaluation(sub_block(Cmd, Rest), PrevEnv, Env) :- eval_commands(Commands, PrevEnv, MediatorPrevEnv), sub_block_evaluation(Statements, MediatorPrevEnv, Env).
sub_block_evaluation(sub_block(Dec), PrevEnv, Env) :- declaration_evaluation(Dec, PrevEnv, Env).
% sub_block_evaluation(sub_block(Cmd), PrevEnv, Env) :- eval_commands(Commands, PrevEnv, Env).
sub_block_evaluation().

declaration_evaluation(const_declaration(const, Datatype, Iden, =, Val, ;, D), PrevEnv, Env) :-
    is_const_declared(Iden, Datatype, Val, PrevEnv, TempEnv),
    declaration_evaluation(D, TempEnv, Env).

declaration_evaluation(const_declaration(const, Datatype, Iden, '=', Val, ';'), PrevEnv, Env) :-
    is_const_declared(Iden, Datatype, Val, PrevEnv, Env).

declaration_evaluation(var_declaration(var, I, ';', D), PrevEnv, Env) :-
    is_variable_declared(I, PrevEnv, TempEnv),
    declaration_evaluation(D, TempEnv, Env).

declaration_evaluation(var_declaration(var, I, ';'), PrevEnv, Env) :-
    is_variable_declared(I, PrevEnv, Env).


is_const_declared(variable(Iden), datatype(Datatype), Val, PrevEnv, [(Iden, const, Datatype, N) | PrevEnv]) :-
    extract_value(Val, N),
    \+ member((Iden, _, _, _), PrevEnv),
    ( valid_datatype_value(Datatype, N)
    -> true
    ;  format('Error: invalid value "~w" for datatype "~w"~n', [N, Datatype]), fail
    ).

% This predicate checks if a variable identifier is already declared and updates the environment.
is_variable_declared(identifier(I), Env, [(I, _)|Env]) :-
    \+ memberchk((I, _), Env).

% This predicate checks if a variable identifier is already declared and returns the existing environment.
is_variable_declared(identifier(I), Env, Env) :-
    memberchk((I , _), Env).

extract_value(number(N), N).
extract_value(string_(S), S).
extract_value(bool(B), B).

valid_datatype_value(int, Value) :-
    integer(Value).
valid_datatype_value(string, Value) :-
    string(Value).
valid_datatype_value(int, Value) :-
    integer(Value).
valid_datatype_value(bool, Value) :-
    (Value == true ; Value == false).