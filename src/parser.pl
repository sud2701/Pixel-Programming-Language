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
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Expr), [;].
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;], declaration(Rest).
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Expr), [;], declaration(Rest).


var_declaration(var_declaration(var, Dtype, Iden, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [;].
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;].
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Expr), [;].
var_declaration(var_declaration(var, Dtype, Iden, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [;], declaration(Rest).
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;], declaration(Rest).
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Expr), [;], declaration(Rest).

variable_datatype(datatype(int)) --> [int].
variable_datatype(datatype(bool)) --> [bool].
variable_datatype(datatype(str)) --> [str].


leftRecursionRemovedCommand(command_assign(Iden, =, Expr, ;)) --> variable(Iden), [=], simple_expression(Expr), [;].
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
leftRecursionRemovedCommand(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left, variable(Iden), [=], booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right, [;].
leftRecursionRemovedCommand(while_loop(Boolean, Cmd)) --> [while], ['('], booleanCondition(Boolean), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(print(Arg)) --> [print], print_statement(Arg), [;].
leftRecursionRemovedCommand(increment(Iden, +, +)) --> variable(Iden), [+], [+], [;].
leftRecursionRemovedCommand(decrement(Iden, -, -)) --> variable(Iden), [-], [-], [;].
leftRecursionRemovedCommand(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], simple_expression(Expr), [;].
leftRecursionRemovedCommand(sub_(Iden, -, =, Expr)) --> variable(Iden), [-], [=], simple_expression(Expr), [;].
leftRecursionRemovedCommand(sub_(Iden, *, =, Expr)) --> variable(Iden), [*], [=], simple_expression(Expr), [;].
leftRecursionRemovedCommand(sub_(Iden, /, =, Expr)) --> variable(Iden), [/], [=], simple_expression(Expr), [;].
leftRecursionRemovedCommand(command_block(Blk)) --> block(Blk).

ternary_expression(Expr) --> simple_expression(Expr).
ternary_expression(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left(), variable(Iden), [=], booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right().
optional_parenthesis_left() --> ['('].
optional_parenthesis_left() --> [].

optional_parenthesis_right() --> [')'].
optional_parenthesis_right() --> [].

else_if_ladder(elif(elif, Bool, Cmd)) --> [else, if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd).
else_if_ladder(elif(elif, Bool, Cmd, Rest)) --> [else, if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), else_if_ladder(Rest).
else_if_ladder(elif(elif, Bool, Cmd, else, Cmd1)) --> [else, if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), [else], leftRecursionRemovedCommand(Cmd1).

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

assignment(assign(Iden,=,Expr)) --> variable(Iden),[=],simple_expression(Expr).
arithmetic_assign(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], simple_expression(Expr).
arithmetic_assign(substract_(Iden, -, =, Expr)) --> variable(Iden), [-], [=], simple_expression(Expr).
arithmetic_assign(multiply_(Iden, *, =, Expr)) --> variable(Iden), [*], [=], simple_expression(Expr).
arithmetic_assign(divide_(Iden, /, =, Expr)) --> variable(Iden), [/], [=], simple_expression(Expr).
