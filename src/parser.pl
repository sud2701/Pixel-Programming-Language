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

const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], expression(Expr), [;], declaration(Rest).
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], expression(Expr), [;].
% declare ternary

var_declaration(var_declaration(var, Dtype, Iden, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [;].
var_declaration(var_declaration(var, Dtype, Iden, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [;], declaration(Rest).
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], expression(Expr), [;].
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], expression(Expr), [;], declaration(Rest).

variable_datatype(datatype(int)) --> [int].
variable_datatype(datatype(bool)) --> [bool].
variable_datatype(datatype(str)) --> [str].


leftRecursionRemovedCommand(command_assign(Iden, =, Expr, ;)) --> variable(Iden), [=], expression(Expr), [;].
leftRecursionRemovedCommand(for_loop(for, '(', Dec, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Expr, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], expression(Expr), [;], booleanCondition(Bool), [;], expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(command_block(Blk)) --> block(Blk).

booleanCondition(boolean_true(true)) --> ['true'].
booleanCondition(boolean_false(false)) --> ['false'].
booleanCondition(boolean_equals(E, == , E1)) --> expression(E), ['=', '='], expression(E1).
booleanCondition(boolean_greater(E, > , E1)) --> expression(E), ['>'], expression(E1).
booleanCondition(boolean_less(E, < , E1)) --> expression(E), ['<'], expression(E1).
booleanCondition(boolean_greater_equal(E, >= , E1)) --> expression(E), ['>', '='], expression(E1).
booleanCondition(boolean_less_equal(E, <= , E1)) --> expression(E), ['<', '='], expression(E1). 
booleanCondition(boolean_negation(not, B)) --> ['not'], booleanCondition(B).
booleanCondition(boolean_negation(!, B)) --> ['!'], booleanCondition(B).
booleanCondition(boolean_negation('!=', B)) --> ['!='], booleanCondition(B).


expression(E0) --> e1(E1), e0(E1, E0).
expression(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], expression(Expr).
expression((Iden, =, Expr)) --> variable(Iden), [=], expression(Expr).
expression(parenthesis(E)) --> ['('], expression(E), [')'].
expression(assign(Iden, =, Expr)) --> variable(Iden), [=], expression(Expr).
expression(increment(Iden, +, +)) --> variable(Iden), [+], [+].
expression(decrement(Iden, -, -)) --> variable(Iden), [-], [-].
expression(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], expression(Expr).
expression(substract_(Iden, -, =, Expr)) --> variable(Iden), [-], [=], expression(Expr).
expression(multiply_(Iden, *, =, Expr)) --> variable(Iden), [*], [=], expression(Expr).
expression(divide_(Iden, /, =, Expr)) --> variable(Iden), [/], [=], expression(Expr).
e0(EIn, EOut) --> [+], e1(E1), e0(add(EIn, E1), EOut).
e0(EIn, EOut) --> [-], e1(E1), e0(substract(EIn, E1), EOut).
e0(E, E) --> [].
e1(E1) --> e2(E2), e1_(E2, E1).
e1_(EIn, EOut) --> [*], e2(E2), e1_(multiply(EIn, E2), EOut).
e1_(EIn, EOut) --> [/], e2(E2), e1_(divide(EIn, E2), EOut).
e1_(E, E) --> [].
e2(I) --> variable(I).
e2(N) --> number(N).

variable(variable(Iden)) --> [Iden], {atom(Iden), \+ member(Iden, ['(', ')', '{', '}', =])}.



number(number(Num)) --> [Num], { number(Num) }.