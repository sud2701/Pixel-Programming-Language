:- table main_block/2.
program_eval(Prog, Env) :-
    program_eval_(Prog, [], Env).

program(main(CodeBlock)) --> [main], block(CodeBlock).

program_eval_(main(CodeBlock), PrevEnv, Env) :-
    block_evaluation(CodeBlock, PrevEnv, Env).


block(empty()) --> ['{'], ['}'].
block(code_block(CodeBlock)) --> ['{'], sub_block(CodeBlock),['}'].

block_evaluation(code_block(SubBlock), PrevEnv, Env):-
    sub_block_evaluation(SubBlock, PrevEnv, Env).

block_evaluation(empty(), PrevEnv, PrevEnv).

sub_block(sub_block(Dec)) --> declaration(Dec).
sub_block(sub_block(Dec, Rest)) --> declaration(Dec), sub_block(Rest).
sub_block(sub_block(Cmd, Rest)) --> leftRecursionRemovedCommand(Cmd), sub_block(Rest).
sub_block(sub_block(Cmd)) --> leftRecursionRemovedCommand(Cmd).
sub_block([]) --> [].

sub_block_evaluation(sub_block(Dec, Rest), PrevEnv, Env) :- declaration_evaluation(Dec, PrevEnv, TempEnv), sub_block_evaluation(Rest, TempEnv, Env).
sub_block_evaluation(sub_block(Cmd, Rest), PrevEnv, Env) :- command_evaluation(Cmd, PrevEnv, TempEnv), sub_block_evaluation(Rest, TempEnv, Env).
sub_block_evaluation(sub_block(Dec), PrevEnv, Env) :- declaration_evaluation(Dec, PrevEnv, Env).
sub_block_evaluation(sub_block(Cmd), PrevEnv, Env) :- command_evaluation(Cmd, PrevEnv, Env).
sub_block_evaluation([], Env, Env).

declaration(Dec) --> const_declaration(Dec).
declaration(Dec) --> var_declaration(Dec).

declaration_evaluation(var_declaration(var, Datatype, Iden, ';', Rest), PrevEnv, Env) :-
    is_variable_declared(Iden, Datatype, PrevEnv, TempEnv),
    declaration_evaluation(Rest, TempEnv, Env).

declaration_evaluation(var_declaration(var, Datatype, Iden, ';'), PrevEnv, Env) :-
    is_variable_declared(Iden, Datatype, PrevEnv, Env).

declaration_evaluation(var_declaration(var, Datatype, Iden, =, Val, ';', Rest), PrevEnv, Env) :-
    is_variable_declared(Iden, Datatype, Val, PrevEnv, TempEnv),
    declaration_evaluation(Rest, TempEnv, Env).

declaration_evaluation(var_declaration(var, Datatype, Iden, =, Val, ';'), PrevEnv, Env) :-
    is_variable_declared(Iden, Datatype, Val, PrevEnv, Env).

declaration_evaluation(const_declaration(const, Datatype, Iden, =, Val, ;, Rest), PrevEnv, Env) :-
    is_const_declared(Iden, Datatype, Val, PrevEnv, TempEnv),
    declaration_evaluation(Rest, TempEnv, Env).

declaration_evaluation(const_declaration(const, Datatype, Iden, =, Val, ';'), PrevEnv, Env) :-
    is_const_declared(Iden, Datatype, Val, PrevEnv, Env).

const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;].
const_declaration(const_declaration(const, Dtype, Iden, =, Bool, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Bool), [;].
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], ternary_expression(Expr), [;].
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;], declaration(Rest).
const_declaration(const_declaration(const, Dtype, Iden, =, Bool, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Bool), [;], declaration(Rest).
const_declaration(const_declaration(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], ternary_expression(Expr), [;], declaration(Rest).

is_const_declared(variable(Iden), datatype(Datatype), Val, PrevEnv, [(Iden, const, Datatype, N) | TempEnv]) :-
    evaluate_extract(Val, PrevEnv, TempEnv, N),
    \+ member((Iden, _, _, _), TempEnv),
    ( valid_datatype_value(Datatype, N)
    -> true
    ;  format('Error: invalid value "~w" for datatype "~w"~n', [N, Datatype]), fail
    ).

evaluate_extract(Val, PrevEnv, Env, N) :-
    extract_value(Val, PrevEnv, TempEnv, N),
    (var(TempEnv) -> Env = PrevEnv; Env = TempEnv).

% is_const_declared(variable(Iden), datatype(Datatype), Val, PrevEnv, [(Iden, const, Datatype, N) | PrevEnv]) :-
%     extract_value(Val, PrevEnv, _, N),
%     \+ member((Iden, _, _, _), PrevEnv),
%     ( valid_datatype_value(Datatype, N)
%     -> true
%     ;  format('Error: invalid value "~w" for datatype "~w"~n', [N, Datatype]), fail
%     ).

var_declaration(var_declaration(var, Dtype, Iden, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [;].
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;].
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Expr), [;].
var_declaration(var_declaration(var, Dtype, Iden, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [;], declaration(Rest).
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expression(Expr), [;], declaration(Rest).
var_declaration(var_declaration(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], booleanCondition(Expr), [;], declaration(Rest).

is_variable_declared(variable(Iden), datatype(Datatype), PrevEnv, [(Iden, var, Datatype, _) | PrevEnv]) :-
    (\+ member((Iden, _, _, _), PrevEnv); throw(error(variable_already_declared, [Iden]))).

is_variable_declared(variable(Iden), datatype(Datatype), Val, PrevEnv, [(Iden, var, Datatype, N) | TempEnv]) :-
    evaluate_extract(Val, PrevEnv, TempEnv, N),
    (\+ member((Iden, _, _, _), TempEnv)
    -> ( valid_datatype_value(Datatype, N)
    -> true ;  format('Error: invalid value "~w" for datatype "~w"~n', [N, Datatype]), fail)
    ; throw(error(variable_already_declared, [Iden]))).

% is_variable_declared(variable(Iden), datatype(Datatype), Val, PrevEnv, [(Iden, var, Datatype, N) | Env]) :-
%     extract_value(Val, PrevEnv, Env, N),
%     (\+ member((Iden, _, _, _), Env)
%     -> ( valid_datatype_value(Datatype, N)
%     -> true ;  format('Error: invalid value "~w" for datatype "~w"~n', [N, Datatype]), fail)
%     ; throw(error(variable_already_declared, [Iden]))).

variable_datatype(datatype(int)) --> [int].
variable_datatype(datatype(bool)) --> [bool].
variable_datatype(datatype(str)) --> [str].

valid_datatype_value(int, Value) :-
    integer(Value).
valid_datatype_value(str, Value) :-
    string(Value).
valid_datatype_value(bool, Value) :-
    (Value == true ; Value == false).

extract_value(number(N), _, _, N).
extract_value(Bool, PrevEnv, Env, B) :- boolean_evaluation(Bool, PrevEnv, Env, B).
extract_value(Expr, PrevEnv, Env, Num) :- eval_expression(Expr, PrevEnv, Env, Num).
extract_value(Expr, PrevEnv, Env, Num) :- eval_ternary_expression(Expr, PrevEnv, Env, Num).


leftRecursionRemovedCommand(command_assign(T)) --> assignment(T), [;].
leftRecursionRemovedCommand(command_assign(T,Cmd)) --> assignment(T), [;], leftRecursionRemovedCommand(Cmd).

leftRecursionRemovedCommand(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd).

leftRecursionRemovedCommand(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd), leftRecursionRemovedCommand(Cmd1).
leftRecursionRemovedCommand(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd), leftRecursionRemovedCommand(Cmd1).
leftRecursionRemovedCommand(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd), leftRecursionRemovedCommand(Cmd1).
leftRecursionRemovedCommand(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd), leftRecursionRemovedCommand(Cmd1).

leftRecursionRemovedCommand(for_loop_range(Iden, in, range, From, To, Jump, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(From), [','], simple_expression(To), [','], simple_expression(Jump), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(for_loop_range_single(Iden, in, range, To, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(To), [')'], leftRecursionRemovedCommand(Cmd).

leftRecursionRemovedCommand(for_loop_range(Iden, in, range, From, To, Jump, Cmd, Cmd1)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(From), [','], simple_expression(To), [','], simple_expression(Jump), [')'], leftRecursionRemovedCommand(Cmd), leftRecursionRemovedCommand(Cmd1).
leftRecursionRemovedCommand(for_loop_range_single(Iden, in, range, To, Cmd, Cmd1)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(To), [')'], leftRecursionRemovedCommand(Cmd), leftRecursionRemovedCommand(Cmd1).

leftRecursionRemovedCommand(if_else(if, Bool, Cmd, else, Cmd1)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), [else], leftRecursionRemovedCommand(Cmd1).
leftRecursionRemovedCommand(if_else_if(if, Bool, Cmd, Rest)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd),  else_if_ladder(Rest).
leftRecursionRemovedCommand(if(if, Bool, Cmd)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(if_else(if, Bool, Cmd, else, Cmd1, Cmd2)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), [else], leftRecursionRemovedCommand(Cmd1), leftRecursionRemovedCommand(Cmd2).
leftRecursionRemovedCommand(if_else_if(if, Bool, Cmd, Rest, Cmd1)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd),  else_if_ladder(Rest), leftRecursionRemovedCommand(Cmd1).
leftRecursionRemovedCommand(if(if, Bool, Cmd, Cmd1)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), leftRecursionRemovedCommand(Cmd1).

leftRecursionRemovedCommand(ternary_operator(Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_parenthesis_left, booleanCondition(Bool), ['?'], commandForTernary(Expr1), [:], commandForTernary(Expr2), optional_parenthesis_right, [;], leftRecursionRemovedCommand(Cmd).

leftRecursionRemovedCommand(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left, booleanCondition(Bool), ['?'], commandForTernary(Expr1), [:], commandForTernary(Expr2), optional_parenthesis_right, [;].

leftRecursionRemovedCommand(while_loop(Boolean, Cmd)) --> [while], ['('], booleanCondition(Boolean), [')'], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(while_loop(Boolean, Cmd, Cmd1)) --> [while], ['('], booleanCondition(Boolean), [')'], leftRecursionRemovedCommand(Cmd), leftRecursionRemovedCommand(Cmd1).

leftRecursionRemovedCommand(print(Arg)) --> [print], print_statement(Arg), [;].
leftRecursionRemovedCommand(print(Arg,Cmd)) --> [print], print_statement(Arg), [;],leftRecursionRemovedCommand(Cmd).

leftRecursionRemovedCommand(increment(Iden, +, +)) --> variable(Iden), [+], [+], [;].
leftRecursionRemovedCommand(increment(Iden, +, +, Cmd)) --> variable(Iden), [+], [+], [;], leftRecursionRemovedCommand(Cmd).
leftRecursionRemovedCommand(decrement(Iden, -, -)) --> variable(Iden), [-], [-], [;].
leftRecursionRemovedCommand(decrement(Iden, -, -, Cmd)) --> variable(Iden), [-], [-], [;], leftRecursionRemovedCommand(Cmd).


% leftRecursionRemovedCommand(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], simple_expression(Expr), [;].
% leftRecursionRemovedCommand(add_(Iden, +, =, Expr, Cmd)) --> variable(Iden), [+], [=], simple_expression(Expr), [;], leftRecursionRemovedCommand(Cmd).

% leftRecursionRemovedCommand(sub_(Iden, -, =, Expr)) --> variable(Iden), [-], [=], simple_expression(Expr), [;].
% leftRecursionRemovedCommand(sub_(Iden, -, =, Expr, Cmd)) --> variable(Iden), [-], [=], simple_expression(Expr), [;], leftRecursionRemovedCommand(Cmd).

% leftRecursionRemovedCommand(multiply_(Iden, *, =, Expr)) --> variable(Iden), [*], [=], simple_expression(Expr), [;].
% leftRecursionRemovedCommand(multiply_(Iden, *, =, Expr, Cmd)) --> variable(Iden), [*], [=], simple_expression(Expr), [;], leftRecursionRemovedCommand(Cmd).

% leftRecursionRemovedCommand(divide_(Iden, /, =, Expr)) --> variable(Iden), [/], [=], simple_expression(Expr), [;].
% leftRecursionRemovedCommand(divide_(Iden, /, =, Expr, Cmd)) --> variable(Iden), [/], [=], simple_expression(Expr), [;], leftRecursionRemovedCommand(Cmd).

leftRecursionRemovedCommand(command_block(Blk)) --> block(Blk).

update_variables([], PrevEnv, PrevEnv).
update_variables([(Var, Type, Datatype, Val)|Rest], PrevEnv, UpdatedEnv) :-
    (select((Var, Type, Datatype, _), PrevEnv, _) ->
        replace(PrevEnv, (Var, Type, Datatype, _), (Var, Type, Datatype, Val), NewTempEnv)
    ;
        NewTempEnv = PrevEnv
    ),
    update_variables(Rest, NewTempEnv, UpdatedEnv).

replace_variable((Var, Type, Datatype, Val), [], [(Var, Type, Datatype, Val)]).
replace_variable((Var, Type, Datatype, Val), [(Var, Type, Datatype, _)|Rest], [(Var, Type, Datatype, Val)|Rest]).
replace_variable((Var, Type, Datatype, Val), [Head|Rest], [Head|NewRest]) :-
    Head \= (Var, Type, Datatype, _),
    replace_variable((Var, Type, Datatype, Val), Rest, NewRest).


evaluate_command_env(Cmd, PrevEnv, Env) :-
    command_evaluation(Cmd, PrevEnv, TempEnv),
    (var(TempEnv) -> Env = PrevEnv; Env = TempEnv).

command_evaluation(command_assign(T), PrevEnv, Env) :-
    assignment_evalutation(T, PrevEnv, Env).

command_evaluation(command_assign(T, Cmd), PrevEnv, Env) :-
    assignment_evalutation(T, PrevEnv, TempEnv),
    command_evaluation(Cmd, TempEnv, Env).

command_evaluation(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd), PrevEnv, Env):-
    declaration_evaluation(Dec, PrevEnv, TempEnv),
    for_loop_evaluation(Bool, Expr, Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_evaluation(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd, Cmd1), PrevEnv, Env):-
    declaration_evaluation(Dec, PrevEnv, TempEnv),
    for_loop_evaluation(Bool, Expr, Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_evaluation(Cmd1, TempTempTempEnv, Env).

command_evaluation(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd), PrevEnv, Env):-
    assignment_evalutation(Asg, PrevEnv, TempEnv),
    for_loop_evaluation(Bool, Expr, Cmd, TempEnv, TempTempTempEnv),
    update_variables(TempTempTempEnv, PrevEnv, Env).

command_evaluation(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1), PrevEnv, Env):-
    assignment_evalutation(Asg, PrevEnv, TempEnv),
    for_loop_evaluation(Bool, Expr, Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_evaluation(Cmd1, TempTempTempEnv, Env).

command_evaluation(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd), PrevEnv, Env) :-
    \+ member((Iden, var, int, _), PrevEnv),
    for_loop_range_evaluation(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | PrevEnv], TempEnv),
    update_variables(TempEnv, PrevEnv, Env).

command_evaluation(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd), PrevEnv, Env) :-
    member((Iden, var, int, _), PrevEnv),
    select((Iden, var, int, _), PrevEnv, TempEnv),
    for_loop_range_evaluation(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | TempEnv], TempTempEnv),
    update_variables(TempTempEnv, [(Iden, var, int, From) | TempEnv], Env).

command_evaluation(for_loop_range(Iden, _, _, _, _, _, _), PrevEnv, _) :-
    member((Iden, var, Type, _), PrevEnv),
    Type \= int,
    writeln('Error: Iden should be of type int').

command_evaluation(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd, Cmd1), PrevEnv, Env) :-
    \+ member((Iden, var, int, _), PrevEnv),
    for_loop_range_evaluation(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | PrevEnv], TempEnv),
    update_variables(TempEnv, PrevEnv, TempTempEnv),
    command_evaluation(Cmd1, TempTempEnv, Env).

command_evaluation(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd, Cmd1), PrevEnv, Env) :-
    member((Iden, var, int, _), PrevEnv),
    select((Iden, var, int, _), PrevEnv, TempEnv),
    for_loop_range_evaluation(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | TempEnv], TempTempEnv),
    update_variables(TempTempEnv, [(Iden, var, int, From) | TempEnv], TempTempTempEnv),
    command_evaluation(Cmd1, TempTempTempEnv, Env).

command_evaluation(for_loop_range(Iden, _, _, _, _, _, _, _), PrevEnv, _) :-
    member((Iden, var, Type, _), PrevEnv),
    Type \= int,
    writeln('Error: Iden should be of type int').


command_evaluation(for_loop_range_single(variable(Iden), in, range, number(To), Cmd), PrevEnv, Env) :-
    \+ member((Iden, var, int, _), PrevEnv),
    for_loop_range_evaluation(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | PrevEnv], TempEnv),
    update_variables(TempEnv, PrevEnv, Env).

command_evaluation(for_loop_range_single(variable(Iden), in, range, number(To), Cmd), PrevEnv, Env) :-
    member((Iden, var, int, _), PrevEnv),
    select((Iden, var, int, _), PrevEnv, TempEnv),
    for_loop_range_evaluation(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | TempEnv], TempTempEnv),
    update_variables(TempTempEnv, [(Iden, var, int, 0) | TempEnv], Env).

command_evaluation(for_loop_range_single(Iden, _, _, _, _, _), PrevEnv, _) :-
    member((Iden, var, Type, _), PrevEnv),
    Type \= int,
    writeln('Error: Iden should be of type int').

command_evaluation(for_loop_range_single(variable(Iden), in, range, number(To), Cmd, Cmd1), PrevEnv, Env) :-
    \+ member((Iden, var, int, _), PrevEnv),
    for_loop_range_evaluation(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | PrevEnv], TempEnv),
    update_variables(TempEnv, PrevEnv, TempTempEnv),
    command_evaluation(Cmd1, TempTempEnv, Env).

command_evaluation(for_loop_range_single(variable(Iden), in, range, number(To), Cmd, Cmd1), PrevEnv, Env) :-
    member((Iden, var, int, _), PrevEnv),
    select((Iden, var, int, _), PrevEnv, TempEnv),
    for_loop_range_evaluation(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | TempEnv], TempTempEnv),
    update_variables(TempTempEnv, [(Iden, var, int, 0) | TempEnv], TempTempTempEnv),
    command_evaluation(Cmd1, TempTempTempEnv, Env).

command_evaluation(for_loop_range_single(Iden, _, _, _, _, _, _), PrevEnv, _) :-
    member((Iden, var, Type, _), PrevEnv),
    Type \= int,
    writeln('Error: Iden should be of type int').


% command_evaluation(ternary_operator(variable(Iden), Bool, '?', Expr1, :, Expr2, Cmd), PrevEnv, Env) :-
%     member((Iden, var, _, _), PrevEnv),
%     boolean_evaluation(Bool, PrevEnv, Env, BoolResult),
%     (BoolResult = true -> eval_ternary_expression(Expr1, PrevEnv, Env, Result)
%     ; eval_ternary_expression(Expr2, PrevEnv, Env, Result)).

command_evaluation(if(if, Bool, Cmd), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_evaluation(if(if, Bool, _), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, Env, false).

command_evaluation(if(if, Bool, Cmd, Cmd1), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_evaluation(Cmd1, TempTempTempEnv, Env).

command_evaluation(if(if, Bool, _, Cmd1), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    command_evaluation(Cmd1, TempEnv, Env).

command_evaluation(if_else(if, Bool, Cmd, else, _), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_evaluation(if_else(if, Bool, _, else, Cmd1),PrevEnv,Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    command_evaluation(Cmd1, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_evaluation(if_else(if, Bool, Cmd, else, _, Cmd1), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_evaluation(Cmd1, TempTempTempEnv, Env).

command_evaluation(if_else(if, Bool, _, else, Cmd1, Cmd2),PrevEnv,Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    command_evaluation(Cmd1, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_evaluation(Cmd2, TempTempTempEnv, Env).

command_evaluation(if_else_if(if, Bool, Cmd, _), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_evaluation(if_else_if(if, Bool, _, Rest), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    else_if_evaluation(Rest, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_evaluation(if_else_if(if, Bool, Cmd, _, Cmd), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_evaluation(Cmd, TempTempTempEnv, Env).

command_evaluation(if_else_if(if, Bool, _, Rest, Cmd), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    else_if_evaluation(Rest, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_evaluation(Cmd, TempTempTempEnv, Env).

command_evaluation(while_loop(Bool, Cmd), PrevEnv, Env):-
    while_evaluaion(Bool, Cmd, PrevEnv, TempEnv),
    update_variables(TempEnv, PrevEnv, Env).

command_evaluation(while_loop(Bool, Cmd, Cmd1), PrevEnv, Env):-
    while_evaluaion(Bool, Cmd, PrevEnv, TempEnv),
    update_variables(TempEnv, PrevEnv, TempTempEnv),
    command_evaluation(Cmd1, TempTempEnv, Env).

command_evaluation(print(Print), PrevEnv, Env):-
    eval_print_cmd_(Print, PrevEnv, Env).

command_evaluation(print(Print, Cmd), PrevEnv, Env):-
    eval_print_cmd_(Print, PrevEnv, TempEnv),
    command_evaluation(Cmd, TempEnv, Env).

command_evaluation(increment(Iden, +, +), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value),
    Increment is Value + 1,
    update_environment(Iden, Increment, TempEnv, Env).

command_evaluation(increment(Iden, +, +, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value),
    Increment is Value + 1,
    update_environment(Iden, Increment, TempEnv, TempTempEnv),
    command_evaluation(Cmd, TempTempEnv, Env).

command_evaluation(decrement(Iden, -, -), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value),
    Decrement is Value - 1,
    update_environment(Iden, Decrement, TempEnv, Env).

command_evaluation(decrement(Iden, -, -, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value),
    Decrement is Value - 1,
    update_environment(Iden, Decrement, TempEnv, TempTempEnv),
    command_evaluation(Cmd, TempTempEnv, Env).

command_evaluation(add_(Iden, +, =, Expr), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 + Value2,
    update_environment(Iden, Result, TempEnv, Env).

command_evaluation(add_(Iden, +, =, Expr, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 + Value2,
    update_environment(Iden, Result, TempEnv, TempTempEnv),
    command_evaluation(Cmd, TempTempEnv, Env).

command_evaluation(sub_(Iden, -, =, Expr), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 - Value2,
    update_environment(Iden, Result, TempEnv, Env).

command_evaluation(sub_(Iden, -, =, Expr, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 - Value2,
    update_environment(Iden, Result, TempEnv, TempTempEnv),
    command_evaluation(Cmd, TempTempEnv, Env).

command_evaluation(multiply_(Iden, *, =, Expr), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 * Value2,
    update_environment(Iden, Result, TempEnv, Env).

command_evaluation(multiply_(Iden, *, =, Expr, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 * Value2,
    update_environment(Iden, Result, TempEnv, TempTempEnv),
    command_evaluation(Cmd, TempTempEnv, Env).

command_evaluation(divide_(Iden, /, =, Expr), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 / Value2,
    update_environment(Iden, Result, TempEnv, Env).

command_evaluation(divide_(Iden, /, =, Expr, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 / Value2,
    update_environment(Iden, Result, TempEnv, TempTempEnv),
    command_evaluation(Cmd, TempTempEnv, Env).

command_evaluation(ternary_operator(Bool, '?', Expr1, :, Expr2), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, BoolResult),
    (BoolResult = true -> command_ternary_evaluation(Expr1, TempEnv, Env)
    ; command_ternary_evaluation(Expr2, TempEnv, Env)).

command_evaluation(ternary_operator(Bool, '?', Expr1, :, Expr2, Cmd), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, BoolResult),
    (BoolResult = true -> command_ternary_evaluation(Expr1, TempEnv, TempTempEnv)
    ; command_ternary_evaluation(Expr2, TempEnv, TempTempEnv)),
    command_evaluation(Cmd, TempTempEnv, Env).

command_evaluation(command_block(Block), PrevEnv, Env) :-
    block_evaluation(Block, PrevEnv, Env).

eval_print_cmd_(Print, PrevEnv, Env) :-
    eval_print_cmd(Print, PrevEnv, TempEnv),
    (var(TempEnv) -> Env = PrevEnv; Env = TempEnv).

eval_print_cmd(print_string(P), PrevEnv, PrevEnv) :-
    write(P).

eval_print_cmd(print_boolean(Bool), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, Env, Result),
    write(Result).

eval_print_cmd(print_expr(Expr), PrevEnv, Env) :-
    evaluate_expr_env(Expr, PrevEnv, Env, Result),
    write(Result).

while_evaluaion(Bool, Cmd, PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_evaluation(Cmd, TempEnv, TempTempEnv),
    while_evaluaion(Bool, Cmd, TempTempEnv, Env).

while_evaluaion(B, _, PrevEnv, Env) :-
    evaluate_boolean_env(B, PrevEnv, Env, false).

else_if_evaluation(elif(elif, Bool, Cmd), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_evaluation(Cmd, TempEnv, Env).

else_if_evaluation(elif(elif, Bool, _), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, Env, false).

else_if_evaluation(elif(elif, Bool, Cmd, _), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool,PrevEnv,TempEnv,true),
    command_evaluation(Cmd, TempEnv, Env).

else_if_evaluation(elif(elif, Bool, _, Rest), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv,false),
    else_if_evaluation(Rest, TempEnv, Env).

else_if_evaluation(elif(elif, Bool, Cmd, else, _),PrevEnv,Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv,true),
    command_evaluation(Cmd, TempEnv, Env).

else_if_evaluation(elif(elif, Bool, _, else, Cmd1), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv,false),
    command_evaluation(Cmd1, TempEnv, Env).
% for_loop_range_evaluation(Iden, Current, To, Jump, Cmd, PrevEnv, Env) :-
%     Current < To,
%     command_evaluation(Cmd, PrevEnv, PrevEnv),
%     NewCurrent is Current + Jump,
%     update_iden_value(Iden, NewCurrent, PrevEnv, UpdatedEnv),
%     for_loop_range_evaluation(Iden, NewCurrent, To, Jump, Cmd, UpdatedEnv, Env).

for_loop_range_evaluation(Iden, Current, To, Jump, Cmd, PrevEnv, Env) :-
    Current < To,
    evaluate_command_env(Cmd, PrevEnv, TempEnv),
    NewCurrent is Current + Jump,
    update_iden_value(Iden, NewCurrent, TempEnv, UpdatedEnv),
    update_variables(UpdatedEnv, PrevEnv, TempTempTempTempEnv),
    for_loop_range_evaluation(Iden, NewCurrent, To, Jump, Cmd, TempTempTempTempEnv, Env).

for_loop_range_evaluation(_, Current, To, _, _, Env, Env) :-
    Current >= To.

update_iden_value(Iden, Value, Env, [(Iden, var, int, Value) | TempEnv]) :-
    select((Iden, var, int, _), Env, TempEnv).

for_loop_evaluation(Bool, Expr, Cmd, PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, BoolValue),
    (   BoolValue == true
    ->  evaluate_command_env(Cmd, TempEnv, TempTempEnv),
        (   is_assignment(Expr)
        ->  assignment_evalutation(Expr, TempTempEnv, TempTempTempEnv)
        ;
        evaluate_extract(Expr, TempTempEnv, TempTempTempEnv, _)
        ),
        update_variables(TempTempTempEnv, PrevEnv, TempTempTempTempEnv),
        for_loop_evaluation(Bool, Expr, Cmd, TempTempTempTempEnv, Env)
    ;   Env = PrevEnv
    ).

is_assignment(assign(variable(_), =, _)).


% for_loop_evaluation(Bool, _, _, PrevEnv, PrevEnv) :-
%     evaluate_boolean_env(Bool, PrevEnv, PrevEnv, false).


ternary_expression(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left(), booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right().
ternary_expression(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left(), variable(Iden), [=], booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right().
ternary_expression(Expr) --> simple_expression(Expr).
ternary_expression(Bool) --> booleanCondition(Bool).
optional_parenthesis_left() --> ['('].
optional_parenthesis_left() --> [].

optional_parenthesis_right() --> [')'].
optional_parenthesis_right() --> [].

eval_ternary_expression(Expr, PrevEnv, Env, Result) :-
    evaluate_expr_env(Expr, PrevEnv, Env, Result).

eval_ternary_expression(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2), PrevEnv, Env, Result) :-
    evaluate_expr_env(Iden, PrevEnv, TempEnv, _),
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, BoolResult),
    (BoolResult = true -> eval_ternary_expression(Expr1, TempEnv, Env, Result)
    ; eval_ternary_expression(Expr2, TempEnv, Env, Result)).

eval_ternary_expression(ternary_operator(Bool, '?', Expr1, :, Expr2), PrevEnv, Env, Result) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, BoolResult),
    (BoolResult = true -> eval_ternary_expression(Expr1, TempEnv, Env, Result)
    ; eval_ternary_expression(Expr2, TempEnv, Env, Result)).

else_if_ladder(elif(elif, Bool, Cmd)) --> [else, if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd).
else_if_ladder(elif(elif, Bool, Cmd, Rest)) --> [else, if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), else_if_ladder(Rest).
else_if_ladder(elif(elif, Bool, Cmd, else, Cmd1)) --> [else, if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), [else], leftRecursionRemovedCommand(Cmd1).

commandForTernary(command_assign(T)) --> assignment(T).
commandForTernary(command_assign(T,Cmd)) --> assignment(T), [;], commandForTernary(Cmd).

commandForTernary(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd).

commandForTernary(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd), commandForTernary(Cmd1).
commandForTernary(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], declaration(Dec), booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd), commandForTernary(Cmd1).
commandForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], simple_expression(Expr), [')'], leftRecursionRemovedCommand(Cmd), commandForTernary(Cmd1).
commandForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], assignment(Asg), [;], booleanCondition(Bool), [;], assignment(Expr), [')'], leftRecursionRemovedCommand(Cmd), commandForTernary(Cmd1).

commandForTernary(for_loop_range(Iden, in, range, From, To, Jump, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(From), [','], simple_expression(To), [','], simple_expression(Jump), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(for_loop_range_single(Iden, in, range, To, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(To), [')'], leftRecursionRemovedCommand(Cmd).

commandForTernary(for_loop_range(Iden, in, range, From, To, Jump, Cmd, Cmd1)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(From), [','], simple_expression(To), [','], simple_expression(Jump), [')'], leftRecursionRemovedCommand(Cmd), commandForTernary(Cmd1).
commandForTernary(for_loop_range_single(Iden, in, range, To, Cmd, Cmd1)) --> [for], variable(Iden), [in], [range], ['('], simple_expression(To), [')'], leftRecursionRemovedCommand(Cmd), commandForTernary(Cmd1).

commandForTernary(if_else(if, Bool, Cmd, else, Cmd1)) --> [if], ['('], booleanCondition(Bool), [')'], commandForTernary(Cmd), [else], leftRecursionRemovedCommand(Cmd1).
commandForTernary(if_else_if(if, Bool, Cmd, Rest)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd),  else_if_ladder(Rest).
commandForTernary(if(if, Bool, Cmd)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd).

commandForTernary(if_else(if, Bool, Cmd, else, Cmd1, Cmd2)) --> [if], ['('], booleanCondition(Bool), [')'], commandForTernary(Cmd), [else], leftRecursionRemovedCommand(Cmd1), commandForTernary(Cmd2).
commandForTernary(if_else_if(if, Bool, Cmd, Rest, Cmd1)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd),  else_if_ladder(Rest), commandForTernary(Cmd1).
commandForTernary(if(if, Bool, Cmd, Cmd1)) --> [if], ['('], booleanCondition(Bool), [')'], leftRecursionRemovedCommand(Cmd), commandForTernary(Cmd1).

commandForTernary(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_parenthesis_left, variable(Iden), [=], booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right, [;], commandForTernary(Cmd).
commandForTernary(ternary_operator(Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_parenthesis_left, booleanCondition(Bool), ['?'], leftRecursionRemovedCommand(Expr1), [:], leftRecursionRemovedCommand(Expr2), optional_parenthesis_right, commandForTernary(Cmd).
commandForTernary(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left, variable(Iden), [=], booleanCondition(Bool), ['?'], ternary_expression(Expr1), [:], ternary_expression(Expr2), optional_parenthesis_right.
commandForTernary(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_parenthesis_left, booleanCondition(Bool), ['?'], commandForTernary(Expr1), [:], commandForTernary(Expr2), optional_parenthesis_right.


commandForTernary(while_loop(Boolean, Cmd)) --> [while], ['('], booleanCondition(Boolean), [')'], leftRecursionRemovedCommand(Cmd).
commandForTernary(while_loop(Boolean, Cmd, Cmd1)) --> [while], ['('], booleanCondition(Boolean), [')'], leftRecursionRemovedCommand(Cmd), commandForTernary(Cmd1).

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

evaluate_command_for_ternary_env(Cmd, PrevEnv, Env) :-
    command_ternary_evaluation(Cmd, PrevEnv, TempEnv),
    (var(TempEnv) -> Env = PrevEnv; Env = TempEnv).

command_ternary_evaluation(command_assign(T), PrevEnv, Env) :-
    assignment_evalutation(T, PrevEnv, Env).

command_ternary_evaluation(command_assign(T, Cmd), PrevEnv, Env) :-
    assignment_evalutation(T, PrevEnv, TempEnv),
    command_ternary_evaluation(Cmd, TempEnv, Env).

command_ternary_evaluation(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd), PrevEnv, Env):-
    declaration_evaluation(Dec, PrevEnv, TempEnv),
    for_loop_evaluation(Bool, Expr, Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_ternary_evaluation(for_loop(for, '(', Dec, Bool, ;, Expr, ')', Cmd, Cmd1), PrevEnv, Env):-
    declaration_evaluation(Dec, PrevEnv, TempEnv),
    for_loop_evaluation(Bool, Expr, Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_ternary_evaluation(Cmd1, TempTempTempEnv, Env).

command_ternary_evaluation(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd), PrevEnv, Env):-
    assignment_evalutation(Asg, PrevEnv, TempEnv),
    for_loop_evaluation(Bool, Expr, Cmd, TempEnv, TempTempTempEnv),
    update_variables(TempTempTempEnv, PrevEnv, Env).

command_ternary_evaluation(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1), PrevEnv, Env):-
    assignment_evalutation(Asg, PrevEnv, TempEnv),
    for_loop_evaluation(Bool, Expr, Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_ternary_evaluation(Cmd1, TempTempTempEnv, Env).

command_ternary_evaluation(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd), PrevEnv, Env) :-
    \+ member((Iden, var, int, _), PrevEnv),
    for_loop_range_evaluation(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | PrevEnv], TempEnv),
    update_variables(TempEnv, PrevEnv, Env).

command_ternary_evaluation(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd), PrevEnv, Env) :-
    member((Iden, var, int, _), PrevEnv),
    select((Iden, var, int, _), PrevEnv, TempEnv),
    for_loop_range_evaluation(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | TempEnv], TempTempEnv),
    update_variables(TempTempEnv, [(Iden, var, int, From) | TempEnv], Env).

command_ternary_evaluation(for_loop_range(Iden, _, _, _, _, _, _), PrevEnv, _) :-
    member((Iden, var, Type, _), PrevEnv),
    Type \= int,
    writeln('Error: Iden should be of type int').

command_ternary_evaluation(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd, Cmd1), PrevEnv, Env) :-
    \+ member((Iden, var, int, _), PrevEnv),
    for_loop_range_evaluation(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | PrevEnv], TempEnv),
    update_variables(TempEnv, PrevEnv, TempTempEnv),
    command_ternary_evaluation(Cmd1, TempTempEnv, Env).

command_ternary_evaluation(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd, Cmd1), PrevEnv, Env) :-
    member((Iden, var, int, _), PrevEnv),
    select((Iden, var, int, _), PrevEnv, TempEnv),
    for_loop_range_evaluation(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | TempEnv], TempTempEnv),
    update_variables(TempTempEnv, [(Iden, var, int, From) | TempEnv], TempTempTempEnv),
    command_ternary_evaluation(Cmd1, TempTempTempEnv, Env).

command_ternary_evaluation(for_loop_range(Iden, _, _, _, _, _, _, _), PrevEnv, _) :-
    member((Iden, var, Type, _), PrevEnv),
    Type \= int,
    writeln('Error: Iden should be of type int').


command_ternary_evaluation(for_loop_range_single(variable(Iden), in, range, number(To), Cmd), PrevEnv, Env) :-
    \+ member((Iden, var, int, _), PrevEnv),
    for_loop_range_evaluation(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | PrevEnv], TempEnv),
    update_variables(TempEnv, PrevEnv, Env).

command_ternary_evaluation(for_loop_range_single(variable(Iden), in, range, number(To), Cmd), PrevEnv, Env) :-
    member((Iden, var, int, _), PrevEnv),
    select((Iden, var, int, _), PrevEnv, TempEnv),
    for_loop_range_evaluation(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | TempEnv], TempTempEnv),
    update_variables(TempTempEnv, [(Iden, var, int, 0) | TempEnv], Env).

command_ternary_evaluation(for_loop_range_single(Iden, _, _, _, _, _), PrevEnv, _) :-
    member((Iden, var, Type, _), PrevEnv),
    Type \= int,
    writeln('Error: Iden should be of type int').

command_ternary_evaluation(for_loop_range_single(variable(Iden), in, range, number(To), Cmd, Cmd1), PrevEnv, Env) :-
    \+ member((Iden, var, int, _), PrevEnv),
    for_loop_range_evaluation(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | PrevEnv], TempEnv),
    update_variables(TempEnv, PrevEnv, TempTempEnv),
    command_ternary_evaluation(Cmd1, TempTempEnv, Env).

command_ternary_evaluation(for_loop_range_single(variable(Iden), in, range, number(To), Cmd, Cmd1), PrevEnv, Env) :-
    member((Iden, var, int, _), PrevEnv),
    select((Iden, var, int, _), PrevEnv, TempEnv),
    for_loop_range_evaluation(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | TempEnv], TempTempEnv),
    update_variables(TempTempEnv, [(Iden, var, int, 0) | TempEnv], TempTempTempEnv),
    command_ternary_evaluation(Cmd1, TempTempTempEnv, Env).

command_ternary_evaluation(for_loop_range_single(Iden, _, _, _, _, _, _), PrevEnv, _) :-
    member((Iden, var, Type, _), PrevEnv),
    Type \= int,
    writeln('Error: Iden should be of type int').


% command_ternary_evaluation(ternary_operator(variable(Iden), Bool, '?', Expr1, :, Expr2, Cmd), PrevEnv, Env) :-
%     member((Iden, var, _, _), PrevEnv),
%     boolean_evaluation(Bool, PrevEnv, Env, BoolResult),
%     (BoolResult = true -> eval_ternary_expression(Expr1, PrevEnv, Env, Result)
%     ; eval_ternary_expression(Expr2, PrevEnv, Env, Result)).

command_ternary_evaluation(if(if, Bool, Cmd), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_ternary_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_ternary_evaluation(if(if, Bool, _), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, Env, false).

command_ternary_evaluation(if(if, Bool, Cmd, Cmd1), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_ternary_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_ternary_evaluation(Cmd1, TempTempTempEnv, Env).

command_ternary_evaluation(if(if, Bool, _, Cmd1), PrevEnv, Env) :-
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    command_ternary_evaluation(Cmd1, TempEnv, Env).

command_ternary_evaluation(if_else(if, Bool, Cmd, else, _), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_ternary_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_ternary_evaluation(if_else(if, Bool, _, else, Cmd1),PrevEnv,Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    command_ternary_evaluation(Cmd1, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_ternary_evaluation(if_else(if, Bool, Cmd, else, _, Cmd1), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_ternary_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_ternary_evaluation(Cmd1, TempTempTempEnv, Env).

command_ternary_evaluation(if_else(if, Bool, _, else, Cmd1, Cmd2),PrevEnv,Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    command_ternary_evaluation(Cmd1, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_ternary_evaluation(Cmd2, TempTempTempEnv, Env).

command_ternary_evaluation(if_else_if(if, Bool, Cmd, _), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_ternary_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_ternary_evaluation(if_else_if(if, Bool, _, Rest), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    else_if_evaluation(Rest, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, Env).

command_ternary_evaluation(if_else_if(if, Bool, Cmd, _, Cmd), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, true),
    command_ternary_evaluation(Cmd, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_ternary_evaluation(Cmd, TempTempTempEnv, Env).

command_ternary_evaluation(if_else_if(if, Bool, _, Rest, Cmd), PrevEnv, Env) :- 
    evaluate_boolean_env(Bool, PrevEnv, TempEnv, false),
    else_if_evaluation(Rest, TempEnv, TempTempEnv),
    update_variables(TempTempEnv, PrevEnv, TempTempTempEnv),
    command_ternary_evaluation(Cmd, TempTempTempEnv, Env).

command_ternary_evaluation(while_loop(Bool, Cmd), PrevEnv, Env):-
    while_evaluaion(Bool, Cmd, PrevEnv, TempEnv),
    update_variables(TempEnv, PrevEnv, Env).

command_ternary_evaluation(while_loop(Bool, Cmd, Cmd1), PrevEnv, Env):-
    while_evaluaion(Bool, Cmd, PrevEnv, TempEnv),
    update_variables(TempEnv, PrevEnv, TempTempEnv),
    command_ternary_evaluation(Cmd1, TempTempEnv, Env).

command_ternary_evaluation(print(Print), PrevEnv, Env):-
    eval_print_cmd_(Print, PrevEnv, Env).

command_ternary_evaluation(print(Print, Cmd), PrevEnv, Env):-
    eval_print_cmd_(Print, PrevEnv, TempEnv),
    command_ternary_evaluation(Cmd, TempEnv, Env).

command_ternary_evaluation(increment(Iden, +, +), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value),
    Increment is Value + 1,
    update_environment(Iden, Increment, TempEnv, Env).

command_ternary_evaluation(increment(Iden, +, +, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value),
    Increment is Value + 1,
    update_environment(Iden, Increment, TempEnv, TempTempEnv),
    command_ternary_evaluation(Cmd, TempTempEnv, Env).

command_ternary_evaluation(decrement(Iden, -, -), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value),
    Decrement is Value - 1,
    update_environment(Iden, Decrement, TempEnv, Env).

command_ternary_evaluation(decrement(Iden, -, -, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value),
    Decrement is Value - 1,
    update_environment(Iden, Decrement, TempEnv, TempTempEnv),
    command_ternary_evaluation(Cmd, TempTempEnv, Env).

command_ternary_evaluation(add_(Iden, +, =, Expr), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 + Value2,
    update_environment(Iden, Result, TempEnv, Env).

command_ternary_evaluation(add_(Iden, +, =, Expr, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 + Value2,
    update_environment(Iden, Result, TempEnv, TempTempEnv),
    command_ternary_evaluation(Cmd, TempTempEnv, Env).

command_ternary_evaluation(sub_(Iden, -, =, Expr), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 - Value2,
    update_environment(Iden, Result, TempEnv, Env).

command_ternary_evaluation(sub_(Iden, -, =, Expr, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 - Value2,
    update_environment(Iden, Result, TempEnv, TempTempEnv),
    command_ternary_evaluation(Cmd, TempTempEnv, Env).

command_ternary_evaluation(multiply_(Iden, *, =, Expr), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 * Value2,
    update_environment(Iden, Result, TempEnv, Env).

command_ternary_evaluation(multiply_(Iden, *, =, Expr, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 * Value2,
    update_environment(Iden, Result, TempEnv, TempTempEnv),
    command_ternary_evaluation(Cmd, TempTempEnv, Env).

command_ternary_evaluation(divide_(Iden, /, =, Expr), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 / Value2,
    update_environment(Iden, Result, TempEnv, Env).

command_ternary_evaluation(divide_(Iden, /, =, Expr, Cmd), PrevEnv, Env) :-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Iden, PrevEnv, TempEnv, Value1),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Value2),
    Result is Value1 / Value2,
    update_environment(Iden, Result, TempEnv, TempTempEnv),
    command_ternary_evaluation(Cmd, TempTempEnv, Env).

booleanCondition(boolean_true(true)) --> [true].
booleanCondition(boolean_false(false)) --> [false].
booleanCondition(boolean_equals(E, == , E1)) --> simple_expression(E), ['=', '='], simple_expression(E1).
booleanCondition(boolean_greater(E, > , E1)) --> simple_expression(E), ['>'], simple_expression(E1).
booleanCondition(boolean_less(E, < , E1)) --> simple_expression(E), ['<'], simple_expression(E1).
booleanCondition(boolean_greater_equal(E, >= , E1)) --> simple_expression(E), ['>', '='], simple_expression(E1).
booleanCondition(boolean_less_equal(E, <= , E1)) --> simple_expression(E), ['<', '='], simple_expression(E1).
booleanCondition(boolean_negation(not, B)) --> ['not'], booleanCondition(B).
booleanCondition(boolean_negation(!, B)) --> ['!'], booleanCondition(B).
booleanCondition(boolean_not_equal(E, '!=', E1)) --> simple_expression(E), ['!', '='], simple_expression(E1).

evaluate_boolean_env(Bool, PrevEnv, Env, Result) :-
    boolean_evaluation(Bool, PrevEnv, TempEnv, Result),
    (var(TempEnv) -> Env = PrevEnv; Env = TempEnv).

evaluate_expr_env(Expr, PrevEnv, Env, Result) :-
    eval_expression(Expr, PrevEnv, TempEnv, Result),
    (var(TempEnv) -> Env = PrevEnv; Env = TempEnv).

boolean_evaluation(boolean_true(true), _, _, true).
boolean_evaluation(boolean_false(false), _, _, false).
boolean_evaluation(boolean_equals(E, == ,E1), PrevEnv, Env, Result) :-
    evaluate_expr_env(E, PrevEnv, TempEnv, Val),
    evaluate_expr_env(E1, TempEnv, Env, Val1),
    compare_value(Val, Val1, Result).
boolean_evaluation(boolean_greater(E, > ,E1), PrevEnv, Env, Result) :-
    evaluate_expr_env(E, PrevEnv, TempEnv, Val),
    evaluate_expr_env(E1, TempEnv, Env, Val1),
    (Val > Val1 -> Result = true
    ; Result = false).
boolean_evaluation(boolean_less(E, < ,E1), PrevEnv, Env, Result) :-
    evaluate_expr_env(E, PrevEnv, TempEnv, Val),
    evaluate_expr_env(E1, TempEnv, Env, Val1),
    (Val < Val1 -> Result = true
    ; Result = false).
boolean_evaluation(boolean_greater_equal(E, >= ,E1), PrevEnv, Env, Result) :-
    evaluate_expr_env(E, PrevEnv, TempEnv, Val),
    evaluate_expr_env(E1, TempEnv, Env, Val1),
    (Val >= Val1 -> Result = true
    ; Result = false).
boolean_evaluation(boolean_less_equal(E, <= ,E1), PrevEnv, Env, Result) :-
    evaluate_expr_env(E, PrevEnv, TempEnv, Val),
    evaluate_expr_env(E1, TempEnv, Env, Val1),
    (Val =< Val1 -> Result = true
    ; Result = false).
boolean_evaluation(boolean_negation(not, B), PrevEnv, Env, Result) :-
    evaluate_boolean_env(B, PrevEnv, Env, TempResult),
    negate(TempResult, Result).
boolean_evaluation(boolean_negation(!, B), PrevEnv, Env, Result) :-
    evaluate_boolean_env(B, PrevEnv, Env, TempResult),
    negate(TempResult, Result).
boolean_evaluation(boolean_not_equal(E, '!=' ,E1), PrevEnv, Env, Result) :-
    evaluate_expr_env(E, PrevEnv, TempEnv, Val),
    evaluate_expr_env(E1, TempEnv, Env, Val1),
    (\+ Val = Val1 -> Result = true
    ; Result = false).

compare_value(X, X, true).
compare_value(X, Y, false) :- \+ X = Y.
negate(true, false).
negate(false, true).

simple_expression(Str) --> string_(Str).
simple_expression(E0) --> e1(E1), e0(E1, E0).
simple_expression(increment(Iden, +, +)) --> variable(Iden), [+], [+].
simple_expression(decrement(Iden, -, -)) --> variable(Iden), [-], [-].

e0(EIn, EOut) --> [+], e1(E1), e0(add(EIn, E1), EOut).
e0(EIn, EOut) --> [-], e1(E1), e0(substract(EIn, E1), EOut).
e0(E, E) --> [].
e1(E1) --> e2(E2), e1_(E2, E1).
e1_(EIn, EOut) --> [*], e2(E2), e1_(multiply(EIn, E2), EOut).
e1_(EIn, EOut) --> [/], e2(E2), e1_(divide(EIn, E2), EOut).
e1_(E, E) --> [].
e2(parenthesis(E)) --> ['('], simple_expression(E), [')'].
e2(I) --> variable(I).
e2(N) --> number(N).

check_same_datatype(Val1, Val2) :-
    ((number(Val1), number(Val2));
    (string(Val1), string(Val2))).

get_value(I, PrevEnv, Val) :-
    member((I, _, _, Val), PrevEnv).

eval_expression(number(Num), _, _, Num).
eval_expression(string_(Str), _, _, Str).
eval_expression(variable(Iden), PrevEnv, _, Value) :- 
    (get_value(Iden, PrevEnv, Value) -> true; throw(error(undeclared_variable(Iden)))).

eval_expression(add(E1, E2), PrevEnv, Env, Result) :-
    evaluate_expr_env(E1, PrevEnv, TempEnv, R1),
    evaluate_expr_env(E2, TempEnv, Env, R2),
    (check_same_datatype(R1, R2) ->
        Result is R1 + R2
    ; Result = throw(error(type_mismatch))).

eval_expression(substract(E1, E2), PrevEnv, Env, Result) :-
    evaluate_expr_env(E1, PrevEnv, TempEnv, R1),
    evaluate_expr_env(E2, TempEnv, Env, R2),
    (check_same_datatype(R1, R2) -> 
        Result is R1 - R2
    ; Result = throw(error(type_mismatch))).

eval_expression(multiply(E1, E2), PrevEnv, Env, Result) :-
    evaluate_expr_env(E1, PrevEnv, TempEnv, R1),
    evaluate_expr_env(E2, TempEnv, Env, R2),
    (check_same_datatype(R1, R2) -> 
        Result is R1 * R2
    ; Result = throw(error(type_mismatch))).

eval_expression(divide(E1, E2), PrevEnv, Env, Result) :-
    evaluate_expr_env(E1, PrevEnv, TempEnv, R1),
    evaluate_expr_env(E2, TempEnv, Env, R2),
    (check_same_datatype(R1, R2) -> 
        (R2 =:= 0 -> throw(error(divide_by_zero)); Result is R1 / R2)
    ; Result = throw(error(type_mismatch))).

eval_expression(parenthesis(E), PrevEnv, Env, Result) :-
    eval_expression(E, PrevEnv, Env, Result).

eval_expression(increment(Iden, +, +), PrevEnv, Env, Result) :-
    eval_expression(Iden, PrevEnv, Env, Value),
    Result is Value + 1,
    update_environment(Iden, Result, PrevEnv, Env),
    asserta(updated_environment(Env)).

eval_expression(decrement(Iden, -, -), PrevEnv, Env, Result) :-
    eval_expression(Iden, PrevEnv, Env, Value),
    Result is Value - 1,
    update_environment(Iden, Result, PrevEnv, Env),
    asserta(updated_environment(Env)).


update_environment(variable(Iden), Val, PrevEnv, Env) :-
    replace(PrevEnv, (Iden, Dec, Dtype, _), (Iden, Dec, Dtype, Val), Env).

replace([H|T], H, New, [New|T]).
replace([H|T], Old, New, [H|NewT]) :-
    replace(T, Old, New, NewT).

print_statement(print_string(Value)) --> ['('], string_(Value), [')'].
print_statement(print_boolean(Bool)) --> ['('], booleanCondition(Bool), [')'].
print_statement(print_expr(Expression)) --> ['('], simple_expression(Expression), [')'].


string_value(string_(Variable), [Variable | Tail], Tail) :- string(Variable).

variable(variable(Iden)) --> [Iden], {atom(Iden), not(number(Iden)), not(member(Iden, [int, float, bool, string, true, false, for,
    if, elif, else, while, range, and, or, not, in, range, <, >, <=, >=, ==,
    '!=', ++, --, +, -, *, /]))}.
number(number(Num)) --> [Num], { number(Num) }.
string_(string_(String)) --> [String], {string(String)}.

assignment(assign(Iden,=,Expr)) --> variable(Iden),[=],ternary_expression(Expr).
assignment(assign(Iden,=,Bool)) --> variable(Iden),[=],booleanCondition(Bool).
assignment(assign(Iden,=,Str)) --> variable(Iden),[=],string_value(Str).
assignment(assign(Iden, +, =, Expr)) --> variable(Iden), [+], [=], ternary_expression(Expr).
assignment(assign(Iden, -, =, Expr)) --> variable(Iden), [-], [=], ternary_expression(Expr).
assignment(assign(Iden, *, =, Expr)) --> variable(Iden), [*], [=], ternary_expression(Expr).
assignment(assign(Iden, /, =, Expr)) --> variable(Iden), [/], [=], ternary_expression(Expr).

assignment_evalutation(assign(Iden, =, Expr), PrevEnv, Env):-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_extract(Expr, PrevEnv, TempEnv, Val),
    update_environment(Iden, Val, TempEnv, Env).

assignment_evalutation(assign(Iden, +, =, Expr), PrevEnv, Env):-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Val),
    update_environment(Iden, Val, TempEnv, Env).

assignment_evalutation(assign(Iden, -, =, Expr), PrevEnv, Env):-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Val),
    update_environment(Iden, Val, TempEnv, Env).

assignment_evalutation(assign(Iden, *, =, Expr), PrevEnv, Env):-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Val),
    update_environment(Iden, Val, TempEnv, Env).

assignment_evalutation(assign(Iden, /, =, Expr), PrevEnv, Env):-
    is_member_of_program_var_int(Iden, PrevEnv),
    evaluate_expr_env(Expr, PrevEnv, TempEnv, Val),
    update_environment(Iden, Val, TempEnv, Env).

is_member_of_program(variable(Iden), Env):-
    memberchk((Iden, _, _, _), Env).

is_member_of_program_var_int(variable(Iden), Env):-
    memberchk((Iden, var, _, _), Env).