
program(t_program(main,'(',')','{','}')) --> [main],['('],[')'],['{'],['}'].
program(t_program(main,'(',')','{',M,'}')) --> [main],['('],[')'],['{'],main(M),['}'].

main(t_main(D,';',C)) --> declaration(D), [';'], command_seq(C).
main(t_main(C)) --> command_seq(C).

declaration(t_declaration(T)) --> const_declaration(T).
declaration(t_declaration(T)) --> var_declaration(T).
declaration(t_declaration(T1,';',T2)) --> const_declaration(T1),[';'],declaration(T2).
declaration(t_declaration(T1,';',T2)) --> var_declaration(T1),[';'],declaration(T2).

var_declaration(t_declaration(var, T1, T2, '=', T3)) --> [var], datatype(T1), identifier(T2), ['='], value(T3).
const_declaration(t_declaration(const,T1,T2,'=',T3)) --> [const],datatype(T1),identifier(T2),['='],value(T3).

command_seq([T]) --> command(T).
command_seq([T1 | T2]) --> command(T1), command_seq(T2).

% comment(t_comment('/','/',T)) --> ['/'],['/'],string(T).

datatype(t_datatype(int)) --> [int].
datatype(t_datatype(bool)) --> [bool].
datatype(t_datatype(str)) --> [str].

value(t_integer(T)) --> numbers(T).
value(t_boolean(T)) --> boolean(T).
value(t_string(T)) --> string_set(T).
value(t_value(T)) --> expr(T).

string_set(T) --> lowercase(T).
string_set(T) --> uppercase(T).
string_set(T) --> numbers(T).
string_set([T1|T2]) --> lowercase(T1),string_set(T2).
string_set([T1|T2]) --> uppercase(T1),string_set(T2).
string_set([T1|T2]) --> numbers(T1),string_set(T2).

identifier(t_id(T)) --> lowercase(T).
identifier(t_id(T)) --> uppercase(T).
identifier(t_id(T1,T2)) --> lowercase(T1),identifier(T2).
identifier(t_id(T1,T2)) --> uppercase(T1),identifier(T2).


relation(t_rel(T1,'>',T2)) --> expr(T1), ['>'], expr(T2).
relation(t_rel(T1,'<',T2)) --> expr(T1), ['<'], expr(T2).
relation(t_rel(T1,'>=',T2)) --> expr(T1), ['>='], expr(T2).
relation(t_rel(T1,'<=',T2)) --> expr(T1), ['<='], expr(T2).
relation(t_rel(T1,'==',T2)) --> expr(T1), ['=='], expr(T2).

boolean(t_bool(T1,or,T2)) --> simple_boolean(T1),[or],boolean(T2).
boolean(t_bool(T)) --> simple_boolean(T).

simple_boolean(t_bool(T1,and,T2)) --> bool_factor(T1),[and],simple_boolean(T2).
simple_boolean(t_bool(T)) --> bool_factor(T).

bool_factor(t_bool(true)) --> [true].
bool_factor(t_bool(false)) --> [false].
bool_factor(t_bool(T)) --> relation(T).
bool_factor(t_bool('(',T,')')) --> ['('],boolean(T),[')'].

expr(t_add(T1,'+',T2)) --> term(T1),['+'],expr(T2).
expr(t_sub(T1,'-',T2)) --> term(T1),['-'],expr(T2).
expr(t_term(T)) --> term(T).

term(t_mul(T1,'*',T2)) --> factor(T1),['*'],term(T2).
term(t_mul(T1,'/',T2)) --> factor(T1),['/'],term(T2).
term(t_factor(T)) --> factor(T).

factor(t_bracket('(',T,')')) --> ['('],expr(T),[')'].
factor(t_id(T)) --> identifier(T).
factor(t_num(T)) --> numbers(T).

lowercase(t_ll(a)) --> [a].
lowercase(t_ll(b)) --> [b].
lowercase(t_ll(c)) --> [c].
lowercase(t_ll(d)) --> [d].
lowercase(t_ll(e)) --> [e].
lowercase(t_ll(f)) --> [f].
lowercase(t_ll(g)) --> [g].
lowercase(t_ll(h)) --> [h].
lowercase(t_ll(i)) --> [i].
lowercase(t_ll(j)) --> [j].
lowercase(t_ll(k)) --> [k].
lowercase(t_ll(l)) --> [l].
lowercase(t_ll(m)) --> [m].
lowercase(t_ll(n)) --> [n].
lowercase(t_ll(o)) --> [o].
lowercase(t_ll(p)) --> [p].
lowercase(t_ll(q)) --> [q].
lowercase(t_ll(r)) --> [r].
lowercase(t_ll(s)) --> [s].
lowercase(t_ll(t)) --> [t].
lowercase(t_ll(u)) --> [u].
lowercase(t_ll(v)) --> [v].
lowercase(t_ll(w)) --> [w].
lowercase(t_ll(x)) --> [x].
lowercase(t_ll(y)) --> [y].
lowercase(t_ll(z)) --> [z].


uppercase(t_ll(A)) --> [A].
uppercase(t_ll(B)) --> [B].
uppercase(t_ll(C)) --> [C].
uppercase(t_ll(D)) --> [D].
uppercase(t_ll(E)) --> [E].
uppercase(t_ll(F)) --> [F].
uppercase(t_ll(G)) --> [G].
uppercase(t_ll(H)) --> [H].
uppercase(t_ll(I)) --> [I].
uppercase(t_ll(J)) --> [J].
uppercase(t_ll(K)) --> [K].
uppercase(t_ll(L)) --> [L].
uppercase(t_ll(M)) --> [M].
uppercase(t_ll(N)) --> [N].
uppercase(t_ll(O)) --> [O].
uppercase(t_ll(P)) --> [P].
uppercase(t_ll(Q)) --> [Q].
uppercase(t_ll(R)) --> [R].
uppercase(t_ll(S)) --> [S].
uppercase(t_ll(T)) --> [T].
uppercase(t_ll(U)) --> [U].
uppercase(t_ll(V)) --> [V].
uppercase(t_ll(W)) --> [W].
uppercase(t_ll(X)) --> [X].
uppercase(t_ll(Y)) --> [Y].
uppercase(t_ll(Z)) --> [Z].


numbers(t_num(0)) --> [0].
numbers(t_num(1)) --> [1].
numbers(t_num(2)) --> [2].
numbers(t_num(3)) --> [3].
numbers(t_num(4)) --> [4].
numbers(t_num(5)) --> [5].
numbers(t_num(6)) --> [6].
numbers(t_num(7)) --> [7].
numbers(t_num(8)) --> [8].
numbers(t_num(9)) --> [9].


command(t_command(T)) --> print_statement(T).
command(t_command(T)) --> assignment(T).
command(t_command(T)) --> ternary_operation(T).

command(t_command(if,'(',B,')','{',C1,'}')) --> [if],['('],boolean(B),[')'],['{'],command_seq(C1),['}'].
command(t_command(if,'(',B,')','{',C1,'}',else,'{',C2,'}')) --> [if],['('],boolean(B),[')'],['{'],command_seq(C1),['}'],[else],['{'],command_seq(C2),['}'].

command(t_command(if,'(',B,')','{',C1,'}',C2,else,'{',C3,'}')) --> [if],['('],boolean(B),[')'],['{'],command_seq(C1),['}'],elseif(C2),[else],['{'],command_seq(C3),['}'].
command(t_command(if,'(',B,')','{',C1,'}',C2)) --> [if],['('],boolean(B),[')'],['{'],command_seq(C1),['}'],elseif(C2).

command(t_command(for,'(',D,';',B,';',A,')','{',C,'}')) --> [for],['('],declaration(D),[';'],boolean(B),[';'],assignment(A),[')'],['{'],command_seq(C),['}'].

command(t_command(for,'(',A1,';',B,';',A2,')','{',C,'}')) --> [for],['('],assignment(A1),[';'],boolean(B),[';'],assignment(A2),[')'],['{'],command_seq(C),['}'].

command(t_command(for,I,in,range,'(',P1,',',P2,',',P3,')','{',C,'}')) --> [for],identifier(I),[in],[range],['('],numbers(P1),[','],numbers(P2),[','],numbers(P3),[')'],['{'],command_seq(C),['}'].

command(t_command(for,I,in,range,'(',P,')','{',C,'}')) --> [for],identifier(I),[in],[range],['('],numbers(P),[')'],['{'],command_seq(C),['}'].

command(t_command(while,'(',B,')','{',C,'}')) --> [while],['('],boolean(B),[')'],['{'],command_seq(C),['}'].

elseif(t_elseif(elseif,'(',B,')','{',C,'}')) --> [elseif],['('],boolean(B),[')'],['{'],command_seq(C),['}'].
elseif(t_elseif(elseif,'(',B,')','{',C,'}',E)) --> [elseif],['('],boolean(B),[')'],['{'],command_seq(C),['}'],elseif(E).


print_statement(t_print(print,'(',T,')',';')) --> [print],['('],value(T),[')'],[';'].
print_statement(t_print(print,'(',T,')',';')) --> [print],['('],expr(T),[')'],[';'].

assignment(t_assignment(T1,'=',T2,';')) --> identifier(T1),['='],expr(T2),[';'].

ternary_operation(t_ternary(B,'?',C1,':',C2,';')) --> boolean(B),['?'],command(C1),[':'],command(C2),[';'].
