:- use_module(token_reader).
:- use_module(parser).
:- use_module(evaluator).
:- set_prolog_flag(double_quotes, string).

main(Filename) :- nl,
    ansi_format([bold,fg(yellow)], 'Starting Parser', []), nl,
    read_file(Filename, FileData),
    program(ParseTree, FileData, []),
    write("Generating Parse Tree: "), successful_flag, nl,
    write(ParseTree), nl,
    ansi_format([bold,fg(yellow)], 'Starting Evaluation', []), nl,
    program_eval(ParseTree, [], NewEnv), nl,
    ansi_format([bold,fg(yellow)], 'Environment after evaluation', []), nl,
    write(NewEnv), nl,
    halt.

successful_flag :- ansi_format([bold,fg(green)], 'SUCCESSFUL', []).