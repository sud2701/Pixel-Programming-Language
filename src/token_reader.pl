:- module(read_file, [read_file/2]).
:- set_prolog_flag(double_quotes, string).


read_file(FileName, ConvertedData) :-
    open(FileName, read, Stream),
    read_stream(Stream, FileData),
    convert(FileData, ConvertedData), !,
    close(Stream).
read_stream(Stream, [CurrentLineCharacters | List]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_codes(CurrentLineCharacters, Codes),
    read_stream(Stream, List), !.

read_stream(Stream, []) :- at_end_of_stream(Stream).

convert([H|T], [N|R]) :- atom_number(H, N), convert(T, R).
convert([H|T], [H|R]) :- atom(H), convert(T, R).
convert([], []).