:- use_module(library(clpfd)).
:- use_module(sym_dict_lookup).
:- use_module(a_instruction).
:- use_module(c_instruction).

expr(A) --> a_instruction(A) | c_instruction(A).

main(Argv) :-
    string_concat(Argv, '.asm', Input),
    string_concat(Argv, '.hack', Output),
    open(Input, read, Str),
    read_file(Str, Lines),
    close(Str),
    clean_input(Lines, Cleaned),
    translate(Cleaned, Out),
    prettify(Out, PrettyOut),
    atomic_list_concat(PrettyOut, '\n', Final),
    open(Output, write, File),
    write(File, Final),
    close(File).

prettify([], []).
prettify([Line|Lines], [Out|Outs]) :-
  atomic_list_concat(Line, '', Out),
  prettify(Lines, Outs).


read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream,Tmp),
    string_chars(Tmp, Line),
    read_file(Stream, Lines).

collect_until(_, [], Seen, Seen, []).
collect_until(N, Rest, [], [], Rest) :-
  not(member(N, Rest)).
collect_until(N, [N | Rest], Seen, Seen, Rest).
collect_until(N, [C | TempList], Seen, Left, Right) :-
  member(N, TempList),
  append(Seen, [C], NewList),
  collect_until(N, TempList, NewList, Left, Right).

create_label(Cs) :-
  collect_until(')', Cs, [], NCs, _),
  atomic_list_concat(NCs, Name),
  sym_dict_set_label(Name).

clean_line([Char|Cs], Acc, Out) :-
  char_type(Char, space),
  clean_line(Cs, Acc, Out).

clean_line([Char|_], Acc, Out) :-
  Char = '/',
  clean_line([], Acc, Out).

clean_line([Char|Cs], Acc, Out) :-
  Char = '(',
  create_label(Cs),
  clean_line([], Acc, Out).

clean_line([Char|Cs], Acc, Out) :-
  not(Char = '/'),
  not(char_type(Char, space)),
  clean_line(Cs, [Char|Acc], Out).

clean_line([], End, REnd) :-
  reverse(End, REnd).

clean_line([], [], []).

clean_input([], []).

clean_input([Line|Lines], Outs) :-
  clean_line(Line, [], Out),
  [] = Out,
  clean_input(Lines, Outs).

clean_input([Line|Lines], [Out|Outs]) :-
  clean_line(Line, [], Out),
  not([] = Out),
  line_count,
  clean_input(Lines, Outs).

translate([], []).
translate([Line|Lines], [Out|Outs]) :-
  phrase(expr(Line), Out),
  translate(Lines, Outs).
