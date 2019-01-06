:- use_module(library(clpfd)).
:- use_module(sym_dict_lookup).
:- use_module(a_instruction).
:- use_module(c_instruction).

expr(A) --> a_instruction(A) | c_instruction(A).


test1 :-
  phrase(c_instruction(["D", "=", "M", "-", "D"]), A),
  print(A).
test2 :-
  phrase(c_instruction(["D", ";", "J", "M", "P"]), C),
  print(C).
test3 :-
  phrase(c_instruction(["D", "=", "M", "-", "D", ";", "J", "M", "P"]), A),
  print(A).
