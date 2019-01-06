:- module(a_instruction, [look_up/2, num_to_bin/2, a_instruction/3]).
:- use_module(sym_dict_lookup).
:- use_module(library(clpfd)).

% A Instruction Helpers.
a_instruction(['@'|Ls]) --> [0], addr(Ls).

addr([A|Ls]) --> { char_type(A, alpha) }, { look_up([A|Ls], Dec), num_to_bin(Dec, Out) }, Out.
addr([A|Ls]) --> { char_type(A, digit) }, { list_to_bin([A|Ls], Out) }, Out.

look_up(Ls, Out) :-
  atomic_list_concat(Ls, Str),
  sym_dict_lookup(Str, Out).

list_to_bin(Ls, Out) :-
  atomic_list_concat(Ls, Atom),
  atom_number(Atom, Num),
  num_to_bin(Num, Out).

num_to_bin(Num, Out) :-
  binary_number(Bin, Num),
  length(Bin, L),
  Padding is 15 - L,
  left_pad(Padding, Bin, Out).

left_pad(0, Target, Target).
left_pad(Length, Target, Out) :-
  New_length is Length - 1,
  left_pad(New_length, [0|Target], Out).

% Binary translation
binary_number(Bits, N) :-
    binary_number_min(Bits, 0, N, N).

binary_number_min([], N, N, _M).
binary_number_min([Bit|Bits], N0, N, M) :-
    Bit in 0..1,
    N1 #= N0*2 + Bit,
    M #>= N1,
    binary_number_min(Bits, N1, N, M).

