:- module(sym_dict_lookup, [sym_dict_lookup/2, sym_dict_set_label/1, line_count/0]).
:- dynamic sym_dict/2.
% Invariant definitions.
sym_dict('R0', 0).
sym_dict('R1', 1).
sym_dict('R2', 2).
sym_dict('R3', 3).
sym_dict('R4', 4).
sym_dict('R5', 5).
sym_dict('R6', 6).
sym_dict('R7', 7).
sym_dict('R8', 8).
sym_dict('R9', 9).
sym_dict('R10', 10).
sym_dict('R11', 11).
sym_dict('R12', 12).
sym_dict('R13', 13).
sym_dict('R14', 14).
sym_dict('R15', 15).
sym_dict('SCREEN', 16384).
sym_dict('KBD', 24576).
sym_dict('SP', 0).
sym_dict('LCL', 1).
sym_dict('ARG', 2).
sym_dict('THIS', 3).
sym_dict('THAT', 4).

% Dynamic predicate used to track address allocation.
sym_dict(custom, 16).
%
% Dynamic predicate used to track lines.
sym_dict(line, 0).

sym_dict_lookup(Key, Value) :-
  sym_dict(Key, Value).
sym_dict_lookup(Key, Value) :-
  not(sym_dict(Key, _)),
  sym_dict(custom, Value),
  retract(sym_dict(custom, Value)),
  assert(sym_dict(Key, Value)),
  NewValue is Value + 1,
  assert(sym_dict(custom, NewValue)).

sym_dict_set_label(Key) :-
  not(sym_dict(Key, _)),
  sym_dict(line, CurrentLine),
  assert(sym_dict(Key, CurrentLine)).

line_count :-
  sym_dict(line, OldVal),
  Value is OldVal + 1,
  retract(sym_dict(line, OldVal)),
  assert(sym_dict(line, Value)).
