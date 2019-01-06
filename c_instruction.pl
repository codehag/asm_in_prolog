:- module(c_instruction, [c_instruction/3]).
:- use_module(sym_dict_lookup).

c_instruction(A) --> {
    comp_splitter(A, [], Dest, Comp, Jmp)
  },
  init, comp(Comp), dest(Dest), jmp(Jmp).

dest(A) --> get_register(A).
jmp(A)  --> { atomic_list_concat(A, O) }, get_jump(O).
comp(A) --> has('M', A), { atomic_list_concat(A, O) }, get_comp(O).

% C Instruction Helpers.

comp_splitter([], Bucket, _, C, []) :-
  reverse(Bucket, C).
comp_splitter([';'|Ls], Bucket, _, C, Ls) :-
  reverse(Bucket, C).
comp_splitter(['='|Ls], Bucket, D, C, J) :-
  reverse(Bucket, D),
  comp_splitter(Ls, [], D, C, J).
comp_splitter([A|Ls], Bucket, D, C, J) :-
  not(A = '='),
  not(A = ';'),
  comp_splitter(Ls, [A|Bucket], D, C, J).

get_register(List) --> has('A', List), has('D', List), has('M', List).

has(_, '')   --> [0].
has(X, List) --> [1], { member(X, List) }.
has(X, List) --> [0], { not(member(X, List)) }.

init --> [1, 1, 1].

get_jump('')    --> [0, 0, 0].
get_jump('JGT') --> [0, 0, 1].
get_jump('JEQ') --> [0, 1, 0].
get_jump('JGE') --> [0, 1, 1].
get_jump('JLT') --> [1, 0, 0].
get_jump('JNE') --> [1, 0, 1].
get_jump('JLE') --> [1, 1, 0].
get_jump('JMP') --> [1, 1, 1].

get_comp('')    --> [1, 0, 1, 0, 1, 0].
get_comp('0')   --> [1, 0, 1, 0, 1, 0].
get_comp('1')   --> [1, 1, 1, 1, 1, 1].
get_comp('-1')  --> [1, 1, 1, 0, 1, 0].

get_comp('D')   --> [0, 0, 1, 1, 0, 0].
get_comp('!D')  --> [0, 0, 1, 1, 0, 1].
get_comp('-D')  --> [0, 0, 1, 1, 1, 1].
get_comp('D+1') --> [0, 1, 1, 1, 1, 1].
get_comp('D-1') --> [0, 0, 1, 1, 1, 0].

get_comp('A')   --> [1, 1, 0, 0, 0, 0].
get_comp('M')   --> [1, 1, 0, 0, 0, 0].
get_comp('!A')  --> [1, 1, 0, 0, 0, 1].
get_comp('!M')  --> [1, 1, 0, 0, 0, 1].
get_comp('A+1') --> [1, 1, 0, 1, 1, 1].
get_comp('M+1') --> [1, 1, 0, 1, 1, 1].
get_comp('A-1') --> [1, 1, 0, 0, 1, 0].
get_comp('M-1') --> [1, 1, 0, 0, 1, 0].

get_comp('D+A') --> [0, 0, 0, 0, 1, 0].
get_comp('D+M') --> [0, 0, 0, 0, 1, 0].
get_comp('D-A') --> [0, 1, 0, 0, 1, 1].
get_comp('D-M') --> [0, 1, 0, 0, 1, 1].
get_comp('A-D') --> [0, 0, 0, 1, 1, 1].
get_comp('M-D') --> [0, 0, 0, 1, 1, 1].

get_comp('D&A') --> [0, 0, 0, 0, 0, 0].
get_comp('D&M') --> [0, 0, 0, 0, 0, 0].
get_comp('D|A') --> [0, 1, 0, 1, 0, 1].
get_comp('D|M') --> [0, 1, 0, 1, 0, 1].
