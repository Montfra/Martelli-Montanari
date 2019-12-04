%% UTIL %%
:- op(20,xfy,?=).
:- style_check(-singleton).

% Prédicats daffichage fournis
% set_echo: ce prédicat active laffichage par le prédicat echo
set_echo :- assert(echo_on).
% clr_echo: ce prédicat inhibe laffichage par le prédicat echo
clr_echo :- retractall(echo_on).
echo(T) :- echo_on, !, write(T).
echo(_).

% for decompose
deleteFirst([H|T], R) :- R = T.

decompose([XE|HE], [XT|HT], [R|H]) :- R = XE ?= XT, decompose(HE, HT, H).
decompose([], [], R) :- R = [].





%%################  EXERCICE 1  ################%%

%% REGLE %%
regle(E ?= T, decompose) :- compound(E), compound(T), functor(E, Name, Arity), functor(T, Name, Arity), !.
regle(E ?= T, rename) :- var(E), var(T), !.
regle(E ?= T, clash):- \+ regle(E ?= T, decompose),!.
regle(E ?= T, orient) :- nonvar(E), var(T), !.
regle(E ?= T, check) :- E \== T, occurCheck(E, T), !.
regle(E ?= T, expand) :- var(E), compound(T), \+ occurCheck(E, T), !.
regle(E ?= T, simplify) :- var(E), atomic(T), !.

occurCheck(E, T) :- var(E), var(T), E==T, !.
occurCheck(E, T) :- nonvar(T), compound(T), arg(I, T, Value), occurCheck(E, Value), !.





%% REDUIT %%
reduit(orient, E ?= T, [E ?= T | R], [T ?= E | R]) :- regle(E ?= T, orient), !, write('ORIENT'), write(E ?= T), write('\n').
reduit(simplify, E ?= T, [E ?= T | R], R) :- regle(E ?= T, simplify), !, E = T, write('SIMPLIFY'), write(E ?= T), write('\n').
reduit(rename, E ?= T, [E ?= T | R], R) :- regle(E ?= T, rename), !, E = T, write('RENAME'), write(E ?= T), write('\n').
reduit(expand, E ?= T, [E ?= T | R], R) :- regle(E ?= T, expand), !, E = T, write('EXPAND'), write(E ?= T), write('\n').
reduit(decompose, E ?= T, [E ?= T | R], I) :- regle(E ?= T, decompose), !, 
  	E=..XE,T=..XT,
    deleteFirst(XE, XXE), deleteFirst(XT, XXT),
    decompose(XXE, XXT, RES),
	append(R, RES, I), write('DECOMPOSE').





%% UNIFICATION %%
unify([]) :- !.
unify([E|T]) :- regle(E, check), write('Unification impossible :( \n'), !.
unify([E|T]) :- \+ regle(E, clash),  write('Unification impossible :( \n'), !.
unify([E|T]) :- write('SYSTEM :'), write([E|T]), reduit(_, E, [E|T], RES), unify(RES), !.
unify([E|T]) :- \+ reduit(_, E, [E|T], RES), unify(T), !.






%%################  EXERCICE 2  ################%%

%% STRATEGY %%
choix_premier([E ?= T|Y], Res, E ?= T, Regle) :- reduit(Regle, E ?= T, [E ?= T|Y], Res).

choix_pondere([E ?= T|Y], Res, E ?= T, _) :- 
    regle(E ?= T, check), write('Unification impossible :( \n'), !;
    \+ regle(E ?= T, clash), write('Unification impossible :( \n'), !;
    reduit(rename, E ?= T, [E ?= T|Y], Res), !;
    reduit(simplify, E ?= T, [E ?= T|Y], Res), !;
    reduit(orient, E ?= T, [E ?= T|Y], Res), !;
    reduit(decompose, E ?= T, [E ?= T|Y], Res), !;
    reduit(expand, E ?= T, [E ?= T|Y], Res), !.

choix_random([E ?= T|Y], Res, E ?= T, _) :- random(0,5,Random), random([E ?= T|Y], Res, E ?= T, Random).

random([E ?= T|Y], Res, E ?= T, 0) :- 
    regle(E ?= T, check), write('Unification impossible :( \n'), !;
    \+ regle(E ?= T, clash), write('Unification impossible :( \n'), !;
    reduit(rename, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 1) :- 
    regle(E ?= T, check), write('Unification impossible :( \n'), !;
    \+ regle(E ?= T, clash), write('Unification impossible :( \n'), !;
    reduit(simplify, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 2) :- 
    regle(E ?= T, check), write('Unification impossible :( \n'), !;
    \+ regle(E ?= T, clash), write('Unification impossible :( \n'), !;
    reduit(orient, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 3) :- 
    regle(E ?= T, check), write('Unification impossible :( \n'), !;
    \+ regle(E ?= T, clash), write('Unification impossible :( \n'), !;
    reduit(decompose, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 4) :- 
    regle(E ?= T, check), write('Unification impossible :( \n'), !;
    \+ regle(E ?= T, clash), write('Unification impossible :( \n'), !;
    reduit(expand, E ?= T, [E ?= T|Y], Res), !.

%% UNIFICATION %%

unify([], choix_premier) :- !.
unify([E|T], choix_premier) :- regle(E, check), write('Unification impossible CHECK :( \n'), write(E), !.
unify([E|T], choix_premier) :- \+ regle(E, clash),  write('Unification impossible CLASH :( \n'), write(E), !.
unify([E|T], choix_premier) :-  write('SYSTEM :'), write([E|T]), choix_premier([E|T], RES, E, Regle), unify(RES, choix_premier), !.
unify([E|T], choix_premier) :- \+ choix_premier([E|T], RES, E, Regle), unify(T, choix_premier), !.

unify([], choix_pondere) :- !.
unify([E|T], choix_pondere) :-  write('SYSTEM :'), write([E|T]), choix_pondere([E|T], RES, E, Regle), unify(RES, choix_pondere), !.
unify([E|T], choix_pondere) :- \+ choix_pondere([E|T], RES, E, Regle), unify(T, choix_pondere), !.


unify([], choix_random) :- !.
unify([E|T], choix_random) :-  write('SYSTEM :'), write([E|T]), choix_random([E|T], RES, E, Regle), unify(RES, choix_random), !.
unify([E|T], choix_random) :- \+ choix_random([E|T], RES, E, Regle), unify(T, choix_random), !.

unify(P,S) :-
  clr_echo, unify(P,S).

trace_unify(P,S) :- 
  set_echo, unify(P,S).



























