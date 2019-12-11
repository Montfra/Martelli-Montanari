% UTIL %
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

decompose([XE|HE], [XT|HT], [R|H]) :- R = XE ?= XT, decompose(HE, HT, H).
decompose([], [], R) :- R = [].

arguments(E, [A|B]) :- argument(E, 1, [A|B]), !.
argument(E, I, [A|B]) :- functor(E, _, Arity), I =< Arity, arg(I, E, Res), A = Res, succ(I, S), argument(E, S, B), !.
argument(E, I, B) :- functor(E, _, Arity), I > Arity, B = [], !.


%################  EXERCICE 1  ################%

% REGLE %
regle(E ?= T, decompose) :- compound(E), compound(T), functor(E, Name, Arity), functor(T, Name, Arity), !.
regle(E ?= T, rename) :- var(E), var(T), !.
regle(E ?= T, clash):- \+ regle(E ?= T, decompose),!.
regle(E ?= T, orient) :- nonvar(E), var(T), !.
regle(E ?= T, check) :- E \== T, occurCheck(E, T), !.
regle(E ?= T, expand) :- var(E), compound(T), \+ occurCheck(E, T), !.
regle(E ?= T, simplify) :- var(E), atomic(T), !.
regle(E ?= T, delete) :- E == T.

occurCheck(E, T) :- var(E), var(T), E==T, !.
occurCheck(E, T) :- nonvar(T), compound(T), arg(I, T, Value), occurCheck(E, Value), !.





% REDUIT %
reduit(delete, E ?= T, [E ?= T | R], R) :- regle(E ?= T, delete), echo('DELETE '), echo(E ?= T), echo('\n').
reduit(orient, E ?= T, [E ?= T | R], [T ?= E | R]) :- regle(E ?= T, orient), !, echo('ORIENT '), echo(E ?= T), echo('\n').
reduit(simplify, E ?= T, [E ?= T | R], R) :- regle(E ?= T, simplify), !, E = T, echo('SIMPLIFY '), echo(E ?= T), echo('\n').
reduit(rename, E ?= T, [E ?= T | R], R) :- regle(E ?= T, rename), !, E = T, echo('RENAME '), echo(E ?= T), echo('\n').
reduit(expand, E ?= T, [E ?= T | R], R) :- regle(E ?= T, expand), !, E = T, echo('EXPAND '), echo(E ?= T), echo('\n').
reduit(decompose, E ?= T, [E ?= T | R], I) :- regle(E ?= T, decompose), !, 
    arguments(E, XE),
    arguments(T, XT),
    decompose(XE, XT, RES),
	append(R, RES, I), echo('DECOMPOSE '), echo(E ?= T), echo('\n').

reduit(check, _, _, _) :- fail, !.
reduit(clash, _, _, _) :- fail, !.




% UNIFICATION %
unify([]) :- !.
unify([E|T]) :- clr_echo, echo('SYSTEM :'), echo([E|T]), echo('\n'), reduit(_, E, [E|T], RES), unify(RES), !.






%################  EXERCICE 2  ################%

% STRATEGY %
choix_premier([E ?= T|Y], Res, E ?= T, Regle) :- reduit(Regle, E ?= T, [E ?= T|Y], Res).

choix_pondere([E ?= T|Y], Res, E ?= T, _) :- ponderation([E ?= T|Y], Res, E ?= T, 1), !.

ponderation([E ?= T|Y], Res, E ?= T, 1) :- 
reduit(check, E ?= T, [E ?= T|Y], Res), !;
ponderation([E ?= T|Y], Res, E ?= T, 2).

ponderation([E ?= T|Y], Res, E ?= T, 1) :- 
reduit(clash, E ?= T, [E ?= T|Y], Res), !;
ponderation([E ?= T|Y], Res, E ?= T, 2).

ponderation([E ?= T|Y], Res, E ?= T, 2) :- 
reduit(rename, E ?= T, [E ?= T|Y], Res), !;
ponderation([E ?= T|Y], Res, E ?= T, 3).

ponderation([E ?= T|Y], Res, E ?= T, 2) :- 
reduit(simplify, E ?= T, [E ?= T|Y], Res), !;
ponderation([E ?= T|Y], Res, E ?= T, 3).

ponderation([E ?= T|Y], Res, E ?= T, 3) :- 
reduit(orient, E ?= T, [E ?= T|Y], Res), !;
ponderation([E ?= T|Y], Res, E ?= T, 4).

ponderation([E ?= T|Y], Res, E ?= T, 4) :- 
reduit(decompose, E ?= T, [E ?= T|Y], Res), !;
ponderation([E ?= T|Y], Res, E ?= T, 5).

ponderation([E ?= T|Y], Res, E ?= T, 5) :- 
reduit(expand, E ?= T, [E ?= T|Y], Res), !;
ponderation([E ?= T|Y], Res, E ?= T, 6).

ponderation([E ?= T|Y], Res, E ?= T, 6) :- 
reduit(delete, E ?= T, [E ?= T|Y], Res), !.

choix_random([E ?= T|Y], Res, E ?= T, _) :- 
    List = [0, 1, 2, 3, 4, 5, 6, 7],
    random_member(X1, List),
    delete(List, X1, List1),
    
    random_member(X2, List1),
    delete(List1, X2, List2),
    
    random_member(X3, List2),
    delete(List2, X3, List3),
    
    random_member(X4, List3),
    delete(List3, X4, List4),
    
    random_member(X5, List4),
    delete(List4, X5, List5),
    
    random_member(X6, List5),
    delete(List5, X6, List6),

    random_member(X7, List6),
    delete(List6, X7, List7),

    random_member(X8, List7),
    delete(List7, X8, List8),
    
    random(0,6, X1), random([E ?= T|Y], Res, E ?= T, Random), !;
    
    random(0,6, X2), random([E ?= T|Y], Res, E ?= T, Random), !;
    
    random(0,6, X3), random([E ?= T|Y], Res, E ?= T, Random), !;
    
    random(0,6, X4), random([E ?= T|Y], Res, E ?= T, Random), !;
    
    random(0,6, X5), random([E ?= T|Y], Res, E ?= T, Random), !;
    
    random(0,6, X6), random([E ?= T|Y], Res, E ?= T, Random), !;

    random(0,6, X7), random([E ?= T|Y], Res, E ?= T, Random), !;

    random(0,6, X8), random([E ?= T|Y], Res, E ?= T, Random), !.

random([E ?= T|Y], Res, E ?= T, 0) :- 
    reduit(rename, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 1) :- 
    reduit(simplify, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 2) :- 
    reduit(orient, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 3) :- 
    reduit(decompose, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 4) :- 
    reduit(expand, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 5) :- 
    reduit(delete, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 6) :- 
    reduit(check, E ?= T, [E ?= T|Y], Res), !.

random([E ?= T|Y], Res, E ?= T, 7) :- 
    reduit(clash, E ?= T, [E ?= T|Y], Res), !.

% UNIFICATION %

unify([], choix_premier) :- !.
unify([E|T], choix_premier) :- echo('SYSTEM :'), echo([E|T]), echo('\n'), choix_premier([E|T], RES, E, Regle), unify(RES, choix_premier), !.

unify([], choix_pondere) :- !.
unify([E|T], choix_pondere) :- echo('SYSTEM :'), echo([E|T]), echo('\n'), choix_pondere([E|T], RES, E, Regle), unify(RES, choix_pondere), !.


unify([], choix_random) :- !.
unify([E|T], choix_random) :- echo('SYSTEM :'), echo([E|T]), echo('\n'), choix_random([E|T], RES, E, Regle), unify(RES, choix_random), !.

unif(P,S) :-
  clr_echo, unify(P,S).

trace_unify(P,S) :- 
  set_echo, unify(P,S).
  
