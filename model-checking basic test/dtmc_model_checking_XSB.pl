%###############################################

% This is a model-checker verifying Probabilistic Computation
% Tree Logic (PCTL) formulas over Discrete-Time Markov Chains (DTMC)

% Working on XSB prolog 3.8

%###############################################

:- module(dtmc_model_checking_XSB,[sat/2]).
:- use_module(clpr,[{}/1]).

:- add_lib_dir(('models')).
:- use_module(translate_model_XSB,[start/1,prop/2,state/1,trans/3]).

%PCTL Model-checking


sat(Formula):-start(E),sat(Formula,E).

% Classic cases
sat(true,_E).
sat(false,_E):-fail.
sat(p(Property),E) :- prop(E,Property).
sat(and(F,G),E) :- sat(F,E), sat(G,E).
sat(or(F,_G),E) :- sat(F,E).
sat(or(_F,G),E) :- sat(G,E).
sat(imp(F,G),E) :- sat(or(not(F),G),E).
sat(not(F),E) :- prob_formula(_)\=F,sat_not(F,E).

sat_not(false,_E).
sat_not(true,_E) :- fail.
sat_not(p(Property),E) :- not(prop(E,Property)).
sat_not(and(F,_G),E) :- sat_not(F,E).
sat_not(and(_F,G),E) :- sat_not(G,E).
sat_not(or(F,G),E) :- sat_not(F,E),sat_not(G,E).
sat_not(imp(F,G),E) :- sat(not(or(not(F),G)),E).
sat_not(not(F),E) :- sat(F,E).

% Probabilistic-formula cases. Operator is =, < >,<= or >=. P is a number between 0 and 1 (or a Variable),
% E is a state.
sat(prob_formula(Operator,P,Ctl_formula),E) :- 
    prob_calc(Ctl_formula,E,P_phi),
    against(P_phi,P,Operator).
sat(not(prob_formula(Operator,P,Ctl_formula)),E) :-
    sat(prob_formula(not(Operator),P,Ctl_formula),E).

% Always bounded formula, we use the dual probabilistic event of f(K,not(F))
sat(prob_formula(Operator,P,g(K,F)),E) :-
    Q is 1-P,
    sat(not(prob_formula(Operator,Q,f(K,not(F)))),E).

% Always formula
sat(prob_formula(Operator,P,g(F)),E) :-
    Q is 1-P,
    sat(not(prob_formula(Operator,Q,f(not(F)))),E).

% Compare different formulas using a specific operator
against(P_phi,P,eq) :- P_phi = P.
against(P_phi,P,inf) :- P_phi =< P.
against(P_phi,P,sup) :- P_phi >= P.
against(P_phi,P,sinf) :- P_phi < P.
against(P_phi,P,ssup) :- P_phi > P.
against(P_phi,P,not(inf)) :- P_phi > P.
against(P_phi,P,not(sup)) :- P_phi < P.
against(P_phi,P,not(sinf)) :- P_phi >= P.
against(P_phi,P,not(ssup)) :- P_phi =< P.

% Next formula
prob_calc(x(F),E,P_phi) :-
    findall(S,(state(S),sat(F,S)),List_S),
    add_prob(List_S,E,P_phi).

% Until Bounded formula
prob_calc(u(_F,0,G),E,P_phi) :- 
    (sat(G,E) -> P_phi=1.0;
    P_phi=0.0).
prob_calc(u(F,K_new,G),E,P_phi) :- 
    K_new > 0,
    state(E),
    (sat(G,E) -> P_phi=1.0 ;
    sat(not(F),E) -> P_phi=0.0;
    (K is K_new -1 ,
    findall(S,trans(E,S,_),List_S),
    add_prob_prime(P_phi,List_S,E,F,G,K))).

% Until formula 
% For this formula we have to calculate the probability for
% all states
prob_calc(u(F,G),E,P_phi) :- 
    prob_calc_u1(F,G,List_S,P_vect),
    state(E),
    (find_prob_u(List_S,P_vect,E,P) -> P_phi=P;
    P_phi=0.0).

% Eventually-bounded formula
prob_calc(f(K,F),E,P_phi) :-
    prob_calc(u(true,K,F),E,P_phi).

% Eventually formula
prob_calc(f(F),E,P_phi) :-
    prob_calc(u(true,F),E,P_phi).


%**********************************************************
% EXPERIMENTAL, WORKING BUT NOT EFFICIENT YET
% dynamic state exploration to compare probabilities

:- dynamic explorex/1.
:- dynamic prob_current/1.

prob_calc_sup(x(F),E,P_phi) :- 
    retractall(explorex(_)),
    {P>=P_phi},
    prob_calc_sup_sub(x(F),E,P).

prob_calc_sup(u(F,K,G),E,P_phi) :- 
    retractall(prob_current(_)),
    assert(prob_current(0.0)),
    prob_calc_sup_sub(u(F,K,G),E,P_phi,1.0).

prob_calc_sup_sub(x(_F),_E,0.0).
prob_calc_sup_sub(x(F),E,P_new) :-
    state(S),
    (not(explorex(S)) ->
    (sat(F,S),
    trans(E,S,P_trans),
    assert(explorex(S)),
    prob_calc_sup_sub(x(F),E,P),
    P_new is P+P_trans);
    fail).

prob_calc_sup_sub(u(F,K_new,G),E,P_phi,P_trace) :-
    sat(G,E) ->
    (prob_current(P),
    P_new is P+P_trace,
    retract(prob_current(P)),
    assert(prob_current(P_new)),
    P_new >= P_phi);
    (K_new > 0,
    state(S),
    trans(E,S,P_trans),
    sat(F,S),
    K is K_new -1,
    P_trace_new = P_trace*P_trans,
    prob_calc_sup_sub(u(F,K,G),S,P_phi,P_trace_new)).

%*******************************************************

% probability sum for the next formula
add_prob([],_,0.0).
add_prob([State|List_States],E,P_phi_new) :- 
    (trans(E,State,P) -> P_trans = P ;
        P_trans=0.0),
    add_prob(List_States,E,P_phi),
    P_phi_new is P_phi + P_trans.

% probability sum for the bounded until formula
add_prob_prime(0.0,[],_,_,_,_).
add_prob_prime(P_phi_new,[S|List_S],E,F,G,K) :-
    trans(E,S,P_trans),
    prob_calc(u(F,K,G),S,P_mat),
    add_prob_prime(P_phi,List_S,E,F,G,K),
    P_phi_new is P_trans*P_mat + P_phi.
   
% 1rst precomputation for the until formula
:- table prob0/3.

prob0(_F,G,E) :- sat(G,E).
prob0(F,G,E1) :- sat(F,E1),trans(E1,E2,_),prob0(F,G,E2).

% 2nd precomputation for the until formula
:- table prob1/3.

prob1(F,G,E) :- not(prob0(F,G,E)).
prob1(F,G,E1) :- sat(F,E1),trans(E1,E2,_),prob1(F,G,E2).

% P_vect is the vector of non-null probabilities 
% for the until formula
prob_calc_u1(F,G,List_S,P_vect) :- 
    findall(S,(state(S),prob0(F,G,S)),List_S),
    prob_calc_u2(F,G,List_S,List_S,P_vect,P_vect).

prob_calc_u2(_F,_G,[],_List_S,[],_P_vect).
prob_calc_u2(F,G,[S|List_S_explored],List_S,[P_phi|P_vect_explored],P_vect) :-
    (not(prob1(F,G,S)) -> P_phi=1.0 ; 
    prob_calc_u3(S,List_S,P_phi,P_vect)),
    prob_calc_u2(F,G,List_S_explored,List_S,P_vect_explored,P_vect).

prob_calc_u3(_S,[],0.0,[]).
prob_calc_u3(S,[E|List_S],P_phi_new,[P|P_vect]) :-
    prob_calc_u3(S,List_S,P_phi,P_vect),
    (trans(S,E,P_trans) -> {P_phi_new = P*P_trans + P_phi};
    {P_phi_new=P_phi}).

find_prob_u([E],[P],E,P).
find_prob_u([S1,S2|List_S],[Prob1,Prob2|P_vect],E,P) :- 
    (E=S1,P=Prob1) ; 
    find_prob_u([S2|List_S],[Prob2|P_vect],E,P).