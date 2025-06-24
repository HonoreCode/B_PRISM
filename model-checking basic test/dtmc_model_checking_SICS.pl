%###############################################

% This is a model-checker verifying Probabilistic Computation
% Tree Logic (PCTL) formulas over Discrete-Time Markov Chains (DTMC)

% Working on SICStus prolog 4.10

%###############################################

:- module(dtmc_model_checking_SICS,[sat/2,search_prob0/3,prob0/3,prob1/3,table_prob0/2,state/1]).
:- use_module(library(clpr),[{}/1]).

:- use_module('models/translate_model_SICS',[start/1,prop/2,state/1,trans/3]).

%PCTL Model-checking


sat(Formula):-start(E),sat(Formula,E).

% Classic cases

sat_not(false,_E).
sat_not(true,_E) :- fail.
sat_not(p(Property),E) :- \+(prop(Property,E)).
sat_not(and(F,_G),E) :- sat_not(F,E).
sat_not(and(_F,G),E) :- sat_not(G,E).
sat_not(or(F,G),E) :- sat_not(F,E),sat_not(G,E).
sat_not(imp(F,G),E) :- sat(not(or(not(F),G)),E).
sat_not(not(F),E) :- sat(F,E).

sat(true,_E).
sat(false,_E):-fail.
sat(p(Property),E) :- prop(Property,E).
sat(and(F,G),E) :- sat(F,E), sat(G,E).
sat(or(F,_G),E) :- sat(F,E).
sat(or(_F,G),E) :- sat(G,E).
sat(imp(F,G),E) :- sat(or(not(F),G),E).
sat(not(F),E) :- prob_formula(_,_,_)\=F,sat_not(F,E).

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
    (sat(G,E) -> P_phi=1.0
        ; P_phi=0.0
    ).
prob_calc(u(F,K_new,G),E,P_phi) :- 
    K_new > 0,
    state(E),
    (sat(G,E) -> P_phi=1.0 
        ; (sat(not(F),E) -> P_phi=0.0
            ; K is K_new -1 ,
            findall(S,trans(E,S,_),List_S),
            add_prob_prime(P_phi,List_S,E,F,G,K)
        )
    ).

% Until formula 
% For this formula we have to calculate the probability for
% all states
prob_calc(u(F,G),E,P_phi) :- 
    retractall(table_prob0(_,_)),
    retractall(table_prob1(_,_)),
%    retractall(good_search(_C)),
    prob_calc_u1(F,G,List_S,P_vect),
    state(E),
    (find_prob_u(List_S,P_vect,E,P) -> P_phi=P
        ; P_phi=0.0
    ).

% Eventually-bounded formula
prob_calc(f(K,F),E,P_phi) :-
    prob_calc(u(true,K,F),E,P_phi).

% Eventually formula
prob_calc(f(F),E,P_phi) :-
    prob_calc(u(true,F),E,P_phi).

%*******************************************************

% probability sum for the next formula
add_prob([],_,0.0).
add_prob([State|List_States],E,P_phi_new) :- 
    (trans(E,State,P) -> P_trans = P 
        ; P_trans=0.0
    ),
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

:- dynamic table_prob0/2.

prob0(_F,_G,E) :-
    table_prob0(E,true),!.

prob0(_F,G,E) :- 
    sat(G,E),
    !,
    asserta(table_prob0(E,true)).

prob0(F,G,E1) :- 
    sat(F,E1),
    assertz(table_prob0(E1,computing)),
    trans(E1,E2,_P),
    \+ table_prob0(E2,computing),
    (prob0(F,G,E2) -> 
        asserta(table_prob0(E1,true)),
        retract(table_prob0(E1,computing))
    ),!.

search_prob0(F,G,E):-
    retractall(table_prob0(_,computing)),
    prob0(F,G,E).
    
% 2nd precomputation for the until formula
:- dynamic table_prob1/2.

prob1(_F,_G,E) :-
    table_prob1(E,true),!.

prob1(_F,_G,E) :- 
    \+(table_prob0(E,true)),
    !,
    asserta(table_prob1(E,true)).

prob1(F,G,E1) :- 
    sat(F,E1),
    assert(table_prob1(E1,computing)),
    trans(E1,E2,_P),
    \+ table_prob1(E2,computing),
    (prob1(F,G,E2) ->
        asserta(table_prob1(E1,true)),
        retract(table_prob1(E1,computing))),!.

% P_vect is the vector of non-null probabilities 
% for the until formula
prob_calc_u1(F,G,List_S,P_vect) :- 
    findall(S,(state(S),search_prob0(F,G,S)),List_S),
    findall(E,(state(E),prob1(F,G,E)),_List),
    prob_calc_u2(F,G,List_S,List_S,P_vect,P_vect).

prob_calc_u2(_F,_G,[],_List_S,[],_P_vect).
prob_calc_u2(F,G,[S|List_S_explored],List_S,[P_phi|P_vect_explored],P_vect) :-
    (table_prob1(S,true) -> prob_calc_u3(S,List_S,P_phi,P_vect) 
        ;   P_phi=1.0
    ),
    prob_calc_u2(F,G,List_S_explored,List_S,P_vect_explored,P_vect).

prob_calc_u3(_S,[],0.0,[]).
prob_calc_u3(S,[E|List_S],P_phi_new,[P|P_vect]) :-
    prob_calc_u3(S,List_S,P_phi,P_vect),
    (trans(S,E,P_trans) -> {P_phi_new = P*P_trans + P_phi}
        ; {P_phi_new=P_phi}
    ).
 
find_prob_u([E],[P],E,P).
find_prob_u([S1,S2|List_S],[Prob1,Prob2|P_vect],E,P) :- 
    (E=S1,P=Prob1) 
        ; find_prob_u([S2|List_S],[Prob2|P_vect],E,P).