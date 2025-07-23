% (c) 2025-2025 Lehrstuhl fuer Softwaretechnik und Programmiersprachen,
% Heinrich Heine Universitaet Duesseldorf
% This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html)

%###############################################

% This is a model-checker verifying Probabilistic Computation
% Tree Logic (PCTL) formulas over Discrete-Time Markov Chains (DTMC)
% written by Honore Marmion

% Working on SICStus prolog 4.10

%###############################################

:- module(dtmc_model_checking,[sat/1, sat/2,search_prob0/3,prob0/3,prob1/3,table_prob0/2,state/1]).
:- use_module(library(clpr),[{}/1]).


% Interface predicates defining the system to be checked

:- use_module(probsrc(xtl_interface),[xtl_transition/4]).
:- use_module(probltlsrc(ltl_propositions), [trans/3, check_transition/4, check_ap/2]).
:- use_module(probsrc(state_space),[visited_expression/2, visited_expression_id/1]).

% TODO: move to specfile and extend to work with B pragmas,...
transition_probability(From,To,Prob) :- trans(From,To,_),
   visited_expression(From,FS),
   visited_expression(To,TS),
   xtl_transition(FS,_,TS,Infos),
   (member(probability/X,Infos) -> Prob=X ; write(no_probability(Infos)),nl,fail).

state(X) :- visited_expression_id(X).

% example public_examples/XTL/markov/SimpleMarkov.P
% ?- use_module(extension('markov/dtmc_model_checking.pl')).
% ?- sat(prob_formula(eq,P,f(p(xtl_predicate_check(finished))))).

%PCTL Model-checking

% This term allow to handle nested formulas in case of dynamic model-checking
:- dynamic node/1.

start(root).

sat(Formula) :- start(E), sat(Formula,E).


% Classic cases

sat_not(false,_E).
sat_not(true,_E) :- fail.
sat_not(p(Property),E) :- \+(check_ap(Property,E)).
sat_not(and(F,_G),E) :- sat_not(F,E).
sat_not(and(_F,G),E) :- sat_not(G,E).
sat_not(or(F,G),E) :- sat_not(F,E),sat_not(G,E).
sat_not(implies(F,G),E) :- sat(not(or(not(F),G)),E).
sat_not(not(F),E) :- sat(F,E).

sat(true,_E).
sat(false,_E):-fail.
sat(p(Property),E) :- check_ap(Property,E).
sat(and(F,G),E) :- sat(F,E), sat(G,E).
sat(or(F,_G),E) :- sat(F,E).
sat(or(_F,G),E) :- sat(G,E).
sat(implies(F,G),E) :- sat(or(not(F),G),E).
sat(not(F),E) :- probformula(_,_,_)\=F,sat_not(F,E).

% Probabilistic-formula cases. Operator is =, < >,<= or >=. P is a number between 0 and 1 (or a Variable),
% E is a state.
sat(probformula(Operator,P,Ctl_formula),E) :- 
    Ctl_formula = u(F,G) ->
        prob_calc(u(F,G),E,P_phi),
        against(P_phi,P,Operator)
    ;   Ctl_formula = f(G) -> 
        sat(probformula(Operator,P,u(true,G)),E)
    ; Ctl_formula = fk(K,G) ->
        sat(probformula(Operator,P,uk(true,K,G)),E)
    ;   sat_dynamic(probformula(Operator,P,Ctl_formula),E)
    .
sat(not(probformula(Operator,P,Ctl_formula)),E) :-
    sat(probformula(not(Operator),P,Ctl_formula),E).

% Always bounded formula, we use the dual probabilistic event of fk(K,not(F))
sat(probformula(Operator,P,gk(K,F)),E) :-
    Q is 1-P,
    sat(not(probformula(Operator,Q,fk(K,not(F)))),E).

% Always formula
sat(probformula(Operator,P,g(F)),E) :-
    Q is 1-P,
    sat(not(probformula(Operator,Q,f(not(F)))),E).

% Check if the formula is nested 
sat_dynamic(probformula(Operator,P,Ctl_formula),E) :-
    (node(Node) -> 
        New_Node is Node +1,
        retract(node(Node)),
        assert(node(New_Node)),
        sat_dynamic_node(probformula(Operator,P,Ctl_formula),E,New_Node)
    ;   assert(node(0)),
        sat_dynamic_node(probformula(Operator,P,Ctl_formula),E,0),
        retractall(node(_))        /*reinitialize nodes*/
    ).

% Use a different technic depending on the operator
% This allow notably the calculation of a probability for the equal operator
sat_dynamic_node(probformula(Operator,P,Ctl_formula),E,Node) :- 
    ((Operator = greater ; Operator= strictly_greater) ->
        ground(P),
        prob_calc(Ctl_formula,E,P,Operator,Node)

    ;   Operator = equal ->
            (ground(P) ->
                prob_calc(Ctl_formula,E,P,equal,Node)

            ;   (prob_calc(Ctl_formula,E,1.0,equal,Node) ->
                    P=1.0
                ;   prob_current(Node,P)))

    ;   Operator = less ->
            ground(P),
            \+(prob_calc(Ctl_formula,E,P,strictly_greater,Node))

    ;   Operator = strictly_less ->
            ground(P),
            \+(prob_calc(Ctl_formula,E,P,greater,Node))
    ).

% Compare different formulas using a specific operator

% For the equal comparison, we compare the results using epsilon precision in case 
% of a given probability
against(P_phi,P,equal) :- 
    ground(P) ->
        P_phi =< P + 0.00000000000000023,
        P =< P_phi + 0.00000000000000023
    ;   P = P_phi
    .
against(P_phi,P,less) :- 
    ground(P),
    P_phi =< P + 0.00000000000000023.   % less
against(P_phi,P,greater) :- 
    ground(P),
    P_phi >= P - 0.00000000000000023.   % greater
against(P_phi,P,strictly_less) :- 
    ground(P),
    P_phi < P + 0.00000000000000023.   % strictly less
against(P_phi,P,strictly_greater) :- 
    ground(P),
    P_phi > P - 0.00000000000000023.   % strictly_greater
against(P_phi,P,not(less)) :- against(P_phi,P,strictly_greater).
against(P_phi,P,not(greater)) :- against(P_phi,P,strictly_less).
against(P_phi,P,not(strictly_less)) :- against(P_phi,P,sup).
against(P_phi,P,not(strictly_greater)) :- against(P_phi,P,less).

% Next formula
:- dynamic prob_current/2.

prob_calc(x(F),E,P_phi,Operator,Node) :- 
    retractall(prob_current(Node,_)),
    assert(prob_current(Node,0.0)),
    prob_calc_sub(x(F),E,P_phi,Operator,Node),!.

% Until Bounded formula
prob_calc(uk(F,K,G),E,P_phi,Operator,Node) :- 
    retractall(prob_current(Node,_)),
    assert(prob_current(Node,0.0)),
    ((sat(F,E) ; sat(G,E)) ->
        prob_calc_sub(uk(F,K,G),E,P_phi,1.0,Operator,Node)
    ;   against(0.0,P_phi,Operator)),!
    .


% Eventually-bounded formula
prob_calc(fk(K,F),E,P_phi,Operator) :-
    prob_calc(uk(true,K,F),E,P_phi,Operator).


% Until formula 
% For this formula we have to calculate the probability for
% all states
prob_calc(u(F,G),E,P_phi) :- 
    retractall(table_prob0(_,_)),
    retractall(table_prob1(_,_)),
    prob_calc_u1(F,G,List_S,P_vect),
    state(E),
    (find_prob_u(List_S,P_vect,E,P) -> P_phi=P
        ; P_phi=0.0
    ).


%*******************************************************

% recursion for the next formula
prob_calc_sub(x(F),E,P_phi,Operator,Node) :-
    transition_probability(E,S,P),
    sat(F,S),
    prob_current(Node,Previous_P),
    Current_prob is Previous_P +P,
    retract(prob_current(Node,Previous_P)),
    assert(prob_current(Node,Current_prob)),
    against(Current_prob,P_phi,Operator).

% recursion for the bounded until formula
prob_calc_sub(uk(F,K_new,G),E,P_phi,P_trace,Operator,Node) :-
    (sat(G,E) ->
        prob_current(Node,P),
        P_new is P+P_trace,
        retract(prob_current(Node,P)),
        assert(prob_current(Node,P_new)),
        against(P_new,P_phi,Operator)
        ;   K_new > 0,
            transition_probability(E,S,P_trans),
            sat(F,S),
            K is K_new -1,
            P_trace_new is P_trace*P_trans,
            prob_calc_sub(uk(F,K,G),S,P_phi,P_trace_new,Operator,Node)
    ).
   
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
    (transition_probability(S,E,P_trans) -> {P_phi_new = P*P_trans + P_phi}
        ; P_phi_new=P_phi
    ).
 
find_prob_u([E],[P],E,P).
find_prob_u([S1,S2|List_S],[Prob1,Prob2|P_vect],E,P) :- 
    (E=S1,P=Prob1) 
        ; find_prob_u([S2|List_S],[Prob2|P_vect],E,P).