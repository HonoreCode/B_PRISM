%###############################################

% This is a model-checker verifying Probabilistic Computation
% Tree Logic (PCTL) formulas over Discrete-Time Markov Chains (DTMC)

% The algorithm was greatly based on the PRISM's one.
% See http://www.prismmodelchecker.org/lectures/pmc/

% Working on SICStus prolog 4.10

%###############################################

:- module(dtmc_model_checking_SICS,[sat/2,sat/1,state/1]).
:- use_module(library(clpr),[{}/1]).

:- use_module('models/translate_model_SICS',[start/1,prop/2,state/1,trans/3]).

%PCTL Model-checking


% This term allow to handle nested formulas in case of dynamic model-checking
:- dynamic node/1.


sat(Formula):-start(E),sat(Formula,E).

% Classic cases

sat_not(false,_E).
sat_not(true,_E) :- fail.
sat_not(p(Property),E) :- \+(prop(Property,E)).
sat_not(and(F,_G),E) :- sat_not(F,E).
sat_not(and(_F,G),E) :- sat_not(G,E).
sat_not(or(F,G),E) :- sat_not(F,E),sat_not(G,E).
sat_not(implies(F,G),E) :- sat(not(or(not(F),G)),E).
sat_not(not(F),E) :- sat(F,E).

sat(true,_E).
sat(false,_E):-fail.
sat(p(Property),E) :- prop(Property,E).
sat(and(F,G),E) :- sat(F,E), sat(G,E).
sat(or(F,_G),E) :- sat(F,E).
sat(or(_F,G),E) :- sat(G,E).
sat(implies(F,G),E) :- sat(or(not(F),G),E).
sat(not(F),E) :- probformula(_,_,_)\=F,sat_not(F,E).

% Probabilistic-formula cases. Operator is =, < >,<= or >=. 
%P is a number between 0 and 1 (or a Variable),
% E is a state.

% Check if the formula is nested, and handle the 
% case of Always (g) and Always-bounded (gk) operators
sat(probformula(Operator,P,Ctl_formula),E) :-
    Ctl_formula = gk(_,_) ->
        sat_gk(probformula(Operator,P,Ctl_formula),E)
    ; Ctl_formula = g(_) ->
        sat_g(probformula(Operator,P,Ctl_formula),E)
    ; (node(Node) -> 
        New_Node is Node +1,
        retract(node(Node)),
        assert(node(New_Node)),
        sat_node(probformula(Operator,P,Ctl_formula),E,New_Node)
    ;   assert(node(0)),
        sat_node(probformula(Operator,P,Ctl_formula),E,0),
        retractall(node(_))        /*reinitialize nodes*/
    ).

% Negation of a probabilistic formula
sat(not(probformula(Operator,P,Ctl_formula)),E) :-
    sat(probformula(not(Operator),P,Ctl_formula),E).

% Always bounded formula, we use the dual probabilistic event of fk(K,not(F))
sat_gk(probformula(Operator,P,gk(K,F)),E) :-
    ground(P) -> 
        Q is 1-P,
        (Operator = equal ->
            sat(probformula(Operator,Q,fk(K,not(F))),E)
        ; Operator = greater ->
            sat(probformula(less,Q,fk(K,not(F))),E)
        ; Operator = less ->
            sat(probformula(greater,Q,fk(K,not(F))),E)
        ; Operator = strictlygreater ->
            sat(probformula(strictlyless,Q,fk(K,not(F))),E)
        ; Operator = strictlyless ->
            sat(probformula(strictlygreater,Q,fk(K,not(F))),E))
    ; ((Operator = equal ->
            sat(probformula(Operator,Q,fk(K,not(F))),E)
        ; Operator = greater ->
            sat(probformula(less,Q,fk(K,not(F))),E)
        ; Operator = less ->
            sat(probformula(greater,Q,fk(K,not(F))),E)
        ; Operator = strictlygreater ->
            sat(probformula(strictlyless,Q,fk(K,not(F))),E)
        ; Operator = strictlyless ->
            sat(probformula(strictlygreater,Q,fk(K,not(F))),E)),
        P is 1-Q)
    .

% Always formula
sat_g(probformula(Operator,P,g(F)),E) :-
    ground(P) -> 
        Q is 1-P,
        (Operator = equal ->
            sat(probformula(Operator,Q,f(not(F))),E)
        ; Operator = greater ->
            sat(probformula(less,Q,f(not(F))),E)
        ; Operator = less ->
            sat(probformula(greater,Q,f(not(F))),E)
        ; Operator = strictlygreater ->
            sat(probformula(strictlyless,Q,f(not(F))),E)
        ; Operator = strictlyless ->
            sat(probformula(strictlygreater,Q,f(not(F))),E))
    ; ((Operator = equal ->
            sat(probformula(Operator,Q,f(not(F))),E)
        ; Operator = greater ->
            sat(probformula(less,Q,f(not(F))),E)
        ; Operator = less ->
            sat(probformula(greater,Q,f(not(F))),E)
        ; Operator = strictlygreater ->
            sat(probformula(strictlyless,Q,f(not(F))),E)
        ; Operator = strictlyless ->
            sat(probformula(strictlygreater,Q,f(not(F))),E)),
        P is 1-Q)
    .

% Check the type of the formula
sat_node(probformula(Operator,P,Ctl_formula),E,Node) :- 
    Ctl_formula = u(F,G) ->
        prob_calc(u(F,G),E,P_phi,Node),
        against(P_phi,P,Operator)
    ;   Ctl_formula = f(G) -> 
        prob_calc(u(true,G),E,P_phi,Node),
        against(P_phi,P,Operator)
    ; Ctl_formula = fk(K,G) ->
        sat_dynamic(probformula(Operator,P,uk(true,K,G)),E,Node)
    ;   sat_dynamic(probformula(Operator,P,Ctl_formula),E,Node)
    .

% Use a different technic depending on the operator
% This allow especially the calculation of a probability for the equal operator
sat_dynamic(probformula(Operator,P,Ctl_formula),E,Node) :- 
    ((Operator = greater ; Operator= strictlygreater) ->
        ground(P),
        prob_calc(Ctl_formula,E,P,Operator,Node)

    ;   Operator = equal ->
            %retractall(prob_current(Node,_)),
            (ground(P) ->
                prob_calc(Ctl_formula,E,P,equal,Node)

            ;   (prob_calc(Ctl_formula,E,1.0,equal,Node) ->
                    P=1.0
                ;   prob_current(Node,P)))

    ;   Operator = less ->
            ground(P),
            \+(prob_calc(Ctl_formula,E,P,strictlygreater,Node))

    ;   Operator = strictlyless ->
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
against(P_phi,P,strictlyless) :- 
    ground(P),
    P_phi + 0.00000000000000023 < P.   % strictly less
against(P_phi,P,strictlygreater) :- 
    ground(P),
    P_phi - 0.00000000000000023 > P .   % strictly greater
against(P_phi,P,not(less)) :- against(P_phi,P,strictlygreater).
against(P_phi,P,not(greater)) :- against(P_phi,P,strictlyless).
against(P_phi,P,not(strictlyless)) :- against(P_phi,P,sup).
against(P_phi,P,not(strictlygreater)) :- against(P_phi,P,less).

% Next formula
:- dynamic prob_current/2.

prob_calc(x(F),E,P_phi,Operator,Node) :- 
    retractall(prob_current(Node,_)),
    assert(prob_current(Node,0.0)),
    (prob_calc_sub(x(F),E,P_phi,Operator,Node) ->
        true
    ; against(0.0,P_phi,Operator)),!.

% Until Bounded formula
prob_calc(uk(F,K,G),E,P_phi,Operator,Node) :- 
    retractall(prob_current(Node,_)),
    assert(prob_current(Node,0.0)),
    (sat(G,E) ->
        retractall(prob_current(Node,_)),
        assert(prob_current(Node,1.0)),
        against(1.0,P_phi,Operator)
    ; sat(F,E) ->
        (prob_calc_sub(uk(F,K,G),E,P_phi,1.0,Operator,Node) ->
            true
        ; against(0.0,P_phi,Operator))
    ; against(0.0,P_phi,Operator)),!.

% Until formula 
% For this formula we have to calculate the probability for
% all states
prob_calc(u(F,G),E,P_phi,Node) :- 
    retractall(table_prob0(_,_,Node)),
    retractall(table_prob1(_,_,Node)),
    prob_calc_u1(F,G,List_S,P_vect,Node),
    state(E),
    (find_prob_u(List_S,P_vect,E,P) -> P_phi=P
        ; P_phi=0.0
    ).


%*******************************************************

% recursion for the next formula
prob_calc_sub(x(F),E,P_phi,Operator,Node) :-
    trans(E,S,P),
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
            trans(E,S,P_trans),
            sat(F,S),
            K is K_new -1,
            P_trace_new is P_trace*P_trans,
            prob_calc_sub(uk(F,K,G),S,P_phi,P_trace_new,Operator,Node)
    ).
   
% 1rst precomputation for the until formula

:- dynamic table_prob0/3.

prob0(_F,_G,E,Node) :-
    table_prob0(E,true,Node),!.

prob0(_F,G,E,Node) :- 
    sat(G,E),
    !,
    asserta(table_prob0(E,true,Node)).

prob0(F,G,E1,Node) :- 
    sat(F,E1),
    assertz(table_prob0(E1,computing,Node)),
    trans(E1,E2,_P),
    \+ table_prob0(E2,computing,Node),
    (prob0(F,G,E2,Node) -> 
        asserta(table_prob0(E1,true,Node)),
        retract(table_prob0(E1,computing,Node))
    ),!.

search_prob0(F,G,E,Node):-
    retractall(table_prob0(_,computing,Node)),
    prob0(F,G,E,Node).
    
% 2nd precomputation for the until formula
:- dynamic table_prob1/3.

prob1(_F,_G,E,Node) :-
    table_prob1(E,true,Node),!.

prob1(_F,_G,E,Node) :- 
    \+(table_prob0(E,true,Node)),
    !,
    asserta(table_prob1(E,true,Node)).

prob1(F,G,E1,Node) :- 
    sat(F,E1),
    \+(sat(G,E1)),
    assert(table_prob1(E1,computing,Node)),
    trans(E1,E2,_P),
    \+ table_prob1(E2,computing,Node),
    (prob1(F,G,E2,Node) ->
        asserta(table_prob1(E1,true,Node)),
        retract(table_prob1(E1,computing,Node))
    ),!.

search_prob1(F,G,E,Node):-
    retractall(table_prob1(_,computing,Node)),
    prob1(F,G,E,Node).

% P_vect is the vector of non-null probabilities 
% for the until formula
prob_calc_u1(F,G,List_S,P_vect,Node) :- 
    findall(S,(state(S),search_prob0(F,G,S,Node)),List_S),
    findall(E,(state(E),search_prob1(F,G,E,Node)),_List),
    prob_calc_u2(F,G,List_S,List_S,P_vect,P_vect,Node).

prob_calc_u2(_F,_G,[],_List_S,[],_P_vect,_Node).
prob_calc_u2(F,G,[S|List_S_explored],List_S,[P_phi|P_vect_explored],P_vect,Node) :-
    (table_prob1(S,true,Node) -> prob_calc_u3(S,List_S,P_phi,P_vect) 
        ;   P_phi=1.0
    ),
    prob_calc_u2(F,G,List_S_explored,List_S,P_vect_explored,P_vect,Node).

prob_calc_u3(_S,[],0.0,[]).
prob_calc_u3(S,[E|List_S],P_phi_new,[P|P_vect]) :-
    prob_calc_u3(S,List_S,P_phi,P_vect),
    (trans(S,E,P_trans) -> {P_phi_new = P*P_trans + P_phi}
        ; P_phi_new=P_phi
    ).
 
find_prob_u([E],[P],E,P).
find_prob_u([S1,S2|List_S],[Prob1,Prob2|P_vect],E,P) :- 
    (E=S1,P=Prob1) 
        ; find_prob_u([S2|List_S],[Prob2|P_vect],E,P).