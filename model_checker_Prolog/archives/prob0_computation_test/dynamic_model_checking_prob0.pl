%###############################################

% This code is intended to compare the efficiency of 
% 2 different versions for the calculus of table_prob0,
% used for unbounded formulas.

% The version 1 is the currently used version in the
% model-checker integrated in ProB.

% One part of it is not optimal in efficiency, and
% this is what led to the version 2 that uses a totally different 
% approach.

% As you can see with the benchmark file (test),
% the version 2 is more efficient in some model
% checking, but slower in some other.

% As the version 1 is more stable in efficiency overall,
% it was decided to keep it in the current model-checker,
% but the version 2 is kept archived if someone want
% to try to work on it and maybe to improve it


% Working on SICStus prolog 4.10

%###############################################

:- module(dynamic_model_checking_prob0,[sat/2,sat/1,state/1]).
:- use_module(library(clpr),[{}/1]).

:- use_module(translate_model_prob0,[start/1,prop/2,state/1,trans/3]).

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
sat(not(F),E) :- 
    F = probformula(Operator,P,Ctl_formula) ->
        negate_operator(Operator,Neg_Operator),
        sat(probformula(Neg_Operator,P,Ctl_formula))
    ; sat_not(F,E).

% Probabilistic-formula cases. Operator is =, < >,<= or >=. 
%P is a number between 0 and 1 (or a Variable),
% E is a state.

% Check if the formula is nested, and handle the 
% case of Always (g) and Always-bounded (gk) operators
sat(probformula(Operator,P,Ctl_formula),E) :-
    Ctl_formula = g1(_) ->
        sat_g1(probformula(Operator,P,Ctl_formula),E)
    ; Ctl_formula = g2(_) ->
        sat_g2(probformula(Operator,P,Ctl_formula),E)
    ; (node(Node) -> 
        New_Node is Node +1,
        retract(node(Node)),
        assert(node(New_Node)),
        sat_node(probformula(Operator,P,Ctl_formula),E,New_Node)
    ;   assert(node(0)),
        sat_node(probformula(Operator,P,Ctl_formula),E,0),
        retractall(node(_))        /*reinitialize nodes*/
    ).

% Always formula, we use the opposite probabilistic event of fk(K,not(F))
% and the associed opposite operator (different than the logically diferent one,
% which is here called 'negation')
sat_g1(probformula(Operator,P,g1(F)),E) :-
    ground(P) -> 
        opposed_operator(Operator,Opp_Operator),
        Q is 1-P,
        sat(probformula(Opp_Operator,Q,f1(not(F))),E)
        
    ;   opposed_operator(Operator,Opp_Operator),
        sat(probformula(Opp_Operator,Q,f1(not(F))),E),
        P is 1-Q
    .

sat_g2(probformula(Operator,P,g2(F)),E) :-
    ground(P) -> 
        opposed_operator(Operator,Opp_Operator),
        Q is 1-P,
        sat(probformula(Opp_Operator,Q,f2(not(F))),E)
        
    ;   opposed_operator(Operator,Opp_Operator),
        sat(probformula(Opp_Operator,Q,f2(not(F))),E),
        P is 1-Q
    .

% Check the type of the formula
sat_node(probformula(Operator,P,Ctl_formula),E,Node) :- 
    Ctl_formula = u1(F,G) ->
        prob_calc_1(u1(F,G),E,P_phi,Node),
        against(P_phi,P,Operator)
    ;   Ctl_formula = f1(G) -> 
        prob_calc_1(u1(true,G),E,P_phi,Node),
        against(P_phi,P,Operator)
    ; Ctl_formula = u2(F,G) ->
        prob_calc_2(u2(F,G),E,P_phi,Node),
        against(P_phi,P,Operator)
    ;   Ctl_formula = f2(G) -> 
        prob_calc_2(u2(true,G),E,P_phi,Node),
        against(P_phi,P,Operator)
    .

% Compare different probabilities using a specific operator
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

% Used to compute the logical negation of
% a probabilistic formula
% General use : negate_operator(Op,not(Op))
negate_operator(less,strictlygreater).
negate_operator(strictlygreater,less).
negate_operator(greater,strictlyless).
negate_operator(strictlyless,greater).

% As in PRISM, the negation of an equal comparison
% will raise an error. Note that, however,
% double negation is allowed
negate_operator(equal,different). 


% Used for formulas with 'Always' (G) Operator
opposed_operator(equal,equal).
opposed_operator(less,greater).
opposed_operator(greater,less).
opposed_operator(strictlygreater,strictlyless).
opposed_operator(strictlyless,strictlygreater).


% Until formula 
% For this formula we have to calculate the probability for
% all states
prob_calc_1(u1(F,G),E,P_phi,Node) :- 
    retractall(table_prob0_version1(_,_,Node)),
    retractall(table_prob1_version1(_,_,Node)),
    prob_calc_u_version_1(F,G,List_S,P_vect,Node),
    (find_prob_u(List_S,P_vect,E,P) -> P_phi=P
        ; P_phi=0.0
    ).
   
prob_calc_2(u2(F,G),E,P_phi,Node) :- 
    retractall(table_prob0_version2(_,Node)),
    retractall(table_prob1_version2(_,_,Node)),
    prob_calc_u_version_2(F,G,List_S,P_vect,Node),
    (find_prob_u(List_S,P_vect,E,P) -> P_phi=P
        ; P_phi=0.0
    ).

%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


% ~ ~ ~ ~ 1rst precomputation for the until formula ~ ~ ~ ~


%    -  -  prob0 calculus VERSION 1     -   -

% This is a graph-based computation to find 
% all the states that can reach a state 
% verifying a property G without leaving
% states verifying a property F

:- dynamic table_prob0_version1/3.

prob0_version1(_F,_G,E,Node) :-
    table_prob0_version1(E,true,Node),!.

prob0_version1(_F,G,E,Node) :- 
    sat(G,E),
    !,
    asserta(table_prob0_version1(E,true,Node)).

prob0_version1(F,G,E1,Node) :- 
    sat(F,E1),
    assertz(table_prob0_version1(E1,computing,Node)),
    trans(E1,E2,_P),
    \+ table_prob0_version1(E2,computing,Node),
    (prob0_version1(F,G,E2,Node) -> 
        asserta(table_prob0_version1(E1,true,Node)),
        retract(table_prob0_version1(E1,computing,Node))
    ),!.

% This part could definitely be improved to 
% reduce computation time, and this is the reason
% why we tried the second version
search_prob0_version1(F,G,E,Node):-
    retractall(table_prob0_version1(_,computing,Node)),
    prob0_version1(F,G,E,Node).
    

% - - - - - - - - - - - - - - - - - - - 

%    -  -  prob0 calculus VERSION 2     -   -

% The idea is to start from the states verifying
% G, and going backwards, in contrary to the 
% version 1 which is going forward.
:- dynamic table_prob0_version2/2.

prob0_version2(_F,[],_Node).

prob0_version2(F,[E|T],Node) :-
    trans(S,E,_P),
    \+(table_prob0_version2(S,Node)),
    sat(F,S),
    assert(table_prob0_version2(S,Node)),
    prob0_version2(F,[E,S|T],Node),!.

prob0_version2(F,[_E|T],Node):-prob0_version2(F,T,Node).


% ~ ~ ~ ~ 2nd precomputation for the until formula ~ ~ ~ ~

% This is a graph-based computation to find 
% all the states that can reach a state 
% which IS NOT verifying a property G without 
% leaving states verifying a property F

%    -  -  prob1 calculus VERSION 1     -   -

:- dynamic table_prob1_version1/3.

prob1_version1(_F,_G,E,Node) :-
    table_prob1_version1(E,true,Node),!.

prob1_version1(_F,_G,E,Node) :- 
    \+(table_prob0_version1(E,true,Node)),
    !,
    asserta(table_prob1_version1(E,true,Node)).

prob1_version1(F,G,E1,Node) :- 
    sat(F,E1),
    \+(sat(G,E1)),
    assert(table_prob1_version1(E1,computing,Node)),
    trans(E1,E2,_P),
    \+ table_prob1_version1(E2,computing,Node),
    (prob1_version1(F,G,E2,Node) ->
        asserta(table_prob1_version1(E1,true,Node)),
        retract(table_prob1_version1(E1,computing,Node))
    ),!.

search_prob1_version1(F,G,E,Node):-
    retractall(table_prob1_version1(_,computing,Node)),
    prob1_version1(F,G,E,Node).


%    -  -  prob1 calculus VERSION 2     -   -

:- dynamic table_prob1_version2/3.

prob1_version2(_F,_G,E,Node) :-
    table_prob1_version2(E,true,Node),!.

prob1_version2(_F,_G,E,Node) :- 
    \+(table_prob0_version2(E,Node)),
    !,
    asserta(table_prob1_version2(E,true,Node)).

prob1_version2(F,G,E1,Node) :- 
    sat(F,E1),
    \+(sat(G,E1)),
    assert(table_prob1_version2(E1,computing,Node)),
    trans(E1,E2,_P),
    \+ table_prob1_version2(E2,computing,Node),
    (prob1_version2(F,G,E2,Node) ->
        asserta(table_prob1_version2(E1,true,Node)),
        retract(table_prob1_version2(E1,computing,Node))
    ),!.

search_prob1_version2(F,G,E,Node):-
    retractall(table_prob1_version2(_,computing,Node)),
    prob1_version2(F,G,E,Node).


%###################################


% Until formula computation

% P_vect is the vector of non-null probabilities 


%       VERSION 1

prob_calc_u_version_1(F,G,List_S,P_vect,Node) :- 
    findall(S,(state(S),search_prob0_version1(F,G,S,Node)),List_S),
    findall(E,(state(E),search_prob1_version1(F,G,E,Node)),_List),
    prob_calc_u2_v1(F,G,List_S,List_S,P_vect,P_vect,Node).

prob_calc_u2_v1(_F,_G,[],_List_S,[],_P_vect,_Node).
prob_calc_u2_v1(F,G,[S|List_S_explored],List_S,[P_phi|P_vect_explored],P_vect,Node) :-
    (table_prob1_version1(S,true,Node) -> prob_calc_u3(S,List_S,P_phi,P_vect) 
        ;   P_phi=1.0
    ),
    prob_calc_u2_v1(F,G,List_S_explored,List_S,P_vect_explored,P_vect,Node).



%       VERSION 2

prob_calc_u_version_2(F,G,List_S,P_vect,Node) :- 
    findall(S,(state(S),sat(G,S),assert(table_prob0_version2(S,Node))),T),
    prob0_version2(F,T,Node),
    findall(S,(table_prob0_version2(S,Node)),List_S),
    findall(E,(state(E),search_prob1_version2(F,G,E,Node)),_List),
    prob_calc_u2_v2(F,G,List_S,List_S,P_vect,P_vect,Node).


prob_calc_u2_v2(_F,_G,[],_List_S,[],_P_vect,_Node).
prob_calc_u2_v2(F,G,[S|List_S_explored],List_S,[P_phi|P_vect_explored],P_vect,Node) :-
    (table_prob1_version2(S,true,Node) -> prob_calc_u3(S,List_S,P_phi,P_vect) 
        ;   P_phi=1.0
    ),
    prob_calc_u2_v2(F,G,List_S_explored,List_S,P_vect_explored,P_vect,Node).

%       -   -   -   -   -   -   -   

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