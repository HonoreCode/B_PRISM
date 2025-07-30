%**************************************

% Module used :
%    - to load the interesting parts from a B model of the
%      CROWDS PROTOCOL translated to Prolog using ProB's 
%      reachability analysis

%    - to assign probabilities to each transition

% See http://www.prismmodelchecker.org/casestudies/crowds.php
% for more informations
% PARAMETERS : 
%    - pf = 0.8
%    - TotalRuns = 3
%    - CrowdSize  = 5

%*************************************

:- module(crowds_5_3_to_LTS_SICS,[start_crowds_5_3/1,prop_crowds_5_3/2,state_crowds_5_3/1,trans_crowds_5_3/3]).

:- consult('../Generated_state_spaces/slep_state_space_crowds53').

%**************************************
% Formulas to try on :

% sat(prob_formula(eq,P,f(p(positive))),0).

% sat(prob_formula(eq,P,f(p(false_positive))),0).

% sat(prob_formula(eq,P,f(p(confidence))),0).

%**************************************

% Definition of the Probabilistic labelled transition system from the state space
% THIS PART MAY DEPEND ON THE MODEL
start_crowds_5_3(0).

prop_crowds_5_3(positive,E) :- 
   packed_visited_expression(
    E,'$bind_lst'(_,l3(_,_,_,l3(_,_,_,l3(
        '$avl_packed'(packed_node((_,_),_,packed_node((0,X),_,_,_),_))
        ,_,_,[_,_]))))
   ),
    X>1.

prop_crowds_5_3(false_positive,E) :-
    packed_visited_expression(
        E,'$bind_lst'(_,l3(_,_,_,l3(_,_,_,l3(
        '$avl_packed'(
            packed_node((2,X2),_,
            packed_node((0,X0),_,_,
            packed_leaf((1,X1))),
            packed_node((3,X3),_,_,
            packed_leaf((4,X4))
                ))),_,_,[_,_]))))
    ),
    X0=<1,
    (X1>1
    ;X2>1
    ;X3>1
    ;X4>1
    ).

prop_crowds_5_3(confidence,E) :- 
    packed_visited_expression(
        E,'$bind_lst'(_,l3(_,_,_,l3(_,_,_,l3(
        '$avl_packed'(
            packed_node((2,X2),_,
            packed_node((0,X0),_,_,
            packed_leaf((1,X1))),
            packed_node((3,X3),_,_,
            packed_leaf((4,X4))
                ))),_,_,[_,_]))))
    ),
    X0>1,
    X1=<1,
    X2=<1,
    X3=<1,
    X4=<1.

trans_crowds_5_3(S1,S2,P) :-
    transition(S1,_,_,S2),
    (transition(S1,'Is_a_good_member'(pred_true),_,S2) -> P is 0.833
        ; transition(S1,'Is_a_good_member'(pred_false),_,S2) -> P is 0.167
        ; transition(S1,'Forward_or_deliver'(pred_true),_,S2) -> P is 0.8
        ; transition(S1,'Forward_or_deliver'(pred_false),_,S2) -> P is 0.2
        ; transition(S1,'Record'(_),_,S2) -> P is 0.2
        ; P is 1.0
    ).

state_crowds_5_3(E):-packed_visited_expression(E,_).
