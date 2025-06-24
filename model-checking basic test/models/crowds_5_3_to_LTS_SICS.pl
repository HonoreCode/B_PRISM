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

:- module(crowds_5_3_to_LTS_SICS,[start_crowds/1,prop_crowds/2,state_crowds/1,trans_crowds/3]).

:- consult(slep_state_space_crowds53).

%**************************************
% Formulas to try on :

% sat(prob_formula(eq,P,f(p(positive))),0).

% sat(prob_formula(eq,P,f(p(false_positive))),0).

% sat(prob_formula(eq,P,f(p(confidence))),0).

%**************************************

% Definition of the Probabilistic labelled transition system from the state space
% THIS PART MAY DEPEND ON THE MODEL
start_crowds(0).

prop_crowds(positive,E) :- 
   packed_visited_expression(
    E,'$bind_lst'(_,l3(_,_,_,l3(_,_,_,l3(
        '$avl_packed'(packed_node((_,_),_,packed_node((0,X),_,_,_),_))
        ,_,_,[_,_]))))
   ),
    X>1.

prop_crowds(false_positive,E) :-
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

prop_crowds(confidence,E) :- 
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

trans_crowds(S1,S2,0.833) :- 
    transition(S1,'Is_a_good_member'(pred_true),_,S2). 

trans_crowds(S1,S2,0.167) :-
    transition(S1,'Is_a_good_member'(pred_false),_,S2). 

trans_crowds(S1,S2,0.8) :- 
    transition(S1,'Forward_or_deliver'(pred_true),_,S2). 

trans_crowds(S1,S2,0.2) :-
    transition(S1,'Forward_or_deliver'(pred_false),_,S2). 

trans_crowds(S1,S2,0.2) :-
    transition(S1,'Record'(_),_,S2).  

trans_crowds(S1,S2,1.0) :- 
    (transition(S1,'New_protocol',_,S2)
    ; transition(S1,'Start_protocol',_,S2)
    ; transition(S1,'Eavesdrop',_,S2)
    ; transition(S1,'Deliver',_,S2)
    ; transition(S1,'Restart',_,S2)).

state_crowds(E):-packed_visited_expression(E,_).
