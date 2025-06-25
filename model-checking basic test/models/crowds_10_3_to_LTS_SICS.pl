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
%    - CrowdSize  = 10

%*************************************

:- module(crowds_10_3_to_LTS_SICS,[start_crowds_10_3/1,prop_crowds_10_3/2,state_crowds_10_3/1,trans_crowds_10_3/3]).

:- consult(slep_state_space_crowds103).

%**************************************
% Formulas to try on :

% sat(prob_formula(eq,P,f(p(positive))),0).

% sat(prob_formula(eq,P,f(p(false_positive))),0).

% sat(prob_formula(eq,P,f(p(confidence))),0).

%**************************************

% Definition of the Probabilistic labelled transition system from the state space
% THIS PART MAY DEPEND ON THE MODEL
start_crowds_10_3(0).

prop_crowds_10_3(positive,E) :- 
    packed_visited_expression(
        E,'$bind_lst'(_,l3(_,_,_,l3(_,_,_,l3(
            '$avl_packed'(
                packed_node(_,_,
                packed_node(_,_,
                packed_leaf((0,X)),_)
                    ,_)),_,_,[_,_]))))
        ),
    X>1.

prop_crowds_10_3(false_positive,E) :-
    packed_visited_expression(
        E,'$bind_lst'(_,l3(_,_,_,l3(_,_,pred_true,l3(
        '$avl_packed'(
            packed_node((4,X4),_,
            packed_node((1,X1),_,
            packed_leaf((0,X0)),
            packed_node((2,X2),_,_,
            packed_leaf((3,X3)))),
            packed_node((7,X7),_,
            packed_node((5,X5),_,_,
            packed_leaf((6,X6))),
            packed_node((8,X8),_,_,
            packed_leaf((9,X9))
                )))),_,_,[_,_]))))
        ),
    X0=<1,
    (X1>1
    ;X2>1
    ;X3>1
    ;X4>1
    ;X5>1
    ;X6>1
    ;X7>1
    ;X8>1
    ;X9>1
    ).

prop_crowds_10_3(confidence,E) :- 
    packed_visited_expression(
        E,'$bind_lst'(_,l3(_,_,_,l3(_,_,pred_true,l3(
        '$avl_packed'(
            packed_node((4,X4),_,
            packed_node((1,X1),_,
            packed_leaf((0,X0)),
            packed_node((2,X2),_,_,
            packed_leaf((3,X3)))),
            packed_node((7,X7),_,
            packed_node((5,X5),_,_,
            packed_leaf((6,X6))),
            packed_node((8,X8),_,_,
            packed_leaf((9,X9))
                )))),_,_,[_,_]))))
        ),
    X0>1,
    X1=<1,
    X2=<1,
    X3=<1,
    X4=<1,
    X5=<1,
    X6=<1,
    X7=<1,
    X8=<1,
    X9=<1.
    

trans_crowds_10_3(S1,S2,0.909) :- 
    transition(S1,'Is_a_good_member'(pred_true),_,S2). 

trans_crowds_10_3(S1,S2,0.091) :-
    transition(S1,'Is_a_good_member'(pred_false),_,S2). 

trans_crowds_10_3(S1,S2,0.8) :- 
    transition(S1,'Forward_or_deliver'(pred_true),_,S2). 

trans_crowds_10_3(S1,S2,0.2) :-
    transition(S1,'Forward_or_deliver'(pred_false),_,S2). 

trans_crowds_10_3(S1,S2,0.1) :-
    transition(S1,'Record'(_),_,S2).  

trans_crowds_10_3(S1,S2,1.0) :- 
    (transition(S1,'New_protocol',_,S2)
    ; transition(S1,'Start_protocol',_,S2)
    ; transition(S1,'Eavesdrop',_,S2)
    ; transition(S1,'Deliver',_,S2)
    ; transition(S1,'Restart',_,S2)).

state_crowds_10_3(E):-packed_visited_expression(E,_).
