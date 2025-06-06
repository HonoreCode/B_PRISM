%**************************************

% Module used :
%    - to load the interesting parts from a B model of the
%      SYNCHRONOUS LEADER ELECTION PROTOCOL translated to Prolog 
%      using ProB's reachability analysis

%    - to assign probabilities to each transition

% See http://www.prismmodelchecker.org/casestudies/synchronous_leader.php 
% for more information
% PARAMETERS : N=4; K=8

%*************************************

:- module(leader_election_to_LTS_XSB,[start_big/1,prop_big/2,state_big/1,trans_big/3]).

:- use_module(usermod(slep_state_space),[packed_visited_expression/2,transition/4]).

%**************************************
% Formulas to try on :

% The leader will be elected in one round :
% sat(prob_formula(eq,P,u(true,5,p(elect))),0).

% The leader will eventually be elected :
% sat(prob_formula(eq,P,u(true,p(elect))),0).

%**************************************

% Definition of the Probabilistic labelled transition system from the state space
% THIS PART MAY DEPEND ON THE MODEL
start_big(0).

prop_big(elect,E) :- 
packed_visited_expression(
    E,
    '$bind_lst'(_,
    l3('$avl_packed'(
        packed_node((2,'$fd_MODES4'),1,
        packed_node((1,'$fd_MODES4'),0,empty,empty),
        packed_node((3,'$fd_MODES4'),1,empty,
        packed_node((4,'$fd_MODES4'),0,empty,empty)))),_,_,_)
        )).

trans_big(S1,S2,0.000244140625) :- 
    transition(S1,pick(_),_,S2). 

trans_big(S1,S2,1.0) :- 
    transition(S1,read1,_,S2);
    transition(S1,read2,_,S2);
    transition(S1,done,_,S2);
    transition(S1,retry,_,S2);
    transition(S1,loop,_,S2).

state_big(E):-packed_visited_expression(E,_).
