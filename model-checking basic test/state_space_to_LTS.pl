:- use_module(clpr,[{}/1]).

%**************************************
% We load the model
% Here the model is issued from a B-model using ProB's reachability analysis
:- [slep_state_space].

%**************************************
% Formulas to try on :

% The leader will be elected in one round :
% sat(prob_formula(eq,P,u(true,5,p(elect))),0).

% The leader will eventually be elected :
% sat(prob_formula(eq,P,u(true,p(elect))),0).

%**************************************

% Definition of the labelled transition system from the state space
% THIS PART MAY DEPEND ON THE MODEL
start(0).

prop(E,elect) :- 
packed_visited_expression(
    E,
    '$bind_lst'(_,
    l3('$avl_packed'(
        packed_node((2,'$fd_MODES4'),1,
        packed_node((1,'$fd_MODES4'),0,empty,empty),
        packed_node((3,'$fd_MODES4'),1,empty,
        packed_node((4,'$fd_MODES4'),0,empty,empty)))),_,_,_)
        )).

trans(S1,S2,0.000244140625) :- 
    transition(S1,pick(_),_,S2). 

trans(S1,S2,1.0) :- 
    transition(S1,read1,_,S2);
    transition(S1,read2,_,S2);
    transition(S1,done,_,S2);
    transition(S1,retry,_,S2);
    transition(S1,loop,_,S2).

state(E):-packed_visited_expression(E,_).
