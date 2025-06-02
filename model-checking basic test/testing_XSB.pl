%####################################################

% Test file to verify some formulas over 2 models :
%    - "loop" is a small model with a loop
%    - "big" is a big model from Prism's Benchmark suite :
%          (Synchronous leader election protocol,
%           parameters : N=4, K=8
%           see http://www.prismmodelchecker.org/casestudies/synchronous_leader.php)

%             WORKING WITH XSB PROLOG

%####################################################

:- module(testing_XSB,[choose_model/1,global_test/0/*,test_loop/0,test_big/0*/]).

:- use_module(dtmc_model_checking_XSB,[sat/2]).

:- use_module(clpr,[{}/1]).

% Small predicate used to compare two floats lists
compare_floats_list([],[]).
compare_floats_list([E1|L1],[E2|L2]) :-
    {E2-0.000000000000005 < E1},
    {E1 < E2+0.000000000000005},
    compare_floats_list(L1,L2).

:- dynamic choose_model/1.

%******************************************

% TESTS FOR THE LOOP MODEL

% Testing a formula with the "next" operator for the loop model
test_loop_next :- 
    sat(prob_formula(eq,0.9,x(prob_formula(eq,0.5,x(p(b))))),0).

% Testing a formula with the "until" operator for the loop model
test_loop_until :- 
    findall(P,sat(prob_formula(eq,P,u(not(p(a)),p(b))),_S),List_P),
    compare_floats_list(
        List_P,
        [0.783333333333334,0.0,0.870370370370371,0.0,1.0,0.944444444444445]
    ).

% Testing a formula with the "until-bounded" operator for the loop model
test_loop_until_bounded :-
    findall(
        P,
        sat(prob_formula(eq,P,u(not(p(a)),5,p(b))),_S),
        [0.73566,0.0,0.84304,0.0,1.0,0.90652]
    ).

% Testing a formula with the "always" operator for the loop model
test_loop_always :-
    findall(S,sat(prob_formula(sup,0.1,g(not(p(b)))),S),[0,1,2,3]).

% Testing a formula with the "eventually" operator for the loop model
test_loop_eventually :- 
    findall(P,sat(prob_formula(eq,P,f(5,p(b))),_S),List_P),
    compare_floats_list(
        List_P,
        [0.75978,0.28368,0.84304,0.0,1.0,0.90652]
    ).

test_loop :- 
    assert(choose_model(loop)),
    test_loop_next,
    test_loop_until,
    test_loop_until_bounded,
    test_loop_always,
    test_loop_eventually,
    retract(choose_model(loop)).

%**********************************************

% The leader will be elected in one round :
test_big_until_bounded :-
    sat(prob_formula(eq,0.95703125,u(true,5,p(elect))),0).

% The leader will eventually be elected :
test_big_until :-
    sat(prob_formula(eq,1.0,u(true,p(elect))),0).

test_big :- 
    assert(choose_model(big)),
    test_big_until,
    test_big_until_bounded,
    retract(choose_model(big)).

%***********************************************

global_test :- test_loop,test_big.