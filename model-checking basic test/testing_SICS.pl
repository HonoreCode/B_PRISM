%####################################################*

% Test file to verify some formulas over 2 models :

%    - "loop" is a small model with a loop

%    - "big" is a big model from Prism's Benchmark suite :
%          (Synchronous leader election protocol,
%           parameters : N=4, K=8
%           see http://www.prismmodelchecker.org/casestudies/synchronous_leader.php)

%    - "crowds_5_3" is an even bigger model from Prism's Benchmark suite :
%          (Crowds Protocol,
%           parameters : PF=0.8, badC=0.167, TotalRuns=3, CrowdSize=5
%           see http://www.prismmodelchecker.org/casestudies/crowds.php)



%   - If you want to test all the tree models, run "?- full_test.", but
%   keep in mind that it will take at least a minute

%   - If you want to do a quick test on the first two models,
%   run "?- partial_test."


%                WORKING WITH SICSTUS

%####################################################

:- module(testing_SICS,[choose_model/1,partial_test/0,full_test/0/*,test_loop/0,test_big/0,test_crowds_5_3/3*/,test_crowds_5_3_positive/0]).

:- use_module(dtmc_model_checking_SICS,[sat/2]).

:- use_module(library(clpr),[{}/1]).

% Small predicate used to compare two floats lists
compare_floats_list([],[]).
compare_floats_list([E1|L1],[E2|L2]) :-
    {E2-0.000000000000005 =< E1},
    {E1 =< E2+0.000000000000005},
    compare_floats_list(L1,L2).

approx_float(F1,F2):-
    {F1 =< 0.00001 + F2},
    {F2 =< 0.00001 + F1}.

:- dynamic choose_model/1.


%******************************************

% TESTS FOR THE LOOP MODEL

% Testing a formula with the "next" operator for the loop model
test_loop_next :- 
    print('Begin loop next formula checking\n'),
    statistics(runtime,[T0|_]),
    (sat(prob_formula(eq,0.9,x(prob_formula(eq,0.5,x(p(b))))),0) -> 
        print('Next formula succeeds\n')
        ; print('Next formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish loop next formula checking. It took ~3d sec.~n \n',[T]).

% Testing a formula with the "until" operator for the loop model
test_loop_until :- 
    print('Begin loop until formula checking\n'),
    statistics(runtime,[T0|_]),
    findall(P,sat(prob_formula(eq,P,u(not(p(a)),p(b))),_S),List_P),
    (compare_floats_list(
        List_P,
        [0.783333333333334,0.0,0.870370370370371,0.0,1.0,0.944444444444445]
        ) -> print('Until formula succeeds\n')
            ; print('Until formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish loop until formula checking. It took ~3d sec.~n \n',[T]).

% Testing a formula with the "until-bounded" operator for the loop model
test_loop_until_bounded :-
    print('Begin loop until bounded formula checking\n'),
    statistics(runtime,[T0|_]),
    (findall(
        P,
        sat(prob_formula(eq,P,u(not(p(a)),5,p(b))),_S),
        [0.73566,0.0,0.84304,0.0,1.0,0.90652]
        ) -> print('Until-bounded formula succeeds\n')
            ; print('Until-bounded formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish loop until bounded formula checking. It took ~3d sec.~n \n',[T]).

% Testing a formula with the "always" operator for the loop model
test_loop_always :-
    print('Begin loop always formula checking\n'),
    statistics(runtime,[T0|_]),
    (findall(
        S,
        sat(prob_formula(sup,0.1,g(not(p(b)))),S),
        [0,1,2,3]
        ) -> print('Always formula succeeds\n')
            ; print('Always formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish loop always formula checking. It took ~3d sec.~n \n',[T]).

% Testing a formula with the "eventually" operator for the loop model
test_loop_eventually :- 
    print('Begin loop eventually formula checking\n'),
    statistics(runtime,[T0|_]),
    findall(P,sat(prob_formula(eq,P,f(5,p(b))),_S),List_P),
    (compare_floats_list(
        List_P,
        [0.75978,0.28368,0.84304,0.0,1.0,0.90652]
        ) -> print('Eventually formula succeeds\n')
            ; print('Eventually formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish loop eventually formula checking. It took ~3d sec.~n \n',[T]).

test_loop :- 
    assert(choose_model(loop)),
    print('Begin loop model tests\n\n'),
    statistics(runtime,[T0|_]),
    test_loop_next,
    test_loop_until,
    test_loop_until_bounded,
    test_loop_always,
    test_loop_eventually,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish loop model tests. It took ~3d sec.~n \n\n',[T]),
    retract(choose_model(loop)).

%**********************************************

% TESTS FOR THE SYNCHRONOUS LEADER ELECTION MODEL

% The leader will be elected in one round :
test_big_until_bounded :-
    print('Begin leader election until bounded formula checking\n'),
    statistics(runtime,[T0|_]),
    (sat(
        prob_formula(
            eq,
            0.95703125,
            u(true,5,p(elect))),
        0) -> print('Until-bounded formula succeeds\n')
            ; print('Until-bounded formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish leader election until bounded formula checking. \n The result is ~h \n It took ~3d sec.~n \n',[0.95703125,T]).

% The leader will eventually be elected :
test_big_until :-
    print('Begin leader election until formula checking\n'),
    statistics(runtime,[T0|_]),
    (sat(
        prob_formula(
            eq,
            1.0,
            u(true,p(elect))),
        0) -> print('Until formula succeeds\n')
            ; print('Until formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish leader election until formula checking. \n The result is ~h \n It took ~3d sec.~n \n',[1.0,T]).

test_big :- 
    assert(choose_model(big)),
    print('Begin leader election model tests\n\n'),
    statistics(runtime,[T0|_]),
    test_big_until,
    test_big_until_bounded,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish leader election model tests. It took ~3d sec.~n \n\n',[T]),
    retract(choose_model(big)).

%***********************************************

% TESTS FOR THE CROWDS PROTOCOL MODEL WITH PAREMETERS 5 & 3

% The adversary will eventually observe the real sender more than once
test_crowds_5_3_positive :-
    print('Begin crowds_5_3 positive formula checking\n'),
    statistics(runtime,[T0|_]),
    sat(prob_formula(eq,P,f(p(positive))),0),
    (approx_float(P,0.13834) -> print('positive formula succeeds\n')
        ; print('positive formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_5_3 positive formula checking. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]).

% The adversary will eventually observe someone other than the real sender more than once
test_crowds_5_3_false_positive :-
    print('Begin crowds_5_3 false positive formula checking\n'),
    statistics(runtime,[T0|_]),
    sat(prob_formula(eq,P,f(p(false_positive))),0),
    (approx_float(P,0.05104) -> print('false positive formula succeeds\n')
        ; print('false positive formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_5_3 false positive formula checking. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]).

% The adversary will eventually observe the real sender (and only him) more than once
test_crowds_5_3_confident :-
    print('Begin crowds_5_3 confident formula checking\n'),
    statistics(runtime,[T0|_]),
    sat(prob_formula(eq,P,f(p(confidence))),0),
    (approx_float(P,0.13834) -> print('confident formula succeeds\n')
        ; print('confident formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_5_3 confident formula checking. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]).

test_crowds_5_3 :-
    assert(choose_model(crowds_5_3)),
    print('Begin crowds_5_3 protocol model tests\n\n'),
    statistics(runtime,[T0|_]),
    test_crowds_5_3_positive,
    test_crowds_5_3_false_positive,
    test_crowds_5_3_confident,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_5_3 protocol tests. It took ~3d sec.~n \n\n',[T]),
    retract(choose_model(crowds_5_3)).


%#################################################################

% TESTS FOR THE CROWDS PROTOCOL MODEL WITH PAREMETERS 10 & 3

% The adversary will eventually observe the real sender more than once
test_crowds_10_3_positive :-
    print('Begin crowds_10_3 positive formula checking\n'),
    statistics(runtime,[T0|_]),
    sat(prob_formula(eq,P,f(p(positive))),0),
    (approx_float(P,0.03679) -> print('positive formula succeeds\n')
        ; print('positive formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_10_3 positive formula checking. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]).

% The adversary will eventually observe someone other than the real sender more than once
test_crowds_10_3_false_positive :-
    print('Begin crowds_10_3 false positive formula checking\n'),
    statistics(runtime,[T0|_]),
    sat(prob_formula(eq,P,f(p(false_positive))),0),
    (approx_float(P,0.01563) -> print('false positive formula succeeds\n')
        ; print('false positive formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_10_3 false positive formula checking. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]).

% The adversary will eventually observe the real sender (and only him) more than once
test_crowds_10_3_confident :-
    print('Begin crowds_10_3 confident formula checking\n'),
    statistics(runtime,[T0|_]),
    sat(prob_formula(eq,P,f(p(confidence))),0),
    (approx_float(P,0.03679) -> print('confident formula succeeds\n')
        ; print('confident formula fails\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_10_3 confident formula checking. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]).

test_crowds_10_3 :-
    assert(choose_model(crowds_10_3)),
    print('Begin crowds_10_3 protocol model tests\n\n'),
    statistics(runtime,[T0|_]),
    test_crowds_10_3_positive,
    test_crowds_10_3_false_positive,
    test_crowds_10_3_confident,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_10_3 protocol tests. It took ~3d sec.~n \n\n',[T]),
    retract(choose_model(crowds_10_3)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% to test the first two models
partial_test :- test_loop,test_big.

% to do a full test, but remind that it may take long (around a minute and half)
full_test :- test_loop,!,test_big,!,test_crowds_5_3,!,test_crowds_10_3.