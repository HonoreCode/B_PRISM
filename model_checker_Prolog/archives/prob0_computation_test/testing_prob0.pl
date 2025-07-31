%####################################################*

% Test file to verify some formulas over the following models :

%    - "loop" is a small model with a loop

%    - "small" is an even smaller model

%    - "big" is a big model from Prism's Benchmark suite :
%          (Synchronous leader election protocol,
%           parameters : N=4, K=8
%           see http://www.prismmodelchecker.org/casestudies/synchronous_leader.php)

%    - "crowds_5_3" is an bigger model from Prism's Benchmark suite :
%          (Crowds Protocol,
%           parameters : PF=0.8, badC=0.167, TotalRuns=3, CrowdSize=5
%           see http://www.prismmodelchecker.org/casestudies/crowds.php)

%    - "crowds_10_3" is an even bigger model from Prism's Benchmark suite :
%          (Crowds Protocol,
%           parameters : PF=0.8, badC=0.091, TotalRuns=3, CrowdSize=10
%           see http://www.prismmodelchecker.org/casestudies/crowds.php)

%   - If you want to test all the different models, run "?- full_test."


%                WORKING WITH SICSTUS

%####################################################

:- module(testing_SICS,[choose_model/1,full_test/0,test_simple/0,test_loop/0,test_big/0,test_crowds_5_3/0,test_crowds_10_3/0]).

:- use_module(dtmc_model_checking_SICS,[sat/2,sat/1,state/1]).

:- use_module(library(clpr),[{}/1]).

% Small predicate used to compare two floats lists
% Needed because there are approximation error using float representation
compare_floats_list([],[]).
compare_floats_list([E1|L1],[E2|L2]) :-
    {E2-0.000000000000005 =< E1},
    {E1 =< E2+0.000000000000005},
    compare_floats_list(L1,L2).


% Small predicate used to compare two floats
% Needed because there is a little difference 
% between CLPFD equations solving and PRISM's one
approx_float(F1,F2):-
    {F1 =< 0.00001 + F2},
    {F2 =< 0.00001 + F1}.


:- dynamic choose_model/1.

%******************************************

% TESTS FOR THE LOOP MODEL


% Testing a formula with the "until" operator for the loop model
test_loop_until :- 
    print('Begin loop until formula checking version 1\n'),
    statistics(runtime,[T0|_]),
    findall(P,(state(S),sat(probformula(equal,P,u1(not(p(a)),p(b))),S)),List_P),
    (compare_floats_list(
        List_P,
        [0.783333333333334,0.0,0.870370370370371,0.0,1.0,0.944444444444445]
        ) -> print('Until formula version 1 SUCCEEDS\n')
            ; print('Until formula version 1 FAILS\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish loop until formula checking version 1.\n It took ~3d sec.~n \n',[T]),
    print('Begin loop until formula checking version 2\n'),
    statistics(runtime,[T2|_]),
    findall(P,(state(S),sat(probformula(equal,P,u2(not(p(a)),p(b))),S)),List_P),
    (compare_floats_list(
        List_P,
        [0.783333333333334,0.0,0.870370370370371,0.0,1.0,0.944444444444445]
        ) -> print('Until formula version 2 SUCCEEDS\n')
            ; print('Until formula version 2 FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish loop until formula checking version 2.\n It took ~3d sec.~n \n',[T]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the version 1 model-checking is ~3d sec.~n faster than the version 2 model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the version 2 model-checking is ~3d sec.~n faster than the version 1 model-checking \n\n\n',[T_comp])
        )
    .

% Testing a formula with the "always" operator for the loop model
test_loop_always :-
    print('Begin loop always formula checking version 1\n'),
    statistics(runtime,[T0|_]),
    (findall(
        S,
        (
            state(S),
            sat(probformula(greater,0.1,g1(not(p(b)))),S)
        ),
        [0,1,2,3]
        ) -> print('Always formula version 1 SUCCEEDS\n')
            ; print('Always formula version 1 FAILS\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish loop always formula checking version 1. It took ~3d sec.~n \n',[T]),
    print('Begin loop always formula checking version 2 \n'),
    statistics(runtime,[T2|_]),
    (findall(
        S,
        (
            state(S),
            sat(probformula(greater,0.1,g2(not(p(b)))),S)
        ),
        [0,1,2,3]
        ) -> print('Always formula version 2 SUCCEEDS\n')
            ; print('Always formula version 2 FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish loop always formula version 2 checking. It took ~3d sec.~n \n',[T]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the version 1 model-checking is ~3d sec.~n faster than the version 2 model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the version 2 model-checking is ~3d sec.~n faster than the version 1 model-checking \n\n\n',[T_comp])
        )
    .

test_loop :- 
    assert(choose_model(loop)),
    print('Begin loop model tests\n\n'),
    statistics(runtime,[T0|_]),
    test_loop_until,!,
    test_loop_always,!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish loop model tests. It took ~3d sec.~n \n\n',[T]),
    retract(choose_model(loop)).

% TESTS FOR THE SYNCHRONOUS LEADER ELECTION MODEL

% The leader will eventually be elected :
test_big_until :-
    print('Begin leader election until formula checking version 1\n'),
    statistics(runtime,[T0|_]),
    (sat(
        probformula(
            equal,
            1.0,
            u1(true,p(elect))),
        0) -> print('Until formula version 1 SUCCEEDS\n')
            ; print('Until formula version 1 FAILS\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish leader election until formula checking version 1. \n The result is ~h \n It took ~3d sec.~n \n',[1.0,T]),
    print('Begin leader election until formula checking version 2\n'),
    statistics(runtime,[T2|_]),
    (sat(
        probformula(
            equal,
            1.0,
            u2(true,p(elect))),
        0) -> print('Until formula version 2 SUCCEEDS\n')
            ; print('Until formula version 2 FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish leader election until formula checking version 2. \n The result is ~h \n It took ~3d sec.~n \n',[1.0,T]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the version 1 model-checking is ~3d sec.~n faster than the version 2 model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the version 2 model-checking is ~3d sec.~n faster than the version 1 model-checking \n\n\n',[T_comp])
        )
    .

test_big :- 
    assert(choose_model(big)),
    print('Begin leader election model tests\n\n'),
    statistics(runtime,[T0|_]),
    test_big_until,!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish leader election model tests. It took ~3d sec.~n \n\n',[T]),
    retract(choose_model(big)).


%***********************************************

% TESTS FOR THE CROWDS PROTOCOL MODEL WITH PAREMETERS 5 & 3

% The adversary will eventually observe the real sender more than once

test_crowds_5_3_positive :-
    print('Begin crowds_5_3 positive formula checking version 1\n'),
    statistics(runtime,[T0|_]),
    sat(probformula(equal,P,f1(p(positive))),0),
    (approx_float(P,0.13834) -> print('positive formula version 1 SUCCEEDS\n')
        ; print('positive formula version 1 FAILS\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_5_3 positive formula checking version 1. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    print('Begin crowds_5_3 positive formula checking version 2\n'),
    statistics(runtime,[T2|_]),
    sat(probformula(equal,P,f2(p(positive))),0),
    (approx_float(P,0.13834) -> print('positive formula version 2 SUCCEEDS\n')
        ; print('positive formula version 2 FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish crowds_5_3 positive formula checking version 2. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the version 1 model-checking is ~3d sec.~n faster than the version 2 model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the version 2 model-checking is ~3d sec.~n faster than the version 1 model-checking \n\n\n',[T_comp])
        )
    .

test_crowds_5_3_false_positive :-
    print('Begin crowds_5_3 false_positive formula checking version 1\n'),
    statistics(runtime,[T0|_]),
    sat(probformula(equal,P,f1(p(false_positive))),0),
    (approx_float(P,0.05104) -> print('false_positive formula version 1 SUCCEEDS\n')
        ; print('false_positive formula version 1 FAILS\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_5_3 false_positive formula checking version 1. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    print('Begin crowds_5_3 false_positive formula checking version 2\n'),
    statistics(runtime,[T2|_]),
    sat(probformula(equal,P,f2(p(false_positive))),0),
    (approx_float(P,0.05104) -> print('false_positive formula version 2 SUCCEEDS\n')
        ; print('false_positive formula version 2 FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish crowds_5_3 false_positive formula checking version 2. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the version 1 model-checking is ~3d sec.~n faster than the version 2 model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the version 2 model-checking is ~3d sec.~n faster than the version 1 model-checking \n\n\n',[T_comp])
        )
    .

test_crowds_5_3_confident :-
    print('Begin crowds_5_3 confident formula checking version 1\n'),
    statistics(runtime,[T0|_]),
    sat(probformula(equal,P,f1(p(confident))),0),
    (approx_float(P,0.13834) -> print('confident formula version 1 SUCCEEDS\n')
        ; print('confident formula version 1 FAILS\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_5_3 confident formula checking version 1. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    print('Begin crowds_5_3 confident formula checking version 2\n'),
    statistics(runtime,[T2|_]),
    sat(probformula(equal,P,f2(p(confident))),0),
    (approx_float(P,0.13834) -> print('confident formula version 2 SUCCEEDS\n')
        ; print('confident formula version 2 FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish crowds_5_3 confident formula checking version 2. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the version 1 model-checking is ~3d sec.~n faster than the version 2 model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the version 2 model-checking is ~3d sec.~n faster than the version 1 model-checking \n\n\n',[T_comp])
        )
    .

test_crowds_5_3 :-
    assert(choose_model(crowds_5_3)),
    print('Begin crowds_5_3 protocol model tests\n\n'),
    statistics(runtime,[T0|_]),
    test_crowds_5_3_positive,!,
    test_crowds_5_3_false_positive,!,
    test_crowds_5_3_confident,!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_5_3 protocol tests. It took ~3d sec.~n \n\n',[T]),
    retract(choose_model(crowds_5_3)).

%#################################################################

% TESTS FOR THE CROWDS PROTOCOL MODEL WITH PAREMETERS 10 & 3

% The adversary will eventually observe the real sender more than once

test_crowds_10_3_positive :-
    print('Begin crowds_10_3 positive formula checking version 1\n'),
    statistics(runtime,[T0|_]),
    sat(probformula(equal,P,f1(p(positive))),0),
    (approx_float(P,0.03679) -> print('positive formula version 1 SUCCEEDS\n')
        ; print('positive formula version 1 FAILS\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_10_3 positive formula checking version 1. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    print('Begin crowds_10_3 positive formula checking version 2\n'),
    statistics(runtime,[T2|_]),
    sat(probformula(equal,P,f2(p(positive))),0),
    (approx_float(P,0.03679) -> print('positive formula version 2 SUCCEEDS\n')
        ; print('positive formula version 2 FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish crowds_10_3 positive formula checking version 2. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the version 1 model-checking is ~3d sec.~n faster than the version 2 model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the version 2 model-checking is ~3d sec.~n faster than the version 1 model-checking \n\n\n',[T_comp])
        )
    .

test_crowds_10_3_false_positive :-
    print('Begin crowds_10_3 false_positive formula checking version 1\n'),
    statistics(runtime,[T0|_]),
    sat(probformula(equal,P,f1(p(false_positive))),0),
    (approx_float(P,0.01563) -> print('false_positive formula version 1 SUCCEEDS\n')
        ; print('false_positive formula version 1 FAILS\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_10_3 false_positive formula checking version 1. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    print('Begin crowds_10_3 false_positive formula checking version 2\n'),
    statistics(runtime,[T2|_]),
    sat(probformula(equal,P,f2(p(false_positive))),0),
    (approx_float(P,0.01563) -> print('false_positive formula version 2 SUCCEEDS\n')
        ; print('false_positive formula version 2 FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish crowds_10_3 false_positive formula checking version 2. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the version 1 model-checking is ~3d sec.~n faster than the version 2 model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the version 2 model-checking is ~3d sec.~n faster than the version 1 model-checking \n\n\n',[T_comp])
        )
    .

test_crowds_10_3_confident :-
    print('Begin crowds_10_3 confident formula checking version 1\n'),
    statistics(runtime,[T0|_]),
    sat(probformula(equal,P,f1(p(confident))),0),
    (approx_float(P,0.03679) -> print('confident formula version 1 SUCCEEDS\n')
        ; print('confident formula version 1 FAILS\n')
    ),
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_10_3 confident formula checking version 1. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    print('Begin crowds_10_3 confident formula checking version 2\n'),
    statistics(runtime,[T2|_]),
    sat(probformula(equal,P,f2(p(confident))),0),
    (approx_float(P,0.03679) -> print('confident formula version 2 SUCCEEDS\n')
        ; print('confident formula version 2 FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish crowds_10_3 confident formula checking version 2. \n The result is ~h \n It took ~3d sec.~n \n',[P,T]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the version 1 model-checking is ~3d sec.~n faster than the version 2 model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the version 2 model-checking is ~3d sec.~n faster than the version 1 model-checking \n\n\n',[T_comp])
        )
    .

test_crowds_10_3 :-
    assert(choose_model(crowds_10_3)),
    print('Begin crowds_10_3 protocol model tests\n\n'),
    statistics(runtime,[T0|_]),
    test_crowds_10_3_positive,!,
    test_crowds_10_3_false_positive,!,
    test_crowds_10_3_confident,!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish crowds_10_3 protocol tests. It took ~3d sec.~n \n\n',[T]),
    retract(choose_model(crowds_10_3)).