%####################################################*

% Test file to compare the efficiency of 2 
% different model-checker for bounded formulas.

% To run the test, query :
% :- test_dynamic_vs_normal.


% Tests are based on the following models :

%    - "loop" is a small model with a loop

%    - "big" is a big model from Prism's Benchmark suite :
%          (Synchronous leader election protocol,
%           parameters : N=4, K=8
%           see http://www.prismmodelchecker.org/casestudies/synchronous_leader.php)

%    - "crowds_5_3" is an even bigger model from Prism's Benchmark suite :
%          (Crowds Protocol,
%           parameters : PF=0.8, badC=0.167, TotalRuns=3, CrowdSize=5
%           see http://www.prismmodelchecker.org/casestudies/crowds.php)



%                WORKING WITH SICSTUS

%####################################################

:- module(bounded_testing_dynamic_vs_normal,[choose_model/1,test_dynamic_vs_normal/0]).

:- use_module(bounded_dtmc_model_checking_dynamic_versus_normal,[sat/2]).
:- use_module(library(clpr),[{}/1]).

:- dynamic choose_model/1.

% Tests for the dynamic state exploration, which is not using findall


    % Tests with synchronous leader election (big) model 

test_dynamic_sup_5_05 :-
    print('Begin dynamic model-checking for the big model with K=5, P=0.5 and greater operator\n'),
    statistics(runtime,[T0|_]),
    (sat(probformula(greater,0.5,dyn(uk(true,5,p(elect)))),0) -> print('dynamic model-check SUCCEEDS\n')
        ; print('dynamic model-check FAILS\n')
    ),!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish dynamic model-checking for the big model with K=5, P=0.5 and greater operator. \n It took ~3d sec.~n \n',[T]),
    print('Begin normal model-checking for the big model with K=5, P=0.5 and greater operator\n'),
    statistics(runtime,[T2|_]),
    (sat(probformula(greater,0.5,uk(true,5,p(elect))),0) -> print('normal model-check SUCCEEDS\n')
        ; print('normal model-check FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish normal model-checking for the big model with K=5, P=0.5 and greater operator. \n It took ~3d sec.~n \n',[T_prime]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the dynamic model-checking is ~3d sec.~n faster than the normal model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the normal model-checking is ~3d sec.~n faster than the dynamic model-checking \n\n\n',[T_comp])
        ).

test_dynamic_ssup_7_02 :-
    print('Begin dynamic model-checking for the big model with K=7, P=0.2 and strictlygreater operator\n'),
    statistics(runtime,[T0|_]),
    (sat(probformula(strictlygreater,0.2,dyn(uk(true,7,p(elect)))),0) -> print('dynamic model-check SUCCEEDS\n')
        ; print('Dynamic model-check FAILS\n')
    ),!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish dynamic model-checking for the big model with K=7, P=0.2 and strictlygreater operator. \n It took ~3d sec.~n \n',[T]),
    print('Begin normal model-checking for the big model with K=7, P=0.2 and strictlygreater operator\n'),
    statistics(runtime,[T2|_]),
    (sat(probformula(strictlygreater,0.2,uk(true,7,p(elect))),0) -> print('normal model-check SUCCEEDS\n')
        ; print('Normal model-check FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish normal model-checking for the big model with K=7, P=0.2 and strictlygreater operator. \n It took ~3d sec.~n \n',[T_prime]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the dynamic model-checking is ~3d sec.~n faster than the normal model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the normal model-checking is ~3d sec.~n faster than the dynamic model-checking \n\n\n',[T_comp])
        ).

test_dynamic_inf_6_097 :-
    print('Begin dynamic model-checking for the big model with K=6, P=0.97 and less operator\n'),
    statistics(runtime,[T0|_]),
    (sat(probformula(less,0.97,dyn(uk(true,6,p(elect)))),0) -> print('dynamic model-check SUCCEEDS\n')
        ; print('Dynamic model-check FAILS\n')
    ),!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish dynamic model-checking for the big model with K=6, P=0.97 and less operator. \n It took ~3d sec.~n \n',[T]),
    print('Begin normal model-checking for the big model with K=6, P=0.97 and less operator\n'),
    statistics(runtime,[T2|_]),
    (sat(probformula(less,0.97,uk(true,6,p(elect))),0) -> print('normal model-check SUCCEEDS\n')
        ; print('Normal model-check FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish normal model-checking for the big model with K=6, P=0.97 and less operator. \n It took ~3d sec.~n \n',[T_prime]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the dynamic model-checking is ~3d sec.~n faster than the normal model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the normal model-checking is ~3d sec.~n faster than the dynamic model-checking \n\n\n',[T_comp])
        ).

test_dynamic_equal_6_097 :-
    print('Begin dynamic model-checking for the big model with K=6, P=0.95703125 and equal operator\n'),
    statistics(runtime,[T0|_]),
    (sat(probformula(equal,0.95703125,dyn(uk(true,6,p(elect)))),0) -> print('dynamic model-check SUCCEEDS\n')
        ; print('Dynamic model-check FAILS\n')
    ),!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish dynamic model-checking for the big model with K=6, P=0.95703125 and equal operator. \n It took ~3d sec.~n \n',[T]),
    print('Begin normal model-checking for the big model with K=6, P=0.95703125 and equal operator\n'),
    statistics(runtime,[T2|_]),
    (sat(probformula(equal,0.95703125,uk(true,6,p(elect))),0) -> print('normal model-check SUCCEEDS\n')
        ; print('Normal model-check FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish normal model-checking for the big model with K=6, P=0.95703125 and equal operator. \n It took ~3d sec.~n \n',[T_prime]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the dynamic model-checking is ~3d sec.~n faster than the normal model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the normal model-checking is ~3d sec.~n faster than the dynamic model-checking \n\n\n',[T_comp])
        ).

   % Tests with loop model 
test_dynamic_next_07_equal :-
    print('Begin next formula dynamic model-checking with P=0.7 and equal operator\n'),
    statistics(runtime,[T0|_]),
    (sat(probformula(equal,P,dyn(x(probformula(equal,0.5,dyn(x(p(b))))))),0),P=0.9 ->
        print('Next formula dynamic checking SUCCEEDS\n')
        ; print('Next formula dynamic checking FAILS\n')
    ),!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish next formula dynamic model-checking with P=0.7 and equal operator. \n It took ~3d sec.~n \n',[T]),
    print('Begin next formula normal model-checking with P=0.7 and equal operator\n'),
    statistics(runtime,[T2|_]),
    (sat(probformula(equal,0.9,x(probformula(equal,0.5,x(p(b))))),0) -> 
        print('Next formula normal checking SUCCEEDS\n')
        ; print('Next formula normal checking FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish next formula normal model-checking with P=0.7 and equal operator. \n It took ~3d sec.~n \n',[T_prime]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the dynamic model-checking is ~3d sec.~n faster than the normal model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the normal model-checking is ~3d sec.~n faster than the dynamic model-checking \n\n\n',[T_comp])
        ).

test_dynamic_next_07_sinf :-
    print('Begin next formula dynamic model-checking with P=0.7 and strictly inferior operator\n'),
    statistics(runtime,[T0|_]),
    (sat(probformula(strictlyless,0.99,dyn(x(probformula(equal,0.5,dyn(x(p(b))))))),0) ->
        print('Next formula dynamic checking SUCCEEDS\n')
        ; print('Next formula dynamic checking FAILS\n')
    ),!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish next formula dynamic model-checking with P=0.7 and strictly inferior operator. \n It took ~3d sec.~n \n',[T]),
    print('Begin next formula normal model-checking with P=0.7 and strictly inferior operator\n'),
    statistics(runtime,[T2|_]),
    (sat(probformula(strictlyless,0.99,x(probformula(equal,0.5,x(p(b))))),0) -> 
        print('Next formula normal checking SUCCEEDS\n')
        ; print('Next formula normal checking FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish next formula normal model-checking with P=0.7 and strictly inferior operator. \n It took ~3d sec.~n \n',[T_prime]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the dynamic model-checking is ~3d sec.~n faster than the normal model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the normal model-checking is ~3d sec.~n faster than the dynamic model-checking \n\n\n',[T_comp])
        ).

test_dynamic_eventually :-
    print('Begin eventually formula dynamic model-checking with k=15\n'),
    statistics(runtime,[T0|_]),
    (sat(probformula(equal,P,dyn(f(15,p(b)))),0),print(P),nl ->
        print('Eventually formula dynamic checking SUCCEEDS\n')
        ; print('Eventually formula dynamic checking FAILS\n')
    ),!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish eventually formula dynamic model-checking with k=15 \n It took ~3d sec.~n \n',[T]),
    print('Begin eventually formula normal model-checking with k=15\n'),
    statistics(runtime,[T2|_]),
    (sat(probformula(equal,P,f(15,p(b))),0) -> 
        print('Eventually formula normal checking SUCCEEDS\n')
        ; print('Eventually formula normal checking FAILS\n')
    ),
    statistics(runtime,[T3|_]),
    T_prime is T3-T2,
    format('Finish eventually formula normal model-checking with k=15. \n It took ~3d sec.~n \n',[T_prime]),
    (T<T_prime ->
        T_comp is T_prime-T,
        format('For this formula, the dynamic model-checking is ~3d sec.~n faster than the normal model-checking \n\n\n',[T_comp])
    ;   T_comp is T-T_prime,
        format('For this formula, the normal model-checking is ~3d sec.~n faster than the dynamic model-checking \n\n\n',[T_comp])
        ).

test_dynamic_vs_normal :-
    asserta(choose_model(big)),
    print('Begin dynamic model-checking tests\n\n'),
    statistics(runtime,[T0|_]),
    test_dynamic_sup_5_05,!,
    test_dynamic_ssup_7_02,!,
    test_dynamic_inf_6_097,!,
    test_dynamic_equal_6_097,!,
    retract(choose_model(big)),
    asserta(choose_model(loop)),
    test_dynamic_next_07_sinf,!,
    test_dynamic_next_07_equal,!,
    test_dynamic_eventually,!,
    statistics(runtime,[T1|_]),
    T is T1-T0,
    format('Finish dynamic model-checking tests. It took ~3d sec.~n \n\n',[T]),
    retract(choose_model(loop)).