% File to translate the state space into something readable for the tester
% WORKING WITH SICSTUS

:- module(bounded_translate_model_dyn,[start/1,prop/2,state/1,trans/3]).

:- use_module('../../../examples/synchronous_leader_election/Prolog/leader_election_to_LTS_SICS',[start_big/1,prop_big/2,state_big/1,trans_big/3]).
:- use_module('../../../examples/simple_loop/Prolog/simplemodelloop',[start_loop/1,prop_loop/2,state_loop/1,trans_loop/3]).
:- use_module('../../../examples/simple_model/Prolog/simplemodel',[start_simple/1,prop_simple/2,state_simple/1,trans_simple/3]).
:- use_module('../../../examples/crowds_protocol/Prolog/crowds_5_3_to_LTS_SICS',[start_crowds_5_3/1,prop_crowds_5_3/2,state_crowds_5_3/1,trans_crowds_5_3/3]).
:- use_module('../../../examples/crowds_protocol/Prolog/crowds_10_3_to_LTS_SICS',[start_crowds_10_3/1,prop_crowds_10_3/2,state_crowds_10_3/1,trans_crowds_10_3/3]).

%########################################################

% This part differ depending on your prolog implementation
:- use_module(bounded_testing_dynamic_vs_normal,[choose_model/1]).

%########################################################

state(S) :-
    choose_model(X),
    (X = loop -> state_loop(S) 
        ; X = big -> state_big(S) 
        ; X = simple -> state_simple(S)  
        ; X = crowds_5_3 -> state_crowds_5_3(S)
        ; X = crowds_10_3 -> state_crowds_10_3(S)
    ).

prop(F,E) :-
    choose_model(X),
    (X = loop -> prop_loop(F,E) 
    ; X = big -> prop_big(F,E) 
    ; X = simple -> prop_simple(F,E) 
    ; X = crowds_5_3 -> prop_crowds_5_3(F,E)
    ; X = crowds_10_3 -> prop_crowds_10_3(F,E)
    ).

trans(E1,E2,P) :-
    choose_model(X),
    (X = loop -> trans_loop(E1,E2,P) 
    ; X = big -> trans_big(E1,E2,P) 
    ; X = simple -> trans_simple(E1,E2,P) 
    ; X = crowds_5_3 -> trans_crowds_5_3(E1,E2,P)
    ; X = crowds_10_3 -> trans_crowds_10_3(E1,E2,P)
    ).

start(S) :-
    choose_model(X),
    (X = loop -> start_loop(S) 
    ; X = big -> start_big(S) 
    ; X = simple -> start_simple(S)
    ; X = crowds_5_3 -> start_crowds_5_3(S)
    ; X = crowds_10_3 -> start_crowds_10_3(S)
    ).