% File to translate the state space into something readable for the tester
% WORKING WITH SICSTUS

:- module(translate_model_SICS,[start/1,prop/2,state/1,trans/3]).

:- use_module(leader_election_to_LTS_SICS,[start_big/1,prop_big/2,state_big/1,trans_big/3]).
:- use_module(simplemodelloop,[start_loop/1,prop_loop/2,state_loop/1,trans_loop/3]).
:- use_module(crowds_5_3_to_LTS_SICS,[start_crowds/1,prop_crowds/2,state_crowds/1,trans_crowds/3]).

%########################################################

% This part differ depending on your prolog implementation
:- use_module('../testing_SICS',[choose_model/1]).

%########################################################

state(S) :-
    choose_model(X),
    (X = loop -> state_loop(S) ;
    X = big -> state_big(S) ;
    X = crowds -> state_crowds(S)).

prop(F,E) :-
    choose_model(X),
    (X = loop -> prop_loop(F,E) ;
    X = big -> prop_big(F,E) ;
    X = crowds -> prop_crowds(F,E)).

trans(E1,E2,P) :-
    choose_model(X),
    (X = loop -> trans_loop(E1,E2,P) ;
    X = big -> trans_big(E1,E2,P) ;
    X = crowds -> trans_crowds(E1,E2,P)).

start(S) :-
    choose_model(X),
    (X = loop -> start_loop(S) ;
    X = big -> start_big(S) ;
    X = crowds -> start_crowds(S)).