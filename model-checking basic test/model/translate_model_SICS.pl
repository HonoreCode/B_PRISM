% File to translate the state space into something readable for the tester
% VALID WITH SICSTUS

:- module(translate_model_SICS,[start/1,prop/2,state/1,trans/3]).

:- use_module(state_space_to_LTS,
    [start_big/1,prop_big/2,state_big/1,trans_big/3]).
:- use_module(simplemodelloop,
    [start_loop/1,prop_loop/2,state_loop/1,trans_loop/3]).

%########################################################

% This part differ depending on your prolog implementation
:- use_module('../testing_SICS',[choose_model/1]).

%########################################################

state(S) :-
    choose_model(big),
    state_big(S).
state(S) :-
    choose_model(loop),
    state_loop(S).

prop(F,E) :-
    choose_model(big),
    prop_big(F,E).
prop(F,E) :-
    choose_model(loop),
    prop_loop(F,E).

trans(E1,E2,P) :-
    choose_model(big),
    trans_big(E1,E2,P).
trans(E1,E2,P) :-
    choose_model(loop),
    trans_loop(E1,E2,P).

start(S) :-
    choose_model(big),
    start_big(S).
start(S) :-
    choose_model(loop),
    start_loop(S).