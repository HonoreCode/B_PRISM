:- module(simplemodel,[start_simple/1,prop_simple/2,state_simple/1,trans_simple/3]).

start_simple(0).

state_simple(0).
state_simple(1).
state_simple(2).

trans_simple(0,1,0.5).
trans_simple(0,0,0.5).
trans_simple(1,0,0.5).
trans_simple(0,2,0.5).

prop_simple(X,X).