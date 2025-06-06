% Simple model with a loop

:- module(simplemodelloop,[start_loop/1,prop_loop/2,state_loop/1,trans_loop/3]).
%:- export start_loop/1.
%*******************************************************
% Some formulas to try on :

% sat(prob_formula(eq,P,x(prob_formula(eq,0.5,x(p(b))))),0).
% sat(prob_formula(eq,P,u(not(p(a)),p(b))),S).
% sat(prob_formula(eq,P,u(not(p(a)),5,p(b))),S).
% sat(prob_formula(sup,0.1,g(not(p(b)))),S).
% sat(prob_formula(eq,P,f(5,p(b))),S).

%********************************************************

state_loop(0).
state_loop(1).
state_loop(2).
state_loop(3).
state_loop(4).
state_loop(5).

start_loop(0).

trans_loop(0,2,0.9).
trans_loop(0,1,0.1).
trans_loop(2,4,0.5).
trans_loop(2,5,0.3).
trans_loop(2,3,0.1).
trans_loop(1,0,0.4).
trans_loop(1,3,0.6).
trans_loop(5,4,0.4).
trans_loop(5,2,0.3).
trans_loop(3,3,1.0).
trans_loop(2,2,0.1).
trans_loop(5,5,0.3).
trans_loop(4,4,1.0).

prop_loop(a,1).
prop_loop(b,4).