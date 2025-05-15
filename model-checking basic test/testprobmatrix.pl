% Model checker for PCTL formulas
% This is heavely based on PRISM model-checker, refer to the lecture 5 on https://www.prismmodelchecker.org/lectures/pmc/
% for more informations.
%
% Some properties to verify :
% ?- sat(prob_formula(sup,0.1,g(not(p(b)))),S).
% ?- sat(prob_formula(eq,P,f(5,p(b))),S).
% ?- sat(prob_formula(eq,P,u(not(p(a)),p(b))),S).
% ?- sat(prob_formula(eq,P,u(not(p(a)),5,p(b))),S).
%
% You can compare the results to the prism model untiltestmodel1.prism along with the property file untiltestproperty.pctl

:- use_module(library(lists),[append/2]).
:- use_module(clpr,[{}/1]).

start(0).

trans(0,2,0.9).
trans(0,1,0.1).
trans(2,4,0.5).
trans(2,5,0.3).
trans(2,3,0.1).
trans(1,0,0.4).
trans(1,3,0.6).
trans(5,4,0.7).
trans(3,3,1.0).
trans(2,2,0.1).
trans(5,5,0.3).
trans(4,4,1.0).

prop(1,a).
prop(4,b).

% Defining probability transition matrix
prob_mat([
    [0.0,0.1,0.9,0.0,0.0,0.0],
    [0.4,0.0,0.0,0.6,0.0,0.0],
    [0.0,0.0,0.1,0.1,0.5,0.3],
    [0.0,0.0,0.0,1.0,0.0,0.0],
    [0.0,0.0,0.0,0.0,1.0,0.0],
    [0.0,0.0,0.0,0.0,0.7,0.3]
    ]).

%##########################################################################

% vector of elements verifying a formula
phi_vect(Formula,Vect) :- 
    phi_vect(Formula,Vect,[0,1,2,3,4,5]).

phi_vect(_,[],[]).
phi_vect(Formula,[V|Vect],[E|States]) :- 
    (sat(Formula,E) -> V=1;V=0),
    phi_vect(Formula,Vect,States).

% matrix-vector product
matrix_Vector_Prod([],_,[]).
matrix_Vector_Prod([Row|Rows],Vect,[I|Result]) :- 
    scalar_Product(Row,Vect,I),
    matrix_Vector_Prod(Rows,Vect,Result).

% scalar product
scalar_Product([],[],0.0).
scalar_Product([R|Row],[V|Vect],I_new) :- 
    scalar_Product(Row,Vect,I),
    {I_new = I + R*V}.

% get the Nth element from a list
pos(L,N,E):-pos(L,N,E,0).

pos([X|_],N,X,N).
pos([_|L],N,E,N1):-N2 is N1+1,pos(L,N,E,N2).

%PCTL Model-checking
sat(Formula):-start(E),sat(Formula,E).

% Classic cases
sat(true,_E).
sat(false,_E):-fail.
sat(p(Property),E):-prop(E,Property).
sat(and(F,G),E) :- sat(F,E), sat(G,E).
sat(or(F,_G),E) :- sat(F,E).
sat(or(_F,G),E) :- sat(G,E).
sat(imp(F,G),E) :- sat(or(not(F),G),E).
sat(not(F),E) :- prob_formula(_)\=F,sat_not(F,E).

sat_not(false,_E).
sat_not(true,_E) :- fail.
sat_not(p(Property),E) :- not(prop(E,Property)).
sat_not(and(F,_G),E) :- sat_not(F,E).
sat_not(and(_F,G),E) :- sat_not(G,E).
sat_not(or(F,G),E) :- sat_not(F,E),sat_not(G,E).
sat_not(imp(F,G),E) :- sat(not(or(not(F),G)),E).
sat_not(not(F),E) :- sat(F,E).

% Probabilistic-formula cases. Operator is =, < >,<= or >=. P is a number between 0 and 1 (or a Variable).
% We procede as follow :
% - First, calculate a probability vector corresponding to the ctl_formula
% - compare probability of the ctl formula starting from state E to P, using the Operator
sat(prob_formula(Operator,P,Ctl_formula),E) :- 
    prob_calc(Ctl_formula,V),
    pos(V,E,P_phi),
    against(P_phi,P,Operator).
sat(not(prob_formula(Operator,P,Ctl_formula)),E) :-
    sat(prob_formula(not(Operator),P,Ctl_formula),E).

% Always bounded formula, we use the dual probabilistic event of f(K,not(F))
sat(prob_formula(Operator,P,g(K,F)),E) :-
    Q is 1-P,
    sat(not(prob_formula(Operator,Q,f(K,not(F)))),E).

% Always formula
sat(prob_formula(Operator,P,g(F)),E) :-
    Q is 1-P,
    sat(not(prob_formula(Operator,Q,f(not(F)))),E).

% Comparing different formulas using a specific operator
against(P_phi,P,eq) :- P_phi = P.
against(P_phi,P,inf) :- P_phi =< P.
against(P_phi,P,sup) :- P_phi >= P.
against(P_phi,P,sinf) :- P_phi < P.
against(P_phi,P,ssup) :- P_phi > P.
against(P_phi,P,not(inf)) :- P_phi > P.
against(P_phi,P,not(sup)) :- P_phi < P.
against(P_phi,P,not(sinf)) :- P_phi >= P.
against(P_phi,P,not(ssup)) :- P_phi =< P.

% Next formula, just a basic matrix-vector computation
prob_calc(x(F),V) :- 
    prob_mat(M),
    phi_vect(F,Phi),
    matrix_Vector_Prod(M,Phi,V).

% Until-bounded formula
% We first compute the states verifying F2
% Then thoses verifying neither F1 and F2
% And we compute K matrix-vector product using prob_until_bound
prob_calc(u(F1,K,F2),V) :- 
    phi_vect(F2,Syes),
    phi_vect(not(or(F1,F2)),Sno),
    prob_mat(Mat),
    prob_until_bound(Mat,K,V,Syes,Sno).

% Until formula
% We compute ALL the states verifying not(E(F1 U F2)) using classic CTL computation techniques (Sno)
% We compute Syes
% We resolve a linear equation using CLP(R)
prob_calc(u(F1,F2),V) :-
    prob0(F1,F2,Sno),
    prob1(F1,F2,Sno,Syes),
    linear_eq(Sno,Syes,V).

% Eventually-bounded formula
prob_calc(f(K,Formula),V) :- prob_calc(u(true,K,Formula),V).

% Eventually formula
prob_calc(f(Formula),V) :- prob_calc(u(true,Formula),V).

% New matrix vector product for the until-bounded formula
% If the element is in Syes (resp Sno), then its probability is 1 (resp 0)
matrix_Vector_Prod_Prime([],[],[],_,[]).
matrix_Vector_Prod_Prime([1|Syes_explored],[0|Sno_explored],[_|Mat],Vect,[1.0|Result]) :-
    matrix_Vector_Prod_Prime(Syes_explored,Sno_explored,Mat,Vect,Result).
matrix_Vector_Prod_Prime([0|Syes_explored],[1|Sno_explored],[_|Mat],Vect,[0.0|Result]):-
    matrix_Vector_Prod_Prime(Syes_explored,Sno_explored,Mat,Vect,Result).
matrix_Vector_Prod_Prime([0|Syes_explored],[0|Sno_explored],[Row|Mat],Vect,[R|Result]):-
    scalar_Product(Row,Vect,R),
    matrix_Vector_Prod_Prime(Syes_explored,Sno_explored,Mat,Vect,Result).

% Until-bounded formula probability vector computation
prob_until_bound(_,0,Syes,Syes,_).
prob_until_bound(Mat,K_new,V_new,Syes,Sno):-
    K_new > 0,
    K is K_new - 1,
    prob_until_bound(Mat,K,V,Syes,Sno),
    matrix_Vector_Prod_Prime(Syes,Sno,Mat,V,V_new).

% Precomputation for the until formula using table computation
% Sno = S\Sat(E(F1 U F2)) in CTL
prob0(F1,F2,Sno):-prob0(F1,F2,Sno,[0,1,2,3,4,5]).

prob0(_F1,_F2,[],[]).
prob0(F1,F2,[I|Sno],[S|States]):-
    (sub_prob0(F1,F2,S) -> I=0;I=1),
    prob0(F1,F2,Sno,States).

:- table sub_prob0/3.

sub_prob0(_F1,F2,S) :- sat(F2,S).
sub_prob0(F1,F2,S):-sat(F1,S),trans(S,S2,_),sub_prob0(F1,F2,S2).

% Second part of the precomputation reusing Sno
prob1(F1,F2,Sno,Syes) :- prob1(F1,F2,Sno,Sno,Syes,0).

prob1(_F1,_F2,[],_Sno,[],_).
prob1(F1,F2,[1|Sno_explored],Sno,[0|Syes_explored],N) :- 
    N_new is N+1, 
    prob1(F1,F2,Sno_explored,Sno,Syes_explored,N_new).
prob1(F1,F2,[0|Sno_explored],Sno,[I|Syes_explored],N) :- 
    N_new is N+1,
    (sub_prob1(F1,Sno,N) -> I=0;I=1),
    prob1(F1,F2,Sno_explored,Sno,Syes_explored,N_new).

:- table sub_prob1/3.

sub_prob1(_F1,Sno,S):-pos(Sno,S,1).
sub_prob1(F1,Sno,S):-
    sat(F1,S),
    trans(S,S2,_),
    sub_prob1(F1,Sno,S2).

% Linear equation to solve
linear_eq(Sno,Syes,V) :- 
    prob_mat(Mat),
    linear_eq(Sno,Syes,V,V,Mat).

linear_eq([],[],[],_,_).
linear_eq([1|Sno],[0|Syes],[0.0|V_explored],V,[_|Mat]) :-
    linear_eq(Sno,Syes,V_explored,V,Mat).
linear_eq([0|Sno],[1|Syes],[1.0|V_explored],V,[_|Mat]) :-
    linear_eq(Sno,Syes,V_explored,V,Mat).
linear_eq([0|Sno],[0|Syes],[P|V_explored],V,[Row|Mat]) :-
    scalar_Product(Row,V,P),
    linear_eq(Sno,Syes,V_explored,V,Mat).