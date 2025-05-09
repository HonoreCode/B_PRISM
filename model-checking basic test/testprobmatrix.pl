:- use_module(library(lists),[append/2]).

start(s0).

trans(s0,s2).
trans(s0,s1).
trans(s2,s4).
trans(s2,s5).
trans(s2,s3).
trans(s1,s0).
trans(s1,s3).
trans(s5,s4).
trans(s3,s3).
trans(s2,s2).
trans(s5,s5).
trans(s4,s4).

prop(s1,a).
prop(s4,b).

% Defining probability transition matrix
prob_mat([
    [0.0,0.1,0.9,0.0,0.0,0.0],
    [0.4,0.0,0.0,0.6,0.0,0.0],
    [0.0,0.0,0.1,0.1,0.5,0.3],
    [0.0,0.0,0.0,1.0,0.0,0.0],
    [0.0,0.0,0.0,0.0,1.0,0.0],
    [0.0,0.0,0.0,0.0,0.7,0.3]
    ]).

% Linking each state to its corresponding row in the probability matrix
indexing(0,s0).
indexing(1,s1).
indexing(2,s2).
indexing(3,s3).
indexing(4,s4).
indexing(5,s5).

%##########################################################################

% vector of elements verifying a formula
phi_vect(Formula,Vect) :- 
    phi_vect(Formula,Vect,[s0,s1,s2,s3,s4,s5]).

phi_vect(_,[],[]).
phi_vect(Formula,[1|Vect],[E|States]) :- 
    sat(E,Formula),
    phi_vect(Formula,Vect,States).
phi_vect(Formula,[0|Vect],[E|States]) :- 
    sat(E,not(Formula)),
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
sat(Formula):-start(E),sat(E,Formula).

% Classic cases
sat(_E,true).
sat(_E,false):-fail.
sat(E,p(Property)):-prop(E,Property).
sat(E,and(F,G)) :- sat(E,F), sat(E,G).
sat(E,or(F,_G)) :- sat(E,F).
sat(E,or(_F,G)) :- sat(E,G).
sat(E,not(F)) :- not (sat(E,F)).

% Probabilistic-formula cases. Operator is =, =< or >=. P is a number between 0 and 1 (or a Variable).
%TODO implement negation & G formula
sat(E,prob_formula(Operator,P,Ctl_formula)) :- 
    prob_calc(Ctl_formula,V),
    indexing(N,E),
    pos(V,N,P_phi),
    against(P_phi,P,Operator).

against(P_phi,P,eq):-P_phi = P.
against(P_phi,P,inf):-P_phi =< P.
against(P_phi,P,sup):-P_phi >= P.

% Next formula
prob_calc(x(F),V) :- 
    prob_mat(M),
    phi_vect(F,Phi),
    matrix_Vector_Prod(M,Phi,V).

% Until-bounded formula
prob_calc(u(F1,K,F2),V) :- 
    phi_vect(F2,Syes),
    phi_vect(not(or(F1,F2)),Sno),
    prob_mat(Mat),
    prob_until_bound(Mat,K,V,Syes,Sno).

% Until formula
prob_calc(u(F1,F2),V) :-
    prob0(F1,F2,Sno),
    prob1(F1,F2,Sno,Syes),
    linear_eq(Sno,Syes,V).

% Eventually-bounded formula
prob_calc(f(K,Formula),V) :- prob_calc(u(true,K,Formula),V).

% Eventually formula
prob_calc(f(Formula),V) :- prob_calc(u(true,Formula),V).

% New matrix vector product for the until-bounded formula
matrix_Vector_Prod_Prime([],[],[],_,_,[]).
matrix_Vector_Prod_Prime([1|Syes_explored],[0|Sno],[_|Mat],Syes,Vect,[R|Result]) :-
    scalar_Product(Syes,Vect,R),
    matrix_Vector_Prod_Prime(Syes_explored,Sno,Mat,Syes,Vect,Result).
matrix_Vector_Prod_Prime([0|Syes_explored],[1|Sno],[_|Mat],Syes,Vect,[0.0|Result]):-
    matrix_Vector_Prod_Prime(Syes_explored,Sno,Mat,Syes,Vect,Result).
matrix_Vector_Prod_Prime([0|Syes_explored],[0|Sno],[Row|Mat],Syes,Vect,[R|Result]):-
    scalar_Product(Row,Vect,R),
    matrix_Vector_Prod_Prime(Syes_explored,Sno,Mat,Syes,Vect,Result).

% Until-bounded formula probability vector computation
prob_until_bound(_,0,Syes,Syes,_).
prob_until_bound(Mat,K_new,V_new,Syes,Sno):-
    K_new > 0,
    K is K_new - 1,
    prob_until_bound(Mat,K,V,Syes,Sno),
    matrix_Vector_Prod_Prime(Syes,Sno,Mat,Syes,V,V_new).

% Precomputation for the until formula
prob0(F1,F2,Sno):-prob0(F1,F2,Sno,[s0,s1,s2,s3,s4,s5]).

prob0(_F1,_F2,[],[]).
prob0(F1,F2,[0|Sno],[S|States]):-
    sub_prob0(F1,F2,S),
    prob0(F1,F2,Sno,States).
prob0(F1,F2,[1|Sno],[S|States]):-
    (not(sub_prob0(F1,F2,S))),
    prob0(F1,F2,Sno,States).

:- table sub_prob0/3.

sub_prob0(_F1,F2,S) :- sat(S,F2).
sub_prob0(F1,F2,S):-sat(S,F1),trans(S,S2),sub_prob0(F1,F2,S2).

% Second part of the precomputation using the first one
prob1(F1,F2,Sno,Syes) :- prob1(F1,F2,Sno,Sno,Syes,0).

prob1(_F1,_F2,[],_Sno,[],_).
prob1(F1,F2,[1|Sno_explored],Sno,[0|Syes],N) :- 
    N_new is N+1, 
    prob1(F1,F2,Sno_explored,Sno,Syes,N_new).
prob1(F1,F2,[0|Sno_explored],Sno,[0|Syes],N) :- 
    N_new is N+1,
    sub_prob1(F1,Sno,N),
    prob1(F1,F2,Sno_explored,Sno,Syes,N_new).
prob1(F1,F2,[0|Sno_explored],Sno,[1|Syes],N) :-
    N_new is N+1,
    (not(sub_prob1(F1,Sno,N))),
    prob1(F1,F2,Sno_explored,Sno,Syes,N_new).

:- table sub_prob1/3.

sub_prob1(_F1,Sno,N):-pos(Sno,N,1).
sub_prob1(F1,Sno,N):-
    indexing(N,S),
    sat(S,F1),
    trans(S,S2),
    indexing(N2,S2),
    sub_prob1(F1,Sno,N2).

% Linear equation to solve
linear_eq(Sno,Syes,V):-linear_eq(Sno,Syes,V,V,0).

linear_eq([],[],[],_,_).
linear_eq([1|Sno],[0|Syes],[0.0|V_explored],V,N):-
    N_new is N + 1,
    linear_eq(Sno,Syes,V_explored,V,N_new).
linear_eq([0|Sno],[1|Syes],[1.0|V_explored],V,N):-
    N_new is N +1,
    linear_eq(Sno,Syes,V_explored,V,N_new).
linear_eq([0|Sno],[0|Syes],[P|V_explored],V,N):-
    prob_mat(Mat),
    pos(Mat,N,Row),
    N_new is N+1,
    scalar_Product(Row,V,P),
    linear_eq(Sno,Syes,V_explored,V,N_new).