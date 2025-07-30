% (c) 2025-2025 Lehrstuhl fuer Softwaretechnik und Programmiersprachen,
% Heinrich Heine Universitaet Duesseldorf
% This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html)

%###############################################

% This is a model-checker verifying Probabilistic Computation
% Tree Logic (PCTL) formulas over Discrete-Time Markov Chains (DTMC)
% written by Honore Marmion

% Working on SICStus prolog 4.10 & ProB nightly version of 28.07.25

%###############################################

:- module(dtmc_model_checking,[pctl_model_check/4,
                               sat/1, sat/2, state/1]).
:- use_module(library(clpr),[{}/1]).


:- use_module(probsrc(error_manager),[add_error/3, add_internal_error/2, add_warning/3]).
:- use_module(probltlsrc(ltl_tools),[temporal_parser/3]).
:- use_module(probsrc(state_space),[find_initialised_states/1, current_state_id/1]).
:- use_module(probsrc(tools),[start_ms_timer/1, stop_ms_timer_with_msg/2]).

pctl_model_check(Formula,_MaxNodes,Mode,Res) :-
    (temporal_parser(Formula,pctl,PCtlFormula) -> true ; add_error(ctl,'PCTL Parser failed: ',Formula),fail),
    %set_max_nr_of_new_impl_trans_nodes(MaxNodes),
    % TODO: provide better feedback, extract probability for P={p} formulas
    (pre_process_formula(PCtlFormula,ProcessedFormula,Bindings,[]) -> true
      ; add_error(dtmc_model_checking,'Could not pre-process: ',PCtlFormula),
        ProcessedFormula = PCtlFormula
    ),
    (Mode= specific_node(ID) -> Start=ID
     ; Mode = starthere -> current_state_id(Start)
     ; Mode = init -> find_initialised_states(SN), member(Start,SN) % TODO try out all start nodes??
     ; add_error(dtmc_model_checking,'Illegal starting mode (init, starthere supported):',Mode),
       Start = root
    ),
    format('Checking PCTL formula from state ~w : ~w~n AST: ~w~n Open Probability Variables: ~w~n',[Start,Formula,ProcessedFormula,Bindings]),
    start_ms_timer(T1),
    (sat(ProcessedFormula,Start)
     -> (Bindings=[] -> Res=true ; Res = solution(Bindings))
      ; Res = false),
      stop_ms_timer_with_msg(T1,'PTCTL model checking').

pre_process_formula(true,true) --> [].
pre_process_formula(false,false) --> [].
pre_process_formula(ap(P),ap(P)) --> [].
pre_process_formula(p(P),p(P)) --> [].
pre_process_formula(probformula(Operator,RefProbAtom,Ctl_formula),
                    probformula(Operator,RefProbNr,Ctl_formula))  -->
        ({ground_number(RefProbAtom,RefProbNr)} -> []
         ; {Operator=equal} -> [RefProbAtom/RefProbNr]
         ; {add_error(dtmc_model_checking,'Illegal reference probability for operator (only equal is supported for symbolic values):',RefProbAtom/Operator)},
           [RefProbAtom/RefProbNr]
        ). % TODO: pre_process_path_formula
pre_process_formula(and(F,G),and(PF,PG)) -->
    pre_process_formula(F,PF),
    pre_process_formula(G,PG).
pre_process_formula(or(F,G),and(PF,PG)) -->
    pre_process_formula(F,PF),
    pre_process_formula(G,PG).
pre_process_formula(implies(F,G),and(PF,PG)) -->
    pre_process_formula(F,PF),
    pre_process_formula(G,PG).
pre_process_formula(not(F),not(PF)) -->
    pre_process_formula(F,PF).

%pre_process_path_formula(u(F,G),u(PF,PG)) -->
%    pre_process_formula(F,PF),
%    pre_process_formula(G,PG).


% Interface predicates defining the system to be checked

:- use_module(probsrc(xtl_interface),[xtl_transition/4]).
:- use_module(probltlsrc(ltl_propositions), [trans/3, check_transition/4, check_ap/2]).
:- use_module(probsrc(state_space),[visited_expression/2, visited_expression_id/1]).

% TODO: move to specfile and extend to work with B pragmas,...
transition_probability(From,To,Prob) :- trans(From,To,_),
   visited_expression(From,FS),
   visited_expression(To,TS),
   xtl_transition(FS,_,TS,Infos),
   (member(probability/X,Infos) -> Prob=X ; write(no_probability(From,To,Infos)),nl,fail).

state(X) :- visited_expression_id(X).

% example public_examples/XTL/markov/SimpleMarkov.P
% ?- use_module(extension('markov/dtmc_model_checking.pl')).
% ?- sat(prob_formula(eq,P,f(p(xtl_predicate_check(finished))))).

% See examples folder and seek for .P extensions for more


%PCTL Model-checking

% This term allow to handle nested formulas in case of dynamic model-checking
:- dynamic node/1.

%start(root).
start(ID) :- find_initialised_states(I), member(ID,I).

sat(Formula) :- start(E), sat(Formula,E).


% Classic cases

sat_not(false,_E).
sat_not(true,_E) :- fail.
sat_not(p(Property),E) :- \+(check_ap(Property,E)).
sat_not(ap(Property),E) :- \+(check_ap(Property,E)).
sat_not(and(F,_G),E) :- sat_not(F,E).
sat_not(and(_F,G),E) :- sat_not(G,E).
sat_not(or(F,G),E) :- sat_not(F,E),sat_not(G,E).
sat_not(implies(F,G),E) :- sat(not(or(not(F),G)),E).
sat_not(equivalence(F,G),E) :- sat_not(and(implies(F,G),implies(G,F)),E).
sat_not(not(F),E) :- sat(F,E).

%sat(Formula,State) :- write(sat(Formula,State)),nl,fail.
sat(true,_E).
sat(false,_E):-fail.
sat(ap(Property),E) :- check_ap(Property,E).
sat(p(Property),E) :- check_ap(Property,E).
sat(and(F,G),E) :- sat(F,E), sat(G,E).
sat(or(F,_G),E) :- sat(F,E).
sat(or(_F,G),E) :- sat(G,E).
sat(implies(F,G),E) :- sat(or(not(F),G),E).
sat(equivalence(F,G),E) :- sat(and(implies(F,G),implies(G,F)),E).
sat(not(F),E) :- 
    F = probformula(Operator,P,Ctl_formula) ->
        negate_operator(Operator,Neg_Operator),
        sat(probformula(Neg_Operator,P,Ctl_formula))
    ; sat_not(F,E).

% Probabilistic-formula cases. Operator is =, < >,<= or >=. 
%P is a number between 0 and 1 (or a Variable),
% E is a state.

% Check if the formula is nested, and handle the 
% case of Always (g) and Always-bounded (gk) operators
sat(probformula(Operator,P,Ctl_formula),E) :-
    Ctl_formula = gk(_,_) ->
        sat_gk(probformula(Operator,P,Ctl_formula),E)
    ; Ctl_formula = g(_) ->
        sat_g(probformula(Operator,P,Ctl_formula),E)
    ; (node(Node) -> 
        New_Node is Node +1,
        retract(node(Node)),
        assert(node(New_Node)),
        sat_node(probformula(Operator,P,Ctl_formula),E,New_Node)
    ;   assert(node(0)),
        sat_node(probformula(Operator,P,Ctl_formula),E,0),
        retractall(node(_))        /*reinitialize nodes*/
    ).

% Always bounded formula, we use the dual probabilistic event of fk(K,not(F))
% We had to use a different way of negating comparison operators in order
sat_gk(probformula(Operator,P,gk(K,F)),E) :-
    ground(P) -> 
        opposed_operator(Operator,Opp_Operator),
        Q is 1-P,
        sat(probformula(Opp_Operator,Q,fk(K,not(F))),E)

    ;   opposed_operator(Operator,Opp_Operator),
        sat(probformula(Opp_Operator,Q,fk(K,not(F))),E),
        P is 1-Q
    .

% Always formula
sat_g(probformula(Operator,P,g(F)),E) :-
    ground(P) -> 
        opposed_operator(Operator,Opp_Operator),
        Q is 1-P,
        sat(probformula(Opp_Operator,Q,f(not(F))),E)
        
    ;   opposed_operator(Operator,Opp_Operator),
        sat(probformula(Opp_Operator,Q,f(not(F))),E),
        P is 1-Q
    .

% Check the type of the formula
sat_node(probformula(Operator,P,Ctl_formula),E,Node) :- 
    Ctl_formula = u(F,G) ->
        prob_calc(u(F,G),E,P_phi,Node),
        against(P_phi,P,Operator)
    ;   Ctl_formula = f(G) -> 
        prob_calc(u(true,G),E,P_phi,Node),
        against(P_phi,P,Operator)
    ; Ctl_formula = fk(K,G) ->
        sat_dynamic(probformula(Operator,P,uk(true,K,G)),E,Node)
    ;   sat_dynamic(probformula(Operator,P,Ctl_formula),E,Node).

% Use a different technic depending on the operator
% This allow especially the calculation of a probability for the equal operator
sat_dynamic(probformula(Operator,P,Ctl_formula),E,Node) :-
    ((Operator = greater ; Operator= strictlygreater) ->
        ground(P),
        prob_calc(Ctl_formula,E,P,Operator,Node)

    ;   Operator = equal ->
            retractall(prob_current(Node,_)),
            (ground(P) ->
                prob_calc(Ctl_formula,E,P,equal,Node)

            ;   (prob_calc(Ctl_formula,E,1.0,equal,Node) ->
                    P=1.0
                ;   prob_current(Node,P)))

    ;   Operator = less ->
            ground(P),
            \+(prob_calc(Ctl_formula,E,P,strictlygreater,Node))

    ;   Operator = strictlyless ->
            ground(P),
            \+(prob_calc(Ctl_formula,E,P,greater,Node))
    ).

% Compare different formulas using a specific operator

% For the equal comparison, we compare the results using epsilon precision in case
% of a given probability
against(P_phi,ReferenceP,equal) :- !,
    (ground_number(ReferenceP,P) ->
        P_phi =< P + 0.00000000000000023,
        P =< P_phi + 0.00000000000000023
    ;   ReferenceP = P_phi % ReferenceP is an open variable
    ).
against(P_phi,ReferenceP,less) :-
    ground_number(ReferenceP,P), !,
    P_phi =< P + 0.00000000000000023.   % less
against(P_phi,ReferenceP,greater) :-
    ground_number(ReferenceP,P), !,
    P_phi >= P - 0.00000000000000023.   % greater
against(P_phi,ReferenceP,strictlyless) :-
    ground_number(ReferenceP,P), !,
    P_phi + 0.00000000000000023 < P .   % strictly less
against(P_phi,ReferenceP,strictlygreater) :-
    ground_number(ReferenceP,P), !,
    P_phi - 0.00000000000000023 > P .   % strictlygreater

% Different operator. We need to decide rather we should
% raise an error in case of the negation of an equal operator
against(P_phi,ReferenceP,different) :- !,
    (ground_number(ReferenceP,P) ->
        (P_phi > P + 0.00000000000000023 ;
         P =< P_phi + 0.00000000000000023
        )
    ;   % ReferenceP is an open variable
        add_warning(dtmc_model_checking,'Using symbolic variable for inequality: ',P_phi),
        (P_phi < 1-0.00000000000000023
          -> ReferenceP = P_phi + 0.00000000000000023
          ; ReferenceP = P_phi - 0.00000000000000023
        )
    ).

against(P1,P2,Op) :- add_internal_error('Illegal comparison operator: ',against(P1,P2,Op)),fail.

% Used to compute the logical negation of
% a probabilistic formula
% General use : negate_operator(Op,not(Op))
negate_operator(equal,different).
negate_operator(less,strictlygreater).
negate_operator(strictlygreater,less).
negate_operator(greater,strictlyless).
negate_operator(strictlyless,greater).

% Used for formulas with 'Always' (G) Operator
opposed_operator(equal,equal).
opposed_operator(less,greater).
opposed_operator(greater,less).
opposed_operator(strictlygreater,strictlyless).
opposed_operator(strictlyless,strictlygreater).

:- use_module(probsrc(tools),[safe_number_codes/2]).
% TODO: in future fully pre-process AST of formula once before launching model checker
ground_number(P,Res) :- number(P),!,Res=P.
ground_number(P,Res) :- atom(P), atom_codes(P,Codes),  % convert atom from parser into number
   safe_number_codes(Nr,Codes),!,Res=Nr.


% Next formula
:- dynamic prob_current/2.

prob_calc(x(F),E,P_phi,Operator,Node) :-
    retractall(prob_current(Node,_)),
    assert(prob_current(Node,0.0)),
    (prob_calc_sub(x(F),E,P_phi,Operator,Node) ->
        true
    ; against(0.0,P_phi,Operator)),!.

% Until Bounded formula
prob_calc(uk(F,K,G),E,P_phi,Operator,Node) :-
    retractall(prob_current(Node,_)),
    assert(prob_current(Node,0.0)),
    (sat(G,E) ->
        retractall(prob_current(Node,_)),
        assert(prob_current(Node,1.0)),
        against(1.0,P_phi,Operator)
    ; sat(F,E) ->
        (prob_calc_sub(uk(F,K,G),E,P_phi,1.0,Operator,Node) ->
            true
        ; against(0.0,P_phi,Operator))
    ; against(0.0,P_phi,Operator)),!.

% Until formula
% For this formula we have to calculate the probability for
% all states
prob_calc(u(F,G),E,P_phi,Node) :-
    retractall(table_prob0(_,_,Node)),
    retractall(table_prob1(_,_,Node)),
    prob_calc_u1(F,G,List_S,P_vect,Node),
    state(E),
    (find_prob_u(List_S,P_vect,E,P) -> P_phi=P
        ; P_phi=0.0
    ).


%*******************************************************

% recursion for the next formula
prob_calc_sub(x(F),E,P_phi,Operator,Node) :-
    transition_probability(E,S,P),
    sat(F,S),
    prob_current(Node,Previous_P),
    Current_prob is Previous_P +P,
    retract(prob_current(Node,Previous_P)),
    assert(prob_current(Node,Current_prob)),
    against(Current_prob,P_phi,Operator).

% recursion for the bounded until formula
prob_calc_sub(uk(F,K_new,G),E,P_phi,P_trace,Operator,Node) :-
    (sat(G,E) ->
        prob_current(Node,P),
        P_new is P+P_trace,
        retract(prob_current(Node,P)),
        assert(prob_current(Node,P_new)),
        against(P_new,P_phi,Operator)
        ;   K_new > 0,
            transition_probability(E,S,P_trans),
            sat(F,S),
            K is K_new -1,
            P_trace_new is P_trace*P_trans,
            prob_calc_sub(uk(F,K,G),S,P_phi,P_trace_new,Operator,Node)
    ).

% 1rst precomputation for the until formula

:- dynamic table_prob0/3.

prob0(_F,_G,E,Node) :-
    table_prob0(E,true,Node),!.

prob0(_F,G,E,Node) :-
    sat(G,E),
    !,
    asserta(table_prob0(E,true,Node)).

prob0(F,G,E1,Node) :-
    sat(F,E1),
    assertz(table_prob0(E1,computing,Node)),
    trans(E1,E2,_P),
    \+ table_prob0(E2,computing,Node),
    (prob0(F,G,E2,Node) ->
        asserta(table_prob0(E1,true,Node)),
        retract(table_prob0(E1,computing,Node))
    ),!.

search_prob0(F,G,E,Node):-
    retractall(table_prob0(_,computing,Node)),
    prob0(F,G,E,Node).

% 2nd precomputation for the until formula
:- dynamic table_prob1/3.

prob1(_F,_G,E,Node) :-
    table_prob1(E,true,Node),!.

prob1(_F,_G,E,Node) :-
    \+(table_prob0(E,true,Node)),
    !,
    asserta(table_prob1(E,true,Node)).

prob1(F,G,E1,Node) :-
    sat(F,E1),
    \+(sat(G,E1)),
    assert(table_prob1(E1,computing,Node)),
    trans(E1,E2,_P),
    \+ table_prob1(E2,computing,Node),
    (prob1(F,G,E2,Node) ->
        asserta(table_prob1(E1,true,Node)),
        retract(table_prob1(E1,computing,Node))
    ),!.

search_prob1(F,G,E,Node):-
    retractall(table_prob1(_,computing,Node)),
    prob1(F,G,E,Node).

% P_vect is the vector of non-null probabilities
% for the until formula
prob_calc_u1(F,G,List_S,P_vect,Node) :-
    findall(S,(state(S),search_prob0(F,G,S,Node)),List_S),
    findall(E,(state(E),search_prob1(F,G,E,Node)),_List),
    prob_calc_u2(F,G,List_S,List_S,P_vect,P_vect,Node).

prob_calc_u2(_F,_G,[],_List_S,[],_P_vect,_Node).
prob_calc_u2(F,G,[S|List_S_explored],List_S,[P_phi|P_vect_explored],P_vect,Node) :-
    (table_prob1(S,true,Node) -> prob_calc_u3(S,List_S,P_phi,P_vect)
        ;   P_phi=1.0
    ),
    prob_calc_u2(F,G,List_S_explored,List_S,P_vect_explored,P_vect,Node).

prob_calc_u3(_S,[],0.0,[]).
prob_calc_u3(S,[E|List_S],P_phi_new,[P|P_vect]) :-
    prob_calc_u3(S,List_S,P_phi,P_vect),
    (transition_probability(S,E,P_trans) -> {P_phi_new = P*P_trans + P_phi}
        ; P_phi_new=P_phi
    ).

find_prob_u([E],[P],E,P).
find_prob_u([S1,S2|List_S],[Prob1,Prob2|P_vect],E,P) :-
    (E=S1,P=Prob1)
        ; find_prob_u([S2|List_S],[Prob2|P_vect],E,P).
