:- dynamic state_space_version_in_file/1.
state_space_version_in_file(1).

:- dynamic history/1.
history([]).


:- dynamic forward_history/1.
forward_history(A) :- fail.


:- dynamic op_trace_ids/1.
op_trace_ids([]).


:- dynamic current_state_id/1.
current_state_id(root).


:- dynamic current_options/1.
current_options([(0,'INITIALISATION(state=idle)','$initialise_machine',0)]).


:- dynamic packed_visited_expression/2.
packed_visited_expression(root,root).
packed_visited_expression(0,'$bind_lst'('$fd_LABELS1',[])).
packed_visited_expression(1,'$bind_lst'('$fd_LABELS2',[])).
packed_visited_expression(2,'$bind_lst'('$fd_LABELS3',[])).
packed_visited_expression(3,'$bind_lst'('$fd_LABELS4',[])).


:- dynamic not_invariant_checked/1.
not_invariant_checked(A) :- fail.


:- dynamic not_interesting/1.
not_interesting(A) :- fail.


:- dynamic max_reached_for_node/1.
max_reached_for_node(A) :- fail.


:- dynamic time_out_for_node/3.
time_out_for_node(A,B,C) :- fail.


:- dynamic use_no_timeout/1.
use_no_timeout(A) :- fail.


:- dynamic transition/4.
transition(root,'$initialise_machine',0,0).
transition(0,'Start',1,1).
transition(1,'Send'(fd(2,'LABELS')),2,1).
transition(1,'Send'(fd(3,'LABELS')),3,2).
transition(1,'Send'(fd(4,'LABELS')),4,3).
transition(2,'Retry',5,0).
transition(3,'Done',6,3).


:- dynamic transition_info/2.
transition_info(A,B) :- fail.


:- dynamic operation_not_yet_covered/1.
operation_not_yet_covered(A) :- fail.


:- dynamic state_error/3.
state_error(A,B,C) :- fail.


:- dynamic not_all_z_saved/1.
not_all_z_saved(A) :- fail.


:- dynamic not_all_transitions_added_saved/1.
not_all_transitions_added_saved(A) :- fail.


:- dynamic bind_skeleton/2.
bind_skeleton(list,[state]).


:- dynamic stored_value/2.
stored_value(A,B) :- fail.


:- dynamic stored_value_hash_to_id/2.
stored_value_hash_to_id(A,B) :- fail.


:- dynamic next_value_id/1.
next_value_id(0).

saved_gennum_count(4).
