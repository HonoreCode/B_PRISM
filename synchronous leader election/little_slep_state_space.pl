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
current_options([(0,'INITIALISATION(P={(1|->0),(2|->0),(3|->0),(4|->0)},S={(1|->choosing),(2|->choosing),(3|->choosing),(4|->choosing)},U={(1|->TRUE),(2|->TRUE),(3|->TRUE),(4|->TRUE)},V={(1|->0),(2|->0),(3|->0),(4|->0)},c=1)','$initialise_machine',0)]).


:- dynamic packed_visited_expression/2.
packed_visited_expression(root,root).
packed_visited_expression(0,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES1'),1,packed_node((1,'$fd_MODES1'),0,empty,empty),packed_node((3,'$fd_MODES1'),1,empty,packed_node((4,'$fd_MODES1'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(1,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(2,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(3,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,2),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,2),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(4,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,3),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,3),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(5,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,4),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,4),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(6,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,5),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,5),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(7,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,6),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,6),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(8,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,7),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,7),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(9,'$bind_lst'('$avl_packed'(packed_node((2,1),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,1),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(10,'$bind_lst'('$avl_packed'(packed_node((2,1),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,1),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[1]))).
packed_visited_expression(11,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[2]))).
packed_visited_expression(12,'$bind_lst'('$avl_packed'(packed_node((2,1),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,1),0,empty,empty)))),[2]))).
packed_visited_expression(13,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,1),0,empty,empty)))),[2]))).
packed_visited_expression(14,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,2),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,2),0,empty,empty)))),[2]))).
packed_visited_expression(15,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,2),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,2),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(16,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,3),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,3),0,empty,empty)))),[2]))).
packed_visited_expression(17,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES3'),1,packed_node((1,'$fd_MODES3'),0,empty,empty),packed_node((3,'$fd_MODES3'),1,empty,packed_node((4,'$fd_MODES3'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(18,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES4'),1,packed_node((1,'$fd_MODES4'),0,empty,empty),packed_node((3,'$fd_MODES4'),1,empty,packed_node((4,'$fd_MODES4'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(19,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,4),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,4),0,empty,empty)))),[2]))).
packed_visited_expression(20,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,5),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,5),0,empty,empty)))),[2]))).
packed_visited_expression(21,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,6),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,6),0,empty,empty)))),[2]))).
packed_visited_expression(22,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,7),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,7),0,empty,empty)))),[2]))).
packed_visited_expression(23,'$bind_lst'('$avl_packed'(packed_node((2,1),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[2]))).
packed_visited_expression(24,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(25,'$bind_lst'('$avl_packed'(packed_node((2,1),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,1),1,empty,packed_node((4,1),0,empty,empty)))),[3]))).
packed_visited_expression(26,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,1),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(27,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,3),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,3),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(28,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,4),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,4),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(29,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES3'),1,packed_node((1,'$fd_MODES3'),0,empty,empty),packed_node((3,'$fd_MODES3'),1,empty,packed_node((4,'$fd_MODES3'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(30,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES1'),1,packed_node((1,'$fd_MODES1'),0,empty,empty),packed_node((3,'$fd_MODES1'),1,empty,packed_node((4,'$fd_MODES1'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(31,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,5),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,5),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(32,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,6),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,6),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(33,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(34,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(35,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,2),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,2),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(36,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,3),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,3),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(37,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,4),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,4),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(38,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,5),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,5),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(39,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,6),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,6),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(40,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,7),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,7),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(41,'$bind_lst'('$avl_packed'(packed_node((2,1),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,1),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(42,'$bind_lst'('$avl_packed'(packed_node((2,1),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_true),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,1),1,packed_node((1,1),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(43,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,7),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,7),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(44,'$bind_lst'('$avl_packed'(packed_node((2,1),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES2'),1,packed_node((1,'$fd_MODES2'),0,empty,empty),packed_node((3,'$fd_MODES2'),1,empty,packed_node((4,'$fd_MODES2'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,1),0,empty,empty)))),[3]))).
packed_visited_expression(45,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES3'),1,packed_node((1,'$fd_MODES3'),0,empty,empty),packed_node((3,'$fd_MODES3'),1,empty,packed_node((4,'$fd_MODES3'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(46,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES3'),1,packed_node((1,'$fd_MODES3'),0,empty,empty),packed_node((3,'$fd_MODES3'),1,empty,packed_node((4,'$fd_MODES3'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(47,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES3'),1,packed_node((1,'$fd_MODES3'),0,empty,empty),packed_node((3,'$fd_MODES3'),1,empty,packed_node((4,'$fd_MODES3'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_false),1,packed_node((1,pred_true),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_true),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).
packed_visited_expression(48,'$bind_lst'('$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),l3('$avl_packed'(packed_node((2,'$fd_MODES3'),1,packed_node((1,'$fd_MODES3'),0,empty,empty),packed_node((3,'$fd_MODES3'),1,empty,packed_node((4,'$fd_MODES3'),0,empty,empty)))),'$avl_packed'(packed_node((2,pred_true),1,packed_node((1,pred_false),0,empty,empty),packed_node((3,pred_false),1,empty,packed_node((4,pred_false),0,empty,empty)))),'$avl_packed'(packed_node((2,0),1,packed_node((1,0),0,empty,empty),packed_node((3,0),1,empty,packed_node((4,0),0,empty,empty)))),[3]))).


:- dynamic not_invariant_checked/1.
not_invariant_checked(A) :- fail.


:- dynamic not_interesting/1.
not_interesting(A) :- fail.


:- dynamic max_reached_for_node/1.
max_reached_for_node(0).
max_reached_for_node(30).


:- dynamic time_out_for_node/3.
time_out_for_node(A,B,C) :- fail.


:- dynamic use_no_timeout/1.
use_no_timeout(A) :- fail.


:- dynamic transition/4.
transition(root,'$initialise_machine',0,0).
transition(0,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(0)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),1,1).
transition(0,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(1)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),2,2).
transition(0,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(2)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),3,3).
transition(0,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(3)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),4,4).
transition(0,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(4)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),5,5).
transition(0,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(5)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),6,6).
transition(0,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(6)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),7,7).
transition(0,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(7)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),8,8).
transition(0,pick(avl_set(node((int(2),int(1)),true,1,node((int(1),int(0)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),9,9).
transition(0,pick(avl_set(node((int(2),int(1)),true,1,node((int(1),int(1)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),10,10).
transition(1,read1,11,11).
transition(10,read1,12,12).
transition(2,read1,13,13).
transition(3,read1,14,14).
transition(14,read1,15,15).
transition(4,read1,16,16).
transition(15,read2,17,17).
transition(17,done,18,18).
transition(5,read1,19,19).
transition(6,read1,20,20).
transition(18,loop,21,18).
transition(7,read1,22,21).
transition(8,read1,23,22).
transition(9,read1,24,23).
transition(11,read1,25,24).
transition(12,read1,26,25).
transition(13,read1,27,26).
transition(16,read1,28,27).
transition(19,read1,29,28).
transition(25,read2,30,29).
transition(29,retry,31,30).
transition(20,read1,32,31).
transition(21,read1,33,32).
transition(30,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(0)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),34,33).
transition(30,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(1)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),35,34).
transition(30,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(2)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),36,35).
transition(30,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(3)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),37,36).
transition(30,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(4)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),38,37).
transition(30,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(5)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),39,38).
transition(30,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(6)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),40,39).
transition(30,pick(avl_set(node((int(2),int(0)),true,1,node((int(1),int(7)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),41,40).
transition(30,pick(avl_set(node((int(2),int(1)),true,1,node((int(1),int(0)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),42,41).
transition(30,pick(avl_set(node((int(2),int(1)),true,1,node((int(1),int(1)),true,0,empty,empty),node((int(3),int(0)),true,1,empty,node((int(4),int(0)),true,0,empty,empty))))),43,42).
transition(22,read1,44,43).
transition(23,read1,45,44).
transition(42,read2,46,45).
transition(24,read2,47,29).
transition(45,done,48,18).
transition(41,read2,49,46).
transition(26,read2,50,17).
transition(46,done,51,18).
transition(27,read2,52,17).
transition(28,read2,53,17).
transition(40,read2,54,47).
transition(31,read2,55,17).
transition(47,done,56,18).
transition(39,read2,57,47).
transition(32,read2,58,17).
transition(38,read2,59,47).
transition(33,read2,60,29).
transition(37,read2,61,47).
transition(36,read2,62,47).
transition(35,read2,63,47).
transition(34,read2,64,47).
transition(43,read2,65,17).
transition(44,read2,66,48).
transition(48,done,67,18).


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
bind_skeleton(list,['P','S','U','V',c]).


:- dynamic stored_value/2.
stored_value(A,B) :- fail.


:- dynamic stored_value_hash_to_id/2.
stored_value_hash_to_id(A,B) :- fail.


:- dynamic next_value_id/1.
next_value_id(0).

saved_gennum_count(49).
