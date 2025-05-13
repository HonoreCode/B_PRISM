MACHINE simple_protocol_system
SETS LABELS={idle,try,fail,sent}
CONSTANTS
  PSUCC, prob_sent
PROPERTIES
  PSUCC = 0.8 &
  prob_sent =
         {try |-> (1.0-PSUCC)/2.0,
          fail |-> (1.0-PSUCC)/2.0,
          sent |-> PSUCC}
VARIABLES state,test_counter
INVARIANT
 state:LABELS &
 test_counter:NATURAL
INITIALISATION
 state:=idle || test_counter:=0
OPERATIONS

 Start=
    PRE state=idle
    THEN state:=try
    END /*@desc "Starting simple protocol system" */;

 Send(next) =
    PRE state = try & next:{try,fail,sent}
    THEN state:=next ||
         test_counter:=test_counter+1
    END /*@desc "Sent message attempt nr: ${test_counter}, result:${next}, probability:${prob_sent(next)}" */;

 Done =
    PRE state=sent
    THEN state:=sent
    END  /*@desc "Retrying to send message after ${test_counter} tries" */
    ;

 Retry =
    PRE state=fail
    THEN state := idle
    END /*@desc "Retrying to send message after ${test_counter} tries" */
END

/* original PRISM example:
dtmc
const double PSUCC;
module M1
s:[0..3] init 0;
[] s=0 -> 1:(s'=1); // Start
[] s=1 -> (1-PSUCC)/2:(s'=1) + (1-PSUCC)/2:(s'=2) + PSUCC:(s'=3);
[] s=2 -> 1:(s'=0); // Done
[] s=3 -> true;  // Retry
endmodule
*/
