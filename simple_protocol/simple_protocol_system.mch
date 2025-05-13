MACHINE simple_protocol_system
SETS LABELS={idle,try,fail,sent}
VARIABLES state
INVARIANT state:LABELS
INITIALISATION state:=idle
OPERATIONS 

Start=
PRE state=idle
THEN state:=try
END;

Send(next) =
PRE state = try & next:{try,fail,sent}
THEN state:=next
END;

Done =
PRE state=sent
THEN state:=sent
END;

Retry = 
PRE state=fail
THEN state:=idle
END
END

/*
dtmc
const double PSUCC;
module M1
s:[0..3] init 0;
[] s=0 -> 1:(s'=1);
[] s=1 -> (1-PSUCC)/2:(s'=1) + (1-PSUCC)/2:(s'=2) + PSUCC:(s'=3);
[] s=2 -> 1:(s'=0);
[] s=3 -> true;
endmodule
*/
