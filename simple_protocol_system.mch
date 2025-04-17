MACHINE simple_protocol_system
SETS LABELS={idle,try,fail,sent}
VARIABLES state,test_counter
INVARIANT state:LABELS & test_counter:NATURAL
INITIALISATION state:=idle||test_counter:=0
OPERATIONS 

Start=
PRE state=idle
THEN state:=try
END;

Send(next) =
PRE state = try & next:{try,fail,sent}
THEN state:=next;test_counter:=test_counter+1
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
