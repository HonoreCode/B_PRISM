MACHINE Crowds_protocol
DEFINITIONS
      TotalRuns == 4;
      CrowdSize == 10;
      MaxGood == 20;

VARIABLES 
      new,runCount,start,run,lastSeen,good,bad,recordLast,badObserve,deliver,done,observe

INVARIANT new:BOOL & runCount:0..TotalRuns & start:BOOL & run:BOOL & lastSeen:0..MaxGood &
      good:BOOL & bad:BOOL & recordLast: BOOL & badObserve:BOOL & deliver:BOOL & done:BOOL &
      observe:0..MaxGood-1 --> 0..TotalRuns

INITIALISATION new:=TRUE || runCount:=TotalRuns || start:=FALSE || run:=FALSE || 
      lastSeen:=MaxGood || good:=FALSE || bad:= FALSE || recordLast:=FALSE || badObserve:=FALSE
      || deliver:= FALSE || done:=FALSE || observe:={i•i:0..MaxGood-1|i|->0}

OPERATIONS 
New_protocol=
PRE new=TRUE & runCount>0 THEN
      runCount:=1;
      new:=FALSE;
      start:=TRUE
END;

Start_protocol =
PRE start=TRUE THEN
      lastSeen:=0;
      run:= TRUE;
      deliver :=FALSE;
      start:=FALSE
END;

Is_a_good_member =
ANY b 
WHERE b:BOOL & good=FALSE & bad = FALSE & deliver = FALSE & run = TRUE THEN
  IF b=TRUE THEN
      good:=TRUE; recordLast:=TRUE; run:=FALSE
  ELSE 
      bad:=TRUE; badObserve:=TRUE; run:=FALSE
  END
END;

Forward_or_deliver =
ANY b
WHERE b:BOOL & good=TRUE & deliver=FALSE & run=TRUE
THEN IF b=TRUE THEN
      good:=FALSE
  ELSE
      deliver:=TRUE
  END
END;

Record =
ANY member
WHERE member:0..MaxGood-1 & recordLast= TRUE THEN
      lastSeen:=member; recordLast:=FALSE; run:=TRUE
END;

Eavesdrop =
PRE badObserve = TRUE & observe(lastSeen)<TotalRuns & lastSeen<MaxGood
THEN observe(lastSeen):=observe(lastSeen)+1;
      deliver:=TRUE;
      run:=TRUE; badObserve:=FALSE
END;

Deliver =
PRE deliver=TRUE & run=TRUE 
THEN done:=TRUE ; deliver := FALSE ; run:=FALSE ; good := FALSE ; bad := FALSE
END;

Restart =
PRE done=TRUE 
THEN new:=TRUE;done:=FALSE; run:=FALSE; lastSeen:=MaxGood
END

END
