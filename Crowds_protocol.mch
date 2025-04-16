/*This machine is trying to translate in B the "Crowds Protocol" PRISM case study, taken from
https://www.prismmodelchecker.org/casestudies/crowds.php. For a better understanding of the case studies and 
the simplifications made —like the fact that a corrupt member directly deliver the message, without
trying to forward it — here's the original paper : https://ieeexplore.ieee.org/document/1021811.

The SimB simulation was used to realise a hypothesis test for the "Positive" & the 
"False positive" cases*/

MACHINE Crowds_protocol
DEFINITIONS
      TotalRuns == 4/*@desc Number of path reformulations*/;
      CrowdSize == 10/*@desc Number of good people in the crowds*/
  //Note that the parameter "Maxsize" is omitted, as operations can be parametrised in B 

VARIABLES 
      new/*@desc Initialize a new protocol instance*/,
      runCount/*@desc Count protocol instances*/,
      start/*@desc Start the protocol*/,
      run/*@desc Run the protocol*/,
      lastSeen/*@desc Last crowd member to touch message*/,
      good/*@desc The crowd member is good*/,
      bad/*@desc The crowd member is corrupt*/,
      recordLast/*@desc Record last seen crowd member?*/,
      badObserve/*@desc Bad members observes who sent msg?*/,
      deliver/*@desc Deliver message to destination?*/,
      done/*@desc Protocol instance finished?*/,
      observe/*@desc How many times each member have been observed by a bad member*/

INVARIANT new:BOOL & runCount:0..TotalRuns & start:BOOL & run:BOOL & lastSeen:0..CrowdSize &
      good:BOOL & bad:BOOL & recordLast: BOOL & badObserve:BOOL & deliver:BOOL & done:BOOL &
      observe:0..CrowdSize-1 --> 0..TotalRuns

INITIALISATION new:=TRUE || runCount:=TotalRuns //The count is decreasing
      || start:=FALSE || run:=FALSE || 
      lastSeen:=CrowdSize //the crowd members are numbered from 0 to CrowdSize-1
 || good:=FALSE || bad:= FALSE || recordLast:=FALSE || badObserve:=FALSE
      || deliver:= FALSE || done:=FALSE || observe:={i•i:0..CrowdSize-1|i|->0}

OPERATIONS 
New_protocol=
PRE new=TRUE & runCount>0 THEN
      runCount:=runCount-1;
      new:=FALSE;
      start:=TRUE
END/*@desc Inititialize a new protocol instance, with a reformulated path*/;

Start_protocol =
PRE start=TRUE THEN
      lastSeen:=0;
      run:= TRUE;
      deliver :=FALSE;
      start:=FALSE
END/*@desc Start the protocol*/;

Is_a_good_member(b) =
PRE b:BOOL & good=FALSE & bad = FALSE & deliver = FALSE & run = TRUE THEN
  IF b=TRUE THEN
      good:=TRUE; recordLast:=TRUE; run:=FALSE
  ELSE 
      bad:=TRUE; badObserve:=TRUE; run:=FALSE
  END
END/*@desc Good or bad crowd member ?*/;

Forward_or_deliver(b) =
PRE b:BOOL & good=TRUE & deliver=FALSE & run=TRUE
THEN IF b=TRUE THEN
      good:=FALSE
  ELSE
      deliver:=TRUE
  END
END/*@desc Choose to forward to another crowd member or deliver to the receiver*/;

Record(member) =
PRE member:0..CrowdSize-1 & recordLast= TRUE THEN
      lastSeen:=member; recordLast:=FALSE; run:=TRUE
END/*@desc Record the last member in touch with the message*/;

Eavesdrop =
PRE badObserve = TRUE & observe(lastSeen)<TotalRuns & lastSeen<CrowdSize
THEN observe(lastSeen):=observe(lastSeen)+1;
      deliver:=TRUE;
      run:=TRUE; badObserve:=FALSE
END/*@desc A corrupt member takes notes of the last member*/;

Deliver =
PRE deliver=TRUE & run=TRUE 
THEN done:=TRUE ; deliver := FALSE ; run:=FALSE ; good := FALSE ; bad := FALSE
END/*@desc Deliver the message to the receiver*/;

Restart =
PRE done=TRUE 
THEN new:=TRUE;done:=FALSE; run:=FALSE; lastSeen:=CrowdSize
END/*Restart a new protocol, with a new path formulation*/

END
