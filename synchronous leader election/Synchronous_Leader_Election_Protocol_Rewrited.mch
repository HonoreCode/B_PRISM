
/* This model is based upon : https://www.prismmodelchecker.org/casestudies/synchronous_leader.php
The probabilistic parameters are represented as an uniform distribution on the SimB activation
for the pick operation.


*/
MACHINE Synchronous_Leader_Election_Protocol
SETS MODES={choosing,reading,deciding,finishing} /*@desc States of the process*/

DEFINITIONS
N==3; 
K==4 

VARIABLES c,
      S,
      U,
      V,
      P

INVARIANT c:1..N-1 &
      S:1..N --> MODES &
      U:1..N --> BOOL &
      V:1..N --> 0..K-1 &
      P:1..N --> 0..K-1 

INITIALISATION 
      c:=1 ||
      S:={i•i:1..N|i|->choosing} ||
      U:={i•i:1..N|i|->TRUE}||
      V:={i•i:1..N|i|->0} || 
      P:={i•i:1..N|i|->0}
OPERATIONS

pick(IDs) =
//Uniform distribution the ID choice in the PRISM model, 
PRE IDs:1..N --> 0..K-1 & ran(S)={choosing}
THEN P:=IDs; V:=IDs; S:={i•i:1..N|i|->reading};U:={i•i:1..N|i|->TRUE}
END/*@desc Each process choose an ID randomly */;

read1 =
PRE ran(S)={reading} & c<N-1
THEN 
      U:={i•i:1..N|i|->bool(not(P(i)=V((i mod N)+1)) & U(i)=TRUE)};
      P:={i•i:dom(U|>{FALSE})|i|->0}\/{i•i:(1..N)\dom(U|>{FALSE})|i|->P(i)};
      V:={i•i:1..N|i|->V((i mod N)+1)};
      c:=c+1
END/*@desc Each process read an ID and compare it to its*/;

read2 = 
PRE ran(S)={reading} & c=N-1
THEN 
     S:={i•i:1..N|i|->deciding};
     U:={i•i:1..N|i|->bool(not(P(i)=V((i mod N)+1)) & U(i)=TRUE)};
     V:={i•i:1..N|i|->0};
     P:={i•i:1..N|i|->0};
     c:=c
END/*@desc Read and prepare to decide*/;

done =
PRE ran(S)={deciding} & TRUE:ran(U)
THEN S:={i•i:1..N|i|->finishing}; U:={i•i:1..N|i|->FALSE};
      V:={i•i:1..N|i|->0}; P:={i•i:1..N|i|->0}
END/*@desc A leader is elected*/;

retry =
PRE ran(S)={deciding} & ran(U)={FALSE}
THEN S:={i•i:1..N|i|->choosing}; U:={i•i:1..N|i|->FALSE};
      V:={i•i:1..N|i|->0}; P:={i•i:1..N|i|->0}
END/*@desc No leader is elected, everyone pick a new value*/;

loop =
PRE ran(S)={finishing}
THEN S:={i•i:1..N|i|->finishing}
END
END
