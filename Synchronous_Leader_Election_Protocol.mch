MACHINE Synchronous_Leader_Election_Protocol
SETS MODES={choosing,reading,deciding,finishing}
/*
CONSTANTS Na,K

PROPERTIES Na:NATURAL & K:NATURAL
*/
DEFINITIONS
N==4;
K==8

VARIABLES c,S,U,V,P

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

pick =
ANY prob //P = 1/K**N
WHERE prob:1..N --> 0..K-1 & ran(S)={choosing}
THEN P:=prob; V:=prob; S:={i•i:1..N|i|->reading}
END;

read1 =
PRE ran(S)={reading} & c<N-1
THEN 
      U:={i•i:1..N|i|->bool(not(P(i)=V((i mod N)+1)) or U(i)=FALSE)};
      P:={i•i:dom(U|>{FALSE})|i|->0}\/{i•i:(1..N)\dom(U|>{FALSE})|i|->P(i)};
      V:={i•i:1..N|i|->V((i mod N)+1)};
      c:=c+1
END;

read2 = 
PRE ran(S)={reading} & c=N-1
THEN P:={i•i:1..N|i|->0};
     S:={i•i:1..N|i|->deciding};
     U:={i•i:1..N|i|->bool(not(P(i)=V((i mod N)+1)) or U(i)=FALSE)};
     V:={i•i:1..N|i|->0};
     c:=c
END;

done =
PRE ran(S)={deciding} & TRUE:ran(U)
THEN S:={i•i:1..N|i|->finishing}; U:={i•i:1..N|i|->FALSE};
      V:={i•i:1..N|i|->0}; P:={i•i:1..N|i|->0}
END;

retry =
PRE ran(S)={deciding} & ran(U)={FALSE}
THEN S:={i•i:1..N|i|->choosing}; U:={i•i:1..N|i|->FALSE};
      V:={i•i:1..N|i|->0}; P:={i•i:1..N|i|->0}
END;

loop =
PRE ran(S)={finishing}
THEN S:={i•i:1..N|i|->finishing};c:=c
END
END
