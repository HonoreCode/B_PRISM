MACHINE Crowds_protocol
DEFINITIONS
      TotalRuns == 4;
      CrowdSize == 10;
      MaxGood == 20;

VARIABLES 
      new,runCount,start,run,lastSeen,good,bad,recordLast,badObserve,deliver,done

PROPERTIES new:BOOL & runCount:0..TotalRuns & start:BOOL & run:BOOL & lastSeen:1..MaxGood &
      good:BOOL & bad:BOOL & recordLast: BOOL & badObserve:BOOL & deliver:BOOL & done:BOOL

INITIALISATION new:=TRUE
END
