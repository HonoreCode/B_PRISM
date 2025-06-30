// CROWDS [Reiter,Rubin]
// Vitaly Shmatikov, 2002

// Note:
// Change everything marked CWDSIZ when changing the size of the crowd
// Change everything marked CWDMAX when increasing max size of the crowd

dtmc

// Probability of forwarding
const double    PF = 0.8;
const double notPF = 0.2;  // must be 1-PF

// Probability that a crowd member is bad
const double  badC = 0.091;
// const double  badC = 0.167;
const double goodC = 0.909;  // must be 1-badC
// const double goodC = 0.833;  // must be 1-badC

const int TotalRuns = 4; // Total number of protocol runs to analyze
const int CrowdSize = 10; // CWDSIZ: actual number of good crowd members
const int MaxGood=20; // CWDMAX: maximum number of good crowd members

// Process definitions
module crowds

	// Auxiliary variables
	launch:   bool init true;       // Start modeling?
	new:      bool init false;      // Initialize a new protocol instance?
	runCount: [0..TotalRuns] init TotalRuns;   // Counts protocol instances
	start:    bool init false;      // Start the protocol?
	run:      bool init false;      // Run the protocol?
	lastSeen: [0..MaxGood] init MaxGood;   // Last crowd member to touch msg
	good:     bool init false;      // Crowd member is good?
	bad:      bool init false;      //              ... bad?
	recordLast: bool init false;    // Record last seen crowd member?
	badObserve: bool init false;    // Bad members observes who sent msg?
	deliver:  bool init false;      // Deliver message to destination?
	done:     bool init false;      // Protocol instance finished?

	// Counters for attackers' observations
	// CWDMAX: 1 counter per each good crowd member
	observe0:  [0..TotalRuns] init 0;
	observe1:  [0..TotalRuns] init 0;
	observe2:  [0..TotalRuns] init 0;
	observe3:  [0..TotalRuns] init 0;
	observe4:  [0..TotalRuns] init 0;
	observe5:  [0..TotalRuns] init 0;
	observe6:  [0..TotalRuns] init 0;
	observe7:  [0..TotalRuns] init 0;
	observe8:  [0..TotalRuns] init 0;
	observe9:  [0..TotalRuns] init 0;

	
	[] launch -> (new'=true) & (runCount'=TotalRuns) & (launch'=false);
	// Set up a new protocol instance
	[] new & runCount>0 -> (runCount'=runCount-1) & (new'=false) & (start'=true);
	
	// SENDER
	// Start the protocol
	[] start -> (lastSeen'=0) & (run'=true) & (deliver'=false) & (start'=false);
	
	// CROWD MEMBERS
	// Good or bad crowd member?
	[] !good & !bad & !deliver & run ->
	              goodC : (good'=true) & (recordLast'=true) & (run'=false) +
	               badC : (bad'=true)  & (badObserve'=true) & (run'=false);

	// GOOD MEMBERS
	// Forward with probability PF, else deliver
	[] good & !deliver & run -> PF : (good'=false) + notPF : (deliver'=true);
	// Record the last crowd member who touched the msg;
	// all good members may appear with equal probability
	//    Note: This is backward.  In the real protocol, each honest
	//          forwarder randomly chooses the next forwarder.
	//          Here, the identity of an honest forwarder is randomly
	//          chosen *after* it has forwarded the message.
	
	[] recordLast & CrowdSize=5 ->
	        1/5 : (lastSeen'=0) & (recordLast'=false) & (run'=true) +
	        1/5 : (lastSeen'=1) & (recordLast'=false) & (run'=true) +
	        1/5 : (lastSeen'=2) & (recordLast'=false) & (run'=true) +
	        1/5 : (lastSeen'=3) & (recordLast'=false) & (run'=true) +
	        1/5 : (lastSeen'=4) & (recordLast'=false) & (run'=true);
	[] recordLast & CrowdSize=10 ->
	        1/10 : (lastSeen'=0) & (recordLast'=false) & (run'=true) +
	        1/10 : (lastSeen'=1) & (recordLast'=false) & (run'=true) +
	        1/10 : (lastSeen'=2) & (recordLast'=false) & (run'=true) +
	        1/10 : (lastSeen'=3) & (recordLast'=false) & (run'=true) +
	        1/10 : (lastSeen'=4) & (recordLast'=false) & (run'=true) +
	        1/10 : (lastSeen'=5) & (recordLast'=false) & (run'=true) +
	        1/10 : (lastSeen'=6) & (recordLast'=false) & (run'=true) +
	        1/10 : (lastSeen'=7) & (recordLast'=false) & (run'=true) +
	        1/10 : (lastSeen'=8) & (recordLast'=false) & (run'=true) +
	        1/10 : (lastSeen'=9) & (recordLast'=false) & (run'=true);
	
	// BAD MEMBERS
	// Remember from whom the message was received and deliver
	// CWDMAX: 1 rule per each good crowd member
	[] lastSeen=0  & badObserve & observe0 <TotalRuns -> (observe0' =observe0 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);
	[] lastSeen=1  & badObserve & observe1 <TotalRuns -> (observe1' =observe1 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);
	[] lastSeen=2  & badObserve & observe2 <TotalRuns -> (observe2' =observe2 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);
	[] lastSeen=3  & badObserve & observe3 <TotalRuns -> (observe3' =observe3 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);
	[] lastSeen=4  & badObserve & observe4 <TotalRuns -> (observe4' =observe4 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);
	[] lastSeen=5  & badObserve & observe5 <TotalRuns -> (observe5' =observe5 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);
	[] lastSeen=6  & badObserve & observe6 <TotalRuns -> (observe6' =observe6 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);
	[] lastSeen=7  & badObserve & observe7 <TotalRuns -> (observe7' =observe7 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);
	[] lastSeen=8  & badObserve & observe8 <TotalRuns -> (observe8' =observe8 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);
	[] lastSeen=9  & badObserve & observe9 <TotalRuns -> (observe9' =observe9 +1) & (deliver'=true) & (run'=true) & (badObserve'=false);

	// RECIPIENT
	// Delivery to destination
	[] deliver & run -> (done'=true) & (deliver'=false) & (run'=false) & (good'=false) & (bad'=false);
	// Start a new instance
	[] done -> (new'=true) & (done'=false) & (run'=false) & (lastSeen'=MaxGood);
	
endmodule

