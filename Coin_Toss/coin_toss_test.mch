MACHINE coin_toss_test
SETS CoinSide = {Heads, Tails}

VARIABLES lastToss,state

INVARIANT lastToss <: CoinSide & card(lastToss) <= 1 & state:REAL

INITIALISATION lastToss := {} || state:=1.0

OPERATIONS
toss(c) = PRE c : CoinSide THEN lastToss := {c}; state:=state+1.0  END

END
