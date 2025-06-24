To test the model checker, start a prolog instance in the "model-checking basic test" directory, load the testing file corresponding to your prolog implementation (e.g. "testing_SICS") and query ":- global_test."

## PCTL syntax
A PCTL formula is written as `sat(Formula,E)`, with `E` being a state and `Formula` being a probabilistic formula or a classic logical operator (`true`, `false`, `and(Formula1,Formula2)`, `or(Formula1,Formula2)`, `imp(Formula1,Formula2)`).
A probabilistic formula is written as `prob_formula(Operator,Probability,CTL_Formula)` with `Operator` being a comparison operator (`sup`,`inf`,`eq`..), `Probability` being a (Prolog) variable or a number between 0 and 1, and `CTL_Formula` being one CTL formula of the follows :
- Next : `x(Property)`
- Until bounded : `u(Property1,K,Property2)`
- Until : `u(Property1,Property2)`
- Always bounded : `g(K,Property)`
- Always : `g(Property)`
- Eventually bounded : `f(K,Property)`
- Eventually : `f(Property)`
