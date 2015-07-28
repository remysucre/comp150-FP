Proof: If adding a bang improves space usage, then adding the bang will improve space usage regardless of other bangs. 

1. a bang evaluates an expression to WHNF
2. an expression is one of: 
    a. a thunk
    b. a value (normal form)
    c. week head normal form
3. bangs improve space usage by eliminating thunks (as expressions or as sub-expressions in an expression of whnf) to values

Lemmas: 
1. if exp(!sub-ex)p plugs thunk leak, !exp(!)

Qustions: 
1. What are the possible relations between two expressions? 
2. Is it correct to assume, if two bangs improve space usage independently, then all bangs are independent? 
3. Is every haskell program an expression? (pure and impure) 
