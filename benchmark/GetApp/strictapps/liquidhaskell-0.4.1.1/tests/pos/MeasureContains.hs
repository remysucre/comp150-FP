module Fixme where

import Language.Haskell.Liquid.Prelude

{-@ LIQUID "--no-termination" @-}

{-@ measure containsV @-}
{-@ measure binderContainsV @-}


binderContainsV ::  Binder n -> Bool
binderContainsV B     = True
binderContainsV (M x) = containsV x

data Binder n = B | M (TT n)
data TT n     = V Int | Other | Bind (Binder n) (TT n)

containsV :: TT n -> Bool
containsV (V i)           = True
containsV (Bind b body) = (binderContainsV b) || (containsV body)
-- containsV (App f arg)     = (containsV f) || (containsV arg)
-- containsV (Proj tm i)     = containsV tm
containsV _               = False


prop1 = liquidAssert (containsV $ V 7)
prop2 = liquidAssert (containsV $ Bind (M (V 5)) Other)