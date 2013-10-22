{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

type Node = String

data Dot a = Dot [(Node, String)] [(Node, Node, Maybe String)] Int a deriving (Eq, Show)

instance Monad Dot where
    return x = Dot [] [] 0 x
    dot >>= f = Dot labels'' edges'' count'' y
                where
                    Dot labels edges _ x = dot
                    Dot labels' edges' _ y = f x
                    edges'' = edges ++ edges'
                    -- contains :: Eq a => [a] -> a -> Bool
                    contains xs x = any (== x) xs 
                    -- appendLabels :: [(Node, String)] -> [(Node, String)] -> [String] -> [(Node, String)]
                    appendLabels [] result _ = result
                    appendLabels (l:ls) result unique = let (node, label) = l
                                                            count = length unique
                                                            newLabel = "__dot_" ++ show count
                                                        in if contains unique label
                                                           then appendLabels ls (result ++ [(node, newLabel)]) (newLabel:unique)
                                                           else appendLabels ls (result ++ [l]) (label:unique)
                    labels'' = appendLabels (labels ++ labels') [] []
                    count'' = length labels''
                    
addNode :: Node -> Dot Node
addNode n = Dot [(n, "__dot_0")] [] 1 n

addEdge :: Node -> Node -> Maybe String -> Dot ()
addEdge src snk label = Dot [] [(src, snk, label)] 0 ()

getDotString :: Dot a -> String -> String
getDotString = undefined
