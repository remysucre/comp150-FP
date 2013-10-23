{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}
module DotWriter 
( Node,
  createNode, equals,
  Color(..),
  Dot,
  addNode, addEdge, addColor,
-- colorToProperty,
  escape,
  toString
) where

import Data.List (nub)

type Node = String

createNode :: String -> Node
createNode = id

equals :: Node -> Node -> Bool
equals = (==)

data Dot a = Dot { 
                   names       :: [(Node, String)],
                   properties  :: [(Node, [Color])],
                   edges       :: [(Node, Node, Maybe String)],
                   value       :: a 
                 } deriving (Show)

data Color = Red | Black deriving (Show)

instance Monad Dot where
    return x = Dot [] [] [] x
    dot >>= f = Dot names'' props'' edges'' y
                where
                    (d_names, props, d_edges, x) = (names dot, properties dot, edges dot, value dot)
                    dot' = f x
                    (names', props', edges', y) = (names dot', properties dot', edges dot', value dot')
                    edges'' = d_edges ++ edges'
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
                    names'' = appendLabels (d_names ++ names') [] []
                    combinedPropLists = props ++ props'
                    -- combinePropertiesHelper :: Node -> [(Node, [Color])] -> [Color]
                    combinePropertiesHelper _ [] = []
                    combinePropertiesHelper node (np:nps) = props' ++ combinePropertiesHelper node nps
                                                                  where
                                                                      (n, p) = np
                                                                      props' = if node == n then p else []
                    -- combineProperties :: Node -> [Color]
                    combineProperties n = combinePropertiesHelper n combinedPropLists
                    nodes = nub $ map fst combinedPropLists
                    listOfProperties = map combineProperties nodes
                    props'' = zip nodes listOfProperties

-- Adding nodes and edges to the Dot string                    
addNode :: Node -> Dot Node
addNode n = Dot [(n, "__dot_0")] [] [] n

addEdge :: Node -> Node -> Maybe String -> Dot ()
addEdge src snk label = Dot [] [] [(src, snk, label)] ()

addColor :: Node -> Color -> Dot ()
addColor n p = Dot [] [(n, [p])] [] ()

escape :: Dot a -> a
escape (Dot _ _ _ x) = x

-- A way to get the Dot String out of the Dot monad
toString :: Dot a -> String
toString dot = "digraph G{\n" ++ getDotNodes nodes nodeNames nodeProps ++ getDotString nodeNames edges' ++ "}\n" 
                                         where
                                             nodes = map fst $ names dot
                                             nodeNames = names dot
                                             nodeProps = properties dot
                                             edges' = edges dot

{-- Helper functions to create the string --}

-- Get the unique name of the node
getName :: Node -> [(Node, a)] -> a
getName _      []     = error "No such node"
getName search (n:ns) = if search == node then name else getName search ns
                         where
                             (node, name) = n

getList :: Node -> [(Node, [a])] -> [a]
getList _      []     = []
getList search (n:ns) = if search == node then name else getList search ns
                         where
                             (node, name) = n

propertiesToString :: [Color] -> String
propertiesToString [] = ""
propertiesToString (Red:ps) = ",color=red" ++ propertiesToString ps
propertiesToString (Black:ps) = ",color=black" ++ propertiesToString ps

-- Write out all nodes with user made labels
getDotNodes :: [Node] -> [(Node, String)] -> [(Node, [Color])] -> String
getDotNodes [] _ _ = ""
getDotNodes (node:nodes) names props =  ("\t" ++ name ++ "[label=" ++ label ++ properties ++ "]\n") ++ (getDotNodes nodes names props)
                                     where
                                         label = node
                                         name = getName node names
                                         -- propertiesToString :: [Color] -> String
                                         properties = propertiesToString $ getList node props

-- Write out all edges                                
getDotString :: [(Node, String)] -> [(Node, Node, Maybe String)] -> String
getDotString [] _ = ""
getDotString _ [] = ""
getDotString names (e:es) = ("\t" ++ srcName ++ " -> " ++ snkName ++ edgeLabel ++ "\n") 
                            ++ getDotString names es
                               where
                                   (src, snk, maybeLabel) = e
                                   srcName = getName src names
                                   snkName = getName snk names
                                   edgeLabel = case maybeLabel of
                                                    Nothing -> ""
                                                    Just l  -> "[label=" ++ l ++ "]"

