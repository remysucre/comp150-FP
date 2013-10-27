{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

{-
   DotWriter: Monad used to help create a graph visualization by
              creating a dot string.

              Alleviates the need for the user to create unique
              names, but requires a String representation of their
              values.

  NOTE: To add a node property, you need to do the following
        1) Define a datatype
        2) Make it an instance of Show
        3) Add it as a constructor to the Property datatype
        4) Add cases to propertiesToString so your property is
           correctly converted to an attribute
        5) Create a function that returns Dot () that adds
           the property to a Node
 
        For examples, see Color, Shape, and Style and their
        corresponding add functions.

  (c)2013 Diogenes A. Nunez
-}            

module Monads.DotWriter 
( Node
  , createNode, equals
  , Color(..), Shape(..), Style(..)
  , Dot
  , addNode, addEdge
  , addColor, addShape, addStyle
  , escape
  , toString
) where

import Data.List (nub)

type Node = String

createNode :: String -> Node
createNode = id

equals :: Node -> Node -> Bool
equals = (==)


-- Color property
data Color = Red | Black
instance Show Color where
    show Red = "red"
    show Black = "black"

-- Shape property
data Shape = None | Circle | Box | Ellipse
                  | DoubleCircle 
instance Show Shape where
    show None = "none"
    show Circle = "circle"
    show Box = "box"
    show Ellipse = "ellipse"
    show DoubleCircle = "doublecircle"

data Style = Empty | Filled | Bold | Rounded
instance Show Style where
    show Empty = "empty"
    show Filled = "filled"
    show Bold = "bold"
    show Rounded = "rounded"

data Property = PColor Color
              | PShape Shape
              | PStyle Style
              deriving (Show)

-- The structure for the DotWriter
data Dot a = Dot { 
                   names       :: [(Node, String)],
                   properties  :: [(Node, [Property])],
                   edges       :: [(Node, Node, Maybe String)],
                   value       :: a 
                 } deriving (Show)

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
                    -- function to ensure all labels are unique
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
                    -- combinePropertiesHelper :: Node -> [(Node, [Property])] -> [Property]
                    combinePropertiesHelper _ [] = []
                    combinePropertiesHelper node (np:nps) = props' ++ combinePropertiesHelper node nps
                                                                  where
                                                                      (n, p) = np
                                                                      props' = if node == n then p else []
                    -- combineProperties :: Node -> [Property]
                    combineProperties n = combinePropertiesHelper n combinedPropLists
                    nodes = nub $ map fst combinedPropLists
                    listOfProperties = map combineProperties nodes
                    props'' = zip nodes listOfProperties

-- Adding nodes and edges to the Dot string                    
addNode :: Node -> Dot Node
addNode n = Dot [(n, "__dot_0")] [] [] n

addEdge :: Node -> Node -> Maybe String -> Dot ()
addEdge src snk label = Dot [] [] [(src, snk, label)] ()

-- Adding Properties to Nodes
addColor :: Node -> Color -> Dot ()
addColor n col = Dot [] [(n, [PColor col])] [] ()

addShape :: Node -> Shape -> Dot ()
addShape n shp = Dot [] [(n, [PShape shp])] [] ()

addStyle :: Node -> Style -> Dot ()
addStyle n sty = Dot [] [(n, [PStyle sty])] [] ()

-- Escaping the monad and grabbing the result
escape :: Dot a -> a
escape (Dot _ _ _ x) = x

-- A way to get the Dot String out of the Dot monad
toString :: Dot a -> String
toString dot = "digraph G{\n" ++ getDotNodes nodes nodeNames nodeProps ++ getDotEdges nodeNames edges' ++ "}\n" 
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

-- Grab a list of items corresponding to a Node
getList :: Node -> [(Node, [a])] -> [a]
getList _      []     = []
getList search (n:ns) = if search == node then name else getList search ns
                         where
                             (node, name) = n

-- Transform all properties into a single String
propertiesToString :: [Property] -> String
propertiesToString [] = ""
propertiesToString (p:ps) = propertyString ++ propertiesToString ps
                            where
                                propertyString = case p of
                                                      PColor c -> ",color=" ++ show c
                                                      PShape None -> ",shape=plaintext"
                                                      PShape shp -> ",shape=" ++ show shp
                                                      PStyle Empty -> ""
                                                      PStyle sty -> ",style=" ++ show sty

-- Write out all nodes with user made labels
getDotNodes :: [Node] -> [(Node, String)] -> [(Node, [Property])] -> String
getDotNodes [] _ _ = ""
getDotNodes (node:nodes) names props =  ("\t" ++ name ++ "[label=" ++ label ++ properties ++ "]\n") ++ (getDotNodes nodes names props)
                                        where
                                            label = node
                                            name = getName node names
                                            properties = propertiesToString $ getList node props

-- Write out all edges with the unique node labels                               
getDotEdges :: [(Node, String)] -> [(Node, Node, Maybe String)] -> String
getDotEdges [] _ = ""
getDotEdges _ [] = ""
getDotEdges names (e:es) = ("\t" ++ srcName ++ " -> " ++ snkName ++ edgeLabel ++ "\n") ++ getDotEdges names es
                           where
                               (src, snk, maybeLabel) = e
                               srcName = getName src names
                               snkName = getName snk names
                               edgeLabel = case maybeLabel of
                                                Nothing -> ""
                                                Just l  -> "[label=" ++ l ++ "]"

