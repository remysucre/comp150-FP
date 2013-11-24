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

  Thanks to 
      - Norman for explaining the concept after attempt #4
      - Max, Andrew, Zhe for designing the Property datatype

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

--import Debug.Trace
import Data.List (nub)

-- Internal Node representation for Dot structure
type Node = (String, String)

createNode :: String -> Node
createNode str = (str, "__dot_0")

equals :: Node -> Node -> Bool
equals = (==)

{--- Node attributes for Graphviz ---}

-- Color of the node
data Color = Red | Black
instance Show Color where
    show Red = "red"
    show Black = "black"

-- Shape of the node
data Shape = None | Circle | Box | Ellipse
                  | DoubleCircle 
instance Show Shape where
    show None = "none"
    show Circle = "circle"
    show Box = "box"
    show Ellipse = "ellipse"
    show DoubleCircle = "doublecircle"

-- Style of the node
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
                   names       :: [Node],
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
                    -- contains :: Eq a => [a] -> a -> Bool
                    contains xs x = any (== x) xs 
                    -- function to ensure all labels are unique
                    -- appendLabels :: [Node] -> [String] -> [(Node, Node)]
                    renameDict [] _ = []
                    renameDict (l:ls) unique = let (label, name) = l
                                                   count = length unique
                                                   newName = "__dot_" ++ show count
                                                   in if contains unique name
                                                      then [(l, (label, newName))] ++ renameDict ls (newName:unique)
                                                      else [(l, l)] ++ renameDict ls (name:unique)
                    dict = renameDict (nub $ d_names ++ names') []
                    names'' = (map snd dict)
                    -- renameEdges :: [(Node, Node, Maybe String)] -> [(Node, Node)] -> [(Node, Node, Maybe String)]
                    renameEdges [] _ = []
                    renameEdges (e:es) nodes = [(src', snk', label)] ++ renameEdges es nodes
                                               where
                                                   (src, snk, label) = e
                                                   src' = case lookup src nodes of
                                                             Nothing -> error $ "No such node as src: " ++ show src ++ " " ++ show nodes
                                                             Just s -> s
                                                   snk' = case lookup snk nodes of
                                                             Nothing -> error $ "No such node as snk: " ++ show snk ++ " " ++ show nodes
                                                             Just s -> s
                    edges'' = renameEdges (d_edges ++ edges') dict
                    -- remapProps :: [(Node, Property)] -> [(Node, Node)] -> [(Node, Property)]
                    remapProps [] _ = []
                    remapProps (np:nps) nodes = [(node', prop)] ++ remapProps nps nodes
                                                where
                                                    (node, prop) = np
                                                    node' = case lookup node nodes of
                                                               Nothing -> error $ "No such node created: " ++ show node ++ " " ++ show nodes
                                                               Just s -> s
                    combinedPropLists = props ++ (remapProps props' dict)
                    -- combinePropertiesHelper :: Node -> [(Node, [Property])] -> [Property]
                    combinePropertiesHelper _ [] = []
                    combinePropertiesHelper node (np:nps) = props' ++ combinePropertiesHelper node nps
                                                                  where
                                                                      (n, p) = np
                                                                      props' = if node == n then p else []
                    -- combineProperties :: Node -> [Property]
                    combineProperties n = combinePropertiesHelper n combinedPropLists
                    listOfProperties = map combineProperties names''
                    props'' = zip names'' listOfProperties

-- Adding nodes and edges to the Dot string                    
addNode :: Dot a -> Node -> Dot Node
addNode dot n = dot' >> return node
                where
                    addNode' n = Dot [n] [] [] n
                    dot' = dot >> addNode' n >> return ()
                    node = head . reverse $ names dot'

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
toString dot = "digraph G{\n" ++ getDotNodes nodes nodeProps ++ getDotEdges edges' ++ "}\n" 
                                         where
                                             nodes = names dot
                                             nodeProps = properties dot
                                             edges' = edges dot

{-- Helper functions to create the string --}

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
getDotNodes :: [Node] -> [(Node, [Property])] -> String
getDotNodes [] _ = ""
getDotNodes (node:nodes) props =  ("\t" ++ name ++ "[label=" ++ label ++ properties ++ "]\n") ++ (getDotNodes nodes props)
                                        where
                                            (label, name) = node
                                            properties = propertiesToString $ getList node props

-- Write out all edges with the unique node labels                               
getDotEdges :: [(Node, Node, Maybe String)] -> String
getDotEdges [] = ""
getDotEdges (e:es) = ("\t" ++ srcName ++ " -> " ++ snkName ++ edgeLabel ++ "\n") ++ getDotEdges es
                           where
                               (src, snk, maybeLabel) = e
                               srcName = snd src
                               snkName = snd snk
                               edgeLabel = case maybeLabel of
                                                Nothing -> ""
                                                Just l  -> "[label=" ++ l ++ "]"

