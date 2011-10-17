module AStar where

-- import Control.Monad.ST
-- import Data.STRef
-- import Util.Queue

data Problem st ac = Problem {
    initialState :: st
  , applicableActions :: st -> [ac]
  , transition :: st -> ac -> st
  , goalTest :: st -> Bool
  , actionCost :: ac -> Integer -- costs are assumed to be additive
  }

data Node st ac = Node {
    nodeState :: st
  , nodeParent :: Maybe (ac, Node st ac)
  , nodeCost :: !Integer
  }

startNode :: st -> Node st ac
startNode x = Node x Nothing 0

nodePath :: Node st ac -> [ac]
nodePath n = np [] n where
  np ret (Node _ Nothing _) = ret
  np ret (Node _ (Just (ac, n')) _) = np (ac:ret) n'

treeSearch :: Frontier f (Node st ac) -> Problem st ac -> Maybe [ac]
treeSearch f pr =
  ts (singleton f (startNode (initialState pr))) where
   ts fr =
     case pop f fr of
       Nothing -> Nothing
       Just (n, fr') ->
         case goalTest pr (nodeState n) of
           True -> Just (nodePath n)
           False -> ts (pushAll f fr' (children pr n))

singleton f v = push f v (empty f)

data Frontier f a = Frontier {
    empty :: f a
  , pop :: f a -> Maybe (a, f a)
  , push :: a -> f a -> f a
  }

pushAll :: Frontier f a -> f a -> [a] -> f a
pushAll f fr = foldl (flip (push f)) fr

children :: Problem st ac -> Node st ac -> [Node st ac]
children pr par@(Node st _ c) = map mkNode (applicableActions pr st) where
  mkNode ac = Node (transition pr st ac) (Just (ac, par)) (c + actionCost pr ac)
