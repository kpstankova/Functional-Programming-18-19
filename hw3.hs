main::IO()
main = do
 print (numOfNodes [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])])
 print (closestToAverage [(Temp 1 23.6),(Temp 6 24.2),(Temp 11 24.2),(Temp 16 21.2),(Temp 21 23.8),(Temp 26 26.5),(Temp 31 24.5)])
--Task1
type Node = (Int,[Int])
numOfNodes:: [Node]->Int
numOfNodes [] = 0
numOfNodes tree=length[m|(m,n)<-tree,(sum n) == (parent m)]
 where parent m = if null [u|(u,v)<-tree,elem m v] then 0 else head[u | (u,v)<-tree,elem m v]
  
--Task2
data Measuring = Temp Int Float

findAvg :: [Float]->Float
findAvg [] = 0
findAvg temps = (sum temps)/(fromIntegral (length temps))

getDay :: Measuring ->Int
getDay (Temp day temperature) = day

closestToAverage :: [Measuring]->Int
closestToAverage [] = 0 
closestToAverage measurings =
  getDay(foldl1(\currMeasuring@(Temp _ currTemp) bestMeasuring@(Temp _ bestTemp) -> 
      if abs(currTemp - avgTemp)<abs(bestTemp-avgTemp) then currMeasuring else bestMeasuring)measurings)
         where avgTemp = findAvg[t|(Temp d t)<-measurings]
        
--Task3
data BTree a = Empty | Node a (BTree a) (BTree a)

treeDepth :: BTree a -> Int
treeDepth Empty = 0
treeDepth (Node value (tr1) (tr2)) = 1 + max (treeDepth tr1) (treeDepth tr2)

getLeft :: BTree a -> BTree a
getLeft (Node value left right) = left

getRight :: BTree a -> BTree a
getRight (Node value left right) = right

getValue :: BTree a -> a
getValue (Node value left right) = value

treeNodesAtLevel :: (Eq b, Num b) => BTree a -> b -> [a]
treeNodesAtLevel Empty _ = []
treeNodesAtLevel (Node value _ _) 0 = [value]
treeNodesAtLevel (Node _ tr1 tr2) n = (treeNodesAtLevel tr1 (n-1)) ++  (treeNodesAtLevel tr2 (n-1))

grandchildrenIncreased :: (Num a, Ord a) => BTree a -> Bool
grandchildrenIncreased Empty = False
grandchildrenIncreased tree
  | (treeDepth tree == 1) || (treeDepth tree == 2) = True
  | otherwise = (grandchildrenIncreased (getLeft tree)) && (grandchildrenIncreased (getRight tree)) && (all (>= ((getValue tree) - 1)) (treeNodesAtLevel tree 2))