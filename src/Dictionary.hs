import Control.Monad
import Data.List
import qualified Data.Map as M

type Tree a = M.Map a [a]
type Dictionary = Tree String
type Graph = Tree Int

mapTree :: (Ord b) => (a -> b) -> Tree a -> Tree b
mapTree f m = M.fromList $ fmap (\(a,as) -> (f a, fmap f as)) $ M.toList m

nubTree :: (Ord a) => [Tree a] -> [Tree a]
nubTree = nubBy (\a b -> go (M.toList a) (M.toList b)) where
  go ((a0,a1):as) ((b0,b1):bs) = a0 == b0 && (sort a1 == sort b1) && go as bs

insertRecursive :: String -> [String] -> Dictionary -> Dictionary
insertRecursive s refs dic = foldl (\d w -> M.insert w [] d) dic' unknown where
  dic' = M.insert s refs dic
  unknown = filter (\r -> r `M.notMember` dic) refs

allItems :: Tree a -> [(a, a)]
allItems = allItems' . M.toList where
  allItems' = concat . fmap (\(k0,k1s) -> fmap (\s -> (k0,s)) k1s)

values :: (Eq a) => Tree a -> [a]
values = nub . foldl (\a (x,y) -> x:y:a) [] . allItems

elems :: (Eq a) => [a] -> [a] -> Bool
elems es k = and $ fmap (\e -> e `elem` k) es

replace :: Int -> [(Int,String)] -> String
replace a tr = maybe (show a) id (lookup a tr)

isEmbedded :: [(Int,Int)] -> [(String,String)] -> [(Int,String)] -> Bool
isEmbedded graph dic tr = dic `elems` graph' where
  graph' = fmap (\(a,b) -> (replace a tr, replace b tr)) graph

completePairs :: Graph -> Dictionary -> [[(Int, String)]]
completePairs graph dic = filter (isEmbedded (allItems graph) (allItems dic)) alls where
  g' = values graph
  d' = values dic
  subgs = nub $ fmap (take (length d')) $ permutations $ g'
  alls = fmap (flip zip d') subgs

complete :: Graph -> Dictionary -> [Dictionary]
complete graph dic = fmap (\tr -> mapTree (flip replace tr) graph) $ completePairs graph dic

main = do
  let a = M.fromList [("mother", ["father", "parent"])]
  putStrLn "---- original dictionary ----"
  print a

  let orderedGraph1 = M.fromList $ [(1,[2]), (2,[3,4,5]), (3,[5,6])]
  putStrLn "---- graph ----"
  print orderedGraph1

  let c1 = complete orderedGraph1 a
  putStrLn "---- completed dictionary ----"
  mapM_ print $ c1
