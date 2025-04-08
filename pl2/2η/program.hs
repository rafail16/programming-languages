import Control.Exception (evaluate)
import Control.Monad (forM_)
import System.TimeIt (timeIt)

data Tree a = Node a [Tree a]
  deriving (Eq, Show, Read)

-- Add your solutions here!
-- problem 2a
leftish :: Int -> Tree Int
leftish 0 = Node 0 []
leftish t = Node t [leftish (t-1)
                   , Node t []
                    ]

-- problem 2b
mirror :: Tree a -> Tree a
mirror (Node x []) = Node x []
mirror (Node a children) = Node a (reverse children [])
  where reverse :: [Tree a] -> [Tree a] -> [Tree a]
        reverse [] res = res
        reverse (x:xs) res = reverse xs ((mirror x): res)
      
-- problem 2c
-- complexity O(n^2) adds from left to right to list of the fringe
fringe_naive :: Tree a -> [a]
fringe_naive t = aux t
  where aux :: Tree a -> [a]
        aux (Node a []) = [a]
        aux (Node x ts) = ts'
          where ts' = auxs ts
        auxs :: [Tree a] -> [a]
        auxs [] = []
        auxs (t : ts) = t' ++ ts'
          where t' = aux t
                ts' = auxs ts

-- problem 2d
-- complexity O(n), as it is like a dfs that will eventually add tail reccursively elements to the fringe list. So at the end it
-- will have just run through the entirety of nodes of out tree
fringe :: Tree a -> [a]
fringe t = leaves t []
  where leaves :: Tree a -> [a] -> [a]
        leaves (Node t []) xs = t:xs
        leaves (Node t ts) xs = (leaves' ts xs)
          where leaves' :: [Tree a] -> [a] -> [a]
                leaves' [] xs = xs
                leaves' (t : ts) xs = leaves t (leaves' ts xs)

-- problem 2e
-- It calls the fringe function so the complexity of the problem is O(n+m) = O(max(n, m)), with n, m being the number of nodes 
-- in the trees we check.
-- Lazinees in Haskell makes it possible for the fringes not to be computed when not needed. So it will compute till the desired 
-- values and so, when we face the first difference the computatuion will stop and result in unequal fringes
same_fringe :: Eq a => Tree a -> Tree a -> Bool
same_fringe t1 t2 = (x == y)
  where x = fringe t1
        y = fringe t2

-- problem 2f 
-- If haskell was an eager language it wouldn't be so easy to check if the two fringes are the same. It is much more 
-- difficult in a language like ML to have the same complexity and manage to have the effectiveness of the different 
-- fringes. If we just want to calculate the fringes and then see if the two lists created are equal, we would indeed
-- have a complexity of O(n+m) all the time, but the false case isn't effective like in Haskell. Both fringes will be 
-- calculated thus the base time will be in order to run the fringe function for both Trees. Having the same lazyness 
-- is a difficult task. In a binary tree we can see the code: https://rosettacode.org/wiki/Same_fringe#OCaml and understand 
-- the problems we have to face. We have to either track the parse state of the tree or use Stream (lazy datatype). However, 
-- if we just want to finish every time in O(n+m) we have to calculate the fringes and then call the same fringe func
-- datatype 'a bintree = Node of 'a * 'a bintree list
-- fun fringe (Node (x,[])) = [x]
--  | fringe (Node(x, ts)) =
--    let
--        fun helpi [] = []
--        | helpi (t:: ts') = (fringe t) @ (helpi ts');
--    in (helpi ts) end;
-- fun same_fringe (a, b)=  ((fringe a) = (fringe b));


-- problem 2g
-- The simplest way to create a Fibonacci tree is to start from n and going left to (n-1) and right to (n-2). This, as  when 
-- we compute Fibonacci numbers, will have to compute all the values and the subtrees so in the end we will need memory relevant 
-- to the number the naive computation of the Fibonacci sequence, so the total memory of the tree will be O(2^n).
fibtree_naive :: Int -> Tree Int
fibtree_naive 0 = Node 0 []
fibtree_naive 1 = Node 1 []
fibtree_naive n = Node (a + b) [(Node a t), (Node b t')]
  where (Node a t) = fibtree_naive (n-1)
        (Node b t') = fibtree_naive (n-2)

-- problem 2h
-- The way to create a fibonacci tree that uses only O(n) memory space is to make sure that each time we use a node we don't compute 
-- it but through the laziness of haskell we just make a link to the a common one for every use. So by creating the tree bottom up we 
-- manage to link every appearance of a node to the first one so we need only O(n) for the tree in Haskell to be kept. This is all 
-- feasible because of the graph reduction done in haskell.
fibtree :: Int -> Tree Int
fibtree n = aux n t1 t2 1 2
  where aux :: Int -> Tree Int -> Tree Int -> Int -> Int -> Tree Int
        t1 =  Node 1 []
        t2 = Node 1 [Node 1 [], Node 0 []]
        aux 1 _ _ _ _ = Node 1 []
        aux 2 _ _ _ _ = Node 1 [Node 1 [], Node 0 []]
        aux n ta tb a b = auxs (n-1) ta tb a b
          where auxs :: Int -> Tree Int -> Tree Int -> Int -> Int -> Tree Int
                auxs 2 ta tb _ b = Node b [tb, ta]
                auxs n ta tb a b = let sum = (a+b) in sum `seq` aux n tb (Node b [tb, ta]) b sum

-- problem 2i
-- After the computation of the expression: mirror (fibtree n), the tree that wiil be created will eventually take space of O(2^n) in memory.
-- This will happen because the tree will be calculated from the iteration in its nodes and therefore we are going to have memory space
-- relative to the number of nodes in the fibonacci tree. There are no longer going to be any edges in the graph reduction that will connect
-- the nodes of the graph to the next we want (they will be reduced through the iteration in the mirror function). 

t = Node 'a' [ Node 'b' [ Node 'd' [Node 'i' []]
                        , Node 'e' [Node 'j' [], Node 'k' []]
                        ]
             , Node 'c' [ Node 'f' [Node 'l' [], Node 'm' []]
                        , Node 'g' []
                        , Node 'h' [Node 'n' []]
                        ]
             ]

tm = Node 'a' [ Node 'c' [ Node 'h' [ Node 'n' []]
                         , Node 'g' []
                         , Node 'f' [Node 'm' [], Node 'l' []]
                         ]
              , Node 'b' [ Node 'e' [Node 'k' [], Node 'j' []]
                         , Node 'd' [Node 'i' []]
                         ]
              ]

test_correctness msg testcases = do
  putStr $ msg ++ ": " ++ (if and testcases then "OK" else "FAIL!!!") ++ "\n"

test_complexity msg range f = forM_ range $ \n -> do
  putStr $ msg ++ " with size " ++ show n ++ ", "
  timeIt $ evaluate $ f n

main = do
  test_correctness "mirror correctness" $
    [mirror t == tm] ++
    [mirror (mirror t) == t | n <- [0..100], let t = leftish n] ++
    [mirror (mirror t) == t | n <- [0..15], let t = fibtree n]
  test_correctness "fringe_naive correctness" $
    [fringe_naive t == "ijklmgn"] ++
    [fringe_naive (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe_naive (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe_naive leftish" [100, 1000, 10000, 20000, 30000] $
    length . fringe_naive . leftish
  test_correctness "fringe correctness" $
    [fringe t == "ijklmgn"] ++
    [fringe (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe leftish" [100, 1000, 10000, 20000, 30000] $
    length . fringe . leftish
  test_correctness "same_fringe correctness" $
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]] ++
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]]
  test_complexity "mirror fibtree_naive" [20, 25, 30, 32] $ \n ->
    let t = fibtree_naive n in mirror (mirror t) == t
  test_complexity "mirror fibtree" [20, 25, 30, 32] $ \n ->
    let t = fibtree n in mirror (mirror t) == t
  test_complexity "same_fringe fibtree_naive" [20, 25, 30, 32] $ \n ->
    same_fringe (fibtree_naive n) (fibtree_naive (n+1))
  test_complexity "same_fringe fibtree" [20, 25, 30, 32, 34, 36] $ \n ->
    same_fringe (fibtree n) (fibtree (n+1))