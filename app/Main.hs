
module Main where
import Data.Char
import Test.QuickCheck
import Prelude
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)


main :: IO ()
main = putStrLn "hello"



--defining the isSorted property
isSorted [] = True
isSorted (x:xs) = fst $ foldl step (True, x) xs
  where step (b, x) y = (b && (x <= y), y)

--quicksort
quicksort1 :: (Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (x:xs) =
  let smallerSorted = quicksort1 [a | a <- xs, a <= x]
      biggerSorted = quicksort1 [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

prop :: [Int] -> [Int] -> Property
prop xs ys = collect (length (xs++ys)) $ reverse (xs++ys) == (reverse xs) ++ (reverse ys)

data Example = E Int
instance Show (Example) where
   show (E x) = "Example "++(show x)

instance Arbitrary Example where
    arbitrary = do
     x <- arbitrary
     return $ (E x)

--define Tree
data Tree  = Node Int Tree Tree | Leaf 
--inOrder
inOrder::Tree -> [Int]
inOrder (Leaf)= []
inOrder (Node a b c) = (inOrder b)++[a]++(inOrder c)

insert::Int->Tree->Tree
insert a (Leaf)= Node a Leaf Leaf
insert a (Node x l r)
  | a < x = (Node x (insert a l) r)
  |otherwise = (Node x l (insert a r))

--insert a whole array:
insert' xs = foldr insert Leaf xs 

--sorted tree property
property2::[Int]->Bool
property2 xs = isSorted(inOrder(insert' xs))

--dataType for representing IO
data IOrep a = GetLine (String -> IOrep a) | PutLine String (IOrep a)| Return a

instance Functor IOrep where
  fmap = liftM

instance Applicative IOrep where
  pure  = return
  (<*>) = ap

--Monad to representing IO
instance Monad IOrep where
	GetLine f >>= g = GetLine (\s -> f s >>= g)
	PutLine s ma >>= g = PutLine s (ma >>= g)
	Return a >>= g = g a
	return = Return


readLn ::Read a => IOrep a
readLn = fmap read (GetLine Return)

print :: Show a => a -> IOrep ()
print x = PutLine (show x) (Return ())

--Datatype to trace the in and outputs
data Trace = Read String Trace
	| Write String Trace
	| Stop

--IOrep to Trace
runrep ::IOrep () -> [String] -> Trace
runrep (GetLine f) (x : xs) = Read x (runrep (f x) xs)
runrep (PutLine s ma) xs = Write s (runrep ma xs)
runrep (Return ()) [ ] = Stop