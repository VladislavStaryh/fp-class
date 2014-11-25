import Control.Monad
import Control.Monad.Writer
import System.Environment
import Control.Monad.State

type Queue = [Int]
enqueue :: Int -> State Queue ()
enqueue x = do
	xs <- get
	put (xs ++ [x])


top :: [a] -> (a,[a])
top [] = error "no elements"
top [x] = (x,[])
top (x:xs) = let (y,ys) = (top xs) in (y,x:ys)


dequeue :: State Queue Int
dequeue = do
	(q) <- get
	let (x,xs) = (top q)
	put xs
	return x

test :: State Queue Int
test = do
	enqueue 1
	enqueue 2
	enqueue 3
	enqueue 4
	t <- dequeue
	--dequeue
	t1 <- dequeue
	dequeue
