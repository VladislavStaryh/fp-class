import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Ord

data Action = Sumand | Multipling | Division
	deriving (Show)

data Operation = Operation Action Int
	deriving (Show)

summand :: Integer -> Reader Integer Integer 	
summand x = do 	
	n <- ask 	
	return (x + n)

multipling :: Integer -> Reader Integer Integer 	
multipling x = do 	
	n <- ask 	
	return (x * n)
					
division :: Integer -> Reader Integer Integer 	
division x = do 	
	n <- ask 	
	return (x * n)


operationFromString::String->Operation
operationFromString s 
	|t=="summand" = Operation Sumand $ read n
	|t=="multiplier" = Operation Sumand $ read n
	|t=="divisor" = Operation Sumand $ read n
	where [t,ch,n] = (words s) 

applyOperation:: Operation->Int->Int
applyOperation (Operation Sumand y) x = x + y
applyOperation (Operation Multipling y) x = x * y
applyOperation (Operation Division y) x = div x y



appyOperations x = do
	op <- ask
	return $ foldr applyOperation op x

operationsFromFile f = readFile f >>= (return . map operationFromString . lines)

					
