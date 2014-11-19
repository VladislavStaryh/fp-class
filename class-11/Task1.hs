import Control.Monad
import Data.List
import Data.Ord


type Action = ((Int->Int->Int),Int)
type ModifyAction = (Int->Int)

modifyAction a = (fst a) (snd a) 


readAction :: String -> Action
readAction str = let [t,r,v] = (words str) in ((oper t), (read v))
			where oper t = if (t == "summand") then (+) else 
				       		if (t == "multiplier") then (*) else
				       			if (t == "divisor") then (div) else error "Operation undefined"

actionsFromFile f = readFile f >>= (return . map readAction . lines)


numFromFile :: FilePath -> IO [Int]
numFromFile f = readFile f >>= (return . (map read) . words)



--reformNumList [x:nL] aL = foldl (>>=) x (fst $ aL)   

reformNumFile f1 f2 = let actionsList = actionsFromFile f2
			  numList = numFromFile f1 
				in reformNumList numList actionsList



					
					
