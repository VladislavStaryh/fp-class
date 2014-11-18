import Control.Monad
import Data.List
import System.Environment

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

readArmorItem str = let [k,t] = (words str) in (ArmorItem (read k) (read t))

loadInventory :: FilePath -> IO [ArmorItem]
loadInventory f = (readFile f) >>= return . map readArmorItem . lines

buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit k i = if length typeList == 5 then Just (ArmorKit k typeList) else Nothing
		 	where
				typeList = (nub $ map (\(ArmorItem _ t) -> t) $ filter (\(ArmorItem ki _) -> ki == k) i)

buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits = undefined

--main = (head `liftM` getArgs) >>= loadInventory >>= undefined >>= print
