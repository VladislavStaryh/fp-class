{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import Control.Monad
import Data.List
import Data.Ord

data Student = Student {name::String, age::Int, group::String}
	deriving (Eq)

instance Show Student where
	show (Student n a g) =  n ++ " " ++ show a ++ " " ++ g ++ "\n"


readStudent str = let [n,a,g] = (words str) in (Student n (read a) g)

groupForStudent :: [String] -> [String]
groupForStudent [] = []
groupForStudent xs = [unwords $ take 3 xs] ++ (groupForStudent $ drop 3 xs)

studentsFromFile :: FilePath -> IO [Student]
studentsFromFile f = readFile f >>= (return . (foldr (\x acc -> readStudent x : acc) []) . groupForStudent . lines)

studentsToFile :: FilePath -> [Student] -> IO ()
studentsToFile f students = writeFile f $ unlines $ map show students
	


main = (++) `liftM` studentsFromFile "students-in.txt" `ap` studentsFromFile "students-in2.txt" >>= studentsToFile "students-out.txt" . sortBy (comparing name)
