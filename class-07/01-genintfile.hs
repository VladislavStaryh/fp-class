{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import System.Environment
import System.Random

createRandomStr :: Int -> Int -> Int -> StdGen -> (String, StdGen)
createRandomStr _ _ 0 gen = ("",gen)
createRandomStr from to 1 gen = let (x, newGen) = randomR(from,to) gen :: (Int, StdGen)
					in ((show x), newGen)

createRandomStr from to n_num gen = let (x, newGen) = randomR(from,to) gen :: (Int, StdGen) 
				    	(tail, newGen') = (createRandomStr from to (n_num-1) newGen)
						in ((((show x)++", ")++tail), newGen')


createRandomText :: Int -> Int -> Int -> Int -> StdGen -> [String]
createRandomText _ _ _ 0 gen = []
createRandomText from to n_num 1 gen = let (str, newGen) = createRandomStr from to n_num gen
	in [str]
createRandomText from to n_num n_str gen = let (str, newGen) = createRandomStr from to n_num gen
	in str:(createRandomText from to n_num (n_str-1) newGen)


createRandomFile :: String -> Int -> Int -> Int -> Int -> IO ()
createRandomFile fname from to n_num n_str = do
	gen <- newStdGen
	let int_lines = createRandomText from to n_num n_str gen
	writeFile fname (unlines int_lines)

main = do
  [fname, from, to, n_num, n_str] <- getArgs
  createRandomFile fname (read from) (read to) (read n_num) (read n_str)
