import System.Environment

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce = reduce a
		| a `mod` 3 == 0 = 0
		| odd a = a^2
		| otherwise = a^3

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF 0 a = a
reduceNF n a = reduceNF (n-1) (fmap reduce a)

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = foldr (\(a,b) t -> (a `mod` b ) : t) []

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe xs = Just (maximum $ toList xs)

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Null"
toEither xs = Right (maximum $ toList xs)

-- воспользуйтесь в этой функции случайными числами
toIO :: Integral a => [(a, a)]  -> IO a
toIO = undefined

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs arg = (head arg, read $ last arg)

readData :: FilePath -> IO [(Int, Int)]
readData fname = do
		text <- readFile fname
		return $ foldr (\x acc -> (parse x) : acc) [] $ text
			where
				parse x = let l = words x in (read $ head l, read $ last l)

main = do
	(fname, n) <- parseArgs `fmap` getArgs
	ps <- readData fname4
	print $ toList ps
  	print $ reduceNF n (toList ps)
	print $ reduceNF n (toMaybe ps)
  	print $ reduceNF n (toEither ps)
  	print $ reduceNF n (toEither ps)
	--reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}
