{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point { x:: Double, y:: Double }
	 deriving (Show, Eq)
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = DirectionLeft | DirectionCenter | DirectionRight
	deriving(Show, Eq)
{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

dirRotate :: Point -> Point -> Point -> Direction
dirRotate a b c
	| f > 0 = DirectionLeft
	| f < 0 = DirectionRight
	| otherwise = DirectionCenter
	where f = (x b - x a) * (y c - y a) - (y b - y a) * (x c - x a)
	
directions :: [Point] -> [Direction]
directions [] = []
directions (x:y:s)= snd $
foldl (\(acc, ss) z -> ( (snd acc, z), dirRotate (fst acc) (snd acc) z : ss) )
((x, y), []) s
	 
{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

graham_scan :: [Point] -> [Point]
graham_scan = undefined

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}
