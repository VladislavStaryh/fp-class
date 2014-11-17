import Control.Monad 
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Either String Pole
updatePole p = if unbalancedRight p then Left "Right side unbalanced" else if unbalancedLeft p then Left "Left side unbalanced" else Right p
  where
	unbalancedRight (l, r) =  r-l > balance
	unbalancedLeft (l, r) =  l-r > balance


landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)

landBalanced :: Birds -> Pole -> Either String Pole
landBalanced n (left, right) = updatePole (left+n, right + n)

banana :: Pole -> Either String Pole
banana = const (Left "Banana is reason")

unlandAll :: Pole -> Either String Pole
unlandAll = const (Right (0,0))

actionByString :: Pole -> String -> Either String Pole
actionByString  p "B" = banana p
actionByString  p "UA" = unlandAll p
actionByString p str = let (s , n) = (head str, read $ drop 2 str :: Birds) in
		if s == 'R' then landRight n p else
		if s == 'B' then landBalanced n p else landLeft n p

--tests = all test [1..3]
  --where
    --test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
      --        >>= landLeft (-1) >>= landRight (-2)) == Nothing
    --test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Just (2, 4)
    --test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Nothing
