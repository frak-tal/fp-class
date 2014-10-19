{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where

import Data.List

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point {
    x :: Double,
    y :: Double} deriving (Show, Eq)
  
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = LeftTurn | Collinear | RightTurn
               deriving (Show, Eq, Ord)

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

pointsToDirection :: (Point, Point, Point) -> Direction
pointsToDirection (a, b, c)
  | z > 0 = LeftTurn
  | z < 0 = RightTurn
  | z == 0 = Collinear
    where
      z = (x b - x a) * (y c - y a) - (y b - y a) * (x c - x a)

directions :: [Point] -> [Direction]
directions [] = []
directions [p1] = []
directions [p1, p2] = []
directions (p1:p2:ps) = map pointsToDirection $ snd $ foldl (\((a, b), res) c -> ((b, c), res ++ [(a, b, c)])) ((p1, p2), []) ps



{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

extractStartPoint :: [Point] -> (Point, [Point])
extractStartPoint [] = error "[]!"
extractStartPoint (p:ps) = foldl (\(p0, lst) cp -> if y cp < y p0 || (y cp == y p0  && x cp < x p0) then (cp, p0:lst) else (p0, cp:lst)) (p, []) ps

sortPoints :: Point -> [Point] -> [Point]
sortPoints p0 = Data.List.sortBy (\p1 p2 -> compare (ang' p1) (ang' p2))
  where
    ang p = atan ((y p - y p0)/(x p - x p0))
    ang' p
      | ang p >= 0 = ang p
      | otherwise = ang p + pi

graham_scan :: [Point] -> [Point]
graham_scan lst = reverse $ proc [p1, p0] lst2
  where
    (p0, lst1) = extractStartPoint lst
    (p1:lst2) = sortPoints p0 lst1
    proc res [] = res
    proc (pp:ppp:res) points
      | length not_suits == 0 = new_res
      | otherwise = proc new_res new_points
      where
        (suits, not_suits) = span (< RightTurn) $ directions (ppp:pp:points)
        new_res
          | length suits == 0 = (ppp:res)
          | length not_suits == 0 = (reverse points) ++ (pp:ppp:res)
          | otherwise = (reverse $ take (length suits - 1) points) ++ (pp:ppp:res)
        new_points = drop (length suits) points

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}

test_graham_scan1 = graham_scan [Point{x=0, y=0}, Point{x=2, y=0.1}, Point{x=2, y=5}, Point {x=1, y=1}, Point{x=0, y=10}] == [Point {x = 0.0, y = 0.0},Point {x = 2.0, y = 0.1},Point {x = 2.0, y = 5.0},Point {x = 0.0, y = 10.0}]

test_graham_scan2 = graham_scan [Point{x=1, y=1}, Point{x=4, y=3}, Point{x=2, y=0}, Point{x=2, y=2}, Point{x=4, y=1}, Point{x=3, y=2}, Point{x=3, y=4}, Point{x=1, y=3}, Point{x=2, y=3}] == [Point{x=2, y=0}, Point{x=4, y=1}, Point{x=4, y=3}, Point{x=3, y=4}, Point{x=1, y=3}, Point{x=1, y=1}]


test_graham_scan3 = graham_scan [Point{x=0, y=0}, Point{x=1, y=1}, Point{x=2, y=10}, Point{x=(-15), y=15}, Point{x=5, y=10}, Point{x=14, y=15}, Point{x=15, y=15}, Point{x=3, y=5}, Point{x=(-2), y=5}, Point{x=(-6), y=11}] == [Point{x=0, y=0}, Point{x=1, y=1}, Point{x=15, y=15}, Point{x=14, y=15}, Point{x=(-15), y=15}]

--False

test_graham_scan3' = graham_scan [Point{x=0, y=0}, Point{x=1, y=1}, Point{x=2, y=10}, Point{x=(-15), y=15}, Point{x=5, y=10}, Point{x=14, y=15}, Point{x=15, y=15}, Point{x=3, y=5}, Point{x=(-2), y=5}, Point{x=(-6), y=11}] == [Point{x=0, y=0}, Point{x=15, y=15}, Point{x=14, y=15}, Point{x=(-15), y=15}]

-- Точка (1, 1) (не попала в результат) - лежит на границе выпуклой оболочки и не является вершиной выпуклой оболочки (как и (14, 15), которая в результат попала)
