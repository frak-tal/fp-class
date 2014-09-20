-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms sec = (sec `div` 3600, sec `mod` 3600 `div` 60, sec `mod` 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt( (x2-x1)^2  + (y2-y1)^2 )

triangle :: (Point, Point, Point) -> (Double, Double)
triangle (z1, z2, z3) = (p, s)
  where
    a = distance z1 z2
    b = distance z1 z3
    c = distance z2 z3
    p = a + b + c
    pp = p / 2
    s = sqrt (pp * (pp-a) * (pp-b) * (pp-c))

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
  | even x = 1 + nEven xs
  | otherwise = nEven xs

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = (2*x) : (doubleElems xs)

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
  | odd x = x : (fltOdd xs)
  | otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
-- б) увеличить элементы с чётными значениями в два раза;
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).

delNeg :: (Num a, Ord a) => [a] -> [a]
delNeg [] = []
delNeg (x:xs)
  | x < 0 = delNeg xs
  | otherwise = x : (delNeg xs)

doubleEven :: Integral a => [a] -> [a]
doubleEven [] = []
doubleEven (x:xs)
  | even x = (2*x) : (doubleEven xs)
  | otherwise = doubleEven xs

exchange :: [a] -> [a]
exchange [] = []
exchange [x] = []
exchange (x:y:xs) = y : x : (exchange xs)

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x+y) : (combine_plus xs ys)

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.

pairedList :: [a] -> [b] -> [(a, b)]
pairedList [] ys = []
pairedList xs [] = []
pairedList (x:xs) (y:ys) = (x,y) : (pairedList xs ys)

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
-- б) в порядке возрастания.

nFirst :: Integer -> [Integer]
nFirst 0 = []
nFirst n = (nFirst (n-1)) ++ [n]

nFirstReversed :: Integer -> [Integer]
nFirstReversed 0 = []
nFirstReversed n = n : (nFirstReversed (n-1))

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.

insertEverywhere :: a -> [a] -> [a]
insertEverywhere el [] = []
insertEverywhere el [x] = [x]
insertEverywhere el (x:y:xs) = x : el : (insertEverywhere el (y:xs))

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).

subLists :: (Eq a) => [a] -> ([a], [a])
subLists [] = ([], [])
subLists (firstEl:xs) = sub_subLists (firstEl:xs)
  where
--    sub_subLists :: (Eq a) => [a] -> ([a], [a])	
    sub_subLists [] = ([], [])
    sub_subLists (y:ys) 
      | y == firstEl = ((y:(fst z)), snd z)
      | otherwise = ([], y:ys)
        where
          z = sub_subLists ys
 

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a

getEl :: [a] -> Int -> a
getEl [] i = error "Out of range"
getEl (x:xs) i 
  | i == 0 = x
  | i > 0 = getEl xs (i-1)
  | otherwise = error "Negative index"

-- б) Eq a => [a] -> a -> Bool

contains :: (Eq a) => [a] -> a -> Bool
contains [] el = False
contains (x:xs) el
  | x == el = True
  | otherwise = contains xs el

-- в) [a] -> Int -> [a]

take' :: [a] -> Int -> [a]
take' xs 0 = []
take' [] n = []
take' (x:xs) n
  | n>0 = (x:(take' xs (n-1)))
  | otherwise = [] 

-- г) a -> Int -> [a]

repeater :: a -> Int -> [a]
repeater el 0 = []
repeater el n
  | n>0 = el : (repeater el (n-1))
  | otherwise = error "n should be >=0"

-- д) [a] -> [a] -> [a]

zipLists :: [a] -> [a] -> [a]
zipLists [] ys = ys
zipLists xs [] = xs
zipLists (x:xs) (y:ys) = x : y : (zipLists xs ys)
  

-- е) Eq a => [a] -> [[a]]

groups :: Eq a => [a] -> [[a]]
groups [] = []
groups (x:xs) = addToGroups x (groups xs)
  where 
    addToGroups el [] = [[el]]
    addToGroups el (g:gs)
      | g `contains` el = (el:g):gs
      | otherwise = g:(addToGroups el gs)


-- ж) [a] -> [(Int, a)]

indexer :: [a] -> [(Int, a)]
indexer [] = []
indexer xs = sub_indexer xs 0
  where
    sub_indexer :: [a] -> Int -> [(Int, a)]
    sub_indexer [] n = []
    sub_indexer (x:xs) n = (n, x) : sub_indexer xs (n+1)

-- з) Eq a => [a] -> [a]

distinct :: Eq a => [a] -> [a]
distinct [] = []
distinct xs = sub_distinct [] xs
  where
    sub_distinct :: Eq a => [a] -> [a] -> [a]
    sub_distinct res [] = res
    sub_distinct res (x:xs) 
      | res `contains` x = sub_distinct res xs
      | otherwise = sub_distinct (res ++ [x]) xs

