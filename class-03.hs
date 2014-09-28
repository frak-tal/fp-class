import Data.Char
import Data.List

{-
Явная рекурсия в решениях хотя и допускается, но не приветствуется. Старайтесь обходиться стандартными
функциями, используя при этом создание функций «на лету». Пытайтесь максимально упростить уже написанные
решения, применяя подходящие функции из модуля Data.List и любых других модулей. Перед выполнением заданий
изучите примеры из лекции по функциям высшего порядка. 
-}

{-
 1. Простейшие задачи на применение функций map и filter.
 1.1 Преобразовать данный список целых чисел следующим образом:
  a) увеличить все его элементы в два раза;
  b) увеличить все его элементы с четными значениями в два раза;
  с) обнулить все его элементы с нечетными значениями;
  d) удалить из него элементы, большие заданного числа k;
  e) отфильтровать его, оставив в списке только отрицательные числа;
  f) удалить из него все положительные чётные числа.
-}

f11a :: Integral a => [a] -> [a]
f11a = map (*2)

f11b :: Integral a => [a] -> [a]
f11b = map (\x -> if (even x) then 2*x else x)

f11c :: Integral a => [a] -> [a]
f11c = map (\x -> if (odd x) then 0 else x)

f11d :: (Integral a, Ord a) => [a] -> a -> [a]
f11d lst k = filter (<=k) lst

f11e :: (Integral a, Ord a) => [a] -> [a]
f11e = filter (<0)

f11f :: (Integral a, Ord a) => [a] -> [a]
f11f = filter (not . (\x -> x>0 && even x))

{-
 1.2 Дан список декартовых координат точек на плоскости (пар вещественных чисел).
     Преобразовать его следующим образом:
  a) отфильтровать список так, чтобы в нём остались точки из заданной координатной четверти;
  b) преобразовать декартовы координаты в полярные.
-}

f12a :: [(Double, Double)] -> Integer -> [(Double, Double)]
f12a lst q = filter (\el -> belongsToQuarter el q) lst 
  where
    belongsToQuarter :: (Double, Double) -> Integer -> Bool
    belongsToQuarter (x, y) 1 = x >= 0 && y >= 0
    belongsToQuarter (x, y) 2 = x <= 0 && y >= 0
    belongsToQuarter (x, y) 3 = x <= 0 && y <= 0
    belongsToQuarter (x, y) 4 = x >= 0 && y <= 0

f12b :: [(Double, Double)] -> [(Double, Double)]
f12b lst = map (\(x, y) -> (sqrt (x*x+y*y), phi (x, y))) lst
  where
    phi :: (Double, Double) -> Double
    phi (0, 0) = 0
    phi (x, y)
      | x >= 0 && y >= 0 = atan (y/x)
      | x < 0 = atan (y/x) + pi
      | x >= 0 && y<0 = atan(y/x) + 2 * pi
 
{-
 1.3 Дан список слов.
  a) Преобразовать все слова к верхнему регистру.
  b) Извлечь из него подсписок слов заданной длины.
  c) Извлечь из него подсписок слов, начинающихся с заданной буквы.
-}


f13a :: [String] -> [String]
f13a = map (map toUpper)

f13b :: [String] -> Int -> [String]
f13b lst l = filter (\str -> length str == l) lst

f13c :: [String] -> Char -> [String]
f13c lst ch = filter (`firstEqualTo` ch) lst
  where
    firstEqualTo :: String -> Char -> Bool
    firstEqualTo "" _ = False
    firstEqualTo (x:xs) ch = x == ch

{-
2. Формирование числовых последовательностей (iterate).
 a) Список натуральных чисел, начиная с 0.
 b) Список чётных чисел.
 c) Список элементов последовательности: a0=1, an=(1+a_(n-1))/2.
 d) Список символов английского алфавита.
 e) Список строк, представляющих n-значные двоичные числа.
-}

nats :: [Integer]
nats = iterate (+1) 0

evens :: [Integer]
evens = iterate (+2) 0

f2c :: [Double]
f2c = iterate ((/2) . (+1)) 1

f2d :: [Char]
f2d = ['a'..'z'] ++ ['A'..'Z']

f2e :: Int -> [String]
f2e n = (iterate (\xs ->  (map ('0' :) xs) ++ (map ('1' :) xs) ) [""]) !! n

f2eWithoutLeadingZeros :: Int -> [String]
f2eWithoutLeadingZeros 1 = f2e 1
f2eWithoutLeadingZeros n = filter (\(x:xs) -> x=='1') $ f2e n


{-
3. Группировка списков.
  a) Дан список символов. Сгруппировать подряд идущие символы по принципу: цифры — не цифры — ...
  b) Дан список пар вещественных чисел (координат точек на плоскости). Сгруппировать подряд идущие
     координаты точек, лежащие в одной координатной четверти.
  c) Дан список и ненулевое натуральное число n. Разбить список на подсписки длиной n каждый.
     Последний подсписок может содержать менее n элементов.
  d) Дан список и ненулевые натуральные числа n и m. Разбить список на перекрывающиеся подсписки
     длиной n элементов со сдвигом относительно предыдущего подсписка на m элементов.
  e) Дан список. Определить длину самого длинного подсписка, содержащего подряд идущие одинаковые элементы.
-}

f3a :: [Char] -> [[Char]]
f3a = groupBy (\x y -> isDigit x && isDigit y || (not . isDigit) x && (not . isDigit) y)

f3b :: [(Double, Double)] -> [[(Double, Double)]]
f3b = groupBy (\x y -> quarter x == quarter y)
  where
    quarter (x, y)
      | x>=0 && y>=0 = 1
      | x<0 && y>=0 = 2
      | x<0 && y<0 = 3
      | x>=0 && y<0 = 4


f3c :: [a] -> Int -> [[a]]
f3c lst n = map (take n) $ filter (\xs -> length xs `mod` n == length lst `mod` n && length xs > 0) (tails lst) 

f3d :: [a] -> Int -> Int -> [[a]]
f3d lst n m = map (take n) $ filter (\xs -> length xs `mod` m == length lst `mod` m && length xs > 0) (tails lst)

f3e :: (Eq a) => [a] -> Int
f3e lst = maximum (map (length) (group lst))  

-- Должно быть True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]

{-
4. Разные задачи.
 a) Дан текст в виде строки символов, содержащий среди прочего числовые данные. Посчитать количество
    всех упоминающихся в тексте чисел.
 b) Найти сумму всех чисел Фибоначчи, удовлетворяющих заданному предикату, в указанном промежутке
    (например: все чётные от 1 до 106).
 c) Дана строка текста и число n. Сформировать список, содержащий n самых часто используемых
    в строке символов.
 d) Дан список чисел. Сформировать список локальных максимумов исходного списка. Локальным максимумом
    называется элемент, больший своих соседей.
 e) Дан список. Продублировать все его элементы.
-}

f4a :: String -> Int
f4a str = length (filter (\(x:xs) -> isDigit x) (f3a str))

fib = 0 : 1 : zipWith (+) fib (tail fib)

f4b :: (Int -> Bool) -> Int -> Int -> Int
f4b p a b = sum $ filter p $ dropWhile (<a) $ takeWhile (<b) fib

f4c :: String -> Int -> [Char]
f4c str n = take n $ map fst $ sortBy (\(ch1, c1) (ch2, c2) -> compare c2 c1) $ map (\(x:xs) -> (x, length(x:xs))) $ group $ sort str 

f4d :: (Ord a) => [a] -> [a]
f4d lst =  map (\(x1:x2:x3:xs) -> x2) $ filter (\(x1:x2:x3:xs) -> x2>x1 && x2>x3) $ filter (\xs -> length xs == 3) $ map (take 3) $ tails lst 

f4e :: [a] -> [a]
f4e lst = concat $ map (\x -> [x, x]) lst
