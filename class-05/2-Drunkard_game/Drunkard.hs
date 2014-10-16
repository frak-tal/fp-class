{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades
          | Clubs
          | Diamonds
          | Hearts
          deriving(Eq, Show)

data Value = Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Ten
           | Jack
           | Queen
           | King
           | Ace
           deriving(Eq, Ord, Show)

data Card = Card Value Suit
          deriving(Show)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ suit1) (Card _ suit2) = suit1 == suit2

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
(Card val1 _) `beats` (Card val2 _) = compare val1 val2

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round ([], deck2) = ([], deck2)
game_round (deck1, []) = (deck1, [])
game_round (card1:deck1, card2:deck2)
  | card1 `beats` card2 == GT = (deck1 ++ [card2, card1], deck2)
  | card1 `beats` card2 == LT = (deck1, deck2 ++ [card1, card2])
  | length next_deck1 > length deck1 || length deck2 == 0 = (next_deck1 ++ [card2, card1], next_deck2)
  | otherwise = (next_deck1, next_deck2 ++ [card1, card2])
    where
      (next_deck1, next_deck2) = game_round (deck1, deck2) 

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second
            deriving(Show, Eq)

game :: ([Card], [Card]) -> (Winner, Int)
game ([], _) = (Second, 0)
game (_, []) = (First, 0)
game decks = (w, rounds+1)
  where
    (w, rounds) = game $ game_round decks

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

test_game_1 = game ([Card Two Hearts, Card King Hearts, Card King Diamonds, Card Jack Diamonds, Card Ten Hearts, Card Jack Clubs, Card Queen Diamonds, Card Ten Clubs, Card Five Diamonds, Card Seven Spades], [Card Two Clubs, Card Seven Diamonds, Card Queen Hearts, Card Four Diamonds, Card Three Clubs, Card Six Spades, Card Ten Diamonds, Card Eight Spades, Card Seven Clubs, Card Eight Diamonds])
-- (First, 17)

test_game_2 = game ([Card King Spades, Card Six Hearts, Card Seven Hearts, Card King Clubs, Card Two Spades, Card Queen Spades, Card Nine Clubs, Card Nine Hearts, Card Five Spades, Card Five Hearts, Card Two Diamonds, Card Nine Spades, Card Eight Clubs, Card Three Spades, Card Four Spades], [Card Jack Spades, Card Six Diamonds, Card Ace Spades, Card Ace Diamonds, Card Ace Clubs, Card Five Clubs, Card Four Clubs, Card Eight Hearts, Card Jack Hearts, Card Ace Hearts, Card Six Clubs, Card Ten Spades]) 
-- ( 
-- Зацикливание?

test_game_3 = game ([Card Queen Hearts, Card Eight Diamonds, Card Six Spades, Card Six Hearts, Card Eight Spades, Card King Clubs, Card Seven Diamonds, Card Five Diamonds, Card Five Spades, Card Four Hearts], [Card Jack Clubs, Card Jack Hearts, Card King Spades, Card Queen Clubs, Card Seven Spades, Card Seven Hearts, Card Seven Clubs, Card Two Clubs, Card Queen Diamonds, Card Ace Clubs])
-- (Second, 54)

test_game_4 = game ([Card Two Hearts, Card King Hearts, Card King Diamonds, Card Jack Diamonds, Card Ten Hearts, Card Jack Clubs, Card Queen Diamonds, Card Ten Clubs, Card Five Diamonds, Card Seven Spades, Card Jack Spades, Card Six Diamonds, Card Ace Clubs, Card Five Clubs, Card Four Clubs, Card Eight Hearts, Card Jack Hearts, Card Six Clubs, Card Ten Spades], [Card Two Clubs, Card Seven Diamonds, Card Queen Hearts, Card Four Diamonds, Card Three Clubs, Card Six Spades, Card Ten Diamonds, Card Eight Spades, Card Seven Clubs, Card Eight Diamonds, Card King Spades, Card Six Hearts, Card Seven Hearts, Card King Clubs, Card Two Spades, Card Queen Spades, Card Nine Clubs, Card Nine Hearts, Card Five Spades, Card Five Hearts, Card Two Diamonds, Card Nine Spades, Card Eight Clubs, Card Three Spades, Card Four Spades])
-- (First,243)

test_game_5 = game ([Card Ace Spades, Card Jack Clubs, Card Eight Diamonds, Card Ace Diamonds, Card King Spades, Card Jack Hearts, Card Ace Hearts, Card Five Hearts, Card Ten Clubs, Card Four Clubs], [Card Two Spades, Card Four Spades, Card Nine Spades, Card Queen Hearts, Card Eight Spades, Card Queen Diamonds, Card Nine Clubs, Card Jack Spades, Card Six Spades, Card Eight Hearts, Card Six Hearts])
-- (First, 26)

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
