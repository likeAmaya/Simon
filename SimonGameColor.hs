-- В этом файле будут реализованы базовые функции для работы с цветом

module SimonGameColor where

import System.Random
import Data.List
import System.IO
import Control.Monad

import Control.Monad.Trans
-----------------------------------------------------------

-- Цвета наших кнопок: желтый, красный, зеленый, синий
data ColorInGame = Yellow | Red | Green | Blue
	deriving (Show, Eq)

-----------------------------------------------------------

-- Состояние, хранящее список последовательно появившихся цветов
type UsedColors = [ColorInGame] 

-----------------------------------------------------------

-- Функция, которая берет первые несколько элементов списка
getNElemFromList :: Int -> UsedColors -> UsedColors
getNElemFromList n list = take n list

-- Функция, которая достает из списка элемент по требуемому индексу
getElemFromListWithId :: [a] -> Int -> a
getElemFromListWithId list 0 = head list -- 0 элемент
getElemFromListWithId list n = getElemFromListWithId (tail list) (n-1)

------------------------------------------------------------

-- Выбор цвета случайным образом
randomColor :: IO ColorInGame
randomColor = do
  number <- randomRIO (1,4) :: IO Int -- выбор из 4 доступных цветов
  let color = getColorOnNumber number -- получение цвета по его номере, используется функция ниже
  return color -- в результате получаем цвет

------------------------------------------------------------

-- Получение цвета по его номеру
getColorOnNumber :: Int -> ColorInGame
getColorOnNumber n
  | (n==1) = Green -- зеленый
  | (n==2) = Red -- красный
  | (n==3) = Yellow -- желтый
  | otherwise = Blue -- синий

------------------------------------------------------------

-- Функция, генерирующая уровень, в которой n - номер уровня и количество зажигающихся цветов на этом уровне
generateGameLevel :: Int -> IO UsedColors
generateGameLevel n = forM [1..n] $ (\a -> do
						x <- randomColor -- случайный цвет
						return x)

-------------------------------------------------------------

-- Функция, проверяющая правильность нажатых пользователем цветов: на вход подается пользовательское состояние и правильное состояние
compareUsedColors :: UsedColors -> UsedColors -> Bool
compareUsedColors userUsedColors trueUsedColors = (userUsedColors == trueUsedColors) -- сравниваем

-------------------------------------------------------------

-- Определение победы на уровне
win :: UsedColors -> UsedColors -> IO()
win st1 st2
	| compareUsedColors st1 st2 = print "Congratulation! Level up"
	| otherwise = print "You lost"

-------------------------------------------------------------
-------------------------------------------------------------

-- Парсеры
parseStr :: String -> [ColorInGame]
parseStr str = parseList $ words str 

parseList :: [String] -> [ColorInGame]
parseList xs = map (\x -> prs x) xs
	where 	
		prs :: String -> ColorInGame
		prs x 
			| (x=="Red") = Red
			| (x=="Blue") = Blue
			| (x=="Yellow") = Yellow
			| otherwise = Green 

-- Преобразование списка цветов UsedColors к строке 

stringLevel :: UsedColors -> String
stringLevel st = foldl(\acc x -> acc ++ " " ++ show x) "" st 

-------------------------------------------------------------

-- Преобразование цвета к строке 

colorToString :: ColorInGame -> String
colorToString color = show color

-------------------------------------------------------------

action n = do
		state <- generateGameLevel n -- Генерируем уровень ( каждому уровню соответствует свое число загадываемых цветов, в результате будет получен писок цветов)
		putStrLn $ stringLevel state -- преобразуем полученный список к строке, далее эти цвета будут отображены на форме с некоторым интервалом
		str <- getLine 				 -- здесь получаем данные игрока
		let userList = parseStr str  -- преобразуем к списку цветов
		win userList state           -- сравниваем данные игрока и заданные игрой. определяем результат
		if (compareUsedColors userList state) then action (n+1) else win userList state  -- либо переходим на следующий уровень, либо выводится сообщение о проигрыше
