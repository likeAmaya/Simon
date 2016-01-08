-- В этом файле будут реализованы базовые функции для работы с цветом

import System.Random
import Data.List
import System.IO
import Control.Monad

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
getElemFromListWithId list 0 = head list
getElemFromListWithId list n = getElemFromListWithId (tail list) (n-1)

------------------------------------------------------------

-- Выбор цвета случайным образом
randomColor :: IO ColorInGame
randomColor = do
number <- randomRIO (1,4) :: IO Int -- выбор из 4 доступных цветов
let color = getColorOnNumber number — получение цвета по его номере, используется функция ниже
return color -- в результате получаем цвет

------------------------------------------------------------
-- Получение цвета по его номеру
getColorOnNumber :: Int -> ColorInGame
getColorOnNumber n
| (n==1) = Green 
| (n==2) = Red
| (n==3) = Yellow
| otherwise = Blue

------------------------------------------------------------