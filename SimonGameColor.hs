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

------------------------------------------------------------
