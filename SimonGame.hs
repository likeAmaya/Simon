-- В этом файле будет реализована работа с пользователем и интерфейс игры

module Main where
import Graphics.UI.WX
import SimonGameColor

import System.IO
import Control.Monad
import Data.List

-----------------------------------------------------------

-- Информация об игре: авторы; какая библиотека для графики была использована
aboutGame :: Window a -> IO ()
aboutGame w
  = infoDialog w "Информация об игре:" "Авторы игры:\nАбащикова Александра,\nГаврилова Амида,\nСмолянинова Дарья\n\nИгра была написана с использованием библиотеки wxHaskell."

-----------------------------------------------------------
  
-- Информация об игре: правила игры для игрока
playerHelp :: Window a -> IO ()
playerHelp w
  = infoDialog w "Памятка игрока"
  (  "Как играть?\n\n"
  ++ "Есть две панели с четырьмя кнопками:\n"
  ++ "На верхней они загораются в процессе игры, показывая игроку какую кнопку нужно нажать.\n"
  ++ "На нижней панели кнопки для игрока.\n" 
  ++ "На них нужно нажимать в зависимости от загорающихся вверху.\n"
  ++ "В случае неправильного нажатия - конец игры, иначе переход на следующий уровень.\n\n"
  )
  
-----------------------------------------------------------

-- Функция для нахождения кнопки по цвету 
findButtonOnColor :: ColorInGame -> [(Button(), ColorInGame)] -> Button() 
findButtonOnColor color list = head $ foldl (\acc x -> if (snd x == color) then fst x : acc else acc) [] list 

-----------------------------------------------------------

-- Функция покраски кнопки в заданный цвет
setColor :: IORef Bool -> Button() -> ColorInGame -> TextCtrl () -> IO() 
setColor refFlag but color label
  | color == Green = do 
       set but [bgcolor := green] 
       set label [ text := "Green" ] 
       writeIORef refFlag False
  | color == Red = do 
       set but [bgcolor := red] 
       set label [ text := "Red" ] 
       writeIORef refFlag False 
  | color == Yellow = do 
       set but [bgcolor := yellow ] 
       set label [ text := "Yellow" ] 
       writeIORef refFlag False 
  | color == Blue = do 
       set but [bgcolor := blue] 
       set label [ text := "Blue" ] 
       writeIORef refFlag False 
  | otherwise = do  -- если нет такого
       set but [bgcolor := black] 
	   
-----------------------------------------------------------

-- Функция возвращения кнопке исходного цвета (обесцвечивание кнопки)
offColor :: IORef Bool -> Button() -> TextCtrl () -> IO() 
offColor refFlag but label = do 
  set but [bgcolor := white ] 
  set label [ text := " " ] 
  writeIORef refFlag True

-----------------------------------------------------------

-- Функция блокировки кнопок (делает кнопки недоступными для пользователя) 
disableButs :: [Button ()] -> IO () 
disableButs buttons = do -- номера кнопок от 0 до 3
  set (getElemFromListWithId buttons 0) [enabled := False] 
  set (getElemFromListWithId buttons 1) [enabled := False] 
  set (getElemFromListWithId buttons 2) [enabled := False] 
  set (getElemFromListWithId buttons 3) [enabled := False] 
return() 

-----------------------------------------------------------

-- В случае проигрыша пользователя
gameOver :: Window a -> IORef UsedColors -> IORef Int -> IORef UsedColors -> TextCtrl () -> TextCtrl() -> IO()

-- userList - пользовательская последовательность
-- levelList - список уровней
-- label - метка с номером уровня
-- levelInfo - информационная переменная

gameOver f userList n levelList label levelInfo = do
	endOfGame f 		-- выводим сообщение о проигрыше
	let tempList = [] 	-- обнуляем пользовательскую последовательность
	writeIORef userList tempList
	writeIORef n 1 		-- генерируем заново 1-ый уровень
	level <- generateGameLevel 1
	writeIORef levelList level
	set label [ text := stringLevel level]
	set levelInfo [ text := "Уровень начат! "]
	return()

-----------------------------------------------------------

-- Конец игры
endOfGame :: Window a -> IO ()
endOfGame w = infoDialog w "Конец игры!" "Неправильная последовательность цветов!"

-----------------------------------------------------------