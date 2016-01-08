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

-- Функция разблокировки кнопок (делает кнопки доступными для пользователя) 
enableButs :: [Button ()] -> IO () 
enableButs buttons = do -- номера кнопок от 0 до 3
  set (getElemFromListWithId buttons 0) [enabled := True] 
  set (getElemFromListWithId buttons 1) [enabled := True] 
  set (getElemFromListWithId buttons 2) [enabled := True] 
  set (getElemFromListWithId buttons 3) [enabled := True] 
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

-- Функция, которая выводит в метку один элемент сгенерированной последовательности
showOneColor :: IORef Bool -> Window a -> [Button ()] -> [(Button(), ColorInGame)] -> IORef UsedColors -> TextCtrl ()-> IO()
showOneColor refFlag f buttons pairs colors label = do
currUsedColors <- readIORef colors -- сгенерированные цвета
if (currUsedColors == []) then (do enableButs buttons; return()) else showOneColor_ refFlag f buttons pairs colors label

showOneColor_ :: IORef Bool -> Window a -> [Button ()] -> [(Button(), ColorInGame)] -> IORef UsedColors -> TextCtrl ()-> IO()
showOneColor_ refFlag f buttons pairs colors label = do
currUsedColors <- readIORef colors -- сгенерированные цвета
fl <- readIORef refFlag -- переменная-флаг
let c = head currUsedColors -- берется первый цвет
let str = colorToString c -- преобразуется к строке
let ourButton = findButtonOnColor c pairs -- находится соответствующая кнопка
-- В зависимости от значения переменной-флага мы либо устанавливаем цвет кнопки на игровой панели, либо обесцвечиваем ее
if (fl == True) then setColor refFlag ourButton c label else offColor refFlag ourButton label
if (currUsedColors == []) then return() else (if fl == False then (writeIORef colors (tail currUsedColors)) else (writeIORef colors currUsedColors))

-----------------------------------------------------------------

-- Функция, которая показывает игроку сгенерированную последовательность поэлементно с задержкой по таймеру
showColorsListWithDelay :: [Button ()] -> [(Button(), ColorInGame)] -> Window a -> IORef UsedColors -> TextCtrl () -> IO()
showColorsListWithDelay buttons pairs f generatedList label = do
    -- копируем generated list
    stateGenList <- readIORef generatedList
    generatedListCopy <- newIORef (stateGenList)
    flag <- newIORef True -- переменная-флаг
    t <- timer f [interval := 1000, on command := showOneColor flag f buttons pairs generatedListCopy label] -- таймер, реагирующий на команду, которая показывает цветную кнопку-метку
    set label [ text := ""]
    return () 

------------------------------------------------------------------


-- Проверка правильности нажатых кнопок и переход на следующий уровень
actionGUI :: [Button ()] -> [(Button(), ColorInGame)] -> Window a -> IORef Int -> TextCtrl () -> IORef UsedColors -> IO()
actionGUI buttons pairs f ref txtTitle constUsedColors = do
    st <- readIORef ref
    state <- generateGameLevel st
    writeIORef constUsedColors state
    showColorsListWithDelay buttons pairs f constUsedColors txtTitle
    writeIORef ref (st+1)
    return ()

-----------------------------------------------------------------

