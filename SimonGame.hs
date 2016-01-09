-- В этом файле будет реализована работа с пользователем и интерфейс игры

module Main where
import Graphics.UI.WX
import SimonGameColor

import System.IO
import Control.Monad
import Data.List
import Data.IORef
import Control.Concurrent (threadDelay)

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
findButtonOnColor color list = head $ foldl (\acc x -> if (snd x == color) then fst x : acc else acc) [] list -- ищем цвет в списке пар

-----------------------------------------------------------

-- Функция покраски кнопки в заданный цвет
setColor :: IORef Bool -> Button() -> ColorInGame -> TextCtrl () -> IO() 
setColor refFlag but color label
  | color == Green = do -- если зеленый
       set but [bgcolor := green] 
       set label [ text := "Green" ] 
       writeIORef refFlag False
  | color == Red = do -- если красный
       set but [bgcolor := red] 
       set label [ text := "Red" ] 
       writeIORef refFlag False 
  | color == Yellow = do -- если желтый
       set but [bgcolor := yellow ] 
       set label [ text := "Yellow" ] 
       writeIORef refFlag False 
  | color == Blue = do -- если зеленый
       set but [bgcolor := blue] 
       set label [ text := "Blue" ] 
       writeIORef refFlag False 
  | otherwise = do  -- если нет такого цвета
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
	set levelInfo [ text := "New level begun"]
	return()

-----------------------------------------------------------

-- Конец игры
endOfGame :: Window a -> IO ()
endOfGame w = infoDialog w "Game end" "Wrong colors sequence" 

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
    st <- readIORef ref -- номер уровня
    state <- generateGameLevel st -- генерирование цветовой последовательности согласно уровню
    writeIORef constUsedColors state
    showColorsListWithDelay buttons pairs f constUsedColors txtTitle -- генерация игрой цветовой последовательности, то есть игрок видит сменяющиеся цвета
    writeIORef ref (st+1)
    return ()

-----------------------------------------------------------------

-- Обнуляет пользовательский список перед генерацией нового игрового уровня и запускает новый уровень
nullUsersList :: [Button()] -> [(Button(), ColorInGame)] -> Window a -> IORef UsedColors -> IORef Int -> TextCtrl () -> IORef UsedColors -> TextCtrl() -> IO()
nullUsersList buttons pairs f userList n textField st1 levelInfo = do
	nn <- readIORef n
	set levelInfo [ text := "You win in level " ++ show (nn-1) ++ "!"] -- генерация сообщения о прохождении уровня
	let tempList = [] -- для нового уровня
	writeIORef userList tempList
	actionGUI buttons pairs f n textField st1 -- запуск с новыми данными
	return()

-----------------------------------------------------------------

-- Функция, вызываемая по нажатию на кнопки 
actionUserButtons :: [Button ()] -> [(Button(), ColorInGame)] -> TextCtrl () -> IORef Int -> Window a -> IORef UsedColors -> IORef UsedColors -> ColorInGame -> TextCtrl () -> IO() 
actionUserButtons buttons pairs textField n w refUser st1 butColor levelInfo = do 
  st <- readIORef refUser 
  constUsedColors <- readIORef st1 
  let modifiedUserList = st ++ [butColor] 
  writeIORef refUser modifiedUserList 
  let partOfGenList = getNElemFromList (length modifiedUserList) constUsedColors 
  -- Сравнивает текущий подсписок с соответствующим по длине подскиском программы лексико-графически 
  let equal = compareUsedColors modifiedUserList partOfGenList 
  -- Если подпоследовательность не равна соответствующей подпоследовательности списка программы, то
  -- была допущена ошибка, программа завершается 
  if (equal == False) then (gameOver w refUser n st1 textField levelInfo) else 
    if (length modifiedUserList == length constUsedColors) then (do 
      disableButs buttons; -- дезактивация кнопок
      nullUsersList buttons pairs w refUser n textField st1 levelInfo ; -- обнуление
      return()) 
    else return()

-----------------------------------------------------------------

-- Запуск всей игры
main :: IO ()
main = start $ startGame

-- Здесь собирается весь наш интерфейс
startGame :: IO ()
startGame =  do
  -- Наша форма
  f <- frameFixed [ text := "Simon", bgcolor := white, clientSize := sz 200 10] -- picture := "/home/oleg/Simon/fon.jpeg"]
  let n = 1 -- начинаем с этого уровня
  state <- generateGameLevel n -- генерируется список цветов, в зависимости от номера уровня
  let level = stringLevel state
  ref <- newIORef (n+1)

   -- Это список, в который записивается сгенерированная программой последовательность для определенного игрового уровня
  gameState <- newIORef state

  -- Это список, который накапливает значения на определенном уровне по нажатию цветных кнопок пользователя
  let userList = []
  gameUserList <- newIORef userList
 
  -- 
  txtTitle <- entry f [text := level , bgcolor := cyan, enabled := False ]
  congr <- entry f [text := "Level begun!", bgcolor := green, enabled := False ] 

  taskb1 <- button f [ text := " ", bgcolor  := white, clientSize := sz 100 100, enabled := False  ]
  taskb2 <- button f [ text := " " , bgcolor  := white, clientSize := sz 100 100, enabled := False ]
  taskb3 <- button f [ text := " " , bgcolor  := white, clientSize := sz 100 100, enabled := False ]
  taskb4 <- button f [ text := " " , bgcolor  := white, clientSize := sz 100 100, enabled := False ]
  
  let listOfPairTaskButtons = (taskb1, Green ):(taskb2, Red):(taskb3, Yellow):(taskb4, Blue):[]
  
  -- gameButton, кнопочки на которые непосредственно нажимает игрок
  b1 <- button f [ text := " ", bgcolor  := green, clientSize := sz 100 100 ]
  b2 <- button f [ text := " " , bgcolor  := red , clientSize := sz 100 100 ]
  b3 <- button f [ text := " " , bgcolor  := yellow , clientSize := sz 100 100 ]
  b4 <- button f [ text := " " , bgcolor  := blue, clientSize := sz 100 100 ]

  -- список, куда мы помещаем кнопки
  let listUserButtons = b1:b2:b3:b4:[]
  
  -- Кнопки с помощью, выходом и информацией об игре 
  q <- button f [ text := "Выход из игры", bgcolor  := red, on command := close f ]
  h <- button f [ text := "Правила игры", bgcolor  := green, on command := playerHelp f ]
  a <- button f [ text := "Об игре", bgcolor := blue, on command := aboutGame f ]

  set b1 [ on command := actionUserButtons listUserButtons listOfPairTaskButtons txtTitle ref f gameUserList gameState Green  congr ]
  set b2 [ on command := actionUserButtons listUserButtons listOfPairTaskButtons txtTitle ref f gameUserList gameState Red congr]
  set b3 [ on command := actionUserButtons listUserButtons listOfPairTaskButtons txtTitle ref f gameUserList gameState Yellow congr ]
  set b4 [ on command := actionUserButtons listUserButtons listOfPairTaskButtons txtTitle ref f gameUserList gameState Blue  congr ]
 
  set f [ layout := column 0 [
        -- Тестовое окно с текущим цветом игры
        margin 1 $ row 1 [ hfill $ minsize (sz 200 25) $ widget txtTitle],
    
    	-- Переход уровня
        margin 1 $ row 1 [ hfill $ minsize (sz 200 25) $  widget congr],
    
        -- Кнопки 
      	-- Первый ряд
        hfloatCenter $ margin 30 $ column 10 [ row 10 [ widget taskb1, widget taskb2, widget b1, widget b2]], 
        -- Второй ряд       
	hfloatCenter $ margin 30 $ column 10 [ row 10 [ widget taskb3, widget taskb4, widget b3, widget b4]], 
        hfloatCenter $  margin 80 $ column 80 [ row 10 [ widget q, widget h, widget a ]]]
   ] 

  return() 
