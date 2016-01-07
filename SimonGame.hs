-- Â ýòîì ôàéëå áóäåò ðåàëèçîâàíà ðàáîòà ñ ïîëüçîâàòåëåì è èíòåðôåéñ èãðû

module Main where
import Graphics.UI.WX
import SimonGameColor

import System.IO
import Control.Monad
import Data.List

-- Информация об игре: авторы; какая библиотека для графики была использована
aboutGame :: Window a -> IO ()
aboutGame w
  = infoDialog w "Информация об игре:" "Авторы игры:\nАбащикова Александра,\nГаврилова Амида,\nСмолянинова Дарья\nИгра была написана с использованием библиотеки wxHaskell"
