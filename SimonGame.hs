-- В этом файле будет реализована работа с пользователем и интерфейс игры

module Main where
import Graphics.UI.WX
import SimonGameColor

import System.IO
import Control.Monad
import Data.List

about :: Window a -> IO ()
about w
  = infoDialog w "Об игре" "Авторы игры: Абащикова, Гаврилова, Смолянинова\n"
