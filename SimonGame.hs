-- � ���� ����� ����� ����������� ������ � ������������� � ��������� ����

module Main where
import Graphics.UI.WX
import SimonGameColor

import System.IO
import Control.Monad
import Data.List

about :: Window a -> IO ()
about w
  = infoDialog w "�� ����" "������ ����: ���������, ���������, �����������\n"
