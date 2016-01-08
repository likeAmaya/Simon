{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--------------------------------------------------------------------------------
{-|	Module      :  Timer
	Copyright   :  (c) Daan Leijen 2003
	License     :  wxWindows
	Maintainer  :  wxhaskell-devel@lists.sourceforge.net
	Stability   :  provisional
	Portability :  portable
Support for milli-second timers.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Timer
            ( Timer, timer, interval
            ) where

import Graphics.UI.WXCore.WxcClasses hiding (Timer)
import Graphics.UI.WXCore.Events  

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events

{--------------------------------------------------------------------
--------------------------------------------------------------------}
-- | A timer generates a 'command' event on a specified milli-second 'interval'.
--
-- * Attributes: 'interval'
--
-- * Instances: 'Able', 'Commanding'
--
type Timer  = TimerEx ()

-- | Create a new timer with a 1 second interval. The timer is automatically discarded
-- when the parent is deleted.
timer :: Window a -> [Prop Timer] -> IO Timer
timer parent props
  = do t <- windowTimerCreate parent
       timerStart t 1000 False
       set t props
       return t

-- | The milli-second interval of the timer.
interval :: Attr Timer Int
interval
  = newAttr "timer-interval"
      (\t   -> timerGetInterval t)
      (\t i -> do runs <- timerIsRuning t
                  if (runs)
                   then do timerStop t
                           isone <- timerIsOneShot t
                           timerStart t i isone
                           return ()
                   else do timerStart t i True
                           timerStop t)

instance Able Timer where
  enabled
    = newAttr "enabled"
        (\t      -> timerIsRuning t)
        (\t able -> do runs <- timerIsRuning t
                       when (runs /= able)
                        (if able then do i <- get t interval
                                         timerStart t i False
                                         return ()
                                 else do timerStop t))

instance Commanding Timer where
  command
    = newEvent "command" timerGetOnCommand timerOnCommand