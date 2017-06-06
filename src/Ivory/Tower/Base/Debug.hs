module Ivory.Tower.Base.Debug where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.Tower.Base.LED
import Ivory.Tower.Base.GPIO

-- toggle LED on each channel message
toggleOnChanTower :: (IvoryZero a, IvoryArea a)
                 => ChanOutput a
                 -> LED
                 -> Tower e (ChanOutput a)
toggleOnChanTower chan_out led = do
  (c_in, c_out) <- channel

  monitor "toggle_on_chan" $ do
    count <- stateInit "toggle_on_chan_count" (ival (0 :: Uint32))
    handler chan_out "toggle_on_chan_handle" $ do
      cine <- emitter c_in 1
      callback $ \msg -> do
        cnt <- deref count
        ifte_ (cnt .& 1 ==? 1)
          (ledOff led)
          (ledOn  led)

        count %= (+1)
        emit cine msg

  return c_out

-- blink on `pin` on every `chan` input
-- sequences 6x pinHigh followed by pinLow
-- to be able to capture signal on oscilloscope
blinkOnChanTower :: (IvoryZero a, IvoryArea a) =>
                     GPIOPin -> ChanOutput a -> Tower e ()
blinkOnChanTower pin chan = do
  monitor "debug" $ do
    handler systemInit "debugInit" $ do
      callback $ const $ pinOut pin
    handler chan "debug" $ do
      callback $ const $ do
        sequence_ $ take 6 $ repeat $ pinHigh pin
        pinLow pin
