
module Ivory.Tower.Base.GPIO where

import Ivory.Language
import Ivory.BSP.STM32.Peripheral.GPIO

-- configure GPIOPin as output
pinOut :: GPIOPin -> Ivory eff()
pinOut pin = pinOutSpeed pin gpio_speed_2mhz

-- configure GPIOPin as output with custom GPIO_Speed
pinOutSpeed :: GPIOPin -> GPIO_Speed -> Ivory eff ()
pinOutSpeed pin speed = do
  pinEnable pin
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed pin speed
  pinSetPUPD pin gpio_pupd_none

-- clear pin
pinLow :: GPIOPin -> Ivory eff ()
pinLow pin = do
  pinSetMode pin gpio_mode_output
  pinClear pin

-- set pin
pinHigh :: GPIOPin -> Ivory eff ()
pinHigh pin = do
  pinSetMode pin gpio_mode_output
  pinSet pin

-- set and clear pin
pinPulse :: GPIOPin -> Ivory eff ()
pinPulse pin = do
  pinSetMode pin gpio_mode_output
  pinSet pin
  pinClear pin

-- set pin to high impedance (HiZ) mode
pinHiZ :: GPIOPin -> Ivory eff ()
pinHiZ pin = pinSetMode pin gpio_mode_analog
