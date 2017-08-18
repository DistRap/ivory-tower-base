{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ivory.Tower.Base.UART where

import Data.Char (ord)
import GHC.TypeLits

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Peripheral.UART.Peripheral

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

-- send string to emitter
puts :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> String -> Ivory eff ()
puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str

-- send char (Uint8) to emitter
putc :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
putc = emitV

-- send hex formatted Uint8 to emitter
putHex :: (GetAlloc eff ~ 'Scope cs)
       => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
putHex e val = do
  let hi = (val .& 0xF0) `iShiftR` 4 + (fromIntegral $ ord '0')
      lo = (val .& 0x0F) + (fromIntegral $ ord '0')

  ifte_ (hi >=? 0x3a) (emitV e $ hi + 7) (emitV e hi)
  ifte_ (lo >=? 0x3a) (emitV e $ lo + 7) (emitV e lo)

-- send hex formatted Uint8 to emitter
-- leading 0 omitting version, 0f -> f
putHex' :: (GetAlloc eff ~ 'Scope cs)
        => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
putHex' e val = do
  let hi = (val .& 0xF0) `iShiftR` 4 + (fromIntegral $ ord '0')
      lo = (val .& 0x0F) + (fromIntegral $ ord '0')

  ifte_ (hi >=? 0x3a) (emitV e $ hi + 7) (when (hi /=? (fromIntegral $ ord '0')) $ emitV e hi)
  ifte_ (lo >=? 0x3a) (emitV e $ lo + 7) (emitV e lo)

-- send hex formatted array of Uint8s to emitter
putHexArray :: (GetAlloc (AllowBreak eff) ~ 'Scope cs,
                IvoryExpr (ref s ('Stored Uint8)),
                IvoryExpr (ref s ('Array len ('Stored Uint8))), IvoryRef ref,
                GHC.TypeLits.KnownNat len) =>
               Emitter ('Stored Uint8)
               -> ref s ('Array len ('Stored Uint8)) -> Ivory eff ()
putHexArray e a = arrayMap $ \i -> do
  x <- deref (a ! i)
  putHex e x

--
-- UART buffer transmits in buffers. This wrapper
-- tower create a channel that allows byte-by-byte transmission
-- and periodically flushes a buffer.
--
-- Usage:
--   (buffered_ostream, istream, mon) <- uartTower ...
--   ostream <- uartUnbuffer (
--      buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))
uartUnbuffer :: forall b e
              . (IvoryString b)
             => BackpressureTransmit b ('Stored IBool)
             -> Tower e (ChanInput ('Stored Uint8))
uartUnbuffer (BackpressureTransmit req res) = do
  c <- channel
  p <- period (Milliseconds 10)
  monitor "uart_unbuffer" $ do
    flush_defer <- state "flush_defer"
    buf <- state "buffer"
    let ready_buf :: Ivory eff IBool
        ready_buf = fmap (>? 0) (deref (buf ~> stringLengthL))

        send_buf :: Emitter b -> Ivory eff ()
        send_buf e = do
          emit e (constRef buf)
          store (buf ~> stringLengthL) 0
          store flush_defer true

    handler (snd c) "uart_byte_tosend" $ do
      callbackV $ \byte -> do
        pos <- deref (buf ~> stringLengthL)
        when (pos <? arrayLen (buf ~> stringDataL)) $ do
          store (buf ~> stringDataL ! toIx pos) byte
          store (buf ~> stringLengthL) (pos + 1)

    handler p "uart_tx_flush" $ do
      e <- emitter req 1
      callback $ const $ do
        defer <- deref flush_defer
        ready <- ready_buf
        when (ready .&& iNot defer) (send_buf e)

    handler res "uart_tx_res" $ do
      callback $ const $ store flush_defer false

  return (fst c)

-- TX buffered uartTower wrapper
--
-- Needs (Proxy :: Proxy SomeUARTBuffer) as last parameter.
--
-- Use with UARTBuffer from Ivory.Tower.Base.UART.Types
-- or provide your own buffer
bufferedUartTower :: forall tx e
                  .  (IvoryString tx)
                  => (e -> ClockConfig)
                  -> UART
                  -> UARTPins
                  -> Integer
                  -> Proxy tx
                  -> Tower e (
                        ChanInput ('Stored Uint8)
                      , ChanOutput ('Stored Uint8))
bufferedUartTower tocc uartPeriph pins baud _ = do

  (buffered_ostream, istream, mon) <- uartTower tocc uartPeriph pins baud
  monitor "bufferedUart" mon
  ostream <- uartUnbuffer (
    buffered_ostream :: BackpressureTransmit tx ('Stored IBool))

  return (ostream, istream)
