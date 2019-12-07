{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Base.UART where

import Control.Monad (forM_)
import Data.Char (ord)
import GHC.TypeLits

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Peripheral.UART -- .Peripheral

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

-- hex encode ivory string struct to emitter
putHexIvoryString :: forall str eff s s2
                  . (IvoryString str, GetAlloc (AllowBreak eff) ~ 'Scope s)
                  => Emitter ('Stored Uint8)
                  -> ConstRef s2 str
                  -> Ivory eff ()
putHexIvoryString o str = do
  len <- str ~>* stringLengthL

  arrayMap $ \ix -> do
    unless (fromIx ix >=? len) $ do
      c <- deref (str ~> stringDataL ! ix)
      putHex o c

-- marshall ivory string struct to emitter
putIvoryString :: forall str eff s s2
                . (IvoryString str, GetAlloc (AllowBreak eff) ~ 'Scope s)
                => Emitter ('Stored Uint8)
                -> ConstRef s2 str
                -> Ivory eff ()
putIvoryString e s = do
              l <- deref (s ~> stringLengthL)
              upTo 0 (toIx (l - 1)) $ \i -> do
                x <- deref ((s ~> stringDataL) ! i)
                emitV e x

-- convert Uint32 to IvoryString
uint32ToString :: (GetBreaks (AllowBreak eff) ~ 'Break,
                   GetAlloc eff ~ 'Scope s1,
                   IvoryString ('Struct t),
                   IvoryStruct t)
               => Uint32
               -> Ivory eff (Ref ('Stack s1) ('Struct t))
uint32ToString num = do
  numTmp <- local $ ival 0
  store numTmp $ num

  str <- local $ stringInit ""

  n <- deref numTmp
  size <- assign $ (castDefault :: IFloat -> Uint8) $
    ceilF $ log (0.1 + (safeCast :: Uint32 -> IFloat) n) / log 10


  ix <- local $ ival (0 :: Uint8)
  forever $ do
    x <- deref numTmp
    cix <- deref ix
    store (str ~> stringDataL ! (toIx $ (safeCast :: Uint8 -> Sint32) (size - 1 - cix) ))
          (bitCast $ (x .% 10) + (fromIntegral $ ord '0'))
    store numTmp $ x `iDiv` 10

    ix += 1

    when (x <=? 1) $ do
      store (str ~> stringLengthL) (safeCast size)
      breakOut

  return str

-- experimental
-- format Sint32 to IvoryString buffer `buf` starting at
-- `offset` position, padded to `targetChars` with `padChar`
sint32ToStringBuffer :: (GetBreaks (AllowBreak eff) ~ 'Break,
                         GetAlloc eff ~ 'Scope s1,
                         IvoryString str)
               => Ref ('Stack s1) str
               -> Sint32
               -> Sint32
               -> Char
               -> Sint32
               -> Ivory eff ()
sint32ToStringBuffer buf offset targetChars padChar num = do
  numTmp <- local $ ival 0
  store numTmp $ abs num

  n <- deref numTmp

  negative <- assign $ num <? 0
  signPad <- assign $ negative ? (1, 0)

  size <- assign $ signPad + ((castDefault :: IDouble -> Sint32) $
    ceilF $ logBase 10 (0.1 + (safeCast :: Sint32 -> IDouble) n))

  (pad :: Ix 1024) <- assign $
    toIx $ (size <? targetChars) ? (targetChars - size, 0)

  unless (pad ==? 0) $
    0 `upTo` (pad - 1) $ \i -> store
      (buf ~> stringDataL ! toIx (offset + fromIx i))
      (fromIntegral $ ord padChar)

  ix <- local $ ival (0 :: Sint32)
  forever $ do
    x <- deref numTmp
    cix <- deref ix
    store (buf ~> stringDataL ! toIx ((size + offset + fromIx pad) - 1 - cix))
          (bitCast $ (signCast :: Sint32 -> Uint32) $ (x .% 10) + (fromIntegral $ ord '0'))
    store numTmp $ x `iDiv` 10

    ix += 1

    cx <- deref numTmp
    when (cx <=? 0) $ do
      store (buf ~> stringLengthL) (safeCast $ size + offset + fromIx pad)
      cix' <- deref ix
      when negative $ store (buf ~> stringDataL ! (toIx $ size + offset + (fromIx pad) - 1 - cix')) (fromIntegral $ ord '-')

      breakOut

  return ()

-- experimental
-- convert IFloat or IDouble to IvoryString with specified
-- `prec` decimal points
floatingToString :: (GetBreaks (AllowBreak eff) ~ 'Break,
                     GetAlloc eff ~ 'Scope s1,
                     IvoryString ('Struct t),
                     IvoryStruct t,
                     Num from, Floating from,
                     RuntimeCast from Sint32,
                     SafeCast Sint32 from)
               => from
               -> Sint32
               -> Ivory eff (Ref ('Stack s1) ('Struct t))
floatingToString num prec = do
  (ipart :: Sint32) <- assign $ castDefault num
  (fpart :: from) <- assign $ abs $ num - safeCast ipart

  buf <- local $ stringInit ""
  -- XXX: hardcoded padding (4)
  sint32ToStringBuffer buf 0 4 ' ' ipart

  when (prec /=? 0) $ do
    iLen <- buf ~>* stringLengthL
    sint32ToStringBuffer buf (iLen + 1) prec '0' $ castDefault (fpart * (10 ** safeCast prec))
    -- add dot
    store ((buf ~> stringDataL) ! (toIx $ iLen)) (fromIntegral $ ord '.')

  -- XXX: handle 10.0, fpart being 0
  -- test 10.005

  return buf

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

-- bridge two UARTs
uartBridge :: ChanInput ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> Tower e ()
uartBridge ostream istream ostream' istream' = do
  monitor "uartBridge" $ do
    handler istream "uartBridgeIstream" $ do
      out <- emitter ostream' 1
      callback $ emit out

    handler istream' "uartBridgeIstream2" $ do
      out <- emitter ostream 1
      callback $ emit out

-- buffer until one of tokens is received
bufferBy :: forall buf e . (IvoryString buf)
           => ChanOutput ('Stored Uint8)
           -> String
           -> Proxy buf
           -> Tower e (ChanOutput buf)
bufferBy istream tokens _ = do
  c <- channel
  monitor "bufferBy" $ do

    (buf :: Ref 'Global buf) <- state "bufferBy_buf"

    handler istream "bufferBy_in" $ do
      out <- emitter (fst c) 1
      callbackV $ \v -> do
        handled <- local $ ival false
        forM_ tokens $ \t -> do
          h <- deref handled
          when (v `isChar` t .&& iNot h) $ do
                clen <- buf ~>* stringLengthL
                when (clen /=? 0) $ emit out (constRef buf)
                store (buf ~> stringLengthL) 0
                store handled true

        h <- deref handled
        unless h $ do
          pos <- deref (buf ~> stringLengthL)
          store ((buf ~> stringDataL) ! toIx pos) v
          buf ~> stringLengthL += 1

  return (snd c)

lfBuffer, crBuffer, crlfBuffer, lineBuffer
           :: forall buf e . (IvoryString buf)
           => ChanOutput ('Stored Uint8)
           -> Proxy buf
           -> Tower e (ChanOutput buf)
lfBuffer istream buf = bufferBy istream "\n" buf
crBuffer istream buf = bufferBy istream "\r" buf
crlfBuffer istream buf = bufferBy istream "\r\n" buf
lineBuffer = crlfBuffer

-- return true if input matches prefix
--
-- define isPrefixOf shortcut as:
--   let isPrefixOf p x = isPrefixOfBuf p x (Proxy :: Proxy SomeBufferType)
--
-- then use as:
--   gotOk <- "ok" `isPrefixOf` res
--   assert $ gotOk
isPrefixOfBuf :: forall buf eff cs s str . (GetBreaks (AllowBreak eff) ~ 'Break,
                  GetAlloc eff ~ 'Scope cs,
                  IvoryString buf,
                  IvoryString str)
              => String
              -> Ref s str
              -> Proxy buf
              -> Ivory eff IBool
isPrefixOfBuf p x _ = do
  (pbuf :: Ref ('Stack s1) buf) <- local $ stringInit p
  tmp <- isPrefixOf' (constRef pbuf) (constRef x)
  return tmp

isPrefixOf' :: (GetBreaks (AllowBreak eff) ~ 'Break,
                GetAlloc eff ~ 'Scope cs,
                IvoryString str1, IvoryString str2)
            => ConstRef s1 str1
            -> ConstRef s2 str2
            -> Ivory eff IBool
isPrefixOf' p x = do
  isPref <- local (ival false)

  plen <- p ~>* stringLengthL
  xlen <- x ~>* stringLengthL

  when (plen <=? xlen) $ -- if target is smaller than prefix just skip comparing
    arrayMap $ \i -> do
      pc <- deref (p ~> stringDataL ! i)
      xc <- deref (x ~> stringDataL ! (toIx . fromIx) i)
      cond_
        [ fromIx i <=? plen - 1 ==> do -- while iterator is smaller than prefix length

            when (pc /=? xc) $ do -- if prefix char differs from targets
              breakOut

        , true ==> do -- end of prefix reached

            store isPref true
            breakOut
        ]

  r <- deref isPref
  return r

-- append char when 'afterChar' character is found
appendCharAfter :: Char
                -> Char
                -> ChanOutput ('Stored Uint8)
                -> Tower e (ChanOutput ('Stored Uint8))
appendCharAfter afterChar appendChar outchan = do
  c <- channel
  monitor "appendAfter" $ do
    handler outchan "appender" $ do
     o <- emitter (fst c) 2
     callbackV $ \v -> do
       emitV o v
       when (v `isChar` afterChar) $ do
         putc o $ fromIntegral $ ord appendChar
  return (snd c)

-- append string after a character is found,
-- prepends it to stream on next write
--
-- useful for prefixing UART stream with
-- e.g. "> " to indicate it comes from device
dbgPrepend :: Char
           -> String
           -> ChanInput ('Stored Uint8)
           -> Tower e (ChanInput ('Stored Uint8))
dbgPrepend afterChar appendWhat outchan = do
  c <- channel
  monitor "appendAfter" $ do
    shouldAppend <- stateInit "shouldAppend" (ival true)
    handler (snd c) "appender" $ do
     o <- emitter outchan 32
     callbackV $ \v -> do
       go <- deref shouldAppend
       when go $ do
         puts o appendWhat
         store shouldAppend false
       emitV o v
       when (v `isChar` afterChar) $ do
         store shouldAppend true
  return (fst c)

-- filter character from upstream channel
filterChar :: Char
           -> ChanOutput ('Stored Uint8)
           -> Tower e (ChanOutput ('Stored Uint8))
filterChar c upstream = do
  chan <- channel
  monitor "filterChar" $ do
    handler upstream "filterCharIn" $ do
      o <- emitter (fst chan) 1
      callbackV $ \v -> do
        unless (v `isChar` c) $ emitV o v
  return $ snd chan

-- replace characters in stream
replaceChar :: Char
            -> Char
            -> ChanOutput ('Stored Uint8)
            -> Tower e (ChanOutput ('Stored Uint8))
replaceChar c by upstream = do
  chan <- channel
  monitor "replaceChar" $ do
    handler upstream "replaceCharIn" $ do
      o <- emitter (fst chan) 1
      callbackV $ \v -> do
        ifte_ (v `isChar` c) (emitV o (fromIntegral $ ord by)) (emitV o v)
  return $ snd chan

echoPrompt :: String
           -> ChanInput  ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput  ('Stored IBool)
           -> Tower p ()
echoPrompt greeting ostream istream ledctl = do
  monitor "echoprompt" $ do
    handler systemInit "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o (greeting ++ "\n")
        puts o prompt

    handler istream "istream" $ do
      l <- emitter ledctl 1
      o <- emitter ostream 32
      callbackV $ \input -> do
        putc o input -- echo to terminal
        let testChar = (input `isChar`)
        cond_
          [ testChar '1'  ==> puts o "\r\noutput on\r\n"  >> emitV l true
          , testChar '2'  ==> puts o "\r\noutput off\r\n" >> emitV l false
          , testChar '0'  ==> puts o "\r\noutput off\r\n" >> emitV l false
          , testChar '\r' .|| testChar '\n' ==> puts o prompt
          ]
  where prompt = "tower> "

intensityPrompt :: String
           -> ChanInput  ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput  ('Stored Uint8)
           -> Tower p ()
intensityPrompt greeting ostream istream ledctl = do
  monitor "echoprompt" $ do
    handler systemInit "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o (greeting ++ "\r\n")
        puts o prompt

    handler istream "istream" $ do
      l <- emitter ledctl 1
      o <- emitter ostream 32
      callbackV $ \input -> do
        putc o input -- echo to terminal
        let testChar = (input `isChar`)
        cond_ $ [
            testChar c ==> puts o " ok\r\n" >> emitV l (fromIntegral (i :: Int))
            | (c,i) <- zip ['0', '1' .. '9'] [0, 1 .. 9]
          ] ++ [
            testChar '\r' .|| testChar '\n' ==> puts o prompt
          ]

  where prompt = "tower> "
