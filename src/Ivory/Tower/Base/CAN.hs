{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Base.CAN where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN ()
import Ivory.Tower.HAL.Bus.Interface

-- wrapper over CAN AbortableTransmit
-- accepts can_messages on channel and transmits on CAN
-- if transmit is pending, it is aborted and new message is sent instead
canSendTower :: AbortableTransmit ('Struct "can_message") ('Stored IBool)
        -> ChanOutput  ('Struct "can_message")
        -> Tower p ()
canSendTower req msg = do
    monitor "canSender" $ do
      tx_pending <- state "tx_pending"
      last_sent  <- state "last_sent"
      handler msg "can_msg" $ do
        abort_emitter <- emitter (abortableAbort    req) 1
        req_emitter   <- emitter (abortableTransmit req) 1
        callback $ \cmsg  -> do
          refCopy last_sent cmsg

          was_pending <- deref tx_pending
          ifte_ was_pending (emitV abort_emitter true) $ do
            emit req_emitter $ constRef last_sent
            store tx_pending true

      handler (abortableComplete req) "tx_complete" $ do
        req_emitter <- emitter (abortableTransmit req) 1
        callbackV $ \ ok -> do
          ifte_ ok (store tx_pending false) $ do
            emit req_emitter $ constRef last_sent
            store tx_pending true

-- from ASCII rep to binary
toBin :: Def ('[Uint8] :-> Uint8)
toBin = proc "toBin" $ \x -> body $ do
 cond_
   [ x >=? (fromIntegral $ ord '0') .&& x <=? (fromIntegral $ ord '9') ==> do
       ret (x - (fromIntegral $ ord '0'))
   , x >=? (fromIntegral $ ord 'A') .&& x <=? (fromIntegral $ ord 'F') ==> do
       ret (x - (fromIntegral $ ord 'A') + 10)
   ]
 ret (x - (fromIntegral $ ord 'a') + 10)

basecanTypes :: Module
basecanTypes = package "basecanTypes" $ do
  incl toBin

-- convert standard CAN ID to array
standardIDToArray :: forall s eff . (GetAlloc eff ~ 'Scope s)
                  => Uint32
                  -> Ivory eff (ConstRef ('Stack s) ('Array 2 ('Stored Uint8)))
standardIDToArray sid = do
  slb <- assign $ sid `iShiftR` 18
  l <- local $ iarray $ fmap (ival . bitCast) [ slb `iShiftR` 8, slb]
  return $ constRef l

-- convert extended CAN ID to array
extendedIDToArray :: forall s eff . (GetAlloc eff ~ 'Scope s)
                  => Uint32
                  -> Ivory eff (ConstRef ('Stack s) ('Array 4 ('Stored Uint8)))
extendedIDToArray eid = do
  l <- local $ iarray $ fmap (ival . bitCast) [ eid `iShiftR` (8*i) | i <- [3,2,1,0]]
  return $ constRef l
