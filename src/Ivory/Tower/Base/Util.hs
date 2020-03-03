{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ivory.Tower.Base.Util where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

-- filter (drop) elements matching predicate
filterChan :: (IvoryInit a, IvoryStore a, IvoryZeroVal a)
           => (a -> IBool)
           -> ChanOutput ('Stored a)
           -> Tower e (ChanOutput ('Stored a))
filterChan predic upstream = do
  chan <- channel
  monitor "filterChan" $ do
    handler upstream "filterChan" $ do
      o <- emitter (fst chan) 1
      callbackV $ \v ->
        unless (predic v) $ emitV o v

  return (snd chan)

-- drop every nth element matching predicate
dropEvery :: (IvoryInit a, IvoryStore a, IvoryZeroVal a)
          => Uint32
          -> (a -> IBool)
          -> ChanOutput ('Stored a)
          -> Tower e (ChanOutput ('Stored a))
dropEvery nth predic upstream = do
  chan <- channel
  monitor "dropEvery" $ do
    counter <- stateInit "dropEveryCounter" (ival (0 :: Uint32))
    handler upstream "dropEveryIn" $ do
      o <- emitter (fst chan) 1
      callbackV $ \v -> do
        ifte_ (predic v)
          (do
             counter += 1
             cnt <- deref counter
             ifte_ (cnt ==? nth) (store counter 0) (emitV o v)
          )
          (emitV o v)

  return $ snd chan

-- merge two input channels into one
--
-- duplicates message received on common
-- channel to both inputs
mergeInputs :: (IvoryZero a, IvoryArea a)
            => ChanInput a
            -> ChanInput a
            -> Tower e (ChanInput a)
mergeInputs a b = do
  common <- channel
  monitor "merger" $ do
    handler (snd common) "mergerCommon" $ do
      ae <- emitter a 1
      be <- emitter b 1
      callback $ \x -> emit ae x >> emit be x

  return (fst common)


-- Create a channel pair which can be used
-- as a side channel for ChanInput.
--
-- Instead of original input channel use
-- input channel created by this tower.
--
-- Example:
-- >  togIn' <- ledToggle ledpin
-- >  (togIn, togOut) <- inputSniffer togIn'
--
-- This allows us to hook onto output channel side
-- which would be normally hidden in ledToggle
inputSniffer :: (IvoryZero a, IvoryArea a)
             => ChanInput a
             -> Tower e (ChanInput a, ChanOutput a)
inputSniffer a = do
  new <- channel
  monitor "fwd" $ do
    handler (snd new) "forwardToOrig" $ do
      oem <- emitter a 1
      callback $ emit oem

  return new

-- Split elements into two channels, first containing
-- elements passing predicate, second failing.
--
-- Example:
-- (passing, failing) <- splitter (>=? 10) someChannel
--
splitter :: (IvoryInit a, IvoryStore a, IvoryZeroVal a)
         => (a -> IBool)
         -> ChanOutput ('Stored a)
         -> Tower e (ChanOutput ('Stored a), ChanOutput ('Stored a))
splitter predic upstream = do
  passing <- filterChan predic upstream
  failing <- filterChan (iNot . predic) upstream
  return (passing, failing)

-- forward ChanOutput `from` to ChanInput `to`
fwd :: (IvoryArea a, IvoryZero a)
    => ChanOutput a
    -> ChanInput a
    -> Tower e ()
fwd from to = do
  monitor "forward" $ do
    handler from "forwardFrom" $ do
      e <- emitter to 1
      callback $ emit e

--instance Functor (ChanOutput) where ???
-- Creates new channel that apples function `f` to
-- ChanOutput `chan` message values
fmapChan :: (IvoryStore a
            , IvoryInit b
            , IvoryZeroVal a
            , IvoryZeroVal b)
         => (a -> b)
         -> ChanOutput ('Stored a)
         -> Tower e (ChanOutput ('Stored b))
fmapChan f chan = do
  nchan <- channel
  monitor "fmapChan" $ do
    handler chan "fmapChan" $ do
      o <- emitter (fst nchan) 1
      callbackV $ emitV o . f

  return (snd nchan)

-- similar as `fmapChan` for ChanInputs
fmapInputChan :: (IvoryStore a
                 , IvoryInit b
                 , IvoryZeroVal a
                 , IvoryZeroVal b)
              => (a -> b)
              -> ChanInput ('Stored b)
              -> Tower e (ChanInput ('Stored a))
fmapInputChan f ichan = do
  nchan <- channel
  monitor "fmapInputChan" $ do
    handler (snd nchan) "fmapInputChan" $ do
      o <- emitter ichan 1
      callbackV $ emitV o . f

  return (fst nchan)

-- sample ChanOutput `chan` into state with `name`
sampler :: (IvoryZero a, IvoryArea a)
       => String
       -> ChanOutput a
       -> Tower e ()
sampler name chan = do
  monitor "variableSampler" $ do
    s <- state name
    handler chan "samplerHandler" $ do
      callback $ refCopy s

-- forward n elements, drop all afterwards
takeChan :: (IvoryZero a, IvoryArea a)
         => Integer
         -> ChanOutput a
         -> Tower e (ChanOutput a)
takeChan n c = do
  nchan <- channel
  monitor "take" $ do
    count <- stateInit "takeCount" (ival (0 :: Uint32))
    handler c "takeChan" $ do
      o <- emitter (fst nchan) 1
      callback $ \x -> do
        current <- deref count
        when (current <? fromIntegral n) $ do
          emit o x
          count += 1

  return (snd nchan)

-- forward 1 element, drop all afterwards
once :: (IvoryZero a, IvoryArea a)
     => ChanOutput a
     -> Tower e (ChanOutput a)
once = takeChan 1
