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
           -> Tower e ()
filterChan predic upstream = do
  chan <- channel
  monitor "filterChan" $ do
    handler upstream "filterChan" $ do
      o <- emitter (fst chan) 1
      callbackV $ \v ->
        unless (predic v) $ emitV o v

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

-- XXX: TODO: splitter  / 
-- (pass, fail) <- splitOn pred chan
