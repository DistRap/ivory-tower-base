{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
module Ivory.Base where

import Ivory.Language
import Ivory.Stdlib


ixToU8 :: ANat n => Ix n -> Uint8
ixToU8 = (bitCast :: Uint32 -> Uint8) . signCast . fromIx

ixToU16 :: ANat n => Ix n -> Uint16
ixToU16 = (bitCast :: Uint32 -> Uint16) . signCast . fromIx

-- copy array data from some offset to new array
arrayCopyFromOffset :: ( ANat n, ANat m, IvoryRef r
                       , IvoryExpr (r s2 ('Array m ('Stored t)))
                       , IvoryExpr (r s2 ('Stored t))
                       , IvoryStore t
                       )
                    => Ref s1 ('Array n ('Stored t))
                    -> r s2 ('Array m ('Stored t))
                    -> Sint32
                    -> Sint32
                    -> Ivory eff ()
arrayCopyFromOffset to from fromOffset end = do
  assert (fromOffset >=? 0 .&& fromOffset <? frLen)
  assert (end        >=? 0 .&& end       <=? toLen)
  arrayMap $ go
  where
  -- The index is w.r.t. the from array.
  go ix =
    cond_
      [   -- We've reached the @end@ index: stop copying.
          (fromIx ix >=? end)
      ==> return ()
          -- We've reached the end of the @to@ array: stop copying.
      ,   (fromIx ix >=? toLen)
      ==> return ()
      ,   true
      ==> (deref (from ! mkIx ix) >>= store (to ! ix))
      ]

  toLen = arrayLen to
  frLen = arrayLen from

  mkIx ix = toIx (fromOffset + fromIx ix)

-- | fold over each bit matching `condition` of number `a`, LSB to MSB
foldBits :: (Num a, IvoryInit a , IvoryStore a, BitCast Uint32 a)
         => (Ix 64 -> IBool)  -- filter condition (only bits matching cond)
         -> (a -> a -> a)     -- operator
         -> a                 -- initial accumulator value
         -> a                 -- input number
         -> Ivory (AllocEffects s)
                  (Ref ('Stack s) ('Stored a))
foldBits condition operator ini x = do
  accum <- local $ ival ini
  comment "YOW"
  upTo 0 (fromIntegral $ iBitSize x - 1) $ \ix -> do
    a <- deref accum
    when (condition ix) $ do
      store accum $ a `operator` ((x `iShiftR` (castIx ix)) .& 0b1)
  return accum
  where
    castIx = bitCast . (signCast :: Sint32 -> Uint32) . fromIx

-- | fold1 over user specified range of bits of number `a`.
-- Instead of user provided initial value
-- uses first bit in specified range. LSB to MSB
fold1BitRange :: (Num a, IvoryInit a, IvoryStore a, BitCast Uint32 a)
              => Ix 64          -- range start
              -> Ix 64          -- range end
              -> (a -> a -> a)  -- operator
              -> a              -- input number
              -> Ivory (AllocEffects s)
                       (Ref ('Stack s) ('Stored a))
fold1BitRange start end operator x = do
  accum <- local $ ival $ (x `iShiftR` (castIx start)) .& 0b1
  upTo (start + 1 :: Ix 64) end $ \ix -> do
    a <- deref accum
    store accum $ a `operator` ((x `iShiftR` (castIx ix)) .& 0b1)
  return accum
  where
    castIx = bitCast . (signCast :: Sint32 -> Uint32) . fromIx

-- | fold1 over all bits of number `a`.
fold1Bits :: (Num a, IvoryInit a, IvoryStore a, BitCast Uint32 a)
          => (a -> a -> a)  -- operator
          -> a              -- input number
          -> Ivory (AllocEffects s)
                   (Ref ('Stack s) ('Stored a))
fold1Bits op x = fold1BitRange 0 (fromIntegral $ iBitSize x - 1) op x

xorBits :: (Num a, IvoryBits a, IvoryInit a, IvoryStore a, BitCast Uint32 a)
         => (Ix 64 -> IBool)  -- filter condition (only bits matching cond)
         -> a
         -> Ivory (AllocEffects s) (Ref ('Stack s) ('Stored a))
xorBits condition = foldBits condition (.^) 0

-- | Exclusive NOR (not . xor)
-- > \a b -> (\x -> 1 - x) $ (a .^ b)
xnor :: (Num a, IvoryBits a) => a -> a -> a
xnor = ((-) 1 .) . (.^)

xnorBits :: (Num a, IvoryBits a, IvoryInit a, IvoryStore a, BitCast Uint32 a)
         => a
         -> Ivory (AllocEffects s) (Ref ('Stack s) ('Stored a))
xnorBits = fold1Bits xnor

xnorBitRange :: (Num a, IvoryBits a, IvoryInit a, IvoryStore a, BitCast Uint32 a)
             => Ix 64
             -> Ix 64
             -> a
             -> Ivory (AllocEffects s) (Ref ('Stack s) ('Stored a))
xnorBitRange start end x = fold1BitRange start end xnor x
