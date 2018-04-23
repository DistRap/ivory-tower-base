module Ivory.Base where

import Ivory.Language

ixToU8 :: ANat n => Ix n -> Uint8
ixToU8 = (bitCast :: Uint32 -> Uint8) . signCast . fromIx

ixToU16 :: ANat n => Ix n -> Uint16
ixToU16 = (bitCast :: Uint32 -> Uint16) . signCast . fromIx
