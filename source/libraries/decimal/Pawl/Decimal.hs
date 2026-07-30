-- | This module defines arbitrary precision decimal numbers.
module Pawl.Decimal where

import qualified Data.Function as Function

-- | A decimal number representing @mantissa * 10 ^ exponent@.
data Decimal = UnsafeDecimal
  { mantissa :: Integer,
    exponent :: Integer
  }
  deriving (Eq, Show)

-- | Creates a normalized decimal number from the given mantissa and exponent.
mkDecimal :: Integer -> Integer -> Decimal
mkDecimal = Function.fix $ \rec m e ->
  if m == 0
    then UnsafeDecimal 0 0
    else
      let (q, r) = quotRem m 10
       in if r == 0
            then rec q (e + 1)
            else UnsafeDecimal m e
