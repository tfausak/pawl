-- | Normalized decimal numbers represented as mantissa and exponent.
--
-- A 'Decimal' value represents the number @mantissa * 10 ^ exponent@. The smart
-- constructor 'mkDecimal' ensures a canonical form by stripping trailing zeros
-- from the mantissa (shifting them into the exponent). Ported from scrod's
-- @Scrod.Decimal@ (§1 of the M3.5 spec), dropping its inline spec.
module Pawl.Type.Decimal where

import qualified Data.Function as Function

data Decimal = MkDecimal
  { mantissa :: Integer,
    exponent :: Integer
  }
  deriving (Eq, Ord, Show)

mkDecimal :: Integer -> Integer -> Decimal
mkDecimal = Function.fix $ \rec m e ->
  if m == 0
    then MkDecimal 0 0
    else
      let (q, r) = quotRem m 10
       in if r == 0
            then rec q (e + 1)
            else MkDecimal m e
