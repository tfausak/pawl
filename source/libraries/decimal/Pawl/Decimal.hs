module Pawl.Decimal where

import qualified Data.Function as Function
import qualified Data.Ord as Ord

-- | A decimal number representing @mantissa * 10 ^ exponent@.
data Decimal = UnsafeDecimal
  { mantissa :: Integer,
    exponent :: Integer
  }
  deriving (Eq, Show)

-- | Compares by numeric value rather than by representation: @2e1@ and @20e0@
-- compare equal even though structural 'Eq' tells them apart. The two instances
-- agree on every value 'mkDecimal' produces, since normalizing makes the
-- representation canonical, so only values built directly with 'UnsafeDecimal'
-- can disagree.
instance Ord Decimal where
  compare x y = case Ord.comparing (signum . mantissa) x y of
    EQ -> case compare (mantissa x) 0 of
      -- Both are negative, so the bigger magnitude is the smaller value.
      LT -> compareMagnitude y x
      -- Both mantissas are zero, whatever the exponents say.
      EQ -> EQ
      GT -> compareMagnitude x y
    o -> o

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

-- | Compares the absolute values of two decimals, neither of which may have a
-- zero mantissa. Comparing 'adjustedExponent' first means values of different
-- magnitudes never scale a mantissa: only a tie reaches the scaling, and there
-- the shift is bounded by the difference in digit counts rather than by the
-- exponents themselves.
compareMagnitude :: Decimal -> Decimal -> Ordering
compareMagnitude x@(UnsafeDecimal mx ex) y@(UnsafeDecimal my ey) =
  case Ord.comparing adjustedExponent x y of
    EQ ->
      let e = min ex ey
       in compare (abs mx * 10 ^ (ex - e)) (abs my * 10 ^ (ey - e))
    o -> o

-- | The power of ten of the leading digit: for a non-zero mantissa, the
-- absolute value of @mantissa * 10 ^ exponent@ is at least @10 ^ n@ and less
-- than @10 ^ (n + 1)@, where @n@ is this.
adjustedExponent :: Decimal -> Integer
adjustedExponent (UnsafeDecimal m e) = e + digitCount m - 1

-- | The number of base ten digits in an integer's absolute value. Zero counts
-- as one digit.
digitCount :: Integer -> Integer
digitCount m =
  Function.fix
    (\rec n d -> if n < 10 then d else rec (quot n 10) (d + 1))
    (abs m)
    1
