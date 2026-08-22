module Pawl.Codec.CounterKind where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CounterName as CounterName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CounterKind as CounterKind

-- | PARAMETRIC for Pawl.Types.CounterKind's reason: the keyword codec is
-- threaded through rather than closed over, so 'Keyword' below never names
-- 'Pawl.Codec.Keyword' directly. CR 122.1b's keyword counter carries the
-- keyword it grants, which is why that arm has a payload at all rather than
-- being another bare tag.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CounterKind.CounterKind keyword)
codec keywordCodec =
  Arm.tagged
    [ Arm.nullary "PlusOnePlusOne" CounterKind.PlusOnePlusOne,
      Arm.nullary "MinusOneMinusOne" CounterKind.MinusOneMinusOne,
      Arm.payload "Keyword" keywordCodec CounterKind.Keyword (\x -> case x of CounterKind.Keyword y -> Just y; _ -> Nothing),
      Arm.nullary "Loyalty" CounterKind.Loyalty,
      Arm.nullary "Lore" CounterKind.Lore,
      Arm.nullary "Defense" CounterKind.Defense,
      Arm.nullary "Time" CounterKind.Time,
      Arm.nullary "Fade" CounterKind.Fade,
      Arm.nullary "Shield" CounterKind.Shield,
      Arm.nullary "Level" CounterKind.Level,
      Arm.payload "Named" CounterName.codec CounterKind.Named (\x -> case x of CounterKind.Named y -> Just y; _ -> Nothing)
    ]
