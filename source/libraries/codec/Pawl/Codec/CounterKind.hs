module Pawl.Codec.CounterKind where

import qualified Data.Typeable as Typeable
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CounterKind as CounterKind

-- | PARAMETRIC for Pawl.Types.CounterKind's reason: the keyword codec is
-- threaded through rather than closed over, so 'Keyword' below never names
-- 'Pawl.Codec.Keyword' directly. CR 122.1b's keyword counter carries the
-- keyword it grants, which is why that arm has a payload at all rather than
-- being another bare tag.
codec :: (Typeable.Typeable keyword) => Codec.Codec keyword -> Codec.Codec (CounterKind.CounterKind keyword)
codec keywordCodec =
  Arm.tagged
    encode
    [ Arm.nullary "PlusOnePlusOne" CounterKind.PlusOnePlusOne,
      Arm.nullary "MinusOneMinusOne" CounterKind.MinusOneMinusOne,
      Arm.payload "Keyword" keywordCodec CounterKind.Keyword,
      Arm.nullary "Loyalty" CounterKind.Loyalty,
      Arm.nullary "Lore" CounterKind.Lore,
      Arm.nullary "Defense" CounterKind.Defense,
      Arm.nullary "Time" CounterKind.Time,
      Arm.nullary "Shield" CounterKind.Shield
    ]
  where
    encode k = case k of
      CounterKind.PlusOnePlusOne -> Common.nullary "PlusOnePlusOne"
      CounterKind.MinusOneMinusOne -> Common.nullary "MinusOneMinusOne"
      CounterKind.Keyword kw -> Common.tagged "Keyword" . Just $ Codec.encode keywordCodec kw
      CounterKind.Loyalty -> Common.nullary "Loyalty"
      CounterKind.Lore -> Common.nullary "Lore"
      CounterKind.Defense -> Common.nullary "Defense"
      CounterKind.Time -> Common.nullary "Time"
      CounterKind.Shield -> Common.nullary "Shield"
