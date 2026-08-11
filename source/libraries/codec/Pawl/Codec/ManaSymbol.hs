module Pawl.Codec.ManaSymbol where

import qualified Numeric.Natural as Natural
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Types.ManaSymbol as ManaSymbol

codec :: Codec.Codec ManaSymbol.ManaSymbol
codec =
  Arm.tagged
    encode
    [ Arm.payload "Generic" naturalCodec ManaSymbol.Generic,
      Arm.payload "OfType" ManaType.codec ManaSymbol.OfType,
      Arm.payload "Hybrid" (Common.tuple ManaType.codec ManaType.codec) (uncurry ManaSymbol.Hybrid),
      Arm.payload "MonocoloredHybrid" ManaType.codec ManaSymbol.MonocoloredHybrid,
      -- A Color, not a ManaType: CR 107.4f's five Phyrexian symbols are all coloured.
      Arm.payload "Phyrexian" Color.codec ManaSymbol.Phyrexian,
      -- Nullary: CR 107.4h's {S} names no mana type and no colour, so there is
      -- nothing for it to carry.
      Arm.nullary "Snow" ManaSymbol.Snow,
      Arm.nullary "Variable" ManaSymbol.Variable
    ]
  where
    encode ms = case ms of
      ManaSymbol.Generic n -> Common.tagged "Generic" . Just $ Common.encodeNatural n
      ManaSymbol.OfType mt -> Common.tagged "OfType" . Just $ Codec.encode ManaType.codec mt
      ManaSymbol.Hybrid a b -> Common.tagged "Hybrid" . Just . Value.array $ [Codec.encode ManaType.codec a, Codec.encode ManaType.codec b]
      ManaSymbol.MonocoloredHybrid mt -> Common.tagged "MonocoloredHybrid" . Just $ Codec.encode ManaType.codec mt
      ManaSymbol.Phyrexian c -> Common.tagged "Phyrexian" . Just $ Codec.encode Color.codec c
      ManaSymbol.Snow -> Common.nullary "Snow"
      ManaSymbol.Variable -> Common.nullary "Variable"

-- | Unnamed, unlike 'Common.scalar': 'Natural.Natural' is not one of pawl's own
-- types, so it earns no $defs entry of its own. Built from
-- 'Common.decodeNatural'/'Common.encodeNatural' rather than
-- 'toEnum'/'fromIntegral', per the repo-wide ban.
naturalCodec :: Codec.Codec Natural.Natural
naturalCodec =
  Codec.MkCodec
    { Codec.encode = Common.encodeNatural,
      Codec.decode = Common.decodeNatural,
      Codec.schema = pure Schema.natural
    }
