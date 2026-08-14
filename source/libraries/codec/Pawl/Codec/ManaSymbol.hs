module Pawl.Codec.ManaSymbol where

import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Hybrid as Hybrid
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaSymbol as ManaSymbol

codec :: Codec.Codec ManaSymbol.ManaSymbol
codec =
  Arm.tagged
    encode
    [ Arm.payload "Generic" Common.natural ManaSymbol.Generic,
      Arm.payload "OfType" ManaType.codec ManaSymbol.OfType,
      Arm.payload "Hybrid" Hybrid.codec ManaSymbol.Hybrid,
      Arm.payload "MonocoloredHybrid" ManaType.codec ManaSymbol.MonocoloredHybrid,
      -- A Color, not a ManaType: CR 107.4f's five MONOCOLOURED Phyrexian symbols
      -- are all coloured. The rule also names ten HYBRID Phyrexian symbols
      -- ({G/U/P}), "both of its component colors" -- a single Color payload
      -- cannot carry that, and this constructor has no counterpart for them (#364).
      Arm.payload "Phyrexian" Color.codec ManaSymbol.Phyrexian,
      -- Nullary: CR 107.4h's {S} names no mana type and no colour, so there is
      -- nothing for it to carry.
      Arm.nullary "Snow" ManaSymbol.Snow,
      Arm.nullary "Variable" ManaSymbol.Variable
    ]
  where
    encode ms = case ms of
      ManaSymbol.Generic n -> Common.tagged "Generic" . Just $ Codec.encode Common.natural n
      ManaSymbol.OfType mt -> Common.tagged "OfType" . Just $ Codec.encode ManaType.codec mt
      ManaSymbol.Hybrid x -> Common.tagged "Hybrid" . Just $ Codec.encode Hybrid.codec x
      ManaSymbol.MonocoloredHybrid mt -> Common.tagged "MonocoloredHybrid" . Just $ Codec.encode ManaType.codec mt
      ManaSymbol.Phyrexian c -> Common.tagged "Phyrexian" . Just $ Codec.encode Color.codec c
      ManaSymbol.Snow -> Common.nullary "Snow"
      ManaSymbol.Variable -> Common.nullary "Variable"
