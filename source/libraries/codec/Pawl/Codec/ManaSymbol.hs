module Pawl.Codec.ManaSymbol where

import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Hybrid as Hybrid
import qualified Pawl.Codec.HybridPhyrexian as HybridPhyrexian
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaSymbol as ManaSymbol

codec :: Codec.Codec ManaSymbol.ManaSymbol
codec =
  Arm.tagged
    [ Arm.payload "Generic" Common.natural ManaSymbol.Generic (\x -> case x of ManaSymbol.Generic y -> Just y; _ -> Nothing),
      Arm.payload "OfType" ManaType.codec ManaSymbol.OfType (\x -> case x of ManaSymbol.OfType y -> Just y; _ -> Nothing),
      Arm.payload "Hybrid" Hybrid.codec ManaSymbol.Hybrid (\x -> case x of ManaSymbol.Hybrid y -> Just y; _ -> Nothing),
      Arm.payload "MonocoloredHybrid" ManaType.codec ManaSymbol.MonocoloredHybrid (\x -> case x of ManaSymbol.MonocoloredHybrid y -> Just y; _ -> Nothing),
      -- A Color, not a ManaType: CR 107.4f's five MONOCOLOURED Phyrexian symbols
      -- are all coloured.
      Arm.payload "Phyrexian" Color.codec ManaSymbol.Phyrexian (\x -> case x of ManaSymbol.Phyrexian y -> Just y; _ -> Nothing),
      -- CR 107.4f's ten HYBRID Phyrexian symbols ({G/U/P}), "both of its
      -- component colors" -- a PAIR of colours, which is what the Color payload
      -- above cannot carry.
      Arm.payload "HybridPhyrexian" HybridPhyrexian.codec ManaSymbol.HybridPhyrexian (\x -> case x of ManaSymbol.HybridPhyrexian y -> Just y; _ -> Nothing),
      -- Nullary: CR 107.4h's {S} names no mana type and no colour, so there is
      -- nothing for it to carry.
      Arm.nullary "Snow" ManaSymbol.Snow,
      Arm.nullary "Variable" ManaSymbol.Variable
    ]
