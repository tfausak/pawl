module Pawl.Codec.CopyException where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SetPowerToughness as SetPowerToughness
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CopyException as CopyException

-- | The ability codec is a PARAMETER rather than an import, for the reason
-- Pawl.Types.CopyException is parametric: the ability codec reaches this one.
codec :: (Typeable.Typeable ability, Eq ability) => Codec.Codec ability -> Codec.Codec (CopyException.CopyException ability)
codec abilityCodec =
  Arm.tagged
    [ Arm.payload "SetPowerToughness" SetPowerToughness.codec CopyException.SetPowerToughness $ \x -> case x of
        CopyException.SetPowerToughness y -> Just y
        _ -> Nothing,
      Arm.payload "GainKeywords" (Common.set Keyword.codec) CopyException.GainKeywords $ \x -> case x of
        CopyException.GainKeywords y -> Just y
        _ -> Nothing,
      Arm.nullary "GainThisAbility" CopyException.GainThisAbility,
      Arm.payload "AddCardTypes" (Common.set CardType.codec) CopyException.AddCardTypes $ \x -> case x of
        CopyException.AddCardTypes y -> Just y
        _ -> Nothing,
      Arm.payload "AddSubtypes" (Common.set Subtype.codec) CopyException.AddSubtypes $ \x -> case x of
        CopyException.AddSubtypes y -> Just y
        _ -> Nothing,
      Arm.payload "AddSupertypes" (Common.set Supertype.codec) CopyException.AddSupertypes $ \x -> case x of
        CopyException.AddSupertypes y -> Just y
        _ -> Nothing,
      Arm.payload "RemoveSupertypes" (Common.set Supertype.codec) CopyException.RemoveSupertypes $ \x -> case x of
        CopyException.RemoveSupertypes y -> Just y
        _ -> Nothing,
      Arm.payload "SetName" CardName.codec CopyException.SetName $ \x -> case x of
        CopyException.SetName y -> Just y
        _ -> Nothing,
      Arm.payload "SetColors" (Common.set Color.codec) CopyException.SetColors $ \x -> case x of
        CopyException.SetColors y -> Just y
        _ -> Nothing,
      Arm.nullary "NoManaCost" CopyException.NoManaCost,
      Arm.nullary "DontCopyColors" CopyException.DontCopyColors,
      Arm.payload "GainAbility" abilityCodec CopyException.GainAbility $ \x -> case x of
        CopyException.GainAbility y -> Just y
        _ -> Nothing
    ]
