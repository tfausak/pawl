module Pawl.Codec.CopyException where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SetPowerToughness as SetPowerToughness
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CopyException as CopyException

codec :: Codec.Codec CopyException.CopyException
codec =
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
        _ -> Nothing
    ]
