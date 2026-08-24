module Pawl.Codec.Source where

import qualified Pawl.Codec.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Codec.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.Codec.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Source as Source

codec :: Codec.Codec Source.Source
codec =
  Arm.tagged
    [ Arm.payload "OfCard" PrintingId.codec Source.OfCard (\x -> case x of Source.OfCard y -> Just y; _ -> Nothing),
      Arm.payload "OfToken" PrintingId.codec Source.OfToken (\x -> case x of Source.OfToken y -> Just y; _ -> Nothing),
      Arm.payload "OfAbility" ActivatedAbilitySource.codec Source.OfAbility (\x -> case x of Source.OfAbility y -> Just y; _ -> Nothing),
      Arm.payload "OfTrigger" TriggeredAbilitySource.codec Source.OfTrigger (\x -> case x of Source.OfTrigger y -> Just y; _ -> Nothing),
      Arm.payload "OfEmblem" PrintingId.codec Source.OfEmblem (\x -> case x of Source.OfEmblem y -> Just y; _ -> Nothing),
      Arm.payload "OfSpellCopy" PrintingId.codec Source.OfSpellCopy (\x -> case x of Source.OfSpellCopy y -> Just y; _ -> Nothing),
      Arm.payload "OfInherentTrigger" InherentTriggerSource.codec Source.OfInherentTrigger (\x -> case x of Source.OfInherentTrigger y -> Just y; _ -> Nothing)
    ]
