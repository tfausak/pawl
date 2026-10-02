module Pawl.Codec.Check where

import qualified Pawl.Codec.AttackersAre as AttackersAre
import qualified Pawl.Codec.BlockersAre as BlockersAre
import qualified Pawl.Codec.CountIs as CountIs
import qualified Pawl.Codec.CountersAre as CountersAre
import qualified Pawl.Codec.DamageIs as DamageIs
import qualified Pawl.Codec.DefendersAre as DefendersAre
import qualified Pawl.Codec.KeywordsAre as KeywordsAre
import qualified Pawl.Codec.LifeIs as LifeIs
import qualified Pawl.Codec.MonarchIs as MonarchIs
import qualified Pawl.Codec.NamesAre as NamesAre
import qualified Pawl.Codec.PlayerCountersAre as PlayerCountersAre
import qualified Pawl.Codec.PowerToughnessIs as PowerToughnessIs
import qualified Pawl.Codec.SubtypesAre as SubtypesAre
import qualified Pawl.Codec.TappedIs as TappedIs
import qualified Pawl.Codec.TypesAre as TypesAre
import qualified Pawl.Codec.ViewIs as ViewIs
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Check as Check

-- | Keyed, as Pawl.Codec.Move is: @{"Life": {"player": "bob", "life": 18}}@.
codec :: Codec.Codec Check.Check
codec =
  Arm.keyed
    tagOf
    [ Arm.payload "Life" LifeIs.codec Check.Life (\x -> case x of Check.Life y -> Just y; _ -> Nothing),
      Arm.payload "Count" CountIs.codec Check.Count (\x -> case x of Check.Count y -> Just y; _ -> Nothing),
      Arm.payload "Damage" DamageIs.codec Check.Damage (\x -> case x of Check.Damage y -> Just y; _ -> Nothing),
      Arm.payload "Tapped" TappedIs.codec Check.Tapped (\x -> case x of Check.Tapped y -> Just y; _ -> Nothing),
      Arm.payload "Counters" CountersAre.codec Check.Counters (\x -> case x of Check.Counters y -> Just y; _ -> Nothing),
      Arm.payload "Types" TypesAre.codec Check.Types (\x -> case x of Check.Types y -> Just y; _ -> Nothing),
      Arm.payload "Attackers" AttackersAre.codec Check.Attackers (\x -> case x of Check.Attackers y -> Just y; _ -> Nothing),
      Arm.payload "Blockers" BlockersAre.codec Check.Blockers (\x -> case x of Check.Blockers y -> Just y; _ -> Nothing),
      Arm.payload "Defenders" DefendersAre.codec Check.Defenders (\x -> case x of Check.Defenders y -> Just y; _ -> Nothing),
      Arm.payload "Monarch" MonarchIs.codec Check.Monarch (\x -> case x of Check.Monarch y -> Just y; _ -> Nothing),
      Arm.payload "PowerToughness" PowerToughnessIs.codec Check.PowerToughness (\x -> case x of Check.PowerToughness y -> Just y; _ -> Nothing),
      Arm.payload "PlayerCounters" PlayerCountersAre.codec Check.PlayerCounters (\x -> case x of Check.PlayerCounters y -> Just y; _ -> Nothing),
      Arm.payload "Subtypes" SubtypesAre.codec Check.Subtypes (\x -> case x of Check.Subtypes y -> Just y; _ -> Nothing),
      Arm.payload "Names" NamesAre.codec Check.Names (\x -> case x of Check.Names y -> Just y; _ -> Nothing),
      Arm.payload "View" ViewIs.codec Check.View (\x -> case x of Check.View y -> Just y; _ -> Nothing),
      Arm.payload "Keywords" KeywordsAre.codec Check.Keywords (\x -> case x of Check.Keywords y -> Just y; _ -> Nothing)
    ]

tagOf :: Check.Check -> String
tagOf x = case x of
  Check.Life {} -> "Life"
  Check.Count {} -> "Count"
  Check.Damage {} -> "Damage"
  Check.Tapped {} -> "Tapped"
  Check.Counters {} -> "Counters"
  Check.Types {} -> "Types"
  Check.Attackers {} -> "Attackers"
  Check.Blockers {} -> "Blockers"
  Check.Defenders {} -> "Defenders"
  Check.Monarch {} -> "Monarch"
  Check.PowerToughness {} -> "PowerToughness"
  Check.PlayerCounters {} -> "PlayerCounters"
  Check.Subtypes {} -> "Subtypes"
  Check.Names {} -> "Names"
  Check.View {} -> "View"
  Check.Keywords {} -> "Keywords"
