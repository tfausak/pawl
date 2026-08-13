module Pawl.Codec.TriggerCondition where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.RoomIndex as RoomIndex
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.TriggerFrequency as TriggerFrequency
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TriggerCondition as TriggerCondition

toJson :: TriggerCondition.TriggerCondition -> Value.Value
toJson c = case c of
  TriggerCondition.SelfEnters -> Common.nullary "SelfEnters"
  TriggerCondition.PermanentEnters f -> Common.tagged "PermanentEnters" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  TriggerCondition.StepBegins p s -> Common.tagged "StepBegins" . Just . Value.array $ [Codec.encode Phase.codec p, Codec.encode TurnScope.codec s]
  TriggerCondition.StateIs c2 -> Common.tagged "StateIs" . Just $ Codec.encode Condition.codec c2
  TriggerCondition.SelfDealsCombatDamageToPlayer -> Common.nullary "SelfDealsCombatDamageToPlayer"
  TriggerCondition.PermanentDealsCombatDamageToPlayer f -> Common.tagged "PermanentDealsCombatDamageToPlayer" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> Common.nullary "CreatureDealtCombatDamageToMonarch"
  TriggerCondition.OpponentLostLifeDuringYourTurn -> Common.nullary "OpponentLostLifeDuringYourTurn"
  TriggerCondition.SelfAttacks f -> Common.tagged "SelfAttacks" . Just $ Codec.encode TriggerFrequency.codec f
  TriggerCondition.SelfAttacksWithAnother f -> Common.tagged "SelfAttacksWithAnother" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  TriggerCondition.CreatureAttacksAlone f -> Common.tagged "CreatureAttacksAlone" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  TriggerCondition.SelfAttacksPlayerWithMostLife -> Common.nullary "SelfAttacksPlayerWithMostLife"
  TriggerCondition.SelfBlocks -> Common.nullary "SelfBlocks"
  TriggerCondition.SelfBlocksCreature -> Common.nullary "SelfBlocksCreature"
  TriggerCondition.SelfBlocksAtLeast n -> Common.tagged "SelfBlocksAtLeast" . Just $ Common.encodeNatural n
  TriggerCondition.SelfBlocksOneOrMore f -> Common.tagged "SelfBlocksOneOrMore" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  TriggerCondition.SelfBecomesBlocked -> Common.nullary "SelfBecomesBlocked"
  TriggerCondition.SelfBecomesBlockedBy f -> Common.tagged "SelfBecomesBlockedBy" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  TriggerCondition.SelfBecomesBlockedByOneOrMore f -> Common.tagged "SelfBecomesBlockedByOneOrMore" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  TriggerCondition.SelfCycled -> Common.nullary "SelfCycled"
  TriggerCondition.SelfRevealedForMiracle -> Common.nullary "SelfRevealedForMiracle"
  TriggerCondition.PlayerDiscards r -> Common.tagged "PlayerDiscards" . Just $ Codec.encode PlayerRelation.codec r
  TriggerCondition.PlayerDrawsNthCard r n -> Common.tagged "PlayerDrawsNthCard" . Just . Value.array $ [Codec.encode PlayerRelation.codec r, Common.encodeNatural n]
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Common.nullary "SelfPutIntoGraveyardFromLibrary"
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> Common.nullary "SelfPutIntoGraveyardFromAnywhere"
  TriggerCondition.SelfDies -> Common.nullary "SelfDies"
  TriggerCondition.PermanentDies f -> Common.tagged "PermanentDies" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  TriggerCondition.SelfLeavesTheBattlefield -> Common.nullary "SelfLeavesTheBattlefield"
  TriggerCondition.HauntedCreatureDies -> Common.nullary "HauntedCreatureDies"
  TriggerCondition.SpellOrAbilityCounters r -> Common.tagged "SpellOrAbilityCounters" . Just $ Codec.encode PlayerRelation.codec r
  TriggerCondition.DamageToPlayerPrevented r -> Common.tagged "DamageToPlayerPrevented" . Just $ Codec.encode PlayerRelation.codec r
  TriggerCondition.PlayerGainsLife r -> Common.tagged "PlayerGainsLife" . Just $ Codec.encode PlayerRelation.codec r
  TriggerCondition.PlayerLosesLife r -> Common.tagged "PlayerLosesLife" . Just $ Codec.encode PlayerRelation.codec r
  TriggerCondition.SelfCountersReached kind n -> Common.tagged "SelfCountersReached" . Just . Value.array $ [Codec.encode (CounterKind.codec Keyword.codec) kind, Common.encodeNatural n]
  TriggerCondition.SelfLastCounterRemoved kind -> Common.tagged "SelfLastCounterRemoved" . Just $ Codec.encode (CounterKind.codec Keyword.codec) kind
  TriggerCondition.SpellCast f s -> Common.tagged "SpellCast" . Just . Value.array $ [Codec.encode (Filter.codec Keyword.codec) f, Codec.encode TurnScope.codec s]
  TriggerCondition.SelfCast -> Common.nullary "SelfCast"
  TriggerCondition.SelfHalfUnlocked n -> Common.tagged "SelfHalfUnlocked" . Just $ Codec.encode CardName.codec n
  TriggerCondition.RoomFullyUnlocked r -> Common.tagged "RoomFullyUnlocked" . Just $ Codec.encode PlayerRelation.codec r
  -- RECURSIVE, and the only condition that is: an AnyOf holds conditions, so both
  -- directions of this codec call themselves.
  TriggerCondition.AnyOf cs -> Common.tagged "AnyOf" . Just . Value.array $ fmap toJson cs
  TriggerCondition.SelfTurnedFaceUp -> Common.nullary "SelfTurnedFaceUp"
  TriggerCondition.PermanentTurnedFaceUp f -> Common.tagged "PermanentTurnedFaceUp" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  TriggerCondition.PermanentBecomesDesignated d f -> Common.tagged "PermanentBecomesDesignated" . Just . Value.array $ [Designation.toJson d, Codec.encode (Filter.codec Keyword.codec) f]
  TriggerCondition.SelfEvolves -> Common.nullary "SelfEvolves"
  TriggerCondition.AttachedCreatureMentors -> Common.nullary "AttachedCreatureMentors"
  TriggerCondition.PermanentSacrificed -> Common.nullary "PermanentSacrificed"
  TriggerCondition.SagaFinalChapterTriggers r -> Common.tagged "SagaFinalChapterTriggers" . Just $ Codec.encode PlayerRelation.codec r
  TriggerCondition.PlayerBecomesMonarch r -> Common.tagged "PlayerBecomesMonarch" . Just $ Codec.encode PlayerRelation.codec r
  TriggerCondition.LoseControlOfBound s -> Common.tagged "LoseControlOfBound" . Just $ Codec.encode SlotName.codec s
  TriggerCondition.RoomEntered r -> Common.tagged "RoomEntered" . Just $ Codec.encode RoomIndex.codec r

fromJson :: Value.Value -> Either Text.Text TriggerCondition.TriggerCondition
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("SelfEnters", _) -> Right TriggerCondition.SelfEnters
    ("PermanentEnters", Just v) -> TriggerCondition.PermanentEnters <$> Codec.decode (Filter.codec Keyword.codec) v
    ("StepBegins", Just (Value.Array (Array.MkArray [p, s]))) -> TriggerCondition.StepBegins <$> Codec.decode Phase.codec p <*> Codec.decode TurnScope.codec s
    ("StateIs", Just v) -> TriggerCondition.StateIs <$> Codec.decode Condition.codec v
    ("SelfDealsCombatDamageToPlayer", _) -> Right TriggerCondition.SelfDealsCombatDamageToPlayer
    ("PermanentDealsCombatDamageToPlayer", Just v) -> TriggerCondition.PermanentDealsCombatDamageToPlayer <$> Codec.decode (Filter.codec Keyword.codec) v
    ("CreatureDealtCombatDamageToMonarch", _) -> Right TriggerCondition.CreatureDealtCombatDamageToMonarch
    ("OpponentLostLifeDuringYourTurn", _) -> Right TriggerCondition.OpponentLostLifeDuringYourTurn
    ("SelfAttacks", Just v) -> TriggerCondition.SelfAttacks <$> Codec.decode TriggerFrequency.codec v
    ("SelfAttacksWithAnother", Just v) -> TriggerCondition.SelfAttacksWithAnother <$> Codec.decode (Filter.codec Keyword.codec) v
    ("CreatureAttacksAlone", Just v) -> TriggerCondition.CreatureAttacksAlone <$> Codec.decode (Filter.codec Keyword.codec) v
    ("SelfAttacksPlayerWithMostLife", _) -> Right TriggerCondition.SelfAttacksPlayerWithMostLife
    ("SelfBlocks", _) -> Right TriggerCondition.SelfBlocks
    ("SelfBlocksCreature", _) -> Right TriggerCondition.SelfBlocksCreature
    ("SelfBlocksAtLeast", Just v) -> TriggerCondition.SelfBlocksAtLeast <$> Common.decodeNatural v
    ("SelfBlocksOneOrMore", Just v) -> TriggerCondition.SelfBlocksOneOrMore <$> Codec.decode (Filter.codec Keyword.codec) v
    ("SelfBecomesBlocked", _) -> Right TriggerCondition.SelfBecomesBlocked
    ("SelfBecomesBlockedBy", Just v) -> TriggerCondition.SelfBecomesBlockedBy <$> Codec.decode (Filter.codec Keyword.codec) v
    ("SelfBecomesBlockedByOneOrMore", Just v) -> TriggerCondition.SelfBecomesBlockedByOneOrMore <$> Codec.decode (Filter.codec Keyword.codec) v
    ("SelfCycled", _) -> Right TriggerCondition.SelfCycled
    ("SelfRevealedForMiracle", _) -> Right TriggerCondition.SelfRevealedForMiracle
    ("PlayerDiscards", Just v) -> TriggerCondition.PlayerDiscards <$> Codec.decode PlayerRelation.codec v
    ("PlayerDrawsNthCard", Just (Value.Array (Array.MkArray [r, n]))) -> TriggerCondition.PlayerDrawsNthCard <$> Codec.decode PlayerRelation.codec r <*> Common.decodeNatural n
    ("SelfPutIntoGraveyardFromLibrary", _) -> Right TriggerCondition.SelfPutIntoGraveyardFromLibrary
    ("SelfPutIntoGraveyardFromAnywhere", _) -> Right TriggerCondition.SelfPutIntoGraveyardFromAnywhere
    ("SelfDies", _) -> Right TriggerCondition.SelfDies
    ("PermanentDies", Just v) -> TriggerCondition.PermanentDies <$> Codec.decode (Filter.codec Keyword.codec) v
    ("SelfLeavesTheBattlefield", _) -> Right TriggerCondition.SelfLeavesTheBattlefield
    ("HauntedCreatureDies", _) -> Right TriggerCondition.HauntedCreatureDies
    ("SpellOrAbilityCounters", Just v) -> TriggerCondition.SpellOrAbilityCounters <$> Codec.decode PlayerRelation.codec v
    ("DamageToPlayerPrevented", Just v) -> TriggerCondition.DamageToPlayerPrevented <$> Codec.decode PlayerRelation.codec v
    ("PlayerGainsLife", Just v) -> TriggerCondition.PlayerGainsLife <$> Codec.decode PlayerRelation.codec v
    ("PlayerLosesLife", Just v) -> TriggerCondition.PlayerLosesLife <$> Codec.decode PlayerRelation.codec v
    ("SelfCountersReached", Just (Value.Array (Array.MkArray [kind, n]))) -> TriggerCondition.SelfCountersReached <$> Codec.decode (CounterKind.codec Keyword.codec) kind <*> Common.decodeNatural n
    ("SelfLastCounterRemoved", Just v) -> TriggerCondition.SelfLastCounterRemoved <$> Codec.decode (CounterKind.codec Keyword.codec) v
    ("SpellCast", Just (Value.Array (Array.MkArray [f, s]))) -> TriggerCondition.SpellCast <$> Codec.decode (Filter.codec Keyword.codec) f <*> Codec.decode TurnScope.codec s
    ("SelfCast", _) -> Right TriggerCondition.SelfCast
    ("SelfHalfUnlocked", Just v) -> TriggerCondition.SelfHalfUnlocked <$> Codec.decode CardName.codec v
    ("RoomFullyUnlocked", Just v) -> TriggerCondition.RoomFullyUnlocked <$> Codec.decode PlayerRelation.codec v
    ("AnyOf", Just (Value.Array (Array.MkArray cs))) -> TriggerCondition.AnyOf <$> traverse fromJson cs
    ("SelfTurnedFaceUp", _) -> Right TriggerCondition.SelfTurnedFaceUp
    ("PermanentTurnedFaceUp", Just v) -> TriggerCondition.PermanentTurnedFaceUp <$> Codec.decode (Filter.codec Keyword.codec) v
    ("PermanentBecomesDesignated", Just (Value.Array (Array.MkArray [d, f]))) ->
      TriggerCondition.PermanentBecomesDesignated <$> Designation.fromJson d <*> Codec.decode (Filter.codec Keyword.codec) f
    ("SelfEvolves", _) -> Right TriggerCondition.SelfEvolves
    ("AttachedCreatureMentors", _) -> Right TriggerCondition.AttachedCreatureMentors
    ("PermanentSacrificed", _) -> Right TriggerCondition.PermanentSacrificed
    ("SagaFinalChapterTriggers", Just v) -> TriggerCondition.SagaFinalChapterTriggers <$> Codec.decode PlayerRelation.codec v
    ("PlayerBecomesMonarch", Just v) -> TriggerCondition.PlayerBecomesMonarch <$> Codec.decode PlayerRelation.codec v
    ("LoseControlOfBound", Just v) -> TriggerCondition.LoseControlOfBound <$> Codec.decode SlotName.codec v
    ("RoomEntered", Just v) -> TriggerCondition.RoomEntered <$> Codec.decode RoomIndex.codec v
    _ -> Left . Text.pack $ "unknown TriggerCondition: " <> t
