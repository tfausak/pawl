module Pawl.Codec.TriggerCondition where

import qualified Pawl.Codec.AbilityAddsMana as AbilityAddsMana
import qualified Pawl.Codec.ActingPermanent as ActingPermanent
import qualified Pawl.Codec.CardLeavesZone as CardLeavesZone
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CardPutIntoGraveyard as CardPutIntoGraveyard
import qualified Pawl.Codec.CardsPutIntoZone as CardsPutIntoZone
import qualified Pawl.Codec.ClassLevel as ClassLevel
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.ControllerBecomesTarget as ControllerBecomesTarget
import qualified Pawl.Codec.CounterPlacement as CounterPlacement
import qualified Pawl.Codec.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast
import qualified Pawl.Codec.CreatureExploits as CreatureExploits
import qualified Pawl.Codec.DieResult as DieResult
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.OwnedZone as OwnedZone
import qualified Pawl.Codec.PermanentActed as PermanentActed
import qualified Pawl.Codec.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Codec.PermanentDealsCombatDamageToPlayer as PermanentDealsCombatDamageToPlayer
import qualified Pawl.Codec.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Codec.PermanentTappedForMana as PermanentTappedForMana
import qualified Pawl.Codec.PermanentsBecomeTargeted as PermanentsBecomeTargeted
import qualified Pawl.Codec.PermanentsDealCombatDamageToPlayer as PermanentsDealCombatDamageToPlayer
import qualified Pawl.Codec.PlacesSticker as PlacesSticker
import qualified Pawl.Codec.PlayerActed as PlayerActed
import qualified Pawl.Codec.PlayerAttacksPlayer as PlayerAttacksPlayer
import qualified Pawl.Codec.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Codec.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.PlaysLand as PlaysLand
import qualified Pawl.Codec.RoomIndex as RoomIndex
import qualified Pawl.Codec.SelfCountersReached as SelfCountersReached
import qualified Pawl.Codec.SelfCountersRemoved as SelfCountersRemoved
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.SpellCast as SpellCast
import qualified Pawl.Codec.StepBegins as StepBegins
import qualified Pawl.Codec.TriggerFrequency as TriggerFrequency
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TriggerCondition as TriggerCondition

-- | RECURSIVE, through the one arm that holds conditions: 'AnyOf' names 'codec'
-- inside its own definition. That terminates because 'Arm.tagged' reaches WHNF
-- as a 'Codec.MkCodec' without forcing its arm list, and the SCHEMA terminates
-- because 'Define.define' registers this type's name before running the body,
-- so the re-entry emits a @$ref@ rather than recursing.
--
-- The five two-element-array payloads this codec used to write are now named
-- objects with records behind them (#1305).
codec :: Codec.Codec TriggerCondition.TriggerCondition
codec =
  let filterCodec = Filter.codec Keyword.codec
   in Arm.tagged
        tagOf
        [ Arm.nullary "SelfEnters" TriggerCondition.SelfEnters,
          Arm.payload "PermanentEnters" filterCodec TriggerCondition.PermanentEnters (\x -> case x of TriggerCondition.PermanentEnters y -> Just y; _ -> Nothing),
          Arm.payload "PermanentsEnter" filterCodec TriggerCondition.PermanentsEnter (\x -> case x of TriggerCondition.PermanentsEnter y -> Just y; _ -> Nothing),
          Arm.payload "StepBegins" StepBegins.codec TriggerCondition.StepBegins (\x -> case x of TriggerCondition.StepBegins y -> Just y; _ -> Nothing),
          Arm.payload "StateIs" Condition.codec TriggerCondition.StateIs (\x -> case x of TriggerCondition.StateIs y -> Just y; _ -> Nothing),
          Arm.payload "SelfDealsCombatDamageToPlayer" PlayerRelation.codec TriggerCondition.SelfDealsCombatDamageToPlayer (\x -> case x of TriggerCondition.SelfDealsCombatDamageToPlayer y -> Just y; _ -> Nothing),
          Arm.nullary "SelfDealsCombatDamageToPlayerOrBattle" TriggerCondition.SelfDealsCombatDamageToPlayerOrBattle,
          Arm.nullary "SelfDealsCombatDamage" TriggerCondition.SelfDealsCombatDamage,
          Arm.payload "SelfDealsDamageToPlayer" PlayerRelation.codec TriggerCondition.SelfDealsDamageToPlayer (\x -> case x of TriggerCondition.SelfDealsDamageToPlayer y -> Just y; _ -> Nothing),
          Arm.nullary "SelfDealsDamageToCreature" TriggerCondition.SelfDealsDamageToCreature,
          Arm.nullary "SelfDealsDamage" TriggerCondition.SelfDealsDamage,
          Arm.nullary "SelfIsDealtDamage" TriggerCondition.SelfIsDealtDamage,
          Arm.payload "PermanentDealsCombatDamageToPlayer" PermanentDealsCombatDamageToPlayer.codec TriggerCondition.PermanentDealsCombatDamageToPlayer (\x -> case x of TriggerCondition.PermanentDealsCombatDamageToPlayer y -> Just y; _ -> Nothing),
          Arm.payload "PermanentsDealCombatDamageToPlayer" PermanentsDealCombatDamageToPlayer.codec TriggerCondition.PermanentsDealCombatDamageToPlayer (\x -> case x of TriggerCondition.PermanentsDealCombatDamageToPlayer y -> Just y; _ -> Nothing),
          Arm.nullary "CreatureDealtCombatDamageToMonarch" TriggerCondition.CreatureDealtCombatDamageToMonarch,
          Arm.nullary "CreaturesDealtCombatDamageToInitiative" TriggerCondition.CreaturesDealtCombatDamageToInitiative,
          Arm.nullary "PlayerTookInitiative" TriggerCondition.PlayerTookInitiative,
          Arm.nullary "OpponentLostLifeDuringYourTurn" TriggerCondition.OpponentLostLifeDuringYourTurn,
          Arm.payload "SelfAttacks" TriggerFrequency.codec TriggerCondition.SelfAttacks (\x -> case x of TriggerCondition.SelfAttacks y -> Just y; _ -> Nothing),
          Arm.payload "SelfAttacksWithAnother" filterCodec TriggerCondition.SelfAttacksWithAnother (\x -> case x of TriggerCondition.SelfAttacksWithAnother y -> Just y; _ -> Nothing),
          Arm.payload "SelfAttacksPermanent" filterCodec TriggerCondition.SelfAttacksPermanent (\x -> case x of TriggerCondition.SelfAttacksPermanent y -> Just y; _ -> Nothing),
          Arm.payload "CreatureAttacksAlone" filterCodec TriggerCondition.CreatureAttacksAlone (\x -> case x of TriggerCondition.CreatureAttacksAlone y -> Just y; _ -> Nothing),
          Arm.nullary "CreatureAttacksYou" TriggerCondition.CreatureAttacksYou,
          Arm.payload "CreatureAttacks" filterCodec TriggerCondition.CreatureAttacks (\x -> case x of TriggerCondition.CreatureAttacks y -> Just y; _ -> Nothing),
          Arm.payload "PlayerAttacks" PlayerRelation.codec TriggerCondition.PlayerAttacks (\x -> case x of TriggerCondition.PlayerAttacks y -> Just y; _ -> Nothing),
          Arm.payload "PlayerAttacksWith" PlayerAttacksWith.codec TriggerCondition.PlayerAttacksWith (\x -> case x of TriggerCondition.PlayerAttacksWith y -> Just y; _ -> Nothing),
          Arm.payload "PlayerAttacksPlayer" PlayerAttacksPlayer.codec TriggerCondition.PlayerAttacksPlayer (\x -> case x of TriggerCondition.PlayerAttacksPlayer y -> Just y; _ -> Nothing),
          Arm.nullary "AttachedPlayerIsAttacked" TriggerCondition.AttachedPlayerIsAttacked,
          Arm.nullary "SelfIsAttacked" TriggerCondition.SelfIsAttacked,
          Arm.nullary "SelfAttacksPlayerWithMostLife" TriggerCondition.SelfAttacksPlayerWithMostLife,
          Arm.nullary "SelfAttacksWhileSaddled" TriggerCondition.SelfAttacksWhileSaddled,
          Arm.payload "SelfAttacksWhile" Condition.codec TriggerCondition.SelfAttacksWhile (\x -> case x of TriggerCondition.SelfAttacksWhile y -> Just y; _ -> Nothing),
          Arm.nullary "SelfBlocks" TriggerCondition.SelfBlocks,
          Arm.payload "CreatureBlocks" filterCodec TriggerCondition.CreatureBlocks (\x -> case x of TriggerCondition.CreatureBlocks y -> Just y; _ -> Nothing),
          Arm.payload "SelfBlocksCreature" filterCodec TriggerCondition.SelfBlocksCreature (\x -> case x of TriggerCondition.SelfBlocksCreature y -> Just y; _ -> Nothing),
          Arm.payload "SelfBlocksAtLeast" Common.natural TriggerCondition.SelfBlocksAtLeast (\x -> case x of TriggerCondition.SelfBlocksAtLeast y -> Just y; _ -> Nothing),
          Arm.payload "SelfBlocksOneOrMore" filterCodec TriggerCondition.SelfBlocksOneOrMore (\x -> case x of TriggerCondition.SelfBlocksOneOrMore y -> Just y; _ -> Nothing),
          Arm.nullary "SelfBecomesBlocked" TriggerCondition.SelfBecomesBlocked,
          Arm.payload "SelfBecomesBlockedBy" filterCodec TriggerCondition.SelfBecomesBlockedBy (\x -> case x of TriggerCondition.SelfBecomesBlockedBy y -> Just y; _ -> Nothing),
          Arm.payload "SelfBecomesBlockedByOneOrMore" filterCodec TriggerCondition.SelfBecomesBlockedByOneOrMore (\x -> case x of TriggerCondition.SelfBecomesBlockedByOneOrMore y -> Just y; _ -> Nothing),
          Arm.payload "CreatureBecomesBlockedByAtLeast" CreatureBecomesBlockedByAtLeast.codec TriggerCondition.CreatureBecomesBlockedByAtLeast (\x -> case x of TriggerCondition.CreatureBecomesBlockedByAtLeast y -> Just y; _ -> Nothing),
          Arm.nullary "SelfAttacksUnblocked" TriggerCondition.SelfAttacksUnblocked,
          Arm.nullary "SelfCycled" TriggerCondition.SelfCycled,
          Arm.nullary "SelfRevealedForMiracle" TriggerCondition.SelfRevealedForMiracle,
          Arm.nullary "SelfExiledForMadness" TriggerCondition.SelfExiledForMadness,
          Arm.nullary "SelfDiscarded" TriggerCondition.SelfDiscarded,
          Arm.payload "PlayerDiscards" PlayerRelation.codec TriggerCondition.PlayerDiscards (\x -> case x of TriggerCondition.PlayerDiscards y -> Just y; _ -> Nothing),
          Arm.payload "PlayerDiscardsCards" PlayerRelation.codec TriggerCondition.PlayerDiscardsCards (\x -> case x of TriggerCondition.PlayerDiscardsCards y -> Just y; _ -> Nothing),
          Arm.payload "PlayerCycles" PlayerRelation.codec TriggerCondition.PlayerCycles (\x -> case x of TriggerCondition.PlayerCycles y -> Just y; _ -> Nothing),
          Arm.payload "PlayerDrawsNthCard" PlayerDrawsNthCard.codec TriggerCondition.PlayerDrawsNthCard (\x -> case x of TriggerCondition.PlayerDrawsNthCard y -> Just y; _ -> Nothing),
          Arm.nullary "SelfPutIntoGraveyardFromLibrary" TriggerCondition.SelfPutIntoGraveyardFromLibrary,
          Arm.nullary "SelfPutIntoGraveyardFromAnywhere" TriggerCondition.SelfPutIntoGraveyardFromAnywhere,
          Arm.nullary "SelfPutIntoGraveyardDuringResolution" TriggerCondition.SelfPutIntoGraveyardDuringResolution,
          Arm.nullary "SelfDies" TriggerCondition.SelfDies,
          Arm.nullary "SelfLeavesGraveyard" TriggerCondition.SelfLeavesGraveyard,
          Arm.payload "CardPutIntoGraveyard" CardPutIntoGraveyard.codec TriggerCondition.CardPutIntoGraveyard (\x -> case x of TriggerCondition.CardPutIntoGraveyard y -> Just y; _ -> Nothing),
          Arm.payload "PermanentDies" filterCodec TriggerCondition.PermanentDies (\x -> case x of TriggerCondition.PermanentDies y -> Just y; _ -> Nothing),
          Arm.payload "PermanentsDie" filterCodec TriggerCondition.PermanentsDie (\x -> case x of TriggerCondition.PermanentsDie y -> Just y; _ -> Nothing),
          Arm.nullary "SelfLeavesTheBattlefield" TriggerCondition.SelfLeavesTheBattlefield,
          Arm.payload "SelfPutFromBattlefieldInto" OwnedZone.codec TriggerCondition.SelfPutFromBattlefieldInto (\x -> case x of TriggerCondition.SelfPutFromBattlefieldInto y -> Just y; _ -> Nothing),
          Arm.payload "PermanentLeavesTheBattlefield" filterCodec TriggerCondition.PermanentLeavesTheBattlefield (\x -> case x of TriggerCondition.PermanentLeavesTheBattlefield y -> Just y; _ -> Nothing),
          Arm.payload "PermanentReturnedToHand" filterCodec TriggerCondition.PermanentReturnedToHand (\x -> case x of TriggerCondition.PermanentReturnedToHand y -> Just y; _ -> Nothing),
          Arm.payload "PermanentsReturnedToHand" filterCodec TriggerCondition.PermanentsReturnedToHand (\x -> case x of TriggerCondition.PermanentsReturnedToHand y -> Just y; _ -> Nothing),
          Arm.payload "CardLeavesZone" CardLeavesZone.codec TriggerCondition.CardLeavesZone (\x -> case x of TriggerCondition.CardLeavesZone y -> Just y; _ -> Nothing),
          Arm.payload "CardsLeaveZone" CardLeavesZone.codec TriggerCondition.CardsLeaveZone (\x -> case x of TriggerCondition.CardsLeaveZone y -> Just y; _ -> Nothing),
          Arm.payload "CardsPutIntoZone" CardsPutIntoZone.codec TriggerCondition.CardsPutIntoZone (\x -> case x of TriggerCondition.CardsPutIntoZone y -> Just y; _ -> Nothing),
          Arm.nullary "AttachedCreatureDies" TriggerCondition.AttachedCreatureDies,
          Arm.nullary "AttachedCreatureBecomesTapped" TriggerCondition.AttachedCreatureBecomesTapped,
          Arm.payload "PermanentsBecomeTapped" filterCodec TriggerCondition.PermanentsBecomeTapped (\x -> case x of TriggerCondition.PermanentsBecomeTapped y -> Just y; _ -> Nothing),
          Arm.nullary "SelfBecomesUntapped" TriggerCondition.SelfBecomesUntapped,
          Arm.nullary "AttachedPermanentTappedForMana" TriggerCondition.AttachedPermanentTappedForMana,
          Arm.payload "PermanentTappedForMana" PermanentTappedForMana.codec TriggerCondition.PermanentTappedForMana (\x -> case x of TriggerCondition.PermanentTappedForMana y -> Just y; _ -> Nothing),
          Arm.payload "AbilityAddsMana" AbilityAddsMana.codec TriggerCondition.AbilityAddsMana (\x -> case x of TriggerCondition.AbilityAddsMana y -> Just y; _ -> Nothing),
          Arm.nullary "SelfManaAbilityResolves" TriggerCondition.SelfManaAbilityResolves,
          Arm.nullary "HauntedCreatureDies" TriggerCondition.HauntedCreatureDies,
          Arm.payload "SpellOrAbilityCounters" PlayerRelation.codec TriggerCondition.SpellOrAbilityCounters (\x -> case x of TriggerCondition.SpellOrAbilityCounters y -> Just y; _ -> Nothing),
          Arm.nullary "AbilityIsCountered" TriggerCondition.AbilityIsCountered,
          Arm.payload "DamageToPlayerPrevented" PlayerRelation.codec TriggerCondition.DamageToPlayerPrevented (\x -> case x of TriggerCondition.DamageToPlayerPrevented y -> Just y; _ -> Nothing),
          Arm.payload "SelfPreventsDamage" filterCodec TriggerCondition.SelfPreventsDamage (\x -> case x of TriggerCondition.SelfPreventsDamage y -> Just y; _ -> Nothing),
          Arm.payload "PlayerGainsLife" PlayerRelation.codec TriggerCondition.PlayerGainsLife (\x -> case x of TriggerCondition.PlayerGainsLife y -> Just y; _ -> Nothing),
          Arm.payload "PlayersGainLife" PlayerRelation.codec TriggerCondition.PlayersGainLife (\x -> case x of TriggerCondition.PlayersGainLife y -> Just y; _ -> Nothing),
          Arm.payload "PlayerLosesLife" PlayerRelation.codec TriggerCondition.PlayerLosesLife (\x -> case x of TriggerCondition.PlayerLosesLife y -> Just y; _ -> Nothing),
          Arm.payload "SelfCountersReached" SelfCountersReached.codec TriggerCondition.SelfCountersReached (\x -> case x of TriggerCondition.SelfCountersReached y -> Just y; _ -> Nothing),
          Arm.payload "SelfBecomesClassLevel" ClassLevel.codec TriggerCondition.SelfBecomesClassLevel (\x -> case x of TriggerCondition.SelfBecomesClassLevel y -> Just y; _ -> Nothing),
          Arm.payload "SelfLastCounterRemoved" SelfCountersRemoved.codec TriggerCondition.SelfLastCounterRemoved (\x -> case x of TriggerCondition.SelfLastCounterRemoved y -> Just y; _ -> Nothing),
          Arm.payload "SelfCountersRemoved" SelfCountersRemoved.codec TriggerCondition.SelfCountersRemoved (\x -> case x of TriggerCondition.SelfCountersRemoved y -> Just y; _ -> Nothing),
          Arm.payload "SelfCounterRemoved" SelfCountersRemoved.codec TriggerCondition.SelfCounterRemoved (\x -> case x of TriggerCondition.SelfCounterRemoved y -> Just y; _ -> Nothing),
          Arm.payload "PermanentsGetCounters" CounterPlacement.codec TriggerCondition.PermanentsGetCounters (\x -> case x of TriggerCondition.PermanentsGetCounters y -> Just y; _ -> Nothing),
          Arm.payload "PermanentGetsCounters" CounterPlacement.codec TriggerCondition.PermanentGetsCounters (\x -> case x of TriggerCondition.PermanentGetsCounters y -> Just y; _ -> Nothing),
          Arm.payload "SpellCast" SpellCast.codec TriggerCondition.SpellCast (\x -> case x of TriggerCondition.SpellCast y -> Just y; _ -> Nothing),
          Arm.nullary "SelfCast" TriggerCondition.SelfCast,
          Arm.payload "SelfBecomesTargeted" PlayerRelation.codec TriggerCondition.SelfBecomesTargeted (\x -> case x of TriggerCondition.SelfBecomesTargeted y -> Just y; _ -> Nothing),
          Arm.payload "ControllerBecomesTarget" ControllerBecomesTarget.codec TriggerCondition.ControllerBecomesTarget (\x -> case x of TriggerCondition.ControllerBecomesTarget y -> Just y; _ -> Nothing),
          Arm.payload "PermanentsBecomeTargeted" PermanentsBecomeTargeted.codec TriggerCondition.PermanentsBecomeTargeted (\x -> case x of TriggerCondition.PermanentsBecomeTargeted y -> Just y; _ -> Nothing),
          Arm.payload "PermanentBecomesTargeted" PermanentsBecomeTargeted.codec TriggerCondition.PermanentBecomesTargeted (\x -> case x of TriggerCondition.PermanentBecomesTargeted y -> Just y; _ -> Nothing),
          Arm.payload "SelfHalfUnlocked" CardName.codec TriggerCondition.SelfHalfUnlocked (\x -> case x of TriggerCondition.SelfHalfUnlocked y -> Just y; _ -> Nothing),
          Arm.payload "RoomFullyUnlocked" PlayerRelation.codec TriggerCondition.RoomFullyUnlocked (\x -> case x of TriggerCondition.RoomFullyUnlocked y -> Just y; _ -> Nothing),
          Arm.payload "AnyOf" (Common.list codec) TriggerCondition.AnyOf (\x -> case x of TriggerCondition.AnyOf y -> Just y; _ -> Nothing),
          Arm.nullary "SelfTurnedFaceUp" TriggerCondition.SelfTurnedFaceUp,
          Arm.payload "SelfTransformedInto" CardName.codec TriggerCondition.SelfTransformedInto (\x -> case x of TriggerCondition.SelfTransformedInto y -> Just y; _ -> Nothing),
          Arm.payload "PermanentTransforms" filterCodec TriggerCondition.PermanentTransforms (\x -> case x of TriggerCondition.PermanentTransforms y -> Just y; _ -> Nothing),
          Arm.payload "PermanentTurnedFaceUp" filterCodec TriggerCondition.PermanentTurnedFaceUp (\x -> case x of TriggerCondition.PermanentTurnedFaceUp y -> Just y; _ -> Nothing),
          Arm.payload "PermanentActs" (PermanentActed.codec ActingPermanent.codec) TriggerCondition.PermanentActs (\x -> case x of TriggerCondition.PermanentActs y -> Just y; _ -> Nothing),
          Arm.nullary "FaceDownPermanentLeavesRevealed" TriggerCondition.FaceDownPermanentLeavesRevealed,
          Arm.payload "PermanentBecomesDesignated" PermanentBecomesDesignated.codec TriggerCondition.PermanentBecomesDesignated (\x -> case x of TriggerCondition.PermanentBecomesDesignated y -> Just y; _ -> Nothing),
          Arm.nullary "AttachedCreatureMentors" TriggerCondition.AttachedCreatureMentors,
          Arm.nullary "SelfExploits" TriggerCondition.SelfExploits,
          Arm.payload "CreatureExploits" CreatureExploits.codec TriggerCondition.CreatureExploits (\x -> case x of TriggerCondition.CreatureExploits y -> Just y; _ -> Nothing),
          Arm.payload "SelfBecomesCrewed" TriggerFrequency.codec TriggerCondition.SelfBecomesCrewed (\x -> case x of TriggerCondition.SelfBecomesCrewed y -> Just y; _ -> Nothing),
          Arm.nullary "SelfCrewsVehicle" TriggerCondition.SelfCrewsVehicle,
          Arm.payload "PermanentSacrificed" PermanentSacrificed.codec TriggerCondition.PermanentSacrificed (\x -> case x of TriggerCondition.PermanentSacrificed y -> Just y; _ -> Nothing),
          Arm.payload "SagaFinalChapterTriggers" PlayerRelation.codec TriggerCondition.SagaFinalChapterTriggers (\x -> case x of TriggerCondition.SagaFinalChapterTriggers y -> Just y; _ -> Nothing),
          Arm.payload "PlayerBecomesMonarch" PlayerRelation.codec TriggerCondition.PlayerBecomesMonarch (\x -> case x of TriggerCondition.PlayerBecomesMonarch y -> Just y; _ -> Nothing),
          Arm.payload "LoseControlOfBound" SlotName.codec TriggerCondition.LoseControlOfBound (\x -> case x of TriggerCondition.LoseControlOfBound y -> Just y; _ -> Nothing),
          Arm.payload "BoundDiesOrIsExiled" SlotName.codec TriggerCondition.BoundDiesOrIsExiled (\x -> case x of TriggerCondition.BoundDiesOrIsExiled y -> Just y; _ -> Nothing),
          Arm.payload "BoundDies" SlotName.codec TriggerCondition.BoundDies (\x -> case x of TriggerCondition.BoundDies y -> Just y; _ -> Nothing),
          Arm.payload "RoomEntered" RoomIndex.codec TriggerCondition.RoomEntered (\x -> case x of TriggerCondition.RoomEntered y -> Just y; _ -> Nothing),
          Arm.payload "PlayerActs" (PlayerActed.codec PlayerRelation.codec) TriggerCondition.PlayerActs (\x -> case x of TriggerCondition.PlayerActs y -> Just y; _ -> Nothing),
          Arm.payload "PlayerLosesGame" PlayerRelation.codec TriggerCondition.PlayerLosesGame (\x -> case x of TriggerCondition.PlayerLosesGame y -> Just y; _ -> Nothing),
          Arm.payload "PlayerPlaysLand" PlaysLand.codec TriggerCondition.PlayerPlaysLand (\x -> case x of TriggerCondition.PlayerPlaysLand y -> Just y; _ -> Nothing),
          Arm.payload "PlayerManifestsDread" PlayerRelation.codec TriggerCondition.PlayerManifestsDread (\x -> case x of TriggerCondition.PlayerManifestsDread y -> Just y; _ -> Nothing),
          Arm.payload "PlayerRollsResult" (DieResult.codec PlayerRelation.codec) TriggerCondition.PlayerRollsResult (\x -> case x of TriggerCondition.PlayerRollsResult y -> Just y; _ -> Nothing),
          Arm.nullary "Visit" TriggerCondition.Visit,
          Arm.nullary "ChaosEnsues" TriggerCondition.ChaosEnsues,
          Arm.payload "PlayerRollsPlaneswalker" PlayerRelation.codec TriggerCondition.PlayerRollsPlaneswalker (\x -> case x of TriggerCondition.PlayerRollsPlaneswalker y -> Just y; _ -> Nothing),
          Arm.nullary "SetInMotion" TriggerCondition.SetInMotion,
          Arm.payload "PlayerWinsCoinFlip" PlayerRelation.codec TriggerCondition.PlayerWinsCoinFlip (\x -> case x of TriggerCondition.PlayerWinsCoinFlip y -> Just y; _ -> Nothing),
          Arm.payload "PlayerLosesCoinFlip" PlayerRelation.codec TriggerCondition.PlayerLosesCoinFlip (\x -> case x of TriggerCondition.PlayerLosesCoinFlip y -> Just y; _ -> Nothing),
          Arm.nullary "SelfBecomesPlotted" TriggerCondition.SelfBecomesPlotted,
          Arm.nullary "Reflexive" TriggerCondition.Reflexive,
          Arm.payload "SelfBecomesAttachedBy" filterCodec TriggerCondition.SelfBecomesAttachedBy (\x -> case x of TriggerCondition.SelfBecomesAttachedBy y -> Just y; _ -> Nothing),
          -- CR 701.3a read from the attachment, and CR 701.3d's mirror of it. Same
          -- shape as the arm above with the Filter over the HOST instead.
          Arm.payload "SelfBecomesAttachedTo" filterCodec TriggerCondition.SelfBecomesAttachedTo (\x -> case x of TriggerCondition.SelfBecomesAttachedTo y -> Just y; _ -> Nothing),
          Arm.payload "SelfBecomesUnattachedFrom" filterCodec TriggerCondition.SelfBecomesUnattachedFrom (\x -> case x of TriggerCondition.SelfBecomesUnattachedFrom y -> Just y; _ -> Nothing),
          -- CR 509.3d from the attacking side's bystander, SelfBecomesBlockedBy's
          -- shape above with the Filter over the attacker instead.
          Arm.payload "PermanentBecomesBlockedBy" filterCodec TriggerCondition.PermanentBecomesBlockedBy (\x -> case x of TriggerCondition.PermanentBecomesBlockedBy y -> Just y; _ -> Nothing),
          Arm.payload "PlacesSticker" PlacesSticker.codec TriggerCondition.PlacesSticker (\x -> case x of TriggerCondition.PlacesSticker y -> Just y; _ -> Nothing)
        ]

tagOf :: TriggerCondition.TriggerCondition -> String
tagOf x = case x of
  TriggerCondition.SelfEnters {} -> "SelfEnters"
  TriggerCondition.PermanentEnters {} -> "PermanentEnters"
  TriggerCondition.PermanentsEnter {} -> "PermanentsEnter"
  TriggerCondition.StepBegins {} -> "StepBegins"
  TriggerCondition.StateIs {} -> "StateIs"
  TriggerCondition.SelfDealsCombatDamageToPlayer {} -> "SelfDealsCombatDamageToPlayer"
  TriggerCondition.SelfDealsCombatDamageToPlayerOrBattle {} -> "SelfDealsCombatDamageToPlayerOrBattle"
  TriggerCondition.SelfDealsCombatDamage {} -> "SelfDealsCombatDamage"
  TriggerCondition.SelfDealsDamageToPlayer {} -> "SelfDealsDamageToPlayer"
  TriggerCondition.SelfDealsDamageToCreature {} -> "SelfDealsDamageToCreature"
  TriggerCondition.SelfDealsDamage {} -> "SelfDealsDamage"
  TriggerCondition.SelfIsDealtDamage {} -> "SelfIsDealtDamage"
  TriggerCondition.PermanentDealsCombatDamageToPlayer {} -> "PermanentDealsCombatDamageToPlayer"
  TriggerCondition.PermanentsDealCombatDamageToPlayer {} -> "PermanentsDealCombatDamageToPlayer"
  TriggerCondition.CreatureDealtCombatDamageToMonarch {} -> "CreatureDealtCombatDamageToMonarch"
  TriggerCondition.CreaturesDealtCombatDamageToInitiative {} -> "CreaturesDealtCombatDamageToInitiative"
  TriggerCondition.PlayerTookInitiative {} -> "PlayerTookInitiative"
  TriggerCondition.OpponentLostLifeDuringYourTurn {} -> "OpponentLostLifeDuringYourTurn"
  TriggerCondition.SelfAttacks {} -> "SelfAttacks"
  TriggerCondition.SelfAttacksWithAnother {} -> "SelfAttacksWithAnother"
  TriggerCondition.SelfAttacksPermanent {} -> "SelfAttacksPermanent"
  TriggerCondition.CreatureAttacksAlone {} -> "CreatureAttacksAlone"
  TriggerCondition.CreatureAttacksYou {} -> "CreatureAttacksYou"
  TriggerCondition.CreatureAttacks {} -> "CreatureAttacks"
  TriggerCondition.PlayerAttacks {} -> "PlayerAttacks"
  TriggerCondition.PlayerAttacksWith {} -> "PlayerAttacksWith"
  TriggerCondition.PlayerAttacksPlayer {} -> "PlayerAttacksPlayer"
  TriggerCondition.AttachedPlayerIsAttacked {} -> "AttachedPlayerIsAttacked"
  TriggerCondition.SelfIsAttacked {} -> "SelfIsAttacked"
  TriggerCondition.SelfAttacksPlayerWithMostLife {} -> "SelfAttacksPlayerWithMostLife"
  TriggerCondition.SelfAttacksWhileSaddled {} -> "SelfAttacksWhileSaddled"
  TriggerCondition.SelfAttacksWhile {} -> "SelfAttacksWhile"
  TriggerCondition.SelfBlocks {} -> "SelfBlocks"
  TriggerCondition.CreatureBlocks {} -> "CreatureBlocks"
  TriggerCondition.SelfBlocksCreature {} -> "SelfBlocksCreature"
  TriggerCondition.SelfBlocksAtLeast {} -> "SelfBlocksAtLeast"
  TriggerCondition.SelfBlocksOneOrMore {} -> "SelfBlocksOneOrMore"
  TriggerCondition.SelfBecomesBlocked {} -> "SelfBecomesBlocked"
  TriggerCondition.SelfBecomesBlockedBy {} -> "SelfBecomesBlockedBy"
  TriggerCondition.SelfBecomesBlockedByOneOrMore {} -> "SelfBecomesBlockedByOneOrMore"
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> "CreatureBecomesBlockedByAtLeast"
  TriggerCondition.SelfAttacksUnblocked {} -> "SelfAttacksUnblocked"
  TriggerCondition.SelfCycled {} -> "SelfCycled"
  TriggerCondition.SelfRevealedForMiracle {} -> "SelfRevealedForMiracle"
  TriggerCondition.SelfExiledForMadness {} -> "SelfExiledForMadness"
  TriggerCondition.SelfDiscarded {} -> "SelfDiscarded"
  TriggerCondition.PlayerDiscards {} -> "PlayerDiscards"
  TriggerCondition.PlayerDiscardsCards {} -> "PlayerDiscardsCards"
  TriggerCondition.PlayerCycles {} -> "PlayerCycles"
  TriggerCondition.PlayerDrawsNthCard {} -> "PlayerDrawsNthCard"
  TriggerCondition.SelfPutIntoGraveyardFromLibrary {} -> "SelfPutIntoGraveyardFromLibrary"
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere {} -> "SelfPutIntoGraveyardFromAnywhere"
  TriggerCondition.SelfPutIntoGraveyardDuringResolution {} -> "SelfPutIntoGraveyardDuringResolution"
  TriggerCondition.SelfDies {} -> "SelfDies"
  TriggerCondition.SelfLeavesGraveyard {} -> "SelfLeavesGraveyard"
  TriggerCondition.CardPutIntoGraveyard {} -> "CardPutIntoGraveyard"
  TriggerCondition.PermanentDies {} -> "PermanentDies"
  TriggerCondition.PermanentsDie {} -> "PermanentsDie"
  TriggerCondition.SelfLeavesTheBattlefield {} -> "SelfLeavesTheBattlefield"
  TriggerCondition.SelfPutFromBattlefieldInto {} -> "SelfPutFromBattlefieldInto"
  TriggerCondition.PermanentLeavesTheBattlefield {} -> "PermanentLeavesTheBattlefield"
  TriggerCondition.PermanentReturnedToHand {} -> "PermanentReturnedToHand"
  TriggerCondition.PermanentsReturnedToHand {} -> "PermanentsReturnedToHand"
  TriggerCondition.CardLeavesZone {} -> "CardLeavesZone"
  TriggerCondition.CardsLeaveZone {} -> "CardsLeaveZone"
  TriggerCondition.CardsPutIntoZone {} -> "CardsPutIntoZone"
  TriggerCondition.AttachedCreatureDies {} -> "AttachedCreatureDies"
  TriggerCondition.AttachedCreatureBecomesTapped {} -> "AttachedCreatureBecomesTapped"
  TriggerCondition.PermanentsBecomeTapped {} -> "PermanentsBecomeTapped"
  TriggerCondition.SelfBecomesUntapped {} -> "SelfBecomesUntapped"
  TriggerCondition.AttachedPermanentTappedForMana {} -> "AttachedPermanentTappedForMana"
  TriggerCondition.PermanentTappedForMana {} -> "PermanentTappedForMana"
  TriggerCondition.AbilityAddsMana {} -> "AbilityAddsMana"
  TriggerCondition.SelfManaAbilityResolves {} -> "SelfManaAbilityResolves"
  TriggerCondition.HauntedCreatureDies {} -> "HauntedCreatureDies"
  TriggerCondition.SpellOrAbilityCounters {} -> "SpellOrAbilityCounters"
  TriggerCondition.AbilityIsCountered {} -> "AbilityIsCountered"
  TriggerCondition.DamageToPlayerPrevented {} -> "DamageToPlayerPrevented"
  TriggerCondition.SelfPreventsDamage {} -> "SelfPreventsDamage"
  TriggerCondition.PlayerGainsLife {} -> "PlayerGainsLife"
  TriggerCondition.PlayersGainLife {} -> "PlayersGainLife"
  TriggerCondition.PlayerLosesLife {} -> "PlayerLosesLife"
  TriggerCondition.SelfCountersReached {} -> "SelfCountersReached"
  TriggerCondition.SelfBecomesClassLevel {} -> "SelfBecomesClassLevel"
  TriggerCondition.SelfLastCounterRemoved {} -> "SelfLastCounterRemoved"
  TriggerCondition.SelfCountersRemoved {} -> "SelfCountersRemoved"
  TriggerCondition.SelfCounterRemoved {} -> "SelfCounterRemoved"
  TriggerCondition.PermanentsGetCounters {} -> "PermanentsGetCounters"
  TriggerCondition.PermanentGetsCounters {} -> "PermanentGetsCounters"
  TriggerCondition.SpellCast {} -> "SpellCast"
  TriggerCondition.SelfCast {} -> "SelfCast"
  TriggerCondition.SelfBecomesTargeted {} -> "SelfBecomesTargeted"
  TriggerCondition.ControllerBecomesTarget {} -> "ControllerBecomesTarget"
  TriggerCondition.PermanentsBecomeTargeted {} -> "PermanentsBecomeTargeted"
  TriggerCondition.PermanentBecomesTargeted {} -> "PermanentBecomesTargeted"
  TriggerCondition.SelfHalfUnlocked {} -> "SelfHalfUnlocked"
  TriggerCondition.RoomFullyUnlocked {} -> "RoomFullyUnlocked"
  TriggerCondition.AnyOf {} -> "AnyOf"
  TriggerCondition.SelfTurnedFaceUp {} -> "SelfTurnedFaceUp"
  TriggerCondition.SelfTransformedInto {} -> "SelfTransformedInto"
  TriggerCondition.PermanentTransforms {} -> "PermanentTransforms"
  TriggerCondition.PermanentTurnedFaceUp {} -> "PermanentTurnedFaceUp"
  TriggerCondition.PermanentActs {} -> "PermanentActs"
  TriggerCondition.FaceDownPermanentLeavesRevealed {} -> "FaceDownPermanentLeavesRevealed"
  TriggerCondition.PermanentBecomesDesignated {} -> "PermanentBecomesDesignated"
  TriggerCondition.AttachedCreatureMentors {} -> "AttachedCreatureMentors"
  TriggerCondition.SelfExploits {} -> "SelfExploits"
  TriggerCondition.CreatureExploits {} -> "CreatureExploits"
  TriggerCondition.SelfBecomesCrewed {} -> "SelfBecomesCrewed"
  TriggerCondition.SelfCrewsVehicle {} -> "SelfCrewsVehicle"
  TriggerCondition.PermanentSacrificed {} -> "PermanentSacrificed"
  TriggerCondition.SagaFinalChapterTriggers {} -> "SagaFinalChapterTriggers"
  TriggerCondition.PlayerBecomesMonarch {} -> "PlayerBecomesMonarch"
  TriggerCondition.LoseControlOfBound {} -> "LoseControlOfBound"
  TriggerCondition.BoundDiesOrIsExiled {} -> "BoundDiesOrIsExiled"
  TriggerCondition.BoundDies {} -> "BoundDies"
  TriggerCondition.RoomEntered {} -> "RoomEntered"
  TriggerCondition.PlayerActs {} -> "PlayerActs"
  TriggerCondition.PlayerLosesGame {} -> "PlayerLosesGame"
  TriggerCondition.PlayerPlaysLand {} -> "PlayerPlaysLand"
  TriggerCondition.PlayerManifestsDread {} -> "PlayerManifestsDread"
  TriggerCondition.PlayerRollsResult {} -> "PlayerRollsResult"
  TriggerCondition.Visit {} -> "Visit"
  TriggerCondition.ChaosEnsues {} -> "ChaosEnsues"
  TriggerCondition.PlayerRollsPlaneswalker {} -> "PlayerRollsPlaneswalker"
  TriggerCondition.SetInMotion {} -> "SetInMotion"
  TriggerCondition.PlayerWinsCoinFlip {} -> "PlayerWinsCoinFlip"
  TriggerCondition.PlayerLosesCoinFlip {} -> "PlayerLosesCoinFlip"
  TriggerCondition.SelfBecomesPlotted {} -> "SelfBecomesPlotted"
  TriggerCondition.Reflexive {} -> "Reflexive"
  TriggerCondition.SelfBecomesAttachedBy {} -> "SelfBecomesAttachedBy"
  TriggerCondition.SelfBecomesAttachedTo {} -> "SelfBecomesAttachedTo"
  TriggerCondition.SelfBecomesUnattachedFrom {} -> "SelfBecomesUnattachedFrom"
  TriggerCondition.PermanentBecomesBlockedBy {} -> "PermanentBecomesBlockedBy"
  TriggerCondition.PlacesSticker {} -> "PlacesSticker"
