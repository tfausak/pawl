{-# LANGUAGE GADTs #-}

-- Two readings of a prompt a scenario needs and the engine does not supply:
-- which seat answers it, and what it is called in a failure message.
module Pawl.Scenario.Prompt where

import qualified Data.Text as Text
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt

-- WHO ANSWERS a prompt: the Decider it carries, which CR 723.5 makes the seat
-- that makes every choice a controlled player would make. Not the PlayerId the
-- prompt is about; the two differ only while CR 723.1's effect is running.
-- Prompt.Concede is the exception CR 723.6 names and answers with its own seat,
-- and the random and shuffle prompts answer Nothing -- randomness is not a
-- choice, so no seat makes it.
--
-- Total on purpose: a hand-kept catalog over a GADT this size drifts silently,
-- and the wildcard that would let it drift is what -Wincomplete-patterns is here
-- to refuse.
deciderOf :: Prompt.Prompt r -> Maybe PlayerId.PlayerId
deciderOf prompt = case prompt of
  Prompt.ChooseAction decider _ _ -> Just (Decider.unwrap decider)
  Prompt.Concede pid -> Just pid
  Prompt.Shuffle {} -> Nothing
  Prompt.RandomFirstPlayer {} -> Nothing
  Prompt.RandomObject {} -> Nothing
  Prompt.RandomPlayer {} -> Nothing
  Prompt.RandomCard {} -> Nothing
  Prompt.ChooseConjuredCard decider _ _ -> Just (Decider.unwrap decider)
  Prompt.RollDie {} -> Nothing
  Prompt.RandomDepth {} -> Nothing
  Prompt.LookUpCard {} -> Nothing
  Prompt.ReferenceCards {} -> Nothing
  Prompt.ReferenceNames {} -> Nothing
  Prompt.ChooseDieResult decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.FlipCoin {} -> Nothing
  Prompt.CallCoin decider _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCoinResult decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDiscard decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseScry decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSurveil decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseFateseal decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExplore decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.RerollDie decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AdjustDieRoll decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRollModifier decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDefender decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseManaSource decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExtraManaSource decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ReverseManaAbilities decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseManaYield decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseManaToSpend decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseProliferate decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRedistribution decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRingBearer decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseBolster decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAmass decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseBlight decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseBehold decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCounterRemoval decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCounterRemovalAmong decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCounterRemovalAtLeast decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCounterRemovalUpTo decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMixedCounterRemoval decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseVote decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseVoteWord decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMovedCounter decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMovedCounters decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMovedCountersAtLeastOne decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDistributedMovedCounters decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMovedCounterOrNone decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePaidEnergy decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseNumber decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseReadAheadChapter decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDamageSource decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDelayedTriggerEvent decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardInGraveyard decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardInHand decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardFromAmong decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardsFromAmong decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDungeon decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCompanion decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseFromOutsideTheGame decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRoom decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseHalf decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLegend decider _ _ -> Just (Decider.unwrap decider)
  Prompt.DeclareAttackers decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAttackTarget decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExert decider _ _ -> Just (Decider.unwrap decider)
  Prompt.DeclareBlockers decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AssignCombatDamage decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTargets decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AnnounceTargets decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLandTypeSwap decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCreatureTypeSwap decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseBasicLandType decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCreatureType decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSearchZones decider _ _ -> Just (Decider.unwrap decider)
  Prompt.Search decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.CastWhileSearching decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseX decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMutateSide decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseForage decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLearn decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTimeTravel decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseClash decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseEntwine decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseBuyback decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSplice decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAssistant decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAssistAmount decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseKicker decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ReturnCommander decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCommandZoneOfferFirst decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLibraryEnd decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ArrangeLibraryArrivals decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ArrangeLibraryCards decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ArrangeGraveyardArrivals decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseModes decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCopyTarget decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseEntryOption decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRiot decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseUnleash decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTribute decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDredge decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRedirect decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePayLifeOnEntry decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRevealOnEntry decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseEnlist decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseEncode decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseColor decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseManaType decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardName decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseOpponent decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseProtector decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePlayer decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseActivePlayer decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderTriggers decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderDamage decider _ _ -> Just (Decider.unwrap decider)
  Prompt.AllocateDamage decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseReplacement decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSacrifices decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExilesFromGraveyard decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMaterials decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCollectEvidence decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAnyNumberToSacrifice decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAnyNumberToReveal decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAnyNumberOfPermanents decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAnyNumberToDiscard decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePermanent decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTapsForTotalPower decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTaps decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseReturns decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAttachment decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTurnUpAttachment decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCost decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePlayPermission decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderCostComponents decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderCombatTolls decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderComponentCards decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderForEach decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLoopMembers decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRepeat decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderTimestamps decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderManaActivations decider _ _ -> Just (Decider.unwrap decider)
  Prompt.DeclareMulligan decider _ _ -> Just (Decider.unwrap decider)
  Prompt.Bottom decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.MulliganAction decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OpeningHandAction decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseOptional decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseClause decider _ _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OfferedCast decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseOfferedCastSpell decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OfferedMiracleReveal decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseToPay decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AnnouncePhyrexianPayment decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AnnounceHybridPayment decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AnnounceHybridHalf decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseReductionHalf decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseReducedCost decider _ _ _ -> Just (Decider.unwrap decider)

-- The prompt's constructor name, for failure messages. Total for deciderOf's
-- reason.
kindOf :: Prompt.Prompt r -> Text.Text
kindOf prompt = Text.pack $ case prompt of
  Prompt.ChooseAction {} -> "ChooseAction"
  Prompt.Concede {} -> "Concede"
  Prompt.Shuffle {} -> "Shuffle"
  Prompt.RandomFirstPlayer {} -> "RandomFirstPlayer"
  Prompt.RandomObject {} -> "RandomObject"
  Prompt.RandomPlayer {} -> "RandomPlayer"
  Prompt.RandomCard {} -> "RandomCard"
  Prompt.ChooseConjuredCard {} -> "ChooseConjuredCard"
  Prompt.RollDie {} -> "RollDie"
  Prompt.RandomDepth {} -> "RandomDepth"
  Prompt.LookUpCard {} -> "LookUpCard"
  Prompt.ReferenceCards {} -> "ReferenceCards"
  Prompt.ReferenceNames {} -> "ReferenceNames"
  Prompt.ChooseDieResult {} -> "ChooseDieResult"
  Prompt.FlipCoin {} -> "FlipCoin"
  Prompt.CallCoin {} -> "CallCoin"
  Prompt.ChooseCoinResult {} -> "ChooseCoinResult"
  Prompt.ChooseDiscard {} -> "ChooseDiscard"
  Prompt.ChooseScry {} -> "ChooseScry"
  Prompt.ChooseSurveil {} -> "ChooseSurveil"
  Prompt.ChooseFateseal {} -> "ChooseFateseal"
  Prompt.ChooseExplore {} -> "ChooseExplore"
  Prompt.RerollDie {} -> "RerollDie"
  Prompt.AdjustDieRoll {} -> "AdjustDieRoll"
  Prompt.ChooseRollModifier {} -> "ChooseRollModifier"
  Prompt.ChooseDefender {} -> "ChooseDefender"
  Prompt.ChooseManaSource {} -> "ChooseManaSource"
  Prompt.ChooseExtraManaSource {} -> "ChooseExtraManaSource"
  Prompt.ReverseManaAbilities {} -> "ReverseManaAbilities"
  Prompt.ChooseManaYield {} -> "ChooseManaYield"
  Prompt.ChooseManaToSpend {} -> "ChooseManaToSpend"
  Prompt.ChooseProliferate {} -> "ChooseProliferate"
  Prompt.ChooseRedistribution {} -> "ChooseRedistribution"
  Prompt.ChooseRingBearer {} -> "ChooseRingBearer"
  Prompt.ChooseBolster {} -> "ChooseBolster"
  Prompt.ChooseAmass {} -> "ChooseAmass"
  Prompt.ChooseBlight {} -> "ChooseBlight"
  Prompt.ChooseBehold {} -> "ChooseBehold"
  Prompt.ChooseCounterRemoval {} -> "ChooseCounterRemoval"
  Prompt.ChooseCounterRemovalAmong {} -> "ChooseCounterRemovalAmong"
  Prompt.ChooseCounterRemovalAtLeast {} -> "ChooseCounterRemovalAtLeast"
  Prompt.ChooseCounterRemovalUpTo {} -> "ChooseCounterRemovalUpTo"
  Prompt.ChooseMixedCounterRemoval {} -> "ChooseMixedCounterRemoval"
  Prompt.ChooseVote {} -> "ChooseVote"
  Prompt.ChooseVoteWord {} -> "ChooseVoteWord"
  Prompt.ChooseMovedCounter {} -> "ChooseMovedCounter"
  Prompt.ChooseMovedCounters {} -> "ChooseMovedCounters"
  Prompt.ChooseMovedCountersAtLeastOne {} -> "ChooseMovedCountersAtLeastOne"
  Prompt.ChooseDistributedMovedCounters {} -> "ChooseDistributedMovedCounters"
  Prompt.ChooseMovedCounterOrNone {} -> "ChooseMovedCounterOrNone"
  Prompt.ChoosePaidEnergy {} -> "ChoosePaidEnergy"
  Prompt.ChooseNumber {} -> "ChooseNumber"
  Prompt.ChooseReadAheadChapter {} -> "ChooseReadAheadChapter"
  Prompt.ChooseDamageSource {} -> "ChooseDamageSource"
  Prompt.ChooseDelayedTriggerEvent {} -> "ChooseDelayedTriggerEvent"
  Prompt.ChooseCardInGraveyard {} -> "ChooseCardInGraveyard"
  Prompt.ChooseCardInHand {} -> "ChooseCardInHand"
  Prompt.ChooseCardFromAmong {} -> "ChooseCardFromAmong"
  Prompt.ChooseCardsFromAmong {} -> "ChooseCardsFromAmong"
  Prompt.ChooseDungeon {} -> "ChooseDungeon"
  Prompt.ChooseCompanion {} -> "ChooseCompanion"
  Prompt.ChooseFromOutsideTheGame {} -> "ChooseFromOutsideTheGame"
  Prompt.ChooseRoom {} -> "ChooseRoom"
  Prompt.ChooseHalf {} -> "ChooseHalf"
  Prompt.ChooseLegend {} -> "ChooseLegend"
  Prompt.DeclareAttackers {} -> "DeclareAttackers"
  Prompt.ChooseAttackTarget {} -> "ChooseAttackTarget"
  Prompt.ChooseExert {} -> "ChooseExert"
  Prompt.DeclareBlockers {} -> "DeclareBlockers"
  Prompt.AssignCombatDamage {} -> "AssignCombatDamage"
  Prompt.ChooseTargets {} -> "ChooseTargets"
  Prompt.AnnounceTargets {} -> "AnnounceTargets"
  Prompt.ChooseLandTypeSwap {} -> "ChooseLandTypeSwap"
  Prompt.ChooseCreatureTypeSwap {} -> "ChooseCreatureTypeSwap"
  Prompt.ChooseBasicLandType {} -> "ChooseBasicLandType"
  Prompt.ChooseCreatureType {} -> "ChooseCreatureType"
  Prompt.ChooseSearchZones {} -> "ChooseSearchZones"
  Prompt.Search {} -> "Search"
  Prompt.CastWhileSearching {} -> "CastWhileSearching"
  Prompt.ChooseX {} -> "ChooseX"
  Prompt.ChooseMutateSide {} -> "ChooseMutateSide"
  Prompt.ChooseForage {} -> "ChooseForage"
  Prompt.ChooseLearn {} -> "ChooseLearn"
  Prompt.ChooseTimeTravel {} -> "ChooseTimeTravel"
  Prompt.ChooseClash {} -> "ChooseClash"
  Prompt.ChooseEntwine {} -> "ChooseEntwine"
  Prompt.ChooseBuyback {} -> "ChooseBuyback"
  Prompt.ChooseSplice {} -> "ChooseSplice"
  Prompt.ChooseAssistant {} -> "ChooseAssistant"
  Prompt.ChooseAssistAmount {} -> "ChooseAssistAmount"
  Prompt.ChooseKicker {} -> "ChooseKicker"
  Prompt.ReturnCommander {} -> "ReturnCommander"
  Prompt.ChooseCommandZoneOfferFirst {} -> "ChooseCommandZoneOfferFirst"
  Prompt.ChooseLibraryEnd {} -> "ChooseLibraryEnd"
  Prompt.ArrangeLibraryArrivals {} -> "ArrangeLibraryArrivals"
  Prompt.ArrangeLibraryCards {} -> "ArrangeLibraryCards"
  Prompt.ArrangeGraveyardArrivals {} -> "ArrangeGraveyardArrivals"
  Prompt.ChooseModes {} -> "ChooseModes"
  Prompt.ChooseCopyTarget {} -> "ChooseCopyTarget"
  Prompt.ChooseEntryOption {} -> "ChooseEntryOption"
  Prompt.ChooseRiot {} -> "ChooseRiot"
  Prompt.ChooseUnleash {} -> "ChooseUnleash"
  Prompt.ChooseTribute {} -> "ChooseTribute"
  Prompt.ChooseDredge {} -> "ChooseDredge"
  Prompt.ChooseRedirect {} -> "ChooseRedirect"
  Prompt.ChoosePayLifeOnEntry {} -> "ChoosePayLifeOnEntry"
  Prompt.ChooseRevealOnEntry {} -> "ChooseRevealOnEntry"
  Prompt.ChooseEnlist {} -> "ChooseEnlist"
  Prompt.ChooseEncode {} -> "ChooseEncode"
  Prompt.ChooseColor {} -> "ChooseColor"
  Prompt.ChooseManaType {} -> "ChooseManaType"
  Prompt.ChooseCardName {} -> "ChooseCardName"
  Prompt.ChooseOpponent {} -> "ChooseOpponent"
  Prompt.ChooseProtector {} -> "ChooseProtector"
  Prompt.ChoosePlayer {} -> "ChoosePlayer"
  Prompt.ChooseActivePlayer {} -> "ChooseActivePlayer"
  Prompt.OrderTriggers {} -> "OrderTriggers"
  Prompt.OrderDamage {} -> "OrderDamage"
  Prompt.AllocateDamage {} -> "AllocateDamage"
  Prompt.ChooseReplacement {} -> "ChooseReplacement"
  Prompt.ChooseSacrifices {} -> "ChooseSacrifices"
  Prompt.ChooseExilesFromGraveyard {} -> "ChooseExilesFromGraveyard"
  Prompt.ChooseMaterials {} -> "ChooseMaterials"
  Prompt.ChooseCollectEvidence {} -> "ChooseCollectEvidence"
  Prompt.ChooseAnyNumberToSacrifice {} -> "ChooseAnyNumberToSacrifice"
  Prompt.ChooseAnyNumberToReveal {} -> "ChooseAnyNumberToReveal"
  Prompt.ChooseAnyNumberOfPermanents {} -> "ChooseAnyNumberOfPermanents"
  Prompt.ChooseAnyNumberToDiscard {} -> "ChooseAnyNumberToDiscard"
  Prompt.ChoosePermanent {} -> "ChoosePermanent"
  Prompt.ChooseTapsForTotalPower {} -> "ChooseTapsForTotalPower"
  Prompt.ChooseTaps {} -> "ChooseTaps"
  Prompt.ChooseReturns {} -> "ChooseReturns"
  Prompt.ChooseAttachment {} -> "ChooseAttachment"
  Prompt.ChooseTurnUpAttachment {} -> "ChooseTurnUpAttachment"
  Prompt.ChooseCost {} -> "ChooseCost"
  Prompt.ChoosePlayPermission {} -> "ChoosePlayPermission"
  Prompt.OrderCostComponents {} -> "OrderCostComponents"
  Prompt.OrderCombatTolls {} -> "OrderCombatTolls"
  Prompt.OrderComponentCards {} -> "OrderComponentCards"
  Prompt.OrderForEach {} -> "OrderForEach"
  Prompt.ChooseLoopMembers {} -> "ChooseLoopMembers"
  Prompt.ChooseRepeat {} -> "ChooseRepeat"
  Prompt.OrderTimestamps {} -> "OrderTimestamps"
  Prompt.OrderManaActivations {} -> "OrderManaActivations"
  Prompt.DeclareMulligan {} -> "DeclareMulligan"
  Prompt.Bottom {} -> "Bottom"
  Prompt.MulliganAction {} -> "MulliganAction"
  Prompt.OpeningHandAction {} -> "OpeningHandAction"
  Prompt.ChooseOptional {} -> "ChooseOptional"
  Prompt.ChooseClause {} -> "ChooseClause"
  Prompt.OfferedCast {} -> "OfferedCast"
  Prompt.ChooseOfferedCastSpell {} -> "ChooseOfferedCastSpell"
  Prompt.OfferedMiracleReveal {} -> "OfferedMiracleReveal"
  Prompt.ChooseToPay {} -> "ChooseToPay"
  Prompt.AnnouncePhyrexianPayment {} -> "AnnouncePhyrexianPayment"
  Prompt.AnnounceHybridPayment {} -> "AnnounceHybridPayment"
  Prompt.AnnounceHybridHalf {} -> "AnnounceHybridHalf"
  Prompt.ChooseReductionHalf {} -> "ChooseReductionHalf"
  Prompt.ChooseReducedCost {} -> "ChooseReducedCost"
