-- CR 605.1a's classification, at both the scales the rule needs it:
-- `manaProduced` asks one EFFECT whether it could add mana, `movesLibraryCard`
-- asks one EFFECT whether it moves a card to or from a library and
-- `costMovesLibraryCard` asks the same of one COST COMPONENT, and
-- `isManaAbility` folds all three over a whole ABILITY and adds the no-target
-- and not-a-loyalty-ability clauses.
--
-- The ability-level half lives here rather than in Pawl.Engine.Mana because
-- Pawl.Engine.Projection needs it -- CR 605.1a's exclusion is half of
-- Filter.HasNonManaActivatedAbility, and Mana imports Projection, so the
-- classification had to sit below both.
--
-- Here rather than in Pawl.Engine.Resolve so that Pawl.Engine.Mana need not
-- import the resolver: Resolve is a high-level module, so Mana -> Resolve ->
-- Combat made a mana payment from inside combat (CR 508.1j) a module cycle.
--
-- Casing on Effect here is not a breach of design.md section 1: the closed half
-- may depend on a CLASSIFICATION of effects, and this function is one. What
-- stays forbidden is a case that acts on WHICH effect it is, and every arm
-- below answers the one question in the type. `costMovesLibraryCard` cases on
-- Pawl.Types.CostComponent for exactly the same reason, and lives HERE rather
-- than beside its siblings in Pawl.Engine.Cost because that module cannot be
-- reached from this one: Cost imports Mana imports Projection imports this.
module Pawl.Engine.ManaAbility where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.DurationRef as DurationRef
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- CR 605.1a: an activated ability is a mana ability if it could add mana AND
-- doesn't target AND moves no card to or from a library AND is not itself a
-- loyalty ability (CR 606.2, which Pawl.Engine.Cost.isLoyaltyCost answers; no
-- loyalty ability in the pool adds mana, so the clause is inert rather than
-- checked here). Read at three sites:
-- Mana.manaRoutesOfGiven includes a mana ability as a source,
-- Activate.activatableGiven refuses to put one on the stack (CR 605.3b), and
-- Projection's view builders answer Filter.HasNonManaActivatedAbility with its
-- negation. What Action.legalActions offers instead is
-- Action.ActivateManaAbility, one per Mana.manaSourcesGiven, which is CR 605.3a's
-- priority window.
--
-- Asked of the WHOLE ability, across every mode -- CR 605.1a's "could add mana"
-- is satisfied by any mode that does, and CR 605.2 keeps it a mana ability even
-- where the game state stops it producing.
--
-- DECLARING a slot is what disqualifies it, not filling one, and CR 605.1a's own
-- "(see rule 115.6)" is why: an ability whose slot may be left empty is "still
-- said to require targets", so a CR 115.6 slot keeps it off this list too.
--
-- The library clause reads BOTH halves of "its cost and effect don't move any
-- card to or from a library" -- `movesLibraryCard` over the effects and
-- `costMovesLibraryCard` over the activation cost's components. Millikin's
-- "{T}, Mill a card: Add {C}" is disqualified by its cost alone, which is what
-- its own reminder text ("Activate only as an instant") records.
--
-- ACTIVATED abilities only, which is CR 605.1a's own scope; `isTriggeredManaAbility`
-- below is CR 605.1b's, and the two rules share no criterion but the no-target one.
--
-- CR 605.1a's closing sentence -- do not take replacement effects other than
-- self-replacement effects into account -- holds by construction rather than by
-- a guard. Every clause here reads an ActivatedAbility's PRINTED effects out of
-- the card, so no replacement effect is in scope to take into account, and the
-- self-replacement ones the rule does admit are written into those effects.
isManaAbility :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
isManaAbility ab =
  not (null (Maybe.mapMaybe manaProduced effects))
    && Map.null (Modal.allTargetSlots (ActivatedAbility.modal ab))
    && not (any movesLibraryCard effects)
    && not (any costMovesLibraryCard (Cost.components (ActivatedAbility.cost ab)))
  where
    effects = Modal.allEffects (ActivatedAbility.modal ab)

-- CR 605.1b: a TRIGGERED ability is a mana ability if it doesn't require a
-- target, triggers from the activation or resolution of an activated mana
-- ability or from mana being added to a player's mana pool, and could add mana
-- when it resolves. `isManaAbility` above is the rule's other half.
--
-- Read at two sites, which are the two halves of CR 605.4a: Pawl.Engine.Cost.tapForManaWith
-- applies one inline as the activation that triggered it finishes, and
-- Pawl.Engine.Engine.placePendingTriggers refuses to put one on the stack.
--
-- No library clause and no loyalty clause: CR 605.1b states neither, where CR
-- 605.1a states both. A triggered ability that mills and adds mana is a mana
-- ability by this rule, and answering otherwise would keep it off the stack of
-- neither road.
--
-- Asked of the WHOLE ability across every mode, `isManaAbility`'s reading of
-- "could add mana", and CR 605.2 keeps it a mana ability where the game state
-- stops it producing.
isTriggeredManaAbility :: TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
isTriggeredManaAbility ab =
  triggersFromMana (TriggeredAbility.condition ab)
    && not (null (Maybe.mapMaybe manaProduced effects))
    && Map.null (Modal.allTargetSlots (TriggeredAbility.modal ab))
  where
    effects = Modal.allEffects (TriggeredAbility.modal ab)

-- CR 605.1b's middle clause, asked of one condition: does it trigger from the
-- activation or resolution of an activated mana ability, or from mana being
-- added to a player's mana pool?
--
-- A CLASSIFICATION of a trigger condition and not a case on an effect's
-- identity: the question is the one CR 605.1b asks, and every arm answers it.
-- The engine already cases on this type for CR 603.8, CR 603.10a and CR 113.6
-- (Pawl.Engine.Event.Trigger's looksBack, batchScoped and zonesTriggeredFrom).
--
-- A WILDCARD and not the exhaustive posture `costMovesLibraryCard` takes,
-- Pawl.Engine.Event.Trigger.enchantedObjectLeaves' shape and its reason: CR
-- 605.1b names three events out of the whole trigger vocabulary, and every other
-- condition in it watches something that is not mana. -Werror will therefore not
-- name this site when a mana-watching condition is added, and each such
-- condition owes an arm here -- mana being added and a mana ability being
-- activated are the two #1572 still holds open.
--
-- CR 603.1b's AnyOf falls to the wildcard, which answers False for a disjunction
-- one of whose disjuncts watches mana. No card prints one, and the rule would
-- want it a mana ability only if EVERY disjunct met CR 605.1b (#1572).
triggersFromMana :: TriggerCondition.TriggerCondition -> Bool
triggersFromMana condition = case condition of
  -- CR 106.12 makes "tapped for mana" the resolution of an activated mana
  -- ability, which is CR 605.1b's first alternative in as many words.
  TriggerCondition.AttachedPermanentTappedForMana -> True
  -- The same resolution watched by a bystander rather than through an
  -- attachment link, so the same alternative and the same answer.
  TriggerCondition.PermanentTappedForMana {} -> True
  _ -> False

-- CR 605.1a's library clause read of ONE cost component: does paying it move a
-- card to or from a library? `movesLibraryCard`'s cost-side twin, and the two
-- must not drift -- that one already answers True of Effect.Mill.
--
-- NOT CR 601.2h's question, which Pawl.Engine.Cost.paidInSecondPass asks: that
-- rule is about a card moving from a library to a PUBLIC zone, where this one is
-- about a library at either end and says nothing about where the card goes.
--
-- The MANA part is not asked, and needs no arm: CR 202.1's symbols spend a mana
-- pool, and no card moves.
--
-- EXHAUSTIVE with no wildcard, the posture every case over this type takes: a
-- new component owes an answer here, and -Werror is what makes it.
costMovesLibraryCard :: CostComponent.CostComponent Keyword.Keyword -> Bool
costMovesLibraryCard component = case component of
  -- CR 701.17a moves the cards out of the paying player's library. The one True
  -- arm in the vocabulary, and the whole reason this function exists.
  CostComponent.MillCards _ -> True
  -- A hand, a graveyard, the battlefield and the stack -- CR 605.1a names a
  -- LIBRARY and none of these is one.
  CostComponent.DiscardCards {} -> False
  CostComponent.DiscardThis _ -> False
  CostComponent.PutCardFromHandOntoBattlefield _ -> False
  CostComponent.SacrificeThis -> False
  CostComponent.Sacrifice {} -> False
  CostComponent.ReturnThis -> False
  CostComponent.ReturnPermanents {} -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileThis -> False
  CostComponent.ExileCardsFromGraveyard {} -> False
  CostComponent.ExileTopFromGraveyard _ -> False
  -- These move no card at all.
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.TapForTotalPower {} -> False
  CostComponent.TapPermanents {} -> False
  CostComponent.PayLife _ -> False
  CostComponent.PayLifeX -> False
  CostComponent.PayEnergyX -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  CostComponent.RemovePlusOneCountersFromThis _ -> False
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.BlightX -> False

-- CR 605: does this effect add mana, and on what instruction? Read by
-- Mana.isManaAbility to keep mana abilities off the stack, and by
-- Mana.manaRoutesOfGiven to enumerate what one activation would add.
--
-- Returns the WHOLE ManaAddition rather than a settled ManaType because CR 605.1a
-- asks whether the ability COULD add mana, which an unresolved colour choice
-- answers yes to; which colour is Cost.tapForMana's prompt, not a static fact.
--
-- The whole payload and not its ManaProduction alone, because the two readers
-- want different halves of it: the classification below asks only whether there
-- is an addition at all, and CR 605.3b's inline payment
-- (Mana.manaOptionsOfGiven) has to stamp what the instruction says onto every
-- unit it adds -- CR 106.6's spending restriction and CR 106.4's retention
-- today, the recipient (#1673) when that lands.
--
-- None of those three narrows the CLASSIFICATION, and CR 605.1a is why: its four
-- criteria say nothing about whose pool the mana goes to ("a player's", not its
-- controller's), how long it lasts, or what it may pay for, so an addition
-- naming any of them is a mana ability just the same.
manaProduced :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Maybe ManaAddition.ManaAddition
manaProduced effect = case effect of
  Effect.AddMana addition -> Just addition
  Effect.DealDamage (DealDamage.MkDealDamage {}) -> Nothing
  Effect.Fight {} -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText {} -> Nothing
  Effect.Search {} -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.Proliferate -> Nothing
  Effect.ChooseCardName _ -> Nothing
  Effect.FromOutsideTheGame _ -> Nothing
  Effect.ExileThisSpell -> Nothing
  Effect.Bolster _ -> Nothing
  Effect.Amass _ -> Nothing
  Effect.Blight _ -> Nothing
  Effect.Earthbend _ -> Nothing
  Effect.TemptWithTheRing -> Nothing
  Effect.Venture {} -> Nothing
  Effect.ExileHandThenDraw -> Nothing
  Effect.PlayerSacrifices {} -> Nothing
  Effect.RestartGame _ -> Nothing
  Effect.ControlPlayerNextTurn _ -> Nothing
  Effect.Destroy {} -> Nothing
  Effect.Sacrifice _ -> Nothing
  Effect.TurnFaceDown _ -> Nothing
  Effect.TurnFaceUp _ -> Nothing
  Effect.RemoveFromCombat _ -> Nothing
  Effect.BecomesBlocked _ -> Nothing
  Effect.MoveToZone {} -> Nothing
  Effect.Draw {} -> Nothing
  Effect.Mill {} -> Nothing
  Effect.Reveal {} -> Nothing
  Effect.LookAt {} -> Nothing
  Effect.Scry {} -> Nothing
  Effect.Surveil {} -> Nothing
  Effect.Fateseal {} -> Nothing
  Effect.Explore {} -> Nothing
  Effect.Discard {} -> Nothing
  Effect.LoseLife {} -> Nothing
  Effect.GainLife {} -> Nothing
  Effect.ExchangeLifeTotals _ -> Nothing
  Effect.SetLifeTotal {} -> Nothing
  Effect.RedistributeLifeTotals -> Nothing
  Effect.IncreaseSpeed {} -> Nothing
  Effect.DecreaseSpeed {} -> Nothing
  Effect.Create {} -> Nothing
  Effect.Conjure {} -> Nothing
  Effect.CreateCopy {} -> Nothing
  Effect.BecomeCopy {} -> Nothing
  Effect.CopyStackObject {} -> Nothing
  Effect.Replace {} -> Nothing
  Effect.SkipNextPhase {} -> Nothing
  -- CR 615.5's rider is not descended into, the same stop Effect.Create makes at
  -- a minted token's abilities: this asks what the effect ITSELF adds, and a
  -- prevention adds no mana. A rider that did would be a mana clause nothing in
  -- the pool prints, and CR 605.1a would then want it seen here.
  Effect.PreventNextDamage {} -> Nothing
  Effect.PreventAllDamage {} -> Nothing
  Effect.RedirectDamage {} -> Nothing
  Effect.Counter {} -> Nothing
  Effect.PutCounters {} -> Nothing
  Effect.PutCountersFrom {} -> Nothing
  Effect.RemoveCounters {} -> Nothing
  Effect.MoveCounters {} -> Nothing
  Effect.GainPlayerCounters {} -> Nothing
  Effect.RemovePlayerCounters {} -> Nothing
  Effect.PayAnyEnergy _ -> Nothing
  Effect.Tap _ -> Nothing
  Effect.Untap _ -> Nothing
  Effect.Detain _ -> Nothing
  Effect.Goad _ -> Nothing
  Effect.MakePlotted _ -> Nothing
  Effect.DoesNotUntapNext _ -> Nothing
  Effect.Transform _ -> Nothing
  Effect.Convert _ -> Nothing
  -- CR 701.42a puts cards onto the battlefield; it adds no mana.
  Effect.Meld {} -> Nothing
  Effect.PhaseOut _ -> Nothing
  Effect.AddPhases _ -> Nothing
  Effect.EndTurn -> Nothing
  Effect.EndCombatPhase -> Nothing
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> Nothing
  Effect.ArmDelayedTrigger {} -> Nothing
  Effect.AffectPlayers {} -> Nothing
  Effect.RequireBlock {} -> Nothing
  Effect.CantBeRegenerated {} -> Nothing
  Effect.ForbidBlock {} -> Nothing
  Effect.ForbidAttack {} -> Nothing
  Effect.ForbidActivation {} -> Nothing
  Effect.RequireAttack {} -> Nothing
  Effect.CreateEmblem {} -> Nothing
  Effect.BecomeMonarch {} -> Nothing
  Effect.TakeTheInitiative {} -> Nothing
  Effect.Designate (Designate.MkDesignate _ _) -> Nothing
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> Nothing
  Effect.Unsuspect _ -> Nothing
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> Nothing
  Effect.Evolve _ -> Nothing
  Effect.Mentor _ -> Nothing
  Effect.Train _ -> Nothing
  Effect.ItBecomes _ -> Nothing
  Effect.ExileUntilMonarch _ -> Nothing
  Effect.ExileHaunting {} -> Nothing
  Effect.Attach _ -> Nothing
  Effect.AttachTarget {} -> Nothing
  Effect.AttachTargetToEach {} -> Nothing
  Effect.AttachBound {} -> Nothing
  Effect.PlaySubgame _ -> Nothing
  Effect.ChoosePlayer _ -> Nothing
  Effect.ChooseOpponentAtRandom _ -> Nothing
  Effect.RollDie {} -> Nothing
  Effect.FlipCoin {} -> Nothing
  Effect.TakeExtraTurn {} -> Nothing
  Effect.ShuffleIntoLibrary {} -> Nothing
  Effect.Shuffle {} -> Nothing
  Effect.OfferCast {} -> Nothing
  Effect.GrantPlayFromExile {} -> Nothing
  -- Descended into, unlike CR 615.5's rider above: rule 608.2f's body runs as
  -- part of THIS effect, so an AddMana in it would be mana this ability adds.
  -- No card in the pool writes one, and CR 605.1a would want it seen if one did.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> Maybe.listToMaybe (Maybe.mapMaybe manaProduced (Foldable.toList body))

-- CR 605.1a's fourth clause, asked of one effect: does it move a card to or from
-- a library? The 2026-08-07 update added the clause, and `isManaAbility` folds
-- this over an ability's effects to answer it.
--
-- WITHIN a library is neither "to" nor "from" one, which is why Scry and
-- Fateseal answer False: CR 701.22 and CR 701.29 reorder cards that never leave
-- the zone they are in. Surveil and Explore answer True because both have a
-- branch that takes the card OUT (CR 701.25a, CR 701.44a), and CR 605.1a's test is
-- what the effect may do rather than what it did on one resolution -- the same
-- reading `manaProduced` gives "could add mana".
movesLibraryCard :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
movesLibraryCard effect = case effect of
  -- CR 121.1: a draw takes the top card of a library into a hand.
  Effect.Draw {} -> True
  -- CR 701.17a: milling puts cards from the top of a library into a graveyard.
  Effect.Mill {} -> True
  -- CR 701.20a's reveal moves nothing (CR 701.20b) -- the look below without
  -- even the binding.
  Effect.Reveal {} -> False
  -- CR 701.20e's look moves nothing at all (CR 701.20b), which is Scry's answer
  -- below and one step shorter: it does not even reorder the library.
  Effect.LookAt {} -> False
  -- CR 701.25a's surveil may put the looked-at cards into a graveyard.
  Effect.Surveil {} -> True
  -- CR 701.44a's explore reveals the top card and may put it into a hand or a
  -- graveyard.
  Effect.Explore {} -> True
  -- CR 701.23a searches a zone -- Pawl.Types.Search names which zones and whose,
  -- and its SearchDestination is where the found card goes. Every zone it can
  -- name is one this asks about, so the answer does not depend on Search.zones. A search finding nothing moves nothing, which CR 605.1a's "don't
  -- move any card" tolerates no better than a mode that adds no mana defeats
  -- "could add mana".
  Effect.Search {} -> True
  -- Both halves of its name: cards go INTO a library.
  Effect.ShuffleIntoLibrary {} -> True
  -- CR 701.24a randomises a library WITHIN itself, which is Scry's and Fateseal's
  -- answer above: no card crosses the zone's boundary in either direction.
  Effect.Shuffle {} -> False
  -- The draw half.
  Effect.ExileHandThenDraw -> True
  -- CR 727.2 / 103.3: every card involved in the restarted game is in the new
  -- game, which starts by shuffling each player's deck into their library.
  Effect.RestartGame _ -> True
  -- CR 729.2: as a subgame starts, "each player takes all the cards in their
  -- main-game library, moves them to their subgame library, and shuffles them".
  Effect.PlaySubgame _ -> True
  -- Reads its payload, as the Meld and Conjure arms below do: the opcode is the
  -- general zone change and only its ZONES answer the question. Three ways to
  -- touch a library: arriving in one, being named as leaving one (CR 113.6m's
  -- origin), or being referred to by position in one.
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref zone _ _ origin _ _) ->
    zone == Zone.Library || origin == Just Zone.Library || refReachesLibrary ref
  -- CR 701.42a's destination is the battlefield, so the "to" half is never a
  -- library; the "from" half is wherever the named cards are, which is the same
  -- question the ref asks above.
  Effect.Meld (Meld.MkMeld ref _) -> refReachesLibrary ref
  Effect.AddMana _ -> False
  Effect.DealDamage (DealDamage.MkDealDamage {}) -> False
  Effect.Fight {} -> False
  Effect.ModifyTarget {} -> False
  Effect.ChangeText {} -> False
  Effect.ExileAllGraveyards -> False
  Effect.Proliferate -> False
  Effect.ChooseCardName _ -> False
  -- CR 400.11: the card comes from OUTSIDE THE GAME, which is not a zone at all
  -- and so is not a library. CR 605.1a's fourth clause asks about libraries, and
  -- this effect moves no card to or from one.
  Effect.FromOutsideTheGame _ -> False
  -- CR 608.2n: the stack to exile, neither of which is a library.
  Effect.ExileThisSpell -> False
  Effect.Bolster _ -> False
  Effect.Amass _ -> False
  Effect.Blight _ -> False
  Effect.Earthbend _ -> False
  Effect.TemptWithTheRing -> False
  Effect.Venture {} -> False
  Effect.PlayerSacrifices {} -> False
  Effect.ControlPlayerNextTurn _ -> False
  Effect.Destroy {} -> False
  Effect.Sacrifice _ -> False
  Effect.TurnFaceDown _ -> False
  Effect.TurnFaceUp _ -> False
  Effect.RemoveFromCombat _ -> False
  Effect.BecomesBlocked _ -> False
  -- CR 701.22 and CR 701.29 rearrange a library's own cards; nothing enters or
  -- leaves it.
  Effect.Scry {} -> False
  Effect.Fateseal {} -> False
  Effect.Discard {} -> False
  Effect.LoseLife {} -> False
  Effect.GainLife {} -> False
  Effect.ExchangeLifeTotals _ -> False
  Effect.SetLifeTotal {} -> False
  Effect.RedistributeLifeTotals -> False
  Effect.IncreaseSpeed {} -> False
  Effect.DecreaseSpeed {} -> False
  -- A token is created rather than moved, and CR 111.1 puts it straight onto the
  -- battlefield.
  Effect.Create {} -> False
  -- A READING rather than a citation: conjure is digital-only, so no rule says
  -- whether it "moves" a card. Answered by the DESTINATION, because CR 605.1a's
  -- clause is about a library being one end of the arrival -- a conjured card
  -- was in no zone, so strictly nothing moves, but a library gains a card, which
  -- is what the clause exists to keep out of a mana ability. A hand, a graveyard
  -- and the battlefield are libraries no more than the battlefield Create's
  -- token below reaches is.
  --
  -- Unproven either way: no printing conjures inside an ability that could add
  -- mana, so no board tells the two answers apart. A regression fence, not a
  -- test-backed behaviour.
  Effect.Conjure (Conjure.MkConjure _ _ destination) -> case destination of
    ConjureDestination.Hand -> False
    ConjureDestination.Library -> True
    ConjureDestination.Graveyard -> False
    ConjureDestination.Battlefield -> False
  Effect.CreateCopy {} -> False
  -- CR 707.4 says so in as many words: the permanent remains on the
  -- battlefield, so no card moves out of a library or anywhere else.
  Effect.BecomeCopy {} -> False
  -- CR 707.10's copy is put onto the stack, not moved there from a zone, and it
  -- is no card at all (CR 112.1a).
  Effect.CopyStackObject {} -> False
  Effect.Replace {} -> False
  Effect.SkipNextPhase {} -> False
  -- CR 615.5's rider is not descended into, the same stop `manaProduced` makes:
  -- this asks what the effect ITSELF does, and Resolve.runPreventionRider runs
  -- the rider against the effect's own source, later, rather than as part of
  -- this ability.
  Effect.PreventNextDamage {} -> False
  Effect.PreventAllDamage {} -> False
  Effect.RedirectDamage {} -> False
  -- CR 701.6: a countered spell goes to its owner's GRAVEYARD.
  Effect.Counter {} -> False
  Effect.PutCounters {} -> False
  Effect.PutCountersFrom {} -> False
  Effect.RemoveCounters {} -> False
  Effect.MoveCounters {} -> False
  Effect.GainPlayerCounters {} -> False
  Effect.RemovePlayerCounters {} -> False
  Effect.PayAnyEnergy _ -> False
  Effect.Tap _ -> False
  Effect.Untap _ -> False
  Effect.Detain _ -> False
  Effect.Goad _ -> False
  Effect.MakePlotted _ -> False
  Effect.DoesNotUntapNext _ -> False
  Effect.Transform _ -> False
  Effect.Convert _ -> False
  Effect.PhaseOut _ -> False
  -- CR 500.1: the added phases bring their own turn-based actions, and a draw
  -- step's draw is one of those rather than an effect of this ability. Same for
  -- the extra turn below.
  Effect.AddPhases _ -> False
  Effect.EndTurn -> False
  Effect.EndCombatPhase -> False
  Effect.TakeExtraTurn {} -> False
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> False
  -- The armed ability is a SEPARATE ability (CR 603.7a), so what it later does
  -- is not this effect's doing. The monarch's draw below is a separate triggered
  -- ability for the same reason (CR 725.2).
  Effect.ArmDelayedTrigger {} -> False
  Effect.BecomeMonarch {} -> False
  Effect.TakeTheInitiative {} -> False
  -- Every Pawl.Types.PlayerEffect is a continuous modification of what a player
  -- may do; none moves a card.
  Effect.AffectPlayers {} -> False
  Effect.RequireBlock {} -> False
  Effect.CantBeRegenerated {} -> False
  Effect.ForbidBlock {} -> False
  Effect.ForbidAttack {} -> False
  Effect.ForbidActivation {} -> False
  Effect.RequireAttack {} -> False
  Effect.CreateEmblem {} -> False
  Effect.Designate (Designate.MkDesignate _ _) -> False
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> False
  Effect.Unsuspect _ -> False
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> False
  Effect.Evolve _ -> False
  Effect.Mentor _ -> False
  Effect.Train _ -> False
  Effect.ItBecomes _ -> False
  Effect.ExileUntilMonarch _ -> False
  Effect.ExileHaunting {} -> False
  Effect.Attach _ -> False
  Effect.AttachTarget {} -> False
  Effect.AttachTargetToEach {} -> False
  Effect.AttachBound {} -> False
  Effect.ChoosePlayer _ -> False
  Effect.ChooseOpponentAtRandom _ -> False
  Effect.RollDie {} -> False
  Effect.FlipCoin {} -> False
  -- The card is wherever the slot bound it, which the opcode itself never
  -- states. Its one producer is CR 310.12b's exiled Siege, so no library is in
  -- reach; a producer that offered the cast of a card in a library would want
  -- True here.
  Effect.OfferCast {} -> False
  -- Writes a permission onto objects an earlier effect already placed, and moves
  -- nothing itself.
  Effect.GrantPlayFromExile {} -> False
  -- Descended into for `manaProduced`'s reason: rule 608.2f's body runs as part
  -- of THIS effect.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> any movesLibraryCard body

-- Which zone an ObjectRef reaches, asked of libraries alone: does the ref name
-- cards that may be IN one? movesLibraryCard's shared half, since CR 605.1a's
-- fourth clause is about the cards an effect moves and two opcodes name theirs
-- the same way -- Effect.MoveToZone, whose own zones answer for the destination,
-- and Effect.Meld, whose destination is fixed at the battlefield.
refReachesLibrary :: ObjectRef.ObjectRef -> Bool
refReachesLibrary ref = case ref of
  ObjectRef.TopOfLibrary {} -> True
  -- TRUE for the arm above's reason: the cards are named by their POSITION in
  -- a library, whatever ends the walk that finds them.
  ObjectRef.TopOfLibraryUntil {} -> True
  -- TRUE, the only sweeping arm that is: CR 605.1a's fourth clause asks
  -- whether the effect moves a card to or from a LIBRARY, and this one names
  -- every card in the resolving controller's. A regression fence rather than a
  -- proven line: rule 605.1a classifies ACTIVATED abilities, and every printing
  -- of this arm puts it under a triggered ability or a spell, so no card in the
  -- pool can tell True from False here. Kept because the rule states it.
  ObjectRef.EachCardInYourLibrary _ -> True
  ObjectRef.InSlot _ -> False
  ObjectRef.EachMatching _ -> False
  ObjectRef.EachCardInGraveyard {} -> False
  ObjectRef.EachCardInYourHand -> False
  ObjectRef.EachCardInHand {} -> False
  ObjectRef.EachCardExiledWithSource {} -> False
  ObjectRef.EachSpell _ -> False
  ObjectRef.EachOnStack _ -> False
  ObjectRef.EachPlayer -> False
  ObjectRef.EachOpponent -> False
  ObjectRef.ChosenPlayer -> False
  ObjectRef.ChosenCardInGraveyard {} -> False
  -- FALSE: the position is in a GRAVEYARD, so the ref itself reaches no library.
  -- Soldevi Digger's own ability still answers True through the MoveToZone's
  -- DESTINATION, which is where "on the bottom of your library" is said.
  ObjectRef.TopOfGraveyard _ -> False
  ObjectRef.ChosenCardInHand {} -> False
  -- TRUE, and the second arm that answers so: a group a look or a reveal
  -- bound is still in the LIBRARY it was shown from (CR 701.20b), so
  -- Commune with the Gods' move takes a card out of one while naming
  -- neither the zone nor a position in it. CR 605.1a asks what the effect
  -- MAY do rather than where one board's group happens to sit, and a group
  -- bound by a mill -- already in a graveyard -- cannot make the answer No.
  ObjectRef.ChosenCardFromAmong {} -> True
  -- TRUE for the arm above's reason, unchanged by taking every match
  -- instead of one: the group a look or a reveal bound is still in the
  -- library it was shown from (CR 701.20b).
  ObjectRef.EachCardFromAmong {} -> True
  ObjectRef.RandomCardInHand _ -> False
  -- The battlefield, the arm this one offers a subset of: EachMatching's
  -- answer, unchanged by a chooser standing between the sweep and the set.
  ObjectRef.AnyNumberMatching _ -> False
  -- The battlefield again, the arm above's answer: taking one match instead
  -- of a subset changes nothing about which zone the ref reaches.
  ObjectRef.ChosenPermanent _ -> False
  -- The battlefield once more, the arm above's answer: naming the source
  -- alongside the match adds no zone, the source being a permanent too.
  ObjectRef.SourceAndChosenPermanent _ -> False
