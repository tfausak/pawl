-- CR 605.1a's classification, at both the scales the rule needs it:
-- `manaProduced` asks one EFFECT whether it could add mana and
-- `movesLibraryCard` asks one EFFECT whether it moves a card to or from a
-- library, and `isManaAbility` folds both over a whole ABILITY and adds the
-- no-target and not-a-loyalty-ability clauses.
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
-- below answers the one question in the type.
module Pawl.Engine.ManaAbility where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.DurationRef as DurationRef
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ManaAddition as ManaAddition
import Pawl.Types.ManaProduction (ManaProduction)
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SetClassLevel as SetClassLevel
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
-- The library clause reads the EFFECT half of "its cost and effect don't move
-- any card to or from a library" only. The COST half is not checked: no
-- Pawl.Types.CostComponent moves a card to or from a library, so there is
-- nothing to ask (#1517).
--
-- ACTIVATED abilities only, which is CR 605.1a's own scope. Not implemented: CR
-- 605.1b's triggered mana ability. No Pawl.Types.TriggerCondition watches mana
-- being added or a mana ability being activated, so no triggered ability can meet
-- the rule, and every one that adds mana resolves off the stack instead --
-- Pawl.Engine.Resolve's Effect.AddMana arm (#1572).
--
-- CR 605.1a's closing sentence -- do not take replacement effects other than
-- self-replacement effects into account -- holds by construction rather than by
-- a guard. Every clause here reads an ActivatedAbility's PRINTED effects out of
-- the card, so no replacement effect is in scope to take into account, and the
-- self-replacement ones the rule does admit are written into those effects.
isManaAbility :: ActivatedAbility.ActivatedAbility Card.Type.Card -> Bool
isManaAbility ab =
  not (null (Maybe.mapMaybe manaProduced effects))
    && Map.null (Modal.allTargetSlots (ActivatedAbility.modal ab))
    && not (any movesLibraryCard effects)
  where
    effects = Modal.allEffects (ActivatedAbility.modal ab)

-- CR 605: does this effect add mana, and how is its type decided? Read by
-- Mana.isManaAbility to keep mana abilities off the stack, and by
-- Mana.manaRoutesOfGiven to enumerate what one activation would add.
--
-- Returns the ManaProduction rather than a settled ManaType because CR 605.1a
-- asks whether the ability COULD add mana, which an unresolved colour choice
-- answers yes to; which colour is Cost.tapForMana's prompt, not a static fact.
--
-- The payload's RECIPIENT is dropped, and the classification is right to drop it:
-- CR 605.1a asks whether the ability could add mana to "a player's" pool, not to
-- its controller's, so an ability naming somebody else is a mana ability just the
-- same. Not implemented: the payment path acting on that recipient -- Cost.tapForMana
-- adds an activation's whole yield to the activator, so a mana ability naming
-- another player would pay the wrong pool. Spectral Searchlight and Valleymaker
-- are the printings; neither is in `data/cards/` (#1673).
--
-- The payload's RETENTION (Pawl.Types.ManaRetention) is dropped for the same
-- reason and just as rightly: CR 605.1a's four criteria say nothing about how
-- long the mana lasts, so a retained AddMana is a mana ability like any other.
-- Not implemented: the payment path acting on it -- Mana.manaOptionsOfGiven
-- stamps Ordinary, so a mana ability that retained its mana would not (#1808).
--
-- The payload's spending RESTRICTION (CR 106.6) is dropped for the third time
-- and just as rightly, CR 605.1a saying nothing about what the mana may pay for.
-- Not implemented: the payment path acting on it -- Mana.manaOptionsOfGiven
-- stamps Nothing, so a mana ability's restricted mana would be spendable on
-- anything. Mishra's Workshop and Cavern of Souls are the printings; neither is
-- in `data/cards/` (#1976).
manaProduced :: Effect Card.Type.Card -> Maybe ManaProduction
manaProduced effect = case effect of
  Effect.AddMana addition -> Just (ManaAddition.production addition)
  Effect.DealDamage (DealDamage.MkDealDamage {}) -> Nothing
  Effect.Fight {} -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText {} -> Nothing
  Effect.Search {} -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.Proliferate -> Nothing
  Effect.Bolster _ -> Nothing
  Effect.Amass _ -> Nothing
  Effect.Blight _ -> Nothing
  Effect.TemptWithTheRing -> Nothing
  Effect.Venture -> Nothing
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
  Effect.CreateCopy {} -> Nothing
  Effect.BecomeCopy {} -> Nothing
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
  Effect.RemoveCounters {} -> Nothing
  Effect.GainPlayerCounters {} -> Nothing
  Effect.RemovePlayerCounters {} -> Nothing
  Effect.PayAnyEnergy _ -> Nothing
  Effect.Tap _ -> Nothing
  Effect.Untap _ -> Nothing
  Effect.Detain _ -> Nothing
  Effect.Goad _ -> Nothing
  Effect.DoesNotUntapNext _ -> Nothing
  Effect.Transform _ -> Nothing
  Effect.PhaseOut _ -> Nothing
  Effect.AddPhases _ -> Nothing
  Effect.EndTurn -> Nothing
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> Nothing
  Effect.ArmDelayedTrigger {} -> Nothing
  Effect.AffectPlayers {} -> Nothing
  Effect.RequireBlock {} -> Nothing
  Effect.RequireAttack {} -> Nothing
  Effect.CreateEmblem {} -> Nothing
  Effect.BecomeMonarch {} -> Nothing
  Effect.Designate (Designate.MkDesignate _ _) -> Nothing
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> Nothing
  Effect.Unsuspect _ -> Nothing
  Effect.Evolve _ -> Nothing
  Effect.Mentor _ -> Nothing
  Effect.Train _ -> Nothing
  Effect.ItBecomes _ -> Nothing
  Effect.ExileUntilMonarch _ -> Nothing
  Effect.ExileHaunting {} -> Nothing
  Effect.Attach _ -> Nothing
  Effect.AttachTarget {} -> Nothing
  Effect.AttachTargetToEach {} -> Nothing
  Effect.PlaySubgame _ -> Nothing
  Effect.ChooseOpponent _ -> Nothing
  Effect.ChooseOpponentAtRandom _ -> Nothing
  Effect.TakeExtraTurn {} -> Nothing
  Effect.ShuffleIntoLibrary {} -> Nothing
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
movesLibraryCard :: Effect Card.Type.Card -> Bool
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
  -- CR 701.23a searches a zone, and this opcode's zone is always a LIBRARY --
  -- Pawl.Types.Search names whose, and its SearchDestination is where the found
  -- card goes. A search finding nothing moves nothing, which CR 605.1a's "don't
  -- move any card" tolerates no better than a mode that adds no mana defeats
  -- "could add mana".
  Effect.Search {} -> True
  -- Both halves of its name: cards go INTO a library.
  Effect.ShuffleIntoLibrary {} -> True
  -- The draw half.
  Effect.ExileHandThenDraw -> True
  -- CR 727.2 / 103.3: every card involved in the restarted game is in the new
  -- game, which starts by shuffling each player's deck into their library.
  Effect.RestartGame _ -> True
  -- CR 729.2: as a subgame starts, "each player takes all the cards in their
  -- main-game library, moves them to their subgame library, and shuffles them".
  Effect.PlaySubgame _ -> True
  -- The one arm that has to read its payload, because the opcode is the general
  -- zone change and only its ZONES answer the question. Three ways to touch a
  -- library: arriving in one, being named as leaving one (CR 113.6m's origin),
  -- or being referred to by position in one.
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref zone _ _ origin _) ->
    zone == Zone.Library || origin == Just Zone.Library || case ref of
      ObjectRef.TopOfLibrary {} -> True
      -- TRUE for the arm above's reason: the cards are named by their POSITION in
      -- a library, whatever ends the walk that finds them.
      ObjectRef.TopOfLibraryUntil {} -> True
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
  Effect.AddMana _ -> False
  Effect.DealDamage (DealDamage.MkDealDamage {}) -> False
  Effect.Fight {} -> False
  Effect.ModifyTarget {} -> False
  Effect.ChangeText {} -> False
  Effect.ExileAllGraveyards -> False
  Effect.Proliferate -> False
  Effect.Bolster _ -> False
  Effect.Amass _ -> False
  Effect.Blight _ -> False
  Effect.TemptWithTheRing -> False
  Effect.Venture -> False
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
  Effect.CreateCopy {} -> False
  -- CR 707.4 says so in as many words: the permanent remains on the
  -- battlefield, so no card moves out of a library or anywhere else.
  Effect.BecomeCopy {} -> False
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
  Effect.RemoveCounters {} -> False
  Effect.GainPlayerCounters {} -> False
  Effect.RemovePlayerCounters {} -> False
  Effect.PayAnyEnergy _ -> False
  Effect.Tap _ -> False
  Effect.Untap _ -> False
  Effect.Detain _ -> False
  Effect.Goad _ -> False
  Effect.DoesNotUntapNext _ -> False
  Effect.Transform _ -> False
  Effect.PhaseOut _ -> False
  -- CR 500.1: the added phases bring their own turn-based actions, and a draw
  -- step's draw is one of those rather than an effect of this ability. Same for
  -- the extra turn below.
  Effect.AddPhases _ -> False
  Effect.EndTurn -> False
  Effect.TakeExtraTurn {} -> False
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> False
  -- The armed ability is a SEPARATE ability (CR 603.7a), so what it later does
  -- is not this effect's doing. The monarch's draw below is a separate triggered
  -- ability for the same reason (CR 725.2).
  Effect.ArmDelayedTrigger {} -> False
  Effect.BecomeMonarch {} -> False
  -- Every Pawl.Types.PlayerEffect is a continuous modification of what a player
  -- may do; none moves a card.
  Effect.AffectPlayers {} -> False
  Effect.RequireBlock {} -> False
  Effect.RequireAttack {} -> False
  Effect.CreateEmblem {} -> False
  Effect.Designate (Designate.MkDesignate _ _) -> False
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> False
  Effect.Unsuspect _ -> False
  Effect.Evolve _ -> False
  Effect.Mentor _ -> False
  Effect.Train _ -> False
  Effect.ItBecomes _ -> False
  Effect.ExileUntilMonarch _ -> False
  Effect.ExileHaunting {} -> False
  Effect.Attach _ -> False
  Effect.AttachTarget {} -> False
  Effect.AttachTargetToEach {} -> False
  Effect.ChooseOpponent _ -> False
  Effect.ChooseOpponentAtRandom _ -> False
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
