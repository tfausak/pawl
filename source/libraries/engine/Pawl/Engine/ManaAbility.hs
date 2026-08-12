-- CR 605.1a's classification, at both the scales the rule needs it: `manaProduced`
-- asks one EFFECT whether it could add mana, and `isManaAbility` folds that over
-- a whole ABILITY and adds the no-target and not-a-loyalty-ability clauses.
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

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import Pawl.Types.ManaProduction (ManaProduction)

-- CR 605.1a: an activated ability is a mana ability if it could add mana AND
-- doesn't target and is not itself a loyalty ability (CR 606.2, which
-- Pawl.Engine.Cost.isLoyaltyCost answers; no loyalty ability in the pool adds
-- mana, so the clause is inert rather than checked here). Read at three sites:
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
isManaAbility :: ActivatedAbility.ActivatedAbility Card.Type.Card -> Bool
isManaAbility ab =
  not (null (Maybe.mapMaybe manaProduced (Modal.allEffects (ActivatedAbility.modal ab))))
    && Map.null (Modal.allTargetSpecs (ActivatedAbility.modal ab))

-- CR 605: does this effect add mana, and how is its type decided? Read by
-- Mana.isManaAbility to keep mana abilities off the stack, and by
-- Mana.manaRoutesOfGiven to enumerate what one activation would add.
--
-- Returns the ManaProduction rather than a settled ManaType because CR 605.1a
-- asks whether the ability COULD add mana, which an unresolved colour choice
-- answers yes to; which colour is Cost.tapForMana's prompt, not a static fact.
manaProduced :: Effect Card.Type.Card -> Maybe ManaProduction
manaProduced effect = case effect of
  Effect.AddMana production -> Just production
  Effect.DealDamage _ _ -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText {} -> Nothing
  Effect.Search {} -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.Proliferate -> Nothing
  Effect.TemptWithTheRing -> Nothing
  Effect.Venture -> Nothing
  Effect.ExileHandThenDraw -> Nothing
  Effect.PlayerSacrifices {} -> Nothing
  Effect.RestartGame -> Nothing
  Effect.ControlPlayerNextTurn _ -> Nothing
  Effect.Destroy {} -> Nothing
  Effect.Sacrifice _ -> Nothing
  Effect.TurnFaceDown _ -> Nothing
  Effect.RemoveFromCombat _ -> Nothing
  Effect.MoveToZone {} -> Nothing
  Effect.Draw {} -> Nothing
  Effect.Mill {} -> Nothing
  Effect.Scry {} -> Nothing
  Effect.Discard {} -> Nothing
  Effect.LoseLife {} -> Nothing
  Effect.GainLife {} -> Nothing
  Effect.ExchangeLifeTotals _ -> Nothing
  Effect.IncreaseSpeed {} -> Nothing
  Effect.Create {} -> Nothing
  Effect.CreateCopy {} -> Nothing
  Effect.Replace {} -> Nothing
  Effect.SkipNextPhase {} -> Nothing
  -- CR 615.5's rider is not descended into, the same stop Effect.Create makes at
  -- a minted token's abilities: this asks what the effect ITSELF adds, and a
  -- prevention adds no mana. A rider that did would be a mana clause nothing in
  -- the pool prints, and CR 605.1a would then want it seen here.
  Effect.PreventNextDamage {} -> Nothing
  Effect.PreventAllDamage {} -> Nothing
  Effect.RedirectDamage {} -> Nothing
  Effect.Counter _ -> Nothing
  Effect.PutCounters {} -> Nothing
  Effect.RemoveCounters {} -> Nothing
  Effect.GainPlayerCounters {} -> Nothing
  Effect.RemovePlayerCounters {} -> Nothing
  Effect.Tap _ -> Nothing
  Effect.Untap _ -> Nothing
  Effect.Transform _ -> Nothing
  Effect.AddPhases _ -> Nothing
  Effect.GainControl _ _ -> Nothing
  Effect.ArmDelayedTrigger {} -> Nothing
  Effect.AffectPlayers {} -> Nothing
  Effect.RequireBlock {} -> Nothing
  Effect.CreateEmblem {} -> Nothing
  Effect.BecomeMonarch {} -> Nothing
  Effect.Designate _ _ -> Nothing
  Effect.Unsuspect _ -> Nothing
  Effect.Evolve _ -> Nothing
  Effect.Mentor _ -> Nothing
  Effect.ItBecomes _ -> Nothing
  Effect.ExileUntilMonarch _ -> Nothing
  Effect.Attach _ -> Nothing
  Effect.AttachTarget {} -> Nothing
  Effect.PlaySubgame _ -> Nothing
  Effect.TakeExtraTurn {} -> Nothing
  Effect.ShuffleIntoLibrary _ -> Nothing
  Effect.OfferCast {} -> Nothing
  Effect.GrantPlayFromExile {} -> Nothing
