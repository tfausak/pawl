-- One of CR 605.1a's three criteria, asked of an EFFECT: could it add mana, and
-- how is its type decided? The ABILITY-level classification that rule actually
-- defines is Pawl.Engine.Mana.isManaAbility, which folds this over an ability's
-- effects and adds the no-target and not-a-loyalty-ability clauses. Extracted from
-- Pawl.Engine.Resolve so that Pawl.Engine.Mana no longer has to import the
-- resolver to ask it.
--
-- That import was an INVERTED edge, and removing it is what lets
-- Pawl.Engine.Combat pay CR 508.1j's cost to attack: Pawl.Engine.Resolve is a
-- high-level module (it reaches Damage, Setup, Mulligan, Replacement and Combat
-- itself), so Mana -> Resolve -> Combat made a mana payment from inside combat a
-- module cycle. Mana is a low-level module and now depends only on low-level
-- ones.
--
-- Casing on Effect here rather than in Pawl.Engine.Resolve is not a breach of that
-- module's charter. design.md section 1 is what the charter enforces -- "the
-- closed half depends on a CLASSIFICATION of effects, never on the IDENTITY of
-- effects" -- and this function IS one of those classifications; design.md's risk
-- register names it by that name, as the `manaProduced()` bit whose absence grew
-- Argentum's mana subsystem 37 `is AddMana*Effect ->` branches. What stays
-- forbidden, here as everywhere, is a case that acts on WHICH effect it is, and
-- every arm below answers the one question in the type.
module Pawl.Engine.ManaAbility where

import qualified Pawl.Types.Card as Card.Type
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import Pawl.Types.ManaProduction (ManaProduction)

-- CR 605: does this effect add mana, and how is its type decided? The "produces
-- mana?" ABI classification (design.md risk register). Read by Mana.isManaAbility
-- to keep mana abilities off the stack, and by Mana.manaRoutesOfGiven to
-- enumerate what one activation of a source would add.
--
-- Returns the ManaProduction rather than a settled ManaType because CR 605.1a
-- asks whether the ability "could add mana", which an unresolved colour choice
-- answers yes to; WHICH colour is Mana.tapForMana's prompt, not a static fact.
manaProduced :: Effect Card.Type.Card -> Maybe ManaProduction
manaProduced effect = case effect of
  Effect.AddMana production -> Just production
  Effect.DealDamage _ _ -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText _ -> Nothing
  Effect.Search _ _ -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.Proliferate -> Nothing
  Effect.ExileHandThenDraw -> Nothing
  Effect.PlayerSacrifices {} -> Nothing
  Effect.RestartGame -> Nothing
  Effect.ControlPlayerNextTurn _ -> Nothing
  Effect.Destroy {} -> Nothing
  Effect.Sacrifice _ -> Nothing
  Effect.RemoveFromCombat _ -> Nothing
  Effect.MoveToZone {} -> Nothing
  Effect.Draw {} -> Nothing
  Effect.Mill {} -> Nothing
  Effect.Discard {} -> Nothing
  Effect.LoseLife {} -> Nothing
  Effect.GainLife {} -> Nothing
  Effect.Create {} -> Nothing
  Effect.Replace {} -> Nothing
  Effect.SkipNextPhase {} -> Nothing
  Effect.PreventNextDamage {} -> Nothing
  Effect.Counter _ -> Nothing
  Effect.PutCounters {} -> Nothing
  Effect.GainPlayerCounters {} -> Nothing
  Effect.Tap _ -> Nothing
  Effect.Untap _ -> Nothing
  Effect.AddPhases _ -> Nothing
  Effect.GainControl _ _ -> Nothing
  Effect.ArmDelayedTrigger {} -> Nothing
  Effect.AffectPlayers {} -> Nothing
  Effect.CreateEmblem {} -> Nothing
  Effect.BecomeMonarch {} -> Nothing
  Effect.ExileUntilMonarch _ -> Nothing
  Effect.Attach _ -> Nothing
  Effect.AttachTarget {} -> Nothing
  Effect.PlaySubgame _ -> Nothing
  Effect.TakeExtraTurn {} -> Nothing
  Effect.ShuffleIntoLibrary _ -> Nothing
