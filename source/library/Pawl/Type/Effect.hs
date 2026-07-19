module Pawl.Type.Effect where

import Pawl.Type.CardCriterion (CardCriterion)
import Pawl.Type.Duration (Duration)
import Pawl.Type.ManaType (ManaType)
import Pawl.Type.Modification (Modification)
import Pawl.Type.Quantity (Quantity)
import Pawl.Type.SlotName (SlotName)

-- The ISA (design.md section 1): first-order, non-recursive, no functions in
-- any field. The ONLY module that may case on a constructor is Pawl.Resolve --
-- the rules core asks classifications, never identities.
data Effect
  = DealDamage SlotName Quantity
  | -- CR 611: create a continuous effect on the slot's target for a duration.
    -- Giant Growth and Serpent's Gift are this one opcode, differing only in the
    -- Modification (layer 7c vs 6). Resolve stores it; it never cases on the
    -- Modification.
    ModifyTarget Duration Modification SlotName
  | -- CR 612: rewrite basic-land-type words in the target spell or permanent. The
    -- SlotName is the target slot; the two basic land types are read from the
    -- caster's binding (Binding.subtypes on Object.bindings) and baked into a
    -- stored ChangeSubtypeWord continuous effect. Resolve stores it; Projection
    -- applies it.
    ChangeText SlotName
  | -- CR 605: add one unit of this mana type. Executed by Mana.tapForMana at
    -- payment (CR 605.3b: a mana ability never uses the stack); Resolve.applyEffect
    -- never runs it. Read by Resolve.manaProduced (the "produces mana?" ABI bit).
    AddMana ManaType
  | -- CR 701.23: search the controller's library for a card matching the
    -- criterion, put it onto the battlefield tapped, then shuffle (Evolving
    -- Wilds' exact shape; destination/tapped are baked in for now).
    Search CardCriterion
  | -- CR 701.10 / Rest in Peace: exile every card in every graveyard. Targetless
    -- and bulk (Rest in Peace's exact shape); a general exile-from-zone is future.
    ExileAllGraveyards
  | -- CR 723.1: "you control target player during that player's next turn."
    -- Installs pending control keyed to the slot's chosen player, with the
    -- ability's controller as the decider. Mindslaver's exact shape.
    ControlPlayerNextTurn SlotName
  deriving (Eq, Ord, Show)
