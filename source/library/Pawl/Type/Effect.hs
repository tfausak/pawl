module Pawl.Type.Effect where

import Pawl.Type.CardCriterion (CardCriterion)
import Pawl.Type.Duration (Duration)
import Pawl.Type.ManaType (ManaType)
import Pawl.Type.Modification (Modification)
import Pawl.Type.Quantity (Quantity)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.Zone (Zone)

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
  | -- CR 701.7 / 700.4: destroy the slot's target permanent -- move it to its
    -- owner's graveyard via the changeZone funnel UNLESS it is indestructible.
    -- NOT MoveToZone slot Graveyard: the indestructible check is why this is its
    -- own opcode (Murder vs Darksteel Myr). A future interceptable "destroy event"
    -- (regeneration, CR 615) is M4d.
    Destroy SlotName
  | -- CR 400.7: move the slot's target object to a zone through the changeZone
    -- funnel. Bounce = MoveToZone slot Hand (owner-relative -- changeZone carries
    -- Object.owner); targeted exile = MoveToZone slot Exile. The destination is
    -- data; one opcode for every targeted single-object move. Distinct from
    -- Destroy (unconditional move, no indestructible check).
    MoveToZone SlotName Zone
  | -- CR 120: the controller draws this many cards. Targetless (a spell's
    -- controller draws, CR 120.2). Empty-library draw is a loss (CR 121.3),
    -- unlike Mill -- the semantic asymmetry that keeps Draw and Mill separate.
    Draw Quantity
  | -- CR 701.13: the slot's target player mills this many (top N of their library
    -- to their graveyard). Milling a short/empty library mills fewer, no penalty
    -- (CR 701.13b) -- unlike Draw, which loses on empty.
    Mill SlotName Quantity
  | -- CR 701.8: the slot's target player discards this many. The DISCARDING player
    -- chooses which (CR 701.8a) via Prompt.ChooseDiscard, routed through
    -- Decide.deciderFor. A hand smaller than the count discards all of it (CR
    -- 701.8b), forced -- so it is not prompted.
    Discard SlotName Quantity
  deriving (Eq, Ord, Show)
