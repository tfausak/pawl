module Pawl.Engine.Binding where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.Types.Binding (Binding)
import qualified Pawl.Types.Binding as Binding
import Pawl.Types.ModeIndex (ModeIndex)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.ProjectedCharacteristics (ProjectedCharacteristics)
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SlotName as SlotName
import Pawl.Types.Subtype (Subtype)

-- CR 601.2b: the reserved slot under which a spell's single chosen X is stored.
-- No card's targetSpecs may name it (the D4 lint enforces this): X is not a
-- target, so it needs a key the target namespace cannot collide with.
variableX :: SlotName
variableX = SlotName.MkSlotName (Text.pack "X")

-- CR 700.2: the reserved slot under which a modal spell's chosen modes are
-- stored. No card's targetSpecs may name it (the D4 lint enforces this): a mode
-- is not a target. Distinct from variableX.
chosenModes :: SlotName
chosenModes = SlotName.MkSlotName (Text.pack "modes")

-- CR 707.5: the reserved slot under which an object's copy snapshot is stored
-- (P2). No card's targetSpecs may name it: a copy source is not a target.
copySource :: SlotName
copySource = SlotName.MkSlotName (Text.pack "copySource")

-- CR 113.7: the reserved slot under which a triggered ability's SOURCE object
-- (the object whose ability triggered) is bound as the ability is placed, so
-- "this creature" / "this enchantment" is a slot read rather than a
-- self-referential opcode. No card's targetSpecs may name it (the D4 lint
-- enforces this): a source is not a target.
--
-- The D4 lint and this reserved slot do not collide, but only because of
-- where the lint currently looks: Resolve.slotsOf DOES return
-- Binding.triggerSource for an effect that reads it (e.g. Sacrifice
-- Binding.triggerSource), so a "declared slots == read slots" equality check
-- run over that effect's OWN mode would demand a matching targetSpecs entry
-- -- which the "never a declared target slot" rule above then forbids,
-- making the two lints mutually unsatisfiable. CardSpec.hs's EQUALITY lint
-- ("every mode's slot reads equal its declared slots") avoids this today because
-- `cardOffends` walks only `Modal.modes (Card.Type.spell card)` -- a card's
-- SPELL modes -- and every `Sacrifice Binding.triggerSource` in this phase lives
-- in a TRIGGERED ability, whose modes that lint never visits.
--
-- Two lints that now exist cover an ability's OWN reads of this slot -- the
-- delayed one (CardSpec.hs, "every slot a delayed ability reads is bound by its
-- card") and the triggered one ("every slot a triggered ability reads is bound
-- for its condition") -- and neither runs into the equality trap above: both add
-- Binding.triggerSource to the AVAILABLE side (the slots a Create or a
-- MoveToZone binds, plus this one, plus the condition's own event slots for the
-- triggered lint) and
-- check the read slots are a SUBSET of that, never an equality. A subset check
-- has no "declared but never read" half to retire, so reserved-slot subtraction
-- is not needed in either. This warning is about a DIFFERENT, still-hypothetical
-- extension: an EQUALITY-style D4 lint (declared == read, the spell-mode lint's
-- own shape) widened to run over a triggered/activated/delayed ability's modes.
-- That extension MUST subtract the reserved slot names (this one, variableX,
-- chosenModes, copySource, you, thatPlayer, became) from the read-slots side
-- before comparing -- loosening the equality to a subset would silently retire
-- its "declared but never read" half instead.
triggerSource :: SlotName
triggerSource = SlotName.MkSlotName (Text.pack "self")

-- CR 109.5: the reserved slot under which a triggered ability's CONTROLLER is
-- bound ("you"), so a targetless self-referential clause -- Sarcomancy's "deals 1
-- damage to you" -- is a slot read rather than a new opcode.
--
-- "No card's targetSpecs may name it" is lint-enforced here as it is for the
-- reserved names above, by a sweep (CardSpec.hs) that collects the target specs
-- of every carrier: a card's spell modes and enchant slot, AND its activated,
-- triggered and delayed abilities' modes. It has to reach the abilities to mean
-- anything for this slot, because "you" is stamped exclusively on TRIGGERED
-- abilities (setYou below is called only when a triggered ability is placed,
-- Pawl.Engine.Engine): a card declaring a "you" target spec there would otherwise be
-- prompted for a target and have the answer silently clobbered by setYou's
-- insert.
--
-- The triggered-ability READ lint is a different check and never covered this:
-- it is a subset check, and "you" sits on its AVAILABLE side precisely because
-- every triggered ability has it bound.
you :: SlotName
you = SlotName.MkSlotName (Text.pack "you")

-- CR 603.2: the reserved slot under which the PLAYER an event trigger's event
-- names is bound -- "that player" in CR 702.70a's "whenever this creature deals
-- combat damage to a player, that player gets N poison counters". Stamped by
-- Pawl.Engine.Event.eventBindings as the trigger is gathered, so the ability's payload
-- reads an ordinary slot rather than needing a "the damaged player" opcode.
--
-- Distinct from `you` (CR 109.5, the ability's CONTROLLER): the player the
-- event names is generally an opponent, and in a multiplayer game which
-- opponent is not derivable from the controller at all.
--
-- Not a target (nothing was chosen), so CR 608.2b has nothing to re-validate --
-- Resolve.resolveEffects' legalSlot answers True for any slot with no target
-- spec, which is how this slot stays readable at resolution. The same "no
-- card's targetSpecs may name it" rule `you` carries applies here, enforced by
-- the same declaration sweep.
--
-- That an effect READING this slot sits under a condition that binds it IS
-- enforced, by Pawl.Engine.Event.eventBindingSlots and CardSpec's "every slot a
-- triggered ability reads is bound for its condition": only CR 510.1b's
-- combat-damage-to-a-player condition stamps it -- which poisonous is one
-- printing of, not the owner of -- so reading it under any other is a failing
-- test rather than a silent no-op.
triggerPlayer :: SlotName
triggerPlayer = SlotName.MkSlotName (Text.pack "thatPlayer")

-- CR 400.7e: the reserved slot under which a zone-change trigger's ARRIVING
-- incarnation is bound -- "the new object that it became in the zone it moved
-- to when the ability triggered, if that zone is a public zone". CR 603.6c says
-- the same thing from the other side: "An ability that attempts to do something
-- to the card that left the battlefield checks for it only in the first zone
-- that it went to."
--
-- ONE slot for both directions of a zone change, because CR 400.7e is one rule
-- about whatever moved, not a rule about the ability's bearer:
--
--   * a DEPARTURE, where the mover is the bearer -- Endless Cockroaches' "when
--     this creature dies, return it to its owner's hand". The paragraphs below
--     are about this case, which is the hard one.
--   * an ENTRY, where the mover is generally NOT the bearer -- Aether Flash's
--     "whenever a creature enters, this enchantment deals 2 damage to it".
--     Here `triggerSource` is the enchantment and this slot is the entrant, so
--     the two name unrelated objects and nothing has to be told apart. The
--     entrant may still be gone by the time the ability resolves (CR 608.2h),
--     which is what makes an effect reading this slot have to tolerate an id
--     that no longer resolves either way.
--
-- For the departure direction it is a SECOND name for what one printed word
-- calls "it", and the two are not interchangeable. CR 400.7 mints a fresh id on
-- every zone change, so a leaves-the-battlefield trigger has two objects to
-- talk about at once:
--
--   * `triggerSource` above is CR 113.7a's SOURCE -- the permanent as it was on
--     the battlefield, which CR 603.10a's look-back is about and which
--     Projection.viewWithLastKnown reads from CR 608.2h last known information.
--     Everything the ability says ABOUT itself ("its power", "if it had a
--     counter on it") is that object.
--   * this slot is the CARD, wherever the move put it. Everything the ability
--     DOES to itself ("return it to its owner's hand", "exile it") is this one,
--     because the other no longer exists to be moved.
--
-- Collapsing them either way is a silent wrong answer rather than a type error:
-- binding only the source makes every such effect a no-op on a dead id, and
-- rebinding the source to the arrival would redirect viewWithLastKnown at all
-- of Pawl.Engine.Resolve's quantity reads onto the graveyard card's printed
-- characteristics.
--
-- Stamped by Pawl.Engine.Event.eventBindings as the trigger is gathered, alongside
-- `triggerPlayer`, and not a target -- the same CR 608.2b posture that slot's
-- comment spells out: Resolve's legalSlot answers True for any slot with no
-- target spec, which is how this one stays readable at resolution, and CR
-- 608.2b's fizzle asks only about the targeted slots so it cannot rescue a
-- spell either. "No card's targetSpecs may name it" is checked by CardSpec.hs
-- over every carrier of target specs, exactly as it is for `you` -- see that
-- comment for why the abilities have to be in scope. Reading it under a
-- condition that never binds it is the direction that IS enforced, by
-- Pawl.Engine.Event.eventBindingSlots (see `triggerPlayer` above).
became :: SlotName
became = SlotName.MkSlotName (Text.pack "became")

-- A binding that names one object and nothing else -- what a token bound by a
-- Create (CR 603.7c) or a trigger's source slot holds.
toObject :: ObjectId -> Binding
toObject oid = Binding.empty {Binding.target = Just (Recipient.ToObject oid)}

-- A binding that names one player and nothing else -- CR 729.1b's subgame
-- loser, bound by Pawl.Engine.Resolve's bindLoserSlot. Mirrors toObject, but the
-- recipient is a player (ToPlayer), not an object.
toPlayer :: PlayerId -> Binding
toPlayer pid = Binding.empty {Binding.target = Just (Recipient.ToPlayer pid)}

-- A binding that names one NUMBER and nothing else -- what a Destroy that counts
-- what it destroyed binds for a later "for each ... destroyed this way" to read
-- (Quantity.InSlot). Mirrors toObject and toPlayer, but the value is an amount
-- rather than a recipient, so it rides the same field CR 601.2b's chosen X does.
toAmount :: Natural -> Binding
toAmount n = Binding.empty {Binding.amount = Just n}

-- Bind an object under the reserved triggerSource slot. A dedicated
-- single-purpose slot, so this insert never clobbers another binding (setCopy's
-- posture).
setTriggerSource :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setTriggerSource oid = Map.insert triggerSource (toObject oid)

-- Bind a player under the reserved you slot. A dedicated single-purpose slot,
-- so this insert never clobbers another binding (setCopy's posture).
setYou :: PlayerId -> Map SlotName Binding -> Map SlotName Binding
setYou pid = Map.insert you (toPlayer pid)

-- Bind a player under the reserved triggerPlayer slot. A dedicated
-- single-purpose slot, so this insert never clobbers another binding (setCopy's
-- posture).
setTriggerPlayer :: PlayerId -> Map SlotName Binding -> Map SlotName Binding
setTriggerPlayer pid = Map.insert triggerPlayer (toPlayer pid)

-- Bind an object under the reserved became slot (CR 400.7e). A dedicated
-- single-purpose slot, so this insert never clobbers another binding (setCopy's
-- posture).
setBecame :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setBecame oid = Map.insert became (toObject oid)

-- The modes chosen for a spell, read from its binding environment. Empty when
-- absent (defensive; cast always stamps it, forced or prompted).
modesOf :: Map SlotName Binding -> Set ModeIndex
modesOf m = Maybe.fromMaybe Set.empty (Binding.modes =<< Map.lookup chosenModes m)

-- Project the chosen targets (CR 601.2c) out of a binding environment, dropping
-- slots with no target. Restores the pre-M4a Object.targets view for readers.
targetsOf :: Map SlotName Binding -> Map SlotName Recipient
targetsOf = Map.mapMaybe Binding.target

-- Project the chosen (from, to) land-type pairs (CR 612), dropping slots without.
subtypesOf :: Map SlotName Binding -> Map SlotName (Subtype, Subtype)
subtypesOf = Map.mapMaybe Binding.subtypes

-- The amount (X) bound at a slot, if any.
amountOf :: SlotName -> Map SlotName Binding -> Maybe Natural
amountOf slot m = Binding.amount =<< Map.lookup slot m

-- The copy snapshot stored on an object, if any (CR 707.2).
copyOf :: Map SlotName Binding -> Maybe ProjectedCharacteristics
copyOf m = Binding.copy =<< Map.lookup copySource m

-- Store a copy snapshot under the reserved copySource slot (P2). copySource is a
-- dedicated single-purpose slot (no target/subtype/amount/mode binding is ever
-- stored there), so overwriting it wholesale is lossless.
setCopy :: ProjectedCharacteristics -> Map SlotName Binding -> Map SlotName Binding
setCopy pc = Map.insert copySource (Binding.empty {Binding.copy = Just pc})

-- Build the binding environment stamped on a stack object at cast: the chosen
-- targets, the chosen land-type pairs, (Just x) the chosen X under variableX,
-- and (when non-empty) the chosen modes under chosenModes. A slot present in
-- several inputs keeps every choice (Magical Hack's slot).
fromChoices ::
  Map SlotName Recipient ->
  Map SlotName (Subtype, Subtype) ->
  Maybe Natural ->
  Set ModeIndex ->
  Map SlotName Binding
fromChoices targets subtypes mAmount mModes =
  let fromTargets = fmap (\r -> Binding.empty {Binding.target = Just r}) targets
      fromSubtypes = fmap (\p -> Binding.empty {Binding.subtypes = Just p}) subtypes
      merged = Map.unionWith mergeBinding fromTargets fromSubtypes
      withX = case mAmount of
        Nothing -> merged
        Just n ->
          Map.insertWith mergeBinding variableX (Binding.empty {Binding.amount = Just n}) merged
   in if Set.null mModes
        then withX
        else Map.insertWith mergeBinding chosenModes (Binding.empty {Binding.modes = Just mModes}) withX

-- Combine two bindings for the same slot, preferring the left's present choice in
-- each field. Inputs are disjoint per field by construction, so this is a total,
-- order-independent merge.
mergeBinding :: Binding -> Binding -> Binding
mergeBinding a b =
  Binding.MkBinding
    { Binding.target = Binding.target a <|> Binding.target b,
      Binding.subtypes = Binding.subtypes a <|> Binding.subtypes b,
      Binding.amount = Binding.amount a <|> Binding.amount b,
      Binding.modes = Binding.modes a <|> Binding.modes b,
      Binding.copy = Binding.copy a <|> Binding.copy b
    }
