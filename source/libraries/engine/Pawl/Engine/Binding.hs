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

-- CR 601.2b: the reserved slot under which a spell's single chosen X is stored.
-- No card's targetSpecs may name it (lint-enforced): X is not a target, so it
-- needs a key the target namespace cannot collide with.
variableX :: SlotName
variableX = SlotName.MkSlotName (Text.pack "X")

-- CR 700.2: the reserved slot under which a modal spell's chosen modes are
-- stored. No card's targetSpecs may name it (lint-enforced): a mode is not a
-- target. Distinct from variableX.
chosenModes :: SlotName
chosenModes = SlotName.MkSlotName (Text.pack "modes")

-- CR 707.5: the reserved slot under which an object's copy snapshot is stored.
-- No card's targetSpecs may name it: a copy source is not a target.
copySource :: SlotName
copySource = SlotName.MkSlotName (Text.pack "copySource")

-- CR 113.7: the reserved slot under which a triggered ability's SOURCE object
-- (the object whose ability triggered) is bound as the ability is placed, so
-- "this creature" / "this enchantment" is a slot read rather than a
-- self-referential opcode. No card's targetSpecs may name it (lint-enforced): a
-- source is not a target.
--
-- Hazard for a future lint. CardSpec.hs's "declared slots == read slots"
-- equality lint walks only a card's SPELL modes, and the ability lints are all
-- SUBSET checks that put this slot on the available side -- so neither collides
-- with the rule above today. But an equality-style lint widened to an ability's
-- modes WOULD be unsatisfiable, because Resolve.slotsOf returns this slot for
-- an effect that reads it while the rule forbids the matching targetSpecs
-- entry. Such a lint must subtract the reserved names (this one, variableX,
-- chosenModes, copySource, you, thatPlayer, became) from the read side;
-- loosening it to a subset check instead would silently retire its "declared
-- but never read" half.
triggerSource :: SlotName
triggerSource = SlotName.MkSlotName (Text.pack "self")

-- CR 109.5: the reserved slot under which an ability's CONTROLLER is bound
-- ("you"), so a targetless self-referential clause -- Sarcomancy's "deals 1
-- damage to you" -- is a slot read rather than a new opcode.
--
-- Stamped on BOTH ability paths, because the rule defines the word for both:
-- "For an activated ability, this is the player who activated the ability. For
-- a triggered ability, this is the controller of the object when the ability
-- triggered". Pawl.Engine.Activate.activateAbility answers the first;
-- Pawl.Engine.Engine.placeBorne answers the second. CR 725.2's monarch pair is
-- the third stamp site (Pawl.Engine.Monarch.placeInherent): those abilities
-- have no object for CR 109.5's second sentence to name a controller of, and CR
-- 725.2 supplies one itself -- "controlled by the player who was the monarch at
-- the time the abilities triggered".
--
-- A SPELL binds nothing here (#719). Every spell in the pool that says "you"
-- says it through an opcode carrying a PlayerRef, which Pawl.Engine.Resolve
-- answers from the resolving controller with no slot involved; a spell mode
-- reading this slot is a failing test rather than a silent no-op, under the same
-- "declared slots == read slots" equality lint that forbids declaring it.
--
-- "No card's targetSpecs may name it" is lint-enforced as for the names above,
-- by a sweep that has to reach the ABILITIES to mean anything here: a card
-- declaring a "you" target spec on one would be prompted for a target and have
-- the answer clobbered by setYou.
you :: SlotName
you = SlotName.MkSlotName (Text.pack "you")

-- CR 603.2: the reserved slot under which the PLAYER an event trigger's event
-- names is bound -- "that player" in CR 702.70a's poisonous. Stamped by
-- Pawl.Engine.Event.eventBindings as the trigger is gathered, so the ability's
-- payload reads an ordinary slot rather than a "the damaged player" opcode.
--
-- Distinct from `you` (CR 109.5, the ability's CONTROLLER): the player the
-- event names is generally an opponent, and in a multiplayer game which
-- opponent is not derivable from the controller at all.
--
-- Not a target (nothing was chosen), so CR 608.2b has nothing to re-validate --
-- Resolve's legalSlot answers True for any slot with no target spec, which is
-- how this stays readable at resolution. `you`'s "no card's targetSpecs may
-- name it" rule applies here too, under the same sweep. That an effect reading
-- this slot sits under a condition that binds it is enforced by
-- Pawl.Engine.Event.eventBindingSlots: only CR 510.1b's
-- combat-damage-to-a-player condition stamps it, so reading it under any other
-- is a failing test.
triggerPlayer :: SlotName
triggerPlayer = SlotName.MkSlotName (Text.pack "thatPlayer")

-- CR 400.7e / CR 603.6c: the reserved slot under which a zone-change trigger's
-- ARRIVING incarnation is bound.
--
-- ONE slot for both directions of a zone change, because CR 400.7e is one rule
-- about whatever moved, not a rule about the ability's bearer:
--
--   * a DEPARTURE, where the mover is the bearer -- Endless Cockroaches.
--   * an ENTRY, where the mover is generally NOT the bearer -- Aether Flash.
--     Here `triggerSource` is the enchantment and this slot is the entrant, two
--     unrelated objects. The entrant may be gone by resolution (CR 608.2h),
--     which is why an effect reading this slot must tolerate a dead id.
--
-- For the departure direction it is a SECOND name for what one printed word
-- calls "it", and the two are not interchangeable, because CR 400.7 mints a
-- fresh id on every zone change:
--
--   * `triggerSource` is CR 113.7a's SOURCE, the permanent as it was on the
--     battlefield, read from CR 608.2h last known information by
--     Projection.viewWithLastKnown. Everything the ability says ABOUT itself.
--   * this slot is the CARD, wherever the move put it. Everything the ability
--     DOES to itself, because the other no longer exists to be moved.
--
-- Collapsing them either way is a silent wrong answer, not a type error:
-- binding only the source makes every such effect a no-op on a dead id, and
-- rebinding the source to the arrival redirects every viewWithLastKnown
-- quantity read onto the graveyard card's printed characteristics.
--
-- Stamped by Pawl.Engine.Event.eventBindings alongside `triggerPlayer`, and not
-- a target -- same CR 608.2b posture as that slot, including the "no card's
-- targetSpecs may name it" sweep and the eventBindingSlots check on reads. A
-- condition that binds it only SOMETIMES is rejected by that same lint: CR
-- 400.7e withholds this slot when the destination is hidden (CR 400.2), so the
-- wider leaves-the-battlefield condition binds it for a death and not for a
-- bounce, and no card may read it under that condition yet (#505).
became :: SlotName
became = SlotName.MkSlotName (Text.pack "became")

-- CR 615.13: the reserved slot under which a prevention trigger's AMOUNT is
-- bound -- "put that many +1/+1 counters" on Selfless Squire, "you gain that much
-- life" on the same family's other cards. Stamped by
-- Pawl.Engine.Event.eventBindings as the trigger is gathered, so the payload
-- reads an ordinary Quantity.InSlot rather than a "how much was prevented"
-- opcode.
--
-- A NUMBER, where triggerPlayer and became are references, so this is the first
-- reserved slot read through Quantity rather than through a Ref. That is why
-- Pawl.Engine.Quantity.evaluateFor's InSlot arm looks on the stack object as well
-- as on the effect's source: an amount an EFFECT bound mid-resolution lives on
-- the source, and one the EVENT supplied lives where every other trigger binding
-- does.
--
-- Not a target (nothing was chosen), so the same CR 608.2b posture and the same
-- "no card's targetSpecs may name it" sweep as `you`, `thatPlayer` and `became`.
-- Not swept: the SlotName an effect BINDS into (Destroy's count, MoveToZone's
-- incarnation), so a card naming this one there would shadow the event's amount
-- on the source, which InSlot reads first (#691).
preventedAmount :: SlotName
preventedAmount = SlotName.MkSlotName (Text.pack "thatMuch")

-- A binding that names one object and nothing else -- what a token bound by a
-- Create (CR 603.7c) or a trigger's source slot holds.
toObject :: ObjectId -> Binding
toObject oid = Binding.empty {Binding.target = Just (Recipient.ToObject oid)}

-- A binding that names one player and nothing else -- CR 729.1b's subgame
-- loser, bound by Pawl.Engine.Resolve's bindLoserSlot. Mirrors toObject, but
-- the recipient is a player (ToPlayer), not an object.
toPlayer :: PlayerId -> Binding
toPlayer pid = Binding.empty {Binding.target = Just (Recipient.ToPlayer pid)}

-- A binding that names one NUMBER and nothing else -- what a Destroy that
-- counts what it destroyed binds for a later "for each ... destroyed this way"
-- to read, and what CR 615.13's prevented amount rides
-- (Quantity.InSlot). Mirrors toObject and toPlayer, but the value is an
-- amount rather than a recipient, so it rides the same field CR 601.2b's chosen
-- X does.
toAmount :: Natural -> Binding
toAmount n = Binding.empty {Binding.amount = Just n}

-- Bind an object under the reserved triggerSource slot. Dedicated and
-- single-purpose, so the insert cannot clobber another binding -- as below.
setTriggerSource :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setTriggerSource oid = Map.insert triggerSource (toObject oid)

-- Bind a player under the reserved you slot.
setYou :: PlayerId -> Map SlotName Binding -> Map SlotName Binding
setYou pid = Map.insert you (toPlayer pid)

-- Bind a player under the reserved triggerPlayer slot.
setTriggerPlayer :: PlayerId -> Map SlotName Binding -> Map SlotName Binding
setTriggerPlayer pid = Map.insert triggerPlayer (toPlayer pid)

-- Bind an object under the reserved became slot (CR 400.7e).
setBecame :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setBecame oid = Map.insert became (toObject oid)

-- Bind a number under the reserved preventedAmount slot (CR 615.13).
setPreventedAmount :: Natural -> Map SlotName Binding -> Map SlotName Binding
setPreventedAmount n = Map.insert preventedAmount (toAmount n)

-- The modes chosen for a spell, read from its binding environment. Empty when
-- absent (defensive; cast always stamps it, forced or prompted).
modesOf :: Map SlotName Binding -> Set ModeIndex
modesOf m = Maybe.fromMaybe Set.empty (Binding.modes =<< Map.lookup chosenModes m)

-- Project the chosen targets (CR 601.2c) out of a binding environment, dropping
-- slots with no target.
targetsOf :: Map SlotName Binding -> Map SlotName Recipient
targetsOf = Map.mapMaybe Binding.target

-- The amount (X) bound at a slot, if any.
amountOf :: SlotName -> Map SlotName Binding -> Maybe Natural
amountOf slot m = Binding.amount =<< Map.lookup slot m

-- The copy snapshot stored on an object, if any (CR 707.2).
copyOf :: Map SlotName Binding -> Maybe ProjectedCharacteristics
copyOf m = Binding.copy =<< Map.lookup copySource m

-- Store a copy snapshot under the reserved copySource slot. Nothing else is
-- ever stored there, so overwriting it wholesale is lossless.
setCopy :: ProjectedCharacteristics -> Map SlotName Binding -> Map SlotName Binding
setCopy pc = Map.insert copySource (Binding.empty {Binding.copy = Just pc})

-- Build the binding environment stamped on a stack object at cast: the chosen
-- targets, the chosen X under variableX, and any chosen modes under
-- chosenModes. The reserved names cannot collide with a target slot
-- (lint-enforced), so the merges below never actually merge; they are
-- insertWith rather than insert so a future binding kind sharing a slot keeps
-- both choices instead of clobbering one.
fromChoices ::
  Map SlotName Recipient ->
  Maybe Natural ->
  Set ModeIndex ->
  Map SlotName Binding
fromChoices targets mAmount mModes =
  let fromTargets = fmap (\r -> Binding.empty {Binding.target = Just r}) targets
      withX = case mAmount of
        Nothing -> fromTargets
        Just n ->
          Map.insertWith mergeBinding variableX (Binding.empty {Binding.amount = Just n}) fromTargets
   in if Set.null mModes
        then withX
        else Map.insertWith mergeBinding chosenModes (Binding.empty {Binding.modes = Just mModes}) withX

-- Combine two bindings for the same slot, preferring the left's present choice
-- in each field. Inputs are disjoint per field by construction, so this is a
-- total, order-independent merge.
mergeBinding :: Binding -> Binding -> Binding
mergeBinding a b =
  Binding.MkBinding
    { Binding.target = Binding.target a <|> Binding.target b,
      Binding.amount = Binding.amount a <|> Binding.amount b,
      Binding.modes = Binding.modes a <|> Binding.modes b,
      Binding.copy = Binding.copy a <|> Binding.copy b
    }
