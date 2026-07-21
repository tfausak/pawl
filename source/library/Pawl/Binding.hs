module Pawl.Binding where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.Type.Binding (Binding)
import qualified Pawl.Type.Binding as Binding
import Pawl.Type.ModeIndex (ModeIndex)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.ProjectedCharacteristics (ProjectedCharacteristics)
import Pawl.Type.Recipient (Recipient)
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.SlotName (SlotName)
import qualified Pawl.Type.SlotName as SlotName
import Pawl.Type.Subtype (Subtype)

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

-- CR 707.9a: the reserved slot under which an object's copy snapshot is stored
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
-- making the two lints mutually unsatisfiable. CardSpec.hs's D4 lint avoids
-- this today because `cardOffends` walks only `Modal.modes (Card.Type.spell
-- card)` -- a card's SPELL modes -- and every `Sacrifice Binding.triggerSource`
-- in this phase lives in a TRIGGERED ability, whose modes the lint never
-- visits. Any future extension of the D4 lint to triggered/activated/delayed
-- ability modes MUST subtract the reserved slot names (this one, variableX,
-- chosenModes, copySource) from the read-slots side before comparing --
-- loosening the equality to a subset would silently retire the lint's
-- "declared but never read" half instead.
triggerSource :: SlotName
triggerSource = SlotName.MkSlotName (Text.pack "self")

-- Bind an object under the reserved triggerSource slot. A dedicated
-- single-purpose slot, so this insert never clobbers another binding (setCopy's
-- posture).
setTriggerSource :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setTriggerSource oid =
  Map.insert triggerSource (Binding.empty {Binding.target = Just (Recipient.ToObject oid)})

-- CR 614.1c / 603.6d: the reserved slot marking that an object that entered as a
-- copy has not yet made its as-enters choice (P2). Set by Event.placeObject,
-- drained at the CR 117.5 boundary by Engine.drainAsEntersChoices.
asEntersPending :: SlotName
asEntersPending = SlotName.MkSlotName (Text.pack "asEntersPending")

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

-- Is an as-enters copy choice still pending for this object?
pendingCopy :: Map SlotName Binding -> Bool
pendingCopy = Map.member asEntersPending

-- Mark an as-enters copy choice pending (Event.placeObject). asEntersPending is a
-- dedicated single-purpose slot, so this insert never clobbers another binding.
markPending :: Map SlotName Binding -> Map SlotName Binding
markPending = Map.insert asEntersPending Binding.empty

-- Clear the as-enters-pending marker (once the choice is made or declined).
clearPending :: Map SlotName Binding -> Map SlotName Binding
clearPending = Map.delete asEntersPending

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
  let fromTargets = Map.map (\r -> Binding.empty {Binding.target = Just r}) targets
      fromSubtypes = Map.map (\p -> Binding.empty {Binding.subtypes = Just p}) subtypes
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
