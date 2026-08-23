-- Mode-scoped structural reads over a Modal payload (CR 700.2), shared by the
-- spell (Face.spell) and both ability types. Parametric in `card` and importing
-- only Type modules and Pawl.Extra, so there is no cycle -- Pawl.Engine.Card
-- imports THIS.
module Pawl.Engine.Modal where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ChooseBetween as ChooseBetween
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeInstance as ModeInstance
import qualified Pawl.Types.ModeSelection as ModeSelection
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SlotName as SlotName
import Pawl.Types.TargetSlot (TargetSlot)

-- Each mode's effects, in printed (mode, then written) order (CR 608.2c) -- one
-- inner list per mode, kept apart. The shape a caller wants when the MODE is
-- the unit of choice but resolution is not in play, where flattening would fuse
-- two alternatives into one. selectionEffects just below is what CR 700.2's
-- selection makes of it, and is what Pawl.Engine.Mana reads.
modeEffects :: Modal.Modal card -> [[Effect card]]
modeEffects m = fmap (Foldable.toList . Mode.allEffects) (Foldable.toList (Modal.modes m))

-- Every effect across all modes, printed (mode, then written) order (CR
-- 608.2c).
allEffects :: Modal.Modal card -> [Effect card]
allEffects m = concat (modeEffects m)

-- CR 700.2/700.2a: every way this payload's SELECTION could be satisfied -- one
-- inner list per legal set of modes, each already flattened into printed (mode,
-- then written) order. The shape a caller wants when the unit of choice is the
-- whole selection rather than a single mode: Pawl.Engine.Mana reads it to
-- enumerate the ways one mana ability could be activated, where a choose-two
-- ability's option is a PAIR of modes adding both modes' mana.
--
-- Reduces to modeEffects when the selection is "choose exactly one", which is
-- what every printed mana ability asks. Targeting is not consulted, so CR
-- 700.2a's "if one of the modes would be illegal ... that mode can't be chosen"
-- does not narrow the list here; the one caller is Pawl.Engine.Mana, and CR
-- 605.1a already keeps a targeting ability from being a mana ability at all.
--
-- Every size the instruction allows, so a RANGE ("Choose one or both")
-- contributes the selections of each size -- one mode alone adds less mana than
-- both together, and both are ways the one activation could go.
selectionEffects :: Modal.Modal card -> [[Effect card]]
selectionEffects m = fmap concat (concatMap (\n -> enumerate n (modeEffects m)) (selectionSizes (Modal.selection m)))
  where
    enumerate = if allowsRepeats m then combinationsWithRepeats else combinations

-- CR 700.2d: "If a player is allowed to choose more than one mode for a modal
-- spell or ability, that player normally can't choose the same mode more than
-- once." So these are the size-n SUBLISTS, taken without repetition and keeping
-- the printed order -- not the size-n sequences.
--
-- Empty when the list is shorter than n, since no selection satisfies such a
-- payload.
combinations :: Natural -> [a] -> [[a]]
combinations n xs
  | n == 0 = [[]]
  | otherwise = case xs of
      [] -> []
      h : t -> fmap (h :) (combinations (n - 1) t) <> combinations n t

-- combinations' other half: CR 700.2d's exception, "some modal spells include
-- the instruction 'You may choose the same mode more than once.'" These are the
-- size-n MULTISUBSETS, keeping the printed order, so one element may be taken
-- repeatedly and the same element's copies stay adjacent.
--
-- Empty only when the list is empty and n > 0 -- unlike combinations, a single
-- option satisfies any n, which is exactly what the exception buys.
combinationsWithRepeats :: Natural -> [a] -> [[a]]
combinationsWithRepeats n xs
  | n == 0 = [[]]
  | otherwise = case xs of
      [] -> []
      -- `h : t` on the left rather than `t`: taking h does not consume it, so h
      -- remains available to the smaller selection.
      h : t -> fmap (h :) (combinationsWithRepeats (n - 1) (h : t)) <> combinationsWithRepeats n t

-- The union of every mode's target slots. Slot names are unique across a card's
-- modes, and that is CHECKED rather than merely intended: a CardSpec lint
-- rejects the card whose two modes declare one name, which this union would
-- otherwise collapse into a single entry.
allTargetSlots :: Modal.Modal card -> Map SlotName TargetSlot
allTargetSlots m = Map.unions (fmap Mode.targetSlots (Foldable.toList (Modal.modes m)))

-- CR 700.2d: the slot a given INSTANCE of a chosen mode fills. "If that mode
-- requires a target, the same player or object may be chosen as the target for
-- each of those modes, or different targets may be chosen" -- so two instances of
-- one repeated mode need two slots, or the second choice would overwrite the
-- first and the rule's "different targets" would be unreachable.
--
-- Occurrence 0 keeps the PRINTED name, so a selection with no repeat -- which is
-- every selection in the pool but Mystic Confluence's, and most of that card's --
-- binds exactly the names it always did, and nothing downstream of a
-- repeat-free cast can tell this function was ever called. Later occurrences take
-- a suffix a card cannot print: Pawl.CardSpec rejects a declared slot name
-- containing '#'.
instanceSlot :: ModeInstance.ModeInstance -> SlotName -> SlotName
instanceSlot mi slot = case ModeInstance.occurrence mi of
  0 -> slot
  k -> SlotName.MkSlotName (SlotName.unwrap slot <> Text.pack ("#" <> show k))

-- CR 608.2c/700.2: the CHOSEN modes themselves, each with the ModeInstance
-- naming which mode it is and which occurrence of it, in printed order. Out-of-
-- range indices contribute nothing (total via Seq.lookup).
--
-- The ORDER is what CR 702.42b demands of an entwined spell, and what CR 700.2d
-- demands of a repeated one ("treated as if that mode appeared that many times in
-- sequence"): ModeIndex order IS printed order, and the chosen Seq is sorted, so
-- a mode chosen twice contributes two adjacent instances.
--
-- Modes rather than a flat effect list because a mode is the unit CR 700.2c
-- scopes targets to, and because it holds the clauses CR 603.5's "may" and CR
-- 118.12a's "unless" each cover -- so a resolver that flattened first could ask
-- neither question where it belongs. The instance rides along so the prompt can
-- name which mode is asking and so the resolver can find that instance's own
-- slots.
chosenModes :: Seq.Seq ModeIndex.ModeIndex -> Modal.Modal card -> [(ModeInstance.ModeInstance, Mode.Mode card)]
chosenModes chosen m =
  let modeAt mi = fmap ((,) mi) (modeAtIndex (ModeInstance.index mi) m)
   in Maybe.mapMaybe modeAt (instancesOf chosen)

-- The chosen indices numbered by occurrence, sorted (CR 608.2c). Split out
-- because the slots and the modes must agree instance for instance, and both
-- read it.
instancesOf :: Seq.Seq ModeIndex.ModeIndex -> [ModeInstance.ModeInstance]
instancesOf chosen =
  let number _ [] = []
      number seen (idx : rest) =
        let k = Maybe.fromMaybe 0 (Map.lookup idx seen)
         in ModeInstance.MkModeInstance idx k : number (Map.insert idx (k + 1) seen) rest
   in number Map.empty (List.sort (Foldable.toList chosen))

-- One mode by index, or Nothing when the index is out of range (total).
modeAtIndex :: ModeIndex.ModeIndex -> Modal.Modal card -> Maybe (Mode.Mode card)
modeAtIndex (ModeIndex.MkModeIndex n) m = Seq.lookup (Natural.toIntSaturating n) (Modal.modes m)

-- CR 608.2c/700.2: only the CHOSEN modes' effects, flattened in ModeIndex then
-- written order, with a mode chosen twice contributing its effects twice (CR
-- 700.2d). A CLASSIFICATION read -- what effects these modes hold, of the kind
-- Pawl.Engine.ManaAbility asks -- where optionality is not part of the question;
-- resolution goes through chosenModes instead.
modesEffects :: Seq.Seq ModeIndex.ModeIndex -> Modal.Modal card -> [Effect card]
modesEffects chosen m = concatMap (Foldable.toList . Mode.allEffects . snd) (chosenModes chosen m)

-- CR 601.2c/700.2c: only the CHOSEN modes' target slots (union), each instance's
-- slots under instanceSlot's name. Two modes may be chosen at once (CR 702.42a's
-- entwine), which is what makes this union's collapse an observable wrong answer
-- rather than a latent one -- so the CardSpec lint keeps a card whose modes share
-- a slot name out of the pool, and instanceSlot keeps one mode's repeats from
-- colliding with themselves.
modesTargetSlots :: Seq.Seq ModeIndex.ModeIndex -> Modal.Modal card -> Map SlotName TargetSlot
modesTargetSlots chosen m = Map.unions (fmap (\mi -> instanceTargetSlots mi m) (instancesOf chosen))

-- One chosen instance's target slots, renamed under that instance's slot names.
-- The inverse of the projection instanceView applies before running the
-- instance's effects, which still read the printed names.
instanceTargetSlots :: ModeInstance.ModeInstance -> Modal.Modal card -> Map SlotName TargetSlot
instanceTargetSlots mi m =
  Map.mapKeys (instanceSlot mi) (maybe Map.empty Mode.targetSlots (modeAtIndex (ModeInstance.index mi) m))

-- CR 700.2d: the binding-shaped environment ONE chosen instance's effects read.
-- Three parts, and each is a rule rather than a convenience:
--
--   * this instance's own slots appear under their PRINTED names, because an
--     effect names the slot its card printed and knows nothing of occurrences.
--     Occurrence 0's are already printed-named, so this is the identity there;
--   * every OTHER chosen instance's slots are removed, so the second instance of
--     a repeated mode cannot see the first's target and silently reuse it -- the
--     shape that would make "different targets may be chosen" unobservable. This
--     also removes a DIFFERENT chosen mode's slots, which the CardSpec read lint
--     already forbids a mode from reading (it is per mode, exactly because CR
--     700.2c scopes a target to the mode that asked for it);
--   * everything that is not a chosen mode's target slot passes through: the
--     reserved bindings (the source, "you", X, the chosen modes), CR 303.4a's
--     enchant slot, and any slot this resolution has defined.
--
-- Not implemented: a slot a mode DEFINES (MoveToZone's, Create's, Destroy's,
-- PlaySubgame's) is still written under its printed name, so two instances of one
-- repeated mode would write to one key (#996).
--
-- Generic in the value so the legality map and the target map are projected by
-- one function, which is what keeps them from disagreeing about which slot
-- belongs to which instance.
--
-- `allSlots` is every chosen instance's slots (modesTargetSlots, already
-- instance-named); `printed` is this instance's own mode's slots, under the names
-- the card prints. Taken as arguments rather than derived from the payload
-- because the two resolution paths hold different things: a spell's `allSlots`
-- also carries CR 303.4a's enchant slot, which is the card's and not any mode's,
-- and so must survive the projection.
instanceView :: Map SlotName TargetSlot -> ModeInstance.ModeInstance -> Map SlotName TargetSlot -> Map SlotName v -> Map SlotName v
instanceView allSlots mi printed env =
  let renamed = Maybe.mapMaybe (\slot -> fmap ((,) slot) (Map.lookup (instanceSlot mi slot) env)) (Map.keys printed)
   in Map.union (Map.fromList renamed) (Map.withoutKeys env (Map.keysSet allSlots))

-- The target slots of one mode by index (CR 700.2c: only the chosen mode's
-- slots). Nothing if the index is out of range (total).
modeTargetSlots :: ModeIndex.ModeIndex -> Modal.Modal card -> Maybe (Map SlotName TargetSlot)
modeTargetSlots (ModeIndex.MkModeIndex n) m =
  fmap Mode.targetSlots (Seq.lookup (Natural.toIntSaturating n) (Modal.modes m))

-- CR 700.2: the FEWEST modes a selection satisfying this instruction may name.
-- Equal to mostOf for an exact instruction, whichever half of CR 700.2d it is;
-- a range's lower bound otherwise ("Choose one or both" is 1).
leastOf :: ModeSelection.ModeSelection -> Natural
leastOf selection = case selection of
  ModeSelection.ChooseExactly n -> n
  ModeSelection.ChooseExactlyWithRepeats n -> n
  ModeSelection.ChooseBetween cb -> ChooseBetween.least cb

-- CR 700.2: the MOST modes a selection satisfying this instruction may name --
-- leastOf's counterpart, and the same number for every exact instruction.
mostOf :: ModeSelection.ModeSelection -> Natural
mostOf selection = case selection of
  ModeSelection.ChooseExactly n -> n
  ModeSelection.ChooseExactlyWithRepeats n -> n
  ModeSelection.ChooseBetween cb -> ChooseBetween.most cb

-- CR 700.2d: does this instruction print "You may choose the same mode more than
-- once"? False is that rule's default -- "that player normally can't choose the
-- same mode more than once" -- and is what every card in the pool but Mystic
-- Confluence answers.
--
-- True implies leastOf == mostOf: no printing pairs the exception with a range,
-- which is why Pawl.Types.ModeSelection has no such constructor and why the
-- repeating arms below read one bound and mean "the count".
allowsRepeatsIn :: ModeSelection.ModeSelection -> Bool
allowsRepeatsIn selection = case selection of
  ModeSelection.ChooseExactly _ -> False
  ModeSelection.ChooseExactlyWithRepeats _ -> True
  ModeSelection.ChooseBetween {} -> False

-- Every size a selection satisfying the printed instruction may have, ascending.
-- One element for an exact instruction, `most - least + 1` for a range.
selectionSizes :: ModeSelection.ModeSelection -> [Natural]
selectionSizes selection = [leastOf selection .. mostOf selection]

-- allowsRepeatsIn over the payload's own printed selection.
allowsRepeats :: Modal.Modal card -> Bool
allowsRepeats = allowsRepeatsIn . Modal.selection

-- CR 700.2a/700.2b: the selection a player has NO choice about, if there is one
-- -- the engine never makes a choice, and never asks a question with one answer.
-- Nothing when a real choice remains, in which case Prompt.ChooseModes is issued.
--
-- Under CR 700.2d's default that is "as many legal modes as the selection demands
-- or fewer": every legal mode must be taken. FEWER is included so the caller's
-- own legality check rejects the whole announcement rather than this function
-- inventing a selection, which is where CR 601.2e/603.3c take over.
--
-- Under the exception it is narrower, because a single legal mode now satisfies
-- any count: one legal mode leaves only "that mode, n times", and no legal mode
-- at all leaves nothing (rejected by the caller as above). Two legal modes and a
-- count of three is a real choice even though there are fewer modes than the
-- count, which is exactly the case the default has no analogue of.
--
-- A RANGE takes the default's arm, measured against its FLOOR, and that is the
-- whole of it: "Choose one or both" with one legal mode leaves only that mode (CR
-- 700.2a makes the other unchoosable), while two legal modes leave one, the other,
-- or both -- three answers, so the prompt is issued. More generally a selection is
-- forced only when one size is available and one subset has it, which for `legal`
-- of size k and bounds least..most is k <= least; every k > least admits at least
-- two answers.
forcedSelection :: Set ModeIndex.ModeIndex -> ModeSelection.ModeSelection -> Maybe (Seq.Seq ModeIndex.ModeIndex)
forcedSelection legal selection
  -- "Choose zero" names nothing whatever is legal. No printing says it; the arm
  -- is here so the two below may assume a positive ceiling.
  | mostOf selection == 0 = Just Seq.empty
  | allowsRepeatsIn selection = case Set.toAscList legal of
      [] -> Just Seq.empty
      [only] -> Just (Seq.replicate (Natural.toIntSaturating (mostOf selection)) only)
      _ -> Nothing
  | Natural.length legal <= leastOf selection = Just (Seq.fromList (Set.toAscList legal))
  | otherwise = Nothing

-- CR 700.2a: is there ANY selection satisfying this instruction, given which
-- modes are legal? The question a cast-legality gate asks before a mode has been
-- chosen, and CR 700.2d's exception is what stops it being a count comparison: a
-- single legal mode satisfies "choose three" when the same mode may be chosen
-- three times, and does not otherwise.
--
-- A range asks it of its FLOOR: "Choose one or both" needs one legal mode, not
-- two, since choosing one is an answer the instruction allows.
selectionPossible :: Set ModeIndex.ModeIndex -> ModeSelection.ModeSelection -> Bool
selectionPossible legal selection
  | mostOf selection == 0 = True
  | allowsRepeatsIn selection = not (Set.null legal)
  | otherwise = Natural.length legal >= leastOf selection

-- CR 601.2b/700.2: does this answer really satisfy the printed instruction? The
-- size is one the instruction allows -- a single number for an exact instruction,
-- anything within the bounds for a range -- every mode named is legal (CR 700.2a),
-- and no mode is named twice unless CR 700.2d's exception is printed. The
-- reject-not-repair gate every announcement path runs before it stamps anything.
selectionSatisfiedBy :: Set ModeIndex.ModeIndex -> ModeSelection.ModeSelection -> Seq.Seq ModeIndex.ModeIndex -> Bool
selectionSatisfiedBy legal selection chosen =
  let distinct = Set.fromList (Foldable.toList chosen)
      size = Natural.length chosen
   in size >= leastOf selection
        && size <= mostOf selection
        && Set.isSubsetOf distinct legal
        && (allowsRepeatsIn selection || Set.size distinct == Seq.length chosen)

-- How many modes are PRINTED, which is not the same question as how many the
-- selection demands. CR 702.42a's entwine is what asks it: this is the count
-- Pawl.Engine.Cast substitutes for selectionCount when the entwine cost is
-- paid.
modeCount :: Modal.Modal card -> Natural
modeCount = Natural.length . Modal.modes
