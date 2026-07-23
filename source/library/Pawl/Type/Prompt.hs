{-# LANGUAGE GADTs #-}

module Pawl.Type.Prompt where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Type.Action (Action)
import Pawl.Type.Cost (Cost)
import Pawl.Type.Decider (Decider)
import Pawl.Type.EntryOption (EntryOption)
import Pawl.Type.ModeIndex (ModeIndex)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.Subtype (Subtype)

data Prompt r where
  ChooseAction :: Decider -> PlayerId -> [Action] -> Prompt Action
  Shuffle :: [ObjectId] -> Prompt [ObjectId]
  -- CR 514.2. The [ObjectId] is the hand; the Natural is how many to discard.
  ChooseDiscard :: Decider -> PlayerId -> [ObjectId] -> Natural -> Prompt [ObjectId]
  -- CR 508.1. The [ObjectId] is the legal attackers; the answer is which of them
  -- attack. Whom they attack is not asked: M1b has exactly one opponent and no
  -- planeswalkers, so there is nothing to choose (#59).
  DeclareAttackers :: Decider -> PlayerId -> [ObjectId] -> Prompt [ObjectId]
  -- CR 509.1. The legal blockers, then the attackers they may block. The answer
  -- maps each blocking creature to the attacker it blocks.
  DeclareBlockers :: Decider -> PlayerId -> [ObjectId] -> [ObjectId] -> Prompt (Map ObjectId ObjectId)
  -- CR 510.1 / 702.19b: the attacker divides its power among the legal recipients.
  -- The Map is recipient -> lethal threshold (blockers -> lethal, the defender ->
  -- 0); trample-ness is entirely in whether the defender is a key and what the
  -- thresholds are. Not asked when the division is forced (single blocker, no
  -- excess). Validation is Damage.legalAssignment. See the M2c spec, section 4.
  AssignCombatDamage :: Decider -> PlayerId -> ObjectId -> Map Recipient Natural -> Natural -> Prompt (Map Recipient Natural)
  -- CR 601.2c. One legal-recipient set per named slot of the spell being cast
  -- (the ObjectId); the answer fills every slot. Slots agree by NAME, never by
  -- position. Not asked when the spell has no slots: zero slots is no choice
  -- at all, and where the rules leave nothing to ask, don't prompt.
  ChooseTargets :: Decider -> PlayerId -> ObjectId -> Map SlotName (Set Recipient) -> Prompt (Map SlotName Recipient)
  -- CR 612 / the D4 binding: choose the two basic land types for a text-changing
  -- spell's slot (Magical Hack: "one basic land type" -> "another"). Bound at cast
  -- alongside ChooseTargets; the legal set is always the five basics, so unlike a
  -- target it never gates castability, which is what makes cast-vs-resolution
  -- timing indistinguishable here (#60).
  ChooseBasicLandTypes :: Decider -> PlayerId -> ObjectId -> SlotName -> Prompt (Subtype, Subtype)
  -- CR 701.23 / 701.23b: the [ObjectId] is the library cards MATCHING the
  -- criterion (the engine pre-filters to legal choices); Nothing is "fail to
  -- find," always permitted for a search of one's own library for a quality.
  SearchLibrary :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
  -- The re-entrant cast opportunity during a library search (Panglacial Wurm).
  -- The [ObjectId] is the searcher's library cards castable-while-searching (the
  -- engine pre-filters to permitted, affordable, fillable). Nothing = decline /
  -- done. Offered in a loop before the search finds (per the ruling), so multiple
  -- copies may be cast. CR 605.3a permits mana activation to pay.
  CastWhileSearching :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
  -- CR 601.2b: choose the value of X while casting (the ObjectId is the spell).
  -- Any Natural; payment (reject-not-repair) rejects an unaffordable choice, so
  -- the engine computes no maximum. Prompted before targets (CR 601.2b precedes
  -- 601.2c), and only when the cost contains a Variable symbol -- a spell with no
  -- {X} is not asked (where the rules leave nothing to choose, don't prompt).
  ChooseX :: Decider -> PlayerId -> ObjectId -> Prompt Natural
  -- CR 601.2b / 700.2a: choose the mode(s) while casting (the ObjectId is the
  -- spell). The Set ModeIndex is the LEGAL modes -- the engine pre-filters to modes
  -- whose targets are all fillable (CR 700.2a). The Natural is how many to choose.
  -- The answer is the chosen subset. Prompted before X and targets, and ONLY when
  -- #legal > count; a forced selection is not asked.
  ChooseModes :: Decider -> PlayerId -> ObjectId -> Set ModeIndex -> Natural -> Prompt (Set ModeIndex)
  -- CR 707.9a / 614.1c / 614.12a: as an object enters AS A COPY (Clone), its
  -- controller chooses which permanent to copy. The ObjectId is the entering
  -- object; the [ObjectId] is the legal copy targets (battlefield creatures other
  -- than itself; the engine pre-filters). Nothing is the "may" decline (CR 707.9a
  -- lets Clone enter as itself, a 0/0). Answered inside the zone change that puts
  -- the object onto the battlefield (CR 614.12a), before the enters event is
  -- recorded -- the choice really is made as the object enters. The legal set
  -- excludes anything entering in the same batch (CR 614.12a: a sibling
  -- entering at the same time is not yet "on the battlefield" when the choice
  -- is made; see Pawl.Replacement's applyReplacementsIn).
  ChooseCopyTarget :: Decider -> PlayerId -> ObjectId -> [ObjectId] -> Prompt (Maybe ObjectId)
  -- CR 208.2b / 614.1c: as an object enters, its controller chooses among the
  -- shapes an "as this creature enters, it becomes your choice of ..." ability
  -- offers (Primal Plasma). The ObjectId is the entering object; the answer is an
  -- index into the offered list.
  --
  -- The chosen shape is written into the object's COPIABLE snapshot (CR 707.2), so
  -- a later Clone copies the choice without any further machinery -- and then, if
  -- it copied the ABILITY too, makes its own choice on top (CR 616.2).
  --
  -- Asked only when two or more options are offered; one option is not a choice.
  ChooseEntryOption :: Decider -> PlayerId -> ObjectId -> [EntryOption] -> Prompt Natural
  -- CR 603.3b: "If a player controlled two or more triggered abilities ... that
  -- player puts them on the stack in any order they choose." The [ObjectId] is
  -- that player's pending triggers, each entry its SOURCE object, in the engine's
  -- canonical order; the answer is a permutation of the entry INDICES, giving the
  -- order they are PUT ON THE STACK (so the last named resolves first).
  --
  -- Positional by necessity, unlike a target slot: each entry carries only its
  -- SOURCE, no ability discriminator. That is CONTINGENT, not a rules property:
  -- it holds only while no single source can have two DISTINCT abilities
  -- triggered in the same batch (two triggers from the SAME ability on one
  -- source really are indistinguishable, and any permutation among those is
  -- equivalent). It holds today only as an accident of the settle schedule --
  -- Sarcomancy already carries two triggered abilities (an ETB and an upkeep
  -- trigger), but they cannot co-trigger because the step event that would fire
  -- the upkeep trigger is always scanned before any spell can resolve to place
  -- Sarcomancy and fire its ETB in the same batch. A source with two distinct
  -- abilities triggered together makes two different abilities identical entries
  -- on the wire while their order genuinely matters, and the payload would need
  -- an ability discriminator alongside the source (#61). Asked ONLY when the player controls two
  -- or more -- with one there is nothing to choose, and where the rules leave
  -- nothing to ask, don't prompt. CR 603.3b's TWO-PART process (first the
  -- triggers whose condition is not another ability triggering, then the rest)
  -- is vacuous while no condition triggers on another ability triggering; this
  -- carries the note, not the machinery.
  OrderTriggers :: Decider -> PlayerId -> [ObjectId] -> Prompt [Natural]
  -- CR 616.1: with two or more applicable replacement or prevention effects in
  -- the highest non-empty bucket, the affected object's controller (or its owner,
  -- or the affected player) chooses which to apply NEXT -- and then the process
  -- repeats over what is applicable now (616.1f), so this is asked once per
  -- iteration, not once per event. The [ObjectId] is each candidate's SOURCE, in
  -- the engine's canonical order (battlefield ascending, then the floating
  -- store); the answer is an index into it.
  --
  -- Positional, and carrying exactly the caveat #61 records for OrderTriggers: a
  -- source with two DISTINCT applicable replacement abilities would put two
  -- different effects on the wire as identical entries. That is reachable in a way
  -- it is not for triggers -- Doubling Season has two replacement abilities -- but
  -- they are in different EVENT CLASSES and so are never candidates for the same
  -- event. A single source with two same-class applicable replacements needs a
  -- discriminator alongside the source (#74).
  --
  -- Asked ONLY when the bucket holds two or more candidates that are not all equal
  -- as values: with one there is nothing to choose, and among equal values every
  -- order yields the same board (each still gets its own CR 614.5 opportunity).
  ChooseReplacement :: Decider -> PlayerId -> [ObjectId] -> Prompt Natural
  -- CR 701.21a: which permanents to sacrifice to pay a cost. The ObjectId is the
  -- spell being cast or the permanent whose ability is being activated; the
  -- [ObjectId] is the payer's permanents matching the component's criterion (the
  -- engine pre-filters, in ascending order); the Natural is how many. The answer
  -- is a Set because one permanent cannot be sacrificed twice for one payment.
  --
  -- Deliberately NOT Prompt.ChooseTargets or the TargetSpec machinery: CR 115.1
  -- makes a target only what the word "target" names, and conflating the two
  -- would let shroud, hexproof and "becomes the target" triggers observe a
  -- sacrifice choice. Its shape mirrors ChooseDiscard (candidates plus a count).
  --
  -- Asked ONLY when there is a choice -- more candidates than the count. Exactly
  -- as many as the count is forced, and where the rules leave nothing to ask,
  -- don't prompt.
  ChooseSacrifices :: Decider -> PlayerId -> ObjectId -> [ObjectId] -> Natural -> Prompt (Set ObjectId)
  -- CR 601.2b: "If the spell has alternative or additional costs that will be
  -- paid as it's being cast ... the player announces their intentions to pay any
  -- or all of those costs." Issued after the modes and before X and targets, at
  -- 601.2b's own position. The ObjectId is the spell; the [Cost] is the PAYABLE
  -- candidates (the engine pre-filters: each candidate from Pawl.Cost.costsFor,
  -- run through total, then tested with canPay at the CR 601.2b X=0 floor).
  --
  -- CR 118.9b makes an alternative cost optional, so a player who can afford both
  -- is genuinely choosing. Asked ONLY when two or more candidates are payable;
  -- one is forced, and where the rules leave nothing to ask, don't prompt.
  ChooseCost :: Decider -> PlayerId -> ObjectId -> [Cost] -> Prompt Cost
