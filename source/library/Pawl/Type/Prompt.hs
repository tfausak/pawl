{-# LANGUAGE GADTs #-}

module Pawl.Type.Prompt where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Type.Action (Action)
import Pawl.Type.Decider (Decider)
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
  -- planeswalkers, so there is nothing to choose. EXPIRES at multiplayer.
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
  -- target it never gates castability. Cast-vs-resolution timing is elided as
  -- indistinguishable (spec §3), expiry named there.
  ChooseBasicLandTypes :: Decider -> PlayerId -> ObjectId -> SlotName -> Prompt (Subtype, Subtype)
  -- CR 701.23 / 701.23b: the [ObjectId] is the library cards MATCHING the
  -- criterion (the engine pre-filters to legal choices); Nothing is "fail to
  -- find," always permitted for a search of one's own library for a quality.
  SearchLibrary :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
