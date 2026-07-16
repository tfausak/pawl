{-# LANGUAGE GADTs #-}

module Pawl.Type.Prompt where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Type.Action (Action)
import Pawl.Type.Decider (Decider)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

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
  -- CR 510.1c: a creature blocked by TWO OR MORE creatures divides its damage
  -- among them freely. Never asked for a single blocker -- CR 510.1c forces all
  -- the damage onto it, and asking would invent a decision the rules do not
  -- offer. The ObjectId is the attacker; the Natural is its power.
  AssignCombatDamage :: Decider -> PlayerId -> ObjectId -> Set ObjectId -> Natural -> Prompt (Map ObjectId Natural)
