-- CR 702.185b's designation in the one voice the rest of the engine cannot
-- supply for itself: the stamp that makes an exiled card a WARPED one. Rule
-- 702.185b defines it by provenance -- "a warped card in exile is one that was
-- exiled by the delayed triggered ability created by a warp ability" -- so the
-- one route into this stamp is Effect.MakeWarped, which is the second effect of
-- that ability (Pawl.Engine.Keyword.warpExile), and its arm sits in
-- Pawl.Engine.Resolve and calls becomeWarped here.
--
-- The rule's other half lives where every other casting question does. CR
-- 702.185a's permission -- "its owner may cast this card after the current turn
-- has ended for as long as it remains exiled" -- is read by
-- Pawl.Engine.Cast.permitsCastWarped off the Object.warped stamp this module
-- writes, and priced by Pawl.Engine.Cost.candidateCostsGiven's `_` arm, that
-- permission stating no cost of its own.
--
-- Pawl.Engine.Plot is the same module one designation over and this one is
-- written to its shape. No event is recorded beside the stamp, where rule
-- 702.170a's route records GameEvent.Plotted: nothing triggers on a card
-- becoming warped, rule 702.185b stating a designation and no trigger condition.
module Pawl.Engine.Warp where

import qualified Data.Map.Strict as Map
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)

-- CR 702.185b: "it becomes a warped card", stamped with the turn the warp
-- ability's delayed trigger exiled it on -- which is what CR 702.185a's "after
-- the current turn has ended" is compared against.
--
-- Read AFTER the move, Pawl.Engine.Plot.stamp's reading: nothing in a zone change
-- ends a turn, so the two numbers agree, and reading the board the stamp is
-- written to is what keeps them from drifting apart if one ever could.
--
-- Takes an ObjectId and nothing else, Pawl.Engine.Plot's invariant: it never asks
-- which CARD is being warped, so the opcode's arm hands it CR 400.7's exiled
-- incarnation rather than the permanent that left the battlefield.
becomeWarped :: ObjectId -> GameState -> GameState
becomeWarped newId gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.warped = Just (GameState.turnNumber gs)}) newId (GameState.objects gs)
    }
