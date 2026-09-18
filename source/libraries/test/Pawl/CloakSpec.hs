{-# LANGUAGE GADTs #-}

-- Covers: CR 701.58 CLOAK -- Pawl.Engine.Cloak, Effect.Cloak's arm in
-- Pawl.Engine.Resolve.Effect, and CR 701.58b's procedure in
-- Pawl.Engine.FaceDown.
--
-- Ransom Note ({1} Artifact -- Clue, "{2}, Sacrifice this artifact: Choose one
-- -- Cloak the top card of your library; ...") is the fixture, activated on the
-- mode that cloaks.
--
-- THE BOARD SHAPE that makes the cases discriminating. The cloaked card is Hill
-- Giant, a 3\/3 for {3}{R}: every number rule 701.58a and rule 701.58b state --
-- the 2\/2, the ward {2}, the mana cost that buys it back -- differs from the
-- card's own, so an assertion below cannot pass on the printed values. A Goblin
-- Piker sits under it in the library, so "the TOP card" is a real claim.
module Pawl.CloakSpec where

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as View
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.FaceDownState as FaceDownState
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.Ward as Ward
import qualified Pawl.Types.Zone as Zone

-- CR 701.58a's listing, written as the rule writes it rather than read off the
-- engine's value, so the assertion is about rule 701.58a and not about
-- FaceDownCharacteristics.disguisedValue agreeing with itself.
ward2 :: Keyword.Keyword
ward2 = Keyword.Ward (Ward.MkWard (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []) Nothing)

-- Take this mode of the ability being activated. PINNED by index rather than
-- searched for, so a mutation cannot be repaired by an answerer that goes
-- looking for whichever mode still works.
choosingMode :: Natural.Natural -> Prompt.Prompt r -> r
choosingMode index p = case p of
  Prompt.ChooseModes {} -> Seq.singleton (ModeIndex.MkModeIndex index)
  _ -> S.identityAnswer p

-- alice: six Mountains -- two for the activation, four for CR 701.58b's {3}{R}
-- -- a Ransom Note on the battlefield, and a library whose top card is Hill
-- Giant with a Goblin Piker beneath it.
--
-- The board is returned beside the result of activating the cloak mode, so
-- every case below can ask what was true before the ability resolved.
cloaked :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Natural.Natural -> m (GameState.GameState, GameState.GameState)
cloaked s registry mode = do
  note <- S.printingOf s registry "Ransom Note"
  mountain <- S.printingOf s registry "Mountain"
  giant <- S.printingOf s registry "Hill Giant"
  piker <- S.printingOf s registry "Goblin Piker"
  let (noteId, g1) = S.addPermanent note S.alice (S.landsFor mountain S.alice 6 (Setup.emptyGame S.bothPlayers))
      (_, g2) = S.addLibraryCard piker S.alice g1
      (_, before) = S.addLibraryCard giant S.alice g2
      after = case Activate.abilitiesFor noteId before of
        [ability] -> S.runPure (choosingMode mode) before (Activate.activateAbility S.alice noteId ability >> Stack.resolveTop)
        _ -> before
  Spec.assertEqWith s "Ransom Note states exactly one activated ability" (length (Activate.abilitiesFor noteId before)) 1
  pure (before, after)

-- How many of alice's objects in this zone are copies of a card with this name.
-- Zone-scoped, unlike S.countByName, because the cases below turn on WHICH zone
-- the card ended up in.
countIn :: Zone.Zone -> CardName.CardName -> GameState.GameState -> Int
countIn zone wanted gs =
  length (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just wanted) (Game.zoneMembers zone S.alice gs))

-- The one permanent alice owns that the ability put onto the battlefield: the
-- arrival that was not there before. Ransom Note itself left as its own cost was
-- paid, so the answer is the cloaked card or nothing at all.
arrival :: GameState.GameState -> GameState.GameState -> Maybe ObjectId.ObjectId
arrival before after =
  List.find (`notElem` Game.zoneMembers Zone.Battlefield S.alice before) (Game.zoneMembers Zone.Battlefield S.alice after)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Cloak" $ do
  Spec.it s "CR 701.58a the top card of the library becomes a face-down 2/2 with ward {2} on the battlefield" $ do
    giant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    (before, after) <- cloaked s registry 0
    case arrival before after of
      Nothing -> Spec.assertFailure s "the cloak put nothing onto the battlefield"
      Just permanent -> do
        -- THE gameplay reading, and first: a 2/2 rather than Hill Giant's
        -- printed 3/3, which is CR 701.58a's own listing applied.
        Spec.assertEqWith s "CR 701.58a a 2/2, not the printed 3/3" (S.powerToughnessOf permanent after) (Just (2, 2))
        Spec.assertBool s (Projection.hasKeyword ward2 permanent after) "CR 701.58a with ward {2}, which is the whole of what cloak adds to manifest"
        Spec.assertEqWith
          s
          "CR 701.58a face down, cloaked"
          (fmap Object.facing (Game.lookupObject permanent after))
          (Just (Facing.FaceDown FaceDownState.MkFaceDownState {FaceDownState.reason = FaceDownReason.Cloaked, FaceDownState.listed = FaceDownCharacteristics.disguisedValue}))
        Spec.assertEqWith s "CR 701.58a and alice controls it" (View.controllerOf permanent after) (Just S.alice)
        -- THE TOP card and no other: the Piker beneath it is where it was, and
        -- the library is one card shorter.
        Spec.assertEqWith s "the Hill Giant left the library" (countIn Zone.Library (S.printingName giant) after) 0
        Spec.assertEqWith s "and it was there before" (countIn Zone.Library (S.printingName giant) before) 1
        Spec.assertEqWith s "while the card beneath it stayed" (countIn Zone.Library (S.printingName piker) after) 1

  -- The paired control, differing in ONE thing: the same Ransom Note on the same
  -- board, activated on the mode that draws instead. Nothing is cloaked, so the
  -- case above cannot be passing on something the fixture does.
  Spec.it s "the draw mode of the same ability cloaks nothing" $ do
    giant <- S.printingOf s registry "Hill Giant"
    (before, after) <- cloaked s registry 2
    Spec.assertEqWith s "CR 121.1 the Hill Giant was drawn rather than cloaked" (countIn Zone.Hand (S.printingName giant) after) 1
    Spec.assertEqWith s "so nothing arrived on the battlefield" (arrival before after) Nothing

  Spec.it s "CR 701.58b the cloaked permanent turns face up for the card's mana cost, and no other procedure is open" $ do
    (before, after) <- cloaked s registry 0
    case arrival before after of
      Nothing -> Spec.assertFailure s "the cloak put nothing onto the battlefield"
      Just permanent -> do
        -- ONE procedure and not four: CR 702.37e's and CR 702.168d's ask for
        -- abilities Hill Giant has not got, and CR 701.40b's asks about the
        -- allower, which was not manifest.
        Spec.assertEqWith s "CR 701.58b only the cloak procedure is open" (FaceDown.turnableFaceUp S.alice after) [(permanent, TurnUpProcedure.Cloak)]
        Spec.assertBool s (notElem (Action.Type.TurnFaceUp permanent TurnUpProcedure.Manifest) (Action.legalActions S.alice after)) "CR 701.40b and the manifest procedure is not offered on a cloaked permanent"
        let up = S.runPure S.identityAnswer after (FaceDown.turnFaceUp S.manaPerformer S.alice TurnUpProcedure.Cloak permanent)
        -- CR 708.8: the effect defining its characteristics ends, so the printed
        -- 3/3 answers again.
        Spec.assertEqWith s "CR 701.58b it regains its normal characteristics: the printed 3/3" (S.powerToughnessOf permanent up) (Just (3, 3))
        -- The PRICE, which is what tells rule 701.58b's procedure from a free
        -- turn-over: two Mountains paid for the activation and four more for
        -- {3}{R}.
        Spec.assertEqWith s "the activation tapped two lands and no more" (S.tappedCount S.alice after) 2
        Spec.assertEqWith s "CR 701.58b four more went down for {3}{R}" (S.tappedCount S.alice up) 6
