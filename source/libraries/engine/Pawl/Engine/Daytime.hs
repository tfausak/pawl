-- | CR 731, "Day and Night": the game's day/night designation, and the daybound
-- and nightbound keyword abilities (CR 702.145) that both read it and change it.
--
-- Pawl.Engine.Monarch's sibling, and for the same reason: rule 731 gives the GAME
-- a designation that rides no card's text, so the rules core holds it. Casing on
-- Keyword.Daybound here is casing on the RULEBOOK, which Pawl.Types.Keyword's own
-- comment licenses -- rule 702 is as much a part of the comprehensive rules as
-- rule 502 is. Nothing here asks which EFFECT anything came from, which is the
-- invariant that matters: Pawl.Engine.Resolve's Effect.ItBecomes arm calls
-- `becomes` and this module never learns what asked.
--
-- WHERE the designation lives follows CR 731.1's own words, "designations that
-- THE GAME ITSELF can have", so it is a GameState field exactly as CR 725.1's
-- monarch is -- and not a permanent's, which is what CR 701.54b makes the
-- Ring-bearer and why that one rides Object.ringBearerFor. Pawl.Engine.Ring's
-- haddock is the worked precedent for choosing between the two.
--
-- Three separate moments can change it, and all three are here:
--
--   * an effect saying so (CR 731.1), through `becomes`
--   * the untap step's turn-based action (CR 502.2 / 731.2), through `untapCheck`
--   * a player merely CONTROLLING a daybound or nightbound permanent while it is
--     neither day nor night (CR 702.145d / 702.145g), through `settle`
--
-- CR 702.145b's and CR 702.145e's last static ability points the other way -- it
-- changes nothing here and instead forbids a transform elsewhere -- so it is
-- `restrictsTransform`, a predicate Pawl.Engine.Resolve reads rather than
-- anything this module performs.
module Pawl.Engine.Daytime where

import qualified Control.Monad.Trans.State.Strict as State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.Daytime (Daytime)
import qualified Pawl.Types.Daytime as Daytime
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ProjectedCharacteristics as PC

-- | CR 702.145: does this permanent have daybound, or nightbound, as the layer
-- fold leaves it?
--
-- The PROJECTED keywords, never the printed ones, because the layer system grants
-- and removes abilities -- a daybound creature under Humility (CR 613.1f) has no
-- daybound ability left to turn it over, and an effect that GRANTED one would be
-- found here. Pawl.Engine.Speed's CR 704.5aa read has the same posture.
--
-- MEMBERSHIP, not a count: both rules ask whether a permanent HAS the keyword, so
-- a second instance turns nothing over twice.
hasKeyword :: Keyword.Keyword -> Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> Bool
hasKeyword kw pcs oid = case Map.lookup oid pcs of
  Nothing -> False
  Just pc -> Map.member kw (PC.keywords pc)

-- | CR 702.145b's third static ability -- "this permanent can't transform except
-- due to its daybound ability" -- and CR 702.145e's second, its nightbound
-- mirror. True when this permanent refuses a transform.
--
-- Stated as a question about the PERMANENT rather than about the instruction,
-- because that is how both rules are worded and because it is what makes the
-- exception fall out for free: the transform the rules DO permit is CR 702.145c's
-- and CR 702.145f's own sweep, `turnDue` below, which reaches
-- Pawl.Engine.Game.turnFaceOver directly and never asks this. Every other road to
-- a turn goes through an instruction, and Pawl.Engine.Resolve's `turnOver` -- the
-- instruction-level wrapper -- is the one caller.
--
-- Daybound and nightbound together, in one predicate, because the two rules
-- forbid the same thing in the same words and neither one's clause reads the
-- designation or the face: CR 702.145c and CR 702.145f do that, and they are
-- `dueToTurn` below. Which of the pair a permanent has decides which of THOSE
-- fires, and decides nothing here.
--
-- The PROJECTED keywords, `hasKeyword` above, for that function's own reason: a
-- permanent stripped by Humility (CR 613.1f) has no daybound ability left, so it
-- has no restriction either -- the restriction IS part of the ability. A GRANTED
-- daybound brings its restriction with it by the same read.
restrictsTransform :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> Bool
restrictsTransform pcs oid =
  hasKeyword Keyword.Daybound pcs oid || hasKeyword Keyword.Nightbound pcs oid

-- | CR 702.145c and CR 702.145f: the permanents that must turn over right now,
-- given the designation the game has.
--
--   * CR 702.145c -- "any time a player controls a permanent that is front face
--     up with daybound and it's night, that player transforms that permanent"
--   * CR 702.145f -- the mirror, a back-face-up nightbound permanent while it is
--     day
--
-- Which face is up is the whole difference between the two, and it is why this
-- cannot be written as one rule about "the wrong designation": a permanent has
-- daybound on its front face and nightbound on its back, so after the turn the
-- OTHER keyword is the one it has, and that one is satisfied. That is what stops
-- this sweeping the same permanent back and forth.
--
-- Neither day nor night turns nothing over -- both rules name a designation, and
-- CR 702.145d/g handle that state instead (`settle` below).
--
-- Ascending, so the sweep's writes are deterministic.
dueToTurn :: GameState -> [ObjectId]
dueToTurn gs = case GameState.daytime gs of
  Nothing -> []
  Just designation ->
    let pcs = Projection.projectAll gs
        due oid = case designation of
          Daytime.Night -> hasKeyword Keyword.Daybound pcs oid && Game.isFrontFaceUp oid gs
          Daytime.Day -> hasKeyword Keyword.Nightbound pcs oid && not (Game.isFrontFaceUp oid gs)
     in filter due (Set.toAscList (GameState.battlefield gs))

-- | CR 702.145c/f, performed: turn every permanent `dueToTurn` names over, and
-- report whether any turned.
--
-- "That player transforms that permanent" names the controller as the actor and
-- gives them nothing to decide, so nobody is prompted -- the engine makes no
-- choice here because the rule leaves none. Both rules add "this happens
-- immediately and isn't a state-based action", which is why `becomes` calls this
-- itself rather than leaving it to the settle.
--
-- ONE timestamp for the whole sweep: a single nightfall turns every daybound
-- permanent over at the same moment, so a later CR 701.27f comparison must not be
-- able to tell them apart. CR 701.27f is not otherwise consulted -- it gates an
-- instruction from an ability on the STACK, and this turn comes from a static
-- ability.
--
-- Runs to a fixed point by construction rather than by looping: turning a
-- permanent over swaps which of the pair of keywords it has, so no permanent this
-- sweep touches is still due afterwards.
--
-- CR 702.145b's FIRST static ability -- "if it is night and this permanent is
-- represented by a double-faced card, it enters transformed" -- is NOT this
-- sweep, and the two must not be confused. That one is CR 712.13a's replacement
-- effect (EntryRewrite.EntersTransformed, minted by
-- Pawl.Engine.Keyword.mintedReplacementsFor), applied as the permanent enters.
-- This sweep is CR 702.145c and CR 702.145f, and reaches only permanents already
-- on the battlefield. Pawl.DaytimeSpec's entrySpec proves the pair apart, on the
-- face the permanent shows when its spell finishes resolving.
--
-- This is the ONE turn CR 702.145b's and CR 702.145e's transform restriction
-- permits, and it reaches Game.turnFaceOver DIRECTLY for exactly that reason:
-- `restrictsTransform` above gates Pawl.Engine.Resolve's instruction-level
-- `turnOver`, which this does not go through. Pawl.DaytimeSpec's
-- restrictionSpec proves the pair -- a spell's transform is refused, the sweep's
-- is not.
--
-- CR 701.27a's event is recorded through the `record` argument rather than
-- inline, and the argument is there because of the module graph rather than
-- because two callers want two answers: Pawl.Engine.Event owns recordEvent and
-- imports THIS module, so the modules above pass Event.recordTransformed down
-- instead.
-- Every road this module has to a turn takes it, so the CR 702.145c/f sweep is
-- not a second, silent way to turn a permanent over.
--
-- Proved at gameplay level by Pawl.TransformSpec's nightfall case, whose fixture
-- is Howlpack Piper // Wildsong Howler: its back face prints "whenever this
-- creature enters or transforms into Wildsong Howler", and the permanent is
-- placed rather than cast so only the transform limb can fire. Removing the
-- record here reddens that case's hand assertion and leaves every case in
-- Pawl.DaytimeSpec green, the one asserting the permanent turned over included:
-- the record is what is lost, not the turn.
turnDue :: ([ObjectId] -> GameState -> GameState) -> Game Bool
turnDue record = do
  gs <- State.get
  case dueToTurn gs of
    [] -> pure False
    due -> do
      let (now, g1) = Game.freshTimestamp gs
          turned = foldr (Game.turnFaceOver now g1) (GameState.objects g1) due
      State.put (record (Game.facesTurned (GameState.objects g1) turned due) g1 {GameState.objects = turned})
      pure True

-- | CR 731.1: "it becomes day" / "it becomes night" -- the game gains that
-- designation. CR 731.1a's "day becomes night" is the same act stated from the
-- other side, the game losing the first designation and gaining the second, which
-- is one assignment because CR 731.1 allows exactly one designation at a time.
--
-- Gaining a designation the game ALREADY has is not a change: rule 731.1a
-- describes becoming the other one, and nothing in rule 731 says a game that is
-- already night becomes night again. So this is idempotent, which is what lets
-- CR 702.145d's "any time" check below call it on every settle without the CR
-- 702.145c sweep firing on every settle too.
--
-- The transform is part of THIS act, not of a later check: CR 702.145c and CR
-- 702.145f both say it "happens immediately and isn't a state-based action", so
-- an effect that goes on to do something else after saying "it becomes night"
-- (Tovolar, Dire Overlord's upkeep trigger) sees the turned permanent.
becomes :: ([ObjectId] -> GameState -> GameState) -> Daytime -> Game Bool
becomes record designation = do
  gs <- State.get
  if GameState.daytime gs == Just designation
    then pure False
    else do
      State.put gs {GameState.daytime = Just designation}
      _ <- turnDue record
      pure True

-- | CR 702.145d and CR 702.145g, the two rules that get a game to a designation
-- in the first place, plus the CR 702.145c/f sweep they and any other change
-- leave owing. Reports whether it changed anything, so the CR 117.5 settle loop
-- knows to run again.
--
--   * CR 702.145d -- "any time a player controls a permanent with daybound, if
--     it's neither day nor night, it becomes day"
--   * CR 702.145g -- "any time a player controls a permanent with nightbound, if
--     it's neither day nor night AND THERE ARE NO PERMANENTS WITH DAYBOUND on the
--     battlefield, it becomes night"
--
-- Daybound wins outright where both are on the battlefield, which is CR 702.145g's
-- own extra clause rather than an ordering choice -- and it is checked over the
-- WHOLE battlefield, not over one player's permanents, because that is the
-- clause's own wording.
--
-- Both rules are dead once the game HAS a designation: each names "neither day
-- nor night", and CR 731.1's last sentence makes that state unreachable
-- afterwards. So the pair fires at most once per game.
--
-- CR 702.145g's own arm is unreachable by the current pool rather than untested
-- by choice: CR 702.145e puts nightbound on the BACK face of a card whose front
-- face has daybound, so a battlefield holding a nightbound permanent holds a
-- daybound one -- the same permanent -- and CR 702.145d has already answered. A
-- card printing nightbound on a face that can enter first would reach it.
--
-- Run in the CR 117.5 settle rather than at each event, on Pawl.Engine.Engine's
-- standing terms: CR 704.3 makes "whenever a player would get priority" the
-- coarsest moment at which a continuously-checked condition can be observed, so a
-- check there is indistinguishable from a check at every moment. The sweep half
-- is also reachable from `becomes` directly, which is what covers the one case
-- the settle is too coarse for -- a permanent turned over mid-resolution.
--
-- ONE board projection per settle pass, never two: the designation is asked
-- FIRST, and the two branches are exclusive -- a game that has a designation
-- pays only `turnDue`'s projection, and a game that has neither pays only the
-- keyword scan's, `dueToTurn` having nothing to look for until it does.
--
-- That projection is paid on every settle pass of every game, werewolves or not,
-- which is the price of reading the keyword off the LAYER FOLD rather than off
-- the printed card. The alternative -- scanning printed keywords first and
-- projecting only if one is found -- would be exact today only because nothing
-- in the pool grants daybound, and that is a claim about the card pool rather
-- than about the rules.
settle :: ([ObjectId] -> GameState -> GameState) -> Game Bool
settle record = do
  gs <- State.get
  case GameState.daytime gs of
    Just _ -> turnDue record
    Nothing ->
      let pcs = Projection.projectAll gs
          onBattlefield = Set.toList (GameState.battlefield gs)
       in if any (hasKeyword Keyword.Daybound pcs) onBattlefield
            then becomes record Daytime.Day
            else
              if any (hasKeyword Keyword.Nightbound pcs) onBattlefield
                then becomes record Daytime.Night
                else pure False

-- | CR 502.2 (stated again as CR 731.2): as the second part of the untap step,
-- the game checks the turn just ended and may change the designation.
--
--   * CR 502.2's first sentence -- day, and the previous turn's active player
--     cast no spells during it, becomes night
--   * its second -- night, and that player cast two or more, becomes day
--   * its third -- neither, and the check doesn't happen at all
--
-- The count is GameState.spellsCastLastTurn, snapshotted at the handoff, because
-- the event log this would otherwise be folded from is cleared there and this
-- runs afterwards. That scalar and not GameState.castsLastTurn beside it: rule
-- 731.2 names exactly one player where a CARD asks about every player, and
-- Engine.beginTurnOf reads the scalar out of that map, so the two agree about the
-- one seat this rule is about.
--
-- A TURN-BASED ACTION that does not use the stack, so it is performed inline by
-- Pawl.Engine.Engine's untap arm and nothing is put anywhere.
--
-- CR 502.2a's shared-team-turns variant is not implemented, because pawl has no
-- teams (#175).
untapCheck :: ([ObjectId] -> GameState -> GameState) -> Game Bool
untapCheck record = do
  gs <- State.get
  case GameState.daytime gs of
    Nothing -> pure False
    Just Daytime.Day -> if GameState.spellsCastLastTurn gs == 0 then becomes record Daytime.Night else pure False
    Just Daytime.Night -> if GameState.spellsCastLastTurn gs >= 2 then becomes record Daytime.Day else pure False
