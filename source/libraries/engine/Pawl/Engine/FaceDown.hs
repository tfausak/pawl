-- Rule 708 in the one voice the rest of the engine cannot supply for itself: CR
-- 116.2b's special action that turns a face-down permanent face up.
--
-- Everything ELSE rule 708 says is arranged so that no module has to know this
-- one exists. CR 708.2's substitution lives at Pawl.Engine.Game.faceOf, so every
-- characteristic read gets it for free; CR 708.3/708.4's turning-over-before-the-
-- move lives at Pawl.Engine.Event.changeZoneFaceDown; CR 708.9's reveal is
-- Object.newIncarnation putting the status back to FaceUp. What is left is an
-- action a player takes, and an action needs a place to be offered from and
-- performed in.
--
-- TWO things this module tells the rest of the engine about, and both are the
-- same fact from opposite sides: turning a permanent face up is not a zone
-- change, so no other funnel sees it, and the abilities that watch for it are
-- ones the permanent only regains as it turns over.
--
-- CR 708.7's EVENT, recorded here because no other funnel would (Skirk
-- Marauder's trigger reads it), and CR 614.1e's PROPOSED EVENT, raised here for
-- the same reason (megamorph's counter rides it). Each is one constructor of
-- an ordinary rules type and never an effect's identity -- Pawl.Engine.Event
-- classifies both like any other.
--
-- TWO PROCEDURES, because CR 708.7's permission belongs to whatever allowed the
-- permanent to be face down and two rules write one: CR 702.37e's, at the morph
-- cost, and CR 701.40b's, at the card's mana cost. CR 701.40c is the case where
-- both are open at once, and the engine offers both rather than picking.
--
-- THE INVARIANT: rules 701.40 and 702.37 are part of the rulebook, so reading
-- Keyword.Morph's cost or a FaceDownReason here is the same closed-half act as
-- reading a Phase. This module never asks which CARD is underneath.
module Pawl.Engine.FaceDown where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import Pawl.Types.TurnUpProcedure (TurnUpProcedure)
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.TypeLine as TypeLine

-- CR 702.37e: "what the permanent's morph cost WOULD BE if it were face up".
-- Nothing when the card underneath has no morph ability, which is the rule's own
-- parenthesis -- "if the permanent wouldn't have a morph cost if it were face
-- up, it can't be turned face up this way".
--
-- Read through Game.faceUpFaceOf, the one door that steps around CR 708.2's
-- substitution, because that substitution is exactly what makes a projected read
-- useless here: a face-down permanent has no keywords at all (CR 708.2a), so
-- Projection.keywordsOf would answer Nothing for every morph creature ever
-- printed. The rule's counterfactual is what licenses it.
--
-- Not a projected read, therefore not affected by Humility: a face-down
-- permanent under a layer-6 ability removal can still be turned face up, since
-- the ability rule 702.37e consults is the one the CARD would have. That is what
-- "would be if it were face up" says, and nothing in the pool observes it.
morphCostOf :: ObjectId -> GameState -> Maybe (Cost Keyword)
morphCostOf oid gs = do
  face <- Game.faceUpFaceOf oid gs
  Keyword.morphCost (Face.keywords face)

-- CR 701.40b: "show all players that the card representing that permanent IS A
-- CREATURE CARD and what THAT CARD'S MANA COST is, pay that cost". Its
-- parenthesis is the two guards below: "if the card representing that permanent
-- isn't a creature card or it doesn't have a mana cost, it can't be turned face
-- up this way."
--
-- Read through Game.faceUpFaceOf for morphCostOf's reason, and the rule words it
-- even more plainly: both guards are about "the CARD representing that
-- permanent" rather than about the permanent, and CR 708.2a has left the
-- permanent itself with no card type and no mana cost at all -- so a projected
-- read would refuse every manifested permanent ever put onto the battlefield.
--
-- The card's own printed types, not its projected ones, for the same reason. CR
-- 701.40b asks what the card IS, which is CR 108.2's question about a card
-- outside the game state rather than CR 613's about a permanent in it.
manifestCostOf :: ObjectId -> GameState -> Maybe (Cost Keyword)
manifestCostOf oid gs = do
  face <- Game.faceUpFaceOf oid gs
  Monad.guard (Set.member CardType.Creature (TypeLine.types (Face.typeLine face)))
  manaCost <- Face.manaCost face
  pure (Cost.Type.MkCost (Just manaCost) [])

-- What one of CR 708.7's two procedures costs on this permanent, or Nothing when
-- that procedure is closed to it. A classification of the two rules, never of a
-- card: which procedure is which is CR 701.40c's own distinction.
costOf :: TurnUpProcedure -> ObjectId -> GameState -> Maybe (Cost Keyword)
costOf procedure oid gs = case procedure of
  TurnUpProcedure.Morph -> morphCostOf oid gs
  TurnUpProcedure.Manifest -> manifestCostOf oid gs

-- CR 116.2b: may this player turn this permanent face up right now, by this
-- procedure? Five conjuncts, each a clause of the rule:
--
--   * it is FACE DOWN (CR 708.2b's mirror -- there is nothing to turn up
--     otherwise);
--   * it is a PERMANENT this player CONTROLS (CR 702.37e's "a face-down
--     permanent you control", CR 701.40b's "a manifested permanent you
--     control"), which is why the battlefield membership and the projected
--     controller are both asked;
--   * the procedure is one this permanent is ELIGIBLE for, which is the only
--     conjunct the two rules disagree on and is `eligible` below;
--   * the procedure's cost exists on the card underneath;
--   * that cost is payable. An action the player cannot take is not offered,
--     which is Pawl.Engine.Action.legalActions' posture for every other action
--     on the menu.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f
-- adjustments: that rule totals the cost of a spell being cast or an ability
-- being activated, and a special action is neither -- the same reading
-- Pawl.Engine.Activate takes of an activation cost (#90).
--
-- The TIMING clause has no conjunct here because the engine is the timing: CR
-- 702.37e's and CR 701.40b's shared "any time you have priority" is satisfied by
-- legalActions being asked only of the priority holder, exactly as CR 116.2a's
-- land play relies on.
canTurnFaceUp :: PlayerId -> TurnUpProcedure -> ObjectId -> GameState -> Bool
canTurnFaceUp pid procedure oid gs =
  let -- CR 701.40b's subject is "a MANIFESTED permanent", so this procedure is
      -- open only to a permanent CR 701.40a turned over; the reason on the status
      -- is how that is known (CR 708.6). Without this guard a morph-CAST creature
      -- could be turned face up for its mana cost, which CR 702.37c/702.37e
      -- allow nowhere.
      --
      -- CR 702.37e's subject is only "a face-down permanent you control WITH A
      -- MORPH ABILITY", which asks about the card and not about the allower --
      -- so that procedure has no reason guard at all, and a permanent Backslide
      -- turned face down is turnable by it. That asymmetry is the rule's, not a
      -- shortcut.
      eligible = case procedure of
        TurnUpProcedure.Morph -> True
        TurnUpProcedure.Manifest ->
          fmap (Facing.reasonOf . Object.facing) (Game.lookupObject oid gs)
            == Just (Just FaceDownReason.Manifested)
   in maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid gs)
        && Projection.controllerOf oid gs == Just pid
        && eligible
        && case costOf procedure oid gs of
          Nothing -> False
          Just cost -> Cost.canPay pid oid cost gs

-- Every way this player may turn a permanent face up right now, in battlefield
-- order -- what Action.TurnFaceUp is built from.
--
-- ONE ENTRY PER PROCEDURE, so a manifested morph card appears twice. That is CR
-- 701.40c in the shape Pawl.Engine.Room.unlockable takes for CR 709.5e's doors:
-- "its controller MAY turn that card face up using EITHER ... OR", two prices,
-- and offering both as legal actions is how the engine declines to choose
-- (docs/design.md's second invariant).
turnableFaceUp :: PlayerId -> GameState -> [(ObjectId, TurnUpProcedure)]
turnableFaceUp pid gs =
  [ (oid, procedure)
  | oid <- Set.toAscList (GameState.battlefield gs),
    procedure <- [TurnUpProcedure.Morph, TurnUpProcedure.Manifest],
    canTurnFaceUp pid procedure oid gs
  ]

-- CR 702.37e and CR 701.40b, in the order both rules share: show all players
-- what the procedure's cost is, pay it, then turn the permanent face up. ONE
-- function for both, because everything after the payment is the same game
-- action -- the two rules differ only in what they showed and what they charged,
-- which is `costOf` and nothing else.
--
-- The SHOWING is not modelled. Nothing in pawl hides a face-down permanent's
-- card from a reader in the first place, so there is no concealment for a reveal
-- to lift (#682).
--
-- REJECT-NOT-REPAIR, the posture Cast.castSpell and Activate.activateAbility
-- both take: a payment that fails restores the state from before it was
-- attempted and the permanent stays face down. The rules' order is what makes
-- that correct rather than merely tidy -- the cost is paid BEFORE the permanent
-- turns over, so a failed payment has turned nothing over to undo.
--
-- CR 708.8 falls out of the shape and is not implemented anywhere: "any effects
-- that have been applied to the face-down permanent still apply to the face-up
-- permanent", and this writes one status field on one object -- no CR 400.7
-- incarnation is minted, so damage, counters, attachments, Auras, the CR 613.7d
-- timestamp and every continuous effect naming the object ride through
-- untouched. Its last sentence is the same non-event: nothing here is a
-- battlefield entry, so no enters-the-battlefield ability is offered one.
--
-- CR 708.11 is the one thing here that is NOT a bare write: "if a face-down
-- permanent would have an 'As [this permanent] is turned face up . . .' ability
-- after it's turned face up, that ability is applied WHILE that permanent is
-- being turned face up, NOT AFTERWARD". That is why the CR 616.1 loop runs
-- between the two lines below rather than after both -- see the note at the
-- call.
turnFaceUp :: PlayerId -> TurnUpProcedure -> ObjectId -> Game ()
turnFaceUp pid procedure oid = do
  before <- State.get
  if not (canTurnFaceUp pid procedure oid before)
    then pure ()
    else case costOf procedure oid before of
      Nothing -> pure ()
      Just cost -> do
        payment <- Cost.pay ManaSpending.AsProduced pid oid cost
        case payment of
          Payment.Unpaid -> State.put before
          -- CR 708.8: the copiable values revert, which for pawl is the status
          -- flipping -- Game.faceOf reads it, so the substitution simply stops
          -- applying and the card's own face answers again.
          Payment.Paid -> do
            State.modify'
              ( \gs ->
                  gs
                    { GameState.objects =
                        Map.adjust (\o -> o {Object.facing = Facing.FaceUp}) oid (GameState.objects gs)
                    }
              )
            -- CR 708.11 / 614.1e: the "as this permanent is turned face up"
            -- abilities, applied HERE -- after the status write and before the
            -- event record -- which is the rule's "while that permanent is being
            -- turned face up, not afterward" written as a position in this
            -- function.
            --
            -- AFTER the status write, and that placement is what makes the rule's
            -- "would have ... AFTER it's turned face up" answerable without a
            -- counterfactual: the permanent has its abilities back by now (CR
            -- 708.2a took them away only while it was face down), so
            -- Projection.replacementsAffecting simply sees the row. Running the
            -- loop first would collect from a permanent with no abilities at all
            -- and apply nothing.
            --
            -- BEFORE the event record, and that half is observable: a CR 614.1e
            -- ability and a CR 708.7 trigger on one card would otherwise be
            -- ordered the wrong way round, and CR 708.11's "not afterward" is
            -- exactly the sentence that decides it. Pawl.FaceDownSpec's second
            -- turnFaceUp call is what proves the loop is inside the PAID branch
            -- rather than run on every ask: a permanent that is already face up
            -- has its megamorph row too, so a loop outside this branch would put
            -- a second counter on.
            --
            -- Monad.void discards the Nothing that would mean the turning does
            -- not happen. No arm reachable from this event returns one -- CR
            -- 614.1e's abilities add to the turning over rather than replacing it
            -- -- and there is nothing left to cancel by this point anyway: the
            -- status is already written.
            Monad.void (Event.applyReplacements (ProposedEvent.WouldTurnFaceUp oid))
            -- CR 708.7 through CR 603.2: Skirk Marauder's "when this creature is
            -- turned face up" watches for this, and this is the only place in the
            -- engine that writes it.
            --
            -- AFTER the status write, matching CR 702.37e's own order. Not
            -- observable either way: CR 117.5's scan runs at
            -- Engine.settleForPriority and reads the log later, never between
            -- these two lines, so no reader can see the permanent mid-turnover.
            --
            -- Inside the PAID branch, which IS observable: turnFaceUp is a
            -- no-op for a permanent that is already face up (canTurnFaceUp's
            -- first conjunct), and such a permanent has its text back -- so an
            -- event recorded unconditionally would fire the ability again on a
            -- second, refused call. Pawl.FaceDownSpec asks twice to prove it.
            -- The unpaid branch is quiet for a different reason: CR 702.37e's
            -- reject-not-repair restores the state the attempt began with, log
            -- and all, and CR 708.2a leaves the still-face-down permanent with
            -- no ability that could have seen the event anyway.
            State.modify' (Event.recordEvent (GameEvent.TurnedFaceUp oid))
