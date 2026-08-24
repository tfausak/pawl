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
-- THREE PROCEDURES, because CR 708.7's permission belongs to whatever allowed
-- the permanent to be face down and three rules write one: CR 702.37e's, at the
-- morph cost; CR 702.168d's, at the disguise cost; and CR 701.40b's, at the
-- card's mana cost. CR 701.40c and CR 701.58d are the cases where two are open at
-- once, and the engine offers both rather than picking.
--
-- A FURTHER ROAD UP that is not a procedure at all: an Effect.TurnFaceUp
-- (Showstopping Surprise), which pays nothing and shows nothing. It shares
-- performTurnFaceUp with the three procedures because CR 701.40g replaces the
-- TURNING OVER and does not care what proposed it.
--
-- THE INVARIANT: rules 701.40, 702.37 and 702.168 are part of the rulebook, so
-- reading Keyword.Morph's cost or a FaceDownReason here is the same closed-half
-- act as reading a Phase. This module never asks which CARD is underneath.
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
import qualified Pawl.Types.PaymentMoment as PaymentMoment
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
-- useless here: a face-down permanent has none of the CARD's keywords (CR
-- 708.2), so Projection.keywordsOf would answer Nothing for every morph creature
-- ever printed. The rule's counterfactual is what licenses it. What a projected
-- read does find is whatever the allower LISTED -- disguise's ward {2} (CR
-- 702.168b) -- which is never a morph or disguise cost.
--
-- Not a projected read, therefore not affected by Humility: a face-down
-- permanent under a layer-6 ability removal can still be turned face up, since
-- the ability rule 702.37e consults is the one the CARD would have. That is what
-- "would be if it were face up" says, and nothing in the pool observes it.
morphCostOf :: ObjectId -> GameState -> Maybe (Cost Keyword)
morphCostOf oid gs = do
  face <- Game.faceUpFaceOf oid gs
  Keyword.morphCost (Face.keywords face)

-- CR 702.168d: "show all players what the permanent's disguise cost WOULD BE if
-- it were face up". Nothing when the card underneath has no disguise ability,
-- which is the rule's own parenthesis -- "if the permanent wouldn't have a
-- disguise cost if it were face up, it can't be turned face up this way".
--
-- morphCostOf with rule 702.168d's price list in place of rule 702.37e's, and
-- every note above applies here word for word: the read goes through
-- Game.faceUpFaceOf because CR 708.2's substitution has taken the card's
-- keywords away, and the rule's counterfactual is what licenses it. The face-down
-- permanent's ONE keyword is the ward CR 702.168b listed, which is not the
-- ability this asks about.
disguiseCostOf :: ObjectId -> GameState -> Maybe (Cost Keyword)
disguiseCostOf oid gs = do
  face <- Game.faceUpFaceOf oid gs
  Keyword.disguiseCost (Face.keywords face)

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
-- The card's own printed types, not its projected ones, for the same reason and
-- morphCostOf's: the rule's subject is the card, so a CR 613 read of the
-- permanent answers a different question.
manifestCostOf :: ObjectId -> GameState -> Maybe (Cost Keyword)
manifestCostOf oid gs = do
  face <- Game.faceUpFaceOf oid gs
  Monad.guard (Set.member CardType.Creature (TypeLine.types (Face.typeLine face)))
  manaCost <- Face.manaCost face
  pure (Cost.Type.MkCost (Just manaCost) [])

-- What one of CR 708.7's three procedures costs on this permanent, or Nothing
-- when that procedure is closed to it. A classification of the three rules,
-- never of a card: which procedure is which is CR 701.40c's own distinction.
costOf :: TurnUpProcedure -> ObjectId -> GameState -> Maybe (Cost Keyword)
costOf procedure oid gs = case procedure of
  TurnUpProcedure.Morph -> morphCostOf oid gs
  TurnUpProcedure.Disguise -> disguiseCostOf oid gs
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
        -- CR 702.168d's subject is "a face-down permanent you control WITH A
        -- DISGUISE ABILITY", rule 702.37e's shape exactly, so this one asks about
        -- the card and not about the allower either. A permanent cloaked or
        -- manifested off a disguise card is turnable this way, which is CR
        -- 701.58d in as many words.
        TurnUpProcedure.Disguise -> True
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
-- 701.40c (and CR 701.58d for disguise) in the shape Pawl.Engine.Room.unlockable
-- takes for CR 709.5e's doors:
-- "its controller MAY turn that card face up using EITHER ... OR", two prices,
-- and offering both as legal actions is how the engine declines to choose
-- (docs/design.md's second invariant).
turnableFaceUp :: PlayerId -> GameState -> [(ObjectId, TurnUpProcedure)]
turnableFaceUp pid gs =
  [ (oid, procedure)
  | oid <- Set.toAscList (GameState.battlefield gs),
    procedure <- [TurnUpProcedure.Morph, TurnUpProcedure.Disguise, TurnUpProcedure.Manifest],
    canTurnFaceUp pid procedure oid gs
  ]

-- CR 702.37e, CR 702.168d and CR 701.40b, in the order all three rules share:
-- show all players what the procedure's cost is, pay it, then turn the permanent
-- face up. ONE function for all of them, because everything after the payment is
-- the same game action -- the rules differ only in what they showed and what they
-- charged, which is `costOf` and nothing else.
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
-- The UNPAID branch is quiet, and for a reason of its own: CR 702.37e's
-- reject-not-repair restores the state the attempt began with, log and all, and
-- CR 708.2 leaves the still-face-down permanent with no ability watching a
-- payment anyway -- disguise's listed ward (CR 702.168b) watches CR 702.21a's
-- targeting and nothing else.
--
-- Everything from the status write on is performTurnFaceUp below, which the
-- effect road shares.
turnFaceUp :: PlayerId -> TurnUpProcedure -> ObjectId -> Game ()
turnFaceUp pid procedure oid = do
  before <- State.get
  if not (canTurnFaceUp pid procedure oid before)
    then pure ()
    else case costOf procedure oid before of
      Nothing -> pure ()
      Just cost -> do
        payment <- Cost.pay PaymentMoment.OutsideResolution Nothing ManaSpending.AsProduced pid oid cost
        case payment of
          Payment.Unpaid -> State.put before
          -- The payment's bound slots are dropped, Pawl.Engine.Ignore's reason:
          -- turning a permanent face up resolves nothing.
          Payment.Paid _ -> performTurnFaceUp (Just procedure) oid

-- CR 701.40g: "if a manifested permanent that's represented by an instant or
-- sorcery card would turn face up, its controller reveals it and leaves it face
-- down".
--
-- TWO conjuncts and both are the rule's own words. MANIFESTED, so the reason on
-- the status is asked (CR 708.6) and a morph-cast permanent is not covered; and
-- the CARD it is REPRESENTED BY is an instant or a sorcery, so the read goes
-- through Game.faceUpFaceOf for manifestCostOf's reason -- CR 708.2a has left the
-- permanent itself with no card type at all, so a projected read would answer
-- about the 2/2 rather than about the card.
--
-- The REVEAL is not modelled, for the reason the procedures' showing is not:
-- nothing in pawl hides a face-down permanent's card from a reader, so there is
-- no concealment for it to lift (#682). What is left of the rule is the second
-- half of its first sentence, and its second sentence.
revealsInsteadOfTurningUp :: ObjectId -> GameState -> Bool
revealsInsteadOfTurningUp oid gs =
  fmap (Facing.reasonOf . Object.facing) (Game.lookupObject oid gs) == Just (Just FaceDownReason.Manifested)
    && maybe
      False
      (not . Set.null . Set.intersection instantOrSorcery . TypeLine.types . Face.typeLine)
      (Game.faceUpFaceOf oid gs)
  where
    instantOrSorcery = Set.fromList [CardType.Instant, CardType.Sorcery]

-- The turning-over itself, once whatever allowed it has allowed it: the status
-- write, CR 708.11's replacement loop, and CR 708.7's event, in that order and
-- for the reasons the notes below give.
--
-- ONE funnel for every road up. The special action above reaches it after CR
-- 116.2b's payment; an Effect.TurnFaceUp reaches it through turnFaceUpByEffect,
-- having paid nothing. CR 701.40g is the whole reason the two share a body
-- rather than each writing the status: the rule replaces the TURNING OVER and
-- says nothing about what proposed it, so a guard on one road would leave the
-- other one wrong.
--
-- The procedure is Maybe for that same asymmetry -- see Pawl.Types.ProposedEvent.
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
-- between the two writes below rather than after both -- see the note at the
-- call.
performTurnFaceUp :: Maybe TurnUpProcedure -> ObjectId -> Game ()
performTurnFaceUp procedure oid = do
  gs <- State.get
  if revealsInsteadOfTurningUp oid gs
    then
      -- CR 701.40g: it stays face down, and NOTHING below runs. The rule's second
      -- sentence -- "abilities that trigger whenever a permanent is turned face
      -- up won't trigger" -- is exactly the GameEvent.TurnedFaceUp that is never
      -- recorded, and CR 614.1e's loop is skipped with it, since a permanent that
      -- did not turn over was never being turned over.
      pure ()
    else do
      -- CR 708.8: the copiable values revert, which for pawl is the status
      -- flipping -- Game.faceOf reads it, so the substitution simply stops
      -- applying and the card's own face answers again.
      State.modify'
        ( \g ->
            g
              { GameState.objects =
                  Map.adjust (\o -> o {Object.facing = Facing.FaceUp}) oid (GameState.objects g)
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
      -- loop first would collect from a permanent holding none of the card's
      -- abilities and apply nothing.
      --
      -- BEFORE the event record, and that half is observable: a CR 614.1e
      -- ability and a CR 708.7 trigger on one card would otherwise be
      -- ordered the wrong way round, and CR 708.11's "not afterward" is
      -- exactly the sentence that decides it. Pawl.FaceDownSpec's second
      -- turnFaceUp call is what proves this body is reached only from
      -- turnFaceUp's PAID branch rather than on every ask: a permanent that
      -- is already face up has its megamorph row too, so a loop run on the
      -- refused call would put a second counter on.
      --
      -- Monad.void discards the Nothing that would mean the turning does
      -- not happen. No arm reachable from this event returns one -- CR
      -- 614.1e's abilities add to the turning over rather than replacing it
      -- -- and there is nothing left to cancel by this point anyway: the
      -- status is already written.
      Monad.void (Event.applyReplacements (ProposedEvent.WouldTurnFaceUp oid procedure))
      -- CR 708.7 through CR 603.2: Skirk Marauder's "when this creature is
      -- turned face up" watches for this, and this is the only place in the
      -- engine that writes it.
      --
      -- AFTER the status write, matching CR 702.37e's own order. Not
      -- observable either way: CR 117.5's scan runs at
      -- Engine.settleForPriority and reads the log later, never between
      -- these two lines, so no reader can see the permanent mid-turnover.
      --
      -- Reached only from a caller that has already refused an
      -- already-face-up permanent -- turnFaceUp's canTurnFaceUp, and
      -- turnFaceUpByEffect's own face-down guard -- and THAT is observable:
      -- such a permanent has its text back, so an event recorded on a
      -- refused call would fire the ability again. Pawl.FaceDownSpec asks
      -- twice to prove it.
      State.modify' (Event.recordEvent (GameEvent.TurnedFaceUp oid))

-- CR 708 by way of an Effect.TurnFaceUp: Showstopping Surprise's "turn it face
-- up if it's face down", with no cost, no procedure and no CR 116.2b special
-- action anywhere in it.
--
-- TWO guards and no more. It is a permanent, so a card the effect reached in
-- some other zone has no face to turn (CR 110.1); and it is FACE DOWN, which is
-- the card's own "if it's face down" -- CR 708.2b's mirror, and the same
-- conjunct canTurnFaceUp opens with. Nothing about a card type, a mana cost or a
-- morph ability: those are CR 701.40b's and CR 702.37e's price lists and belong
-- to the procedures, not to the turning-over. CR 701.40g is the one restriction
-- that survives, and it lives in performTurnFaceUp because it is about the
-- turning-over rather than about the road.
--
-- No controller argument: the effect's controller is who resolved it, and
-- nothing left here reads a player -- CR 701.40g's reveal is the permanent's own
-- controller's and is not modelled (#682).
turnFaceUpByEffect :: ObjectId -> Game ()
turnFaceUpByEffect oid = do
  gs <- State.get
  Monad.when
    ( Set.member oid (GameState.battlefield gs)
        && maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid gs)
    )
    (performTurnFaceUp Nothing oid)
