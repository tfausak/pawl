-- Rule 708 in the one voice the rest of the engine cannot supply for itself: CR
-- 116.2b's special action that turns a face-down permanent face up.
--
-- Everything ELSE rule 708 says is arranged so that no module has to know this
-- one exists. CR 708.2's substitution lives at Pawl.Engine.Game.faceOf, so every
-- characteristic read gets it for free; CR 708.3/708.4's turning-over-before-the-
-- move lives at Pawl.Engine.Event.changeZoneFaceDown; CR 708.9's reveal is
-- Object.newIncarnation putting the status back to FaceUp, and the Revealed
-- event Pawl.Engine.Event.changeZoneAttaching and
-- Pawl.Engine.Departure.objectsLeaveWith record. What is left is an
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
-- FOUR PROCEDURES, because CR 708.7's permission belongs to whatever allowed
-- the permanent to be face down and four rules write one: CR 702.37e's, at the
-- morph cost; CR 702.168d's, at the disguise cost; CR 701.40b's, at the card's
-- mana cost; and CR 701.58b's, at the card's mana cost too. CR 701.40c and CR
-- 701.58d are the cases where two are open at once, and the engine offers both
-- rather than picking.
--
-- A FURTHER ROAD UP that is not a procedure at all: an Effect.TurnFaceUp
-- (Showstopping Surprise), which pays nothing and shows nothing. It shares
-- performTurnFaceUp with the four procedures because CR 701.40g replaces the
-- TURNING OVER and does not care what proposed it.
--
-- THE INVARIANT: rules 701.40, 701.58, 702.37 and 702.168 are part of the
-- rulebook, so reading Keyword.Morph's cost or a FaceDownReason here is the
-- same closed-half act as reading a Phase. This module never asks which CARD is
-- underneath.
module Pawl.Engine.FaceDown where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
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
import qualified Pawl.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import Pawl.Types.TurnUpProcedure (TurnUpProcedure)
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.TurnedFaceUp as TurnedFaceUp
import qualified Pawl.Types.TypeLine as TypeLine

-- CR 702.37e: "what the permanent's morph cost WOULD BE if it were face up",
-- one entry per distinct morph cost (Keyword.morphCosts). Empty when the
-- permanent would have no morph ability face up, which is the rule's own
-- parenthesis -- "if the permanent wouldn't have a morph cost if it were face
-- up, it can't be turned face up this way". Read off `faceUpKeywords` below.
morphCostsOf :: ObjectId -> GameState -> [Cost Keyword]
morphCostsOf oid gs = Keyword.morphCosts (faceUpKeywords oid gs)

-- CR 702.168d: "show all players what the permanent's disguise cost WOULD BE if
-- it were face up" -- morphCostsOf with rule 702.168d's price list in place of
-- rule 702.37e's, read off the same keywords.
disguiseCostsOf :: ObjectId -> GameState -> [Cost Keyword]
disguiseCostsOf oid gs = Keyword.disguiseCosts (faceUpKeywords oid gs)

-- The keywords rule 702.37e's and rule 702.168d's counterfactual asks about,
-- from two reads joined.
--
-- The CARD's: Game.faceUpCastingFaceOf, the one door that steps around CR
-- 708.2's substitution, which has taken the card's keywords off the face-down
-- permanent. CR 707.2: morph is copiable, so a card carrying copied values has
-- the copied card's. Not a projected read, so a layer-6 removal applied to the
-- face-down permanent leaves it -- Pawl.FaceDownSpec's "CR 613.7f turning face up
-- restamps the permanent after a removal that had wiped its grant" turns a
-- Turn to Frog'd Tracker up by its morph.
--
-- And the GRANTED ones: the permanent's own projection on a board where it is
-- face up, which is where CR 708.8's "any effects that have been applied to the
-- face-down permanent still apply" puts an effect that gives it the ability. A
-- disguise granted at the card's own mana cost (Disguise Agent) is priced there
-- at that face-up cost rather than at the face-down permanent's none (CR
-- 708.2a). Pawl.CommanderSpec's "CR 702.168a Disguise Agent's disguise casts a
-- commander face down and turns it up for its mana cost" proves it.
faceUpKeywords :: ObjectId -> GameState -> Set.Set Keyword
faceUpKeywords oid gs =
  let up = gs {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.FaceUp}) oid (GameState.objects gs)}
   in foldMap Face.keywordSet (Game.faceUpCastingFaceOf oid gs) <> Map.keysSet (Projection.keywordsOf oid up)

-- CR 701.40b and CR 701.58b, which say it in the same words: "show all players
-- that the card representing that permanent IS A CREATURE CARD and what THAT
-- CARD'S MANA COST is, pay that cost". Their shared parenthesis is the two
-- guards below: "if the card representing that permanent isn't a creature card
-- or it doesn't have a mana cost, it can't be turned face up this way."
--
-- ONE function for the two rules, and the PRICE is the whole of what it answers:
-- which permanents each rule is open to is its subject rather than its cost, and
-- that lives in canTurnFaceUp's `eligible` below.
--
-- Read through Game.faceUpFaceOf for morphCostsOf's reason, and the rule words it
-- even more plainly: both guards are about "the CARD representing that
-- permanent" rather than about the permanent, and CR 708.2a has left the
-- permanent itself with no card type and no mana cost at all -- so a projected
-- read would refuse every manifested permanent ever put onto the battlefield.
--
-- The card's own printed types, not its projected ones, for the same reason and
-- morphCostsOf's: the rule's subject is the card, so a CR 613 read of the
-- permanent answers a different question.
creatureCardCostOf :: ObjectId -> GameState -> Maybe (Cost Keyword)
creatureCardCostOf oid gs = do
  face <- Game.faceUpCastingFaceOf oid gs
  -- CR 707.2: card types are copiable too, so a copy stamp's types decide.
  let types = maybe (TypeLine.types (Face.typeLine face)) PC.cardTypes (Game.copyStampOf =<< Game.lookupObject oid gs)
  Monad.guard (Set.member CardType.Creature types)
  manaCost <- Face.manaCost face
  pure (Cost.Type.MkCost (Just manaCost) [])

-- What one of CR 708.7's four procedures may cost on this permanent, empty when
-- that procedure is closed to it. A classification of the four rules, never of
-- a card: which procedure is which is CR 701.40c's own distinction.
costsOf :: TurnUpProcedure -> ObjectId -> GameState -> [Cost Keyword]
costsOf procedure oid gs = case procedure of
  TurnUpProcedure.Morph -> morphCostsOf oid gs
  TurnUpProcedure.Disguise -> disguiseCostsOf oid gs
  TurnUpProcedure.Manifest -> Maybe.maybeToList (creatureCardCostOf oid gs)
  -- CR 701.58b's price list is rule 701.40b's, so the same reader answers both.
  TurnUpProcedure.Cloak -> Maybe.maybeToList (creatureCardCostOf oid gs)

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
--   * one such cost is payable. An action the player cannot take is not offered,
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
        -- CR 701.58b's subject is "a CLOAKED permanent you control", the arm
        -- above's shape one rule over: the reason on the status is how that is
        -- known (CR 708.6), and without this guard a manifested permanent would
        -- be turnable by a rule whose subject it is not.
        TurnUpProcedure.Cloak ->
          fmap (Facing.reasonOf . Object.facing) (Game.lookupObject oid gs)
            == Just (Just FaceDownReason.Cloaked)
   in maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid gs)
        && Projection.controllerOf oid gs == Just pid
        && eligible
        && any (payable pid oid gs) (costsOf procedure oid gs)

-- Cost.canPay for the special action, canTurnFaceUp's last conjunct and the
-- filter turnFaceUp offers its choice through. A cost with an X in it is
-- payable when it is payable with X chosen as zero (CR 107.3d).
payable :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payable = payableAtX 0

-- CR 107.3d: is this cost payable with its X chosen as this number?
payableAtX :: Natural -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableAtX x pid oid gs cost = Cost.canPay (PaymentSubject.TurningFaceUp oid) pid oid (Cost.substituteX x cost) gs

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
  do
    oid <- Set.toAscList (GameState.battlefield gs)
    procedure <- [TurnUpProcedure.Morph, TurnUpProcedure.Disguise, TurnUpProcedure.Manifest, TurnUpProcedure.Cloak]
    Monad.guard (canTurnFaceUp pid procedure oid gs)
    pure (oid, procedure)

-- CR 702.37e, CR 702.168d, CR 701.40b and CR 701.58b, in the order all four
-- rules share: show all players what the procedure's cost is, pay it, then turn
-- the permanent face up. ONE function for all of them, because everything after the payment is
-- the same game action -- the rules differ only in what they showed and what they
-- charged, which is `costOf` and nothing else.
--
-- The SHOWING is not modelled. Nothing in pawl hides a face-down permanent's
-- card from a reader in the first place, so there is no concealment for a reveal
-- to lift (#1412).
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
turnFaceUp :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> TurnUpProcedure -> ObjectId -> Game ()
turnFaceUp perform pid procedure oid = do
  before <- State.get
  if not (canTurnFaceUp pid procedure oid before)
    then pure ()
    else do
      -- CR 702.37b's "a megamorph cost is a morph cost": a card printing two
      -- morph abilities has two prices for one procedure, and which is paid is
      -- the player's (Prompt.ChooseCost, Activate's posture), since CR 702.37b's
      -- counter hangs on it. Only the payable ones are offered, and a question
      -- with one answer is not asked.
      chosen <- case filter (payable pid oid before) (costsOf procedure oid before) of
        [] -> pure Nothing
        [only] -> pure (Just only)
        offered -> do
          answer <- Game.choose (Prompt.ChooseCost (Decide.deciderFor pid before) pid oid offered)
          pure (List.find (== answer) offered)
      Monad.forM_ chosen $ \cost -> do
        -- CR 107.3d: the X in a special action's cost is chosen "immediately
        -- before they pay that cost", Pawl.Engine.Plot.plot's prompt and for its
        -- reasons: the bound is advisory, and an answer the board cannot pay
        -- takes the whole action away. Nothing where the cost has no X.
        announcedX <-
          if Cost.hasVariable cost
            then fmap Just (Game.choose (Prompt.ChooseX (Decide.deciderFor pid before) pid oid 0 (Cost.greatestPayableX Nothing (\x -> payableAtX x pid oid before cost) cost)))
            else pure Nothing
        Monad.when (payableAtX (Maybe.fromMaybe 0 announcedX) pid oid before cost) $ do
          -- CR 118.13c: a symbol payable in multiple ways is announced by the player
          -- taking the special action "immediately before they pay that cost" -- after
          -- the gate above, since what is announced is how to pay a cost already
          -- chosen, and before the mana window Cost.pay opens.
          --
          -- CR 601.2f's totalling is `pure`, Pawl.Engine.Resolve.Effect.payGatePaidBy's
          -- reason: pawl gathers cost adjustments for a SPELL (Pawl.Engine.Cast) and
          -- for an ACTIVATION (Pawl.Engine.Activate) and nowhere else, and CR 601.2f
          -- is a casting rule that reaches no special action, so the announced cost IS
          -- the cost that will be paid and this offer stays exactly as permissive as
          -- the payability gate above. Discarded, Pawl.Engine.Activate's reason: rule
          -- 702.150a asks about the player who CAST the object.
          (announced, _) <- Cost.announce (PaymentSubject.TurningFaceUp oid) ManaSpending.AsProduced pid oid pure (Cost.substituteX (Maybe.fromMaybe 0 announcedX) cost)
          payment <- Cost.pay perform before PaymentMoment.OutsideResolution (PaymentSubject.TurningFaceUp oid) Nothing ManaSpending.AsProduced pid oid announced
          case payment of
            -- CR 733.1's reversal, Pawl.Engine.Foretell.foretell's reason: this
            -- special action IS the whole of what failed, so `before` goes to
            -- Cost.pay and the reversal -- the payer's choice about the CR 605.3a
            -- window included -- happens there.
            Payment.Unpaid -> pure ()
            -- The payment's bound slots are dropped, Pawl.Engine.Ignore's reason:
            -- turning a permanent face up resolves nothing. The PRINTED cost rides
            -- the proposed event, since CR 702.37b's megamorph row compares it
            -- against the card's; the X rides beside it.
            Payment.Paid _ -> performTurnFaceUp (Just (procedure, cost)) announcedX oid

-- CR 701.40g and CR 701.58g, one sentence each and the same sentence: "if a
-- manifested [cloaked] permanent that's represented by an instant or sorcery
-- card would turn face up, its controller reveals it and leaves it face down".
--
-- TWO conjuncts and both are the rules' own words. MANIFESTED OR CLOAKED, so the
-- reason on the status is asked (CR 708.6) and a morph-cast permanent is not
-- covered; and the CARD it is REPRESENTED BY is an instant or a sorcery, so the
-- read goes through Game.faceUpFaceOf for creatureCardCostOf's reason -- CR
-- 708.2a has left the permanent itself with no card type at all, so a projected
-- read would answer about the 2/2 rather than about the card.
--
-- The REVEAL is not modelled, for the reason the procedures' showing is not:
-- nothing in pawl hides a face-down permanent's card from a reader, so there is
-- no concealment for it to lift (#1412). What is left of the rule is the second
-- half of its first sentence, and its second sentence.
--
-- EnteredFaceDown joins them on Magar of the Magic Strings' ruling (2022-10-07),
-- which gives its face-down instant or sorcery card the same treatment. Yedora,
-- Grave Gardener's EnteredFaceDown returns whatever card died, so a manifested
-- Divination it brings back stays face down by the same reading.
--
-- CR 730.2g is the same replacement for a face-down MERGED permanent, asked of
-- every card component (Game.componentsOf) with no reason at all, and read off
-- each card's own characteristics (Card.combined) rather than the permanent's.
revealsInsteadOfTurningUp :: ObjectId -> GameState -> Bool
revealsInsteadOfTurningUp oid gs =
  let instantOrSorcery = Set.fromList [CardType.Instant, CardType.Sorcery]
      isInstantOrSorcery = not . Set.null . Set.intersection instantOrSorcery . TypeLine.types . Face.typeLine
      object = Game.lookupObject oid gs
      reason = fmap (Facing.reasonOf . Object.facing) object
      manifested =
        (reason == Just (Just FaceDownReason.Manifested) || reason == Just (Just FaceDownReason.Cloaked) || reason == Just (Just FaceDownReason.EnteredFaceDown))
          && maybe False isInstantOrSorcery (Game.faceUpFaceOf oid gs)
      components = foldMap (Seq.filter Game.componentIsCard . Game.componentsOf . Object.source) object
      merged =
        maybe False (Facing.isFaceDown . Object.facing) object
          && any (maybe False (isInstantOrSorcery . Card.combined) . (`Game.cardOfPrinting` gs) . Game.printingOfComponent) components
   in manifested || merged

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
-- The procedure and the cost it paid are Maybe for that same asymmetry -- see
-- Pawl.Types.ProposedEvent.
--
-- CR 708.8 falls out of the shape and is not implemented anywhere: "any effects
-- that have been applied to the face-down permanent still apply to the face-up
-- permanent", and this writes one status field on one object -- no CR 400.7
-- incarnation is minted, so damage, counters, attachments, Auras and every
-- continuous effect naming the object ride through untouched. Its last sentence
-- is the same non-event: nothing here is a battlefield entry, so no
-- enters-the-battlefield ability is offered one.
--
-- The TIMESTAMP is the exception on that list, and CR 613.7f is why: "a permanent
-- receives a new timestamp each time it turns face up or face down". Game.turnFacing
-- writes it, so this road and Pawl.Engine.Resolve's Effect.TurnFaceDown arm cannot
-- disagree about the rule.
--
-- CR 708.11 is the one thing here that is NOT a bare write: "if a face-down
-- permanent would have an 'As [this permanent] is turned face up . . .' ability
-- after it's turned face up, that ability is applied WHILE that permanent is
-- being turned face up, NOT AFTERWARD". That is why the CR 616.1 loop runs
-- between the two writes below rather than after both -- see the note at the
-- call.
performTurnFaceUp :: Maybe (TurnUpProcedure, Cost Keyword) -> Maybe Natural -> ObjectId -> Game ()
performTurnFaceUp road announcedX oid = do
  gs <- State.get
  if revealsInsteadOfTurningUp oid gs
    then
      -- CR 701.40g / 730.2g: it stays face down, and NOTHING below runs. The rule's second
      -- sentence -- "abilities that trigger whenever a permanent is turned face
      -- up won't trigger" -- is exactly the GameEvent.TurnedFaceUp that is never
      -- recorded, and CR 614.1e's loop is skipped with it, since a permanent that
      -- did not turn over was never being turned over.
      pure ()
    else do
      -- CR 708.8: the copiable values revert, which for pawl is the status
      -- flipping -- Game.faceOf reads it, so the substitution simply stops
      -- applying and the card's own face answers again. Through Game.turnFacing,
      -- which carries CR 613.7f's new timestamp with the write.
      State.modify' (Game.turnFacing Facing.FaceUp oid)
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
      Monad.void (Event.applyReplacements (ProposedEvent.WouldTurnFaceUp oid road))
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
      --
      -- The X chosen for the cost rides the event for CR 702.37f and CR
      -- 702.168e (Pawl.Types.TurnedFaceUp).
      State.modify' (Event.recordEvent (GameEvent.TurnedFaceUp TurnedFaceUp.MkTurnedFaceUp {TurnedFaceUp.object = oid, TurnedFaceUp.announcedX = announcedX}))

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
-- controller's and is not modelled (#1412).
turnFaceUpByEffect :: ObjectId -> Game ()
turnFaceUpByEffect oid = do
  gs <- State.get
  Monad.when
    ( Set.member oid (GameState.battlefield gs)
        && maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid gs)
    )
    (performTurnFaceUp Nothing Nothing oid)
