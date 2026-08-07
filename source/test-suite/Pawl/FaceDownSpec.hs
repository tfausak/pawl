{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.FaceDown, Pawl.Engine.Card.faceDownFace and the face-down
-- arms threaded through Pawl.Engine.Game.faceOf, Pawl.Engine.Cast,
-- Pawl.Engine.Cost.costsFor, Pawl.Engine.Event.changeZoneFaceDown and
-- Pawl.Engine.Stack -- rule 708 as far as morph reaches it.
--
-- TWO morph cards carry this file, one per half of rule 708.
--
-- Ainok Tracker is the SUBSTITUTION's card. {5}{R} Creature -- Dog Scout 3/3,
-- "First strike / Morph {4}{R}". Every axis CR 708.2a substitutes is observable
-- on it and none of them coincides with the face-down value: 3/3 against the
-- rule's 2/2, two subtypes against none, a keyword against none, a name against
-- none, and a mana value of 6 against CR 202.3a's 0. A 2/2 morph creature would
-- leave the headline assertion passing whether the substitution happened or not.
--
-- Skirk Marauder is the TRIGGER's card, and the only printing rule 708.7 needs.
-- {1}{R} Creature -- Goblin 2/1, "Morph {2}{R} / When this creature is turned
-- face up, it deals 2 damage to any target." Three different 2s meet on it --
-- the damage, CR 708.2a's face-down 2/2 and the printed 2 power -- so the damage
-- is asserted as a LIFE-TOTAL DELTA on the chosen target and never as a board
-- fact, which is the one reading none of the others can fake.
module Pawl.FaceDownSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "FaceDown" $ do
  offerSpec s registry
  castSpec s registry
  turnFaceUpSpec s registry

-- CR 708.2a's name: the empty one, which matches no printed card.
noName :: CardName.CardName
noName = CardName.MkCardName Text.empty

-- alice holds one card of a morph printing with `n` untapped Mountains in play,
-- in her own precombat main phase with priority.
morphBoard :: Printing.Printing -> Printing.Printing -> Int -> (GameState.GameState, ObjectId.ObjectId)
morphBoard mountain morph n = S.handOne morph (S.landsInPlay mountain n)

-- The permanent a move added to the battlefield between these two states, or
-- Nothing when it added none or several. Identifies the new incarnation without
-- asking what card is under it, which is the whole point on this board.
enteredOne :: GameState.GameState -> GameState.GameState -> Maybe ObjectId.ObjectId
enteredOne before after =
  case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
    [oid] -> Just oid
    _ -> Nothing

-- Cast the card with the given facing and let it resolve, returning the
-- permanent it became.
castAndResolve :: Printing.Printing -> Facing.Facing -> GameState.GameState -> ObjectId.ObjectId -> (GameState.GameState, Maybe ObjectId.ObjectId)
castAndResolve morph facing gs oid =
  let after =
        S.runPure
          S.identityAnswer
          gs
          (Cast.castSpell S.alice oid (S.printingName morph) facing >> Stack.resolveTop)
   in (after, enteredOne gs after)

-- CR 702.37d: "You can't normally cast a card face down. A morph ability allows
-- you to do so." Two casts of one card, offered side by side and gated apart.
offerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
offerSpec s registry = Spec.describe s "Offer" $ do
  -- CR 702.37a prices the face-down cast at {3} and CR 118.9's alternative
  -- replaces the mana cost, so three Mountains buy the morph cast and not the
  -- {5}{R} one. THE DISCRIMINATOR for Cost.faceDownCost: were the face-down
  -- candidate the card's printed cost, no cast at all would be offered here,
  -- and were it free the {5}{R} one would still be missing.
  Spec.it s "CR 702.37a a morph cast is offered for {3} where the printed cost is not" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 3
        offered = Action.legalActions S.alice gs
        name = S.printingName ainok
    Spec.assertBool s (elem (Action.Type.Cast oid name Facing.FaceDown) offered) "the face-down cast is offered"
    Spec.assertBool s (notElem (Action.Type.Cast oid name Facing.FaceUp) offered) "the {5}{R} cast is not"

  -- Six Mountains pay either, so both actions stand on the menu at once and the
  -- player picks. The engine makes no choice here (docs/design.md's second
  -- invariant): nothing collapses the pair.
  Spec.it s "CR 702.37d both casts are offered when both are affordable" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 6
        offered = Action.legalActions S.alice gs
        name = S.printingName ainok
    Spec.assertBool s (elem (Action.Type.Cast oid name Facing.FaceDown) offered) "the face-down cast is offered"
    Spec.assertBool s (elem (Action.Type.Cast oid name Facing.FaceUp) offered) "and so is the {5}{R} one"

  -- A card with no morph ability offers no face-down cast at all: the offer
  -- comes from CR 702.37a's keyword and not from the rules.
  Spec.it s "CR 702.37d a card without morph offers no face-down cast" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = morphBoard mountain piker 6
        offered = Action.legalActions S.alice gs
    Spec.assertBool s (elem (Action.Type.Cast oid (S.printingName piker) Facing.FaceUp) offered) "the ordinary cast is offered"
    Spec.assertBool s (notElem (Action.Type.Cast oid (S.printingName piker) Facing.FaceDown) offered) "and no face-down one is"

  -- CR 708.4: "effects that care about the characteristics of a spell will see
  -- only the face-down spell's characteristics", and CR 702.37c says the same of
  -- the prohibitions applied to the cast. A Null Chamber whose chosen name is
  -- "Ainok Tracker" therefore stops the {5}{R} cast and does not stop the morph
  -- one -- the face-down spell has no name (CR 708.2a) for the prohibition to
  -- match.
  --
  -- THE POSITIVE CONTROL is the face-up half of the same assertion: the same
  -- Chamber on the same board really does stop a cast, so "the morph cast is
  -- offered" is not passing because the Chamber does nothing.
  Spec.it s "CR 708.4 a prohibition naming the card stops the face-up cast and not the morph one" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    nullChamber <- S.printingOf s registry "Null Chamber"
    let (base, oid) = morphBoard mountain ainok 6
        (chamber, withChamber) = S.addCreature nullChamber S.alice base
        -- CR 614.1c's as-enters choice, written straight onto the permanent:
        -- the Chamber's own entry replacement prompts for it, and this file is
        -- about rule 708 rather than about that prompt (Pawl.PlayerEffectSpec
        -- covers the choice itself).
        gs = withChosenNames chamber (Set.singleton (S.printingName ainok)) withChamber
        offered = Action.legalActions S.alice gs
        name = S.printingName ainok
    Spec.assertBool s (notElem (Action.Type.Cast oid name Facing.FaceUp) offered) "the named card cannot be cast face up"
    Spec.assertBool s (elem (Action.Type.Cast oid name Facing.FaceDown) offered) "but the morph cast is nameless and still offered"

-- Put a set of chosen card names onto a permanent (CR 201.4 / 614.1c).
withChosenNames :: ObjectId.ObjectId -> Set.Set CardName.CardName -> GameState.GameState -> GameState.GameState
withChosenNames oid names gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.chosenNames = names}) oid (GameState.objects gs)}

-- CR 708.2a / 708.4: what the face-down spell and the permanent it becomes are.
castSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
castSpec s registry = Spec.describe s "Cast" $ do
  -- THE PROVING TEST. Every axis rule 708.2a substitutes, read off the
  -- permanent the morph cast produced, against a card that differs from the
  -- rule's values on all of them.
  Spec.it s "CR 708.2a a morph cast becomes a 2/2 with no name, no subtypes and no abilities" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 3
        (after, entered) = castAndResolve ainok Facing.FaceDown gs oid
    case entered of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just permanent -> do
        Spec.assertEqWith s "CR 708.2a a 2/2, not the printed 3/3" (S.powerToughnessOf permanent after) (Just (2, 2))
        Spec.assertEqWith s "CR 708.2a no name" (Projection.nameOf permanent after) noName
        Spec.assertEqWith s "CR 708.2a no subtypes, not Dog Scout" (Projection.subtypesOf permanent after) Set.empty
        Spec.assertBool s (not (Projection.hasKeyword Keyword.FirstStrike permanent after)) "CR 708.2a no text, so no first strike"
        -- CR 110.5 / 708.4's last sentence: the permanent the spell becomes is a
        -- face-down permanent, so the status survived the stack-to-battlefield
        -- move CR 400.7 would otherwise forget.
        Spec.assertEqWith s "CR 708.4 it is face down" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceDown)
    -- CR 702.37a: {3} was paid, not {5}{R}. Three Mountains were in play and all
    -- three are tapped, which is the whole board.
    Spec.assertEqWith s "CR 702.37a three mana paid" (S.tappedCount S.alice after) 3

  -- CR 202.3a through CR 708.2a's "no mana cost": a face-down object's mana
  -- value is 0, and not the 6 the card underneath prints. Read through the
  -- filter view every mana-value question goes through.
  Spec.it s "CR 708.2a / 202.3a a face-down permanent has mana value 0" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 3
        (after, entered) = castAndResolve ainok Facing.FaceDown gs oid
    case entered of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just permanent ->
        Spec.assertEqWith
          s
          "mana value 0"
          (Filter.manaValue (Projection.viewOfObject permanent after))
          (Just 0)

  -- The face-up cast of the same card off the same fixture, so every assertion
  -- above is known to be about the FACING and not about the fixture: cast face
  -- up for {5}{R} and the permanent is the printed 3/3 Dog Scout with first
  -- strike.
  Spec.it s "CR 110.5b the ordinary cast of the same card is a 3/3 Dog Scout" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 6
        (after, entered) = castAndResolve ainok Facing.FaceUp gs oid
    case entered of
      Nothing -> Spec.assertFailure s "the ordinary cast did not reach the battlefield"
      Just permanent -> do
        Spec.assertEqWith s "the printed 3/3" (S.powerToughnessOf permanent after) (Just (3, 3))
        Spec.assertEqWith s "the printed name" (Projection.nameOf permanent after) (S.printingName ainok)
        Spec.assertEqWith s "the printed subtypes" (Projection.subtypesOf permanent after) (Set.fromList [Subtype.Dog, Subtype.Scout])
        Spec.assertBool s (Projection.hasKeyword Keyword.FirstStrike permanent after) "and first strike"
        Spec.assertEqWith s "face up" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceUp)

-- CR 116.2b / 702.37e / 708.8: the special action.
turnFaceUpSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
turnFaceUpSpec s registry = Spec.describe s "Turning face up" $ do
  -- CR 702.37e: the morph cost is REQUIRED. Ainok Tracker's is {4}{R}, so four
  -- Mountains left untapped after the {3} cast are one short and the action is
  -- not offered; five are enough and it is. THE DISCRIMINATOR for the
  -- payability conjunct in FaceDown.canTurnFaceUp -- drop it and the four-land
  -- board offers the action too.
  Spec.it s "CR 702.37e the morph cost is required before the action is offered" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let short = faceDownWith mountain ainok 7
        enough = faceDownWith mountain ainok 8
    case (short, enough) of
      (Just (shortGs, shortId), Just (enoughGs, enoughId)) -> do
        -- Both boards really do carry a face-down permanent, so the empty
        -- answer below is the COST failing and not the permanent missing.
        Spec.assertEqWith s "the short board has a face-down permanent" (fmap Object.facing (Game.lookupObject shortId shortGs)) (Just Facing.FaceDown)
        Spec.assertEqWith s "four untapped Mountains cannot pay {4}{R}" (FaceDown.turnableFaceUp S.alice shortGs) []
        Spec.assertEqWith s "five can" (FaceDown.turnableFaceUp S.alice enoughGs) [enoughId]
      _ -> Spec.assertFailure s "the morph cast did not reach the battlefield"

  -- CR 702.37e / 708.8, in both directions off ONE board: the permanent is the
  -- face-down 2/2 before the action and the printed 3/3 Dog Scout with first
  -- strike after it, and the morph cost really was spent.
  Spec.it s "CR 702.37e turning face up regains the printed characteristics" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    case faceDownWith mountain ainok 8 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (before, permanent) -> do
        Spec.assertEqWith s "a 2/2 before" (S.powerToughnessOf permanent before) (Just (2, 2))
        Spec.assertEqWith s "no subtypes before" (Projection.subtypesOf permanent before) Set.empty
        Spec.assertEqWith s "three lands tapped before" (S.tappedCount S.alice before) 3
        let after = S.runPure S.identityAnswer before (FaceDown.turnFaceUp S.alice permanent)
        Spec.assertEqWith s "CR 708.8 the printed 3/3 after" (S.powerToughnessOf permanent after) (Just (3, 3))
        Spec.assertEqWith s "CR 708.8 the printed name after" (Projection.nameOf permanent after) (S.printingName ainok)
        Spec.assertEqWith s "CR 708.8 the printed subtypes after" (Projection.subtypesOf permanent after) (Set.fromList [Subtype.Dog, Subtype.Scout])
        Spec.assertBool s (Projection.hasKeyword Keyword.FirstStrike permanent after) "CR 708.8 first strike after"
        Spec.assertEqWith s "CR 110.5 face up after" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceUp)
        -- CR 702.37e: "pay that cost". Five more Mountains went down for the
        -- {4}{R}, on top of the three the morph cast spent.
        Spec.assertEqWith s "CR 702.37e eight lands tapped after" (S.tappedCount S.alice after) 8

  -- CR 702.37e's "a face-down permanent YOU control": the opponent is never
  -- offered the action, whatever they could pay.
  Spec.it s "CR 702.37e only the controller may take the action" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    case faceDownWith mountain ainok 8 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (gs, permanent) -> do
        Spec.assertEqWith s "alice may" (FaceDown.turnableFaceUp S.alice gs) [permanent]
        Spec.assertEqWith s "bob may not" (FaceDown.turnableFaceUp S.bob gs) []

  -- CR 708.8's "any effects that have been applied to the face-down permanent
  -- still apply to the face-up permanent", read where it changes an outcome: two
  -- damage is lethal to the face-down 2/2 (CR 704.5g) and is not lethal to the
  -- 3/3 it turns into, and the damage is still marked either way.
  Spec.it s "CR 708.8 damage marked on the 2/2 carries onto the 3/3" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    case faceDownWith mountain ainok 8 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (gs, permanent) -> do
        let damaged = S.markDamage permanent 2 gs
            -- The control: left face down, CR 704.5g buries it.
            leftDown = S.settleSba damaged
            turned = S.runPure S.identityAnswer damaged (FaceDown.turnFaceUp S.alice permanent)
            settled = S.settleSba turned
        Spec.assertBool s (not (S.onBattlefield permanent leftDown)) "CR 704.5g two damage is lethal to the face-down 2/2"
        Spec.assertBool s (S.onBattlefield permanent settled) "CR 708.8 the 3/3 survives the same two damage"
        Spec.assertEqWith s "CR 708.8 and the damage is still marked" (S.damageOf permanent settled) (Just 2)

  -- THE PROVING TEST for CR 708.7. Skirk Marauder is cast face down for CR
  -- 702.37a's {3}, turned face up for its {2}{R} morph cost, and the ability it
  -- regains as it turns over deals its 2 to bob.
  --
  -- The damage is read as a LIFE-TOTAL DELTA and never off the board: 2 damage,
  -- CR 708.2a's face-down 2/2 and the printed 2 power are all the same number, so
  -- an assertion about the creature could not tell them apart. bob's 20 -> 18 can
  -- come from nothing else on this board.
  --
  -- bob is answered explicitly rather than left to S.identityAnswer, which picks
  -- the lowest-sorting candidate -- alice, the controller, which would make the
  -- positive case indistinguishable from the "no target was chosen" control.
  --
  -- THE BEFORE assertion is CR 708.4 through CR 708.2a: the face-down spell became
  -- a face-down permanent, which has no text at all, so neither the cast nor the
  -- battlefield entry could fire anything. That is what makes 18 the TURNING's
  -- doing rather than the entry's.
  Spec.it s "CR 708.7 turning Skirk Marauder face up deals its 2 to the chosen target" $ do
    mountain <- S.printingOf s registry "Mountain"
    marauder <- S.printingOf s registry "Skirk Marauder"
    case faceDownWith mountain marauder 6 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (before, permanent) -> do
        Spec.assertEqWith s "CR 708.3 the face-down entry fired nothing" (S.lifeOf S.bob before) (Just 20)
        -- The control: the action really is on offer, so a silent engine below
        -- cannot be a permanent that simply never turned over.
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [permanent]
        let after = S.runPure (aimAt S.bob) before (FaceDown.turnFaceUp S.alice permanent >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 708.7 bob took the 2" (S.lifeOf S.bob after) (Just 18)
        -- CR 115.1: the ability TARGETS, so the damage went where it was aimed
        -- and was not broadcast at the table.
        Spec.assertEqWith s "and alice took none" (S.lifeOf S.alice after) (Just 20)
        Spec.assertEqWith s "CR 708.8 the printed 2/1" (S.powerToughnessOf permanent after) (Just (2, 1))
        Spec.assertEqWith s "CR 708.8 the printed name" (Projection.nameOf permanent after) (S.printingName marauder)
        Spec.assertEqWith s "CR 110.5 face up" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceUp)
        -- {3} for the cast and {2}{R} for the morph cost: six, which is a
        -- multiple of neither printed cost alone.
        Spec.assertEqWith s "CR 702.37a/702.37e six mana in all" (S.tappedCount S.alice after) 6
        -- CR 702.37e's "a FACE-DOWN permanent you control", and the guard on the
        -- whole action: the permanent is face up now, so there is nothing left to
        -- turn over and asking again is a no-op. THE DISCRIMINATOR for the event
        -- being recorded inside FaceDown.turnFaceUp's paid branch rather than on
        -- entry -- record it unconditionally and this second call fires the
        -- ability again, since by now the permanent has its text back to see it
        -- with.
        let again = S.runPure (aimAt S.bob) after (FaceDown.turnFaceUp S.alice permanent >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 702.37e asking again turns nothing over and fires nothing" (S.lifeOf S.bob again) (Just 18)

  -- CR 708.8's last sentence, said the way Skirk Marauder can say it: a permanent
  -- that entered the battlefield FACE UP was never TURNED face up, so the same
  -- ability on the same card stays silent. Cast for the printed {1}{R} this time.
  --
  -- The stronger half of the pair with the case above. An engine that fired this
  -- ability on any arrival -- an entry, a Moved event into the battlefield --
  -- would pass every assertion up there and fail here.
  --
  -- Answered with aimAt as well, so a trigger that DID go on the stack would find
  -- its target and reach bob. Nothing is being kept quiet by an unanswerable
  -- prompt.
  Spec.it s "CR 708.8 a Skirk Marauder cast FACE UP was never turned face up" $ do
    mountain <- S.printingOf s registry "Mountain"
    marauder <- S.printingOf s registry "Skirk Marauder"
    let (gs, oid) = morphBoard mountain marauder 2
        (cast, entered) = castAndResolve marauder Facing.FaceUp gs oid
        settled = S.runPure (aimAt S.bob) cast Engine.priorityLoop
    case entered of
      Nothing -> Spec.assertFailure s "the ordinary cast did not reach the battlefield"
      Just permanent -> do
        -- The control: it really did arrive, face up and printed, so the silence
        -- below is CR 708.7 and not an empty battlefield.
        Spec.assertEqWith s "CR 110.5b the printed 2/1 arrived" (S.powerToughnessOf permanent settled) (Just (2, 1))
        Spec.assertEqWith s "face up" (fmap Object.facing (Game.lookupObject permanent settled)) (Just Facing.FaceUp)
        Spec.assertEqWith s "CR 702.37a the printed {1}{R} was paid, not {3}" (S.tappedCount S.alice settled) 2
    Spec.assertEqWith s "CR 708.7 nothing was turned face up, so bob took nothing" (S.lifeOf S.bob settled) (Just 20)

  -- CR 702.37e's "pay that cost", on the failure side: two Mountains left after
  -- the {3} cast cannot pay {2}{R}, FaceDown.turnFaceUp restores the state it
  -- began with, and a permanent that never turned over fires nothing.
  --
  -- The silence here is NOT evidence about where the event is recorded, and the
  -- case does not claim to be: CR 708.2a leaves the still-face-down permanent
  -- with no ability that could see the event, so an engine recording it in the
  -- unpaid branch too would pass this anyway. What this pins is the reject: the
  -- permanent is still face down, and mana that could not pay bought nothing. The
  -- second call in the proving test above is what discriminates the placement.
  Spec.it s "CR 702.37e an unpayable morph cost turns nothing over and fires nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    marauder <- S.printingOf s registry "Skirk Marauder"
    case faceDownWith mountain marauder 5 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (before, permanent) -> do
        Spec.assertEqWith s "two Mountains cannot pay {2}{R}" (FaceDown.turnableFaceUp S.alice before) []
        let after = S.runPure (aimAt S.bob) before (FaceDown.turnFaceUp S.alice permanent >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 702.37e it is still face down" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceDown)
        Spec.assertEqWith s "and bob took nothing" (S.lifeOf S.bob after) (Just 20)

  -- CR 603.2 through the bearer: the ability fires for the permanent it is ON and
  -- not for any permanent turning face up.
  --
  -- TWO Skirk Marauders, flipped one after the other. By the second flip the
  -- FIRST one is face up and carrying its ability again (CR 708.2a took it away
  -- only while it was face down), so a matcher that ignored the bearer would fire
  -- both abilities on that one event and cost bob 4 instead of 2. The running
  -- total is asserted after each flip, so the two flips are told apart.
  --
  -- Twelve Mountains: {3} + {3} for the casts and {2}{R} + {2}{R} for the morph
  -- costs.
  Spec.it s "CR 603.2 a face-up Skirk Marauder does not fire off the OTHER one turning face up" $ do
    mountain <- S.printingOf s registry "Mountain"
    marauder <- S.printingOf s registry "Skirk Marauder"
    let (base, firstCard) = S.handOne marauder (S.landsInPlay mountain 12)
        (secondCard, both) = S.addHandCard marauder S.alice base
        (afterFirst, firstEntered) = castAndResolve marauder Facing.FaceDown both firstCard
        (afterSecond, secondEntered) = castAndResolve marauder Facing.FaceDown afterFirst secondCard
    case (firstEntered, secondEntered) of
      (Just one, Just two) -> do
        let flippedOne = S.runPure (aimAt S.bob) afterSecond (FaceDown.turnFaceUp S.alice one >> Engine.priorityLoop)
            flippedTwo = S.runPure (aimAt S.bob) flippedOne (FaceDown.turnFaceUp S.alice two >> Engine.priorityLoop)
        Spec.assertEqWith s "the first flip is worth 2" (S.lifeOf S.bob flippedOne) (Just 18)
        -- Both are face up now, and only the one that turned over fired.
        Spec.assertEqWith s "CR 603.2 the second flip is worth 2 more and not 4" (S.lifeOf S.bob flippedTwo) (Just 16)
        Spec.assertEqWith s "both are face up" (fmap Object.facing (Game.lookupObject two flippedTwo)) (Just Facing.FaceUp)
        Spec.assertEqWith s "CR 702.37a/702.37e twelve mana in all" (S.tappedCount S.alice flippedTwo) 12
      _ -> Spec.assertFailure s "both morph casts did not reach the battlefield"

-- The one target slot of Skirk Marauder's ability, answered with `who` rather
-- than left to S.identityAnswer's lowest-sorting candidate -- which is alice, the
-- ability's own controller, and so the control rather than the positive case.
aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimAt who p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer who)) sets
  _ -> S.identityAnswer p

-- A resolved face-down permanent of a morph printing on a board of `n`
-- Mountains, three of which CR 702.37a's {3} has tapped. Nothing if the cast did
-- not land.
faceDownWith :: Printing.Printing -> Printing.Printing -> Int -> Maybe (GameState.GameState, ObjectId.ObjectId)
faceDownWith mountain morph n =
  let (gs, oid) = morphBoard mountain morph n
      (after, entered) = castAndResolve morph Facing.FaceDown gs oid
   in fmap (\permanent -> (after, permanent)) entered
