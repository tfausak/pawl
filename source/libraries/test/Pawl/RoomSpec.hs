{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 709.5's Room cards end to end: Pawl.Types.Layout's Room arm and the
-- Pawl.Engine.Card functions that read it (CR 709.4's combined view, CR 709.3's
-- castable halves, CR 709.5's roomFace subtraction, CR 709.5d's entering
-- designation), plus the modules those classifications reach --
-- Pawl.Engine.Game.faceOf substitutes the subtracted view,
-- Pawl.Engine.Event.changeZoneAttaching writes the entering designation and
-- records CR 709.5h's event, Pawl.Engine.Room offers and carries out CR
-- 116.2m/709.5e's special action, and Pawl.Engine.Action/Pawl.Engine.Engine put
-- it on the menu, and Pawl.Engine.Resolve's SetHalfLocked arm carries out CR
-- 709.5f/g's instructions. Pawl.Engine.Projection.namesOf gives the permanent one name
-- per UNLOCKED door (CR 709.4a), which makes a Room with both doors open the
-- pool's only permanent with two names at once. CR 709.5i's "fully unlocks" is
-- here too, which reaches
-- Event.fullyUnlockedAfter at both writers of an unlocked designation and
-- Event.matchesTrigger's RoomFullyUnlocked and AnyOf arms.
--
-- THREE printed cards. Roaring Furnace // Steaming Sauna, DSK 230, picked because
-- its two doors disagree about everything a reader can see -- {1}{R} against
-- {3}{U}{U}, a triggered ability that fires on the door opening against a
-- persistent pair (a static "you have no maximum hand size" and an end-step
-- draw) -- so a permanent whose wrong half was subtracted is visible in every
-- assertion here rather than in one narrow one. The persistent half is what
-- makes the subtraction observable at all: a door whose only text is "when you
-- unlock this door" looks the same whether its text was subtracted or its
-- trigger simply did not match. And Balemurk Leech, DSK 84, which is the pool's
-- one CR 709.5i trigger and the one card whose ability watches a Room it is not.
-- And Keys to the House, DSK 254, which is the pool's producer of CR 709.5f's
-- unlock and CR 709.5g's lock as EFFECTS rather than as CR 116.2m's special
-- action, so the two rules the special action cannot reach get a card.
--
-- Pawl.Engine.Room's own coverage is here rather than beside Resolve's, since
-- rule 709.5c's derivation is what both the action and the opcode filter their
-- offers through.
--
-- THREE SEATS, so "target creature an opponent controls" cannot coincide with
-- "a creature you control": bob holds the only legal target and carol holds
-- none -- and so that CR 109.5's "each opponent" cannot coincide with "each
-- player", which is what the CR 709.5i case turns on.
module Pawl.RoomSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Room as Room
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- The two doors, by the names CR 709.4a gives them, plus the joined name CR
-- 709.4 gives the card off the battlefield.
furnaceName, saunaName, joinedName :: CardName.CardName
furnaceName = CardName.MkCardName (Text.pack "Roaring Furnace")
saunaName = CardName.MkCardName (Text.pack "Steaming Sauna")
joinedName = CardName.MkCardName (Text.pack "Roaring Furnace//Steaming Sauna")

-- The board every gameplay case starts from: three seats, alice active in her
-- precombat main phase with priority and the stack empty (CR 709.5e's window),
-- ten untapped lands of hers, three spare Mountains in her hand, one library
-- card to draw, and bob's Wall of Stone as the only creature any opponent
-- controls.
--
-- THREE cards in hand behind the Room, and a 0/8 wall to take the damage: CR
-- 709.5h's trigger deals damage equal to the cards in alice's hand, so the
-- three left after the Room is cast is a number no other quantity on the board
-- coincides with -- not the lands (ten), not the toughness (eight), not the
-- doors' costs (two and five).
--
-- Returns the Room's id in alice's hand.
setUp ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
setUp s registry = do
  room <- S.printingOf s registry "Roaring Furnace"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  wall <- S.printingOf s registry "Wall of Stone"
  let addAll printing pid n gs = List.foldl' (\g _ -> snd (S.addCreature printing pid g)) gs [1 .. n :: Int]
      handAll printing n gs = List.foldl' (\g _ -> snd (S.addHandCard printing S.alice g)) gs [1 .. n :: Int]
      lands = addAll island S.alice 6 (addAll mountain S.alice 4 (Setup.emptyGame S.threePlayers))
      (wallId, withWall) = S.addCreature wall S.bob lands
      (_, deck) = S.addLibraryCard mountain S.alice withWall
      (roomId, filled) = S.addHandCard room S.alice (handAll mountain 3 deck)
  pure
    ( roomId,
      wallId,
      filled
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Cast one door and let the resulting permanent settle, triggers and all.
castDoor :: CardName.CardName -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castDoor door oid gs =
  let cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid door Facing.FaceUp))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in resolveAll (settle resolved)

settle :: GameState.GameState -> GameState.GameState
settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)

resolveAll :: GameState.GameState -> GameState.GameState
resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)

-- CR 603.2b's event for alice's end step, with the phase moved to match --
-- Pawl.TriggerSpec's beginEndStep, which is how every step trigger in the suite
-- is fired.
beginEndStep :: GameState.GameState -> GameState.GameState
beginEndStep gs =
  Event.recordEvent
    (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice))
    (gs {GameState.phase = Phase.Ending EndingStep.EndStep})

-- The Room permanent on the battlefield: the one permanent that is not a land
-- and not bob's wall. Found by having a card name rather than followed by id,
-- since CR 400.7 mints a new one as the spell resolves.
roomPermanent :: GameState.GameState -> [ObjectId.ObjectId]
roomPermanent gs =
  [ o
  | o <- Set.toList (GameState.battlefield gs),
    let names = Projection.namesOf o gs,
    -- A door's name, or NO name at all -- which is what a Room with both doors
    -- shut has (CR 709.5).
    Set.null names || not (Set.disjoint names (Set.fromList [furnaceName, saunaName]))
  ]

-- The riders an effect that merely puts a permanent onto the battlefield asks
-- for: CR 110.5b's untapped and face up, no CR 508.1 attacking entry, and CR
-- 712.14's untransformed default.
plainEntry :: EntryRiders.EntryRiders Natural
plainEntry = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}

-- Every unlock this player is offered right now, as CR 709.5e's pair.
unlocksOffered :: GameState.GameState -> [(ObjectId.ObjectId, CardName.CardName)]
unlocksOffered gs = [(o, n) | A.Unlock o n <- Action.legalActions S.alice gs]

-- Keys to the House's SECOND ability, "{3}, {T}, Sacrifice this artifact: Lock
-- or unlock a door of target Room you control. Activate only as a sorcery." The
-- first is an ordinary basic-land tutor and shares nothing with rule 709.5.
lockAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
lockAbility keys = case drop 1 (Face.activatedAbilities (S.combinedFace keys)) of
  ability : _ -> ability
  -- Unreachable: the printing has two. A no-mode ability with an unpayable cost
  -- keeps the helper total and is offered nowhere, so a card that lost its
  -- second ability fails the case rather than silently activating the first.
  [] -> ActivatedAbility.MkActivatedAbility (Cost.MkCost Nothing []) (Modal.MkModal Seq.empty (ModeSelection.ChooseExactly 1)) [] Nothing Nothing

-- Whether that ability is on alice's menu, which is what says the board really
-- is in CR 307.5's sorcery-speed window with the cost payable -- and so that a
-- case below asserting the effect is not passing on an ability nobody could
-- have activated.
lockOffered :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> Bool
lockOffered keys keysId gs = elem (A.Activate keysId (lockAbility keys)) (Action.legalActions S.alice gs)

-- Answers everything Keys to the House asks as it resolves: CR 601.2c's target,
-- CR 608.2d's branch by ordinal, and CR 709.5f/g's door by name.
--
-- The TARGET is answered by FILTERING the offered recipients rather than by
-- building one, since a hand-built recipient of another tag is dropped by CR
-- 608.2b's re-read with no error. The BRANCH and the DOOR are answered by value:
-- each is raised at most once per case here, and each answer is filtered back
-- through the offer by the engine, which is what the lock case below turns on.
keysAnswer :: ObjectId.ObjectId -> ClauseIndex.ClauseIndex -> CardName.CardName -> Prompt.Prompt r -> r
keysAnswer room branch door p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (Set.filter ((==) (Just room) . Recipient.objectOf) . snd) asked
  Prompt.ChooseClause {} -> branch
  Prompt.ChooseHalf {} -> door
  _ -> S.identityAnswer p

-- Activate the lock ability and resolve it, triggers and all.
activateKeys :: Printing.Printing -> ObjectId.ObjectId -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
activateKeys keys keysId answer gs =
  let activated = snd (Engine.runGamePure answer gs (Activate.activateAbility S.alice keysId (lockAbility keys)))
   in resolveAll (settle (snd (Engine.runGamePure answer activated Stack.resolveTop)))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Room" $ do
  -- CR 709.5's first words are "Some split cards are permanent cards with a
  -- single shared type line", so off the battlefield a Room is CR 709.4's split
  -- card and nothing more: two names joined, the two mana costs concatenated,
  -- both halves' text. The falsifier is the double-faced reading -- front face
  -- only -- which would name the card Roaring Furnace and price it at 2.
  Spec.it s "CR 709.4 in a hand a Room is its two halves combined" $ do
    room <- S.cardOf s registry "Roaring Furnace"
    let face = Card.combined room
    Spec.assertEqWith s "both names" (Face.name face) joinedName
    Spec.assertEqWith s "{1}{R} and {3}{U}{U} concatenated" (Quantity.manaValueOf face) 7
    Spec.assertEqWith s "and both halves' triggered abilities" (length (Face.triggeredAbilities face)) 2
  -- CR 709.3: "A player chooses which half of a split card they are casting
  -- before putting it onto the stack", which the printing's own reminder text
  -- repeats ("You may cast either half"). One legal action per door, so the
  -- engine never makes the choice.
  --
  -- The second assertion is what keeps the first from passing on pricing alone:
  -- with only two lands the {3}{U}{U} door is unaffordable and would be dropped
  -- at the cost gate whatever castableFaces answered.
  Spec.it s "CR 709.3 both halves are offered as casts" $ do
    room <- S.printingOf s registry "Roaring Furnace"
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    (_, _, rich) <- setUp s registry
    let poor = fst (S.handOne room (S.landsInPlay mountain 2))
        casts gs = [c | A.Cast _ c _ <- Action.legalActions S.alice gs]
    Spec.assertEqWith
      s
      "the card proposes both of its halves"
      (fmap Face.name (Card.castableFaces (Printing.card room)))
      [furnaceName, saunaName]
    Spec.assertEqWith s "ten lands: both doors" (List.sort (casts rich)) (List.sort [furnaceName, saunaName])
    Spec.assertEqWith s "two Mountains: only the {1}{R} door" (casts poor) [furnaceName]
    -- Named so the Island fixture cannot be dropped without a failure.
    Spec.assertEqWith s "the blue door needs Islands" (Face.name (Card.combined (Printing.card island))) (CardName.MkCardName (Text.pack "Island"))
  -- THE PROVING CASE, and both halves of what CR 709.5 asks for.
  --
  -- CR 709.5d: "A permanent with a shared type line is given the 'left half
  -- unlocked' designation as it enters the battlefield if its left half was cast
  -- as a spell." Cast Roaring Furnace and only that door is open.
  --
  -- CR 709.5: the shared type line "represents two static abilities" -- "As long
  -- as this permanent doesn't have the 'right half unlocked' designation, it
  -- doesn't have the name, mana cost, or rules text of this object's right
  -- half". So the permanent is named Roaring Furnace alone, is worth 2 rather
  -- than 7, and Steaming Sauna's two abilities are simply not there: alice keeps
  -- CR 402.2's maximum hand size and draws nothing at her end step.
  --
  -- CR 709.5h fires the open door's own trigger on the way in -- "regardless of
  -- whether it was given that designation while entering the battlefield" -- for
  -- three damage, the number of cards left in alice's hand.
  --
  -- Four different wrong answers fail this case: a combined view that never
  -- subtracts (the joined name, mana value 7, and a draw at end step); a
  -- subtraction that took the wrong door (named Steaming Sauna, no damage); a
  -- designation that was dropped on the way out of the stack (no name at all and
  -- no damage); and a trigger that does not fire on the entering designation (no
  -- damage).
  Spec.it s "CR 709.5/709.5d casting one door opens it and subtracts the other" $ do
    (roomId, wallId, gs) <- setUp s registry
    let after = castDoor furnaceName roomId gs
    case roomPermanent after of
      [permId] -> do
        Spec.assertEqWith
          s
          "CR 709.5d: the left door, and only the left door, is unlocked"
          (fmap Object.unlockedHalves (Game.lookupObject permId after))
          (Just (Set.singleton furnaceName))
        -- CR 709.5c gives a Room designations rather than a face that is up, so
        -- the field CR 712.13 would have written stays empty.
        Spec.assertEqWith
          s
          "and it shows no single face"
          (fmap Object.face (Game.lookupObject permId after))
          (Just Nothing)
        Spec.assertEqWith s "the locked door's NAME is subtracted" (Projection.namesOf permId after) (Set.singleton furnaceName)
        Spec.assertEqWith
          s
          "the locked door's MANA COST is subtracted"
          (fmap Quantity.manaValueOf (Game.manaCostFaceOf permId after))
          (Just 2)
        Spec.assertEqWith
          s
          "the locked door's RULES TEXT is subtracted"
          (length (Projection.triggeredAbilitiesOf permId after))
          1
      other -> Spec.assertFailure s ("expected one Room permanent, got " <> show (length other))
    -- The open door's own text DOES work: CR 709.5h fired as the Room entered.
    Spec.assertEqWith s "three cards in hand, so three damage" (S.damageOf wallId after) (Just 3)
    -- The locked door's text does not, and these are the two observations that
    -- separate a subtracted half from an unwired one -- both are positively
    -- controlled by the unlock case below.
    Spec.assertEqWith s "alice still has a maximum hand size" (PlayerEffect.maximumHandSize S.alice after) (Just 7)
    Spec.assertEqWith
      s
      "and draws nothing at her end step"
      (S.handSize S.alice (resolveAll (settle (beginEndStep after))))
      (S.handSize S.alice after)
  -- THE POSITIVE CONTROL, and CR 709.5e's special action: "A player who controls
  -- a permanent that has one or more locked halves may pay the mana cost of a
  -- locked half of that permanent to give that permanent the appropriate
  -- unlocked designation."
  --
  -- The same board as the case above, one action later. Every assertion that
  -- case makes about Steaming Sauna's text flips.
  Spec.it s "CR 709.5e paying the unlock cost opens the other door" $ do
    (roomId, wallId, gs) <- setUp s registry
    let after = castDoor furnaceName roomId gs
    case roomPermanent after of
      [permId] -> do
        Spec.assertEqWith
          s
          "the shut door is offered, at its own price"
          (unlocksOffered after)
          [(permId, saunaName)]
        let opened = resolveAll (settle (snd (Engine.runGamePure S.identityAnswer after (Room.unlock S.alice permId saunaName))))
        Spec.assertEqWith
          s
          "CR 709.5c: both designations now"
          (fmap Object.unlockedHalves (Game.lookupObject permId opened))
          (Just (Set.fromList [furnaceName, saunaName]))
        -- CR 709.4a, THE PLURAL CASE: with both doors open the permanent has
        -- TWO names, and "has this name" is a membership test rather than a
        -- comparison. Each half answers True on its own; the joined string the
        -- combined Face renders them as answers False, which is the reading a
        -- single CardName gets wrong -- it would match "Roaring
        -- Furnace//Steaming Sauna" and neither door.
        Spec.assertEqWith s "both names" (Projection.namesOf permId opened) (Set.fromList [furnaceName, saunaName])
        Spec.assertEqWith s "the left door's name is one of them" (Projection.hasName furnaceName permId opened) True
        Spec.assertEqWith s "the right door's name is one of them" (Projection.hasName saunaName permId opened) True
        Spec.assertEqWith s "and the two joined is NOT a name it has" (Projection.hasName joinedName permId opened) False
        Spec.assertEqWith
          s
          "both mana costs"
          (fmap Quantity.manaValueOf (Game.manaCostFaceOf permId opened))
          (Just 7)
        Spec.assertEqWith s "five more lands are tapped" (S.tappedCount S.alice opened - S.tappedCount S.alice after) 5
        Spec.assertEqWith s "the static ability works now" (PlayerEffect.maximumHandSize S.alice opened) Nothing
        Spec.assertEqWith
          s
          "and so does the end-step draw"
          (S.handSize S.alice (resolveAll (settle (beginEndStep opened))) - S.handSize S.alice opened)
          1
        Spec.assertEqWith s "nothing is left to unlock" (unlocksOffered opened) []
        -- CR 709.5h's "a PARTICULAR half": the BLUE door opening is not the red
        -- door's trigger, so the wall takes no second helping of damage.
        Spec.assertEqWith s "and the other door's trigger did not fire again" (S.damageOf wallId opened) (Just 3)
      other -> Spec.assertFailure s ("expected one Room permanent, got " <> show (length other))
  -- THE PROVING CASE for CR 709.5g: "Some spells and abilities instruct a player
  -- to 'lock' half of a permanent. To lock half of a permanent, a player chooses
  -- an unlocked half of that permanent, and that permanent loses the appropriate
  -- unlocked designation."
  --
  -- Keys to the House, DSK 254, {1} Artifact: "{3}, {T}, Sacrifice this
  -- artifact: Lock or unlock a door of target Room you control. Activate only as
  -- a sorcery." The pool's producer, and the pool's only MANDATORY either-or
  -- clause pair (Pawl.Types.Clause.orElse) -- it prints no "may" over the two
  -- branches, unlike Twiddle.
  --
  -- Starts from the board the CR 709.5e case above leaves: BOTH doors open. That
  -- is what makes the door choice a real one (rule 709.5g's "an unlocked half"
  -- offers two), and it is what lets the assertions run the CR 709.5 subtraction
  -- backwards -- every reading the unlock case flipped on flips back off, and
  -- only for the door alice named.
  --
  -- alice locks STEAMING SAUNA, the SECOND candidate in printed order, so an
  -- implementation that ignored her answer and took the head would shut the red
  -- door instead and fail on the name, the mana value and the trigger count
  -- alike.
  Spec.it s "CR 709.5g locking a door takes its designation back away" $ do
    (roomId, wallId, gs) <- setUp s registry
    keys <- S.printingOf s registry "Keys to the House"
    island <- S.printingOf s registry "Island"
    let (keysId, withKeys) = S.addCreature keys S.alice gs
        -- Eight spare Islands. The board pays for the cast, CR 709.5e's unlock,
        -- the {3} here and then Steaming Sauna's {3}{U}{U} unlock cost a second
        -- time, which is what the last assertion reads -- an unlock the board
        -- could not afford is not offered whatever the designation says.
        funded = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) withKeys [1 .. 8 :: Int]
        after = castDoor furnaceName roomId funded
    case roomPermanent after of
      [permId] -> do
        let opened = resolveAll (settle (snd (Engine.runGamePure S.identityAnswer after (Room.unlock S.alice permId saunaName))))
        -- The control: both doors are open and both halves are readable, which
        -- is the state every assertion below is measured against.
        Spec.assertEqWith
          s
          "the control: both designations before the lock"
          (fmap Object.unlockedHalves (Game.lookupObject permId opened))
          (Just (Set.fromList [furnaceName, saunaName]))
        Spec.assertBool s (lockOffered keys keysId opened) "CR 307.5: the lock ability is on alice's menu"
        let locked = activateKeys keys keysId (keysAnswer permId (ClauseIndex.MkClauseIndex 0) saunaName) opened
        -- THE GAMEPLAY-LEVEL ASSERTION, and it is CR 709.5's subtraction rather
        -- than a count: with the blue door shut the permanent stops having the
        -- NAME of that half. A lock that took the wrong door leaves this set
        -- {Steaming Sauna}; a lock that never happened leaves both.
        Spec.assertEqWith s "the locked door's NAME is subtracted again" (Projection.namesOf permId locked) (Set.singleton furnaceName)
        Spec.assertEqWith
          s
          "and its MANA COST with it"
          (fmap Quantity.manaValueOf (Game.manaCostFaceOf permId locked))
          (Just 2)
        Spec.assertEqWith
          s
          "and its RULES TEXT: the static ability is gone"
          (PlayerEffect.maximumHandSize S.alice locked)
          (Just 7)
        Spec.assertEqWith
          s
          "and the end-step draw with it"
          (S.handSize S.alice (resolveAll (settle (beginEndStep locked))) - S.handSize S.alice locked)
          0
        -- CR 709.5c, underneath: the designation itself is what went away, and
        -- only the one alice named.
        Spec.assertEqWith
          s
          "CR 709.5g: the blue door's designation alone is gone"
          (fmap Object.unlockedHalves (Game.lookupObject permId locked))
          (Just (Set.singleton furnaceName))
        -- CR 709.5e reads the same derivation backwards: a door that was locked
        -- is one the special action may pay to open again.
        Spec.assertEqWith s "and the shut door is offered for its unlock cost again" (unlocksOffered locked) [(permId, saunaName)]
        -- No CR 709.5h trigger fires on a lock, and the red door's own trigger
        -- did not fire a second time either: the wall's damage is where the cast
        -- left it.
        Spec.assertEqWith s "no trigger fired on the lock" (S.damageOf wallId locked) (Just 3)
      other -> Spec.assertFailure s ("expected one Room permanent, got " <> show (length other))
  -- CR 709.5f, the same card's other branch: "To unlock half of a permanent, a
  -- player chooses a LOCKED half of that permanent, and that permanent is given
  -- the appropriate unlocked designation."
  --
  -- The same ability on a Room with one door open, with alice announcing CR
  -- 608.2d's SECOND branch instead. Two things ride on it that the lock case
  -- cannot show: that the pair is a real either-or -- the ordinal alice names is
  -- what picks the branch, and Pawl.Engine.Replay's default would have picked
  -- the first -- and that CR 709.5f's unlock reaches the same event CR 709.5e's
  -- special action does, since Roaring Furnace's damage trigger is on the door
  -- being opened rather than on how.
  --
  -- Cast the BLUE door, so the red one is what is shut and its trigger has not
  -- fired yet; the damage is then unambiguous evidence of which door opened.
  Spec.it s "CR 709.5f the other branch unlocks a door instead" $ do
    (roomId, wallId, gs) <- setUp s registry
    keys <- S.printingOf s registry "Keys to the House"
    island <- S.printingOf s registry "Island"
    let (keysId, withKeys) = S.addCreature keys S.alice gs
        funded = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) withKeys [1 .. 4 :: Int]
        after = castDoor saunaName roomId funded
    Spec.assertEqWith s "the control: the blue door alone deals no damage" (S.damageOf wallId after) (Just 0)
    case roomPermanent after of
      [permId] -> do
        Spec.assertBool s (lockOffered keys keysId after) "CR 307.5: the ability is on alice's menu"
        let unlocked = activateKeys keys keysId (keysAnswer permId (ClauseIndex.MkClauseIndex 1) furnaceName) after
        Spec.assertEqWith s "CR 709.5h: opening the red door fired its trigger" (S.damageOf wallId unlocked) (Just 3)
        Spec.assertEqWith s "and both doors are readable now" (Projection.namesOf permId unlocked) (Set.fromList [furnaceName, saunaName])
        Spec.assertEqWith
          s
          "CR 709.5f: the red door's designation was given"
          (fmap Object.unlockedHalves (Game.lookupObject permId unlocked))
          (Just (Set.fromList [furnaceName, saunaName]))
      other -> Spec.assertFailure s ("expected one Room permanent, got " <> show (length other))
  -- CR 709.5h, on the door the special action opened rather than the one the
  -- cast did: "These abilities trigger when that permanent is given the
  -- appropriate unlocked designation, REGARDLESS of whether it was given that
  -- designation while entering the battlefield or after entering the
  -- battlefield."
  --
  -- Cast the blue door instead, so Roaring Furnace's trigger has no chance to
  -- fire on the way in -- and its damage is what proves it fired when the door
  -- was paid for. The other half of the assertion is CR 709.5h's "a PARTICULAR
  -- half": opening Steaming Sauna above dealt no damage, so an implementation
  -- that fired every unlock trigger on every designation would already have
  -- failed the case before this one.
  Spec.it s "CR 709.5h the trigger fires when the special action opens its door" $ do
    (roomId, wallId, gs) <- setUp s registry
    let after = castDoor saunaName roomId gs
    Spec.assertEqWith s "the blue door alone deals no damage" (S.damageOf wallId after) (Just 0)
    case roomPermanent after of
      [permId] -> do
        Spec.assertEqWith s "the RED door is what is shut" (unlocksOffered after) [(permId, furnaceName)]
        let opened = resolveAll (settle (snd (Engine.runGamePure S.identityAnswer after (Room.unlock S.alice permId furnaceName))))
        Spec.assertEqWith s "and opening it fires the trigger" (S.damageOf wallId opened) (Just 3)
      other -> Spec.assertFailure s ("expected one Room permanent, got " <> show (length other))
  -- CR 709.5i: "Some abilities trigger when a player 'fully unlocks' a permanent
  -- with a shared type line. Such an ability triggers when that permanent has one
  -- of the two unlocked designations and gets the other, or when it has neither
  -- designation and gains both."
  --
  -- Balemurk Leech, DSK 84, {1}{B} Creature -- Leech 2/2: "Eerie -- Whenever an
  -- enchantment you control enters and whenever you fully unlock a Room, each
  -- opponent loses 1 life." Eerie is an ABILITY WORD (CR 207.2c: ability words
  -- "have no special rules meaning"), so this is CR 603.1b's "more than one
  -- trigger condition" on ONE ability -- Pawl.Types.TriggerCondition's AnyOf --
  -- rather than two abilities.
  --
  -- THREE SEATS carry the whole weight of "each opponent": on two seats "each
  -- opponent", "an opponent" and "each player" all coincide. alice's life total
  -- is asserted in both directions below, and it is the single assertion that
  -- separates CR 109.5's Opponent from EachPlayer.
  --
  -- The shape is a negative sandwiched between two positives on ONE board:
  --
  --   * the Room enters as an Enchantment alice controls, so the FIRST arm of
  --     the AnyOf fires and each opponent drops to 19;
  --   * that same board is the negative -- CR 709.5d gave the permanent its
  --     FIRST designation, not its second, so CR 709.5i is not satisfied and the
  --     Leech fired exactly once. 19 and not 18 is the whole assertion;
  --   * CR 116.2m/709.5e's special action opens the other door, the permanent
  --     "has one of the two unlocked designations and gets the other", and the
  --     SECOND arm fires once for 18.
  --
  -- The negative discriminates only because the third step is the positive on
  -- the same trigger and the same board: without it, an engine that never fired
  -- the fully-unlocked arm at all would pass step two.
  --
  -- The Room's own CR 709.5h trigger and Eerie fire simultaneously as the Room
  -- enters, and CR 603.3b lets alice order them on the stack. Every assertion is
  -- therefore about the state after everything has resolved, never about the
  -- order. The wall's damage is asserted alongside, so a board on which nothing
  -- happened at all cannot pass.
  --
  -- Life totals 20/19/18 are distinct from the Leech's 2/2 and from Roaring
  -- Furnace's 3 damage, so no assertion here can be satisfied by the wrong
  -- quantity.
  Spec.it s "CR 709.5i fully unlocking a Room fires once, and only on the second door" $ do
    (roomId, wallId, gs) <- setUp s registry
    leech <- S.printingOf s registry "Balemurk Leech"
    let (_, board) = S.addCreature leech S.alice gs
        after = castDoor furnaceName roomId board
    -- CR 603.6a through the FIRST arm: a Room is an Enchantment (CR 709.5a
    -- leaves the shared type line alone), and alice controls it.
    Spec.assertEqWith s "the enchantment entering costs bob a life" (S.lifeOf S.bob after) (Just 19)
    Spec.assertEqWith s "and carol a life" (S.lifeOf S.carol after) (Just 19)
    -- CR 109.5: "each opponent" is every player who is not the ability's
    -- controller, and alice is not one of them.
    Spec.assertEqWith s "alice loses nothing" (S.lifeOf S.alice after) (Just 20)
    -- The board is demonstrably live rather than inert: the Room's own CR 709.5h
    -- trigger resolved on the way in.
    Spec.assertEqWith s "and the Room's own trigger still dealt its damage" (S.damageOf wallId after) (Just 3)
    case roomPermanent after of
      [permId] -> do
        -- THE NEGATIVE. One designation, and it is the permanent's FIRST -- CR
        -- 709.5i wants the OTHER designation arriving on a permanent that
        -- already has one. The 19s above are the Leech having fired once, not
        -- twice.
        Spec.assertEqWith
          s
          "CR 709.5d gave one designation, so CR 709.5i is not satisfied"
          (fmap Object.unlockedHalves (Game.lookupObject permId after))
          (Just (Set.singleton furnaceName))
        let opened = resolveAll (settle (snd (Engine.runGamePure S.identityAnswer after (Room.unlock S.alice permId saunaName))))
        Spec.assertEqWith
          s
          "the control: CR 709.5e opened the second door"
          (fmap Object.unlockedHalves (Game.lookupObject permId opened))
          (Just (Set.fromList [furnaceName, saunaName]))
        -- CR 709.5i fires ONCE, so one more life apiece and no more.
        Spec.assertEqWith s "fully unlocking costs bob one more life" (S.lifeOf S.bob opened) (Just 18)
        Spec.assertEqWith s "and carol one more" (S.lifeOf S.carol opened) (Just 18)
        Spec.assertEqWith s "and alice still nothing" (S.lifeOf S.alice opened) (Just 20)
      other -> Spec.assertFailure s ("expected one Room permanent, got " <> show (length other))
  -- CR 116.2m / 709.5e's TIMING: "A player can take this action any time they
  -- have priority and the stack is empty during a main phase of their turn."
  --
  -- Three refusals, each against the same positively controlled board -- the
  -- offer the proving case already saw. Without the control each refusal would
  -- pass for an engine that never offers an unlock at all.
  Spec.it s "CR 116.2m the unlock is offered only at sorcery speed" $ do
    (roomId, _, gs) <- setUp s registry
    let after = castDoor furnaceName roomId gs
        onStack = after {GameState.stack = [roomId]}
        inCombat = after {GameState.phase = Phase.Combat CombatStep.BeginningOfCombat}
        bobsTurn = after {GameState.activePlayer = S.bob}
    Spec.assertBool s (not (null (unlocksOffered after))) "the control: a main phase with an empty stack offers the unlock"
    Spec.assertEqWith s "not with a nonempty stack" (unlocksOffered onStack) []
    Spec.assertEqWith s "not outside a main phase" (unlocksOffered inCombat) []
    Spec.assertEqWith s "not on another player's turn" (unlocksOffered bobsTurn) []
  -- CR 709.5e's other gate, and the one that is not about timing: the unlock
  -- cost has to be payable. Tapping alice's lands leaves the door shut, and the
  -- untapped board above is the control.
  Spec.it s "CR 709.5e an unaffordable door is not offered" $ do
    (roomId, _, gs) <- setUp s registry
    let after = castDoor furnaceName roomId gs
        drained = List.foldl' (flip S.tapObject) after (Set.toList (GameState.battlefield after))
    Spec.assertBool s (not (null (unlocksOffered after))) "the control: ten untapped lands pay {3}{U}{U}"
    Spec.assertEqWith s "a tapped-out controller is offered nothing" (unlocksOffered drained) []
  -- CR 400.7: "an object that moves from one zone to another becomes a new object
  -- with no memory of, or relation to, its previous existence." An unlocked
  -- designation is per-incarnation state and goes back with the rest, which is
  -- what makes CR 709.5d the ONLY writer of it on an entry -- a Room that dies
  -- and is returned comes back with its doors shut unless a half was cast again.
  --
  -- Asserted on Object.newIncarnation directly rather than through a bounce,
  -- because Pawl.Engine.Event.changeZoneAttaching writes the field on every move
  -- as well (CR 709.5d) and would mask the forgetting on the funnel path. The
  -- three hand-written movers that call newIncarnation and not the funnel --
  -- Setup's CR 727.2 restart and CR 729.5 funnel-back, and Departure's CR 800.4a
  -- exile -- are what this covers.
  Spec.it s "CR 400.7 a new incarnation of a Room has neither designation" $ do
    (roomId, _, gs) <- setUp s registry
    let after = castDoor furnaceName roomId gs
    case roomPermanent after of
      [permId] -> case Game.lookupObject permId after of
        Nothing -> Spec.assertFailure s "expected to find the Room permanent"
        Just obj -> do
          Spec.assertEqWith s "the control: this incarnation has the red door open" (Object.unlockedHalves obj) (Set.singleton furnaceName)
          Spec.assertEqWith s "the next one has neither" (Object.unlockedHalves (Object.newIncarnation obj)) Set.empty
      other -> Spec.assertFailure s ("expected one Room permanent, got " <> show (length other))
  -- CR 709.5d's last sentence: "If it's entering the battlefield and neither half
  -- was cast as a spell, it enters with neither unlocked designation." Put the
  -- card onto the battlefield from the hand and both doors are shut -- so CR
  -- 709.5 subtracts both halves and the permanent has no name and no text at
  -- all.
  Spec.it s "CR 709.5d a Room put onto the battlefield enters with both doors shut" $ do
    (roomId, _, gs) <- setUp s registry
    let put = snd (Engine.runGamePure S.identityAnswer gs (Event.changeZoneEntering roomId Zone.Battlefield LibraryPosition.defaultValue plainEntry Nothing))
    case roomPermanent put of
      [permId] -> do
        Spec.assertEqWith
          s
          "no unlocked designation"
          (fmap Object.unlockedHalves (Game.lookupObject permId put))
          (Just Set.empty)
        Spec.assertEqWith s "and so no name" (Projection.namesOf permId put) Set.empty
        Spec.assertEqWith s "and no rules text" (length (Projection.triggeredAbilitiesOf permId put)) 0
        -- CR 709.5a: "Each half of a split card with a shared type line shares
        -- the types and subtypes listed on that card's shared type line." CR
        -- 709.5 subtracts the name, the mana cost and the rules text -- and
        -- nothing else -- so a Room with both doors shut is still an
        -- Enchantment Room on the battlefield.
        Spec.assertEqWith s "though CR 709.5a leaves the card types" (Projection.cardTypesOf permId put) (Set.singleton CardType.Enchantment)
        Spec.assertEqWith s "and the shared subtype" (Projection.subtypesOf permId put) (Set.singleton Subtype.Room)
      other -> Spec.assertFailure s ("expected one Room permanent, got " <> show (length other))
