-- Covers CR 709.5's Room cards end to end: Pawl.Types.Layout's Room arm and the
-- Pawl.Engine.Card functions that read it (CR 709.4's combined view, CR 709.3's
-- castable halves, CR 709.5's roomFace subtraction, CR 709.5d's entering
-- designation), plus the four modules those classifications reach --
-- Pawl.Engine.Game.faceOf substitutes the subtracted view,
-- Pawl.Engine.Event.changeZoneAttaching writes the entering designation and
-- records CR 709.5h's event, Pawl.Engine.Room offers and carries out CR
-- 116.2m/709.5e's special action, and Pawl.Engine.Action/Pawl.Engine.Engine put
-- it on the menu. CR 709.5i's "fully unlocks" is here too, which reaches
-- Event.fullyUnlockedAfter at both writers of an unlocked designation and
-- Event.matchesTrigger's RoomFullyUnlocked and AnyOf arms.
--
-- TWO printed cards. Roaring Furnace // Steaming Sauna, DSK 230, picked because
-- its two doors disagree about everything a reader can see -- {1}{R} against
-- {3}{U}{U}, a triggered ability that fires on the door opening against a
-- persistent pair (a static "you have no maximum hand size" and an end-step
-- draw) -- so a permanent whose wrong half was subtracted is visible in every
-- assertion here rather than in one narrow one. The persistent half is what
-- makes the subtraction observable at all: a door whose only text is "when you
-- unlock this door" looks the same whether its text was subtracted or its
-- trigger simply did not match. And Balemurk Leech, DSK 84, which is the pool's
-- one CR 709.5i trigger and the one card whose ability watches a Room it is not.
--
-- THREE SEATS, so "target creature an opponent controls" cannot coincide with
-- "a creature you control": bob holds the only legal target and carol holds
-- none -- and so that CR 109.5's "each opponent" cannot coincide with "each
-- player", which is what the CR 709.5i case turns on.
module Pawl.RoomSpec where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
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
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
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
    (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)
    (gs {GameState.phase = Phase.Ending EndingStep.EndStep})

-- The Room permanent on the battlefield: the one permanent that is not a land
-- and not bob's wall. Found by having a card name rather than followed by id,
-- since CR 400.7 mints a new one as the spell resolves.
roomPermanent :: GameState.GameState -> [ObjectId.ObjectId]
roomPermanent gs =
  [ o
  | o <- Set.toList (GameState.battlefield gs),
    elem (Projection.nameOf o gs) [furnaceName, saunaName, joinedName, CardName.MkCardName Text.empty]
  ]

-- The riders an effect that merely puts a permanent onto the battlefield asks
-- for: CR 110.5b's untapped, no CR 508.1 attacking entry, and CR 712.14's
-- untransformed default.
plainEntry :: EntryRiders.EntryRiders
plainEntry = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False}

-- Every unlock this player is offered right now, as CR 709.5e's pair.
unlocksOffered :: GameState.GameState -> [(ObjectId.ObjectId, CardName.CardName)]
unlocksOffered gs = [(o, n) | A.Unlock o n <- Action.legalActions S.alice gs]

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
        Spec.assertEqWith s "the locked door's NAME is subtracted" (Projection.nameOf permId after) furnaceName
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
        Spec.assertEqWith s "both names" (Projection.nameOf permId opened) joinedName
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
        Spec.assertEqWith s "and so no name" (Projection.nameOf permId put) (CardName.MkCardName Text.empty)
        Spec.assertEqWith s "and no rules text" (length (Projection.triggeredAbilitiesOf permId put)) 0
        -- CR 709.5a: "Each half of a split card with a shared type line shares
        -- the types and subtypes listed on that card's shared type line." CR
        -- 709.5 subtracts the name, the mana cost and the rules text -- and
        -- nothing else -- so a Room with both doors shut is still an
        -- Enchantment Room on the battlefield.
        Spec.assertEqWith s "though CR 709.5a leaves the card types" (Projection.cardTypesOf permId put) (Set.singleton CardType.Enchantment)
        Spec.assertEqWith s "and the shared subtype" (Projection.subtypesOf permId put) (Set.singleton Subtype.Room)
      other -> Spec.assertFailure s ("expected one Room permanent, got " <> show (length other))
