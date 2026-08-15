{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Dungeon (CR 309 and CR 701.49, "venture into the dungeon"),
-- the two fields it writes -- Pawl.Types.Object's ventureRoom and
-- Pawl.Types.Player's dungeon -- Pawl.Engine.Resolve's Effect.Venture arm, and
-- Pawl.Engine.Sba's CR 704.5t pass.
--
-- Gameplay-level throughout: every case activates Secret Door's "{4}{U}: Venture
-- into the dungeon" and resolves it through the stack rather than calling
-- `venture` directly, so what is asserted is the whole path from card JSON to the
-- room ability's effect on the board.
module Pawl.DungeonSpec where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Dungeon as Dungeon
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.Zone as Zone

-- Secret Door's one activated ability, "{4}{U}: Venture into the dungeon.
-- Activate only as a sorcery." Taken off the printing rather than rebuilt, so the
-- case exercises the card file.
ventureAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Card
ventureAbility printing = case concatMap Face.activatedAbilities (Card.faces (Printing.card printing)) of
  ab : _ -> ab
  [] -> error "Secret Door has no activated ability"

-- The room this player's dungeon card has its venture marker on (CR 309.4), or
-- Nothing when they own no dungeon in the command zone.
markerOf :: PlayerId -> GameState.GameState -> Maybe RoomIndex.RoomIndex
markerOf pid gs = Dungeon.inDungeon pid gs >>= \oid -> Game.lookupObject oid gs >>= Object.ventureRoom

-- Every dungeon card this player owns in the command zone (CR 309.3). A LIST and
-- not a Bool, because "did re-entering mint a SECOND dungeon?" is a question the
-- rule cares about and a Bool cannot answer.
dungeonsOf :: PlayerId -> GameState.GameState -> [ObjectId]
dungeonsOf pid gs =
  let isDungeon oid = maybe False Dungeon.isDungeonFace (Game.faceOf oid gs)
   in filter isDungeon (Game.zoneMembers Zone.Command pid gs)

-- The names of everything on the battlefield, sorted. What the room abilities are
-- read through: a Goblin token and a Treasure token are two different names, which
-- is how "which room did the marker enter?" becomes observable at gameplay level.
namesInPlay :: GameState.GameState -> [String]
namesInPlay gs =
  List.sort
    ( Maybe.mapMaybe
        (\oid -> fmap (show . CardName.unwrap . Face.name) (Game.faceOf oid gs))
        (Set.toList (GameState.battlefield gs))
    )

-- Answers every prompt an activation and a room ability raise: pay mana with
-- whatever is offered first, take every target offered, and otherwise defer to
-- S.identityAnswer -- which answers Prompt.ChooseRoom with the FIRST arrow.
paying :: Prompt.Prompt r -> r
paying p = case p of
  Prompt.ChooseManaSource _ _ candidates -> Just (NonEmpty.head candidates)
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(n, cands) -> Set.fromList (take (Natural.toIntSaturating n) (Set.toList cands))) sets
  _ -> S.identityAnswer p

-- `paying`, but answering Prompt.ChooseRoom with the LAST arrow offered.
--
-- The discriminator, and the reason both exist: Dungeon.advance offers a room's
-- arrows in ascending order, so an implementation that never prompted -- or that
-- ignored the reply -- would always follow the lowest-numbered one. A case
-- asserting the room the OTHER arrow leads to passes only for an implementation
-- that asked and honoured the answer.
payingLastRoom :: Prompt.Prompt r -> r
payingLastRoom p = case p of
  Prompt.ChooseRoom _ _ _ candidates -> NonEmpty.last candidates
  _ -> paying p

-- Resolve the whole stack, settling between each resolution so a room ability that
-- triggered on the venture is placed and then resolved too. Bounded, so a bug that
-- kept the stack full fails the case rather than hanging it.
resolveAll :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAll answer = go (20 :: Int)
  where
    go n gs
      | n <= 0 = gs
      | null (GameState.stack gs) = gs
      | otherwise = go (n - 1) (S.runPure answer gs (Stack.resolveTop >> Engine.settleForPriority))

-- One venture: activate Secret Door's ability, then resolve everything it puts on
-- the stack.
ventureOnce :: (forall r. Prompt.Prompt r -> r) -> ActivatedAbility.ActivatedAbility Card.Card -> ObjectId -> GameState.GameState -> GameState.GameState
ventureOnce answer ability doorId gs =
  let activated = S.runPure answer gs (Activate.activateAbility S.alice doorId ability)
   in resolveAll answer (S.runPure answer activated Engine.settleForPriority)

-- `n` ventures in a row, all answered the same way.
ventureTimes :: Int -> (forall r. Prompt.Prompt r -> r) -> ActivatedAbility.ActivatedAbility Card.Card -> ObjectId -> GameState.GameState -> GameState.GameState
ventureTimes n answer ability doorId gs = List.foldl' (\acc _ -> ventureOnce answer ability doorId acc) gs [1 .. n]

-- alice controls a Secret Door and `lands` untapped Islands, owns Lost Mine of
-- Phandelver outside the game (CR 309.2), and has cards left to draw.
--
-- The lands are what make repeated ventures possible in ONE main phase: Secret
-- Door's ability has no tap in its cost, so the only limit on how deep a case can
-- go is mana.
dungeonBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId, GameState.GameState)
dungeonBoard island door lostMine lands =
  let stocked = List.foldl' (\g _ -> snd (S.addLibraryCard island S.alice g)) (S.landsInPlay island lands) [1 .. (4 :: Int)]
      (doorId, g1) = S.addCreature door S.alice stocked
      -- CR 309.2: the dungeon is recorded on the player and no object is minted
      -- for it, so its printing is interned here rather than by an object build.
      (lostMineId, g2) = Game.intern lostMine g1
      owned p = p {Player.dungeon = Just lostMineId}
   in (doorId, g2 {GameState.players = Map.adjust owned S.alice (GameState.players g2)})

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Dungeon" $ do
  -- CR 701.49a and CR 309.4a: the first venture brings the dungeon in and puts the
  -- marker on the topmost room. The gate.
  Spec.it s "CR 701.49a the first venture puts the dungeon in the command zone with the marker on the topmost room" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    let (doorId, gs) = dungeonBoard island door lostMine 5
        after = ventureOnce paying (ventureAbility door) doorId gs
    Spec.assertEqWith s "one dungeon in the command zone" (length (dungeonsOf S.alice after)) 1
    Spec.assertEqWith s "the marker is on the topmost room" (markerOf S.alice after) (Just RoomIndex.topmost)
    -- CR 309.2a: it is the card alice owns, by name.
    Spec.assertEqWith
      s
      "and it is the dungeon she owns"
      (fmap (\oid -> fmap Face.name (Game.faceOf oid after)) (dungeonsOf S.alice after))
      [Just (CardName.MkCardName (Text.pack "Lost Mine of Phandelver"))]
  -- CR 309.4c and CR 701.22a: entering Cave Entrance triggers its room ability,
  -- which is the printed "Scry 1". The dungeon carried no ability there at all
  -- until Effect.Scry landed, so this is the case that proves the topmost room
  -- does its job.
  --
  -- One Mountain on top of dungeonBoard's four Islands, because the Islands are
  -- interchangeable: a library of one printing cannot tell a card that moved
  -- from a card that stayed. The two boards are identical and differ only in
  -- the answer given to Prompt.ChooseScry.
  Spec.it s "CR 309.4c / 701.22a Cave Entrance's Scry 1 puts the top card where its controller says" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    let (doorId, base) = dungeonBoard island door lostMine 5
        (marker, gs) = S.addLibraryCard mountain S.alice base
        answering split p = case p of
          Prompt.ChooseScry {} -> split
          _ -> paying p
        libraryOf = Game.zoneMembers Zone.Library S.alice
        bottomed = ventureOnce (answering ([marker], [])) (ventureAbility door) doorId gs
        kept = ventureOnce (answering ([], [marker])) (ventureAbility door) doorId gs
    Spec.assertEqWith s "the Mountain starts on top" (take 1 (libraryOf gs)) [marker]
    Spec.assertEqWith s "the marker is on Cave Entrance either way" (markerOf S.alice bottomed, markerOf S.alice kept) (Just RoomIndex.topmost, Just RoomIndex.topmost)
    Spec.assertEqWith s "bottoming it moved it to the bottom" (take 1 (reverse (libraryOf bottomed))) [marker]
    Spec.assertBool s (take 1 (libraryOf bottomed) /= [marker]) "so it is no longer on top"
    Spec.assertEqWith s "and keeping it left it on top" (take 1 (libraryOf kept)) [marker]
  -- CR 309.5a: "if there are multiple arrows pointing away from the room the
  -- player's venture marker is on, THEY CHOOSE one of them to follow."
  --
  -- The two boards differ in exactly one thing -- the answer to Prompt.ChooseRoom
  -- -- and the rooms the two arrows lead to have DIFFERENT effects, so which room
  -- was entered is observable on the battlefield rather than only in the marker.
  -- Cave Entrance's own arrows lead to Goblin Lair (a 1/1 Goblin) and Mine Tunnels
  -- (a Treasure).
  Spec.it s "CR 309.5a a room with two arrows asks which one to follow, and the answer decides the room" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    let (doorId, gs) = dungeonBoard island door lostMine 10
        firstArrow = ventureTimes 2 paying (ventureAbility door) doorId gs
        lastArrow = ventureTimes 2 payingLastRoom (ventureAbility door) doorId gs
    Spec.assertEqWith s "the first arrow leads to Goblin Lair" (markerOf S.alice firstArrow) (Just (RoomIndex.MkRoomIndex 1))
    Spec.assertEqWith s "the last arrow leads to Mine Tunnels" (markerOf S.alice lastArrow) (Just (RoomIndex.MkRoomIndex 2))
    -- CR 309.4c: the room ability of the room ENTERED, and only that one.
    Spec.assertBool s (List.elem "\"Goblin Token\"" (namesInPlay firstArrow)) "Goblin Lair created a Goblin"
    Spec.assertBool s (notElem "\"Treasure Token\"" (namesInPlay firstArrow)) "and not a Treasure"
    Spec.assertBool s (List.elem "\"Treasure Token\"" (namesInPlay lastArrow)) "Mine Tunnels created a Treasure"
    Spec.assertBool s (notElem "\"Goblin Token\"" (namesInPlay lastArrow)) "and not a Goblin"
  -- CR 701.49b: venturing while already in a dungeon MOVES the marker rather than
  -- entering a second dungeon. CR 309.3 is the other half of the same sentence --
  -- "a player can own only one dungeon card in the command zone at a time".
  --
  -- Three ventures, so the marker has moved twice and the second move went through
  -- a room the first venture could not have reached.
  Spec.it s "CR 701.49b / 309.3 venturing again advances the one dungeon instead of entering another" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    let (doorId, gs) = dungeonBoard island door lostMine 15
        after = ventureTimes 3 paying (ventureAbility door) doorId gs
    Spec.assertEqWith s "still exactly one dungeon" (length (dungeonsOf S.alice after)) 1
    -- Cave Entrance -> Goblin Lair -> Storeroom, following the first arrow each
    -- time.
    Spec.assertEqWith s "the marker reached Storeroom" (markerOf S.alice after) (Just (RoomIndex.MkRoomIndex 3))
    -- CR 603.3d: Storeroom's "put a +1/+1 counter on target creature" chose its
    -- target as the ability was placed, from the command zone.
    Spec.assertBool
      s
      (any (\oid -> maybe False (not . Map.null . Object.counters) (Game.lookupObject oid after)) (Set.toList (GameState.battlefield after)))
      "Storeroom's room ability put a counter on something"
  -- CR 704.5t / 309.6: the marker reaching the bottommost room removes the dungeon
  -- from the game -- but only once its room ability has left the stack, which is
  -- what makes Temple of Dumathoin's draw observable at all.
  --
  -- CR 701.49a then applies again, because the player is once more in no dungeon:
  -- the fifth venture enters a FRESH Lost Mine of Phandelver at the topmost room.
  -- CR 309.5b is why the same card may come back -- it was removed from the game,
  -- and outside the game is where alice's copy lives.
  Spec.it s "CR 704.5t / 701.49a the bottommost room ends the dungeon, and venturing again starts a new one" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    let (doorId, gs) = dungeonBoard island door lostMine 30
        before = S.handSize S.alice gs
        finished = ventureTimes 4 paying (ventureAbility door) doorId gs
        again = ventureOnce paying (ventureAbility door) doorId finished
    -- CR 309.4c: Temple of Dumathoin drew a card, so the last room's ability
    -- resolved BEFORE CR 704.5t removed the card it was printed on.
    Spec.assertEqWith s "Temple of Dumathoin's draw resolved" (S.handSize S.alice finished) (before + 1)
    Spec.assertEqWith s "and the dungeon left the game" (dungeonsOf S.alice finished) []
    Spec.assertEqWith s "venturing again enters a dungeon" (length (dungeonsOf S.alice again)) 1
    Spec.assertEqWith s "at the topmost room" (markerOf S.alice again) (Just RoomIndex.topmost)
  -- CR 309.2: the dungeon a player owns comes from their DECK definition, which is
  -- the one path the boards above skip by writing Player.dungeon directly.
  Spec.it s "CR 309.2 a deck's dungeon card is recorded on its player and minted into no zone" $ do
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    let deck = Deck.MkDeck {Deck.cards = Map.empty, Deck.commander = Nothing, Deck.dungeon = Just lostMine}
        after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice deck)
    Spec.assertEqWith s "alice owns it" (Map.lookup S.alice (GameState.players after) >>= Player.dungeon >>= \i -> Game.printingOf i after) (Just lostMine)
    Spec.assertEqWith s "bob owns none" (Map.lookup S.bob (GameState.players after) >>= Player.dungeon) Nothing
    -- CR 309.2 / 400.11: it begins OUTSIDE the game, so setup mints no object for
    -- it at all.
    Spec.assertEqWith s "and no object was minted for it" (Map.size (GameState.objects after)) 0
