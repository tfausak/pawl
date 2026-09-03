{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Dungeon (CR 309 and CR 701.49, "venture into the dungeon"),
-- the three fields it reads or writes -- Pawl.Types.Object's ventureRoom and
-- Pawl.Types.Player's dungeons and completedDungeons -- Pawl.Engine.Resolve's
-- Effect.Venture arm, and Pawl.Engine.Sba's CR 704.5t pass.
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
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Dungeon as Dungeon
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Secret Door's one activated ability, "{4}{U}: Venture into the dungeon.
-- Activate only as a sorcery." Taken off the printing rather than rebuilt, so the
-- case exercises the card file.
ventureAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)
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

-- The NAMES of the dungeon cards this player owns in the command zone. Which
-- dungeon a venture entered is the whole subject of CR 701.49d, and a card name
-- is how a board says it.
dungeonNamesOf :: PlayerId -> GameState.GameState -> [String]
dungeonNamesOf pid gs =
  List.sort
    ( Maybe.mapMaybe
        (\oid -> fmap (show . CardName.unwrap . Face.name) (Game.faceOf oid gs))
        (dungeonsOf pid gs)
    )

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

-- `paying`, but answering Prompt.ChooseDungeon with the LAST dungeon offered.
--
-- The discriminator for CR 309.2a, and `payingLastRoom`'s reason one prompt over:
-- Dungeon.enter offers the printings a player owns in ascending interned order, so
-- an implementation that never prompted -- or that ignored the reply -- would
-- always enter the lowest-numbered one. A case asserting what the OTHER dungeon's
-- topmost room did passes only for an implementation that asked and honoured the
-- answer.
payingLastDungeon :: Prompt.Prompt r -> r
payingLastDungeon p = case p of
  Prompt.ChooseDungeon _ _ candidates -> NonEmpty.last candidates
  _ -> paying p

-- `paying`, but answering Prompt.ChooseDungeon with the FIRST dungeon offered and
-- running Secret Entrance's search to completion.
--
-- `paying` alone declines a search (S.identityAnswer's posture), which would find
-- no land and leave the two boards of the CR 701.49d case below indistinguishable
-- by hand size. The two search arms only make Undercity's topmost room DO its
-- printed thing; which dungeon was entered is decided before either is raised.
payingFirstDungeon :: Prompt.Prompt r -> r
payingFirstDungeon p = case p of
  Prompt.ChooseDungeon _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseSearchZones _ _ zones -> zones
  Prompt.Search _ _ candidates count -> take (Natural.toIntSaturating count) candidates
  _ -> paying p

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
ventureOnce :: (forall r. Prompt.Prompt r -> r) -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> ObjectId -> GameState.GameState -> GameState.GameState
ventureOnce answer ability doorId gs =
  let activated = S.runPure answer gs (Activate.activateAbility S.alice doorId ability)
   in resolveAll answer (S.runPure answer activated Engine.settleForPriority)

-- `n` ventures in a row, all answered the same way.
ventureTimes :: Int -> (forall r. Prompt.Prompt r -> r) -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> ObjectId -> GameState.GameState -> GameState.GameState
ventureTimes n answer ability doorId gs = List.foldl' (\acc _ -> ventureOnce answer ability doorId acc) gs [1 .. n]

-- The battlefield objects whose face carries this name, as `namesInPlay` spells
-- one.
inPlayNamed :: String -> GameState.GameState -> [ObjectId]
inPlayNamed name gs =
  filter
    (\oid -> fmap (show . CardName.unwrap . Face.name) (Game.faceOf oid gs) == Just name)
    (Set.toList (GameState.battlefield gs))

-- alice controls a Synthetic Undercity Stair and 25 untapped Islands -- five
-- activations of its {4}{U} -- owns Undercity outside the game, and has a library
-- of exactly TEN cards: Goblin Piker, Cabal Evangel and eight Islands.
--
-- Ten so that Throne of the Dead Three's "reveal the top ten cards of your
-- library" covers the WHOLE library whatever order the earlier rooms leave it in.
-- Every card the room's "then shuffle" randomises is then one the reveal left in
-- place, which is the negative the case below turns on, and both creature cards
-- are among the revealed however the shuffles fell.
--
-- Nothing on the last-arrow path leaves the library: Secret Entrance's search is
-- declined, Lost Well only scries, and Stash and Catacombs create tokens. So the
-- count holds through all five ventures without the case having to predict a
-- permutation.
throneBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId, ObjectId, ObjectId, GameState.GameState)
throneBoard island piker evangel stair undercity =
  let stocked = List.foldl' (\g _ -> snd (S.addLibraryCard island S.alice g)) (S.landsInPlay island 25) [1 .. (8 :: Int)]
      (pikerId, g1) = S.addLibraryCard piker S.alice stocked
      (evangelId, g2) = S.addLibraryCard evangel S.alice g1
      (stairId, g3) = S.addCreature stair S.alice g2
      (dungeonId, g4) = Game.intern undercity g3
      owned p = p {Player.dungeons = Set.singleton dungeonId}
   in (stairId, pikerId, evangelId, g4 {GameState.players = Map.adjust owned S.alice (GameState.players g4)})

-- `payingLastRoom`, plus the two answers Throne of the Dead Three needs.
--
-- Prompt.Shuffle is REVERSED. CR 701.24a leaves a shuffle observable only through
-- the order it produces, and the engine rolls nothing -- it asks -- so this
-- fixture's permutation is the whole of the randomness and the case is
-- deterministic. Reversing rather than permuting: a library of two or more
-- distinct cards is never its own reverse, so "the order changed" is decidable
-- against the pre-shuffle list rather than against a seed.
--
-- Prompt.ChooseCardFromAmong is FILTERED to `chosen` rather than answered by
-- position: the offered set is what the engine built out of the revealed group,
-- and picking out of it is what makes "the room asked and honoured the answer"
-- observable -- the other creature card is left in the library, where the
-- assertions read it.
throneAnswer :: ObjectId -> Prompt.Prompt r -> r
throneAnswer chosen p = case p of
  Prompt.Shuffle ids -> reverse ids
  Prompt.ChooseCardFromAmong _ _ _ offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find (== chosen) (NonEmpty.toList offered))
  _ -> payingLastRoom p

-- alice controls a Secret Door and `lands` untapped Islands, owns `dungeons`
-- outside the game (CR 309.2), and has cards left to draw.
--
-- The lands are what make repeated ventures possible in ONE main phase: Secret
-- Door's ability has no tap in its cost, so the only limit on how deep a case can
-- go is mana.
--
-- The dungeons are interned in the order given, and Player.dungeons is ordered by
-- interned id, so the FIRST argument is the head of what Prompt.ChooseDungeon
-- offers -- which is what `payingLastDungeon` above is the discriminator against.
dungeonBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> Int -> (ObjectId, GameState.GameState)
dungeonBoard island door dungeons lands =
  let stocked = List.foldl' (\g _ -> snd (S.addLibraryCard island S.alice g)) (S.landsInPlay island lands) [1 .. (4 :: Int)]
      (doorId, g1) = S.addCreature door S.alice stocked
      -- CR 309.2: a dungeon is recorded on the player and no object is minted for
      -- it, so its printing is interned here rather than by an object build.
      intern (ids, g) printing = let (i, g') = Game.intern printing g in (ids <> [i], g')
      (dungeonIds, g2) = List.foldl' intern ([], g1) dungeons
      owned p = p {Player.dungeons = Set.fromList dungeonIds}
   in (doorId, g2 {GameState.players = Map.adjust owned S.alice (GameState.players g2)})

-- The names of the cards in one of a player's zones. What both cases below read,
-- because CR 400.7 makes a returned card a new object with a new id and only its
-- printed name survives the trip.
namesIn :: Zone.Zone -> PlayerId -> GameState.GameState -> Set.Set CardName.CardName
namesIn zone pid gs = Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))

-- `paying`, but taking every "may" offered.
--
-- The only optional clause either case below can raise is the one being read --
-- Dungeon Crawler's return, and Acererak's own trigger is mandatory -- so this is
-- not a blanket yes standing in for a specific answer.
payingOptional :: Prompt.Prompt r -> r
payingOptional p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> paying p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Dungeon" $ do
  -- CR 701.49a and CR 309.4a: the first venture brings the dungeon in and puts the
  -- marker on the topmost room. The gate.
  Spec.it s "CR 701.49a the first venture puts the dungeon in the command zone with the marker on the topmost room" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    let (doorId, gs) = dungeonBoard island door [lostMine] 5
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
    let (doorId, base) = dungeonBoard island door [lostMine] 5
        (marker, gs) = S.addLibraryCard mountain S.alice base
        answering :: ([ObjectId], [ObjectId]) -> Prompt.Prompt r -> r
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
    let (doorId, gs) = dungeonBoard island door [lostMine] 10
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
    let (doorId, gs) = dungeonBoard island door [lostMine] 15
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
    let (doorId, gs) = dungeonBoard island door [lostMine] 30
        before = S.handSize S.alice gs
        finished = ventureTimes 4 paying (ventureAbility door) doorId gs
        again = ventureOnce paying (ventureAbility door) doorId finished
    -- CR 309.4c: Temple of Dumathoin drew a card, so the last room's ability
    -- resolved BEFORE CR 704.5t removed the card it was printed on.
    Spec.assertEqWith s "Temple of Dumathoin's draw resolved" (S.handSize S.alice finished) (before + 1)
    Spec.assertEqWith s "and the dungeon left the game" (dungeonsOf S.alice finished) []
    Spec.assertEqWith s "venturing again enters a dungeon" (length (dungeonsOf S.alice again)) 1
    Spec.assertEqWith s "at the topmost room" (markerOf S.alice again) (Just RoomIndex.topmost)
  -- CR 309.7: "a player completes a dungeon as that dungeon card is removed from
  -- the game." Read at gameplay level through Gloom Stalker, whose "as long as
  -- you've completed a dungeon, this creature has double strike" is a CR 604.2
  -- static whose condition is Quantity.DungeonsCompleted compared to 1.
  --
  -- The board is the case above's -- four ventures down the FIRST arrow each time
  -- (Cave Entrance, Goblin Lair, Storeroom, Temple of Dumathoin), which that case
  -- already proves ends with the dungeon removed from the game. Nothing on that
  -- path touches a life total; Dark Pool, on the last-arrow path, does, which is
  -- the other reason for the first arrow.
  --
  -- What discriminates is CR 702.4b's SECOND combat damage step. Gloom Stalker is
  -- 2/3 and the only attacker -- Secret Door has defender (CR 702.3b) and the
  -- Goblin Lair token was created this turn and is sick (CR 302.6) -- so bob ends
  -- on 18 without the double strike and on 16 with it. No other object on the
  -- board can move his life, and only a second combat damage step reaches 16.
  Spec.it s "CR 309.7 / 702.4b completing a dungeon is remembered, and Gloom Stalker's double strike reads it" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    stalker <- S.printingOf s registry "Gloom Stalker"
    let (doorId, base) = dungeonBoard island door [lostMine] 30
        (stalkerId, gs) = S.addCreature stalker S.alice base
        -- Storeroom's "put a +1/+1 counter on target creature" is the one prompt
        -- on this path with a choice worth pinning: a counter landing on Gloom
        -- Stalker would make it 3/4 and shift both asserted life totals, so the
        -- board would stop being checkable by hand. FILTERED out of the offered
        -- set rather than hand-built, so the recipient is the engine's own.
        onTheDoor :: Prompt.Prompt r -> r
        onTheDoor p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, cands) -> Set.filter ((== Just doorId) . Recipient.objectOf) cands) sets
          _ -> paying p
        finished = ventureTimes 4 onTheDoor (ventureAbility door) doorId gs
        -- CR 506.2: alice is active, so bob defends. Stated rather than derived,
        -- for S.combatBoardOf's reason -- a board assembled by direct calls never
        -- ran CR 703.4h's turn-based action.
        staged =
          finished
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]},
              GameState.remaining =
                Seq.fromList
                  [ Phase.Combat CombatStep.DeclareBlockers,
                    Phase.Combat CombatStep.CombatDamage,
                    Phase.Combat CombatStep.EndOfCombat,
                    Phase.PostcombatMain
                  ]
            }
        after = S.runCombat (S.attackTo S.bob) staged
    -- The gameplay-level assertion, and FIRST, so no fence below can absorb a
    -- mutation ahead of it.
    Spec.assertEqWith s "bob took 2 twice" (S.lifeOf S.bob after) (Just 16)
    -- The dungeon really did leave the game, so the case is not green because
    -- nothing happened.
    Spec.assertEqWith s "and the dungeon had left the game" (dungeonsOf S.alice finished) []
    -- Count AND identity, so a Goblin that wandered into combat is visible.
    Spec.assertEqWith s "Gloom Stalker was the only attacker" (S.attackerDeclarationsOf after) [stalkerId]
    Spec.assertEqWith
      s
      "and alice has one dungeon completed"
      (fmap Player.completedDungeons (Map.lookup S.alice (GameState.players after)))
      (Just 1)
  -- CR 309.2a \/ 701.49a: "they choose a dungeon card they own from outside the
  -- game." The elision this case retires -- with one dungeon owned there was
  -- nothing to ask, and Dungeon.enter took it without asking.
  --
  -- The two boards differ in exactly one thing: the answer to
  -- Prompt.ChooseDungeon. Read at gameplay level through LIFE TOTALS, because the
  -- two dungeons' topmost rooms differ there and nowhere a partial fix could
  -- reach another way -- Lost Mine's Cave Entrance scries, Tomb's Trapped Entry is
  -- "each player loses 1 life". Asserting Player.dungeons instead would prove
  -- nothing: CR 309.5b makes the supply outside the game a supply rather than a
  -- stock, so nothing is taken out of it either way.
  Spec.it s "CR 309.2a a player owning two dungeons chooses which one to enter" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    tomb <- S.printingOf s registry "Tomb of Annihilation"
    let (doorId, gs) = dungeonBoard island door [lostMine, tomb] 5
        lives g = (S.lifeOf S.alice g, S.lifeOf S.bob g)
        headDungeon = ventureOnce paying (ventureAbility door) doorId gs
        lastDungeon = ventureOnce payingLastDungeon (ventureAbility door) doorId gs
    -- THE BEHAVIOUR, first, so no fence below can absorb a mutation ahead of it:
    -- Tomb's Trapped Entry fired, which only entering Tomb reaches.
    Spec.assertEqWith s "CR 119.3: choosing Tomb cost each player 1 life" (lives lastDungeon) (Just 19, Just 19)
    -- The pair board, one answer away: Lost Mine's Cave Entrance touches no life
    -- total, so this is what makes the reading above the ANSWER's doing.
    Spec.assertEqWith s "and choosing Lost Mine cost nobody any" (lives headDungeon) (Just 20, Just 20)
    -- Identity, so a board that entered neither -- or entered both -- is visible.
    Spec.assertEqWith
      s
      "the command zone holds the dungeon that was chosen, each way"
      ( fmap (\oid -> fmap Face.name (Game.faceOf oid headDungeon)) (dungeonsOf S.alice headDungeon),
        fmap (\oid -> fmap Face.name (Game.faceOf oid lastDungeon)) (dungeonsOf S.alice lastDungeon)
      )
      ( [Just (CardName.MkCardName (Text.pack "Lost Mine of Phandelver"))],
        [Just (CardName.MkCardName (Text.pack "Tomb of Annihilation"))]
      )
    -- CR 309.3 still holds under a supply of two: one dungeon in the command zone.
    Spec.assertEqWith s "and only one of them, either way" (length (dungeonsOf S.alice headDungeon), length (dungeonsOf S.alice lastDungeon)) (1, 1)
  -- CR 118.12a down the ROOM-ABILITY path, which no card had reached before Tomb
  -- of Annihilation: Veils of Fear is "each player loses 2 life unless they
  -- discard a card", and Dungeon.roomPending mints that ability rather than
  -- reading it off a permanent's face. A gate that found no payers there would
  -- lose nobody any life and look exactly like a rule that did not apply.
  --
  -- Two ventures: Trapped Entry (each player loses 1), then its first arrow into
  -- Veils of Fear. bob holds no cards, so CR 118.3 never offers him the cost and
  -- he loses the 2 on every board; alice holds one, and the two boards differ in
  -- exactly one thing -- her answer to Prompt.ChooseToPay.
  Spec.it s "CR 118.12a Veils of Fear offers each player the discard and only the seats that declined lose the life" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    tomb <- S.printingOf s registry "Tomb of Annihilation"
    let (doorId, base) = dungeonBoard island door [lostMine, tomb] 10
        (gs, _) = S.handOne island base
        aliceDeclines = ventureTimes 2 payingLastDungeon (ventureAbility door) doorId gs
        alicePays = ventureTimes 2 paying2 (ventureAbility door) doorId gs
        paying2 :: Prompt.Prompt r -> r
        paying2 p = case p of
          Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
            | d == player && player == S.alice -> PaymentDecision.Pays
          _ -> payingLastDungeon p
        lives g = (S.lifeOf S.alice g, S.lifeOf S.bob g)
    -- THE BEHAVIOUR, first: alice's own answer, and only hers, decides whether
    -- she takes Veils of Fear's 2. bob is never offered and takes it either way,
    -- which is what tells the gate's plural read apart from a whole-table sweep.
    Spec.assertEqWith s "CR 119.3: alice declined, so both seats lost 1 then 2" (lives aliceDeclines) (Just 17, Just 17)
    Spec.assertEqWith s "and alice paying spared her the 2 and nobody else" (lives alicePays) (Just 19, Just 17)
    -- CR 701.9a: the payment really moved the card out of her hand.
    Spec.assertEqWith s "her hand: one card, discarded only on the paying board" (S.handSize S.alice gs, S.handSize S.alice aliceDeclines, S.handSize S.alice alicePays) (1, 1, 0)
    -- The board really is two rooms into Tomb, so the readings are Veils of
    -- Fear's and not some other room's.
    Spec.assertEqWith s "the marker reached Veils of Fear" (markerOf S.alice aliceDeclines, markerOf S.alice alicePays) (Just (RoomIndex.MkRoomIndex 1), Just (RoomIndex.MkRoomIndex 1))
  -- CR 309.2: the dungeons a player owns come from their DECK definition, which is
  -- the one path the boards above skip by writing Player.dungeons directly.
  Spec.it s "CR 309.2 a deck's dungeon cards are recorded on its player and minted into no zone" $ do
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    tomb <- S.printingOf s registry "Tomb of Annihilation"
    let deck = Deck.MkDeck {Deck.cards = Map.empty, Deck.commander = Nothing, Deck.vanguard = Nothing, Deck.dungeons = Set.fromList [lostMine, tomb], Deck.sideboard = Map.empty}
        after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice deck)
        ownedBy pid = fmap List.sort (traverse (\i -> Game.printingOf i after) (Set.toList (maybe Set.empty Player.dungeons (Map.lookup pid (GameState.players after)))))
    Spec.assertEqWith s "alice owns both" (ownedBy S.alice) (Just (List.sort [lostMine, tomb]))
    Spec.assertEqWith s "bob owns none" (fmap Player.dungeons (Map.lookup S.bob (GameState.players after))) (Just Set.empty)
    -- CR 309.2 / 400.11: it begins OUTSIDE the game, so setup mints no object for
    -- it at all.
    Spec.assertEqWith s "and no object was minted for it" (Map.size (GameState.objects after)) 0
  -- CR 701.49d: "venture into [quality] is a variant of venture into the dungeon.
  -- If a player is instructed to 'venture into [quality]' while they don't own a
  -- dungeon card in the command zone, they choose a dungeon card they own from
  -- outside the game with the indicated quality."
  --
  -- data/cards/undercity.json is the DUNGEON face alone. The printing's other
  -- face, The Initiative, is a reminder rather than a source: CR 726.2 makes the
  -- three initiative abilities inherent, with no source, so transcribing them onto
  -- a face would be wrong as well as unreachable. Pawl.Engine.Initiative mints
  -- them instead, and Pawl.InitiativeSpec drives them.
  --
  -- Synthetic Undercity Stair is still this case's producer: rule 726.2's is the
  -- only "venture into Undercity" any printing states, and it reaches CR 701.49d
  -- through the initiative rather than through an ability a card can bear.
  --
  -- ONE board, two abilities on it: Synthetic Undercity Stair's "venture into
  -- Undercity" and Secret Door's plain "venture into the dungeon". alice owns
  -- both dungeons either way, so the only difference between the two readings is
  -- which instruction was given.
  --
  -- Each answer is PINNED against the clause it discriminates. The quality board
  -- answers Prompt.ChooseDungeon with the FIRST offered, which is Lost Mine of
  -- Phandelver (Player.dungeons is ordered by interned id and dungeonBoard interns
  -- in the order given): an implementation ignoring CR 701.49d would offer both
  -- and enter Lost Mine. The plain board answers with the LAST, which is Undercity:
  -- an implementation ignoring Undercity's own "you can't enter this dungeon
  -- unless you 'venture into Undercity'" would offer both and enter Undercity.
  -- Neither prompt is raised at all once both clauses hold, since each venture is
  -- left one candidate.
  Spec.it s "CR 701.49d venturing into Undercity enters the dungeon carrying that quality, where a plain venture enters the other" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    stair <- S.printingOf s registry "Synthetic Undercity Stair"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    undercity <- S.printingOf s registry "Undercity"
    let (doorId, base) = dungeonBoard island door [lostMine, undercity] 12
        (stairId, gs) = S.addCreature stair S.alice base
        -- payingFirstDungeon's search arms with its dungeon answer replaced. The
        -- search arms are carried over deliberately: an implementation that
        -- entered Undercity here would then run Secret Entrance's search, so the
        -- hand-size assertion below catches it rather than only the name.
        plainAnswer :: Prompt.Prompt r -> r
        plainAnswer p = case p of
          Prompt.ChooseDungeon _ _ candidates -> NonEmpty.last candidates
          _ -> payingFirstDungeon p
        quality = ventureOnce payingFirstDungeon (ventureAbility stair) stairId gs
        plain = ventureOnce plainAnswer (ventureAbility door) doorId gs
    -- CR 309.4c: the room ability of the TOPMOST room of whichever dungeon was
    -- entered, which is the two dungeons' own difference on the board -- Secret
    -- Entrance searches a basic land into its owner's hand, and Cave Entrance only
    -- scries. Read before the marker and the name below, which are the cheaper
    -- proxies for the same fact.
    Spec.assertEqWith s "Secret Entrance put a basic land into alice's hand" (S.handSize S.alice quality) (S.handSize S.alice gs + 1)
    Spec.assertEqWith s "and the plain venture reached Cave Entrance, which touches no hand" (S.handSize S.alice plain) (S.handSize S.alice gs)
    Spec.assertEqWith s "the quality named Undercity, so Undercity is the dungeon in the command zone" (dungeonNamesOf S.alice quality) ["\"Undercity\""]
    Spec.assertEqWith s "and the plain venture entered the dungeon Undercity's own text leaves it" (dungeonNamesOf S.alice plain) ["\"Lost Mine of Phandelver\""]
    -- CR 309.3 / 309.4a: one dungeon each, marker on the topmost room -- the
    -- variant changes which card comes in, and nothing else about entering.
    Spec.assertEqWith s "one dungeon apiece" (length (dungeonsOf S.alice quality), length (dungeonsOf S.alice plain)) (1, 1)
    Spec.assertEqWith s "each with its marker on the topmost room" (markerOf S.alice quality, markerOf S.alice plain) (Just RoomIndex.topmost, Just RoomIndex.topmost)
  -- CR 701.24a: "to shuffle a library ... randomize the cards within it so that no
  -- player knows their order" -- and nothing else. Undercity's bottommost room,
  -- Throne of the Dead Three, is the pool's producer for that sentence standing
  -- alone: "Reveal the top ten cards of your library. Put a creature card from
  -- among them onto the battlefield with three +1/+1 counters on it. It gains
  -- hexproof until your next turn. Then shuffle."
  --
  -- Five ventures down the LAST arrow each time -- Secret Entrance, Lost Well,
  -- Stash, Catacombs, Throne of the Dead Three -- which is the one path to the
  -- bottommost room on which no room takes a card out of the library, so the
  -- reveal covers the whole of it.
  --
  -- The CONTROL is the fourth venture, Catacombs' "create a 4/1 black Skeleton
  -- creature token with menace", under the SAME interpreter: it touches no library,
  -- so the order is unchanged across it. The two boards differ in exactly one
  -- thing, which room the marker reached.
  --
  -- Then the negative CR 701.24a is about. The nine revealed cards that stay in
  -- the library keep the OBJECT IDS they had: CR 400.7 mints a fresh id for every
  -- completed zone change, so identical ids are what "nothing moved" means, and
  -- the fifth venture's zone-change log holds the one move the room does state and
  -- nothing into a library.
  Spec.it s "CR 701.24a Throne of the Dead Three's \"then shuffle\" randomises the library and moves nothing" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    evangel <- S.printingOf s registry "Cabal Evangel"
    stair <- S.printingOf s registry "Synthetic Undercity Stair"
    undercity <- S.printingOf s registry "Undercity"
    let (stairId, pikerId, evangelId, gs) = throneBoard island piker evangel stair undercity
        answering :: Prompt.Prompt r -> r
        answering = throneAnswer evangelId
        libraryOf = Game.zoneMembers Zone.Library S.alice
        atStash = ventureTimes 3 answering (ventureAbility stair) stairId gs
        atCatacombs = ventureOnce answering (ventureAbility stair) stairId atStash
        atThrone = ventureOnce answering (ventureAbility stair) stairId atCatacombs
        -- The fifth venture's own zone changes, the fourth's log subtracted off:
        -- S.zoneChangesOf accumulates over the whole turn and all five run in one.
        thisVenture = drop (length (S.zoneChangesOf atCatacombs)) (S.zoneChangesOf atThrone)
    -- CR 701.24a, at gameplay level: the library the room left in place comes back
    -- in a different order, and under this interpreter exactly the reversed one.
    -- An implementation that shuffled nothing would answer
    -- `List.delete evangelId (libraryOf atCatacombs)`, which is the same list the
    -- other way round.
    Spec.assertEqWith s "the room's \"then shuffle\" reversed what the reveal left in the library" (libraryOf atThrone) (reverse (List.delete evangelId (libraryOf atCatacombs)))
    -- The control: the venture before it, answered the same way, creates a token
    -- and shuffles nothing.
    Spec.assertEqWith s "where Catacombs, one room earlier, left the order alone" (libraryOf atCatacombs) (libraryOf atStash)
    -- CR 400.7: the same objects, so the shuffle was not nine library-to-library
    -- zone changes.
    Spec.assertEqWith s "and every card that stayed kept its object id" (Set.fromList (libraryOf atThrone)) (Set.delete evangelId (Set.fromList (libraryOf atCatacombs)))
    Spec.assertEqWith s "the only zone change the room made is the creature card's, to the battlefield" (fmap (\zc -> (ZoneChange.departed zc, ZoneChange.from zc, ZoneChange.to zc)) thisVenture) [(evangelId, Zone.Library, Zone.Battlefield)]
    -- The clauses the shuffle follows, so the room is transcribed whole. The
    -- chosen card is the one the answerer picked out of the offered pair, and the
    -- other is still in the library -- which is how "the room asked" is read.
    Spec.assertEqWith s "Cabal Evangel was put onto the battlefield" (length (inPlayNamed "\"Cabal Evangel\"" atThrone)) 1
    Spec.assertBool s (List.elem pikerId (libraryOf atThrone)) "and Goblin Piker, the creature card not chosen, stayed in the library"
    Spec.assertEqWith
      s
      "with three +1/+1 counters on it"
      (fmap (\oid -> Map.lookup CounterKind.PlusOnePlusOne (maybe Map.empty Object.counters (Game.lookupObject oid atThrone))) (inPlayNamed "\"Cabal Evangel\"" atThrone))
      [Just 3]
    Spec.assertBool s (all (\oid -> Projection.hasKeyword (Keyword.Hexproof Nothing) oid atThrone) (inPlayNamed "\"Cabal Evangel\"" atThrone)) "and hexproof until alice's next turn"
  -- CR 309.7: completing a dungeon is an EVENT, so "whenever you complete a
  -- dungeon" can be transcribed. Dungeon Crawler {B} Creature -- Zombie 2/1,
  -- "This creature enters tapped. Whenever you complete a dungeon, you may
  -- return this card from your graveyard to your hand." (name, cost, type line,
  -- P\/T and Oracle text checked against Scryfall, 2026-08-31.)
  --
  -- CR 113.6m is what lets it watch from the graveyard: the condition triggers
  -- perfectly well from the battlefield, and only the effect's own "from your
  -- graveyard" pins the ability there -- Pawl.ZoneTriggerSpec's Squee, Goblin
  -- Nabob is the same sentence on an upkeep trigger.
  --
  -- TWO boards differing in exactly one thing: FOUR ventures down the first arrow
  -- of Lost Mine of Phandelver, which the case above proves ends with the dungeon
  -- removed from the game, against THREE, which leaves the marker one room short.
  -- Read on the printed name (CR 400.7 gives the returned card a fresh id).
  Spec.it s "CR 309.7 completing a dungeon triggers Dungeon Crawler out of the graveyard" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    crawler <- S.printingOf s registry "Dungeon Crawler"
    let crawlerName = CardName.MkCardName (Text.pack "Dungeon Crawler")
        (doorId, base) = dungeonBoard island door [lostMine] 30
        (_, gs) = S.addGraveyardCard crawler S.alice base
        completed = ventureTimes 4 payingOptional (ventureAbility door) doorId gs
        short = ventureTimes 3 payingOptional (ventureAbility door) doorId gs
    -- The gameplay-level assertion, and FIRST, so no fence below can absorb a
    -- mutation ahead of it.
    Spec.assertBool s (Set.member crawlerName (namesIn Zone.Hand S.alice completed)) "Dungeon Crawler is in alice's hand"
    Spec.assertBool s (not (Set.member crawlerName (namesIn Zone.Graveyard S.alice completed))) "and no longer in her graveyard"
    -- The negative half of the same pair: one venture short, the dungeon is still
    -- in the command zone and nothing has been completed.
    Spec.assertBool s (Set.member crawlerName (namesIn Zone.Graveyard S.alice short)) "three ventures leave it in the graveyard"
    Spec.assertBool s (not (Set.member crawlerName (namesIn Zone.Hand S.alice short))) "and out of hand"
    Spec.assertEqWith s "because the dungeon is still in the command zone" (length (dungeonsOf S.alice short)) 1
    Spec.assertEqWith s "where four ventures removed it" (dungeonsOf S.alice completed) []
    Spec.assertBool s (elem (GameEvent.DungeonCompleted S.alice) (S.eventsOf completed)) "and the completion was recorded as an event"
  -- CR 309.7 read of a NAMED dungeon. Acererak the Archlich {2}{B} Legendary
  -- Creature -- Zombie Wizard 5\/5, "When Acererak enters, if you haven't
  -- completed Tomb of Annihilation, return Acererak to its owner's hand and
  -- venture into the dungeon." (name, cost, type line, P\/T and Oracle text
  -- checked against Scryfall, 2026-08-31.)
  --
  -- THREE boards. The first is the gate -- nothing completed, so the CR 603.4
  -- intervening "if" holds and Acererak bounces. The other two differ in EXACTLY
  -- one thing, the dungeon alice owns and therefore completes, and are what
  -- separate the named read from the tally: both leave
  -- Player.completedDungeons at 1, so an implementation reading the count
  -- answers them the same way and one of the two goes red.
  --
  -- Read on the printed name in each zone rather than on an object id, because
  -- CR 400.7 makes the returned card a new object.
  Spec.it s "CR 309.7 / 603.4 Acererak asks WHICH dungeon was completed, not how many" $ do
    island <- S.printingOf s registry "Island"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    tomb <- S.printingOf s registry "Tomb of Annihilation"
    acererak <- S.printingOf s registry "Acererak the Archlich"
    let acererakName = CardName.MkCardName (Text.pack "Acererak the Archlich")
        -- Acererak arrives with CR 603.6a's enters event, so the trigger is
        -- gathered, placed and then resolved off the same board every case uses.
        arrive gs =
          let (_, entered) = S.entersWithTrigger acererak S.alice gs
           in resolveAll payingOptional (S.runPure payingOptional entered Engine.settleForPriority)
        completing dungeon =
          let (doorId, gs) = dungeonBoard island door [dungeon] 30
           in ventureTimes 4 payingOptional (ventureAbility door) doorId gs
        untouched = snd (dungeonBoard island door [lostMine] 30)
        afterNothing = arrive untouched
        afterLostMine = arrive (completing lostMine)
        afterTomb = arrive (completing tomb)
    -- The gameplay-level assertions, and FIRST, so no fence below can absorb a
    -- mutation ahead of them.
    Spec.assertBool s (Set.member acererakName (namesIn Zone.Hand S.alice afterNothing)) "having completed nothing, Acererak returns to hand"
    Spec.assertBool s (Set.member acererakName (namesIn Zone.Hand S.alice afterLostMine)) "having completed Lost Mine of Phandelver, Acererak still returns to hand"
    Spec.assertBool s (Set.member acererakName (namesIn Zone.Battlefield S.alice afterTomb)) "having completed Tomb of Annihilation, Acererak stays on the battlefield"
    Spec.assertBool s (not (Set.member acererakName (namesIn Zone.Battlefield S.alice afterLostMine))) "and is off the battlefield on the Lost Mine board"
    -- Neither of the two discriminating boards is green because nothing happened:
    -- each really completed one dungeon, and the tally cannot tell them apart.
    Spec.assertEqWith
      s
      "both boards completed exactly one dungeon"
      (fmap (fmap Player.completedDungeons . Map.lookup S.alice . GameState.players) [afterLostMine, afterTomb])
      [Just 1, Just 1]
    Spec.assertEqWith
      s
      "and they differ only in its name"
      (fmap (fmap Player.completedDungeonNames . Map.lookup S.alice . GameState.players) [afterLostMine, afterTomb])
      [ Just (Set.singleton (CardName.MkCardName (Text.pack "Lost Mine of Phandelver"))),
        Just (Set.singleton (CardName.MkCardName (Text.pack "Tomb of Annihilation")))
      ]
