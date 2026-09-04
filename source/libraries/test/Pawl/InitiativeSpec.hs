{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Initiative (CR 726, the initiative), the GameState field it
-- writes, Pawl.Engine.Resolve's Effect.TakeTheInitiative arm and the CR 726.4
-- clause Pawl.Engine.Departure runs.
--
-- Gameplay-level throughout: the designation is taken by resolving Aarakocra
-- Sneak's entry trigger, and every venture CR 726.2 causes is read off the
-- dungeon card in the command zone and its venture marker (CR 309.4a) rather than
-- by calling Pawl.Engine.Dungeon directly.
module Pawl.InitiativeSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
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
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Zone as Zone

-- The dungeon cards a player owns in the command zone, by name (CR 309.3). What
-- "did this player venture?" is read through, since entering a dungeon is the
-- first thing a venture does for a player who owns none.
dungeonNamesOf :: PlayerId -> GameState.GameState -> [String]
dungeonNamesOf pid gs =
  List.sort
    ( Maybe.mapMaybe
        (\oid -> fmap (show . CardName.unwrap . Face.name) (Game.faceOf oid gs))
        (filter (\oid -> maybe False Dungeon.isDungeonFace (Game.faceOf oid gs)) (Game.zoneMembers Zone.Command pid gs))
    )

-- The room this player's venture marker is on (CR 309.4), or Nothing when they
-- own no dungeon in the command zone. What "did this player venture AGAIN?" is
-- read through.
markerOf :: PlayerId -> GameState.GameState -> Maybe RoomIndex.RoomIndex
markerOf pid gs = Dungeon.inDungeon pid gs >>= \oid -> Game.lookupObject oid gs >>= Object.ventureRoom

-- The players CR 726.1's event says took the initiative, in order -- so a case
-- can assert both who took it and that nothing else was recorded. CR 726.5's
-- re-take is exactly a second entry here with one designation on the board.
takings :: GameState.GameState -> [PlayerId]
takings gs = Maybe.mapMaybe took (S.eventsOf gs)
  where
    took event = case event of
      GameEvent.TookInitiative pid -> Just pid
      _ -> Nothing

-- Answers everything an entry trigger, a venture and Undercity's rooms raise:
-- take every target offered, and run Secret Entrance's search to completion so
-- the topmost room's printed effect is observable in a hand. Prompt.ChooseRoom
-- falls through to S.identityAnswer, which takes the FIRST arrow.
answering :: Prompt.Prompt r -> r
answering p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(n, cands) -> Set.fromList (take (Natural.toIntSaturating n) (Set.toList cands))) sets
  Prompt.ChooseSearchZones _ _ zones -> zones
  Prompt.Search _ _ candidates count -> take (Natural.toIntSaturating count) candidates
  _ -> S.identityAnswer p

-- Settle, then resolve the whole stack, settling between each resolution so an
-- ability that triggered on the resolution before it is placed and resolved too
-- -- which is the whole of CR 726.2's chain: the take, then the venture it
-- causes. Bounded, so a bug that kept the stack full fails the case rather than
-- hanging it.
resolveAll :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAll answer gs0 = go (20 :: Int) (S.runPure answer gs0 Engine.settleForPriority)
  where
    go n gs
      | n <= 0 = gs
      | null (GameState.stack gs) = gs
      | otherwise = go (n - 1) (S.runPure answer gs (Stack.resolveTop >> Engine.settleForPriority))

-- Every named player owns Undercity outside the game (CR 309.2), which is what
-- CR 701.49d's "venture into Undercity" needs to find. Interned ONCE, so every
-- owner names the same printing.
owningUndercity :: Printing.Printing -> [PlayerId] -> GameState.GameState -> GameState.GameState
owningUndercity undercity owners gs =
  let (dungeonId, g1) = Game.intern undercity gs
      own p = p {Player.dungeons = Set.singleton dungeonId}
   in g1 {GameState.players = List.foldl' (flip (Map.adjust own)) (GameState.players g1) owners}

-- Three seats, every one of them owning Undercity, alice's library stocked with
-- Islands for Secret Entrance to search, and a Goblin Piker apiece for bob and
-- carol -- carol being the control seat no case should ever touch.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId, ObjectId, GameState.GameState)
board island piker undercity =
  let stocked = List.foldl' (\g _ -> snd (S.addLibraryCard island S.alice g)) (Setup.emptyGame S.threePlayers) [1 .. (3 :: Int)]
      (bobs, g1) = S.addCreature piker S.bob stocked
      (carols, g2) = S.addCreature piker S.carol g1
   in (bobs, carols, owningUndercity undercity [S.alice, S.bob, S.carol] g2)

-- CR 726.2: one creature's combat damage to the player who has the initiative.
-- Driven by the damage EVENT rather than by a full combat, Pawl.EventTriggerSpec's
-- monarch group's posture one rule over.
combatDamageTo :: PlayerId -> ObjectId -> GameEvent.GameEvent
combatDamageTo holder damager =
  GameEvent.DamageDealt (DamageEvent.MkDamageEvent damager (Recipient.ToPlayer holder) 2 False False False 0 Nothing DamageKind.Combat)

-- A REAL combat, where `combatDamageTo` above is a hand-written event: alice
-- attacks bob with `mine`, bob blocks with `theirs`, carol holds `hers` and never
-- joins. Positioned at the declare attackers step with bob named the sole
-- defending player, which CR 703.4h's turn-based action would otherwise have
-- filled in; every seat owns Undercity, as `board`'s seats do.
combatBoard :: Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> [Printing.Printing] -> ([ObjectId], [ObjectId], [ObjectId], GameState.GameState)
combatBoard undercity mine theirs hers =
  let (base, ours, yours, theirsToo) = S.threePlayerCombat mine theirs hers
      staged =
        base
          { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            GameState.combat = (GameState.combat base) {Combat.Type.defenders = [S.bob]}
          }
   in (ours, yours, theirsToo, owningUndercity undercity [S.alice, S.bob, S.carol] staged)

-- `answering` plus a combat aimed at bob: attack with everything, block with
-- everything, and divide a trampler's damage the way CR 510.1c and CR 702.19b
-- together require -- each blocker's own threshold first, the excess through to
-- bob. The thresholds the prompt offers ARE CR 510.1c's lethal amounts, so
-- nothing here restates a creature's toughness. Written out rather than left to
-- S.identityAnswer, which never names a player recipient and would put the whole
-- assignment on the blocker.
tramplingAtBob :: Prompt.Prompt r -> r
tramplingAtBob p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let toBlockers = Map.delete (Recipient.ToPlayer S.bob) thresholds
     in Map.insert (Recipient.ToPlayer S.bob) (n - sum (Map.elems toBlockers)) toBlockers
  Prompt.DeclareAttackers {} -> S.attackTo S.bob p
  Prompt.DeclareBlockers {} -> S.attackTo S.bob p
  Prompt.ChooseDefender {} -> S.attackTo S.bob p
  Prompt.ChooseAttackTarget {} -> S.attackTo S.bob p
  _ -> answering p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Initiative" $ do
  -- CR 726.1: "there is no initiative in a game until an effect instructs a
  -- player to take the initiative", and CR 726.2's third ability: "whenever a
  -- player takes the initiative, that player ventures into Undercity".
  Spec.it s "CR 726.1/726.2 Aarakocra Sneak's entry gives its controller the initiative, and taking it ventures her into Undercity" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    sneak <- S.printingOf s registry "Aarakocra Sneak"
    let (_, _, base) = board island piker undercity
        (_, gs) = S.entersWithTrigger sneak S.alice base
        after = resolveAll answering gs
    Spec.assertEqWith s "no initiative before the Sneak's entry trigger resolved" (GameState.initiative gs) Nothing
    -- CR 309.4c: Undercity's topmost room, Secret Entrance, searched a basic land
    -- into alice's hand. The room ability is what the venture is READ through: a
    -- take that ventured nobody leaves the hand where it was.
    Spec.assertEqWith s "Secret Entrance put a basic land into alice's hand" (S.handSize S.alice after) (S.handSize S.alice gs + 1)
    Spec.assertEqWith s "CR 726.1 alice has the initiative" (GameState.initiative after) (Just S.alice)
    Spec.assertEqWith s "CR 701.49d she entered Undercity and no other dungeon" (dungeonNamesOf S.alice after) ["\"Undercity\""]
    Spec.assertEqWith s "CR 309.4a with her marker on the topmost room" (markerOf S.alice after) (Just RoomIndex.topmost)
    Spec.assertEqWith s "CR 726.1 exactly one take was recorded, naming her" (takings after) [S.alice]
    Spec.assertEqWith s "and neither opponent ventured" (dungeonNamesOf S.bob after, dungeonNamesOf S.carol after) ([], [])
    Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []

  -- CR 726.2, first ability: "at the beginning of the upkeep of the player who
  -- has the initiative, that player ventures into Undercity."
  --
  -- ONE board, two readings: the upkeep of alice's turn against the upkeep of
  -- bob's. alice holds the initiative and is already in Undercity on both, so the
  -- only difference is whose upkeep began -- an implementation reading "the
  -- beginning of every upkeep" would advance her marker on both.
  Spec.it s "CR 726.2 the holder's own upkeep ventures her again, and an opponent's upkeep does not" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    sneak <- S.printingOf s registry "Aarakocra Sneak"
    let (_, _, base) = board island piker undercity
        (_, entering) = S.entersWithTrigger sneak S.alice base
        inUndercity = resolveAll answering entering
        upkeepOf pid = resolveAll answering (S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Beginning BeginningStep.Upkeep) pid)] inUndercity)
        hers = upkeepOf S.alice
        his = upkeepOf S.bob
    Spec.assertEqWith s "she is in Undercity on the topmost room before either upkeep" (markerOf S.alice inUndercity) (Just RoomIndex.topmost)
    -- CR 309.4c/701.49b: the marker followed an arrow out of Secret Entrance, so
    -- the upkeep really ventured. S.identityAnswer takes the first arrow, which is
    -- Forge, the room after the topmost.
    Spec.assertEqWith s "her own upkeep advanced the marker one room" (markerOf S.alice hers) (Just (RoomIndex.MkRoomIndex 1))
    Spec.assertEqWith s "where bob's upkeep left it on the topmost room" (markerOf S.alice his) (Just RoomIndex.topmost)
    Spec.assertEqWith s "and neither upkeep moved the designation" (GameState.initiative hers, GameState.initiative his) (Just S.alice, Just S.alice)
    Spec.assertEqWith s "no second dungeon was entered on either" (dungeonNamesOf S.alice hers, dungeonNamesOf S.alice his) (["\"Undercity\""], ["\"Undercity\""])
    Spec.assertEqWith s "and bob, whose upkeep it was, ventured into nothing" (dungeonNamesOf S.bob his) []

  -- CR 726.2, second ability: "whenever one or more creatures a player controls
  -- deal combat damage to the player who has the initiative, the controller of
  -- those creatures takes the initiative" -- which CR 726.2's third ability then
  -- ventures.
  --
  -- Three seats, so "that player" and "an opponent" cannot collapse: carol's Piker
  -- deals no damage and carol must be untouched throughout.
  Spec.it s "CR 726.2 an opponent's creature connecting with the holder takes the initiative and ventures him" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    sneak <- S.printingOf s registry "Aarakocra Sneak"
    let (bobs, _, base) = board island piker undercity
        (_, entering) = S.entersWithTrigger sneak S.alice base
        hers = resolveAll answering entering
        after = resolveAll answering (S.withEvents [combatDamageTo S.alice bobs] hers)
    Spec.assertEqWith s "alice held the initiative before the damage" (GameState.initiative hers) (Just S.alice)
    -- CR 701.49a: bob owned no dungeon in the command zone, so his venture ENTERED
    -- Undercity -- the gameplay-level reading of "that player ventures".
    Spec.assertEqWith s "CR 701.49d bob entered Undercity" (dungeonNamesOf S.bob after) ["\"Undercity\""]
    Spec.assertEqWith s "CR 726.3 bob has the initiative, and alice no longer does" (GameState.initiative after) (Just S.bob)
    -- S.withEvents rewrites the log, so alice's own take is not in it: what this
    -- reads is the takes since the damage, which is bob's and nothing else.
    Spec.assertEqWith s "CR 726.2 one take was recorded since the damage, naming bob" (takings after) [S.bob]
    Spec.assertEqWith s "alice's own marker did not move" (markerOf S.alice after) (Just RoomIndex.topmost)
    Spec.assertEqWith s "and carol, whose Piker dealt nothing, ventured into no dungeon" (dungeonNamesOf S.carol after) []
    Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []

  -- CR 726.2's "one or more creatures a player controls": TWO of bob's creatures
  -- connecting in one combat damage step is ONE trigger, so bob takes the
  -- initiative once and ventures once.
  --
  -- The paired control is the case above, whose board differs in exactly one
  -- thing: how many of bob's creatures dealt the damage. A per-damager reading
  -- gives bob two takes, and CR 726.5 makes the second one venture him again --
  -- so the marker, not just the count, tells the two apart.
  Spec.it s "CR 726.2 two of one player's creatures connecting is one take and one venture" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    sneak <- S.printingOf s registry "Aarakocra Sneak"
    let (bobs, _, base) = board island piker undercity
        (second, withSecond) = S.addCreature piker S.bob base
        (_, entering) = S.entersWithTrigger sneak S.alice withSecond
        hers = resolveAll answering entering
        after = resolveAll answering (S.withEvents [combatDamageTo S.alice bobs, combatDamageTo S.alice second] hers)
    Spec.assertEqWith s "bob's marker is on the topmost room, so he ventured exactly once" (markerOf S.bob after) (Just RoomIndex.topmost)
    -- ONE, where a per-damager reading records two: S.withEvents rewrote the log
    -- before the damage, so every entry here is the damage's doing.
    Spec.assertEqWith s "CR 726.2 exactly one take was recorded for bob's two creatures" (takings after) [S.bob]
    Spec.assertEqWith s "and he holds the initiative" (GameState.initiative after) (Just S.bob)

  -- CR 726.2 makes the hand-off "controlled by the player who had the initiative
  -- at the time the abilities triggered", so when the damage that triggers it
  -- also kills the holder, CR 800.4d keeps it off the stack and CR 726.4 alone
  -- moves the initiative -- to the ACTIVE player, not the damager's controller.
  -- The Court of Grace ruling says the same of the monarch's twin; #3148 claimed
  -- the opposite.
  --
  -- Three seats with the damager's controller NOT the active player is the only
  -- board where the two readings differ, and no real combat reaches it (CR
  -- 508.1a: only the active player's creatures attack; CR 506.4: a controller
  -- change removes a creature from combat), so the damage is `combatDamageTo`'s
  -- hand-written event and bob's life total is written down by hand to match it:
  -- 1 going in, 2 dealt.
  Spec.it s "CR 726.4/800.4d lethal combat damage to the holder hands the initiative to the active player, not the damager's controller" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    let (_, carols, base) = board island piker undercity
        held = S.withInitiative S.bob base
        dying = held {GameState.players = Map.adjust (\p -> p {Player.life = -1}) S.bob (GameState.players held)}
        after = resolveAll answering (S.withEvents [combatDamageTo S.bob carols] dying)
    Spec.assertEqWith s "bob held the initiative going in" (GameState.initiative held) (Just S.bob)
    Spec.assertEqWith s "alice is the active player, and carol's Piker dealt the damage" (GameState.activePlayer held, Projection.controllerOf carols held) (S.alice, Just S.carol)
    -- The rule: CR 726.4's hand-off is the only take.
    Spec.assertEqWith s "CR 726.4 alice, the active player, has the initiative" (GameState.initiative after) (Just S.alice)
    Spec.assertEqWith s "CR 800.4d exactly one take was recorded, the hand-off's, naming her" (takings after) [S.alice]
    -- CR 726.4 says alice TAKES the initiative, so CR 726.2's third ability
    -- ventures her; the departed bob and the untouched carol venture nowhere.
    Spec.assertEqWith s "CR 726.2/701.49d and, having taken it, she entered Undercity" (dungeonNamesOf S.alice after) ["\"Undercity\""]
    Spec.assertEqWith s "carol, whose Piker dealt the damage, ventured into nothing" (dungeonNamesOf S.carol after) []
    Spec.assertEqWith s "CR 704.5a bob lost the game" (Game.stillPlaying after) [S.alice, S.carol]
    Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []

  -- CR 603.10, first sentence: the trigger condition is checked against the
  -- objects that exist IMMEDIATELY AFTER the damage, and the CR 704.5g
  -- destruction that kills a trampler its blocker traded with is a later event.
  -- Engine.performSettle runs that state-based action before the trigger scan, so
  -- a live read of the damager found an id Event.placeObject had already retired,
  -- and the hand-off never happened; see #3132.
  --
  -- A REAL combat, not a hand-written damage event, because the fixture that
  -- rewrites the log is exactly the fixture that cannot produce this board: alice
  -- attacks bob with War Mammoth (3/3 trample) and bob blocks with Boggart Brute
  -- (3/2), so the Mammoth assigns the Brute its lethal 2 and tramples 1 through,
  -- and the Brute's 3 kills the Mammoth in the same step. Both die together.
  Spec.it s "CR 726.2 a trampler that trades with its blocker still hands the initiative over" $ do
    undercity <- S.printingOf s registry "Undercity"
    mammoth <- S.printingOf s registry "War Mammoth"
    brute <- S.printingOf s registry "Boggart Brute"
    piker <- S.printingOf s registry "Goblin Piker"
    let (mine, theirs, _, base) = combatBoard undercity [mammoth] [brute] [piker]
        held = S.withInitiative S.bob base
        after = resolveAll tramplingAtBob (S.fightWith tramplingAtBob held)
    Spec.assertEqWith s "bob held the initiative going in" (GameState.initiative held) (Just S.bob)
    -- The board this case is about: the damager is GONE by the time the scan
    -- runs. Neither this nor the life total can tell the two readings apart --
    -- the Mammoth dies and bob loses 1 under both -- so they pin the board rather
    -- than prove the rule.
    Spec.assertEqWith s "CR 704.5g the Mammoth traded with the Brute, so both are off the battlefield" (fmap (\oid -> S.onBattlefield oid after) (mine <> theirs)) [False, False]
    Spec.assertEqWith s "CR 702.19b and 1 point trampled through to bob" (S.lifeOf S.bob after) (Just 19)
    -- The rule: alice's dead Mammoth still took the initiative for her, and CR
    -- 726.2's third ability then ventured her.
    Spec.assertEqWith s "CR 726.2/726.3 alice has the initiative" (GameState.initiative after) (Just S.alice)
    Spec.assertEqWith s "CR 701.49d and, having taken it, she entered Undercity" (dungeonNamesOf S.alice after) ["\"Undercity\""]
    Spec.assertEqWith s "CR 726.2 exactly one take was recorded, naming her" (takings after) [S.alice]
    Spec.assertEqWith s "and carol, who never joined the combat, ventured into nothing" (dungeonNamesOf S.carol after) []
    Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []

  -- CR 726.2's grouping is by CONTROLLER, so two players' creatures connecting in
  -- ONE damage step are TWO triggers, both controlled by the holder (CR 726.2's
  -- "the player who had the initiative at the time the abilities triggered"). CR
  -- 603.3b lets her order her own two, and the stack resolves them last-first, so
  -- the take recorded LAST is the one that stands.
  --
  -- The case above the previous one -- two of ONE player's creatures -- is the
  -- paired control: same shape, one controller, one trigger.
  Spec.it s "CR 726.2 two players' creatures connecting in one step are two takes, and the last one stands" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    sneak <- S.printingOf s registry "Aarakocra Sneak"
    let (bobs, carols, base) = board island piker undercity
        (_, entering) = S.entersWithTrigger sneak S.alice base
        hers = resolveAll answering entering
        after = resolveAll answering (S.withEvents [combatDamageTo S.alice bobs, combatDamageTo S.alice carols] hers)
    -- The rule, read at gameplay level: BOTH opponents ventured, so both triggers
    -- resolved. A grouping that collapsed the two controllers into one leaves
    -- whichever of them lost the race in no dungeon at all.
    Spec.assertEqWith s "CR 701.49d bob and carol each entered Undercity" (dungeonNamesOf S.bob after, dungeonNamesOf S.carol after) (["\"Undercity\""], ["\"Undercity\""])
    Spec.assertEqWith s "CR 726.2 two takes were recorded, one per controller" (List.sort (takings after)) [S.bob, S.carol]
    -- CR 726.3: one designation, and it is the LAST take's -- the two abilities
    -- resolve one after the other and the second hand-off moves it again.
    Spec.assertEqWith s "CR 726.3 the designation is the last take's" (GameState.initiative after) (Maybe.listToMaybe (reverse (takings after)))
    Spec.assertEqWith s "alice's own marker did not move" (markerOf S.alice after) (Just RoomIndex.topmost)

  -- CR 510.4 / CR 726.2: a first-strike damage step and the regular one are TWO
  -- batches, and each ends in a settle -- so the hand-off from the first has
  -- already resolved when the second is scanned, and the second is asked about the
  -- NEW holder. Two batches, two takes.
  --
  -- The paired control is the same two damage events delivered in ONE batch, where
  -- carol's creature hits a bob who does not yet have the initiative and triggers
  -- nothing. The boards differ in nothing but the batching.
  Spec.it s "CR 726.2 two damage batches are two takes, where the same two hits in one batch are one" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    sneak <- S.printingOf s registry "Aarakocra Sneak"
    let (bobs, carols, base) = board island piker undercity
        (_, entering) = S.entersWithTrigger sneak S.alice base
        hers = resolveAll answering entering
        firstStrike = resolveAll answering (S.withEvents [combatDamageTo S.alice bobs] hers)
        regular = resolveAll answering (S.withEvents [combatDamageTo S.bob carols] firstStrike)
        fused = resolveAll answering (S.withEvents [combatDamageTo S.alice bobs, combatDamageTo S.bob carols] hers)
    Spec.assertEqWith s "the first batch handed it to bob" (GameState.initiative firstStrike) (Just S.bob)
    Spec.assertEqWith s "CR 726.2 the second batch, asked about the new holder, handed it to carol" (GameState.initiative regular) (Just S.carol)
    Spec.assertEqWith s "CR 701.49d so carol ventured too" (dungeonNamesOf S.carol regular) ["\"Undercity\""]
    Spec.assertEqWith s "and the second batch recorded her take" (takings regular) [S.carol]
    -- The control: in ONE batch carol's hit lands on a bob who is still not the
    -- holder, so only bob's take is recorded and carol ventures into nothing.
    Spec.assertEqWith s "CR 726.2 fused into one batch it is bob's take alone" (takings fused) [S.bob]
    Spec.assertEqWith s "and carol, who hit a bob without the initiative, ventured into nothing" (dungeonNamesOf S.carol fused) []

  -- CR 726.5: "if the player who currently has the initiative is instructed to
  -- take the initiative, this causes the last triggered ability in 726.2 to
  -- trigger but does not create a second initiative designation."
  Spec.it s "CR 726.5 the holder taking it again ventures her again and creates no second designation" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    sneak <- S.printingOf s registry "Aarakocra Sneak"
    let (_, _, base) = board island piker undercity
        (_, entering) = S.entersWithTrigger sneak S.alice base
        hers = resolveAll answering entering
        (_, again) = S.entersWithTrigger sneak S.alice hers
        after = resolveAll answering again
    Spec.assertEqWith s "she was in Undercity on the topmost room before the second Sneak" (markerOf S.alice hers) (Just RoomIndex.topmost)
    -- CR 726.5's first half, read at gameplay level: the re-take ventured her, so
    -- the marker followed an arrow. An implementation reading Monarch.crown's
    -- "already the monarch does not become the monarch" here would leave it.
    Spec.assertEqWith s "the re-take ventured her one room on" (markerOf S.alice after) (Just (RoomIndex.MkRoomIndex 1))
    -- CR 726.5's second half: ONE designation. A list rather than a Bool, because
    -- "is there a second dungeon?" is the question a second designation's venture
    -- would answer differently.
    Spec.assertEqWith s "and one dungeon card, so no second designation ventured separately" (dungeonNamesOf S.alice after) ["\"Undercity\""]
    Spec.assertEqWith s "she still has the initiative" (GameState.initiative after) (Just S.alice)
    -- S.entersWithTrigger rewrites the log, so this is the SECOND Sneak's take
    -- alone -- the first is proved by `hers` above having put her in Undercity.
    Spec.assertEqWith s "and the re-take was recorded, naming her" (takings after) [S.alice]

  -- CR 726.4: "if the player who has the initiative leaves the game, the active
  -- player takes the initiative at the same time that player leaves the game."
  Spec.it s "CR 726.4 the holder departs on someone else's turn: the active player takes it" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    let (_, _, base) = board island piker undercity
        held = S.withInitiative S.bob base
        gone = S.departs Departure.Type.Conceded S.bob held
        settled = resolveAll answering gone
    Spec.assertEqWith s "alice is the active player on this board" (GameState.activePlayer held) S.alice
    -- CR 726.2's third ability fires off the reassignment's own take, which is
    -- what makes the hand-off a TAKE rather than a field write: alice ventures.
    Spec.assertEqWith s "CR 701.49d so alice, having taken it, entered Undercity" (dungeonNamesOf S.alice settled) ["\"Undercity\""]
    Spec.assertEqWith s "CR 726.4 alice has the initiative" (GameState.initiative gone) (Just S.alice)
    Spec.assertEqWith s "and exactly one take was recorded, naming her" (takings gone) [S.alice]

  -- CR 726.4's second sentence: "if the active player is leaving the game or if
  -- there is no active player, the next player in turn order takes the
  -- initiative."
  Spec.it s "CR 726.4 the holder departs on their own turn: the next seat in turn order takes it" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    undercity <- S.printingOf s registry "Undercity"
    let (_, _, base) = board island piker undercity
        held = S.withInitiative S.alice base
        gone = S.departs Departure.Type.Conceded S.alice held
    Spec.assertEqWith s "bob, the seat after alice's" (GameState.initiative gone) (Just S.bob)
    Spec.assertEqWith s "and the take names him, so the walk's branch records too" (takings gone) [S.bob]
