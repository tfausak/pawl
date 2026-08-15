{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Rad (CR 728, "Rad Counters"), PlayerCounterKind.Rad on
-- Player.counters (CR 122.1i), Pawl.Engine.Quantity's PlayerCounters arm, the
-- MillTally that Pawl.Engine.Resolve's Mill arm binds, and
-- Effect.RemovePlayerCounters.
--
-- Gameplay-level. The Master, Transcendent is the producer -- {1}{B}{G}{U}
-- Legendary Artifact Creature, "When The Master enters, target player gets two
-- rad counters" -- cast off four lands, so the counters that rule 728.1's ability
-- then eats were put there by a card rather than written into the state. It is
-- also the pool's first card to give a TARGET player counters, which is why it
-- aims at bob: a recipient plumbed to the resolving controller would put them on
-- alice instead and pass a test that named neither.
--
-- The Master's OTHER ability -- "{T}: Put target creature card in a graveyard
-- that was milled this turn onto the battlefield under your control. It's a
-- green Mutant with base power and toughness 3/3." -- has the last group, since
-- rule 728.1's own mill is what stocks the graveyard it reads (CR 701.17a,
-- Filter.MilledThisTurn).
--
-- The groups after the first arrange the counters directly (S.addPlayerCounter),
-- because what they vary is the LIBRARY -- how many nonland cards the mill turned
-- up -- and casting the producer again would prove nothing new about that.
module Pawl.RadSpec where

import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Rad counters" $ do
  producerSpec s registry
  abilitySpec s registry
  reanimationSpec s registry

-- CR 122.1i through the card that hands the counters out.
producerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
producerSpec s registry = Spec.describe s "The Master, Transcendent" $ do
  Spec.it s "CR 122.1i its enters trigger gives the TARGETED player two rad counters" $ do
    master <- S.printingOf s registry "The Master, Transcendent"
    lands <- fourColorLands s registry
    let (gs, spellId) = S.handOne master (boardOf lands)
        cast = S.runPure (targeting S.bob) gs (S.cast S.alice spellId)
        after = S.runPure (targeting S.bob) cast Engine.priorityLoop
    Spec.assertEqWith s "bob has two rad counters" (radOf S.bob after) 2
    -- The falsifier for a recipient plumbed to the resolving controller (#120):
    -- alice cast it, and alice gets nothing.
    Spec.assertEqWith s "and alice, who cast it, has none" (radOf S.alice after) 0
  -- The whole rule, end to end: a card puts the counters on, and the player they
  -- landed on pays rule 728.1's price on their own next precombat main phase.
  Spec.it s "CR 728.1 those counters then mill bob, cost him life and burn themselves down" $ do
    master <- S.printingOf s registry "The Master, Transcendent"
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    lands <- fourColorLands s registry
    let (gs, spellId) = S.handOne master (boardOf lands)
        cast = S.runPure (targeting S.bob) gs (S.cast S.alice spellId)
        entered = S.runPure (targeting S.bob) cast Engine.priorityLoop
        -- Three cards, of which the mill reaches the top TWO, both nonland; the
        -- Mountain under them proves the mill counts rad counters and not the
        -- library, and keeps the milled count (2) apart from the number of
        -- cards there were (3).
        stocked = libraryTopped [bolt, bolt, mountain] S.bob entered
        after = S.runPure S.identityAnswer (precombatMainOf S.bob stocked) (Engine.runStep >> Engine.priorityLoop)
    Spec.assertEqWith s "two cards milled" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertEqWith s "and one left in the library" (length (Game.zoneMembers Zone.Library S.bob after)) 1
    Spec.assertEqWith s "bob lost 1 life per nonland card milled" (S.lifeOf S.bob after) (Just 18)
    -- Both counters spent on the two nonland cards: rule 728.1's ability eats
    -- what it fires on, so a second precombat main phase would find nothing.
    Spec.assertEqWith s "and no rad counter is left" (radOf S.bob after) 0

-- CR 728.1's ability on its own, over the boards that vary what the mill finds.
abilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
abilitySpec s registry = Spec.describe s "CR 728.1's inherent ability" $ do
  Spec.it s "mills as many cards as the player has rad counters, and pays for each nonland one" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let base = S.addPlayerCounter PlayerCounterKind.Rad 3 S.alice (Setup.emptyGame S.bothPlayers)
        -- Three counters, so three cards: two nonland and one land, with a
        -- fourth card the mill must not reach. Every number in the answer is
        -- different -- 3 milled, 2 paid for, 1 counter left, 1 card spared --
        -- so no two of them can be swapped without the test noticing.
        stocked = libraryTopped [bolt, mountain, bolt, bolt] S.alice base
        after = S.runPure S.identityAnswer (precombatMainOf S.alice stocked) (Engine.runStep >> Engine.priorityLoop)
    Spec.assertEqWith s "three cards milled" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 3
    Spec.assertEqWith s "one card spared" (length (Game.zoneMembers Zone.Library S.alice after)) 1
    Spec.assertEqWith s "2 life lost, one per nonland card" (S.lifeOf S.alice after) (Just 18)
    Spec.assertEqWith s "and 3 - 2 = 1 rad counter left" (radOf S.alice after) 1
  -- THE FALSIFIER for a tally that counts every milled card: this board mills
  -- three and pays for none of them.
  Spec.it s "CR 728.1 a mill that turns up only lands costs no life and no counters" $ do
    mountain <- S.printingOf s registry "Mountain"
    let base = S.addPlayerCounter PlayerCounterKind.Rad 3 S.alice (Setup.emptyGame S.bothPlayers)
        stocked = libraryTopped [mountain, mountain, mountain, mountain] S.alice base
        after = S.runPure S.identityAnswer (precombatMainOf S.alice stocked) (Engine.runStep >> Engine.priorityLoop)
    Spec.assertEqWith s "three cards milled all the same" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 3
    Spec.assertEqWith s "no life lost" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and all three rad counters still there" (radOf S.alice after) 3
  -- CR 603.4's intervening "if". A player with none does not trigger at all.
  Spec.it s "CR 728.1 a player with no rad counters mills nothing" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    let stocked = libraryTopped [bolt, bolt] S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer (precombatMainOf S.alice stocked) (Engine.runStep >> Engine.priorityLoop)
    Spec.assertEqWith s "the library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 2
    Spec.assertEqWith s "and no life is lost" (S.lifeOf S.alice after) (Just 20)
    -- CR 603.4 is checked as the event OCCURS, so the ability is never PLACED at
    -- all -- a difference other players can see, and one that survives the CR
    -- 608.2a recheck doing the same job on resolution. Read at the settle point,
    -- because Engine.runStep's own priority round would have resolved it away.
    Spec.assertEqWith s "and no ability went on the stack at all" (length (GameState.stack (settledAtPrecombatMain S.alice stocked))) 0
  -- The same reading in the positive direction, so the case above cannot pass by
  -- the ability never existing.
  Spec.it s "CR 603.4 a player WITH rad counters does put it on the stack" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.addPlayerCounter PlayerCounterKind.Rad 1 S.alice (Setup.emptyGame S.bothPlayers)
        stocked = libraryTopped [bolt, bolt] S.alice base
    Spec.assertEqWith s "one ability on the stack" (length (GameState.stack (settledAtPrecombatMain S.alice stocked))) 1
  -- CR 505.1a: only the active player has a precombat main phase, so rule
  -- 728.1's "that player" is never an opponent of the turn's.
  Spec.it s "CR 728.1 an opponent's rad counters wait for their own turn" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.addPlayerCounter PlayerCounterKind.Rad 3 S.bob (Setup.emptyGame S.bothPlayers)
        stocked = libraryTopped [bolt, bolt, bolt, bolt] S.bob base
        -- ALICE's precombat main phase.
        after = S.runPure S.identityAnswer (precombatMainOf S.alice stocked) (Engine.runStep >> Engine.priorityLoop)
    Spec.assertEqWith s "bob's library is untouched" (length (Game.zoneMembers Zone.Library S.bob after)) 4
    Spec.assertEqWith s "bob loses no life" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and keeps all three counters" (radOf S.bob after) 3

-- CR 701.17a through the card that reads a mill back: The Master's "{T}: Put
-- target creature card in a graveyard that was milled this turn onto the
-- battlefield under your control."
--
-- The two boards are a PAIR differing in one thing. Both give alice one rad
-- counter, so rule 728.1 mills exactly one card on her precombat main phase;
-- both leave a Goblin Piker and a Berserkers of Blood Ridge in her graveyard by
-- the time she activates. What differs is WHICH of them the mill put there.
--
-- Berserkers is in the graveyard from the start in both, and is what makes the
-- filter observable: S.identityAnswer takes the LEAST recipient, and a card
-- placed before the mill has the lower object id, so an engine that let The
-- Master name any creature card would reanimate the Berserkers instead.
reanimationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
reanimationSpec s registry = Spec.describe s "The Master, Transcendent's reanimation" $ do
  Spec.it s "CR 701.17a it takes the card the turn's mill binned, as a 3/3 green Mutant" $ do
    master <- S.printingOf s registry "The Master, Transcendent"
    piker <- S.printingOf s registry "Goblin Piker"
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    mountain <- S.printingOf s registry "Mountain"
    let (masterId, bystanderId, milledBoard) = radBoard master berserkers
        -- The Piker on top, so rule 728.1's one-card mill is what puts it in the
        -- graveyard. The Mountains under it are the library CR 104.3c needs.
        stocked = libraryTopped [piker, mountain, mountain] S.alice milledBoard
        after = activated masterId master (afterTheMill stocked)
        reanimated = filter (/= masterId) (Game.zoneMembers Zone.Battlefield S.alice after)
    Spec.assertEqWith s "the ability was offered" (activationsOffered masterId (afterTheMill stocked)) 1
    Spec.assertEqWith s "one card came back" (length reanimated) 1
    -- Every number here differs from the Piker's printed 2/1 red Goblin Warrior,
    -- so no reading of the card leaves two of them the same.
    Spec.assertEqWith s "as a 3/3" (fmap (\o -> (Projection.powerOf o after, Projection.toughnessOf o after)) reanimated) [(Just 3, Just 3)]
    Spec.assertEqWith s "green and nothing else" (fmap (\o -> Projection.colorsOf o after) reanimated) [Set.singleton Color.Green]
    Spec.assertEqWith s "a Mutant and nothing else" (fmap (\o -> Projection.subtypesOf o after) reanimated) [Set.singleton Subtype.Mutant]
    Spec.assertEqWith s "under alice's control" (fmap (\o -> Projection.controllerOf o after) reanimated) [Just S.alice]
    -- The falsifier for a filter that admitted any creature card: the Berserkers
    -- was in the graveyard before the mill and is still the only thing in it.
    -- Its IDENTITY is the assertion, since a bundle that overwrites colour, type
    -- and size leaves the two cards indistinguishable by characteristic.
    Spec.assertEqWith s "and the card that was NOT milled stayed put" (Game.zoneMembers Zone.Graveyard S.alice after) [bystanderId]
  -- The same board with the mill turned onto a land, so the Piker reaches the
  -- graveyard by being PUT there rather than by being milled. Nothing else moves.
  Spec.it s "CR 701.17a a creature card that reached the graveyard another way is no target" $ do
    master <- S.printingOf s registry "The Master, Transcendent"
    piker <- S.printingOf s registry "Goblin Piker"
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    mountain <- S.printingOf s registry "Mountain"
    let (masterId, _, radded) = radBoard master berserkers
        (pikerId, placed) = S.addGraveyardCard piker S.alice radded
        stocked = libraryTopped [mountain, mountain, mountain] S.alice placed
        milled = afterTheMill stocked
        after = activated masterId master milled
    -- CR 602.2b/601.2c: an ability with a target slot no candidate fits cannot be
    -- activated at all, which is where the divergence would show first.
    Spec.assertEqWith s "the mill happened" (length (Game.zoneMembers Zone.Library S.alice milled)) 2
    Spec.assertEqWith s "but the ability is not offered" (activationsOffered masterId milled) 0
    Spec.assertEqWith s "and the Piker is still in the graveyard" (fmap Object.zone (Game.lookupObject pikerId after)) (Just Zone.Graveyard)
    Spec.assertEqWith s "with nothing but The Master on the battlefield" (Game.zoneMembers Zone.Battlefield S.alice after) [masterId]

-- alice with The Master settled on the battlefield, one rad counter, and one
-- creature card sitting in her graveyard from the start.
radBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
radBoard master bystander =
  let (masterId, board) = S.addCreature master S.alice (Setup.emptyGame S.bothPlayers)
      (bystanderId, withBystander) = S.addGraveyardCard bystander S.alice board
   in (masterId, bystanderId, S.addPlayerCounter PlayerCounterKind.Rad 1 S.alice withBystander)

-- The board after rule 728.1's ability has resolved on alice's precombat main
-- phase, with alice holding priority again.
afterTheMill :: GameState.GameState -> GameState.GameState
afterTheMill gs =
  let after = S.runPure S.identityAnswer (precombatMainOf S.alice gs) (Engine.runStep >> Engine.priorityLoop)
   in after {GameState.priority = Just S.alice}

-- How many activations of this object CR 602.2b offers alice -- 0 where the
-- ability's one target slot has no candidate.
activationsOffered :: ObjectId.ObjectId -> GameState.GameState -> Int
activationsOffered oid gs = length (filter (isActivateOf oid) (Action.legalActions S.alice gs))

isActivateOf :: ObjectId.ObjectId -> A.Action -> Bool
isActivateOf oid action = case action of
  A.Activate o _ -> o == oid
  _ -> False

-- alice activates The Master's one activated ability and everything resolves.
-- S.identityAnswer picks the least recipient offered, which is deliberately NOT
-- the milled card -- see the group's note.
activated :: ObjectId.ObjectId -> Printing.Printing -> GameState.GameState -> GameState.GameState
activated masterId master gs =
  S.runPure S.identityAnswer gs (Activate.activateAbility S.alice masterId (theReanimation master) >> Engine.priorityLoop)

-- The Master's sole activated ability, off its printed face.
theReanimation :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Card
theReanimation printing = case Face.activatedAbilities (S.combinedFace printing) of
  ability : _ -> ability
  [] -> error "Pawl.RadSpec: The Master, Transcendent has no activated ability"

-- How many rad counters this player has (CR 122.1i), zero for a player who has
-- never had one.
radOf :: PlayerId.PlayerId -> GameState.GameState -> Natural.Natural
radOf = S.playerCounterOf PlayerCounterKind.Rad

-- The four lands The Master's {1}{B}{G}{U} needs, one of each colour it asks for
-- plus one to pay the generic.
fourColorLands :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m [Printing.Printing]
fourColorLands s registry = traverse (S.printingOf s registry) ["Swamp", "Forest", "Island", "Mountain"]

-- alice's side of the board: one untapped land of each printing.
boardOf :: [Printing.Printing] -> GameState.GameState
boardOf = foldr (\land gs -> snd (S.addCreature land S.alice gs)) (Setup.emptyGame S.bothPlayers)

-- The given printings in pid's library, FIRST ONE ON TOP -- S.addLibraryCard
-- puts each new card at the front, so the list is laid down back to front.
libraryTopped :: [Printing.Printing] -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
libraryTopped printings pid gs = foldl (\g p -> snd (S.addLibraryCard p pid g)) gs (reverse printings)

-- The board just after pid's precombat main phase began, settled for priority:
-- the moment CR 603.4 has decided whether rule 728.1's ability is on the stack,
-- and before any priority round could resolve it away. The StepBegan record is
-- staged directly, as Pawl.TriggerSpec's StepBegins cases stage theirs, because
-- Engine.runStep writes it and then runs that very priority round.
settledAtPrecombatMain :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
settledAtPrecombatMain pid gs =
  let staged = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan Phase.PrecombatMain pid)] (precombatMainOf pid gs)
   in S.runPure S.identityAnswer staged Engine.settleForPriority

-- A board sitting in pid's precombat main phase, the moment CR 728.1 names.
precombatMainOf :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
precombatMainOf pid gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = pid,
      GameState.priority = Just pid
    }

-- An interpreter that aims every target slot at one player, where
-- S.identityAnswer takes the least recipient -- which on this board is alice,
-- the caster.
targeting :: PlayerId.PlayerId -> Prompt.Prompt r -> r
targeting pid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap (\(_, legal) -> Set.filter (== Recipient.ToPlayer pid) legal) sets
  _ -> S.identityAnswer p
