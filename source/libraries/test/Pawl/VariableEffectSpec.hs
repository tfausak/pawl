{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over the effects whose size or recipients a player picks:
-- amass, blight (CR 701.68), support, bolster, and the variable target counts.
-- The machinery is Pawl.ResolveSpec.
module Pawl.VariableEffectSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterName as CounterName
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetCount as TargetCount
import qualified Pawl.Types.Zone as Zone

-- Pays what a resolving spell or ability offers `who`, and declines elsewhere,
-- so a test can tell an honoured answer from the fallback. Guarded on a NAMED
-- player, which is what makes CR 118.12's "who" provable. Rank-1, like
-- Pawl.Support.attackTo, so this is the `forall r. Prompt r -> r` that
-- Replay.record wants. Pawl.CounterspellSpec keeps its own copy.
paysFor :: PlayerId.PlayerId -> Prompt.Prompt r -> r
paysFor who p = case p of
  Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
    | d == who && player == who ->
        PaymentDecision.Pays
  _ -> S.identityAnswer p

-- The pay-or-not answers in a transcript, in order.
payResponses :: [Response.Response] -> [Response.Response]
payResponses = filter isPayResponse

isPayResponse :: Response.Response -> Bool
isPayResponse response = case response of
  Response.ChoseToPay _ -> True
  _ -> False

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Put k cards of a printing into pid's library, each on top of the last, for a
-- draw to find.
stockLibrary :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
stockLibrary printing pid k gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. k]

-- CR 601.2c's announcement, answered with a stated number for every variable
-- slot -- where S.identityAnswer announces as many as the board allows.
announcingCount :: Natural -> Prompt.Prompt r -> r
announcingCount n p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const n) offers
  _ -> S.identityAnswer p

-- Announces `n` targets per slot and aims them at `wanted`, in that order of
-- preference. S.identityAnswer would take the least Recipients instead, which on
-- these boards is not what the assertions are about.
takingTargets :: Natural -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
takingTargets n wanted p = case p of
  Prompt.AnnounceTargets {} -> announcingCount n p
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (maybe False (\oid -> elem oid wanted) . Recipient.objectOf) sets
  _ -> S.identityAnswer p

-- The +1/+1 counters on one permanent.
plusCountersOn :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural
plusCountersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)

-- CR 601.2c's count above one, read at resolution.
--
-- Hearts on Fire {1}{R} Instant (data/cards/hearts-on-fire.json): "One or two
-- target creatures each get +2/+1 until end of turn." A range whose minimum is
-- neither zero nor its maximum, so it exercises both ends -- castability gates on
-- the minimum, the announcement chooses between one and two, and CR 608.2b's
-- per-recipient legality shows on the survivor when the other target goes.
--
-- Agent Bishop, Man in Black {2}{W} 1/2 (data/cards/agent-bishop-man-in-black.json):
-- "At the beginning of combat on your turn, put a +1/+1 counter on each of up to
-- two target creatures." The same count on a TRIGGERED ability, where
-- Resolve.resolveModes rather than Resolve.targetsAllIllegal asks CR 608.2b's
-- question.
multiTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
multiTargetSpec s registry = Spec.describe s "MultiTarget" $ do
  -- Three creatures with three different power/toughness boxes, so which two were
  -- pumped is legible; two targets out of three is what makes the count a choice
  -- rather than a sweep.
  Spec.it s "CR 601.2c Hearts on Fire pumps the two creatures it named, and only those" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- heartsBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        after = resolveOne answer gs spellId
    Spec.assertEqWith s "the Piker is +2/+1" (S.powerToughnessOf pikerId after) (Just (4, 2))
    Spec.assertEqWith s "the Wall is +2/+1" (S.powerToughnessOf wallId after) (Just (2, 9))
    Spec.assertEqWith s "the Rats, whom nobody named, are untouched" (S.powerToughnessOf ratsId after) (Just (1, 1))
  -- The same board and the same spell, differing only in the announced number.
  Spec.it s "CR 601.2c announcing one target pumps one creature" $ do
    (pikerId, _, wallId, gs, spellId) <- heartsBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 1 [pikerId, wallId]
        after = resolveOne answer gs spellId
    Spec.assertEqWith s "the Piker is +2/+1" (S.powerToughnessOf pikerId after) (Just (4, 2))
    Spec.assertEqWith s "and the second creature it would have taken is untouched" (S.powerToughnessOf wallId after) (Just (0, 8))
  -- CR 608.2b: "Illegal targets, if any, won't be affected by parts of a
  -- resolving spell's effect for which they're illegal." One target of two leaves
  -- the battlefield between the announcement and the resolution, which under a
  -- per-SLOT reading of that rule would take the survivor down with it.
  Spec.it s "CR 608.2b one of two targets leaving does not stop the other being pumped" $ do
    (pikerId, _, wallId, gs, spellId) <- heartsBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        gone = snd (Engine.runGamePure answer cast (Event.changeZone wallId Zone.Graveyard))
        after = snd (Engine.runGamePure answer gone Stack.resolveTop)
    Spec.assertEqWith s "the surviving target is +2/+1" (S.powerToughnessOf pikerId after) (Just (4, 2))
    Spec.assertBool s (not (Set.member wallId (GameState.battlefield after))) "and the other one is gone"
  -- CR 601.2c's minimum, which is castability's question: one legal creature is
  -- enough for "one or two", and none is not. Both boards hold the same two
  -- Mountains, so the creature is the only difference between them.
  Spec.it s "CR 601.2c a minimum above zero gates castability" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    hearts <- S.printingOf s registry "Hearts on Fire"
    let lands = S.landsInPlay mountain 2
        (_, withCreature) = S.addCreature piker S.bob lands
        castable board = let (gs, spellId) = S.handOne hearts board in S.castable S.alice spellId gs
    Spec.assertBool s (castable withCreature) "one creature is enough for one or two targets"
    Spec.assertBool s (not (castable lands)) "and no creature is not"
  -- The ability path's own CR 608.2b (Resolve.resolveModes), which the spell path
  -- above does not reach.
  Spec.it s "CR 601.2c Agent Bishop's trigger counters the two creatures it named" $ do
    (bishopId, pikerId, ratsId, wallId, gs) <- bishopBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        after = S.runPure answer (S.runPure answer gs Engine.settleForPriority) Stack.resolveTop
    Spec.assertEqWith s "the Piker took a counter" (plusCountersOn pikerId after) (Just 1)
    Spec.assertEqWith s "so did the Wall" (plusCountersOn wallId after) (Just 1)
    Spec.assertEqWith s "the Rats, whom nobody named, took none" (plusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "and neither did Bishop" (plusCountersOn bishopId after) (Just 0)
  Spec.it s "CR 608.2b Agent Bishop's trigger still counters the target that survives" $ do
    (_, pikerId, _, wallId, gs) <- bishopBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        -- The trigger is PLACED (CR 603.3, targets announced) and then one of them
        -- leaves, so CR 608.2b has something to re-validate when it resolves.
        placed = S.runPure answer gs Engine.settleForPriority
        gone = S.runPure answer placed (Event.changeZone wallId Zone.Graveyard)
        after = S.runPure answer gone Stack.resolveTop
    Spec.assertEqWith s "the surviving target took its counter" (plusCountersOn pikerId after) (Just 1)
    Spec.assertBool s (not (Set.member wallId (GameState.battlefield after))) "and the other one is gone"

-- Two Mountains for Hearts on Fire, three of bob's creatures with three distinct
-- printed boxes (2/1, 1/1, 0/8), and the spell in alice's hand.
heartsBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
heartsBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  hearts <- S.printingOf s registry "Hearts on Fire"
  let (pikerId, g1) = S.addCreature piker S.bob (S.landsInPlay mountain 2)
      (ratsId, g2) = S.addCreature rats S.bob g1
      (wallId, g3) = S.addCreature wall S.bob g2
      (gs, spellId) = S.handOne hearts g3
  pure (pikerId, ratsId, wallId, gs, spellId)

-- Agent Bishop on alice's battlefield with the same three creatures, at the
-- beginning of her combat, where its ability triggers.
bishopBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
bishopBoard s registry = do
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  bishop <- S.printingOf s registry "Agent Bishop, Man in Black"
  let (bishopId, g1) = S.addCreature bishop S.alice (Setup.emptyGame S.bothPlayers)
      (pikerId, g2) = S.addCreature piker S.bob g1
      (ratsId, g3) = S.addCreature rats S.bob g2
      (wallId, g4) = S.addCreature wall S.bob g3
      combat = Phase.Combat CombatStep.BeginningOfCombat
      gs =
        Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan combat S.alice)) $
          g4
            { GameState.phase = combat,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
  pure (bishopId, pikerId, ratsId, wallId, gs)

-- Cast a spell from alice's hand and resolve it.
resolveOne :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
resolveOne answer gs spellId =
  let cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- CR 701.47 amass, which is an opcode: Effect.Amass over a subtype and a Quantity,
-- whose Army token, candidate pool and counter kind are rule 701.47a's rather than
-- the card's.
--
-- Relentless Advance {3}{U} Sorcery (data/cards/relentless-advance.json): "Amass
-- Zombies 3.", and nothing else -- so every token and every counter on these boards
-- came from this keyword action.
--
-- Mordor Muster {1}{B} Sorcery (data/cards/mordor-muster.json): "You draw a card
-- and you lose 1 life. Amass Orcs 1." A SECOND subtype is what makes rule 701.47a's
-- last instruction observable at all: a card that only ever amasses its own subtype
-- cannot tell the type addition from the token's printed types, because the token it
-- would have created already has them.
--
-- Three and one are distinct from each other and from their sum, so a counter
-- assertion cannot be satisfied by a coincidence: 3, 1, 4 and 6 each name exactly
-- one history of amasses.
amassSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
amassSpec s registry = Spec.describe s "Amass" $ do
  Spec.it s "CR 701.47a amass with no Army creates the 0/0 black Army token the rule prints" $ do
    (gs, advanceId, _) <- amassBoard s registry
    let after = resolveOne S.identityAnswer gs advanceId
    case S.tokensOf after of
      [army] -> do
        Spec.assertEqWith s "black, which is the rule's colour and not the card's" (Projection.colorsOf army after) (Set.singleton Color.Black)
        -- CR 111.4: rule 701.47a names no token, so the name is its subtypes plus
        -- the word "Token".
        Spec.assertEqWith s "named for its subtypes" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Zombie Army Token") S.alice after) 1
        Spec.assertEqWith s "a Zombie Army" (Projection.subtypesOf army after) (Set.fromList [Subtype.Zombie, Subtype.Army])
        Spec.assertEqWith s "three +1/+1 counters" (plusCountersOn army after) (Just 3)
        -- CR 704.3: state-based actions are not checked mid-resolution, so the 0/0
        -- the rule prints lives to take its counters.
        Spec.assertEqWith s "0/0 plus three counters" (S.powerToughnessOf army after) (Just (3, 3))
      other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
  -- Rule 701.47a's first instruction taken the other way, and its last, on one
  -- board: the second amass finds an Army and so creates nothing, and its subtype
  -- lands on the Army that was already there.
  Spec.it s "CR 701.47a a second amass creates no second token and adds its subtype to the Army" $ do
    (gs, advanceId, musterId) <- amassBoard s registry
    let after = resolveOne S.identityAnswer (resolveOne S.identityAnswer gs advanceId) musterId
    case S.tokensOf after of
      [army] -> do
        Spec.assertEqWith s "three counters and then one more" (plusCountersOn army after) (Just 4)
        -- CR 205.1b: "in addition to its other types", so the Zombie survives the
        -- Orc rather than being replaced by it.
        Spec.assertEqWith s "an Orc Zombie Army" (Projection.subtypesOf army after) (Set.fromList [Subtype.Orc, Subtype.Zombie, Subtype.Army])
        Spec.assertEqWith s "0/0 plus four counters" (S.powerToughnessOf army after) (Just (4, 4))
      other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
  -- CR 701.47a's "an Army creature YOU CONTROL", both times it appears: bob's Army
  -- neither stops alice's token being created nor takes her counters.
  Spec.it s "CR 701.47a an opponent's Army is not an Army you control" $ do
    (gs, advanceId, musterId) <- opposedAmassBoard s registry
    let after = resolveOne S.identityAnswer (resolveFor S.bob S.identityAnswer gs musterId) advanceId
    case S.tokensOf after of
      [bobArmy, aliceArmy] -> do
        Spec.assertEqWith s "alice amassed her own Army" (plusCountersOn aliceArmy after) (Just 3)
        Spec.assertEqWith s "a Zombie Army" (Projection.subtypesOf aliceArmy after) (Set.fromList [Subtype.Zombie, Subtype.Army])
        Spec.assertEqWith s "bob's Army kept the one counter he amassed" (plusCountersOn bobArmy after) (Just 1)
        Spec.assertEqWith s "and did not become a Zombie" (Projection.subtypesOf bobArmy after) (Set.fromList [Subtype.Orc, Subtype.Army])
      other -> Spec.assertFailure s ("expected exactly two tokens, got " <> show (length other))
  Spec.it s "CR 701.47a amass counters the Army its controller chose" $ do
    (bobArmy, aliceArmy, gs, advanceId) <- stolenArmyBoard s registry
    let after = resolveOne (amassing aliceArmy) gs advanceId
    Spec.assertEqWith s "her own Army, whom she named, took three more" (plusCountersOn aliceArmy after) (Just 6)
    Spec.assertEqWith s "the borrowed Army took none" (plusCountersOn bobArmy after) (Just 1)
    Spec.assertEqWith s "and no third token was created" (length (S.tokensOf after)) 2
  -- The same board and the same spell, differing only in the answer: the engine
  -- makes no choice, so the other Army is equally reachable -- and the subtype
  -- follows the choice, which is what makes rule 701.47a's last instruction act on
  -- the CHOSEN Army rather than on the amassing player's Armies at large.
  Spec.it s "CR 701.47a the same board answered the other way counters the other Army" $ do
    (bobArmy, aliceArmy, gs, advanceId) <- stolenArmyBoard s registry
    let after = resolveOne (amassing bobArmy) gs advanceId
    Spec.assertEqWith s "the borrowed Army, whom she named, took three" (plusCountersOn bobArmy after) (Just 4)
    Spec.assertEqWith s "and became a Zombie as well as an Orc" (Projection.subtypesOf bobArmy after) (Set.fromList [Subtype.Orc, Subtype.Zombie, Subtype.Army])
    Spec.assertEqWith s "her own Army took none" (plusCountersOn aliceArmy after) (Just 3)
    Spec.assertEqWith s "and stayed a plain Zombie Army" (Projection.subtypesOf aliceArmy after) (Set.fromList [Subtype.Zombie, Subtype.Army])
  -- Where the rules leave nothing to ask, do not ask. The two boards differ in how
  -- many Armies their controller has, which is the whole of what makes rule
  -- 701.47a's choice a choice.
  Spec.it s "CR 701.47a a lone Army raises no prompt" $ do
    (alone, aloneSpell, _) <- amassBoard s registry
    (_, _, two, twoSpell) <- stolenArmyBoard s registry
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseAmass {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks g spellId =
          State.execState (Engine.runGame countingAnswer g (S.cast S.alice spellId >> Stack.resolveTop)) 0
    -- The first amass on the first board has no Army at all until it makes one, and
    -- one Army is the whole of the candidate set.
    Spec.assertEqWith s "one Army: nothing to ask" (asks alone aloneSpell) 0
    Spec.assertEqWith s "two Armies: one real decision" (asks two twoSpell) 1

-- alice has six Islands and six Swamps untapped, Relentless Advance and Mordor
-- Muster in hand, and a card left in her library for the Muster's draw (CR 104.3c).
-- Twelve lands rather than the six the two spells cost, so that whichever lands the
-- first payment takes, the second is still payable.
amassBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
amassBoard s registry = do
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  advance <- S.printingOf s registry "Relentless Advance"
  muster <- S.printingOf s registry "Mordor Muster"
  let g1 = S.landsFor swamp S.alice 6 (S.landsInPlay island 6)
      (g2, advanceId) = S.handOne advance g1
      (musterId, g3) = S.addHandCard muster S.alice g2
      g4 = snd (S.addLibraryCard island S.alice g3)
  pure (g4, advanceId, musterId)

-- amassBoard with the Muster moved across the table: bob holds it, with his own
-- lands and his own library card, so the two spells are cast from two seats. The
-- same printings, the same counts and the same phase -- only the seat differs.
opposedAmassBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
opposedAmassBoard s registry = do
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  advance <- S.printingOf s registry "Relentless Advance"
  muster <- S.printingOf s registry "Mordor Muster"
  let g1 = S.landsFor swamp S.bob 6 (S.landsInPlay island 6)
      (g2, advanceId) = S.handOne advance g1
      (musterId, g3) = S.addHandCard muster S.bob g2
      g4 = snd (S.addLibraryCard island S.bob g3)
  pure (g4, advanceId, musterId)

-- Two Armies under one player's control, which is the only board on which rule
-- 701.47a's choice is a choice. bob amasses Orcs, alice amasses Zombies, and alice
-- then gains control of bob's Army (CR 613.1b's layer 2) -- so her second Relentless
-- Advance sees two Armies, one of each subtype. Returns bob's Army, alice's own, the
-- board and the spell still in her hand.
--
-- Ten Islands, since alice casts Relentless Advance twice: six leaves the second
-- unpayable, and an uncast spell is a board that proves nothing.
stolenArmyBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
stolenArmyBoard s registry = do
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  advance <- S.printingOf s registry "Relentless Advance"
  muster <- S.printingOf s registry "Mordor Muster"
  let g1 = S.landsFor swamp S.bob 6 (S.landsInPlay island 10)
      (g2, firstId) = S.handOne advance g1
      (secondId, g3) = S.addHandCard advance S.alice g2
      (musterId, g4) = S.addHandCard muster S.bob g3
      g5 = snd (S.addLibraryCard island S.bob g4)
      amassed = resolveOne S.identityAnswer (resolveFor S.bob S.identityAnswer g5 musterId) firstId
  case S.tokensOf amassed of
    [bobArmy, aliceArmy] -> pure (bobArmy, aliceArmy, S.giveControl bobArmy S.alice amassed, secondId)
    other -> Spec.assertFailure s ("expected exactly two tokens, got " <> show (length other))

-- resolveOne for a seat other than alice's: bob casts and the spell resolves.
resolveFor :: PlayerId.PlayerId -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
resolveFor pid answer gs spellId =
  let cast = snd (Engine.runGamePure answer gs (S.cast pid spellId))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- CR 701.68 blight, which is an opcode: Effect.Blight over a PlayerRef and a
-- Quantity, whose candidate pool and counter kind are rule 701.68a's rather than
-- the card's. A bare printed "blight N" is CR 109.5's `Relative You`, which is
-- what every case here reads; blightPlayerSpec below is the other reading.
--
-- Sinister Gnarlbark {2}{B} 0/4 Creature -- Treefolk Warlock
-- (data/cards/sinister-gnarlbark.json): "At the beginning of your end step, draw a
-- card and blight 1." (Name, cost, type line, P/T and oracle text checked against
-- Scryfall.) Every -1/-1 counter on these boards came from the keyword action, and
-- the draw beside it is what shows the REST of a mandatory instruction still runs
-- when the blight itself cannot (CR 101.3).
--
-- The pool is UNCONSTRAINED, which is the whole difference from bolster: the
-- boards below carry a 2/1, a 1/1, a 0/8 and the 0/4 source -- toughnesses 1, 1, 8
-- and 4, so the least is TIED and two creatures are clear of it. A case that names
-- the 0/8 proves no least-toughness narrowing is happening, and one that names the
-- source proves the blighting permanent is in its own pool.
--
-- Every assertion below reads counters on ONE named creature and reads the other
-- three back as zero, so no case can be satisfied by counters that landed
-- somewhere else.
blightSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blightSpec s registry = Spec.describe s "Blight" $ do
  Spec.it s "CR 701.68a blight 1 counters the creature its controller chose" $ do
    (pikerId, ratsId, wallId, gnarlbarkId, gs) <- blightBoard s registry S.alice
    let after = S.runPure (blighting pikerId) gs Stack.resolveTop
    Spec.assertEqWith s "the Piker, whom their controller named, took one" (minusCountersOn pikerId after) (Just 1)
    Spec.assertEqWith s "the Rats took none" (minusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "nor did the Wall" (minusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "nor the Gnarlbark itself" (minusCountersOn gnarlbarkId after) (Just 0)
    Spec.assertEqWith s "and the card was drawn" (S.handSize S.alice after) 1
  -- The same board and the same trigger, differing only in the answer: the engine
  -- makes no choice, so every other creature in the pool is equally reachable.
  Spec.it s "CR 701.68a the same board answered another way counters that creature" $ do
    (pikerId, ratsId, wallId, gnarlbarkId, gs) <- blightBoard s registry S.alice
    let after = S.runPure (blighting ratsId) gs Stack.resolveTop
    Spec.assertEqWith s "the Rats, whom their controller named, took one" (minusCountersOn ratsId after) (Just 1)
    Spec.assertEqWith s "the Piker took none" (minusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor did the Wall" (minusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "nor the Gnarlbark itself" (minusCountersOn gnarlbarkId after) (Just 0)
  -- Rule 701.68a's pool is "a creature you control" and stops there. The 0/8 is the
  -- TOUGHEST creature on the board and the 0/4 is the blighting permanent itself;
  -- both are candidates, where CR 701.39a's least-toughness narrowing would offer
  -- neither.
  Spec.it s "CR 701.68a any creature its controller controls is a candidate, including the source" $ do
    (pikerId, ratsId, wallId, _, wallBoard) <- blightBoard s registry S.alice
    (_, _, _, gnarlbarkId, sourceBoard) <- blightBoard s registry S.alice
    let onWall = S.runPure (blighting wallId) wallBoard Stack.resolveTop
        onSource = S.runPure (blighting gnarlbarkId) sourceBoard Stack.resolveTop
    Spec.assertEqWith s "the Wall, at toughness 8, took one" (minusCountersOn wallId onWall) (Just 1)
    Spec.assertEqWith s "and the 1/1 beside it took none" (minusCountersOn ratsId onWall) (Just 0)
    Spec.assertEqWith s "the Gnarlbark blighted itself" (minusCountersOn gnarlbarkId onSource) (Just 1)
    Spec.assertEqWith s "and the Piker took none" (minusCountersOn pikerId onSource) (Just 0)
  -- CR 701.68a's "a creature YOU CONTROL". The two boards hold the same four
  -- printings and differ in exactly one thing -- which seat the other three
  -- creatures sit on -- and the answer names bob's Piker on both. It is never
  -- offered, so alice's own Gnarlbark takes the counter instead.
  Spec.it s "CR 701.68a an opponent's creature is not a creature you control" $ do
    (pikerId, ratsId, wallId, gnarlbarkId, gs) <- blightBoard s registry S.bob
    let after = S.runPure (blighting pikerId) gs Stack.resolveTop
    Spec.assertEqWith s "alice's Gnarlbark, her only creature, took one" (minusCountersOn gnarlbarkId after) (Just 1)
    Spec.assertEqWith s "bob's Piker, whom the answer named, took none" (minusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor did bob's Rats" (minusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "nor his Wall" (minusCountersOn wallId after) (Just 0)
  -- Where the rules leave nothing to ask, do not ask. The same pair of boards, and
  -- what differs is how many creatures the blighting player controls -- which is
  -- the whole of what makes rule 701.68a's choice a choice.
  Spec.it s "CR 701.68a a lone creature raises no prompt" $ do
    (_, _, _, _, four) <- blightBoard s registry S.alice
    (_, _, _, _, alone) <- blightBoard s registry S.bob
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseBlight {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks g = State.execState (Engine.runGame countingAnswer g Stack.resolveTop) 0
    Spec.assertEqWith s "one creature: nothing to ask" (asks alone) 0
    Spec.assertEqWith s "four creatures: one real decision" (asks four) 1
  -- CR 101.3: the impossible PART is ignored, not the instruction. The Gnarlbark
  -- dies to state-based actions with its own trigger already on the stack (CR
  -- 603.3b), so the blight has no creature to reach -- and the draw beside it still
  -- happens, which is what tells "ignored" apart from "aborted".
  Spec.it s "CR 101.3 a controller with no creature blights nothing and draws anyway" $ do
    (pikerId, _, _, gnarlbarkId, gs) <- blightBoard s registry S.bob
    let dead = S.settleSba (S.markDamage gnarlbarkId 4 gs)
        after = S.runPure S.identityAnswer dead Stack.resolveTop
    Spec.assertBool s (not (S.onBattlefield gnarlbarkId after)) "the Gnarlbark left the battlefield before its trigger resolved"
    Spec.assertEqWith s "the card was drawn all the same" (S.handSize S.alice after) 1
    Spec.assertEqWith s "and bob's creatures, who were never candidates, took nothing" (minusCountersOn pikerId after) (Just 0)

-- Sinister Gnarlbark on alice's battlefield and Goblin Piker, Typhoid Rats and Wall
-- of Stone on `pid`'s, with a card in alice's library for the draw (CR 104.3c), her
-- end step begun and the trigger settled onto the stack (CR 603.3b). Returns the
-- three creatures, the Gnarlbark and that state.
--
-- The seat is the ONLY parameter, so the pool board and its negative are the same
-- four printings, the same library and the same step.
blightBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
blightBoard s registry pid = do
  swamp <- S.printingOf s registry "Swamp"
  gnarlbark <- S.printingOf s registry "Sinister Gnarlbark"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  let (pikerId, g1) = S.addCreature piker pid (Setup.emptyGame S.bothPlayers)
      (ratsId, g2) = S.addCreature rats pid g1
      (wallId, g3) = S.addCreature wall pid g2
      (gnarlbarkId, g4) = S.addCreature gnarlbark S.alice g3
      g5 = snd (S.addLibraryCard swamp S.alice g4)
      endStep = Phase.Ending EndingStep.EndStep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice))
          (g5 {GameState.phase = endStep, GameState.activePlayer = S.alice})
  pure (pikerId, ratsId, wallId, gnarlbarkId, snd (Engine.runGamePure S.identityAnswer begun Engine.settleForPriority))

-- Answers Prompt.ChooseBlight with a named creature, deferring everything else to
-- S.identityAnswer. PINNED BY ID rather than picked by searching the candidates, so
-- a mutation to the candidate sweep cannot quietly repair the answer.
blighting :: ObjectId.ObjectId -> Prompt.Prompt r -> r
blighting oid p = case p of
  Prompt.ChooseBlight {} -> oid
  _ -> S.identityAnswer p

-- The -1/-1 counters on one permanent, plusCountersOn's sibling: Nothing once the
-- object is gone, which is what keeps "took none" apart from "is not there".
minusCountersOn :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural
minusCountersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject oid gs)

-- CR 701.68a's "you" is whoever the instruction ADDRESSES, which need not be the
-- resolving controller -- the axis Effect.Blight's PlayerRef adds.
--
-- High Perfect Morcant {2}{B}{G} 4/4 Legendary Creature -- Elf Noble
-- (data/cards/high-perfect-morcant.json): "Whenever High Perfect Morcant or
-- another Elf you control enters, each opponent blights 1." (Name, cost, type
-- line, P/T and oracle text checked against Scryfall.) Its second printed ability,
-- "Tap three untapped Elves you control: Proliferate. Activate only as a
-- sorcery", is transcribed as of #1650's fix: CostComponent's TapPermanents is
-- the count-plus-criterion cost that ability wants, and Pawl.CostSpec's "High
-- Perfect Morcant" group is what proves it pays.
--
-- WHY MORCANT and not Champion of the Weird, which #1491's body nominates: that
-- card's "As an additional cost to cast this spell, behold a Goblin and exile it"
-- is CR 701.4's keyword action, which pawl does not have (gap #876). Dropping an
-- additional cost would leave pawl's card WEAKER than printed, which disqualifies
-- it.
--
-- THREE SEATS, because "each opponent" and "every player but you" and "the one
-- other seat" are the same set on a two-player board. alice is the active player,
-- so APNAP order is alice, bob, carol.
--
-- Every seat holds a 1-toughness creature and an 0/8, so each blighter faces a
-- real choice and which creature took the counter is readable off the toughness.
-- The pinned answer is each seat's SECOND creature, which is never
-- Pawl.Engine.Blight.candidates' ascending head -- so an answerer that ignored the
-- seat, or a prompt raised for the wrong seat, cannot land on it by accident.
blightPlayerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blightPlayerSpec s registry = Spec.describe s "BlightPlayer" $ do
  -- The blighters are the CONTROLLER'S opponents, and the controller is not among
  -- them: both of alice's creatures and the Morcant itself end at zero, on a board
  -- where alice controls three of the seven creatures in play.
  Spec.it s "CR 701.68a whole card: each opponent blights and the trigger's controller does not" $ do
    (morcantId, (aPiker, aWall), (bRats, bWall), (cPiker, cWall), gs) <- morcantBoard s registry S.alice
    let after = S.runPure (blightingFor [(S.bob, bWall), (S.carol, cWall)]) gs Stack.resolveTop
    Spec.assertEqWith s "bob's Wall, whom he named, took one" (minusCountersOn bWall after) (Just 1)
    Spec.assertEqWith s "carol's Wall, whom she named, took one" (minusCountersOn cWall after) (Just 1)
    Spec.assertEqWith s "bob's Rats took none" (minusCountersOn bRats after) (Just 0)
    Spec.assertEqWith s "nor did carol's Piker" (minusCountersOn cPiker after) (Just 0)
    Spec.assertEqWith s "alice, who controls the trigger, blights nothing: her Piker took none" (minusCountersOn aPiker after) (Just 0)
    Spec.assertEqWith s "nor did her Wall" (minusCountersOn aWall after) (Just 0)
    Spec.assertEqWith s "nor the Morcant itself" (minusCountersOn morcantId after) (Just 0)
  -- The same seven printings, the same six creatures, the same trigger: the ONE
  -- difference is which seat the Morcant entered under. The set that blights moves
  -- with it, which is what tells `Relative Opponent` apart from any fixed seat.
  Spec.it s "CR 701.68a the same board with the Morcant under another seat blights the other two" $ do
    (morcantId, (aPiker, aWall), (bRats, bWall), (_, cWall), gs) <- morcantBoard s registry S.bob
    let after = S.runPure (blightingFor [(S.alice, aWall), (S.carol, cWall)]) gs Stack.resolveTop
    Spec.assertEqWith s "alice's Wall, whom she named, took one" (minusCountersOn aWall after) (Just 1)
    Spec.assertEqWith s "carol's Wall, whom she named, took one" (minusCountersOn cWall after) (Just 1)
    Spec.assertEqWith s "alice's Piker took none" (minusCountersOn aPiker after) (Just 0)
    Spec.assertEqWith s "bob, who now controls the trigger, blights nothing: his Rats took none" (minusCountersOn bRats after) (Just 0)
    Spec.assertEqWith s "nor did his Wall" (minusCountersOn bWall after) (Just 0)
    Spec.assertEqWith s "nor the Morcant itself" (minusCountersOn morcantId after) (Just 0)
  -- Rule 701.68a's "a creature YOU control" is read per BLIGHTER and not once for
  -- the resolution: bob's answer names carol's Wall, which was never offered to
  -- him, so the head of his own pool takes the counter instead -- and carol's Wall
  -- stays clean, which is what says his answer did not reach across the table.
  Spec.it s "CR 701.68a each blighter's pool is their own creatures" $ do
    (_, (aPiker, aWall), (bRats, bWall), (cPiker, cWall), gs) <- morcantBoard s registry S.alice
    let after = S.runPure (blightingFor [(S.bob, cWall), (S.carol, cPiker)]) gs Stack.resolveTop
    Spec.assertEqWith s "bob's Rats, the head of his own pool, took the counter" (minusCountersOn bRats after) (Just 1)
    Spec.assertEqWith s "his Wall took none" (minusCountersOn bWall after) (Just 0)
    Spec.assertEqWith s "carol's Piker, whom she named, took one" (minusCountersOn cPiker after) (Just 1)
    Spec.assertEqWith s "carol's Wall, whom BOB named, took none" (minusCountersOn cWall after) (Just 0)
    Spec.assertEqWith s "alice's Piker took none" (minusCountersOn aPiker after) (Just 0)
    Spec.assertEqWith s "nor did her Wall" (minusCountersOn aWall after) (Just 0)
  -- Who was ASKED, rather than what the counters say: the prompt is raised for each
  -- blighter and for nobody else, in APNAP order (CR 101.4) off alice's turn.
  Spec.it s "CR 101.4 the prompt is raised for each blighter in APNAP order" $ do
    (_, _, _, _, underAlice) <- morcantBoard s registry S.alice
    (_, _, _, _, underBob) <- morcantBoard s registry S.bob
    let asking :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
        asking p = case p of
          Prompt.ChooseBlight _ pid _ _ -> do
            State.modify (<> [pid])
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asked g = State.execState (Engine.runGame asking g Stack.resolveTop) []
    Spec.assertEqWith s "alice's Morcant asks her two opponents, in turn order" (asked underAlice) [S.bob, S.carol]
    Spec.assertEqWith s "bob's asks his, which APNAP puts the active player first" (asked underBob) [S.alice, S.carol]

-- Six creatures -- Goblin Piker and Wall of Stone for alice, Typhoid Rats and Wall
-- of Stone for bob, Goblin Piker and Wall of Stone for carol -- and High Perfect
-- Morcant entering under `pid` with its CR 603.6a trigger settled onto the stack
-- but NOT resolved. Returns the Morcant, the three pairs and that state.
--
-- The seat is the ONLY parameter, so the board and its counterpart are the same
-- seven printings on the same three seats.
--
-- Each pair is added lowest-toughness first, so Pawl.Engine.Blight.candidates'
-- ascending head is the 1-toughness creature and the pinned 0/8 is the other one.
morcantBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  m (ObjectId.ObjectId, (ObjectId.ObjectId, ObjectId.ObjectId), (ObjectId.ObjectId, ObjectId.ObjectId), (ObjectId.ObjectId, ObjectId.ObjectId), GameState.GameState)
morcantBoard s registry pid = do
  morcant <- S.printingOf s registry "High Perfect Morcant"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  let (aPiker, g1) = S.addCreature piker S.alice S.threePlayerGame
      (aWall, g2) = S.addCreature wall S.alice g1
      (bRats, g3) = S.addCreature rats S.bob g2
      (bWall, g4) = S.addCreature wall S.bob g3
      (cPiker, g5) = S.addCreature piker S.carol g4
      (cWall, g6) = S.addCreature wall S.carol g5
      (morcantId, g7) = S.entersWithTrigger morcant pid g6
  pure (morcantId, (aPiker, aWall), (bRats, bWall), (cPiker, cWall), snd (Engine.runGamePure S.identityAnswer g7 Engine.settleForPriority))

-- Answers Prompt.ChooseBlight with the creature pinned for the SEAT the prompt
-- names, deferring everything else to S.identityAnswer. Keyed by seat rather than
-- by a single object, which is what makes "the prompt was raised for that player"
-- observable off the counters: a prompt carrying the wrong seat falls through to
-- the identity answer and lands somewhere the assertions read as zero.
blightingFor :: [(PlayerId.PlayerId, ObjectId.ObjectId)] -> Prompt.Prompt r -> r
blightingFor pins p = case p of
  Prompt.ChooseBlight _ pid _ _ | Just oid <- List.lookup pid pins -> oid
  _ -> S.identityAnswer p

-- CR 101.4's LAST sentence, blightPlayerSpec above having proved its first:
-- "then the actions happen simultaneously". Two seats blight off one trigger, and
-- the question this group asks is whether that is one event or two.
--
-- Only a CR 603.2c batch condition can tell -- "whenever one or more counters are
-- put on one or more permanents", which reads the whole event group where every
-- other counter condition reads one placement. Nothing printed carries that
-- reading of a -1/-1 counter, so the producer is a synthetic:
--
--   * Synthetic Wilting Census {1}{B} Enchantment
--     (data/cards/synthetic-wilting-census.json): "Whenever one or more -1/-1
--     counters are put on one or more creatures, draw a card."
--
-- WHY A SYNTHETIC, in two queries. Scryfall o:"counters are put on one or more",
-- 2026-08-25, matches one printing: Cloaked Cadet, whose batch spans creatures but
-- counts +1/+1 counters on Humans, and rule 701.68a puts only -1/-1 counters --
-- so it can watch nothing blight does. Scryfall o:"one or more -1/-1 counters are
-- put", same date, matches Wickersmith's Tools and Auntie Ool, Cursewretch, whose
-- kind is right and whose batch is scoped to "A CREATURE" -- one creature, so they
-- fire once per creature however the placements are grouped, and two seats blight
-- them twice under either reading. A printing refuting the synthetic is one whose
-- batch spans creatures AND whose kind is -1/-1.
--
-- The COUNTERS are the same either way, which is why the card drawn is what the
-- cases read: one card for the batch, where a seat-at-a-time placement draws two.
blightSimultaneitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blightSimultaneitySpec s registry =
  let -- The distinct EventGroups the log's -1/-1 placements carry. The
      -- precondition the batch case rests on, asserted rather than assumed: were
      -- the two placements not one group, "once for the batch" would be proving
      -- nothing about rule 101.4.
      placementGroups gs =
        List.nub
          ( Maybe.mapMaybe
              ( \logged -> case LoggedEvent.event logged of
                  GameEvent.CountersPut change
                    | CounterChange.kind change == CounterKind.MinusOneMinusOne -> Just (LoggedEvent.group logged)
                  _ -> Nothing
              )
              (Foldable.toList (GameState.events gs))
          )
      -- alice's Synthetic Wilting Census and her High Perfect Morcant entering
      -- with its CR 603.6a trigger settled onto the stack, plus ONE Wall of Stone
      -- for each opponent. A single candidate per seat, so rule 701.68a's choice
      -- raises no prompt and nothing about the answerer is load-bearing here --
      -- which seat was asked in which order is blightPlayerSpec's question.
      --
      -- A Wall of Stone is an 0/8, so a -1/-1 counter is nowhere near CR 704.5f
      -- and every blighted creature is still standing when the counters are read.
      --
      -- Three Swamps in alice's library, so the Census can draw twice over without
      -- CR 104.3c deciding the case for it: a board that could not answer TWO
      -- cards would pass the batch assertion for the wrong reason.
      censusBoard opponents game = do
        census <- S.printingOf s registry "Synthetic Wilting Census"
        morcant <- S.printingOf s registry "High Perfect Morcant"
        wall <- S.printingOf s registry "Wall of Stone"
        swamp <- S.printingOf s registry "Swamp"
        let stocked = foldr (\_ g -> snd (S.addLibraryCard swamp S.alice g)) game [1 .. 3 :: Int]
            withCensus = snd (S.addCreature census S.alice stocked)
            (walls, withWalls) = List.foldl' (\(acc, g) pid -> let (oid, g') = S.addCreature wall pid g in (acc <> [oid], g')) ([], withCensus) opponents
            (_, entered) = S.entersWithTrigger morcant S.alice withWalls
        pure (walls, snd (Engine.runGamePure S.identityAnswer entered Engine.settleForPriority))
      -- Settle and resolve until the stack is empty: the Morcant's trigger puts the
      -- counters, and the Census's own trigger only reaches the stack at the CR
      -- 117.5 scan after it. Resolving the top alone would leave the card undrawn.
      resolveEverything gs =
        let settled = S.runPure S.identityAnswer gs Engine.settleForPriority
         in if null (GameState.stack settled)
              then settled
              else resolveEverything (S.runPure S.identityAnswer settled Stack.resolveTop)
   in Spec.describe s "BlightSimultaneity" $ do
        -- The proving case. bob and carol both blight, both Walls take a counter,
        -- and alice draws ONE card -- 2 is the seat-at-a-time reading and 0 is
        -- silence.
        Spec.it s "CR 101.4 two seats blighting are one event, so the Census draws one card" $ do
          (walls, board) <- censusBoard [S.bob, S.carol] S.threePlayerGame
          let after = resolveEverything board
          Spec.assertEqWith s "alice drew one card for the whole batch" (S.handSize S.alice after) 1
          Spec.assertEqWith s "both opponents' Walls took a counter" (fmap (\oid -> minusCountersOn oid after) walls) [Just 1, Just 1]
          Spec.assertEqWith s "and the two placements were one event group" (length (placementGroups after)) 1
          Spec.assertEqWith s "alice held nothing before" (S.handSize S.alice board) 0
        -- The same card with ONE blighter, which is what says the condition fires
        -- at all rather than the batch case passing because nothing triggered: one
        -- placement, one group, one card -- the reading both implementations share.
        Spec.it s "CR 603.2c one seat blighting draws one card too" $ do
          (walls, board) <- censusBoard [S.bob] (Setup.emptyGame S.bothPlayers)
          let after = resolveEverything board
          Spec.assertEqWith s "alice drew her card" (S.handSize S.alice after) 1
          Spec.assertEqWith s "bob's Wall took the counter" (fmap (\oid -> minusCountersOn oid after) walls) [Just 1]
          Spec.assertEqWith s "one placement, one group" (length (placementGroups after)) 1
        -- The control that separates "once per event GROUP" from a dedup coarser
        -- than the group -- once ever, or once per turn -- which the boards above
        -- cannot tell apart, each holding one batch. alice's Dawnhand Dissident is
        -- another Elf entering, so the Morcant already standing triggers a second
        -- time (CR 603.6a), both opponents blight again, and that second batch is
        -- an event group of its own: TWO cards, where a coarser dedup leaves the
        -- one card the first batch drew and a per-seat reading gives four.
        --
        -- What this cannot separate is per-group from per-SCAN: the two batches are
        -- two resolutions and so two CR 117.5 scans. Nothing in data/cards puts
        -- -1/-1 counters in two groups inside ONE scan; Pawl.ZoneTriggerSpec's "CR
        -- 704.3 two death groups in one trigger scan are two trigger events" is
        -- what tells those two readings apart, on the death side.
        --
        -- S.entersWithTrigger REWRITES the log, so the group count below reads the
        -- second batch alone rather than both.
        Spec.it s "CR 603.2c a second blight is a second trigger event" $ do
          dissident <- S.printingOf s registry "Dawnhand Dissident"
          (walls, board) <- censusBoard [S.bob, S.carol] S.threePlayerGame
          let after = resolveEverything board
              again = resolveEverything (snd (S.entersWithTrigger dissident S.alice after))
          Spec.assertEqWith s "alice drew a second card for the second batch" (S.handSize S.alice again) 2
          Spec.assertEqWith s "she held one after the first" (S.handSize S.alice after) 1
          Spec.assertEqWith s "both Walls took a second counter" (fmap (\oid -> minusCountersOn oid again) walls) [Just 2, Just 2]
          Spec.assertEqWith s "and the second batch was one event group" (length (placementGroups again)) 1

-- CR 603.2c's SECOND sentence, where blightSimultaneitySpec above proves its
-- first: "whenever one or more -1/-1 counters are put on A CREATURE" names each
-- creature the placements touched, so one batch spanning two of them contains two
-- occurrences and fires twice.
--
--   * Wickersmith's Tools {3} Artifact
--     (data/cards/wickersmiths-tools.json): "Whenever one or more -1/-1 counters
--     are put on a creature, put a charge counter on this artifact." Its two
--     other abilities -- "{T}: Add one mana of any color", and the {5}, {T},
--     sacrifice that makes X tapped 2/2 Scarecrows for X charge counters -- are
--     transcribed whole and go unexercised here.
--
-- (Its name, cost, type line and oracle text checked against Scryfall.)
--
-- WHY THIS PRINTING and not Auntie Ool, Cursewretch, the other card of the
-- wording: the Auntie reads "that creature" back off the trigger, and no slot
-- names it (#2342). The Tools carry no rider at all, so the charge counters on
-- them are a clean count of how many times the ability fired.
--
-- The -1/-1 COUNTERS are the same under either reading of the trigger, which is
-- why what the cases read is the count on the Tools: two for a two-creature
-- batch, where the batch reading gives one.
perCreatureCountersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
perCreatureCountersSpec s registry =
  let charge = CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "charge"))
      -- The distinct EventGroups the log's -1/-1 placements carry, blightSimultaneitySpec's
      -- copy: the precondition the two-creature case rests on, asserted rather than
      -- assumed. Were the placements not one group, "twice out of one batch" would
      -- be proving nothing.
      placementGroups gs =
        List.nub
          ( Maybe.mapMaybe
              ( \logged -> case LoggedEvent.event logged of
                  GameEvent.CountersPut change
                    | CounterChange.kind change == CounterKind.MinusOneMinusOne -> Just (LoggedEvent.group logged)
                  _ -> Nothing
              )
              (Foldable.toList (GameState.events gs))
          )
      -- alice's Wickersmith's Tools and her High Perfect Morcant entering with its
      -- CR 603.6a trigger settled onto the stack, plus ONE Wall of Stone for each
      -- opponent. A single candidate per seat, so rule 701.68a's choice raises no
      -- prompt and nothing about the answerer is load-bearing here.
      --
      -- A Wall of Stone is an 0/8, so a -1/-1 counter is nowhere near CR 704.5f and
      -- every blighted creature is still standing when the counters are read.
      toolsBoard opponents game = do
        tools <- S.printingOf s registry "Wickersmith's Tools"
        morcant <- S.printingOf s registry "High Perfect Morcant"
        wall <- S.printingOf s registry "Wall of Stone"
        let (toolsId, withTools) = S.addCreature tools S.alice game
            (walls, withWalls) = List.foldl' (\(acc, g) pid -> let (oid, g') = S.addCreature wall pid g in (acc <> [oid], g')) ([], withTools) opponents
            (_, entered) = S.entersWithTrigger morcant S.alice withWalls
        pure (toolsId, walls, snd (Engine.runGamePure S.identityAnswer entered Engine.settleForPriority))
      -- Settle and resolve until the stack is empty, blightSimultaneitySpec's copy:
      -- the Morcant's trigger puts the counters, and the Tools' own triggers only
      -- reach the stack at the CR 117.5 scan after it.
      resolveEverything gs =
        let settled = S.runPure S.identityAnswer gs Engine.settleForPriority
         in if null (GameState.stack settled)
              then settled
              else resolveEverything (S.runPure S.identityAnswer settled Stack.resolveTop)
   in Spec.describe s "PerCreatureCounters" $ do
        -- The proving case, and the one the batch reading gets wrong: bob and carol
        -- both blight, one batch touches two creatures, and the ability fires ONCE
        -- PER CREATURE. 1 is the batch reading and 0 is silence.
        Spec.it s "CR 603.2c a batch touching two creatures fires the per-creature trigger twice" $ do
          (toolsId, walls, board) <- toolsBoard [S.bob, S.carol] S.threePlayerGame
          let after = resolveEverything board
          Spec.assertEqWith s "the Tools took a charge counter for each creature the batch touched" (S.counterOf charge toolsId after) 2
          Spec.assertEqWith s "both opponents' Walls took a -1/-1 counter" (fmap (\oid -> minusCountersOn oid after) walls) [Just 1, Just 1]
          Spec.assertEqWith s "and the two placements were one event group" (length (placementGroups after)) 1
          Spec.assertEqWith s "the Tools carried no charge counter before" (S.counterOf charge toolsId board) 0
        -- The same card with ONE blighter, which is the reading both implementations
        -- share: one creature, one trigger. What it rules out is a Tools that fires
        -- per SEAT or per batch member regardless of the count -- and it is why the
        -- case above's 2 cannot be read as "fires twice whatever happened".
        Spec.it s "CR 603.2c one creature in the batch fires it once" $ do
          (toolsId, walls, board) <- toolsBoard [S.bob] (Setup.emptyGame S.bothPlayers)
          let after = resolveEverything board
          Spec.assertEqWith s "the Tools took one charge counter" (S.counterOf charge toolsId after) 1
          Spec.assertEqWith s "bob's Wall took the counter" (fmap (\oid -> minusCountersOn oid after) walls) [Just 1]
          Spec.assertEqWith s "one placement, one group" (length (placementGroups after)) 1
        -- The other half of "ONE OR MORE counters ... on a creature": two counters
        -- landing on ONE creature at once is one placement and so one trigger, where
        -- a per-COUNTER reading would fire twice. Dawnhand Dissident's second
        -- ability is the producer -- "{T}, Blight 2: Exile target card from a
        -- graveyard" -- and rule 701.68a's blight 2 puts both counters on a single
        -- creature in one go.
        --
        -- The blight is a COST (CR 602.1b), so the counters are on the board with
        -- the exile still on the stack; resolveEverything below then lets the Tools'
        -- trigger reach the stack and resolve.
        Spec.it s "CR 603.2c two counters on one creature at once is one trigger" $ do
          tools <- S.printingOf s registry "Wickersmith's Tools"
          dissident <- S.printingOf s registry "Dawnhand Dissident"
          wall <- S.printingOf s registry "Wall of Stone"
          swamp <- S.printingOf s registry "Swamp"
          let (toolsId, g1) = S.addCreature tools S.alice (Setup.emptyGame S.bothPlayers)
              (dissidentId, g2) = S.addCreature dissident S.alice g1
              (wallId, g3) = S.addCreature wall S.alice g2
              (_, g4) = S.addGraveyardCard swamp S.bob g3
              board = g4 {GameState.priority = Just S.alice}
          activated <- case Projection.abilitiesOf dissidentId board of
            _ : exile : _ -> do
              Spec.assertBool s (Activate.activatable S.alice dissidentId exile board) "the blight 2 ability is activatable"
              pure (S.runPure (blighting wallId) board (Activate.activateAbility S.alice dissidentId exile))
            _ -> Spec.assertFailure s "expected the Dissident to carry two activated abilities"
          let after = resolveEverything activated
          Spec.assertEqWith s "the Tools took ONE charge counter for the two counters that landed together" (S.counterOf charge toolsId after) 1
          Spec.assertEqWith s "the Wall took both -1/-1 counters" (minusCountersOn wallId after) (Just 2)
          Spec.assertEqWith s "in one placement, so one event group" (length (placementGroups after)) 1

-- CR 701.68 blight as a COST (CostComponent.Blight), which is the position most of
-- the pool prints it in and the one CR 701.68b's "they can't choose to blight"
-- decides. Two cards, one for each of the two moments a cost is paid:
--
--   * Dawnhand Dissident {B} 1/2 Creature -- Elf Warlock
--     (data/cards/dawnhand-dissident.json): "{T}, Blight 1: Surveil 1." CR 602.1b
--     and CR 601.2h -- paid as the ability is ACTIVATED, which is what the first
--     case reads off a board whose stack has not resolved yet. (A third printed
--     ability, casting creature spells exiled with it by removing counters, is not
--     transcribed -- gap #1648. Omitting a permission leaves pawl's card STRICTER
--     than printed.)
--
--   * Boggart Mischief {2}{B} Kindred Enchantment -- Goblin
--     (data/cards/boggart-mischief.json): "When this enchantment enters, you may
--     blight 1. If you do, create two 1/1 black and red Goblin creature tokens."
--     CR 118.12's "[A player] may [do something]. If [that player] does, [effect]"
--     -- a cost paid on RESOLUTION, so rule 701.68b's refusal arrives as CR 118.3's
--     unpayable cost and the offer is never made.
--
-- (Both cards' names, costs, type lines, P/T and oracle text checked against
-- Scryfall.)
--
-- WHY BOGGART MISCHIEF and not one of the six creatures printing the same "you may
-- blight": an ENCHANTMENT can enter while its controller controls no creature, and
-- a creature cannot. The negative board below would be unreachable with any of
-- them.
--
-- The negative is the positive board with the two creatures moved to bob's seat --
-- same printings, same lands, same stock, same trigger, and bob's Typhoid Rats
-- sitting on the battlefield either way, so "controls no creature" is what differs
-- and not "no creature exists".
blightCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blightCostSpec s registry = Spec.describe s "BlightCost" $ do
  let mischiefBoard pid = do
        swamp <- S.printingOf s registry "Swamp"
        mischief <- S.printingOf s registry "Boggart Mischief"
        piker <- S.printingOf s registry "Goblin Piker"
        wall <- S.printingOf s registry "Wall of Stone"
        rats <- S.printingOf s registry "Typhoid Rats"
        pure (mischiefEnters swamp mischief piker wall rats pid)
      dissidentBoard pid = do
        swamp <- S.printingOf s registry "Swamp"
        dissident <- S.printingOf s registry "Dawnhand Dissident"
        piker <- S.printingOf s registry "Goblin Piker"
        wall <- S.printingOf s registry "Wall of Stone"
        pure (dissidentOnBoard swamp dissident piker wall pid)
  -- CR 602.1b / CR 601.2h: the counters are on the board with the ability still
  -- ON THE STACK. No effect-position blight can pass this case -- an
  -- Effect.Blight puts its counters as the ability RESOLVES -- and the untouched
  -- library beside it is what says the resolution has not happened.
  Spec.it s "CR 601.2h whole card: Dawnhand Dissident's blight is paid as the ability is activated" $ do
    (dissidentId, pikerId, wallId, board) <- dissidentBoard S.alice
    activated <- activateBlighting s wallId dissidentId board
    Spec.assertEqWith s "the Wall, whom alice named, took the counter" (minusCountersOn wallId activated) (Just 1)
    Spec.assertEqWith s "the Piker took none" (minusCountersOn pikerId activated) (Just 0)
    Spec.assertEqWith s "nor did the Dissident itself" (minusCountersOn dissidentId activated) (Just 0)
    Spec.assertEqWith s "the {T} beside it was paid too" (fmap Object.tapped (Game.lookupObject dissidentId activated)) (Just TapState.Tapped)
    Spec.assertEqWith s "and the ability is still on the stack: nothing has resolved" (length (GameState.stack activated)) 1
    Spec.assertEqWith s "so the surveil has not happened -- both cards are still in the library" (length (Game.zoneMembers Zone.Library S.alice activated)) 2
  -- The same board and the same activation, differing only in the answer: the
  -- engine makes no choice, so the Dissident is as reachable as the Wall.
  Spec.it s "CR 701.68a the same activation answered another way counters that creature" $ do
    (dissidentId, pikerId, wallId, board) <- dissidentBoard S.alice
    activated <- activateBlighting s dissidentId dissidentId board
    Spec.assertEqWith s "the Dissident blighted itself" (minusCountersOn dissidentId activated) (Just 1)
    Spec.assertEqWith s "the Wall took none" (minusCountersOn wallId activated) (Just 0)
    Spec.assertEqWith s "nor did the Piker" (minusCountersOn pikerId activated) (Just 0)
  -- CR 701.68a's "a creature YOU CONTROL", read through the cost. The same four
  -- printings with the other two creatures on bob's seat, and the answer names
  -- bob's Piker on both boards: it is never offered, so the Dissident -- alice's
  -- only creature, and therefore no choice at all -- pays.
  Spec.it s "CR 701.68a an opponent's creature cannot pay your blight cost" $ do
    (dissidentId, pikerId, wallId, board) <- dissidentBoard S.bob
    activated <- activateBlighting s pikerId dissidentId board
    Spec.assertEqWith s "the Dissident, alice's only creature, took the counter" (minusCountersOn dissidentId activated) (Just 1)
    Spec.assertEqWith s "bob's Piker, whom the answer named, took none" (minusCountersOn pikerId activated) (Just 0)
    Spec.assertEqWith s "nor did his Wall" (minusCountersOn wallId activated) (Just 0)
  -- CR 118.12's positive branch over a blight: paying it makes the tokens.
  Spec.it s "CR 118.12 whole card: paying Boggart Mischief's blight creates the two Goblins" $ do
    (pikerId, wallId, ratsId, onStack) <- mischiefBoard S.alice
    let ((_, after), transcript) = Replay.record (paysBlighting wallId) onStack Stack.resolveTop
    Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    Spec.assertEqWith s "the Wall, whom she named, took the counter" (minusCountersOn wallId after) (Just 1)
    Spec.assertEqWith s "her Piker took none" (minusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor did bob's Rats" (minusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "and the two Goblins arrived" (length (S.tokensOf after)) 2
  -- The same board and the same trigger, differing in NOTHING but the answer.
  -- alice COULD pay, so the empty board below is her refusal rather than CR 118.3's.
  Spec.it s "CR 118.12 declining Boggart Mischief's blight makes no token and no counter" $ do
    (pikerId, wallId, _, onStack) <- mischiefBoard S.alice
    let ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
    Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    Spec.assertEqWith s "the Wall took nothing" (minusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "nor did the Piker" (minusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "and no token was created" (length (S.tokensOf after)) 0
  -- CR 701.68b, the whole point of the pair: a player who controls no creature
  -- "can't choose to blight", so the option is NOT OFFERED rather than offered and
  -- declined. Proved by the transcript under the interpreter that WOULD have paid,
  -- on a board that still holds a creature -- bob's Rats -- so nothing here passes
  -- for want of a creature on the battlefield.
  Spec.it s "CR 701.68b whole card: a controller with no creature is never offered the blight" $ do
    (pikerId, wallId, ratsId, onStack) <- mischiefBoard S.bob
    let ((_, after), transcript) = Replay.record (paysBlighting ratsId) onStack Stack.resolveTop
    Spec.assertEqWith s "alice was never asked" (payResponses transcript) []
    Spec.assertEqWith s "bob's Rats, whom the answer named, took nothing" (minusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "nor did his Piker" (minusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor his Wall" (minusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "and no token was created" (length (S.tokensOf after)) 0

-- Boggart Mischief cast from alice's hand off three Swamps and resolved, with its
-- CR 603.6a enters trigger settled onto the stack but NOT resolved --
-- kinGuardOnStack's shape. Goblin Piker and Wall of Stone go to `pid`; Typhoid
-- Rats always to bob. Returns the three creatures and that state.
--
-- The seat is the ONLY parameter, so the two boards below are the same five
-- printings, the same three Swamps and the same trigger.
--
-- Toughnesses 1, 8 and 1 across the three, and the two that can be candidates are
-- 1 and 8, so which creature took a counter is readable off the board rather than
-- inferred.
mischiefEnters ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
mischiefEnters swamp mischief piker wall rats pid =
  let (pikerId, g1) = S.addCreature piker pid (S.landsInPlay swamp 3)
      (wallId, g2) = S.addCreature wall pid g1
      (ratsId, g3) = S.addCreature rats S.bob g2
      (g4, spellId) = S.handOne mischief g3
      cast = S.runPure S.identityAnswer g4 (S.cast S.alice spellId)
   in (pikerId, wallId, ratsId, S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority))

-- Dawnhand Dissident on alice's battlefield, Goblin Piker and Wall of Stone on
-- `pid`'s, and two Swamps in alice's library for the surveil to move. Returns the
-- three creatures and that state.
--
-- Two library cards, so the surveil neither empties the library (CR 104.3c) nor
-- leaves "the resolution has not happened" and "there was nothing to surveil"
-- looking alike.
dissidentOnBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
dissidentOnBoard swamp dissident piker wall pid =
  let (dissidentId, g1) = S.addCreature dissident S.alice (Setup.emptyGame S.bothPlayers)
      (pikerId, g2) = S.addCreature piker pid g1
      (wallId, g3) = S.addCreature wall pid g2
      (_, g4) = S.addLibraryCard swamp S.alice g3
      (_, g5) = S.addLibraryCard swamp S.alice g4
   in (dissidentId, pikerId, wallId, g5 {GameState.priority = Just S.alice})

-- alice activates the Dissident's FIRST projected ability -- "{T}, Blight 1:
-- Surveil 1" -- naming `wanted` for the blight, and the stack is left alone. The
-- Activate.activatable assertion is what keeps the case honest: without it, a
-- board where the ability could not legally have been activated at all would still
-- show the counters, since Activate.activateAbility trusts its caller.
activateBlighting ::
  (Monad m) =>
  Spec.Spec m n ->
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  m GameState.GameState
activateBlighting s wanted oid gs = case Projection.abilitiesOf oid gs of
  surveil : _ -> do
    Spec.assertBool s (Activate.activatable S.alice oid surveil gs) "the ability is activatable"
    pure (S.runPure (blighting wanted) gs (Activate.activateAbility S.alice oid surveil))
  [] -> Spec.assertFailure s "expected the permanent to carry an activated ability"

-- Answers Prompt.ChooseBlight with a named creature and Prompt.ChooseToPay with
-- Pays, deferring everything else to S.identityAnswer. Both halves are PINNED, so
-- a mutation cannot be repaired by an answerer that goes looking for a legal
-- option.
paysBlighting :: ObjectId.ObjectId -> Prompt.Prompt r -> r
paysBlighting oid p = case p of
  Prompt.ChooseBlight {} -> oid
  _ -> paysFor S.alice p

-- Answers Prompt.ChooseAmass with a named Army, deferring everything else to
-- S.identityAnswer. PINNED BY ID rather than picked by searching the candidates, so
-- a mutation to the candidate sweep cannot quietly repair the answer.
amassing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
amassing oid p = case p of
  Prompt.ChooseAmass {} -> oid
  _ -> S.identityAnswer p

-- CR 701.41 support, which is card DATA and no opcode: "support N" is written out
-- as the counters it means, over a CR 601.2c slot of 0 to N.
--
-- N is a LITERAL range here. A count that reads X -- The Crowd Goes Wild's
-- "Support X" -- is not expressible (#1271).
--
-- Lead by Example {1}{G} Instant (data/cards/lead-by-example.json): "Support 2.",
-- and nothing else -- CR 701.41a's INSTANT reading, which has no "other" in it.
--
-- Joraga Auxiliary {1}{G}{W} 2/3 (data/cards/joraga-auxiliary.json):
-- "{4}{G}{W}: Support 2.", CR 701.41a's PERMANENT reading, whose "other" is a
-- Not IsSource on the slot.
--
-- Three readings of "up to two target creatures" a careless board cannot tell
-- apart -- two of three, one of three, and none -- so each case names a different
-- number of targets on the same board and the creature nobody named is asserted
-- untouched. Three candidates for a slot that takes two, because a prompt offered
-- exactly as many candidates as it needs is never asked.
supportSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
supportSpec s registry = Spec.describe s "Support" $ do
  Spec.it s "CR 701.41a support 2 counters each of the two creatures it named" $ do
    (pikerId, wallId, ratsId, gs, spellId) <- leadBoard s registry []
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        after = resolveOne answer gs spellId
    Spec.assertEqWith s "the Piker took a counter" (plusCountersOn pikerId after) (Just 1)
    Spec.assertEqWith s "so did the Wall" (plusCountersOn wallId after) (Just 1)
    Spec.assertEqWith s "the Rats, whom nobody named, took none" (plusCountersOn ratsId after) (Just 0)
  -- The same board and the same spell, differing only in the announced number:
  -- "up to two" allows one, and one is not two.
  Spec.it s "CR 601.2c support 2 announcing one target counters only that one" $ do
    (pikerId, wallId, ratsId, gs, spellId) <- leadBoard s registry []
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 1 [pikerId, wallId]
        after = resolveOne answer gs spellId
    Spec.assertEqWith s "the Piker took a counter" (plusCountersOn pikerId after) (Just 1)
    Spec.assertEqWith s "and the second creature it could have taken took none" (plusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "nor did the Rats" (plusCountersOn ratsId after) (Just 0)
  -- CR 115.6's zero. Lead by Example has no second clause, so what makes the
  -- declined case observable is that no counter appears anywhere: an engine that
  -- chose the targets itself would put two.
  Spec.it s "CR 115.6 support 2 announcing no targets counters nobody" $ do
    (pikerId, wallId, ratsId, gs, spellId) <- leadBoard s registry []
    let after = resolveOne decliningTargets gs spellId
    Spec.assertEqWith s "the Piker took none" (plusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor the Wall" (plusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "nor the Rats" (plusCountersOn ratsId after) (Just 0)
  -- CR 122.6 / 614.16: each of support's placements reaches the funnel on its own,
  -- so a counter-scaling replacement gets an opportunity against every target
  -- rather than one against the batch. Doubling Season reads whose PERMANENT it is,
  -- which is why both targets here are alice's.
  Spec.it s "CR 122.6 Doubling Season doubles support's counter on each target" $ do
    (pikerId, wallId, _, seasoned, seasonedSpell) <- leadBoard s registry [("Doubling Season", S.alice)]
    (barePiker, bareWall, _, bare, bareSpell) <- leadBoard s registry []
    let seasonedAfter = resolveOne (takingTargets 2 [pikerId, wallId]) seasoned seasonedSpell
        bareAfter = resolveOne (takingTargets 2 [barePiker, bareWall]) bare bareSpell
    Spec.assertEqWith s "1 * 2 on the Piker" (plusCountersOn pikerId seasonedAfter) (Just 2)
    Spec.assertEqWith s "1 * 2 on the Wall too" (plusCountersOn wallId seasonedAfter) (Just 2)
    Spec.assertEqWith s "and one each without the enchantment" (plusCountersOn barePiker bareAfter, plusCountersOn bareWall bareAfter) (Just 1, Just 1)
  -- The same funnel from the other side: half of one counter, rounded down, is
  -- none, so zero, one and two are three distinct answers to the same board.
  -- Vorinclex reads who is PUTTING the counters (CR 122.6a), and the targets here
  -- are alice's own permanents -- which is what separates it from Doubling Season's
  -- recipient reading, since bob's praetor halves them anyway.
  Spec.it s "CR 122.6a an opponent's Vorinclex halves support's counters away" $ do
    (pikerId, wallId, _, watched, watchedSpell) <- leadBoard s registry [("Vorinclex, Monstrous Raider", S.bob)]
    (barePiker, bareWall, _, bare, bareSpell) <- leadBoard s registry []
    let watchedAfter = resolveOne (takingTargets 2 [pikerId, wallId]) watched watchedSpell
        bareAfter = resolveOne (takingTargets 2 [barePiker, bareWall]) bare bareSpell
    Spec.assertEqWith s "half of one on the Piker" (plusCountersOn pikerId watchedAfter) (Just 0)
    Spec.assertEqWith s "half of one on the Wall" (plusCountersOn wallId watchedAfter) (Just 0)
    Spec.assertEqWith s "and one each without the praetor" (plusCountersOn barePiker bareAfter, plusCountersOn bareWall bareAfter) (Just 1, Just 1)
  -- CR 701.41a's "other", which only the PERMANENT reading has. The answerer names
  -- the Auxiliary FIRST, so a slot that offered it would spend one of its two
  -- targets on it and leave one of the Rats and the Piker at zero.
  Spec.it s "CR 701.41a support on a permanent cannot choose that permanent" $ do
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    rats <- S.printingOf s registry "Typhoid Rats"
    wall <- S.printingOf s registry "Wall of Stone"
    joraga <- S.printingOf s registry "Joraga Auxiliary"
    let (_, g0) = S.addCreature plains S.alice (S.landsInPlay forest 5)
        (auxId, g1) = S.addCreature joraga S.alice g0
        (pikerId, g2) = S.addCreature piker S.bob g1
        (ratsId, g3) = S.addCreature rats S.bob g2
        (wallId, g4) = S.addCreature wall S.bob g3
        board = g4 {GameState.priority = Just S.alice}
        answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [auxId, pikerId, ratsId]
    case Activate.abilitiesFor auxId board of
      [ability] -> do
        let after = S.runPure answer board (Activate.activateAbility S.alice auxId ability >> Stack.resolveTop)
        Spec.assertEqWith s "the Auxiliary itself, which support excludes, took none" (plusCountersOn auxId after) (Just 0)
        Spec.assertEqWith s "the Piker took one" (plusCountersOn pikerId after) (Just 1)
        Spec.assertEqWith s "and so did the Rats" (plusCountersOn ratsId after) (Just 1)
        Spec.assertEqWith s "the Wall, whom nobody named, took none" (plusCountersOn wallId after) (Just 0)
      abilities -> Spec.assertFailure s ("expected one ability, got " <> show (length abilities))

-- Two Forests for Lead by Example, three creatures with three distinct printed
-- boxes (2/1, 0/8, 1/1) so which of them took a counter is legible, and the spell
-- in alice's hand. The first two are alice's, since Doubling Season's clause reads
-- whose permanent takes the counter; the Rats are bob's. `extra` seats further
-- printings by name, which is the only difference between a case and its control.
leadBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [(String, PlayerId.PlayerId)] ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
leadBoard s registry extra = do
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  lead <- S.printingOf s registry "Lead by Example"
  extras <- mapM (\(name, pid) -> fmap (\p -> (p, pid)) (S.printingOf s registry name)) extra
  let (pikerId, g1) = S.addCreature piker S.alice (S.landsInPlay forest 2)
      (wallId, g2) = S.addCreature wall S.alice g1
      (ratsId, g3) = S.addCreature rats S.bob g2
      g4 = List.foldl' (\g (p, pid) -> snd (S.addCreature p pid g)) g3 extras
      (gs, spellId) = S.handOne lead g4
  pure (pikerId, wallId, ratsId, gs, spellId)

-- CR 701.39 bolster, which is an opcode: Effect.Bolster over a Quantity, whose
-- candidate pool and counter kind are rule 701.39a's rather than the card's.
--
-- Cached Defenses {2}{G} Sorcery (data/cards/cached-defenses.json): "Bolster 3.",
-- and nothing else -- so every counter that appears on these boards came from this
-- keyword action and nothing else on the card can stand in for it.
--
-- Two readings of "the least toughness ... or tied for least" a careless board
-- cannot tell apart -- the engine picking for the player, and the player picking
-- -- so the two boards below differ in exactly one thing each. The tie is
-- deliberate: a lone creature at the minimum is never asked about, and three
-- creatures at 1, 1 and 8 make "which of the two" and "not the third" separate
-- questions.
--
-- Three is bolster's own N, and it is distinct from every toughness on the board,
-- so no assertion can be satisfied by a coincidence.
bolsterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bolsterSpec s registry = Spec.describe s "Bolster" $ do
  Spec.it s "CR 701.39a bolster 3 counters the creature its controller chose" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- bolsterBoard s registry
    let after = resolveOne (bolstering ratsId) gs spellId
    Spec.assertEqWith s "the Rats, whom their controller named, took three" (plusCountersOn ratsId after) (Just 3)
    Spec.assertEqWith s "the Piker, tied with them, took none" (plusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor did the Wall" (plusCountersOn wallId after) (Just 0)
  -- The same board and the same spell, differing only in the answer: the engine
  -- makes no choice, so the other half of the tie is equally reachable.
  Spec.it s "CR 701.39a the same tie answered the other way counters the other creature" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- bolsterBoard s registry
    let after = resolveOne (bolstering pikerId) gs spellId
    Spec.assertEqWith s "the Piker, whom their controller named, took three" (plusCountersOn pikerId after) (Just 3)
    Spec.assertEqWith s "the Rats took none" (plusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "nor did the Wall" (plusCountersOn wallId after) (Just 0)
  -- "With the least toughness" is a filter on the candidates rather than advice:
  -- the Wall is named and still gets nothing, because it was never offered.
  Spec.it s "CR 701.39a a creature that is not tied for least toughness cannot be chosen" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- bolsterBoard s registry
    let after = resolveOne (bolstering wallId) gs spellId
    Spec.assertEqWith s "the Wall, at toughness 8, took none" (plusCountersOn wallId after) (Just 0)
    -- One of the tied pair took all three, and the fallback picks which; what is
    -- under test is that the counters did not follow the answer.
    Spec.assertEqWith
      s
      "the tie took them instead"
      (fmap (+) (plusCountersOn pikerId after) <*> plusCountersOn ratsId after)
      (Just 3)
  -- CR 701.39a's "among creatures you control". The two smallest creatures on the
  -- battlefield are bob's, and neither is a candidate: alice's own Wall is the
  -- whole of her pool however large it is.
  Spec.it s "CR 701.39a bolster looks only at creatures its controller controls" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- opposedBolsterBoard s registry
    let after = resolveOne S.identityAnswer gs spellId
    Spec.assertEqWith s "alice's Wall, her only creature, took three" (plusCountersOn wallId after) (Just 3)
    Spec.assertEqWith s "bob's Piker, at toughness 1, took none" (plusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor did bob's Rats" (plusCountersOn ratsId after) (Just 0)
  -- Where the rules leave nothing to ask, do not ask. The two boards differ in
  -- whether the least toughness is TIED, which is the whole of what makes rule
  -- 701.39a's choice a choice.
  Spec.it s "CR 701.39a a lone creature at the least toughness raises no prompt" $ do
    (_, _, _, tied, tiedSpell) <- bolsterBoard s registry
    (_, _, _, alone, aloneSpell) <- opposedBolsterBoard s registry
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseBolster {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks g spellId =
          State.execState (Engine.runGame countingAnswer g (S.cast S.alice spellId >> Stack.resolveTop)) 0
    Spec.assertEqWith s "one creature at the minimum: nothing to ask" (asks alone aloneSpell) 0
    Spec.assertEqWith s "two tied for it: one real decision" (asks tied tiedSpell) 1

-- Three Forests for Cached Defenses, and three of alice's creatures whose printed
-- toughnesses are 1, 1 and 8 -- a TIE at the least, and a third creature well
-- clear of it -- with the spell in her hand.
bolsterBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
bolsterBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  defenses <- S.printingOf s registry "Cached Defenses"
  let (pikerId, g1) = S.addCreature piker S.alice (S.landsInPlay forest 3)
      (ratsId, g2) = S.addCreature rats S.alice g1
      (wallId, g3) = S.addCreature wall S.alice g2
      (gs, spellId) = S.handOne defenses g3
  pure (pikerId, ratsId, wallId, gs, spellId)

-- bolsterBoard with the tied pair moved across the table: the two creatures at
-- toughness 1 are BOB's, so alice's pool is her Wall alone. The same three
-- printings, the same three lands and the same spell -- only the seats differ.
opposedBolsterBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
opposedBolsterBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  defenses <- S.printingOf s registry "Cached Defenses"
  let (pikerId, g1) = S.addCreature piker S.bob (S.landsInPlay forest 3)
      (ratsId, g2) = S.addCreature rats S.bob g1
      (wallId, g3) = S.addCreature wall S.alice g2
      (gs, spellId) = S.handOne defenses g3
  pure (pikerId, ratsId, wallId, gs, spellId)

-- Answers Prompt.ChooseBolster with a named creature, deferring everything else to
-- S.identityAnswer. PINNED BY ID rather than picked by searching the candidates,
-- so a mutation to the candidate sweep cannot quietly repair the answer.
bolstering :: ObjectId.ObjectId -> Prompt.Prompt r -> r
bolstering oid p = case p of
  Prompt.ChooseBolster {} -> oid
  _ -> S.identityAnswer p

-- CR 115.6: declines every optional slot, announcing zero targets. Everything
-- else is S.identityAnswer's answer, which for ChooseTargets fills what it is
-- offered -- so the two answerers differ in exactly one decision.
decliningTargets :: Prompt.Prompt r -> r
decliningTargets p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const 0) offers
  _ -> S.identityAnswer p

-- Announces one named slot and declines the rest.
announcingOnly :: SlotName.SlotName -> Prompt.Prompt r -> r
announcingOnly slot p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> Map.mapWithKey (\name (count, legal) -> if name == slot then TargetCount.ceilingOn (Natural.length legal) count else 0) offers
  _ -> S.identityAnswer p

-- CR 115.6's "up to one target", read at resolution.
--
-- Rat Out {B} Instant (data/cards/rat-out.json): "Up to one target creature gets
-- -1/-1 until end of turn. You create a 1/1 black Rat creature token with 'This
-- token can't block.'" The Rat is what makes the zero-target case OBSERVABLE: a
-- spell that fizzled under CR 608.2b would make no token, and the card's own
-- graveyard trip is the same either way.
--
-- Explosive Entry {1}{R} Sorcery (data/cards/explosive-entry.json): "Destroy up
-- to one target artifact. Put a +1/+1 counter on up to one target creature." Two
-- independently optional slots, so one can be taken while the other is declined.
upToOneTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
upToOneTargetSpec s registry = Spec.describe s "UpToOneTarget" $ do
  Spec.it s "CR 115.6 Rat Out aimed at a creature shrinks it and still makes the Rat" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ratOut <- S.printingOf s registry "Rat Out"
    let (victim, board) = S.addCreature piker S.bob (S.landsInPlay swamp 1)
        (gs, spellId) = S.handOne ratOut board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "power 1" (Projection.powerOf victim after) (Just 1)
    Spec.assertEqWith s "toughness 0" (Projection.toughnessOf victim after) (Just 0)
    Spec.assertEqWith s "one Rat" (length (S.tokensOf after)) 1
  -- The same board and the same spell, differing only in the CR 601.2c
  -- announcement: zero targets rather than one.
  Spec.it s "CR 115.6 Rat Out with zero targets announced resolves, leaving the creature alone" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ratOut <- S.printingOf s registry "Rat Out"
    let (victim, board) = S.addCreature piker S.bob (S.landsInPlay swamp 1)
        (gs, spellId) = S.handOne ratOut board
        cast = snd (Engine.runGamePure decliningTargets gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure decliningTargets cast Stack.resolveTop)
    Spec.assertEqWith s "still 2/1" (Projection.powerOf victim after) (Just 2)
    Spec.assertEqWith s "still 2/1" (Projection.toughnessOf victim after) (Just 1)
    -- CR 608.2b does not fizzle a spell that chose no targets at all.
    Spec.assertEqWith s "the Rat was still made" (length (S.tokensOf after)) 1
  Spec.it s "CR 115.6 Explosive Entry takes one slot and declines the other" $ do
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    piker <- S.printingOf s registry "Goblin Piker"
    explosiveEntry <- S.printingOf s registry "Explosive Entry"
    let (equipment, withArtifact) = S.addCreature bonesplitter S.bob (S.landsInPlay mountain 2)
        (creature, board) = S.addCreature piker S.bob withArtifact
        (gs, spellId) = S.handOne explosiveEntry board
        answer = announcingOnly (SlotName.MkSlotName (Text.pack "artifact"))
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertBool s (not (Set.member equipment (GameState.battlefield after))) "the artifact was destroyed"
    Spec.assertEqWith s "the creature got no counter" (Projection.powerOf creature after) (Just 2)
  Spec.it s "CR 115.6 Explosive Entry takes both slots" $ do
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    piker <- S.printingOf s registry "Goblin Piker"
    explosiveEntry <- S.printingOf s registry "Explosive Entry"
    let (equipment, withArtifact) = S.addCreature bonesplitter S.bob (S.landsInPlay mountain 2)
        (creature, board) = S.addCreature piker S.bob withArtifact
        (gs, spellId) = S.handOne explosiveEntry board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertBool s (not (Set.member equipment (GameState.battlefield after))) "the artifact was destroyed"
    Spec.assertEqWith s "and the creature got its counter" (Projection.powerOf creature after) (Just 3)
  -- The same rule on an ACTIVATED ability (CR 602.2b routes it through CR
  -- 601.2c), where Resolve.resolveModes rather than Resolve.targetsAllIllegal
  -- asks CR 608.2b's question. Conjurer's Bauble {1} Artifact: "{T}, Sacrifice
  -- this artifact: Put up to one target card from your graveyard on the bottom of
  -- your library. Draw a card." The draw is the observer, and the sacrifice is a
  -- COST, so it is paid either way and cannot stand in for one.
  Spec.it s "CR 115.6 Conjurer's Bauble with zero targets announced still draws" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    bauble <- S.printingOf s registry "Conjurer's Bauble"
    let (baubleId, placed) = S.addCreature bauble S.alice (S.landsInPlay plains 1)
        (_, buried) = S.addGraveyardCard piker S.alice placed
        board = (stockLibrary piker S.alice 5 buried) {GameState.priority = Just S.alice}
        activate :: (forall r. Prompt.Prompt r -> r) -> ActivatedAbility.ActivatedAbility Card.Type.Card -> GameState.GameState
        activate answer ability = S.runPure answer board $ do
          Activate.activateAbility S.alice baubleId ability
          Stack.resolveTop
        graveyardSize gs = Seq.length (Map.findWithDefault Seq.empty S.alice (GameState.graveyard gs))
    case Activate.abilitiesFor baubleId board of
      [ability] -> do
        let declined = activate decliningTargets ability
            taken = activate S.identityAnswer ability
        Spec.assertEqWith s "declining still draws" (S.handSize S.alice declined) 1
        -- The Bauble's own sacrifice, and the Piker still where it was.
        Spec.assertEqWith s "the graveyard kept its card and gained the Bauble" (graveyardSize declined) 2
        Spec.assertEqWith s "taking the target draws too" (S.handSize S.alice taken) 1
        Spec.assertEqWith s "and the Piker went to the library" (graveyardSize taken) 1
      abilities -> Spec.assertFailure s ("expected one ability, got " <> show (length abilities))

-- CR 608.2f's per-object BODY, and the per-iteration binding that makes it more
-- than a repeated opcode.
--
-- Soulfire Eruption {6}{R}{R}{R} Sorcery (data/cards/soulfire-eruption.json) --
-- "Choose any number of target creatures, planeswalkers, and/or players. For
-- each of them, exile the top card of your library, then Soulfire Eruption deals
-- damage equal to that card's mana value to that permanent or player. You may
-- play the exiled cards until the end of your next turn." (name, cost, type line
-- and Oracle text checked against api.scryfall.com.) It is rule 608.2f's own
-- second example.
--
-- NO DEPARTURES FROM THE PRINTED CARD are left: "any number of target" is the
-- unbounded count, and the permission carries the printed duration. How long
-- that duration lasts is Pawl.ExpirySpec's question, not this group's.
--
-- The THREE-seat board tells apart every wrong reading of the loop:
--
--   * A BODY PER MEMBER, not one body. Three victims are named, so a loop that
--     ran the body once damages one seat and leaves two at twenty.
--   * A FRESH BINDING PER MEMBER, not one shared. The three cards exiled have
--     mana values 1, 2 and 4 -- pairwise distinct AND pairwise-sum distinct, so
--     no two readings land on one number -- and each victim's damage must be its
--     OWN card's. A loop that bound the first exiled card, or the last, for
--     every victim gives all three seats the same damage.
--   * A DEPLETING RESOURCE, which is the whole reason this shape needs an
--     opcode. Each iteration reads "the top card of your library" AFTER the
--     previous one exiled its own, so the three cards must be three DIFFERENT
--     cards, and the two under them must still be in the library in order (CR
--     401.2). A body re-reading the pre-loop board would exile one card three
--     times.
--   * APNAP (CR 608.2f's primary determination). alice is the active player and
--     the seating is [alice, bob, carol], so the top card goes to alice, the
--     next to bob and the third to carol -- reversing or permuting the order
--     permutes the three life totals, which are distinct.
--   * A PER-ITERATION GRANT. Every exiled card carries CR 601.3's play
--     permission, so the third body instruction ran for each member rather than
--     once for whichever card was bound last.
--
-- Ogre Sentry sits under bob so the target pool always holds one more candidate
-- than the announcement takes: the choice is a real choice rather than one
-- short-circuited by having exactly as many candidates as it needs, and a loop
-- that swept the battlefield instead of the slot would reach it.
--
-- The FOUR-SEAT case is CR 601.2c's "any number" proper: the card prints no
-- maximum, so four is announceable on a board that offers five candidates, and
-- nothing but the board bounds it. The ZERO case is the other end of that same
-- range.
soulfireEruptionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulfireEruptionSpec s registry =
  let -- alice casts off nine Mountains at `seats`, announcing `n` targets and
      -- aiming them at the players; the priority loop resolves the spell, which
      -- is what makes this gameplay-level rather than an applyEffect call.
      -- alice's library holds `stock`, DEEPEST FIRST -- S.addLibraryCard puts
      -- each card on top, so the last name given is the top card.
      board n seats stock = do
        mountain <- S.printingOf s registry "Mountain"
        soulfireEruption <- S.printingOf s registry "Soulfire Eruption"
        sentry <- S.printingOf s registry "Ogre Sentry"
        stocked <- mapM (S.printingOf s registry) stock
        let g1 = S.landsFor mountain S.alice 9 seats
            g2 = List.foldl' (\g pr -> snd (S.addLibraryCard pr S.alice g)) g1 stocked
            g3 = snd (S.addCreature sentry S.bob g2)
            (withSpell, spell) = S.handOne soulfireEruption g3
            afterCast = S.runPure (aimingAtEveryPlayer n) withSpell (S.cast S.alice spell)
        pure (S.runPure (aimingAtEveryPlayer n) afterCast Engine.priorityLoop)
      -- The same spell off the same nine Mountains, but the victims are
      -- PERMANENTS: bob controls Wall of Stone (0/8) and Nessian Asp (4/5), carol
      -- controls Sandbar Crocodile (6/5), and `wanted` picks which two of the six
      -- candidates the announcement takes. alice's library is Benalish Hero (1)
      -- under Hill Giant (4), so the pair of numbers the loop hands out is 4 then
      -- 1 and which victim gets which is the whole of what the order decides.
      -- Every toughness exceeds 4, so both victims survive to be read.
      victimBoard wanted order = do
        mountain <- S.printingOf s registry "Mountain"
        soulfireEruption <- S.printingOf s registry "Soulfire Eruption"
        wallOfStone <- S.printingOf s registry "Wall of Stone"
        nessianAsp <- S.printingOf s registry "Nessian Asp"
        sandbarCrocodile <- S.printingOf s registry "Sandbar Crocodile"
        stocked <- mapM (S.printingOf s registry) ["Benalish Hero", "Hill Giant"]
        let g1 = S.landsFor mountain S.alice 9 S.threePlayerGame
            g2 = List.foldl' (\g pr -> snd (S.addLibraryCard pr S.alice g)) g1 stocked
            (wall, g3) = S.addCreature wallOfStone S.bob g2
            (asp, g4) = S.addCreature nessianAsp S.bob g3
            (crocodile, g5) = S.addCreature sandbarCrocodile S.carol g4
            (withSpell, spell) = S.handOne soulfireEruption g5
            ((_, after), transcript) = Replay.record (soulfireOrdering (wanted wall asp crocodile) order) withSpell (S.cast S.alice spell >> Engine.priorityLoop)
        pure (S.damageOf wall after, S.damageOf asp after, S.damageOf crocodile after, transcript)
      orderAnswersIn = Maybe.mapMaybe (\r -> case r of Response.OrderedForEach o -> Just o; _ -> Nothing)
      named = Just . CardName.MkCardName . Text.pack
      exiledNames pid = List.sort . namesIn Zone.Exile pid
      permissionsIn pid gs = fmap (Maybe.isJust . Object.playableFromExile) (Maybe.mapMaybe (\oid -> Game.lookupObject oid gs) (Game.zoneMembers Zone.Exile pid gs))
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "SoulfireEruption" $ do
        Spec.it s "CR 608.2f each victim takes the mana value of the card exiled FOR IT, in APNAP order" $ do
          after <- board 3 S.threePlayerGame ["Sabretooth Tiger", "Bird Maiden", "Hill Giant", "Goblin Piker", "Benalish Hero"]
          Spec.assertEqWith
            s
            "three DIFFERENT cards left the library, top first, and the two under them stayed in order"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            ( List.sort [named "Benalish Hero", named "Goblin Piker", named "Hill Giant"],
              [named "Bird Maiden", named "Sabretooth Tiger"]
            )
          Spec.assertEqWith
            s
            "Benalish Hero (1) to alice, Goblin Piker (2) to bob, Hill Giant (4) to carol"
            (lives after)
            (Just 19, Just 18, Just 16)
          Spec.assertEqWith
            s
            "all three exiled cards carry the play permission, so the grant ran once per iteration"
            (permissionsIn S.alice after)
            [True, True, True]
        -- CR 609.3 inside the loop: the library runs out mid-sweep, so the
        -- iterations that find no top card exile nothing and their DealDamage
        -- has no mana value to read (Quantity.AgainstSlot answers Nothing, and an
        -- unevaluable quantity is a no-op). The batch is not shortened -- the
        -- members were swept before the first pass -- so the third seat simply
        -- takes nothing.
        Spec.it s "CR 609.3 a library that runs out mid-loop leaves the later members untouched" $ do
          after <- board 3 S.threePlayerGame ["Goblin Piker", "Benalish Hero"]
          Spec.assertEqWith
            s
            "alice took 1 and bob took 2; carol found no card and took nothing"
            (lives after)
            (Just 19, Just 18, Just 20)
          Spec.assertEqWith
            s
            "both cards were exiled and the library is empty"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            (List.sort [named "Benalish Hero", named "Goblin Piker"], [])
          Spec.assertEqWith s "the game has no result: an empty library is not itself a loss" (GameState.result after) Nothing
        -- CR 601.2c's "any number of target ...", which is what the card prints
        -- and what data/cards/soulfire-eruption.json says: no printed maximum, so
        -- the ceiling is the candidate count and nothing else. FOUR seats and
        -- bob's Ogre Sentry make five candidates and four are announced -- more
        -- than the three the other cases take, so any reading that puts a literal
        -- cap back on the slot fails HERE and nowhere else.
        --
        -- The four cards exiled have mana values 1, 2, 4 and 8: pairwise distinct
        -- and pairwise-sum distinct, so no two readings of the loop land on one
        -- life total.
        Spec.it s "CR 601.2c any number of targets: four announced on a five-candidate board" $ do
          after <- board 4 S.fourPlayerGame ["Sabretooth Tiger", "Bird Maiden", "Excruciator", "Hill Giant", "Goblin Piker", "Benalish Hero"]
          Spec.assertEqWith
            s
            "FOUR different cards left the library, top first, and the two under them stayed in order"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            ( List.sort [named "Benalish Hero", named "Goblin Piker", named "Hill Giant", named "Excruciator"],
              [named "Bird Maiden", named "Sabretooth Tiger"]
            )
          Spec.assertEqWith
            s
            "Benalish Hero (1) to alice, Goblin Piker (2) to bob, Hill Giant (4) to carol, Excruciator (8) to dave"
            (lives after, S.lifeOf S.dave after)
            ((Just 19, Just 18, Just 16), Just 12)
        -- The same card and the same board as the first case, differing in
        -- exactly one thing: the CR 601.2c announcement is zero rather than
        -- three. "Any number" includes none, so the low end of an unbounded range
        -- is announceable, and the sweep runs over an empty set.
        --
        -- What this case CANNOT tell apart is resolving from fizzling: CR 608.2b
        -- puts a fizzled spell in the same graveyard a resolved one reaches, and
        -- with no targets the effect does nothing either way. That CR 115.6 leaves
        -- a zero-target spell untargeted, and so not fizzled, is proved by Rat
        -- Out's token in upToOneTargetSpec above.
        Spec.it s "CR 601.2c zero announced against an unbounded count: nothing is exiled and nobody is damaged" $ do
          after <- board 0 S.threePlayerGame ["Sabretooth Tiger", "Bird Maiden", "Hill Giant", "Goblin Piker", "Benalish Hero"]
          Spec.assertEqWith s "no seat lost life" (lives after) (Just 20, Just 20, Just 20)
          Spec.assertEqWith
            s
            "the library is untouched and nothing was exiled"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            ([], [named "Benalish Hero", named "Goblin Piker", named "Hill Giant", named "Bird Maiden", named "Sabretooth Tiger"])
          Spec.assertEqWith
            s
            "and the spell left the stack for its owner's graveyard"
            (namesIn Zone.Graveyard S.alice after)
            [named "Soulfire Eruption"]
        -- CR 608.2f's SECONDARY sentence: two of the victims are objects ONE
        -- player controls, so their relative order is the resolving controller's
        -- -- alice's, though bob is the one who controls them, which is the whole
        -- of what separates this from APNAP. The pair below differs in exactly one
        -- thing, the answer alice gives, and the damage swaps with it. Pinned
        -- rather than searched: soulfireOrdering returns the permutation it was
        -- handed, so an engine that ignored the answer could not be repaired by
        -- the answerer finding another legal one.
        --
        -- Wall of Stone is the lower ObjectId, so it heads the offered group and
        -- [0, 1] is the engine's own former order. [1, 0] is therefore the half
        -- that goes red if the answer is dropped.
        Spec.it s "CR 608.2f the resolving controller orders one player's permanents: alice puts Wall of Stone first" $ do
          (wall, asp, crocodile, transcript) <- victimBoard (\w a _ -> [Recipient.ToCreature w, Recipient.ToCreature a]) [0, 1]
          Spec.assertEqWith
            s
            "Hill Giant (4) to the Wall, Benalish Hero (1) to the Asp, and carol's untargeted Crocodile untouched"
            (wall, asp, crocodile)
            (Just 4, Just 1, Just 0)
          Spec.assertEqWith s "and alice was asked for the order" (orderAnswersIn transcript) [[0, 1]]
        Spec.it s "CR 608.2f the resolving controller orders one player's permanents: alice puts Nessian Asp first" $ do
          (wall, asp, crocodile, transcript) <- victimBoard (\w a _ -> [Recipient.ToCreature w, Recipient.ToCreature a]) [1, 0]
          Spec.assertEqWith
            s
            "the same board and the same two victims, with the two mana values swapped by the answer alone"
            (wall, asp, crocodile)
            (Just 1, Just 4, Just 0)
          Spec.assertEqWith s "and the answer alice gave is the one recorded" (orderAnswersIn transcript) [[1, 0]]
        -- The elision, and the negative half of the pair above: two victims under
        -- two different seats are two groups of one, so rule 608.2f's secondary
        -- sentence has no relative order to give away and the primary
        -- determination settles everything. bob precedes carol in APNAP order, so
        -- the answer offered here is never asked for -- and would reverse the
        -- damage if it were.
        Spec.it s "CR 608.2f victims under different seats are ordered by APNAP alone, with nothing asked" $ do
          (wall, asp, crocodile, transcript) <- victimBoard (\w _ c -> [Recipient.ToCreature w, Recipient.ToCreature c]) [1, 0]
          Spec.assertEqWith
            s
            "Hill Giant (4) to bob's Wall and Benalish Hero (1) to carol's Crocodile; bob's untargeted Asp untouched"
            (wall, asp, crocodile)
            (Just 4, Just 0, Just 1)
          Spec.assertEqWith s "and no order was asked for" (orderAnswersIn transcript) []

-- Announces two targets, aims them at `wanted`, and answers CR 608.2f's
-- intra-seat order with `order` VERBATIM. Pinned rather than searched for a
-- legal permutation: an answerer that looked for one would find the engine's own
-- again after a mutation, and the assertion would stay green while the choice was
-- gone.
soulfireOrdering :: [Recipient.Recipient] -> [Natural] -> Prompt.Prompt r -> r
soulfireOrdering wanted order p = case p of
  Prompt.AnnounceTargets {} -> announcingCount 2 p
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (`elem` wanted) sets
  Prompt.OrderForEach {} -> order
  _ -> S.identityAnswer p

-- CR 601.2c: announce `n` targets per slot and aim them at the PLAYERS, which on
-- these boards leaves bob's Ogre Sentry -- a legal candidate of the same
-- AnyTarget pool -- deliberately unchosen.
aimingAtEveryPlayer :: Natural -> Prompt.Prompt r -> r
aimingAtEveryPlayer n p = case p of
  Prompt.AnnounceTargets {} -> announcingCount n p
  Prompt.ChooseTargets _ _ _ sets -> S.preferring isPlayerRecipient sets
  _ -> S.identityAnswer p
  where
    isPlayerRecipient r = case r of
      Recipient.ToPlayer _ -> True
      Recipient.ToCreature _ -> False
      Recipient.ToPlaneswalker _ -> False
      Recipient.ToBattle _ -> False
      Recipient.ToObject _ -> False

-- CR 107.14's "you may pay any amount of {E}" (Effect.PayAnyEnergy): the payer
-- names the amount as the spell RESOLVES -- not at CR 601.2b, which is what
-- separates this from CostComponent.PayLifeX's announced X -- and what they paid
-- is what the next effect of the same resolution reads.
--
-- Harnessed Lightning {1}{R} Instant (data/cards/harnessed-lightning.json):
-- "Choose target creature. You get {E}{E}{E} (three energy counters), then you
-- may pay any amount of {E}. Harnessed Lightning deals that much damage to that
-- creature." Name, cost, type line and oracle text checked against Scryfall
-- 2026-08-19.
--
-- WHY HARNESSED LIGHTNING and not Die Young, which #121's body nominates: the
-- two say the same sentence, and this one reads the amount back as damage rather
-- than as a -1/-1 per {E}, so nothing but the payment is under test.
payAnyEnergySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
payAnyEnergySpec s registry =
  Spec.describe s "Harnessed Lightning (CR 107.14)" $ do
    -- The amount is a real read, not a fixed number: alice banks two {E}, the
    -- spell gives her three more, and she names four of the five. Every number
    -- on the board is distinct, so no two readings of the rule coincide.
    Spec.it s "CR 107.14 the {E} its controller pays is the damage it deals" $ do
      (wallId, cast) <- harnessedBoard s registry
      let after = S.runPure (paying 4) cast Stack.resolveTop
      Spec.assertEqWith s "the Wall took the 4 damage alice paid for" (S.damageOf wallId after) (Just 4)
      Spec.assertEqWith s "and one of her five energy counters is left" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 1
    -- Zero is one of the "any amount" the card offers, and paying it is what the
    -- printed "may" declines to. The energy assertion is what tells this apart
    -- from a spell that never resolved at all -- the three {E} it gives are in
    -- the total either way, and CR 120.8 makes 0 damage no damage.
    Spec.it s "CR 107.14 paying nothing deals nothing and keeps the counters" $ do
      (wallId, cast) <- harnessedBoard s registry
      let after = S.runPure (paying 0) cast Stack.resolveTop
      Spec.assertEqWith s "the Wall took no damage" (S.damageOf wallId after) (Just 0)
      Spec.assertEqWith s "and all five energy counters are still hers" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 5
    -- CR 118.3: "a player can't pay a cost without having the necessary resources
    -- to pay it fully". The answer is nine and the board holds five, so a trusted
    -- answer would deal nine and leave a negative count.
    Spec.it s "CR 118.3 an answer above the payer's energy is capped at what they have" $ do
      (wallId, cast) <- harnessedBoard s registry
      let after = S.runPure (paying 9) cast Stack.resolveTop
      Spec.assertEqWith s "the Wall took five, not nine" (S.damageOf wallId after) (Just 5)
      Spec.assertEqWith s "and alice spent every counter she had" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 0

-- alice holds Harnessed Lightning with two Mountains untapped for its {1}{R} and
-- two {E} already banked; bob's Wall of Stone (0/8) is the only creature in play,
-- so the cast's one target is determined and survives every amount below. Returns
-- the Wall and the state with the spell cast and waiting on the stack.
harnessedBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, GameState.GameState)
harnessedBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  wall <- S.printingOf s registry "Wall of Stone"
  harnessed <- S.printingOf s registry "Harnessed Lightning"
  let (wallId, withWall) = S.addCreature wall S.bob (S.landsInPlay mountain 2)
      (inHand, spellId) = S.handOne harnessed withWall
      banked = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice inHand
  pure (wallId, snd (Engine.runGamePure S.identityAnswer banked (S.cast S.alice spellId)))

-- Answers Prompt.ChoosePaidEnergy with a fixed amount, deferring everything else
-- to S.identityAnswer. PINNED rather than read off the prompt's own bound, so a
-- mutation to that bound cannot quietly repair the answer.
paying :: Natural -> Prompt.Prompt r -> r
paying n p = case p of
  Prompt.ChoosePaidEnergy {} -> n
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  upToOneTargetSpec s registry
  multiTargetSpec s registry
  supportSpec s registry
  bolsterSpec s registry
  amassSpec s registry
  blightSpec s registry
  blightPlayerSpec s registry
  blightSimultaneitySpec s registry
  perCreatureCountersSpec s registry
  blightCostSpec s registry
  soulfireEruptionSpec s registry
  payAnyEnergySpec s registry
