{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Speed (CR 702.179, "start your engines!"), the CR 704.5z
-- arm Pawl.Engine.Sba runs from it, Pawl.Types.Player's speed field,
-- Pawl.Engine.Quantity's Speed arm (CR 702.179e/702.179f), and CR 702.178a's max
-- speed gate -- Pawl.Types.ActivatedAbility's condition, applied by
-- Pawl.Engine.Projection.abilitiesGiven.
--
-- Gameplay-level throughout. Muraganda Raceway is the whole fixture: a Land
-- printing "Start your engines!", "{T}: Add {C}" and "Max speed — {T}: Add
-- {C}{C}", so one card supplies both halves -- the resource and something whose
-- presence depends on it.
--
-- Every case about how speed CHANGES gets there through the rules: started by the
-- state-based action, raised by an opponent losing life to a Sign in Blood or a
-- Lightning Bolt cast through the stack. The `atSpeed` helper at the foot of this
-- module writes a speed directly, and only cases about what READS speed use it --
-- rule 702.179d admits one increase a turn, so a board at 4 is four turns of
-- setup that would prove nothing the increase cases have not already proved.
module Pawl.SpeedSpec where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Speed as Speed
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

-- This player's speed, as Player.speed holds it -- Nothing for a player who has
-- none (CR 702.179b), which is the state CR 704.5z looks for and is NOT the same
-- as Just 0.
speedOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe (Maybe Natural.Natural)
speedOf pid gs = fmap Player.speed (Map.lookup pid (GameState.players gs))

-- Alice's board: two Swamps and one Muraganda Raceway, with Sign in Blood in her
-- hand. S.handOne sets the precombat main phase and makes Alice the active
-- player, which is what CR 702.179d's "during your turn" needs.
--
-- BOTH libraries are stocked, and Alice's is not an afterthought: Sign in Blood
-- aims its draw and its life loss at the same player, so a case that aims it at
-- Alice decks her -- and CR 704.5b would then lose her the game before the speed
-- trigger was ever gathered, passing that case for a reason that has nothing to
-- do with rule 702.179.
raceBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
raceBoard raceway swamp signInBlood filler =
  let base = S.landsInPlay swamp 2
      (racewayId, withRaceway) = S.addCreature raceway S.alice base
      stock pid g = foldr (\_ g' -> snd (S.addLibraryCard filler pid g')) g [1 .. (4 :: Int)]
      (gs, spellId) = S.handOne signInBlood (stock S.alice (stock S.bob withRaceway))
   in (gs, racewayId, spellId)

-- Aim every target slot at Bob, so Sign in Blood's shared slot makes BOB the
-- player who loses life. Alice is the caster, so this is an OPPONENT losing life
-- -- CR 702.179d's event, and the whole point of the card being aimed rather than
-- left to whichever player a set happens to offer first.
atBob :: Prompt.Prompt r -> r
atBob p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
  _ -> S.identityAnswer p

-- Aim at Alice instead: the same card, the same life loss, but the loser is the
-- caster rather than an opponent.
atAlice :: Prompt.Prompt r -> r
atAlice p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.alice)) sets
  _ -> S.identityAnswer p

-- Cast the spell, resolve it, and settle -- which runs the state-based actions
-- (CR 704.3) and places whatever triggered (CR 603.3b). Then resolve whatever the
-- settle put on the stack and settle again, so an inherent speed trigger has both
-- happened and finished.
castResolveSettle :: (forall r. Prompt.Prompt r -> r) -> PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castResolveSettle answer pid spellId gs =
  let cast = S.runPure answer gs (S.cast pid spellId)
      resolved = S.runPure answer cast (Stack.resolveTop >> Engine.settleForPriority)
   in S.runPure answer resolved (Stack.resolveTop >> Engine.settleForPriority)

-- How many mana ended up in Alice's pool after tapping this source, with every
-- prompt answered by `answer`.
pooledBy :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> Int
pooledBy answer oid gs = case Game.poolOf S.alice (S.runPure answer gs (Mana.tapForMana oid)) of
  Mana.Type.MkMana units -> length units

-- Answer Prompt.ChooseManaYield with the LAST candidate. The discriminator for
-- the max speed case: a Raceway at max speed offers two yields, {C} and {C}{C},
-- and the {C}{C} one is the ability CR 702.178a granted. Answering with the first
-- would pass whether or not the grant happened.
biggestYield :: Prompt.Prompt r -> r
biggestYield p = case p of
  Prompt.ChooseManaYield _ _ _ candidates -> last (foldr (:) [] candidates)
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Speed" $ do
  startYourEnginesSpec s registry
  maxSpeedSpec s registry
  increaseSpec s registry

startYourEnginesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
startYourEnginesSpec s registry = Spec.describe s "StartYourEngines" $ do
  -- CR 704.5z, and CR 702.179a's whole content: the keyword does nothing on its
  -- own, and the state-based action is what turns controlling the permanent into
  -- having speed.
  Spec.it s "CR 704.5z a player controlling Muraganda Raceway with no speed gets speed 1" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    let (_, gs) = S.addCreature raceway S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "before the check, nobody has speed (CR 702.179b)" (speedOf S.alice gs) (Just Nothing)
    let after = S.settleSba gs
    Spec.assertEqWith s "alice's engines started" (speedOf S.alice after) (Just (Just 1))
    -- Bob controls no such permanent, so CR 704.5z passes him by. The falsifier
    -- for an implementation that gave everybody speed.
    Spec.assertEqWith s "bob, who controls none, still has no speed" (speedOf S.bob after) (Just Nothing)
  -- CR 704.5z fires only for a player who has NO speed, which is what makes it
  -- terminate: the settle loop re-checks until nothing acts (CR 704.3), and a
  -- clause that set speed to 1 unconditionally would never stop acting.
  Spec.it s "CR 704.5z does not fire again once speed exists" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, spellId) = raceBoard raceway swamp signInBlood piker
        raised = castResolveSettle atBob S.alice spellId gs
    Spec.assertEqWith s "speed rose to 2 and the check did not drag it back" (speedOf S.alice raised) (Just (Just 2))
  -- CR 305.7 / 702.179a: the keyword is read off the PROJECTION, so an effect
  -- that takes the permanent's rules text away takes its engines with it. Blood
  -- Moon and not Humility, because CR 613.1f's strip is aimed at creatures and
  -- the Raceway is a land -- CR 305.7 is the strip that reaches one.
  Spec.it s "CR 305.7 a Blood Moon'd Raceway starts nobody's engines" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (_, board) = S.addCreature raceway S.alice (Setup.emptyGame S.bothPlayers)
        (_, moonlit) = S.addCreature bloodMoon S.bob board
        after = S.settleSba moonlit
    Spec.assertEqWith s "no speed, the keyword having been stripped" (speedOf S.alice after) (Just Nothing)

maxSpeedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
maxSpeedSpec s registry = Spec.describe s "MaxSpeed" $ do
  -- CR 702.178a: "as long as your speed is 4, this object has '[Ability]'". At
  -- speed 1 the Raceway has ONE activated ability, its printed "{T}: Add {C}".
  Spec.it s "CR 702.178a below max speed the Raceway has only its unconditional mana ability" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    let (racewayId, board) = S.addCreature raceway S.alice (Setup.emptyGame S.bothPlayers)
        gs = S.settleSba board
    Spec.assertEqWith s "speed 1" (speedOf S.alice gs) (Just (Just 1))
    Spec.assertEqWith s "one activated ability" (length (Projection.abilitiesOf racewayId gs)) 1
    Spec.assertEqWith s "and tapping it makes one mana" (pooledBy biggestYield racewayId gs) 1
  -- CR 702.179f: a player who has NO speed at all reads as speed 0 for anything
  -- that asks. Settling is deliberately skipped, which is the only way to see a
  -- controller of a Raceway with no speed.
  --
  -- TWO assertions, because the gate alone cannot say this. An unanswered
  -- quantity and a 0 both fail "exactly 4" -- Condition.holds collapses the
  -- undeterminable case to False -- so the ability count below rules out a
  -- stand-in of 4 and nothing finer. Speed.speedOf is the reading CR 702.179f
  -- actually fixes, and Just 0 against the field's Nothing is what tells the two
  -- apart.
  Spec.it s "CR 702.179f a controller with no speed reads as 0, not as unanswered" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    let (racewayId, gs) = S.addCreature raceway S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "the field holds no speed at all (CR 702.179b)" (speedOf S.alice gs) (Just Nothing)
    Spec.assertEqWith s "but every reader sees 0 (CR 702.179f)" (Speed.speedOf S.alice gs) (Just 0)
    Spec.assertEqWith s "so the max speed ability is absent" (length (Projection.abilitiesOf racewayId gs)) 1
  -- CR 702.178a's other side, and the case the whole gate exists for: at speed 4
  -- the object HAS the granted ability, and tapping it makes two mana.
  Spec.it s "CR 702.178a at max speed the Raceway gains its second mana ability" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    let (racewayId, board) = S.addCreature raceway S.alice (Setup.emptyGame S.bothPlayers)
        gs = atSpeed 4 S.alice (S.settleSba board)
    Spec.assertEqWith s "two activated abilities" (length (Projection.abilitiesOf racewayId gs)) 2
    Spec.assertEqWith s "and the granted one makes two mana" (pooledBy biggestYield racewayId gs) 2
  -- CR 604.1 makes a static ability "simply true", so CR 702.178a's clause is
  -- re-asked on every read rather than latched: dropping the same player's speed
  -- back takes the ability away again, with no event and no resolution in
  -- between. Nothing here distinguishes "exactly 4" from "at least 4" -- speed
  -- cannot exceed 4 (CR 702.179d), so no board tells the two apart.
  Spec.it s "CR 604.1 the grant is re-asked, not latched" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    let (racewayId, board) = S.addCreature raceway S.alice (Setup.emptyGame S.bothPlayers)
        settled = S.settleSba board
        fast = atSpeed 4 S.alice settled
        slowAgain = atSpeed 3 S.alice fast
    Spec.assertEqWith s "two at speed 4" (length (Projection.abilitiesOf racewayId fast)) 2
    Spec.assertEqWith s "one again at speed 3" (length (Projection.abilitiesOf racewayId slowAgain)) 1
  -- The layer system is asked BEFORE CR 702.178a's gate, so a Raceway whose rules
  -- text CR 305.7 stripped has no ability left to grant however fast its
  -- controller is. The falsifier for a gate that added the ability rather than
  -- letting one through.
  Spec.it s "CR 305.7 the strip beats the grant even at max speed" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (racewayId, board) = S.addCreature raceway S.alice (Setup.emptyGame S.bothPlayers)
        (_, moonlit) = S.addCreature bloodMoon S.bob board
        gs = atSpeed 4 S.alice moonlit
    Spec.assertEqWith s "no activated abilities at all" (Projection.abilitiesOf racewayId gs) []

increaseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
increaseSpec s registry = Spec.describe s "Increase" $ do
  -- CR 702.179d, the whole ability: an opponent lost life on Alice's turn, so her
  -- speed goes up by 1. Sign in Blood is cast and resolved through the stack, and
  -- the inherent trigger it wakes resolves off the stack in turn.
  Spec.it s "CR 702.179d an opponent losing life on your turn raises your speed" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, spellId) = raceBoard raceway swamp signInBlood piker
        after = castResolveSettle atBob S.alice spellId gs
    Spec.assertEqWith s "bob lost two life" (S.lifeOf S.bob after) (fmap (subtract 2) (S.lifeOf S.bob gs))
    Spec.assertEqWith s "so alice's speed rose from 1 to 2" (speedOf S.alice after) (Just (Just 2))
  -- CR 119.2: damage dealt to a player CAUSES that player to lose life, so a
  -- Lightning Bolt to the face is the same event as Sign in Blood's clause as far
  -- as CR 702.179d is concerned. The falsifier for recording life loss only where
  -- an effect said "lose life" -- which is how most speed actually rises in play.
  Spec.it s "CR 119.2 damage to an opponent raises your speed too" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.landsInPlay mountain 1
        (_, withRaceway) = S.addCreature raceway S.alice base
        (gs, spellId) = S.handOne lightningBolt withRaceway
        after = castResolveSettle atBob S.alice spellId gs
    Spec.assertEqWith s "bob took three" (S.lifeOf S.bob after) (fmap (subtract 3) (S.lifeOf S.bob gs))
    Spec.assertEqWith s "and alice's speed rose from 1 to 2" (speedOf S.alice after) (Just (Just 2))
  -- CR 702.179d's "one or more OPPONENTS": the caster losing life herself is not
  -- the event. The falsifier for a matcher that fired on any life loss at all.
  Spec.it s "CR 702.179d your own life loss raises nothing" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, spellId) = raceBoard raceway swamp signInBlood piker
        after = castResolveSettle atAlice S.alice spellId gs
    Spec.assertEqWith s "alice lost two life" (S.lifeOf S.alice after) (fmap (subtract 2) (S.lifeOf S.alice gs))
    -- She is still in the game, which is what makes the assertion below say
    -- something: a decked Alice (CR 704.5b) would stop being an opponent of
    -- herself and pass this case for the wrong reason.
    Spec.assertEqWith s "and she is still playing" (GameState.result after) Nothing
    Spec.assertEqWith s "and her speed stayed at 1" (speedOf S.alice after) (Just (Just 1))
  -- CR 702.179d's "this ability triggers only once each turn". Two separate
  -- castings, two separate life losses, one increase.
  Spec.it s "CR 702.179d the increase happens only once each turn" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, _, firstSpell) = raceBoard raceway swamp signInBlood piker
        once = castResolveSettle atBob S.alice firstSpell gs0
        -- Two more untapped Swamps: the first casting tapped hers, and this case
        -- is about the trigger limit rather than about mana.
        refuelled = foldr (\_ g -> snd (S.addCreature swamp S.alice g)) once [1 .. (2 :: Int)]
        (secondSpell, withAnother) = S.addHandCard signInBlood S.alice refuelled
        twice = castResolveSettle atBob S.alice secondSpell withAnother
    Spec.assertEqWith s "bob lost four life over the two castings" (S.lifeOf S.bob twice) (fmap (subtract 4) (S.lifeOf S.bob gs0))
    Spec.assertEqWith s "but alice's speed rose only once" (speedOf S.alice twice) (Just (Just 2))
  -- CR 702.179d's intervening "if your speed is less than 4", at CR 603.4's
  -- gather-time check: a player already at max speed does not trigger, so speed
  -- stops at 4 rather than climbing past it. CR 702.179e is why 4 is the number.
  Spec.it s "CR 702.179d speed stops at 4" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, _, spellId) = raceBoard raceway swamp signInBlood piker
        gs = atSpeed 4 S.alice gs0
        after = castResolveSettle atBob S.alice spellId gs
    Spec.assertEqWith s "bob still lost the life" (S.lifeOf S.bob after) (fmap (subtract 2) (S.lifeOf S.bob gs))
    Spec.assertEqWith s "and alice is still at 4, not 5" (speedOf S.alice after) (Just (Just 4))
  -- CR 702.179d's "during YOUR turn": the same life loss on the opponent's own
  -- turn raises nobody's speed. Bob is the active player and casts it at himself,
  -- so Alice -- who has the speed -- is not the active player and her ability
  -- never fires.
  Spec.it s "CR 702.179d a life loss on somebody else's turn raises nothing" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, _, _) = raceBoard raceway swamp signInBlood piker
        -- Bob's own two Swamps and his own copy, and the turn handed to him.
        withBobsLands = foldr (\_ g -> snd (S.addCreature swamp S.bob g)) gs0 [1 .. (2 :: Int)]
        (bobSpell, withBobsSpell) = S.addHandCard signInBlood S.bob withBobsLands
        started = S.settleSba withBobsSpell
        bobsTurn = started {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
        after = castResolveSettle atBob S.bob bobSpell bobsTurn
    Spec.assertEqWith s "bob lost two life on his own turn" (S.lifeOf S.bob after) (fmap (subtract 2) (S.lifeOf S.bob bobsTurn))
    Spec.assertEqWith s "and alice's speed did not move" (speedOf S.alice after) (Just (Just 1))
  -- CR 702.179d's per-turn limit is per TURN, not per game: the handoff clears
  -- it, so the same board raises speed again next turn. Nothing else in the
  -- engine resets it.
  Spec.it s "CR 702.179d a new turn restores the once-each-turn allowance" $ do
    raceway <- S.printingOf s registry "Muraganda Raceway"
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, _, firstSpell) = raceBoard raceway swamp signInBlood piker
        once = castResolveSettle atBob S.alice firstSpell gs0
        -- The turn handoff, which is the only thing under test here. Two more
        -- untapped Swamps rather than an untap step, so nothing but
        -- Engine.beginTurnOf stands between the two castings.
        nextTurn = foldr (\_ g -> snd (S.addCreature swamp S.alice g)) (Engine.beginTurnOf S.alice once) [1 .. (2 :: Int)]
        (secondSpell, withAnother) = S.addHandCard signInBlood S.alice nextTurn
        twice = castResolveSettle atBob S.alice secondSpell withAnother
    Spec.assertEqWith s "one increase on the first turn" (speedOf S.alice once) (Just (Just 2))
    Spec.assertEqWith s "and another on the second" (speedOf S.alice twice) (Just (Just 3))

-- Put this player at exactly this speed, bypassing CR 702.179d's climb. Every
-- case that uses it is about what READS speed (CR 702.178a's gate, CR 702.179e's
-- max speed), not about how speed got there -- which increaseSpec proves through
-- the rules.
atSpeed :: Natural.Natural -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
atSpeed n pid gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.speed = Just n}) pid (GameState.players gs)}
