{-# LANGUAGE GADTs #-}

-- Covers: CR 102.3's teammates, CR 804's deploy creatures option
-- (Pawl.Engine.Deploy) and CR 808's Team vs. Team variant --
-- Pawl.Types.Teams, the Pawl.Types.GameSettings field that carries them, and the
-- readers that answer "who are my opponents", one case each: Pawl.Engine.Combat's
-- attackableOpponents (CR 506.2a), Pawl.Engine.Resolve's playerRefPlayers
-- (PlayerRef.Relative Opponent), Pawl.Engine.Target's slot filter (the
-- Filter.IsPlayer atom, which reads Pawl.Engine.Filter's Context), Pawl.Engine's
-- PlayerEffect.inScope (PlayerScope.Opponents), Pawl.Engine.Replacement's
-- matchesZoneOwner (CR 400.3's owner, for a zone-change redirect) and
-- Pawl.Engine.Count's playersFor (the same PlayerRef under a Count).
-- Pawl.BattleSpec holds the seventh, CR 310.12a's protector candidates, beside
-- its siblings.
--
-- FOUR SEATS IN TWO TEAMS throughout, which is the smallest board on which CR
-- 102.3 and CR 806.1 disagree: at three seats in two teams a player has one
-- teammate and one opponent, so "every other player" and "every player not on my
-- team" differ by one seat and a reading that answered the WRONG one seat looks
-- like a reading that answered none. Four seats leave alice two opponents and one
-- teammate, so the count, the poison counters and the attack all separate three
-- readings: CR 102.3's, CR 806.1's free-for-all, and one that names nobody.
--
-- The teams are CR 808.2's -- each team sits together, so turn order
-- [alice, bob, carol, dave] puts alice's teammate bob in the seat a free-for-all
-- reading reaches FIRST. That is deliberate: a defective reading takes bob before
-- it takes anybody else, so no case here can pass by stopping early.
--
-- CR 808.3a needs nothing from these boards: the attack multiple players option
-- is already the one in use by default (Pawl.Types.GameSettings.attackOption),
-- which is why the combat case can read the whole defending group.
module Pawl.TeamSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.ActivateSpec as ActivateSpec
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.InitiativeSpec as InitiativeSpec
import qualified Pawl.PreventionSpec as PreventionSpec
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.SpeedSpec as SpeedSpec
import qualified Pawl.Support as S
import qualified Pawl.TurnSpec as TurnSpec
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- CR 808.1 / CR 808.2: alice and bob against carol and dave, each team in
-- adjacent seats of the turn order [alice, bob, carol, dave].
twoTeams :: GameState.GameState -> GameState.GameState
twoTeams = S.inTeams [[S.alice, S.bob], [S.carol, S.dave]]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Teams" $ do
  -- CR 506.2a with CR 102.3: the attacking player's opponents are the defending
  -- players, and a teammate is not one of them.
  --
  -- TWO DECLARATIONS on ONE board, differing only in whom the Piker is announced
  -- as attacking, so the negative cannot pass for want of a legal attacker: the
  -- positive proves the same creature, on the same board, may attack an opponent.
  Spec.it s "CR 102.3 a creature cannot attack its controller's teammate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (mine, staged) = S.addPermanent piker S.alice (twoTeams S.fourPlayerGame)
        board =
          staged
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
              GameState.remaining =
                Seq.fromList
                  [ Phase.Combat CombatStep.DeclareAttackers,
                    Phase.Combat CombatStep.DeclareBlockers,
                    Phase.Combat CombatStep.CombatDamage,
                    Phase.Combat CombatStep.EndOfCombat,
                    Phase.PostcombatMain,
                    Phase.Ending EndingStep.EndStep,
                    Phase.Ending EndingStep.Cleanup
                  ]
            }
        settled = S.runPure S.identityAnswer board (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
    Spec.assertBool
      s
      (not (Combat.legalAttackDeclarationAs S.alice [(mine, AttackTarget.OfPlayer S.bob)] settled))
      "CR 102.3 alice's creature may not attack her teammate bob"
    Spec.assertBool
      s
      (Combat.legalAttackDeclarationAs S.alice [(mine, AttackTarget.OfPlayer S.carol)] settled)
      "CR 102.3 the same creature may attack her opponent carol"
    -- CR 802.2 as the proxy behind both: the defending players are exactly the
    -- other team, in APNAP order.
    Spec.assertEqWith
      s
      "CR 802.2 the defending players are the other team"
      (Combat.Type.defenders (GameState.combat settled))
      [S.carol, S.dave]
  -- CR 702.11c through Pawl.Engine.PlayerEffect.inScope's PlayerScope.Opponents,
  -- which is the reader neither the PlayerRef nor the target-slot case above
  -- touches: "'Hexproof' on a player means 'You can't be the target of spells or
  -- abilities your opponents control.'"
  --
  -- Leyline of Sanctity on BOB, and the three readings come apart on one board:
  -- his teammate alice may still target him, his opponent carol may not, and he
  -- may target himself (rule 702.11c names only opponents). Pawl.TargetSpec's
  -- Leyline case is the same card with no teams, where alice is the opponent.
  Spec.it s "CR 702.11c hexproof from opponents does not stop a teammate" $ do
    leyline <- S.printingOf s registry "Leyline of Sanctity"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, warded) = S.addPermanent leyline S.bob (twoTeams S.fourPlayerGame)
    case S.spellTargetSlot bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSlot -> do
        let legalFor who = Target.legalRecipients (Just who) S.noSource theSlot warded
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) (legalFor S.alice)) "CR 102.3 alice, bob's teammate, may still bolt him"
        Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.bob) (legalFor S.carol))) "CR 702.11c carol, his opponent, may not"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) (legalFor S.bob)) "and bob may bolt himself"
  -- CR 102.3 through the zone-owner reader of ControllerRelation.Opponents,
  -- Pawl.Engine.Replacement's matchesZoneOwner (judged, like its siblings, by
  -- relationHolds), which the three cases above do not reach: a zone change
  -- asks who OWNS the moving card (CR 400.3), not who controls anything.
  --
  -- Leyline of the Void, {2}{B}{B} Enchantment: "If a card would be put into an
  -- opponent's graveyard from anywhere, exile it instead." ONE board and one
  -- funnel, with two cards differing in nothing but their owner -- bob's, alice's
  -- teammate, and carol's, her opponent -- so the negative cannot pass for want
  -- of a working redirect: carol's card is exiled on the same board.
  Spec.it s "CR 102.3 a teammate's card is not put into an opponent's graveyard" $ do
    leyline <- S.printingOf s registry "Leyline of the Void"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent leyline S.alice (twoTeams S.fourPlayerGame)
        (teammates, g1) = S.addLibraryCard piker S.bob g0
        (opponents, g2) = S.addLibraryCard piker S.carol g1
        after = S.runPure S.identityAnswer g2 (Event.changeZone teammates Zone.Graveyard)
        alsoAfter = S.runPure S.identityAnswer after (Event.changeZone opponents Zone.Graveyard)
    Spec.assertEqWith s "CR 102.3 bob's card reached bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob alsoAfter)) 1
    Spec.assertEqWith s "and nothing of bob's was exiled" (length (Game.zoneMembers Zone.Exile S.bob alsoAfter)) 0
    Spec.assertEqWith s "CR 614.1a carol's was exiled instead" (length (Game.zoneMembers Zone.Exile S.carol alsoAfter)) 1
    Spec.assertEqWith s "and never reached carol's graveyard" (length (Game.zoneMembers Zone.Graveyard S.carol alsoAfter)) 0
  sharedTurnsSpec s registry

-- CR 805.1: twoTeams with the shared team turns option on.
sharedTurns :: GameState.GameState -> GameState.GameState
sharedTurns gs = gs {GameState.settings = (GameState.settings gs) {GameSettings.sharedTeamTurns = True}}

-- CR 805.4's shared team turns. Every case is a PAIR of boards differing only in
-- the option: twoTeams with it and twoTeams without, so a case cannot pass for
-- the teams alone.
sharedTurnsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sharedTurnsSpec s registry = Spec.describe s "SharedTeamTurns" $ do
  -- Each library holds cards enough for the turns run, so CR 704.5b ends nothing.
  let stockedWith island option =
        let teamed = option (twoTeams S.fourPlayerGame)
            stockOne gs pid = List.foldl' (\g _ -> snd (S.addLibraryCard island pid g)) gs [1 :: Int .. 4]
         in List.foldl' stockOne teamed [S.alice, S.bob, S.carol, S.dave]
      takers n gs =
        if n <= (0 :: Int)
          then []
          else
            let next = fst (TurnSpec.runTurn S.identityAnswer gs)
             in GameState.activePlayer next : takers (n - 1) next
  -- CR 805.4: the turn passes team to team, so bob's seat is passed over after
  -- alice's turn and dave's after carol's.
  Spec.it s "CR 805.4 each team takes turns rather than each player" $ do
    island <- S.printingOf s registry "Island"
    Spec.assertEqWith s "carol's team, then alice's, then carol's" (takers 3 (stockedWith island sharedTurns)) [S.carol, S.alice, S.carol]
    Spec.assertEqWith s "without the option every seat takes one" (takers 3 (stockedWith island id)) [S.bob, S.carol, S.dave]
  -- CR 502.2a / 731.2a: bob casts one spell on his team's turn and alice none, so
  -- the previous turn's active team cast a spell. The count the next untap step
  -- reads is bob's one, where the unshared game reads alice's none.
  Spec.it s "CR 502.2a the handoff records what the previous active team cast" $ do
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let run option =
          let lands = S.landsFor mountain S.bob 1 (option (twoTeams S.fourPlayerGame))
              (held, staged) = S.addHandCard bolt S.bob lands
              board =
                staged
                  { GameState.phase = Phase.PrecombatMain,
                    GameState.activePlayer = S.alice,
                    GameState.priority = Just S.bob
                  }
              cast = S.runPure S.castAnswer board (S.cast S.bob held)
           in GameState.spellsCastLastTurn (Engine.beginTurnOf S.carol cast)
    Spec.assertEqWith s "bob's one spell counts for his team" (run sharedTurns) 1
    Spec.assertEqWith s "without the option only alice's none counts" (run id) 0
  -- CR 805.4d's other half: an ability that does not refer to "that player"
  -- triggers once, however many players share the turn.
  --
  -- Khabál Ghoul, {2}{B} Creature -- Zombie 1/1: "At the beginning of each end
  -- step, put a +1/+1 counter on this creature for each creature that died this
  -- turn."
  Spec.it s "CR 805.4d an ability not naming that player triggers once for the team" $ do
    ghoul <- S.printingOf s registry "Khabál Ghoul"
    let run option =
          let endStep = Phase.Ending EndingStep.EndStep
              (_, placed) = S.addPermanent ghoul S.carol (option (twoTeams S.fourPlayerGame))
              began =
                S.withEvents
                  [GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)]
                  placed {GameState.phase = endStep, GameState.activePlayer = S.alice}
           in length (GameState.stack (snd (Engine.runGamePure S.identityAnswer began Engine.placePendingTriggers)))
    Spec.assertEqWith s "one trigger with the option" (run sharedTurns) 1
    Spec.assertEqWith s "and one without" (run id) 1
  -- CR 702.179d / 805.4: alice and bob are both active players, so one opponent's
  -- life loss raises both their speeds -- each spends their OWN once-each-turn
  -- limit, which Engine.limitKey's controller component is what keeps apart.
  Spec.it s "CR 702.179d each active teammate's speed rises once" $ do
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let atCarol :: Prompt.Prompt r -> r
        atCarol p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.carol))) sets
          _ -> S.identityAnswer p
        run option =
          let lands = S.landsFor mountain S.alice 1 (option (twoTeams S.fourPlayerGame))
              (held, staged) = S.addHandCard bolt S.alice lands
              board =
                SpeedSpec.atSpeed 2 S.bob $
                  SpeedSpec.atSpeed
                    1
                    S.alice
                    staged
                      { GameState.phase = Phase.PrecombatMain,
                        GameState.activePlayer = S.alice,
                        GameState.priority = Just S.alice
                      }
              cast = S.runPure atCarol board (S.cast S.alice held)
              after = S.runPure atCarol cast Engine.priorityLoop
           in (S.lifeOf S.carol after, fmap (`SpeedSpec.speedOf` after) [S.alice, S.bob])
    Spec.assertEqWith s "carol took three, and both speeds rose" (run sharedTurns) (Just 17, [Just (Just 2), Just (Just 3)])
    Spec.assertEqWith s "without the option only alice's rose" (run id) (Just 17, [Just (Just 2), Just (Just 2)])
  -- CR 805.5 / 805.5b: the team holds priority, so once bob casts his Bolt his
  -- team has it again, and alice -- who passed before he cast -- is asked before
  -- carol's team is. The sequence of players asked is the engine's own output.
  Spec.it s "CR 805.5 a teammate who passed is asked again before the team passes" $ do
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let bobCastsOnce :: Prompt.Prompt r -> State.State ([PlayerId.PlayerId], Bool) r
        bobCastsOnce p = case p of
          Prompt.ChooseAction _ pid actions -> do
            (asked, spent) <- State.get
            let casts = [a | a@Action.Cast {} <- actions]
            case casts of
              a : _ | pid == S.bob && not spent -> State.put (asked <> [pid], True) >> pure a
              _ -> State.put (asked <> [pid], spent) >> pure Action.Pass
          Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Set.singleton (Recipient.ToPlayer S.carol))) sets)
          _ -> pure (S.identityAnswer p)
        run option =
          let lands = S.landsFor mountain S.bob 1 (option (twoTeams S.fourPlayerGame))
              (_, staged) = S.addHandCard bolt S.bob lands
              board =
                staged
                  { GameState.phase = Phase.PrecombatMain,
                    GameState.activePlayer = S.alice,
                    GameState.priority = Just S.alice
                  }
              ((_, after), (asked, _)) = State.runState (Engine.runGame bobCastsOnce board Engine.priorityLoop) ([], False)
           in (asked, S.lifeOf S.carol after)
    Spec.assertEqWith
      s
      "CR 805.5b alice is asked after bob's cast, before carol and dave"
      (run sharedTurns)
      ([S.alice, S.bob, S.bob, S.alice, S.carol, S.dave, S.alice, S.bob, S.carol, S.dave], Just 17)
    Spec.assertEqWith
      s
      "without the option priority passes from bob to carol"
      (run id)
      ([S.alice, S.bob, S.bob, S.carol, S.dave, S.alice, S.alice, S.bob, S.carol, S.dave], Just 17)
  -- CR 805.7 / 805.2: the active team's triggers are one set, ordered by its
  -- primary player, interleaved; the nonactive team's go on after. Dave and alice
  -- are a team whose seats wrap the turn order [alice, bob, carol, dave], so dave
  -- is its rightmost seat and primary while alice is the active player, and a
  -- plain turn-order rotation from alice would put bob between them.
  --
  -- Soul Warden, {W} Creature: "Whenever another creature enters, you gain 1
  -- life." Two are alice's and one each is dave's and bob's; a Goblin Piker
  -- entering triggers all four.
  Spec.it s "CR 805.7 the primary player orders the whole team's triggers" $ do
    warden <- S.printingOf s registry "Soul Warden"
    piker <- S.printingOf s registry "Goblin Piker"
    let run option =
          let teamed = option (S.inTeams [[S.dave, S.alice], [S.bob, S.carol]] S.fourPlayerGame)
              (first, g1) = S.addPermanent warden S.alice teamed
              (second, g2) = S.addPermanent warden S.alice g1
              (daves, g3) = S.addPermanent warden S.dave g2
              (bobs, g4) = S.addPermanent warden S.bob g3
              (entrant, g5) = S.addPermanent piker S.carol g4
              entered = ZoneChange.MkZoneChange entrant entrant Zone.Stack Zone.Battlefield
              began = S.withEvents [GameEvent.Moved (Moved.moved entered (Projection.project entrant g5))] g5
              -- Dave's between alice's two, which only one choice over all
              -- three can reach.
              wanted = fmap TriggerSource.OfObject [first, daves, second]
              interleave :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
              interleave p = case p of
                Prompt.OrderTriggers _ pid entries -> do
                  State.modify' (<> [pid])
                  pure [i | source <- wanted, (i, entry) <- zip [0 ..] entries, TriggerEntry.source entry == source]
                _ -> pure (S.identityAnswer p)
              ((_, placed), asked) = State.runState (Engine.runGame interleave began Engine.placePendingTriggers) []
              sourceOf oid = case fmap Object.source (Game.lookupObject oid placed) of
                Just (Source.OfTrigger triggered) -> Just (TriggeredAbilitySource.source triggered)
                _ -> Nothing
           in (asked, fmap sourceOf (GameState.stack placed), (first, second, daves, bobs))
        (sharedAsked, sharedStack, (a1, a2, d, b)) = run sharedTurns
        (aloneAsked, aloneStack, _) = run id
    Spec.assertEqWith s "CR 805.7 dave's trigger sits between alice's, under bob's" sharedStack (fmap Just [b, a2, d, a1])
    Spec.assertEqWith s "CR 805.2 dave, the primary player, was asked" sharedAsked [S.dave]
    Spec.assertEqWith s "without the option alice orders her own two and dave's goes on last" aloneStack (fmap Just [d, b, a2, a1])
    Spec.assertEqWith s "and alice is the one asked" aloneAsked [S.alice]
  -- CR 805.5b / 800.4j: the active TEAM receives priority, so with alice gone her
  -- teammate dave is asked first, though bob is the next seat after hers. Dave
  -- and alice are the team whose seats wrap the turn order.
  Spec.it s "CR 805.5b a departed active player's teammate receives priority" $ do
    let asking :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
        asking p = case p of
          Prompt.ChooseAction _ pid _ -> State.modify' (<> [pid]) >> pure Action.Pass
          _ -> pure (S.identityAnswer p)
        run option =
          let teamed = option (S.inTeams [[S.dave, S.alice], [S.bob, S.carol]] S.fourPlayerGame)
              board =
                teamed
                  { GameState.phase = Phase.PrecombatMain,
                    GameState.activePlayer = S.alice,
                    GameState.players = Map.adjust (\player -> player {Player.status = Status.Departed Departure.Lost}) S.alice (GameState.players teamed)
                  }
           in State.execState (Engine.runGame asking board Engine.priorityLoop) []
    Spec.assertEqWith s "CR 805.5b dave, then bob's team" (run sharedTurns) [S.dave, S.bob, S.carol]
    Spec.assertEqWith s "without the option bob, the next seat, is first" (run id) [S.bob, S.carol, S.dave]
  -- CR 805.3a: the starting team declares its mulligans first, then the other
  -- team. Alice starts, and her teammate dave's seat wraps the turn order, so a
  -- plain turn-order walk would ask bob and carol before him.
  Spec.it s "CR 805.3a the starting team declares its mulligans first" $ do
    island <- S.printingOf s registry "Island"
    let keeping :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
        keeping p = case p of
          Prompt.DeclareMulligan _ pid _ -> State.modify' (<> [pid]) >> pure MulliganDecision.Keep
          _ -> pure (S.identityAnswer p)
        seats = [S.alice, S.bob, S.carol, S.dave]
        run option =
          let teamed = option (S.inTeams [[S.dave, S.alice], [S.bob, S.carol]] S.fourPlayerGame)
              stockOne gs pid = List.foldl' (\g _ -> snd (S.addLibraryCard island pid g)) gs [1 :: Int .. 8]
              board = List.foldl' stockOne teamed seats
           in State.execState (Engine.runGame keeping board (Mulligan.openingHands S.performer seats)) []
    Spec.assertEqWith s "CR 805.3a alice and dave, then bob and carol" (run sharedTurns) [S.alice, S.dave, S.bob, S.carol]
    Spec.assertEqWith s "without the option each seat in turn order" (run id) seats
  -- CR 803.1a / 803.1b: left and right are SEATS, which CR 805.6's team-grouped
  -- APNAP order does not preserve. Dave and alice's team wraps the turn order
  -- [alice, bob, carol, dave], so alice's right-hand seat is her teammate dave
  -- and she may attack nobody, while her left is bob.
  Spec.it s "CR 803.1b attack right reads the seat, not the team order" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let run option =
          let teamed = sharedTurns (S.inTeams [[S.dave, S.alice], [S.bob, S.carol]] S.fourPlayerGame)
              (mine, staged) = S.addPermanent piker S.alice teamed {GameState.settings = (GameState.settings teamed) {GameSettings.attackOption = Just option}}
              board =
                staged
                  { GameState.activePlayer = S.alice,
                    GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
                    GameState.remaining = Seq.fromList (drop 5 Turn.allPhases)
                  }
              settled = S.runPure S.identityAnswer board (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
           in fmap (\pid -> Combat.legalAttackDeclarationAs S.alice [(mine, AttackTarget.OfPlayer pid)] settled) [S.bob, S.carol]
    Spec.assertEqWith s "CR 803.1b attacking right, neither bob nor carol" (run AttackOption.Rightward) [False, False]
    Spec.assertEqWith s "CR 803.1a attacking left, bob only" (run AttackOption.Leftward) [True, False]
  -- CR 603.2c / 805.10a: alice's and bob's Pikers connect with carol in one CR
  -- 510.2 step, and Norn's Decree's "one or more creatures AN OPPONENT controls"
  -- is one occurrence per opponent -- so each attacking player gets a poison
  -- counter, where one trigger for the step would name only one of them.
  --
  -- Norn's Decree, {2}{W} Enchantment: "Whenever one or more creatures an
  -- opponent controls deal combat damage to you, that opponent gets a poison
  -- counter. ..."
  Spec.it s "CR 603.2c two opponents' creatures dealing combat damage together are two occurrences" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    decree <- S.printingOf s registry "Norn's Decree"
    let run option =
          let (_, withDecree) = S.addPermanent decree S.carol (atCombat (option (twoTeams S.fourPlayerGame)))
              (_, withAlice) = S.addPermanent piker S.alice withDecree
              (_, board) = S.addPermanent piker S.bob withAlice
              after = S.runCombat (S.attackTo S.carol) board
              poisonOf pid = S.playerCounterOf PlayerCounterKind.Poison pid after
           in (poisonOf S.alice, poisonOf S.bob, S.lifeOf S.carol after)
    Spec.assertEqWith s "alice and bob each got a poison counter" (run sharedTurns) (1, 1, Just 16)
    Spec.assertEqWith s "without the option only alice attacked" (run id) (1, 0, Just 18)
  -- CR 805.10b / 508.3b: the active team makes ONE combined attack, so carol is
  -- attacked once however many teammates sent creatures at her.
  --
  -- Curse of Vitality, {2}{W} Enchantment -- Aura Curse: "Enchant player /
  -- Whenever enchanted player is attacked, you gain 2 life. Each opponent
  -- attacking that player does the same."
  Spec.it s "CR 508.3b a player two teammates attack is attacked once" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    curse <- S.printingOf s registry "Curse of Vitality"
    let run option =
          let (aura, withCurse) = S.addPermanent curse S.dave (atCombat (option (twoTeams S.fourPlayerGame)))
              (_, withAlice) = S.addPermanent piker S.alice (S.attachTo aura (Recipient.ToPlayer S.carol) withCurse)
              (_, board) = S.addPermanent piker S.bob withAlice
              after = S.runCombat (S.attackTo S.carol) board
           in fmap (`S.lifeOf` after) [S.alice, S.bob, S.carol, S.dave]
    Spec.assertEqWith s "CR 508.3b one trigger: dave, alice and bob each gained 2" (run sharedTurns) [Just 22, Just 22, Just 16, Just 22]
    Spec.assertEqWith s "without the option only alice attacked" (run id) [Just 22, Just 20, Just 18, Just 22]
  -- CR 805.10b / 805.2 / 800.4j: with alice gone her team still attacks, and
  -- bob, now its primary player, declares it.
  Spec.it s "CR 805.2 a departed active player's teammate declares the attack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let asking :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
        asking p = case p of
          Prompt.DeclareAttackers _ pid _ -> State.modify' (<> [pid]) >> pure (S.attackTo S.carol p)
          _ -> pure (S.attackTo S.carol p)
        run option =
          let teamed = option (twoTeams S.fourPlayerGame)
              (_, staged) = S.addPermanent piker S.bob (atCombat teamed)
              board =
                staged
                  { GameState.priority = Just S.bob,
                    GameState.players = Map.adjust (\player -> player {Player.status = Status.Departed Departure.Lost}) S.alice (GameState.players staged)
                  }
              ((_, after), asked) = State.runState (Engine.runGame asking board S.combatGame) []
           in (S.lifeOf S.carol after, asked)
    Spec.assertEqWith s "bob declared and his Piker dealt carol 2" (run sharedTurns) (Just 18, [S.bob])
    Spec.assertEqWith s "without the option nobody declared" (run id) (Just 20, [])
  -- CR 805.8: ONE effect skipping two players on a team skips that team's step
  -- once. Carol turns Brine Elemental face up, so alice and bob each skip their
  -- next untap step: their team's next untap step is skipped and the one after
  -- is not.
  --
  -- Brine Elemental, {4}{U}{U} 5/4 Creature -- Elemental: "Morph {5}{U}{U}.
  -- When this creature is turned face up, each opponent skips their next untap
  -- step."
  Spec.it s "CR 805.8 one effect skips a team's step once" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    brine <- S.printingOf s registry "Brine Elemental"
    let (alices, g1) = S.addPermanent piker S.alice (stockedWith island sharedTurns)
        (bobs, g2) = S.addPermanent piker S.bob g1
        withLands = List.foldl' (\g _ -> snd (S.addPermanent island S.carol g)) g2 [1 .. (10 :: Int)]
        (brineId, g3) = S.addHandCard brine S.carol withLands
        board = (S.tapObject bobs (S.tapObject alices g3)) {GameState.phase = Phase.PrecombatMain, GameState.remaining = S.phasesAfter Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
        down = S.runPure S.identityAnswer board (Cast.castSpell S.manaPerformer S.carol brineId (S.printingName brine) (Facing.faceDown FaceDownReason.Morphed) >> Stack.resolveTop)
        tapped oid gs = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Tapped
        -- The rest of the current turn and carol's, then the untap step of
        -- alice's team's next turn.
        nextUntap gs = S.runPure S.identityAnswer (fst (TurnSpec.runTurn S.identityAnswer (fst (TurnSpec.runTurn S.identityAnswer gs)))) Engine.runStep
    case Set.toList (Set.difference (GameState.battlefield down) (GameState.battlefield board)) of
      [permanent] -> do
        let armed = S.runPure S.identityAnswer down (FaceDown.turnFaceUp S.manaPerformer S.carol TurnUpProcedure.Morph permanent >> Engine.priorityLoop)
            first_ = nextUntap armed
            second = nextUntap first_
        Spec.assertEqWith s "the team's next untap step was skipped" (fmap (`tapped` first_) [alices, bobs]) [True, True]
        Spec.assertEqWith s "and the one after untapped both" (fmap (`tapped` second) [alices, bobs]) [False, False]
      _ -> Spec.assertFailure s "the face-down cast did not reach the battlefield"
  -- CR 805.8: controlling a player controls their team. Carol activates
  -- Mindslaver at bob on her own turn, so on alice's team's next turn carol makes
  -- alice's decisions too -- and, deciding alice's attack, declares nothing.
  -- The control board activates nothing, and alice's Piker attacks carol.
  --
  -- Mindslaver, {6} Legendary Artifact: "{4}, {T}, Sacrifice Mindslaver: You
  -- control target player during that player's next turn."
  Spec.it s "CR 805.8 controlling a player controls their team" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    mindslaver <- S.printingOf s registry "Mindslaver"
    let run activating =
          let (_, withPiker) = S.addPermanent piker S.alice (stockedWith island sharedTurns)
              (slaver, placed) = S.addPermanent mindslaver S.carol withPiker
              lands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.carol g)) placed [1 .. (4 :: Int)]
              board = lands {GameState.phase = Phase.PrecombatMain, GameState.remaining = S.phasesAfter Phase.PrecombatMain, GameState.activePlayer = S.carol, GameState.priority = Just S.carol}
              armed =
                if activating
                  then S.runPure (PreventionSpec.aimPlayer S.bob) board (Activate.activateAbility S.carol slaver (ActivateSpec.theAbility mindslaver) >> Stack.resolveTop)
                  else board
              -- Carol declines to attack with anything she decides for; alice
              -- attacks with everything.
              deciding :: Prompt.Prompt r -> r
              deciding p = case p of
                Prompt.DeclareAttackers decider _ ids -> if decider == Decider.MkDecider S.carol then [] else ids
                _ -> S.attackTo S.carol p
              -- The rest of carol's turn, then alice's team's whole turn.
              after = fst (TurnSpec.runTurn deciding (fst (TurnSpec.runTurn deciding armed)))
           in (fmap (\pid -> Map.member pid (GameState.pendingControl armed)) [S.alice, S.bob], S.lifeOf S.carol after)
    Spec.assertEqWith s "carol controlled alice's attack, so no Piker hit her" (run True) ([True, True], Just 20)
    Spec.assertEqWith s "without Mindslaver alice's Piker dealt carol 2" (run False) ([False, False], Just 18)
  -- CR 726.4 / 805.2: the same choice for the initiative, and the taker ventures
  -- into Undercity (CR 726.2).
  Spec.it s "CR 726.4 the active team's primary player names who takes the initiative" $ do
    undercity <- S.printingOf s registry "Undercity"
    let run option =
          let board = S.withInitiative S.carol (InitiativeSpec.owningUndercity undercity [S.alice, S.bob, S.carol, S.dave] (option (daveAliceTeams S.fourPlayerGame)))
              (chosen, gone) = departNaming S.dave S.carol board
              settled = InitiativeSpec.resolveAll InitiativeSpec.answering gone
           in (fmap (`InitiativeSpec.dungeonNamesOf` settled) [S.alice, S.dave], GameState.initiative gone, chosen)
        (dungeons, holder, asked) = run sharedTurns
        (dungeonsAlone, holderAlone, askedAlone) = run id
    Spec.assertEqWith s "dave ventured into Undercity, alice did not" dungeons [[], ["\"Undercity\""]]
    Spec.assertEqWith s "dave has the initiative" holder (Just S.dave)
    Spec.assertEqWith s "dave, the primary player, chose between alice and him" asked [(S.dave, [S.alice, S.dave])]
    Spec.assertEqWith s "without the option alice ventured, dave did not" dungeonsAlone [["\"Undercity\""], []]
    Spec.assertEqWith s "and alice has the initiative" holderAlone (Just S.alice)
    Spec.assertEqWith s "and nobody was asked" askedAlone []

-- Dave and alice against bob and carol, alice's turn. Their team wraps the turn
-- order, so dave is its primary player (CR 805.2) while alice is the turn's seat.
daveAliceTeams :: GameState.GameState -> GameState.GameState
daveAliceTeams = S.inTeams [[S.dave, S.alice], [S.bob, S.carol]]

-- `leaving` concedes, every CR 725.4 / 726.4 active-player choice naming `named`
-- where offered; the answer is each choice's (chooser, candidates).
departNaming :: PlayerId.PlayerId -> PlayerId.PlayerId -> GameState.GameState -> ([(PlayerId.PlayerId, [PlayerId.PlayerId])], GameState.GameState)
departNaming named leaving board =
  let record :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, [PlayerId.PlayerId])] r
      record p = case p of
        Prompt.ChooseActivePlayer _ pid offer -> do
          State.modify' (<> [(pid, NonEmpty.toList offer)])
          pure (Maybe.fromMaybe (NonEmpty.head offer) (List.find (== named) (NonEmpty.toList offer)))
        _ -> pure (S.identityAnswer p)
      ((_, gone), asked) = State.runState (Engine.runGame record board (Departure.depart Departure.Conceded leaving)) []
   in (asked, gone)

-- alice's beginning of combat step, the rest of her turn to come.
atCombat :: GameState.GameState -> GameState.GameState
atCombat gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
      GameState.priority = Just S.alice,
      GameState.remaining = S.phasesAfter (Phase.Combat CombatStep.BeginningOfCombat)
    }
