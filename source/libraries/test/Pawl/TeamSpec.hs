{-# LANGUAGE GADTs #-}

-- Covers: CR 102.3's teammates and CR 808's Team vs. Team variant --
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
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.SpeedSpec as SpeedSpec
import qualified Pawl.Support as S
import qualified Pawl.TurnSpec as TurnSpec
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Departure as Departure
import qualified Pawl.Types.EndingStep as EndingStep
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
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
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
  -- CR 102.3 through the effect DSL's PlayerRef.Relative Opponent, which
  -- Pawl.Engine.Resolve.Slots.playerRefPlayers resolves.
  --
  -- Prologue to Phyresis, {1}{U} Instant: "Each opponent gets a poison counter.
  -- Draw a card." The whole card is that one instruction plus a draw, so nothing
  -- else can move a counter, and the four totals are read as ONE list so that no
  -- seat's answer can be checked without the others.
  Spec.it s "CR 102.3 each opponent skips the teammate" $ do
    prologue <- S.printingOf s registry "Prologue to Phyresis"
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    let lands = S.landsFor island S.alice 2 (twoTeams S.fourPlayerGame)
        stocked = snd (S.addLibraryCard plains S.alice lands)
        (held, staged) = S.addHandCard prologue S.alice stocked
        board =
          staged
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        cast = S.runPure S.identityAnswer board (S.cast S.alice held)
        after = S.runPure S.identityAnswer cast Engine.priorityLoop
    Spec.assertEqWith
      s
      "CR 102.3 only carol and dave are poisoned"
      (fmap (\pid -> S.playerCounterOf PlayerCounterKind.Poison pid after) [S.alice, S.bob, S.carol, S.dave])
      [0, 0, 1, 1]
    Spec.assertEqWith s "the spell resolved" (GameState.stack after) []
  -- CR 102.3 through a TARGET SLOT, which neither case above reaches: the offer
  -- comes from Pawl.Engine.Target's Filter.IsPlayer atom rather than from a
  -- PlayerRef, and it is filtered against a Context built by Target.slotContext.
  --
  -- Ravenous Rats, {1}{B} Rat: "When this creature enters, target opponent
  -- discards a card." The OFFER is what is asserted, and it is the engine's own
  -- output: an answer naming bob would be filtered out rather than obeyed, so
  -- reading the offer is what distinguishes a slot that never admitted him.
  Spec.it s "CR 102.3 a target opponent slot does not offer the teammate" $ do
    rats <- S.printingOf s registry "Ravenous Rats"
    swamp <- S.printingOf s registry "Swamp"
    let lands = S.landsFor swamp S.alice 2 (twoTeams S.fourPlayerGame)
        (held, staged) = S.addHandCard rats S.alice lands
        board =
          staged
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        recording :: Prompt.Prompt r -> State.State [[Recipient.Recipient]] r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.modify' (<> fmap (Set.toAscList . snd) (Map.elems sets))
            -- The slot's own announced number, not the whole offer: CR 603.3d
            -- judges a trigger's announcement for its COUNT as well, and "target
            -- opponent" answered with both would be re-asked rather than obeyed.
            pure (S.preferring (const True) sets)
          _ -> pure (S.identityAnswer p)
        cast = S.runPure S.identityAnswer board (S.cast S.alice held)
        offered = State.execState (Engine.runGame recording cast Engine.priorityLoop) []
    Spec.assertEqWith
      s
      "CR 102.3 only carol and dave are offered"
      offered
      [[Recipient.ToPlayer S.carol, Recipient.ToPlayer S.dave]]
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
  -- CR 102.3 through a Count over the same reference, which
  -- Pawl.Engine.Count.playersFor resolves and which no effect above reaches.
  --
  -- Tyranid Invasion, {3}{G} Sorcery: "Create a number of 3/3 green Tyranid
  -- Warrior creature tokens with trample equal to the number of opponents you
  -- have." Pawl.CountSpec's group is the same card at three seats with no teams,
  -- where the answer is 2 for a different reason; here three other seats make 2
  -- the team answer and 3 the free-for-all one.
  Spec.it s "CR 102.3 a count of opponents answers two rather than three" $ do
    invasion <- S.printingOf s registry "Tyranid Invasion"
    forest <- S.printingOf s registry "Forest"
    let lands = S.landsFor forest S.alice 4 (twoTeams S.fourPlayerGame)
        (held, staged) = S.addHandCard invasion S.alice lands
        board =
          staged
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        cast = S.runPure S.identityAnswer board (S.cast S.alice held)
        after = S.runPure S.identityAnswer cast Engine.priorityLoop
    Spec.assertEqWith s "one token per opponent, and bob is not one" (length (S.tokensOf after)) 2
    Spec.assertEqWith s "the spell resolved" (GameState.stack after) []
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
  -- CR 805.4b / 502.3: bob is an active player on alice's turn, so he untaps and
  -- draws in it. Carol is not, and does neither.
  Spec.it s "CR 805.4b each player on the active team untaps and draws" $ do
    island <- S.printingOf s registry "Island"
    let run option =
          let (bobs, staged) = S.addPermanent island S.bob (stockedWith island option)
              (carols, placed) = S.addPermanent island S.carol staged
              board = S.tapObject carols (S.tapObject bobs placed)
           in fst (TurnSpec.runTurn S.identityAnswer board)
        after = run sharedTurns
        alone = run id
        observe gs = (fmap (`S.handSize` gs) [S.bob, S.carol], fmap (`S.tappedCount` gs) [S.bob, S.carol])
    Spec.assertEqWith s "bob drew and untapped on alice's turn, carol neither" (observe after) ([1, 0], [0, 1])
    Spec.assertEqWith s "without the option bob does neither" (observe alone) ([0, 0], [1, 1])
  -- CR 805.4c: bob may play a land during his team's turn. The land play is
  -- offered to whoever holds priority, so the one board differs only in the
  -- option.
  Spec.it s "CR 805.4c each player on the active team may play a land" $ do
    forest <- S.printingOf s registry "Forest"
    let run option =
          let (_, staged) = S.addHandCard forest S.bob (option (twoTeams S.fourPlayerGame))
              board =
                staged
                  { GameState.phase = Phase.PrecombatMain,
                    GameState.activePlayer = S.alice,
                    GameState.priority = Just S.alice
                  }
           in S.runPure S.playLandAnswer board Engine.priorityLoop
    Spec.assertEqWith s "bob's Forest is on the battlefield" (S.countOnBattlefieldByName (S.printingName forest) S.bob (run sharedTurns)) 1
    Spec.assertEqWith s "without the option it stays in his hand" (S.countOnBattlefieldByName (S.printingName forest) S.bob (run id)) 0
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
  -- CR 805.4 / 514.1: bob is an active player, so his cleanup discard happens on
  -- alice's turn. He holds eight and draws a ninth; two go.
  Spec.it s "CR 514.1 each player on the active team discards to hand size" $ do
    island <- S.printingOf s registry "Island"
    let run option =
          let board = List.foldl' (\g _ -> snd (S.addHandCard island S.bob g)) (stockedWith island option) [1 :: Int .. 8]
           in S.handSize S.bob (fst (TurnSpec.runTurn S.identityAnswer board))
    Spec.assertEqWith s "bob ends alice's turn at seven" (run sharedTurns) 7
    Spec.assertEqWith s "without the option he keeps eight" (run id) 8
  -- CR 805.4 / 603.2: "your upkeep" is bob's upkeep on his team's turn.
  --
  -- Bitterblossom, {1}{B} Kindred Enchantment: "At the beginning of your upkeep,
  -- you lose 1 life and create a 1/1 black Faerie Rogue creature token with
  -- flying."
  Spec.it s "CR 805.4 a teammate's your-upkeep trigger fires on the team's turn" $ do
    island <- S.printingOf s registry "Island"
    bitterblossom <- S.printingOf s registry "Bitterblossom"
    let run option =
          let (_, board) = S.addPermanent bitterblossom S.bob (stockedWith island option)
              after = fst (TurnSpec.runTurn S.identityAnswer board)
           in (S.lifeOf S.bob after, length (S.tokensOf after))
    Spec.assertEqWith s "bob lost 1 life and made a Faerie" (run sharedTurns) (Just 19, 1)
    Spec.assertEqWith s "without the option nothing happened" (run id) (Just 20, 0)
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
