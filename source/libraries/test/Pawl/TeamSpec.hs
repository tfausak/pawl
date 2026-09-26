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

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.ActivateSpec as ActivateSpec
import qualified Pawl.Engine.Action as Action
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
import qualified Pawl.Engine.Projection.View as Projection
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
import qualified Pawl.Types.OptionalDecision as OptionalDecision
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
  -- CR 804.2 through a PAIR of boards differing only in the option. alice,
  -- bob and carol against dave, so alice has two teammates to choose between
  -- and the offer separates CR 102.3's teammates from every other seat; carol is
  -- chosen rather than the first offered, so the control change reads the
  -- target rather than a default. The Piker prints no activated ability, so
  -- whatever alice can activate on it is the one CR 804.2 gives it.
  Spec.it s "CR 804.2 a creature taps to hand itself to a teammate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (mine, staged) = S.addPermanent piker S.alice (S.inTeams [[S.alice, S.bob, S.carol], [S.dave]] S.fourPlayerGame)
        board deploy =
          staged
            { GameState.settings = (GameState.settings staged) {GameSettings.deployCreatures = deploy},
              GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        offers gs = [ability | Action.Activate oid ability <- Action.legalActions S.alice gs, oid == mine]
        recording :: Prompt.Prompt r -> State.State [[Recipient.Recipient]] r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.modify' (<> fmap (Set.toAscList . snd) (Map.elems sets))
            pure (S.preferring (== Recipient.ToPlayer S.carol) sets)
          _ -> pure (S.identityAnswer p)
        step gs ability = do
          (_, activated) <- Engine.runGame recording gs (Activate.activateAbility S.alice mine ability)
          snd <$> Engine.runGame recording activated Stack.resolveTop
        (after, offered) = State.runState (Foldable.foldlM step (board True) (offers (board True))) []
    Spec.assertEqWith s "CR 804.2 carol controls alice's Piker" (Projection.controllerOf mine after) (Just S.carol)
    Spec.assertEqWith s "CR 102.3 only alice's teammates are offered" offered [[Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol]]
    Spec.assertEqWith s "CR 804.1 without the option the Piker has nothing to activate" (length (offers (board False))) 0
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
  -- CR 805.4d: "each player's upkeep" reading "that player" triggers once per
  -- active player. Carol controls it, so "that player" is neither her nor alone
  -- the seat the turn began with.
  --
  -- Elkin Lair, {3}{R} World Enchantment: "At the beginning of each player's
  -- upkeep, that player exiles a card at random from their hand. ..." Each hand
  -- holds one card, so the random pick is forced.
  Spec.it s "CR 805.4d an each-player's-upkeep trigger fires once per active teammate" $ do
    island <- S.printingOf s registry "Island"
    lair <- S.printingOf s registry "Elkin Lair"
    let run option =
          let (_, placed) = S.addPermanent lair S.carol (stockedWith island option)
              board = List.foldl' (\g pid -> snd (S.addHandCard island pid g)) placed [S.alice, S.bob]
              after = fst (TurnSpec.runTurn S.identityAnswer board)
           in fmap (\pid -> length (Game.zoneMembers Zone.Exile pid after)) [S.alice, S.bob]
    Spec.assertEqWith s "alice and bob each exiled their card" (run sharedTurns) [1, 1]
    Spec.assertEqWith s "without the option only alice did" (run id) [1, 0]
  -- CR 805.4d / 603.4: the intervening "if" is asked of each opponent in turn.
  -- Only bob holds seven, so a single trigger naming alice would do nothing.
  --
  -- Ebony Owl Netsuke, {2} Artifact: "At the beginning of each opponent's
  -- upkeep, if that player has seven or more cards in hand, this artifact deals
  -- 4 damage to that player."
  Spec.it s "CR 805.4d an each-opponent's-upkeep trigger asks its if of each active opponent" $ do
    island <- S.printingOf s registry "Island"
    owl <- S.printingOf s registry "Ebony Owl Netsuke"
    let run option =
          let (_, placed) = S.addPermanent owl S.carol (stockedWith island option)
              hands = List.foldl' (\g _ -> snd (S.addHandCard island S.alice g)) placed [1 :: Int .. 2]
              board = List.foldl' (\g _ -> snd (S.addHandCard island S.bob g)) hands [1 :: Int .. 8]
              after = fst (TurnSpec.runTurn S.identityAnswer board)
           in fmap (`S.lifeOf` after) [S.alice, S.bob]
    Spec.assertEqWith s "bob took 4, alice nothing" (run sharedTurns) [Just 20, Just 16]
    Spec.assertEqWith s "without the option bob's upkeep has not come" (run id) [Just 20, Just 20]
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
  -- CR 805.10a: without the attack multiple players option CR 507.1 asks the
  -- active player to choose ONE defending player; under the shared team turns
  -- option the whole nonactive team defends and nobody is asked.
  Spec.it s "CR 805.10a the nonactive team is the defending team" $ do
    let run option =
          let teamed = option (twoTeams S.fourPlayerGame)
              board = (atCombat teamed) {GameState.settings = (GameState.settings teamed) {GameSettings.attackOption = Nothing}}
              settled = S.runPure S.identityAnswer board (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
           in Combat.Type.defenders (GameState.combat settled)
    Spec.assertEqWith s "carol and dave both defend" (run sharedTurns) [S.carol, S.dave]
    Spec.assertEqWith s "without the option alice chose carol alone" (run id) [S.carol]
  -- CR 805.10b: the active team has one combined attack, so bob's Goblin Piker
  -- attacks on alice's turn.
  Spec.it s "CR 805.10b a teammate's creature attacks on the team's turn" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let run option =
          let (_, board) = S.addPermanent piker S.bob (atCombat (option (twoTeams S.fourPlayerGame)))
           in S.lifeOf S.carol (S.runCombat (S.attackTo S.carol) board)
    Spec.assertEqWith s "bob's Piker dealt carol 2" (run sharedTurns) (Just 18)
    Spec.assertEqWith s "without the option it was not offered" (run id) (Just 20)
  -- CR 805.10d: the defending team has one combined block, so dave's creature
  -- may block a creature attacking his teammate carol, which CR 802.4a forbids
  -- without the option.
  --
  -- Hill Giant, {3}{R} 3/3 Creature -- Giant.
  Spec.it s "CR 805.10d a teammate's creature blocks an attacker aimed at the team" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    let run option =
          let (mine, staged) = S.addPermanent piker S.alice (atCombat (option (twoTeams S.fourPlayerGame)))
              (_, board) = S.addPermanent giant S.dave staged
              after = S.runCombat (S.attackTo S.carol) board
           in (S.lifeOf S.carol after, S.onBattlefield mine after)
    Spec.assertEqWith s "dave's Giant blocked and killed the Piker" (run sharedTurns) (Just 20, False)
    Spec.assertEqWith s "without the option the Piker got through" (run id) (Just 18, True)
  -- CR 805.10b / 508.1j: each attacking player pays the toll on their own
  -- creatures. Bob's Forests pay for bob's Piker; alice has no mana, so a toll
  -- charged to her would rewind the attack.
  --
  -- Ghostly Prison, {2}{W} Enchantment: "Creatures can't attack you unless
  -- their controller pays {2} for each creature they control that's attacking
  -- you."
  Spec.it s "CR 805.10b a teammate pays the toll on their own attacker" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    let run option =
          let (_, staged) = S.addPermanent piker S.bob (atCombat (option (twoTeams S.fourPlayerGame)))
              (_, imprisoned) = S.addPermanent prison S.carol staged
              board = List.foldl' (\g _ -> snd (S.addPermanent forest S.bob g)) imprisoned [1 :: Int, 2]
              after = S.runCombat (S.attackTo S.carol) board
           in (S.lifeOf S.carol after, S.tappedCount S.bob after)
    Spec.assertEqWith s "bob tapped both Forests and his Piker, which dealt carol 2" (run sharedTurns) (Just 18, 3)
    Spec.assertEqWith s "without the option nothing attacked or paid" (run id) (Just 20, 0)
  -- CR 805.10a / 508.3d: bob is an attacking player, so "whenever you attack"
  -- triggers for him when a creature he controls is declared.
  --
  -- Boggart Prankster, {1}{B} 1/3 Creature -- Goblin Warrior: "Whenever you
  -- attack, target attacking Goblin you control gets +1/+0 until end of turn."
  Spec.it s "CR 508.3d a teammate's whenever-you-attack trigger fires" $ do
    prankster <- S.printingOf s registry "Boggart Prankster"
    let run option =
          let (_, board) = S.addPermanent prankster S.bob (atCombat (option (twoTeams S.fourPlayerGame)))
           in S.lifeOf S.carol (S.runCombat (S.attackTo S.carol) board)
    Spec.assertEqWith s "bob's Prankster pumped itself and dealt carol 2" (run sharedTurns) (Just 18)
    Spec.assertEqWith s "without the option it was not offered" (run id) (Just 20)
  -- CR 805.10a / 701.43a: bob exerts his own attacker, so it is his next untap
  -- step that it skips -- his team's next turn, where his tapped Forest untaps
  -- beside it. The pair differs only in the exert.
  --
  -- Glory-Bound Initiate, {1}{W} 3/1 Creature -- Human Warrior: "You may exert
  -- this creature as it attacks. When you do, it gets +1/+3 and gains lifelink
  -- until end of turn."
  Spec.it s "CR 701.43a a teammate's exerted attacker skips his next untap step" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    initiate <- S.printingOf s registry "Glory-Bound Initiate"
    let exerting :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
        exerting decision p = case p of
          Prompt.ChooseExert {} -> decision
          _ -> S.attackTo S.carol p
        tapped oid gs = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Tapped
        run decision =
          let (mine, staged) = S.addPermanent initiate S.bob (atCombat (stockedWith island sharedTurns))
              (witness, placed) = S.addPermanent forest S.bob staged
              after = S.runCombat (exerting decision) (S.tapObject witness placed)
              -- The rest of alice's turn, carol's, then the untap step of
              -- alice's team's next one.
              later = snd (Engine.runGamePure (exerting decision) (fst (TurnSpec.runTurn (exerting decision) (fst (TurnSpec.runTurn (exerting decision) after)))) Engine.runStep)
           in (S.lifeOf S.carol after, tapped mine later, tapped witness later)
    Spec.assertEqWith s "the exerted Initiate dealt carol 4 and stayed tapped while bob's Forest untapped" (run OptionalDecision.Exercises) (Just 16, True, False)
    Spec.assertEqWith s "declined, it dealt 3 and untapped beside the Forest" (run OptionalDecision.Declines) (Just 17, False, False)
  -- CR 805.10a / 702.154a: bob enlists a creature he controls, which is a
  -- creature of an attacking player.
  --
  -- Yavimaya Steelcrusher, {1}{R} 2/2 Creature -- Ape Warrior: "Enlist (As this
  -- creature attacks, you may tap a nonattacking creature you control without
  -- summoning sickness. When you do, add its power to this creature's until end
  -- of turn.)" Hill Giant, 3/3, stays home to be enlisted.
  Spec.it s "CR 702.154a a teammate enlists a creature he controls" $ do
    steelcrusher <- S.printingOf s registry "Yavimaya Steelcrusher"
    giant <- S.printingOf s registry "Hill Giant"
    let run option =
          let (ape, staged) = S.addPermanent steelcrusher S.bob (atCombat (option (twoTeams S.fourPlayerGame)))
              (_, board) = S.addPermanent giant S.bob staged
              enlisting :: Prompt.Prompt r -> r
              enlisting p = case p of
                Prompt.DeclareAttackers _ _ ids -> filter (== ape) ids
                Prompt.ChooseEnlist _ _ _ offer -> Just (NonEmpty.head offer)
                _ -> S.attackTo S.carol p
           in S.lifeOf S.carol (S.runCombat enlisting board)
    Spec.assertEqWith s "the Steelcrusher took the Giant's 3 and dealt carol 5" (run sharedTurns) (Just 15)
    Spec.assertEqWith s "without the option it was not offered" (run id) (Just 20)
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
  -- CR 805.10a / 506.3b: bob is an attacking player, so tokens he is told to put
  -- onto the battlefield attacking do attack.
  --
  -- Hanweir Garrison, {2}{R} 2/3 Creature -- Human Soldier: "Whenever this
  -- creature attacks, create two 1/1 red Human creature tokens that are tapped
  -- and attacking."
  Spec.it s "CR 805.10a a teammate's tokens enter attacking" $ do
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let run option =
          let (_, board) = S.addPermanent garrison S.bob (atCombat (option (twoTeams S.fourPlayerGame)))
           in S.lifeOf S.carol (S.runCombat (S.attackTo S.carol) board)
    Spec.assertEqWith s "the Garrison and both Humans dealt carol 4" (run sharedTurns) (Just 16)
    Spec.assertEqWith s "without the option the Garrison was not offered" (run id) (Just 20)
  -- CR 805.10c / 207.2c: raid asks whether YOU attacked, and a teammate's attack
  -- is not yours. Bob casts the Skullhunter after combat; the legs differ only
  -- in whose Piker attacked.
  --
  -- Mardu Skullhunter, {1}{B} 2/1 Creature -- Human Warrior: "This creature
  -- enters tapped. Raid -- When this creature enters, if you attacked this turn,
  -- target opponent discards a card."
  Spec.it s "CR 805.10c raid reads the teammate's own attack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    skullhunter <- S.printingOf s registry "Mardu Skullhunter"
    let run attacker =
          let (alices, staged) = S.addPermanent piker S.alice (atCombat (twoTeams S.fourPlayerGame))
              (bobs, placed) = S.addPermanent piker S.bob staged
              lands = List.foldl' (\g _ -> snd (S.addPermanent swamp S.bob g)) placed [1 :: Int, 2]
              (_, held) = S.addHandCard skullhunter S.bob lands
              board = sharedTurns (List.foldl' (\g pid -> snd (S.addHandCard island pid g)) held [S.carol, S.dave])
              chosen = if attacker == S.alice then alices else bobs
              raiding :: Prompt.Prompt r -> r
              raiding p = case p of
                Prompt.DeclareAttackers _ _ ids -> filter (== chosen) ids
                Prompt.ChooseAction {} -> S.castAnswer p
                _ -> S.attackTo S.carol p
              after = S.runPure raiding (S.runCombat raiding board) Engine.runStep
           in (S.handSize S.carol after + S.handSize S.dave after, S.countOnBattlefieldByName (S.printingName skullhunter) S.bob after)
    Spec.assertEqWith s "bob attacked, so his raid made an opponent discard" (run S.bob) (1, 1)
    Spec.assertEqWith s "only alice attacked, so bob's Skullhunter entered and nobody discarded" (run S.alice) (2, 1)
  -- CR 805.10c / 702.121a: melee counts the opponents YOU attacked. Bob's Wings
  -- attack carol; alice's Piker attacks dave in one leg and carol in the other,
  -- and the Wings are 2/2 in both.
  --
  -- Wings of the Guard, {1}{W} 1/1 Creature -- Bird: "Flying. Melee (Whenever
  -- this creature attacks, it gets +1/+1 until end of turn for each opponent you
  -- attacked this combat.)"
  Spec.it s "CR 805.10c melee counts the teammate's own opponents" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    wings <- S.printingOf s registry "Wings of the Guard"
    let run pikerAt =
          let (alices, staged) = S.addPermanent piker S.alice (atCombat (sharedTurns (twoTeams S.fourPlayerGame)))
              (_, board) = S.addPermanent wings S.bob staged
              aiming :: Prompt.Prompt r -> r
              aiming p = case p of
                Prompt.ChooseAttackTarget _ _ oid options ->
                  let who = if oid == alices then pikerAt else S.carol
                   in Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer who) (NonEmpty.toList options))
                _ -> S.attackTo S.carol p
              after = S.runCombat aiming board
           in (S.lifeOf S.carol after, S.lifeOf S.dave after)
    Spec.assertEqWith s "the Wings dealt carol 2 while alice's Piker dealt dave 2" (run S.dave) (Just 18, Just 18)
    Spec.assertEqWith s "with both at carol she took 4" (run S.carol) (Just 16, Just 20)
  -- CR 805.10c / 508.3c: "you attack with two or more creatures" counts only
  -- the creatures you control. Bob attacks beside alice with one Piker in one
  -- leg and with two in the other.
  --
  -- Military Intelligence, {1}{U} Enchantment: "Whenever you attack with two or
  -- more creatures, draw a card."
  Spec.it s "CR 508.3c a teammate's attack-with-two counts only his creatures" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    intelligence <- S.printingOf s registry "Military Intelligence"
    let run bobs =
          let (_, staged) = S.addPermanent intelligence S.bob (atCombat (stockedWith island sharedTurns))
              withAlice = snd (S.addPermanent piker S.alice staged)
              board = List.foldl' (\g _ -> snd (S.addPermanent piker S.bob g)) withAlice [1 .. bobs]
           in S.handSize S.bob (S.runCombat (S.attackTo S.carol) board)
    Spec.assertEqWith s "one Piker of bob's beside alice's draws nothing" (run (1 :: Int)) 0
    Spec.assertEqWith s "two of bob's draw a card" (run 2) 1
  -- CR 805.8: a skip naming bob is his team's, so Fatigue aimed at him takes
  -- the draw step of alice's team's next turn from both of them. The control
  -- casts nothing.
  --
  -- Fatigue, {1}{U} Sorcery: "Target player skips their next draw step."
  Spec.it s "CR 805.8 a teammate's skip is the team's" $ do
    island <- S.printingOf s registry "Island"
    fatigue <- S.printingOf s registry "Fatigue"
    let run casting =
          let (held, staged) = S.addHandCard fatigue S.alice (S.landsFor island S.alice 2 (stockedWith island sharedTurns))
              board = staged {GameState.phase = Phase.PrecombatMain, GameState.remaining = S.phasesAfter Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
              cast = if casting then PreventionSpec.castEach (PreventionSpec.aimPlayer S.bob) board [held] else board
              -- The rest of alice's turn and carol's, then the untap, upkeep and
              -- draw steps of alice's team's next.
              next = fst (TurnSpec.runTurn S.identityAnswer (fst (TurnSpec.runTurn S.identityAnswer cast)))
              drawn = S.runPure S.identityAnswer next (Monad.replicateM_ 3 Engine.runStep)
           in fmap (\pid -> S.handSize pid drawn - S.handSize pid next) [S.alice, S.bob]
    Spec.assertEqWith s "neither alice nor bob drew" (run True) [0, 0]
    Spec.assertEqWith s "without Fatigue both drew" (run False) [1, 1]
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
  -- CR 702.22k / 805.9: with bob's banding Hero among the creatures carol's
  -- Brigade blocks, an active player divides the Brigade's damage, and bob, the
  -- banding ability's controller, names which. He names himself, then alice.
  -- Without the option bob's creatures cannot attack on alice's turn, so the
  -- control gives alice them: she is the one active player, and nobody is asked.
  -- With a Hero each, the two banding controllers leave the choice to their
  -- team, and CR 805.2 gives it to alice, the team's primary player.
  --
  -- Benalish Hero, {W} 1/1 Creature -- Human Soldier, banding. Foriysian
  -- Brigade, {3}{W} 2/4 Creature -- Human Soldier, "This creature can block an
  -- additional creature each combat."
  Spec.it s "CR 805.9 the banding creature's controller names the active player who divides" $ do
    hero <- S.printingOf s registry "Benalish Hero"
    piker <- S.printingOf s registry "Goblin Piker"
    brigade <- S.printingOf s registry "Foriysian Brigade"
    let run option owner (other, otherOwner) named =
          let (heroId, g1) = S.addPermanent hero owner (atCombat (option (twoTeams S.fourPlayerGame)))
              (otherId, g2) = S.addPermanent other otherOwner g1
              (_, board) = S.addPermanent brigade S.carol g2
              blocked = Set.fromList [heroId, otherId]
              record :: Prompt.Prompt r -> State.State [(Bool, PlayerId.PlayerId)] r
              record p = case p of
                Prompt.ChoosePlayer _ pid _ offer -> do
                  State.modify' (<> [(False, pid)])
                  pure (Maybe.fromMaybe (NonEmpty.head offer) (List.find (== named) (NonEmpty.toList offer)))
                Prompt.AssignCombatDamage _ pid _ thresholds n -> do
                  State.modify' (<> [(True, pid)])
                  pure $ case filter S.isCreatureRecipient (Map.keys thresholds) of
                    r : _ -> Map.singleton r n
                    [] -> Map.empty
                Prompt.DeclareBlockers _ _ mine _ -> pure (Map.fromList (fmap (\b -> (b, blocked)) mine))
                _ -> pure (S.attackTo S.carol p)
           in snd (State.runState (Engine.runGame record board S.combatGame) [])
    -- Each entry is (was it the division, who was asked); False is CR 805.9's
    -- choice of active player.
    Spec.assertEqWith s "bob named himself, so bob divided" (run sharedTurns S.bob (piker, S.bob) S.bob) [(False, S.bob), (True, S.bob)]
    Spec.assertEqWith s "bob named alice, so alice divided" (run sharedTurns S.bob (piker, S.bob) S.alice) [(False, S.bob), (True, S.alice)]
    Spec.assertEqWith s "without the option alice divided unasked" (run id S.alice (piker, S.alice) S.bob) [(True, S.alice)]
    Spec.assertEqWith s "a Hero each, so alice named bob" (run sharedTurns S.bob (hero, S.alice) S.bob) [(False, S.alice), (True, S.bob)]
  -- CR 725.4 / 805.2: carol, the monarch, concedes before alice's team's turn.
  -- No rule names which active player takes the crown, so the team decides and
  -- dave, its primary player, names himself over alice, the turn's seat. He then
  -- draws at the team's end step (CR 725.2). Without the option alice is the one
  -- active player and takes it unasked.
  Spec.it s "CR 725.4 the active team's primary player names the new monarch" $ do
    island <- S.printingOf s registry "Island"
    let run option =
          let teamed = option (daveAliceTeams S.fourPlayerGame)
              stockOne gs pid = List.foldl' (\g _ -> snd (S.addLibraryCard island pid g)) gs [1 :: Int .. 4]
              board = S.withMonarch S.carol (List.foldl' stockOne teamed [S.alice, S.bob, S.carol, S.dave])
              (chosen, gone) = departNaming S.dave S.carol board
              after = fst (TurnSpec.runTurn S.identityAnswer gone)
           in (fmap (`S.handSize` after) [S.alice, S.dave], GameState.monarch gone, chosen)
        (hands, monarch, asked) = run sharedTurns
        (handsAlone, monarchAlone, askedAlone) = run id
    Spec.assertEqWith s "dave drew his draw-step card and the monarch's, alice one" hands [1, 2]
    Spec.assertEqWith s "dave is the monarch" monarch (Just S.dave)
    Spec.assertEqWith s "dave, the primary player, chose between alice and him" asked [(S.dave, [S.alice, S.dave])]
    Spec.assertEqWith s "without the option alice drew both, dave none" handsAlone [2, 0]
    Spec.assertEqWith s "and alice is the monarch" monarchAlone (Just S.alice)
    Spec.assertEqWith s "and nobody was asked" askedAlone []
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
