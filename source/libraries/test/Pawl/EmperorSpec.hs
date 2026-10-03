{-# LANGUAGE GADTs #-}

-- Covers: CR 809's Emperor variant -- Pawl.Engine.Emperor's setUp (CR 809.3,
-- 809.6a), Pawl.Engine.Setup.randomEmperorFirst (CR 809.4) and fallsWith (CR 809.5b, 809.5c, read by Pawl.Engine.Departure's
-- departTogether), Pawl.Engine.Combat's attackableOpponents under
-- AttackOption.Adjacent (CR 809.3c) over Pawl.Engine.Game.neighbours, and
-- Pawl.Engine.Departure's outcomeAfterLeaving for a team (CR 104.2c).
--
-- SIX SEATS, turn order [alice, bob, carol, dave, erin, frank]: alice, bob and
-- carol are one team with bob its emperor, dave, erin and frank the other with
-- erin its emperor. Each emperor sits between two generals of their own.
module Pawl.EmperorSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Emperor as Emperor
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Emperors as Emperors
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TeamId as TeamId
import qualified Pawl.Types.Teams as Teams

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Emperor" $ do
  -- CR 809.3a through CR 809.6a's minima: a general reaches the nearest
  -- opposing general, an emperor the second nearest.
  Spec.it s "CR 809.3a emperors have range 2 and generals range 1" $
    Spec.assertEqWith
      s
      "CR 809.3a"
      (RangeOfInfluence.unwrap (GameSettings.rangeOfInfluence (GameState.settings emperorGame)))
      (Map.fromList [(S.alice, 1), (S.bob, 2), (S.carol, 1), (S.dave, 1), (erin, 2), (frank, 1)])

  -- CR 809.6a: two teams of four, each emperor in its team's second seat. The
  -- emperor at seat 1 has opposing generals 2, 3 and 3 seats away, so needs 3;
  -- the general at seat 2 has none nearer than 2.
  Spec.it s "CR 809.6a larger teams take the minimum ranges" $ do
    let eight = fmap PlayerId.MkPlayerId [0 .. 7]
        byTeam = Teams.MkTeams (Map.fromList (zip eight (fmap TeamId.MkTeamId [0, 0, 0, 0, 1, 1, 1, 1])))
        seconds = Emperors.MkEmperors (Map.fromList [(TeamId.MkTeamId 0, PlayerId.MkPlayerId 1), (TeamId.MkTeamId 1, PlayerId.MkPlayerId 5)])
    Spec.assertEqWith
      s
      "CR 809.6a"
      (Emperor.ranges byTeam seconds eight)
      (Map.fromList (zip eight [1, 3, 2, 1, 1, 3, 2, 1]))

  -- CR 809.5b / 104.2c: erin, an emperor, is at 0 life. Her team loses with her
  -- and bob's team, the last one playing, wins. The paired board puts frank, a
  -- general, at 0 instead: he leaves alone and the game goes on.
  -- CR 809.4 through the whole of Setup.newGame: the random draw is answered
  -- with its second candidate, pinned by position, so erin goes first and turn
  -- order runs on to her left. Recorded: the draw offered the emperors alone.
  Spec.it s "CR 809.4 a randomly determined emperor goes first" $ do
    forest <- S.printingOf s registry "Forest"
    let deck = Deck.fromCards (Map.singleton forest 20)
        matchup = fmap (\pid -> (pid, deck)) (S.alice NonEmpty.:| drop 1 seats)
        ((_, started), drawnFrom) = State.runState (Engine.runGame secondDrawn emperorGame (Setup.newGame S.performer matchup)) []
    Spec.assertEqWith s "CR 809.4 erin takes the first turn, turn order to her left" (GameState.activePlayer started, GameState.turnOrder started) (erin, [erin, frank, S.alice, S.bob, S.carol, S.dave])
    Spec.assertEqWith s "the draw was among the emperors alone" drawnFrom [[S.bob, erin]]

  Spec.it s "CR 809.5b a team loses the game if its emperor loses" $ do
    let atZero pid = S.settleSba emperorGame {GameState.players = Map.adjust (\p -> p {Player.life = 0}) pid (GameState.players emperorGame)}
    Spec.assertEqWith s "CR 809.5b erin's generals leave with her" (Game.stillPlaying (atZero erin)) [S.alice, S.bob, S.carol]
    Spec.assertEqWith s "CR 104.2c and bob's team wins" (GameState.result (atZero erin)) (Just (Result.TeamWon (TeamId.MkTeamId 0)))
    Spec.assertEqWith s "a general leaves alone" (Game.stillPlaying (atZero frank)) [S.alice, S.bob, S.carol, S.dave, erin]
    Spec.assertEqWith s "and the game goes on" (GameState.result (atZero frank)) Nothing
    -- CR 104.3a: an emperor who concedes takes the team with her, and it is the
    -- team that loses.
    Spec.assertEqWith
      s
      "CR 809.5b dave loses when erin concedes"
      (fmap Player.status (Map.lookup S.dave (GameState.players (S.departs Departure.Type.Conceded erin emperorGame))))
      (Just (Status.Departed Departure.Type.Lost))

  -- CR 809.5c / 801.15: frank casts Divine Intervention ("... When you remove
  -- the last intervention counter from this enchantment, the game is a draw.")
  -- and two of his upkeeps pass. The draw is for frank and, at his range of 1,
  -- erin and alice; erin's draw is her team's, so dave draws too. Bob and carol
  -- are left, one team, and win.
  Spec.it s "CR 809.5c the game is a draw for a team if it is a draw for its emperor" $ do
    plains <- S.printingOf s registry "Plains"
    intervention <- S.printingOf s registry "Divine Intervention"
    let (spellId, g0) = S.addHandCard intervention frank (S.landsFor plains frank 8 emperorGame)
        played = castResolved frank spellId (onMainOf frank g0)
        drawn = upkeepOf frank (upkeepOf frank played)
    Spec.assertEqWith s "CR 809.5c dave draws with erin" (Game.stillPlaying drawn) [S.bob, S.carol]
    Spec.assertEqWith
      s
      "CR 809.5c as a draw"
      (fmap Player.status (Map.lookup S.dave (GameState.players drawn)))
      (Just (Status.Departed Departure.Type.Drew))
    Spec.assertEqWith s "CR 104.2c bob's team wins" (GameState.result drawn) (Just (Result.TeamWon (TeamId.MkTeamId 0)))
  where
    erin = PlayerId.MkPlayerId 4
    frank = PlayerId.MkPlayerId 5
    seats = [S.alice, S.bob, S.carol, S.dave, erin, frank]
    teams = Teams.MkTeams (Map.fromList (zip seats (fmap TeamId.MkTeamId [0, 0, 0, 1, 1, 1])))
    emperors = Emperors.MkEmperors (Map.fromList [(TeamId.MkTeamId 0, S.bob), (TeamId.MkTeamId 1, erin)])
    emperorGame = Emperor.setUp teams emperors (Setup.emptyGame (S.alice NonEmpty.:| drop 1 seats))
    resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
    onMainOf pid gs = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = pid, GameState.priority = Just pid}
    castResolved :: PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
    castResolved pid spellId gs = resolveAll (S.runPure S.identityAnswer gs (S.cast pid spellId))
    -- CR 503.1: `pid`'s upkeep begins and its triggers resolve.
    upkeepOf pid gs =
      let upkeep = Phase.Beginning BeginningStep.Upkeep
          began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
       in resolveAll (S.runPure S.identityAnswer began Engine.settleForPriority)

-- Answers every CR 809.4 draw with its second candidate, recording each
-- candidate list; everything else is S.identityAnswer's.
secondDrawn :: Prompt.Prompt r -> State.State [[PlayerId.PlayerId]] r
secondDrawn p = case p of
  Prompt.RandomFirstPlayer candidates -> do
    State.modify' (<> [NonEmpty.toList candidates])
    pure $ case candidates of
      _ NonEmpty.:| (second : _) -> second
      first NonEmpty.:| [] -> first
  _ -> pure (S.identityAnswer p)
