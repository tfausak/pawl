{-# LANGUAGE GADTs #-}

-- Runs a scenario: places its board, answers the engine's prompts from its
-- timeline, and evaluates its checks. The one boundary through which something
-- other than the rules core says what happens in a game (#146), and the test
-- suite's gameplay harness.
module Pawl.Scenario where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Codec.Check as Codec.Check
import qualified Pawl.Codec.Choices as Codec.Choices
import qualified Pawl.Codec.Move as Codec.Move
import qualified Pawl.Codec.Phase as Codec.Phase
import qualified Pawl.Codec.Reference as Codec.Reference
import qualified Pawl.Codec.Timed as Codec.Timed
import qualified Pawl.Engine.Attach as Attach
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Script as Script
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Registry as Registry
import qualified Pawl.Scenario.Prompt as Prompt
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Activation as Activation
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackersAre as AttackersAre
import qualified Pawl.Types.BlockersAre as BlockersAre
import qualified Pawl.Types.Board as Board
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Casting as Casting
import qualified Pawl.Types.Check as Check
import qualified Pawl.Types.Choices as Choices
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CountIs as CountIs
import qualified Pawl.Types.CountersAre as CountersAre
import qualified Pawl.Types.DamageIs as DamageIs
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.DefendersAre as DefendersAre
import qualified Pawl.Types.Emperors as Emperors
import qualified Pawl.Types.Entry as Entry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.KeywordsAre as KeywordsAre
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.LifeIs as LifeIs
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.MonarchIs as MonarchIs
import qualified Pawl.Types.Move as Move
import qualified Pawl.Types.NamesAre as NamesAre
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Paying as Paying
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Placement as Placement
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCountersAre as PlayerCountersAre
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PowerToughnessIs as PowerToughnessIs
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt.Type
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence
import qualified Pawl.Types.Readiness as Readiness
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Reference as Reference
import qualified Pawl.Types.Scenario as Scenario
import qualified Pawl.Types.ScenarioFailure as Failure
import qualified Pawl.Types.Seat as Seat
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Staged as Staged
import qualified Pawl.Types.SubtypesAre as SubtypesAre
import qualified Pawl.Types.Answer as Answer
import qualified Pawl.Scenario.Reply as Reply
import qualified Pawl.Types.TappedIs as TappedIs
import qualified Pawl.Types.TargetCount as TargetCount
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.Timed as Timed
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TypeSwap as TypeSwap
import qualified Pawl.Types.TypesAre as TypesAre
import qualified Pawl.Types.When as When
import qualified Pawl.Types.Zone as Zone

-- | What a run carries between prompts: the entries not yet taken, keyed by
-- their moment, the board's labels, and the cast or activation whose own
-- prompts are still being answered, and a move answered to be refused, with
-- the kind of prompt it answered.
data Rehearsal = MkRehearsal
  { queues :: Map.Map When.When (Seq.Seq Timed.Timed),
    staged :: Staged.Staged,
    pending :: Maybe (When.When, Move.Move, Choices.Choices),
    refusing :: Maybe (When.When, Move.Move, Text.Text)
  }

type Run = State.StateT Rehearsal (Either Failure.ScenarioFailure)

-- | Build, run from the board's step, check. The entry point for a scenario
-- written as data: whole steps (CR 500.1) until the timeline is spent, the game
-- is over, or the turn passes the last one the timeline names. Its final checks
-- then run on that state.
run :: (Monad m) => Registry.Registry m -> Scenario.Scenario -> m (Either Failure.ScenarioFailure GameState.GameState)
run registry scenario = do
  staging <- stage registry (Scenario.board scenario)
  pure $ do
    board <- staging
    (final, rehearsal) <- State.runStateT (playOut stepBudget (Staged.state board)) (rehearsalOf (Scenario.timeline scenario) board)
    settle rehearsal final
    State.evalStateT (mapM_ (expect Nothing final) (Scenario.final scenario)) rehearsal
    pure final

-- | How many steps a data scenario may run: a bound, so a scenario whose moment
-- never comes fails rather than hangs.
stepBudget :: Natural
stepBudget = 1000

playOut :: Natural -> GameState.GameState -> Run GameState.GameState
playOut budget gs = do
  remaining <- State.gets queues
  let lastTurn = maybe 0 (When.turn . fst) (Map.lookupMax remaining)
  if Map.null remaining || Maybe.isJust (GameState.result gs) || budget == 0 || GameState.turnNumber gs > lastTurn
    then pure gs
    else do
      (_, next) <- Engine.runGameAsked answerPrompt gs Engine.runStep
      playOut (budget - 1) next

-- | Run one explicit engine entry point under a timeline, for a Haskell caller
-- whose subject is a subsystem rather than a whole game. An entry whose moment
-- never arrives is a failure, not a silently skipped claim.
rehearse :: Seq.Seq Timed.Timed -> Staged.Staged -> Game.Type.Game a -> Either Failure.ScenarioFailure (a, GameState.GameState)
rehearse timeline board game = do
  ((value, final), rehearsal) <- State.runStateT (Engine.runGameAsked answerPrompt (Staged.state board) game) (rehearsalOf timeline board)
  settle rehearsal final
  pure (value, final)

rehearsalOf :: Seq.Seq Timed.Timed -> Staged.Staged -> Rehearsal
rehearsalOf timeline board =
  let add queue timed = Map.insertWith (flip (Seq.><)) (Timed.when timed) (Seq.singleton timed) queue
   in MkRehearsal
        { queues = Foldable.foldl' add Map.empty timeline,
          staged = board,
          pending = Nothing,
          refusing = Nothing
        }

-- | What a finished run still owes: choices its last action never used, and
-- entries whose moment never came, told apart by whether any is a move.
settle :: Rehearsal -> GameState.GameState -> Either Failure.ScenarioFailure ()
settle rehearsal final = case (pending rehearsal, refusing rehearsal) of
  (Just (key, verb, choices), _)
    | choices /= Choices.none -> Left (Failure.MkUnusedActionChoices key verb choices)
  (_, Just (key, verb, _)) -> Left (Failure.MkUnrefusedMove key verb Nothing)
  _ ->
    let remaining = foldMap snd (Map.toAscList (queues rehearsal))
        turn = GameState.turnNumber final
        step = GameState.phase final
     in if Seq.null remaining
          then Right ()
          else
            if any isMove remaining
              then Left (Failure.MkUnreachedEntries turn step remaining)
              else Left (Failure.MkUnrunChecks turn step remaining)

-- Board ----------------------------------------------------------------------

-- | Place a board directly, without zone-change events or settlement, so
-- nothing it places fires anything. The validity it guarantees is
-- REPRESENTATIONAL only: labels are unique, seats and controllers exist, and
-- every object sits in exactly the zone its Object records.
stage :: (Monad m) => Registry.Registry m -> Board.Board -> m (Either Failure.ScenarioFailure Staged.Staged)
stage registry board =
  let seated = NonEmpty.zip (Board.seats board) (fmap PlayerId.MkPlayerId (0 NonEmpty.:| [1 ..]))
      ids = Map.fromList (fmap (Bifunctor.first Seat.name) (NonEmpty.toList seated))
      seatOf label = maybe (Left (Failure.MkUnknownMonarch label)) Right (Map.lookup label ids)
   in case (boardFailure ids board, Map.lookup (Board.active board) ids, traverse seatOf (Board.monarch board)) of
        (Just failure, _, _) -> pure (Left failure)
        (_, Nothing, _) -> pure (Left (Failure.MkUnknownActivePlayer (Board.active board)))
        (_, _, Left failure) -> pure (Left failure)
        (Nothing, Just active, Right monarch) -> do
          let base = Setup.emptyGame (fmap snd seated)
              lives = Map.fromList (fmap (\(seat, pid) -> (pid, Seat.life seat)) (NonEmpty.toList seated))
              counters = Map.fromList (fmap (\(seat, pid) -> (pid, Seat.counters seat)) (NonEmpty.toList seated))
              positioned =
                base
                  { GameState.activePlayer = active,
                    GameState.phase = Board.phase board,
                    GameState.remaining = Seq.drop 1 (Seq.dropWhileL (/= Board.phase board) (Seq.fromList Turn.allPhases)),
                    GameState.players = Map.mapWithKey (\pid p -> p {Player.life = Map.findWithDefault (Player.life p) pid lives, Player.counters = Map.findWithDefault (Player.counters p) pid counters}) (GameState.players base),
                    GameState.monarch = monarch,
                    GameState.settings =
                      (GameState.settings base)
                        { GameSettings.attackOption = Board.attackOption board,
                          GameSettings.brawl = Board.brawl board,
                          GameSettings.sharedTeamTurns = Board.sharedTeamTurns board,
                          GameSettings.deployCreatures = Board.deployCreatures board,
                          GameSettings.teams = Teams.MkTeams (Map.fromList [(pid, team) | (seat, pid) <- NonEmpty.toList seated, Just team <- [Seat.team seat]]),
                          GameSettings.rangeOfInfluence = RangeOfInfluence.MkRangeOfInfluence (Map.fromList [(pid, range) | (seat, pid) <- NonEmpty.toList seated, Just range <- [Seat.range seat]]),
                          GameSettings.emperors = Emperors.MkEmperors (Map.fromList [(team, pid) | (seat, pid) <- NonEmpty.toList seated, Seat.emperor seat, Just team <- [Seat.team seat]])
                        }
                  }
          placed <- placeSeats registry (NonEmpty.toList seated) (Staged.MkStaged positioned ids Map.empty)
          pure (fmap designateDefenders (placed >>= attachAll (placementsOf board)))

-- | CR 506.2 / CR 507.1: a board past the beginning of combat has its defending
-- players settled already, so the turn-based action that step would have taken
-- is taken here. The engine's own answer picks among defenders.
designateDefenders :: Staged.Staged -> Staged.Staged
designateDefenders board = case GameState.phase (Staged.state board) of
  Phase.Combat step
    | step > CombatStep.BeginningOfCombat ->
        board {Staged.state = snd (Engine.runGamePure quietAnswer (Staged.state board) Combat.designateDefenders)}
  _ -> board

quietAnswer :: Prompt.Type.Prompt r -> r
quietAnswer p = case p of
  Prompt.Type.ChooseAction {} -> Action.Pass
  _ -> Script.declining p

-- | Every placement on the board, in placement order.
placementsOf :: Board.Board -> [Placement.Placement]
placementsOf board = concatMap (concatMap (Foldable.toList . snd) . zonesOf) (NonEmpty.toList (Board.seats board))

-- | A seat's zones in the order they are placed, which is creation order.
zonesOf :: Seat.Seat -> [(Zone.Zone, Seq.Seq Placement.Placement)]
zonesOf seat =
  [ (Zone.Battlefield, Seat.battlefield seat),
    (Zone.Hand, Seat.hand seat),
    (Zone.Graveyard, Seat.graveyard seat),
    (Zone.Library, Seat.library seat),
    (Zone.Exile, Seat.exile seat)
  ]

boardFailure :: Map.Map Label.Label PlayerId.PlayerId -> Board.Board -> Maybe Failure.ScenarioFailure
boardFailure ids board =
  let seats = NonEmpty.toList (Board.seats board)
      placements = placementsOf board
      labels = fmap Seat.name seats <> Maybe.mapMaybe Placement.label placements
      counts = Map.fromListWith (+) (fmap (\label -> (label, 1 :: Natural)) labels)
      duplicate = fmap fst (List.find ((> 1) . snd) (Map.toAscList counts))
      unknownController = List.find (\label -> not (Map.member label ids)) (Maybe.mapMaybe Placement.controller placements)
      -- CR 809.2: one emperor per team, and an emperor is on one.
      emperors = filter Seat.emperor seats
      illegalEmperor = List.find (maybe True (\team -> length (filter ((== Just team) . Seat.team) emperors) > 1) . Seat.team) emperors
   in case duplicate of
        Just label -> Just (Failure.MkDuplicateLabel label)
        Nothing -> case unknownController of
          Just label -> Just (Failure.MkUnknownController label)
          Nothing -> fmap (Failure.MkIllegalEmperor . Seat.name) illegalEmperor

placeSeats :: (Monad m) => Registry.Registry m -> [(Seat.Seat, PlayerId.PlayerId)] -> Staged.Staged -> m (Either Failure.ScenarioFailure Staged.Staged)
placeSeats registry seated board = case seated of
  [] -> pure (Right board)
  (seat, pid) : rest -> do
    let placeZone next (zone, placements) = case next of
          Left failure -> pure (Left failure)
          Right sofar -> placeAll registry zone pid (Foldable.toList placements) sofar
    placed <- Monad.foldM placeZone (Right board) (zonesOf seat)
    case placed of
      Left failure -> pure (Left failure)
      Right next -> placeSeats registry rest next

-- | CR 301.5 / 303.4: each placement naming a host is attached to it, once
-- every object exists, and only where Pawl.Engine.Attach would allow it. Staging
-- creates one object per placement, in placementsOf's order, so the two zip.
attachAll :: [Placement.Placement] -> Staged.Staged -> Either Failure.ScenarioFailure Staged.Staged
attachAll placements board = Monad.foldM attachOne board (zip (Map.keys (GameState.objects (Staged.state board))) placements)
  where
    attachOne sofar (oid, placement) = case Placement.attached placement of
      Nothing -> Right sofar
      Just host ->
        let gs = Staged.state sofar
            destination = case (Map.lookup host (Staged.objects sofar), Map.lookup host (Staged.seats sofar)) of
              (Just hostId, _) -> Just (Recipient.ToObject hostId)
              (_, Just pid) -> Just (Recipient.ToPlayer pid)
              _ -> Nothing
         in case destination >>= \recipient -> Attach.attachmentFor oid recipient gs of
              Nothing -> Left (Failure.MkIllegalAttachment host)
              Just recipient ->
                Right sofar {Staged.state = gs {GameState.objects = Map.adjust (\obj -> obj {Object.attachedTo = Just recipient}) oid (GameState.objects gs)}}

placeAll :: (Monad m) => Registry.Registry m -> Zone.Zone -> PlayerId.PlayerId -> [Placement.Placement] -> Staged.Staged -> m (Either Failure.ScenarioFailure Staged.Staged)
placeAll registry zone owner placements board = case placements of
  [] -> pure (Right board)
  placement : rest -> do
    found <- Registry.fetchCard registry (Placement.card placement)
    case found of
      Nothing -> pure (Left (Failure.MkUnknownCard (Placement.card placement)))
      Just _ | Placement.token placement && zone /= Zone.Battlefield -> pure (Left (Failure.MkTokenOffBattlefield (Placement.card placement)))
      Just card -> do
        let (printingId, interned) = Game.intern (Printing.ofCard card) (Staged.state board)
            (oid, placed) = Setup.placeCard zone owner printingId interned
            controller = Maybe.fromMaybe owner (Placement.controller placement >>= \label -> Map.lookup label (Staged.seats board))
            adjust obj =
              obj
                { Object.enteredUnder = if controller == owner then Nothing else Just controller,
                  Object.tapped = Placement.tapped placement,
                  Object.damage = Placement.damage placement,
                  Object.sickness = case Placement.readiness placement of
                    Readiness.Sick -> Sickness.Sick
                    Readiness.Ready -> Sickness.Settled controller,
                  -- Object.counterTimestamps stays empty: CR 613.7c then reads
                  -- every placed counter as old as its permanent.
                  Object.counters = Placement.counters placement,
                  Object.source = if Placement.token placement then Source.OfToken printingId else Object.source obj
                }
            next =
              board
                { Staged.state = placed {GameState.objects = Map.adjust adjust oid (GameState.objects placed)},
                  Staged.objects = foldr (\label -> Map.insert label oid) (Staged.objects board) (Placement.label placement)
                }
        placeAll registry zone owner rest next

-- Answering -------------------------------------------------------------------

answerPrompt :: Asked.Asked r -> Run r
answerPrompt asked = do
  let prompt = Asked.prompt asked
      gs = Asked.game asked
      kind = Prompt.kindOf prompt
  decider <- traverse labelOf (Prompt.deciderOf prompt)
  -- Hoisted above every arm: a nested game (CR 729.1) is out of scope for every
  -- prompt alike, a pending action's sub-choices included.
  if not (null (Asked.enclosing asked))
    then failWith (Failure.MkNestedGamePrompt (GameState.turnNumber gs) (GameState.phase gs) decider kind)
    else do
      -- CR 733.1: a refused move is reversed, so the engine's next prompt is
      -- the one it answered, asked again.
      refusal <- State.gets refusing
      case refusal of
        Just (key, verb, answered)
          | answered /= kind || decider /= Just (When.player key) || GameState.turnNumber gs /= When.turn key || GameState.phase gs /= When.phase key ->
              failWith (Failure.MkUnrefusedMove key verb (Just kind))
        _ -> State.modify' (\rehearsal -> rehearsal {refusing = Nothing})
      waiting <- State.gets pending
      case waiting of
        Just (key, verb, choices)
          | subChoiceFor key prompt decider choices -> answerActionChoice key verb choices asked
          -- Any other prompt means the action has finished. A choice it never
          -- asked for is a scenario error, reported here rather than at the
          -- next prompt that happens to want an answer.
          | choices /= Choices.none -> do
              -- A prompt another rule raises mid-action (a kicker, a
              -- shuffle) is answered by an Answer and leaves the action
              -- pending.
              found <- takeAnswer gs decider kind
              case found of
                Just (answerKey, index, timed) -> answerGeneric gs answerKey kind index timed prompt
                Nothing -> failWith (Failure.MkUnusedActionChoices key verb choices)
        Just {} -> do
          State.modify' (\rehearsal -> rehearsal {pending = Nothing})
          answerTopPrompt decider asked
        Nothing -> answerTopPrompt decider asked

-- | Whether a prompt is one a cast or activation asks its own decider between
-- the ChooseAction that began it and its completion (CR 601.2b-h, CR 602.2b).
-- A target prompt is only while the move still holds targets: once they are
-- spent, the next one is a triggered ability's (CR 603.3d).
subChoiceFor :: When.When -> Prompt.Type.Prompt r -> Maybe Label.Label -> Choices.Choices -> Bool
subChoiceFor key prompt decider choices =
  decider == Just (When.player key) && case prompt of
    Prompt.Type.ChooseTargets {} -> holdsTargets
    Prompt.Type.AnnounceTargets {} -> holdsTargets
    Prompt.Type.ChooseModes {} -> True
    Prompt.Type.ChooseX {} -> True
    Prompt.Type.ChooseCost {} -> True
    Prompt.Type.OrderCostComponents {} -> True
    Prompt.Type.ChooseManaSource {} -> True
    Prompt.Type.ChooseExtraManaSource {} -> True
    Prompt.Type.ChooseManaYield {} -> True
    _ -> False
  where
    holdsTargets = Maybe.isJust (Choices.targets choices) || not (Map.null (Choices.targetsBySlot choices))

answerTopPrompt :: Maybe Label.Label -> Asked.Asked r -> Run r
answerTopPrompt decider asked = do
  -- An Answer naming this prompt wins over its dedicated move, so a recording
  -- can answer anything one way.
  found <- case Asked.prompt asked of
    Prompt.Type.ChooseAction {} -> pure Nothing
    Prompt.Type.Concede {} -> pure Nothing
    prompt -> takeAnswer (Asked.game asked) decider (Prompt.kindOf prompt)
  case found of
    Just (key, index, timed) -> answerGeneric (Asked.game asked) key (Prompt.kindOf (Asked.prompt asked)) index timed (Asked.prompt asked)
    Nothing -> answerDedicated decider asked

answerDedicated :: Maybe Label.Label -> Asked.Asked r -> Run r
answerDedicated decider asked =
  let prompt = Asked.prompt asked
      gs = Asked.game asked
      kind = Prompt.kindOf prompt
      unscheduled offers = failWith (Failure.MkUnscheduledPrompt (GameState.turnNumber gs) (GameState.phase gs) decider kind offers)
   in case prompt of
        Prompt.Type.ChooseAction who _ actions -> answerActionPrompt gs (Decider.unwrap who) actions
        -- CR 723.6: keyed on the conceding player, whom no one decides for.
        Prompt.Type.Concede pid -> do
          key <- whenOf gs pid
          found <- takeMove key
          case found of
            Just (index, timed)
              | Timed.entry timed == Entry.Do Move.Concede -> case Timed.source timed of
                  Just ref -> failWith (Failure.MkUnexpectedQualifier key kind ref)
                  Nothing -> do
                    popAt key index
                    pure Concession.Concedes
            _ -> pure Concession.Continues
        Prompt.Type.ChooseDefender who _ candidates -> do
          key <- whenOf gs (Decider.unwrap who)
          offers <- fmap (fmap Label.unwrap) (mapM labelOf (NonEmpty.toList candidates))
          onEntry unscheduled key kind offers (takeUnqualified key kind) $ \verb -> case verb of
            Move.ChooseDefender label -> Just $ do
              pid <- resolvePlayer label
              if List.elem pid candidates
                then pure pid
                else failWith (Failure.MkActionNotOffered key verb offers)
            _ -> Nothing
        Prompt.Type.DeclareAttackers who _ candidates -> do
          key <- whenOf gs (Decider.unwrap who)
          offers <- describeAll gs candidates
          onEntry unscheduled key kind offers (takeUnqualified key kind) $ \verb -> case verb of
            Move.Attack refs -> Just (fmap Foldable.toList (mapM (resolveOffered gs key kind candidates) refs))
            _ -> Nothing
        Prompt.Type.ChooseAttackTarget who _ source candidates -> do
          key <- whenOf gs (Decider.unwrap who)
          offers <- mapM (describeTarget gs) (NonEmpty.toList candidates)
          onEntry unscheduled key kind offers (takeForSource gs key source) $ \verb -> case verb of
            Move.ChooseAttackTarget ref -> Just $ do
              target <- resolveEither ref gs
              let matches candidate = case (target, candidate) of
                    (Left pid, AttackTarget.OfPlayer other) -> pid == other
                    (Right oid, AttackTarget.OfPlaneswalker other) -> oid == other
                    (Right oid, AttackTarget.OfBattle other) -> oid == other
                    _ -> False
              case List.find matches candidates of
                Just chosen -> pure chosen
                Nothing -> failWith (Failure.MkActionNotOffered key verb offers)
            _ -> Nothing
        Prompt.Type.DeclareBlockers who _ blockers attackers -> do
          let candidates = blockers <> attackers
          key <- whenOf gs (Decider.unwrap who)
          offers <- describeAll gs candidates
          onEntry unscheduled key kind offers (takeUnqualified key kind) $ \verb -> case verb of
            Move.Block blocks -> Just (fmap Map.fromList (mapM (resolveBlock gs key kind candidates) (Map.toAscList blocks)))
            _ -> Nothing
        -- Passed through unvalidated: CR 510.1c-d's legality is the engine's
        -- to enforce, and a runner that checked it first would hide that.
        Prompt.Type.AssignCombatDamage who _ source thresholds _ -> do
          key <- whenOf gs (Decider.unwrap who)
          let offered = Map.keysSet thresholds
          offers <- describeAll gs (Maybe.mapMaybe Recipient.objectOf (Set.toList offered))
          onEntry unscheduled key kind offers (takeForSource gs key source) $ \verb -> case verb of
            Move.AssignDamage assignment -> Just (fmap Map.fromList (mapM (resolveDamage gs key kind offered) (Map.toAscList assignment)))
            _ -> Nothing
        -- CR 603.3b: every entry named by its source, a repeated source taking
        -- its entries in the order offered; the engine wants their positions.
        Prompt.Type.OrderTriggers who _ entries -> do
          key <- whenOf gs (Decider.unwrap who)
          offers <- mapM (describeTriggerSource gs . TriggerEntry.source) entries
          onEntry unscheduled key kind offers (takeUnqualified key kind) $ \verb -> case verb of
            Move.OrderTriggers named -> Just (orderTriggers gs key verb offers entries (Foldable.toList named))
            _ -> Nothing
        -- A target prompt outside a cast or activation, a triggered ability's
        -- most often (CR 603.3d), keyed like ChooseOptional below.
        Prompt.Type.ChooseTargets who _ asking offered -> do
          key <- whenOf gs (Decider.unwrap who)
          let named = Maybe.fromMaybe asking (Game.abilitySourceOf asking gs)
          onEntry unscheduled key kind [] (takeForSource gs key named) $ \verb -> case verb of
            Move.ChooseTargets chosen -> Just (resolveSlots gs key verb offered chosen)
            _ -> Nothing
        -- CR 601.2c's announcement, answered from the ChooseTargets entry that
        -- will name the targets, which stays queued for that prompt.
        Prompt.Type.AnnounceTargets who _ asking offered -> do
          key <- whenOf gs (Decider.unwrap who)
          let named = Maybe.fromMaybe asking (Game.abilitySourceOf asking gs)
          found <- takeForSource gs key named
          case fmap (Timed.entry . snd) found of
            Just (Entry.Do verb@(Move.ChooseTargets chosen)) -> announceSlots key verb offered chosen
            Just entry -> failWith (Failure.MkUnexpectedPrompt key entry kind [])
            Nothing -> unscheduled []
        -- Keyed by what is resolving, since two "may"s can share a moment: the
        -- spell, or the object an ability came from (CR 113.7), which is what
        -- a board can label.
        Prompt.Type.ChooseOptional who _ resolving _ _ _ -> do
          key <- whenOf gs (Decider.unwrap who)
          let named = Maybe.fromMaybe resolving (Game.abilitySourceOf resolving gs)
          onEntry unscheduled key kind [] (takeForSource gs key named) $ \verb -> case verb of
            Move.ChooseOptional decision -> Just (pure decision)
            _ -> Nothing
        -- CR 118.12a: keyed like ChooseOptional, by the object offering the cost.
        -- Paying leaves the move's choices pending for the payment's own prompts.
        Prompt.Type.ChooseToPay who _ offering _ _ _ -> do
          key <- whenOf gs (Decider.unwrap who)
          let named = Maybe.fromMaybe offering (Game.abilitySourceOf offering gs)
          onEntry unscheduled key kind [] (takeForSource gs key named) $ \verb -> case verb of
            Move.ChooseToPay paying -> Just $ do
              Monad.when (Paying.decision paying == PaymentDecision.Pays) $
                State.modify' (\rehearsal -> rehearsal {pending = Just (key, verb, Paying.choices paying)})
              pure (Paying.decision paying)
            _ -> Nothing
        -- CR 612.1's text change, keyed like ChooseOptional by what applies it.
        Prompt.Type.ChooseLandTypeSwap who _ applying _ _ -> do
          key <- whenOf gs (Decider.unwrap who)
          let named = Maybe.fromMaybe applying (Game.abilitySourceOf applying gs)
          onEntry unscheduled key kind [] (takeForSource gs key named) $ \verb -> case verb of
            Move.ChooseTypeSwap swap -> Just (pure (TypeSwap.from swap, TypeSwap.to swap))
            _ -> Nothing
        Prompt.Type.ChooseCreatureTypeSwap who _ applying _ _ -> do
          key <- whenOf gs (Decider.unwrap who)
          let named = Maybe.fromMaybe applying (Game.abilitySourceOf applying gs)
          onEntry unscheduled key kind [] (takeForSource gs key named) $ \verb -> case verb of
            Move.ChooseTypeSwap swap -> Just (pure (TypeSwap.from swap, TypeSwap.to swap))
            _ -> Nothing
        -- CR 707.5 / 614.12a: keyed by the object entering as the copy; an
        -- unoffered permanent fails rather than reaching the engine.
        Prompt.Type.ChooseCopyTarget who _ entering candidates -> do
          key <- whenOf gs (Decider.unwrap who)
          offers <- describeAll gs candidates
          onEntry unscheduled key kind offers (takeForSource gs key entering) $ \verb -> case verb of
            Move.ChooseCopyTarget chosen -> Just (traverse (resolveOffered gs key kind candidates) chosen)
            _ -> Nothing
        -- CR 614.1a's optional redirect, a "may" keyed by the redirect's own
        -- source like ChooseOptional above.
        Prompt.Type.ChooseRedirect who _ source _ -> do
          key <- whenOf gs (Decider.unwrap who)
          onEntry unscheduled key kind [] (takeForSource gs key source) $ \verb -> case verb of
            Move.ChooseOptional decision -> Just (pure decision)
            _ -> Nothing
        -- The order named is the whole group, each object once; the engine
        -- wants it as positions in the group it offered.
        Prompt.Type.OrderTimestamps who _ group -> do
          key <- whenOf gs (Decider.unwrap who)
          offers <- describeAll gs group
          onEntry unscheduled key kind offers (takeUnqualified key kind) $ \verb -> case verb of
            Move.OrderTimestamps refs -> Just $ do
              chosen <- mapM (resolveOffered gs key kind group) (Foldable.toList refs)
              if List.sort chosen == List.sort group
                then pure (Maybe.mapMaybe (\oid -> List.elemIndex oid group >>= Int.toNatural) chosen)
                else failWith (Failure.MkActionNotOffered key verb offers)
            _ -> Nothing
        _ -> unscheduled []

-- | The first Answer naming this prompt at its moment: the decider's, or the
-- active player's for a prompt nobody decides (a shuffle, a die).
takeAnswer :: GameState.GameState -> Maybe Label.Label -> Text.Text -> Run (Maybe (When.When, Int, Timed.Timed))
takeAnswer gs decider kind = do
  label <- maybe (labelOf (GameState.activePlayer gs)) pure decider
  let key = When.MkWhen (GameState.turnNumber gs) (GameState.phase gs) label
      names timed = case Timed.entry timed of
        Entry.Do (Move.Answer answer) -> Answer.prompt answer == kind
        _ -> False
  entries <- queueAt key
  pure (fmap (\(index, timed) -> (key, index, timed)) (List.find (names . snd) (zip [0 ..] (Foldable.toList entries))))

answerGeneric :: GameState.GameState -> When.When -> Text.Text -> Int -> Timed.Timed -> Prompt.Type.Prompt r -> Run r
answerGeneric gs key kind index timed prompt = case Timed.entry timed of
  Entry.Do verb@(Move.Answer answer) -> do
    popAt key index
    let resolve needs = case needs of
          Reply.Done a -> pure a
          Reply.Failed problem -> failWith (Failure.MkUnexpectedActionChoice key verb (kind <> Text.pack ": " <> problem))
          Reply.NeedObject ref k -> resolveObject ref gs >>= resolve . k
          Reply.NeedPlayer label k -> resolvePlayer label >>= resolve . k
    resolve (Reply.decode (Reply.shapeOf prompt) (Answer.with answer))
  entry -> failWith (Failure.MkUnexpectedPrompt key entry kind [])

-- | A priority prompt: first the checks at the head of this moment, in timeline
-- order, then the first move that takes priority, and a pass when there is
-- none. An entry for a prompt the engine elided stays queued rather than
-- blocking the move behind it, and is reported as unreached.
answerActionPrompt :: GameState.GameState -> PlayerId.PlayerId -> [Action.Action] -> Run Action.Action
answerActionPrompt gs pid actions = do
  key <- whenOf gs pid
  runLeadingChecks key gs
  entries <- queueAt key
  let takesPriority timed = case Timed.entry timed of
        Entry.Do Move.Cast {} -> True
        Entry.Do Move.PlayLand {} -> True
        Entry.Do Move.Activate {} -> True
        Entry.Do Move.Pass -> True
        _ -> False
  case List.find (takesPriority . snd) (zip [0 ..] (Foldable.toList entries)) of
    Nothing -> pure Action.Pass
    Just (index, timed) -> case (Timed.source timed, Timed.entry timed) of
      (Just ref, _) -> failWith (Failure.MkUnexpectedQualifier key (Text.pack "ChooseAction") ref)
      (Nothing, Entry.Do Move.Pass) -> do
        popAt key index
        pure Action.Pass
      (Nothing, Entry.Do verb) -> do
        offered <- mapM (describeAction gs) actions
        matching <- actionsMatching gs verb actions
        chosen <- case matching of
          [] -> failWith (Failure.MkActionNotOffered key verb offered)
          [action] -> pure action
          _ -> failWith (Failure.MkAmbiguousAction key verb offered)
        popAt key index
        State.modify' (\rehearsal -> rehearsal {pending = Just (key, verb, choicesOf verb)})
        pure chosen
      (Nothing, Entry.Refuse _) -> pure Action.Pass
      (Nothing, Entry.Expect _) -> pure Action.Pass

-- | The checks at the head of a moment's queue, evaluated and taken in order,
-- stopping at the first move.
runLeadingChecks :: When.When -> GameState.GameState -> Run ()
runLeadingChecks key gs = do
  entries <- queueAt key
  case Seq.lookup 0 entries of
    Just timed
      | Entry.Expect check <- Timed.entry timed -> case Timed.source timed of
          Just ref -> failWith (Failure.MkUnexpectedQualifier key (Text.pack "check") ref)
          Nothing -> do
            expect (Just key) gs check
            popAt key 0
            runLeadingChecks key gs
    _ -> pure ()

choicesOf :: Move.Move -> Choices.Choices
choicesOf verb = case verb of
  Move.Cast casting -> Casting.choices casting
  Move.Activate activation -> Activation.choices activation
  _ -> Choices.none

actionsMatching :: GameState.GameState -> Move.Move -> [Action.Action] -> Run [Action.Action]
actionsMatching gs verb actions = case verb of
  Move.Cast casting -> do
    oid <- resolveObject (Casting.object casting) gs
    pure (filter (\action -> case action of Action.Cast candidate _ _ -> candidate == oid; _ -> False) actions)
  Move.PlayLand ref -> do
    oid <- resolveObject ref gs
    pure (filter (\action -> case action of Action.Play candidate _ -> candidate == oid; _ -> False) actions)
  Move.Activate activation -> do
    oid <- resolveObject (Activation.object activation) gs
    let selected = fmap (\index -> List.genericDrop index (Projection.abilitiesOf oid gs)) (Activation.ability activation)
    pure (filter (matchesActivation oid selected) actions)
  _ -> pure []

matchesActivation :: ObjectId.ObjectId -> Maybe [ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)] -> Action.Action -> Bool
matchesActivation oid selected action = case action of
  Action.Activate candidate ability -> candidate == oid && maybe True ((== Just ability) . Maybe.listToMaybe) selected
  _ -> False

answerActionChoice :: When.When -> Move.Move -> Choices.Choices -> Asked.Asked r -> Run r
answerActionChoice key verb choices asked =
  let prompt = Asked.prompt asked
      gs = Asked.game asked
      kind = Prompt.kindOf prompt
      unexpected :: Run b
      unexpected = failWith (Failure.MkUnexpectedActionChoice key verb kind)
   in case prompt of
        -- Slot by slot: this prompt takes the slots it offers, every one named.
        Prompt.Type.ChooseTargets _ _ _ offered
          | not (Map.null (Choices.targetsBySlot choices)) -> do
              let mine = Map.restrictKeys (Choices.targetsBySlot choices) (Map.keysSet offered)
              Monad.unless (Map.size mine == Map.size offered) unexpected
              chosen <- resolveSlots gs key verb offered mine
              updateChoices (\current -> current {Choices.targetsBySlot = Map.withoutKeys (Choices.targetsBySlot current) (Map.keysSet offered)})
              pure chosen
        Prompt.Type.AnnounceTargets _ _ _ offered
          | not (Map.null (Choices.targetsBySlot choices)) -> do
              let mine = Map.restrictKeys (Choices.targetsBySlot choices) (Map.keysSet offered)
              Monad.unless (Map.size mine == Map.size offered) unexpected
              counts <- announceSlots key verb offered mine
              -- A slot announced at zero raises no ChooseTargets to spend it.
              updateChoices (\current -> current {Choices.targetsBySlot = Map.withoutKeys (Choices.targetsBySlot current) (Map.keysSet (Map.filter (== 0) counts))})
              pure counts
        Prompt.Type.ChooseTargets _ _ _ offered -> case (Choices.targets choices, Map.toList offered) of
          (Just targets, [(slot, (count, candidates))])
            | Natural.length targets == count -> do
                resolved <- mapM (\ref -> resolveRecipient gs ref candidates unexpected) targets
                let selected = Set.fromList resolved
                if Natural.length selected == count
                  then do
                    updateChoices (\current -> current {Choices.targets = Nothing})
                    pure (Map.singleton slot selected)
                  else unexpected
          _ -> unexpected
        -- CR 601.2c: a single variable slot announces as many targets as the
        -- move names, and the list stays for the ChooseTargets that follows;
        -- none follows an announced zero, so an empty list is spent here.
        Prompt.Type.AnnounceTargets _ _ _ offered -> case (Choices.targets choices, Map.toList offered) of
          (Just targets, [(slot, (range, candidates))])
            | TargetCount.least range <= Natural.length targets
                && Natural.length targets <= TargetCount.ceilingOn (Natural.length candidates) range -> do
                Monad.when (null targets) (updateChoices (\current -> current {Choices.targets = Nothing}))
                pure (Map.singleton slot (Natural.length targets))
          _ -> unexpected
        Prompt.Type.ChooseModes _ _ _ legal selection -> case Choices.modes choices of
          Just modes
            | Modal.selectionSatisfiedBy legal selection modes -> do
                updateChoices (\current -> current {Choices.modes = Nothing})
                pure modes
          _ -> unexpected
        Prompt.Type.ChooseX _ _ _ minimumX maximumX -> case Choices.x choices of
          Just x
            | minimumX <= x && x <= maximumX -> do
                updateChoices (\current -> current {Choices.x = Nothing})
                pure x
          _ -> unexpected
        Prompt.Type.ChooseCost _ _ _ candidates -> case Choices.cost choices of
          Just wanted -> case filter ((== Just wanted) . Cost.mana) candidates of
            [cost] -> do
              updateChoices (\current -> current {Choices.cost = Nothing})
              pure cost
            _ -> unexpected
          Nothing -> unexpected
        -- A PERMUTATION is the only legal answer, so the order is checked
        -- against the printed indices rather than trusted: one naming an index
        -- twice would pay one component twice and skip another.
        Prompt.Type.OrderCostComponents _ _ _ components -> case Choices.costOrder choices of
          Just order
            | List.sort order == fmap fst (zip [0 ..] components) -> do
                updateChoices (\current -> current {Choices.costOrder = Nothing})
                pure order
          _ -> unexpected
        Prompt.Type.ChooseManaSource _ _ candidates -> answerManaSource gs key verb choices kind candidates
        Prompt.Type.ChooseExtraManaSource _ _ candidates -> answerManaSource gs key verb choices kind candidates
        Prompt.Type.ChooseManaYield _ _ _ candidates -> case Seq.viewl (Choices.manaYields choices) of
          Seq.EmptyL -> unexpected
          -- Recipient-blind: the yield is matched on its units alone.
          wanted Seq.:< rest -> case filter ((== Mana.Type.unwrap wanted) . Mana.yieldUnits) (NonEmpty.toList candidates) of
            [option] -> do
              updateChoices (\current -> current {Choices.manaYields = rest})
              pure option
            _ -> unexpected
        _ -> unexpected

answerManaSource :: GameState.GameState -> When.When -> Move.Move -> Choices.Choices -> Text.Text -> NonEmpty.NonEmpty ObjectId.ObjectId -> Run (Maybe ObjectId.ObjectId)
answerManaSource gs key verb choices kind candidates = case Seq.viewl (Choices.manaSources choices) of
  Seq.EmptyL -> failWith (Failure.MkUnexpectedActionChoice key verb kind)
  wanted Seq.:< rest -> do
    resolved <- traverse (resolveOffered gs key kind (NonEmpty.toList candidates)) wanted
    updateChoices (\current -> current {Choices.manaSources = rest})
    pure resolved

updateChoices :: (Choices.Choices -> Choices.Choices) -> Run ()
updateChoices f =
  State.modify' $ \rehearsal ->
    rehearsal {pending = fmap (\(key, verb, choices) -> (key, verb, f choices)) (pending rehearsal)}

-- | The require-match-pop sequence every non-priority prompt shares: nothing at
-- this moment is an unscheduled prompt, an entry with another verb an
-- unexpected one, and the pop happens once, before the arm resolves anything.
onEntry ::
  ([Text.Text] -> Run a) ->
  When.When ->
  Text.Text ->
  [Text.Text] ->
  Run (Maybe (Int, Timed.Timed)) ->
  (Move.Move -> Maybe (Run a)) ->
  Run a
onEntry unscheduled key kind offers select match = do
  found <- select
  case found of
    Nothing -> unscheduled offers
    Just (index, timed) -> case Timed.entry timed of
      Entry.Do verb
        | Just action <- match verb -> do
            popAt key index
            action
      Entry.Refuse verb
        | Just action <- match verb -> do
            popAt key index
            State.modify' (\rehearsal -> rehearsal {refusing = Just (key, verb, kind)})
            action
      entry -> failWith (Failure.MkUnexpectedPrompt key entry kind offers)

-- Checks -----------------------------------------------------------------------

-- | Fails the run unless the check holds on this state.
expect :: Maybe When.When -> GameState.GameState -> Check.Check -> Run ()
expect key gs check = do
  observed <- observe gs check
  case observed of
    Nothing -> pure ()
    Just actual -> failWith (Failure.MkCheckFailed key check actual)

-- | Nothing when the check holds, else what was there instead.
observe :: GameState.GameState -> Check.Check -> Run (Maybe Text.Text)
observe gs check = case check of
  Check.Life (LifeIs.MkLifeIs label life) -> do
    pid <- resolvePlayer label
    let actual = fmap Player.life (Map.lookup pid (GameState.players gs))
    pure (if actual == Just life then Nothing else Just (Text.pack (maybe "no such player" show actual)))
  Check.Count (CountIs.MkCountIs label zone card count) -> do
    pid <- resolvePlayer label
    let members = case zone of
          -- CR 110.2: on the battlefield, what a player has is what they
          -- control, and a permanent's name is its projected one.
          Zone.Battlefield -> filter (\oid -> Projection.controllerOf oid gs == Just pid) (Set.toList (GameState.battlefield gs))
          _ -> Game.zoneMembers zone pid gs
        namesOf oid = case zone of
          Zone.Battlefield -> Projection.namesOf oid gs
          _ -> Game.namesOf oid gs
        actual = Natural.length (filter (Set.member card . namesOf) members)
    pure (if actual == count then Nothing else Just (Text.pack (show actual)))
  Check.Damage (DamageIs.MkDamageIs ref damage) -> do
    oid <- resolveObject ref gs
    let actual = fmap Object.damage (Game.lookupObject oid gs)
    pure (if actual == Just damage then Nothing else Just (Text.pack (maybe "no such object" show actual)))
  Check.Tapped (TappedIs.MkTappedIs ref tapped) -> do
    oid <- resolveObject ref gs
    let actual = fmap Object.tapped (Game.lookupObject oid gs)
    pure (if actual == Just tapped then Nothing else Just (Text.pack (maybe "no such object" show actual)))
  Check.Counters (CountersAre.MkCountersAre ref kind count) -> do
    oid <- resolveObject ref gs
    let actual = fmap (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)
    pure (if actual == Just count then Nothing else Just (Text.pack (maybe "no such object" show actual)))
  -- The projected types (CR 613.1d), never the printed card's.
  Check.Types (TypesAre.MkTypesAre ref types) -> do
    oid <- resolveObject ref gs
    let actual = Projection.cardTypesOf oid gs
    pure (if actual == types then Nothing else Just (Text.pack (show (Set.toList actual))))
  Check.Attackers (AttackersAre.MkAttackersAre expected) -> do
    wanted <- fmap Map.fromList (mapM (\(attacker, target) -> (,) <$> resolveObject attacker gs <*> resolveEither target gs) (Map.toList expected))
    let actual = Combat.Type.attackers (GameState.combat gs)
        targetOf target = case target of
          AttackTarget.OfPlayer pid -> Left pid
          AttackTarget.OfPlaneswalker oid -> Right oid
          AttackTarget.OfBattle oid -> Right oid
    if fmap targetOf actual == wanted
      then pure Nothing
      else do
        described <- mapM (\(attacker, target) -> (\a t -> a <> Text.pack " attacking " <> t) <$> describeObject gs attacker <*> describeTarget gs target) (Map.toList actual)
        pure (Just (if null described then Text.pack "nothing attacking" else Text.intercalate (Text.pack ", ") described))
  Check.Blockers (BlockersAre.MkBlockersAre ref expected) -> do
    oid <- resolveObject ref gs
    wanted <- traverse (fmap Set.fromList . mapM (`resolveObject` gs) . Set.toList) expected
    let actual = Map.lookup oid (Combat.Type.blockers (GameState.combat gs))
    if actual == wanted
      then pure Nothing
      else case actual of
        Nothing -> pure (Just (Text.pack "unblocked"))
        Just blockers -> fmap (\named -> Just (Text.pack "blocked by [" <> Text.intercalate (Text.pack ", ") named <> Text.pack "]")) (describeAll gs (Set.toList blockers))
  Check.Defenders (DefendersAre.MkDefendersAre expected) -> do
    actual <- mapM labelOf (Combat.Type.defenders (GameState.combat gs))
    pure (if actual == expected then Nothing else Just (Text.pack (show (fmap Label.unwrap actual))))
  Check.PlayerCounters (PlayerCountersAre.MkPlayerCountersAre label kind count) -> do
    pid <- resolvePlayer label
    let actual = fmap (Map.findWithDefault 0 kind . Player.counters) (Map.lookup pid (GameState.players gs))
    pure (if actual == Just count then Nothing else Just (Text.pack (maybe "no such player" show actual)))
  -- The projected subtypes (CR 613.1d), never the printed card's.
  Check.Subtypes (SubtypesAre.MkSubtypesAre ref subtypes) -> do
    oid <- resolveObject ref gs
    let actual = Projection.subtypesOf oid gs
    pure (if actual == subtypes then Nothing else Just (Text.pack (show (Set.toList actual))))
  -- The projected names (CR 707.2's copiable values), never the printed card's.
  Check.Names (NamesAre.MkNamesAre ref names) -> do
    oid <- resolveObject ref gs
    let actual = Projection.namesOf oid gs
    pure (if actual == names then Nothing else Just (Text.pack (show (fmap CardName.unwrap (Set.toList actual)))))
  -- The projected keywords (CR 613.1f), counted by instance.
  Check.Keywords (KeywordsAre.MkKeywordsAre ref keyword count) -> do
    oid <- resolveObject ref gs
    let actual = Map.findWithDefault 0 keyword (Projection.keywordsOf oid gs)
    pure (if actual == count then Nothing else Just (Text.pack (show actual)))
  Check.Monarch (MonarchIs.MkMonarchIs expected) -> do
    actual <- traverse labelOf (GameState.monarch gs)
    pure (if actual == expected then Nothing else Just (maybe (Text.pack "nobody") Label.unwrap actual))
  -- The projected values (CR 613.4), never the printed card's.
  Check.PowerToughness (PowerToughnessIs.MkPowerToughnessIs ref power toughness) -> do
    oid <- resolveObject ref gs
    let actual = (,) <$> Projection.powerOf oid gs <*> Projection.toughnessOf oid gs
    pure (if actual == Just (power, toughness) then Nothing else Just (maybe (Text.pack "no power and toughness") (\(p, t) -> Text.pack (show p <> "/" <> show t)) actual))

-- Queues -----------------------------------------------------------------------

-- | The key an entry answered by `pid` would carry now. `pid` is the DECIDER.
whenOf :: GameState.GameState -> PlayerId.PlayerId -> Run When.When
whenOf gs pid = fmap (When.MkWhen (GameState.turnNumber gs) (GameState.phase gs)) (labelOf pid)

queueAt :: When.When -> Run (Seq.Seq Timed.Timed)
queueAt key = State.gets (Map.findWithDefault Seq.empty key . queues)

-- | The first move at `key`, skipping checks, which wait for priority.
takeMove :: When.When -> Run (Maybe (Int, Timed.Timed))
takeMove key = do
  entries <- queueAt key
  pure (List.find (isMove . snd) (zip [0 ..] (Foldable.toList entries)))

isMove :: Timed.Timed -> Bool
isMove timed = case Timed.entry timed of
  Entry.Do _ -> True
  Entry.Refuse _ -> True
  Entry.Expect _ -> False

-- | The first move at `key`, for a prompt with no source of its own, which a
-- source qualifier could never match.
takeUnqualified :: When.When -> Text.Text -> Run (Maybe (Int, Timed.Timed))
takeUnqualified key kind = do
  found <- takeMove key
  case found of
    Just (_, timed)
      | Just ref <- Timed.source timed -> failWith (Failure.MkUnexpectedQualifier key kind ref)
    _ -> pure found

-- | The first move at `key` whose source is `source`, else the first with none:
-- several prompts at one moment arrive in the engine's order, which a scenario
-- cannot predict, so a qualified entry is found by its source. Every qualifier
-- at the moment is resolved, so a dangling one fails here rather than sitting
-- in the queue until the end.
takeForSource :: GameState.GameState -> When.When -> ObjectId.ObjectId -> Run (Maybe (Int, Timed.Timed))
takeForSource gs key source = do
  entries <- queueAt key
  let moves = filter (isMove . snd) (zip [0 ..] (Foldable.toList entries))
  qualifiers <- mapM (\(index, timed) -> fmap (\oid -> (index, timed, oid)) (traverse (\ref -> resolveObject ref gs) (Timed.source timed))) moves
  let qualified = List.find (\(_, _, oid) -> oid == Just source) qualifiers
      unqualified = List.find (\(_, _, oid) -> Maybe.isNothing oid) qualifiers
  pure (fmap (\(index, timed, _) -> (index, timed)) (qualified <|> unqualified))

popAt :: When.When -> Int -> Run ()
popAt key index =
  State.modify' $ \rehearsal ->
    let remaining = case Map.lookup key (queues rehearsal) of
          Nothing -> queues rehearsal
          Just entries ->
            let rest = Seq.deleteAt index entries
             in if Seq.null rest then Map.delete key (queues rehearsal) else Map.insert key rest (queues rehearsal)
     in rehearsal {queues = remaining}

failWith :: Failure.ScenarioFailure -> Run a
failWith failure = State.StateT (const (Left failure))

-- References -------------------------------------------------------------------

-- | Every live object whose card answers to `name`, in creation order and in
-- every zone -- the numbering Reference.Printed counts in. The printed card,
-- since a reference names a card rather than asking a rules question about it.
namedObjects :: CardName.CardName -> GameState.GameState -> [ObjectId.ObjectId]
namedObjects name gs =
  let wanted = Registry.slugFor name
      matches oid = case Game.cardOf oid gs of
        Nothing -> False
        Just card -> any ((== wanted) . Registry.slugFor . Face.name) (NonEmpty.toList (Card.faces card))
   in filter matches (Map.keys (GameState.objects gs))

labelOf :: PlayerId.PlayerId -> Run Label.Label
labelOf pid = do
  seats <- State.gets (Staged.seats . staged)
  pure $ case List.find ((== pid) . snd) (Map.toList seats) of
    Just (label, _) -> label
    Nothing -> Label.MkLabel (Text.pack ("player " <> show (PlayerId.unwrap pid)))

resolvePlayer :: Label.Label -> Run PlayerId.PlayerId
resolvePlayer label = do
  seats <- State.gets (Staged.seats . staged)
  case Map.lookup label seats of
    Just pid -> pure pid
    Nothing -> failWith (Failure.MkNotAPlayer label)

resolveObject :: Reference.Reference -> GameState.GameState -> Run ObjectId.ObjectId
resolveObject ref gs = case ref of
  Reference.Labelled label -> do
    board <- State.gets staged
    case Map.lookup label (Staged.objects board) of
      Just oid
        | Map.member oid (GameState.objects gs) -> pure oid
        | otherwise -> failWith (Failure.MkUnknownObject ref True)
      Nothing
        | Map.member label (Staged.seats board) -> failWith (Failure.MkNotAnObject ref)
        | otherwise -> failWith (Failure.MkUnknownObject ref False)
  Reference.Printed name occurrence -> case occurrence of
    0 -> failWith (Failure.MkUnknownObject ref False)
    _ -> case List.genericDrop (occurrence - 1) (namedObjects name gs) of
      oid : _ -> pure oid
      [] -> failWith (Failure.MkUnknownObject ref False)

-- | A seat (Left) or an object (Right), for a reference that may name either.
resolveEither :: Reference.Reference -> GameState.GameState -> Run (Either PlayerId.PlayerId ObjectId.ObjectId)
resolveEither ref gs = do
  seats <- State.gets (Staged.seats . staged)
  case ref of
    Reference.Labelled label
      | Just pid <- Map.lookup label seats -> pure (Left pid)
    _ -> fmap Right (resolveObject ref gs)

-- | The one offered recipient a reference names, whatever kind the offer
-- calls it: a planeswalker is offered as one, a creature as another.
-- | The positions of the offered entries in the order named: each name takes
-- the first entry not yet taken whose source it is, and every entry is taken.
orderTriggers :: GameState.GameState -> When.When -> Move.Move -> [Text.Text] -> [TriggerEntry.TriggerEntry] -> [Maybe Reference.Reference] -> Run [Natural]
orderTriggers gs key verb offers entries named = do
  wanted <- mapM (traverse (`resolveObject` gs)) named
  let sourceOf entry = case TriggerEntry.source entry of
        TriggerSource.OfObject oid -> Just oid
        TriggerSource.Sourceless -> Nothing
      pick taken source = List.find (\i -> notElem i taken && fmap sourceOf (Maybe.listToMaybe (drop i entries)) == Just source) [0 .. length entries - 1]
      step acc source = acc >>= \taken -> (\i -> Just (taken <> [i])) =<< pick taken source
  case List.foldl' step (Just []) wanted of
    Just order
      | length order == length entries -> pure (Maybe.mapMaybe Int.toNatural order)
    _ -> failWith (Failure.MkActionNotOffered key verb offers)

describeTriggerSource :: GameState.GameState -> TriggerSource.TriggerSource -> Run Text.Text
describeTriggerSource gs source = case source of
  TriggerSource.OfObject oid -> describeObject gs oid
  TriggerSource.Sourceless -> pure (Text.pack "null")

-- | A ChooseTargets entry's answer: every offered slot named, each with exactly
-- the number of distinct offered recipients it takes.
resolveSlots :: GameState.GameState -> When.When -> Move.Move -> Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient) -> Map.Map SlotName.SlotName (Seq.Seq Reference.Reference) -> Run (Map.Map SlotName.SlotName (Set.Set Recipient.Recipient))
resolveSlots gs key verb offered chosen =
  let refused :: Run b
      refused = failWith (Failure.MkActionNotOffered key verb (fmap SlotName.unwrap (Map.keys offered)))
   in if Map.keysSet chosen /= Map.keysSet offered
        then refused
        else
          Map.traverseWithKey
            ( \slot (count, candidates) -> do
                let refs = Foldable.toList (Map.findWithDefault Seq.empty slot chosen)
                selected <- fmap Set.fromList (mapM (\ref -> resolveRecipient gs ref candidates refused) refs)
                if Natural.length refs == count && Natural.length selected == count then pure selected else refused
            )
            offered

-- | How many targets each offered slot's entry names, within CR 601.2c's range.
announceSlots :: When.When -> Move.Move -> Map.Map SlotName.SlotName (TargetCount.TargetCount, Set.Set Recipient.Recipient) -> Map.Map SlotName.SlotName (Seq.Seq Reference.Reference) -> Run (Map.Map SlotName.SlotName Natural)
announceSlots key verb offered chosen =
  let refused :: Run b
      refused = failWith (Failure.MkActionNotOffered key verb (fmap SlotName.unwrap (Map.keys offered)))
   in if Map.keysSet chosen /= Map.keysSet offered
        then refused
        else
          Map.traverseWithKey
            ( \slot (range, candidates) ->
                let n = Natural.length (Map.findWithDefault Seq.empty slot chosen)
                 in if TargetCount.least range <= n && n <= TargetCount.ceilingOn (Natural.length candidates) range then pure n else refused
            )
            offered

resolveRecipient :: GameState.GameState -> Reference.Reference -> Set.Set Recipient.Recipient -> Run Recipient.Recipient -> Run Recipient.Recipient
resolveRecipient gs ref offered unmatched = do
  target <- resolveEither ref gs
  let matches recipient = case target of
        Left pid -> recipient == Recipient.ToPlayer pid
        Right oid -> Recipient.objectOf recipient == Just oid
  case filter matches (Set.toList offered) of
    [recipient] -> pure recipient
    _ -> unmatched

-- | resolveObject, plus the check that the prompt offered what it found:
-- otherwise the engine drops the non-candidate after the entry was taken, and
-- the scenario passes proving nothing.
resolveOffered :: GameState.GameState -> When.When -> Text.Text -> [ObjectId.ObjectId] -> Reference.Reference -> Run ObjectId.ObjectId
resolveOffered gs key kind offered ref = do
  oid <- resolveObject ref gs
  if List.elem oid offered
    then pure oid
    else do
      offers <- describeAll gs offered
      failWith (Failure.MkUnofferedObject key kind (Codec.Reference.toText ref) offers)

resolveBlock :: GameState.GameState -> When.When -> Text.Text -> [ObjectId.ObjectId] -> (Reference.Reference, Set.Set Reference.Reference) -> Run (ObjectId.ObjectId, Set.Set ObjectId.ObjectId)
resolveBlock gs key kind offered (blocker, attackers) = do
  blockerId <- resolveOffered gs key kind offered blocker
  attackerIds <- mapM (resolveOffered gs key kind offered) (Set.toAscList attackers)
  pure (blockerId, Set.fromList attackerIds)

-- | CR 510.1a: a recipient must be one the prompt offered a threshold for.
resolveDamage :: GameState.GameState -> When.When -> Text.Text -> Set.Set Recipient.Recipient -> (Reference.Reference, Natural) -> Run (Recipient.Recipient, Natural)
resolveDamage gs key kind offered (ref, amount) = do
  offers <- describeAll gs (Maybe.mapMaybe Recipient.objectOf (Set.toList offered))
  recipient <- resolveRecipient gs ref offered (failWith (Failure.MkUnofferedObject key kind (Codec.Reference.toText ref) offers))
  pure (recipient, amount)

-- Rendering --------------------------------------------------------------------

-- | How a failure names an object: its label, or its card name and occurrence,
-- the two ways a scenario could have written it.
describeObject :: GameState.GameState -> ObjectId.ObjectId -> Run Text.Text
describeObject gs oid = do
  labels <- State.gets (Staged.objects . staged)
  pure $ case List.find ((== oid) . snd) (Map.toAscList labels) of
    Just (label, _) -> Codec.Reference.toText (Reference.Labelled label)
    Nothing -> case Game.cardOf oid gs of
      Nothing -> Text.pack ("object " <> show (ObjectId.unwrap oid))
      Just card ->
        let name = Face.name (NonEmpty.head (Card.faces card))
            occurrence = Natural.length (takeWhile (/= oid) (namedObjects name gs)) + 1
         in Codec.Reference.toText (Reference.Printed name occurrence)

describeAll :: GameState.GameState -> [ObjectId.ObjectId] -> Run [Text.Text]
describeAll gs = mapM (describeObject gs)

describeTarget :: GameState.GameState -> AttackTarget.AttackTarget -> Run Text.Text
describeTarget gs target = case target of
  AttackTarget.OfPlayer pid -> fmap (Codec.Reference.toText . Reference.Labelled) (labelOf pid)
  AttackTarget.OfPlaneswalker oid -> describeObject gs oid
  AttackTarget.OfBattle oid -> describeObject gs oid

describeAction :: GameState.GameState -> Action.Action -> Run Text.Text
describeAction gs action = case action of
  Action.Pass -> pure (Text.pack "Pass")
  Action.Cast oid _ _ -> fmap (Text.pack "Cast " <>) (describeObject gs oid)
  Action.Play oid _ -> fmap (Text.pack "PlayLand " <>) (describeObject gs oid)
  Action.Activate oid _ -> fmap (Text.pack "Activate " <>) (describeObject gs oid)
  _ -> pure (Text.pack (show action))

-- | One line per failure, naming entries in the JSON a scenario writes them in.
render :: Failure.ScenarioFailure -> Text.Text
render failure = case failure of
  Failure.MkDuplicateLabel label -> Label.unwrap label <> Text.pack " labels two things on the board"
  Failure.MkUnknownActivePlayer label -> Label.unwrap label <> Text.pack " is active but has no seat"
  Failure.MkUnknownController label -> Label.unwrap label <> Text.pack " controls a placed card but has no seat"
  Failure.MkUnknownCard name -> Text.pack "no card named " <> CardName.unwrap name
  Failure.MkIllegalAttachment label -> Text.pack "a placement cannot be attached to " <> Label.unwrap label
  Failure.MkIllegalEmperor label -> Label.unwrap label <> Text.pack " is an emperor on no team, or not its team's only one"
  Failure.MkTokenOffBattlefield name -> Text.pack "a token " <> CardName.unwrap name <> Text.pack " placed off the battlefield"
  Failure.MkUnknownMonarch label -> Label.unwrap label <> Text.pack " is the monarch but has no seat"
  Failure.MkUnknownObject ref known ->
    Codec.Reference.toText ref
      <> Text.pack " names nothing in the game"
      <> (if known then Text.pack " (the board did label it, so it has since left)" else Text.empty)
  Failure.MkNotAnObject ref -> Codec.Reference.toText ref <> Text.pack " names a seat, not an object"
  Failure.MkNotAPlayer label -> Label.unwrap label <> Text.pack " names no seat"
  Failure.MkUnofferedObject key kind named offers ->
    renderWhen key <> Text.pack ": the " <> kind <> Text.pack " prompt did not offer " <> named <> renderOffers offers
  Failure.MkUnexpectedQualifier key kind ref ->
    renderWhen key <> Text.pack ": " <> kind <> Text.pack " has no source, so the source " <> Codec.Reference.toText ref <> Text.pack " matches nothing"
  Failure.MkNestedGamePrompt turn step decider kind ->
    renderLocation turn step decider <> Text.pack ": " <> kind <> Text.pack " was asked inside a nested game"
  Failure.MkUnscheduledPrompt turn step decider kind offers ->
    renderLocation turn step decider <> Text.pack ": nothing is scheduled for the " <> kind <> Text.pack " prompt" <> renderOffers offers
  Failure.MkUnexpectedPrompt key entry kind offers ->
    renderWhen key <> Text.pack ": " <> renderEntry entry <> Text.pack " does not answer the " <> kind <> Text.pack " prompt" <> renderOffers offers
  Failure.MkActionNotOffered key verb offers ->
    renderWhen key <> Text.pack ": " <> renderMove verb <> Text.pack " was not offered" <> renderOffers offers
  Failure.MkAmbiguousAction key verb offers ->
    renderWhen key <> Text.pack ": " <> renderMove verb <> Text.pack " matched more than one offer" <> renderOffers offers
  Failure.MkUnexpectedActionChoice key verb kind ->
    renderWhen key <> Text.pack ": " <> renderMove verb <> Text.pack " has no answer for the " <> kind <> Text.pack " prompt"
  Failure.MkUnusedActionChoices key verb choices ->
    renderWhen key <> Text.pack ": " <> renderMove verb <> Text.pack " finished without using " <> Common.render (Codec.encode Codec.Choices.codec choices)
  Failure.MkUnrefusedMove key verb next ->
    renderWhen key <> Text.pack ": " <> renderMove verb <> Text.pack " stood; " <> maybe (Text.pack "nothing was asked again") (\k -> Text.pack "the next prompt was " <> k) next
  Failure.MkUnreachedEntries turn step timed ->
    renderTimeds timed <> Text.pack " never came; the game stopped at " <> renderLocation turn step Nothing
  Failure.MkUnrunChecks turn step timed ->
    renderTimeds timed <> Text.pack " never ran; the game stopped at " <> renderLocation turn step Nothing
  Failure.MkCheckFailed key check observed ->
    maybe (Text.pack "at the end") renderWhen key
      <> Text.pack ": "
      <> Common.render (Codec.encode Codec.Check.codec check)
      <> Text.pack " failed; it was "
      <> observed

renderWhen :: When.When -> Text.Text
renderWhen key = renderLocation (When.turn key) (When.phase key) (Just (When.player key))

renderLocation :: Natural -> Phase.Phase -> Maybe Label.Label -> Text.Text
renderLocation turn step decider =
  Text.intercalate (Text.pack ", ") ([Text.pack ("turn " <> show turn), Text.pack (Codec.Phase.flatName step)] <> foldMap (pure . Label.unwrap) decider)

renderMove :: Move.Move -> Text.Text
renderMove = Common.render . Codec.encode Codec.Move.codec

renderEntry :: Entry.Entry -> Text.Text
renderEntry entry = case entry of
  Entry.Do verb -> renderMove verb
  Entry.Refuse verb -> Text.pack "refused " <> renderMove verb
  Entry.Expect check -> Common.render (Codec.encode Codec.Check.codec check)

renderTimeds :: Seq.Seq Timed.Timed -> Text.Text
renderTimeds = Text.intercalate (Text.pack "; ") . fmap (Common.render . Codec.encode Codec.Timed.codec) . Foldable.toList

renderOffers :: [Text.Text] -> Text.Text
renderOffers offers =
  if null offers
    then Text.empty
    else Text.pack "; it offered " <> Text.intercalate (Text.pack ", ") offers
