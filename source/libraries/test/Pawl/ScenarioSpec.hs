-- Covers Pawl.Scenario: what placing a board guarantees and what it does not,
-- how an entry is keyed to a moment and matched to a prompt, when a check runs,
-- and which scenario mistakes are reported rather than silently absorbed. The
-- scenarios under data/scenarios are its gameplay-level cases.
module Pawl.ScenarioSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Codec.Scenario as Codec.Scenario
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Validate as Validate
import qualified Pawl.Registry as Registry
import qualified Pawl.Scenario as Scenario
import qualified Pawl.Scenario.Load as Load
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.Board as Board
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Choices as Choices
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Move as Move
import qualified Pawl.Types.Placement as Placement
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Readiness as Readiness
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scenario as Scenario.Type
import qualified Pawl.Types.ScenarioFailure as ScenarioFailure
import qualified Pawl.Types.Seat as Seat
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Staged as Staged
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TeamId as TeamId
import qualified Pawl.Types.When as When
import qualified System.Directory as Directory

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Scenario" $ do
  Spec.it s "a board is structurally coherent but not implicitly settled" $ do
    let piker = CardName.MkCardName (Text.pack "Goblin Piker")
        attacker =
          (S.objectSetup piker)
            { Placement.label = Just (Label.MkLabel (Text.pack "attacker")),
              Placement.damage = 1,
              Placement.readiness = Readiness.Ready
            }
        alice =
          (S.playerSetup S.alice)
            { Seat.battlefield = Seq.singleton attacker
            }
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.beginningOfCombat
    result <- Scenario.stage registry setup
    case result of
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right built -> do
        let raw = Staged.state built
            settled = S.runPure S.identityAnswer raw Engine.settleForPriority
        Spec.assertEqWith s "the raw board still has its lethally damaged creature" (S.creaturesInPlay S.alice raw) 1
        Spec.assertEqWith s "construction emitted no history" (S.eventsOf raw) []
        Spec.assertEqWith s "explicit settlement removes it" (S.creaturesInPlay S.alice settled) 0

  Spec.it s "duplicate aliases are rejected" $ do
    let same = S.aliased "same" (S.permanent "Goblin Piker")
        alice = (S.playerSetup S.alice) {Seat.battlefield = Seq.fromList [same, same]}
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
    result <- Scenario.stage registry setup
    Spec.assertEqWith s "duplicate alias" result (Left (ScenarioFailure.MkDuplicateLabel (Label.MkLabel (Text.pack "same"))))

  Spec.it s "CR 809.2 an emperor seat on no team is rejected" $ do
    let alice = (S.playerSetup S.alice) {Seat.emperor = True}
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
    result <- Scenario.stage registry setup
    Spec.assertEqWith s "teamless emperor" result (Left (ScenarioFailure.MkIllegalEmperor (Label.MkLabel (Text.pack "alice"))))

  Spec.it s "CR 810.4 teammates showing different life totals on a shared-life board are rejected" $ do
    let alice = (S.playerSetup S.alice) {Seat.team = Just (TeamId.MkTeamId 1), Seat.life = 7}
        bob = (S.playerSetup S.bob) {Seat.team = Just (TeamId.MkTeamId 1)}
        setup = (S.board (alice NonEmpty.:| [bob]) S.alice S.precombatMain) {Board.sharedTeamLife = True}
    result <- Scenario.stage registry setup
    Spec.assertEqWith s "unshared life" result (Left (ScenarioFailure.MkUnsharedLife (Label.MkLabel (Text.pack "alice"))))

  Spec.it s "CR 111.7 a token placed off the battlefield is rejected" $ do
    let token = (S.permanent "Goblin Piker") {Placement.token = True}
        alice = (S.playerSetup S.alice) {Seat.hand = Seq.singleton token}
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
    result <- Scenario.stage registry setup
    Spec.assertEqWith s "token in hand" result (Left (ScenarioFailure.MkTokenOffBattlefield (CardName.MkCardName (Text.pack "Goblin Piker"))))

  Spec.it s "CR 712.8 a placement showing a face its card lacks is rejected" $ do
    let shown = (S.permanent "Goblin Piker") {Placement.face = Just (CardName.MkCardName (Text.pack "Nightfall Predator"))}
        alice = (S.playerSetup S.alice) {Seat.battlefield = Seq.singleton shown}
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
    result <- Scenario.stage registry setup
    Spec.assertEqWith s "unknown face" result (Left (ScenarioFailure.MkUnknownFace (CardName.MkCardName (Text.pack "Goblin Piker")) (CardName.MkCardName (Text.pack "Nightfall Predator"))))

  Spec.it s "CR 310.9 a protector naming no seat is rejected" $ do
    let battle = (S.permanent "Invasion of Dominaria") {Placement.protector = Just (Label.MkLabel (Text.pack "carol"))}
        alice = (S.playerSetup S.alice) {Seat.battlefield = Seq.singleton battle}
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
    result <- Scenario.stage registry setup
    Spec.assertEqWith s "unknown protector" result (Left (ScenarioFailure.MkUnknownProtector (Label.MkLabel (Text.pack "carol"))))

  Spec.it s "unreached scheduled entries fail" $ do
    let setup = S.duel S.precombatMain [] []
        script = S.turn 1 [S.on S.precombatMain S.alice (S.attack [])]
    built <- S.buildBoardOrFail s registry setup
    case Scenario.rehearse script built (pure ()) of
      Left (ScenarioFailure.MkUnreachedEntries _ _ entries) ->
        Spec.assertEqWith s "the unreached entry" entries script
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the unreached entry was silently ignored"

  Spec.it s "entries at the same moment are consumed in source order" $ do
    let setup =
          S.duel
            S.declareAttackers
            [S.settled "first" "Goblin Piker", S.settled "second" "Goblin Piker"]
            []
        script =
          S.turn
            1
            [ S.on S.declareAttackers S.alice (S.attack [S.aliasRef "first"]),
              S.on S.declareAttackers S.alice (S.attack [S.namedRef "Goblin Piker" 2])
            ]
    built <- S.buildBoardOrFail s registry setup
    case (Map.lookup (Label.MkLabel (Text.pack "first")) (Staged.objects built), Map.lookup (Label.MkLabel (Text.pack "second")) (Staged.objects built)) of
      (Just first, Just second) -> do
        let prompt = Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.alice [first, second]
            askTwice = (,) <$> Game.ask prompt <*> Game.ask prompt
        case Scenario.rehearse script built askTwice of
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right (chosen, _) ->
            Spec.assertEqWith s "same-moment source order" chosen ([first], [second])
      _ -> Spec.assertFailure s "the board omitted an alias"

  Spec.it s "CR 723.5 a script is keyed on the decider, not the affected player" $ do
    -- alice decides for bob. An entry written under alice answers the prompt;
    -- one written under bob is never reached, because bob answers nothing.
    let setup = S.duel S.declareAttackers [S.settled "attacker" "Goblin Piker"] []
        attack = S.attack [S.aliasRef "attacker"]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (Label.MkLabel (Text.pack "attacker")) (Staged.objects built) of
      Nothing -> Spec.assertFailure s "the board omitted an alias"
      Just attacker -> do
        let prompt = Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.bob [attacker]
            ask = Game.ask prompt
        case Scenario.rehearse (S.turn 1 [S.on S.declareAttackers S.alice attack]) built ask of
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right (chosen, _) -> Spec.assertEqWith s "alice's entry answered bob's prompt" chosen [attacker]
        case Scenario.rehearse (S.turn 1 [S.on S.declareAttackers S.bob attack]) built ask of
          Left (ScenarioFailure.MkUnscheduledPrompt _ _ _ kind _) ->
            Spec.assertEqWith s "bob's entry answered nothing" kind (Text.pack "DeclareAttackers")
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right _ -> Spec.assertFailure s "an entry keyed on the affected player was consumed"

  Spec.it s "unoffered actions pass without consuming a later combat entry" $ do
    let setup = S.duel S.precombatMain [] []
        script = S.turn 1 [S.on S.precombatMain S.alice (S.attack [])]
    built <- S.buildBoardOrFail s registry setup
    let priority = do
          State.modify' (\gs -> gs {GameState.priority = Just S.alice})
          Engine.priorityLoop
    case Scenario.rehearse script built priority of
      Left (ScenarioFailure.MkUnreachedEntries _ _ entries) ->
        Spec.assertEqWith s "ChooseAction passed and left the combat entry alone" entries script
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the unrelated combat entry was consumed"

  Spec.it s "a scheduled cast is selected from the offered actions" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        alice = S.hand S.alice [spell]
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        verb = S.castAction (S.aliasRef "spell") Choices.none
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (Label.MkLabel (Text.pack "spell")) (Staged.objects built) of
      Nothing -> Spec.assertFailure s "the board omitted the hand alias"
      Just oid -> do
        let action = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass, action]
        case Scenario.rehearse script built (Game.ask prompt) of
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right (chosen, after) -> do
            Spec.assertEqWith s "the offered cast" chosen action
            Spec.assertEqWith s "answering emitted no history itself" (S.eventsOf after) []

  Spec.it s "a scheduled action that was not offered fails" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        setup = S.board (S.hand S.alice [spell] NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        verb = S.castAction (S.aliasRef "spell") Choices.none
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
        prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass]
    built <- S.buildBoardOrFail s registry setup
    case Scenario.rehearse script built (Game.ask prompt) of
      Left (ScenarioFailure.MkActionNotOffered _ failed _) ->
        Spec.assertEqWith s "the rejected verb" failed verb
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the unoffered cast was accepted"

  Spec.it s "an underspecified action that matches two offers fails" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        setup = S.board (S.hand S.alice [spell] NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        verb = S.castAction (S.aliasRef "spell") Choices.none
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (Label.MkLabel (Text.pack "spell")) (Staged.objects built) of
      Nothing -> Spec.assertFailure s "the board omitted the hand alias"
      Just oid -> do
        let front = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            back = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker Back")) Facing.FaceUp
            prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [front, back]
        case Scenario.rehearse script built (Game.ask prompt) of
          Left (ScenarioFailure.MkAmbiguousAction _ failed _) ->
            Spec.assertEqWith s "the ambiguous verb" failed verb
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right _ -> Spec.assertFailure s "the ambiguous cast was guessed"

  Spec.it s "attached choices are consumed by their action" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        target = S.aliased "target" (S.permanent "Goblin Piker")
        alice = (S.hand S.alice [spell]) {Seat.battlefield = Seq.singleton target}
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        choices =
          Choices.none
            { Choices.targets = Just [S.aliasRef "target"],
              Choices.modes = Just (Seq.singleton (ModeIndex.MkModeIndex 1)),
              Choices.x = Just 3,
              Choices.cost = Just (ManaCost.MkManaCost [])
            }
        verb = S.castAction (S.aliasRef "spell") choices
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case (Map.lookup (Label.MkLabel (Text.pack "spell")) (Staged.objects built), Map.lookup (Label.MkLabel (Text.pack "target")) (Staged.objects built)) of
      (Just spellId, Just targetId) -> do
        let action = A.Cast spellId (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            actionPrompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass, action]
            mode = ModeIndex.MkModeIndex 1
            modePrompt = Prompt.ChooseModes (Decider.MkDecider S.alice) S.alice spellId (Set.singleton mode) (ModeSelection.ChooseExactly 1)
            xPrompt = Prompt.ChooseX (Decider.MkDecider S.alice) S.alice spellId 0 9
            cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost []), Cost.components = []}
            costPrompt = Prompt.ChooseCost (Decider.MkDecider S.alice) S.alice spellId [cost]
            slot = SlotName.MkSlotName (Text.pack "target")
            targetPrompt =
              Prompt.ChooseTargets
                (Decider.MkDecider S.alice)
                S.alice
                spellId
                (Map.singleton slot (1, Set.singleton (Recipient.ToCreature targetId)))
            asks = (,,,,) <$> Game.ask actionPrompt <*> Game.ask modePrompt <*> Game.ask xPrompt <*> Game.ask costPrompt <*> Game.ask targetPrompt
        case Scenario.rehearse script built asks of
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right ((chosen, modes, x, chosenCost, targets), _) -> do
            Spec.assertEqWith s "the priority action" chosen action
            Spec.assertEqWith s "the action's modes" modes (Seq.singleton mode)
            Spec.assertEqWith s "the action's X" x 3
            Spec.assertEqWith s "the action's cost" chosenCost cost
            Spec.assertEqWith s "the action's target" targets (Map.singleton slot (Set.singleton (Recipient.ToCreature targetId)))
      _ -> Spec.assertFailure s "the board omitted an action alias"

  Spec.it s "an unused attached choice fails" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        setup = S.board (S.hand S.alice [spell] NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        choices = Choices.none {Choices.x = Just 3}
        verb = S.castAction (S.aliasRef "spell") choices
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (Label.MkLabel (Text.pack "spell")) (Staged.objects built) of
      Nothing -> Spec.assertFailure s "the board omitted the hand alias"
      Just oid -> do
        let action = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass, action]
        case Scenario.rehearse script built (Game.ask prompt) of
          Left (ScenarioFailure.MkUnusedActionChoices _ failed _) ->
            Spec.assertEqWith s "the unfinished verb" failed verb
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right _ -> Spec.assertFailure s "the unused X was ignored"

  Spec.it s "a leftover choice fails at the next prompt rather than answering it" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        setup = S.board (S.hand S.alice [spell] NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        choices = Choices.none {Choices.manaSources = Seq.singleton Nothing}
        verb = S.castAction (S.aliasRef "spell") choices
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (Label.MkLabel (Text.pack "spell")) (Staged.objects built) of
      Nothing -> Spec.assertFailure s "the board omitted the hand alias"
      Just oid -> do
        let action = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass, action]
        case Scenario.rehearse script built (Game.ask prompt *> Game.ask prompt) of
          Left (ScenarioFailure.MkUnusedActionChoices _ failed _) ->
            Spec.assertEqWith s "the unfinished verb" failed verb
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right _ -> Spec.assertFailure s "the leftover mana source was carried into the next priority"

  Spec.it s "another decider's sub-choice is not answered from the pending action" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        setup = S.board (S.hand S.alice [spell] NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        choices = Choices.none {Choices.x = Just 3}
        verb = S.castAction (S.aliasRef "spell") choices
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (Label.MkLabel (Text.pack "spell")) (Staged.objects built) of
      Nothing -> Spec.assertFailure s "the board omitted the hand alias"
      Just oid -> do
        let action = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            actionPrompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass, action]
            xPrompt = Prompt.ChooseX (Decider.MkDecider S.bob) S.bob oid 0 9
        case Scenario.rehearse script built (Game.ask actionPrompt *> Game.ask xPrompt) of
          Left (ScenarioFailure.MkUnusedActionChoices _ failed _) ->
            Spec.assertEqWith s "the unfinished verb" failed verb
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right _ -> Spec.assertFailure s "bob's X was answered from alice's cast"

  Spec.it s "an action queued behind an unreached entry is still taken" $ do
    let land = S.aliased "land" (S.cardSetup "Mountain")
        setup = S.board (S.hand S.alice [land] NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        stranded = S.on S.precombatMain S.alice (S.attack [])
        script = S.turn 1 [stranded, S.on S.precombatMain S.alice (S.playLand (S.aliasRef "land"))]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (Label.MkLabel (Text.pack "land")) (Staged.objects built) of
      Nothing -> Spec.assertFailure s "the board omitted the hand alias"
      Just oid -> do
        let action = A.Play oid Nothing
            prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass, action]
        case Scenario.rehearse script built (Game.ask prompt) of
          Left (ScenarioFailure.MkUnreachedEntries _ _ entries) ->
            Spec.assertEqWith s "only the stranded entry remains" entries (S.turn 1 [stranded])
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right _ -> Spec.assertFailure s "the stranded entry was consumed"

  Spec.it s "a scheduled defender must be offered" $ do
    let setup = S.board (S.playerSetup S.alice NonEmpty.:| [S.playerSetup S.bob, S.playerSetup S.carol]) S.alice S.beginningOfCombat
        verb = S.chooseDefender S.carol
        script = S.turn 1 [S.on S.beginningOfCombat S.alice verb]
        prompt = Prompt.ChooseDefender (Decider.MkDecider S.alice) S.alice (S.bob NonEmpty.:| [S.carol])
    built <- S.buildBoardOrFail s registry setup
    case Scenario.rehearse script built (Game.ask prompt) of
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right (chosen, _) -> Spec.assertEqWith s "the named defender" chosen S.carol

  Spec.it s "concession is opt-in at its scheduled moment" $ do
    let setup = S.duel S.precombatMain [] []
        concede = S.turn 1 [S.on S.precombatMain S.alice Move.Concede]
    built <- S.buildBoardOrFail s registry setup
    case Scenario.rehearse concede built (Game.ask (Prompt.Concede S.alice)) of
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right (answer, _) ->
        Spec.assertEqWith s "scheduled concession" answer Concession.Concedes

  Spec.it s "an unscheduled non-action prompt fails" $ do
    let setup = S.duel S.beginningOfCombat [S.ready (S.permanent "Goblin Piker")] []
    built <- S.buildBoardOrFail s registry setup
    case Scenario.rehearse Seq.empty built S.combatGame of
      Left (ScenarioFailure.MkUnscheduledPrompt _ _ _ kind _) ->
        Spec.assertEqWith s "the prompt kind" kind (Text.pack "DeclareAttackers")
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the attackers prompt was silently answered"

  Spec.it s "a board past the beginning of combat can attack" $ do
    -- CR 506.2 / CR 703.4h: the defending player is settled during the beginning
    -- of combat step, so a board positioned AFTER it has to arrive with that
    -- done. Without it Combat.declareAttackers finds no defending player, skips
    -- its prompt, and the attack entry reports itself as never reached.
    let setup = S.duel S.declareAttackers [S.settled "attacker" "Goblin Piker"] []
        script = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.aliasRef "attacker"])]
    after <- S.play s registry setup script S.combatGame
    Spec.assertEqWith s "bob took the attacker's two" (S.lifeOf S.bob after) (Just 18)

  Spec.it s "a reference the prompt did not offer is a failure, not a dropped entry" $ do
    -- Both Pikers are alice's, so "Goblin Piker" resolves; only the untapped
    -- one is a legal attacker (CR 508.1a), so the prompt never offers the first.
    -- The engine would filter it out AFTER the entry was popped, leaving a green
    -- script with nobody attacking.
    let tapped = (S.ready (S.permanent "Goblin Piker")) {Placement.tapped = TapState.Tapped}
        setup = S.duel S.declareAttackers [tapped, S.ready (S.permanent "Goblin Piker")] []
        script = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.namedRef "Goblin Piker" 1])]
    built <- S.buildBoardOrFail s registry setup
    case Scenario.rehearse script built S.combatGame of
      Left (ScenarioFailure.MkUnofferedObject _ kind named offers) -> do
        Spec.assertEqWith s "the unoffered reference" named (Text.pack "Goblin Piker")
        Spec.assertEqWith s "the prompt it came from" kind (Text.pack "DeclareAttackers")
        Spec.assertEqWith s "what it did offer" offers [Text.pack "Goblin Piker#2"]
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the unoffered attacker was silently dropped"

  Spec.it s "a qualified assignment is matched by its source, not by its position" $ do
    -- Two double-blocked attackers are prompted in the engine's order over
    -- Combat.attackers, which the script does not know. The assignments are
    -- listed in the opposite order on purpose: matching by position answers the
    -- first prompt with the second attacker's map.
    let setup =
          S.duel
            S.beginningOfCombat
            [S.settled "left" "Goblin Piker", S.settled "right" "Goblin Piker"]
            [ S.settled "left first" "Goblin Piker",
              S.settled "left second" "Goblin Piker",
              S.settled "right first" "Goblin Piker",
              S.settled "right second" "Goblin Piker"
            ]
        left = S.aliasRef "left"
        right = S.aliasRef "right"
        script =
          S.turn
            1
            [ S.on S.declareAttackers S.alice (S.attack [left, right]),
              S.on
                S.declareBlockers
                S.bob
                ( S.block
                    [ (S.aliasRef "left first", left),
                      (S.aliasRef "left second", left),
                      (S.aliasRef "right first", right),
                      (S.aliasRef "right second", right)
                    ]
                ),
              S.onSource
                S.combatDamage
                S.alice
                right
                ( S.assignDamage
                    [ (S.aliasRef "right first", 1),
                      (S.aliasRef "right second", 1)
                    ]
                ),
              S.onSource
                S.combatDamage
                S.alice
                left
                ( S.assignDamage
                    [ (S.aliasRef "left first", 1),
                      (S.aliasRef "left second", 1)
                    ]
                )
            ]
    fought <- S.play s registry setup script S.combatGame
    -- Each Piker is a 2/1, so one point is lethal: every blocker dies.
    Spec.assertEqWith s "all four blockers were assigned lethal damage" (S.creaturesInPlay S.bob (S.settleSba fought)) 0

  Spec.it s "a qualifier on a prompt with no source is a script error" $ do
    let attacker = S.aliasRef "attacker"
        setup = S.duel S.declareAttackers [S.settled "attacker" "Goblin Piker"] []
        script = S.turn 1 [S.onSource S.declareAttackers S.alice attacker (S.attack [attacker])]
    built <- S.buildBoardOrFail s registry setup
    case Scenario.rehearse script built S.combatGame of
      Left (ScenarioFailure.MkUnexpectedQualifier _ kind ref) -> do
        Spec.assertEqWith s "the prompt that has no source" kind (Text.pack "DeclareAttackers")
        Spec.assertEqWith s "the qualifier it carried" ref attacker
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the dangling qualifier was ignored"

  -- CR 733.1: a refused move is one the engine reverses and asks for again, so
  -- a legal declaration under `refuse` stands and fails the run.
  Spec.it s "a refused move that stands fails, naming the prompt asked next" $ do
    result <-
      runJson
        s
        registry
        ( "{\"description\":\"x\",\"board\":{\"seats\":[{\"name\":\"alice\",\"battlefield\":[{\"card\":\"Goblin Piker\",\"label\":\"attacker\",\"ready\":true}]},{\"name\":\"bob\",\"battlefield\":[{\"card\":\"Goblin Piker\",\"ready\":true}]}],\"active\":\"alice\",\"step\":\"DeclareAttackers\"},"
            <> "\"timeline\":[{\"turn\":1,\"step\":\"DeclareAttackers\",\"player\":\"alice\",\"do\":{\"Attack\":[\"$attacker\"]}},"
            <> "{\"turn\":1,\"step\":\"DeclareBlockers\",\"player\":\"bob\",\"refuse\":{\"Block\":{}}}]}"
        )
    case result of
      Left (ScenarioFailure.MkUnrefusedMove key verb next) -> do
        Spec.assertEqWith s "the moment it answered" key (When.MkWhen 1 S.declareBlockers (S.seatLabel S.bob))
        Spec.assertEqWith s "the move that stood" verb (Move.Block Map.empty)
        Spec.assertBool s (next /= Just (Text.pack "DeclareBlockers")) "the declaration was asked again"
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "a declaration that stood passed as refused"

  -- The two ways a check can fail are never spelled alike: one that ran and
  -- read false, and one whose moment never came and so asserted nothing.
  Spec.it s "a check that reads false fails as a false check, naming what it saw" $ do
    result <- runJson s registry (attackThen "{\"turn\":1,\"step\":\"EndOfCombat\",\"player\":\"alice\",\"check\":{\"Life\":{\"player\":\"bob\",\"life\":17}}}" "[]")
    case result of
      Left (ScenarioFailure.MkCheckFailed (Just key) _ observed) -> do
        Spec.assertEqWith s "the moment it ran at" key (When.MkWhen 1 S.endOfCombat (S.seatLabel S.alice))
        Spec.assertEqWith s "what it saw" observed (Text.pack "18")
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "a false check passed"

  Spec.it s "a check whose moment never comes fails as unrun, not as false" $ do
    -- CR 514.3: no player receives priority in the cleanup step, so a check
    -- waiting for alice's priority there never runs.
    result <- runJson s registry (attackThen "{\"turn\":1,\"step\":\"Cleanup\",\"player\":\"alice\",\"check\":{\"Life\":{\"player\":\"bob\",\"life\":18}}}" "[]")
    case result of
      Left (ScenarioFailure.MkUnrunChecks _ _ entries) -> Spec.assertEqWith s "the unrun check" (length entries) 1
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "an unrun check passed"

  Spec.it s "a final check runs where the run stopped, once the timeline is spent" $ do
    -- The attack is the last entry, so the run stops after the declare
    -- attackers step, before any damage: bob is still at 20 there.
    result <- runJson s registry (attackThen "" "[{\"Life\":{\"player\":\"bob\",\"life\":18}}]")
    case result of
      Left (ScenarioFailure.MkCheckFailed Nothing _ observed) -> Spec.assertEqWith s "bob before damage" observed (Text.pack "20")
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the final check read a state past the run's end"

  Spec.it s "a label naming a seat is not an object" $ do
    result <- runJson s registry (attackThenAttacking "$bob")
    case result of
      Left (ScenarioFailure.MkNotAnObject _) -> pure ()
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "a seat was declared as an attacker"

-- Every scenario under data/scenarios: each file matches the schema pawl
-- emits for it, and runs clean.
-- The corpus is one directory per source spec, so a root's scenarios are every
-- .json file beneath it, ascending by path.
loadSpec :: Spec.Spec IO n -> n ()
loadSpec s = Spec.describe s "Pawl.Scenario.Load" $ do
  Spec.it s "loadRoot finds scenarios in subdirectories, ascending by path" $ do
    found <- S.withCorpusDir "scenario-load-nested" [("b.json", Text.pack "{}"), ("notes.txt", Text.empty)] $ \dir -> do
      Directory.createDirectoryIfMissing True (dir <> "/sub")
      ByteString.writeFile (dir <> "/sub/a.json") (Encoding.encodeUtf8 (Text.pack "{}"))
      loaded <- Load.loadRoot dir
      pure (fmap (drop (length dir) . fst) loaded)
    Spec.assertEqWith s "both .json files, the nested one included" found ["/b.json", "/sub/a.json"]

corpusSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> [(FilePath, Either Text.Text Scenario.Type.Scenario)] -> n ()
corpusSpec s registry = Spec.describe s "Scenarios" . mapM_ (uncurry (scenarioCase s registry))

scenarioCase :: Spec.Spec IO n -> Registry.Registry IO -> FilePath -> Either Text.Text Scenario.Type.Scenario -> n ()
scenarioCase s registry path decoded =
  let file = reverse (takeWhile (/= '/') (reverse path))
      name = case decoded of
        Left _ -> file
        Right scenario -> file <> ": " <> Text.unpack (Scenario.Type.description scenario)
   in Spec.it s name $ case decoded of
        Left problem -> Spec.assertFailure s (path <> ": " <> Text.unpack problem)
        Right scenario -> do
          bytes <- ByteString.readFile path
          case Encoding.decodeUtf8' bytes of
            Left problem -> Spec.assertFailure s (path <> ": " <> show problem)
            Right contents -> case Common.parse contents of
              Left problem -> Spec.assertFailure s (path <> ": " <> Text.unpack problem)
              Right value -> Spec.assertEqWith s "matches the scenario schema" (Validate.validate (Define.run (Codec.schema Codec.Scenario.codec)) value) []
          result <- Scenario.run registry scenario
          case result of
            Left failure -> Spec.assertFailure s (S.renderFailure failure)
            Right _ -> pure ()

-- One scenario decoded and run, failing the case if it will not decode.
runJson :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m (Either ScenarioFailure.ScenarioFailure GameState.GameState)
runJson s registry json = case Common.parse (Text.pack json) >>= Codec.decode Codec.Scenario.codec of
  Left problem -> Spec.assertFailure s (Text.unpack problem)
  Right scenario -> Scenario.run registry scenario

-- alice's settled Piker attacks bob, then `entry` (if any) and `final`.
attackThen :: String -> String -> String
attackThen entry final =
  "{\"description\":\"x\",\"board\":{\"seats\":[{\"name\":\"alice\",\"battlefield\":[{\"card\":\"Goblin Piker\",\"label\":\"attacker\",\"ready\":true}]},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"DeclareAttackers\"},"
    <> "\"timeline\":[{\"turn\":1,\"step\":\"DeclareAttackers\",\"player\":\"alice\",\"do\":{\"Attack\":[\"$attacker\"]}}"
    <> (if null entry then "" else "," <> entry)
    <> "],\"final\":"
    <> final
    <> "}"

-- The same board, attacking with whatever `attacker` names.
attackThenAttacking :: String -> String
attackThenAttacking attacker =
  "{\"description\":\"x\",\"board\":{\"seats\":[{\"name\":\"alice\",\"battlefield\":[{\"card\":\"Goblin Piker\",\"label\":\"attacker\",\"ready\":true}]},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"DeclareAttackers\"},"
    <> "\"timeline\":[{\"turn\":1,\"step\":\"DeclareAttackers\",\"player\":\"alice\",\"do\":{\"Attack\":[\""
    <> attacker
    <> "\"]}}]}"
