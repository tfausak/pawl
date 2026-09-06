-- Covers the board builder and keyed script interpreter in Pawl.Support: what a
-- structurally coherent board construction guarantees and what it does not, how
-- an entry is keyed to a moment and matched to a prompt, and which script
-- mistakes are reported rather than silently absorbed. The fixtures and
-- answerers in the rest of Pawl.Support are proved by the specs that use them.
module Pawl.SupportSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Support" $ do
  Spec.it s "a board is structurally coherent but not implicitly settled" $ do
    let piker = CardName.MkCardName (Text.pack "Goblin Piker")
        attacker =
          (S.objectSetup piker)
            { S.objectAlias = Just (S.MkObjectAlias (Text.pack "attacker")),
              S.objectDamage = 1,
              S.objectSickness = Sickness.Settled S.alice
            }
        alice =
          (S.playerSetup S.alice)
            { S.setupBattlefield = Seq.singleton attacker
            }
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.beginningOfCombat
    result <- S.buildBoard registry setup
    case result of
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right built -> do
        let raw = S.builtState built
            settled = S.runPure S.identityAnswer raw Engine.settleForPriority
        Spec.assertEqWith s "the raw board still has its lethally damaged creature" (S.creaturesInPlay S.alice raw) 1
        Spec.assertEqWith s "construction emitted no history" (S.eventsOf raw) []
        Spec.assertEqWith s "explicit settlement removes it" (S.creaturesInPlay S.alice settled) 0

  Spec.it s "duplicate aliases are rejected" $ do
    let same = S.aliased "same" (S.permanent "Goblin Piker")
        alice = (S.playerSetup S.alice) {S.setupBattlefield = Seq.fromList [same, same]}
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
    result <- S.buildBoard registry setup
    Spec.assertEqWith s "duplicate alias" result (Left (S.MkDuplicateAlias (S.MkObjectAlias (Text.pack "same"))))

  Spec.it s "unreached scheduled entries fail" $ do
    let setup = S.duel S.precombatMain [] []
        script = S.turn 1 [S.on S.precombatMain S.alice (S.attack [])]
    built <- S.buildBoardOrFail s registry setup
    case S.runScript script built (pure ()) of
      Left (S.MkUnreachedEntries _ _ entries) ->
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
    case (Map.lookup (S.MkObjectAlias (Text.pack "first")) (S.builtAliases built), Map.lookup (S.MkObjectAlias (Text.pack "second")) (S.builtAliases built)) of
      (Just first, Just second) -> do
        let prompt = Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.alice [first, second]
            askTwice = (,) <$> Game.ask prompt <*> Game.ask prompt
        case S.runScript script built askTwice of
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
    case Map.lookup (S.MkObjectAlias (Text.pack "attacker")) (S.builtAliases built) of
      Nothing -> Spec.assertFailure s "the board omitted an alias"
      Just attacker -> do
        let prompt = Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.bob [attacker]
            ask = Game.ask prompt
        case S.runScript (S.turn 1 [S.on S.declareAttackers S.alice attack]) built ask of
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right (chosen, _) -> Spec.assertEqWith s "alice's entry answered bob's prompt" chosen [attacker]
        case S.runScript (S.turn 1 [S.on S.declareAttackers S.bob attack]) built ask of
          Left (S.MkUnscheduledPrompt _ kind _) ->
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
    case S.runScript script built priority of
      Left (S.MkUnreachedEntries _ _ entries) ->
        Spec.assertEqWith s "ChooseAction passed and left the combat entry alone" entries script
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the unrelated combat entry was consumed"

  Spec.it s "a scheduled cast is selected from the offered actions" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        alice = S.hand S.alice [spell]
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        verb = S.castAction (S.aliasRef "spell") S.noChoices
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (S.MkObjectAlias (Text.pack "spell")) (S.builtAliases built) of
      Nothing -> Spec.assertFailure s "the board omitted the hand alias"
      Just oid -> do
        let action = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass, action]
        case S.runScript script built (Game.ask prompt) of
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right (chosen, after) -> do
            Spec.assertEqWith s "the offered cast" chosen action
            Spec.assertEqWith s "answering emitted no history itself" (S.eventsOf after) []

  Spec.it s "a scheduled action that was not offered fails" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        setup = S.board (S.hand S.alice [spell] NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        verb = S.castAction (S.aliasRef "spell") S.noChoices
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
        prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass]
    built <- S.buildBoardOrFail s registry setup
    case S.runScript script built (Game.ask prompt) of
      Left (S.MkActionNotOffered _ failed _) ->
        Spec.assertEqWith s "the rejected verb" failed verb
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the unoffered cast was accepted"

  Spec.it s "an underspecified action that matches two offers fails" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        setup = S.board (S.hand S.alice [spell] NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        verb = S.castAction (S.aliasRef "spell") S.noChoices
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (S.MkObjectAlias (Text.pack "spell")) (S.builtAliases built) of
      Nothing -> Spec.assertFailure s "the board omitted the hand alias"
      Just oid -> do
        let front = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            back = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker Back")) Facing.FaceUp
            prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [front, back]
        case S.runScript script built (Game.ask prompt) of
          Left (S.MkAmbiguousAction _ failed _) ->
            Spec.assertEqWith s "the ambiguous verb" failed verb
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right _ -> Spec.assertFailure s "the ambiguous cast was guessed"

  Spec.it s "attached choices are consumed by their action" $ do
    let spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
        target = S.aliased "target" (S.permanent "Goblin Piker")
        alice = (S.hand S.alice [spell]) {S.setupBattlefield = Seq.singleton target}
        setup = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
        choices =
          S.noChoices
            { S.choiceTargets = Just [S.MkObjectTarget (S.aliasRef "target")],
              S.choiceModes = Just (Seq.singleton (ModeIndex.MkModeIndex 1)),
              S.choiceX = Just 3,
              S.choiceCost = Just (ManaCost.MkManaCost [])
            }
        verb = S.castAction (S.aliasRef "spell") choices
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case (Map.lookup (S.MkObjectAlias (Text.pack "spell")) (S.builtAliases built), Map.lookup (S.MkObjectAlias (Text.pack "target")) (S.builtAliases built)) of
      (Just spellId, Just targetId) -> do
        let action = A.Cast spellId (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            actionPrompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass, action]
            mode = ModeIndex.MkModeIndex 1
            modePrompt = Prompt.ChooseModes (Decider.MkDecider S.alice) S.alice spellId (Set.singleton mode) (ModeSelection.ChooseExactly 1)
            xPrompt = Prompt.ChooseX (Decider.MkDecider S.alice) S.alice spellId 9
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
        case S.runScript script built asks of
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
        choices = S.noChoices {S.choiceX = Just 3}
        verb = S.castAction (S.aliasRef "spell") choices
        script = S.turn 1 [S.on S.precombatMain S.alice verb]
    built <- S.buildBoardOrFail s registry setup
    case Map.lookup (S.MkObjectAlias (Text.pack "spell")) (S.builtAliases built) of
      Nothing -> Spec.assertFailure s "the board omitted the hand alias"
      Just oid -> do
        let action = A.Cast oid (CardName.MkCardName (Text.pack "Goblin Piker")) Facing.FaceUp
            prompt = Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [A.Pass, action]
        case S.runScript script built (Game.ask prompt) of
          Left (S.MkUnusedActionChoices _ failed _) ->
            Spec.assertEqWith s "the unfinished verb" failed verb
          Left failure -> Spec.assertFailure s (S.renderFailure failure)
          Right _ -> Spec.assertFailure s "the unused X was ignored"

  Spec.it s "a scheduled defender must be offered" $ do
    let setup = S.board (S.playerSetup S.alice NonEmpty.:| [S.playerSetup S.bob, S.playerSetup S.carol]) S.alice S.beginningOfCombat
        verb = S.chooseDefender S.carol
        script = S.turn 1 [S.on S.beginningOfCombat S.alice verb]
        prompt = Prompt.ChooseDefender (Decider.MkDecider S.alice) S.alice (S.bob NonEmpty.:| [S.carol])
    built <- S.buildBoardOrFail s registry setup
    case S.runScript script built (Game.ask prompt) of
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right (chosen, _) -> Spec.assertEqWith s "the named defender" chosen S.carol

  Spec.it s "concession is opt-in at its scheduled moment" $ do
    let setup = S.duel S.precombatMain [] []
        concede = S.turn 1 [S.on S.precombatMain S.alice S.MkConcede]
    built <- S.buildBoardOrFail s registry setup
    case S.runScript concede built (Game.ask (Prompt.Concede S.alice)) of
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right (answer, _) ->
        Spec.assertEqWith s "scheduled concession" answer Concession.Concedes

  Spec.it s "an unscheduled non-action prompt fails" $ do
    let setup = S.duel S.beginningOfCombat [S.ready (S.permanent "Goblin Piker")] []
    built <- S.buildBoardOrFail s registry setup
    case S.runScript Seq.empty built S.combatGame of
      Left (S.MkUnscheduledPrompt _ kind _) ->
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
    -- Both Pikers are alice's, so "Goblin Piker 1" resolves; only the untapped
    -- one is a legal attacker (CR 508.1a), so the prompt never offers the first.
    -- The engine would filter it out AFTER the entry was popped, leaving a green
    -- script with nobody attacking.
    let tapped = (S.ready (S.permanent "Goblin Piker")) {S.objectTapState = TapState.Tapped}
        setup = S.duel S.declareAttackers [tapped, S.ready (S.permanent "Goblin Piker")] []
        script = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.namedRef "Goblin Piker" 1])]
    built <- S.buildBoardOrFail s registry setup
    case S.runScript script built S.combatGame of
      Left (S.MkUnofferedObject _ kind named offers) -> do
        Spec.assertEqWith s "the unoffered reference" named (Text.pack "Goblin Piker 1")
        Spec.assertEqWith s "the prompt it came from" kind (Text.pack "DeclareAttackers")
        Spec.assertEqWith s "what it did offer" offers [Text.pack "Goblin Piker 2"]
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
                    [ (S.MkCreatureRecipient (S.aliasRef "right first"), 1),
                      (S.MkCreatureRecipient (S.aliasRef "right second"), 1)
                    ]
                ),
              S.onSource
                S.combatDamage
                S.alice
                left
                ( S.assignDamage
                    [ (S.MkCreatureRecipient (S.aliasRef "left first"), 1),
                      (S.MkCreatureRecipient (S.aliasRef "left second"), 1)
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
    case S.runScript script built S.combatGame of
      Left (S.MkUnexpectedQualifier _ kind ref) -> do
        Spec.assertEqWith s "the prompt that has no source" kind (Text.pack "DeclareAttackers")
        Spec.assertEqWith s "the qualifier it carried" ref attacker
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the dangling qualifier was ignored"
