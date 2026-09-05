-- Covers the arrangement builder and keyed script interpreter in Pawl.Support:
-- what a structurally coherent board construction guarantees and what it does
-- not, how a command is keyed to a moment and matched to a prompt, and which
-- script mistakes are reported rather than silently absorbed. The fixtures and
-- answerers in the rest of Pawl.Support are proved by the specs that use them.
module Pawl.SupportSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Support" $ do
  Spec.it s "an arrangement is structurally coherent but not implicitly settled" $ do
    let piker = CardName.MkCardName (Text.pack "Goblin Piker")
        attacker =
          (S.objectSetup piker)
            { S.objectAlias = Just (S.MkObjectAlias (Text.pack "attacker")),
              S.objectDamage = 1,
              S.objectReadiness = S.MkReady
            }
        alice =
          (S.playerSetup S.alice)
            { S.setupBattlefield = Seq.singleton attacker
            }
        setup =
          S.arrangement
            (alice NonEmpty.:| [S.playerSetup S.bob])
            S.alice
            (Phase.Combat CombatStep.BeginningOfCombat)
    result <- S.arrange registry setup
    case result of
      Left failure -> Spec.assertFailure s (show failure)
      Right built -> do
        let raw = S.builtState built
            settled = S.runPure S.identityAnswer raw Engine.settleForPriority
        Spec.assertEqWith s "the raw arrangement still has its lethally damaged creature" (S.creaturesInPlay S.alice raw) 1
        Spec.assertEqWith s "construction emitted no history" (S.eventsOf raw) []
        Spec.assertEqWith s "explicit settlement removes it" (S.creaturesInPlay S.alice settled) 0

  Spec.it s "duplicate aliases are rejected" $ do
    let piker = CardName.MkCardName (Text.pack "Goblin Piker")
        same = Just (S.MkObjectAlias (Text.pack "same"))
        first = (S.objectSetup piker) {S.objectAlias = same}
        second = (S.objectSetup piker) {S.objectAlias = same}
        alice = (S.playerSetup S.alice) {S.setupBattlefield = Seq.fromList [first, second]}
        setup = S.arrangement (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice Phase.PrecombatMain
    result <- S.arrange registry setup
    Spec.assertEqWith s "duplicate alias" result (Left (S.MkDuplicateAlias (S.MkObjectAlias (Text.pack "same"))))

  Spec.it s "unreached scheduled commands fail" $ do
    let setup = S.arrangement (S.playerSetup S.alice NonEmpty.:| [S.playerSetup S.bob]) S.alice Phase.PrecombatMain
        moment = S.MkMoment (S.MkTurnNumber 1) Phase.PrecombatMain S.alice
        scheduled = S.MkScheduled moment Nothing (S.MkAttack Seq.empty)
    result <- S.arrange registry setup
    case result of
      Left failure -> Spec.assertFailure s (show failure)
      Right built ->
        case S.runScript (Seq.singleton scheduled) built (pure ()) of
          Left (S.MkUnreachedCommands _ _ commands) ->
            Spec.assertEqWith s "the unreached command" commands (Seq.singleton scheduled)
          Left failure -> Spec.assertFailure s (show failure)
          Right _ -> Spec.assertFailure s "the unreached command was silently ignored"

  Spec.it s "commands at the same moment are consumed in source order" $ do
    let firstAlias = S.MkObjectAlias (Text.pack "first")
        secondAlias = S.MkObjectAlias (Text.pack "second")
        permanent alias = S.ready (S.aliased alias (S.permanent "Goblin Piker"))
        alice = S.battlefield S.alice [permanent "first", permanent "second"]
        setup =
          S.arrangement
            (alice NonEmpty.:| [S.playerSetup S.bob])
            S.alice
            (Phase.Combat CombatStep.DeclareAttackers)
        moment = S.MkMoment (S.MkTurnNumber 1) (Phase.Combat CombatStep.DeclareAttackers) S.alice
        script =
          Seq.fromList
            [ S.MkScheduled moment Nothing (S.attack [S.aliasRef "first"]),
              S.MkScheduled moment Nothing (S.attack [S.namedRef "Goblin Piker" 2])
            ]
    result <- S.arrange registry setup
    case result of
      Left failure -> Spec.assertFailure s (show failure)
      Right built ->
        case (Map.lookup firstAlias (S.builtAliases built), Map.lookup secondAlias (S.builtAliases built)) of
          (Just first, Just second) -> do
            let prompt = Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.alice [first, second]
                askTwice = (,) <$> Game.ask prompt <*> Game.ask prompt
            case S.runScript script built askTwice of
              Left failure -> Spec.assertFailure s (show failure)
              Right (chosen, _) ->
                Spec.assertEqWith s "same-moment source order" chosen ([first], [second])
          _ -> Spec.assertFailure s "arrangement omitted an alias"

  Spec.it s "unoffered actions pass without consuming a later combat command" $ do
    let setup = S.arrangement (S.playerSetup S.alice NonEmpty.:| [S.playerSetup S.bob]) S.alice Phase.PrecombatMain
        moment = S.MkMoment (S.MkTurnNumber 1) Phase.PrecombatMain S.alice
        scheduled = S.MkScheduled moment Nothing (S.MkAttack Seq.empty)
    result <- S.arrange registry setup
    case result of
      Left failure -> Spec.assertFailure s (show failure)
      Right built -> do
        let priority = do
              State.modify' (\gs -> gs {GameState.priority = Just S.alice})
              Engine.priorityLoop
        case S.runScript (Seq.singleton scheduled) built priority of
          Left (S.MkUnreachedCommands _ _ commands) ->
            Spec.assertEqWith s "ChooseAction passed and left the combat command alone" commands (Seq.singleton scheduled)
          Left failure -> Spec.assertFailure s (show failure)
          Right _ -> Spec.assertFailure s "the unrelated combat command was consumed"

  Spec.it s "concession is opt-in at its scheduled moment" $ do
    let setup = S.arrangement (S.playerSetup S.alice NonEmpty.:| [S.playerSetup S.bob]) S.alice Phase.PrecombatMain
        concede = S.at 1 Phase.PrecombatMain S.alice S.MkConcede
    result <- S.arrange registry setup
    case result of
      Left failure -> Spec.assertFailure s (show failure)
      Right built ->
        case S.runScript (Seq.singleton concede) built (Game.ask (Prompt.Concede S.alice)) of
          Left failure -> Spec.assertFailure s (show failure)
          Right (answer, _) ->
            Spec.assertEqWith s "scheduled concession" answer Concession.Concedes

  Spec.it s "an unscheduled non-action prompt fails" $ do
    let piker = CardName.MkCardName (Text.pack "Goblin Piker")
        attacker =
          (S.objectSetup piker)
            { S.objectReadiness = S.MkReady,
              S.objectTapState = TapState.Untapped
            }
        alice = (S.playerSetup S.alice) {S.setupBattlefield = Seq.singleton attacker}
        setup =
          S.arrangement
            (alice NonEmpty.:| [S.playerSetup S.bob])
            S.alice
            (Phase.Combat CombatStep.BeginningOfCombat)
    result <- S.arrange registry setup
    case result of
      Left failure -> Spec.assertFailure s (show failure)
      Right built ->
        case S.runScript Seq.empty built S.combatGame of
          Left (S.MkUnscheduledPrompt _ kind _) ->
            Spec.assertEqWith s "the prompt kind" kind (Text.pack "DeclareAttackers")
          Left failure -> Spec.assertFailure s (show failure)
          Right _ -> Spec.assertFailure s "the attackers prompt was silently answered"

  Spec.it s "an arrangement past the beginning of combat can attack" $ do
    -- CR 506.2 / CR 703.4h: the defending player is settled during the beginning
    -- of combat step, so a board arranged AFTER it has to arrive with that done.
    -- Without it Combat.declareAttackers finds no defending player, skips its
    -- prompt, and the attack command reports itself as never reached.
    let attacker = S.aliasRef "attacker"
        setup =
          S.arrangement
            ( S.battlefield S.alice [S.ready (S.aliased "attacker" (S.permanent "Goblin Piker"))]
                NonEmpty.:| [S.battlefield S.bob []]
            )
            S.alice
            (Phase.Combat CombatStep.DeclareAttackers)
        script = Seq.singleton (S.at 1 (Phase.Combat CombatStep.DeclareAttackers) S.alice (S.attack [attacker]))
    built <- S.arrangeOrFail s registry setup
    (_, after) <- S.runScriptOrFail s script built S.combatGame
    Spec.assertEqWith s "bob took the attacker's two" (S.lifeOf S.bob after) (Just 18)

  Spec.it s "a reference the prompt did not offer is a failure, not a dropped command" $ do
    -- Both Pikers are alice's, so "Goblin Piker 1" resolves; only the untapped
    -- one is a legal attacker (CR 508.1a), so the prompt never offers the first.
    -- The engine would filter it out AFTER the command was popped, leaving a
    -- green script with nobody attacking.
    let tapped = (S.ready (S.permanent "Goblin Piker")) {S.objectTapState = TapState.Tapped}
        setup =
          S.arrangement
            ( S.battlefield S.alice [tapped, S.ready (S.permanent "Goblin Piker")]
                NonEmpty.:| [S.battlefield S.bob []]
            )
            S.alice
            (Phase.Combat CombatStep.DeclareAttackers)
        script =
          Seq.singleton
            ( S.at
                1
                (Phase.Combat CombatStep.DeclareAttackers)
                S.alice
                (S.attack [S.namedRef "Goblin Piker" 1])
            )
    built <- S.arrangeOrFail s registry setup
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
    let permanent name = S.ready (S.aliased name (S.permanent "Goblin Piker"))
        setup =
          S.arrangement
            ( S.battlefield S.alice [permanent "left", permanent "right"]
                NonEmpty.:| [ S.battlefield
                                S.bob
                                [ permanent "left first",
                                  permanent "left second",
                                  permanent "right first",
                                  permanent "right second"
                                ]
                            ]
            )
            S.alice
            (Phase.Combat CombatStep.BeginningOfCombat)
        left = S.aliasRef "left"
        right = S.aliasRef "right"
        script =
          Seq.fromList
            [ S.at 1 (Phase.Combat CombatStep.DeclareAttackers) S.alice (S.attack [left, right]),
              S.at
                1
                (Phase.Combat CombatStep.DeclareBlockers)
                S.bob
                ( S.block
                    [ (S.aliasRef "left first", left),
                      (S.aliasRef "left second", left),
                      (S.aliasRef "right first", right),
                      (S.aliasRef "right second", right)
                    ]
                ),
              S.atSource
                1
                (Phase.Combat CombatStep.CombatDamage)
                S.alice
                right
                ( S.assignDamage
                    [ (S.MkCreatureRecipient (S.aliasRef "right first"), 1),
                      (S.MkCreatureRecipient (S.aliasRef "right second"), 1)
                    ]
                ),
              S.atSource
                1
                (Phase.Combat CombatStep.CombatDamage)
                S.alice
                left
                ( S.assignDamage
                    [ (S.MkCreatureRecipient (S.aliasRef "left first"), 1),
                      (S.MkCreatureRecipient (S.aliasRef "left second"), 1)
                    ]
                )
            ]
    built <- S.arrangeOrFail s registry setup
    (_, fought) <- S.runScriptOrFail s script built S.combatGame
    -- Each Piker is a 2/1, so one point is lethal: every blocker dies.
    Spec.assertEqWith s "all four blockers were assigned lethal damage" (S.creaturesInPlay S.bob (S.settleSba fought)) 0

  Spec.it s "a qualifier on a prompt with no source is a script error" $ do
    let attacker = S.aliasRef "attacker"
        setup =
          S.arrangement
            ( S.battlefield S.alice [S.ready (S.aliased "attacker" (S.permanent "Goblin Piker"))]
                NonEmpty.:| [S.battlefield S.bob []]
            )
            S.alice
            (Phase.Combat CombatStep.DeclareAttackers)
        script =
          Seq.singleton
            ( S.atSource
                1
                (Phase.Combat CombatStep.DeclareAttackers)
                S.alice
                attacker
                (S.attack [attacker])
            )
    built <- S.arrangeOrFail s registry setup
    case S.runScript script built S.combatGame of
      Left (S.MkUnexpectedQualifier _ kind ref) -> do
        Spec.assertEqWith s "the prompt that has no source" kind (Text.pack "DeclareAttackers")
        Spec.assertEqWith s "the qualifier it carried" ref attacker
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the dangling qualifier was ignored"
