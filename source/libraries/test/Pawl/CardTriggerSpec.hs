{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over one printed trigger per card, from Curse of Vitality
-- to Betrayal: attacks, the monarch, keyword actions, becoming attached and
-- becoming tapped. Split out of Pawl.EventTriggerSpec, which keeps the
-- machinery.
module Pawl.CardTriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Goad as Goad
import qualified Pawl.Engine.Plot as Plot
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ControlChanged as ControlChanged
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- CR 508.3a / 603.3d: Anafenza, the Foremost's OTHER ability -- "whenever this
-- creature attacks, put a +1/+1 counter on another target tapped creature you
-- control". Here because the card was added for its CR 614.1a redirect
-- (Pawl.EventSpec's Anafenza group), and a card's second ability is not exercised
-- by the first one's tests.
--
-- The target filter is `And [Not IsSource, IsTapped, ControlledBy You]`, and the
-- board gives each conjunct exactly one thing to reject: Anafenza herself is
-- tapped and hers, so only "another" keeps her out; the Wall of Stone is hers and
-- not her, so only being untapped does (CR 702.3b keeps it home, so declaring
-- attackers never taps it); and bob's Piker is tapped and not her, so only its
-- controller does. The Piker attacking beside her satisfies all three -- CR
-- 508.1f taps a declared attacker -- and is the only legal target.
-- CR 508.3b: the first trigger in the pool whose arity is the DECLARATION's
-- rather than the attacking creature's.
--
-- Curse of Vitality {2}{W} Enchantment -- Aura Curse is the card: "enchant
-- player / Whenever enchanted player is attacked, you gain 2 life. Each opponent
-- attacking that player does the same." Rule 508.3b's "one or more creatures are
-- declared as attackers attacking that player" is the whole trigger, so TWO
-- attackers sent at one player is the case that separates it from CR 508.3a's
-- per-creature form (Marchesa's Decree, Pawl.KeywordTriggerSpec): the Curse pays
-- 2 life, never 4.
--
-- THREE SEATS, and load-bearing twice over. The enchanted player is bob, the
-- Curse is CAROL's, and alice attacks -- so "you" (carol), the attacked player
-- (bob) and the attacking player (alice) are three different seats, and the
-- second sentence's "each opponent attacking that player" has somebody to name
-- who is neither. At two seats every one of those collapses.
--
-- bob's Jace is what makes the OfPlayer test observable: CR 508.1b lists player
-- and planeswalker separately, so the same two attackers sent at a planeswalker
-- bob controls leave the Curse silent even though CR 508.5 still makes bob the
-- defending player -- which is the leg a condition reading the defending player
-- (CreatureAttacksYou's field) would get wrong.
curseOfVitalitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
curseOfVitalitySpec s registry =
  let board = do
        curse <- S.printingOf s registry "Curse of Vitality"
        piker <- S.printingOf s registry "Goblin Piker"
        jace <- S.printingOf s registry "Jace Beleren"
        case S.threePlayerCombat [piker, piker] [jace] [curse] of
          (gs0, ours, [walker], [aura]) ->
            -- Loyalty so CR 704.5i does not bury the Jace before CR 508.1b can
            -- offer it, and the Curse attached to BOB, which is what makes the
            -- ability's subject a seat its controller does not hold.
            pure (Just (ours, walker, S.attachTo aura (Recipient.ToPlayer S.bob) (S.addCounter CounterKind.Loyalty 3 walker gs0)))
          _ -> pure Nothing
      -- Attacks with everything, aims every attacker at `target`, and makes
      -- `who` CR 507.1's defending player. The target is FILTERED out of the
      -- offered set rather than built, so a leg whose announcement CR 508.1b
      -- never offered falls back visibly and the record assertions catch it.
      aimedAt :: AttackTarget.AttackTarget -> PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedAt target who p = case p of
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== target) (NonEmpty.toList options))
        _ -> S.attackTo who p
      -- The same three seats with the attacking player and the Curse's controller
      -- COLLAPSED onto carol, whose turn it now is. Everything else is the board
      -- above: two Pikers, the Curse on bob.
      ownTurnBoard = do
        curse <- S.printingOf s registry "Curse of Vitality"
        piker <- S.printingOf s registry "Goblin Piker"
        case S.threePlayerCombat [] [] [piker, piker, curse] of
          (gs0, [], [], [_, _, aura]) ->
            pure (Just (S.attachTo aura (Recipient.ToPlayer S.bob) gs0 {GameState.activePlayer = S.carol}, aura))
          _ -> pure Nothing
      -- The same board with the declaration itself declined: the leg that parts
      -- "the enchanted player was attacked" from "the step began".
      standingStill :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      standingStill who p = case p of
        Prompt.DeclareAttackers {} -> []
        _ -> aimedAt (AttackTarget.OfPlayer who) who p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
      sentAt gs = Map.elems (Combat.Type.attackers (GameState.combat gs))
   in Spec.describe s "Curse of Vitality" $ do
        -- The proving test. Both of alice's Pikers attack bob, and the Curse pays
        -- ONCE: carol 2 for "you gain 2 life", alice 2 for "each opponent
        -- attacking that player", bob nothing. A per-attacker arity would make
        -- those 24s.
        Spec.it s "CR 508.3b whole card: two creatures attacking the enchanted player pay the Curse once" $ do
          built <- board
          case built of
            Just (_, _, gs) -> do
              let after = atBlockers (aimedAt (AttackTarget.OfPlayer S.bob) S.bob) gs
              Spec.assertEqWith s "carol gained 2 once and alice, attacking bob, gained 2 with her" (lives after) (Just 22, Just 20, Just 22)
              Spec.assertEqWith s "CR 508.1b both Pikers really were declared attacking bob" (sentAt after) [AttackTarget.OfPlayer S.bob, AttackTarget.OfPlayer S.bob]
            Nothing -> Spec.assertFailure s "fixture should give alice two Pikers, bob a Jace and carol the Curse"
        -- The same two attackers sent at a planeswalker the enchanted player
        -- controls. CR 508.5 still makes bob the defending player, so this is the
        -- falsifier for a condition reading that field instead of CR 508.1b's
        -- announcement.
        Spec.it s "CR 508.3b a creature attacking the enchanted player's planeswalker does not attack the player" $ do
          built <- board
          case built of
            Just (_, walker, gs) -> do
              let after = atBlockers (aimedAt (AttackTarget.OfPlaneswalker walker) S.bob) gs
              Spec.assertEqWith s "nobody gained life" (lives after) (Just 20, Just 20, Just 20)
              Spec.assertEqWith s "CR 508.1b and the Jace really is what was attacked" (sentAt after) [AttackTarget.OfPlaneswalker walker, AttackTarget.OfPlaneswalker walker]
            Nothing -> Spec.assertFailure s "fixture should give alice two Pikers, bob a Jace and carol the Curse"
        -- The same declaration aimed at the OTHER opponent, whom the Curse does
        -- not enchant: the falsifier for a condition that fired on any
        -- declaration.
        Spec.it s "CR 508.3b a declaration attacking the other player leaves the Curse silent" $ do
          built <- board
          case built of
            Just (_, _, gs) -> do
              let after = atBlockers (aimedAt (AttackTarget.OfPlayer S.carol) S.carol) gs
              Spec.assertEqWith s "nobody gained life" (lives after) (Just 20, Just 20, Just 20)
              Spec.assertEqWith s "CR 508.1b and carol really was the one attacked" (sentAt after) [AttackTarget.OfPlayer S.carol, AttackTarget.OfPlayer S.carol]
            Nothing -> Spec.assertFailure s "fixture should give alice two Pikers, bob a Jace and carol the Curse"
        -- The Curse's own controller doing the attacking, which is the only board
        -- on which "each OPPONENT attacking that player" is observable: carol
        -- attacks bob with her own creatures, so the one player attacking the
        -- enchanted player is not an opponent of the Curse's controller and the
        -- second sentence names nobody. carol gains 2 for the first sentence and
        -- no more. Reachable because CR 508.1 lets only the active player declare,
        -- so this needs carol's turn rather than a second attacker.
        Spec.it s "CR 508.6 the Curse's own controller attacking pays only the first sentence" $ do
          built <- ownTurnBoard
          case built of
            Just (gs, _) -> do
              let after = atBlockers (aimedAt (AttackTarget.OfPlayer S.bob) S.bob) gs
              Spec.assertEqWith s "carol gained 2 and nobody gained for the second sentence" (lives after) (Just 20, Just 20, Just 22)
              Spec.assertEqWith s "CR 508.1b and both of carol's creatures really attacked bob" (sentAt after) [AttackTarget.OfPlayer S.bob, AttackTarget.OfPlayer S.bob]
            Nothing -> Spec.assertFailure s "fixture should give carol two Pikers and the Curse"
        -- No declaration at all, on the same board and against the same defending
        -- player: the falsifier for a condition that fired on the STEP.
        Spec.it s "CR 508.3b a declare attackers step with no attackers pays nothing" $ do
          built <- board
          case built of
            Just (_, _, gs) -> do
              let after = atBlockers (standingStill S.bob) gs
              Spec.assertEqWith s "nobody gained life" (lives after) (Just 20, Just 20, Just 20)
              Spec.assertEqWith s "and nothing was declared" (sentAt after) []
            Nothing -> Spec.assertFailure s "fixture should give alice two Pikers, bob a Jace and carol the Curse"

-- CR 508.3d: the third of rule 508.3's three arities, and the first trigger in
-- the pool whose subject is the ATTACKING PLAYER.
--
-- Boggart Prankster {1}{B} Creature -- Goblin Warrior 1/3 is the card: "Whenever
-- you attack, target attacking Goblin you control gets +1/+0 until end of turn."
-- Rule 508.3d triggers "if one or more creatures that player controls are
-- declared as attackers", so ONE declaration is one trigger however many
-- creatures it named and however many things they were sent at.
--
-- Two attacking Goblins is what parts it from CR 508.3a's per-attacker form
-- (SelfAttacks, CreatureAttacksYou): both are legal targets, so a per-attacker
-- reading resolves TWO pumps and the Prankster is a 3/3 rather than a 2/3. One
-- attacker would make the two arities agree, which is the trap.
--
-- The same declaration SPLIT across bob and a Jace bob controls is what parts it
-- from CR 508.3b's per-target form (AttachedPlayerIsAttacked): CR 508.1b lists
-- player and planeswalker separately, so that reading sees two
-- GameEvent.BecameAttacked events and pumps twice. Every single-defender board
-- lets the two agree, which is why this one exists.
--
-- The Prankster targets ITSELF on both, being an attacking Goblin alice
-- controls, so the assertion is one creature's power and cannot be reached by
-- pumping the other one -- and the Piker's own 2/1 is asserted beside it.
--
-- The Prankster HELD BACK is the third board, and it is what parts rule 508.3d
-- from a self-scoped reading: its controller attacked, so it triggers even
-- though it is not attacking, and the Piker is then the only legal target.
boggartPranksterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
boggartPranksterSpec s registry =
  let -- Declares exactly the creatures `plan` names, announces each at the target
      -- `plan` pairs with it, and points every target slot at `aim`. Both choices
      -- are FILTERED out of what the engine offered rather than built: an
      -- announcement CR 508.1b never offered falls back visibly to the head, and
      -- a hand-built Recipient of the right object in the wrong shape would be
      -- dropped at CR 608.2b with no error.
      answering :: [(ObjectId.ObjectId, AttackTarget.AttackTarget)] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      answering plan aim p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (\oid -> List.elem oid (fmap fst plan)) ids
        Prompt.ChooseAttackTarget _ _ oid options ->
          Maybe.fromMaybe (NonEmpty.head options) (List.find (\t -> List.lookup oid plan == Just t) (NonEmpty.toList options))
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature aim) . snd) sets
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      -- alice's Prankster and Piker, bob empty. Both Goblins, both untapped and
      -- Settled, so both are legal attackers and both are legal targets.
      plainBoard = do
        prankster <- S.printingOf s registry "Boggart Prankster"
        piker <- S.printingOf s registry "Goblin Piker"
        case S.combatBoardOf [prankster, piker] [] of
          (gs, [pranksterId, pikerId], []) -> pure (Just (pranksterId, pikerId, gs))
          _ -> pure Nothing
      -- The same two attackers with a planeswalker to split them across. The
      -- loyalty keeps CR 704.5i from burying the Jace before CR 508.1b can offer
      -- it, exactly as Pawl.EventTriggerSpec's Curse of Vitality board does.
      splitBoard = do
        prankster <- S.printingOf s registry "Boggart Prankster"
        piker <- S.printingOf s registry "Goblin Piker"
        jace <- S.printingOf s registry "Jace Beleren"
        case S.combatBoardOf [prankster, piker] [jace] of
          (gs, [pranksterId, pikerId], [walker]) ->
            pure (Just (pranksterId, pikerId, walker, S.addCounter CounterKind.Loyalty 3 walker gs))
          _ -> pure Nothing
   in Spec.describe s "Boggart Prankster" $ do
        -- The proving test. TWO Goblins are declared together and the Prankster
        -- is a 2/3: one declaration, one trigger, one +1/+0. A reading at CR
        -- 508.3a's arity makes it a 3/3.
        Spec.it s "CR 508.3d whole card: two creatures declared together trigger it once" $ do
          built <- plainBoard
          case built of
            Just (pranksterId, pikerId, gs) -> do
              let after = atBlockers (answering [(pranksterId, AttackTarget.OfPlayer S.bob), (pikerId, AttackTarget.OfPlayer S.bob)] pranksterId) gs
              Spec.assertEqWith s "the Prankster took ONE +1/+0" (S.powerToughnessOf pranksterId after) (Just (2, 3))
              Spec.assertEqWith s "and the Piker, which the trigger did not target, took none" (S.powerToughnessOf pikerId after) (Just (2, 1))
              Spec.assertEqWith s "CR 508.1b both Goblins really were declared attacking bob" (sentAt after) (Map.fromList [(pranksterId, AttackTarget.OfPlayer S.bob), (pikerId, AttackTarget.OfPlayer S.bob)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Prankster and a Piker"
        -- The same declaration aimed at TWO different things, which records two
        -- GameEvent.BecameAttacked events and one GameEvent.AttackersDeclared.
        -- Still a 2/3.
        Spec.it s "CR 508.3d one declaration split across a player and their planeswalker still triggers it once" $ do
          built <- splitBoard
          case built of
            Just (pranksterId, pikerId, walker, gs) -> do
              let after = atBlockers (answering [(pranksterId, AttackTarget.OfPlayer S.bob), (pikerId, AttackTarget.OfPlaneswalker walker)] pranksterId) gs
              Spec.assertEqWith s "the Prankster took ONE +1/+0" (S.powerToughnessOf pranksterId after) (Just (2, 3))
              Spec.assertEqWith s "and the Piker took none" (S.powerToughnessOf pikerId after) (Just (2, 1))
              Spec.assertEqWith s "CR 508.1b and the declaration really did name two different targets" (sentAt after) (Map.fromList [(pranksterId, AttackTarget.OfPlayer S.bob), (pikerId, AttackTarget.OfPlaneswalker walker)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Prankster and a Piker, and bob a Jace"
        -- The bearer held out of combat. Rule 508.3d asks about its CONTROLLER,
        -- so it triggers anyway, and "target attacking Goblin you control" then
        -- has exactly one candidate.
        Spec.it s "CR 508.3d the Prankster need not attack: its controller's declaration is the event" $ do
          built <- plainBoard
          case built of
            Just (pranksterId, pikerId, gs) -> do
              let after = atBlockers (answering [(pikerId, AttackTarget.OfPlayer S.bob)] pikerId) gs
              Spec.assertEqWith s "the Piker, the only attacking Goblin, took the +1/+0" (S.powerToughnessOf pikerId after) (Just (3, 1))
              Spec.assertEqWith s "and the Prankster, not attacking, is untouched" (S.powerToughnessOf pranksterId after) (Just (1, 3))
              Spec.assertEqWith s "CR 508.1b and only the Piker was declared" (sentAt after) (Map.fromList [(pikerId, AttackTarget.OfPlayer S.bob)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Prankster and a Piker"
        -- No declaration at all, on the same board: rule 508.3d's "one or more",
        -- and the falsifier for an event recorded per STEP rather than per
        -- declaration. A REGRESSION FENCE here rather than a proof -- the two
        -- readings agree, because a trigger that did fire would find no
        -- attacking Goblin to target and CR 603.3d would remove it. Avatar Roku,
        -- Firebender's group below proves it instead, its trigger targeting
        -- nothing.
        Spec.it s "CR 508.3d a declare attackers step with no attackers is not attacking" $ do
          built <- plainBoard
          case built of
            Just (pranksterId, pikerId, gs) -> do
              let after = atBlockers (answering [] pranksterId) gs
              Spec.assertEqWith s "the Prankster is its printed 1/3" (S.powerToughnessOf pranksterId after) (Just (1, 3))
              Spec.assertEqWith s "and the Piker its printed 2/1" (S.powerToughnessOf pikerId after) (Just (2, 1))
              Spec.assertEqWith s "and nothing was declared" (sentAt after) Map.empty
            Nothing -> Spec.assertFailure s "fixture should give alice a Prankster and a Piker"

-- CR 508.3d's OTHER subject. The rule says "[a player]", and the Prankster above
-- prints the CR 109.5 "you" reading of it; this is the "a player" reading, which
-- is PlayerRelation.AnyPlayer. The pair is what pins the payload.
--
-- Avatar Roku, Firebender {3}{R}{R}{R} Legendary Creature -- Human Avatar 6/6:
-- "Whenever a player attacks, add six {R}. Until end of combat, you don't lose
-- this mana as steps end. {R}{R}{R}: Target creature gets +3/+0 until end of
-- turn."
--
-- Nothing is omitted from the card. The retention sentence is CR 500.5a's
-- ManaRetention.UntilEndOfCombat, and Pawl.ManaSpec's group of the same name is
-- what proves it; this group is about the trigger's payload alone.
--
-- The assertion here is Roku's POWER and not its pool, which the retention does
-- not change: alice holds no lands, so the {R}{R}{R} activation is affordable
-- only through the trigger, and six {R} pays for exactly two activations. The
-- answerer takes every activation offered and the MANA bounds it, so a trigger
-- adding the wrong amount shows as the wrong power -- and the pool is empty at
-- the moment this group reads it under every reading of the trigger, retained or
-- not.
--
-- One fixture, and the two discriminating boards differ in exactly one thing:
-- who is active.
--
--   * bob active and declaring. Only AnyPlayer fires alice's Roku, so hardcoding
--     You reads 6/6 here.
--   * alice active and declaring. AnyPlayer and You agree, so this is the board
--     that falsifies hardcoding Opponent.
--
-- Roku's trigger TARGETS NOTHING, which is what lets the first board see a
-- difference at all: CR 603.3d removes a trigger with no legal target, which is
-- how Boggart Prankster's "target attacking Goblin you control" hides the same
-- distinction on every board.
--
-- The Opponent arm is borne by Ever-Watching Threshold, whose group is below,
-- and no board tells it from AnyPlayer: CR 506.2 and CR 508.1 let only the
-- active player declare and no player attacks themselves, so every card printing
-- either phrase sees the same declarations. What the boards below prove is that
-- this implementation does not behave as Opponent.
avatarRokuSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
avatarRokuSpec s registry =
  let isActivate a = case a of
        A.Activate _ _ -> True
        _ -> False
      -- Attacks with everything, aims every announcement at `defending`, takes
      -- every activation the engine offers, and points every target slot at
      -- Roku. Both choices are FILTERED out of what was offered rather than
      -- built: a hand-built Recipient of the right object in the wrong shape
      -- would be dropped at CR 608.2b with no error.
      answering :: PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      answering defending aim p = case p of
        Prompt.ChooseAttackTarget _ _ _ options ->
          Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer defending) (NonEmpty.toList options))
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature aim) . snd) sets
        Prompt.ChooseAction _ _ options -> case filter isActivate options of
          a : _ -> a
          [] -> A.Pass
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      -- alice's Roku, bob's Piker, and NO lands on either side.
      fixture = do
        roku <- S.printingOf s registry "Avatar Roku, Firebender"
        piker <- S.printingOf s registry "Goblin Piker"
        case S.combatBoardOf [roku] [piker] of
          (gs, [rokuId], [pikerId]) -> pure (Just (rokuId, pikerId, gs))
          _ -> pure Nothing
      -- combatBoardOf hardcodes alice as the active player and CR 506.2's second
      -- sentence then makes bob the defender. Both are turned around here, which
      -- is the whole difference between the two boards.
      bobsTurn gs =
        gs
          { GameState.activePlayer = S.bob,
            GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.alice]}
          }
   in Spec.describe s "Avatar Roku, Firebender" $ do
        -- The proving test: a player who is NOT the ability's controller
        -- declares, and rule 508.3d's "a player" covers them.
        Spec.it s "CR 508.3d \"whenever a player attacks\" fires on an opponent's declaration" $ do
          built <- fixture
          case built of
            Just (rokuId, pikerId, gs) -> do
              let after = atBlockers (answering S.alice rokuId) (bobsTurn gs)
              Spec.assertEqWith s "Roku spent the six {R} bob's declaration added, twice" (S.powerToughnessOf rokuId after) (Just (12, 6))
              Spec.assertEqWith s "CR 508.1 and it was bob's Piker that was declared, at alice" (sentAt after) (Map.fromList [(pikerId, AttackTarget.OfPlayer S.alice)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Roku and bob a Piker"
        -- The same fixture with alice active. AnyPlayer includes CR 109.5's
        -- "you", so the trigger fires here too -- and a payload misread as
        -- Opponent would not.
        Spec.it s "CR 508.3d \"a player\" includes the ability's own controller" $ do
          built <- fixture
          case built of
            Just (rokuId, _, gs) -> do
              let after = atBlockers (answering S.bob rokuId) gs
              Spec.assertEqWith s "Roku spent the six {R} its own controller's declaration added, twice" (S.powerToughnessOf rokuId after) (Just (12, 6))
              Spec.assertEqWith s "CR 508.1 and it was alice's Roku that was declared, at bob" (sentAt after) (Map.fromList [(rokuId, AttackTarget.OfPlayer S.bob)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Roku and bob a Piker"
        -- No declaration at all, on the first board: rule 508.3d's "one or more".
        -- Not a fence here, unlike the Prankster's version -- Roku's trigger
        -- targets nothing, so a spurious firing would buy two activations and
        -- show as 12/6.
        Spec.it s "CR 508.3d a declare attackers step with no attackers adds nothing" $ do
          built <- fixture
          case built of
            Just (rokuId, _, gs) -> do
              let after = atBlockers (\p -> case p of Prompt.DeclareAttackers {} -> []; _ -> answering S.alice rokuId p) (bobsTurn gs)
              Spec.assertEqWith s "Roku is its printed 6/6" (S.powerToughnessOf rokuId after) (Just (6, 6))
              Spec.assertEqWith s "and nothing was declared" (sentAt after) Map.empty
            Nothing -> Spec.assertFailure s "fixture should give alice a Roku and bob a Piker"

-- CR 508.3c: rule 508.3d's arity narrowed by a quality of what was declared, and
-- the first trigger in the pool whose subject is the attacking PLAYER and whose
-- payload also names a kind of creature.
--
-- Hermes, Overseer of Elpis {3}{U} Legendary Creature -- Elder Wizard 2/4 is the
-- card: "Whenever you attack with one or more Birds, scry 2." The printed
-- quantifier is "one or more", so ONE declaration is ONE trigger however many
-- Birds it named.
--
-- Hermes is an Elder Wizard and NOT a Bird, so on every board here the bearer is
-- a bystander and the Filter is doing the work. Its other ability watches casts
-- and nothing in this group casts anything.
--
-- Two attacking Birds is what parts this from CR 508.3a's per-attacker form: a
-- reading against GameEvent.AttackerDeclared scries TWICE. ONE Bird would make
-- the two arities agree, which is the trap, so the two-Bird case is the
-- load-bearing one.
--
-- Hermes attacking ALONE is what parts it from the unfiltered CR 508.3d form
-- (TriggerCondition.PlayerAttacks): a reading that dropped the Filter scries
-- once where this one scries not at all.
--
-- The scry is read as LIBRARY ORDER and not as a prompt count. CR 701.22a moves
-- no card out of the library, so the library's SIZE cannot tell two scries from
-- one; its order can, the answerer bottoming everything it is shown, which sends
-- two more cards under per resolution.
hermesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hermesSpec s registry =
  let -- Declares exactly the creatures `plan` names and announces each at the
      -- target `plan` pairs with it, FILTERED out of what the engine offered
      -- rather than built, for boggartPranksterSpec's reason above.
      answering :: [(ObjectId.ObjectId, AttackTarget.AttackTarget)] -> Prompt.Prompt r -> r
      answering plan p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (\oid -> List.elem oid (fmap fst plan)) ids
        Prompt.ChooseAttackTarget _ _ oid options ->
          Maybe.fromMaybe (NonEmpty.head options) (List.find (\t -> List.lookup oid plan == Just t) (NonEmpty.toList options))
        -- Everything looked at goes UNDER, in the order it was looked at. Pinned
        -- structurally rather than searched for: this answer cannot find its way
        -- back to the right library after a mutation changed how many times the
        -- ability resolved.
        Prompt.ChooseScry _ _ looked -> (looked, [])
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      libraryOf = Game.zoneMembers Zone.Library S.alice
      -- alice holds Hermes and two Birds, all Settled and untapped; bob holds
      -- nothing, so no block intervenes. Six DISTINCT cards under alice's
      -- library, deep enough that two scry 2s never wrap.
      fixture = do
        hermes <- S.printingOf s registry "Hermes, Overseer of Elpis"
        maiden <- S.printingOf s registry "Bird Maiden"
        raven <- S.printingOf s registry "Augury Raven"
        piker <- S.printingOf s registry "Goblin Piker"
        mountain <- S.printingOf s registry "Mountain"
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        plains <- S.printingOf s registry "Plains"
        swamp <- S.printingOf s registry "Swamp"
        case S.combatBoardOf [hermes, maiden, raven] [maiden] of
          (gs, [hermesId, maidenId, ravenId], [theirBirdId]) ->
            -- addLibraryCard puts its card ON TOP, so the deepest is stocked
            -- first and `ids` comes out top-first.
            let deal (acc, g) printing = let (oid, g1) = S.addLibraryCard printing S.alice g in (oid : acc, g1)
                (ids, stocked) = List.foldl' deal ([], gs) [swamp, plains, island, forest, mountain, piker]
             in pure (Just (hermesId, maidenId, ravenId, theirBirdId, ids, stocked))
          _ -> pure Nothing
      -- combatBoardOf hardcodes alice as the active player and CR 506.2's second
      -- sentence then makes bob the defender. Both are turned around here, which
      -- is the whole difference between the last board and the others.
      bobsTurn gs =
        gs
          { GameState.activePlayer = S.bob,
            GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.alice]}
          }
   in Spec.describe s "Hermes, Overseer of Elpis" $ do
        -- The proving case. TWO Birds are declared together and exactly TWO cards
        -- go under: one declaration, one trigger, one scry 2. A reading at CR
        -- 508.3a's arity bottoms four.
        Spec.it s "CR 508.3c whole card: two Birds declared together scry ONCE" $ do
          built <- fixture
          case built of
            Just (_, maidenId, ravenId, _, ids, gs) -> case ids of
              [piker, mountain, forest, island, plains, swamp] -> do
                let after = atBlockers (answering [(maidenId, AttackTarget.OfPlayer S.bob), (ravenId, AttackTarget.OfPlayer S.bob)]) gs
                Spec.assertEqWith s "the library started top-first piker, mountain, forest, island, plains, swamp" (libraryOf gs) ids
                Spec.assertEqWith s "ONE scry 2: piker and mountain went under, and nothing else moved" (libraryOf after) [forest, island, plains, swamp, piker, mountain]
                Spec.assertEqWith s "CR 508.1b both Birds really were declared attacking bob" (sentAt after) (Map.fromList [(maidenId, AttackTarget.OfPlayer S.bob), (ravenId, AttackTarget.OfPlayer S.bob)])
              _ -> Spec.assertFailure s "expected six library cards"
            Nothing -> Spec.assertFailure s "fixture should give alice a Hermes and two Birds"
        -- ONE Bird, declared alongside the non-Bird bearer. Still one scry 2 --
        -- the control that rules out a reading counting the declaration's
        -- creatures rather than the Birds among them, which would bottom four
        -- here.
        Spec.it s "CR 508.3c one Bird beside the non-Bird bearer still scries once" $ do
          built <- fixture
          case built of
            Just (hermesId, maidenId, _, _, ids, gs) -> case ids of
              [piker, mountain, forest, island, plains, swamp] -> do
                let after = atBlockers (answering [(hermesId, AttackTarget.OfPlayer S.bob), (maidenId, AttackTarget.OfPlayer S.bob)]) gs
                Spec.assertEqWith s "ONE scry 2 again: piker and mountain went under" (libraryOf after) [forest, island, plains, swamp, piker, mountain]
                Spec.assertEqWith s "CR 508.1b Hermes and the Bird really were declared" (sentAt after) (Map.fromList [(hermesId, AttackTarget.OfPlayer S.bob), (maidenId, AttackTarget.OfPlayer S.bob)])
              _ -> Spec.assertFailure s "expected six library cards"
            Nothing -> Spec.assertFailure s "fixture should give alice a Hermes and two Birds"
        -- Hermes attacking ALONE. Rule 508.3c asks for a creature the Filter
        -- admits, and an Elder Wizard is not one, so the ability does not
        -- trigger and the library is untouched. The falsifier for a reading that
        -- ignored the Filter.
        Spec.it s "CR 508.3c a declaration naming no Bird does not trigger it" $ do
          built <- fixture
          case built of
            Just (hermesId, _, _, _, ids, gs) -> case ids of
              [_, _, _, _, _, _] -> do
                let after = atBlockers (answering [(hermesId, AttackTarget.OfPlayer S.bob)]) gs
                Spec.assertEqWith s "no scry: the library is exactly as it was stocked" (libraryOf after) ids
                Spec.assertEqWith s "CR 508.1b and Hermes really was declared, alone" (sentAt after) (Map.fromList [(hermesId, AttackTarget.OfPlayer S.bob)])
              _ -> Spec.assertFailure s "expected six library cards"
            Nothing -> Spec.assertFailure s "fixture should give alice a Hermes and two Birds"
        -- CR 109.5's "you": BOB declares, with a Bird bob controls, and alice's
        -- Hermes stays silent. The falsifier for a reading that dropped the
        -- PlayerRelation -- every board above has alice declaring, so on those
        -- three the relation is invisible.
        Spec.it s "CR 508.3c an opponent attacking with a Bird does not trigger it" $ do
          built <- fixture
          case built of
            Just (_, _, _, theirBirdId, ids, gs) -> do
              let after = atBlockers (answering [(theirBirdId, AttackTarget.OfPlayer S.alice)]) (bobsTurn gs)
              Spec.assertEqWith s "no scry: alice's library is exactly as it was stocked" (libraryOf after) ids
              Spec.assertEqWith s "CR 508.1b and bob's Bird really was declared attacking alice" (sentAt after) (Map.fromList [(theirBirdId, AttackTarget.OfPlayer S.alice)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Hermes and two Birds"

-- CR 508.3c at a floor ABOVE one, which is the whole difference between this
-- group and hermesSpec above: the same condition counts the creatures the Filter
-- admits instead of asking whether there is any.
--
-- Military Intelligence {1}{U} Enchantment is the card: "Whenever you attack
-- with two or more creatures, draw a card." One ability, one effect, and no
-- quality narrowing at all -- the Filter is HasCardType Creature, which every
-- declared attacker satisfies (CR 508.1a), so on every board here the COUNT is
-- doing the work and the Filter is doing none.
--
-- The bearer is an enchantment and never attacks, so it is a bystander
-- throughout, hermesSpec's posture.
--
-- Three attackers is what parts "at least two" from "exactly two", and one
-- attacker is what parts it from Hermes' "one or more". Both are needed: a floor
-- read as equality passes the two-attacker board, and a dropped floor passes
-- both the two- and the three-attacker boards.
--
-- The draw is read as the HAND'S CONTENTS rather than as a hand size, because
-- the library is stocked with distinctly named cards and a second resolution
-- would move a second, nameable one. By name and not by object id: CR 400.7
-- makes a drawn card a new object, so the id the library held is not the id the
-- hand holds and only the name carries across the move.
militaryIntelligenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
militaryIntelligenceSpec s registry =
  let -- Declares exactly the creatures `plan` names, each attacking bob,
      -- FILTERED out of what the engine offered rather than built, hermesSpec's
      -- reason.
      answering :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      answering plan p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (\oid -> List.elem oid plan) ids
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      libraryOf = Game.zoneMembers Zone.Library S.alice
      -- alice holds the enchantment and three Settled untapped creatures; bob
      -- holds two of his own, so the last board can declare two without alice's.
      -- Four DISTINCT cards under alice's library, deeper than any board here
      -- draws, so CR 104.3c never fires.
      fixture = do
        intelligence <- S.printingOf s registry "Military Intelligence"
        piker <- S.printingOf s registry "Goblin Piker"
        maiden <- S.printingOf s registry "Bird Maiden"
        raven <- S.printingOf s registry "Augury Raven"
        mountain <- S.printingOf s registry "Mountain"
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        plains <- S.printingOf s registry "Plains"
        case S.combatBoardOf [intelligence, piker, maiden, raven] [piker, maiden] of
          (gs, [_, pikerId, maidenId, ravenId], [theirPikerId, theirMaidenId]) ->
            -- addLibraryCard puts its card ON TOP, so the deepest is stocked
            -- first and `ids` comes out top-first.
            let deal (acc, g) printing = let (oid, g1) = S.addLibraryCard printing S.alice g in (oid : acc, g1)
                (ids, stocked) = List.foldl' deal ([], gs) [plains, island, forest, mountain]
             in pure (Just (pikerId, maidenId, ravenId, theirPikerId, theirMaidenId, ids, stocked))
          _ -> pure Nothing
      -- combatBoardOf hardcodes alice as the active player and CR 506.2's second
      -- sentence then makes bob the defender. Both are turned around here, which
      -- is the whole difference between the last board and the others.
      bobsTurn gs =
        gs
          { GameState.activePlayer = S.bob,
            GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.alice]}
          }
   in Spec.describe s "Military Intelligence" $ do
        -- The proving case: exactly two declared, exactly one card drawn.
        Spec.it s "CR 508.3c whole card: two attackers declared together draw ONE card" $ do
          built <- fixture
          case built of
            Just (pikerId, maidenId, _, _, _, ids, gs) -> case ids of
              [mountain, forest, island, plains] -> do
                let after = atBlockers (answering [pikerId, maidenId]) gs
                Spec.assertEqWith s "alice's hand started empty" (handNames S.alice gs) []
                Spec.assertEqWith s "the library started top-first Mountain, Forest, Island, Plains" (libraryOf gs) [mountain, forest, island, plains]
                Spec.assertEqWith s "ONE draw: the Mountain off the top and nothing else is in hand" (handNames S.alice after) ["Mountain"]
                Spec.assertEqWith s "and the library kept the other three, in order" (libraryOf after) [forest, island, plains]
                Spec.assertEqWith s "CR 508.1b both creatures really were declared attacking bob" (sentAt after) (Map.fromList [(pikerId, AttackTarget.OfPlayer S.bob), (maidenId, AttackTarget.OfPlayer S.bob)])
              _ -> Spec.assertFailure s "expected four library cards"
            Nothing -> Spec.assertFailure s "fixture should give alice a Military Intelligence and three creatures"
        -- ONE attacker. The floor is two, so nothing triggers -- the falsifier
        -- for a reading that kept Hermes' "one or more".
        Spec.it s "CR 508.3c one attacker is below the floor and draws nothing" $ do
          built <- fixture
          case built of
            Just (pikerId, _, _, _, _, ids, gs) -> do
              let after = atBlockers (answering [pikerId]) gs
              Spec.assertEqWith s "no draw: alice's hand is still empty" (handNames S.alice after) []
              Spec.assertEqWith s "and the library is exactly as it was stocked" (libraryOf after) ids
              Spec.assertEqWith s "CR 508.1b and the one creature really was declared" (sentAt after) (Map.fromList [(pikerId, AttackTarget.OfPlayer S.bob)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Military Intelligence and three creatures"
        -- THREE attackers. Still one card: "two or more" is a floor and the
        -- ability fires once per DECLARATION. The falsifier both for a floor read
        -- as equality, which draws nothing here, and for a per-attacker arity,
        -- which draws three.
        Spec.it s "CR 508.3c three attackers still draw exactly one card" $ do
          built <- fixture
          case built of
            Just (pikerId, maidenId, ravenId, _, _, ids, gs) -> case ids of
              [_, forest, island, plains] -> do
                let after = atBlockers (answering [pikerId, maidenId, ravenId]) gs
                Spec.assertEqWith s "ONE draw again: only the Mountain off the top is in hand" (handNames S.alice after) ["Mountain"]
                Spec.assertEqWith s "and the library kept the other three, in order" (libraryOf after) [forest, island, plains]
                Spec.assertEqWith s "CR 508.1b all three really were declared attacking bob" (sentAt after) (Map.fromList [(pikerId, AttackTarget.OfPlayer S.bob), (maidenId, AttackTarget.OfPlayer S.bob), (ravenId, AttackTarget.OfPlayer S.bob)])
              _ -> Spec.assertFailure s "expected four library cards"
            Nothing -> Spec.assertFailure s "fixture should give alice a Military Intelligence and three creatures"
        -- CR 109.5's "you": BOB declares two, and alice's enchantment stays
        -- silent. The falsifier for a reading that dropped the PlayerRelation --
        -- every board above has alice declaring, so on those three it is
        -- invisible.
        Spec.it s "CR 508.3c an opponent attacking with two creatures does not trigger it" $ do
          built <- fixture
          case built of
            Just (_, _, _, theirPikerId, theirMaidenId, ids, gs) -> do
              let after = atBlockers (answering [theirPikerId, theirMaidenId]) (bobsTurn gs)
              Spec.assertEqWith s "no draw: alice's hand is still empty" (handNames S.alice after) []
              Spec.assertEqWith s "and alice's library is exactly as it was stocked" (libraryOf after) ids
              Spec.assertEqWith s "CR 508.1b and bob's two creatures really were declared attacking alice" (sentAt after) (Map.fromList [(theirPikerId, AttackTarget.OfPlayer S.alice), (theirMaidenId, AttackTarget.OfPlayer S.alice)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Military Intelligence and three creatures"

anafenzaAttackSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
anafenzaAttackSpec s registry =
  let countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- Records every CR 601.2c legal-recipient set offered, verbatim, and
      -- answers everything aggressively -- which declares every legal attacker,
      -- so the declaration really happens.
      --
      -- The LEGAL SET is what this asserts on rather than only the outcome, and
      -- that is the difference between a discriminating test and a passing one:
      -- with the Piker the lowest-id candidate, an answerer that takes the first
      -- offer reaches the same board whether or not the filter rejected anything.
      recordTargets :: Prompt.Prompt r -> State.State [Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient)] r
      recordTargets p = case p of
        Prompt.ChooseTargets _ _ _ sets -> do
          State.modify' (<> [sets])
          pure (S.aggressiveAnswer p)
        _ -> pure (S.aggressiveAnswer p)
   in Spec.describe s "Anafenza attacks" . Spec.it s "CR 508.3a the attack trigger counters another tapped creature its controller controls" $ do
        anafenza <- S.printingOf s registry "Anafenza, the Foremost"
        piker <- S.printingOf s registry "Goblin Piker"
        wallOfStone <- S.printingOf s registry "Wall of Stone"
        case S.combatBoardOf [anafenza, piker, wallOfStone] [piker] of
          (gs0, [anafenzaId, pikerId, wallId], [theirs]) -> do
            -- bob's Piker is TAPPED, so `ControlledBy You` is the only conjunct
            -- keeping it out of the offer. Left untapped it would be rejected by
            -- IsTapped instead, and the assertion would hold with the
            -- controller clause deleted.
            let gs = S.tapObject theirs gs0
                ((_, settled), offered) =
                  State.runState (Engine.runGame recordTargets gs (Engine.runStep >> Engine.priorityLoop)) []
            Spec.assertEqWith
              s
              "the Piker attacking beside her is the only legal target"
              (fmap (fmap snd . Map.elems) offered)
              [[Set.singleton (Recipient.ToCreature pikerId)]]
            Spec.assertEqWith s "and it took the counter" (countersOn pikerId settled) (Just 1)
            Spec.assertEqWith s "\"another\" keeps Anafenza off her own trigger" (countersOn anafenzaId settled) (Just 0)
            Spec.assertEqWith s "an untapped creature is not a legal target" (countersOn wallId settled) (Just 0)
            Spec.assertEqWith s "and neither is a creature bob controls" (countersOn theirs settled) (Just 0)
          _ -> Spec.assertFailure s "fixture should give alice Anafenza, a Piker and a Wall, and bob a Piker"

-- CR 603.4: an attack trigger with an intervening "if" that reads WHAT the
-- declaration was aimed at.
--
-- Ever-Watching Threshold {2}{U} Enchantment is the card, and nothing is omitted
-- from it: "Whenever an opponent attacks, if they attacked you and/or a
-- planeswalker you control, draw a card."
--
-- The trigger is CR 508.3d's PlayerAttacks, once per declaration; the "if" is a
-- Pawl.Types.Condition whose two disjuncts read Filter.DeclaredAttackedThisCombat
-- off the two subjects the printed sentence names -- alice herself, through
-- Scope.OverPlayers, and a planeswalker she controls, through the battlefield.
--
-- "THEY" is not read, and cannot be: CR 506.2 makes the attacking player the
-- active player, so every creature in a combat phase's declaration is that one
-- player's, and CR 506.4 removes a creature from combat the moment its
-- controller changes (Pawl.Engine.Combat's controlChanged). So "they attacked
-- you" and "a creature attacked you" are the same question, by rule rather than
-- by the pool's current shape.
--
-- Three boards, all three-seat, all with bob active and declaring:
--
--   * bob attacks CAROL. Rule 508.3d's own condition holds -- alice's opponent
--     declared attackers -- and the clause is the only thing that can stop it.
--   * bob attacks ALICE. The first disjunct.
--   * bob attacks alice's JACE. The second disjunct, and the leg that parts this
--     card's clause from CR 508.5's defending player -- which is alice on the
--     Jace board and on the alice board alike, so an implementation reading that
--     field could not tell the two disjuncts apart.
--
-- PlayerRelation.Opponent is BORNE by this card and still not discriminated from
-- AnyPlayer: CR 506.2/508.1 let only the active player declare, and no player
-- attacks themselves, so alice declaring leaves the "if" false whichever relation
-- is read. What the boards do falsify is You -- bob declares on all three, and a
-- You reading fires nothing at all.
everWatchingThresholdSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
everWatchingThresholdSpec s registry =
  let -- Declares `attacker` alone and announces it at `target`, FILTERED out of
      -- what the engine offered rather than built, for seiferSpec's reason.
      answering :: ObjectId.ObjectId -> AttackTarget.AttackTarget -> Prompt.Prompt r -> r
      answering attacker target p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== target) (NonEmpty.toList options))
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      -- How many of rule 508.3d's triggers CR 603.2 wrote down. Rule 603.4's
      -- first sentence is why this is 0 on the silent board rather than 1: the
      -- clause is checked at the GATHER, so an ability it rejects "does nothing"
      -- and never becomes a trigger to record. Asserted beside the hand because
      -- the hand alone cannot tell a clause that held from a draw that failed.
      fired gs = length [() | GameEvent.AbilityTriggered record <- S.eventsOf gs, isPlayerAttacks (TriggeredAbility.condition (AbilityTriggered.ability record))]
      -- bob active and declaring, with `defending` settled as CR 506.2a's one
      -- defending player. combatBoardOf's tail of steps, so S.runToStep can walk
      -- from the declare attackers step to the next one.
      bobAttacking defending gs =
        gs
          { GameState.activePlayer = S.bob,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [defending]},
            GameState.remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain,
                  Phase.Ending EndingStep.EndStep,
                  Phase.Ending EndingStep.Cleanup
                ]
          }
      -- alice holds the Threshold and a Jace stocked with loyalty (CR 704.5i
      -- would otherwise take a loyalty-0 planeswalker away before attackers are
      -- declared); bob holds one Piker; carol holds nothing, so bob's only
      -- creature is the whole declaration whichever seat he is pointed at. Three
      -- cards under alice's library, so CR 104.3c never decks her.
      fixture = do
        threshold <- S.printingOf s registry "Ever-Watching Threshold"
        piker <- S.printingOf s registry "Goblin Piker"
        jace <- S.printingOf s registry "Jace Beleren"
        island <- S.printingOf s registry "Island"
        case S.threePlayerCombat [threshold, jace] [piker] [] of
          (gs0, [_, jaceId], [pikerId], []) ->
            let stock g = snd (S.addLibraryCard island S.alice g)
             in pure (Just (jaceId, pikerId, stock (stock (stock (S.addCounter CounterKind.Loyalty 3 jaceId gs0)))))
          _ -> pure Nothing
   in Spec.describe s "Ever-Watching Threshold" $ do
        -- The proving test: the declaration reached alice, so the clause is true
        -- and the ability draws.
        Spec.it s "CR 603.4 the clause holds when the declaring opponent attacked you" $ do
          built <- fixture
          case built of
            Just (_, pikerId, gs) -> do
              let after = atBlockers (answering pikerId (AttackTarget.OfPlayer S.alice)) (bobAttacking S.alice gs)
              Spec.assertEqWith s "CR 121.1 alice drew the card" (S.handSize S.alice after) 1
              Spec.assertEqWith s "CR 508.3d and one trigger fired for the one declaration" (fired after) 1
              Spec.assertEqWith s "CR 508.1 and it was bob's Piker, declared at alice" (sentAt after) (Map.fromList [(pikerId, AttackTarget.OfPlayer S.alice)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Threshold and a Jace, and bob a Piker"
        -- The second disjunct. CR 508.5 makes alice the defending player here
        -- too, so only a clause reading the ANNOUNCED target can tell this board
        -- from the one above -- and only one reading the planeswalker rather than
        -- the player can tell it from the one below.
        Spec.it s "CR 603.4 the clause holds when a planeswalker you control was attacked" $ do
          built <- fixture
          case built of
            Just (jaceId, pikerId, gs) -> do
              let after = atBlockers (answering pikerId (AttackTarget.OfPlaneswalker jaceId)) (bobAttacking S.alice gs)
              Spec.assertEqWith s "CR 121.1 alice drew the card" (S.handSize S.alice after) 1
              Spec.assertEqWith s "CR 508.3d and one trigger fired for the one declaration" (fired after) 1
              Spec.assertEqWith s "CR 508.1b and the Piker really was declared at Jace" (sentAt after) (Map.fromList [(pikerId, AttackTarget.OfPlaneswalker jaceId)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Threshold and a Jace, and bob a Piker"
        -- The negative, and the same fixture: bob attacks the third seat, so
        -- neither disjunct holds. Rule 603.4's first sentence -- the ability
        -- "triggers only if" the clause is true -- is what makes `fired` 0 here
        -- and 1 on both boards above, and rule 508.3d's condition is satisfied on
        -- all three alike.
        Spec.it s "CR 603.4 the clause fails when the declaration went at a third player" $ do
          built <- fixture
          case built of
            Just (_, pikerId, gs) -> do
              let after = atBlockers (answering pikerId (AttackTarget.OfPlayer S.carol)) (bobAttacking S.carol gs)
              Spec.assertEqWith s "CR 121.1 alice drew nothing" (S.handSize S.alice after) 0
              Spec.assertEqWith s "CR 603.4 and the ability did not trigger at all" (fired after) 0
              Spec.assertEqWith s "CR 508.1 and the Piker was declared at carol" (sentAt after) (Map.fromList [(pikerId, AttackTarget.OfPlayer S.carol)])
            Nothing -> Spec.assertFailure s "fixture should give alice a Threshold and a Jace, and bob a Piker"

-- Rule 508.3d's condition, read off the log entry CR 603.2 writes for a trigger.
isPlayerAttacks :: TriggerCondition.TriggerCondition -> Bool
isPlayerAttacks condition = case condition of
  TriggerCondition.PlayerAttacks _ -> True
  _ -> False

-- CR 508.3e: "whenever [a player] attacks [another player]" -- the last of rule
-- 508.3's arities, and the only one whose subject is a PAIR of players.
--
-- Seifer, Balamb Rival {2}{B}{R} Legendary Creature -- Human Mercenary 4/3 is
-- the card, and this group is its SECOND line: "whenever you attack a player,
-- goad target creature that player controls". Its third line is CR 509.3e's and
-- lives in Pawl.KeywordTriggerSpec; its first is first strike.
--
-- The payload is what makes the condition's second subject observable at all.
-- "That player" is the ATTACKED player, bound under
-- Pawl.Engine.Binding.triggerPlayer, and the target slot's
-- Filter.ControlledByBound reads it -- so an arm that bound the attacking player
-- instead offers alice's own creatures, and one that bound nothing offers
-- nobody and CR 603.3d removes the trigger. Both are visible below, because the
-- goaded creature is asserted by IDENTITY: bob controls two Giants, the
-- answerer pins the second, and alice's attacking Elves is asserted untouched
-- beside them.
--
-- The Jace board proves rule 508.3e's last sentence, "it won't trigger if a
-- creature attacks a planeswalker or a battle", and it is the firing board with
-- one permanent added and the announcement moved. Rule 508.3e's other exclusion
-- -- a creature put onto the battlefield attacking -- is not tested here: CR
-- 508.4 says such a creature was never declared and
-- Pawl.Engine.Combat.putOntoBattlefieldAttacking records no event at all, so no
-- board can tell a correct implementation from any other.
--
-- The third board moves Seifer to the DEFENDING seat, which is what proves the
-- relation is read: CR 109.5's "you" is Seifer's controller, bob does not
-- declare, and a reading of AnyPlayer would goad one of bob's own Giants.
seiferSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
seiferSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- Declares `attacker` alone, announces it at `target`, and points the
      -- goad's one target slot at `victim`. Both choices are FILTERED out of
      -- what the engine offered rather than built, so a recipient CR 608.2b
      -- would drop at resolution cannot pass for the right one, and a mutation
      -- cannot be repaired by an answerer that hunts for something legal.
      answering :: ObjectId.ObjectId -> AttackTarget.AttackTarget -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      answering attacker target victim p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== target) (NonEmpty.toList options))
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just victim) . Recipient.objectOf) . snd) sets
        _ -> S.aggressiveAnswer p
      -- One declaration naming TWO attackers, announced at two different things
      -- CR 508.1b admits: `one` at bob and `two` at bob's planeswalker. The goad
      -- still points at `victim`, and everything else is `answering` above.
      splitting :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      splitting one two jace victim p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (\oid -> oid == one || oid == two) ids
        Prompt.ChooseAttackTarget _ _ oid options ->
          let wanted = if oid == one then AttackTarget.OfPlayer S.bob else AttackTarget.OfPlaneswalker jace
           in Maybe.fromMaybe (NonEmpty.head options) (List.find (== wanted) (NonEmpty.toList options))
        _ -> answering one (AttackTarget.OfPlayer S.bob) victim p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      -- How many of rule 508.3e's triggers CR 603.2 wrote down. Read off the log
      -- rather than at gameplay level because a trigger this condition should
      -- not have fired leaves NO gameplay trace on these boards: `thatPlayer` is
      -- bound only off an AttackTarget.OfPlayer, so a spurious firing finds no
      -- legal target and CR 603.3d removes it. The count is the closest
      -- observable there is, and it is what the two silent boards below lead
      -- with.
      fired gs = length [() | GameEvent.AbilityTriggered record <- S.eventsOf gs, isPlayerAttacksPlayer (TriggeredAbility.condition (AbilityTriggered.ability record))]
   in Spec.describe s "Seifer, Balamb Rival" $ do
        -- The proving test: alice attacks bob, so rule 508.3e's two subjects are
        -- alice and bob, and the goad lands on the Giant the trigger named.
        Spec.it s "CR 508.3e attacking a player goads a creature that player controls" $ do
          (gs, mine, theirs) <- board ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant", "Hill Giant"]
          case (mine, theirs) of
            ([elves, _], [first, second]) -> do
              let after = atBlockers (answering elves (AttackTarget.OfPlayer S.bob) second) gs
              Spec.assertEqWith s "CR 701.15a the named Giant is goaded by Seifer's controller" (Goad.goadedBy second after) (Set.singleton S.alice)
              Spec.assertEqWith s "and the Giant the trigger did not name is not" (Goad.goadedBy first after) Set.empty
              -- "That player controls" is bob's, so alice's own attacker was
              -- never a candidate -- the leg that parts the attacked player from
              -- the attacking one.
              Spec.assertEqWith s "and neither is alice's own attacker" (Goad.goadedBy elves after) Set.empty
              Spec.assertEqWith s "CR 508.1 and the Elves really was declared at bob" (sentAt after) (Map.fromList [(elves, AttackTarget.OfPlayer S.bob)])
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob two Giants"
        -- The same board with a Jace added and the one announcement moved to it.
        -- CR 508.5 still makes bob the defending player, so this is the leg an
        -- arm reading that field instead of the event's AttackTarget gets wrong.
        --
        -- Jace is stocked with loyalty by hand: S.addCreature puts a printing
        -- onto the battlefield with no counters, and CR 704.5i would take a
        -- loyalty-0 planeswalker away before attackers are declared.
        Spec.it s "CR 508.3e attacking a planeswalker that player controls leaves it silent" $ do
          (gs, mine, theirs) <- board ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant", "Hill Giant", "Jace Beleren"]
          case (mine, theirs) of
            ([elves, _], [first, second, jace]) -> do
              let ready = S.addCounter CounterKind.Loyalty 3 jace gs
                  after = atBlockers (answering elves (AttackTarget.OfPlaneswalker jace) second) ready
              Spec.assertEqWith s "CR 508.3e no trigger at all" (fired after) 0
              Spec.assertEqWith s "CR 701.15a and neither Giant is goaded" (Goad.goadedBy second after, Goad.goadedBy first after) (Set.empty, Set.empty)
              Spec.assertEqWith s "and the attack really was declared at Jace" (sentAt after) (Map.fromList [(elves, AttackTarget.OfPlaneswalker jace)])
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob two Giants and a Jace"
        -- CR 109.5's "you": the relation is read against the ability's
        -- CONTROLLER, so a Seifer bob controls watches bob's declarations and
        -- not alice's. The firing board with Seifer moved one seat, and nothing
        -- else changed -- and bob's Giants are exactly the creatures a misread
        -- would goad, since the attacked player is bob either way.
        Spec.it s "CR 508.3e a Seifer the defending player controls is silent" $ do
          (gs, mine, theirs) <- board ["Llanowar Elves"] ["Hill Giant", "Hill Giant", "Seifer, Balamb Rival"]
          case (mine, theirs) of
            ([elves], [first, second, _]) -> do
              let after = atBlockers (answering elves (AttackTarget.OfPlayer S.bob) second) gs
              Spec.assertEqWith s "CR 701.15a neither Giant is goaded" (Goad.goadedBy second after, Goad.goadedBy first after) (Set.empty, Set.empty)
              Spec.assertEqWith s "CR 508.3e and no trigger fired to be removed" (fired after) 0
              Spec.assertEqWith s "and the Elves really was declared at bob" (sentAt after) (Map.fromList [(elves, AttackTarget.OfPlayer S.bob)])
            _ -> Spec.assertFailure s "fixture should give alice an Elves, and bob two Giants and a Seifer"
        -- Rule 508.3e's ARITY, which is CR 508.3b's per-TARGET one: TWO
        -- attackers, ONE attacked player, one trigger. A reading against the
        -- per-creature GameEvent.AttackerDeclared (CR 508.3a) fires twice, and
        -- so does one that took every GameEvent.BecameAttacked without narrowing
        -- to OfPlayer -- which is what the second attacker being aimed at bob's
        -- Jace is for. One attacker, or both sent at bob, would let all three
        -- readings agree.
        --
        -- Counted off the event log rather than at gameplay level because this
        -- card cannot show the difference: goading the same Giant twice is
        -- indistinguishable from goading it once (CR 701.15a, the Set the
        -- goadedBy field keeps).
        Spec.it s "CR 508.3e a declaration split across a player and a planeswalker fires it once" $ do
          (gs, mine, theirs) <- board ["Llanowar Elves", "Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant", "Hill Giant", "Jace Beleren"]
          case (mine, theirs) of
            ([one, two, _], [_, second, jace]) -> do
              let ready = S.addCounter CounterKind.Loyalty 3 jace gs
                  after = atBlockers (splitting one two jace second) ready
              Spec.assertEqWith s "one trigger from the one attacked PLAYER" (fired after) 1
              Spec.assertEqWith s "CR 701.15a and the Giant it named is goaded" (Goad.goadedBy second after) (Set.singleton S.alice)
              Spec.assertEqWith s "CR 508.1b and the declaration really did split" (sentAt after) (Map.fromList [(one, AttackTarget.OfPlayer S.bob), (two, AttackTarget.OfPlaneswalker jace)])
            _ -> Spec.assertFailure s "fixture should give alice two Elves and a Seifer, and bob two Giants and a Jace"

-- Rule 508.3e's condition, read off the log entry CR 603.2 writes for a trigger.
isPlayerAttacksPlayer :: TriggerCondition.TriggerCondition -> Bool
isPlayerAttacksPlayer condition = case condition of
  TriggerCondition.PlayerAttacksPlayer {} -> True
  _ -> False

-- CR 508.3e's SECOND subject named, with Lulu, Stern Guardian {2}{U} Legendary
-- Creature -- Human Wizard 2/3: "Whenever an opponent attacks you, choose
-- target creature attacking you. Put a stun counter on that creature." Seifer
-- above leaves that subject bare (AnyPlayer); this one pins it to You, and the
-- pair is what makes the field observable.
--
-- THREE SEATS, because two collapse it: CR 506.2a has the attacking player
-- choose one opponent as a turn-based action, so the two boards below differ in
-- exactly that answer -- alice attacks bob on one and carol on the other, with
-- bob's Lulu, alice's two Pikers and everything else identical.
--
-- Opponent on the ATTACKING side is not observable here and is not claimed to
-- be: the attacked side is already pinned to Lulu's controller, and a player
-- cannot attack themselves, so AnyPlayer would pick the same declarations. It
-- is Seifer's board that exercises that half of the payload.
luluSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
luluSpec s registry =
  let -- Answers CR 506.2a's turn-based choice with `defender`, sends every
      -- Piker at that player, and points the stun's one target slot at
      -- `victim`. Every choice is FILTERED out of what the engine offered
      -- rather than built, seiferSpec's reason above.
      answering :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      answering defender attackers victim p = case p of
        Prompt.ChooseDefender _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== defender) (NonEmpty.toList options))
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer defender) (NonEmpty.toList options))
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just victim) . Recipient.objectOf) . snd) sets
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      stunOn oid gs = fmap (Map.findWithDefault 0 CounterKind.Stun . Object.counters) (Game.lookupObject oid gs)
      fired gs = length [() | GameEvent.AbilityTriggered record <- S.eventsOf gs, isPlayerAttacksPlayer (TriggeredAbility.condition (AbilityTriggered.ability record))]
   in Spec.describe s "Lulu, Stern Guardian" $ do
        -- The proving test: alice picks bob, so rule 508.3e's two subjects are
        -- alice and bob and the trigger fires. TWO Pikers so the target slot is
        -- a real choice -- one candidate would let the prompt short-circuit and
        -- the answer would prove nothing.
        Spec.it s "CR 508.3e an opponent attacking you stuns a creature attacking you" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          lulu <- S.printingOf s registry "Lulu, Stern Guardian"
          let (gs, mine, theirs, _) = S.threePlayerCombat [piker, piker] [lulu] []
          case (mine, theirs) of
            ([first, second], [_]) -> do
              let after = atBlockers (answering S.bob [first, second] second) gs
              Spec.assertEqWith s "CR 122.1d the Piker the trigger named has a stun counter" (stunOn second after) (Just 1)
              Spec.assertEqWith s "and the Piker it did not name has none" (stunOn first after) (Just 0)
              Spec.assertEqWith s "CR 508.3e one trigger, from the one attacked player" (fired after) 1
              Spec.assertEqWith s "CR 508.1b and both Pikers really were declared at bob" (sentAt after) (Map.fromList [(first, AttackTarget.OfPlayer S.bob), (second, AttackTarget.OfPlayer S.bob)])
            _ -> Spec.assertFailure s "fixture should give alice two Pikers and bob a Lulu"
        -- The same board with CR 506.2a's answer moved to carol, and nothing
        -- else changed. This is the leg the attacked relation buys: a payload
        -- leaving that subject bare fires here too.
        --
        -- Led by the TRIGGER COUNT rather than by the counters, because the
        -- counters cannot tell the two readings apart -- Lulu's own target
        -- filter is "attacking you", so a spurious firing finds no legal target
        -- and CR 603.3d removes it before anything is placed. The count is the
        -- closest observable there is, seiferSpec's silent boards' reason.
        Spec.it s "CR 508.3e an opponent attacking somebody else leaves it silent" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          lulu <- S.printingOf s registry "Lulu, Stern Guardian"
          let (gs, mine, theirs, _) = S.threePlayerCombat [piker, piker] [lulu] []
          case (mine, theirs) of
            ([first, second], [_]) -> do
              let after = atBlockers (answering S.carol [first, second] second) gs
              Spec.assertEqWith s "CR 508.3e no trigger at all" (fired after) 0
              Spec.assertEqWith s "CR 122.1d and neither Piker is stunned" (stunOn second after, stunOn first after) (Just 0, Just 0)
              Spec.assertEqWith s "CR 508.1b and both Pikers really were declared at carol" (sentAt after) (Map.fromList [(first, AttackTarget.OfPlayer S.carol), (second, AttackTarget.OfPlayer S.carol)])
            _ -> Spec.assertFailure s "fixture should give alice two Pikers and bob a Lulu"

-- CR 508.3d's SUBJECT named by the payload: the player who declared the
-- attackers, bound under Pawl.Engine.Binding.attackingPlayer.
--
-- Synthetic Marauder's Toll {2}{W} Enchantment
-- (data/cards/synthetic-marauders-toll.json): "Whenever a player attacks, the
-- attacking player loses 2 life." SYNTHETIC because both printings that name
-- rule 508.3d's player back need a clause pawl cannot express yet: Norn's
-- Decree {2}{W} prints "if one or more players being attacked are poisoned"
-- (no Filter atom reads a player's poison counters) beside a GROUPED
-- combat-damage trigger, and Mirkwood Trapper {1}{G}{U} prints "if they aren't
-- attacking you" beside a choice made by a player who is not the ability's
-- controller (#2930). Both are the real producers and neither is weakened
-- here; the toll's one line is rule 508.3d's binding and nothing else.
--
-- THREE SEATS, because two collapse the attacker onto either the ability's
-- controller or the attacked player: bob holds the enchantment, alice
-- declares, and CR 506.2a's answer sends the declaration at carol. So the
-- three readings a wrong arm could take -- the declarer, CR 109.5's "you", and
-- CR 508.1b's announced player -- are three different life totals.
--
-- The ACTIVE player is not discriminated from the declarer and cannot be: CR
-- 506.2 lets only the active player declare attackers, so the two are the same
-- seat on every board the rules admit.
marauderTollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
marauderTollSpec s registry =
  let -- Answers CR 506.2a's turn-based choice with `defender` and sends every
      -- Piker at that player, both FILTERED out of what the engine offered
      -- rather than built, luluSpec's reason above.
      answering :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      answering defender attackers p = case p of
        Prompt.ChooseDefender _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== defender) (NonEmpty.toList options))
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer defender) (NonEmpty.toList options))
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
      fired gs = length [() | GameEvent.AbilityTriggered record <- S.eventsOf gs, isPlayerAttacks (TriggeredAbility.condition (AbilityTriggered.ability record))]
      fixture = do
        piker <- S.printingOf s registry "Goblin Piker"
        toll <- S.printingOf s registry "Synthetic Marauder's Toll"
        pure (S.threePlayerCombat [piker, piker] [toll] [])
   in Spec.describe s "Synthetic Marauder's Toll" $ do
        -- The proving test: alice declares, so the 2 life comes off alice --
        -- not off bob, whose enchantment it is, and not off carol, whom the
        -- declaration was announced at.
        Spec.it s "CR 508.3d the declaring player is the one that pays" $ do
          (gs, mine, theirs, _) <- fixture
          case (mine, theirs) of
            ([first, second], [_]) -> do
              let after = atBlockers (answering S.carol [first, second]) gs
              Spec.assertEqWith s "CR 119.3 alice lost the 2 life and neither other seat lost any" (lives after) (Just 18, Just 20, Just 20)
              Spec.assertEqWith s "CR 508.3d one trigger for the one declaration, however many creatures were in it" (fired after) 1
              Spec.assertEqWith s "CR 508.1b and both Pikers really were declared at carol" (sentAt after) (Map.fromList [(first, AttackTarget.OfPlayer S.carol), (second, AttackTarget.OfPlayer S.carol)])
            _ -> Spec.assertFailure s "fixture should give alice two Pikers and bob a Toll"
        -- The same board with CR 506.2a's answer moved to bob, and nothing else
        -- changed. The attacked player moves and the payer does not, which is
        -- what parts this slot from the one CR 508.3e's arm stamps.
        Spec.it s "CR 508.3d the payer does not follow who was attacked" $ do
          (gs, mine, theirs, _) <- fixture
          case (mine, theirs) of
            ([first, second], [_]) -> do
              let after = atBlockers (answering S.bob [first, second]) gs
              Spec.assertEqWith s "CR 119.3 alice still lost the 2 life, and bob none" (lives after) (Just 18, Just 20, Just 20)
              Spec.assertEqWith s "CR 508.3d and it fired once here too" (fired after) 1
              Spec.assertEqWith s "CR 508.1b and both Pikers really were declared at bob" (sentAt after) (Map.fromList [(first, AttackTarget.OfPlayer S.bob), (second, AttackTarget.OfPlayer S.bob)])
            _ -> Spec.assertFailure s "fixture should give alice two Pikers and bob a Toll"

-- CR 508.3e's TWO subjects named by one payload, which is what parts the
-- attacking player's slot from the attacked player's.
--
-- Synthetic Reprisal Ledger {1}{U}{B} Enchantment
-- (data/cards/synthetic-reprisal-ledger.json): "Whenever a player attacks
-- another player, the attacking player loses 2 life and the attacked player
-- loses 3 life." SYNTHETIC because Archnemesis {1}{U}{B}, the printing that
-- names rule 508.3e's attacking player ("whenever a player attacks you, you may
-- attach this Aura to that player"), needs CR 303.4's attaching an Aura to a
-- PLAYER and a CR 508.3e attacked side named by attachment, neither of which
-- pawl has (#2931). Seifer, Balamb Rival and Lulu,
-- Stern Guardian above read the attacked player alone.
--
-- THREE SEATS and TWO DIFFERENT AMOUNTS, so no pair of readings agrees: bob
-- holds the Ledger, alice declares, carol is announced. An arm binding one
-- player under both slots takes 5 life off one seat, and an arm that swapped
-- them takes 3 off alice.
reprisalLedgerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
reprisalLedgerSpec s registry =
  let answering :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      answering defender attackers p = case p of
        Prompt.ChooseDefender _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== defender) (NonEmpty.toList options))
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer defender) (NonEmpty.toList options))
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
      fired gs = length [() | GameEvent.AbilityTriggered record <- S.eventsOf gs, isPlayerAttacksPlayer (TriggeredAbility.condition (AbilityTriggered.ability record))]
      fixture = do
        piker <- S.printingOf s registry "Goblin Piker"
        ledger <- S.printingOf s registry "Synthetic Reprisal Ledger"
        pure (S.threePlayerCombat [piker, piker] [ledger] [])
   in Spec.describe s "Synthetic Reprisal Ledger" $ do
        -- The proving test: three seats, three life totals, and the two slots
        -- land on the two players rule 508.3e names.
        Spec.it s "CR 508.3e the attacker and the attacked player are told apart" $ do
          (gs, mine, theirs, _) <- fixture
          case (mine, theirs) of
            ([first, second], [_]) -> do
              let after = atBlockers (answering S.carol [first, second]) gs
              Spec.assertEqWith s "CR 119.3 alice lost 2 as the attacker, carol 3 as the attacked player, bob none" (lives after) (Just 18, Just 20, Just 17)
              Spec.assertEqWith s "CR 508.3e one trigger from the one attacked player" (fired after) 1
              Spec.assertEqWith s "CR 508.1b and both Pikers really were declared at carol" (sentAt after) (Map.fromList [(first, AttackTarget.OfPlayer S.carol), (second, AttackTarget.OfPlayer S.carol)])
            _ -> Spec.assertFailure s "fixture should give alice two Pikers and bob a Ledger"
        -- CR 506.2a's answer moved to bob, so the ability's own controller is
        -- the attacked player: the 3 follows the announcement onto bob while
        -- the 2 stays on alice.
        Spec.it s "CR 508.3e the attacked player's 3 follows the announcement" $ do
          (gs, mine, theirs, _) <- fixture
          case (mine, theirs) of
            ([first, second], [_]) -> do
              let after = atBlockers (answering S.bob [first, second]) gs
              Spec.assertEqWith s "CR 119.3 alice still lost 2, and bob lost the 3" (lives after) (Just 18, Just 17, Just 20)
              Spec.assertEqWith s "CR 508.3e and it fired once here too" (fired after) 1
              Spec.assertEqWith s "CR 508.1b and both Pikers really were declared at bob" (sentAt after) (Map.fromList [(first, AttackTarget.OfPlayer S.bob), (second, AttackTarget.OfPlayer S.bob)])
            _ -> Spec.assertFailure s "fixture should give alice two Pikers and bob a Ledger"

-- CR 122.1's experience counters READ, with Ezuri, Claw of Progress {2}{G}{U}
-- Legendary Creature -- Phyrexian Elf Warrior 3/3: "Whenever a creature you
-- control with power 2 or less enters, you get an experience counter. At the
-- beginning of combat on your turn, put X +1/+1 counters on another target
-- creature you control, where X is the number of experience counters you have."
--
-- Pawl.ZoneTriggerSpec's permanentDiesSpec is where the counters are HANDED
-- OUT, with Meren of Clan Nel Toth. Nothing counted them until this card: an experience counter is
-- CR 122.1's bare first sentence and no rule reads one, so the only possible
-- reader is a card's own text, and the pool had none.
--
-- Both of Ezuri's abilities are triggered, which is why the whole card sits in
-- this spec rather than being split. The first is CR 603.6a's second written
-- form ("whenever a [type] enters") narrowed by a POWER CEILING, and the second is
-- a CR 603.2b step trigger whose Quantity is Quantity.PlayerCounters -- the arm
-- CR 728.1's rad mill already used for a rule, aimed for the first time at a
-- counter kind only card text can see.
--
-- Every number on these boards is arranged not to coincide, because arithmetic
-- is all this card does. The target's printed 2/1 is not the experience count
-- (3, then 5), the count is not the number of creatures its controller controls
-- (5, then 2), and the two counts differ from each other -- so a payload that
-- added a constant, counted the board, or read the wrong counter kind lands on a
-- power and toughness no assertion here accepts.
ezuriExperienceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ezuriExperienceSpec s registry =
  let experienceOf = S.playerCounterOf PlayerCounterKind.Experience
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- The board sitting in pid's beginning of combat step -- CR 506.1's first
      -- combat step, rule 507 -- which is the moment Ezuri's second ability
      -- names. Staged directly, as Pawl.RadSpec stages its precombat main phase,
      -- because Engine.runStep is what writes the CR 603.2b StepBegan record this
      -- trigger matches.
      atBeginningOfCombat pid gs =
        gs
          { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.activePlayer = pid,
            GameState.priority = Just pid
          }
      -- Every target slot aimed at one object, where S.identityAnswer would take
      -- the least Recipient -- which on the first board below is one of the three
      -- Pikers rather than the permanent every assertion is about.
      aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimAt oid p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
        _ -> S.identityAnswer p
      -- alice casts the spell in her hand and lets the stack empty, so the spell
      -- resolves and so does whatever Ezuri's entry trigger put on top of it.
      castAndResolve sid gs =
        let onStack = S.runPure S.identityAnswer gs (S.cast S.alice sid)
         in S.runPure S.identityAnswer onStack Engine.priorityLoop
      -- alice's Ezuri beside one Bonded Construct, and nothing else. The
      -- Construct is ARRANGED rather than cast, so it contributes no enters
      -- event and no experience counter of its own -- every counter on these
      -- boards is one the test put there deliberately.
      ezuriAndTarget = do
        ezuri <- S.printingOf s registry "Ezuri, Claw of Progress"
        construct <- S.printingOf s registry "Bonded Construct"
        let (ezuriId, withEzuri) = S.addCreature ezuri S.alice (Setup.emptyGame S.bothPlayers)
            (targetId, gs) = S.addCreature construct S.alice withEzuri
        pure (ezuriId, targetId, gs)
   in Spec.describe s "Ezuri, Claw of Progress" $ do
        -- The whole arc #858 asks for, at gameplay level: alice CASTS three
        -- small creature spells, the counters accumulate on her, and a
        -- permanent's size changes by exactly that many. The Construct she
        -- already had is the target, so its printed 2/1 is untouched by the
        -- casting and 5/4 can only be 2/1 plus three.
        Spec.it s "CR 122.1 three cast creature spells become three experience counters, and the combat trigger spends them" $ do
          ezuri <- S.printingOf s registry "Ezuri, Claw of Progress"
          construct <- S.printingOf s registry "Bonded Construct"
          piker <- S.printingOf s registry "Goblin Piker"
          mountain <- S.printingOf s registry "Mountain"
          let (_, withEzuri) = S.addCreature ezuri S.alice (S.landsInPlay mountain 6)
              (targetId, board) = S.addCreature construct S.alice withEzuri
              (gs0, firstPiker) = S.handOne piker board
              (secondPiker, gs1) = S.addHandCard piker S.alice gs0
              (thirdPiker, gs2) = S.addHandCard piker S.alice gs1
              cast = castAndResolve thirdPiker (castAndResolve secondPiker (castAndResolve firstPiker gs2))
              combat = S.runPure (aimAt targetId) (atBeginningOfCombat S.alice cast) (Engine.runStep >> Engine.priorityLoop)
          Spec.assertEqWith s "alice started with no experience" (experienceOf S.alice gs2) 0
          Spec.assertEqWith s "three 2/1 spells resolved, so three experience counters" (experienceOf S.alice cast) 3
          Spec.assertEqWith s "bob, who cast nothing, has none" (experienceOf S.bob cast) 0
          Spec.assertEqWith s "the Construct took one +1/+1 counter per experience counter" (countersOn targetId combat) (Just 3)
          Spec.assertEqWith s "so its printed 2/1 reads 5/4" (S.powerToughnessOf targetId combat) (Just (5, 4))
          -- READING a player's counters is not removing them, and CR 728.1's rad
          -- mill -- the pool's other user of this Quantity, which removes one
          -- counter per nonland card it milled -- is why that is worth an
          -- assertion. Ezuri's printed text says only "the number of experience
          -- counters you have", so alice keeps all three.
          Spec.assertEqWith s "and alice still has all three experience counters" (experienceOf S.alice combat) 3
        -- The control at a DIFFERENT count, which is what stops a payload that
        -- hardcodes three from passing the case above. Same two permanents, five
        -- counters instead of three, and 2/1 reads 7/6.
        --
        -- The offered target set is asserted too, because the outcome alone does
        -- not discriminate: with only two creatures on the board, an answerer
        -- taking the first offer reaches the same place whether or not "another"
        -- rejected Ezuri.
        Spec.it s "CR 122.1 five experience counters put five, and \"another\" keeps Ezuri off her own trigger" $ do
          (ezuriId, targetId, board) <- ezuriAndTarget
          let gs = S.addPlayerCounter PlayerCounterKind.Experience 5 S.alice board
              recordTargets :: Prompt.Prompt r -> State.State [Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient)] r
              recordTargets p = case p of
                Prompt.ChooseTargets _ _ _ sets -> do
                  State.modify' (<> [sets])
                  pure (aimAt targetId p)
                _ -> pure (aimAt targetId p)
              ((_, combat), offered) =
                State.runState (Engine.runGame recordTargets (atBeginningOfCombat S.alice gs) (Engine.runStep >> Engine.priorityLoop)) []
          Spec.assertEqWith
            s
            "the Construct is the only legal target"
            (fmap (fmap snd . Map.elems) offered)
            [[Set.singleton (Recipient.ToCreature targetId)]]
          Spec.assertEqWith s "five counters, not three" (countersOn targetId combat) (Just 5)
          Spec.assertEqWith s "so its printed 2/1 reads 7/6" (S.powerToughnessOf targetId combat) (Just (7, 6))
          Spec.assertEqWith s "and Ezuri, whom \"another\" excludes, took none" (countersOn ezuriId combat) (Just 0)
          Spec.assertEqWith s "leaving her printed 3/3" (S.powerToughnessOf ezuriId combat) (Just (3, 3))
        -- ZERO, the case a "for each" that quietly means "one" would pass. The
        -- ability still triggers and still resolves -- CR 603.2b says nothing
        -- about the count -- so the Construct staying 2/1 has to come from the
        -- Quantity reading 0 rather than from nothing happening, and the stack
        -- assertion is what tells those apart.
        Spec.it s "CR 122.1 no experience counters put no +1/+1 counters, though the ability still resolves" $ do
          (_, targetId, board) <- ezuriAndTarget
          let staged = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Combat CombatStep.BeginningOfCombat) S.alice)] (atBeginningOfCombat S.alice board)
              settled = S.runPure (aimAt targetId) staged Engine.settleForPriority
              combat = S.runPure (aimAt targetId) (atBeginningOfCombat S.alice board) (Engine.runStep >> Engine.priorityLoop)
          Spec.assertEqWith s "alice has no experience counters" (experienceOf S.alice board) 0
          Spec.assertEqWith s "the ability went on the stack anyway" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "no +1/+1 counter was put" (countersOn targetId combat) (Just 0)
          Spec.assertEqWith s "so the Construct keeps its printed 2/1" (S.powerToughnessOf targetId combat) (Just (2, 1))
        -- "WITH POWER 2 OR LESS", the Filter.PowerAtMost arm. Hill Giant is 3/3
        -- and Goblin Piker is 2/1, so the same Ezuri pays one experience counter
        -- for the second and nothing for the first. BOTH halves are here, because
        -- a filter that always rejected and one that always admitted are told
        -- apart only by running both.
        Spec.it s "CR 208.1 power 2 or less: a 3/3 entering pays nothing, a 2/1 pays one" $ do
          ezuri <- S.printingOf s registry "Ezuri, Claw of Progress"
          hillGiant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          mountain <- S.printingOf s registry "Mountain"
          let boardWith n = snd (S.addCreature ezuri S.alice (S.landsInPlay mountain n))
              (giantGs, giantSpell) = S.handOne hillGiant (boardWith 4)
              (pikerGs, pikerSpell) = S.handOne piker (boardWith 2)
          Spec.assertEqWith s "the 3/3 gives alice nothing" (experienceOf S.alice (castAndResolve giantSpell giantGs)) 0
          Spec.assertEqWith s "the 2/1 gives her one" (experienceOf S.alice (castAndResolve pikerSpell pikerGs)) 1
        -- "YOU CONTROL", read through CR 109.5 against the ability's controller
        -- (CR 603.3a). bob's 2/1 entering in front of alice's Ezuri is a creature
        -- with power 2 or less entering, and it pays nobody.
        Spec.it s "CR 109.5 you control: an opponent's 2/1 entering gives alice nothing" $ do
          ezuri <- S.printingOf s registry "Ezuri, Claw of Progress"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withEzuri) = S.addCreature ezuri S.alice (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger piker S.bob withEzuri
              after = S.runPure S.identityAnswer entered (Engine.settleForPriority >> Engine.priorityLoop)
          Spec.assertEqWith s "alice gets no experience counter" (experienceOf S.alice after) 0
          Spec.assertEqWith s "and neither does bob, who has no Ezuri" (experienceOf S.bob after) 0

-- CR 122.1's OBJECT counters read WITHOUT NAMING A KIND, with Savanti Romero,
-- Time's Exile {3}{B}{B} Legendary Creature -- Demon Wizard 4/4: "Trample. At the
-- beginning of combat on your turn, put a +1/+1 counter on Savanti Romero. Then
-- you draw X cards and lose X life, where X is the number of counters on Savanti
-- Romero."
--
-- Quantity.ObjectCountersOfAnyKind is what "the number of counters" is, and it
-- is a SUM over every kind rather than a lookup in one. Quantity.ObjectCounters
-- -- the arm that names a kind, Promising Duskmage's "if it had a +1/+1 counter
-- on it" (Pawl.ZoneTriggerSpec's counterLookBackSpec) -- cannot express this
-- clause at all, which is why the arm exists; see #994.
--
-- STUN COUNTERS are what make the two readings disagree. CR 122.1d gives a stun
-- counter its own rule and no relation to power or toughness, so a permanent
-- carrying two of them plus one +1/+1 counter has THREE counters on it and ONE
-- +1/+1 counter -- and a per-kind read of the +1/+1 kind answers the same 1 it
-- would answer with no stun counters there at all. A board built only out of
-- +1/+1 counters proves nothing here, since both readings agree on it.
--
-- Nothing untaps on these boards, so CR 122.1d's replacement effect never fires
-- and the stun counters sit there being counted, which is all this card asks of
-- them.
--
-- The step is staged and the trigger resolved by hand rather than run through
-- the priority loop: the payload is a draw and a life loss, and a loop that
-- reached alice's next draw step would move the same two numbers for a reason
-- this group is not about.
savantiRomeroSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
savantiRomeroSpec s registry =
  let countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- Ezuri's staging above, for its reason: Engine.runStep is what writes the
      -- CR 603.2b StepBegan record, and this group supplies the record directly
      -- so that only the trigger and its resolution run.
      atBeginningOfCombat pid gs =
        gs
          { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.activePlayer = pid,
            GameState.priority = Just pid
          }
      -- alice's Savanti Romero alone, with seven Swamps in her library -- more
      -- than any leg draws through, so CR 104.3c decks nobody -- and an empty
      -- hand, so every card in it afterwards arrived from this trigger.
      savantiBoard stuns = do
        savanti <- S.printingOf s registry "Savanti Romero, Time's Exile"
        swamp <- S.printingOf s registry "Swamp"
        let (savantiId, withSavanti) = S.addCreature savanti S.alice (Setup.emptyGame S.bothPlayers)
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) withSavanti [1 .. 7 :: Int]
            stunned = if stuns > 0 then S.addCounter CounterKind.Stun stuns savantiId stocked else stocked
        pure (savantiId, stunned)
      combatTrigger board =
        let staged = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Combat CombatStep.BeginningOfCombat) S.alice)] (atBeginningOfCombat S.alice board)
            settled = S.runPure S.identityAnswer staged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)
   in Spec.describe s "Savanti Romero, Time's Exile" $ do
        -- The gameplay-level discrimination. Two stun counters and the +1/+1
        -- counter the trigger itself puts make THREE counters, so alice draws
        -- three and loses three. A read that named the +1/+1 kind would draw one
        -- and lose one on this very board.
        Spec.it s "CR 122.1 the number of counters sums across kinds, so two stun counters and one +1/+1 counter draw three" $ do
          (savantiId, board) <- savantiBoard 2
          let (settled, after) = combatTrigger board
          Spec.assertEqWith s "two stun counters and no +1/+1 counter to start" (countersOn savantiId board) (Map.singleton CounterKind.Stun 2)
          Spec.assertEqWith s "an empty hand and seven cards in the library" (S.handSize S.alice board, librarySize S.alice board) (0, 7)
          Spec.assertEqWith s "alice is at twenty life" (S.lifeOf S.alice board) (Just 20)
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          -- THE assertion this arm exists for: three cards, one per counter of
          -- EITHER kind. One card is what the per-kind reading answers.
          Spec.assertEqWith s "three cards drawn, one per counter of either kind" (S.handSize S.alice after, librarySize S.alice after) (3, 4)
          Spec.assertEqWith s "and three life lost, off the same number" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "the counters afterwards are one +1/+1 beside the two stun" (countersOn savantiId after) (Map.fromList [(CounterKind.PlusOnePlusOne, 1), (CounterKind.Stun, 2)])
          -- CR 122.1a is a DIFFERENT fact from the tally: only the +1/+1 counter
          -- touches power and toughness, so a 4/4 reads 5/5 and not 7/7.
          Spec.assertEqWith s "CR 122.1a its printed 4/4 reads 5/5, the stun counters changing nothing" (S.powerToughnessOf savantiId after) (Just (5, 5))
        -- The same board with the stun counters taken away and nothing else
        -- changed: one counter, one card, one life. It is what stops a payload
        -- that hardcodes three from passing the case above, and -- since the only
        -- counter here is the one the trigger put -- what shows CR 608.2c's order
        -- reads the tally AFTER the placement rather than before it.
        Spec.it s "CR 122.1 with no stun counters the same trigger draws one, counting the counter it just put" $ do
          (savantiId, board) <- savantiBoard 0
          let (settled, after) = combatTrigger board
          Spec.assertEqWith s "no counters at all to start" (countersOn savantiId board) Map.empty
          Spec.assertEqWith s "an empty hand and seven cards in the library" (S.handSize S.alice board, librarySize S.alice board) (0, 7)
          Spec.assertEqWith s "the trigger reached the stack just the same" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "one card drawn, not three" (S.handSize S.alice after, librarySize S.alice after) (1, 6)
          Spec.assertEqWith s "and one life lost, not three" (S.lifeOf S.alice after) (Just 19)
          Spec.assertEqWith s "the only counter on it is the +1/+1 the trigger put" (countersOn savantiId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so its printed 4/4 reads 5/5 here too" (S.powerToughnessOf savantiId after) (Just (5, 5))
        -- A THIRD kind, at a third count. Two stun and three shield counters plus
        -- the +1/+1 make six, which is neither the number of KINDS on it (three)
        -- nor the GREATEST per-kind tally (three) -- the two other folds of the
        -- same map that would pass both cases above.
        Spec.it s "CR 122.1 three kinds at three counts sum rather than being counted or maximized" $ do
          (savantiId, board) <- savantiBoard 2
          let shielded = S.addCounter CounterKind.Shield 3 savantiId board
              (_, after) = combatTrigger shielded
          Spec.assertEqWith s "two stun and three shield counters to start" (countersOn savantiId shielded) (Map.fromList [(CounterKind.Shield, 3), (CounterKind.Stun, 2)])
          Spec.assertEqWith s "six cards drawn, not three" (S.handSize S.alice after, librarySize S.alice after) (6, 1)
          Spec.assertEqWith s "and six life lost" (S.lifeOf S.alice after) (Just 14)

-- CR 601.2i's cast trigger with a payload aimed at a TARGET PLAYER: the pool's
-- first card to hand out poison counters (CR 122.1f, whose tenth loses the game
-- under CR 704.5c) to a player who was CHOSEN rather than derived from the
-- ability's controller (#120).
--
-- Hand of the Praetors, {3}{B} Creature -- Phyrexian Zombie 3/2: "Infect. Other
-- creatures you control with infect get +1/+1. Whenever you cast a creature
-- spell with infect, target player gets a poison counter." Only the third line
-- is this group's subject. The anthem is Pawl.PowerToughnessSpec's, and what the
-- printed infect keyword does to damage is Pawl.DamageSpec's ground already (CR
-- 702.90b).
--
-- The printed condition narrows THREE things in one sentence -- who cast it (CR
-- 109.5's "you", which for a triggered ability is CR 603.3a's controller of the
-- source at the trigger moment), that it was a creature spell, and that it had
-- infect (CR 702.90) -- and the Filter carries all three. Each case below moves
-- exactly one of them, so a Filter that always answered True is distinguishable
-- from one that reads each half.
--
-- THREE SEATS, which the PAYLOAD wants as much as the condition does. On a
-- two-seat board with alice casting, "target player" answered as bob and "an
-- opponent" put the counter in the same place. carol is the seat that separates
-- them: she is a legal target that was not chosen, so an effect that poisoned
-- every opponent fails here too. She serves the condition's "you cast" case for
-- Young Pyromancer's reason as well -- "bob cast it" is not "an opponent cast
-- it" until someone else is sitting there.
handOfThePraetorsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
handOfThePraetorsSpec s registry =
  let poisonOf = S.playerCounterOf PlayerCounterKind.Poison
      -- The trigger's one target slot, answered with `who` rather than left to
      -- S.identityAnswer, whose lowest-sorting candidate on this board is alice
      -- -- the caster, and so the wrong answer to prove anything with.
      aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
        _ -> S.identityAnswer p
      -- alice bears the Hand; alice and bob each get two Forests (Glistener
      -- Elf's {G}) and two Mountains (Goblin Piker's {1}{R}). carol gets no
      -- land: she never casts, and is only ever a seat the counter must miss.
      board forest mountain hand =
        let addLands pid n printing g = List.foldl' (\g' _ -> snd (S.addCreature printing pid g')) g [1 .. (n :: Int)]
            withLands =
              addLands S.bob 2 mountain
                . addLands S.bob 2 forest
                . addLands S.alice 2 mountain
                $ addLands S.alice 2 forest S.threePlayerGame
            (_, withHand) = S.addCreature hand S.alice withLands
         in withHand
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castAndResolve who caster oid gs = S.runPure (aimAt who) (S.runPure (aimAt who) gs (S.cast caster oid)) Engine.priorityLoop
   in Spec.describe s "Hand of the Praetors" $ do
        -- THE case: the counter lands on the player the answerer named, and on
        -- nobody else. Glistener Elf, {G} Creature -- Phyrexian Elf Warrior 1/1
        -- with infect, is the spell cast.
        Spec.it s "CR 601.2i casting an infect creature spell poisons the TARGETED player" $ do
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          hand <- S.printingOf s registry "Hand of the Praetors"
          elf <- S.printingOf s registry "Glistener Elf"
          let (elfId, gs) = S.addHandCard elf S.alice (board forest mountain hand)
              after = castAndResolve S.bob S.alice elfId gs
          Spec.assertEqWith s "nobody is poisoned before the cast" (poisonOf S.bob gs) 0
          Spec.assertEqWith s "bob, who was targeted, has one poison counter" (poisonOf S.bob after) 1
          -- The falsifier for a payload plumbed to the ability's controller:
          -- alice cast it and alice gets nothing.
          Spec.assertEqWith s "alice, who cast it, has none" (poisonOf S.alice after) 0
          -- And the falsifier for one plumbed to every opponent.
          Spec.assertEqWith s "and carol, who was not targeted, has none" (poisonOf S.carol after) 0
        -- The same board and the same answerer, aimed the other way: alice may
        -- target herself, since CR 115.1 puts every player in the pool and
        -- nothing on this card narrows it. A payload that read the caster would
        -- pass this case and fail the one above, and a payload that read an
        -- opponent would do the reverse -- neither passes both.
        Spec.it s "CR 115.1 the same trigger aimed at its own controller poisons her instead" $ do
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          hand <- S.printingOf s registry "Hand of the Praetors"
          elf <- S.printingOf s registry "Glistener Elf"
          let (elfId, gs) = S.addHandCard elf S.alice (board forest mountain hand)
              after = castAndResolve S.alice S.alice elfId gs
          Spec.assertEqWith s "alice, who targeted herself, has one" (poisonOf S.alice after) 1
          Spec.assertEqWith s "bob has none" (poisonOf S.bob after) 0
          Spec.assertEqWith s "and neither has carol" (poisonOf S.carol after) 0
        -- The INFECT half of the Filter, moved on its own: alice still casts, and
        -- what she casts is still a creature spell. Goblin Piker, {1}{R} Creature
        -- -- Goblin Warrior 2/1, has no keyword at all.
        Spec.it s "CR 702.90 a creature spell WITHOUT infect fires nothing" $ do
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          hand <- S.printingOf s registry "Hand of the Praetors"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, gs) = S.addHandCard piker S.alice (board forest mountain hand)
              after = castAndResolve S.bob S.alice pikerId gs
          -- Positive control: the cast really happened and really resolved, so
          -- the silence below is the Filter's answer rather than a fixture that
          -- could not pay for anything.
          Spec.assertEqWith s "the Piker resolved onto the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
          Spec.assertEqWith s "and nobody is poisoned" (poisonOf S.bob after) 0
          Spec.assertEqWith s "not even the caster" (poisonOf S.alice after) 0
        -- The "you cast" half, moved on its own: the same infect creature spell,
        -- cast from the seat to alice's left. carol makes "bob cast it" a
        -- different statement from "an opponent cast it".
        Spec.it s "CR 109.5 'you cast': an OPPONENT's infect creature spell fires nothing" $ do
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          hand <- S.printingOf s registry "Hand of the Praetors"
          elf <- S.printingOf s registry "Glistener Elf"
          let base = board forest mountain hand
              (bobsElf, withBobs) = S.addHandCard elf S.bob base
              (alicesElf, gs) = S.addHandCard elf S.alice withBobs
              byBob = castAndResolve S.bob S.bob bobsElf gs
              byAlice = castAndResolve S.bob S.alice alicesElf gs
          Spec.assertEqWith s "bob's own cast poisons nobody" (poisonOf S.bob byBob) 0
          Spec.assertEqWith s "not alice" (poisonOf S.alice byBob) 0
          Spec.assertEqWith s "and not carol" (poisonOf S.carol byBob) 0
          -- The same board, one caster apart: alice casting her own copy is what
          -- proves the seat is the only thing the silence above turns on.
          Spec.assertEqWith s "the same board poisons bob for alice's own cast" (poisonOf S.bob byAlice) 1

-- Custodi Lich, {3}{B}{B} Creature -- Zombie Cleric 4/2: "When this creature
-- enters, you become the monarch. Whenever you become the monarch, target player
-- sacrifices a creature of their choice." Both printed sentences are in
-- data/cards/custodi-lich.json; nothing is omitted.
--
-- The pool's producer for TriggerCondition.PlayerBecomesMonarch (CR 725.1). The
-- card is its own trigger's cause -- the first ability crowns its controller and
-- the second watches that crowning -- which makes the whole chain observable off
-- one entry, and CR 725.2's crown steal reaches the same condition by a route
-- the card has nothing to do with.
--
-- THREE SEATS throughout. At two players "you" and "an opponent" name
-- complementary halves of a two-element set, so a relation-free arm and a You
-- arm agree on every board; the third seat is what makes crowning somebody who
-- is neither the Lich's controller nor the sacrifice victim expressible.
--
-- Distinct power/toughness on every creature (Lich 4/2, Boggart Brute 3/2,
-- Goblin Piker 2/1, Bird Maiden 1/2, Bog Wraith 3/3) so no assertion below can
-- pass on a numeric coincidence, and the edict's victim always holds TWO
-- creatures so CR 701.21a's choice is a real prompt rather than a forced single
-- candidate -- bob in most cases, carol in the CR 725.4 one, where bob leaves.
monarchTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monarchTriggerSpec s registry =
  let -- Names `victim` for every target slot that offers them. S.identityAnswer
      -- picks the least Recipient, which would aim the edict at alice herself.
      targetsPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      targetsPlayer victim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer victim) sets
        _ -> S.identityAnswer p
      resolveAll :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      resolveAll answer gs = snd (Engine.runGamePure answer gs Engine.priorityLoop)
      -- bob's two creatures and carol's one, on top of whatever the caller
      -- built. carol is the control seat: nothing in either test should ever
      -- touch her, so a payload that hit "a player" rather than the targeted one
      -- is visible.
      bystanders piker birdMaiden bogWraith base =
        let (_, g1) = S.addCreature piker S.bob base
            (_, g2) = S.addCreature birdMaiden S.bob g1
         in snd (S.addCreature bogWraith S.carol g2)
      -- CR 725.2's crown steal, driven by the damage EVENT rather than by a full
      -- combat: Monarch.inherentMatch reads the recorded DamageEvent, and
      -- ExpirySpec's monarch group drives the same rule the same way.
      combatDamageTo monarch damager =
        S.withEvents [GameEvent.DamageDealt (DamageEvent.MkDamageEvent damager (Recipient.ToPlayer monarch) 2 False False False 0 Nothing DamageKind.Combat)]
      -- Attack bob with everything, block with everything, and divide a
      -- trampler's damage the way CR 510.1c and CR 702.19b together require --
      -- each blocker's own threshold first, the excess through to bob. The
      -- thresholds the prompt offers ARE CR 510.1c's lethal amounts, so nothing
      -- here restates a creature's toughness. Written out rather than left to
      -- S.identityAnswer, which never names a player recipient and would put the
      -- whole assignment on the blocker. Pawl.InitiativeSpec keeps its own copy,
      -- Pawl.Support being too expensive a home for a two-case helper.
      tramplingAtBob :: Prompt.Prompt r -> r
      tramplingAtBob p = case p of
        Prompt.AssignCombatDamage _ _ _ thresholds n ->
          let toBlockers = Map.delete (Recipient.ToPlayer S.bob) thresholds
           in Map.insert (Recipient.ToPlayer S.bob) (n - sum (Map.elems toBlockers)) toBlockers
        _ -> S.attackTo S.bob p
   in Spec.describe s "MonarchTrigger" $ do
        -- The whole chain off one entry: CR 603.6a's entry trigger crowns alice,
        -- Effect.BecomeMonarch records CR 725.1's event, and the second ability
        -- matches it.
        Spec.it s "CR 725.1 Custodi Lich whole card: entering crowns alice, and that crowning fires her edict" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (Setup.emptyGame S.threePlayers)
              (lich, gs) = S.entersWithTrigger custodiLich S.alice base
              after = resolveAll (targetsPlayer S.bob) gs
          Spec.assertEqWith s "no monarch before the Lich resolved its entry trigger" (GameState.monarch gs) Nothing
          Spec.assertEqWith s "CR 725.1 alice is the monarch" (GameState.monarch after) (Just S.alice)
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "and the crowning recorded its event"
          Spec.assertEqWith s "CR 701.21a the targeted bob lost exactly one of his two" (S.creaturesInPlay S.bob after) 1
          Spec.assertEqWith s "carol, untargeted, lost none" (S.creaturesInPlay S.carol after) 1
          Spec.assertBool s (S.onBattlefield lich after) "and alice's own Lich is untouched"
          Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []
        -- Gatherer, 2016-08-23, on this very card: "Abilities that trigger
        -- whenever you 'become the monarch' trigger only if you aren't already
        -- the monarch. For example, if you are already the monarch as Custodi
        -- Lich enters the battlefield, its last ability won't trigger." So a
        -- crowning of the player who already holds the crown is not an event at
        -- all, and Monarch.crown records nothing for it -- which is also what
        -- keeps this reading and CR 725's exile watch (Palace Jailer's "until an
        -- opponent becomes the monarch") answering the same question the same
        -- way.
        --
        -- The case above is the exact paired control: same card, same seats, same
        -- answerer, and the one difference is who holds the crown as the Lich
        -- enters.
        Spec.it s "CR 725.3 a player who is ALREADY the monarch does not become the monarch, so the Lich's edict stays silent" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (S.withMonarch S.alice (Setup.emptyGame S.threePlayers))
              (lich, gs) = S.entersWithTrigger custodiLich S.alice base
              after = resolveAll (targetsPlayer S.bob) gs
          Spec.assertEqWith s "alice was the monarch before the Lich entered" (GameState.monarch gs) (Just S.alice)
          Spec.assertEqWith s "CR 701.21a bob, whom the edict would have targeted, kept both of his" (S.creaturesInPlay S.bob after) 2
          Spec.assertEqWith s "carol kept hers" (S.creaturesInPlay S.carol after) 1
          Spec.assertEqWith s "alice still holds the crown, so the entry trigger did resolve" (GameState.monarch after) (Just S.alice)
          Spec.assertBool s (notElem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "and recorded no crowning, because nobody became the monarch"
          Spec.assertBool s (S.onBattlefield lich after) "the Lich itself is on the battlefield"
          Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []
        -- CR 603.3a / 109.5: the relation is read against the ABILITY'S
        -- CONTROLLER, so a crowning of somebody else is silence. Denethor, Stone
        -- Seer's "target player becomes the monarch" is the pool's one way to
        -- crown a chosen player, and it records the very same event the test
        -- above matched -- so what separates the two tests is WHO was crowned and
        -- nothing else.
        Spec.it s "CR 603.3a/109.5 a crowning of bob does not fire alice's Custodi Lich" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          denethor <- S.printingOf s registry "Denethor, Stone Seer"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (Setup.emptyGame S.threePlayers)
              lands = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) base [1 .. 4 :: Int]
              -- addCreature, not entersWithTrigger: the Lich is ALREADY on the
              -- battlefield with its entry trigger long since resolved, so the
              -- only crowning in this test is Denethor's.
              (lich, g1) = S.addCreature custodiLich S.alice lands
              (denethorId, g2) = S.addCreature denethor S.alice g1
              gs = g2 {GameState.priority = Just S.alice}
              -- Denethor's two slots, named separately (CR 601.2c lets one
              -- ability write "target" twice): the crown goes to bob, and the 3
              -- damage to CAROL the player, so nothing on the board dies and a
              -- creature count that moved can only have been a sacrifice. Any
              -- OTHER slot -- which today means only the Lich's edict, if it
              -- wrongly fired -- takes bob, so a trigger that should have stayed
              -- silent is loud when it does not.
              denethorAnswers = Map.fromList [(SlotName.MkSlotName (Text.pack "player"), Recipient.ToPlayer S.bob), (SlotName.MkSlotName (Text.pack "damage"), Recipient.ToPlayer S.carol)]
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets ->
                  Map.mapWithKey
                    ( \slot offer ->
                        let wanted = Map.findWithDefault (Recipient.ToPlayer S.bob) slot denethorAnswers
                         in Map.findWithDefault Set.empty slot (S.preferring (== wanted) (Map.singleton slot offer))
                    )
                    sets
                _ -> S.identityAnswer p
              activated = case Face.activatedAbilities (S.combinedFace denethor) of
                ability : _ -> S.runPure answer gs (Activate.activateAbility S.alice denethorId ability)
                [] -> gs
              after = resolveAll answer activated
          Spec.assertEqWith s "no monarch going in" (GameState.monarch gs) Nothing
          Spec.assertEqWith s "CR 725.1 bob, the targeted player, took the crown" (GameState.monarch after) (Just S.bob)
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.bob) (S.eventsOf after)) "and the event names bob, so there really was a crowning to match"
          Spec.assertBool s (S.onBattlefield lich after) "alice's Lich is still on the battlefield to have watched it"
          -- The discriminating trio: nobody sacrificed anything. Under a
          -- relation-free arm bob would have lost one, and under an inverted
          -- relation so would whoever the edict targeted.
          Spec.assertEqWith s "bob kept both of his" (S.creaturesInPlay S.bob after) 2
          Spec.assertEqWith s "carol kept hers" (S.creaturesInPlay S.carol after) 1
          -- The ability really resolved in full, so "no sacrifice" cannot mean
          -- "nothing happened": carol took Denethor's 3.
          Spec.assertEqWith s "CR 115.4 carol, the any-target, took the 3" (S.lifeOf S.carol after) (Just 17)
          Spec.assertEqWith s "the stack is empty, so no trigger is waiting" (GameState.stack after) []
        -- CR 725.2's crown steal reaches the SAME condition by a route the card
        -- has nothing to do with: the inherent ability has no source, and
        -- Monarch.inherentMatch rather than Event.matchesTrigger is what fires
        -- it. What the Lich matches is the crowning, not the entry that usually
        -- causes one.
        Spec.it s "CR 725.2 a stolen crown is a crowning, and fires the same trigger" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          boggartBrute <- S.printingOf s registry "Boggart Brute"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (S.withMonarch S.bob (Setup.emptyGame S.threePlayers))
              (lich, g1) = S.addCreature custodiLich S.alice base
              (brute, gs) = S.addCreature boggartBrute S.alice g1
              after = resolveAll (targetsPlayer S.bob) (combatDamageTo S.bob brute gs)
          Spec.assertEqWith s "bob wore the crown going in" (GameState.monarch gs) (Just S.bob)
          Spec.assertEqWith s "CR 725.2 alice's creature took it off him" (GameState.monarch after) (Just S.alice)
          Spec.assertBool s (S.onBattlefield lich after) "the Lich watched from the battlefield"
          Spec.assertEqWith s "CR 725.1 alice's trigger fired: the targeted bob sacrificed one" (S.creaturesInPlay S.bob after) 1
          Spec.assertEqWith s "carol lost none" (S.creaturesInPlay S.carol after) 1
          Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
        -- The discriminating twin of the test above: the SAME board, the same
        -- inherent ability, the same event shape -- only the creature that dealt
        -- the damage differs, so the crown lands on carol instead of alice. An
        -- arm that ignored the relation would fire here too.
        Spec.it s "CR 725.2/109.5 a crown stolen by carol does not fire alice's trigger" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          boggartBrute <- S.printingOf s registry "Boggart Brute"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (S.withMonarch S.bob (Setup.emptyGame S.threePlayers))
              (lich, g1) = S.addCreature custodiLich S.alice base
              (_, gs) = S.addCreature boggartBrute S.alice g1
              wraith = case filter (\oid -> S.soleFaceName oid gs == S.printingName bogWraith) (Game.zoneMembers Zone.Battlefield S.carol gs) of
                oid : _ -> oid
                [] -> S.noSource
              after = resolveAll (targetsPlayer S.bob) (combatDamageTo S.bob wraith gs)
          Spec.assertEqWith s "CR 725.2 carol took the crown" (GameState.monarch after) (Just S.carol)
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.carol) (S.eventsOf after)) "and the crowning event names carol"
          Spec.assertBool s (S.onBattlefield lich after) "alice's Lich is still there, and still silent"
          Spec.assertEqWith s "bob kept both of his" (S.creaturesInPlay S.bob after) 2
          Spec.assertEqWith s "carol kept hers" (S.creaturesInPlay S.carol after) 1
          Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
        -- CR 725.4's third route into the crown: no effect and no inherent
        -- ability, just the monarch leaving the game. Three seats are mandatory
        -- twice over -- Departure.continuesAfterDeparture skips all of CR 800.4a
        -- at two (CR 800.1), and the edict's victim has to be somebody other
        -- than the departed monarch and the Lich's controller.
        --
        -- The bystanders helper is not used: its two creatures sit with bob, who
        -- is the one leaving here, so carol holds the pair instead (Goblin Piker
        -- 2/1, Bird Maiden 1/2) and CR 701.21a's choice stays a real prompt.
        Spec.it s "CR 725.4 a departure crowns alice, and that crowning fires her edict" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          let base = S.withMonarch S.bob (Setup.emptyGame S.threePlayers)
              (lich, g1) = S.addCreature custodiLich S.alice base
              (_, g2) = S.addCreature piker S.carol g1
              (_, gs) = S.addCreature birdMaiden S.carol g2
              -- CR 104.3a: bob concedes, so the crown is reassigned inside the
              -- departure rather than by anything that resolves afterwards.
              departed = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.bob)
              after = resolveAll (targetsPlayer S.carol) departed
          Spec.assertEqWith s "bob wore the crown going in" (GameState.monarch gs) (Just S.bob)
          Spec.assertEqWith s "alice is the active player, so CR 725.4's first sentence crowns her" (GameState.activePlayer gs) S.alice
          Spec.assertEqWith s "CR 725.4 alice is the monarch" (GameState.monarch after) (Just S.alice)
          Spec.assertBool s (S.onBattlefield lich after) "alice's Lich watched from the battlefield"
          -- Asserted BEFORE the event, so a run with the record deleted fails
          -- here rather than on the event and the payload is what is pinned.
          Spec.assertEqWith s "CR 701.21a the targeted carol lost exactly one of her two" (S.creaturesInPlay S.carol after) 1
          Spec.assertEqWith s "and alice, untargeted, still has her Lich" (S.creaturesInPlay S.alice after) 1
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "and the reassignment recorded its crowning"
          Spec.assertEqWith s "CR 104.2a two survivors, so the game is still going" (GameState.result after) Nothing
          Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []
        -- CR 725.2 makes the crown steal "controlled by the player who was the
        -- monarch at the time the abilities triggered", so when the damage that
        -- triggers it also kills the monarch, CR 800.4d keeps it off the stack
        -- and CR 725.4 alone moves the crown -- to the ACTIVE player, not the
        -- damager's controller. Gatherer's Court of Grace ruling says the same:
        -- the steal "doesn't resolve", and the attacker's controller usually
        -- ends up monarch only because "it is likely their turn". #3148 claimed
        -- the opposite.
        --
        -- Three seats with the damager's controller NOT the active player is the
        -- only board where the two readings differ, and no real combat reaches
        -- it (CR 508.1a: only the active player's creatures attack; CR 506.4: a
        -- controller change removes a creature from combat), so the damage is a
        -- hand-written event and bob's life total is written down by hand at
        -- what the 2 it records would have left him with.
        Spec.it s "CR 725.4/800.4d lethal combat damage to the monarch crowns the active player, not the damager's controller" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (S.withMonarch S.bob (Setup.emptyGame S.threePlayers))
              wraith = case Game.zoneMembers Zone.Battlefield S.carol base of
                oid : _ -> oid
                [] -> S.noSource
              dying = base {GameState.players = Map.adjust (\p -> p {Player.life = -1}) S.bob (GameState.players base)}
              after = resolveAll S.identityAnswer (combatDamageTo S.bob wraith dying)
          Spec.assertEqWith s "bob wore the crown going in" (GameState.monarch base) (Just S.bob)
          Spec.assertEqWith s "alice is the active player, and carol's Wraith dealt the damage" (GameState.activePlayer base, Projection.controllerOf wraith base) (S.alice, Just S.carol)
          -- The rule: CR 725.4's hand-off is the only crowning.
          Spec.assertEqWith s "CR 725.4 alice, the active player, is the monarch" (GameState.monarch after) (Just S.alice)
          Spec.assertEqWith s "CR 800.4d carol was never crowned: bob's steal trigger did not reach the stack" (filter (== GameEvent.BecameMonarch S.carol) (S.eventsOf after)) []
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "and the hand-off recorded its crowning, naming her"
          Spec.assertEqWith s "CR 704.5a bob lost the game" (Game.stillPlaying after) [S.alice, S.carol]
          Spec.assertEqWith s "CR 104.2a two survivors, so the game is still going" (GameState.result after) Nothing
          Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []
        -- CR 603.10, first sentence: the crown steal is checked against the
        -- objects that exist IMMEDIATELY AFTER the damage, and the CR 704.5g
        -- destruction that kills a trampler its blocker traded with is a LATER
        -- event. Engine.performSettle runs that state-based action before the
        -- trigger scan, so a live read of the damager found an id
        -- Event.placeObject had already retired, and the crown never moved; see
        -- #3132.
        --
        -- A REAL combat, not the group's hand-written damage event, because the
        -- fixture that rewrites the log is exactly the fixture that cannot produce
        -- this board: alice attacks bob with War Mammoth (3/3 trample) and bob
        -- blocks with Boggart Brute (3/2), so the Mammoth assigns the Brute its
        -- lethal 2 and tramples 1 through, and the Brute's 3 kills the Mammoth in
        -- the same step. carol never joins.
        Spec.it s "CR 725.2 a trampler that trades with its blocker still steals the crown" $ do
          warMammoth <- S.printingOf s registry "War Mammoth"
          boggartBrute <- S.printingOf s registry "Boggart Brute"
          piker <- S.printingOf s registry "Goblin Piker"
          let (base, mine, theirs, _) = S.threePlayerCombat [warMammoth] [boggartBrute] [piker]
              staged =
                base
                  { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
                    GameState.combat = (GameState.combat base) {Combat.Type.defenders = [S.bob]}
                  }
              held = S.withMonarch S.bob staged
              after = resolveAll tramplingAtBob (S.fightWith tramplingAtBob held)
          Spec.assertEqWith s "bob wore the crown going in" (GameState.monarch held) (Just S.bob)
          -- The board this case is about. Neither this nor the life total can tell
          -- the two readings apart -- the Mammoth dies and bob loses 1 under both
          -- -- so they pin the board rather than prove the rule.
          Spec.assertEqWith s "CR 704.5g the Mammoth traded with the Brute, so both are off the battlefield" (fmap (\oid -> S.onBattlefield oid after) (mine <> theirs)) [False, False]
          Spec.assertEqWith s "CR 702.19b and 1 point trampled through to bob" (S.lifeOf S.bob after) (Just 19)
          -- The rule: alice's dead Mammoth still crowned her.
          Spec.assertEqWith s "CR 725.2 alice is the monarch" (GameState.monarch after) (Just S.alice)
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "and the crowning recorded its event, naming her"
          Spec.assertEqWith s "carol, who never joined the combat, kept her creature" (S.creaturesInPlay S.carol after) 1
          Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []

-- CR 603.7: Ray of Command's THIRD sentence -- "When you lose control of the
-- creature, tap it." A delayed triggered ability whose event is a CONTROL CHANGE,
-- which is the observation point Engine.sampleControl exists to provide: control is
-- derived (CR 613.1b layer 2), so the CR 514.2 sweep that ends the spell's
-- until-end-of-turn control effect announces nothing, and the diff against
-- GameState.controlSample is what mints the GameEvent.ControlChanged the condition
-- matches. CR 514.3a is what then gives the trigger its round: a triggered ability
-- waiting during the cleanup step gets put on the stack and the active player gets
-- priority.
--
-- THREE SEATS, because the condition reads ONE of them. "You" is the ability's
-- controller (CR 603.7d, alice), the creature's owner and the player control
-- returns to is bob, and carol holds a creature alice steals with a card that has no
-- third sentence. On a two-player board "you", "the creature's owner" and "an
-- opponent" collapse, and a condition matching the wrong one of the three would
-- still pass.
--
-- ACT OF TREASON is the negative leg, and the two legs run on ONE board: the same
-- mana, the same seats, two identical tapped Goblin Pikers, the same cleanup step.
-- The single difference is which card did the stealing -- Act of Treason ({2}{R}
-- Sorcery, "Gain control of target creature until end of turn. Untap that creature.
-- It gains haste until end of turn.") prints the same three effects and NOT the tap
-- sentence, so carol's creature coming home untapped is what shows the tap is Ray of
-- Command's own ability rather than anything the cleanup machinery does to a
-- returning permanent.
--
-- Both victims start TAPPED and are untapped by the first sentence of whichever card
-- steals them, so the board makes a ROUND TRIP: tapped, untapped by the spell, tapped
-- again by the trigger. `Tapped` at the end therefore cannot be state left standing,
-- and the untapped reading in the middle is what rules that out.
rayOfCommandSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
rayOfCommandSpec s registry = Spec.describe s "RayOfCommand" $ do
  Spec.it s "CR 603.7 Ray of Command whole card: the borrowed creature is TAPPED when control reverts at cleanup, and Act of Treason's is not" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    rayOfCommand <- S.printingOf s registry "Ray of Command"
    actOfTreason <- S.printingOf s registry "Act of Treason"
    let addN n printing pid g = if n <= (0 :: Int) then g else addN (n - 1) printing pid (snd (S.addCreature printing pid g))
        lands = addN 3 mountain S.alice (addN 4 island S.alice S.threePlayerGame)
        (bobPiker, g1) = S.addCreature piker S.bob lands
        (carolPiker, g2) = S.addCreature piker S.carol g1
        (rayId, g3) = S.addHandCard rayOfCommand S.alice g2
        (actId, g4) = S.addHandCard actOfTreason S.alice g3
        -- Both victims start TAPPED, so the first sentence of each card (CR 701.26b)
        -- has something to do and `Tapped` at the end cannot be state left standing.
        staged = S.tapObject carolPiker (S.tapObject bobPiker g4)
        resolveOne victim spellId g =
          S.settleSba (S.runPure (aimAtVictim victim) (S.runPure (aimAtVictim victim) g (S.cast S.alice spellId)) Stack.resolveTop)
        stolen = resolveOne carolPiker actId (resolveOne bobPiker rayId staged)
        scheduled = stolen {GameState.remaining = Seq.fromList [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]}
        afterMain = S.runPure S.identityAnswer scheduled Engine.runStep
        afterEnd = S.runPure S.identityAnswer afterMain Engine.runStep
        afterCleanup = S.runPure S.identityAnswer afterEnd Engine.runStep
        tapStateOf oid g = fmap Object.tapped (Game.lookupObject oid g)
    -- The theft really happened, and left both creatures untapped. Without these the
    -- tap assertion below could pass on a board where nothing was stolen at all.
    Spec.assertEqWith s "Ray of Command gave alice control of bob's Piker" (Projection.controllerOf bobPiker stolen) (Just S.alice)
    Spec.assertEqWith s "Act of Treason gave her carol's" (Projection.controllerOf carolPiker stolen) (Just S.alice)
    Spec.assertEqWith s "CR 701.26b and both were untapped by the first sentence of each" (fmap (\oid -> tapStateOf oid stolen) [bobPiker, carolPiker]) [Just TapState.Untapped, Just TapState.Untapped]
    -- CR 514.2 ran, so the control effects ended and control reverted.
    Spec.assertEqWith s "the cleanup step really ran" (GameState.phase afterEnd) (Phase.Ending EndingStep.Cleanup)
    Spec.assertEqWith s "CR 514.2 bob has his Piker back" (Projection.controllerOf bobPiker afterCleanup) (Just S.bob)
    Spec.assertEqWith s "and carol hers" (Projection.controllerOf carolPiker afterCleanup) (Just S.carol)
    -- The sentence under test, asserted FIRST of the three claims about the finished
    -- board: a mutation that stops the trigger firing must go red HERE rather than on
    -- the event record below, which the turn handoff would also have cleared.
    Spec.assertEqWith s "CR 603.7 Ray of Command's third sentence tapped it" (tapStateOf bobPiker afterCleanup) (Just TapState.Tapped)
    Spec.assertEqWith s "Act of Treason prints no such sentence, so carol's comes home untapped" (tapStateOf carolPiker afterCleanup) (Just TapState.Untapped)
    Spec.assertEqWith s "CR 603.7b the entry is spent, so nothing is still armed" (GameState.delayedTriggers afterCleanup) Seq.empty
    Spec.assertEqWith s "and the stack is empty" (GameState.stack afterCleanup) []
    -- CR 514.3a: the trigger got its round INSIDE this turn -- the rule's last sentence
    -- begins another cleanup step rather than passing the turn. That is also what keeps
    -- the event record below readable, since Engine.beginTurnOf clears the log at the
    -- handoff.
    Spec.assertEqWith s "CR 514.3a the turn has not handed off" (GameState.turnNumber afterCleanup) (GameState.turnNumber scheduled)
    -- The observation point fired at all.
    Spec.assertBool s (elem (GameEvent.ControlChanged (ControlChanged.MkControlChanged bobPiker S.alice S.bob)) (S.eventsOf afterCleanup)) "Engine.sampleControl minted CR 603.2's event for the reversion"
  where
    -- Narrows every target slot to one object, `aimedCast`'s filter without its cast
    -- pinning: the board holds two stealable creatures on purpose, so the engine's
    -- first offer is not the one either leg means. Filtering the OFFERED set rather
    -- than naming a Recipient keeps the answer in whatever shape the slot offered.
    aimAtVictim :: ObjectId.ObjectId -> Prompt.Prompt r -> r
    aimAtVictim oid p = case p of
      Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just oid) . Recipient.objectOf) legal) sets
      _ -> S.identityAnswer p

-- Matoya, Archon Elder {2}{U} Legendary Creature -- Human Warlock 1/4, "Whenever
-- you scry or surveil, draw a card" -- CR 603.1b's AnyOf over
-- TriggerCondition.PlayerScries and TriggerCondition.PlayerSurveils, so one card
-- proves both of CR 701.22d and CR 701.25d.
--
-- The two firing sources are DIFFERENT cards already in the pool -- Crystal
-- Ball's "{1}, {T}: Scry 2" and Curate's "Surveil 2. Draw a card." -- which is
-- what keeps the two keyword actions apart: a condition that folded them would
-- fire on the board its own half never touched, and each group below has the
-- other card nowhere near it.
--
-- HAND SIZE is the reading throughout, and always against a PAIRED board that
-- differs only in whether Matoya is on the battlefield. Curate draws a card of
-- its own, so an absolute number would prove nothing about the trigger.
matoyaTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
matoyaTriggerSpec s registry =
  let -- alice's board: four Islands, a Crystal Ball, `stock` cards on her
      -- library (top-first Goblin Piker then Bird Maiden), and Matoya only when
      -- asked for. Her hand starts EMPTY, so every hand card below was drawn.
      scryBoardFor withMatoya stock = do
        island <- S.printingOf s registry "Island"
        crystalBall <- S.printingOf s registry "Crystal Ball"
        matoya <- S.printingOf s registry "Matoya, Archon Elder"
        piker <- S.printingOf s registry "Goblin Piker"
        maiden <- S.printingOf s registry "Bird Maiden"
        let (ballId, placed) = S.addCreature crystalBall S.alice (S.landsInPlay island 4)
            watched = if withMatoya then snd (S.addCreature matoya S.alice placed) else placed
            deal g p = snd (S.addLibraryCard p S.alice g)
            stocked = List.foldl' deal watched (reverse (take stock [piker, maiden]))
        pure (ballId, stocked {GameState.priority = Just S.alice})
      -- Activate the Ball and settle: the ability resolves, the scry happens and
      -- any trigger it raised is placed and resolved in the same round. A board
      -- offering any other number of abilities activates none, which fails every
      -- assertion rather than passing one for a reason the case did not choose.
      runBall who ballId gs = case Activate.abilitiesFor ballId gs of
        [ability] ->
          let activated = S.runPure keepAll gs (Activate.activateAbility who ballId ability)
           in S.runPure keepAll activated Engine.priorityLoop
        _ -> gs
      -- Keeps every looked-at card on top, for both keyword actions. Pinned
      -- rather than derived: what this group reads is the TRIGGER, and an
      -- answerer that moved cards about would let a graveyard or a library order
      -- stand in for the draw.
      keepAll :: Prompt.Prompt r -> r
      keepAll p = case p of
        Prompt.ChooseScry _ _ looked -> ([], looked)
        Prompt.ChooseSurveil _ _ looked -> ([], looked)
        _ -> S.identityAnswer p
      -- alice's board for the surveil half: two Islands, Curate in hand, four
      -- cards on her library, Matoya only when asked for.
      surveilBoardFor withMatoya = do
        island <- S.printingOf s registry "Island"
        curate <- S.printingOf s registry "Curate"
        matoya <- S.printingOf s registry "Matoya, Archon Elder"
        piker <- S.printingOf s registry "Goblin Piker"
        maiden <- S.printingOf s registry "Bird Maiden"
        mountain <- S.printingOf s registry "Mountain"
        forest <- S.printingOf s registry "Forest"
        let watched =
              if withMatoya
                then snd (S.addCreature matoya S.alice (S.landsInPlay island 2))
                else S.landsInPlay island 2
            deal g p = snd (S.addLibraryCard p S.alice g)
            stocked = List.foldl' deal watched [forest, mountain, maiden, piker]
            (board, spellId) = S.handOne curate stocked
        pure (spellId, board {GameState.priority = Just S.alice})
      runCurate spellId gs =
        let cast = S.runPure keepAll gs (S.cast S.alice spellId)
         in S.runPure keepAll cast Engine.priorityLoop
   in Spec.describe s "MatoyaKeywordActionTrigger" $ do
        -- CR 701.22d, the whole card on the scry side. The pair differs in
        -- Matoya and in nothing else, so the one extra card in hand is the
        -- trigger and cannot be Crystal Ball's doing -- rule 701.22a moves no
        -- card out of the library at all.
        Spec.it s "CR 701.22d Crystal Ball's scry draws Matoya's card" $ do
          (ballId, board) <- scryBoardFor True 2
          (bareBall, bare) <- scryBoardFor False 2
          let after = runBall S.alice ballId board
              baseline = runBall S.alice bareBall bare
          Spec.assertEqWith s "alice's hand started empty" (S.handSize S.alice board) 0
          Spec.assertBool s (elem (GameEvent.Scried S.alice) (S.eventsOf after)) "CR 701.22d the scry recorded its event"
          Spec.assertEqWith s "Matoya drew her one card" (S.handSize S.alice after) 1
          Spec.assertEqWith s "and without Matoya the same scry draws nothing" (S.handSize S.alice baseline) 0
          Spec.assertEqWith s "the stack is empty, so the trigger really resolved" (GameState.stack after) []
        -- CR 701.22d's "even if some or all of those actions were impossible",
        -- and the case that discriminates WHERE the event is recorded: a library
        -- of exactly one card gives scry 2 nothing to decide -- top and bottom
        -- are one position -- so Resolve.scryOne asks no question and reorders
        -- nothing. The scry happened all the same, and Matoya draws that card.
        --
        -- Recording the event inside scryOne's `decided` guard passes every
        -- assertion in the case above and fails this one.
        Spec.it s "CR 701.22d a scry with nothing to decide still draws Matoya's card" $ do
          (ballId, board) <- scryBoardFor True 1
          (bareBall, bare) <- scryBoardFor False 1
          let after = runBall S.alice ballId board
              baseline = runBall S.alice bareBall bare
          Spec.assertBool s (elem (GameEvent.Scried S.alice) (S.eventsOf after)) "CR 701.22d the scry is still an event"
          Spec.assertEqWith s "Matoya drew the lone card" (S.handSize S.alice after) 1
          Spec.assertEqWith s "so alice's library is empty" (length (Game.zoneMembers Zone.Library S.alice after)) 0
          Spec.assertEqWith s "and without Matoya nothing was drawn" (S.handSize S.alice baseline) 0
          Spec.assertEqWith s "the card stayed on the library instead" (length (Game.zoneMembers Zone.Library S.alice baseline)) 1
        -- CR 603.3a / 109.5: the relation is read against the ABILITY'S
        -- CONTROLLER, so an opponent's scry is silence. The same Crystal Ball
        -- activation as the first case, moved one seat over and nothing else.
        Spec.it s "CR 109.5 bob's scry does not draw for alice's Matoya" $ do
          island <- S.printingOf s registry "Island"
          crystalBall <- S.printingOf s registry "Crystal Ball"
          matoya <- S.printingOf s registry "Matoya, Archon Elder"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (_, withMatoya) = S.addCreature matoya S.alice (S.landsInPlay island 4)
              lands = S.landsFor island S.bob 4 withMatoya
              (ballId, placed) = S.addCreature crystalBall S.bob lands
              deal who g p = snd (S.addLibraryCard p who g)
              -- ALICE's library is stocked too, and that is not decoration: an
              -- inverted relation fires Matoya here, and a draw off an empty
              -- library (CR 121.4) moves no card -- so without these two the
              -- assertion below would pass for a reason this case did not choose.
              stocked = List.foldl' (deal S.alice) (List.foldl' (deal S.bob) placed [maiden, piker]) [maiden, piker]
              board = stocked {GameState.priority = Just S.bob}
              after = runBall S.bob ballId board
          Spec.assertBool s (elem (GameEvent.Scried S.bob) (S.eventsOf after)) "bob really scried, so there was an event to match"
          Spec.assertEqWith s "alice, whose Matoya it is, drew nothing" (S.handSize S.alice after) 0
          Spec.assertEqWith s "and bob drew nothing either, his scry moving no card out of his library" (S.handSize S.bob after) 0
        -- CR 701.25d, the whole card on the surveil side. Curate draws a card
        -- itself, which is exactly why the baseline board is here: two cards in
        -- hand against one is the trigger.
        Spec.it s "CR 701.25d Curate's surveil draws Matoya's card on top of its own" $ do
          (spellId, board) <- surveilBoardFor True
          (bareSpell, bare) <- surveilBoardFor False
          let after = runCurate spellId board
              baseline = runCurate bareSpell bare
          Spec.assertBool s (elem (GameEvent.Surveiled S.alice) (S.eventsOf after)) "CR 701.25d the surveil recorded its event"
          Spec.assertBool s (notElem (GameEvent.Scried S.alice) (S.eventsOf after)) "and a surveil is not a scry"
          Spec.assertEqWith s "Curate's draw plus Matoya's" (S.handSize S.alice after) 2
          Spec.assertEqWith s "against Curate's alone" (S.handSize S.alice baseline) 1
          -- The answerer kept both looked-at cards, so nothing but Curate itself
          -- is in the graveyard: this is #1342's own requirement that a surveil
          -- which binned NOTHING still fires, and the assertion a trigger built
          -- on CR 701.25a's zone changes would fail.
          Spec.assertEqWith s "and nothing was binned, so the trigger is not counting cards moved" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

-- Feywild Trickster {2}{U} Creature -- Gnome Warlock 2/2, "Whenever you roll one
-- or more dice, create a 1/1 blue Faerie Dragon creature token with flying" --
-- the pool's producer for TriggerCondition.PlayerRollsDice (CR 706.1).
--
-- THE ROLLER is Djinni Windseer ("Flying / When this creature enters, roll a
-- d20. / 1-9 | Scry 1. / 10-19 | Scry 2. / 20 | Scry 3."), already in the pool
-- and reached by one S.entersWithTrigger. Ancient Copper Dragon, the other
-- roller, needs a whole combat and mints Treasures of its own.
--
-- THE ASSERTED QUANTITY is how many permanents NAMED "Faerie Dragon Token" a
-- seat has, never a total token count: the Windseer's own striations move
-- library cards rather than minting anything, but a count by name is what says
-- WHICH ability resolved rather than that something did.
--
-- CR 603.3 IS THE SEQUENCING. The roll happens during the resolution of the
-- Windseer's enters trigger, so the Trickster's ability triggers there and is
-- put on the stack only the next time a player would receive priority -- one
-- place/resolve cycle short of the token. `runRoll` runs the cycle twice.
--
-- TWO LEGS AT MINIMUM, in opposite directions. Leg one alone is passed
-- identically by PlayerRelation.You, by AnyPlayer, and by a condition that
-- ignores its relation; the bob leg is what tells them apart, PlayerRelation
-- Opponent included -- under that reading alice's Trickster fires on bob's roll.
-- Leg three puts a Trickster on BOTH seats, so one event is watched from two
-- seats at once and only the roller's fires.
feywildTricksterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
feywildTricksterSpec s registry =
  let faerieDragon = CardName.MkCardName (Text.pack "Faerie Dragon Token")
      -- alice's library stocked from four different printings so the Windseer's
      -- scry has something to look at and cannot deck her (CR 104.3c), the
      -- Tricksters placed on the named seats, and the Windseer entering under
      -- `roller` with its CR 603.6a trigger pending.
      rollBoard tricksters roller = do
        djinni <- S.printingOf s registry "Djinni Windseer"
        trickster <- S.printingOf s registry "Feywild Trickster"
        deck <- traverse (S.printingOf s registry) ["Goblin Piker", "Bird Maiden", "Mountain", "Forest"]
        let deal who gs printing = snd (S.addLibraryCard printing who gs)
            stocked = List.foldl' (deal S.alice) (Setup.emptyGame S.bothPlayers) deck
            libraries = List.foldl' (deal S.bob) stocked deck
            watched = List.foldl' (\gs who -> snd (S.addCreature trickster who gs)) libraries tricksters
            (_, entered) = S.entersWithTrigger djinni roller watched
        pure entered
      -- Pins the d20 to 13 -- not 1, which Replay.defaultAnswer would supply
      -- unasked, not 20, the die's own size, and not 0 -- and bottoms every
      -- look, DiceSpec.tableAnswer's reasons.
      rollAnswerer :: Prompt.Prompt r -> r
      rollAnswerer p = case p of
        Prompt.RollDie _ -> 13
        Prompt.ChooseScry _ _ looked -> (looked, [])
        _ -> S.identityAnswer p
      -- CR 603.3: place and resolve TWICE. The first cycle resolves the
      -- Windseer's enters trigger, which is where the roll happens; the
      -- Trickster's ability triggers during that resolution and reaches the
      -- stack only in the second.
      --
      -- The stack is DRAINED rather than popped once, which the third case
      -- below needs: two Tricksters put two abilities on one stack, and
      -- resolving only the top cannot tell "alice's did not trigger" from
      -- "alice's is still sitting there".
      runRoll gs =
        let drain n g =
              if n <= (0 :: Int) || null (GameState.stack g)
                then g
                else drain (n - 1) (S.runPure rollAnswerer g Stack.resolveTop)
            cycleOnce g = drain 8 (S.runPure rollAnswerer g Engine.placePendingTriggers)
         in cycleOnce (cycleOnce gs)
   in Spec.describe s "PlayerRollsDice" $ do
        -- CR 706.1: alice's own Windseer rolls, alice's Trickster fires. The
        -- paired board differs in the Trickster and in nothing else, so the
        -- token is the trigger rather than anything the Windseer did.
        Spec.it s "CR 706.1 alice's roll creates alice's Faerie Dragon" $ do
          board <- rollBoard [S.alice] S.alice
          bare <- rollBoard [] S.alice
          let after = runRoll board
              baseline = runRoll bare
          Spec.assertEqWith
            s
            "CR 706.1: one Faerie Dragon token for alice's roll"
            (S.countOnBattlefieldByName faerieDragon S.alice after)
            1
          Spec.assertEqWith
            s
            "and without the Trickster the same roll mints nothing"
            (S.countOnBattlefieldByName faerieDragon S.alice baseline)
            0
          Spec.assertBool s (elem (GameEvent.DiceRolled S.alice) (S.eventsOf after)) "CR 706.1 the roll recorded its event under the roller"
          Spec.assertEqWith s "the stack is empty, so the trigger really resolved" (GameState.stack after) []
        -- CR 109.5 / 603.3a: the relation is read against the ABILITY'S
        -- CONTROLLER. The same board one seat over -- bob's Windseer, alice's
        -- Trickster -- and this is the leg the unit exists for: a condition
        -- reading PlayerRelation.AnyPlayer, or ignoring its payload, mints a
        -- token here.
        Spec.it s "CR 109.5 bob's roll does not fire alice's Trickster" $ do
          board <- rollBoard [S.alice] S.bob
          let after = runRoll board
          Spec.assertEqWith
            s
            "CR 109.5: alice, whose Trickster it is, has no Faerie Dragon"
            (S.countOnBattlefieldByName faerieDragon S.alice after)
            0
          Spec.assertEqWith
            s
            "and bob, who rolled, has none either -- he controls no Trickster"
            (S.countOnBattlefieldByName faerieDragon S.bob after)
            0
          Spec.assertBool s (elem (GameEvent.DiceRolled S.bob) (S.eventsOf after)) "bob really rolled, so there was an event to match"
          Spec.assertBool s (notElem (GameEvent.DiceRolled S.alice) (S.eventsOf after)) "and the event names the roller, not the watcher"
        -- Both seats hold a Trickster and bob rolls, so the two readings of
        -- "you" -- the ability's controller and the roller -- fall on different
        -- seats with the same event on the log. Only bob's fires.
        Spec.it s "CR 109.5 with a Trickster on each side only the roller's fires" $ do
          board <- rollBoard [S.alice, S.bob] S.bob
          let after = runRoll board
          Spec.assertEqWith
            s
            "CR 109.5: bob rolled, so bob's Trickster made the token"
            (S.countOnBattlefieldByName faerieDragon S.bob after)
            1
          Spec.assertEqWith
            s
            "and alice's Trickster, watching the same event, made none"
            (S.countOnBattlefieldByName faerieDragon S.alice after)
            0

-- Tavern Scoundrel {1}{R} Creature -- Human Rogue 1/3, "Whenever you win a coin
-- flip, create two Treasure tokens. / {1}, {T}, Sacrifice another permanent:
-- Flip a coin." -- the pool's producer for TriggerCondition.PlayerWinsCoinFlip
-- (CR 705.2).
--
-- ONE CARD carries both halves, which is why no second producer is on the board:
-- the activated ability is the only flipper and the triggered ability is the
-- only watcher.
--
-- THE BOARD is deliberately TWO permanents a seat -- the Scoundrel and one
-- untapped Mountain -- and that count is load-bearing twice over. The Mountain
-- pays the {1}, CR 602.2b's window over CR 601.2g running before the
-- components, and it is then the ONLY candidate CR 701.21a's cost has, so
-- Prompt.ChooseSacrifices is
-- elided and no answerer stands between the card's Filter and what dies. The
-- printed word "another" is Not IsSource: under a bare IsSource the Scoundrel
-- itself would be the only candidate, so WHICH permanent left the battlefield
-- reads that Filter directly.
--
-- THE ASSERTED QUANTITY is how many permanents NAMED "Treasure Token" a seat
-- has. By name rather than by token total for feywildTricksterSpec's reason: it
-- says WHICH ability resolved rather than that something did.
--
-- THREE LEGS, with randomness pinned by CONSTANT in both directions --
-- Replay.defaultAnswer would supply Heads to both prompts unasked, and so a win,
-- which is exactly the leg a run that asked nothing could fake.
--
--   * WON (call Heads, face Heads): two Treasures.
--   * LOST (call Heads, face Tails): none. Not decoration -- it is the only leg
--     separating "triggers on a WIN" from "triggers on any flip at all", and the
--     flip really happened, which the log assertion beside it pins.
--   * BOB'S WIN with a Scoundrel on BOTH seats: one event, two watchers, and
--     only the flipper's fires. AnyPlayer, Opponent and a condition ignoring its
--     relation each differ from You here.
--
-- CR 603.3 IS THE SEQUENCING. The flip happens during the resolution of the
-- activated ability, so the trigger reaches the stack only the next time a player
-- would receive priority -- one place/resolve cycle short of the tokens.
-- `runFlip` runs the cycle twice.
tavernScoundrelSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tavernScoundrelSpec s registry =
  let treasure = CardName.MkCardName (Text.pack "Treasure Token")
      mountainName = CardName.MkCardName (Text.pack "Mountain")
      -- A Scoundrel and one Mountain for each named seat, and nothing else.
      flipBoard seats = do
        scoundrel <- S.printingOf s registry "Tavern Scoundrel"
        mountain <- S.printingOf s registry "Mountain"
        let step (ids, gs) who =
              let (oid, withCreature) = S.addCreature scoundrel who gs
               in (ids <> [oid], S.landsFor mountain who 1 withCreature)
        pure (List.foldl' step ([], Setup.emptyGame S.bothPlayers) seats)
      -- Pins CR 705.2's two questions by constant, never by anything derived from
      -- the prompt, so the engine cannot repair either answer after a mutation.
      flipAnswer :: CoinFace.CoinFace -> CoinFace.CoinFace -> Prompt.Prompt r -> r
      flipAnswer face called p = case p of
        Prompt.FlipCoin -> face
        Prompt.CallCoin {} -> called
        _ -> S.identityAnswer p
      runFlip face called who scoundrelId gs =
        let drain n g =
              if n <= (0 :: Int) || null (GameState.stack g)
                then g
                else drain (n - 1) (S.runPure (flipAnswer face called) g Stack.resolveTop)
            cycleOnce g = drain 8 (S.runPure (flipAnswer face called) g Engine.placePendingTriggers)
         in case Activate.abilitiesFor scoundrelId gs of
              [ability] -> Right (cycleOnce (cycleOnce (S.runPure (flipAnswer face called) gs (Activate.activateAbility who scoundrelId ability))))
              other -> Left (length other)
      oneAbility n = "expected exactly one activated ability, got " <> show n
   in Spec.describe s "PlayerWinsCoinFlip" $ do
        -- CR 705.2: the call matched the face, so alice won her own flip and her
        -- own Scoundrel fires.
        Spec.it s "CR 705.2 a won flip creates two Treasures" $ do
          (ids, board) <- flipBoard [S.alice]
          case ids of
            [scoundrelId] -> case runFlip CoinFace.Heads CoinFace.Heads S.alice scoundrelId board of
              Left n -> Spec.assertFailure s (oneAbility n)
              Right after -> do
                Spec.assertEqWith
                  s
                  "CR 705.2: two Treasure tokens for the won flip"
                  (S.countOnBattlefieldByName treasure S.alice after)
                  2
                -- The printed "another", read off the board rather than off the
                -- Filter: the land paid CR 701.21a's cost and the Scoundrel did
                -- not.
                Spec.assertBool s (S.onBattlefield scoundrelId after) "CR 701.21a the Scoundrel did not sacrifice itself (another)"
                Spec.assertEqWith s "and the Mountain it sacrificed instead is gone" (S.countOnBattlefieldByName mountainName S.alice after) 0
                Spec.assertEqWith s "the stack is empty, so the trigger really resolved" (GameState.stack after) []
            _ -> Spec.assertFailure s "expected exactly one Scoundrel"
        -- CR 705.2's other half, on the SAME board: the call did not match, so the
        -- flip was lost and nothing fires. A condition matching the flip rather
        -- than the win mints two Treasures here.
        Spec.it s "CR 705.2 a lost flip creates none" $ do
          (ids, board) <- flipBoard [S.alice]
          case ids of
            [scoundrelId] -> case runFlip CoinFace.Tails CoinFace.Heads S.alice scoundrelId board of
              Left n -> Spec.assertFailure s (oneAbility n)
              Right after -> do
                Spec.assertEqWith
                  s
                  "CR 705.2: a lost flip mints no Treasure"
                  (S.countOnBattlefieldByName treasure S.alice after)
                  0
                -- The flip HAPPENED, which keeps the zero above from passing for
                -- an ability that never activated at all.
                Spec.assertBool
                  s
                  (elem (GameEvent.CoinFlipped CoinFlipped.MkCoinFlipped {CoinFlipped.flipper = S.alice, CoinFlipped.won = Just False}) (S.eventsOf after))
                  "CR 705.1 the flip is recorded even though CR 705.2 lost it"
            _ -> Spec.assertFailure s "expected exactly one Scoundrel"
        -- CR 109.5 / 603.3a: the relation is read against the ABILITY'S
        -- CONTROLLER. Both seats hold a Scoundrel and bob flips, so the two
        -- readings of "you" fall on different seats over one event.
        Spec.it s "CR 109.5 with a Scoundrel on each side only the flipper's fires" $ do
          (ids, board) <- flipBoard [S.alice, S.bob]
          case ids of
            [_, bobsScoundrelId] -> case runFlip CoinFace.Heads CoinFace.Heads S.bob bobsScoundrelId board of
              Left n -> Spec.assertFailure s (oneAbility n)
              Right after -> do
                Spec.assertEqWith
                  s
                  "CR 705.2: bob won the flip, so bob's Scoundrel made the Treasures"
                  (S.countOnBattlefieldByName treasure S.bob after)
                  2
                Spec.assertEqWith
                  s
                  "and alice's Scoundrel, watching the same event, made none"
                  (S.countOnBattlefieldByName treasure S.alice after)
                  0
            _ -> Spec.assertFailure s "expected a Scoundrel on each side"

-- Aloe Alchemist {1}{G} Creature -- Plant Warlock 3/2, "Trample; When this card
-- becomes plotted, target creature gets +3/+2 and gains trample until end of
-- turn; Plot {1}{G}" -- the pool's producer for TriggerCondition
-- SelfBecomesPlotted (CR 702.170a, CR 702.170c).
--
-- The one condition in the pool whose bearer is in EXILE when it fires: CR
-- 702.170b's special action exiles the card as it becomes plotted, so
-- Event.zonesTriggeredFrom has to answer Zone.Exile for it and Event.eventTriggers
-- finds the bearer through its standing exile scan.
--
-- Distinct power/toughness on the two creatures (Goblin Piker 2/1, Bird Maiden
-- 1/2) so +3/+2 cannot be read off the wrong one, and the Maiden is bob's, so a
-- payload that hit every creature is visible.
aloeAlchemistSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aloeAlchemistSpec s registry =
  let aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimAt oid p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just oid) . Recipient.objectOf) sets
        _ -> S.identityAnswer p
      sorcerySpeed gs =
        gs
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
   in Spec.describe s "AloeAlchemistPlotTrigger" $ do
        -- The whole card: alice takes CR 116.2k's special action, the card lands
        -- in exile as a plotted card, and the ability printed on it fires from
        -- there.
        Spec.it s "CR 702.170a plotting Aloe Alchemist pumps the targeted creature" $ do
          forest <- S.printingOf s registry "Forest"
          aloe <- S.printingOf s registry "Aloe Alchemist"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (pikerId, g1) = S.addCreature piker S.alice (S.landsInPlay forest 2)
              (maidenId, g2) = S.addCreature maiden S.bob g1
              (aloeId, g3) = S.addHandCard aloe S.alice g2
              gs = sorcerySpeed g3
              plotted = S.runPure (aimAt pikerId) gs (Plot.plot S.manaPerformer S.alice aloeId)
              after = S.runPure (aimAt pikerId) plotted Engine.priorityLoop
          Spec.assertEqWith s "the Piker started 2/1" (S.powerToughnessOf pikerId gs) (Just (2, 1))
          Spec.assertBool s (any isPlotted (S.eventsOf after)) "CR 702.170a the plot recorded its event"
          Spec.assertEqWith s "CR 702.170a the targeted Piker is 5/3" (S.powerToughnessOf pikerId after) (Just (5, 3))
          Spec.assertEqWith s "bob's untargeted Maiden is untouched" (S.powerToughnessOf maidenId after) (Just (1, 2))
          Spec.assertEqWith s "the card is in exile" (length (GameState.exile after)) 1
          Spec.assertEqWith s "and the stack is empty, so the trigger resolved" (GameState.stack after) []
        -- The DISCRIMINATING negative: a plot event that names a DIFFERENT card.
        -- Aloe Alchemist sits in exile the whole time -- so
        -- Event.eventTriggers' exile scan really offers it, and the only thing
        -- that keeps it quiet is the id on the event.
        --
        -- Djinn of Fool's Fall {3}{U} is the pool's other plot card and prints no
        -- such trigger, which is what makes it the control.
        Spec.it s "CR 702.170a plotting another card does not fire an exiled Aloe Alchemist" $ do
          island <- S.printingOf s registry "Island"
          aloe <- S.printingOf s registry "Aloe Alchemist"
          djinn <- S.printingOf s registry "Djinn of Fool's Fall"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, g1) = S.addCreature piker S.alice (S.landsInPlay island 4)
              (_, g2) = S.addExiledCard aloe S.alice g1
              (djinnId, g3) = S.addHandCard djinn S.alice g2
              gs = sorcerySpeed g3
              plotted = S.runPure (aimAt pikerId) gs (Plot.plot S.manaPerformer S.alice djinnId)
              after = S.runPure (aimAt pikerId) plotted Engine.priorityLoop
          Spec.assertBool s (any isPlotted (S.eventsOf after)) "the Djinn really became plotted, so there was an event to match"
          Spec.assertEqWith s "both cards are in exile, so the Alchemist was there to be offered" (length (GameState.exile after)) 2
          Spec.assertEqWith s "and the Piker is still 2/1" (S.powerToughnessOf pikerId after) (Just (2, 1))
          Spec.assertEqWith s "with nothing waiting on the stack" (GameState.stack after) []

-- Whether an event is CR 702.170a's plot, whichever card it names. The id is
-- CR 400.7's exile incarnation, which no fixture can predict.
isPlotted :: GameEvent.GameEvent -> Bool
isPlotted event = case event of
  GameEvent.Plotted _ -> True
  _ -> False

-- Wildgrowth Walker {1}{G} Creature -- Elemental 1/3, "Whenever a creature you
-- control explores, put a +1/+1 counter on this creature and you gain 3 life" --
-- the pool's producer for TriggerCondition.PermanentExplores (CR 701.44b).
--
-- Merfolk Branchwalker {1}{G} 2/1, "When this creature enters, it explores", is
-- the firing source and was already in the pool. It takes a +1/+1 counter of its
-- own on the nonland branch, so BOTH creatures are read in every case: a payload
-- that grew the explorer rather than the watcher is otherwise invisible.
--
-- Three seats are not needed and two are: what the Filter says is "you control",
-- and the paired board moves the Branchwalker from alice to bob and changes
-- nothing else.
wildgrowthWalkerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wildgrowthWalkerSpec s registry =
  let -- alice always controls the Walker; `explorer` controls the Branchwalker
      -- and owns the stocked library, since CR 701.44a reveals off the
      -- exploring permanent's controller's library.
      board explorer deck = do
        walker <- S.printingOf s registry "Wildgrowth Walker"
        branchwalker <- S.printingOf s registry "Merfolk Branchwalker"
        printings <- mapM (S.printingOf s registry) deck
        let (walkerId, g1) = S.addCreature walker S.alice (Setup.emptyGame S.bothPlayers)
            deal g p = snd (S.addLibraryCard p explorer g)
            stocked = List.foldl' deal g1 (reverse printings)
            (branchId, g2) = S.entersWithTrigger branchwalker explorer stocked
        pure (walkerId, branchId, g2)
      -- Bins the revealed card, so the explore's own zone change happens too --
      -- which is what keeps this trigger from being read off a graveyard
      -- arrival by accident.
      binIt :: Prompt.Prompt r -> r
      binIt p = case p of
        Prompt.ChooseExplore {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      settle gs = S.runPure binIt gs Engine.priorityLoop
   in Spec.describe s "WildgrowthWalkerExploreTrigger" $ do
        -- The whole card. The Branchwalker's own +1/+1 counter and the Walker's
        -- are read separately, so "a counter went somewhere" cannot pass for
        -- "the counter went on the Walker".
        Spec.it s "CR 701.44b a creature alice controls exploring grows her Walker" $ do
          (walkerId, branchId, gs) <- board S.alice ["Goblin Piker", "Bird Maiden"]
          let after = settle gs
          Spec.assertEqWith s "the Walker started 1/3" (S.powerToughnessOf walkerId gs) (Just (1, 3))
          Spec.assertBool s (elem (GameEvent.Explored branchId) (S.eventsOf after)) "CR 701.44b the explore recorded its event"
          Spec.assertEqWith s "the Walker took its +1/+1 counter" (S.powerToughnessOf walkerId after) (Just (2, 4))
          Spec.assertEqWith s "and the Branchwalker took its own, which is a different counter" (S.powerToughnessOf branchId after) (Just (3, 2))
          Spec.assertEqWith s "alice gained 3" (S.lifeOf S.alice after) (Just 23)
          Spec.assertEqWith s "bob gained none" (S.lifeOf S.bob after) (Just 20)
          Spec.assertEqWith s "the stack is empty, so the trigger resolved" (GameState.stack after) []
        -- The Filter's own half, CR 109.5's "you control": the same board with
        -- the Branchwalker one seat over. It still explores -- its counter says
        -- so -- and alice's Walker stays put.
        Spec.it s "CR 109.5 bob's creature exploring does not grow alice's Walker" $ do
          (walkerId, branchId, gs) <- board S.bob ["Goblin Piker", "Bird Maiden"]
          let after = settle gs
          Spec.assertBool s (elem (GameEvent.Explored branchId) (S.eventsOf after)) "bob's Branchwalker really explored"
          Spec.assertEqWith s "so it took its own counter" (S.powerToughnessOf branchId after) (Just (3, 2))
          Spec.assertEqWith s "but alice's Walker is still 1/3" (S.powerToughnessOf walkerId after) (Just (1, 3))
          Spec.assertEqWith s "and alice gained no life" (S.lifeOf S.alice after) (Just 20)
        -- CR 701.44b's "even if some or all of those actions were impossible":
        -- an empty library reveals nothing, so nothing is a land card and
        -- nothing is binned. The permanent explored all the same.
        Spec.it s "CR 701.44b an explore off an empty library still grows the Walker" $ do
          (walkerId, branchId, gs) <- board S.alice []
          let after = settle gs
          Spec.assertBool s (elem (GameEvent.Explored branchId) (S.eventsOf after)) "the explore is still an event"
          Spec.assertEqWith s "the Walker grew" (S.powerToughnessOf walkerId after) (Just (2, 4))
          Spec.assertEqWith s "alice gained 3" (S.lifeOf S.alice after) (Just 23)
          Spec.assertEqWith s "and nothing was binned" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

-- CR 701.3a's attachment event, read from the HOST's side by
-- TriggerCondition.SelfBecomesAttachedBy.
--
-- Bramble Elemental, {3}{G}{G} Creature -- Elemental 4/4, "Whenever an Aura
-- becomes attached to this creature, create two 1/1 green Saproling creature
-- tokens."
--
-- TWO emit sites, and a leg apiece, because the rules reach the same trigger by
-- two roads: CR 608.3c puts a resolving Aura spell onto the battlefield already
-- attached (Pawl.Engine.Event's zone-change funnel writes the seed), and CR
-- 701.3a moves a permanent that is already there (Event.attach). Deleting either
-- emit leaves the other leg green, which is why neither stands alone.
--
-- NOTHING here goes through Pawl.Support.attach, which writes Object.attachedTo
-- directly and records no event: a leg built on it would read zero before and
-- zero after and could not tell this engine from one that had never heard of the
-- rule.
-- Tokens only, and by SUBTYPE: the Elemental's own board is full of creatures,
-- and counting them would drift the moment a fixture changed.
saprolingsOf :: PlayerId.PlayerId -> GameState.GameState -> Int
saprolingsOf pid gs =
  length
    ( filter
        (\oid -> Set.member Subtype.Saproling (Projection.subtypesOf oid gs) && Projection.controllerOf oid gs == Just pid)
        (S.tokensOf gs)
    )

-- Answers every target slot with the offered recipients that name one object.
--
-- FILTERS the offered set rather than building a Recipient, AuraSpec's
-- aimAtOffered posture: Pacifism's enchant slot pools creatures, and a
-- hand-built recipient of another shape is dropped by CR 608.2b's re-read at
-- resolution with no error to see.
aimAtOffered :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtOffered oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- Both of an attach-moving ability's prompts: its target slot, and CR 701.3a's
-- destination choice.
moveOnto :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
moveOnto subject dest p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just subject) . Recipient.objectOf) . snd) sets
  Prompt.ChooseAttachment {} -> dest
  _ -> S.identityAnswer p

-- The CR 117.5 boundary scans for triggers, then the one it placed resolves.
-- Narrower than the priority loop, which would sweep the rest of the board too.
fireTriggers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
fireTriggers answer gs =
  let placed = S.runPure answer gs Engine.settleForPriority
   in S.runPure answer placed Stack.resolveTop

-- What is attached to `host`, by whichever tag the attaching permanent's own
-- rules text names it -- Pawl.AuraSpec's attachedTo.
attachmentsOn :: ObjectId.ObjectId -> GameState.GameState -> [ObjectId.ObjectId]
attachmentsOn host gs =
  filter
    (\oid -> (Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf) == Just host)
    (Set.toList (GameState.battlefield gs))

firstActivatedOf :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
firstActivatedOf printing = case Face.activatedAbilities (S.combinedFace printing) of
  ability : _ -> Just ability
  [] -> Nothing

brambleElementalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
brambleElementalSpec s registry =
  Spec.describe s "CR 701.3a a trigger on becoming attached" $ do
    -- CR 608.3c: the Aura spell resolves and is put onto the battlefield
    -- attached to what it targeted. The attachment is written inside the zone
    -- change, on the CR 400.7 incarnation, so this leg is the entry emit and
    -- reaches Event.attach not at all.
    Spec.it s "CR 608.3c whole card: casting Pacifism on the Elemental creates two Saprolings" $ do
      plains <- S.printingOf s registry "Plains"
      bramble <- S.printingOf s registry "Bramble Elemental"
      pacifism <- S.printingOf s registry "Pacifism"
      let (brambleId, board) = S.addCreature bramble S.alice (S.landsInPlay plains 3)
          (armed, auraSpell) = S.handOne pacifism board
          cast = S.runPure (aimAtOffered brambleId) armed (S.cast S.alice auraSpell)
          entered = S.runPure (aimAtOffered brambleId) cast Stack.resolveTop
          after = fireTriggers (aimAtOffered brambleId) entered
      Spec.assertEqWith s "CR 603.2 two Saprolings once the trigger resolves" (saprolingsOf S.alice after) 2
      Spec.assertEqWith s "and none on the board the Aura was cast from" (saprolingsOf S.alice armed) 0
      -- The precondition the count rests on: an Aura that never landed would
      -- make zero the right answer for the wrong reason.
      Spec.assertEqWith s "the Aura is attached to the Elemental" (length (attachmentsOn brambleId entered)) 1
    -- CR 701.3a's other road: an Aura already on the battlefield MOVES.
    -- Crown of the Ages, "{4}, {T}: Attach target Aura attached to a creature
    -- to another creature" -- Unholy Strength enters on a Piker, where
    -- nothing triggers, and is then moved onto the Elemental.
    --
    -- The Piker half is the pair's other board, one object different: same
    -- Aura, same cast, same resolution, a host that is not the watcher.
    Spec.it s "CR 701.3a whole card: Crown of the Ages moving an Aura onto the Elemental creates two Saprolings" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      bramble <- S.printingOf s registry "Bramble Elemental"
      unholyStrength <- S.printingOf s registry "Unholy Strength"
      crown <- S.printingOf s registry "Crown of the Ages"
      let (pikerId, base1) = S.addCreature piker S.alice (S.landsInPlay swamp 7)
          -- A DECOY creature, so Crown's "another creature" offers two
          -- destinations and Attach.chooseHost really asks rather than
          -- eliding at a single candidate.
          (_, base2) = S.addCreature piker S.alice base1
          (brambleId, base3) = S.addCreature bramble S.alice base2
          (armed, auraSpell) = S.handOne unholyStrength base3
          onPiker = S.runPure (aimAtOffered pikerId) armed (S.cast S.alice auraSpell >> Stack.resolveTop)
          settledOnPiker = fireTriggers (aimAtOffered pikerId) onPiker
      case attachmentsOn pikerId settledOnPiker of
        [] -> Spec.assertFailure s "Unholy Strength should have entered attached to the Piker"
        auraId : _ -> do
          let (withCrown, crownSpell) = S.handOne crown settledOnPiker
              resolved = S.runPure S.identityAnswer withCrown (S.cast S.alice crownSpell >> Stack.resolveTop)
              crownIds = filter (\oid -> Game.cardOf oid resolved == Just (Printing.card crown)) (Set.toList (GameState.battlefield resolved))
          case (crownIds, firstActivatedOf crown) of
            (crownId : _, Just move) -> do
              let ready = resolved {GameState.priority = Just S.alice}
                  activated = S.runPure (moveOnto auraId brambleId) ready (Activate.activateAbility S.alice crownId move)
                  moved = S.runPure (moveOnto auraId brambleId) activated Stack.resolveTop
                  after = fireTriggers (moveOnto auraId brambleId) moved
              Spec.assertEqWith s "CR 603.2 two Saprolings once the move's trigger resolves" (saprolingsOf S.alice after) 2
              -- The other board, one object different: the same Aura entering
              -- on a creature that is not the watcher fires nothing.
              Spec.assertEqWith s "and none while the Aura sat on the Piker" (saprolingsOf S.alice settledOnPiker) 0
              Spec.assertEqWith s "the Aura really moved onto the Elemental" (attachmentsOn brambleId moved) [auraId]
            _ -> Spec.assertFailure s "Crown of the Ages should have resolved onto the battlefield with one activated ability"
    -- "An AURA", the word the Filter carries, on the SAME emit site as the
    -- leg above: Bonesplitter's equip attaches an Equipment to the Elemental
    -- through Event.attach, and nothing happens.
    --
    -- Discriminating only because that leg is the positive on this path --
    -- alone it would pass against an engine with no event at all.
    Spec.it s "CR 702.6a equipping the Elemental with Bonesplitter creates nothing" $ do
      plains <- S.printingOf s registry "Plains"
      bramble <- S.printingOf s registry "Bramble Elemental"
      bonesplitter <- S.printingOf s registry "Bonesplitter"
      let (brambleId, base1) = S.addCreature bramble S.alice (S.landsInPlay plains 3)
          (bladeId, base2) = S.addCreature bonesplitter S.alice base1
          ready = base2 {GameState.priority = Just S.alice}
      -- From the PROJECTION, not the face: Bonesplitter declares CR 702.6a's
      -- keyword and prints no activated ability, so the equip is minted by
      -- Pawl.Engine.Keyword and appended by Pawl.Engine.Projection.
      case Projection.abilitiesOf bladeId ready of
        [] -> Spec.assertFailure s "Bonesplitter should offer rule 702.6a's minted equip ability"
        equip : _ -> do
          let activated = S.runPure (aimAtOffered brambleId) ready (Activate.activateAbility S.alice bladeId equip)
              equipped = S.runPure (aimAtOffered brambleId) activated Stack.resolveTop
              after = fireTriggers (aimAtOffered brambleId) equipped
          Spec.assertEqWith s "no Saproling: an Equipment is not an Aura" (saprolingsOf S.alice after) 0
          -- Without this the zero says nothing: an equip that never happened
          -- would read the same.
          Spec.assertEqWith s "though the Equipment really did become attached" (attachmentsOn brambleId equipped) [bladeId]
          Spec.assertEqWith s "which CR 301.5f's +2/+0 confirms" (S.powerToughnessOf brambleId equipped) (Just (6, 4))

-- CR 613.1f layer 6, the TRIGGERED half of the grant: Sixth Sense ({G}
-- Enchantment -- Aura, "Enchant creature / Enchanted creature has 'Whenever this
-- creature deals combat damage to a player, you may draw a card.'", checked
-- against Scryfall on 2026-08-20) is the cheapest printing whose whole text box
-- is one quoted triggered ability, so nothing but the grant is under test.
--
-- Presence of Gond (Pawl.ActivateSpec) is the activated half of the same
-- Modification arm. What this group adds is the other side of the fold: a
-- granted ability has to be found by the CR 603.2 scan, not only by the
-- projection, and Pawl.Engine.Event.eventTriggers reads
-- ProjectedCharacteristics.triggeredAbilities to do it.
--
-- Three seats, and the two that matter are DIFFERENT players: alice controls the
-- enchanted attacker, carol controls the Aura, bob is the defending player. CR
-- 113.7 makes the enchanted creature the granted ability's source and CR 603.3a
-- makes its controller the trigger's controller, so the "you" that draws is
-- alice. A granter-anchored reading would draw for carol, and the two hands are
-- what tell those readings apart -- one seat could not.
sixthSenseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sixthSenseSpec s registry = Spec.describe s "CR 613.1f a granted triggered ability" $ do
  -- The gameplay-level proof, and the pair's positive half.
  Spec.it s "CR 603.3a whole card: the enchanted creature connects and ITS controller draws" $ do
    ps <- traverse (S.printingOf s registry) ["Goblin Piker", "Sixth Sense", "Mountain", "Island"]
    case ps of
      [piker, sense, mountain, island] -> case sixthSenseBoard piker sense mountain island True of
        ([attackerId], _, gs) -> do
          let after = S.runCombat sixthSenseAnswer gs
          Spec.assertEqWith s "alice drew the one card her library held" (handNames S.alice after) ["Mountain"]
          Spec.assertEqWith s "and the Aura's controller drew nothing" (handNames S.carol after) []
          Spec.assertEqWith s "CR 510.1b the Piker's 2 damage reached bob" (S.lifeOf S.bob after) (Just 18)
          Spec.assertBool s (S.onBattlefield attackerId after) "the unblocked attacker survived combat"
        _ -> Spec.assertFailure s "fixture should give alice exactly one attacker"
      _ -> Spec.assertFailure s "four printings"
  -- The pair's other half: the same board, the same combat, the Aura sitting on
  -- carol's battlefield unattached. Nothing else differs, so a draw here would
  -- mean the trigger came from somewhere other than the grant.
  Spec.it s "CR 303.4m an unattached Sixth Sense grants nothing and nobody draws" $ do
    ps <- traverse (S.printingOf s registry) ["Goblin Piker", "Sixth Sense", "Mountain", "Island"]
    case ps of
      [piker, sense, mountain, island] -> case sixthSenseBoard piker sense mountain island False of
        ([_], _, gs) -> do
          let after = S.runCombat sixthSenseAnswer gs
          Spec.assertEqWith s "alice's hand is still empty" (handNames S.alice after) []
          Spec.assertEqWith s "and so is carol's" (handNames S.carol after) []
          Spec.assertEqWith s "the same combat still happened" (S.lifeOf S.bob after) (Just 18)
        _ -> Spec.assertFailure s "fixture should give alice exactly one attacker"
      _ -> Spec.assertFailure s "four printings"
  -- Where the ability ends up, CR 113.7: on the RECEIVER, and not on the Aura
  -- that prints the words.
  Spec.it s "CR 113.7 the enchanted creature has the trigger and the Aura does not" $ do
    ps <- traverse (S.printingOf s registry) ["Goblin Piker", "Sixth Sense", "Mountain", "Island"]
    case ps of
      [piker, sense, mountain, island] -> case (sixthSenseBoard piker sense mountain island True, sixthSenseBoard piker sense mountain island False) of
        (([attackerId], senseId, enchanted), ([bareId], _, unenchanted)) -> do
          Spec.assertEqWith s "one triggered ability on the enchanted creature" (length (Projection.triggeredAbilitiesOf attackerId enchanted)) 1
          Spec.assertEqWith s "the Piker prints none of its own" (length (Projection.triggeredAbilitiesOf bareId unenchanted)) 0
          Spec.assertEqWith s "and the granter does not have what it grants" (length (Projection.triggeredAbilitiesOf senseId enchanted)) 0
        _ -> Spec.assertFailure s "fixture should give alice exactly one attacker"
      _ -> Spec.assertFailure s "four printings"

-- alice attacks with one settled Goblin Piker, carol holds the Aura, bob defends
-- with nothing. Both libraries hold exactly one card, and DIFFERENT cards, so
-- "who drew" is answerable by name; stocking carol's as well keeps CR 104.3c out
-- of the negative reading, where a wrongly-controlled trigger would otherwise
-- deck her instead of drawing.
sixthSenseBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
sixthSenseBoard piker sense mountain island attached =
  let (base, mine, _, _) = S.threePlayerCombat [piker] [] []
      stocked = snd (S.addLibraryCard island S.carol (snd (S.addLibraryCard mountain S.alice base)))
      (senseId, withAura) = S.addCreature sense S.carol stocked
      board = case mine of
        [attackerId] | attached -> S.attach senseId attackerId withAura
        _ -> withAura
   in (mine, senseId, board)

-- Attacks bob with everything and takes every "may". CR 507.1 leaves the
-- defending player to the active player's choice on a three-seat board, so it
-- has to be pinned.
sixthSenseAnswer :: Prompt.Prompt r -> r
sixthSenseAnswer p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.attackTo S.bob p

-- The names in a player's hand, sorted. Names rather than a count, because the
-- two libraries hold different cards and which one moved is the question.
handNames :: PlayerId.PlayerId -> GameState.GameState -> [String]
handNames pid gs =
  List.sort
    [ Text.unpack (CardName.unwrap (S.soleFaceName oid gs))
    | oid <- Game.zoneMembers Zone.Hand pid gs
    ]

-- CR 701.26a's "becomes tapped", over a whole card. Betrayal ({U} Enchantment --
-- Aura, "Enchant creature an opponent controls / Whenever enchanted creature
-- becomes tapped, you draw a card.", checked against Scryfall on 2026-08-24) is
-- the cheapest printing whose whole text box is that one trigger, so nothing but
-- the condition and the event under it is on trial.
--
-- It is the FUNNEL that this group exists to prove. Before it there was no tap
-- funnel at all: five sites wrote Object.tapped directly, and a GameEvent arm
-- with nothing appending it would have been inert. Two of the five must stay
-- direct, which CR 603.2e states outright -- a permanent that ENTERS tapped never
-- transitioned -- and the enters-tapped leg below is what pins that.
--
-- THREE SEATS, and the two that matter are different players: alice controls the
-- enchanted attacker, carol controls the Aura, bob is the defending player. CR
-- 109.5 makes the trigger's "you" the controller of the object when it triggered,
-- and that object is the AURA -- so carol draws, not alice. Two seats would put
-- the Aura's controller and the defending player on one seat and could not tell a
-- defender-anchored reading apart from CR 109.5's.
--
-- TWO attackers on alice's side, only one of them enchanted, and this is what
-- makes the "enchanted" half of the condition falsifiable: both tap in the same
-- CR 508.1f action, so a matcher that ignored Object.attachedTo would draw carol
-- two cards rather than one. Carol's library holds three Islands so that one draw,
-- two draws and a CR 104.3c deck-out are three distinguishable outcomes.
betrayalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
betrayalSpec s registry = Spec.describe s "CR 701.26a a becomes-tapped trigger" $ do
  -- The gameplay-level proof, and the leg that pins CR 508.1f's route in
  -- particular: "attacking simply causes creatures to become tapped", so the
  -- declaration has to reach the same funnel a cost or an effect does.
  Spec.it s "CR 508.1f whole card: declaring the enchanted creature as an attacker draws the AURA's controller a card" $ do
    board <- betrayalBoard s registry True
    case board of
      ([enchanted, bare], _, gs) -> do
        let after = S.runCombat (S.attackTo S.bob) gs
        Spec.assertEqWith s "CR 109.5 carol, who controls the Aura, drew exactly one card" (handNames S.carol after) ["Island"]
        Spec.assertEqWith s "and alice, who controls the tapped creature, drew none" (handNames S.alice after) []
        -- The preconditions the count above rests on, AFTER it so neither can
        -- absorb a mutation aimed at the funnel or the matcher.
        Spec.assertEqWith s "CR 508.1f both attackers really became tapped" (fmap (`tapStatusOf` after) [enchanted, bare]) [Just TapState.Tapped, Just TapState.Tapped]
        Spec.assertEqWith s "so the ONE draw is the attachment link's doing and not the tap's" (S.tappedCount S.alice after) 2
      _ -> Spec.assertFailure s "fixture should give alice exactly two attackers"
  -- The pair's other half, and the leg that pins WHICH permanent the condition is
  -- about: the same board, and the tap goes to the creature the Aura does NOT
  -- enchant. One thing differs, and it is the only thing the matcher reads.
  Spec.it s "CR 303.4b tapping a creature the Aura does not enchant draws nothing" $ do
    board <- betrayalBoard s registry True
    case board of
      ([enchanted, bare], _, gs) -> do
        let after = settleAndResolve (S.runPure S.identityAnswer gs (Event.tap bare))
        Spec.assertEqWith s "CR 303.4b carol's hand is still empty" (handNames S.carol after) []
        Spec.assertEqWith s "though that creature really did become tapped" (tapStatusOf bare after) (Just TapState.Tapped)
        Spec.assertEqWith s "and the enchanted one, untouched, did not" (tapStatusOf enchanted after) (Just TapState.Untapped)
      _ -> Spec.assertFailure s "fixture should give alice exactly two attackers"
  -- CR 704.5m, and the reason an unattached Aura is NOT the negative to build
  -- here: it never gets to watch anything, because the state-based action buries
  -- it before the combat starts. Asserted rather than assumed, so the empty hand
  -- below is not read as evidence about the matcher.
  Spec.it s "CR 704.5m an unattached Betrayal is buried before it can watch a tap" $ do
    board <- betrayalBoard s registry False
    case board of
      ([enchanted, _], aura, gs) -> do
        let after = S.runCombat (S.attackTo S.bob) gs
        Spec.assertBool s (not (S.onBattlefield aura after)) "CR 704.5m the Aura attached to nothing is off the battlefield"
        Spec.assertEqWith s "so carol drew nothing" (handNames S.carol after) []
        Spec.assertEqWith s "though the same creature still became tapped" (tapStatusOf enchanted after) (Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should give alice exactly two attackers"
  -- CR 701.26a's second sentence, "only untapped permanents can be tapped", and
  -- the reason the funnel needs a guard it did not need as a bare assignment: the
  -- write is idempotent and the EVENT is not. The divergence is a COUNT rather
  -- than a time, so the exact hand is asserted -- both readings agree on "more
  -- than nothing".
  Spec.it s "CR 701.26a tapping the enchanted creature a second time is no event and draws nothing more" $ do
    board <- betrayalBoard s registry True
    case board of
      ([enchanted, _], _, gs) -> do
        let once = settleAndResolve (S.runPure S.identityAnswer gs (Event.tap enchanted))
            twice = settleAndResolve (S.runPure S.identityAnswer gs (Event.tap enchanted >> Event.tap enchanted))
        Spec.assertEqWith s "CR 701.26a the second tap drew nothing: one card, not two" (handNames S.carol twice) ["Island"]
        Spec.assertEqWith s "which is what one tap already drew" (handNames S.carol once) ["Island"]
        Spec.assertEqWith s "and the creature is tapped either way" (fmap (tapStatusOf enchanted) [once, twice]) [Just TapState.Tapped, Just TapState.Tapped]
      _ -> Spec.assertFailure s "fixture should give alice exactly two attackers"
  -- CR 603.2e: "An ability that triggers when a permanent 'becomes tapped' ...
  -- doesn't trigger if the permanent enters the battlefield in that state." The
  -- pair is the same board under the two writes -- Event.enterTapped, which
  -- Pawl.Engine.Resolve.putTapped mirrors, against Event.tap -- so the only thing
  -- that differs is which one the engine used.
  Spec.it s "CR 603.2e a permanent stamped tapped as it enters fires nothing" $ do
    board <- betrayalBoard s registry True
    case board of
      ([enchanted, _], _, gs) -> do
        let entered = settleAndResolve (S.runPure S.identityAnswer gs (Event.enterTapped enchanted))
            tapped = settleAndResolve (S.runPure S.identityAnswer gs (Event.tap enchanted))
        Spec.assertEqWith s "CR 603.2e carol drew nothing off the entering stamp" (handNames S.carol entered) []
        Spec.assertEqWith s "though the very same tap through the funnel draws her a card" (handNames S.carol tapped) ["Island"]
        Spec.assertEqWith s "and both left the creature tapped, so the boards differ in nothing else" (fmap (tapStatusOf enchanted) [entered, tapped]) [Just TapState.Tapped, Just TapState.Tapped]
      _ -> Spec.assertFailure s "fixture should give alice exactly two attackers"
  -- CR 608.2f's route into the funnel, through a real resolving spell: Dream's
  -- Grip ({U} Instant, "Choose one -- Tap target permanent; or untap target
  -- permanent." plus Entwine {1}) is the cheapest printing whose first mode is a
  -- bare Effect.Tap, so what is on trial is that opcode reaching Event.tap.
  --
  -- TWO seats here and not three: this leg is about the route, and CR 109.5's
  -- "you" is already settled by the combat leg above. bob holds the Aura on
  -- alice's Piker, so it is his library the draw comes out of and alice's spell
  -- that does the tapping.
  Spec.it s "CR 608.2f a resolving Tap effect goes through the same funnel and draws" $ do
    island <- S.printingOf s registry "Island"
    grip <- S.printingOf s registry "Dream's Grip"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    betrayal <- S.printingOf s registry "Betrayal"
    let (pikerId, gs1) = S.addCreature piker S.alice (S.landsInPlay island 2)
        (auraId, gs2) = S.addCreature betrayal S.bob gs1
        gs3 = snd (S.addLibraryCard mountain S.bob (snd (S.addLibraryCard mountain S.bob (S.attach auraId pikerId gs2))))
        (board, spellId) = S.handOne grip gs3
        cast = S.runPure (aimEveryTargetAt pikerId) board (S.cast S.alice spellId)
        after = settleAndResolve (S.runPure (aimEveryTargetAt pikerId) cast Stack.resolveTop)
    Spec.assertEqWith s "CR 608.2f the Aura's controller drew off the spell's tap" (handNames S.bob after) ["Mountain"]
    Spec.assertEqWith s "and the spell really tapped the enchanted creature" (tapStatusOf pikerId after) (Just TapState.Tapped)
  -- CR 701.19a's other route into the funnel: "instead remove all damage marked
  -- on it and its controller taps it". A regeneration is a tap like any other, and
  -- the shield is what makes the destruction not happen.
  Spec.it s "CR 701.19a regenerating the enchanted creature taps it, and that draws too" $ do
    board <- betrayalBoard s registry True
    case board of
      ([enchanted, _], _, gs) -> do
        let shielded = S.addRegenShield enchanted gs
            after = settleAndResolve (S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [enchanted]))
        Spec.assertEqWith s "CR 701.19a carol drew off the regeneration's tap" (handNames S.carol after) ["Island"]
        Spec.assertBool s (S.onBattlefield enchanted after) "the shield really stopped the destruction"
        Spec.assertEqWith s "and the regenerated creature really is tapped" (tapStatusOf enchanted after) (Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should give alice exactly two attackers"

-- alice attacks with two settled Goblin Pikers, carol holds the Aura, bob defends
-- with nothing. The FIRST Piker is the one the Aura enchants when `attached`.
--
-- Carol's library holds three Islands and alice's one Mountain, so every count
-- below is a real count rather than a CR 104.3c loss, and `handNames` says WHOSE
-- library a card came out of.
betrayalBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
betrayalBoard s registry attached = do
  piker <- S.printingOf s registry "Goblin Piker"
  betrayal <- S.printingOf s registry "Betrayal"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  let (base, mine, _, _) = S.threePlayerCombat [piker, piker] [] []
      stocked = Foldable.foldl' (\g _ -> snd (S.addLibraryCard island S.carol g)) (snd (S.addLibraryCard mountain S.alice base)) [1 :: Int, 2, 3]
      (auraId, withAura) = S.addCreature betrayal S.carol stocked
      board = case mine of
        enchanted : _ | attached -> S.attach auraId enchanted withAura
        _ -> withAura
  pure (mine, auraId, board)

-- Put whatever triggered on the stack (CR 603.3) and resolve the stack down, so a
-- board that fired TWO triggers reads differently from one that fired one. A
-- reading taken with the triggers still on the stack could not tell them apart at
-- gameplay level.
settleAndResolve :: GameState.GameState -> GameState.GameState
settleAndResolve gs0 =
  let go n g =
        if n <= (0 :: Int) || null (GameState.stack g)
          then g
          else go (n - 1) (S.runPure S.identityAnswer g Stack.resolveTop)
   in go 8 (S.runPure S.identityAnswer gs0 Engine.settleForPriority)

-- Every target slot a modal spell offers, aimed at one permanent. Dream's Grip
-- offers one slot per chosen mode and only one mode is chosen here, so the map is
-- a single entry; a hand-built Recipient would be a different recipient from the
-- offered one (CR 608.2b), which is why this rewrites the OFFER.
aimEveryTargetAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimEveryTargetAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- The tap status of one object, Nothing where it is not on the board at all --
-- which a precondition assertion must be able to say apart from "untapped".
tapStatusOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStatusOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  anafenzaAttackSpec s registry
  curseOfVitalitySpec s registry
  boggartPranksterSpec s registry
  avatarRokuSpec s registry
  everWatchingThresholdSpec s registry
  hermesSpec s registry
  militaryIntelligenceSpec s registry
  seiferSpec s registry
  luluSpec s registry
  marauderTollSpec s registry
  reprisalLedgerSpec s registry
  ezuriExperienceSpec s registry
  savantiRomeroSpec s registry
  handOfThePraetorsSpec s registry
  monarchTriggerSpec s registry
  matoyaTriggerSpec s registry
  feywildTricksterSpec s registry
  tavernScoundrelSpec s registry
  aloeAlchemistSpec s registry
  wildgrowthWalkerSpec s registry
  rayOfCommandSpec s registry
  brambleElementalSpec s registry
  sixthSenseSpec s registry
  betrayalSpec s registry
