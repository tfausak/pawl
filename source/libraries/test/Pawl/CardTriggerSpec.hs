{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over one printed trigger per card, from Curse of Vitality
-- to Betrayal: attacks, the monarch, keyword actions, becoming attached and
-- becoming tapped. Split out of Pawl.EventTriggerSpec, which keeps the
-- machinery.
module Pawl.CardTriggerSpec where

import qualified Control.Monad as Monad
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
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
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
import qualified Pawl.Scenario as Scenario
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Board as Board
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
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PermanentActed as PermanentActed
import qualified Pawl.Types.PermanentAction as PermanentAction
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Placement as Placement
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerActed as PlayerActed
import qualified Pawl.Types.PlayerAction as PlayerAction
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Seat as Seat
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Staged as Staged
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

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

-- CR 508.3c's "they": Tyvar the Bellicose's "whenever one or more Elves you
-- control attack, they gain deathtouch until end of turn", read through
-- Binding.attackingCreatures.
--
-- alice declares a Llanowar Elves and a Goblin Piker and keeps Tyvar and a
-- second Llanowar Elves home. Each creature parts one wrong reading: the Piker
-- attacked but is no Elf (every declared attacker), and the two held back are
-- Elves alice controls that did not attack (the Filter alone).
tyvarAttackSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
tyvarAttackSpec s registry =
  let answering :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      answering plan p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (\oid -> List.elem oid plan) ids
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      sentAt gs = Combat.Type.attackers (GameState.combat gs)
      deathtouch = Projection.hasKeyword Keyword.Deathtouch
   in Spec.describe s "Tyvar the Bellicose, attacking"
        . Spec.it s "CR 508.3c the Elves that attacked gain deathtouch, and nothing else does"
        $ do
          tyvar <- S.printingOf s registry "Tyvar the Bellicose"
          elves <- S.printingOf s registry "Llanowar Elves"
          piker <- S.printingOf s registry "Goblin Piker"
          case S.combatBoardOf [tyvar, elves, elves, piker] [] of
            (gs, [tyvarId, attackingElf, homeElf, pikerId], []) -> do
              let after = atBlockers (answering [attackingElf, pikerId]) gs
              Spec.assertBool s (deathtouch attackingElf after) "CR 508.3c the Elf that attacked gained deathtouch"
              Spec.assertBool s (not (deathtouch pikerId after)) "the Piker attacked but is no Elf, so it gained nothing"
              Spec.assertBool s (not (deathtouch homeElf after)) "an Elf alice controls that did not attack gained nothing"
              Spec.assertBool s (not (deathtouch tyvarId after)) "and neither did Tyvar, an Elf that stayed home"
              Spec.assertBool s (not (deathtouch attackingElf gs)) "the fixture: the Elf had no deathtouch before combat"
              Spec.assertEqWith s "CR 508.1b the Elf and the Piker really were declared attacking bob" (sentAt after) (Map.fromList [(attackingElf, AttackTarget.OfPlayer S.bob), (pikerId, AttackTarget.OfPlayer S.bob)])
            _ -> Spec.assertFailure s "fixture should give alice Tyvar, two Llanowar Elves and a Goblin Piker"

-- The same slot under a DealDamage: Lightmine Field's "whenever one or more
-- creatures attack, this enchantment deals damage to each of those creatures
-- equal to the number of attacking creatures".
--
-- alice declares a Hill Giant and a Goblin Piker and keeps a Llanowar Elves
-- home: two attackers, so two damage to each. The Giant survives it with the
-- damage marked, the Piker does not, and the Elf is no attacker and is spared.
lightmineFieldSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
lightmineFieldSpec s registry =
  let answering :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      answering plan p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (\oid -> List.elem oid plan) ids
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      damageOn oid gs = fmap Object.damage (Game.lookupObject oid gs)
   in Spec.describe s "Lightmine Field"
        . Spec.it s "CR 508.3c each of those creatures is dealt damage equal to the number attacking"
        $ do
          field <- S.printingOf s registry "Lightmine Field"
          giant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          elves <- S.printingOf s registry "Llanowar Elves"
          case S.combatBoardOf [field, giant, piker, elves] [] of
            (gs, [_, giantId, pikerId, elfId], []) -> do
              let after = atBlockers (answering [giantId, pikerId]) gs
              Spec.assertEqWith s "CR 508.3c the Giant, one of those creatures, has two damage marked" (damageOn giantId after) (Just 2)
              Spec.assertBool s (not (S.onBattlefield pikerId after)) "the Piker, the other, took two and died"
              Spec.assertEqWith s "the Elf did not attack, so it was dealt nothing" (damageOn elfId after) (Just 0)
            _ -> Spec.assertFailure s "fixture should give alice a Lightmine Field, a Hill Giant, a Goblin Piker and a Llanowar Elves"

-- The same slot counted, through Quantity.BoundCount: Screaming Swarm's
-- "whenever you attack with one or more creatures, target player mills that
-- many cards". alice declares a Hill Giant and a Goblin Piker and keeps the
-- Swarm and a Llanowar Elves home, so "that many" is two where the creatures
-- she controls are four; bob, the target, holds five library cards.
-- CR 603.4 over CR 205.3m: Littjara Kinseekers (a changeling) "when this
-- creature enters, if you control three or more creatures that share a creature
-- type, put a +1/+1 counter on this creature". alice casts it off four Islands
-- beside a Hill Giant and one other creature; the boards differ only in that
-- creature. The Kinseekers counts itself, so a Giant and a Goblin make two
-- two-member groups -- it shares a type with each, but they share none.
littjaraKinseekersSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
littjaraKinseekersSpec s registry =
  Spec.describe s "Littjara Kinseekers"
    . Spec.it s "CR 205.3m three creatures must hold ONE creature type in common, the changeling itself among them"
    $ do
      kinseekers <- S.printingOf s registry "Littjara Kinseekers"
      giant <- S.printingOf s registry "Hill Giant"
      piker <- S.printingOf s registry "Goblin Piker"
      island <- S.printingOf s registry "Island"
      -- The cast object is a new one on the battlefield (CR 400.7), so the
      -- Kinseekers is found there by name.
      let cast second =
            let placed = List.foldl' (\g p -> snd (S.addPermanent p S.alice g)) (S.landsInPlay island 4) [giant, second]
                (kid, gs) = S.addHandCard kinseekers S.alice placed
                ready = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
                onStack = S.runPure S.identityAnswer ready (S.cast S.alice kid)
             in S.runPure S.identityAnswer onStack Engine.priorityLoop
          counters after = fmap (\oid -> S.counterOf CounterKind.PlusOnePlusOne oid after) (filter (`S.onBattlefield` after) (Scenario.namedObjects (S.printingName kinseekers) after))
      Spec.assertEqWith s "with two Giants, three Giants: the trigger put a +1/+1 counter" (counters (cast giant)) [1]
      Spec.assertEqWith s "with a Giant and a Goblin, no type is held three ways: the Kinseekers entered without one" (counters (cast piker)) [0]

screamingSwarmSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
screamingSwarmSpec s registry =
  let answering :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      answering plan p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (\oid -> List.elem oid plan) ids
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) sets
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
   in Spec.describe s "Screaming Swarm"
        . Spec.it s "CR 608.2i the target player mills one card per creature attacked with"
        $ do
          swarm <- S.printingOf s registry "Screaming Swarm"
          giant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          elves <- S.printingOf s registry "Llanowar Elves"
          island <- S.printingOf s registry "Island"
          case S.combatBoardOf [swarm, giant, piker, elves] [] of
            (gs, [_, giantId, pikerId, _], []) -> do
              let stocked = List.foldl' (\g _ -> snd (S.addLibraryCard island S.bob g)) gs [1 :: Int .. 5]
                  after = atBlockers (answering [giantId, pikerId]) stocked
              Spec.assertEqWith s "CR 701.17a bob milled two, one per attacker" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
              Spec.assertEqWith s "and three stay in his library" (length (Game.zoneMembers Zone.Library S.bob after)) 3
              Spec.assertEqWith s "alice milled nothing" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
            _ -> Spec.assertFailure s "fixture should give alice a Screaming Swarm, a Hill Giant, a Goblin Piker and a Llanowar Elves"

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
-- AnyPlayer: on these boards CR 506.2/508.1 let only the active player declare
-- (CR 805.10a's shared team turns widen that), and no player attacks
-- themselves, so alice declaring leaves the "if" false whichever relation is
-- read. What the boards do falsify is You -- bob declares on all three, and a
-- You reading fires nothing at all.
everWatchingThresholdSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
      fired gs = length (Maybe.mapMaybe (\event -> case event of GameEvent.AbilityTriggered record | isPlayerAttacks (TriggeredAbility.condition (AbilityTriggered.ability record)) -> Just (); _ -> Nothing) (S.eventsOf gs))
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
        -- The negative, and the same fixture: bob attacks the third seat, so
        -- neither disjunct holds. Rule 603.4's first sentence -- the ability
        -- "triggers only if" the clause is true -- is what makes `fired` 0 here
        -- and 1 on the boards of the two cr-603-4-the-clause-holds-*.json
        -- scenarios, and rule 508.3d's condition is satisfied on all three alike.
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
      fired gs = length (Maybe.mapMaybe (\event -> case event of GameEvent.AbilityTriggered record | isPlayerAttacksPlayer (TriggeredAbility.condition (AbilityTriggered.ability record)) -> Just (); _ -> Nothing) (S.eventsOf gs))
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
        -- Jace is stocked with loyalty by hand: S.addPermanent puts a printing
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
luluSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
      fired gs = length (Maybe.mapMaybe (\event -> case event of GameEvent.AbilityTriggered record | isPlayerAttacksPlayer (TriggeredAbility.condition (AbilityTriggered.ability record)) -> Just (); _ -> Nothing) (S.eventsOf gs))
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

-- Norn's Decree {2}{W} Enchantment (data/cards/norns-decree.json; Oracle text
-- checked against api.scryfall.com 2026-09-30): "Whenever one or more creatures
-- an opponent controls deal combat damage to you, that opponent gets a poison
-- counter. / Whenever a player attacks, if one or more players being attacked
-- are poisoned, the attacking player draws a card."
--
-- The second ability is CR 508.3d's SUBJECT named by the payload: the player
-- who declared the attackers, bound under Pawl.Engine.Binding.attackingPlayer,
-- behind a CR 603.4 intervening "if" over CR 122.1f's poisoned players among
-- those CR 508.3b declared attacked. The first is CR 603.2c's batch reading of
-- combat damage, narrowed to CR 109.5's "you" and naming the damagers'
-- controller under Pawl.Engine.Binding.damagersController.
--
-- THREE SEATS, because two collapse the attacker onto either the ability's
-- controller or the attacked player: bob holds the enchantment, alice
-- declares, and CR 506.2a's answer sends the declaration at bob or carol.
--
-- The ACTIVE player is not discriminated from the declarer: without the shared
-- team turns option CR 506.2 lets only the active player declare attackers, so
-- the two are the same seat on these boards. Pawl.TeamSpec's shared-turns
-- Norn's Decree case is the board that parts two damagers' controllers.
nornsDecreeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nornsDecreeSpec s registry =
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
      hands gs = (S.handSize S.alice gs, S.handSize S.bob gs, S.handSize S.carol gs)
      poison gs = (S.playerCounterOf PlayerCounterKind.Poison S.alice gs, S.playerCounterOf PlayerCounterKind.Poison S.bob gs, S.playerCounterOf PlayerCounterKind.Poison S.carol gs)
      fired gs = length (Maybe.mapMaybe (\event -> case event of GameEvent.AbilityTriggered record | isPlayerAttacks (TriggeredAbility.condition (AbilityTriggered.ability record)) -> Just (); _ -> Nothing) (S.eventsOf gs))
      -- Every seat's library stocked, so the draw has a card to take and CR
      -- 704.5b ends nobody's game; `poisoned` names who starts with one poison
      -- counter.
      fixture poisoned = do
        piker <- S.printingOf s registry "Goblin Piker"
        decree <- S.printingOf s registry "Norn's Decree"
        let (gs, mine, theirs, others) = S.threePlayerCombat [piker, piker] [decree] []
            stocked = List.foldl' (\g pid -> snd (S.addLibraryCard piker pid g)) gs [S.alice, S.bob, S.carol]
        pure (List.foldl' (flip (S.addPlayerCounter PlayerCounterKind.Poison 1)) stocked poisoned, mine, theirs, others)
   in Spec.describe s "Norn's Decree" $ do
        -- The proving test: alice declares at a poisoned carol, so the card goes
        -- to alice -- not to bob, whose enchantment it is, and not to carol,
        -- whom the declaration was announced at.
        Spec.it s "CR 508.3d the declaring player draws when a player being attacked is poisoned" $ do
          (gs, mine, _, _) <- fixture [S.carol]
          case mine of
            [first, second] -> do
              let after = atBlockers (answering S.carol [first, second]) gs
              Spec.assertEqWith s "CR 121.1 alice drew the card and neither other seat did" (hands after) (1, 0, 0)
              Spec.assertEqWith s "CR 508.3d one trigger for the one declaration, however many creatures were in it" (fired after) 1
              Spec.assertEqWith s "CR 508.1b and both Pikers really were declared at carol" (sentAt after) (Map.fromList [(first, AttackTarget.OfPlayer S.carol), (second, AttackTarget.OfPlayer S.carol)])
            _ -> Spec.assertFailure s "fixture should give alice two Pikers"
        -- The same board with CR 506.2a's answer moved to a poisoned bob. The
        -- attacked player moves and the drawer does not, which is what parts
        -- this slot from the one CR 508.3e's arm stamps.
        Spec.it s "CR 508.3d the drawer does not follow who was attacked" $ do
          (gs, mine, _, _) <- fixture [S.bob, S.carol]
          case mine of
            [first, second] -> do
              let after = atBlockers (answering S.bob [first, second]) gs
              Spec.assertEqWith s "CR 121.1 alice still drew the card, and bob none" (hands after) (1, 0, 0)
              Spec.assertEqWith s "CR 508.1b and both Pikers really were declared at bob" (sentAt after) (Map.fromList [(first, AttackTarget.OfPlayer S.bob), (second, AttackTarget.OfPlayer S.bob)])
            _ -> Spec.assertFailure s "fixture should give alice two Pikers"
        -- The first case's board with the poison counter moved from carol to
        -- bob, whom nobody attacks: a poisoned player is at the table, but none
        -- of the players being attacked is one.
        Spec.it s "CR 603.4 no draw when only a player not being attacked is poisoned" $ do
          (gs, mine, _, _) <- fixture [S.bob]
          case mine of
            [first, second] -> do
              let after = atBlockers (answering S.carol [first, second]) gs
              Spec.assertEqWith s "CR 603.4 nobody drew" (hands after) (0, 0, 0)
              Spec.assertEqWith s "CR 508.1b and both Pikers really were declared at carol" (sentAt after) (Map.fromList [(first, AttackTarget.OfPlayer S.carol), (second, AttackTarget.OfPlayer S.carol)])
            _ -> Spec.assertFailure s "fixture should give alice two Pikers"
        -- The first ability. alice's two Pikers connect with bob in one CR
        -- 510.2 step, so "that opponent" is alice and she gets ONE counter --
        -- the batch is one occurrence, not one per Piker.
        Spec.it s "CR 603.2c the damagers' controller gets one poison counter per combat damage step" $ do
          (gs, mine, _, _) <- fixture []
          case mine of
            [first, second] -> do
              let after = S.runCombat (answering S.bob [first, second]) gs
              Spec.assertEqWith s "CR 122.1f alice got one poison counter, and bob and carol none" (poison after) (1, 0, 0)
              Spec.assertEqWith s "CR 510.2 both Pikers really dealt bob combat damage" (S.lifeOf S.bob after) (Just 16)
            _ -> Spec.assertFailure s "fixture should give alice two Pikers"
        -- The same board with the declaration at carol: the damage reaches a
        -- player, but not the Decree's controller.
        Spec.it s "CR 603.2c no poison counter when the damage is dealt to another player" $ do
          (gs, mine, _, _) <- fixture []
          case mine of
            [first, second] -> do
              let after = S.runCombat (answering S.carol [first, second]) gs
              Spec.assertEqWith s "CR 122.1f nobody got a poison counter" (poison after) (0, 0, 0)
              Spec.assertEqWith s "CR 510.2 both Pikers really dealt carol combat damage" (S.lifeOf S.carol after) (Just 16)
            _ -> Spec.assertFailure s "fixture should give alice two Pikers"

-- Mirkwood Trapper {1}{G}{U} Creature -- Elf Scout 1/4
-- (data/cards/mirkwood-trapper.json; Oracle text checked against
-- api.scryfall.com 2026-09-30): "Whenever a player attacks you, target attacking
-- creature gets -2/-0 until end of turn. / Whenever a player attacks, if they
-- aren't attacking you, that player chooses an attacking creature. It gets
-- +2/+0 until end of turn."
--
-- The second ability's chooser is CR 508.3d's attacking player, read through
-- Pawl.Types.ChosenPermanent's chooser rather than CR 109.5's "you"; its CR
-- 603.4 "if" counts that player's creatures attacking the Trapper's
-- controller. Three seats: bob holds the Trapper, alice declares, and the
-- declaration goes at carol or at bob.
mirkwoodTrapperSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mirkwoodTrapperSpec s registry =
  let -- CR 506.2a's defender and the Pikers FILTERED out of what the engine
      -- offered, nornsDecreeSpec's answerer, plus the CR 608.2d choice: who was
      -- asked is recorded, and the LAST offered permanent is taken, pinned by
      -- position rather than searched for.
      answering :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
      answering defender attackers p = case p of
        Prompt.ChooseDefender _ _ options -> pure (Maybe.fromMaybe (NonEmpty.head options) (List.find (== defender) (NonEmpty.toList options)))
        Prompt.DeclareAttackers _ _ ids -> pure (filter (`elem` attackers) ids)
        Prompt.ChooseAttackTarget _ _ _ options -> pure (Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer defender) (NonEmpty.toList options)))
        Prompt.ChoosePermanent _ chooser _ options -> do
          State.modify' (<> [chooser])
          pure (NonEmpty.last options)
        _ -> pure (S.aggressiveAnswer p)
      -- CR 507 and CR 508 in full: the beginning of combat step, then the
      -- declare attackers step with its triggers resolved.
      throughDeclaration defender attackers gs = State.runState (Engine.runGame (answering defender attackers) gs (Engine.runStep >> Engine.runStep)) []
      powers ids gs = fmap (`Projection.powerOf` gs) ids
      fixture = do
        piker <- S.printingOf s registry "Goblin Piker"
        trapper <- S.printingOf s registry "Mirkwood Trapper"
        pure (S.threePlayerCombat [piker, piker] [trapper] [])
   in Spec.describe s "Mirkwood Trapper" $ do
        -- The proving test: alice attacks carol, not bob, so alice -- not bob,
        -- whose Trapper it is -- chooses which of her attackers gets +2/+0.
        Spec.it s "CR 508.3d the attacking player chooses the creature when they aren't attacking you" $ do
          (gs, mine, _, _) <- fixture
          case mine of
            [first, second] -> do
              let ((_, after), asked) = throughDeclaration S.carol [first, second] gs
              Spec.assertEqWith s "CR 608.2d alice was asked, once" asked [S.alice]
              Spec.assertEqWith s "CR 613.4c the Piker she chose got +2/+0 and the other nothing" (powers [first, second] after) [Just 2, Just 4]
            _ -> Spec.assertFailure s "fixture should give alice two Pikers"
        -- The same board with the declaration at bob: "if they aren't attacking
        -- you" is false, so nobody chooses, and the first ability's -2/-0 lands
        -- on the attacker bob targets instead.
        Spec.it s "CR 603.4 no choice when they are attacking you, and the first ability shrinks an attacker" $ do
          (gs, mine, _, _) <- fixture
          case mine of
            [first, second] -> do
              let ((_, after), asked) = throughDeclaration S.bob [first, second] gs
              Spec.assertEqWith s "CR 603.4 nobody was asked to choose" asked []
              Spec.assertEqWith s "CR 508.3e one Piker got -2/-0 and neither got +2/+0" (List.sort (powers [first, second] after)) [Just 0, Just 2]
            _ -> Spec.assertFailure s "fixture should give alice two Pikers"

-- CR 508.3e with the attacked side named by attachment (CR 303.4b), and its
-- attacking player bound for a CR 701.3a move, with Archnemesis {1}{U}{B}
-- Enchantment -- Aura: "Enchant opponent / Whenever you attack enchanted
-- player, that player loses 2 life. You draw a card and gain 2 life. / Whenever
-- a player attacks you, you may attach this Aura to that player."
--
-- THREE SEATS, since two collapse "you", "enchanted player" and "an opponent".
-- The first board splits alice's attack between the enchanted bob and carol;
-- the second hands the Aura to bob, enchanting carol, so alice attacking carol
-- is CR 508.3b's "enchanted player is attacked" but not rule 508.3e's "you
-- attack".
archnemesisSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
archnemesisSpec s registry =
  let answering :: [(ObjectId.ObjectId, PlayerId.PlayerId)] -> Prompt.Prompt r -> r
      answering aims p = case p of
        Prompt.ChooseAttackTarget _ _ attacker options -> Maybe.fromMaybe (NonEmpty.head options) (do defender <- lookup attacker aims; List.find (== AttackTarget.OfPlayer defender) (NonEmpty.toList options))
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` fmap fst aims) ids
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.aggressiveAnswer p
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
      enchanting oid gs = Recipient.playerOf =<< (Object.attachedTo =<< Game.lookupObject oid gs)
      onto who placement = placement {Placement.attached = Just (S.seatLabel who)}
      piker name = S.settled name "Goblin Piker"
      stocked seat = seat {Seat.library = Seq.fromList [S.permanent "Island"]}
      yours =
        S.board
          ( stocked (S.battlefield S.alice [piker "first", piker "second", S.aliased "archnemesis" (onto S.bob (S.permanent "Archnemesis"))])
              NonEmpty.:| [S.playerSetup S.bob, S.playerSetup S.carol]
          )
          S.alice
          S.beginningOfCombat
      theirs =
        S.board
          ( S.battlefield S.alice [piker "first", piker "second"]
              NonEmpty.:| [stocked (S.battlefield S.bob [S.aliased "archnemesis" (onto S.carol (S.permanent "Archnemesis"))]), S.playerSetup S.carol]
          )
          S.alice
          S.beginningOfCombat
      atBlockers aims built = S.runToStep S.declareBlockers (answering aims) (Staged.state built)
   in Spec.describe s "Archnemesis" $ do
        Spec.it s "CR 508.3e you attack enchanted player: that player loses 2, you draw and gain 2" $ do
          built <- S.buildBoardOrFail s registry yours
          case (aliasIn "first" built, aliasIn "second" built) of
            (Just first, Just second) -> do
              let after = atBlockers [(first, S.bob), (second, S.carol)] built
              Spec.assertEqWith s "CR 119.3 bob lost 2 and alice gained 2, and carol, also attacked, lost nothing" (lives after, S.handSize S.alice after) ((Just 22, Just 18, Just 20), 1)
            _ -> Spec.assertFailure s "fixture should alias alice's two Pikers"
        Spec.it s "CR 508.3e another player attacking the enchanted player does not trigger it" $ do
          built <- S.buildBoardOrFail s registry theirs
          case (aliasIn "first" built, aliasIn "archnemesis" built) of
            (Just first, Just archnemesis) -> do
              let after = atBlockers [(first, S.carol)] built
              Spec.assertEqWith s "CR 508.3e nobody's life or hand moved, and the Aura stayed on carol" (lives after, S.handSize S.bob after, enchanting archnemesis after) ((Just 20, Just 20, Just 20), 0, Just S.carol)
            _ -> Spec.assertFailure s "fixture should alias alice's Piker and bob's Archnemesis"
        Spec.it s "CR 701.3a a player attacking you gets the Aura" $ do
          built <- S.buildBoardOrFail s registry theirs
          case (aliasIn "first" built, aliasIn "archnemesis" built) of
            (Just first, Just archnemesis) -> do
              let after = atBlockers [(first, S.bob)] built
              Spec.assertEqWith s "CR 701.3a Archnemesis now enchants alice, the attacking player" (enchanting archnemesis after) (Just S.alice)
            _ -> Spec.assertFailure s "fixture should alias alice's Piker and bob's Archnemesis"

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
        let (ezuriId, withEzuri) = S.addPermanent ezuri S.alice (Setup.emptyGame S.bothPlayers)
            (targetId, gs) = S.addPermanent construct S.alice withEzuri
        pure (ezuriId, targetId, gs)
   in Spec.describe s "Ezuri, Claw of Progress" $ do
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
          let boardWith n = snd (S.addPermanent ezuri S.alice (S.landsInPlay mountain n))
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
          let (_, withEzuri) = S.addPermanent ezuri S.alice (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger piker S.bob withEzuri
              after = S.runPure S.identityAnswer entered (Engine.settleForPriority >> Engine.priorityLoop)
          Spec.assertEqWith s "alice gets no experience counter" (experienceOf S.alice after) 0
          Spec.assertEqWith s "and neither does bob, who has no Ezuri" (experienceOf S.bob after) 0
        -- CR 603.10's first sentence: the entrant's power is read immediately
        -- after it entered, not after CR 704.5j's legend rule buries the
        -- Kongming that pumped it. A second Kongming, "Sleeping Dragon" (2/2,
        -- "Other creatures you control get +1/+1") enters as a 3/3; alice keeps
        -- it, and it is a 2/2 by the trigger scan. The pair differs only in the
        -- first Kongming.
        Spec.it s "CR 603.10 a Kongming that entered as a 3/3 pays nothing, though the legend rule leaves it a 2/2" $ do
          ezuri <- S.printingOf s registry "Ezuri, Claw of Progress"
          kongming <- S.printingOf s registry "Kongming, \"Sleeping Dragon\""
          plains <- S.printingOf s registry "Plains"
          let (ezuriId, withEzuri) = S.addPermanent ezuri S.alice (S.landsInPlay plains 4)
              (firstId, withFirst) = S.addPermanent kongming S.alice withEzuri
              keepNewcomer :: Prompt.Prompt r -> r
              keepNewcomer p = case p of
                Prompt.ChooseLegend _ _ candidates -> Maybe.fromMaybe (NonEmpty.head candidates) (List.find (/= firstId) (NonEmpty.toList candidates))
                _ -> S.identityAnswer p
              resolveWith gs0 =
                let (gs, spell) = S.handOne kongming gs0
                 in S.runPure keepNewcomer (S.runPure keepNewcomer gs (S.cast S.alice spell)) Engine.priorityLoop
              pumped = resolveWith withFirst
              alone = resolveWith withEzuri
          Spec.assertEqWith s "the newcomer entered as a 3/3, so alice gets nothing" (experienceOf S.alice pumped) 0
          Spec.assertBool s (not (Set.member firstId (GameState.battlefield pumped))) "the legend rule buried the first Kongming"
          Spec.assertEqWith s "and the newcomer pumps Ezuri, so it survived" (S.powerToughnessOf ezuriId pumped) (Just (4, 4))
          Spec.assertEqWith s "with no first Kongming it enters as a 2/2 and pays one" (experienceOf S.alice alone) 1

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
        let (savantiId, withSavanti) = S.addPermanent savanti S.alice (Setup.emptyGame S.bothPlayers)
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
        let (_, g1) = S.addPermanent piker S.bob base
            (_, g2) = S.addPermanent birdMaiden S.bob g1
         in snd (S.addPermanent bogWraith S.carol g2)
      -- CR 725.2's crown steal, driven by the damage EVENT rather than by a full
      -- combat: Event.Trigger.inherentTriggers reads the recorded DamageEvent, and
      -- ExpirySpec's monarch group drives the same rule the same way.
      combatDamageTo monarch damager =
        S.withEvents [GameEvent.DamageDealt (DamageEvent.MkDamageEvent damager (Recipient.ToPlayer monarch) 2 False False False 0 Nothing Nothing mempty False DamageKind.Combat)]
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
        -- CR 725.2's crown steal reaches the SAME condition by a route the card
        -- has nothing to do with: the inherent ability has no source, and
        -- Event.Trigger.inherentTriggers rather than the scan over objects is
        -- what gathers it. What the Lich matches is the crowning, not the entry
        -- that usually causes one.
        Spec.it s "CR 725.2 a stolen crown is a crowning, and fires the same trigger" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          boggartBrute <- S.printingOf s registry "Boggart Brute"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (S.withMonarch S.bob (Setup.emptyGame S.threePlayers))
              (lich, g1) = S.addPermanent custodiLich S.alice base
              (brute, gs) = S.addPermanent boggartBrute S.alice g1
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
              (lich, g1) = S.addPermanent custodiLich S.alice base
              (_, gs) = S.addPermanent boggartBrute S.alice g1
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
              (lich, g1) = S.addPermanent custodiLich S.alice base
              (_, g2) = S.addPermanent piker S.carol g1
              (_, gs) = S.addPermanent birdMaiden S.carol g2
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
    let addN n printing pid g = if n <= (0 :: Int) then g else addN (n - 1) printing pid (snd (S.addPermanent printing pid g))
        lands = addN 3 mountain S.alice (addN 4 island S.alice S.threePlayerGame)
        (bobPiker, g1) = S.addPermanent piker S.bob lands
        (carolPiker, g2) = S.addPermanent piker S.carol g1
        (rayId, g3) = S.addHandCard rayOfCommand S.alice g2
        (actId, g4) = S.addHandCard actOfTreason S.alice g3
        -- Both victims start TAPPED, so the first sentence of each card (CR 701.26b)
        -- has something to do and `Tapped` at the end cannot be state left standing.
        staged = S.tapObject carolPiker (S.tapObject bobPiker g4)
        -- Narrows every target slot to one object, `aimedCast`'s filter without its cast
        -- pinning: the board holds two stealable creatures on purpose, so the engine's
        -- first offer is not the one either leg means. Filtering the OFFERED set rather
        -- than naming a Recipient keeps the answer in whatever shape the slot offered.
        aimAtVictim :: ObjectId.ObjectId -> Prompt.Prompt r -> r
        aimAtVictim oid p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just oid) . Recipient.objectOf) legal) sets
          _ -> S.identityAnswer p
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

-- Matoya, Archon Elder {2}{U} Legendary Creature -- Human Warlock 1/4, "Whenever
-- you scry or surveil, draw a card" -- CR 603.1b's AnyOf over
-- TriggerCondition.PlayerActs under PlayerAction.Scry and PlayerAction.Surveil, so one card
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
matoyaTriggerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
        let (ballId, placed) = S.addPermanent crystalBall S.alice (S.landsInPlay island 4)
            watched = if withMatoya then snd (S.addPermanent matoya S.alice placed) else placed
            deal g p = snd (S.addLibraryCard p S.alice g)
            stocked = List.foldl' deal watched (reverse (take stock [piker, maiden]))
        pure (ballId, stocked {GameState.priority = Just S.alice})
      -- Activate the Ball and settle: the ability resolves, the scry happens and
      -- any trigger it raised is placed and resolved in the same round. A board
      -- offering any other number of abilities activates none, which fails every
      -- assertion rather than passing one for a reason the case did not choose.
      runBall who ballId gs = case Activatable.abilitiesFor ballId gs of
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
   in Spec.describe s "MatoyaKeywordActionTrigger" $ do
        -- CR 701.22d's "even if some or all of those actions were impossible",
        -- and the case that discriminates WHERE the event is recorded: a library
        -- of exactly one card gives scry 2 nothing to decide -- top and bottom
        -- are one position -- so Resolve.decideScry asks no question and reorders
        -- nothing. The scry happened all the same, and Matoya draws that card.
        --
        -- Recording the event only for a scryer decideScry asked passes every
        -- assertion in the case above and fails this one.
        Spec.it s "CR 701.22d a scry with nothing to decide still draws Matoya's card" $ do
          (ballId, board) <- scryBoardFor True 1
          (bareBall, bare) <- scryBoardFor False 1
          let after = runBall S.alice ballId board
              baseline = runBall S.alice bareBall bare
          Spec.assertBool s (elem (GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.Scry S.alice)) (S.eventsOf after)) "CR 701.22d the scry is still an event"
          Spec.assertEqWith s "Matoya drew the lone card" (S.handSize S.alice after) 1
          Spec.assertEqWith s "so alice's library is empty" (length (Game.zoneMembers Zone.Library S.alice after)) 0
          Spec.assertEqWith s "and without Matoya nothing was drawn" (S.handSize S.alice baseline) 0
          Spec.assertEqWith s "the card stayed on the library instead" (length (Game.zoneMembers Zone.Library S.alice baseline)) 1

-- Feywild Trickster {2}{U} Creature -- Gnome Warlock 2/2, "Whenever you roll one
-- or more dice, create a 1/1 blue Faerie Dragon creature token with flying" --
-- the pool's producer for PlayerAction.RollDice (CR 706.1).
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
            watched = List.foldl' (\gs who -> snd (S.addPermanent trickster who gs)) libraries tricksters
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
   in Spec.describe s "PlayerActs RollDice" $ do
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
          Spec.assertBool s (elem (GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.RollDice S.alice)) (S.eventsOf after)) "CR 706.1 the roll recorded its event under the roller"
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
          Spec.assertBool s (elem (GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.RollDice S.bob)) (S.eventsOf after)) "bob really rolled, so there was an event to match"
          Spec.assertBool s (notElem (GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.RollDice S.alice)) (S.eventsOf after)) "and the event names the roller, not the watcher"
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

-- Karplusan Minotaur {2}{R}{R} Creature -- Minotaur Warrior 3/3, "Cumulative
-- upkeep--Flip a coin. / Whenever you win a coin flip, this creature deals 1
-- damage to any target. / Whenever you lose a coin flip, this creature deals 1
-- damage to any target of an opponent's choice." -- the pool's only producer for
-- TriggerCondition.PlayerLosesCoinFlip (CR 705.2) and for CostComponent.FlipCoin
-- (CR 705.1 as a cost).
--
-- ONE CARD carries both halves again, Tavern Scoundrel's shape: its cumulative
-- upkeep is the flipper and its two triggers are the watchers, so the flip a COST
-- makes is what proves the cost road records rule 705.1's event at all.
--
-- THREE SEATS, because the losing trigger's slot is announced by an OPPONENT (CR
-- 115.1, Pawl.Types.TargetSlot's chooser) and at two seats that collapses onto
-- the one thing alice could have named herself. Every announcing seat names the
-- seat BEFORE it -- alice names carol, bob names alice -- so WHICH seat announced
-- is readable straight off a life total, and the won and lost legs land their
-- damage on different players.
--
-- THE TWO LEGS differ in the FACE alone, against a call pinned to heads in both:
--
--   * WON (heads): alice announces, so carol takes 1.
--   * LOST (tails): alice names an opponent, bob announces, so ALICE takes 1 --
--     a seat she would never have named. A condition matching the flip rather
--     than its outcome fires both triggers here and damages carol too.
--
-- CR 705.2's FIRST SENTENCE is the third leg, and the discriminating board this
-- whole condition exists for: Molten Sentry's as-enters flip has no winner, so
-- CoinFlipped.won is Nothing rather than Just False, and NEITHER trigger fires.
-- Before PlayerLosesCoinFlip there was no board that told those two apart.
--
-- CR 603.3 IS THE SEQUENCING throughout: the flip happens while the cumulative
-- upkeep ability or the entry replacement is being carried out, so the damage
-- trigger reaches the stack only at the next priority.
karplusanMinotaurSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
karplusanMinotaurSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- CounterKeywordTriggerSpec's cumulative upkeep device: one upkeep for
      -- alice, run to the end of the priority loop so CR 603.3 gathers and
      -- resolves both the keyword's ability and the damage trigger off its flip.
      steppedTo pid gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
      board = do
        minotaur <- S.printingOf s registry "Karplusan Minotaur"
        pure (S.addPermanent minotaur S.alice (Setup.emptyGame S.threePlayers))
      runUpkeep face gs =
        let settled = snd (Engine.runGamePure (minotaurAnswer face) (steppedTo S.alice gs) Engine.settleForPriority)
         in snd (Engine.runGamePure (minotaurAnswer face) settled Engine.priorityLoop)
      flips gs = [flipped | GameEvent.CoinFlipped flipped <- S.eventsOf gs]
   in Spec.describe s "PlayerLosesCoinFlip" $ do
        -- CR 705.2: the call did not match the face, so alice lost the flip her
        -- own cumulative upkeep paid, and only the losing trigger fires.
        Spec.it s "CR 705.2 a lost flip damages the target an opponent names" $ do
          (_, gs) <- board
          let after = runUpkeep CoinFace.Tails gs
          -- The behaviour, ahead of every proxy. bob announced and named alice;
          -- alice announces only for the WINNING trigger, and would have named
          -- carol.
          Spec.assertEqWith s "CR 705.2 alice, whom bob named off her lost flip, took the damage" (S.lifeOf S.alice after) (Just 19)
          Spec.assertEqWith s "CR 705.2 and carol, whom the winning trigger would have hit, took none" (S.lifeOf S.carol after) (Just 20)
          Spec.assertEqWith s "nor did bob, who announced but named nobody" (S.lifeOf S.bob after) (Just 20)
          -- The flip really happened, and rule 705.2 really lost it -- which is
          -- what keeps the two totals above from passing for an upkeep that
          -- flipped nothing.
          Spec.assertEqWith s "CR 705.1 one flip, and CR 705.2 lost it" (flips after) [CoinFlipped.MkCoinFlipped {CoinFlipped.flipper = S.alice, CoinFlipped.won = Just False}]
        -- The SAME board and the same call, differing in the face alone: alice
        -- won, so the other trigger fires and announces at her own seat.
        Spec.it s "CR 705.2 the same call against heads fires the winning trigger instead" $ do
          (_, gs) <- board
          let after = runUpkeep CoinFace.Heads gs
          Spec.assertEqWith s "CR 705.2 carol, whom alice named off her won flip, took the damage" (S.lifeOf S.carol after) (Just 19)
          Spec.assertEqWith s "CR 705.2 and alice, whom the losing trigger would have hit, took none" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "CR 705.1 one flip, and CR 705.2 won it" (flips after) [CoinFlipped.MkCoinFlipped {CoinFlipped.flipper = S.alice, CoinFlipped.won = Just True}]

-- alice pays rule 702.24a's cumulative upkeep and calls heads; the coin shows
-- `face`. Both questions are pinned by CONSTANT so the engine cannot repair
-- either after a mutation, and CR 705.2's two answers are deliberately different
-- questions -- Prompt.CallCoin is the choice, Prompt.FlipCoin the flip.
--
-- Every announcing seat names the seat BEFORE it, Pawl.TargetSpec's rotation and
-- for its reason: keyed on the ASKING seat, which the prompt carries, so a pure
-- answerer cannot answer the two announcements alike. The answer is FILTERED out
-- of the offer rather than built, so it is the recipient the pool produced (CR
-- 608.2b).
minotaurAnswer :: CoinFace.CoinFace -> Prompt.Prompt r -> r
minotaurAnswer face p = case p of
  Prompt.FlipCoin -> face
  Prompt.CallCoin {} -> CoinFace.Heads
  Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
    | d == S.alice && player == S.alice -> PaymentDecision.Pays
  Prompt.ChooseTargets _ asker _ asked -> fmap (\(_, offered) -> Set.filter ((Just (seatBeforeMinotaur asker) ==) . Recipient.playerOf) offered) asked
  _ -> S.identityAnswer p

-- The rotation minotaurAnswer names its victim by.
seatBeforeMinotaur :: PlayerId.PlayerId -> PlayerId.PlayerId
seatBeforeMinotaur pid
  | pid == S.alice = S.carol
  | pid == S.bob = S.alice
  | otherwise = S.bob

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
          let (pikerId, g1) = S.addPermanent piker S.alice (S.landsInPlay forest 2)
              (maidenId, g2) = S.addPermanent maiden S.bob g1
              (aloeId, g3) = S.addHandCard aloe S.alice g2
              gs = sorcerySpeed g3
              plotted = S.runPure (aimAt pikerId) gs (mapM_ (Plot.plot S.manaPerformer S.alice aloeId) (Plot.plotCostsOf S.alice aloeId gs))
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
          let (pikerId, g1) = S.addPermanent piker S.alice (S.landsInPlay island 4)
              (_, g2) = S.addExiledCard aloe S.alice g1
              (djinnId, g3) = S.addHandCard djinn S.alice g2
              gs = sorcerySpeed g3
              plotted = S.runPure (aimAt pikerId) gs (mapM_ (Plot.plot S.manaPerformer S.alice djinnId) (Plot.plotCostsOf S.alice djinnId gs))
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
-- the pool's producer for PermanentAction.Explore (CR 701.44b).
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
        let (walkerId, g1) = S.addPermanent walker S.alice (Setup.emptyGame S.bothPlayers)
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
          Spec.assertBool s (elem (GameEvent.PermanentActed (PermanentActed.MkPermanentActed PermanentAction.Explore branchId)) (S.eventsOf after)) "CR 701.44b the explore recorded its event"
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
          Spec.assertBool s (elem (GameEvent.PermanentActed (PermanentActed.MkPermanentActed PermanentAction.Explore branchId)) (S.eventsOf after)) "bob's Branchwalker really explored"
          Spec.assertEqWith s "so it took its own counter" (S.powerToughnessOf branchId after) (Just (3, 2))
          Spec.assertEqWith s "but alice's Walker is still 1/3" (S.powerToughnessOf walkerId after) (Just (1, 3))
          Spec.assertEqWith s "and alice gained no life" (S.lifeOf S.alice after) (Just 20)
        -- CR 701.44b's "even if some or all of those actions were impossible":
        -- an empty library reveals nothing, so nothing is a land card and
        -- nothing is binned. The permanent explored all the same.
        Spec.it s "CR 701.44b an explore off an empty library still grows the Walker" $ do
          (walkerId, branchId, gs) <- board S.alice []
          let after = settle gs
          Spec.assertBool s (elem (GameEvent.PermanentActed (PermanentActed.MkPermanentActed PermanentAction.Explore branchId)) (S.eventsOf after)) "the explore is still an event"
          Spec.assertEqWith s "the Walker grew" (S.powerToughnessOf walkerId after) (Just (2, 4))
          Spec.assertEqWith s "alice gained 3" (S.lifeOf S.alice after) (Just 23)
          Spec.assertEqWith s "and nothing was binned" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
        -- The ACTION half, CR 603.2: a connive by a creature alice controls is
        -- the same PermanentActed event with another action, so this is the
        -- board that tells "explores" from "acts". Raffine's Informant connives
        -- on entry and discards the Hill Giant, so its own counter shows the
        -- connive happened.
        Spec.it s "CR 701.50f a creature alice controls conniving does not grow her Walker" $ do
          walker <- S.printingOf s registry "Wildgrowth Walker"
          informant <- S.printingOf s registry "Raffine's Informant"
          forest <- S.printingOf s registry "Forest"
          giant <- S.printingOf s registry "Hill Giant"
          let (walkerId, g1) = S.addPermanent walker S.alice (Setup.emptyGame S.bothPlayers)
              (_, g2) = S.addLibraryCard forest S.alice g1
              (giantId, g3) = S.addHandCard giant S.alice g2
              (informantId, gs) = S.entersWithTrigger informant S.alice g3
              discardGiant :: Prompt.Prompt r -> r
              discardGiant p = case p of
                Prompt.ChooseDiscard {} -> [giantId]
                _ -> S.identityAnswer p
              after = S.runPure discardGiant gs Engine.priorityLoop
          Spec.assertEqWith s "alice's Walker is still 1/3" (S.powerToughnessOf walkerId after) (Just (1, 3))
          Spec.assertEqWith s "and alice gained no life" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "the Informant did connive: it took its own counter" (S.powerToughnessOf informantId after) (Just (3, 2))

-- CR 701.50a over a whole card. Raffine's Informant {1}{W} Creature -- Human
-- Wizard 2/1, "When this creature enters, it connives." (Oracle text checked
-- against api.scryfall.com, 2026-09-11.)
--
-- One board, two answers: alice holds a Hill Giant and a Mountain, draws the
-- Forest on top of her library, and discards whichever card the answer pins by
-- id. Nothing else differs, so the counter is the nonland question alone. Three
-- cards in hand when the discard is asked, so the prompt is a real choice.
raffinesInformantSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
raffinesInformantSpec s registry =
  let board = do
        informant <- S.printingOf s registry "Raffine's Informant"
        giant <- S.printingOf s registry "Hill Giant"
        mountain <- S.printingOf s registry "Mountain"
        forest <- S.printingOf s registry "Forest"
        let (_, g1) = S.addLibraryCard forest S.alice (Setup.emptyGame S.bothPlayers)
            (giantId, g2) = S.addHandCard giant S.alice g1
            (mountainId, g3) = S.addHandCard mountain S.alice g2
            (informantId, g4) = S.entersWithTrigger informant S.alice g3
        pure (informantId, giantId, mountainId, g4)
      discarding :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      discarding pick p = case p of
        Prompt.ChooseDiscard {} -> [pick]
        _ -> S.identityAnswer p
      settle pick gs = S.runPure (discarding pick) gs Engine.priorityLoop
      graveyardNames gs = List.sort (fmap (\oid -> Text.unpack (CardName.unwrap (S.soleFaceName oid gs))) (Game.zoneMembers Zone.Graveyard S.alice gs))
   in Spec.describe s "RaffinesInformantConnive" $ do
        Spec.it s "CR 701.50a discarding a nonland card puts a +1/+1 counter on the conniver" $ do
          (informantId, giantId, _, gs) <- board
          let after = settle giantId gs
          Spec.assertEqWith s "the Informant took its +1/+1 counter" (S.powerToughnessOf informantId after) (Just (3, 2))
          Spec.assertEqWith s "the Hill Giant was the card discarded" (graveyardNames after) ["Hill Giant"]
          Spec.assertEqWith s "and alice drew the Forest first" (handNames S.alice after) ["Forest", "Mountain"]
        Spec.it s "CR 701.50a discarding a land card puts no counter on the conniver" $ do
          (informantId, _, mountainId, gs) <- board
          let after = settle mountainId gs
          Spec.assertEqWith s "the Informant is still 2/1" (S.powerToughnessOf informantId after) (Just (2, 1))
          Spec.assertEqWith s "the Mountain was the card discarded" (graveyardNames after) ["Mountain"]
          Spec.assertEqWith s "and alice drew the Forest first" (handNames S.alice after) ["Forest", "Hill Giant"]

-- CR 701.50d over a whole card. Raffine, Scheming Seer {W}{U}{B} Legendary
-- Creature -- Sphinx Demon 1/4, "Flying, ward {1} / Whenever you attack, target
-- attacking creature connives X, where X is the number of attacking creatures."
-- (Oracle text checked against api.scryfall.com, 2026-09-12.)
--
-- THREE creatures are declared, so X is 3: a reading that kept CR 701.50a's one
-- card draws two fewer and can grow the conniver by at most one. The target is
-- the Goblin Piker rather than Raffine, so the counters have somewhere to land
-- that is not the ability's source.
--
-- Two boards, differing in the HAND. On the first alice's hand is empty, so the
-- three drawn cards are the whole hand and CR 609.3 forces the discard -- no
-- prompt, and the counter count is the nonland question alone. On the second she
-- holds four lands before the draw, so seven cards are offered against a count
-- of three and the discard is a real choice, pinned by id. The three she picks
-- are not the first three cards of her hand, so a reading that ignored the
-- answer and took the hand in order would bin different cards.
raffineSchemingSeerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
raffineSchemingSeerSpec s registry =
  let -- Declares all three of alice's creatures at bob, targets the Piker with
      -- the connive, and discards exactly the cards `picks` names -- FILTERED
      -- out of what the engine offered rather than built, so a mutation cannot
      -- be repaired by an answerer that goes looking for a legal option.
      answering :: [ObjectId.ObjectId] -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      answering attackers aim picks p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (\oid -> List.elem oid attackers) ids
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature aim) . snd) sets
        Prompt.ChooseDiscard _ _ held _ -> filter (\oid -> List.elem oid held) picks
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      libraryOf = Game.zoneMembers Zone.Library S.alice
      graveyardNames gs = List.sort (fmap (\oid -> Text.unpack (CardName.unwrap (S.soleFaceName oid gs))) (Game.zoneMembers Zone.Graveyard S.alice gs))
      -- alice controls Raffine, a Goblin Piker and an Augury Raven, all Settled
      -- and untapped; bob controls nothing, so no block intervenes. `top` is
      -- stocked last and drawn first, `hand` starts in her hand.
      fixture top hand = do
        raffine <- S.printingOf s registry "Raffine, Scheming Seer"
        piker <- S.printingOf s registry "Goblin Piker"
        raven <- S.printingOf s registry "Augury Raven"
        deep <- Monad.mapM (S.printingOf s registry) ["Swamp", "Plains"]
        tops <- Monad.mapM (S.printingOf s registry) top
        helds <- Monad.mapM (S.printingOf s registry) hand
        case S.combatBoardOf [raffine, piker, raven] [] of
          (gs, [raffineId, pikerId, ravenId], _) ->
            let stock (acc, g) printing = let (oid, g1) = S.addLibraryCard printing S.alice g in (oid : acc, g1)
                (stockedIds, stocked) = List.foldl' stock ([], gs) (deep <> reverse tops)
                hold (acc, g) printing = let (oid, g1) = S.addHandCard printing S.alice g in (acc <> [oid], g1)
                (heldIds, ready) = List.foldl' hold ([], stocked) helds
             in pure (Just (raffineId, pikerId, ravenId, stockedIds, heldIds, ready))
          _ -> pure Nothing
   in Spec.describe s "Raffine, Scheming Seer" $ do
        -- The proving case. Three attackers, three cards drawn, all three
        -- discarded, two of them nonland: the Piker is 2/1 plus two counters.
        Spec.it s "CR 701.50d whole card: three attackers connive 3, one counter per nonland discarded" $ do
          built <- fixture ["Hill Giant", "Bird Maiden", "Mountain"] []
          case built of
            Just (raffineId, pikerId, ravenId, stockedIds, _, gs) -> do
              let after = atBlockers (answering [raffineId, pikerId, ravenId] pikerId []) gs
              Spec.assertEqWith s "CR 701.50d the Piker took one +1/+1 counter per nonland card discarded" (S.powerToughnessOf pikerId after) (Just (4, 3))
              Spec.assertEqWith s "all three drawn cards were discarded" (graveyardNames after) ["Bird Maiden", "Hill Giant", "Mountain"]
              Spec.assertEqWith s "and exactly three cards left the library" (length (libraryOf after)) (length stockedIds - 3)
              Spec.assertEqWith s "leaving nothing in hand" (handNames S.alice after) []
            Nothing -> Spec.assertFailure s "fixture should give alice Raffine, a Piker and a Raven"

-- CR 701.50f over a whole card. Iron Monger, Sadistic Tycoon {2}{B} Legendary
-- Artifact Creature -- Human Villain 2/2, "Flying / Whenever a creature you
-- control connives, put a +1/+1 counter on each Villain you control." (Oracle
-- text checked against api.scryfall.com, 2026-09-12.)
--
-- Two boards differing in ONE thing: who controls the conniving Raffine's
-- Informant. alice's Monger grows off her own Informant and not off bob's, so
-- the Filter is doing the work rather than the event's mere presence -- which
-- both boards assert, so a reading that recorded nothing cannot pass either.
--
-- The conniving seat's hand is empty and its library holds one nonland card, so
-- CR 609.3 forces the discard and no prompt arises.
ironMongerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ironMongerSpec s registry =
  let board conniver = do
        monger <- S.printingOf s registry "Iron Monger, Sadistic Tycoon"
        informant <- S.printingOf s registry "Raffine's Informant"
        giant <- S.printingOf s registry "Hill Giant"
        let (mongerId, g1) = S.addPermanent monger S.alice (Setup.emptyGame S.bothPlayers)
            (_, g2) = S.addLibraryCard giant conniver g1
            (informantId, g3) = S.entersWithTrigger informant conniver g2
        pure (mongerId, informantId, g3)
      settle gs = S.runPure S.identityAnswer gs Engine.priorityLoop
   in Spec.describe s "Iron Monger, Sadistic Tycoon" $ do
        Spec.it s "CR 701.50f whenever a creature you control connives, each Villain you control grows" $ do
          (mongerId, informantId, gs) <- board S.alice
          let after = settle gs
          Spec.assertEqWith s "CR 701.50f alice's Monger took its +1/+1 counter" (S.powerToughnessOf mongerId after) (Just (3, 3))
          Spec.assertBool s (elem (GameEvent.PermanentActed (PermanentActed.MkPermanentActed PermanentAction.Connive informantId)) (S.eventsOf after)) "the connive recorded its event"
          Spec.assertEqWith s "and the Informant grew off its own nonland discard" (S.powerToughnessOf informantId after) (Just (3, 2))
        -- The negative, one thing different: bob controls the Informant. The
        -- connive still happens and still records its event, so what fails is the
        -- Filter's "you control" and nothing else.
        Spec.it s "CR 701.50f an opponent's connive leaves the Monger alone" $ do
          (mongerId, informantId, gs) <- board S.bob
          let after = settle gs
          Spec.assertEqWith s "alice's Monger is still 2/2" (S.powerToughnessOf mongerId after) (Just (2, 2))
          Spec.assertBool s (elem (GameEvent.PermanentActed (PermanentActed.MkPermanentActed PermanentAction.Connive informantId)) (S.eventsOf after)) "bob's Informant really did connive"
          Spec.assertEqWith s "and bob's Informant grew off its own nonland discard" (S.powerToughnessOf informantId after) (Just (3, 2))

-- CR 701.50e over a whole card. Spymaster's Vault, Land, "{B}, {T}: Target
-- creature you control connives X, where X is the number of creatures that died
-- this turn." (Oracle text checked against api.scryfall.com, 2026-09-25.)
--
-- alice's Iron Monger is both the target and the watcher: a connive would draw
-- the Hill Giant on top of her library, discard it, grow the Monger once for the
-- nonland discard and once more off its own CR 701.50f trigger. Two boards
-- differing in ONE thing: whether a Goblin Piker died this turn, which is X.
spymastersVaultSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spymastersVaultSpec s registry =
  let board killFirst = do
        vault <- S.printingOf s registry "Spymaster's Vault"
        swamp <- S.printingOf s registry "Swamp"
        monger <- S.printingOf s registry "Iron Monger, Sadistic Tycoon"
        piker <- S.printingOf s registry "Goblin Piker"
        giant <- S.printingOf s registry "Hill Giant"
        let (vaultId, g1) = S.addPermanent vault S.alice (Setup.emptyGame S.bothPlayers)
            (swampId, g2) = S.addPermanent swamp S.alice g1
            (mongerId, g3) = S.addPermanent monger S.alice g2
            (pikerId, g4) = S.addPermanent piker S.alice g3
            (_, g5) = S.addLibraryCard giant S.alice g4
            g6 = if killFirst then S.runPure S.identityAnswer g5 (Event.destroy Regenerability.Regenerable [pikerId]) else g5
            floated = S.runPure S.identityAnswer g6 (S.tapForMana swampId)
            ability = case Face.activatedAbilities (S.combinedFace vault) of
              _ : connive : _ -> connive
              _ -> error "Pawl.CardTriggerSpec: Spymaster's Vault has two activated abilities"
            activated = snd (Engine.runGamePure (aimAtOffered mongerId) floated (Activate.activateAbility S.alice vaultId ability))
        pure (mongerId, S.runPure S.identityAnswer activated Engine.priorityLoop)
   in Spec.describe s "Spymaster's Vault" $ do
        Spec.it s "CR 701.50e with nothing dead, conniving 0 draws, discards, grows and triggers nothing" $ do
          (mongerId, after) <- board False
          Spec.assertEqWith s "no card was drawn" (length (Game.zoneMembers Zone.Hand S.alice after), length (Game.zoneMembers Zone.Library S.alice after)) (0, 1)
          Spec.assertEqWith s "the Monger neither grew nor triggered" (S.powerToughnessOf mongerId after) (Just (2, 2))
          Spec.assertBool s (notElem (GameEvent.PermanentActed (PermanentActed.MkPermanentActed PermanentAction.Connive mongerId)) (S.eventsOf after)) "and no connive event was recorded"
        Spec.it s "CR 701.50d with a creature dead, the Monger connives 1" $ do
          (mongerId, after) <- board True
          Spec.assertEqWith s "the Giant was drawn and discarded" (length (Game.zoneMembers Zone.Hand S.alice after), length (Game.zoneMembers Zone.Library S.alice after)) (0, 0)
          Spec.assertEqWith s "one counter for the nonland discard and one off its own trigger" (S.powerToughnessOf mongerId after) (Just (4, 4))

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

brambleElementalSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
brambleElementalSpec s registry =
  Spec.describe s "CR 701.3a a trigger on becoming attached" $ do
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
      let (brambleId, base1) = S.addPermanent bramble S.alice (S.landsInPlay plains 3)
          (bladeId, base2) = S.addPermanent bonesplitter S.alice base1
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

-- Is this permanent tapped? Read off the object rather than counted, because the
-- cases below name WHICH creature and a count could not.
isTapped :: ObjectId.ObjectId -> GameState.GameState -> Bool
isTapped oid gs = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Tapped

-- The CR 117.5 boundary and the stack, run until neither has anything left --
-- what a leg needs when one trigger's resolution is what fires the next.
settleTriggers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
settleTriggers answer =
  let go n gs =
        let placed = S.runPure answer gs Engine.settleForPriority
         in if n <= 0 || null (GameState.stack placed)
              then placed
              else go (n - 1) (S.runPure answer placed Stack.resolveTop)
   in go (10 :: Int)

-- Rule 702.6a's minted equip ability, off the PROJECTION: an Equipment declares
-- the keyword and prints no activated ability of its own.
equipAbilityOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
equipAbilityOf oid gs = case Projection.abilitiesOf oid gs of
  ability : _ -> Just ability
  [] -> Nothing

-- CR 701.3a's attachment event read from the ATTACHMENT's side, by
-- TriggerCondition.SelfBecomesAttachedTo -- the mirror of Bramble Elemental's
-- group above.
--
-- Enormous Energy Blade, {2}{B} Artifact -- Equipment: "Equipped creature gets
-- +4/+0. / Whenever this Equipment becomes attached to a creature, tap that
-- creature. / Equip {2}" (Oracle text checked against Scryfall on 2026-09-05).
--
-- TWO untapped creatures, and the equip aimed at one of them. With a single
-- candidate every reading of "that creature" agrees -- the bearer's own host, the
-- only creature on the board, the equip's target -- and a tapped permanent would
-- be right by accident.
enormousEnergyBladeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
enormousEnergyBladeSpec s registry =
  Spec.describe s "CR 701.3a a trigger on becoming attached, read by the Equipment" $ do
    Spec.it s "CR 702.6a whole card: the equip taps the creature it went onto and not the other" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      maiden <- S.printingOf s registry "Bird Maiden"
      blade <- S.printingOf s registry "Enormous Energy Blade"
      let (pikerId, base1) = S.addPermanent piker S.alice (S.landsFor swamp S.alice 4 (Setup.emptyGame S.bothPlayers))
          (maidenId, base2) = S.addPermanent maiden S.alice base1
          (bladeId, base3) = S.addPermanent blade S.alice base2
          ready = base3 {GameState.priority = Just S.alice}
      case equipAbilityOf bladeId ready of
        Nothing -> Spec.assertFailure s "Enormous Energy Blade should offer rule 702.6a's minted equip ability"
        Just equip -> do
          -- The precondition the two assertions below rest on: S.addPermanent
          -- leaves what it places untapped, and a board that arrived tapped would
          -- make the first of them true for the fixture's reason.
          Spec.assertBool s (not (isTapped pikerId ready) && not (isTapped maidenId ready)) "both creatures start untapped"
          let activated = S.runPure (aimAtOffered pikerId) ready (Activate.activateAbility S.alice bladeId equip)
              equipped = S.runPure (aimAtOffered pikerId) activated Stack.resolveTop
              after = settleTriggers (aimAtOffered pikerId) equipped
          Spec.assertBool s (isTapped pikerId after) "CR 701.3a the creature the Blade went onto is tapped"
          Spec.assertBool s (not (isTapped maidenId after)) "and the other creature, which it did not, is untapped"
          -- Proxies, after the two above: without them the pair could pass on a
          -- board where no equip happened and nothing was ever going to tap.
          Spec.assertEqWith s "the Blade really is attached to the Piker" (attachmentsOn pikerId after) [bladeId]
          Spec.assertEqWith s "which CR 301.5f's +4/+0 confirms" (S.powerToughnessOf pikerId after) (Just (6, 1))

-- CR 701.3d's unattachment event, read by the attachment through
-- TriggerCondition.SelfBecomesUnattachedFrom.
--
-- Grafted Wargear, {3} Artifact -- Equipment: "Equipped creature gets +3/+2. /
-- Whenever this Equipment becomes unattached from a permanent, sacrifice that
-- permanent. / Equip {0}" (Oracle text checked against Scryfall on 2026-09-05).
--
-- THREE legs, because rule 701.3d reaches the same event by three roads in this
-- engine and each has its own emit site: the Equipment MOVING to another creature
-- (Pawl.Engine.Event.attach), the host becoming an illegal one (CR 704.5n, the
-- detach fold in Pawl.Engine.Sba), and the Equipment LEAVING THE BATTLEFIELD (the
-- zone-change funnel), which is also the leg CR 603.10c's look-back is for --
-- there the bearer is in a graveyard by the time its own trigger is gathered.
-- Deleting any one emit leaves the other two green. The fourth road, an unattach
-- instruction (Pawl.Engine.Event.detach), is data/scenarios/unattach's Disarm
-- scenario.
graftedWargearSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graftedWargearSpec s registry =
  Spec.describe s "CR 701.3d a trigger on becoming unattached" $ do
    -- CR 701.3a's move: equipping an Equipment that is already equipping
    -- something makes it cease to be attached to the old host.
    Spec.it s "CR 701.3a whole card: re-equipping onto another creature sacrifices the one it left" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      maiden <- S.printingOf s registry "Bird Maiden"
      wargear <- S.printingOf s registry "Grafted Wargear"
      let (pikerId, base1) = S.addPermanent piker S.alice (S.landsFor swamp S.alice 4 (Setup.emptyGame S.bothPlayers))
          (maidenId, base2) = S.addPermanent maiden S.alice base1
          (gearId, base3) = S.addPermanent wargear S.alice base2
          ready = base3 {GameState.priority = Just S.alice}
      case equipAbilityOf gearId ready of
        Nothing -> Spec.assertFailure s "Grafted Wargear should offer rule 702.6a's minted equip ability"
        Just equip -> do
          let onPiker = settleTriggers (aimAtOffered pikerId) (S.runPure (aimAtOffered pikerId) ready (Activate.activateAbility S.alice gearId equip >> Stack.resolveTop))
              moved = S.runPure (aimAtOffered maidenId) (onPiker {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice gearId equip >> Stack.resolveTop)
              after = settleTriggers (aimAtOffered maidenId) moved
          Spec.assertBool s (not (S.onBattlefield pikerId after)) "CR 701.3d the creature the Wargear left was sacrificed"
          Spec.assertBool s (S.onBattlefield maidenId after) "and the creature it moved onto was not"
          -- The other board this leg needs, one act different: the FIRST equip
          -- attached the Wargear to a creature that was attached to nothing, so
          -- no unattachment happened and nothing was sacrificed.
          Spec.assertBool s (S.onBattlefield pikerId onPiker) "the first equip, which came off nothing, sacrificed no one"
          Spec.assertEqWith s "and the Wargear really did move" (attachmentsOn maidenId after) [gearId]
    -- CR 704.5n: the host gains protection from artifacts, so the Equipment is
    -- attached to an illegal permanent and the state-based action detaches it.
    -- The host is still on the battlefield when the trigger resolves, which is
    -- what makes the sacrifice observable at all.
    Spec.it s "CR 704.5n whole card: a host that turns illegal is sacrificed as the Wargear falls off" $ do
      swamp <- S.printingOf s registry "Swamp"
      tower <- S.printingOf s registry "Tower of the Magistrate"
      piker <- S.printingOf s registry "Goblin Piker"
      wargear <- S.printingOf s registry "Grafted Wargear"
      let (pikerId, base1) = S.addPermanent piker S.alice (S.landsFor swamp S.alice 4 (Setup.emptyGame S.bothPlayers))
          (towerId, base2) = S.addPermanent tower S.alice base1
          (gearId, base3) = S.addPermanent wargear S.alice base2
          ready = base3 {GameState.priority = Just S.alice}
      case equipAbilityOf gearId ready of
        Nothing -> Spec.assertFailure s "Grafted Wargear should offer rule 702.6a's minted equip ability"
        Just equip -> do
          let onPiker = settleTriggers (aimAtOffered pikerId) (S.runPure (aimAtOffered pikerId) ready (Activate.activateAbility S.alice gearId equip >> Stack.resolveTop))
              protection = drop 1 (Projection.abilitiesOf towerId onPiker)
          case protection of
            [] -> Spec.assertFailure s "Tower of the Magistrate should print a protection ability beside its mana ability"
            grant : _ -> do
              let granted = S.runPure (aimAtOffered pikerId) (onPiker {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice towerId grant >> Stack.resolveTop)
                  after = settleTriggers (aimAtOffered pikerId) granted
              Spec.assertBool s (not (S.onBattlefield pikerId after)) "CR 701.3d the host the Wargear fell off was sacrificed"
              Spec.assertBool s (S.onBattlefield gearId after) "CR 704.5n and the Wargear itself stayed on the battlefield"
              -- The precondition: without the equip there is nothing to fall off,
              -- and the protection alone would leave the same board.
              Spec.assertBool s (S.onBattlefield pikerId onPiker) "the Piker was alive and equipped before the protection"

-- CR 613.1f layer 6, the TRIGGERED half of the grant: Sixth Sense ({G}
-- Enchantment -- Aura, "Enchant creature / Enchanted creature has 'Whenever this
-- creature deals combat damage to a player, you may draw a card.'", checked
-- against Scryfall on 2026-08-20) is the cheapest printing whose whole text box
-- is one quoted triggered ability, so nothing but the grant is under test.
--
-- Presence of Gond (Pawl.ActivateSpec) is the activated half of the same
-- Modification arm. What this group adds is the other side of the fold: a
-- granted ability has to be found by the CR 603.2 scan, not only by the
-- projection, and Pawl.Engine.Event.Trigger.eventTriggers reads
-- ProjectedCharacteristics.triggeredAbilities to do it.
--
-- Three seats, and the two that matter are DIFFERENT players: alice controls the
-- enchanted attacker, carol controls the Aura, bob is the defending player. CR
-- 113.7 makes the enchanted creature the granted ability's source and CR 603.3a
-- makes its controller the trigger's controller, so the "you" that draws is
-- alice. A granter-anchored reading would draw for carol, and the two hands are
-- what tell those readings apart -- one seat could not.
sixthSenseSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
sixthSenseSpec s registry = Spec.describe s "CR 613.1f a granted triggered ability" $ do
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
      (senseId, withAura) = S.addPermanent sense S.carol stocked
      board = case mine of
        [attackerId] | attached -> S.attach senseId attackerId withAura
        _ -> withAura
   in (mine, senseId, board)

-- The names in a player's hand, sorted. Names rather than a count, because the
-- two libraries hold different cards and which one moved is the question.
handNames :: PlayerId.PlayerId -> GameState.GameState -> [String]
handNames pid gs =
  List.sort
    (fmap (\oid -> Text.unpack (CardName.unwrap (S.soleFaceName oid gs))) (Game.zoneMembers Zone.Hand pid gs))

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
  -- Pawl.Engine.Resolve.Effect.putOntoBattlefield mirrors, against Event.tap -- so the only thing
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
-- Deeproot Pilgrimage {1}{U}: "Whenever one or more nontoken Merfolk you control
-- become tapped, create a 1/1 blue Merfolk creature token with hexproof."
-- Checked against Scryfall 2026-09-24. Two Merfolk Spies attack together, and
-- CR 508.1f taps them as one action, so the Pilgrimage fires once.
deeprootPilgrimageSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
deeprootPilgrimageSpec s registry =
  Spec.describe s "CR 603.2c a batch of permanents becoming tapped" . Spec.it s "CR 508.1f two Merfolk attacking together make one Deeproot Pilgrimage token" $ do
    spy <- S.printingOf s registry "Merfolk Spy"
    pilgrimage <- S.printingOf s registry "Deeproot Pilgrimage"
    let (board, attackers, _) = S.combatBoardOf [spy, spy] []
        (_, gs) = S.addPermanent pilgrimage S.alice board
        after = S.runCombat (S.attackTo S.bob) gs
    Spec.assertEqWith s "CR 603.2c one Merfolk token, not the two two events would make" (length (S.tokensOf after)) 1
    -- The precondition, AFTER the assertion above.
    Spec.assertEqWith s "CR 508.1f both Spies really became tapped" (fmap (`tapStatusOf` after) attackers) [Just TapState.Tapped, Just TapState.Tapped]

betrayalBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
betrayalBoard s registry attached = do
  piker <- S.printingOf s registry "Goblin Piker"
  betrayal <- S.printingOf s registry "Betrayal"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  let (base, mine, _, _) = S.threePlayerCombat [piker, piker] [] []
      stocked = Foldable.foldl' (\g _ -> snd (S.addLibraryCard island S.carol g)) (snd (S.addLibraryCard mountain S.alice base)) [1 :: Int, 2, 3]
      (auraId, withAura) = S.addPermanent betrayal S.carol stocked
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

-- The tap status of one object, Nothing where it is not on the board at all --
-- which a precondition assertion must be able to say apart from "untapped".
tapStatusOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStatusOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- Raid, and the question neither near-miss asks. CR 207.2c makes the ability word
-- itself meaningless, so "if you attacked this turn" is ordinary card text and CR
-- 603.4's intervening "if" is the whole mechanism: the trigger checks the
-- condition when the permanent enters, and CR 608.2a checks it again on
-- resolution.
--
-- Mardu Skullhunter {1}{B} Creature -- Human Warrior 2/1 (KTK, Oracle text
-- checked against Scryfall 2026-09-08): "This creature enters tapped. / Raid --
-- When this creature enters, if you attacked this turn, target opponent discards
-- a card." Quantity.AttackersDeclaredThisTurn compared against 1 is that clause.
--
-- ONE board separates the three readings, which is why it is built the way it is:
-- alice's Cabal Evangel attacks bob's Jace Beleren and is killed by his blocking
-- Hill Giant, and the Skullhunter is cast in the postcombat main phase.
--
--   * Quantity.OpponentsAttacked counts the OPPONENTS a declaration reached (CR
--     508.3b), and CR 506.3's planeswalker is not one, so it answers 0 here.
--   * Filter.AttackedThisTurn asks a CANDIDATE whether it attacked, so a count of
--     it over the battlefield answers 0 once the attacker has died.
--   * CR 608.2i's look-back over the turn's declarations answers 1, and bob
--     discards.
--
-- The control leg is the same board with the attack declined, which is the only
-- difference between the two: the Skullhunter still enters, and bob keeps both
-- cards.
marduSkullhunterSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
marduSkullhunterSpec s registry = Spec.describe s "MarduSkullhunter" $ do
  Spec.it s "CR 603.4 raid: an attack aimed at a planeswalker by a creature that then died still discards" $ do
    built <- S.buildBoardOrFail s registry raidBoard
    case (aliasIn "jace" built, aliasIn "attacker" built) of
      (Just jaceId, Just attackerId) -> do
        let atBlockers = S.runPure (aimAtPlaneswalker jaceId) (Staged.state built) Engine.runStep
            after = S.runPure (aimAtPlaneswalker jaceId) atBlockers (Monad.replicateM_ 4 Engine.runStep)
        Spec.assertEqWith s "CR 603.4 / 608.2i: bob discarded, so raid saw the declaration" (S.handSize S.bob after) 1
        Spec.assertEqWith s "and it really was announced at the planeswalker (CR 508.1b), where OpponentsAttacked counts 0" (Map.lookup attackerId (Combat.Type.attackers (GameState.combat atBlockers))) (Just (AttackTarget.OfPlaneswalker jaceId))
        Spec.assertBool s (not (S.onBattlefield attackerId after)) "and the attacker was dead by then, where a Count of Filter.AttackedThisTurn over the battlefield reads 0"
      _ -> Spec.assertFailure s "fixture should alias bob's Jace and alice's attacker"

-- alice at the declare attackers step with one 2/2 to attack with, two Swamps for
-- the {1}{B}, and the Skullhunter in hand; bob with a Jace Beleren to be attacked,
-- a 3/3 to block with, and two cards to lose one of. Distinct values throughout:
-- the blocker outlives the attacker it kills, and bob's hand goes 2 to 1.
raidBoard :: Board.Board
raidBoard =
  S.board
    ( (S.battlefield S.alice [S.settled "attacker" "Cabal Evangel", S.permanent "Swamp", S.permanent "Swamp"])
        { Seat.hand = Seq.singleton (S.aliased "skullhunter" (S.cardSetup "Mardu Skullhunter"))
        }
        NonEmpty.:| [ (S.battlefield S.bob [S.aliased "jace" (S.permanent "Jace Beleren"), S.settled "blocker" "Hill Giant"])
                        { Seat.hand = Seq.fromList [S.cardSetup "Forest", S.cardSetup "Mountain"]
                        }
                    ]
    )
    S.alice
    S.declareAttackers

aliasIn :: String -> Staged.Staged -> Maybe ObjectId.ObjectId
aliasIn name built = Map.lookup (Label.MkLabel (Text.pack name)) (Staged.objects built)

-- S.fightAnswer with CR 508.1b's announcement aimed at the planeswalker rather
-- than at bob. Everything else is shared with the control answerer below, so the
-- two boards differ in the declaration alone.
aimAtPlaneswalker :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtPlaneswalker jaceId p = case p of
  Prompt.ChooseAttackTarget _ _ _ options ->
    Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlaneswalker jaceId) (NonEmpty.toList options))
  _ -> S.fightAnswer p

-- CR 702.110b's "a creature" read as an OBJECT rather than as an occurrence: the
-- creature a rule 702.110a sacrifice fed to the exploiter, bound under
-- Pawl.Engine.Binding.exploitedCreature and compared against by
-- Filter.ToughnessLessThanBound (CR 208.1).
--
-- Profaner of the Dead {3}{U} Creature -- Snake Wizard 3/3 is the card, whole:
-- "Exploit / When this creature exploits a creature, return to their owners'
-- hands all creatures your opponents control with toughness less than the
-- exploited creature's toughness" (name, cost, type line and Oracle text checked
-- against api.scryfall.com, 2026-09-18).
--
-- TWO BOARDS DIFFERING IN THE SACRIFICE ALONE, which is what makes the bound
-- creature's IDENTITY the thing under test: alice may exploit a Hill Giant
-- (toughness 3) or a Goblin Piker (toughness 1), and the opponents' board is the
-- same either way. Under the Giant the toughness-1 and toughness-2 creatures go
-- to hand and the toughness-4 one stays; under the Piker nothing moves at all,
-- which is also where rule 208.1's comparison being STRICT is visible -- a
-- toughness-1 creature is not under a toughness of 1.
--
-- THREE SEATS, so "your opponents control" cannot collapse onto one player, and
-- alice keeps a toughness-1 creature of her own through both legs: it is under
-- the Giant's toughness and stays, which is the clause's "your opponents" half.
profanerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
profanerSpec s registry =
  let -- Takes rule 702.110a's offer and sacrifices the NAMED candidate, pinned by
      -- id: the Profaner and alice's other creatures are all offered, so a fixture
      -- taking the first would prove nothing about which creature the slot bound.
      exploiting :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      exploiting victim p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChoosePermanent _ _ _ offered ->
          Maybe.fromMaybe (NonEmpty.head offered) (List.find (== victim) (NonEmpty.toList offered))
        _ -> S.identityAnswer p
      -- The Profaner on the stack over the shared board: alice holds the two
      -- creatures either leg may sacrifice, bob three of distinct toughness and
      -- carol one more, so every toughness the comparison has to separate is on
      -- the board at once.
      board = do
        profaner <- S.printingOf s registry "Profaner of the Dead"
        piker <- S.printingOf s registry "Goblin Piker"
        giant <- S.printingOf s registry "Hill Giant"
        gnat <- S.printingOf s registry "Gnat Miser"
        evangel <- S.printingOf s registry "Cabal Evangel"
        warrior <- S.printingOf s registry "Hollow Warrior"
        mauler <- S.printingOf s registry "Dwarven Mauler"
        let base = Setup.emptyGame S.threePlayers
            (pikerId, withPiker) = S.addPermanent piker S.alice base
            (giantId, withGiant) = S.addPermanent giant S.alice withPiker
            (gnatId, withGnat) = S.addPermanent gnat S.bob withGiant
            (evangelId, withEvangel) = S.addPermanent evangel S.bob withGnat
            (warriorId, withWarrior) = S.addPermanent warrior S.bob withEvangel
            (maulerId, withMauler) = S.addPermanent mauler S.carol withWarrior
            (_, staged) = S.spellOnStack profaner S.alice withMauler
        pure (pikerId, giantId, gnatId, evangelId, warriorId, maulerId, staged)
      -- The Profaner resolves, rule 702.110a's entry trigger goes on the stack and
      -- resolves, and rule 702.110b's event then puts the printed trigger on and
      -- resolves it too.
      played :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      played answer staged =
        S.runPure
          answer
          staged
          ( Stack.resolveTop
              >> Engine.settleForPriority
              >> Stack.resolveTop
              >> Engine.settleForPriority
              >> Stack.resolveTop
          )
   in Spec.describe s "Profaner of the Dead" $ do
        Spec.it s "CR 702.110b bounces the opponents' creatures under the exploited creature's toughness" $ do
          (pikerId, giantId, gnatId, evangelId, warriorId, maulerId, staged) <- board
          let after = played (exploiting giantId) staged
          Spec.assertBool s (not (S.onBattlefield gnatId after)) "CR 208.1 bob's toughness-1 creature is under the exploited Giant's 3 and went to hand"
          Spec.assertBool s (not (S.onBattlefield evangelId after)) "and bob's toughness-2 creature did too"
          Spec.assertBool s (not (S.onBattlefield maulerId after)) "and carol's toughness-1 creature, the other opponent's"
          Spec.assertBool s (S.onBattlefield warriorId after) "CR 208.1 bob's toughness-4 creature is not under 3 and stayed"
          Spec.assertBool s (S.onBattlefield pikerId after) "and alice's own toughness-1 creature stayed, the clause reaching opponents only"
          Spec.assertEqWith s "the two bob lost are in his hand" (S.handSize S.bob after) 2
          Spec.assertBool s (not (S.onBattlefield giantId after)) "CR 702.110a the Hill Giant alice chose really was sacrificed"
        -- The paired board, differing in the sacrifice and nothing else: a
        -- toughness of 1 leaves rule 208.1's strict comparison with nothing under
        -- it, so the same trigger fires and moves nobody.
        Spec.it s "CR 702.110b exploiting a toughness-1 creature bounces nothing" $ do
          (pikerId, giantId, gnatId, evangelId, warriorId, maulerId, staged) <- board
          let after = played (exploiting pikerId) staged
          Spec.assertBool s (S.onBattlefield gnatId after) "CR 208.1 bob's toughness-1 creature is not under a toughness of 1"
          Spec.assertBool s (S.onBattlefield evangelId after) "nor is bob's toughness-2 creature"
          Spec.assertBool s (S.onBattlefield warriorId after) "nor bob's toughness-4 one"
          Spec.assertBool s (S.onBattlefield maulerId after) "nor carol's toughness-1 creature"
          Spec.assertEqWith s "bob's hand is empty" (S.handSize S.bob after) 0
          Spec.assertBool s (S.onBattlefield giantId after) "and the Hill Giant alice did not choose stayed"
          Spec.assertBool s (not (S.onBattlefield pikerId after)) "CR 702.110a the Goblin Piker she did choose was sacrificed"

-- CR 702.110b read by a bystander: TriggerCondition.CreatureExploits puts the
-- exploiter and the exploited creature to a Filter each, both read off last
-- known information since either may be the creature sacrificed.
--
-- Skull Skaab {U}{B} Creature -- Zombie 2/2, "Exploit / Whenever a creature you
-- control exploits a nontoken creature, create a 2/2 black Zombie creature
-- token"; Colonel Autumn {1}{W}{B} Legendary Creature -- Human Soldier 2/3,
-- "Lifelink / Exploit / Other legendary creatures you control have exploit. /
-- Whenever a creature you control exploits a creature, put a +1/+1 counter on
-- each creature you control"; Henry Wu, InGen Geneticist {B}{G}{U} Legendary
-- Creature -- Human Scientist 1/4, "Henry Wu and other Human creatures you
-- control have exploit. / Whenever a creature you control exploits a non-Human
-- creature, draw a card. If the exploited creature had power 3 or greater,
-- create a Treasure token" (all checked against api.scryfall.com, 2026-09-28).
--
-- THREE SEATS throughout, the fixture's convention, and every negative is the
-- paired board of a positive, differing in the one choice or seat named.
creatureExploitsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
creatureExploitsSpec s registry =
  let -- Takes rule 702.110a's offer and sacrifices the first offered creature the
      -- predicate admits, and aims any Qarsi Sadist trigger at its first legal
      -- player.
      exploiting :: (ObjectId.ObjectId -> Bool) -> Prompt.Prompt r -> r
      exploiting wanted p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChoosePermanent _ _ _ offered ->
          Maybe.fromMaybe (NonEmpty.head offered) (List.find wanted (NonEmpty.toList offered))
        Prompt.ChooseTargets _ _ _ slots -> fmap (\(_, legal) -> Set.take 1 legal) slots
        _ -> S.identityAnswer p
      -- Resolves the spell and then every trigger it leads to, settling between
      -- each so CR 603.3 puts the next one on. Bounded, so a stack that never
      -- empties fails the assertions rather than hanging.
      drain :: Int -> Game.Type.Game ()
      drain n = Monad.when (n > 0) $ do
        stack <- State.gets GameState.stack
        Monad.unless (null stack) (Stack.resolveTop >> Engine.settleForPriority >> drain (n - 1))
      played :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      played answer staged = S.runPure answer staged (drain 8)
      named text = CardName.MkCardName (Text.pack text)
      zombies = S.countOnBattlefieldByName (named "Zombie Token") S.alice
      -- Skull Skaab on the stack over a Hill Giant and a Goblin Piker TOKEN of
      -- alice's: either may be exploited, and so may the Skaab itself.
      skaabBoard = do
        skaab <- S.printingOf s registry "Skull Skaab"
        giant <- S.printingOf s registry "Hill Giant"
        piker <- S.printingOf s registry "Goblin Piker"
        let base = Setup.emptyGame S.threePlayers
            (giantId, withGiant) = S.addPermanent giant S.alice base
            (tokenId, withToken) = S.addToken (Printing.card piker) S.alice withGiant
            (_, staged) = S.spellOnStack skaab S.alice withToken
        pure (giantId, tokenId, staged)
      -- Skull Skaab already on alice's battlefield, and a Qarsi Sadist on the
      -- stack over a Hill Giant, both of `caster`'s.
      bystanderBoard caster = do
        skaab <- S.printingOf s registry "Skull Skaab"
        sadist <- S.printingOf s registry "Qarsi Sadist"
        giant <- S.printingOf s registry "Hill Giant"
        let base = Setup.emptyGame S.threePlayers
            (skaabId, withSkaab) = S.addPermanent skaab S.alice base
            (giantId, withGiant) = S.addPermanent giant caster withSkaab
            (_, staged) = S.spellOnStack sadist caster withGiant
        pure (skaabId, giantId, staged)
      -- Henry Wu on alice's battlefield over a Hill Giant (3 power), a Goblin
      -- Piker (2) and a Cabal Evangel (a Human), with a second Cabal Evangel on
      -- the stack that his first ability gives exploit. Her library is stocked
      -- for the draw.
      henryBoard = do
        henry <- S.printingOf s registry "Henry Wu, InGen Geneticist"
        giant <- S.printingOf s registry "Hill Giant"
        piker <- S.printingOf s registry "Goblin Piker"
        evangel <- S.printingOf s registry "Cabal Evangel"
        let base = Setup.emptyGame S.threePlayers
            (_, withHenry) = S.addPermanent henry S.alice base
            (giantId, withGiant) = S.addPermanent giant S.alice withHenry
            (pikerId, withPiker) = S.addPermanent piker S.alice withGiant
            (humanId, withHuman) = S.addPermanent evangel S.alice withPiker
            (_, stocked) = S.addLibraryCard piker S.alice (snd (S.addLibraryCard piker S.alice withHuman))
            (_, staged) = S.spellOnStack evangel S.alice stocked
        pure (giantId, pikerId, humanId, staged)
      treasures = S.countOnBattlefieldByName (named "Treasure Token") S.alice
   in Spec.describe s "CreatureExploits" $ do
        Spec.it s "CR 702.110b Skull Skaab exploiting a nontoken creature makes a Zombie" $ do
          (giantId, tokenId, staged) <- skaabBoard
          let after = played (exploiting (== giantId)) staged
          Spec.assertEqWith s "CR 702.110b alice got one Zombie" (zombies after) 1
          Spec.assertBool s (not (S.onBattlefield giantId after)) "CR 702.110a the Hill Giant was the creature sacrificed"
          Spec.assertBool s (S.onBattlefield tokenId after) "and the token stayed"
        -- The paired board, differing in the victim alone: a token fails "a
        -- nontoken creature", read off last known information since CR 111.7's
        -- token has ceased to exist by the time the trigger is asked.
        Spec.it s "CR 702.110b Skull Skaab exploiting a token makes nothing" $ do
          (giantId, tokenId, staged) <- skaabBoard
          let after = played (exploiting (== tokenId)) staged
          Spec.assertEqWith s "CR 111.1 no Zombie for a token" (zombies after) 0
          Spec.assertBool s (not (S.onBattlefield tokenId after)) "CR 702.110a the token was the creature sacrificed"
          Spec.assertBool s (S.onBattlefield giantId after) "and the Hill Giant stayed"
        -- The Colonel Autumn ruling: a creature that exploits ITSELF still
        -- triggers, which is CR 603.10a's look-back at the sacrifice.
        Spec.it s "CR 702.110b Skull Skaab exploiting itself still triggers" $ do
          (giantId, tokenId, staged) <- skaabBoard
          let after = played (exploiting (\oid -> oid /= giantId && oid /= tokenId)) staged
          Spec.assertEqWith s "CR 603.10a alice got one Zombie off the Skaab's own sacrifice" (zombies after) 1
          Spec.assertEqWith s "CR 702.110a the Skaab itself was sacrificed" (S.countOnBattlefieldByName (named "Skull Skaab") S.alice after) 0
          Spec.assertBool s (S.onBattlefield giantId after) "and the Hill Giant stayed"
        Spec.it s "CR 702.110b another creature alice controls exploiting triggers the Skaab" $ do
          (skaabId, giantId, staged) <- bystanderBoard S.alice
          let after = played (exploiting (== giantId)) staged
          Spec.assertEqWith s "CR 702.110b alice got one Zombie off the Sadist's exploit" (zombies after) 1
          Spec.assertBool s (S.onBattlefield skaabId after) "and the Skaab watched from the battlefield"
          Spec.assertBool s (not (S.onBattlefield giantId after)) "CR 702.110a the Hill Giant was the creature sacrificed"
        -- The paired board, differing in the seat that casts the Sadist and
        -- controls its victim: bob's exploiter is not "a creature you control".
        Spec.it s "CR 702.110b an opponent's creature exploiting does not trigger the Skaab" $ do
          (skaabId, giantId, staged) <- bystanderBoard S.bob
          let after = played (exploiting (== giantId)) staged
          Spec.assertEqWith s "CR 109.5 alice got no Zombie off bob's exploit" (zombies after) 0
          Spec.assertBool s (S.onBattlefield skaabId after) "and the Skaab was there to see it"
          Spec.assertBool s (not (S.onBattlefield giantId after)) "CR 702.110a bob's Hill Giant really was exploited"
        -- Henry Wu grants the exploit, and the exploited creature's power is read
        -- off last known information by the Treasure clause.
        Spec.it s "CR 702.110b Henry Wu draws and makes a Treasure off a power-3 non-Human" $ do
          (giantId, _, _, staged) <- henryBoard
          let after = played (exploiting (== giantId)) staged
          Spec.assertEqWith s "CR 608.2h the Hill Giant had power 3, so a Treasure" (treasures after) 1
          Spec.assertEqWith s "CR 702.110b and alice drew one card" (S.handSize S.alice after) 1
          Spec.assertBool s (not (S.onBattlefield giantId after)) "CR 702.110a the Hill Giant was the creature sacrificed"
        Spec.it s "CR 702.110b Henry Wu draws but makes no Treasure off a power-2 non-Human" $ do
          (_, pikerId, _, staged) <- henryBoard
          let after = played (exploiting (== pikerId)) staged
          Spec.assertEqWith s "CR 608.2h the Goblin Piker had power 2, so no Treasure" (treasures after) 0
          Spec.assertEqWith s "CR 702.110b but alice drew one card" (S.handSize S.alice after) 1
          Spec.assertBool s (not (S.onBattlefield pikerId after)) "CR 702.110a the Goblin Piker was the creature sacrificed"
        Spec.it s "CR 702.110b Henry Wu ignores an exploited Human" $ do
          (_, _, humanId, staged) <- henryBoard
          let after = played (exploiting (== humanId)) staged
          Spec.assertEqWith s "CR 205.3m exploiting a Human draws nothing" (S.handSize S.alice after) 0
          Spec.assertBool s (not (S.onBattlefield humanId after)) "CR 702.110a the Cabal Evangel was the creature sacrificed"
        -- Colonel Autumn's grant reaches a legendary creature entering, and her
        -- trigger reads "a creature" with no further narrowing.
        Spec.it s "CR 702.110b Colonel Autumn counters every creature alice controls" $ do
          autumn <- S.printingOf s registry "Colonel Autumn"
          jedit <- S.printingOf s registry "Jedit Ojanen"
          giant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          let base = Setup.emptyGame S.threePlayers
              (autumnId, withAutumn) = S.addPermanent autumn S.alice base
              (giantId, withGiant) = S.addPermanent giant S.alice withAutumn
              (pikerId, withPiker) = S.addPermanent piker S.alice withGiant
              (_, staged) = S.spellOnStack jedit S.alice withPiker
              after = played (exploiting (== giantId)) staged
          Spec.assertEqWith s "CR 702.110b the Colonel got a +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne autumnId after) 1
          Spec.assertEqWith s "and so did the Goblin Piker" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 1
          Spec.assertBool s (not (S.onBattlefield giantId after)) "CR 702.110a Jedit Ojanen's granted exploit sacrificed the Hill Giant"

-- Fire Lord Zuko {R}{W}{B} Legendary Creature -- Ally Human Noble 2/4,
-- "Firebending X, where X is Fire Lord Zuko's power. / Whenever you cast a spell
-- from exile and whenever a permanent you control enters from exile, put a
-- +1/+1 counter on each creature you control." Written as two triggered
-- abilities, one per trigger event: no cast is also an entry, so they never
-- fire on the same event. The entry half is Breathless Knight's intervening
-- EnteredFrom, which reads the event log and so answers the same at resolution
-- (Pawl.Engine.Quantity's EnteredFrom arm). Nothing is omitted, so pawl's Zuko
-- is neither stricter nor weaker than printed.
fireLordZukoSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
fireLordZukoSpec s registry =
  let named name gs = filter (\o -> fmap S.nameOf (Game.cardOf o gs) == Just (CardName.MkCardName (Text.pack name))) (Set.toList (GameState.battlefield gs))
      step gs action = S.runPure S.identityAnswer gs (action >> Engine.settleForPriority)
   in Spec.describe s "Fire Lord Zuko" $ do
        -- Embereth Shieldbreaker's Adventure is cast from HAND, and the creature
        -- then cast from EXILE enters from the stack: of the three events only the
        -- second is either trigger's, so Zuko gets one counter and the Knight,
        -- entering after the trigger resolved, none.
        Spec.it s "a spell cast from exile puts a counter on each creature you control" $ do
          zuko <- S.printingOf s registry "Fire Lord Zuko"
          shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
          mountain <- S.printingOf s registry "Mountain"
          bonesplitter <- S.printingOf s registry "Bonesplitter"
          let (zukoId, withZuko) = S.addPermanent zuko S.alice (S.landsInPlay mountain 3)
              (_, board) = S.addPermanent bonesplitter S.alice withZuko
              (gs, oid) = S.handOne shieldbreaker board
              adventured = step (step gs (Cast.castSpell S.manaPerformer S.alice oid (CardName.MkCardName (Text.pack "Battle Display")) Facing.FaceUp)) Stack.resolveTop
          case Game.zoneMembers Zone.Exile S.alice adventured of
            [exiledId] -> do
              let recast = step adventured (Cast.castSpell S.manaPerformer S.alice exiledId (CardName.MkCardName (Text.pack "Embereth Shieldbreaker")) Facing.FaceUp)
                  after = step (step recast Stack.resolveTop) Stack.resolveTop
              Spec.assertEqWith
                s
                "the cast from exile gave Zuko one counter, and the Knight none"
                (S.powerToughnessOf zukoId after, fmap (`S.powerToughnessOf` after) (named "Embereth Shieldbreaker" after))
                (Just (3, 5), [Just (2, 1)])
            other -> Spec.assertFailure s ("expected one card on an adventure, got " <> show (length other))

-- Leyline Phantom ({4}{U} Creature -- Illusion 5/5, "When this creature deals
-- combat damage, return it to its owner's hand.", Oracle text checked against
-- Scryfall 2026-09-30): CR 510.1c sends a blocked attacker's damage to its
-- blockers, and CR 510.2 deals the step's damage as one event, so damage to two
-- blockers is one trigger event (CR 603.2c), never one per recipient. Two Goblin
-- Pikers block it, so no player is dealt anything.
leylinePhantomSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
leylinePhantomSpec s registry = Spec.describe s "Leyline Phantom" $ do
  Spec.it s "CR 510.2 / 603.2c combat damage to two blockers triggers it once" $ do
    phantom <- S.printingOf s registry "Leyline Phantom"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.combatBoardOf [phantom] [piker, piker] of
      (gs0, [phantomId], [first, second]) -> do
        let blocked = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer gs0
            -- One damage to each blocker but the first, the rest to the first:
            -- CR 510.1c's division, so both are dealt damage.
            dividing :: Prompt.Prompt r -> r
            dividing p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n -> case Map.keys thresholds of
                k : rest -> Map.fromList ((k, n - List.genericLength rest) : fmap (\r -> (r, 1)) rest)
                [] -> thresholds
              _ -> S.identityAnswer p
            dealt = S.runPure dividing blocked (Monad.void Damage.dealCombatDamage >> Engine.settleForPriority)
            resolved = S.runPure S.identityAnswer dealt Stack.resolveTop
        Spec.assertEqWith s "CR 603.2c one trigger for the step, not one per blocker" (length (GameState.stack dealt)) 1
        Spec.assertEqWith s "CR 510.1c both blockers were dealt combat damage" (S.onBattlefield first dealt, S.onBattlefield second dealt) (False, False)
        Spec.assertBool s (not (S.onBattlefield phantomId resolved)) "the Phantom left the battlefield"
        Spec.assertEqWith s "and is in alice's hand" (S.handSize S.alice resolved) 1
      _ -> Spec.assertFailure s "fixture should have one attacker and two blockers"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  fireLordZukoSpec s registry
  profanerSpec s registry
  creatureExploitsSpec s registry
  anafenzaAttackSpec s registry
  boggartPranksterSpec s registry
  avatarRokuSpec s registry
  everWatchingThresholdSpec s registry
  tyvarAttackSpec s registry
  lightmineFieldSpec s registry
  screamingSwarmSpec s registry
  littjaraKinseekersSpec s registry
  seiferSpec s registry
  luluSpec s registry
  nornsDecreeSpec s registry
  mirkwoodTrapperSpec s registry
  archnemesisSpec s registry
  ezuriExperienceSpec s registry
  savantiRomeroSpec s registry
  monarchTriggerSpec s registry
  matoyaTriggerSpec s registry
  feywildTricksterSpec s registry
  karplusanMinotaurSpec s registry
  aloeAlchemistSpec s registry
  wildgrowthWalkerSpec s registry
  raffinesInformantSpec s registry
  raffineSchemingSeerSpec s registry
  ironMongerSpec s registry
  spymastersVaultSpec s registry
  rayOfCommandSpec s registry
  brambleElementalSpec s registry
  enormousEnergyBladeSpec s registry
  graftedWargearSpec s registry
  sixthSenseSpec s registry
  betrayalSpec s registry
  deeprootPilgrimageSpec s registry
  marduSkullhunterSpec s registry
  leylinePhantomSpec s registry
