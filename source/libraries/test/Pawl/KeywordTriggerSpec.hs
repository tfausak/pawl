{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Keyword's triggered abilities -- the CR 702 keywords whose rule
-- text IS a trigger -- gathered by the same Pawl.Engine.Event scan a printed
-- trigger goes through, plus the CR 508/509 combat declarations several of them
-- ride on. The machinery is Pawl.TriggerSpec.
module Pawl.KeywordTriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event.Binding as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection.View
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural.Extra
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- CR 702.70: poisonous -- the first keyword whose rule text IS a triggered
-- ability, so it is minted by Pawl.Engine.Keyword and gathered by the same
-- Pawl.Engine.Event.Trigger.eventTriggers scan a printed trigger goes through, with the damaged
-- player carried across in the reserved Binding.triggerPlayer slot.
poisonousSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
poisonousSpec s registry =
  let -- Hang `n` Auras off `host`, each owned by alice. Attached directly rather
      -- than cast: the cast path is proved once, by the whole-card test below.
      hang printing n host gs =
        List.foldl'
          (\g _ -> let (aura, g1) = S.addPermanent printing S.alice g in S.attach aura host g1)
          gs
          (replicate n ())
      -- alice attacks with one `attacking` creature wearing `n` copies of the
      -- `aura`; bob defends with one creature per printing in `theirs`.
      board attacking aura n theirs = case S.combatBoardOf [attacking] theirs of
        (gs, attacker : _, blockers) -> Just (hang aura n attacker gs, attacker, blockers)
        _ -> Nothing
   in Spec.describe s "Poisonous" $ do
        -- CR 702.70b: "If a creature has multiple instances of poisonous, each
        -- triggers separately." So the count is a MULTIPLICITY, not a sum --
        -- the opposite of CR 702.164b's toxic, which sums its N values into one
        -- rider. The falsifier is a mint that collapses the count to one
        -- ability.
        Spec.it s "CR 702.70b each instance of poisonous is its own ability" $ do
          Spec.assertEqWith s "poisonous 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Poisonous 1) 2)) [Keyword.poisonous 1, Keyword.poisonous 1]
          Spec.assertEqWith s "and poisonous 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Poisonous 3) 1)) [Keyword.poisonous 3]
        -- Rule 702.70 is the only keyword in the pool that mints an ability;
        -- every other one is read where it matters (Projection.hasKeyword, the
        -- infect/toxic damage riders), so it must mint nothing here.
        Spec.it s "CR 702.164 toxic mints no triggered ability" $ do
          Spec.assertEqWith s "toxic is a damage rider, not a trigger" (Keyword.triggeredAbilitiesOf (Map.fromList [(Keyword.Type.Toxic 2, 1), (Keyword.Type.Flying, 1), (Keyword.Type.Infect, 1)])) []
        -- CR 702.70a's "that player": the trigger's own event names them, and
        -- the scan stamps them under the reserved slot as it gathers. The
        -- falsifier is an implementation that hands the poison to the ability's
        -- controller (Binding.you) instead.
        Spec.it s "CR 603.2 the damaged player rides the trigger in the reserved slot" $ do
          let ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 7) (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing Nothing mempty False DamageKind.Combat)
              bindings = Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing Map.empty S.alice (TriggerCondition.SelfDealsCombatDamageToPlayer PlayerRelation.AnyPlayer) ev
          Spec.assertEqWith s "bob is bound under thatPlayer" (Binding.targetsOf bindings) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.bob)))
        -- The proving test. CR 702.70a: "Whenever this creature deals combat
        -- damage to a player, that player gets N poison counters." bob is dealt
        -- the Piker's two damage AND gets three poison -- poisonous is not
        -- infect (CR 702.90b), so the life still goes.
        Spec.it s "CR 702.70a Snake Cult Initiation gives the damaged player three poison" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case board piker initiation 1 [] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs, attacker, _) -> do
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker gs) "the enchanted creature has poisonous 3"
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob has three poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
              Spec.assertEqWith s "and lost the two life as well" (S.lifeOf S.bob after) (Just 18)
              Spec.assertEqWith s "alice, who controls the ability, gets none" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
        -- What separates poisonous from infect and toxic: it is a TRIGGERED
        -- ability, so the poison arrives when the ability resolves, not as the
        -- damage is dealt. `fightWith` deals combat damage without ever reaching
        -- a priority boundary, so nothing has been gathered yet.
        Spec.it s "CR 702.70a the poison rides the stack, not the damage" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case board piker initiation 1 [] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs, _, _) -> do
              let fought = S.fightWith S.aggressiveAnswer gs
              Spec.assertEqWith s "damage is dealt" (S.lifeOf S.bob fought) (Just 18)
              Spec.assertEqWith s "but no poison until the trigger resolves" (S.playerCounterOf PlayerCounterKind.Poison S.bob fought) 0
        -- CR 702.70b at the board level: two Auras are two poisonous 3
        -- abilities, so two triggers and six counters. The falsifier is a
        -- projection that keeps keywords in a set -- the second grant collapses
        -- into the first and bob takes three.
        Spec.it s "CR 702.70b two Snake Cult Initiations trigger separately for six poison" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case board piker initiation 2 [] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs, _, _) -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob has six poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 6
        -- CR 702.70a is scoped to combat damage dealt TO A PLAYER: a blocked
        -- creature deals its damage to the blocker, so the ability never
        -- triggers and the blocker (not being a player) gets nothing either.
        Spec.it s "CR 702.70a a blocked creature poisons nobody" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case board piker initiation 1 [piker] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs, _, _) -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0
              Spec.assertEqWith s "and lost no life" (S.lifeOf S.bob after) (Just 20)
        -- CR 613.1f / 613 layer 6: the ability is derived from the POST-LAYER
        -- keywords, so Humility's LoseAllAbilities (a later timestamp, so it
        -- applies after the Aura's grant) takes it away with no arm of its own.
        -- The falsifier is a mint that reads the PRINTED keywords or the Aura's
        -- own static ability instead of the projection.
        Spec.it s "CR 613 Humility strips poisonous along with everything else" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          humility <- S.printingOf s registry "Humility"
          case board piker initiation 1 [] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs0, attacker, _) -> do
              let gs = S.withHumility humility gs0
              Spec.assertBool s (not (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker gs)) "the keyword is gone"
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "so bob takes no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0
              Spec.assertEqWith s "only the 1/1's one damage" (S.lifeOf S.bob after) (Just 19)
        -- CR 702.70a's "that player" is whoever was DEALT the damage. In a
        -- multiplayer game (CR 800.1) that is not derivable from the ability's
        -- controller, since CR 506.2a has the attacking player choose which
        -- opponent becomes the defending player. The two runs differ only in
        -- the answer to
        -- Prompt.ChooseDefender, so a "give it to the opponent" implementation
        -- cannot pass both.
        Spec.it s "CR 702.70a the poison follows whichever opponent was attacked" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case S.threePlayerCombat [piker] [] [] of
            (_, [], _, _) -> Spec.assertFailure s "fixture should have an attacker"
            (base, attacker : _, _, _) -> do
              let gs = hang initiation 1 attacker base
                  hitBob = S.runCombat (S.attackTo S.bob) gs
                  hitCarol = S.runCombat (S.attackTo S.carol) gs
              Spec.assertEqWith s "bob, attacked, has three poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob hitBob) 3
              Spec.assertEqWith s "carol, untouched, has none" (S.playerCounterOf PlayerCounterKind.Poison S.carol hitBob) 0
              Spec.assertEqWith s "and the other way round" (S.playerCounterOf PlayerCounterKind.Poison S.carol hitCarol) 3
              Spec.assertEqWith s "bob untouched this time" (S.playerCounterOf PlayerCounterKind.Poison S.bob hitCarol) 0
        -- The whole card, through the real cast path (design.md section 4): pay
        -- {3}{B}, target the Piker, let the Aura enter attached (CR 303.4), then
        -- attack. Everything above hangs the Aura on by fiat.
        Spec.it s "CR 702.70 whole card: cast Snake Cult Initiation, attack, and bob is poisoned" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          swamp <- S.printingOf s registry "Swamp"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case S.combatBoardOf [piker] [] of
            (_, [], _) -> Spec.assertFailure s "fixture should have an attacker"
            (gs0, attacker : _, _) -> do
              let withSwamps = List.foldl' (\g _ -> snd (S.addPermanent swamp S.alice g)) gs0 (replicate 4 ())
                  (spellId, inHand) = S.addHandCard initiation S.alice withSwamps
                  cast = S.runPure S.aggressiveAnswer inHand {GameState.priority = Just S.alice} (S.cast S.alice spellId)
                  resolved = S.runPure S.aggressiveAnswer cast Stack.resolveTop
                  after = S.runCombat S.aggressiveAnswer resolved
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker resolved) "the Aura granted poisonous 3"
              Spec.assertEqWith s "bob has three poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
              Spec.assertEqWith s "and took the Piker's two" (S.lifeOf S.bob after) (Just 18)

-- CR 702.115a: "Ingest is a triggered ability. 'Ingest' means 'Whenever this
-- creature deals combat damage to a player, that player exiles the top card of
-- their library.'" Poisonous' condition and poisonous' reserved "that player"
-- slot over a different payload, so what is new here is the PAYLOAD: a zone move
-- whose source is a library nobody targeted.
--
-- Culling Drone is the card -- {1}{B} 2/2 with devoid and ingest and nothing
-- else, so nothing else on it can produce the exile the assertions read.
--
-- Every board stocks the libraries with TWO DISTINCT printings, top and second,
-- and reads the exile zone by NAME. That is what tells "the top card" apart from
-- "a card": an implementation that exiled the bottom, or two, or the wrong
-- player's, puts a different name in exile rather than the same one.
ingestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ingestSpec s registry =
  let -- addLibraryCard puts each card ON TOP, so the deeper card is added first
      -- and `top` ends up as CR 401.2's head.
      stock deeper top pid gs = snd (S.addLibraryCard top pid (snd (S.addLibraryCard deeper pid gs)))
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      nameOfCard = CardName.MkCardName . Text.pack
   in Spec.describe s "Ingest" $ do
        -- CR 702.115b: "If a creature has multiple instances of ingest, each
        -- triggers separately." So the count is a MULTIPLICITY, poisonous'
        -- reading rather than shadow's redundancy. The falsifier is a mint that
        -- collapses the count to one ability.
        Spec.it s "CR 702.115b each instance of ingest is its own ability" $ do
          Spec.assertEqWith s "ingest held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Ingest 2)) [Keyword.ingest, Keyword.ingest]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Ingest 1)) [Keyword.ingest]
        -- The proving test. Unblocked combat damage to bob exiles bob's top card
        -- and leaves the card under it where it was. alice's library is stocked
        -- with two OTHER printings and is asserted untouched, which is what
        -- separates rule 702.115a's "that player" from the ability's controller:
        -- a payload built on Binding.you rather than Binding.triggerPlayer would
        -- exile the Island.
        Spec.it s "CR 702.115a whole card: Culling Drone exiles the damaged player's top card" $ do
          drone <- S.printingOf s registry "Culling Drone"
          piker <- S.printingOf s registry "Goblin Piker"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          mountain <- S.printingOf s registry "Mountain"
          case S.combatBoardOf [drone] [] of
            (_, [], _) -> Spec.assertFailure s "fixture should have an attacker"
            (base, attacker : _, _) -> do
              let gs = stock island mountain S.alice (stock swamp piker S.bob base)
                  after = S.runCombat S.aggressiveAnswer gs
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Ingest attacker gs) "the Drone has ingest"
              Spec.assertEqWith s "the Piker, bob's top card, is in exile" (namesIn Zone.Exile S.bob after) (Set.singleton (nameOfCard "Goblin Piker"))
              Spec.assertEqWith s "and the Swamp under it stayed in the library" (namesIn Zone.Library S.bob after) (Set.singleton (nameOfCard "Swamp"))
              Spec.assertEqWith s "alice, who controls the ability, exiles nothing" (namesIn Zone.Exile S.alice after) Set.empty
              Spec.assertEqWith s "and keeps both her cards" (namesIn Zone.Library S.alice after) (Set.fromList [nameOfCard "Island", nameOfCard "Mountain"])
              -- Ingest is not a replacement for the damage: rule 702.115a's
              -- ability is additional, so the two life still goes.
              Spec.assertEqWith s "bob took the Drone's two" (S.lifeOf S.bob after) (Just 18)
        -- The negative, on the SAME board but for one blocker: rule 702.115a is
        -- scoped to combat damage dealt TO A PLAYER, and a blocked creature
        -- assigns its damage to the creatures blocking it (CR 510.1c).
        Spec.it s "CR 702.115a a blocked Culling Drone exiles nothing" $ do
          drone <- S.printingOf s registry "Culling Drone"
          piker <- S.printingOf s registry "Goblin Piker"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          case S.combatBoardOf [drone] [piker] of
            (_, [], _) -> Spec.assertFailure s "fixture should have an attacker"
            (base, _, _) -> do
              let gs = stock swamp mountain S.bob base
                  after = S.runCombat S.aggressiveAnswer gs
              -- The Piker blocked and took the Drone's two, which is what keeps
              -- this from passing on a board where no damage was dealt at all.
              Spec.assertEqWith s "the blocking Piker died" (namesIn Zone.Graveyard S.bob after) (Set.singleton (nameOfCard "Goblin Piker"))
              Spec.assertEqWith s "nothing is exiled" (namesIn Zone.Exile S.bob after) Set.empty
              Spec.assertEqWith s "both cards stayed in the library" (namesIn Zone.Library S.bob after) (Set.fromList [nameOfCard "Mountain", nameOfCard "Swamp"])
              Spec.assertEqWith s "and bob lost no life" (S.lifeOf S.bob after) (Just 20)
        -- CR 702.115a says nothing about a shortfall, so an empty library exiles
        -- nothing and costs nothing: CR 104.3c's loss is on DRAWING, and this is
        -- a move. The same board as the proving test, one thing different.
        Spec.it s "CR 702.115a an empty library exiles nothing and loses nobody" $ do
          drone <- S.printingOf s registry "Culling Drone"
          case S.combatBoardOf [drone] [] of
            (_, [], _) -> Spec.assertFailure s "fixture should have an attacker"
            (base, _, _) -> do
              let after = S.runCombat S.aggressiveAnswer base
              Spec.assertEqWith s "nothing is exiled" (namesIn Zone.Exile S.bob after) Set.empty
              Spec.assertEqWith s "bob is still in the game" (S.lifeOf S.bob after) (Just 18)
        -- CR 800.1 at three seats, the poisonous spec's shape: rule 702.115a's
        -- "that player" is whoever was DEALT the damage, and at two players that
        -- is indistinguishable from "the attacker's one opponent". The two runs
        -- differ only in the answer to Prompt.ChooseDefender.
        Spec.it s "CR 702.115a the exile follows whichever opponent was attacked" $ do
          drone <- S.printingOf s registry "Culling Drone"
          piker <- S.printingOf s registry "Goblin Piker"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          mountain <- S.printingOf s registry "Mountain"
          case S.threePlayerCombat [drone] [] [] of
            (_, [], _, _) -> Spec.assertFailure s "fixture should have an attacker"
            (base0, _, _, _) -> do
              let base = stock swamp piker S.bob (stock island mountain S.carol base0)
                  hitBob = S.runCombat (S.attackTo S.bob) base
                  hitCarol = S.runCombat (S.attackTo S.carol) base
              Spec.assertEqWith s "bob, attacked, exiles his Piker" (namesIn Zone.Exile S.bob hitBob) (Set.singleton (nameOfCard "Goblin Piker"))
              Spec.assertEqWith s "carol, untouched, exiles nothing" (namesIn Zone.Exile S.carol hitBob) Set.empty
              Spec.assertEqWith s "and the other way round" (namesIn Zone.Exile S.carol hitCarol) (Set.singleton (nameOfCard "Mountain"))
              Spec.assertEqWith s "bob untouched this time" (namesIn Zone.Exile S.bob hitCarol) Set.empty

-- CR 702.86a: "Annihilator is a triggered ability. 'Annihilator N' means
-- 'Whenever this creature attacks, defending player sacrifices N permanents.'"
-- Rule 702 states it as a triggered ability, like CR 702.70a's poisonous and CR
-- 702.91a's battle cry, so it is minted by
-- Pawl.Engine.Keyword and gathered by the same Pawl.Engine.Event.Trigger.eventTriggers
-- scan.
--
-- Slivdrazi Monstrosity is the card, and it reaches annihilator the long way
-- round: "Eldrazi you control are Slivers in addition to their other types"
-- (layer 4) feeds "Slivers you control have devoid and annihilator 1" (layer 6),
-- so a Slaughter Drone -- printed an Eldrazi with no annihilator anywhere on it
-- -- is what attacks. That dependency is CR 613.8's, already pinned for the
-- devoid half in Pawl.ColorSpec.
--
-- What separates this keyword from its two siblings is the PLAYER: rule 702.86a
-- names the DEFENDING player, whom CR 508.5 reads off what the creature is
-- attacking, and CR 508.5a makes that one specific player determined per
-- attacking creature. THREE SEATS is what makes that assertable -- at two
-- players "the defending player" and "the attacker's one opponent" are the same
-- player, so an implementation that bound the wrong one would pass.
annihilatorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
annihilatorSpec s registry =
  let -- Declares `attacker` and nothing else, attacks `who`, declines all
      -- blocks, and sacrifices the LAST candidate offered.
      --
      -- Every clause is there to keep an assertion from passing by accident.
      -- Declaring one creature keeps the trigger count at one, so "annihilator 1
      -- sacrificed one permanent" is not two abilities coinciding. Declining
      -- blocks keeps combat damage from removing a permanent the edict did not
      -- take. And taking the LAST candidate rather than the first is what proves
      -- the PROMPT is honoured: Replay.defaultAnswer takes the first `count`
      -- candidates, so an engine that ignored the answer would take the other
      -- permanent.
      declaring :: ObjectId.ObjectId -> PlayerId.PlayerId -> Prompt.Prompt r -> r
      declaring attacker who p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.ChooseAttackTarget {} -> S.attackTo who p
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.DeclareBlockers {} -> Map.empty
        Prompt.ChooseSacrifices _ _ _ candidates _ -> Set.fromList (take 1 (reverse candidates))
        _ -> S.aggressiveAnswer p
      -- alice fields Slivdrazi Monstrosity and a Slaughter Drone; bob and carol
      -- each field a Goblin Piker and a Mountain.
      --
      -- TWO permanents each, and that is the point: Effect.PlayerSacrifices
      -- elides the prompt when the candidates do not outnumber the count (CR
      -- 609.3), so a player with exactly one permanent would prove only the
      -- forced path. One of the two is a LAND, which rule 702.86a's unqualified
      -- "N permanents" admits -- and which is the permanent that goes.
      board = do
        slivdrazi <- S.printingOf s registry "Slivdrazi Monstrosity"
        drone <- S.printingOf s registry "Slaughter Drone"
        piker <- S.printingOf s registry "Goblin Piker"
        mountain <- S.printingOf s registry "Mountain"
        pure (S.threePlayerCombat [slivdrazi, drone] [piker, mountain] [piker, mountain])
   in Spec.describe s "Annihilator" $ do
        -- CR 702.86b: "If a creature has multiple instances of annihilator, each
        -- triggers separately." The count is a MULTIPLICITY, exactly as CR
        -- 702.70b makes poisonous'. The falsifier is a mint that collapses the
        -- count to one ability.
        Spec.it s "CR 702.86b each instance of annihilator is its own ability" $ do
          Spec.assertEqWith s "annihilator 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Annihilator 1) 2)) [Keyword.annihilator 1, Keyword.annihilator 1]
          Spec.assertEqWith s "and annihilator 2 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Annihilator 2) 1)) [Keyword.annihilator 2]
        -- CR 508.5 through CR 603.2: the declaration event carries the defending
        -- player, and the scan stamps them under the reserved slot rule 702.86a's
        -- "defending player" reads. The falsifier is an arm that binds the
        -- attacking side instead.
        Spec.it s "CR 603.2 the defending player rides the declaration in the reserved slot" $ do
          let bindings = Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing Map.empty S.alice (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime) (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared (ObjectId.MkObjectId 7) S.carol (AttackTarget.OfPlayer S.carol) 1))
          Spec.assertEqWith s "carol is bound under thatPlayer" (Binding.targetsOf bindings) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.carol)))
        -- CR 613.8's dependency, read off the projection before any attack: WHICH
        -- permanents actually carry the granted keyword. Without this the two
        -- board cases below could pass off a keyword nobody has.
        Spec.it s "CR 702.86 Slivdrazi Monstrosity grants annihilator 1 to the Slivers it makes" $ do
          (gs, ours, yours, _) <- board
          case (ours, yours) of
            (slivdrazi : drone : _, piker : _) -> do
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Annihilator 1) drone gs) "the Eldrazi, made a Sliver, has annihilator 1"
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Annihilator 1) slivdrazi gs) "and so does Slivdrazi itself, being a Sliver"
              Spec.assertBool s (not (Projection.hasKeyword (Keyword.Type.Annihilator 1) piker gs)) "bob's creature, which alice does not control, does not"
            _ -> Spec.assertFailure s "fixture should have two permanents a side"
        -- The proving test. alice attacks bob, so CR 508.5 makes bob the
        -- defending player and rule 702.86a makes him sacrifice one permanent of
        -- HIS choice -- the Mountain, which is the candidate the interpreter
        -- named and not the one the engine's fallback would have taken. carol,
        -- an opponent who was not attacked, loses nothing.
        Spec.it s "CR 702.86a the attacked player sacrifices one permanent of their own choosing" $ do
          (gs, ours, yours, hers) <- board
          case (ours, yours, hers) of
            (_ : drone : _, bobsPiker : bobsMountain : _, carolsPiker : carolsMountain : _) -> do
              let after = S.runCombat (declaring drone S.bob) gs
              Spec.assertEqWith s "bob is left with only the Piker" (Game.zoneMembers Zone.Battlefield S.bob after) [bobsPiker]
              Spec.assertBool s (notElem bobsMountain (Game.zoneMembers Zone.Battlefield S.bob after)) "and the permanent he named, the Mountain, is what went"
              Spec.assertEqWith s "carol, not the defending player, sacrifices nothing" (Game.zoneMembers Zone.Battlefield S.carol after) [carolsPiker, carolsMountain]
            _ -> Spec.assertFailure s "fixture should have two permanents a side"
        -- CR 508.5a: the defending player is one SPECIFIC player, and which one
        -- is settled by CR 506.2a's choice. The only difference between this run
        -- and the one above is the answer to Prompt.ChooseDefender, so an
        -- implementation that bound the attacker's controller, or "the opponent",
        -- or a fixed seat cannot pass both.
        Spec.it s "CR 508.5 the sacrifice follows whichever opponent was attacked" $ do
          (gs, ours, yours, hers) <- board
          case (ours, yours, hers) of
            (slivdrazi : drone : _, bobsPiker : bobsMountain : _, carolsPiker : _) -> do
              let after = S.runCombat (declaring drone S.carol) gs
              Spec.assertEqWith s "carol, attacked this time, is left with only the Piker" (Game.zoneMembers Zone.Battlefield S.carol after) [carolsPiker]
              Spec.assertEqWith s "and bob, untouched, keeps both" (Game.zoneMembers Zone.Battlefield S.bob after) [bobsPiker, bobsMountain]
              Spec.assertEqWith s "alice, who controls the ability, sacrifices nothing" (Game.zoneMembers Zone.Battlefield S.alice after) [slivdrazi, drone]
            _ -> Spec.assertFailure s "fixture should have two permanents a side"

-- CR 702.91a: "Battle cry is a triggered ability. 'Battle cry' means 'Whenever
-- this creature attacks, each other attacking creature gets +1/+0 until end of
-- turn.'" Rule 702 states it as a triggered
-- ability, like CR 702.70a's poisonous and CR 702.86a's annihilator, so it is
-- minted by Pawl.Engine.Keyword
-- and gathered by the same Pawl.Engine.Event.Trigger.eventTriggers scan.
--
-- Hero of Bladehold is the card, and it is here for a second reason: battle cry
-- and its printed "whenever this creature attacks, create two 1/1 white Soldier
-- creature tokens that are tapped and attacking" are TWO DISTINCT triggered
-- abilities of ONE source keyed on ONE event, so declaring it as an attacker
-- puts two entries into a single CR 603.3b ordering choice. That is #61's case:
-- under a source-only payload the two are identical on the wire while their
-- order genuinely matters, since battle cry pumps "each OTHER attacking
-- creature" and CR 611.2c fixes the affected set as the effect begins.
--
-- The card's official ruling (2011-06-01) states the outcome this group pins:
-- "Whenever Hero of Bladehold attacks, both abilities will trigger. You can put
-- them onto the stack in any order. If the token-creating ability resolves
-- first, the tokens each get +1/+0 until end of turn from the battle cry
-- ability."
battleCrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
battleCrySpec s registry =
  let -- Records every CR 603.3b ordering payload offered, verbatim, answering it
      -- canonically and leaving every other prompt to the aggressive answerer --
      -- which declares every legal attacker, so the declaration really happens.
      recordEntries :: Prompt.Prompt r -> State.State [[TriggerEntry.TriggerEntry]] r
      recordEntries p = case p of
        Prompt.OrderTriggers _ _ entries -> do
          State.modify' (<> [entries])
          pure (zipWith const [0 ..] entries)
        _ -> pure (S.aggressiveAnswer p)
      -- Names one of the two entries by WHICH ABILITY it is and puts it first or
      -- last in the permutation. `cryFirst` is about RESOLUTION: the answer is
      -- the order the abilities are PUT ON the stack, and the stack is LIFO, so
      -- the entry named LAST resolves FIRST.
      --
      -- This answerer is the discriminator's whole point (#61). Both entries hang
      -- on the one Hero, so under the source-only payload this replaced there was
      -- nothing to select on but a blind index -- and an index is not something a
      -- player can be asked to mean.
      resolvingFirst :: Bool -> Prompt.Prompt r -> r
      resolvingFirst cryFirst p = case p of
        Prompt.OrderTriggers _ _ entries ->
          let indexed = zip [0 ..] entries
              isCry entry = TriggerEntry.ability (snd entry) == Keyword.battleCry
              pick keep = fmap fst (filter ((==) keep . isCry) indexed)
           in if cryFirst then pick False <> pick True else pick True <> pick False
        _ -> S.aggressiveAnswer p
      powersOf oids gs = fmap (`Projection.powerOf` gs) oids
   in Spec.describe s "BattleCry" $ do
        -- CR 702.91b: "If a creature has multiple instances of battle cry, each
        -- triggers separately." So the count is a MULTIPLICITY, exactly as CR
        -- 702.70b makes poisonous' one -- the falsifier is a mint that collapses
        -- the count to a single ability. Asked of the mint directly rather than
        -- of a board, unlike poisonous' own gameplay-level pair: no card in this
        -- pool prints battle cry twice, and nothing here grants it, so a second
        -- instance is not reachable through play (Snake Cult Initiation is what
        -- makes it reachable for poisonous).
        Spec.it s "CR 702.91b each instance of battle cry is its own ability" $ do
          Spec.assertEqWith s "battle cry held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.BattleCry 2)) [Keyword.battleCry, Keyword.battleCry]
          Spec.assertEqWith s "and held once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.BattleCry 1)) [Keyword.battleCry]
        -- THE proving test (#61). One source, two DIFFERENT abilities, one
        -- event: the payload's two entries must not be the same value, or the
        -- player being asked for an order has no way to say which order they
        -- mean. The falsifier is the source-only payload this replaced, where
        -- both entries read `OfObject hero`.
        Spec.it s "CR 603.3b two DIFFERENT abilities of one source are distinguishable entries" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          let (gs, _, _) = S.combatBoardOf [hero] []
              (_, payloads) = State.runState (Engine.runGame recordEntries gs Engine.runStep) []
          case payloads of
            [[a, b]] -> do
              Spec.assertBool s (a /= b) "the two entries are distinguishable"
              Spec.assertEqWith s "both hang on the one Hero" (TriggerEntry.source a) (TriggerEntry.source b)
              Spec.assertEqWith s "and exactly one of them is rule 702.91a's battle cry" (length (filter ((==) Keyword.battleCry . TriggerEntry.ability) [a, b])) 1
            other -> Spec.assertFailure s ("expected one ordering payload of two entries, got " <> show (fmap length other))
        -- The other half of #61: two triggers of the SAME ability must stay
        -- INDISTINGUISHABLE, or the engine would be asking a question with no
        -- answer. CR 603.6a fires the watcher's ability once per entering
        -- creature, and Hero of Bladehold's token-maker puts two Soldiers onto
        -- the battlefield at once, so the second ordering choice of the same
        -- combat is a pair of entries differing only in which token each
        -- remembers -- a difference the entry deliberately does not carry.
        --
        -- The watcher is Aether Flash rather than Soul Warden BECAUSE its payload
        -- reads the entrant: Engine.orderInert now elides the prompt outright for
        -- a watcher that reads nothing, so a batch that still reaches the wire is
        -- the only place two equal entries can be observed at all.
        Spec.it s "CR 603.6a two triggers of the SAME ability stay indistinguishable" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          aetherFlash <- S.printingOf s registry "Aether Flash"
          case S.combatBoardOf [hero, aetherFlash] [] of
            (gs, [_, flashId], _) -> case snd (State.runState (Engine.runGame recordEntries gs Engine.runStep) []) of
              [[a, b], [w1, w2]] -> do
                Spec.assertBool s (a /= b) "the Hero's two abilities are still distinguishable"
                Spec.assertEqWith s "the second choice is the Flash's" (TriggerEntry.source w1) (TriggerSource.OfObject flashId)
                Spec.assertEqWith s "and its two triggers are the same ability from the same source" w1 w2
              other -> Spec.assertFailure s ("expected two ordering payloads of two entries each, got " <> show (fmap length other))
            _ -> Spec.assertFailure s "fixture should give alice a Hero and an Aether Flash"
        -- CR 702.91a's "each OTHER attacking creature", read one word at a time.
        -- The Piker is another attacking creature and gets +1/+0; the Hero is
        -- attacking but is not OTHER; the Wall is neither pumped nor an attacker
        -- at all (CR 702.3b's defender keeps it home), so it fixes that the set
        -- is attackers rather than "creatures you control".
        Spec.it s "CR 702.91a each OTHER attacking creature, and nothing else" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          piker <- S.printingOf s registry "Goblin Piker"
          wallOfStone <- S.printingOf s registry "Wall of Stone"
          case S.combatBoardOf [hero, piker, wallOfStone] [] of
            (gs, [heroId, pikerId, wallId], _) -> do
              let declared = S.runPure S.aggressiveAnswer gs Engine.runStep
              Spec.assertEqWith s "the other attacker is +1/+0" (Projection.powerOf pikerId declared) (Just 3)
              Spec.assertEqWith s "+1/+0 leaves toughness alone" (Projection.toughnessOf pikerId declared) (Just 1)
              Spec.assertEqWith s "the Hero does not pump itself" (Projection.powerOf heroId declared) (Just 3)
              Spec.assertEqWith s "and a creature that is not attacking is not pumped" (Projection.powerOf wallId declared) (Just 0)
            _ -> Spec.assertFailure s "fixture should give alice a Hero, a Piker and a Wall of Stone"
        -- THE order-matters pair, and the card's own ruling (2011-06-01): "If the
        -- token-creating ability resolves first, the tokens each get +1/+0 until
        -- end of turn from the battle cry ability."
        --
        -- CR 611.2c is why: "the set of objects it affects is determined when
        -- that continuous effect begins. After that point, the set won't change."
        -- So a Soldier that arrives after battle cry has begun is never in the
        -- set, and one that arrives before it is.
        Spec.it s "CR 603.3b/702.91a resolving the token-maker first pumps the Soldiers" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          let (gs, _, _) = S.combatBoardOf [hero] []
              after = S.runCombat (resolvingFirst False) gs
          Spec.assertEqWith s "two 2/1 Soldiers" (powersOf (S.tokensOf after) after) [Just 2, Just 2]
          Spec.assertEqWith s "so bob takes 3 + 2 + 2" (S.lifeOf S.bob after) (Just 13)
        -- The same board, the same cards, the opposite answer: battle cry
        -- resolves while the Hero is the only attacker, finds no other attacking
        -- creature, and the Soldiers arrive afterwards at their printed 1/1.
        Spec.it s "CR 603.3b/702.91a resolving battle cry first leaves the Soldiers unpumped" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          let (gs, _, _) = S.combatBoardOf [hero] []
              after = S.runCombat (resolvingFirst True) gs
          Spec.assertEqWith s "two 1/1 Soldiers" (powersOf (S.tokensOf after) after) [Just 1, Just 1]
          Spec.assertEqWith s "so bob takes 3 + 1 + 1" (S.lifeOf S.bob after) (Just 15)

-- CR 702.108a: "Prowess is a triggered ability. 'Prowess' means 'Whenever you
-- cast a noncreature spell, this creature gets +1/+1 until end of turn.'" The
-- rule text IS a triggered ability, like CR
-- 702.70a's, CR 702.86a's and CR 702.91a's, and the first minted trigger to watch
-- something other than its bearer's combat: the event is CR 601.2i's, so
-- Pawl.Engine.Keyword.prowess mints TriggerCondition.SpellCast.
--
-- Monastery Swiftspear, {R} Creature -- Human Monk 1/2 with haste and prowess.
-- 1/2 rather than a square body on purpose: prowess is +1/+1 and battle cry is
-- +1/+0, so every assertion below reads BOTH power and toughness -- a power-only
-- one cannot tell the two payloads apart -- and an asymmetric base also catches
-- a swapped pair of arguments to Modification.ModifyPowerToughness.
--
-- Boil, {3}{R} Instant "Destroy all Islands", is the noncreature spell, for
-- youngPyromancerSpec's reasons: it targets nothing, so no answerer choice
-- enters the fixture, and nobody here controls an Island, so its resolution
-- moves nothing an assertion reads. Goblin Piker, {1}{R}, is the creature spell.
--
-- The printed sentence narrows two things at once -- who cast it and what it was
-- -- so each case below moves exactly one, and the negatives carry the positive
-- control that the cast really happened. THREE seats, carol being the one that
-- is neither the caster nor the ability's controller: at two players "the caster
-- is not you" and "the caster is that one opponent" are the same sentence.
prowessSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
prowessSpec s registry =
  let -- alice bears the Swiftspear and has four Mountains, bob four as well, so
      -- a negative never fails for want of mana; carol is the third seat.
      board mountain swiftspear =
        let addLands pid n g = List.foldl' (\g2 _ -> snd (S.addPermanent mountain pid g2)) g [1 .. (n :: Int)]
            withLands = addLands S.bob 4 (addLands S.alice 4 S.threePlayerGame)
            (spearId, withSpear) = S.addPermanent swiftspear S.alice withLands
         in ( spearId,
              withSpear
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
      sizeOf oid gs = (Projection.powerOf oid gs, Projection.toughnessOf oid gs)
   in Spec.describe s "Prowess" $ do
        -- THE case: the trigger fires, and the pump is the one rule 702.108a
        -- names rather than merely some pump.
        Spec.it s "CR 702.108a whole card: casting an instant makes Monastery Swiftspear 2/3" $ do
          mountain <- S.printingOf s registry "Mountain"
          swiftspear <- S.printingOf s registry "Monastery Swiftspear"
          boil <- S.printingOf s registry "Boil"
          let (spearId, base) = board mountain swiftspear
              (boilId, gs) = S.addHandCard boil S.alice base
              after = castAndResolve S.alice boilId gs
          Spec.assertEqWith s "1/2 before the cast" (sizeOf spearId gs) (Just 1, Just 2)
          Spec.assertEqWith s "and 2/3 once the trigger resolves" (sizeOf spearId after) (Just 2, Just 3)
        -- "Noncreature", moved on its own: alice still casts, and only what she
        -- casts changes. Without this a filter that admitted every spell and one
        -- that read the card type are indistinguishable.
        Spec.it s "CR 702.108a a CREATURE spell pumps nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          swiftspear <- S.printingOf s registry "Monastery Swiftspear"
          piker <- S.printingOf s registry "Goblin Piker"
          let (spearId, base) = board mountain swiftspear
              (pikerId, gs) = S.addHandCard piker S.alice base
              after = castAndResolve S.alice pikerId gs
          -- Positive control: the cast really happened and really resolved, so
          -- the silence below is the Filter's answer and not a fixture that
          -- never cast anything.
          Spec.assertEqWith s "the Piker resolved onto the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
          Spec.assertEqWith s "and the Swiftspear is still 1/2" (sizeOf spearId after) (Just 1, Just 2)
        -- "You", moved on its own: the same instant from the seat to alice's
        -- left. The paired assertion on the same board is what proves the seat
        -- is the only thing the silence turns on.
        Spec.it s "CR 109.5 'you cast': an OPPONENT's instant pumps nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          swiftspear <- S.printingOf s registry "Monastery Swiftspear"
          boil <- S.printingOf s registry "Boil"
          let (spearId, base) = board mountain swiftspear
              (bobsBoil, withBobs) = S.addHandCard boil S.bob base
              (alicesBoil, gs) = S.addHandCard boil S.alice withBobs
              byBob = castAndResolve S.bob bobsBoil gs
              byAlice = castAndResolve S.alice alicesBoil gs
          Spec.assertEqWith s "bob's cast really resolved" (length (Game.zoneMembers Zone.Graveyard S.bob byBob)) 1
          Spec.assertEqWith s "and left the Swiftspear at 1/2" (sizeOf spearId byBob) (Just 1, Just 2)
          Spec.assertEqWith s "the same board pumps for alice's own cast" (sizeOf spearId byAlice) (Just 2, Just 3)
        -- CR 514.2: "until end of turn" is armed to the cleanup step, and CR
        -- 611.2c's frozen set is a single creature, so the whole effect goes.
        -- Run as the turn-based action rather than by advancing turns, which
        -- would deck a fixture player (CR 104.3c).
        Spec.it s "CR 514.2 the pump is gone at the cleanup step" $ do
          mountain <- S.printingOf s registry "Mountain"
          swiftspear <- S.printingOf s registry "Monastery Swiftspear"
          boil <- S.printingOf s registry "Boil"
          let (spearId, base) = board mountain swiftspear
              (boilId, gs) = S.addHandCard boil S.alice base
              after = castAndResolve S.alice boilId gs
              cleaned = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
          Spec.assertEqWith s "2/3 while the effect lasts" (sizeOf spearId after) (Just 2, Just 3)
          Spec.assertEqWith s "and 1/2 again afterwards" (sizeOf spearId cleaned) (Just 1, Just 2)
        -- CR 702.108b: "If a creature has multiple instances of prowess, each
        -- triggers separately." Asked of the mint rather than of a board, as
        -- battle cry's is: no card in this pool prints prowess twice and nothing
        -- here grants it, so a second instance is unreachable through play.
        Spec.it s "CR 702.108b each instance of prowess is its own ability" $ do
          Spec.assertEqWith s "prowess held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Prowess 2)) [Keyword.prowess, Keyword.prowess]
          Spec.assertEqWith s "and held once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Prowess 1)) [Keyword.prowess]

-- Cast `spell` and narrow every target slot it offers to `victim`, answering
-- everything else as S.identityAnswer does. The cast is pinned to the one card
-- rather than left to S.castAnswer because a padded hand holds other castable
-- cards, and a leg that spent the mana on one of those would never reach it.
aimedCast :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedCast spell victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just victim) . Recipient.objectOf) legal) sets
  Prompt.ChooseAction _ _ actions -> case filter (S.isCastOf spell) actions of
    action : _ -> action
    [] -> A.Pass
  _ -> S.identityAnswer p

-- CR 509.3e: "Whenever [a creature] blocks two or more creatures, . . ." -- the
-- form that reads HOW MANY, matched against the same grouped
-- GameEvent.BlocksDeclared SelfBlocks reads.
--
-- Lairwatch Giant {5}{W} Creature -- Giant Warrior 5/3, "This creature can block
-- an additional creature each combat / Whenever this creature blocks two or more
-- creatures, it gains first strike until end of turn", is the card, and the only
-- one that can reach the condition on its own text: the permission it prints is
-- what lets the count get to two.
selfBlocksAtLeastSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBlocksAtLeastSpec s registry =
  let blockEverything :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      blockEverything blocker p = case p of
        Prompt.DeclareBlockers _ _ _ attackers -> Map.singleton blocker (Set.fromList attackers)
        _ -> S.aggressiveAnswer p
      blockOne :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      blockOne blocker p = case p of
        Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
          [] -> Map.empty
          a : _ -> Map.singleton blocker (Set.singleton a)
        _ -> S.aggressiveAnswer p
   in Spec.describe s "SelfBlocksAtLeast" . Spec.it s "CR 509.3e blocking two grants first strike, blocking one does not" $ do
        piker <- S.printingOf s registry "Goblin Piker"
        giant <- S.printingOf s registry "Lairwatch Giant"
        let (gs, _, theirs) = S.combatBoardOf [piker, piker] [giant]
            atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
            atDamage answer = S.runToStep (Phase.Combat CombatStep.CombatDamage) answer gs
        case theirs of
          [oid] ->
            -- Both legs are the same board and the same card, differing only in
            -- how many attackers the declaration gave it -- which is rule
            -- 509.3e's own variable.
            Spec.assertEqWith
              s
              "two blocks grant it, one does not"
              ( Projection.hasKeyword Keyword.Type.FirstStrike oid (atDamage (blockEverything oid)),
                Projection.hasKeyword Keyword.Type.FirstStrike oid (atDamage (blockOne oid))
              )
              (True, False)
          _ -> Spec.assertFailure s "fixture should give bob one Lairwatch Giant"

-- CR 509.3a: "Whenever [a creature] blocks, . . ." -- the blocking side's
-- declaration trigger, matched against GameEvent.BlocksDeclared, which only
-- Pawl.Engine.Combat.declareBlockers appends, once per blocking creature.
--
-- Pride Guardian {W} Creature -- Cat Monk 0/3, "Defender / Whenever this creature
-- blocks, you gain 3 life", is the card. It is the cheapest producer in the pool:
-- its payload names nothing about the attacker it blocked, so these cases isolate
-- the trigger CONDITION, and 0 power keeps combat damage from moving the number
-- the assertions read.
--
-- The blocker is BOB's, since CR 509.1 has the defending player declare blocks,
-- so every life total below is read off the defending seat.
selfBlocksSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBlocksSpec s registry =
  let -- Attacks with everything and declines every block. aggressiveAnswer's
      -- control leg: the same game with CR 509.1's declaration switched off, and
      -- the only difference between the two answerers.
      noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      -- Blocks EVERY attacker with `blocker` alone, which aggressiveAnswer
      -- cannot express: it puts every blocker on the first attacker.
      blockEverything :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      blockEverything blocker p = case p of
        Prompt.DeclareBlockers _ _ _ attackers -> Map.singleton blocker (Set.fromList attackers)
        _ -> S.aggressiveAnswer p
      board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
   in Spec.describe s "SelfBlocks" $ do
        -- The proving test, and its control. alice attacks with a 2/1 Goblin
        -- Piker; bob blocks with the Guardian. 20 + 3 = 23 blocking, and
        -- 20 - 2 = 18 declining -- distinct numbers, and neither reachable from
        -- the other by an off-by-one.
        Spec.it s "CR 509.3a whole card: blocking gains 3 life, declining to block gains none" $ do
          (gs, _, theirs) <- board ["Goblin Piker"] ["Pride Guardian"]
          let blocked = S.runCombat S.aggressiveAnswer gs
              unblocked = S.runCombat noBlocks gs
          case theirs of
            [guardian] -> do
              Spec.assertEqWith s "the 0/3 Guardian survives the Piker's 2" (S.lifeOf S.bob blocked) (Just 23)
              Spec.assertBool s (S.onBattlefield guardian blocked) "and is still on the battlefield"
              Spec.assertEqWith s "alice gains nothing: the trigger is the blocker controller's (CR 603.3a)" (S.lifeOf S.alice blocked) (Just 20)
              Spec.assertEqWith s "control leg: no block, no gain, and the Piker's 2 gets through" (S.lifeOf S.bob unblocked) (Just 18)
            _ -> Spec.assertFailure s "fixture should give bob one Pride Guardian"
        -- CR 509.2a: the abilities that triggered on blockers being declared go
        -- onto the stack before the active player gets priority, so they resolve
        -- in the declare blockers step -- not at combat damage, and not at end of
        -- combat. Read at the combat damage step, before any damage is dealt.
        Spec.it s "CR 509.2a the trigger has already resolved when the combat damage step begins" $ do
          (gs, _, _) <- board ["Goblin Piker"] ["Pride Guardian"]
          let atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer gs
          Spec.assertEqWith s "the fixture reached the combat damage step" (GameState.phase atDamage) (Phase.Combat CombatStep.CombatDamage)
          Spec.assertEqWith s "and bob is already at 23" (S.lifeOf S.bob atDamage) (Just 23)
        -- CR 603.2: the condition is the BEARER's own declaration. bob blocks one
        -- attacker with two creatures, so two declarations are recorded and only
        -- one of them is the Guardian's. The falsifier is a match that ignores
        -- the blocker on the event: that fires twice, for 26.
        Spec.it s "CR 603.2 another creature's block does not fire the Guardian's ability" $ do
          (gs, _, _) <- board ["Goblin Piker"] ["Pride Guardian", "Goblin Piker"]
          let after = S.runCombat S.aggressiveAnswer gs
          Spec.assertEqWith s "one gain of 3, not two" (S.lifeOf S.bob after) (Just 23)
        -- CR 509.1a gives each blocker one creature to block by default, so a
        -- second ATTACKER adds a declaration the Guardian is not in. alice attacks with
        -- two Pikers and aggressiveAnswer blocks the first; the second's 2 gets
        -- through. 20 + 3 - 2 = 21. The falsifier is a condition that matched an
        -- attacker's declaration too -- three events rather than one, for 25.
        Spec.it s "CR 509.3a an attacker's own declaration is not a block" $ do
          (gs, _, _) <- board ["Goblin Piker", "Goblin Piker"] ["Pride Guardian"]
          let after = S.runCombat S.aggressiveAnswer gs
          Spec.assertEqWith s "gained 3 once, then took 2 from the unblocked Piker" (S.lifeOf S.bob after) (Just 21)
        -- Rule 509.3a's "even if it blocks multiple creatures", now that a
        -- creature can. A High Ground gives bob's team the arity, the Guardian
        -- blocks both Pikers, and the gain is 3 once rather than 3 twice. The
        -- falsifier is a match on the PAIRWISE GameEvent.BecameBlocking, which
        -- fires per attacker blocked: 26.
        Spec.it s "CR 509.3a blocking TWO creatures still gains 3 once" $ do
          (gs, _, theirs) <- board ["Goblin Piker", "Goblin Piker"] ["Pride Guardian", "High Ground"]
          case theirs of
            [guardian, _] -> do
              let after = S.runCombat (blockEverything guardian) gs
              Spec.assertEqWith s "one gain of 3, not two" (S.lifeOf S.bob after) (Just 23)
            _ -> Spec.assertFailure s "fixture should give bob a Guardian and a High Ground"
        -- The other side of the same coin, and CR 508.3a's own words: a creature
        -- that BLOCKS did not attack. Hanweir Garrison {2}{R} 2/3, "Whenever this
        -- creature attacks, create two 1/1 red Human creature tokens that are
        -- tapped and attacking", is the pool's cheapest attack trigger; here it
        -- is bob's, and blocking. The falsifier is a SelfAttacks arm that matched
        -- the blocking declaration: two tokens rather than none.
        Spec.it s "CR 508.3a a block is not an attack, so a blocking Hanweir Garrison makes no tokens" $ do
          (blocking, _, _) <- board ["Goblin Piker"] ["Hanweir Garrison"]
          (attacking, _, _) <- board ["Hanweir Garrison"] ["Goblin Piker"]
          Spec.assertEqWith s "the Garrison blocked and made nothing" (length (S.tokensOf (S.runCombat S.aggressiveAnswer blocking))) 0
          -- The positive control: the same card on the attacking side really does
          -- have the ability, so the zero above is a fact about blocking rather
          -- than about the fixture.
          Spec.assertEqWith s "the same card attacking makes two" (length (S.tokensOf (S.runCombat S.aggressiveAnswer attacking))) 2

-- CR 509.3b: "Whenever [a creature] blocks a creature, . . ." -- selfBlocksSpec's
-- condition with the attacker NAMED, bound under Binding.blockedCreature and
-- compared against the condition's own Filter.
--
-- Loyal Sentry {W} Creature -- Human Soldier 1/1, "When this creature blocks a
-- creature, destroy that creature and this creature", is the unnarrowed card: the
-- trigger is its whole text, and "that creature" is the binding under test.
-- Netcaster Spider and Crimson Roc are the narrowed pair, in the last case below.
-- Every reading is
-- taken at the COMBAT DAMAGE step, before damage is dealt, so a death there is
-- the trigger's (CR 509.2a) and never combat's.
selfBlocksCreatureSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBlocksCreatureSpec s registry =
  let noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      -- aggressiveAnswer blocks the FIRST attacker with everything, which cannot
      -- tell "the attacker the bearer blocked" from "the first attacker". This
      -- one blocks the SECOND.
      blockSecond :: Prompt.Prompt r -> r
      blockSecond p = case p of
        Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
          _ : a : _ -> Map.fromList (fmap (\b -> (b, Set.singleton a)) mine)
          _ -> Map.empty
        _ -> S.aggressiveAnswer p
      board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
      atDamageWithout = S.runToStep (Phase.Combat CombatStep.CombatDamage) noBlocks
      atDamageSecond = S.runToStep (Phase.Combat CombatStep.CombatDamage) blockSecond
   in Spec.describe s "SelfBlocksCreature" $ do
        -- The proving test, and its control: the same board with CR 509.1's
        -- declaration switched off. Blocking, both creatures are gone before
        -- damage; declining, both are alive and the Piker's 2 gets through.
        Spec.it s "CR 509.3b whole card: blocking destroys the attacker and the Sentry" $ do
          (gs, mine, theirs) <- board ["Goblin Piker"] ["Loyal Sentry"]
          case (mine, theirs) of
            ([piker], [sentry]) -> do
              Spec.assertEqWith
                s
                "both are gone at the combat damage step, and bob took nothing"
                (S.onBattlefield piker (atDamage gs), S.onBattlefield sentry (atDamage gs), S.lifeOf S.bob (S.runCombat S.aggressiveAnswer gs))
                (False, False, Just 20)
              Spec.assertEqWith
                s
                "control leg: no block, so neither dies and the Piker's 2 gets through"
                (S.onBattlefield piker (atDamageWithout gs), S.onBattlefield sentry (atDamageWithout gs), S.lifeOf S.bob (S.runCombat noBlocks gs))
                (True, True, Just 18)
            _ -> Spec.assertFailure s "fixture should give each seat one creature"
        -- The binding, which is the whole difference from CR 509.3a: "that
        -- creature" is the attacker THIS blocker was declared against, not the
        -- first one nor the bearer. alice attacks with a 2/1 Piker and a 3/3 Hill
        -- Giant; the Sentry blocks the Giant.
        --
        -- The load-bearing reading is the Piker's: a binding taken off the wrong
        -- attacker kills it instead, and one that named the bearer kills nothing
        -- but the Sentry.
        Spec.it s "CR 509.3b that creature is the attacker the bearer blocked" $ do
          (gs, mine, theirs) <- board ["Goblin Piker", "Hill Giant"] ["Loyal Sentry"]
          case (mine, theirs) of
            ([piker, giant], [sentry]) -> do
              let struck = atDamageSecond gs
              Spec.assertEqWith
                s
                "the blocked Giant died, the unblocked Piker lived, and the Sentry died with it"
                (S.onBattlefield giant struck, S.onBattlefield piker struck, S.onBattlefield sentry struck)
                (False, True, False)
              Spec.assertEqWith s "and only the Piker's 2 reached bob" (S.lifeOf S.bob (S.runCombat blockSecond gs)) (Just 18)
            _ -> Spec.assertFailure s "fixture should give alice two attackers and bob one blocker"
        -- CR 509.3b's bearer is the BLOCKER. The same card attacking and becoming
        -- blocked matches nothing, which is what pins the arm's `blocker ==
        -- bearer` against reading the pair the other way round -- under that
        -- reading the attacking Sentry triggers and destroys itself in the
        -- declare blockers step.
        Spec.it s "CR 509.3b becoming blocked is not blocking" $ do
          (gs, mine, theirs) <- board ["Loyal Sentry"] ["Goblin Piker"]
          case (mine, theirs) of
            ([sentry], [piker]) -> do
              let struck = atDamage gs
              Spec.assertEqWith
                s
                "nothing triggered, so both are still there when damage is about to be dealt"
                (S.onBattlefield sentry struck, S.onBattlefield piker struck)
                (True, True)
              -- The positive control on the same pair of cards: with the Sentry
              -- blocking instead, both are gone by then.
              (blocking, otherPikers, otherSentries) <- board ["Goblin Piker"] ["Loyal Sentry"]
              let controlStruck = atDamage blocking
              Spec.assertEqWith
                s
                "the same two cards with the Sentry blocking do trigger"
                (fmap (`S.onBattlefield` controlStruck) (otherPikers <> otherSentries))
                [False, False]
            _ -> Spec.assertFailure s "fixture should give each seat one creature"
        -- CR 509.3b's FILTER, which narrows the printed "a creature" to Netcaster
        -- Spider's "a creature with flying" and Crimson Roc's "without flying".
        --
        -- Netcaster Spider {2}{G} Creature -- Spider 2/3, "Reach / Whenever this
        -- creature blocks a creature with flying, this creature gets +2/+0 until
        -- end of turn". Crimson Roc {4}{R} Creature -- Bird 2/2, "Flying /
        -- Whenever this creature blocks a creature without flying, this creature
        -- gets +1/+0 and gains first strike until end of turn". CR 702.9b is what
        -- lets both of them block the flier, and lets the Roc block the ground
        -- creature.
        --
        -- BOTH stand on bob's side of BOTH boards, so the two boards differ in
        -- exactly one thing: whether alice's lone attacker has flying. No-Regrets
        -- Egret 2/2 flying and Icehide Golem 2/2 are the attackers -- same stats
        -- and same seat.
        --
        -- Two cards whose Filters are each other's negation is what tells "the
        -- Filter is read" from "the field is there and ignored": a hardcoded
        -- HasKeyword Flying agrees with the Spider on both boards and gets the Roc
        -- backwards on both, and an always-true Filter gets one leg of each card
        -- wrong. Each board carries a leg that FIRES beside the leg that does not,
        -- so neither absence can pass on a board where no block happened.
        Spec.it s "CR 509.3b the Filter narrows which attacker fires it" $ do
          (flier, _, blockingFlier) <- board ["No-Regrets Egret"] ["Netcaster Spider", "Crimson Roc"]
          (ground, _, blockingGround) <- board ["Icehide Golem"] ["Netcaster Spider", "Crimson Roc"]
          case (blockingFlier, blockingGround) of
            ([spiderF, rocF], [spiderG, rocG]) -> do
              let struckFlier = atDamage flier
                  struckGround = atDamage ground
              Spec.assertEqWith
                s
                "the Spider fires on the flier and not on the ground creature, and the Roc the other way round"
                ( S.powerToughnessOf spiderF struckFlier,
                  S.powerToughnessOf spiderG struckGround,
                  S.powerToughnessOf rocG struckGround,
                  S.powerToughnessOf rocF struckFlier
                )
                (Just (4, 3), Just (2, 3), Just (3, 2), Just (2, 2))
              -- The Roc's second clause, on the leg that fired and the leg that
              -- did not: rule 702.7a's first strike is the half a power reading
              -- cannot see.
              Spec.assertEqWith
                s
                "and the granted first strike came with the pump, on that leg alone"
                ( Map.member Keyword.Type.FirstStrike (Projection.keywordsOf rocG struckGround),
                  Map.member Keyword.Type.FirstStrike (Projection.keywordsOf rocF struckFlier)
                )
                (True, False)
            _ -> Spec.assertFailure s "fixture should give bob a Spider and a Roc on each board"

-- CR 509.3e's FILTERED forms, both halves of one printed sentence: "whenever
-- [a creature] blocks or becomes blocked by one or more [black] creatures". The
-- rule's last sentence covers "at least a certain number", and one is the number
-- every filtered printing states.
--
-- Serra Inquisitors {4}{W} Creature -- Human Cleric 3/3, "Whenever this creature
-- blocks or becomes blocked by one or more black creatures, this creature gets
-- +2/+0 until end of turn", is the card, and one CR 603.1b ability with two
-- conditions rather than two abilities. A creature cannot block and be blocked in
-- the same combat (CR 509.1a chooses from the DEFENDING player's creatures), so
-- at most one branch of the AnyOf can fire per combat.
--
-- Bog Wraith 3/3 is the black creature and Hill Giant 3/3 the control: same stats
-- and same seat, differing only in colour, so a leg that stops firing stopped on
-- the Filter. Every reading is taken at the COMBAT DAMAGE step, before damage is
-- dealt, so 3 -> 5 is the trigger's and never combat's.
selfBlocksOneOrMoreSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBlocksOneOrMoreSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- Blocks EVERY attacker with `blocker` alone, which aggressiveAnswer cannot
      -- express: it puts every blocker on the first attacker.
      blockEverything :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      blockEverything blocker p = case p of
        Prompt.DeclareBlockers _ _ _ attackers -> Map.singleton blocker (Set.fromList attackers)
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      afterCombat :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      afterCombat = S.runToStep (Phase.Combat CombatStep.EndOfCombat)
      -- `board` with one creature card added to bob's HAND, which is what
      -- Aetherplasm's second clause puts onto the battlefield blocking. The hand
      -- holds that card alone, so the two arrival legs below differ in the CARD
      -- and in nothing else.
      plasmBoard mine theirs card = do
        (gs0, ours, yours) <- board mine theirs
        printing <- S.printingOf s registry card
        let (handId, withCard) = S.addHandCard printing S.bob gs0
        pure (withCard, ours, yours, handId)
      -- Declares `blockers` against the lone attacker, takes both of
      -- Aetherplasm's printed "may"s, and takes `card` out of hand.
      --
      -- The offer is FILTERED rather than replaced: clause 0 has already put
      -- Aetherplasm back in hand beside it and both are creature cards, so the
      -- choice is a real one, and a leg where `card` is not offered takes the
      -- fallback instead of succeeding on a hand-built id.
      --
      -- The declaration is spelled out here rather than left to the answerer
      -- S.runToStep was handed: that function stops when the phase first
      -- matches, which is BEFORE CR 509.1's turn-based action, so an answer of
      -- Map.empty would silently leave the attacker unblocked.
      swapping :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      swapping attacker blockers card p = case p of
        Prompt.DeclareBlockers {} -> Map.fromList (fmap (\b -> (b, Set.singleton attacker)) blockers)
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseCardInHand _ _ _ offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find (== card) (NonEmpty.toList offered))
        _ -> S.aggressiveAnswer p
      survivors :: String -> GameState.GameState -> Int
      survivors name = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack name)) S.bob
      -- The ids Combat.blockers holds for an attacker, narrowed to the ones
      -- still on the battlefield. Nothing prunes a blocker's id as it leaves
      -- (Pawl.Engine.Damage's liveness filter is what makes the assignment
      -- right), and Aetherplasm leaves ALIVE, so its stale id is still in the
      -- set below.
      liveBlockers :: ObjectId.ObjectId -> GameState.GameState -> [ObjectId.ObjectId]
      liveBlockers attacker gs = filter (`S.onBattlefield` gs) (Set.toList (Combat.blockersOf attacker gs))
   in Spec.describe s "SelfBlocksOneOrMore" $ do
        -- The proving test for the BLOCKING half, and its control: two boards
        -- differing only in the attacker's colour.
        Spec.it s "CR 509.3e whole card: blocking a black creature is +2/+0, blocking a nonblack one is nothing" $ do
          (black, _, mine) <- board ["Bog Wraith"] ["Serra Inquisitors"]
          (other, _, theirs) <- board ["Hill Giant"] ["Serra Inquisitors"]
          case (mine, theirs) of
            ([blocking], [control]) ->
              Spec.assertEqWith
                s
                "5/3 against the Wraith, 3/3 against the Giant"
                (S.powerToughnessOf blocking (atDamage S.aggressiveAnswer black), S.powerToughnessOf control (atDamage S.aggressiveAnswer other))
                (Just (5, 3), Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give bob one Serra Inquisitors on each board"
        -- Rule 509.3e's "one or more" is ONE trigger however many admitted
        -- creatures were blocked. A High Ground gives bob the arity, the
        -- Inquisitors blocks both Wraiths, and the answer is 5/3 rather than 7/3.
        -- The falsifier is a match on the pairwise GameEvent.BecameBlocking,
        -- which is CR 509.3b's arity: that fires twice.
        Spec.it s "CR 509.3e blocking TWO black creatures is +2/+0 once" $ do
          (gs, _, theirs) <- board ["Bog Wraith", "Bog Wraith"] ["Serra Inquisitors", "High Ground"]
          case theirs of
            [inquisitors, _] ->
              Spec.assertEqWith
                s
                "one pump, not two"
                (S.powerToughnessOf inquisitors (atDamage (blockEverything inquisitors) gs))
                (Just (5, 3))
            _ -> Spec.assertFailure s "fixture should give bob an Inquisitors and a High Ground"
        -- The ATTACKING half, and its control: the same pair of boards with the
        -- Inquisitors on alice's side, so the branch that fires is the other one.
        Spec.it s "CR 509.3e whole card: becoming blocked by a black creature is +2/+0, by a nonblack one is nothing" $ do
          (black, mine, _) <- board ["Serra Inquisitors"] ["Bog Wraith"]
          (other, theirs, _) <- board ["Serra Inquisitors"] ["Hill Giant"]
          case (mine, theirs) of
            ([attacking], [control]) ->
              Spec.assertEqWith
                s
                "5/3 blocked by the Wraith, 3/3 blocked by the Giant"
                (S.powerToughnessOf attacking (atDamage S.aggressiveAnswer black), S.powerToughnessOf control (atDamage S.aggressiveAnswer other))
                (Just (5, 3), Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Serra Inquisitors on each board"
        -- The attacking half's arity, which is what separates this condition from
        -- SelfBecomesBlockedBy: two Wraiths block the one Inquisitors, so two
        -- GameEvent.BecameBlocking are recorded and exactly one
        -- GameEvent.AttackerBlocked. The falsifier is a match on the pairwise
        -- event: that fires twice, for 7/3.
        Spec.it s "CR 509.3e becoming blocked by TWO black creatures is +2/+0 once" $ do
          (gs, mine, _) <- board ["Serra Inquisitors"] ["Bog Wraith", "Bog Wraith"]
          case mine of
            [inquisitors] ->
              Spec.assertEqWith s "one pump, not two" (S.powerToughnessOf inquisitors (atDamage S.aggressiveAnswer gs)) (Just (5, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Serra Inquisitors"
        -- "One or more" is a floor over the ADMITTED blockers, not a demand on all
        -- of them: a Wraith and a Giant block together and the trigger still
        -- fires. The falsifier is an `all` in place of the `any`, which answers
        -- 3/3 here while agreeing with every other case in this group.
        Spec.it s "CR 509.3e one admitted blocker among two is enough" $ do
          (gs, mine, _) <- board ["Serra Inquisitors"] ["Bog Wraith", "Hill Giant"]
          case mine of
            [inquisitors] ->
              Spec.assertEqWith s "the Wraith alone fires it" (S.powerToughnessOf inquisitors (atDamage S.aggressiveAnswer gs)) (Just (5, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Serra Inquisitors"
        -- CR 603.2: the condition is the BEARER's own block. A Goblin Piker blocks
        -- the Wraith while the Inquisitors stands by, so the declaration records a
        -- GameEvent.BlocksDeclared naming somebody else. A regression fence rather
        -- than a proof of one line: the arm's `blocker == bearer` and its
        -- Combat.blockers read each rule this board out on their own, so no single
        -- mutation turns it red.
        Spec.it s "CR 603.2 another creature's block does not pump the Inquisitors" $ do
          (gs, _, theirs) <- board ["Bog Wraith"] ["Serra Inquisitors", "Goblin Piker"]
          case theirs of
            [inquisitors, piker] ->
              Spec.assertEqWith
                s
                "the bystander stays 3/3"
                (S.powerToughnessOf inquisitors (atDamage (blockEverything piker) gs))
                (Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give bob an Inquisitors and a Piker"
        -- Rule 509.3e's SECOND sentence, "effects that add or remove blockers
        -- can also cause such abilities to trigger", for the ATTACKING half.
        -- Aetherplasm {2}{U}{U} 1/1 declares the block, its own trigger returns
        -- it to hand and puts a creature card onto the battlefield blocking the
        -- same attacker (CR 509.4), and the Inquisitors becomes blocked by that
        -- arrival. The declaration admits nobody -- Aetherplasm is blue -- so
        -- every reading agrees until the arrival, and the arrival is the whole
        -- difference.
        --
        -- Two boards and one answerer, differing in the one CARD bob holds:
        -- Disowned Ancestor {B} 0/4, and Secret Door {U} 0/4 as the control.
        -- Both are 0/4, so the two legs agree on what the
        -- arrival can do and on what survives its own return damage, and differ
        -- only in colour -- and 4 is the one toughness that separates 3 from 5.
        -- Power 0 is why nothing else on the board moves either way. Secret
        -- Door's activated ability is sorcery-speed and bob has no mana; the
        -- Ancestor's outlast is sorcery-speed too.
        --
        -- WHAT DOES NOT DISCRIMINATE, and each is a board a reader reaches for
        -- first:
        --
        --   * Cabal Evangel 2/2 as the arrival, which the issue drafted. The
        --     grant is +2/+0, so the 3/3 kills a 2/2 under both readings and
        --     takes 2 and lives under both.
        --   * declining Aetherplasm's first "may". Clause 1 hangs on it (CR
        --     608.2c), so that leg differs in two things -- the blocker that
        --     stays and the arrival that never comes.
        --   * reading the Inquisitors' power alone, which is what the cases
        --     above this one do. It says the trigger fired but nothing about
        --     what the grant reached, so the Ancestor's death is asserted first
        --     and the power after it.
        Spec.it s "CR 509.3e whole card: a black creature put onto the battlefield blocking is +2/+0, a blue one is nothing" $ do
          (blackGs, blackMine, blackTheirs, ancestor) <- plasmBoard ["Serra Inquisitors"] ["Aetherplasm"] "Disowned Ancestor"
          (blueGs, blueMine, blueTheirs, door) <- plasmBoard ["Serra Inquisitors"] ["Aetherplasm"] "Secret Door"
          case (blackMine, blackTheirs, blueMine, blueTheirs) of
            ([inquisitors], [plasm], [control], [otherPlasm]) -> do
              Spec.assertEqWith
                s
                "the Ancestor takes 5 and dies, and the same board with a blue arrival leaves it standing"
                (survivors "Disowned Ancestor" (afterCombat (swapping inquisitors [plasm] ancestor) blackGs), survivors "Secret Door" (afterCombat (swapping control [otherPlasm] door) blueGs))
                (0, 1)
              -- The control leg is not vacuous: its arrival really did come and
              -- really is blocking, so the survival above is the Filter and not
              -- a clause that never ran.
              Spec.assertEqWith
                s
                "control: the blue arrival is blocking the Inquisitors all the same"
                (length (liveBlockers control (atDamage (swapping control [otherPlasm] door) blueGs)))
                1
              -- The pump itself, after the gameplay quantity.
              Spec.assertEqWith
                s
                "5/3 against the Ancestor, 3/3 against the Door"
                (S.powerToughnessOf inquisitors (atDamage (swapping inquisitors [plasm] ancestor) blackGs), S.powerToughnessOf control (atDamage (swapping control [otherPlasm] door) blueGs))
                (Just (5, 3), Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Serra Inquisitors and bob one Aetherplasm on each board"
        -- The CROSSING and not the arrival: an admitted creature that joins an
        -- attacker ALREADY blocked by an admitted one is no second becoming, so
        -- the ability fires once for the whole combat. The case above's board
        -- with a Bog Wraith 3/3 declared alongside Aetherplasm, and nothing else
        -- changed -- the declaration fires the trigger there, and the Ancestor
        -- arrives into a block a black creature is already part of.
        --
        -- Read at the combat damage step, before damage: the Wraith's 3 kills a
        -- 5/3 and a 7/3 alike, so the survivors cannot tell the two apart.
        Spec.it s "CR 509.3e a black creature joining a block a black creature is already in is +2/+0 once" $ do
          (gs, mine, theirs, ancestor) <- plasmBoard ["Serra Inquisitors"] ["Aetherplasm", "Bog Wraith"] "Disowned Ancestor"
          case (mine, theirs) of
            ([inquisitors], [plasm, wraith]) -> do
              let joined = atDamage (swapping inquisitors [plasm, wraith] ancestor) gs
              Spec.assertEqWith s "one pump, not two" (S.powerToughnessOf inquisitors joined) (Just (5, 3))
              -- Anti-vacuity: the arrival did come and did join THIS attacker's
              -- block, so the single pump is the crossing and not a clause that
              -- never ran.
              Spec.assertEqWith
                s
                "CR 509.4: the Wraith and the Ancestor are both blocking the Inquisitors"
                (length (liveBlockers inquisitors joined), survivors "Disowned Ancestor" joined)
                (2, 1)
            _ -> Spec.assertFailure s "fixture should give alice one Serra Inquisitors, and bob an Aetherplasm and a Wraith"

-- CR 509.3e read by a BYSTANDER on the ATTACKING side: "whenever a creature
-- attacking one of your opponents becomes blocked by two or more creatures".
-- The rule's last sentence makes the number a floor rather than an exact count,
-- and two is the only floor above one that a printing states on this side --
-- Scryfall o:"becomes blocked by two or more", 2026-08-21, matches Seifer alone,
-- o:"becomes blocked by three or more" matches nothing, and o:"becomes blocked
-- by" o:"or more creatures" adds only Godsend, whose number is one.
--
-- Seifer, Balamb Rival {2}{B}{R} Legendary Creature -- Human Mercenary 4/3,
-- "First strike / Whenever a creature attacking one of your opponents becomes
-- blocked by two or more creatures, that attacking creature gains deathtouch
-- until end of turn", is the card.
--
-- Seifer's second line, "Whenever you attack a player, goad target creature that
-- player controls", is CR 508.3e's and lives in Pawl.EventTriggerSpec with the
-- rest of rule 508.3's player subjects. It fires on every board below, which is
-- why `firedBy` asks which CONDITION triggered rather than counting Seifer's
-- triggers.
--
-- Llanowar Elves 1/1 attacks and Hill Giant 3/3 blocks, which is what makes the
-- grant observable without depending on how a damage assignment is split: one
-- power kills a 3/3 only under CR 704.5h. Seifer never joins the combat on
-- either side, the condition being a bystander's.
creatureBecomesBlockedByAtLeastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
creatureBecomesBlockedByAtLeastSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- Attacks with `attacker` alone and blocks it with `blockers` alone, which
      -- neither S.aggressiveAnswer nor selfBlocksOneOrMoreSpec's blockEverything
      -- can express: both send everything the seat has into combat, and Seifer
      -- has to stay out of it on whichever seat it sits.
      declaring :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      declaring attacker blockers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
          [] -> Map.empty
          a : _ -> Map.fromList (fmap (\b -> (b, Set.singleton a)) blockers)
        _ -> S.aggressiveAnswer p
      afterCombat :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> GameState.GameState -> GameState.GameState
      afterCombat attacker blockers = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (declaring attacker blockers)
      -- The same declaration aimed at a PLANESWALKER instead (CR 508.1b), which
      -- is the one prompt `declaring` never sees: combatBoardOf's boards offer
      -- the defending player alone.
      declaringAt :: ObjectId.ObjectId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      declaringAt walker attacker blockers p = case p of
        Prompt.ChooseAttackTarget _ _ _ options -> case filter (== AttackTarget.OfPlaneswalker walker) (NonEmpty.toList options) of
          target : _ -> target
          [] -> NonEmpty.head options
        _ -> declaring attacker blockers p
      afterCombatAt :: ObjectId.ObjectId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> GameState.GameState -> GameState.GameState
      afterCombatAt walker attacker blockers = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (declaringAt walker attacker blockers)
      giants :: GameState.GameState -> Int
      giants = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Hill Giant")) S.bob
      -- `board` with the defending seat stocked to cast Flash Foliage `copies`
      -- times in the declare blockers step: three Forests per copy for its
      -- {2}{G}, that many copies in hand, and that many cards left in the
      -- library so its draw is never a CR 104.3c loss. Everything else is held
      -- fixed against `board`.
      --
      -- Duplicated from Pawl.CombatEffectSpec's foliageBoard rather than hoisted
      -- into Pawl.Support, which rebuilds every spec in the tree.
      foliageBoard copies mine theirs = do
        (gs0, ours, yours) <- board mine theirs
        forest <- S.printingOf s registry "Forest"
        foliage <- S.printingOf s registry "Flash Foliage"
        let lands = List.foldl' (\g _ -> snd (S.addPermanent forest S.bob g)) gs0 (replicate (3 * copies) ())
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard forest S.bob (snd (S.addHandCard foliage S.bob g)))) lands (replicate copies ())
        pure (stocked, ours, yours)
      -- foliageBoard with a Doubling Season on the DEFENDING seat, which is the
      -- whole difference: CR 614.16 makes one Flash Foliage mint two Saprolings,
      -- and Combat.putOntoBattlefieldBlocking puts BOTH onto the battlefield
      -- blocking the same attacker before any player gets priority (CR 509.2a).
      -- One copy of the spell, so nothing here is a second casting.
      doublingFoliageBoard mine theirs = do
        (gs0, ours, yours) <- foliageBoard 1 mine theirs
        season <- S.printingOf s registry "Doubling Season"
        pure (snd (S.addPermanent season S.bob gs0), ours, yours)
      -- The attack declared and the game handed over AT the declare blockers
      -- step. S.runToStep stops when the phase first matches, which is BEFORE CR
      -- 509.1's turn-based action, so the declaration is still ahead of the
      -- handover and the answerer each leg CONTINUES with is what makes it --
      -- which is why every such answerer below carries `declaring` rather than
      -- Map.empty, an empty answer there silently unblocking the attacker. The
      -- same blockers are named here so the split cannot matter either way.
      -- Flash Foliage's "only during combat after blockers are declared" reads
      -- Combat.blockersDeclared, so no leg can cast it ahead of the declaration.
      atBlockers :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> GameState.GameState -> GameState.GameState
      atBlockers attacker blockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (declaring attacker blockers)
      -- Declares `blockers`, casts every Flash Foliage bob can afford at
      -- `victim`, and pins CR 510.1c's division of `attacker`'s damage onto
      -- `wall`.
      --
      -- The offered target set is FILTERED rather than replaced, so a leg whose
      -- victim the card's own slot does not admit takes no target at all instead
      -- of quietly succeeding on a hand-built recipient that CR 608.2b's re-read
      -- would drop.
      --
      -- The division is pinned BY ID because it is the one prompt a second
      -- blocker raises: S.identityAnswer would dump the attacker's whole point
      -- onto whichever recipient the Map surfaced first, and the 1/1 Saproling
      -- dies to it under every reading.
      casting :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      casting attacker blockers victim wall p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, rs) -> Set.filter (== Recipient.ToCreature victim) rs) sets
        Prompt.ChooseAction {} -> S.castAnswer p
        Prompt.AssignCombatDamage _ _ damager _ _
          | damager == attacker -> Map.singleton (Recipient.ToCreature wall) 1
        _ -> declaring attacker blockers p
      -- Scoped to rule 509.3e's CONDITION and not merely to Seifer: the card's
      -- other trigger (CR 508.3e) fires off the same declaration, so counting
      -- the source alone would count both.
      firedBy :: ObjectId.ObjectId -> GameState.GameState -> Int
      firedBy oid gs =
        length
          ( filter
              ( \event -> case event of
                  GameEvent.AbilityTriggered record ->
                    AbilityTriggered.source record == TriggerSource.OfObject oid
                      && case TriggeredAbility.condition (AbilityTriggered.ability record) of
                        TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> True
                        _ -> False
                  _ -> False
              )
              (S.eventsOf gs)
          )
   in Spec.describe s "CreatureBecomesBlockedByAtLeast" $ do
        -- The proving test and its control on ONE board: the same Elves, the same
        -- two Giants, the same Seifer, and only the size of the block differs. Two
        -- blockers clear rule 509.3e's floor and the Elves' one damage destroys a
        -- 3/3; one blocker does not, and nothing dies.
        Spec.it s "CR 509.3e whole card: blocked by two grants deathtouch, blocked by one does not" $ do
          (gs, mine, theirs) <- board ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant", "Hill Giant"]
          case (mine, theirs) of
            ([elves, _], [first, second]) -> do
              Spec.assertEqWith
                s
                "a Giant dies to the 1/1 when two blocked, and none dies when one did"
                (giants (afterCombat elves [first, second] gs), giants (afterCombat elves [first] gs))
                (1, 2)
              -- The control leg really fought: the lone blocker took the Elves'
              -- damage and lived through it, so the leg above differs in CR
              -- 704.5h and not in whether combat happened.
              Spec.assertEqWith
                s
                "and the lone blocker was damaged rather than untouched"
                (S.damageOf first (afterCombat elves [first] gs))
                (Just 1)
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob two Giants"
        -- CR 109.5's "you": the PlayerRelation is read against the ability's
        -- CONTROLLER, so a Seifer that bob controls watches creatures attacking
        -- ALICE. This board is the case above's firing leg with Seifer moved one
        -- seat, and nothing else changed.
        Spec.it s "CR 603.3a a Seifer the defending player controls is silent" $ do
          (gs, mine, theirs) <- board ["Llanowar Elves"] ["Hill Giant", "Hill Giant", "Seifer, Balamb Rival"]
          case (mine, theirs) of
            ([elves], [first, second, _]) ->
              Spec.assertEqWith
                s
                "the Elves attacks Seifer's own controller, so both Giants live"
                (giants (afterCombat elves [first, second] gs))
                2
            _ -> Spec.assertFailure s "fixture should give alice an Elves, and bob two Giants and a Seifer"
        -- CR 508.1b: a creature attacking a PLANESWALKER an opponent controls is
        -- not attacking that opponent, so Seifer stays silent -- which is why the
        -- arm reads Combat.attackers rather than CR 508.5's defending player,
        -- whom an attacked planeswalker resolves to. The firing board with a Jace
        -- added and the attack aimed at him, and nothing else changed.
        --
        -- Jace Beleren is stocked with loyalty by hand: S.addPermanent puts a
        -- printing onto the battlefield with no counters, and CR 704.5i would
        -- take a loyalty-0 planeswalker away before attackers are declared.
        Spec.it s "CR 508.1b attacking an opponent's planeswalker is not attacking the opponent" $ do
          (gs, mine, theirs) <- board ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant", "Hill Giant", "Jace Beleren"]
          case (mine, theirs) of
            ([elves, _], [first, second, jace]) -> do
              let ready = S.addCounter CounterKind.Loyalty 3 jace gs
                  after = afterCombatAt jace elves [first, second] ready
              Spec.assertEqWith
                s
                "the Elves is aimed at Jace, so both Giants live"
                (giants after)
                2
              -- The leg is not vacuous: the declaration really did name the
              -- planeswalker, so the silence above is CR 508.1b's and not a
              -- declaration that never happened.
              Spec.assertEqWith
                s
                "and the attack really was declared at Jace"
                (Map.lookup elves (Combat.Type.attackers (GameState.combat after)))
                (Just (AttackTarget.OfPlaneswalker jace))
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob two Giants and a Jace"
        -- Rule 509.3e's arity: ONE trigger for the declaration, not one per
        -- blocker. Deliberately counted off the event log rather than read at
        -- gameplay level, because this card cannot show the difference -- a
        -- second grant of deathtouch to the same creature is indistinguishable
        -- from the first. The falsifier is a match on the pairwise
        -- GameEvent.BecameBlocking, which is CR 509.3d's arity: that fires twice.
        Spec.it s "CR 509.3e two blockers fire it once, not once each" $ do
          (gs, mine, theirs) <- board ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant", "Hill Giant"]
          case (mine, theirs) of
            ([elves, seifer], [first, second]) ->
              Spec.assertEqWith
                s
                "one trigger from Seifer"
                (firedBy seifer (afterCombat elves [first, second] gs))
                1
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob two Giants"
        -- Rule 509.3e's SECOND sentence, "effects that add or remove blockers
        -- can also cause such abilities to trigger", and the one producer the
        -- pool has for it: a creature PUT ONTO THE BATTLEFIELD blocking. The
        -- Elves is declared blocked by ONE Hill Giant, which leaves the floor
        -- uncrossed and Seifer silent, and then Flash Foliage's Saproling joins
        -- the block and crosses it. That arrival records no
        -- GameEvent.AttackerBlocked at all -- CR 509.3c's "only if the attacking
        -- creature was an unblocked creature at that time" withholds it, and
        -- that guard is the rule's own and not a shortcut -- so the arm the
        -- cases above exercise cannot see it.
        --
        -- WHAT DOES NOT DISCRIMINATE, and each is a board a reader reaches for
        -- before this one:
        --
        --   * the cases above's TWO-Giant declaration with the token added as a
        --     third blocker. The trigger fired at the declaration already, so
        --     both readings agree at one dead Giant.
        --   * the token as the attacker's FIRST blocker, which is how
        --     Pawl.CombatEffectSpec's Flash Foliage boards are built. The count
        --     reaches one against a floor of two and both readings stay silent.
        --     The declared Hill Giant is not decoration: it is what makes the
        --     arrival a CROSSING rather than an arrival.
        --   * leaving CR 510.1c's division to the fixture. Two blockers really
        --     do ask the attacker's controller, and the Elves' single point
        --     landing on the 1/1 Saproling instead kills one creature under both
        --     readings and leaves the Giant standing under both. Pinned by id in
        --     `casting`.
        --   * counting Seifer's triggers. A partial fix that fires the trigger
        --     with nothing bound under `thatAttackingCreature` grants deathtouch
        --     to nobody and passes a count. The Giant's death is the quantity;
        --     the count comes after it.
        Spec.it s "CR 509.3e whole card: a Saproling put onto the battlefield blocking pushes an already-blocked attacker over the floor" $ do
          (gs, mine, theirs) <- foliageBoard 1 ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant"]
          case (mine, theirs) of
            ([elves, seifer], [giant]) -> do
              let declared = atBlockers elves [giant] gs
                  joined = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (casting elves [giant] elves giant) declared
                  -- The control: the same board and the same declaration, with
                  -- the spell left in bob's hand. One blocker, floor uncrossed,
                  -- no deathtouch.
                  alone = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (declaring elves [giant]) declared
              Spec.assertEqWith
                s
                "the Giant dies to the 1/1 once the token joins the block, and lives when it does not"
                (giants joined, giants alone)
                (0, 1)
              -- The control leg really fought, so the difference above is CR
              -- 704.5h and not a combat that did not happen.
              Spec.assertEqWith
                s
                "control: the lone blocker took the Elves' one point and lived through it"
                (S.damageOf giant alone)
                (Just 1)
              -- Anti-vacuity on the firing leg: the token did arrive and did
              -- join THIS attacker's block, so the Giant's death is the
              -- crossing rather than a spell that fizzled.
              Spec.assertEqWith
                s
                "CR 509.4: two creatures are blocking the Elves on the firing leg"
                (Set.size (Combat.blockersOf elves (S.runToStep (Phase.Combat CombatStep.CombatDamage) (casting elves [giant] elves giant) declared)))
                2
              -- Rule 509.3e's arity, after the gameplay quantity: the arrival
              -- fires it once, and the declaration that preceded it fired it not
              -- at all.
              Spec.assertEqWith
                s
                "and Seifer triggered exactly once across the whole combat"
                (firedBy seifer joined)
                1
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob one Giant"
        -- The floor really is a floor: the same arrival with NO declared blocker
        -- under it takes the count to one, not two, and nothing fires. The board
        -- is the case above's, Hill Giant included, and the ONE difference is
        -- that bob declares nothing with it -- so what fires the trigger there
        -- is the CROSSING and not the arrival.
        --
        -- Counted off the event log rather than read at gameplay level, and the
        -- reason is the card: the Elves' one point kills a 1/1 Saproling with or
        -- without deathtouch, so nothing on the board moves. The case above is
        -- where the gameplay quantity lives.
        Spec.it s "CR 509.3e a Saproling blocking an unblocked attacker leaves the floor uncrossed" $ do
          (gs, mine, theirs) <- foliageBoard 1 ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant"]
          case (mine, theirs) of
            ([elves, seifer], [giant]) -> do
              let joined = S.runToStep (Phase.Combat CombatStep.CombatDamage) (casting elves [] elves giant) (atBlockers elves [] gs)
              Spec.assertEqWith
                s
                "one blocker is under rule 509.3e's floor of two, so Seifer never triggered"
                (firedBy seifer joined)
                0
              -- Anti-vacuity: the token really did arrive and really is blocking,
              -- so the silence is the count and not a spell that never resolved.
              Spec.assertEqWith
                s
                "and the Saproling is blocking the Elves all the same"
                (Combat.blockersOf elves joined)
                (Set.fromList (S.tokensOf joined))
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob one Giant to leave undeclared"
        -- The other side of the same comparison: once the floor HAS been
        -- crossed, a further arrival does not cross it again. Two Flash Foliages
        -- against one declared Hill Giant take the block from one to three, and
        -- the attacker becomes blocked by two or more creatures exactly once.
        --
        -- Off the event log for the case above's reason, and here it is forced:
        -- a second grant of deathtouch to a creature that already has it moves
        -- nothing at all on any board.
        Spec.it s "CR 509.3e a further arrival past the floor does not cross it again" $ do
          (gs, mine, theirs) <- foliageBoard 2 ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant"]
          case (mine, theirs) of
            ([elves, seifer], [giant]) -> do
              let joined = S.runToStep (Phase.Combat CombatStep.CombatDamage) (casting elves [giant] elves giant) (atBlockers elves [giant] gs)
              -- Anti-vacuity FIRST here, because the assertion under test is a
              -- count that a board where the second spell never resolved would
              -- also satisfy.
              Spec.assertEqWith
                s
                "both Saprolings arrived, so the Elves is blocked by three creatures"
                (Set.size (Combat.blockersOf elves joined))
                3
              Spec.assertEqWith
                s
                "and Seifer triggered once, on the arrival that took the count to two"
                (firedBy seifer joined)
                1
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob one Giant"
        -- Rule 509.3e's floor crossed by TWO arrivals at once, which is the case
        -- no per-arrival reading of the live blocker count can state: CR 614.16
        -- doubles Flash Foliage's Saproling, so the block goes from one declared
        -- Hill Giant straight to three and the count never lands on two. The
        -- attacker plainly did become blocked by two or more creatures, and once.
        --
        -- The case above's board with a Doubling Season added on the defending
        -- seat and one Flash Foliage instead of two, so the difference from it is
        -- the SIMULTANEITY rather than the number of arrivals: there the count
        -- steps 1, 2, 3 and here it jumps 1, 3.
        Spec.it s "CR 509.3e whole card: two Saprolings arriving at once cross the floor together" $ do
          (gs, mine, theirs) <- doublingFoliageBoard ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant"]
          case (mine, theirs) of
            ([elves, seifer], [giant]) -> do
              let declared = atBlockers elves [giant] gs
                  joined = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (casting elves [giant] elves giant) declared
                  -- The control: the same board and the same declaration with the
                  -- spell left in bob's hand, so the Giant faces the Elves alone.
                  alone = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (declaring elves [giant]) declared
              Spec.assertEqWith
                s
                "the Giant dies to the 1/1 once the two Saprolings join the block, and lives when they do not"
                (giants joined, giants alone)
                (0, 1)
              -- Anti-vacuity: the doubling really happened and both tokens really
              -- are blocking THIS attacker, so the death above is the crossing
              -- rather than a spell that fizzled or a Season that did nothing.
              Spec.assertEqWith
                s
                "CR 614.16: three creatures are blocking the Elves on the firing leg"
                (Set.size (Combat.blockersOf elves (S.runToStep (Phase.Combat CombatStep.CombatDamage) (casting elves [giant] elves giant) declared)))
                3
              -- Rule 509.3e's arity, after the gameplay quantity: the pair of
              -- arrivals is ONE crossing, not one apiece.
              Spec.assertEqWith
                s
                "and Seifer triggered exactly once across the whole combat"
                (firedBy seifer joined)
                1
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob one Giant"
        -- The same pair of arrivals landing on an UNBLOCKED attacker, which is
        -- the leg where CR 509.3c does record GameEvent.AttackerBlocked -- one
        -- creature was blocking the Elves as it became blocked, so that event's
        -- tally is under the floor and the second Saproling is what crosses it.
        -- Still ONE trigger between the two events, where a becoming that counted
        -- the arrivals after it would fire for that as well.
        --
        -- The case above's board with bob declaring nothing, and counted off the
        -- event log rather than at gameplay level for the same reason the
        -- uncrossed case above is: deathtouch or not, the Elves' one point kills
        -- a 1/1 Saproling, so no board moves.
        Spec.it s "CR 509.3e two Saprolings arriving at once at an unblocked attacker fire it once, not twice" $ do
          (gs, mine, theirs) <- doublingFoliageBoard ["Llanowar Elves", "Seifer, Balamb Rival"] ["Hill Giant"]
          case (mine, theirs) of
            ([elves, seifer], [giant]) -> do
              let joined = S.runToStep (Phase.Combat CombatStep.CombatDamage) (casting elves [] elves giant) (atBlockers elves [] gs)
              -- Anti-vacuity FIRST: the assertion under test is a count, which a
              -- board where the spell never resolved would also satisfy.
              Spec.assertEqWith
                s
                "both Saprolings arrived, so the Elves is blocked by two creatures"
                (Set.size (Combat.blockersOf elves joined))
                2
              Spec.assertEqWith
                s
                "and Seifer triggered once for the pair, not once for the becoming and once for the crossing"
                (firedBy seifer joined)
                1
            _ -> Spec.assertFailure s "fixture should give alice an Elves and a Seifer, and bob one Giant to leave undeclared"

-- CR 509.3c: "Whenever [a creature] becomes blocked, . . ." -- the ATTACKING
-- side of the same declaration selfBlocksSpec reads, matched against
-- GameEvent.AttackerBlocked.
--
-- Sacred Prey {G} Creature -- Horse 1/1, "Whenever this creature becomes blocked,
-- you gain 1 life", is the card: the cheapest producer in the pool, and its
-- payload names nothing about the blockers, so these cases isolate the
-- CONDITION. The gain lands on the ATTACKING seat (alice), which is the seat
-- combat damage never moves here, so every number below is the trigger's alone.
selfBecomesBlockedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBecomesBlockedSpec s registry =
  let noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
   in Spec.describe s "SelfBecomesBlocked" $ do
        -- The proving test, and its control: the same game with CR 509.1's
        -- declaration switched off. 20 + 1 = 21 blocked, 20 declining -- and bob
        -- moves the other way, 20 blocked against 20 - 1 = 19 letting it through,
        -- so no single number can be read two ways.
        Spec.it s "CR 509.3c whole card: becoming blocked gains 1 life, going unblocked gains none" $ do
          (gs, mine, _) <- board ["Sacred Prey"] ["Goblin Piker"]
          let blocked = S.runCombat S.aggressiveAnswer gs
              unblocked = S.runCombat noBlocks gs
          case mine of
            [prey] -> do
              Spec.assertEqWith s "alice gained 1" (S.lifeOf S.alice blocked) (Just 21)
              Spec.assertEqWith s "and bob took nothing: the blocked Prey's 1 went to the Piker" (S.lifeOf S.bob blocked) (Just 20)
              Spec.assertBool s (not (S.onBattlefield prey blocked)) "the 1/1 Prey died to the Piker's 2, after its trigger had resolved"
              Spec.assertEqWith s "control leg: unblocked, so no gain" (S.lifeOf S.alice unblocked) (Just 20)
              Spec.assertEqWith s "and its 1 gets through" (S.lifeOf S.bob unblocked) (Just 19)
            _ -> Spec.assertFailure s "fixture should give alice one Sacred Prey"
        -- CR 509.3c's "only once each combat for that creature, even if it's
        -- blocked by multiple creatures". Two Pikers block the one Prey, so two
        -- GameEvent.BecameBlocking are recorded and exactly one
        -- GameEvent.AttackerBlocked. The falsifier is a condition matched against
        -- the declaration's pairs instead: that fires twice, for 22.
        Spec.it s "CR 509.3c two blockers on one attacker still gain 1, not 2" $ do
          (gs, _, _) <- board ["Sacred Prey"] ["Goblin Piker", "Goblin Piker"]
          Spec.assertEqWith s "one gain of 1" (S.lifeOf S.alice (S.runCombat S.aggressiveAnswer gs)) (Just 21)
        -- CR 509.3a and CR 509.3c on one board, which is what tells the two arms
        -- apart: alice's Prey becomes blocked by bob's Guardian, so alice gains 1
        -- and bob gains 3 off a single declaration. Either arm reading the other's
        -- event moves one of those two numbers.
        Spec.it s "CR 509.3a and CR 509.3c fire on opposite sides of one declaration" $ do
          (gs, _, _) <- board ["Sacred Prey"] ["Pride Guardian"]
          let after = S.runCombat S.aggressiveAnswer gs
          Spec.assertEqWith s "the attacker's controller gained 1" (S.lifeOf S.alice after) (Just 21)
          Spec.assertEqWith s "the blocker's controller gained 3" (S.lifeOf S.bob after) (Just 23)
        -- The converse, and CR 509.3c's own words: a creature that BLOCKS does not
        -- become blocked. Here the Prey is bob's and blocking a Piker; the
        -- falsifier is an arm that matched GameEvent.BecameBlocking, which would
        -- put bob at 21.
        Spec.it s "CR 509.3c blocking is not becoming blocked, so a blocking Sacred Prey gains nothing" $ do
          (gs, _, _) <- board ["Goblin Piker"] ["Sacred Prey"]
          Spec.assertEqWith s "bob gained nothing" (S.lifeOf S.bob (S.runCombat S.aggressiveAnswer gs)) (Just 20)
        -- CR 509.3c's guard on its THIRD producer, and the one thing rule
        -- 509.3e's arrival road must not cost: "It will also trigger if that
        -- creature becomes blocked by an effect or by a creature that's put onto
        -- the battlefield as a blocker, but only if the attacking creature was
        -- an unblocked creature at that time." The Prey is already blocked by a
        -- declared Piker when Flash Foliage's Saproling joins it, so the arrival
        -- finds a blocked creature and this trigger must stay silent.
        --
        -- A REGRESSION FENCE with a NAMED falsifier rather than a pair of legs,
        -- because both legs of any pair read 21 and only a wrong engine reads
        -- 22. Combat.putOntoBattlefieldBlocking withholds
        -- GameEvent.AttackerBlocked for exactly this, and dropping that guard is
        -- the shortest-looking way to make an arrival reach
        -- CreatureBecomesBlockedByAtLeast above. It is the wrong way, and 22 is
        -- what says so.
        Spec.it s "CR 509.3c a Saproling joining an already-blocked attacker does not make it become blocked twice" $ do
          (gs0, mine, theirs) <- board ["Sacred Prey"] ["Goblin Piker"]
          forest <- S.printingOf s registry "Forest"
          foliage <- S.printingOf s registry "Flash Foliage"
          case (mine, theirs) of
            ([prey], [piker]) -> do
              -- Three Forests for Flash Foliage's {2}{G} and one card left in
              -- bob's library so its draw is not a CR 104.3c loss.
              let lands = List.foldl' (\g _ -> snd (S.addPermanent forest S.bob g)) gs0 (replicate 3 ())
                  gs = snd (S.addLibraryCard forest S.bob (snd (S.addHandCard foliage S.bob lands)))
                  -- Handed over AT the declare blockers step, which is before
                  -- CR 509.1's turn-based action: `casting` below is what
                  -- declares the Piker, through its S.aggressiveAnswer base, and
                  -- Flash Foliage's "after blockers are declared" restriction is
                  -- what keeps the spell behind that declaration.
                  declared = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
                  casting :: Prompt.Prompt r -> r
                  casting p = case p of
                    Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, rs) -> Set.filter (== Recipient.ToCreature prey) rs) sets
                    Prompt.ChooseAction {} -> S.castAnswer p
                    Prompt.AssignCombatDamage {} -> Map.singleton (Recipient.ToCreature piker) 1
                    _ -> S.aggressiveAnswer p
                  joined = S.runToStep (Phase.Combat CombatStep.CombatDamage) casting declared
              Spec.assertEqWith s "alice gained 1 for the declaration and nothing for the arrival" (S.lifeOf S.alice joined) (Just 21)
              -- Anti-vacuity: the Saproling really did arrive and really is
              -- blocking the Prey, so the silence is CR 509.3c's guard and not a
              -- spell that never resolved.
              Spec.assertEqWith s "CR 509.4: two creatures are blocking the Prey" (Set.size (Combat.blockersOf prey joined)) 2
            _ -> Spec.assertFailure s "fixture should give alice a Sacred Prey and bob a Goblin Piker"

-- CR 509.1h read from the UNBLOCKED side: "an attacking creature ... with no
-- creatures declared as blockers for it becomes an unblocked creature", which
-- the glossary's "attacks and isn't blocked" entry sends here.
-- selfBecomesBlockedSpec above is the other branch of the same declaration.
--
-- Eternal of Harsh Truths {2}{U} Creature -- Zombie Cleric 1/3 is the card, and
-- it prints BOTH branches: afflict 2 (CR 702.130a, CR 509.3c) and "whenever this
-- creature attacks and isn't blocked, draw a card". One board therefore shows
-- the two branches excluding each other, and their observables are disjoint --
-- a life total on the defending seat against a card in the attacking seat's
-- hand.
--
-- THREE SEATS, for afflictSpec's reason: at two players the defending player and
-- the attacker's one opponent collapse.
--
-- Every number is distinct on purpose: afflict is 2, the Eternal's power is 1,
-- and the draw is 1 card. A leg that lost 2 life cannot be read as a leg that
-- took 1 combat damage.
--
-- alice's library is stocked, or the draw would find nothing and CR 121.4 would
-- lose her the game -- leaving the leg that is supposed to show a card in hand
-- showing 0 for a reason that is not the trigger.
selfAttacksUnblockedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfAttacksUnblockedSpec s registry =
  let -- Attacks `who` with everything and lets them block with everything.
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.ChooseAttackTarget {} -> S.attackTo who p
        _ -> S.aggressiveAnswer p
      -- The same, with CR 509.1's declaration switched off -- the control leg,
      -- and the only difference between the two answerers.
      declining :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      declining who p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> attacking who p
      stock piker gs = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) gs [1 :: Int, 2, 3]
      board theirs others = do
        eternal <- S.printingOf s registry "Eternal of Harsh Truths"
        piker <- S.printingOf s registry "Goblin Piker"
        let (gs, ours, yours, hers) = S.threePlayerCombat [eternal] (fmap (const piker) theirs) (fmap (const piker) others)
        pure (stock piker gs, ours, yours, hers)
      -- All three life totals plus alice's hand as one reading, so no mutation
      -- can hide behind the order the assertions happen to be written in.
      state gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs, S.handSize S.alice gs)
   in Spec.describe s "SelfAttacksUnblocked" $ do
        -- The proving test and its control, on ONE board differing only in the
        -- answer to Prompt.DeclareBlockers. Unblocked: alice draws, and bob takes
        -- the Eternal's 1. Blocked: alice draws nothing, and bob loses 2 to
        -- afflict instead of taking damage. Before this change the declaration
        -- recorded no event for an unblocked attacker at all, so the first leg
        -- read 0 cards.
        Spec.it s "CR 509.1h whole card: an unblocked Eternal of Harsh Truths draws a card, a blocked one does not" $ do
          (gs, _, yours, _) <- board [()] [()]
          let unblocked = S.runCombat (declining S.bob) gs
              blocked = S.runCombat (attacking S.bob) gs
          case yours of
            [piker] -> do
              Spec.assertEqWith s "unblocked: alice drew 1, bob took the Eternal's 1" (state unblocked) (Just 20, Just 19, Just 20, 1)
              Spec.assertEqWith s "blocked: no draw, and afflict 2 instead of damage" (state blocked) (Just 20, Just 18, Just 20, 0)
              Spec.assertBool s (S.onBattlefield piker unblocked) "the unblocked leg left bob's Piker alone"
            _ -> Spec.assertFailure s "fixture should give bob one Goblin Piker"
        -- CR 509.1h's last sentence: "a creature remains blocked even if all the
        -- creatures blocking it are removed from combat." The 1/3 Eternal kills
        -- the 2/1 Piker at CR 510.2, emptying its Combat.blockers entry, and
        -- alice still draws nothing through the end of combat. The falsifier is
        -- an implementation that samples the map for an attacker with no
        -- CURRENT blockers rather than recording the declaration's own event.
        Spec.it s "CR 509.1h losing every blocker does not make the Eternal unblocked" $ do
          (gs, ours, yours, _) <- board [()] [()]
          let after = S.runCombat (attacking S.bob) gs
          case (ours, yours) of
            ([eternal], [piker]) -> do
              Spec.assertBool s (not (S.onBattlefield piker after)) "the 2/1 Piker died to the Eternal's 1"
              Spec.assertBool s (S.onBattlefield eternal after) "and the 1/3 Eternal survived the Piker's 2"
              Spec.assertEqWith s "alice still drew nothing" (S.handSize S.alice after) 0
            _ -> Spec.assertFailure s "fixture should give alice an Eternal and bob a Piker"
        -- The board where NOBODY can block, which is the one the old code could
        -- not see: with no creature on either defending side, CR 509.1's
        -- declaration raises no prompt at all, and rule 509.1h still makes the
        -- attacker unblocked. The falsifier is recording the event inside the
        -- loop that is guarded on there being a legal blocker.
        Spec.it s "CR 509.1h an attacker nobody could block is unblocked too" $ do
          (gs, _, _, _) <- board [] []
          Spec.assertEqWith s "alice drew 1, bob took the Eternal's 1" (state (S.runCombat (attacking S.bob) gs)) (Just 20, Just 19, Just 20, 1)
        -- CR 508.5: the third seat. The only difference from the first leg above
        -- is which opponent was attacked, and the draw is ONE either way -- the
        -- ability's controller draws, not a card per opponent and not a card per
        -- seat that did not block.
        Spec.it s "CR 509.1h the draw follows the attack rather than the seat count" $ do
          (gs, _, _, _) <- board [()] [()]
          Spec.assertEqWith s "carol took the 1 this time, and alice still drew exactly 1" (state (S.runCombat (declining S.carol) gs)) (Just 20, Just 20, Just 19, 1)
        -- CR 603.2: the condition is the BEARER's own attack. bob's Eternal is
        -- standing still while alice's Piker goes by unblocked, so the
        -- declaration records a GameEvent.AttackerUnblocked naming somebody else.
        Spec.it s "CR 603.2 a bystanding Eternal of Harsh Truths draws nothing" $ do
          eternal <- S.printingOf s registry "Eternal of Harsh Truths"
          piker <- S.printingOf s registry "Goblin Piker"
          let (gs, _, _, _) = S.threePlayerCombat [piker] [eternal] []
              after = S.runCombat (declining S.bob) gs
          Spec.assertEqWith s "bob took the Piker's 2 and drew nothing" (S.lifeOf S.bob after, S.handSize S.bob after) (Just 18, 0)

-- CR 702.85a's cascade, the first keyword ability that functions on the STACK:
-- "When you cast this spell, exile cards from the top of your library until you
-- exile a nonland card whose mana value is less than this spell's mana value. You
-- may cast that card without paying its mana cost if the resulting spell's mana
-- value is less than this spell's mana value. Then put all cards exiled this way
-- that weren't cast on the bottom of your library in a random order."
--
-- Bloodbraid Elf {2}{R}{G} Creature -- Elf Berserker 3/2 -- "Haste / Cascade"
-- (Oracle text checked 2026-09-10) -- is the producer, and the cheapest printing
-- carrying cascade once. Apex Devastator {8}{G}{G} Creature -- Chimera Hydra
-- 10/10 -- "Cascade, cascade, cascade, cascade" (Oracle text checked 2026-09-14)
-- is the producer for CR 702.85c's per-instance clause, two cases below.
--
-- The library is stocked so that each conjunct of rule 702.85a's walk is what
-- stops or fails to stop it: a Hill Giant of mana value 4 exactly (the Elf's own,
-- so "less than" and not "no greater than" is what passes it by), a Mountain
-- (mana value 0, a LAND, so the cheapness alone is not enough), Russet Wolves at
-- 4 again, and then the Goblin Piker at 2, which ends it. Think Twice sits under
-- the Piker and is never reached, which is how the walk's stopping is visible at
-- all.
cascadeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cascadeSpec s registry = Spec.describe s "Cascade" $ do
  -- CR 702.85c: each instance triggers separately, so the mint is one ability per
  -- instance, poisonous' reading. The falsifier is a roster that mints it once.
  Spec.it s "CR 702.85a cascade is minted for a spell on the stack and nowhere else" $ do
    Spec.assertEqWith s "the stack roster mints it" (Keyword.stackTriggeredAbilitiesOf (Map.singleton Keyword.Type.Cascade 1)) [Keyword.cascade]
    Spec.assertEqWith s "and the battlefield roster does not" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Cascade 1)) []
    Spec.assertEqWith s "rule 702.85a's condition is the cast" (TriggeredAbility.condition Keyword.cascade) TriggerCondition.SelfCast

  -- THE PROVING TEST.
  Spec.it s "CR 702.85a casting Bloodbraid Elf exiles down to the Goblin Piker, casts it free and bottoms the rest" $ do
    elf <- S.printingOf s registry "Bloodbraid Elf"
    giant <- S.printingOf s registry "Hill Giant"
    mountain <- S.printingOf s registry "Mountain"
    wolves <- S.printingOf s registry "Russet Wolves"
    piker <- S.printingOf s registry "Goblin Piker"
    think <- S.printingOf s registry "Think Twice"
    forest <- S.printingOf s registry "Forest"
    let base = Setup.emptyGame S.bothPlayers
        -- S.addLibraryCard puts each card ON TOP, so this stocks the library
        -- bottom first.
        (_, g1) = S.addLibraryCard think S.alice base
        (_, g2) = S.addLibraryCard piker S.alice g1
        (_, g3) = S.addLibraryCard wolves S.alice g2
        (_, g4) = S.addLibraryCard mountain S.alice g3
        (_, g5) = S.addLibraryCard giant S.alice g4
        -- Rule 702.85a's {2}{R}{G} has to be paid for real: the FREE cast is the
        -- Piker's, and a board that paid nothing for either would not tell them
        -- apart.
        (_, g6) = S.addPermanent mountain S.alice g5
        (_, g7) = S.addPermanent mountain S.alice g6
        (_, g8) = S.addPermanent forest S.alice g7
        (_, g9) = S.addPermanent forest S.alice g8
        (_, g10) = S.addHandCard elf S.alice g9
        before =
          g10
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        after = S.runPure cascading before Engine.priorityLoop
        namesIn zone pid gs = Set.fromList (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs))
        orderedIn zone pid gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)
        named = CardName.MkCardName . Text.pack
    Spec.assertEqWith
      s
      "the Piker the walk stopped at was cast without paying its mana cost, and the Elf behind it resolved too"
      (namesIn Zone.Battlefield S.alice after)
      (Set.fromList [named "Bloodbraid Elf", named "Goblin Piker", named "Mountain", named "Forest"])
    Spec.assertEqWith
      s
      "the three cards the walk passed over sit under the card it never reached, in the order the random-order channel handed back"
      (orderedIn Zone.Library S.alice after)
      [named "Think Twice", named "Hill Giant", named "Russet Wolves", named "Mountain"]
    -- Proxies, AFTER the two behavioural assertions so neither can absorb a
    -- mutation: nothing is left in exile (rule 702.85a's last sentence empties
    -- it), and alice paid for the Elf alone.
    Spec.assertEqWith s "nothing stayed in exile" (namesIn Zone.Exile S.alice after) Set.empty
    Spec.assertEqWith s "four lands paid the Elf's {2}{R}{G} and nothing paid the Piker's" (S.tappedCount S.alice after) 4

  -- CR 702.85a's SECOND condition, driven through a real cascade rather than
  -- through Pawl.Engine.Resolve.Effect.offerCast alone: Flaxen Intruder //
  -- Welcome Home {G} Creature -- Human Berserker 1/2 // {5}{G}{G} Sorcery --
  -- Adventure (Oracle text checked 2026-09-12) is a card of mana value 1, so the
  -- Elf's walk stops at it, and its Adventure half (CR 715.3) has mana value 7, so
  -- the bound the offer carries refuses that half alone.
  --
  -- The answerer PREFERS the Adventure by name, so a bound that admitted both
  -- halves would put three Bear tokens on the battlefield and no Berserker. That
  -- the Berserker enters is the offer being narrowed to one half, not a
  -- preference of the engine's.
  Spec.it s "CR 702.85a cascading into an adventurer card withholds the half the bound refuses" $ do
    elf <- S.printingOf s registry "Bloodbraid Elf"
    giant <- S.printingOf s registry "Hill Giant"
    mountain <- S.printingOf s registry "Mountain"
    intruder <- S.printingOf s registry "Flaxen Intruder"
    think <- S.printingOf s registry "Think Twice"
    forest <- S.printingOf s registry "Forest"
    let base = Setup.emptyGame S.bothPlayers
        -- Bottom first: the Giant is passed for being the Elf's own mana value,
        -- the Mountain for being a land, and the Intruder ends the walk. Think
        -- Twice is never reached.
        (_, g1) = S.addLibraryCard think S.alice base
        (_, g2) = S.addLibraryCard intruder S.alice g1
        (_, g3) = S.addLibraryCard mountain S.alice g2
        (_, g4) = S.addLibraryCard giant S.alice g3
        (_, g5) = S.addPermanent mountain S.alice g4
        (_, g6) = S.addPermanent mountain S.alice g5
        (_, g7) = S.addPermanent forest S.alice g6
        (_, g8) = S.addPermanent forest S.alice g7
        (_, g9) = S.addHandCard elf S.alice g8
        before =
          g9
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        after = S.runPure cascadingIntoTheAdventure before Engine.priorityLoop
        namesIn zone pid gs = Set.fromList (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs))
        named = CardName.MkCardName . Text.pack
    Spec.assertEqWith
      s
      "the creature half was the only one offered, so the Berserker entered and no Bear token did"
      (namesIn Zone.Battlefield S.alice after)
      (Set.fromList [named "Bloodbraid Elf", named "Flaxen Intruder", named "Mountain", named "Forest"])
    -- Proxies, AFTER the behaviour: the walk really did stop at the Intruder
    -- rather than run out of library, and nothing paid for the free half.
    Spec.assertEqWith s "the two cards the walk passed over are back in the library under the one it never reached" (length (Game.zoneMembers Zone.Library S.alice after)) 3
    Spec.assertEqWith s "nothing stayed in exile" (namesIn Zone.Exile S.alice after) Set.empty
    Spec.assertEqWith s "four lands paid the Elf's {2}{R}{G} and nothing paid the Berserker's {G}" (S.tappedCount S.alice after) 4

  -- THE PROVING TEST for CR 702.85c. The library's top four cards are each a
  -- nonland of mana value under the Devastator's ten, so every walk stops at the
  -- first card it exiles and casts it; which cascade takes which is immaterial,
  -- since the four instances are identical. Think Twice sits underneath and is
  -- never reached, so a fifth walk would be visible as its absence.
  --
  -- The four hits carry DISTINCT names, so the battlefield tells one cascade from
  -- two from four: a roster that mints cascade once leaves the Giant, the Wolves
  -- and the Spider in the library.
  Spec.it s "CR 702.85c each of Apex Devastator's four printed cascades triggers" $ do
    apex <- S.printingOf s registry "Apex Devastator"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    wolves <- S.printingOf s registry "Russet Wolves"
    spider <- S.printingOf s registry "Giant Spider"
    think <- S.printingOf s registry "Think Twice"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    let base = Setup.emptyGame S.bothPlayers
        -- S.addLibraryCard puts each card ON TOP, so this stocks the library
        -- bottom first: the Piker ends up on top.
        (_, g1) = S.addLibraryCard think S.alice base
        (_, g2) = S.addLibraryCard spider S.alice g1
        (_, g3) = S.addLibraryCard wolves S.alice g2
        (_, g4) = S.addLibraryCard giant S.alice g3
        (_, g5) = S.addLibraryCard piker S.alice g4
        -- {8}{G}{G} is paid for real, so the free casts are the cascades' and a
        -- board that paid nothing for anything would not tell them apart.
        lands = replicate 8 mountain <> replicate 2 forest
        g6 = List.foldl' (\g printing -> snd (S.addPermanent printing S.alice g)) g5 lands
        (_, g7) = S.addHandCard apex S.alice g6
        before =
          g7
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        after = S.runPure cascading before Engine.priorityLoop
        namesIn zone pid gs = Set.fromList (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs))
        orderedIn zone pid gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)
        named = CardName.MkCardName . Text.pack
    Spec.assertEqWith
      s
      "all four walks cast the card they stopped at, so four free creatures joined the Devastator"
      (namesIn Zone.Battlefield S.alice after)
      (Set.fromList [named "Apex Devastator", named "Goblin Piker", named "Hill Giant", named "Russet Wolves", named "Giant Spider", named "Mountain", named "Forest"])
    Spec.assertEqWith
      s
      "the four walks took four cards and the fifth was never reached"
      (orderedIn Zone.Library S.alice after)
      [named "Think Twice"]
    -- Proxies, AFTER the behaviour: rule 702.85a's last sentence empties exile,
    -- and the ten lands paid the Devastator alone.
    Spec.assertEqWith s "nothing stayed in exile" (namesIn Zone.Exile S.alice after) Set.empty
    Spec.assertEqWith s "ten lands paid the {8}{G}{G} and nothing paid the four free spells" (S.tappedCount S.alice after) 10

  -- CR 707.2: the printed count is a copiable value, so a Clone of Apex
  -- Devastator has four cascades of its own. Read through
  -- Pawl.Engine.Projection.keywordsOf, which is where a copy and the card
  -- underneath it part company -- Game.faceOf would answer Clone's own printed
  -- face for the copy and the Devastator's for the original, and pass either way.
  Spec.it s "CR 707.2 a Clone of Apex Devastator copies all four instances" $ do
    apex <- S.printingOf s registry "Apex Devastator"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (apexId, board) = S.addPermanent apex S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = S.runPure copyingTheDevastator staged (Stack.resolveTop >> Engine.settleForPriority)
        isClone oid = fmap Face.name (Game.faceOf oid resolved) == Just (CardName.MkCardName (Text.pack "Clone"))
    case filter isClone (Set.toList (GameState.battlefield resolved)) of
      [cloneId] ->
        Spec.assertEqWith s "the Clone projects four cascades" (Projection.keywordsOf cloneId resolved) (Map.singleton Keyword.Type.Cascade 4)
      _ -> Spec.assertFailure s "expected exactly one Clone on the battlefield"
    Spec.assertEqWith s "and the Devastator it copied still has its own four" (Projection.keywordsOf apexId resolved) (Map.singleton Keyword.Type.Cascade 4)

-- CR 702.60a's ripple, cascade's neighbour on the stack roster: "When you cast
-- this spell, you may reveal the top N cards of your library, or, if there are
-- fewer than N cards in your library, you may reveal all the cards in your
-- library. If you reveal cards from your library this way, you may cast any of
-- those cards with the same name as this spell without paying their mana costs,
-- then put all revealed cards not cast this way on the bottom of your library in
-- any order."
--
-- Surging Dementia {1}{B} Sorcery -- "Ripple 4 / Target player discards a card"
-- (Oracle text checked 2026-09-14) -- is the producer. Its reminder text drops
-- the rule's "in any order"; the rule is what is transcribed.
--
-- The library is stocked so that the SAME-NAME filter is what picks the cards
-- out and not their position: two more Surging Dementias sit at the first and
-- third place of the top four, with a Goblin Piker and a Hill Giant between and
-- after them. Both of those are castable creatures, so a filter admitting
-- everything would put them on the battlefield for free. Think Twices sit under
-- the four and are never revealed, which is how the reveal's DEPTH is visible at
-- all.
rippleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rippleSpec s registry =
  let -- The library, stocked bottom first (S.addLibraryCard puts each card ON
      -- TOP), under two Swamps for the printed {1}{B} and four cards in bob's
      -- hand for the discards to take.
      --
      -- `under` is the depth of Think Twices beneath the top four, and it is what
      -- makes the COUNT of casts observable at all: a free cast's own ripple
      -- triggers again (CR 702.60a), and a card this reveal passed over goes to
      -- the BOTTOM -- so with fillers in between, a second Dementia left behind by
      -- a one-card offer is out of the next reveal's reach rather than picked up
      -- by it.
      board top under = do
        dementia <- S.printingOf s registry "Surging Dementia"
        think <- S.printingOf s registry "Think Twice"
        swamp <- S.printingOf s registry "Swamp"
        mountain <- S.printingOf s registry "Mountain"
        stock <- mapM (S.printingOf s registry) top
        let base = Setup.emptyGame S.bothPlayers
            g1 = List.foldl' (\g _ -> snd (S.addLibraryCard think S.alice g)) base (replicate under ())
            g2 = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) g1 (reverse stock)
            g3 = S.landsFor swamp S.alice 2 g2
            g4 = List.foldl' (\g _ -> snd (S.addHandCard mountain S.bob g)) g3 (replicate 4 ())
            (oid, g5) = S.addHandCard dementia S.alice g4
        pure (oid, g5 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice})
      -- alice casts the Dementia at bob and the stack is resolved down to empty,
      -- keeping the whole transcript: CR 401.4's arrangement is a RESPONSE and
      -- not a board, so nothing else can tell a stated order from a random one.
      runWith :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> ([Response.Response], GameState.GameState)
      runWith answer oid gs =
        let step (log_, g) action = let ((_, g'), more) = Replay.record answer g (action >> Engine.settleForPriority) in (log_ <> more, g')
            resolveAll acc = if null (GameState.stack (snd acc)) then acc else resolveAll (step acc Stack.resolveTop)
         in resolveAll (step ([], gs) (S.cast S.alice oid))
      run = runWith rippling
      namesIn zone pid gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)
      named = CardName.MkCardName . Text.pack
      arrangements = length . filter isArrangement
   in Spec.describe s "Ripple" $ do
        -- CR 702.60b: each instance triggers separately, so the mint is one
        -- ability per instance, cascade's reading. The falsifier is a roster that
        -- mints it once.
        Spec.it s "CR 702.60a ripple is minted for a spell on the stack and nowhere else" $ do
          Spec.assertEqWith s "the stack roster mints it" (Keyword.stackTriggeredAbilitiesOf (Map.singleton (Keyword.Type.Ripple 4) 1)) [Keyword.ripple 4]
          Spec.assertEqWith s "and the battlefield roster does not" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Ripple 4) 1)) []
          Spec.assertEqWith s "CR 702.60b two printed instances are two abilities" (Keyword.stackTriggeredAbilitiesOf (Map.singleton (Keyword.Type.Ripple 4) 2)) [Keyword.ripple 4, Keyword.ripple 4]
          Spec.assertEqWith s "rule 702.60a's condition is the cast" (TriggeredAbility.condition (Keyword.ripple 4)) TriggerCondition.SelfCast

        -- THE PROVING TEST.
        Spec.it s "CR 702.60a the reveal casts BOTH same-named cards free and bottoms the rest" $ do
          (oid, gs) <- board ["Surging Dementia", "Goblin Piker", "Surging Dementia", "Hill Giant"] 5
          let (_, after) = run oid gs
          Spec.assertEqWith
            s
            "both Surging Dementias among the four were cast, so bob discarded three cards in all"
            (S.handSize S.bob after)
            1
          Spec.assertEqWith
            s
            "and the two cards that do not share the spell's name were not cast, free or otherwise"
            (Set.fromList (namesIn Zone.Battlefield S.alice after))
            (Set.singleton (named "Swamp"))
          -- Proxies, AFTER the two behavioural assertions so neither can absorb a
          -- mutation: three Dementias finished in the graveyard, and only the
          -- first of them was paid for.
          Spec.assertEqWith s "three Surging Dementias reached the graveyard" (length (filter (== named "Surging Dementia") (namesIn Zone.Graveyard S.alice after))) 3
          Spec.assertEqWith s "two Swamps paid the printed {1}{B} and nothing paid the two free casts" (S.tappedCount S.alice after) 2

        -- CR 702.60a's "YOU MAY reveal", the board above with ONE thing changed --
        -- the answer to that one question. Declining it is not the same as
        -- revealing and casting nothing: the four cards stay where they were,
        -- which is what a reveal that happened would not leave behind.
        Spec.it s "CR 702.60a declining the reveal leaves the top of the library where it was" $ do
          (oid, gs) <- board ["Surging Dementia", "Goblin Piker", "Surging Dementia", "Hill Giant"] 5
          let (log_, after) = runWith decliningTheReveal oid gs
          Spec.assertEqWith s "no card was revealed, so nothing was cast and bob discarded once" (S.handSize S.bob after) 3
          Spec.assertEqWith
            s
            "and the four cards are still on top in the order they were stocked"
            (take 4 (namesIn Zone.Library S.alice after))
            [named "Surging Dementia", named "Goblin Piker", named "Surging Dementia", named "Hill Giant"]
          -- A proxy, AFTER the behaviour: nothing reached the bottom, so CR
          -- 401.4 had nothing to arrange.
          Spec.assertEqWith s "CR 401.4 nothing was arranged" (arrangements log_) 0

        -- The negative, the board above with ONE thing changed -- which cards the
        -- top four are. Nothing shares the spell's name, so rule 702.60a's offer
        -- names nobody and all four go to the bottom.
        Spec.it s "CR 702.60a a reveal finding no same-named card casts nothing and bottoms all four" $ do
          (oid, gs) <- board ["Goblin Piker", "Hill Giant", "Russet Wolves", "Mountain"] 1
          let (log_, after) = run oid gs
          Spec.assertEqWith s "bob discarded to the one Dementia alice paid for and no other" (S.handSize S.bob after) 3
          Spec.assertEqWith
            s
            "the four revealed cards went under the card the reveal never reached"
            (take 1 (namesIn Zone.Library S.alice after), length (namesIn Zone.Library S.alice after))
            ([named "Think Twice"], 5)
          -- CR 401.4, which is the whole of what rule 702.60a's "in any order"
          -- says differently from rule 702.85a's "in a random order": the owner
          -- arranges the batch, so exactly one arrangement was asked of alice. A
          -- LibraryPlacement.RandomOrder here would ask none.
          Spec.assertEqWith s "CR 401.4 alice was asked to arrange the four cards she bottomed" (arrangements log_) 1
          Spec.assertEqWith s "two Swamps paid the printed {1}{B} and nothing else was cast" (S.tappedCount S.alice after) 2

-- CR 702.40a's storm: "When you cast this spell, copy it for each other spell
-- that was cast before it this turn."
--
-- Grapeshot {1}{R} Sorcery -- "Grapeshot deals 1 damage to any target. / Storm"
-- (Oracle text checked 2026-09-11) -- is the producer.
--
-- The count is of spells cast BEFORE Grapeshot, by ANY player (Grapeshot's
-- rulings): alice and bob each cast one, then alice casts one more in response to
-- the storm trigger. Two copies is right. A count of alice's spells alone, or a
-- single copy whatever the count, makes one; a this-turn tally read at
-- resolution makes three. Each leaves bob at a different life total.
stormSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stormSpec s registry = Spec.describe s "Storm" $ do
  Spec.it s "CR 702.40a storm is minted for a spell on the stack and nowhere else" $ do
    Spec.assertEqWith s "the stack roster mints it" (Keyword.stackTriggeredAbilitiesOf (Map.singleton Keyword.Type.Storm 1)) [Keyword.storm]
    Spec.assertEqWith s "and the battlefield roster does not" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Storm 1)) []

  -- THE PROVING TEST.
  Spec.it s "CR 702.40a Grapeshot copies itself once per spell cast before it, and not for one cast in response" $ do
    grapeshot <- S.printingOf s registry "Grapeshot"
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let lands = S.landsFor mountain S.bob 1 (S.landsFor mountain S.alice 4 (Setup.emptyGame S.bothPlayers))
        (firstBolt, g1) = S.addHandCard bolt S.alice lands
        (bobBolt, g2) = S.addHandCard bolt S.bob g1
        (responseBolt, g3) = S.addHandCard bolt S.alice g2
        (grapeshotId, g4) = S.addHandCard grapeshot S.alice g3
        board =
          g4
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain
            }
        -- Every target prompt answered with that player: each cast's, and CR
        -- 707.10c's offer to re-target a copy.
        step pid gs action = snd (Engine.runGamePure (pinTarget (Recipient.ToPlayer pid)) gs (action >> Engine.settleForPriority))
        castAndResolve caster pid gs oid = step pid (step pid gs {GameState.priority = Just caster} (S.cast caster oid)) Stack.resolveTop
        -- The two spells cast before Grapeshot: alice's Bolt at bob, bob's at alice.
        before = castAndResolve S.bob S.alice (castAndResolve S.alice S.bob board firstBolt) bobBolt
        -- Grapeshot at bob, its storm trigger on the stack above it, and alice's
        -- second Bolt at bob in response to the trigger.
        responded = step S.bob (step S.bob before {GameState.priority = Just S.alice} (S.cast S.alice grapeshotId)) (S.cast S.alice responseBolt)
        -- The Bolt, the trigger, the copies, then Grapeshot: resolved down to an
        -- empty stack, however many copies there were.
        resolveAll gs = if null (GameState.stack gs) then gs else resolveAll (step S.bob gs Stack.resolveTop)
        after = resolveAll responded
    Spec.assertEqWith s "bob took two Bolts' 6, Grapeshot's 1 and TWO copies' 2" (S.lifeOf S.bob after) (Just 11)
    Spec.assertEqWith s "alice took bob's Bolt" (S.lifeOf S.alice after) (Just 17)

-- CR 702.56a's replicate: "As an additional cost to cast this spell, you may pay
-- [cost] any number of times" and "When you cast this spell, if a replicate cost
-- was paid for it, copy it for each time its replicate cost was paid."
--
-- Pyromatics {1}{R} Instant -- "Replicate {1}{R} / Pyromatics deals 1 damage to
-- any target." (Oracle text checked on Scryfall, 2026-09-11) -- is the producer.
--
-- Six Mountains: the printed {1}{R} and the replicate cost twice. Three damage to
-- bob is the original plus TWO copies; one copy whatever the count leaves him at
-- 18, and no trigger at all at 19.
replicateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
replicateSpec s registry = Spec.describe s "Replicate" $ do
  Spec.it s "CR 702.56a replicate's trigger is minted for a spell on the stack and nowhere else" $ do
    let keyword = Keyword.Type.Replicate (Cost.MkCost Nothing [])
    Spec.assertEqWith s "the stack roster mints one" (length (Keyword.stackTriggeredAbilitiesOf (Map.singleton keyword 1))) 1
    Spec.assertEqWith s "and the battlefield roster mints none" (Keyword.triggeredAbilitiesOf (Map.singleton keyword 1)) []

  -- THE PROVING TEST.
  Spec.it s "CR 702.56a Pyromatics replicated twice deals its damage three times; unreplicated, once" $ do
    pyromatics <- S.printingOf s registry "Pyromatics"
    mountain <- S.printingOf s registry "Mountain"
    let (pyroId, g1) = S.addHandCard pyromatics S.alice (S.landsFor mountain S.alice 6 (Setup.emptyGame S.bothPlayers))
        board =
          g1
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        -- CR 601.2b's announcement answered `times`; CR 707.10c's per-copy offer
        -- and the original's own target both pinned to bob.
        step times gs action = snd (Engine.runGamePure (paidTimesAt times (Recipient.ToPlayer S.bob)) gs (action >> Engine.settleForPriority))
        resolveAll times gs = if null (GameState.stack gs) then gs else resolveAll times (step times gs Stack.resolveTop)
        after :: Natural.Natural -> GameState.GameState
        after times = resolveAll times (step times board (S.cast S.alice pyroId))
    Spec.assertEqWith s "bob took the original's 1 and TWO copies' 2" (S.lifeOf S.bob (after 2)) (Just 17)
    Spec.assertEqWith s "CR 603.4 unreplicated, the original's 1 alone" (S.lifeOf S.bob (after 0)) (Just 19)

-- CR 702.153a's casualty: "As an additional cost to cast this spell, you may
-- sacrifice a creature with power N or greater" and "When you cast this spell, if
-- a casualty cost was paid for it, copy it."
--
-- Light 'Em Up {1}{R} Sorcery -- "Casualty 2 / Light 'Em Up deals 2 damage to
-- target creature or planeswalker." (Oracle text checked on Scryfall,
-- 2026-09-11) -- is the producer.
--
-- Two boards differing in ONE answer: bob's Hill Giant is a 3/3, so the printed
-- 2 leaves it standing and the copy's second 2 kills it. Alice's sacrifice is
-- Jedit Ojanen, a 5/5, so no number in the board coincides with another.
casualtySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
casualtySpec s registry = Spec.describe s "Casualty" $ do
  Spec.it s "CR 702.153a casualty's trigger is minted for a spell on the stack and nowhere else" $ do
    let keyword = Keyword.Type.Casualty 2
    Spec.assertEqWith s "the stack roster mints one" (length (Keyword.stackTriggeredAbilitiesOf (Map.singleton keyword 1))) 1
    Spec.assertEqWith s "and the battlefield roster mints none" (Keyword.triggeredAbilitiesOf (Map.singleton keyword 1)) []

  -- THE PROVING TEST.
  Spec.it s "CR 702.153a Light 'Em Up with its casualty paid deals its damage twice; unpaid, once" $ do
    lightEmUp <- S.printingOf s registry "Light 'Em Up"
    mountain <- S.printingOf s registry "Mountain"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    giant <- S.printingOf s registry "Hill Giant"
    let (_, withJedit) = S.addPermanent jedit S.alice (S.landsFor mountain S.alice 2 (Setup.emptyGame S.bothPlayers))
        (giantId, withGiant) = S.addPermanent giant S.bob withJedit
        (spellId, g1) = S.addHandCard lightEmUp S.alice withGiant
        board =
          g1
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        step times gs action = snd (Engine.runGamePure (paidTimesAt times (Recipient.ToObject giantId)) gs (action >> Engine.settleForPriority))
        resolveAll times gs = if null (GameState.stack gs) then gs else resolveAll times (step times gs Stack.resolveTop)
        after :: Natural.Natural -> GameState.GameState
        after times = resolveAll times (step times board (S.cast S.alice spellId))
        standing gs = (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Hill Giant")) S.bob gs, S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Jedit Ojanen")) S.alice gs)
    Spec.assertEqWith s "CR 704.5g the copy's second 2 killed the 3/3, and Jedit paid for it" (standing (after 1)) (0, 0)
    Spec.assertEqWith s "CR 603.4 unpaid, 2 damage alone leaves the 3/3 standing beside Jedit" (standing (after 0)) (1, 1)

  -- The floor rule 702.153a states, on the same board with alice's creature
  -- swapped for one under it: Dryad Arbor is a 1/1, so there is nothing she may
  -- sacrifice to casualty 2 and CR 601.2f leaves the cost unpayable however she
  -- answers.
  Spec.it s "CR 702.153a a creature under casualty's power floor cannot pay it" $ do
    lightEmUp <- S.printingOf s registry "Light 'Em Up"
    mountain <- S.printingOf s registry "Mountain"
    arbor <- S.printingOf s registry "Dryad Arbor"
    giant <- S.printingOf s registry "Hill Giant"
    let (_, withArbor) = S.addPermanent arbor S.alice (S.landsFor mountain S.alice 2 (Setup.emptyGame S.bothPlayers))
        (giantId, withGiant) = S.addPermanent giant S.bob withArbor
        (spellId, g1) = S.addHandCard lightEmUp S.alice withGiant
        board =
          g1
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        step gs action = snd (Engine.runGamePure (paidTimesAt 1 (Recipient.ToObject giantId)) gs (action >> Engine.settleForPriority))
        resolveAll gs = if null (GameState.stack gs) then gs else resolveAll (step gs Stack.resolveTop)
        after = resolveAll (step board (S.cast S.alice spellId))
    Spec.assertEqWith
      s
      "the 3/3 took 2 and stands, and the 1/1 was never sacrificed"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Hill Giant")) S.bob after, S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Dryad Arbor")) S.alice after)
      (1, 1)

-- CR 702.69a's gravestorm: "When you cast this spell, copy it for each permanent
-- that was put into a graveyard from the battlefield this turn."
--
-- Ominous Harvest {2}{B} Sorcery -- "Gravestorm / Target player draws a card and
-- loses 1 life." (Oracle text checked on Scryfall, 2026-09-13) -- is the
-- producer.
--
-- THE BOARD distinguishes rule 702.69a's count from the two counts it is not.
-- Alice Bolts two of bob's Hill Giants before casting: two PERMANENTS reached a
-- graveyard FROM THE BATTLEFIELD, while the two Bolts reached one from the STACK
-- and rule 702.69a does not count those. Two copies, so bob draws three cards
-- and loses three life; a count of every card put into a graveyard makes it five
-- and no trigger at all makes it one.
gravestormSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gravestormSpec s registry = Spec.describe s "Gravestorm" $ do
  Spec.it s "CR 702.69a gravestorm is minted for a spell on the stack and nowhere else" $ do
    Spec.assertEqWith s "the stack roster mints it" (Keyword.stackTriggeredAbilitiesOf (Map.singleton Keyword.Type.Gravestorm 1)) [Keyword.gravestorm]
    Spec.assertEqWith s "and the battlefield roster does not" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Gravestorm 1)) []

  -- THE PROVING TEST.
  Spec.it s "CR 702.69a Ominous Harvest copies itself once per permanent that died, and not for the Bolts in the graveyard" $ do
    harvest <- S.printingOf s registry "Ominous Harvest"
    bolt <- S.printingOf s registry "Lightning Bolt"
    giant <- S.printingOf s registry "Hill Giant"
    mountain <- S.printingOf s registry "Mountain"
    swamp <- S.printingOf s registry "Swamp"
    let lands = S.landsFor swamp S.alice 3 (S.landsFor mountain S.alice 2 (Setup.emptyGame S.bothPlayers))
        (firstGiant, g1) = S.addPermanent giant S.bob lands
        (secondGiant, g2) = S.addPermanent giant S.bob g1
        (firstBolt, g3) = S.addHandCard bolt S.alice g2
        (secondBolt, g4) = S.addHandCard bolt S.alice g3
        (harvestId, g5) = S.addHandCard harvest S.alice g4
        -- CR 104.3c: bob draws three, so his library must hold more than three.
        stocked = foldr (\_ gs -> snd (S.addLibraryCard giant S.bob gs)) g5 [1 :: Int .. 5]
        board =
          stocked
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain
            }
        step recipient gs action = snd (Engine.runGamePure (pinTarget recipient) gs (action >> Engine.settleForPriority))
        castAndResolve recipient gs oid = step recipient (step recipient gs {GameState.priority = Just S.alice} (S.cast S.alice oid)) Stack.resolveTop
        -- Each Bolt aimed by its own answerer, so the two structurally identical
        -- target prompts cannot be answered the same way.
        killed = castAndResolve (Recipient.ToCreature secondGiant) (castAndResolve (Recipient.ToCreature firstGiant) board firstBolt) secondBolt
        resolveAll gs = if null (GameState.stack gs) then gs else resolveAll (step (Recipient.ToPlayer S.bob) gs Stack.resolveTop)
        after = resolveAll (step (Recipient.ToPlayer S.bob) killed {GameState.priority = Just S.alice} (S.cast S.alice harvestId))
    Spec.assertEqWith s "CR 700.4 bob lost 1 life to the original and 1 to each of TWO copies" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "and drew three cards, the two Giants having left his battlefield" (S.handSize S.bob after) 3

-- CR 702.78a's conspire: "As an additional cost to cast this spell, you may tap
-- two untapped creatures you control that each share a color with it" and "When
-- you cast this spell, if its conspire cost was paid, copy it."
--
-- Burn Trail {3}{R} Sorcery -- "Burn Trail deals 3 damage to any target. /
-- Conspire" (Oracle text checked on Scryfall, 2026-09-13) -- is the producer.
--
-- THE BOARD: alice holds THREE untapped red Hill Giants, so rule 702.78a's
-- choice of two is a real one rather than a set the prompt would elide, and one
-- untapped GREEN Giant Spider, which the cost's colour clause must keep out of
-- the offer. Burn Trail is red, so "share a color with it" is a question the
-- board can answer both ways.
conspireSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
conspireSpec s registry = Spec.describe s "Conspire" $ do
  Spec.it s "CR 702.78a conspire's trigger is minted for a spell on the stack and nowhere else" $ do
    Spec.assertEqWith s "the stack roster mints one" (length (Keyword.stackTriggeredAbilitiesOf (Map.singleton Keyword.Type.Conspire 1))) 1
    Spec.assertEqWith s "and the battlefield roster mints none" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Conspire 1)) []

  -- THE PROVING TEST.
  Spec.it s "CR 702.78a Burn Trail with its conspire paid deals its damage twice; unpaid, once" $ do
    burnTrail <- S.printingOf s registry "Burn Trail"
    giant <- S.printingOf s registry "Hill Giant"
    spider <- S.printingOf s registry "Giant Spider"
    mountain <- S.printingOf s registry "Mountain"
    let lands = S.landsFor mountain S.alice 4 (Setup.emptyGame S.bothPlayers)
        (firstGiant, g1) = S.addPermanent giant S.alice lands
        (secondGiant, g2) = S.addPermanent giant S.alice g1
        (thirdGiant, g3) = S.addPermanent giant S.alice g2
        (spiderId, g4) = S.addPermanent spider S.alice g3
        (spellId, g5) = S.addHandCard burnTrail S.alice g4
        board =
          g5
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        step times gs action = snd (Engine.runGamePure (conspiringWith [firstGiant, secondGiant] times (Recipient.ToPlayer S.bob)) gs (action >> Engine.settleForPriority))
        resolveAll times gs = if null (GameState.stack gs) then gs else resolveAll times (step times gs Stack.resolveTop)
        after :: Natural.Natural -> GameState.GameState
        after times = resolveAll times (step times board (S.cast S.alice spellId))
        tappedOf oid gs = fmap ((== TapState.Tapped) . Object.tapped) (Game.lookupObject oid gs)
    Spec.assertEqWith s "bob took the original's 3 and the copy's 3" (S.lifeOf S.bob (after 1)) (Just 14)
    Spec.assertEqWith s "CR 603.4 unpaid, the original's 3 alone" (S.lifeOf S.bob (after 0)) (Just 17)
    Spec.assertEqWith
      s
      "CR 702.78a the two red creatures alice chose paid, the third red one and the green one did not; unpaid, none of them is tapped"
      (fmap (`tappedOf` after 1) [firstGiant, secondGiant, thirdGiant, spiderId], fmap (`tappedOf` after 0) [firstGiant, secondGiant, thirdGiant, spiderId])
      ([Just True, Just True, Just False, Just False], [Just False, Just False, Just False, Just False])

  -- The colour clause, on the same board with alice's red creatures swapped for
  -- green ones: CR 105.2 leaves nothing sharing Burn Trail's red, so CR 601.2f
  -- leaves the cost unpayable however she answers.
  Spec.it s "CR 702.78a creatures sharing none of the spell's colours cannot pay conspire" $ do
    burnTrail <- S.printingOf s registry "Burn Trail"
    spider <- S.printingOf s registry "Giant Spider"
    mountain <- S.printingOf s registry "Mountain"
    let lands = S.landsFor mountain S.alice 4 (Setup.emptyGame S.bothPlayers)
        (firstSpider, g1) = S.addPermanent spider S.alice lands
        (secondSpider, g2) = S.addPermanent spider S.alice g1
        (thirdSpider, g3) = S.addPermanent spider S.alice g2
        (spellId, g4) = S.addHandCard burnTrail S.alice g3
        board =
          g4
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        step gs action = snd (Engine.runGamePure (conspiringWith [firstSpider, secondSpider] 1 (Recipient.ToPlayer S.bob)) gs (action >> Engine.settleForPriority))
        resolveAll gs = if null (GameState.stack gs) then gs else resolveAll (step gs Stack.resolveTop)
        after = resolveAll (step board (S.cast S.alice spellId))
        tappedOf oid gs = fmap ((== TapState.Tapped) . Object.tapped) (Game.lookupObject oid gs)
    Spec.assertEqWith s "bob took the original's 3 and no copy's" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "and no green creature was tapped" (fmap (`tappedOf` after) [firstSpider, secondSpider, thirdSpider]) [Just False, Just False, Just False]

-- CR 601.2b's conspire announcement answered `times` times, CR 702.78a's tap
-- pinned to the named creatures by FILTERING the offered set, and every target
-- prompt aimed at one recipient.
conspiringWith :: [ObjectId.ObjectId] -> Natural.Natural -> Recipient.Recipient -> Prompt.Prompt r -> r
conspiringWith fodder times recipient p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.MkKickerDecision times
  Prompt.ChooseTaps _ _ _ offered _ -> Set.fromList (filter (`elem` fodder) offered)
  _ -> pinTarget recipient p

-- CR 601.2b's optional additional cost answered `times` times, with every target
-- prompt -- the spell's own and CR 707.10c's per-copy offer -- pinned to one
-- recipient: Pawl.CastSpec's paidTimes crossed with `pinTarget` below.
paidTimesAt :: Natural.Natural -> Recipient.Recipient -> Prompt.Prompt r -> r
paidTimesAt times recipient p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.MkKickerDecision times
  _ -> pinTarget recipient p

-- Answer a ChooseTargets by FILTERING the offered set down to one recipient
-- (Pawl.CopySpec's pinTarget).
pinTarget :: Recipient.Recipient -> Prompt.Prompt r -> r
pinTarget recipient p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter (== recipient) offered) asked
  _ -> S.identityAnswer p

-- alice casts the one spell her hand holds, takes CR 608.2g's offer, and rotates
-- the batch the random-order channel asks about -- a rotation of three being
-- neither the identity nor its own inverse, so an engine that never consulted the
-- channel bottoms the three in a different order. The library reads the answer
-- back REVERSED, Pawl.Engine.Game.insertIntoZone performing the moves from the
-- stated end inward, and a rotation survives that too.
cascading :: Prompt.Prompt r -> r
cascading p = case p of
  Prompt.ChooseAction _ _ actions -> Maybe.fromMaybe A.Pass (List.find isCast actions)
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  Prompt.Shuffle ids -> case ids of
    h : t -> t <> [h]
    [] -> []
  _ -> S.identityAnswer p

-- alice takes rule 702.60a's "may reveal" (Prompt.ChooseOptional, whose default
-- is to decline), takes CR 608.2g's offer of every card it names, aims each
-- Dementia's "target player" at bob, and ROTATES CR 401.4's arrangement rather
-- than answering with the identity, so an arrangement in the transcript is an
-- answer alice gave rather than one the channel would have produced anyway.
--
-- Every Dementia's target prompt is answered the same way on purpose: what this
-- board separates is which CARDS were cast, not who they hit, and pinning the
-- one legal opponent keeps a free cast from being refused for want of a target.
rippling :: Prompt.Prompt r -> r
rippling p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  Prompt.ArrangeLibraryArrivals _ _ _ ids -> case zipWith const [0 ..] ids of
    h : t -> t <> [h]
    [] -> []
  _ -> pinTarget (Recipient.ToPlayer S.bob) p

-- `rippling` with rule 702.60a's one "may" declined, and nothing else changed:
-- the two boards differ in that answer alone.
decliningTheReveal :: Prompt.Prompt r -> r
decliningTheReveal p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  _ -> rippling p

-- CR 401.4's answer in a transcript: what says the owner was ASKED, where the
-- board says only where the cards ended up.
isArrangement :: Response.Response -> Bool
isArrangement response = case response of
  Response.ArrangedLibraryArrivals _ -> True
  _ -> False

-- The cascade answerer above, plus CR 715.3's choice between an adventurer card's
-- two halves answered with the ADVENTURE, pinned by name: a bound that admitted
-- both halves would then cast Welcome Home, so the Berserker entering is the
-- offer having been narrowed rather than the answer.
cascadingIntoTheAdventure :: Prompt.Prompt r -> r
cascadingIntoTheAdventure p = case p of
  Prompt.ChooseOfferedCastSpell _ _ options ->
    Maybe.fromMaybe (NonEmpty.head options) (List.find ((== CardName.MkCardName (Text.pack "Welcome Home")) . snd) (NonEmpty.toList options))
  _ -> cascading p

-- CR 614.12a's as-enters copy choice answered with the one legal source, which on
-- the Clone board is the Devastator. Pinned by NAME rather than searched, so a
-- mutation that drops the printed count cannot be repaired by the answerer
-- finding some other permanent.
copyingTheDevastator :: Prompt.Prompt r -> r
copyingTheDevastator p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> Maybe.listToMaybe legal
  _ -> S.identityAnswer p

isCast :: A.Action -> Bool
isCast action = case action of
  A.Cast {} -> True
  _ -> False

-- Rule 702.30a's "unless you pay" answered yes for one seat --
-- Pawl.CounterKeywordTriggerSpec's helper of the same name, duplicated rather
-- than hoisted. S.identityAnswer declines every CR 118.12 offer, and everything
-- else falls through to it, including the mana window the payment opens.
paysFor :: PlayerId.PlayerId -> Prompt.Prompt r -> r
paysFor who p = case p of
  Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
    | d == who && player == who ->
        PaymentDecision.Pays
  _ -> S.identityAnswer p

-- The pay-or-not answers in a transcript, in order -- duplicated for `paysFor`'s
-- reason. An EMPTY list is what says a choice was never put to the player, which
-- is the assertion rule 702.30a's window needs.
payResponses :: [Response.Response] -> [Response.Response]
payResponses = filter isPayResponse

isPayResponse :: Response.Response -> Bool
isPayResponse response = case response of
  Response.ChoseToPay _ -> True
  _ -> False

-- `paysFor` with CR 614.1c's as-enters copy choice PINNED to one named permanent
-- -- Pawl.CopySpec's copyNamed posture and its reason, so a mutation cannot be
-- repaired by the answerer finding some other legal source. The trigger order is
-- pinned too: two echo triggers at one upkeep are CR 603.3b's choice.
copyingPayingFor :: ObjectId.ObjectId -> PlayerId.PlayerId -> Prompt.Prompt r -> r
copyingPayingFor wanted who p = case p of
  Prompt.ChooseCopyTarget {} -> Just wanted
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  _ -> paysFor who p

-- The battlefield object whose PRINTED card is a Clone -- Pawl.CopySpec's
-- printedOnBattlefield narrowed to the one card this group copies with. Game.faceOf
-- is right HERE and nowhere else in the group: the question is which printing the
-- object is, not what it projects as.
cloneOf :: GameState.GameState -> Maybe ObjectId.ObjectId
cloneOf gs =
  let isIt oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Clone"))
   in List.find isIt (Set.toList (GameState.battlefield gs))

-- CR 702.30 echo, whose whole rule is one upkeep trigger with a CLOCK: rule
-- 702.30a's "if this permanent came under your control since the beginning of
-- your last upkeep" is a window and not a one-shot, so it opens again for a
-- player who takes control later. Pawl.Types.Object.controlClock holds the
-- window and Pawl.Engine.Engine.advanceControlClock turns it, one seat per
-- upkeep.
--
-- Two printings, so no number below reads two ways:
--
--   * Pouncing Jaguar {G} Creature -- Cat 2/2, whose whole printed text is
--     "Echo {G}".
--   * Uktabi Drake {G} Creature -- Drake 2/1, "Flying, haste" and "Echo
--     {1}{G}{G}" -- an echo cost that is NOT its mana cost, which is what tells
--     the keyword's payload apart from a re-read of the card. CR 702.30b's
--     errata is why every echo cost is printed at all.
--
-- Driven through Engine.runStep and never a synthetic StepBegan: the clock turns
-- in Engine.runStepThatBegan, so a fixture that only recorded CR 603.2b's event
-- would leave every permanent at ControlClock.Gained and each leg below would
-- pass for the wrong reason.
echoSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
echoSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- `pid`'s upkeep step, whole: CR 603.2b's event, the turn-based actions,
      -- the trigger gather and the priority round that resolves it. No untap
      -- step between two of them, deliberately -- alice's Forests stay tapped, so
      -- a second payment is visible as a second Forest rather than as the same
      -- one twice.
      atStepOf step pid gs =
        gs
          { GameState.phase = step,
            GameState.activePlayer = pid,
            GameState.priority = Just pid,
            GameState.remaining = S.phasesAfter step
          }
      atUpkeepOf = atStepOf upkeep
      ranUpkeep :: (forall r. Prompt.Prompt r -> r) -> PlayerId.PlayerId -> GameState.GameState -> (((), GameState.GameState), [Response.Response])
      ranUpkeep answer pid gs = Replay.record answer (atUpkeepOf pid gs) Engine.runStep
      afterUpkeep :: (forall r. Prompt.Prompt r -> r) -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
      afterUpkeep answer pid gs = S.runPure answer (atUpkeepOf pid gs) Engine.runStep
      afterStep :: (forall r. Prompt.Prompt r -> r) -> Phase.Phase -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
      afterStep answer step pid gs = S.runPure answer (atStepOf step pid gs) Engine.runStep
      jaguarBoard forests = do
        forest <- S.printingOf s registry "Forest"
        jaguar <- S.printingOf s registry "Pouncing Jaguar"
        pure (S.addPermanent jaguar S.alice (S.landsFor forest S.alice forests (Setup.emptyGame S.bothPlayers)))
   in Spec.describe s "Echo" $ do
        -- The proving test, and rule 702.30a's window in one board: the upkeep
        -- after the Jaguar arrived asks, and the one after that does not.
        Spec.it s "CR 702.30a the first of your upkeeps asks the cost and the next asks nothing" $ do
          (oid, gs) <- jaguarBoard 3
          let ((_, first), firstLog) = ranUpkeep (paysFor S.alice) S.alice gs
              ((_, second), secondLog) = ranUpkeep (paysFor S.alice) S.alice first
          Spec.assertEqWith s "CR 702.30a one Forest paid the echo cost" (S.tappedCount S.alice first) 1
          Spec.assertBool s (S.onBattlefield oid first) "so the Jaguar survived its first upkeep"
          Spec.assertEqWith s "and alice was offered it exactly once" (length (payResponses firstLog)) 1
          -- The whole of rule 702.30a: by the next upkeep, control was gained
          -- longer ago than the last upkeep began, so nothing triggers and no
          -- choice is put to alice.
          Spec.assertEqWith s "CR 702.30a no second Forest went to a second upkeep" (S.tappedCount S.alice second) 1
          Spec.assertEqWith s "because nothing was offered at all" (payResponses secondLog) []
          Spec.assertBool s (S.onBattlefield oid second) "and the Jaguar is still there"
        -- The same board and the same upkeep, differing in NOTHING but the answer
        -- to rule 702.30a's "unless": three untapped Forests can cover {G}, so
        -- this leg separates declining from being unable to pay.
        Spec.it s "CR 702.30a declining the payment sacrifices it with the mana still up" $ do
          (oid, gs) <- jaguarBoard 3
          let after = afterUpkeep S.identityAnswer S.alice gs
          Spec.assertEqWith s "CR 702.30a no Forest was spent" (S.tappedCount S.alice after) 0
          Spec.assertBool s (not (S.onBattlefield oid after)) "and the Jaguar went anyway"
          Spec.assertEqWith s "CR 701.21a into its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
        -- Rule 702.30a says "YOUR upkeep" (CR 603.3a) and measures the window in
        -- YOUR upkeeps. Both halves are here: bob's upkeep neither triggers the
        -- Jaguar nor spends alice's window, which is what an
        -- Engine.advanceControlClock turning every seat's entry would break.
        Spec.it s "CR 603.3a bob's upkeep neither asks nor spends alice's window" $ do
          (oid, gs) <- jaguarBoard 3
          let ((_, bobs), bobLog) = ranUpkeep (paysFor S.alice) S.bob gs
              ((_, alices), aliceLog) = ranUpkeep (paysFor S.alice) S.alice bobs
          Spec.assertEqWith s "CR 603.3a bob's upkeep spent no mana of alice's" (S.tappedCount S.alice bobs) 0
          Spec.assertBool s (S.onBattlefield oid bobs) "and left the Jaguar untouched"
          Spec.assertEqWith s "CR 702.30a alice's own upkeep still asks, so bob's did not turn her clock" (S.tappedCount S.alice alices) 1
          Spec.assertEqWith s "the transcripts agreeing: nothing offered on bob's upkeep, one offer on alice's" (length (payResponses bobLog), length (payResponses aliceLog)) (0, 1)
        -- CR 702.30a's control-change clause, the subtle half: alice's window has
        -- already closed when bob takes the Jaguar, and bob's own next upkeep
        -- opens a fresh one. An implementation that spent the clock once per
        -- OBJECT rather than once per player-and-object asks bob nothing.
        Spec.it s "CR 702.30a a player who takes control owes echo at their own next upkeep" $ do
          (oid, gs0) <- jaguarBoard 3
          forest <- S.printingOf s registry "Forest"
          island <- S.printingOf s registry "Island"
          controlMagic <- S.printingOf s registry "Control Magic"
          -- TWO of alice's upkeeps first, so her own window is shut before bob
          -- takes the Jaguar: with it still open, a reader that asked the OWNER's
          -- entry rather than the CONTROLLER's would answer bob's upkeep right by
          -- accident.
          let paid = afterUpkeep (paysFor S.alice) S.alice gs0
              elapsed = afterUpkeep (paysFor S.alice) S.alice paid
              (held, staged) = S.addHandCard controlMagic S.bob (S.landsFor forest S.bob 4 (S.landsFor island S.bob 4 elapsed))
              stolen = S.runPure (paysFor S.bob) staged (S.cast S.bob held >> Stack.resolveTop)
              ((_, bobs), bobLog) = ranUpkeep (paysFor S.bob) S.bob stolen
          Spec.assertEqWith s "the Jaguar really is bob's now" (Projection.View.controllerOf oid stolen) (Just S.bob)
          Spec.assertEqWith s "CR 702.30a a fifth land of bob's paid echo, on top of Control Magic's four" (S.tappedCount S.bob bobs) 5
          Spec.assertBool s (S.onBattlefield oid bobs) "so the Jaguar survived under its new controller"
          Spec.assertEqWith s "alice, who already paid once, spent nothing more" (S.tappedCount S.alice bobs) 1
          Spec.assertEqWith s "and bob was the one offered it" (length (payResponses bobLog)) 1
        -- CR 702.30a asks whether the permanent came under your control since
        -- your last upkeep, NOT whether this is the first time you ever
        -- controlled it. alice's window has already closed when bob borrows the
        -- Jaguar for a turn, and CR 514.2 ending the Act of Treason gives it back
        -- to her -- which is a fresh coming-under-her-control and opens hers
        -- again. CR 400.7 is not involved: the permanent never left the
        -- battlefield, so this is the same incarnation with the same clock.
        --
        -- The settle after the Act of Treason resolves is a REAL precondition and
        -- not tidiness: Engine.sampleControl sees control move by diffing two
        -- samples, so a theft and a hand-back that both fell between one sample
        -- and the next would be invisible to it. A game gives bob priority there;
        -- this script has to say so.
        Spec.it s "CR 702.30a control coming back re-opens the window it closed" $ do
          (oid, gs0) <- jaguarBoard 4
          mountain <- S.printingOf s registry "Mountain"
          treason <- S.printingOf s registry "Act of Treason"
          let elapsed = afterUpkeep (paysFor S.alice) S.alice (afterUpkeep (paysFor S.alice) S.alice gs0)
              (held, staged) = S.addHandCard treason S.bob (S.landsFor mountain S.bob 3 elapsed)
              stolen = S.runPure (paysFor S.bob) (atStepOf S.precombatMain S.bob staged) (S.cast S.bob held >> Stack.resolveTop >> Engine.settleForPriority)
              reverted = afterStep (paysFor S.bob) (Phase.Ending EndingStep.Cleanup) S.bob stolen
              ((_, back), backLog) = ranUpkeep (paysFor S.alice) S.alice reverted
          Spec.assertEqWith s "alice's two upkeeps spent one Forest and shut her window" (S.tappedCount S.alice elapsed) 1
          Spec.assertEqWith s "the Jaguar really was bob's" (Projection.View.controllerOf oid stolen) (Just S.bob)
          Spec.assertEqWith s "CR 514.2 and alice's again once bob's turn ended" (Projection.View.controllerOf oid reverted) (Just S.alice)
          Spec.assertEqWith s "CR 702.30a a second Forest of alice's paid echo, so her window re-opened" (S.tappedCount S.alice back) 2
          Spec.assertBool s (S.onBattlefield oid back) "and the Jaguar survived that upkeep too"
          Spec.assertEqWith s "she being offered it once" (length (payResponses backLog)) 1
        -- The copy tripwire. A Clone of the Jaguar HAS echo -- CR 707.2 copies the
        -- printed keyword -- and CR 702.30a's clock starts for the copy as it
        -- enters. An implementation reading the PRINTED card (Game.faceOf) rather
        -- than the projection finds "Clone", which has no echo, and asks once.
        Spec.it s "CR 707.2 a Clone of the Jaguar owes its own echo" $ do
          (oid, gs0) <- jaguarBoard 4
          clone <- S.printingOf s registry "Clone"
          let (_, staged) = S.spellOnStack clone S.alice gs0
              copied = S.runPure (copyingPayingFor oid S.alice) staged Stack.resolveTop
              ((_, after), log') = ranUpkeep (copyingPayingFor oid S.alice) S.alice copied
          Spec.assertEqWith s "the Clone entered as a 2/2, so it really copied the Jaguar" (fmap (\c -> Projection.powerOf c after) (cloneOf copied)) (Just (Just 2))
          Spec.assertEqWith s "CR 707.2 two Forests went, one echo per permanent" (S.tappedCount S.alice after) 2
          Spec.assertEqWith s "both of them offered" (length (payResponses log')) 2
          Spec.assertEqWith s "with both still on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 6
        -- The cost comes off the keyword's payload, not off the card: Uktabi
        -- Drake's mana cost is {G} and its echo cost {1}{G}{G}. A pair of boards
        -- differing in exactly one thing -- two Forests or three.
        Spec.it s "CR 702.30a the echo cost is the keyword's and not the mana cost" $ do
          forest <- S.printingOf s registry "Forest"
          drake <- S.printingOf s registry "Uktabi Drake"
          let boardOf n = S.addPermanent drake S.alice (S.landsFor forest S.alice n (Setup.emptyGame S.bothPlayers))
              (twoId, two) = boardOf 2
              (threeId, three) = boardOf 3
              short = afterUpkeep (paysFor S.alice) S.alice two
              enough = afterUpkeep (paysFor S.alice) S.alice three
          -- CR 118.3: two lands cannot cover {1}{G}{G}, so the offer is never
          -- made and rule 702.30a's "unless" runs. One land would cover the
          -- printed {G}.
          Spec.assertBool s (not (S.onBattlefield twoId short)) "CR 702.30a two Forests could not pay {1}{G}{G}, so the Drake was sacrificed"
          Spec.assertEqWith s "and none of them was spent" (S.tappedCount S.alice short) 0
          Spec.assertBool s (S.onBattlefield threeId enough) "three Forests could, so that Drake lived"
          Spec.assertEqWith s "spending all THREE, never the one its mana cost prints" (S.tappedCount S.alice enough) 3

-- CR 702.110 exploit, whose rule is two things at once: rule 702.110a's entry
-- trigger, "you may sacrifice a creature", and rule 702.110b's definition of the
-- phrase every printing's SECOND ability keys on -- "when this creature exploits
-- a creature".
--
-- Qarsi Sadist {1}{B} Creature -- Human Cleric 1/3 is the printing, "Exploit"
-- plus "When this creature exploits a creature, target opponent loses 2 life and
-- you gain 2 life". Every exploit printing pairs the keyword with its own
-- "exploits" trigger -- MTGJSON 2026-08-23, keywords containing Exploit, 25
-- names, no exception -- so the pair is what a gameplay test can reach at all.
--
-- THREE SEATS, so "target opponent" and "you" cannot collapse onto one player.
exploitSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exploitSpec s registry =
  let -- Declines rule 702.110a's "may" and answers nothing else, so the negative
      -- leg differs from the positive one in exactly that answer.
      declining :: Prompt.Prompt r -> r
      declining p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Declines
        _ -> S.identityAnswer p
      -- Takes rule 702.110a's offer, sacrifices the LAST candidate offered -- the
      -- Sadist is on the battlefield too, so a fixture taking the first would
      -- prove nothing about which creature the choice reached -- and aims rule
      -- 702.110b's trigger at bob.
      exploiting :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      exploiting victim p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChoosePermanent _ _ _ offered ->
          Maybe.fromMaybe (NonEmpty.head offered) (List.find (== victim) (NonEmpty.toList offered))
        Prompt.ChooseTargets _ _ _ slots ->
          fmap (\(_, legal) -> Set.filter (== Recipient.ToPlayer S.bob) legal) slots
        _ -> S.identityAnswer p
      -- The Sadist on the stack over a board holding two other creatures of
      -- alice's, distinctly named so the sacrifice is readable by name, and
      -- carol seated so "target opponent" has two candidates.
      sadistBoard = do
        sadist <- S.printingOf s registry "Qarsi Sadist"
        piker <- S.printingOf s registry "Goblin Piker"
        giant <- S.printingOf s registry "Hill Giant"
        let base = Setup.emptyGame S.threePlayers
            (_, withPiker) = S.addPermanent piker S.alice base
            (giantId, withGiant) = S.addPermanent giant S.alice withPiker
            (spell, staged) = S.spellOnStack sadist S.alice withGiant
        pure (spell, giantId, staged)
      -- The Sadist resolves, its entry trigger goes on the stack and resolves,
      -- and whatever rule 702.110b's event then triggers goes on and resolves
      -- too. Engine.settleForPriority between them is what puts each trigger on
      -- the stack (CR 603.3).
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
   in Spec.describe s "Exploit" $ do
        -- The proving test: rule 702.110a's sacrifice happened, and rule
        -- 702.110b's phrase fired the printed trigger off the back of it.
        Spec.it s "CR 702.110b taking the sacrifice fires the printed exploits trigger" $ do
          (_, giantId, staged) <- sadistBoard
          let after = played (exploiting giantId) staged
          Spec.assertEqWith s "CR 702.110b bob lost 2 life to the exploits trigger" (S.lifeOf S.bob after) (Just 18)
          Spec.assertEqWith s "and alice gained 2" (S.lifeOf S.alice after) (Just 22)
          Spec.assertEqWith s "carol, the other opponent, was untouched" (S.lifeOf S.carol after) (Just 20)
          Spec.assertBool s (not (S.onBattlefield giantId after)) "CR 702.110a the Hill Giant alice chose was the creature sacrificed"
          Spec.assertEqWith s "and the Goblin Piker she did not choose stayed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 1
        -- The same board, differing in NOTHING but the answer to rule 702.110a's
        -- "may": no sacrifice means no rule 702.110b event, so the printed
        -- trigger never fires and nobody's life moves.
        Spec.it s "CR 702.110a declining the sacrifice fires nothing" $ do
          (_, giantId, staged) <- sadistBoard
          let after = played declining staged
          Spec.assertEqWith s "CR 702.110b bob lost nothing, the exploits trigger never firing" (S.lifeOf S.bob after) (Just 20)
          Spec.assertEqWith s "and alice gained nothing" (S.lifeOf S.alice after) (Just 20)
          Spec.assertBool s (S.onBattlefield giantId after) "CR 702.110a the Hill Giant stayed on the battlefield"

-- CR 702.101a: "Extort is a triggered ability. 'Extort' means 'Whenever you cast
-- a spell, you may pay {W/B}. If you do, each opponent loses 1 life and you gain
-- life equal to the total life lost this way.'"
--
-- Syndic of Tithes, {1}{W} Creature -- Human Cleric 2/2, whose whole text is
-- extort, so nothing else on the card can move a life total.
--
-- THREE SEATS, which rule 702.101a's "each opponent" needs: at two players "each
-- opponent loses 1" and "you gain 1" are the same number, and the gain could be
-- a literal 1 rather than the total. Three makes the gain 2 where the loss is 1.
--
-- Luminesce, {W} Instant, is the spell cast: it targets nothing, prevents damage
-- from colours nobody here is dealing, and above all touches no life total, so
-- every life figure below is extort's.
extortSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
extortSpec s registry =
  let board = do
        plains <- S.printingOf s registry "Plains"
        syndic <- S.printingOf s registry "Syndic of Tithes"
        luminesce <- S.printingOf s registry "Luminesce"
        -- Three Plains: one for Luminesce and one for rule 702.101a's {W/B},
        -- with a third spare so declining is never "could not pay".
        let withLands = S.landsFor plains S.alice 3 S.threePlayerGame
            (_, withSyndic) = S.addPermanent syndic S.alice withLands
            (spellId, staged) = S.addHandCard luminesce S.alice withSyndic
        pure
          ( spellId,
            staged
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice,
                GameState.remaining = S.phasesAfter Phase.PrecombatMain
              }
          )
      castAndResolve :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndResolve answer oid gs = S.runPure answer (S.runPure answer gs (S.cast S.alice oid)) Engine.priorityLoop
   in Spec.describe s "Extort" $ do
        -- THE case: paying drains BOTH opponents a life each and gains alice the
        -- two they lost between them, which is what "the total life lost this
        -- way" says and what a literal 1 could not produce.
        Spec.it s "CR 702.101a whole card: paying {W/B} drains each opponent and gains that much" $ do
          (spellId, gs) <- board
          let after = castAndResolve (paysFor S.alice) spellId gs
          Spec.assertEqWith s "CR 702.101a alice gained the total the two of them lost" (S.lifeOf S.alice after) (Just 22)
          Spec.assertEqWith s "bob lost 1" (S.lifeOf S.bob after) (Just 19)
          Spec.assertEqWith s "and carol lost 1" (S.lifeOf S.carol after) (Just 19)
        -- The same board differing in NOTHING but the answer to rule 702.101a's
        -- "may", with the mana still up, so this separates declining from being
        -- unable to pay.
        Spec.it s "CR 702.101a declining the payment moves no life" $ do
          (spellId, gs) <- board
          let after = castAndResolve S.identityAnswer spellId gs
          Spec.assertEqWith s "nobody's life moved" (fmap (\pid -> S.lifeOf pid after) [S.alice, S.bob, S.carol]) [Just 20, Just 20, Just 20]
          Spec.assertEqWith s "though Luminesce really resolved" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertEqWith s "and no Plains was spent on the offer" (S.tappedCount S.alice after) 1
        -- CR 702.101b: "If a permanent has multiple instances of extort, each
        -- triggers separately." Asked of the mint, as prowess' 702.108b is: no
        -- card here prints extort twice and nothing grants it.
        Spec.it s "CR 702.101b each instance of extort is its own ability" $ do
          Spec.assertEqWith s "extort held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Extort 2)) [Keyword.extort, Keyword.extort]
          Spec.assertEqWith s "and held once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Extort 1)) [Keyword.extort]

-- CR 702.191a: "Increment is a triggered ability. 'Increment' means 'Whenever you
-- cast a spell, if this permanent is a creature and the amount of mana spent to
-- cast that spell is greater than this creature's power or this creature's
-- toughness, put a +1/+1 counter on this creature.'"
--
-- Hungry Graffalon, {3}{G} Creature -- Giraffe 3/4 with reach and increment. The
-- 3/4 body is what makes the two spells below discriminating: FOUR mana clears
-- the power and THREE clears neither, so a comparison that read ">=" rather than
-- rule 702.191a's ">" fires on both.
--
-- Boil, {3}{R} Instant, is the four-mana spell and Trumpet Blast, {2}{R}, the
-- three-mana one. Neither moves anything an assertion here reads: nobody here
-- controls an Island and nobody is attacking.
incrementSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
incrementSpec s registry =
  let board spell = do
        mountain <- S.printingOf s registry "Mountain"
        graffalon <- S.printingOf s registry "Hungry Graffalon"
        printing <- S.printingOf s registry spell
        let withLands = S.landsFor mountain S.alice 4 (Setup.emptyGame S.bothPlayers)
            (giraffeId, withGiraffe) = S.addPermanent graffalon S.alice withLands
            (spellId, staged) = S.addHandCard printing S.alice withGiraffe
        pure
          ( giraffeId,
            spellId,
            staged
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice,
                GameState.remaining = S.phasesAfter Phase.PrecombatMain
              }
          )
      castAndResolve oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast S.alice oid)) Engine.priorityLoop
      sizeOf oid gs = (Projection.powerOf oid gs, Projection.toughnessOf oid gs)
   in Spec.describe s "Increment" $ do
        -- THE case: four mana beats the 3 power, so the counter goes on and the
        -- projection reads 4/5.
        Spec.it s "CR 702.191a whole card: a four-mana spell grows Hungry Graffalon" $ do
          (giraffeId, spellId, gs) <- board "Boil"
          let after = castAndResolve spellId gs
          Spec.assertEqWith s "CR 702.191a the +1/+1 counter made it 4/5" (sizeOf giraffeId after) (Just 4, Just 5)
          Spec.assertEqWith s "3/4 before the cast" (sizeOf giraffeId gs) (Just 3, Just 4)
        -- The strict inequality on its own: three mana equals neither 3 nor 4 and
        -- exceeds neither, so nothing happens. Without this leg an AtLeast over
        -- the bare power and rule 702.191a's "greater than" are the same test.
        Spec.it s "CR 702.191a three mana is not GREATER than the 3 power" $ do
          (giraffeId, spellId, gs) <- board "Trumpet Blast"
          let after = castAndResolve spellId gs
          Spec.assertEqWith s "CR 702.191a Hungry Graffalon is still 3/4" (sizeOf giraffeId after) (Just 3, Just 4)
          Spec.assertEqWith s "though Trumpet Blast really resolved" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
        -- CR 702.191b: "If a creature has multiple instances of increment, each
        -- one triggers separately." Asked of the mint, as rule 702.101b's is.
        Spec.it s "CR 702.191b each instance of increment is its own ability" $ do
          Spec.assertEqWith s "increment held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Increment 2)) [Keyword.increment, Keyword.increment]
          Spec.assertEqWith s "and held once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Increment 1)) [Keyword.increment]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  cascadeSpec s registry
  rippleSpec s registry
  stormSpec s registry
  replicateSpec s registry
  casualtySpec s registry
  gravestormSpec s registry
  conspireSpec s registry
  echoSpec s registry
  exploitSpec s registry
  poisonousSpec s registry
  ingestSpec s registry
  annihilatorSpec s registry
  battleCrySpec s registry
  prowessSpec s registry
  extortSpec s registry
  incrementSpec s registry
  selfBlocksSpec s registry
  selfBlocksAtLeastSpec s registry
  selfBlocksOneOrMoreSpec s registry
  creatureBecomesBlockedByAtLeastSpec s registry
  selfBlocksCreatureSpec s registry
  selfBecomesBlockedSpec s registry
  selfAttacksUnblockedSpec s registry
  partnerWithSpec s registry

-- CR 702.124j's SECOND ability: "When this permanent enters, target player may
-- search their library for a card named [name], reveal it, put it into their
-- hand, then shuffle." Silvar, Devourer of the Free names Trynn, Champion of
-- Freedom, and is CAST rather than placed, so the entry is the game's own.
--
-- THREE seats, because two collapse "target player" onto "the one opponent" and
-- an engine that searched the CONTROLLER's library would be caught only by luck.
-- alice casts, carol is targeted, and bob is the seat neither role names.
--
-- A Trynn in EVERY library, because one library holding the only copy cannot
-- tell "read carol's library" from "read every library". The Goblin Piker in
-- carol's library gives Filter.HasName a card to reject; it is added second, so
-- Support.addLibraryCard makes it the head and a filter that admitted everything
-- would fetch it instead.
partnerWithSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
partnerWithSpec s registry =
  let board swamp mountain piker silvar trynn =
        let lands =
              List.foldl'
                (\g p -> snd (S.addPermanent p S.alice g))
                S.threePlayerGame
                [swamp, swamp, swamp, mountain, mountain]
            (aliceCard, g1) = S.addLibraryCard trynn S.alice lands
            (bobCard, g2) = S.addLibraryCard trynn S.bob g1
            (_, g3) = S.addLibraryCard trynn S.carol g2
            (carolPiker, g4) = S.addLibraryCard piker S.carol g3
            (gs, spellId) = S.handOne silvar g4
         in (gs, spellId, aliceCard, bobCard, carolPiker)
      settle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
      settle answer gs spellId =
        let cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
         in snd (Engine.runGamePure answer cast Engine.priorityLoop)
      handNamesOf pid gs = fmap (`S.soleFaceName` gs) (Game.zoneMembers Zone.Hand pid gs)
   in Spec.describe s "Partner with" $ do
        Spec.it s "CR 702.124j the entry trigger searches the TARGET player's library" $ do
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          silvar <- S.printingOf s registry "Silvar, Devourer of the Free"
          trynn <- S.printingOf s registry "Trynn, Champion of Freedom"
          let (gs, spellId, aliceCard, bobCard, carolPiker) = board swamp mountain piker silvar trynn
              settled = settle atCarolSearching gs spellId
              trynnName = CardName.MkCardName (Text.pack "Trynn, Champion of Freedom")
          Spec.assertEqWith s "CR 702.124j: the named card is in the TARGETED player's hand" (handNamesOf S.carol settled) [trynnName]
          Spec.assertEqWith s "CR 702.124j: and carol's library kept only the card the name rejected" (Game.zoneMembers Zone.Library S.carol settled) [carolPiker]
          Spec.assertEqWith s "the caster's own library is untouched" (Game.zoneMembers Zone.Library S.alice settled) [aliceCard]
          Spec.assertEqWith s "and so is the third seat's" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
          Spec.assertEqWith s "CR 702.124j: the find was revealed, which is the rule's own word" (fmap fst (S.revealsOf settled)) [S.carol]
        -- The negative, on the same board with ONE thing changed: the answer.
        -- Support.identityAnswer declines every optional, so rule 702.124j's
        -- "may" leaves the named card in the library.
        Spec.it s "CR 702.124j the targeted player may decline" $ do
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          silvar <- S.printingOf s registry "Silvar, Devourer of the Free"
          trynn <- S.printingOf s registry "Trynn, Champion of Freedom"
          let (gs, spellId, _, _, _) = board swamp mountain piker silvar trynn
              settled = settle atCarolDeclining gs spellId
          Spec.assertEqWith s "CR 702.124j: carol's hand is empty, the search never happened" (handNamesOf S.carol settled) []
          Spec.assertEqWith s "and her library still holds both cards" (List.length (Game.zoneMembers Zone.Library S.carol settled)) 2

-- Silvar's answerer: aim the trigger at carol wherever she is offered (a
-- preference rather than a filter, so a slot she is no candidate for still gets
-- a legal answer), then exercise rule 702.124j's "may" and take the find ONLY as
-- carol.
--
-- Both pins are on carol rather than on "whoever is asked": an engine that put
-- the option or the search to the spell's controller finds nothing at all,
-- rather than being helpfully answered with a legal choice.
atCarolSearching :: Prompt.Prompt r -> r
atCarolSearching p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap
      (\(n, legal) -> Set.fromList (take (Natural.Extra.toIntSaturating n) (ListUtils.nubOrd (filter (== Recipient.ToPlayer S.carol) (Set.toAscList legal) <> Set.toAscList legal))))
      sets
  Prompt.ChooseOptional _ pid _ _ _ -> if pid == S.carol then OptionalDecision.Exercises else OptionalDecision.Declines
  Prompt.Search _ pid matches cap -> if pid == S.carol then List.genericTake cap matches else []
  _ -> S.identityAnswer p

-- atCarolSearching with rule 702.124j's "may" declined, and nothing else
-- changed: the target is still aimed at carol, so the two boards differ in
-- exactly that answer.
atCarolDeclining :: Prompt.Prompt r -> r
atCarolDeclining p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  _ -> atCarolSearching p
