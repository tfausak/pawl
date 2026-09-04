{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Keyword's triggered abilities -- the CR 702 keywords whose rule
-- text IS a trigger -- gathered by the same Pawl.Engine.Event scan a printed
-- trigger goes through, plus the CR 508/509 combat declarations several of them
-- ride on. The machinery is Pawl.TriggerSpec.
module Pawl.KeywordTriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event.Binding as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
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
        foldl
          (\g _ -> let (aura, g1) = S.addCreature printing S.alice g in S.attach aura host g1)
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
          let ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 7) (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat)
              bindings = Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev
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
              let withSwamps = foldl (\g _ -> snd (S.addCreature swamp S.alice g)) gs0 (replicate 4 ())
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
          let bindings = Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime) (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared (ObjectId.MkObjectId 7) S.carol 1))
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
-- moves nothing an assertion reads. Goblin Piker, {2}{R}, is the creature spell.
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
        let addLands pid n g = List.foldl' (\g' _ -> snd (S.addCreature mountain pid g')) g [1 .. (n :: Int)]
            withLands = addLands S.bob 4 (addLands S.alice 4 S.threePlayerGame)
            (spearId, withSpear) = S.addCreature swiftspear S.alice withLands
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
        let lands = List.foldl' (\g _ -> snd (S.addCreature forest S.bob g)) gs0 (replicate (3 * copies) ())
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
        pure (snd (S.addCreature season S.bob gs0), ours, yours)
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
          [ ()
          | GameEvent.AbilityTriggered record <- S.eventsOf gs,
            AbilityTriggered.source record == TriggerSource.OfObject oid,
            case TriggeredAbility.condition (AbilityTriggered.ability record) of
              TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> True
              _ -> False
          ]
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
        -- Jace Beleren is stocked with loyalty by hand: S.addCreature puts a
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
              let lands = List.foldl' (\g _ -> snd (S.addCreature forest S.bob g)) gs0 (replicate 3 ())
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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  poisonousSpec s registry
  ingestSpec s registry
  annihilatorSpec s registry
  battleCrySpec s registry
  prowessSpec s registry
  selfBlocksSpec s registry
  selfBlocksAtLeastSpec s registry
  selfBlocksOneOrMoreSpec s registry
  creatureBecomesBlockedByAtLeastSpec s registry
  selfBlocksCreatureSpec s registry
  selfBecomesBlockedSpec s registry
  selfAttacksUnblockedSpec s registry
