-- Pawl.Engine.Keyword's triggered abilities -- the CR 702 keywords whose rule
-- text IS a trigger -- gathered by the same Pawl.Engine.Event scan a printed
-- trigger goes through, plus the CR 508/509 combat declarations several of them
-- ride on. The machinery is Pawl.TriggerSpec.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.KeywordTriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.Zone as Zone

-- CR 702.70: poisonous -- the first keyword whose rule text IS a triggered
-- ability, so it is minted by Pawl.Engine.Keyword and gathered by the same
-- Pawl.Engine.Event.eventTriggers scan a printed trigger goes through, with the damaged
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
              bindings = Event.eventBindings TriggerCondition.SelfDealsCombatDamageToPlayer ev
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
-- Pawl.Engine.Keyword and gathered by the same Pawl.Engine.Event.eventTriggers
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
          let bindings = Event.eventBindings (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime) (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared (ObjectId.MkObjectId 7) S.carol 1))
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
-- and gathered by the same Pawl.Engine.Event.eventTriggers scan.
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
        -- falsifier is a match on the PAIRWISE GameEvent.BlockerDeclared, which
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
        -- The falsifier is a match on the pairwise GameEvent.BlockerDeclared,
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
        -- GameEvent.BlockerDeclared are recorded and exactly one
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
        -- GameEvent.BlockerDeclared are recorded and exactly one
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
        -- falsifier is an arm that matched GameEvent.BlockerDeclared, which would
        -- put bob at 21.
        Spec.it s "CR 509.3c blocking is not becoming blocked, so a blocking Sacred Prey gains nothing" $ do
          (gs, _, _) <- board ["Goblin Piker"] ["Sacred Prey"]
          Spec.assertEqWith s "bob gained nothing" (S.lifeOf S.bob (S.runCombat S.aggressiveAnswer gs)) (Just 20)

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

-- CR 702.68a's frenzy, which rule 702 states as a triggered ability: "'Frenzy N'
-- means 'Whenever this creature attacks and isn't blocked, it gets +N/+0 until
-- end of turn.'" selfAttacksUnblockedSpec above proves CR 509.1h's event; this
-- group proves the keyword Pawl.Engine.Keyword.frenzy mints on top of it, and
-- with it rule 702's first +N/+0 -- bushido's +N/+N with the toughness term at
-- zero, which is why every reading below is a PAIR.
--
-- Frenzy Sliver {1}{B} Creature -- Sliver 1/1 is the card, and it prints frenzy
-- as a GRANT -- "all Sliver creatures have frenzy 1" -- so the ability is minted
-- off the projection's POST-LAYER keyword count rather than off a printed
-- keyword. Venser's Sliver {5} Artifact Creature -- Sliver 3/3, a vanilla, is
-- the Sliver it lands on, and that split is what keeps the numbers apart: the
-- bonus is 1 and the attacker's power is 3, so a payload reading its source's
-- own power would say 6 where the rule says 4, and one reading the granting
-- permanent's would say 4/2.
--
-- THREE SEATS. Rule 702.68a names no defending player, so what the third seat
-- buys here is narrower than afflictSpec's: at two players "the player attacked"
-- and "the attacker's opponent" collapse, and a bonus wrongly scoped to the seat
-- count rather than to the attack would read the same either way.
--
-- Giant Spider 2/4 is the blocker, so the BLOCKED leg carries an observable of
-- its own rather than only an absence: 3 damage leaves it alive where the 4 a
-- wrongly fired frenzy would deal kills it.
--
-- Readings of power and toughness are taken at the COMBAT DAMAGE step, after the
-- trigger has resolved in the declare blockers step and before damage is dealt.
frenzySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
frenzySpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.threePlayerCombat ours yours [])
      -- CR 508.1's declaration narrowed to the named creatures, against `who`.
      -- S.aggressiveAnswer attacks with everything and would take whichever
      -- defender sorts first, so a case that is about WHICH creature and WHICH
      -- seat has to say both.
      plan :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan who attackers p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      -- The same, with CR 509.1's declaration switched off. The blocked and
      -- unblocked legs below differ in this and nothing else.
      declining :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      declining who attackers p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> plan who attackers p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
   in Spec.describe s "Frenzy" $ do
        -- The proving test and its control, on ONE board differing only in the
        -- answer to Prompt.DeclareBlockers. The Frenzy Sliver stays home in both,
        -- so what is told apart is the bonus and not the presence of the grant.
        Spec.it s "CR 702.68a whole card: an unblocked Venser's Sliver is 4/3, a blocked one is 3/3" $ do
          (gs, mine, theirs, _) <- board ["Frenzy Sliver", "Venser's Sliver"] ["Giant Spider"]
          case (mine, theirs) of
            ([sliver, venser], [spider]) -> do
              Spec.assertEqWith
                s
                "unblocked: +1/+0 on the attacker, and none on the Frenzy Sliver at home"
                (S.powerToughnessOf venser (atDamage (declining S.bob [venser]) gs), S.powerToughnessOf sliver (atDamage (declining S.bob [venser]) gs))
                (Just (4, 3), Just (1, 1))
              Spec.assertEqWith
                s
                "blocked: the attacker is its printed 3/3"
                (S.powerToughnessOf venser (atDamage (plan S.bob [venser]) gs))
                (Just (3, 3))
              Spec.assertEqWith
                s
                "unblocked: bob took 4"
                (S.lifeOf S.bob (S.runCombat (declining S.bob [venser]) gs))
                (Just 16)
              let blocked = S.runCombat (plan S.bob [venser]) gs
              Spec.assertEqWith
                s
                "blocked: bob took nothing and the 2/4 Spider survived 3"
                (S.lifeOf S.bob blocked, S.onBattlefield spider blocked)
                (Just 20, True)
            _ -> Spec.assertFailure s "fixture should give alice two Slivers and bob a Spider"
        -- CR 509.1h's last sentence: "a creature remains blocked even if all the
        -- creatures blocking it are removed from combat." The 3/3 Sliver kills the
        -- 2/1 Piker at CR 510.2 and still never gets the bonus.
        Spec.it s "CR 509.1h losing every blocker does not earn the bonus" $ do
          (gs, mine, theirs, _) <- board ["Frenzy Sliver", "Venser's Sliver"] ["Goblin Piker"]
          case (mine, theirs) of
            ([_, venser], [piker]) -> do
              let after = S.runCombat (plan S.bob [venser]) gs
              Spec.assertBool s (not (S.onBattlefield piker after)) "the 2/1 Piker died to the Sliver's 3"
              Spec.assertEqWith
                s
                "the Sliver is still its printed 3/3 and bob took nothing"
                (S.powerToughnessOf venser after, S.lifeOf S.bob after)
                (Just (3, 3), Just 20)
            _ -> Spec.assertFailure s "fixture should give alice two Slivers and bob a Piker"
        -- Rule 509.1h carries no condition about anyone being ABLE to block, so
        -- the board where nobody can is the bonus' board too.
        Spec.it s "CR 509.1h an attacker nobody could block gets the bonus" $ do
          (gs, mine, _, _) <- board ["Frenzy Sliver", "Venser's Sliver"] []
          case mine of
            [_, venser] -> do
              Spec.assertEqWith s "4/3 at the damage step" (S.powerToughnessOf venser (atDamage (plan S.bob [venser]) gs)) (Just (4, 3))
              Spec.assertEqWith s "and bob took 4" (S.lifeOf S.bob (S.runCombat (plan S.bob [venser]) gs)) (Just 16)
            _ -> Spec.assertFailure s "fixture should give alice two Slivers"
        -- "ALL SLIVER CREATURES", read as the card's own filter: the bearer is in
        -- it and a Hill Giant is not. Both attack unblocked on one board, so the
        -- two halves cannot hide behind each other -- a grant that missed the
        -- bearer and a grant that reached everything both leave bob at 14, and
        -- only the pair of sizes tells them apart.
        Spec.it s "CR 702.68a the grant reaches the bearer and stops at the Slivers" $ do
          (gs, mine, _, _) <- board ["Frenzy Sliver", "Hill Giant"] []
          case mine of
            [sliver, giant] -> do
              Spec.assertEqWith
                s
                "the 1/1 Sliver is 2/1 and the 3/3 Giant is untouched"
                (S.powerToughnessOf sliver (atDamage (plan S.bob [sliver, giant]) gs), S.powerToughnessOf giant (atDamage (plan S.bob [sliver, giant]) gs))
                (Just (2, 1), Just (3, 3))
              Spec.assertEqWith s "bob took 2 and 3" (S.lifeOf S.bob (S.runCombat (plan S.bob [sliver, giant]) gs)) (Just 15)
            _ -> Spec.assertFailure s "fixture should give alice a Frenzy Sliver and a Hill Giant"
        -- The third seat, which CR 508.1b's announcement is what changes: bob
        -- keeps his Spider and cannot block for carol, so the bonus follows the
        -- attack rather than the seat. Rule 702.68a names no defending player,
        -- and that is the point of asserting both life totals.
        Spec.it s "CR 509.1h the bonus follows the attack rather than the seat count" $ do
          (gs, mine, _, _) <- board ["Frenzy Sliver", "Venser's Sliver"] ["Giant Spider"]
          case mine of
            [_, venser] -> do
              let after = S.runCombat (plan S.carol [venser]) gs
              Spec.assertEqWith s "4/3 all the same" (S.powerToughnessOf venser (atDamage (plan S.carol [venser]) gs)) (Just (4, 3))
              Spec.assertEqWith s "carol took the 4 and bob took nothing" (S.lifeOf S.bob after, S.lifeOf S.carol after) (Just 20, Just 16)
            _ -> Spec.assertFailure s "fixture should give alice two Slivers"
        -- CR 702.68b: "if a creature has multiple instances of frenzy, each
        -- triggers separately" -- poisonous' multiplicity, asserted at the MINT
        -- because no board in this pool grants a second instance.
        Spec.it s "CR 702.68b each instance is its own ability" $ do
          Spec.assertEqWith s "frenzy 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Frenzy 1) 2)) [Keyword.frenzy 1, Keyword.frenzy 1]
          Spec.assertEqWith s "and frenzy 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Frenzy 3) 1)) [Keyword.frenzy 3]

-- CR 702.83a's exalted, which rule 702 states as a triggered
-- ability, and with it CR 506.5 -- "attacks alone", the one attack-trigger form
-- that is about the DECLARATION's size rather than about one creature.
--
-- Aven Squire {1}{W} Creature -- Bird Soldier 1/1 is the card: flying and
-- exalted, and flying decides nothing here, since every reading is taken before
-- damage whoever blocked. Hill Giant 3/3 is the creature it pumps, chosen so no
-- reading lands on the same pair -- 3/3 -> 4/4 is not 1/1 -> 2/2, and neither is
-- +2/+2's 5/5.
--
-- Every reading is taken at the COMBAT DAMAGE step, before damage is dealt, so
-- the pump is read directly rather than through what survives combat.
exaltedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exaltedSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to one named creature. The whole point of
      -- the group: S.aggressiveAnswer attacks with EVERYTHING, which is the
      -- not-alone board rather than the alone one.
      only :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      only oid p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== oid) ids
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
   in Spec.describe s "Exalted" $ do
        -- The proving test, and the one that pins WHICH object the payload moves.
        -- alice's Aven Squire stays home while her Hill Giant attacks alone: the
        -- GIANT is 4/4 and the Squire is untouched. A payload written as prowess'
        -- Filter.IsSource would move the Squire's 1/1 and leave the Giant at 3/3,
        -- and a self-scoped condition would not fire at all -- one assertion over
        -- both, so neither can hide behind the other.
        Spec.it s "CR 702.83a whole card: the Squire stays home and the lone attacker is 4/4" $ do
          (gs, mine, _) <- board ["Aven Squire", "Hill Giant"] []
          case mine of
            [squire, giant] ->
              Spec.assertEqWith
                s
                "the attacking Giant took the +1/+1 and the Squire took none"
                (S.powerToughnessOf giant (atDamage (only giant) gs), S.powerToughnessOf squire (atDamage (only giant) gs))
                (Just (4, 4), Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Squire and a Giant"
        -- CR 506.5's "the ONLY creature declared as an attacker", which is the
        -- count on GameEvent.AttackerDeclared. The SAME board as above, differing
        -- only in the declaration: with the Squire attacking too, nobody is alone
        -- and neither creature is pumped.
        Spec.it s "CR 506.5 two attackers is nobody attacking alone" $ do
          (gs, mine, _) <- board ["Aven Squire", "Hill Giant"] []
          case mine of
            [squire, giant] ->
              Spec.assertEqWith
                s
                "both are at their printed sizes"
                (S.powerToughnessOf giant (atDamage S.aggressiveAnswer gs), S.powerToughnessOf squire (atDamage S.aggressiveAnswer gs))
                (Just (3, 3), Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Squire and a Giant"
        -- "A creature you control" reaches the bearer as readily as anything else:
        -- rule 702.83a excludes nothing, so an Aven Squire attacking by itself
        -- pumps itself to 2/2.
        Spec.it s "CR 702.83a the bearer attacking alone pumps itself" $ do
          (gs, mine, _) <- board ["Aven Squire"] []
          case mine of
            [squire] -> Spec.assertEqWith s "1/1 became 2/2" (S.powerToughnessOf squire (atDamage S.aggressiveAnswer gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Squire"
        -- "YOU control", read against CR 109.5's "you" -- the ability's controller
        -- (CR 603.3a). bob's Aven Squire watches alice's Giant attack alone and
        -- stays silent. Same declaration as the proving test, same Giant, and the
        -- only difference is which seat holds the Squire.
        Spec.it s "CR 702.83a an opponent's Squire does not pump the attacker" $ do
          (gs, mine, _) <- board ["Hill Giant"] ["Aven Squire"]
          case mine of
            [giant] -> Spec.assertEqWith s "the Giant is its printed 3/3" (S.powerToughnessOf giant (atDamage (only giant) gs)) (Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Giant"
        -- Two exalted permanents are two abilities and two +1/+1s, which is CR
        -- 603.2 rather than a clause of rule 702.83: unlike CR 702.28c's shadow,
        -- rule 702.83 prints no "multiple instances are redundant" sentence.
        Spec.it s "CR 603.2 two Squires make the lone attacker 5/5" $ do
          (gs, mine, _) <- board ["Aven Squire", "Aven Squire", "Hill Giant"] []
          case mine of
            [_, _, giant] -> Spec.assertEqWith s "3/3 took both pumps" (S.powerToughnessOf giant (atDamage (only giant) gs)) (Just (5, 5))
            _ -> Spec.assertFailure s "fixture should give alice two Squires and a Giant"
        -- CR 508.1a's declaration is a SET, so a broken interpreter naming one
        -- creature twice has still declared one attacker. Combat.declareAttackers
        -- deduplicates before it counts; without that the count would be 2, so
        -- the Giant would not be attacking alone and would go unpumped.
        Spec.it s "CR 508.1a a repeated id is still one attacker" $ do
          (gs, mine, _) <- board ["Aven Squire", "Hill Giant"] []
          case mine of
            [_, giant] -> do
              let twice :: Prompt.Prompt r -> r
                  twice p = case p of
                    Prompt.DeclareAttackers _ _ ids -> concatMap (\i -> if i == giant then [i, i] else []) ids
                    _ -> S.aggressiveAnswer p
              Spec.assertEqWith s "the Giant is 4/4 all the same" (S.powerToughnessOf giant (atDamage twice gs)) (Just (4, 4))
            _ -> Spec.assertFailure s "fixture should give alice a Squire and a Giant"
        -- The same multiplicity one permanent over, asserted of the MINT because
        -- no printing in the pool carries exalted twice -- as flanking's, bushido's
        -- and prowess' instance cases are.
        Spec.it s "CR 603.2 two instances mint two abilities, both CR 506.5" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Exalted 2
              expected =
                TriggerCondition.CreatureAttacksAlone
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith s "each watching CR 506.5, filtered on the attacker's controller" (fmap TriggeredAbility.condition abilities) [expected, expected]

-- CR 702.134a's mentor, which rule 702 states as a triggered ability
-- and the FIRST whose ability TARGETS -- so this is the group that runs a
-- keyword-minted TargetSlot through CR 601.2c's choosing, and with it
-- Filter.PowerLessThanSource, the one atom whose bound is the source's own power
-- rather than a literal.
--
-- Blade Instructor {2}{W} Creature -- Human Soldier 3/1 is the card: mentor and
-- nothing else, so every number below is the keyword's. Its fellow attackers are
-- the pool's vanillas, picked so no two readings land on the same pair -- a
-- mentored Goblin Piker is 3/2, which is neither its printed 2/1 nor the
-- Instructor's 3/1, and a mentored Icehide Golem is 3/3, which is neither.
--
-- Every reading is taken at the COMBAT DAMAGE step, after the trigger has
-- resolved in the declare attackers step and before damage is dealt, so the
-- counter is read directly rather than through what survives combat.
mentorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mentorSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures, and CR 603.3d's
      -- target named outright. S.aggressiveAnswer attacks with everything and
      -- Replay.defaultAnswer would take whichever target sorts first, so a case
      -- that is about WHICH creature has to say both itself.
      plan :: [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      plan attackers target p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature target))) sets
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      -- CR 122.1: what is actually on the permanent, which a +1/+1 EFFECT would
      -- leave empty while reading the same 3/2.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "Mentor" $ do
        -- The proving test. Both attack, the Instructor mentors the smaller
        -- attacker, and the assertion covers all three things at once: the
        -- counter lands on the TARGET, the bearer takes none, and what landed is
        -- a CR 122.1a counter rather than a pump.
        Spec.it s "CR 702.134a whole card: the mentored attacker takes a +1/+1 counter" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker"] []
          case mine of
            [instructor, piker] -> do
              let after = atDamage (plan [instructor, piker] piker) gs
              Spec.assertEqWith
                s
                "the Piker is 3/2 and the Instructor is untouched"
                (S.powerToughnessOf piker after, S.powerToughnessOf instructor after)
                (Just (3, 2), Just (3, 1))
              Spec.assertEqWith s "and it is a counter" (countersOn piker after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice an Instructor and a Piker"
        -- "POWER LESS THAN this creature's power" is strict, so a 3/3 attacking
        -- beside a 3-power Instructor is no legal target -- and neither is the
        -- Instructor itself, which is why nothing at all is mentored here. Same
        -- declaration as the proving test; only the fellow attacker's power
        -- differs. S.aggressiveAnswer rather than `plan`, so that a filter that
        -- admitted the Giant would take the default target and go red.
        Spec.it s "CR 702.134a a creature whose power is not less is no legal target" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Hill Giant"] []
          case mine of
            [instructor, giant] -> do
              let after = atDamage S.aggressiveAnswer gs
              Spec.assertEqWith
                s
                "both are at their printed sizes"
                (S.powerToughnessOf giant after, S.powerToughnessOf instructor after)
                (Just (3, 3), Just (3, 1))
            _ -> Spec.assertFailure s "fixture should give alice an Instructor and a Giant"
        -- CR 508.1k's "attacking": the same Piker, small enough and on the same
        -- side, is no target while it stays home. The answerer aims at it anyway,
        -- so an ability that dropped the IsAttacking conjunct would mentor it.
        Spec.it s "CR 508.1k a creature that stayed home is no legal target" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker"] []
          case mine of
            [instructor, piker] ->
              Spec.assertEqWith
                s
                "the Piker is its printed 2/1"
                (S.powerToughnessOf piker (atDamage (plan [instructor] piker) gs))
                (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice an Instructor and a Piker"
        -- CR 603.3d, which sends a trigger through CR 601.2c-d: with TWO smaller
        -- attackers the rules leave which one open,
        -- so the controller is asked and the answer is honoured. More candidates
        -- than the slot needs, so the prompt cannot be short-circuited away.
        Spec.it s "CR 603.3d the controller picks which smaller attacker is mentored" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker", "Icehide Golem"] []
          case mine of
            [instructor, piker, golem] -> do
              let after = atDamage (plan [instructor, piker, golem] golem) gs
              Spec.assertEqWith
                s
                "the Golem took the counter and the Piker did not"
                (S.powerToughnessOf golem after, S.powerToughnessOf piker after)
                (Just (3, 3), Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice an Instructor, a Piker and a Golem"
        -- The bound is the SOURCE's power and not a number written into the
        -- ability: Hammer Dropper {2}{R}{W} Creature -- Giant Soldier 5/2 is the
        -- pool's other mentor, and the Hill Giant its 3-power sibling could not
        -- touch two cases up is a legal target for it -- same board, same
        -- declaration, only the mentor's power differs.
        Spec.it s "CR 702.134a a 5-power mentor reaches the 3/3 a 3-power one cannot" $ do
          (gs, mine, _) <- board ["Hammer Dropper", "Hill Giant"] []
          case mine of
            [dropper, giant] ->
              Spec.assertEqWith
                s
                "3 < 5, so the Giant is 4/4"
                (S.powerToughnessOf giant (atDamage (plan [dropper, giant] giant) gs))
                (Just (4, 4))
            _ -> Spec.assertFailure s "fixture should give alice a Dropper and a Giant"
        -- CR 608.2b re-checks the slot as the ability resolves, and rule 702.134a's
        -- comparison is part of what it re-checks. Two Instructors both aim at the
        -- 2/1 Piker; the first counter makes it 3/2, and 3 is no longer less than
        -- 3, so the second ability has no legal target and does not resolve. An
        -- engine that only checked at CR 601.2c would leave a 4/3.
        --
        -- That the second ability EXISTS is asserted at the mint below, not here:
        -- this board cannot tell a fizzled second trigger from a missing one.
        Spec.it s "CR 608.2b the second mentor's target is no longer legal" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Blade Instructor", "Goblin Piker"] []
          case mine of
            [first, second, piker] -> do
              let after = atDamage (plan [first, second, piker] piker) gs
              Spec.assertEqWith s "one counter landed" (S.powerToughnessOf piker after) (Just (3, 2))
              Spec.assertEqWith s "one, not two" (countersOn piker after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice two Instructors and a Piker"
        -- The same multiplicity asserted of the MINT, as exalted's and flanking's
        -- instance cases are, and with it the slot the gameplay cases above can
        -- only see through its effects: CR 508.3a's condition, and a target slot whose
        -- filter is the rule's two printed narrowings.
        Spec.it s "CR 702.134b two instances mint two targeting abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Mentor 2
              expectedSlot =
                TargetSlot.required
                  Pool.Creatures
                  (Just (Filter.Type.And [Filter.Type.IsAttacking, Filter.Type.PowerLessThanSource]))
              slotsOf ability = concatMap (Map.elems . Mode.targetSlots) (Modal.modes (TriggeredAbility.modal ability))
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each watching CR 508.3a's declaration"
            (fmap TriggeredAbility.condition abilities)
            [TriggerCondition.SelfAttacks TriggerFrequency.EveryTime, TriggerCondition.SelfAttacks TriggerFrequency.EveryTime]
          Spec.assertEqWith s "each with rule 702.134a's one slot" (concatMap slotsOf abilities) [expectedSlot, expectedSlot]

-- CR 702.134c, the OTHER half of rule 702.134: not mentor's own attack trigger but
-- an ability that watches a mentor ability RESOLVE. "An ability that triggers
-- whenever a creature mentors another creature triggers whenever a mentor ability
-- whose source is the first creature and whose target is the second creature
-- resolves", which is TriggerCondition.AttachedCreatureMentors read off
-- GameEvent.Mentored.
--
-- Aegis of the Legion {R}{W} Artifact -- Equipment is the card and the only printing
-- that reads rule 702.134c: "Equipped creature gets +1/+1 and has mentor. Whenever
-- equipped creature mentors a creature, put a shield counter on that creature. Equip
-- {3}". Every case below equips it by fixture (CR 301.5a's attachment as a state,
-- not the ability that makes it), so what is under test is the trigger rather than
-- CR 702.6a's equip.
--
-- Hill Giant 3/3 wears it, which makes it a 4/4 with mentor -- so no number here is
-- printed on any card in the board: the mentor's 4 is the Equipment's bonus, the
-- mentored Goblin Piker's 3/2 is its printed 2/1 plus rule 702.134a's counter, and 4
-- is not 3 is not 2. The Aegis itself is a fourth reading again, holding no counters
-- at all.
--
-- What separates "a creature MENTORED another" from "a creature WITH MENTOR
-- attacked" is the pair of declarations: the Giant attacking beside the Piker
-- mentors it, and the Giant attacking ALONE triggers rule 702.134a all the same and
-- mentors nothing, rule 702.134a's target having to be an attacking creature. The
-- two boards are the same board; only the attackers differ.
mentorsTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mentorsTriggerSpec s registry =
  let board mine = do
        ours <- mapM (S.printingOf s registry) mine
        pure (S.combatBoardOf ours [])
      plan :: [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      plan attackers target p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature target))) sets
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "CR 702.134c a creature mentoring another" $ do
        -- The proving test, and the whole of rule 702.134c in one board: the mentor
        -- ability resolves, and the ability watching it puts its counter on the
        -- creature that was MENTORED -- rule 702.134c's "second creature", which is
        -- neither the Aegis (the ability's own source) nor the Giant (the first
        -- creature). Rule 702.134a's +1/+1 counter sits beside it on the same
        -- permanent, so the two kinds are told apart rather than counted together.
        Spec.it s "CR 702.134c the mentored creature takes the shield counter" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion"]
          case mine of
            [giant, piker, aegis] -> do
              let after = atDamage (plan [giant, piker] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "the Piker carries rule 702.134a's counter and rule 702.134c's"
                (countersOn piker after)
                (Map.fromList [(CounterKind.PlusOnePlusOne, 1), (CounterKind.Shield, 1)])
              Spec.assertEqWith
                s
                "and neither the mentor nor the Equipment carries either"
                (countersOn giant after, countersOn aegis after)
                (Map.empty, Map.empty)
              Spec.assertEqWith
                s
                "the equipped Giant is a 4/4 and the mentored Piker a 3/2"
                (S.powerToughnessOf giant after, S.powerToughnessOf piker after)
                (Just (4, 4), Just (3, 2))
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker and an Aegis"
        -- The negative, and the case that makes the one above about MENTORING rather
        -- than about attacking: the same board, with the Piker held back. Rule
        -- 702.134a's ability still triggers -- the Giant attacked -- but rule
        -- 702.134a's target must be an attacking creature (CR 508.1k), so the
        -- ability has no legal target, never resolves, and rule 702.134c's event
        -- never happens. The answerer still aims at the Piker, so an engine that
        -- mentored a creature that stayed home would put both counters on it.
        Spec.it s "CR 702.134c attacking is not mentoring: nothing was mentored" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion"]
          case mine of
            [giant, piker, aegis] -> do
              let after = atDamage (plan [giant] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "no counters anywhere"
                (countersOn piker after, countersOn giant after)
                (Map.empty, Map.empty)
              Spec.assertEqWith
                s
                "and the Piker is its printed 2/1"
                (S.powerToughnessOf piker after)
                (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker and an Aegis"
        -- CR 122.6: BOTH counters go on through the placement funnel, so a CR 614.16
        -- replacement reaches them. Doubling Season ({5}{G}, "If an effect would put
        -- one or more counters on a permanent you control, it puts twice that many")
        -- doubles each, and 2 and 2 is a different reading from 1 and 1: rule
        -- 702.134a's counter would not double if the mentor opcode wrote it straight
        -- onto the permanent, and rule 702.134c's would not if the shield counter
        -- did.
        Spec.it s "CR 122.6 Doubling Season doubles both of them" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion", "Doubling Season"]
          case mine of
            [giant, piker, aegis, _] -> do
              let after = atDamage (plan [giant, piker] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "two of each"
                (countersOn piker after)
                (Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.Shield, 2)])
              Spec.assertEqWith s "so the Piker is a 4/3" (S.powerToughnessOf piker after) (Just (4, 3))
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker, an Aegis and a Doubling Season"
        -- The same funnel narrowed to ONE of the two kinds, which is what tells the
        -- readings apart that Doubling Season above leaves symmetrical: Hardened
        -- Scales ({G}, "If one or more +1/+1 counters would be put on a creature you
        -- control, that many plus one are put instead") reaches rule 702.134a's
        -- counter and not rule 702.134c's, so the Piker ends on two +1/+1 counters
        -- and one shield counter -- a pair no other reading of this board produces.
        Spec.it s "CR 614.16 Hardened Scales reaches the +1/+1 counter alone" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion", "Hardened Scales"]
          case mine of
            [giant, piker, aegis, _] -> do
              let after = atDamage (plan [giant, piker] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "two +1/+1 counters, one shield counter"
                (countersOn piker after)
                (Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.Shield, 1)])
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker, an Aegis and a Hardened Scales"
        -- CR 301.5f's "equipped creature", which is the whole of what the condition
        -- narrows by. A mentoring happens -- Blade Instructor's own printed mentor
        -- (CR 702.134a) puts its counter on the Piker -- and the Aegis, worn by an
        -- Icehide Golem that stayed home, is watching the wrong creature, so no
        -- shield counter is put. An engine that read the condition as "a creature
        -- mentors" rather than "equipped creature mentors" would fire here.
        Spec.it s "CR 702.134c another creature's mentoring is not the equipped creature's" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker", "Icehide Golem", "Aegis of the Legion"]
          case mine of
            [instructor, piker, golem, aegis] -> do
              let after = atDamage (plan [instructor, piker] piker) (S.attach aegis golem gs)
              Spec.assertEqWith
                s
                "the Instructor's counter landed and no shield counter did"
                (countersOn piker after)
                (Map.singleton CounterKind.PlusOnePlusOne 1)
              Spec.assertEqWith
                s
                "nor anywhere else"
                (countersOn golem after, countersOn instructor after, countersOn aegis after)
                (Map.empty, Map.empty, Map.empty)
            _ -> Spec.assertFailure s "fixture should give alice an Instructor, a Piker, a Golem and an Aegis"

-- CR 702.149a's training, which rule 702 states as a triggered
-- ability -- and the first whose trigger CONDITION reads the rest of the
-- declaration, through Filter.PowerGreaterThanSource and the source power
-- TriggerCondition.SelfAttacksWithAnother supplies.
--
-- Apprentice Sharpshooter {2}{G} Creature -- Human Archer 1/4 is the card: reach
-- and training, and reach touches nothing here, so every number below is the
-- keyword's. Its 1 power is what the companions are measured against -- Goblin
-- Piker's 2 clears it, a second Sharpshooter's 1 does not.
--
-- Readings are taken at the DECLARE BLOCKERS step, one step after the trigger
-- resolves, so the counter is read before combat damage can move anything.
trainingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trainingSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures. S.aggressiveAnswer
      -- attacks with everything, so a case about WHO attacks has to say it.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      atBlockers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      -- CR 122.1: what is actually on the permanent, which a +1/+1 EFFECT would
      -- leave empty while reading the same 2/5.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "Training" $ do
        -- The proving test. Both attack, the Piker's 2 beats the Sharpshooter's 1,
        -- and the assertion covers all three things at once: the counter lands on
        -- the BEARER, the companion takes none, and what landed is a CR 122.1a
        -- counter rather than a pump.
        Spec.it s "CR 702.149a whole card: attacking beside a bigger creature trains" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Goblin Piker"] []
          case mine of
            [sharpshooter, piker] -> do
              let after = atBlockers (plan [sharpshooter, piker]) gs
              Spec.assertEqWith
                s
                "the Sharpshooter is 2/5 and the Piker is untouched"
                (S.powerToughnessOf sharpshooter after, S.powerToughnessOf piker after)
                (Just (2, 5), Just (2, 1))
              Spec.assertEqWith s "and it is a counter" (countersOn sharpshooter after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice a Sharpshooter and a Piker"
        -- "POWER GREATER THAN this creature's power" is strict, so two 1-power
        -- Sharpshooters attacking together train neither. Same declaration shape
        -- as the proving test; only the companion's power differs.
        Spec.it s "CR 702.149a a companion whose power is only equal trains nobody" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Apprentice Sharpshooter"] []
          case mine of
            [first, second] -> do
              let after = atBlockers (plan [first, second]) gs
              Spec.assertEqWith
                s
                "both are at their printed size"
                (S.powerToughnessOf first after, S.powerToughnessOf second after)
                (Just (1, 4), Just (1, 4))
            _ -> Spec.assertFailure s "fixture should give alice two Sharpshooters"
        -- CR 508.3a's "attack": the Hill Giant is bigger and on the same side, and
        -- it trains nothing while it stays home. The falsifier for a condition that
        -- swept the battlefield instead of the declaration.
        Spec.it s "CR 508.3a a bigger creature that stayed home is no companion" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Hill Giant"] []
          case mine of
            [sharpshooter, _] ->
              Spec.assertEqWith
                s
                "the Sharpshooter is its printed 1/4"
                (S.powerToughnessOf sharpshooter (atBlockers (plan [sharpshooter]) gs))
                (Just (1, 4))
            _ -> Spec.assertFailure s "fixture should give alice a Sharpshooter and a Giant"
        -- Two combat phases in one turn, which is what makes the COMBAT RECORD the
        -- right source and the event log the wrong one: the log keeps the whole
        -- turn's declarations, so a log-fold would find Aurelia in the
        -- second declaration she is not part of and train the Sharpshooter twice.
        -- Aurelia, the Warleader {2}{R}{R}{W}{W} 3/4 is the pool's extra-combat
        -- attacker, and her 3 power clears the Sharpshooter's 1 in the first phase.
        Spec.it s "CR 702.149a the added combat phase counts only its own declaration" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Aurelia, the Warleader"] []
          case mine of
            [sharpshooter, aurelia] -> do
              let first = atBlockers (plan [sharpshooter, aurelia]) gs
                  second = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (plan [sharpshooter]) first
                  after = atBlockers (plan [sharpshooter]) second
              Spec.assertEqWith s "one counter from the first declaration" (S.powerToughnessOf sharpshooter first) (Just (2, 5))
              Spec.assertEqWith s "the second phase really ran a declaration" (GameState.phase second) (Phase.Combat CombatStep.DeclareAttackers)
              Spec.assertEqWith s "and it added no second counter" (countersOn sharpshooter after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice a Sharpshooter and Aurelia"
        -- The same multiplicity asserted of the MINT, as mentor's and flanking's
        -- instance cases are: CR 702.149b says each instance triggers separately,
        -- and no card in the pool prints training twice.
        Spec.it s "CR 702.149b two instances mint two abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Training 2
              expected =
                TriggerCondition.SelfAttacksWithAnother
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.PowerGreaterThanSource])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each watching CR 702.149a's declaration"
            (fmap TriggeredAbility.condition abilities)
            [expected, expected]

-- CR 702.149c's second trigger form: "when this creature trains" means "when a
-- resolving training ability puts one or more +1/+1 counters on this creature".
--
-- Savior of Ollenbock {1}{W}{W} Creature -- Human Soldier 1/2 is the only paper
-- printing, and the whole card is here: training, "whenever this creature trains,
-- exile up to one other target creature from the battlefield or creature card
-- from a graveyard", and "when this creature leaves the battlefield, put the
-- exiled cards onto the battlefield under their owners' control".
--
-- The exile clause is what makes the trigger OBSERVABLE at gameplay level: rule
-- 702.149c's marker is otherwise invisible, the counter it rides being an
-- ordinary +1/+1 counter. So every case below reads the exile rather than the
-- counter, and the counter assertions are there to prove the training half
-- happened at all.
--
-- The pair of boards differs in exactly one thing: the companion's POWER, moved
-- across rule 702.149a's threshold by a continuous effect rather than by swapping
-- the card, so seats, timing, stock and the declaration are identical.
--
-- The other-source case is Battlegrowth's counter, which is the discrimination
-- this whole unit exists for: a +1/+1 counter arriving from anything but a
-- resolving training ability trains nobody.
saviorOfOllenbockSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
saviorOfOllenbockSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures, trainingSpec's.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      -- CR 601.2c's announcement for the trained creature's trigger: one target,
      -- PINNED rather than searched. An answerer that picked a legal option would
      -- find another one after a mutation and repair the assertion; this one hands
      -- back the recipient the case names, tag and all -- ToCreature for the
      -- battlefield half of the pool, ToObject for the graveyard half.
      aimingAt :: Recipient.Recipient -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      aimingAt recipient attackers p = case p of
        Prompt.AnnounceTargets _ _ _ offers -> fmap (const 1) offers
        Prompt.ChooseTargets _ _ _ asked -> fmap (const (Set.singleton recipient)) asked
        _ -> plan attackers p
      atBlockers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      nameOf oid gs = fmap Face.name (Game.faceOf oid gs)
      -- Exile is one shared zone (CR 400.1), so this is everything in it whoever
      -- owns it. By NAME, because CR 400.7 mints the exiled card a fresh id.
      exiledNames gs = List.sort (Maybe.mapMaybe (`nameOf` gs) (Set.toList (GameState.exile gs)))
      controlledNames pid gs =
        List.sort (Maybe.mapMaybe (\oid -> if Projection.controllerOf oid gs == Just pid then nameOf oid gs else Nothing) (Set.toList (GameState.battlefield gs)))
      graveyardNames pid gs = List.sort (Maybe.mapMaybe (`nameOf` gs) (Game.zoneMembers Zone.Graveyard pid gs))
      -- Destroy the Savior (CR 701.8a), settle so the CR 117.5 boundary scans the
      -- departure and places the leaves-the-battlefield trigger, then resolve it --
      -- promiseOfTomorrowReturnSpec's killIt, and BOTH states come back for its
      -- reason: "the ability triggered" is only readable at the placement.
      killIt oid gs =
        let killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [oid])
            placed = S.runPure S.identityAnswer killed Engine.settleForPriority
         in (placed, S.runPure S.identityAnswer placed Stack.resolveTop)
      named = CardName.MkCardName . Text.pack
      -- Rule 702.149a's threshold crossed from below by a continuous effect: the
      -- Piker's 2 power becomes 1, which is the Savior's own, and CR 702.149a's
      -- "greater" is strict.
      shrink oid = S.withEffect oid (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal (-1)) (Quantity.Type.Literal 0)))
   in Spec.describe s "CR 702.149c a trigger on training" $ do
        -- The proving test. The Piker's 2 clears the Savior's 1, the training
        -- ability resolves and puts the counter, and rule 702.149c's trigger then
        -- exiles the creature it targeted.
        Spec.it s "CR 702.149c whole card: training exiles the targeted creature" $ do
          (gs, mine, theirs) <- board ["Savior of Ollenbock", "Goblin Piker"] ["Hill Giant"]
          case (mine, theirs) of
            ([savior, piker], [giant]) -> do
              let after = atBlockers (aimingAt (Recipient.ToCreature giant) [savior, piker]) gs
              Spec.assertEqWith
                s
                "the counter landed on the Savior and not on its companion"
                (countersOn savior after, countersOn piker after)
                (Map.singleton CounterKind.PlusOnePlusOne 1, Map.empty)
              Spec.assertEqWith s "and the trigger exiled the Giant" (exiledNames after) [named "Hill Giant"]
              Spec.assertEqWith s "which bob no longer controls" (controlledNames S.bob after) []
            _ -> Spec.assertFailure s "fixture should give alice a Savior and a Piker, and bob a Giant"
        -- The one-difference control: the same board with the companion's power
        -- one lower, so rule 702.149a's strict "greater" is not met, nothing
        -- trains, and rule 702.149c's trigger never fires.
        Spec.it s "CR 702.149a a companion whose power is only equal exiles nothing" $ do
          (gs, mine, theirs) <- board ["Savior of Ollenbock", "Goblin Piker"] ["Hill Giant"]
          case (mine, theirs) of
            ([savior, piker], [giant]) -> do
              let weakened = shrink piker gs
                  after = atBlockers (aimingAt (Recipient.ToCreature giant) [savior, piker]) weakened
              Spec.assertEqWith s "the companion really is a 1/1 now" (S.powerToughnessOf piker weakened) (Just (1, 1))
              Spec.assertEqWith s "no counter was put" (countersOn savior after) Map.empty
              Spec.assertEqWith s "and nothing was exiled" (exiledNames after) []
              Spec.assertBool s (S.onBattlefield giant after) "the Giant is where it was"
            _ -> Spec.assertFailure s "fixture should give alice a Savior and a Piker, and bob a Giant"
        -- The pool's OTHER half, and the tag that goes with it: a creature card in
        -- a graveyard is ToObject, where the battlefield half is ToCreature. Bob's
        -- graveyard, so "a graveyard" is not read as the controller's own.
        Spec.it s "CR 404.1 whole card: the same slot reaches a creature card in a graveyard" $ do
          (gs, mine, _) <- board ["Savior of Ollenbock", "Goblin Piker"] []
          sentry <- S.printingOf s registry "Ogre Sentry"
          case mine of
            [savior, piker] -> do
              let (card, stocked) = S.addGraveyardCard sentry S.bob gs
                  after = atBlockers (aimingAt (Recipient.ToObject card) [savior, piker]) stocked
              Spec.assertEqWith s "the Savior trained" (countersOn savior after) (Map.singleton CounterKind.PlusOnePlusOne 1)
              Spec.assertEqWith s "and the graveyard card is in exile" (exiledNames after) [named "Ogre Sentry"]
              Spec.assertEqWith s "out of bob's graveyard" (graveyardNames S.bob after) []
            _ -> Spec.assertFailure s "fixture should give alice a Savior and a Piker"
        -- CR 607.2a's linked set read back by the card's third ability. The victim
        -- is BOB's card, so "under their owners' control" is observable: the
        -- ability's controller is alice, and an owner-blind return would hand her
        -- the Sentry.
        Spec.it s "CR 607.2a whole card: the Savior leaving the battlefield returns what it exiled, to its owner" $ do
          (gs, mine, _) <- board ["Savior of Ollenbock", "Goblin Piker"] []
          sentry <- S.printingOf s registry "Ogre Sentry"
          case mine of
            [savior, piker] -> do
              let (card, stocked) = S.addGraveyardCard sentry S.bob gs
                  exiled = atBlockers (aimingAt (Recipient.ToObject card) [savior, piker]) stocked
                  (placed, after) = killIt savior exiled
              Spec.assertEqWith s "the premise: the Sentry is in exile" (exiledNames exiled) [named "Ogre Sentry"]
              Spec.assertEqWith s "the departure placed one trigger" (length (GameState.stack placed)) 1
              Spec.assertEqWith s "exile is empty again" (exiledNames after) []
              Spec.assertEqWith s "and bob controls the Sentry" (controlledNames S.bob after) [named "Ogre Sentry"]
              Spec.assertEqWith s "while alice keeps only her Piker" (controlledNames S.alice after) [named "Goblin Piker"]
            _ -> Spec.assertFailure s "fixture should give alice a Savior and a Piker"
        -- The discrimination rule 702.149c is FOR: a +1/+1 counter arriving from
        -- Battlegrowth ({G} Instant, "put a +1/+1 counter on target creature") is
        -- the same counter the training ability would have put, and it trains
        -- nobody. Nothing attacks, so nothing else could.
        Spec.it s "CR 702.149c a +1/+1 counter from another source is not training" $ do
          savior <- S.printingOf s registry "Savior of Ollenbock"
          giant <- S.printingOf s registry "Hill Giant"
          forest <- S.printingOf s registry "Forest"
          battlegrowth <- S.printingOf s registry "Battlegrowth"
          let (saviorId, withSavior) = S.addCreature savior S.alice (S.landsInPlay forest 1)
              (giantId, withGiant) = S.addCreature giant S.bob withSavior
              (handed, spellId) = S.handOne battlegrowth withGiant
              cast = snd (Engine.runGamePure (aimingAt (Recipient.ToCreature saviorId) []) handed (S.cast S.alice spellId))
              resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
              -- The SETTLE is the load-bearing step, not the resolution: CR 603.3
              -- places a trigger when the boundary scans the log, so a state read
              -- straight off the resolution cannot tell "never triggered" from
              -- "triggered and not yet placed". Without this the case passes
              -- against a condition that fires on every counter placement.
              after = S.runPure S.identityAnswer resolved Engine.settleForPriority
          Spec.assertEqWith s "the counter really arrived" (countersOn saviorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "and the settle placed no trigger" (length (GameState.stack after)) 0
          Spec.assertEqWith s "and nothing was exiled" (exiledNames after) []
          Spec.assertBool s (S.onBattlefield giantId after) "the Giant bob controls is untouched"

-- CR 702.147a's decayed: a combat restriction and a triggered ability that arms
-- a CR 603.7 DELAYED one -- the first minted ability to arm anything, and so the
-- first Effect.ArmDelayedTrigger whose name is on no face
-- (Keyword.mintedDelayedAbilities).
--
-- Falcon Abomination {2}{U} Creature -- Zombie Bird 2/2 is the producer: flying,
-- and "when this creature enters, create a 2/2 black Zombie creature token with
-- decayed". Decayed is printed on tokens far more often than on cards, so the
-- keyword arrives here through the card's own Create -- codec-parsed card data,
-- never a hand-built face -- and the Falcon beside it is the control, a creature
-- of the same size and controller with no decayed.
--
-- bob defends with NOTHING, deliberately: a blocked 2/2 token would die to CR
-- 704.5g whether or not rule 702.147a did anything, so the sacrifice assertion
-- would pass for the wrong reason.
decayedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
decayedSpec s registry =
  let settleFor gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- CR 302.6: the token is minted this turn and so is summoning sick, and
      -- nothing in the pool gives a decayed token haste -- so this is the state a
      -- turn later would reach, and the one fixture step below that is not the
      -- card's own doing.
      settled oid gs =
        gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Settled S.alice}) oid (GameState.objects gs)}
      -- alice's Falcon Abomination, entered and its trigger resolved, with the
      -- Zombie token settled beside it.
      board = do
        falcon <- S.printingOf s registry "Falcon Abomination"
        let (gs0, _, _) = S.combatBoardOf [] []
            (bird, gs1) = S.entersWithTrigger falcon S.alice gs0
            made = resolveAll (settleFor gs1)
        pure (bird, made, S.tokensOf made)
      noAttacks :: Prompt.Prompt r -> r
      noAttacks p = case p of
        Prompt.DeclareAttackers {} -> []
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      atMain = S.runToStep Phase.PostcombatMain
   in Spec.describe s "Decayed (CR 702.147)" $ do
        -- CR 509.1b through rule 702.147a's static half, unleash's carrier with
        -- no gate. The Falcon is on the same board with the same controller, so a
        -- restriction that stopped every creature blocking cannot pass.
        Spec.it s "CR 702.147a the token enters with decayed and cannot block" $ do
          (bird, made, tokens) <- board
          case tokens of
            [zombie] -> do
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Decayed zombie made) "the token has decayed"
              Spec.assertBool s (not (Combat.canBlock S.alice zombie made)) "so it cannot block"
              Spec.assertBool s (Combat.canBlock S.alice bird made) "while the Falcon that made it can"
              Spec.assertEqWith s "and only the Falcon is offered" (Combat.legalBlockers S.alice made) [bird]
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 508.3a's declaration puts the minted trigger on the stack, and its
        -- resolution arms rule 702's own delayed ability -- the only armed entry
        -- on the board, since no card here declares one.
        Spec.it s "CR 702.147a attacking arms one delayed ability" $ do
          (_, made, tokens) <- board
          case tokens of
            [zombie] -> do
              let after = atBlockers S.aggressiveAnswer (settled zombie made)
              Spec.assertBool s (elem zombie (S.attackerDeclarationsOf after)) "the token really attacked"
              Spec.assertEqWith s "one delayed ability waiting" (Seq.length (GameState.delayedTriggers after)) 1
              Spec.assertBool s (S.onBattlefield zombie after) "and it is still there at declare blockers"
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 511.2: an ability that triggers "at end of combat" triggers as the
        -- end of combat step begins. The Falcon attacked too and survives, so
        -- "everything alice attacked with died" cannot pass this.
        Spec.it s "CR 511.2 the delayed ability sacrifices it at end of combat" $ do
          (bird, made, tokens) <- board
          case tokens of
            [zombie] -> do
              let after = atMain S.aggressiveAnswer (settled zombie made)
              Spec.assertBool s (not (S.onBattlefield zombie after)) "the token is gone"
              Spec.assertBool s (S.onBattlefield bird after) "while the Falcon that attacked beside it is still there"
              Spec.assertEqWith s "and the delayed ability is spent" (Seq.length (GameState.delayedTriggers after)) 0
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- THE PAIR THAT MAKES THE TRIGGER REAL. Same board, same fixture, and
        -- only the declaration different: rule 702.147a sacrifices a creature
        -- that ATTACKED, so a decayed creature held back survives its own end of
        -- combat. CR 508.8 skips the declare blockers and combat damage steps on
        -- this run, which the end of combat step is not among.
        Spec.it s "CR 702.147a a decayed creature that did not attack is not sacrificed" $ do
          (_, made, tokens) <- board
          case tokens of
            [zombie] -> do
              let after = atMain noAttacks (settled zombie made)
              Spec.assertEqWith s "nothing was declared" (S.attackerDeclarationsOf after) []
              Spec.assertEqWith s "so nothing was armed" (Seq.length (GameState.delayedTriggers after)) 0
              Spec.assertBool s (S.onBattlefield zombie after) "and the token is still there"
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))

-- CR 603.2's "that player" reaching a TARGET SLOT rather than an effect's
-- operand: Trygon Predator's "whenever this creature deals combat damage to a
-- player, you may destroy target artifact or enchantment THAT PLAYER controls".
-- The slot is narrowed by Filter.ControlledByBound, baked to the damaged player
-- by Pawl.Engine.Filter.bakeBound at both of CR 115's moments -- CR 603.3d's
-- choosing and CR 608.2b's re-check.
--
-- THREE SEATS, and every seat holds the same permanent (Bad Moon), so the board
-- differs in exactly one thing: who controls it. A filter that read "an
-- opponent" would admit bob's, and one that dropped the controller conjunct
-- would admit alice's own; the answerer below takes the LOWEST-numbered legal
-- target of each slot, and alice's Moon is added before bob's and bob's before
-- carol's, so either mistake takes a different permanent rather than passing.
trygonPredatorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trygonPredatorSpec s registry =
  let plan :: Prompt.Prompt r -> r
      plan p = case p of
        Prompt.ChooseDefender {} -> S.carol
        -- CR 603.5's printed "may", always exercised: a declined clause would
        -- destroy nothing, and this group is about which permanent it reaches.
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseTargets _ _ _ asked -> fmap (maybe Set.empty Set.singleton . Set.lookupMin . snd) asked
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board = do
        trygon <- S.printingOf s registry "Trygon Predator"
        badMoon <- S.printingOf s registry "Bad Moon"
        let (gs0, mine, theirs, others) = S.threePlayerCombat [trygon, badMoon] [badMoon] [badMoon]
            -- S.threePlayerCombat starts at the beginning of combat, so the
            -- declarations are run as steps (which is what fills CR 508.5's
            -- defending player) and only the damage is dealt by hand -- that is
            -- the seam CR 603.3d's placement needs to be observable in, since a
            -- whole step would resolve the trigger as well.
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) plan gs0
            fought = S.runPure plan atDamage Damage.dealCombatDamage
        pure (mine, theirs, others, S.runPure plan fought Engine.settleForPriority)
   in Spec.describe s "TrygonPredator" $ do
        -- THE proving test. CR 603.3d picks the target as the ability is put on
        -- the stack, and the binding it stamps is read back here rather than
        -- inferred from what died -- so this says which permanent was OFFERED,
        -- not merely which one an effect happened to reach.
        Spec.it s "CR 603.3d the slot admits only the damaged player's permanent" $ do
          (mine, theirs, others, placed) <- board
          case (mine, theirs, others, GameState.stack placed) of
            ([_, alices], [bobs], [carols], [abilityId]) -> do
              let bindings = maybe Map.empty Object.bindings (Game.lookupObject abilityId placed)
                  slotOf name = Map.lookup (SlotName.MkSlotName (Text.pack name)) (Binding.targetsOf bindings)
              Spec.assertEqWith
                s
                "carol took the damage, so she is the player the event bound"
                (slotOf "thatPlayer")
                (Just (Set.singleton (Recipient.ToPlayer S.carol)))
              Spec.assertEqWith
                s
                "and carol's Bad Moon is the one target the slot admitted"
                (slotOf "target")
                (Just (Set.singleton (Recipient.ToObject carols)))
              Spec.assertBool s (alices /= bobs && bobs /= carols) "the three Moons are distinct objects"
            _ -> Spec.assertFailure s "fixture should give alice a Predator and a Moon, bob and carol a Moon each, and place one trigger"
        -- The same board run to the end: the ability resolves and destroys the
        -- one permanent, leaving both other seats' untouched. The whole card,
        -- CR 701.8a's destruction included.
        Spec.it s "CR 608.2c whole card: only carol's Bad Moon is destroyed" $ do
          (mine, theirs, others, placed) <- board
          let after = S.runPure plan placed Engine.priorityLoop
          case (mine, theirs, others) of
            ([_, alices], [bobs], [carols]) -> do
              Spec.assertBool s (not (S.onBattlefield carols after)) "carol's Moon is destroyed"
              Spec.assertBool s (S.onBattlefield bobs after) "bob's Moon is untouched"
              Spec.assertBool s (S.onBattlefield alices after) "and so is alice's own"
            _ -> Spec.assertFailure s "fixture should give each seat a Moon"
        -- CR 608.2b at the OTHER moment: the target changes hands after it was
        -- chosen, so it is no longer a permanent that player controls and the
        -- ability's only target is illegal. The pair differs in exactly the
        -- control change -- same board, same answers, same stack -- which is
        -- what makes the survival the rule's and not the fixture's.
        Spec.it s "CR 608.2b a target that changes hands is no longer that player's" $ do
          (_, _, others, placed) <- board
          case others of
            [carols] -> do
              let stolen = S.runPure plan (S.giveControl carols S.bob placed) Engine.priorityLoop
                  kept = S.runPure plan placed Engine.priorityLoop
              Spec.assertBool s (S.onBattlefield carols stolen) "bob controls it now, so the ability fizzles"
              Spec.assertBool s (not (S.onBattlefield carols kept)) "and without the change it is destroyed"
            _ -> Spec.assertFailure s "fixture should give carol a Moon"

-- BOTH halves of one DamageDealt event read by one bearer-scoped trigger:
-- Questing Beast {2}{G}{G} Legendary Creature -- Beast 4/4, "whenever Questing
-- Beast deals combat damage to an opponent, it deals THAT MUCH damage to target
-- planeswalker THAT PLAYER controls". The amount rides
-- Pawl.Engine.Binding.eventAmount and the player Binding.triggerPlayer, and the
-- target slot narrows by Filter.ControlledByBound off the second -- so the two
-- slots the condition stamps are read at once, one as a Quantity and one as a
-- filter.
--
-- THREE SEATS, bob and carol holding the same planeswalker printing, so "that
-- player controls" is a different set from "an opponent controls" and from "a
-- planeswalker" (trygonPredatorSpec above makes the same distinction for the
-- destroy half of the pattern).
--
-- THREE DISTINCT NUMBERS, so the loyalty count names one reading of "that much"
-- and rejects two: a -1/-1 counter makes the Beast a 3/3 before it connects, so
-- the event carries 3 rather than the printed 4, and four +1/+1 counters added
-- AFTER the damage but BEFORE the ability resolves leave it a 7/7, so a payload
-- reading the source's power at resolution would take 7. Both walkers start on 6
-- loyalty counters, which survives 3 and 4 and dies to 7.
--
-- "An opponent" is deliberately not transcribed, and this is a rules equivalence
-- rather than an elision: CR 508.1a lets only the active player's creatures
-- attack, CR 506.2 and CR 506.2a make the defending player one of the attacking
-- player's opponents, CR 510.1b assigns an unblocked creature's combat damage to
-- what it is attacking, and CR 506.4 removes a permanent from combat if its
-- controller changes. A creature can only ever deal combat damage to a player who
-- is its controller's opponent, so the nullary condition admits exactly the
-- printed events.
questingBeastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
questingBeastSpec s registry =
  let plan :: Prompt.Prompt r -> r
      plan p = case p of
        Prompt.ChooseDefender {} -> S.carol
        -- CR 508.1b's choice pinned to the PLAYER: carol's own planeswalker is a
        -- legal attack target too, and attacking it would deal no combat damage
        -- to a player at all.
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer S.carol) (NonEmpty.toList options))
        Prompt.ChooseTargets _ _ _ asked -> fmap (maybe Set.empty Set.singleton . Set.lookupMin . snd) asked
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board = do
        beast <- S.printingOf s registry "Questing Beast"
        karn <- S.printingOf s registry "Karn Liberated"
        let (gs0, mine, theirs, others) = S.threePlayerCombat [beast] [karn] [karn]
            loyal = List.foldl' (flip (S.addCounter CounterKind.Loyalty 6)) gs0 (theirs <> others)
            shrunk = List.foldl' (flip (S.addCounter CounterKind.MinusOneMinusOne 1)) loyal mine
            -- The same seam trygonPredatorSpec uses: the declarations run as
            -- steps, the damage is dealt by hand, and settleForPriority places
            -- the trigger -- which leaves a state where the ability is on the
            -- stack and has not resolved.
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) plan shrunk
            fought = S.runPure plan atDamage Damage.dealCombatDamage
            placed = S.runPure plan fought Engine.settleForPriority
        pure (mine, theirs, others, shrunk, placed)
   in Spec.describe s "QuestingBeast" $ do
        -- THE proving test, at gameplay level: what carol's planeswalker lost.
        Spec.it s "CR 120.3c whole card: that much is the damage the event carried" $ do
          (mine, theirs, others, before, placed) <- board
          case (mine, theirs, others) of
            ([beastId], [bobs], [carols]) -> do
              let bumped = S.addCounter CounterKind.PlusOnePlusOne 4 beastId placed
                  after = S.runPure plan bumped Engine.priorityLoop
              -- The fixture's own preconditions, asserted rather than assumed:
              -- neither can be reddened by the binding under test.
              Spec.assertEqWith s "the -1/-1 counter makes the Beast a 3/3 before it connects" (S.powerToughnessOf beastId before) (Just (3, 3))
              Spec.assertEqWith s "and both walkers start on 6 loyalty" (S.counterOf CounterKind.Loyalty bobs before, S.counterOf CounterKind.Loyalty carols before) (6, 6)
              Spec.assertEqWith s "CR 120.3c: 6 - 3, not 6 - 4 and not dead on 7" (S.counterOf CounterKind.Loyalty carols after) 3
              Spec.assertEqWith s "and bob's planeswalker is untouched" (S.counterOf CounterKind.Loyalty bobs after) 6
              Spec.assertEqWith s "CR 510.1b: carol herself took the Beast's 3" (S.lifeOf S.carol after) (Just 17)
              Spec.assertEqWith s "CR 704.5q: the Beast is a 7/7 by the time the ability resolves" (S.powerToughnessOf beastId after) (Just (7, 7))
            _ -> Spec.assertFailure s "fixture should give alice a Beast and bob and carol a planeswalker each"
        -- The slots themselves, read back off the placed ability rather than
        -- inferred from what happened -- so this says which player the event
        -- named and which permanent the filter OFFERED.
        Spec.it s "CR 603.3d the slot admits only the damaged player's planeswalker" $ do
          (_, theirs, others, _, placed) <- board
          case (theirs, others, GameState.stack placed) of
            ([bobs], [carols], [abilityId]) -> do
              let bindings = maybe Map.empty Object.bindings (Game.lookupObject abilityId placed)
                  slotOf name = Map.lookup (SlotName.MkSlotName (Text.pack name)) (Binding.targetsOf bindings)
              Spec.assertEqWith s "carol took the damage, so she is the player the event bound" (slotOf "thatPlayer") (Just (Set.singleton (Recipient.ToPlayer S.carol)))
              Spec.assertEqWith s "and carol's planeswalker is the one target the slot admitted" (slotOf "target") (Just (Set.singleton (Recipient.ToObject carols)))
              Spec.assertBool s (bobs /= carols) "the two planeswalkers are distinct objects"
            _ -> Spec.assertFailure s "fixture should give bob and carol a planeswalker each and place one trigger"

-- The same "that player", stamped by the FILTERED twin of that condition and read
-- by a BYSTANDER: Larceny {3}{B}{B} Enchantment, "whenever a creature you control
-- deals combat damage to a player, that player discards a card". The whole card is
-- that one clause, so every reading below is the condition's.
--
-- THREE SEATS, each holding three cards, so the damaged player (carol), the
-- damager's controller (alice) and a bystanding opponent (bob) are three
-- different hands -- and three cards apiece makes "discarded once" (two left)
-- distinguishable from "discarded twice" (one) and from "not at all" (three).
larcenySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
larcenySpec s registry =
  let plan :: Prompt.Prompt r -> r
      plan p = case p of
        Prompt.ChooseDefender {} -> S.carol
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer S.carol) (NonEmpty.toList options))
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board = do
        larceny <- S.printingOf s registry "Larceny"
        piker <- S.printingOf s registry "Goblin Piker"
        let (gs0, _, _, _) = S.threePlayerCombat [larceny, piker] [] []
            stocked = List.foldl' (\g pid -> List.foldl' (\g' _ -> snd (S.addHandCard piker pid g')) g [(), (), ()]) gs0 [S.alice, S.bob, S.carol]
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) plan stocked
            fought = S.runPure plan atDamage Damage.dealCombatDamage
            placed = S.runPure plan fought Engine.settleForPriority
        pure (stocked, placed)
   in Spec.describe s "Larceny" $ do
        -- THE proving test, at gameplay level: whose hand shrank.
        Spec.it s "CR 603.2 whole card: the DAMAGED player discards, not the damager's controller" $ do
          (before, placed) <- board
          let after = S.runPure plan placed Engine.priorityLoop
          Spec.assertEqWith s "all three seats start on three cards" (S.handSize S.alice before, S.handSize S.bob before, S.handSize S.carol before) (3, 3, 3)
          Spec.assertEqWith s "carol was dealt the combat damage, so carol discarded exactly one" (S.handSize S.carol after) 2
          Spec.assertEqWith s "alice, whose creature dealt it, discarded none" (S.handSize S.alice after) 3
          Spec.assertEqWith s "and bob, who was not in the combat, none either" (S.handSize S.bob after) 3
          Spec.assertEqWith s "CR 510.1b: the Piker's 2 reached carol" (S.lifeOf S.carol after) (Just 18)
        -- The slot itself, read off the placed ability.
        Spec.it s "CR 603.2 the damaged player is what the event stamped" $ do
          (_, placed) <- board
          case GameState.stack placed of
            [abilityId] -> do
              let bindings = maybe Map.empty Object.bindings (Game.lookupObject abilityId placed)
              Spec.assertEqWith s "thatPlayer is carol" (Map.lookup (SlotName.MkSlotName (Text.pack "thatPlayer")) (Binding.targetsOf bindings)) (Just (Set.singleton (Recipient.ToPlayer S.carol)))
            other -> Spec.assertFailure s ("expected exactly one placed trigger, got " <> show (length other))

-- CR 702.39a's provoke, which rule 702 states as a triggered
-- ability and the FIRST whose payload creates a CR 509.1c blocking REQUIREMENT
-- -- so this is the group that runs a resolution-created requirement through the
-- declare blockers step, and with it Filter.ControlledByDefendingPlayer.
--
-- Goblin Grappler {R} Creature -- Goblin 1/1 is the card: provoke and nothing
-- else, so every reading below is the keyword's. Its victims are the pool's
-- vanillas.
--
-- The answerer DECLINES to block throughout. That is what makes every positive
-- reading a claim about CR 509.1c: the block that happens is the one the rules
-- force, never one the interpreter asked for.
provokeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
provokeSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- Exercise or decline rule 702.39a's "may", aim its target, and never
      -- block voluntarily. S.aggressiveAnswer would block with everything and
      -- Script.declining would decline the "may", so a case about either has to
      -- say both itself.
      plan :: OptionalDecision.OptionalDecision -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      plan may target p = case p of
        Prompt.ChooseOptional {} -> may
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature target))) sets
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      atBlockers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)
   in Spec.describe s "Provoke" $ do
        -- The proving test, and it covers both halves of rule 702.39a at once:
        -- bob's only creature is TAPPED, so CR 509.1a makes it no candidate at
        -- all until the untap, and the block that follows is CR 509.1c's
        -- requirement overriding an interpreter that declined to block.
        Spec.it s "CR 702.39a whole card: the provoked creature untaps and blocks" $ do
          (gs0, mine, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case (mine, theirs) of
            ([grappler], [piker]) -> do
              let after = atDamage (plan OptionalDecision.Exercises piker) (S.tapObject piker gs0)
              Spec.assertEqWith s "the Piker is blocking the Grappler" (Combat.blockersOf grappler after) (Set.singleton piker)
            _ -> Spec.assertFailure s "fixture should give alice a Grappler and bob a Piker"
        -- CR 603.5 / 608.2e: one printed "may" over one clause, so declining it
        -- withholds BOTH instructions. The same board and the same declining
        -- blocker answer as the proving test; only the answer to the "may"
        -- differs, which is what makes that test's block the keyword's.
        Spec.it s "CR 603.5 declining the may leaves the creature tapped and blocking nothing" $ do
          (gs0, mine, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case (mine, theirs) of
            ([grappler], [piker]) -> do
              let after = atDamage (plan OptionalDecision.Declines piker) (S.tapObject piker gs0)
              Spec.assertEqWith s "nothing blocks" (Combat.blockersOf grappler after) Set.empty
              Spec.assertEqWith s "and it is still tapped" (tapStateOf piker after) (Just TapState.Tapped)
            _ -> Spec.assertFailure s "fixture should give alice a Grappler and bob a Piker"
        -- The REQUIREMENT alone, with the untap taken out of the picture: bob's
        -- creature is already untapped, so it could have blocked or not, and CR
        -- 509.1c is the only thing that makes declining illegal.
        Spec.it s "CR 509.1c an untapped provoked creature must block anyway" $ do
          (gs0, mine, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case (mine, theirs) of
            ([grappler], [piker]) ->
              Spec.assertEqWith
                s
                "the Piker is blocking"
                (Combat.blockersOf grappler (atDamage (plan OptionalDecision.Exercises piker) gs0))
                (Set.singleton piker)
            _ -> Spec.assertFailure s "fixture should give alice a Grappler and bob a Piker"
        -- The control for the case above, and the reason it is not vacuous: the
        -- same board with a provokeless attacker lets the declining answer
        -- stand. Goblin Piker {1}{R} 2/1 is the pool's vanilla, so the ONLY
        -- difference between the two boards is the keyword.
        Spec.it s "CR 509.1 the same board without provoke lets the defender decline" $ do
          (gs0, mine, theirs) <- board ["Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([attacker], [piker]) ->
              Spec.assertEqWith
                s
                "nothing blocks"
                (Combat.blockersOf attacker (atDamage (plan OptionalDecision.Exercises piker) gs0))
                Set.empty
            _ -> Spec.assertFailure s "fixture should give alice and bob a Piker each"
        -- CR 508.5 at THREE seats, where "defending player" and "an opponent"
        -- come apart: alice attacks carol, so bob's creature is an opponent's and
        -- is still no legal target. Asked of the slot itself rather than through
        -- a block, because a wrongly admitted target would be untapped and then
        -- pruned by CR 509.1b anyway -- the illegal CHOICE is the observable.
        --
        -- The slot is read off the MINTED ability rather than restated here, so
        -- this is a claim about what provoke writes and not about the atom alone.
        Spec.it s "CR 508.5 only the defending player's creature is a legal target" $ do
          grappler <- S.printingOf s registry "Goblin Grappler"
          piker <- S.printingOf s registry "Goblin Piker"
          let (gs0, mine, theirs, others) = S.threePlayerCombat [grappler] [piker] [piker]
              minted = concatMap (concatMap (Map.elems . Mode.targetSlots) . Modal.modes . TriggeredAbility.modal) (Keyword.abilitiesFor Keyword.Type.Provoke 1)
          case (mine, theirs, others, minted) of
            ([attacker], [bobs], [carols], [slot]) -> do
              let after = atBlockers (S.attackTo S.carol) gs0
              Spec.assertEqWith
                s
                "carol is the defending player, so only her creature"
                (Target.legalRecipients (Just S.alice) attacker slot after)
                (Set.singleton (Recipient.ToCreature carols))
              Spec.assertBool
                s
                (Set.notMember (Recipient.ToCreature bobs) (Target.legalRecipients (Just S.alice) attacker slot after))
                "bob is an opponent but not the defender"
            _ -> Spec.assertFailure s "fixture should give each seat one creature"
        -- CR 500.5a: "this combat" ends with the combat PHASE, so the stored
        -- requirement is swept before the postcombat main phase. Read off the
        -- store rather than through a block, there being no second declare
        -- blockers step in one combat phase to observe it in.
        Spec.it s "CR 500.5a the stored requirement lasts exactly the combat phase" $ do
          (gs0, _, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case theirs of
            [piker] -> do
              let atBlock = atBlockers (plan OptionalDecision.Exercises piker) gs0
                  -- S.runToStep stops as soon as combat is left, so naming the
                  -- postcombat main phase runs the rest of the combat phase.
                  afterCombat = S.runToStep Phase.PostcombatMain (plan OptionalDecision.Exercises piker) atBlock
              Spec.assertEqWith s "stored while the ability has resolved" (length (GameState.blockRequirements atBlock)) 1
              Spec.assertEqWith s "and gone once the phase ends" (GameState.blockRequirements afterCombat) []
            _ -> Spec.assertFailure s "fixture should give bob a Piker"
        -- CR 702.39b's instances, asserted of the MINT as mentor's are, and with
        -- them the slot the gameplay cases can only see through its effects: CR
        -- 508.3a's condition, and rule 702.39a's one printed narrowing.
        Spec.it s "CR 702.39b two instances mint two targeting abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Provoke 2
              expectedSlot = TargetSlot.required Pool.Creatures (Just Filter.Type.ControlledByDefendingPlayer)
              slotsOf ability = concatMap (Map.elems . Mode.targetSlots) (Modal.modes (TriggeredAbility.modal ability))
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each on CR 508.3a's condition"
            (fmap TriggeredAbility.condition abilities)
            [TriggerCondition.SelfAttacks TriggerFrequency.EveryTime, TriggerCondition.SelfAttacks TriggerFrequency.EveryTime]
          Spec.assertEqWith s "each with rule 702.39a's one slot" (concatMap slotsOf abilities) [expectedSlot, expectedSlot]

-- CR 702.112a's renown, which rule 702 states as a triggered
-- ability -- and the first minted one carrying an intervening "if" (CR 603.4),
-- which is the whole of why it fires once and not once per connection.
--
-- Rhox Maulers {4}{G} Creature -- Rhino Soldier 4/4 is the card: trample and
-- renown 2. The 2 is what separates "N counters" from "a counter"; the trample is
-- why the blocked case needs a blocker that absorbs all four damage (Apprentice
-- Sharpshooter, 1/4), since a smaller one would let renown's own event through.
--
-- Valeron Wardens {2}{G} Creature -- Human Monk 1/3 is the second card, and the
-- only printing that WATCHES the designation: renown 2 plus "whenever a creature
-- you control becomes renowned, draw a card" (CR 702.112b's marker read by
-- something other than renown itself).
--
-- CR 702.112b's "until it leaves the battlefield" is read on Object.newIncarnation
-- directly, below. Pawl.SetupSpec's "no per-incarnation state survives" case does
-- NOT cover it -- that case asks whether the forgetting is idempotent, which is
-- blind to a field it never touches.
-- CR 702.100: evolve, whose rule text IS a triggered
-- ability, and the first whose intervening "if" is about the EVENT's object
-- rather than its bearer -- so this is the group that runs a Condition reading
-- another object through Quantity.AgainstSlot at Binding.became, and the first
-- disjunction (Condition.Any) in the pool.
--
-- Cloudfin Raptor {U} Creature -- Bird Mutant 0/1 is the card: flying and evolve,
-- so every counter below is the keyword's. Its 0/1 body is what makes the two
-- halves of rule 702.100a's "and/or" separable at all, and each entrant is chosen
-- to satisfy exactly one of them:
--
--   * Goblin Piker 2/1 -- power only (2 > 0, and 1 is not > 1).
--   * Llanowar Augur 0/3 -- toughness only (0 is not > 0, and 3 > 1).
--   * Birds of Paradise 0/1 -- neither, which is what makes "greater" strict.
--
-- A test whose entrant beat the Raptor on both axes would pass whichever half
-- were implemented, and would not be a test of the disjunction at all.
--
-- The last two cases are CR 608.2a's re-check read against CR 608.2h, and they
-- are a pair: the same Piker leaves the battlefield before the trigger resolves
-- either way, and the only difference is the numbers the record filed for it --
-- its own 2/1 when damage killed it, 0/0 when a shrink did.
evolveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
evolveSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- CR 122.1: what is on the permanent, which a +1/+1 EFFECT would leave
      -- empty while reading the same size.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- A Raptor under alice, then one creature entering under `pid` with CR
      -- 603.6a's event, so the scan has something to match.
      board raptor printing pid =
        let (raptorId, gs1) = S.addCreature raptor S.alice (Setup.emptyGame S.bothPlayers)
            (enteringId, gs2) = S.entersWithTrigger printing pid gs1
         in (raptorId, enteringId, gs2)
      evolvesAgainst raptor printing = do
        entrant <- S.printingOf s registry printing
        let (raptorId, _, gs) = board raptor entrant S.alice
        pure (raptorId, resolveAll (settle gs))
   in Spec.describe s "Evolve" $ do
        -- The proving test, and rule 702.100a's POWER half alone: the Piker's 1
        -- toughness does not beat the Raptor's 1, so an implementation that read
        -- only toughness leaves this board untouched.
        Spec.it s "CR 702.100a whole card: a 2/1 entering beats the Raptor's power and evolves it" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          (raptorId, after) <- evolvesAgainst raptor "Goblin Piker"
          Spec.assertEqWith s "one counter" (countersOn raptorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so it is a 1/2" (S.powerToughnessOf raptorId after) (Just (1, 2))
        -- Rule 702.100a's TOUGHNESS half alone, and the mirror of the case above:
        -- the Augur's 0 power does not beat the Raptor's 0, so an implementation
        -- that read only power leaves this board untouched.
        Spec.it s "CR 702.100a a 0/3 entering beats only the toughness, and evolves it all the same" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          (raptorId, after) <- evolvesAgainst raptor "Llanowar Augur"
          Spec.assertEqWith s "one counter" (countersOn raptorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so it is a 1/2" (S.powerToughnessOf raptorId after) (Just (1, 2))
        -- "GREATER" is strict on both axes: a 0/1 entering ties the Raptor twice
        -- and evolves nothing. The falsifier for a comparison written as "at
        -- least".
        Spec.it s "CR 702.100a a 0/1 entering ties both axes and evolves nothing" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          (raptorId, after) <- evolvesAgainst raptor "Birds of Paradise"
          Spec.assertEqWith s "no counters" (countersOn raptorId after) Map.empty
          Spec.assertEqWith s "it is its printed 0/1" (S.powerToughnessOf raptorId after) (Just (0, 1))
        -- "A creature YOU CONTROL": the same Piker that evolves the Raptor from
        -- alice's side does nothing from bob's, and nothing reaches the stack --
        -- CR 603.4 says an ability whose "if" is false does not trigger, but here
        -- it is the CONDITION that rejects the event.
        Spec.it s "CR 702.100a an opponent's creature entering is not a trigger at all" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, _, gs) = board raptor piker S.bob
              settled = settle gs
          Spec.assertEqWith s "nothing on the stack" (GameState.stack settled) []
          Spec.assertEqWith s "and no counters" (countersOn raptorId (resolveAll settled)) Map.empty
        -- CR 608.2a, the case that makes rule 702.100a's "if" an intervening one
        -- rather than part of the event: the trigger is on the stack legitimately,
        -- and a pump on the BEARER in response makes it resolve doing nothing. The
        -- proving test above is the control -- same board, same Piker.
        Spec.it s "CR 608.2a pumping the Raptor in response takes the counter away" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, _, gs) = board raptor piker S.alice
              onStack = settle gs
              responded = S.withEffect raptorId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 2))) onStack
              after = resolveAll responded
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "the Raptor is a 2/3, which the Piker beats on neither axis" (S.powerToughnessOf raptorId responded) (Just (2, 3))
          Spec.assertEqWith s "so no counter on resolution" (countersOn raptorId after) Map.empty
        -- CR 608.2h, the other half of that re-check: the ENTRANT is killed while
        -- the trigger waits, and rule 702.100a's rulings say the comparison is
        -- made against the power and toughness it last had on the battlefield --
        -- not against an object with no characteristics. Only the RESOLUTION check
        -- can observe this: at gather time the entrant has just entered and is
        -- still there by construction, so the read this pins is Stack's alone.
        --
        -- LETHAL DAMAGE rather than a shrink is what kills it, and that is the
        -- whole design of the board: a shrink would change the very numbers under
        -- test, where damage leaves them alone (CR 704.5g destroys the Piker at
        -- the 2/1 the record files). So last known information answers TRUE here
        -- and a blank object answers False, which is what makes the two readings
        -- distinguishable at all.
        Spec.it s "CR 608.2h a Piker killed in response evolves the Raptor from its last known 2/1" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, pikerId, gs) = board raptor piker S.alice
              onStack = settle gs
              dead = settle (S.markDamage pikerId 1 onStack)
              after = resolveAll dead
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "and the Piker is gone before it resolves" (S.onBattlefield pikerId dead) False
          Spec.assertEqWith s "the counter goes on all the same" (countersOn raptorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so it is a 1/2" (S.powerToughnessOf raptorId after) (Just (1, 2))
        -- The case above's paired negative -- same Raptor, same Piker, same
        -- departure before resolution, and the single difference is HOW it left:
        -- a -2/-1 kills it at 0/0 (CR 704.5f) instead, and 0/0 beats the Raptor's
        -- 0/1 on neither axis. So the numbers the record filed are what the
        -- re-check reads, rather than the entrant's PRINTED 2/1 -- which would put
        -- the counter on -- and rather than a departed entrant being waved through
        -- unexamined.
        Spec.it s "CR 608.2h an entrant shrunk to 0/0 as it died evolves nothing" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, pikerId, gs) = board raptor piker S.alice
              onStack = settle gs
              shrunk = S.withEffect pikerId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal (-2)) (Quantity.Type.Literal (-1)))) onStack
              dead = settle shrunk
              after = resolveAll dead
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "the Piker left as a 0/0" (S.powerToughnessOf pikerId shrunk) (Just (0, 0))
          Spec.assertEqWith s "and is gone before the trigger resolves" (S.onBattlefield pikerId dead) False
          Spec.assertEqWith s "no counters" (countersOn raptorId after) Map.empty
          Spec.assertEqWith s "the Raptor is its printed 0/1" (S.powerToughnessOf raptorId after) (Just (0, 1))
        -- CR 702.100d: each instance triggers separately, asserted of the MINT as
        -- prowess' and training's are, no printing carrying evolve twice.
        Spec.it s "CR 702.100d two instances mint two abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Evolve 2
              expected =
                TriggerCondition.PermanentEnters
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each watching CR 603.6a's entry"
            (fmap TriggeredAbility.condition abilities)
            [expected, expected]

-- CR 702.100b: a creature "evolves" when one or more +1/+1 counters are put on it
-- as a result of its evolve ability RESOLVING -- the marker rule 702.100b makes
-- other abilities able to identify. Renegade Krasis {1}{G}{G} 3/2 is the card:
-- evolve, plus "whenever this creature evolves, put a +1/+1 counter on each other
-- creature you control with a +1/+1 counter on it".
--
-- Four permanents, each pinning one conjunct of that sentence:
--
--   * the Krasis itself -- "each OTHER", so its own count must stay at the one
--     its evolve put there.
--   * alice's Goblin Piker and Hill Giant, each seeded with a counter -- two
--     recipients, so "EACH other creature" is more than one object.
--   * alice's Birds of Paradise, with none -- "with a +1/+1 counter on it".
--   * bob's Piker, seeded with one -- "you control".
--
-- The ENTRANT is Llanowar Augur 0/3: it beats the Krasis' 2 toughness and nothing
-- else, so the Krasis evolves. Goblin Piker 2/1 is the entrant that does not,
-- which the self-scope case below turns on.
krasisSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
krasisSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      seeded printing pid gs =
        let (oid, g1) = S.addCreature printing pid gs
         in (oid, S.addCounter CounterKind.PlusOnePlusOne 1 oid g1)
      boardOn base = do
        krasisPrinting <- S.printingOf s registry "Renegade Krasis"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        birdsPrinting <- S.printingOf s registry "Birds of Paradise"
        giantPrinting <- S.printingOf s registry "Hill Giant"
        let (krasis, g1) = S.addCreature krasisPrinting S.alice base
            (mine, g2) = seeded pikerPrinting S.alice g1
            (giant, g3) = seeded giantPrinting S.alice g2
            (birds, g4) = S.addCreature birdsPrinting S.alice g3
            (theirs, g5) = seeded pikerPrinting S.bob g4
        pure (krasis, mine, giant, birds, theirs, g5)
      board = boardOn (Setup.emptyGame S.bothPlayers)
   in Spec.describe s "Renegade Krasis" $ do
        -- The proving test.
        Spec.it s "CR 702.100b whole card: the Krasis evolves and pays out its other counter-bearers" $ do
          augur <- S.printingOf s registry "Llanowar Augur"
          (krasis, mine, giant, birds, theirs, gs) <- board
          let (_, entered) = S.entersWithTrigger augur S.alice gs
              after = resolveAll (settle entered)
          Spec.assertEqWith s "the Krasis keeps only its evolve counter" (plusOnes krasis after) 1
          Spec.assertEqWith s "alice's Piker gains a second" (plusOnes mine after) 2
          Spec.assertEqWith s "and so does her Giant -- EACH other creature" (plusOnes giant after) 2
          Spec.assertEqWith s "the counterless Bird gains none" (plusOnes birds after) 0
          Spec.assertEqWith s "bob's counter-bearer gains none" (plusOnes theirs after) 1
        -- Self-scoped, not filtered: a Cloudfin Raptor evolving beside the Krasis
        -- is another creature alice controls evolving, and the Krasis' ability
        -- says "this creature". The Piker 2/1 beats the Raptor's 0/1 power and
        -- neither of the Krasis' numbers, so exactly one of the two evolves.
        Spec.it s "CR 702.100b another creature evolving is not this creature evolving" $ do
          raptorPrinting <- S.printingOf s registry "Cloudfin Raptor"
          pikerPrinting <- S.printingOf s registry "Goblin Piker"
          (krasis, mine, _, _, _, gs) <- board
          let (raptor, withRaptor) = S.addCreature raptorPrinting S.alice gs
              (_, entered) = S.entersWithTrigger pikerPrinting S.alice withRaptor
              after = resolveAll (settle entered)
          Spec.assertEqWith s "the Raptor did evolve" (plusOnes raptor after) 1
          Spec.assertEqWith s "the Krasis did not" (plusOnes krasis after) 0
          Spec.assertEqWith s "so its trigger paid out nothing" (plusOnes mine after) 1
        -- CR 702.100b's "as a result of its evolve ability resolving": the same
        -- counter, on the same permanent, from Battlegrowth instead, is not an
        -- evolution. The falsifier for a condition written against
        -- GameEvent.CountersPut.
        Spec.it s "CR 702.100b a +1/+1 counter from anything else is not an evolution" $ do
          forest <- S.printingOf s registry "Forest"
          battlegrowth <- S.printingOf s registry "Battlegrowth"
          (krasis, mine, _, _, _, gs) <- boardOn (S.landsInPlay forest 1)
          let (handed, spellId) = S.handOne battlegrowth gs
              cast = snd (Engine.runGamePure (aimedCast spellId krasis) handed (S.cast S.alice spellId))
              after = resolveAll (settle cast)
          Spec.assertEqWith s "the Krasis took the counter" (plusOnes krasis after) 1
          Spec.assertEqWith s "and nothing evolved, so nothing was paid out" (plusOnes mine after) 1

renownSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
renownSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures, trainingSpec's
      -- plan: S.aggressiveAnswer attacks with everything, so a case about who
      -- attacks in which phase has to say so.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      -- CR 122.1: what is actually on the permanent, which a +2/+2 EFFECT would
      -- leave empty while reading the same 6/6.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- CR 702.112b's designation itself, which no characteristic reports.
      renownedness oid gs = fmap (Set.member Designation.Renowned . Object.designations) (Game.lookupObject oid gs)
      -- CR 104.3c: a draw case needs a library to draw from, and more of one than
      -- it draws, so an extra draw is visible rather than fatal.
      stock printing n pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      -- CR 509.1: no blocks. S.aggressiveAnswer blocks with everything, which
      -- would put the defender's own watcher in front of an attacker.
      noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
   in Spec.describe s "Renown" $ do
        -- The proving test. CR 702.112a: two counters on the BEARER, and the
        -- designation with them. The counter assertion is what separates rule
        -- 702.112a's placement from a pump, and the 2 what separates N from 1.
        Spec.it s "CR 702.112a whole card: Rhox Maulers connects and takes two +1/+1 counters" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] []
          case mine of
            [maulers] -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob took the printed four" (S.lifeOf S.bob after) (Just 16)
              Spec.assertEqWith s "two counters, not one" (countersOn maulers after) (Map.singleton CounterKind.PlusOnePlusOne 2)
              Spec.assertEqWith s "so it is a 6/6" (S.powerToughnessOf maulers after) (Just (6, 6))
              Spec.assertEqWith s "and it is renowned" (renownedness maulers after) (Just True)
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 702.112a is scoped to combat damage dealt TO A PLAYER. The 1/4
        -- absorbs all four (CR 702.19b leaves nothing to trample over), so the
        -- event never happens and neither half of the ability runs.
        Spec.it s "CR 702.112a a fully blocked Maulers is renowned by nobody" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] ["Apprentice Sharpshooter"]
          case mine of
            [maulers] -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob lost no life" (S.lifeOf S.bob after) (Just 20)
              Spec.assertEqWith s "no counters" (countersOn maulers after) Map.empty
              Spec.assertEqWith s "and no designation" (renownedness maulers after) (Just False)
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 603.4's intervening "if", at the board level: a second connection in
        -- the same turn finds the creature already renowned, so nothing is added.
        -- Aurelia, the Warleader is the pool's extra combat phase, and she untaps
        -- the Maulers to attack again. The life drop is the discriminator -- it
        -- proves the second combat really connected, so a green assertion cannot
        -- mean the phase never ran.
        Spec.it s "CR 702.112a a second connection adds nothing, the creature being renowned" $ do
          (gs, mine, _) <- board ["Rhox Maulers", "Aurelia, the Warleader"] []
          case mine of
            [maulers, aurelia] -> do
              let first = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers, aurelia]) gs
                  second = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (plan [maulers]) first
                  after = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers]) second
              Spec.assertEqWith s "the first combat renowned it" (countersOn maulers first) (Map.singleton CounterKind.PlusOnePlusOne 2)
              Spec.assertEqWith s "the second phase really ran a declaration" (GameState.phase second) (Phase.Combat CombatStep.DeclareAttackers)
              Spec.assertEqWith s "and the second really connected, for six" (S.lifeOf S.bob after) (Just 7)
              Spec.assertEqWith s "but added no third counter" (countersOn maulers after) (Map.singleton CounterKind.PlusOnePlusOne 2)
            _ -> Spec.assertFailure s "fixture should give alice a Maulers and Aurelia"
        -- What separates renown from a damage rider: it is a TRIGGERED ability, so
        -- the counters arrive when it resolves, not as the damage is dealt.
        -- S.fightWith deals combat damage without reaching a priority boundary,
        -- so nothing has been gathered yet -- poisonous' case, read on an object.
        Spec.it s "CR 702.112a the counters ride the stack, not the damage" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] []
          case mine of
            [maulers] -> do
              let fought = S.fightWith S.aggressiveAnswer gs
              Spec.assertEqWith s "damage is dealt" (S.lifeOf S.bob fought) (Just 16)
              Spec.assertEqWith s "but no counters until the trigger resolves" (countersOn maulers fought) Map.empty
              Spec.assertEqWith s "and no designation either" (renownedness maulers fought) (Just False)
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 702.112b's "it stays renowned UNTIL IT LEAVES THE BATTLEFIELD": the
        -- designation is per-incarnation state, so CR 400.7's forgetting is what
        -- ends it and a Maulers that dies and returns must connect again.
        --
        -- Asserted on Object.newIncarnation directly, as Pawl.RoomSpec's unlocked
        -- designations are: nothing writes this field on an entry, so a bounce
        -- would read the same forgetting through more machinery. Pawl.SetupSpec's
        -- CR 400.7 case does NOT cover it -- `forgotten` asks whether the
        -- forgetting is idempotent, which is blind to a field it never touches.
        Spec.it s "CR 702.112b the designation does not survive CR 400.7" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] []
          case mine of
            [maulers] -> case Game.lookupObject maulers (S.runCombat S.aggressiveAnswer gs) of
              Nothing -> Spec.assertFailure s "expected to find the Maulers"
              Just obj -> do
                Spec.assertEqWith s "the control: this incarnation is renowned" (Set.member Designation.Renowned (Object.designations obj)) True
                Spec.assertEqWith s "the next one is not" (Set.member Designation.Renowned (Object.designations (Object.newIncarnation obj))) False
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 702.112b's designation read by a WATCHER, which is what the rule
        -- calls it a marker FOR: Valeron Wardens {2}{G} Creature -- Human Monk
        -- 1/3, renown 2 and "whenever a creature you control becomes renowned,
        -- draw a card". Both attackers connect, so the Wardens' trigger fires
        -- TWICE -- once for the Maulers and once for itself, which is what "a
        -- creature you control" says and a self-scoped reading would not.
        --
        -- The library is stocked past the two draws, so a third draw would show as
        -- an extra card rather than as CR 104.3c losing alice the game before the
        -- assertions run.
        Spec.it s "CR 702.112b a watcher draws once per creature that becomes renowned" $ do
          (gs, mine, _) <- board ["Valeron Wardens", "Rhox Maulers"] []
          piker <- S.printingOf s registry "Goblin Piker"
          case mine of
            [wardens, maulers] -> do
              let after = S.runCombat S.aggressiveAnswer (stock piker 3 S.alice gs)
              Spec.assertEqWith s "both connected, for five" (S.lifeOf S.bob after) (Just 15)
              Spec.assertEqWith s "the Wardens is renowned" (renownedness wardens after) (Just True)
              Spec.assertEqWith s "and so is the Maulers" (renownedness maulers after) (Just True)
              Spec.assertEqWith s "so two cards were drawn, not one" (length (Game.zoneMembers Zone.Hand S.alice after)) 2
              Spec.assertEqWith s "leaving one in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
            _ -> Spec.assertFailure s "fixture should give alice a Wardens and a Maulers"
        -- What the condition is NOT: combat damage. Goblin Piker connects for two
        -- and has no renown, so it never becomes renowned and contributes no draw
        -- -- the one card is the Wardens' own designation.
        Spec.it s "CR 702.112b a creature that connects without renown draws nothing" $ do
          (gs, mine, _) <- board ["Valeron Wardens", "Goblin Piker"] []
          piker <- S.printingOf s registry "Goblin Piker"
          case mine of
            [wardens, goblin] -> do
              let after = S.runCombat S.aggressiveAnswer (stock piker 3 S.alice gs)
              Spec.assertEqWith s "both connected, for three" (S.lifeOf S.bob after) (Just 17)
              Spec.assertEqWith s "the Wardens is renowned" (renownedness wardens after) (Just True)
              Spec.assertEqWith s "the Piker is not" (renownedness goblin after) (Just False)
              Spec.assertEqWith s "so exactly one card was drawn" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
            _ -> Spec.assertFailure s "fixture should give alice a Wardens and a Piker"
        -- CR 109.5's "you control", and with it WHICH permanent the Filter reads:
        -- bob has a Valeron Wardens of his own, watching from the defending side.
        -- Nothing he controls becomes renowned, so he draws nothing -- an arm that
        -- read the BEARER instead of the event's subject would have his Wardens
        -- match itself and draw twice.
        Spec.it s "CR 702.112b the defender's own Wardens sees no creature of his become renowned" $ do
          (gs, mine, theirs) <- board ["Valeron Wardens", "Rhox Maulers"] ["Valeron Wardens"]
          piker <- S.printingOf s registry "Goblin Piker"
          case (mine, theirs) of
            ([wardens, maulers], [hisWardens]) -> do
              let after = S.runCombat noBlocks (stock piker 3 S.bob (stock piker 3 S.alice gs))
              Spec.assertEqWith s "both of alice's connected, for five" (S.lifeOf S.bob after) (Just 15)
              Spec.assertEqWith s "hers are renowned" (fmap (`renownedness` after) [wardens, maulers]) [Just True, Just True]
              Spec.assertEqWith s "his is not" (renownedness hisWardens after) (Just False)
              Spec.assertEqWith s "she drew two" (length (Game.zoneMembers Zone.Hand S.alice after)) 2
              Spec.assertEqWith s "and he drew none" (length (Game.zoneMembers Zone.Hand S.bob after)) 0
            _ -> Spec.assertFailure s "fixture should give alice a Wardens and a Maulers, bob a Wardens"
        -- CR 702.112c: "if a creature has multiple instances of renown, each
        -- triggers separately". Asserted of the MINT, as poisonous' multiplicity
        -- is, no card in the pool printing renown twice. What rule 702.112c says
        -- happens NEXT -- the second resolving to nothing -- is the intervening
        -- "if" the gameplay cases above read.
        Spec.it s "CR 702.112c each instance of renown is its own ability" $ do
          Spec.assertEqWith s "renown 2 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Renown 2) 2)) [Keyword.renown 2, Keyword.renown 2]
          Spec.assertEqWith s "and renown 6 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Renown 6) 1)) [Keyword.renown 6]

-- CR 701.37b's designation watched from outside the monstrosity action that sets
-- it: "monstrous is a designation ... that the monstrosity action and OTHER SPELLS
-- AND ABILITIES can identify", read through
-- TriggerCondition.PermanentBecomesDesignated -- the same condition Valeron
-- Wardens uses for renowned, with the other designation as its payload.
--
-- Arbor Colossus {2}{G}{G}{G} Creature -- Giant 6/6, "Reach. {3}{G}{G}{G}:
-- Monstrosity 3. When this creature becomes monstrous, destroy target creature
-- with flying an opponent controls."
--
-- bob holds Bird Maiden 1/2 flying and Goblin Piker 2/1: the Piker is the
-- falsifier for a target slot that dropped "with flying", and both are his, so no
-- assertion here turns on the seat.
--
-- TWELVE Forests, not six: the second-monstrosity case has to be able to PAY for
-- its activation, or it would prove nothing but an unpayable cost.
--
-- The DESIGNATION is what the last case turns on. Valeron Wardens watches the same
-- condition with Renowned, so a matcher that compared only the event's SHAPE would
-- draw alice a card when her Colossus became monstrous.
arborColossusSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
arborColossusSpec s registry =
  let monstrousness oid gs = fmap (Set.member Designation.Monstrous . Object.designations) (Game.lookupObject oid gs)
      plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      -- The trigger TARGETS (CR 603.3d), so the answerer has to aim it; `victim`
      -- pins the choice rather than searching for a legal one, which is what lets
      -- the Piker case below fail rather than repair itself.
      aimed :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimed victim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just victim) . Recipient.objectOf) legal) sets
        _ -> S.identityAnswer p
      -- One activation of the one monstrosity ability, its trigger settled onto
      -- the stack and resolved, with `victim` aimed at.
      monstrosity colossus victim gs = case Activate.abilitiesFor colossus gs of
        [ability]
          -- CR 701.37a's condition is the CLAUSE's, not an activation
          -- restriction, so a monstrous permanent's ability stays activatable --
          -- which is what makes the second case below a real activation rather
          -- than an unpaid one.
          | Activate.activatable S.alice colossus ability gs ->
              Right . snd . Engine.runGamePure (aimed victim) gs $ do
                Activate.activateAbility S.alice colossus ability
                Stack.resolveTop
                Engine.settleForPriority
                Engine.priorityLoop
        [_] -> Left 0
        other -> Left (length other)
      board extra = do
        colossusPrinting <- S.printingOf s registry "Arbor Colossus"
        forest <- S.printingOf s registry "Forest"
        maidenPrinting <- S.printingOf s registry "Bird Maiden"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        base <- extra (S.landsInPlay forest 12)
        let (colossus, g1) = S.addCreature colossusPrinting S.alice base
            (maiden, g2) = S.addCreature maidenPrinting S.bob g1
            (piker, g3) = S.addCreature pikerPrinting S.bob g2
        pure (colossus, maiden, piker, g3)
   in Spec.describe s "Arbor Colossus" $ do
        -- The proving test. CR 701.37a's counters and designation, and then rule
        -- 701.37b's marker read by an ability of the same permanent: the flier dies.
        Spec.it s "CR 701.37b whole card: monstrosity 3 marks the Colossus and its trigger destroys the flier" $ do
          (colossus, maiden, piker, gs) <- board pure
          case monstrosity colossus maiden gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "not monstrous to begin with" (monstrousness colossus gs) (Just False)
              Spec.assertEqWith s "three counters, not one" (plusOnes colossus after) 3
              Spec.assertEqWith s "so it is a 9/9" (S.powerToughnessOf colossus after) (Just (9, 9))
              Spec.assertEqWith s "and it is monstrous" (monstrousness colossus after) (Just True)
              Spec.assertBool s (not (S.onBattlefield maiden after)) "the targeted flier was destroyed"
              Spec.assertBool s (S.onBattlefield piker after) "and the ground creature was not"
        -- CR 701.37a's "if this permanent isn't monstrous": the SECOND activation
        -- on the same board does nothing, so the trigger never fires and bob's
        -- second flier lives. The same mana, the same seats, the same creatures --
        -- the one difference is that the Colossus is already monstrous.
        Spec.it s "CR 701.37a a second monstrosity marks nothing, so nothing triggers" $ do
          (colossus, maiden, _, gs) <- board pure
          maidenPrinting <- S.printingOf s registry "Bird Maiden"
          case monstrosity colossus maiden gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right once -> do
              let (second, withSecond) = S.addCreature maidenPrinting S.bob once
              case monstrosity colossus second withSecond of
                Left n -> Spec.assertFailure s ("expected the monstrous Colossus to stay activatable, got " <> show n)
                Right twice -> do
                  Spec.assertEqWith s "still three counters, not six" (plusOnes colossus twice) 3
                  Spec.assertBool s (S.onBattlefield second twice) "the second flier survived, nothing having become monstrous"
        -- The designation is LOAD-BEARING in the CLAUSE CONDITION too, and one board
        -- can carry two designations at once: Rune-Brand Juggler {2}{B}{R} 3/3,
        -- "When this creature enters, suspect up to one target creature you control",
        -- aimed at the Colossus. CR 701.60b's mark is not CR 701.37b's, so CR
        -- 701.37a's "if this permanent isn't monstrous" still holds and monstrosity
        -- still does its whole job. A Quantity arm that read "has SOME designation"
        -- would fail the condition and put nothing on the Colossus at all.
        Spec.it s "CR 701.37a a suspected Colossus is still not monstrous" $ do
          jugglerPrinting <- S.printingOf s registry "Rune-Brand Juggler"
          (colossus, maiden, _, gs) <- board pure
          let (_, entering) = S.entersWithTrigger jugglerPrinting S.alice gs
              suspected = snd (Engine.runGamePure (aimed colossus) entering Engine.priorityLoop)
          Spec.assertBool s (Set.member Designation.Suspected (maybe Set.empty Object.designations (Game.lookupObject colossus suspected))) "the Juggler suspected the Colossus"
          Spec.assertEqWith s "which leaves it not monstrous" (monstrousness colossus suspected) (Just False)
          case monstrosity colossus maiden suspected of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "so monstrosity still places its three counters" (plusOnes colossus after) 3
              Spec.assertEqWith s "and still marks it monstrous" (monstrousness colossus after) (Just True)
              Spec.assertBool s (not (S.onBattlefield maiden after)) "and its trigger still fired"
        -- The designation is LOAD-BEARING in the match, not just the event's shape.
        -- Valeron Wardens {2}{G} 1/3 watches "whenever a creature you control
        -- becomes renowned" -- the same TriggerCondition constructor with Renowned
        -- in it -- and the Colossus becoming monstrous is not that. alice's library
        -- is stocked, so a spurious draw is visible rather than fatal (CR 104.3c).
        Spec.it s "CR 701.37b a creature becoming monstrous is not a creature becoming renowned" $ do
          wardensPrinting <- S.printingOf s registry "Valeron Wardens"
          piker <- S.printingOf s registry "Goblin Piker"
          (colossus, maiden, _, gs) <- board (pure . snd . S.addLibraryCard piker S.alice . snd . S.addCreature wardensPrinting S.alice)
          case monstrosity colossus maiden gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "the Colossus is monstrous" (monstrousness colossus after) (Just True)
              Spec.assertBool s (not (S.onBattlefield maiden after)) "so its own trigger did fire"
              Spec.assertEqWith s "and the Wardens drew nothing" (length (Game.zoneMembers Zone.Hand S.alice after)) 0

-- CR 702.63 vanishing, which rule 702 states as triggered
-- abilities -- and the first whose rule text spans BOTH mints, since rule
-- 702.63a's three abilities are one CR 614.1c entry replacement
-- (Keyword.mintedReplacementsFor, riot's position) and two triggers.
--
-- Waning Wurm {3}{B} Creature -- Zombie Wurm 7/6 is the card, and it is nothing
-- but the keyword: no second ability can put a counter on it, take one off, or
-- keep it alive, so every number below is vanishing's own.
--
-- Vanishing 2 rather than a larger printing (Calciderm's 4) because two is the
-- smallest N that tells the two triggers apart: the first upkeep must remove one
-- and NOT sacrifice, the second must do both.
vanishingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vanishingSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- One upkeep for `pid`, run to the end of the priority loop, so the
      -- trigger is gathered (CR 603.3) and resolved.
      upkeepOf pid gs =
        let began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
            settled = snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)
         in (settled, snd (Engine.runGamePure S.identityAnswer settled Engine.priorityLoop))
      after pid gs = snd (upkeepOf pid gs)
      times = S.counterOf CounterKind.Time
      -- The wurm CAST rather than placed, because rule 702.63a's first ability is
      -- a replacement on the entry -- S.addCreature builds the object directly and
      -- so reaches no CR 616.1 loop, which is what the counterless case below
      -- turns on.
      castWurm = do
        swamp <- S.printingOf s registry "Swamp"
        wurm <- S.printingOf s registry "Waning Wurm"
        let base = S.landsInPlay swamp 4
            (held, gs0) = S.addHandCard wurm S.alice base
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
        pure (wurmOn entered, entered)
      wurmOn gs =
        let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Waning Wurm"))
         in List.find named (Set.toList (GameState.battlefield gs))
   in Spec.describe s "Vanishing" $ do
        -- The proving test, and all three of rule 702.63a's abilities in one
        -- board: two counters on the entry, one removed at each of alice's
        -- upkeeps, and the sacrifice when the last one goes.
        Spec.it s "CR 702.63a whole card: the Wurm enters with two time counters and counts them down" $ do
          (found, entered) <- castWurm
          case found of
            Nothing -> Spec.assertFailure s "Waning Wurm did not reach the battlefield"
            Just wurm -> do
              Spec.assertEqWith s "two time counters on the entry" (times wurm entered) 2
              let first = after S.alice entered
              Spec.assertEqWith s "one after the first upkeep" (times wurm first) 1
              Spec.assertBool s (S.onBattlefield wurm first) "and it is still on the battlefield"
              let second = after S.alice first
              Spec.assertEqWith s "none after the second" (times wurm second) 0
              Spec.assertBool s (not (S.onBattlefield wurm second)) "so the last removal sacrificed it"
              -- CR 701.21a: a sacrifice is a move to the OWNER's graveyard, and
              -- not a destruction -- so this is the zone the wurm is in.
              Spec.assertEqWith s "in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice second)) 1
        -- Rule 702.63a says "YOUR upkeep", which is TurnScope.ControllersTurn: an
        -- opponent's upkeep is not this trigger, and an arm reading EachTurn would
        -- count the wurm down twice as fast.
        Spec.it s "CR 702.63a bob's upkeep removes nothing" $ do
          (found, entered) <- castWurm
          case found of
            Nothing -> Spec.assertFailure s "Waning Wurm did not reach the battlefield"
            Just wurm -> do
              let (settled, resolved) = upkeepOf S.bob entered
              Spec.assertEqWith s "nothing was even put on the stack" (GameState.stack settled) []
              Spec.assertEqWith s "so both counters are still there" (times wurm resolved) 2
              Spec.assertBool s (S.onBattlefield wurm resolved) "and the wurm is untouched"
        -- CR 603.4's intervening "if": rule 702.63a's second ability does not
        -- trigger AT ALL on an upkeep where the permanent has no time counter, so
        -- nothing reaches the stack. S.addCreature is what reaches this board --
        -- it places the wurm without running rule 702.63a's entry replacement, the
        -- position a card that lost its counters some other way would be in.
        --
        -- It also pins rule 702.63a's THIRD ability to the REMOVAL rather than to
        -- the count: a wurm sitting at zero is not sacrificed, because no last
        -- counter came off.
        Spec.it s "CR 603.4 a wurm with no time counters neither triggers nor is sacrificed" $ do
          wurm <- S.printingOf s registry "Waning Wurm"
          let (oid, gs) = S.addCreature wurm S.alice (Setup.emptyGame S.bothPlayers)
              (settled, resolved) = upkeepOf S.alice gs
          Spec.assertEqWith s "it really has none" (times oid gs) 0
          Spec.assertEqWith s "nothing on the stack" (GameState.stack settled) []
          Spec.assertBool s (S.onBattlefield oid resolved) "and it survives its own upkeep"
        -- CR 702.63c: "if a permanent has multiple instances of vanishing, each
        -- works separately". Asserted of BOTH mints, as renown's multiplicity is
        -- asserted of one, no card in the pool printing vanishing twice.
        --
        -- Spelled out rather than compared against Keyword.vanishing itself: an
        -- assertion written that way says only that two copies are two copies,
        -- and a mint that dropped one of the pair would repair it silently.
        Spec.it s "CR 702.63c each instance is its own three abilities" $ do
          let counted = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)
              emptied = TriggerCondition.SelfLastCounterRemoved CounterKind.Time
          Spec.assertEqWith
            s
            "vanishing 2 held twice mints four triggers, two of each kind"
            (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Vanishing 2) 2)))
            [counted, emptied, counted, emptied]
          Spec.assertEqWith
            s
            "and two entry rewrites of two time counters each, which is what makes them add up"
            (Keyword.mintedReplacementsFor (Keyword.Type.Vanishing 2) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.Time 2)))))

-- CR 702.32 fading, vanishing's neighbour and the reason the two are separate
-- keywords rather than one with a counter kind on it. Rule 702.32a states TWO
-- abilities where rule 702.63a states three, and it hangs the sacrifice on an
-- upkeep where no counter can come off rather than on the removal of the last
-- one -- so a fading N permanent sees N+1 of its controller's upkeeps and a
-- vanishing N permanent sees N.
--
-- That off-by-one is what the board below is built to read, and it is the whole
-- reason the second upkeep gets an assertion of its own: a fading 2 creature that
-- reached zero counters is still on the battlefield, which is exactly where a
-- vanishing 2 creature is not.
--
-- Skyshroud Ridgeback {G} Creature -- Beast 2/3 is the card, and it is nothing
-- but the keyword: no second ability can put a fade counter on it, take one off
-- or keep it alive, so every number below is fading's own. Fading 2 for
-- vanishing's reason -- two is the smallest N that puts a counted-down upkeep
-- between the entry and the sacrifice.
fadingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fadingSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- vanishingSpec's, and the same reasons: one upkeep for `pid`, run to the
      -- end of the priority loop so the trigger is gathered (CR 603.3) and
      -- resolved.
      upkeepOf pid gs =
        let began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
            settled = snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)
         in (settled, snd (Engine.runGamePure S.identityAnswer settled Engine.priorityLoop))
      after pid gs = snd (upkeepOf pid gs)
      fades = S.counterOf CounterKind.Fade
      -- CAST rather than placed, for vanishingSpec's reason: rule 702.32a's first
      -- ability is a replacement on the entry, and S.addCreature reaches no CR
      -- 616.1 loop.
      castRidgeback = do
        forest <- S.printingOf s registry "Forest"
        ridgeback <- S.printingOf s registry "Skyshroud Ridgeback"
        let base = S.landsInPlay forest 4
            (held, gs0) = S.addHandCard ridgeback S.alice base
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
        pure (ridgebackOn entered, entered)
      ridgebackOn gs =
        let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Skyshroud Ridgeback"))
         in List.find named (Set.toList (GameState.battlefield gs))
   in Spec.describe s "Fading" $ do
        -- The proving test, and both of rule 702.32a's abilities in one board.
        Spec.it s "CR 702.32a whole card: the Ridgeback enters with two fade counters and outlives them by an upkeep" $ do
          (found, entered) <- castRidgeback
          case found of
            Nothing -> Spec.assertFailure s "Skyshroud Ridgeback did not reach the battlefield"
            Just ridgeback -> do
              Spec.assertEqWith s "two fade counters on the entry" (fades ridgeback entered) 2
              let first = after S.alice entered
              Spec.assertEqWith s "one after the first upkeep" (fades ridgeback first) 1
              Spec.assertBool s (S.onBattlefield ridgeback first) "and it is still on the battlefield"
              let second = after S.alice first
              Spec.assertEqWith s "none after the second" (fades ridgeback second) 0
              -- Rule 702.32a rather than rule 702.63a: the removal that empties
              -- the pile sacrifices nothing, because the rule's "if you can't" is
              -- about a removal that did not happen.
              Spec.assertBool s (S.onBattlefield ridgeback second) "and STILL on it, which a vanishing 2 creature would not be"
              let third = after S.alice second
              Spec.assertBool s (not (S.onBattlefield ridgeback third)) "the third upkeep could remove none, so it was sacrificed"
              -- CR 701.21a: a sacrifice is a move to the OWNER's graveyard and not
              -- a destruction.
              Spec.assertEqWith s "in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice third)) 1
              Spec.assertEqWith s "and the pile it was counting is still empty" (fades ridgeback third) 0
        -- Rule 702.32a says "YOUR upkeep", which is TurnScope.ControllersTurn: an
        -- arm reading EachTurn would count the Ridgeback down twice as fast.
        Spec.it s "CR 702.32a bob's upkeep removes nothing" $ do
          (found, entered) <- castRidgeback
          case found of
            Nothing -> Spec.assertFailure s "Skyshroud Ridgeback did not reach the battlefield"
            Just ridgeback -> do
              let (settled, resolved) = upkeepOf S.bob entered
              Spec.assertEqWith s "nothing was even put on the stack" (GameState.stack settled) []
              Spec.assertEqWith s "so both counters are still there" (fades ridgeback resolved) 2
              Spec.assertBool s (S.onBattlefield ridgeback resolved) "and the Ridgeback is untouched"
        -- Rule 702.32a states NO intervening "if", which is the other half of the
        -- difference from rule 702.63a: the ability triggers on an upkeep where
        -- the pile is already empty, and that firing IS the sacrifice.
        -- S.addCreature is what reaches this board -- it places the Ridgeback
        -- without running the entry replacement, the position a card that lost its
        -- counters some other way would be in.
        Spec.it s "CR 702.32a a Ridgeback with no fade counters triggers and is sacrificed at once" $ do
          ridgeback <- S.printingOf s registry "Skyshroud Ridgeback"
          let (oid, gs) = S.addCreature ridgeback S.alice (Setup.emptyGame S.bothPlayers)
              (settled, resolved) = upkeepOf S.alice gs
          Spec.assertEqWith s "it really has none" (fades oid gs) 0
          Spec.assertEqWith s "and the ability still reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (not (S.onBattlefield oid resolved)) "so its own first upkeep took it"
        -- The mint, spelled out for vanishingSpec's reason: an assertion written
        -- against Keyword.fading itself would say only that one copy is one copy.
        -- Rule 702.32 states no multiplicity clause, so each instance is its own
        -- pair.
        Spec.it s "CR 702.32a each instance is its own two abilities" $ do
          Spec.assertEqWith
            s
            "fading 2 held twice mints two upkeep triggers"
            (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Fading 2) 2)))
            (replicate 2 (TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)))
          Spec.assertEqWith
            s
            "and two entry rewrites of two FADE counters each, never rule 702.63a's time counters"
            (Keyword.mintedReplacementsFor (Keyword.Type.Fading 2) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.Fade 2)))))

-- CR 702.43 modular, whose rule text also spans BOTH of
-- Pawl.Engine.Keyword's mints -- one CR 614.1c entry replacement and one death
-- trigger. What is new is the trigger's PAYLOAD: rule 702.43a
-- counts "each +1/+1 counter on this permanent" at a moment when the permanent
-- is in a graveyard, so the number comes from CR 608.2h last known information.
-- Pawl.ZoneTriggerSpec's counterLookBackSpec proves the same record answering
-- an intervening "if"; this is the first read of it at RESOLUTION.
--
-- Two printings, so no number below can be read two ways:
--
--   * Arcbound Hybrid {4} Artifact Creature -- Beast 0/0, haste and modular 2.
--   * Arcbound Worker {1} Artifact Creature -- Construct 0/0, modular 1.
--
-- The dying Hybrid is SEEDED to three counters against its printed modular 2,
-- which is the discriminator that matters: an implementation reading the
-- keyword's N instead of the counters on the permanent moves 2, and one reading a
-- literal moves 1. Only counting the pile moves 3.
--
-- Murder does the killing, counterLookBackSpec's reason: a 0/0 body plus counters
-- makes lethal damage a different number per leg, and CR 701.8a's destroy does
-- not care.
modularSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
modularSpec s registry =
  let plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      -- Rule 702.43a's "you may", exercised. S.identityAnswer declines it, which
      -- is what the declining leg below rides.
      exercising :: Prompt.Prompt r -> r
      exercising p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- A Hybrid seeded with three +1/+1 counters and one companion creature,
      -- with a Murder in hand. The Hybrid is added FIRST so it holds the lesser
      -- ObjectId: Murder's pool is Pool.Creatures and identityAnswer takes the
      -- least recipient, so this is what aims the removal at it rather than at
      -- the companion.
      board companion = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        hybrid <- S.printingOf s registry "Arcbound Hybrid"
        other <- S.printingOf s registry companion
        let lands = S.landsInPlay swamp 3
            (hybridId, g1) = S.addCreature hybrid S.alice lands
            g2 = S.addCounter CounterKind.PlusOnePlusOne 3 hybridId g1
            (otherId, g3) = S.addCreature other S.alice g2
            -- The companion carries a counter of its own, so a payload that
            -- overwrote rather than added would be visible, and so that a 0/0
            -- Worker survives CR 704.5f.
            g4 = S.addCounter CounterKind.PlusOnePlusOne 1 otherId g3
            -- CR 104.3c: nothing here draws, but a stocked library keeps a leg
            -- from ending on an empty one.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g4 [1 .. 5 :: Int]
        pure (hybridId, otherId, S.handOne murder stocked)
      -- Cast the Murder, resolve it (the Hybrid dies), settle so the death
      -- trigger is gathered (CR 603.3), then resolve the trigger.
      murderIt :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      murderIt answer (gs, spellId) =
        let cast = S.runPure answer gs (S.cast S.alice spellId)
            destroyed = S.runPure answer cast Stack.resolveTop
            settled = S.runPure answer destroyed Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      -- A printing CAST rather than placed, because rule 702.43a's first ability
      -- is a replacement on the ENTRY -- S.addCreature reaches no CR 616.1 loop.
      castOne name lands = do
        swamp <- S.printingOf s registry "Swamp"
        printing <- S.printingOf s registry name
        let (held, gs0) = S.addHandCard printing S.alice (S.landsInPlay swamp lands)
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
            named oid = fmap Face.name (Game.faceOf oid entered) == Just (CardName.MkCardName (Text.pack name))
        pure (List.find named (Set.toList (GameState.battlefield entered)), entered)
   in Spec.describe s "Modular" $ do
        -- Rule 702.43a's FIRST ability, at both printed values: the N is the
        -- card's and not the rule's, so one leg alone could not tell a mint that
        -- always placed one counter from a correct one.
        Spec.it s "CR 702.43a the entry places the printed N of +1/+1 counters" $ do
          (foundWorker, workerBoard) <- castOne "Arcbound Worker" 1
          case foundWorker of
            Nothing -> Spec.assertFailure s "Arcbound Worker did not reach the battlefield"
            Just worker -> do
              Spec.assertEqWith s "modular 1 enters with one counter" (plusOnes worker workerBoard) 1
              -- CR 122.1a at layer 7c, which is also why a printed 0/0 survives
              -- CR 704.5f at all.
              Spec.assertEqWith s "so the printed 0/0 is a 1/1" (S.powerToughnessOf worker workerBoard) (Just (1, 1))
          (foundHybrid, hybridBoard) <- castOne "Arcbound Hybrid" 4
          case foundHybrid of
            Nothing -> Spec.assertFailure s "Arcbound Hybrid did not reach the battlefield"
            Just hybrid -> do
              Spec.assertEqWith s "modular 2 enters with two" (plusOnes hybrid hybridBoard) 2
              Spec.assertEqWith s "a 2/2" (S.powerToughnessOf hybrid hybridBoard) (Just (2, 2))
        -- The proving test. Rule 702.43a's SECOND ability, counting the pile the
        -- dead permanent had rather than its printed N.
        Spec.it s "CR 702.43a whole card: the dead Hybrid moves all three of its counters" $ do
          (hybridId, workerId, gs) <- board "Arcbound Worker"
          let (settled, after) = murderIt exercising gs
          Spec.assertEqWith s "the Hybrid held three, not its printed two" (plusOnes hybridId (fst gs)) 3
          Spec.assertEqWith s "the Worker held one" (plusOnes workerId (fst gs)) 1
          Spec.assertEqWith s "the death trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (not (S.onBattlefield hybridId after)) "and the Hybrid is gone"
          -- CR 608.2h: four is one plus THREE, so the count came from the last
          -- known record. Two would be the printed N and one a literal.
          Spec.assertEqWith s "the Worker is up to four" (plusOnes workerId after) 4
          Spec.assertEqWith s "so it is a 4/4" (S.powerToughnessOf workerId after) (Just (4, 4))
        -- CR 603.5's "may" is a real fork, and the control for the case above --
        -- same board, same Murder, and the trigger still reaches the stack.
        Spec.it s "CR 603.5 declining the may leaves the counters nowhere" $ do
          (_, workerId, gs) <- board "Arcbound Worker"
          let (settled, after) = murderIt S.identityAnswer gs
          Spec.assertEqWith s "the trigger reached the stack all the same" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "the Worker is still on one" (plusOnes workerId after) 1
          Spec.assertEqWith s "a 1/1" (S.powerToughnessOf workerId after) (Just (1, 1))
        -- CR 608.2h in isolation, counterLookBackSpec's third case in the payload
        -- rather than in an intervening "if": the record is emptied while the
        -- trigger sits on the stack, which no rule can do to last known
        -- information -- so only a payload that really reads it notices.
        Spec.it s "CR 608.2h the count comes from the last known record, not from the board" $ do
          (hybridId, workerId, gs) <- board "Arcbound Worker"
          let (settled, _) = murderIt exercising gs
              forgotten =
                settled
                  { GameState.lastKnown =
                      Map.adjust (\lk -> lk {LastKnown.counters = Map.empty}) hybridId (GameState.lastKnown settled)
                  }
              after = S.runPure exercising forgotten Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and resolved off it" (GameState.stack after) []
          Spec.assertEqWith s "and moved nothing, the record being empty" (plusOnes workerId after) 1
        -- "Target ARTIFACT creature": Goblin Piker 2/1 is a creature and not an
        -- artifact, so CR 603.3d finds no legal target and the ability never
        -- reaches the stack. The case above is the control -- the only difference
        -- between the two boards is which creature stands beside the Hybrid.
        Spec.it s "CR 702.43a a nonartifact creature is no target at all" $ do
          (_, pikerId, gs) <- board "Goblin Piker"
          let (settled, after) = murderIt exercising gs
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
          Spec.assertEqWith s "and the Piker is still on the one it started with" (plusOnes pikerId after) 1
        -- CR 702.43b: each instance works separately. Asserted of BOTH mints,
        -- vanishing's position, no printing in the pool carrying modular twice.
        -- Spelled out rather than compared against Keyword.modular itself, for
        -- vanishingSpec's reason.
        Spec.it s "CR 702.43b each instance is its own two abilities" $ do
          Spec.assertEqWith
            s
            "modular 2 held twice mints two death triggers"
            (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Modular 2) 2)))
            [TriggerCondition.SelfDies, TriggerCondition.SelfDies]
          Spec.assertEqWith
            s
            "and two entry rewrites of two counters each, which is what makes them add up"
            (Keyword.mintedReplacementsFor (Keyword.Type.Modular 2) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.PlusOnePlusOne 2)))))

-- CR 510.1b / 510.2's combat damage watched by a BYSTANDER rather than by the
-- creature that dealt it -- TriggerCondition.PermanentDealsCombatDamageToPlayer,
-- the filtered twin of poisonousSpec's SelfDealsCombatDamageToPlayer.
--
-- Tovolar, Dire Overlord {1}{R}{G} Legendary Creature -- Human Werewolf 3/3 is
-- the card: "whenever a Wolf or Werewolf you control deals combat damage to a
-- player, draw a card". Both faces print it; the back face's copy goes through
-- Pawl.CardSpec's corpus lints, but no case here reaches it -- that needs the CR
-- 731 transform Pawl.DaytimeSpec drives.
--
-- Tovolar is himself a Werewolf, so the filter admits the watcher: a self-scoped
-- reading would draw one card where these cases draw two. Russet Wolves (Wolf
-- 3/3) is the other subtype of the printed "or", and Goblin Piker (Goblin Warrior
-- 2/1) is the creature the filter must reject.
--
-- Every library is stocked past the draws, so an extra draw shows as an extra
-- card rather than as CR 104.3c ending the game before the assertions run.
tovolarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tovolarSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      stock printing n pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)
      -- CR 508.1's declaration narrowed to the named creatures, and CR 509.1's
      -- left empty: S.aggressiveAnswer attacks and blocks with everything, which
      -- a case about one attacker connecting cannot allow.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      -- CR 508.1 / 509.1: one named attacker, met by one named blocker.
      oneOnOne :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      oneOnOne attacker blocker p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.DeclareBlockers _ _ _ attackers -> Map.singleton blocker (Set.fromList attackers)
        _ -> S.aggressiveAnswer p
   in Spec.describe s "Filtered combat damage" $ do
        -- The proving test. Three unblocked attackers, two of which the filter
        -- admits: Tovolar for "Werewolf" and the Wolves for "Wolf". The Piker
        -- connects too and draws nothing, which is the filter doing its work
        -- inside the same event.
        Spec.it s "CR 510.2 a bystander draws once per Wolf or Werewolf that connects" $ do
          (gs, _, _) <- board ["Tovolar, Dire Overlord", "Russet Wolves", "Goblin Piker"] []
          piker <- S.printingOf s registry "Goblin Piker"
          let after = S.runCombat S.aggressiveAnswer (stock piker 4 S.alice gs)
          Spec.assertEqWith s "all three connected, for eight" (S.lifeOf S.bob after) (Just 12)
          Spec.assertEqWith s "so two cards were drawn, not three and not one" (handSize S.alice after) 2
          Spec.assertEqWith s "leaving two in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 2
        -- CR 510.1c: a BLOCKED creature assigns its combat damage to the blocker,
        -- so the Wolves deals its three to bob's Piker and the condition's
        -- player-recipient half rejects the event. The watcher is on the board and
        -- the damager is a Wolf she controls; only the recipient differs.
        Spec.it s "CR 510.1c combat damage dealt to a creature draws nothing" $ do
          (gs, mine, theirs) <- board ["Tovolar, Dire Overlord", "Russet Wolves"] ["Goblin Piker"]
          piker <- S.printingOf s registry "Goblin Piker"
          case (mine, theirs) of
            ([_, wolves], [blocker]) -> do
              let after = S.runCombat (oneOnOne wolves blocker) (stock piker 4 S.alice gs)
              Spec.assertEqWith s "bob took none of it" (S.lifeOf S.bob after) (Just 20)
              Spec.assertEqWith s "and his Piker died for it" (S.creaturesInPlay S.bob after) 0
              Spec.assertEqWith s "so no card was drawn" (handSize S.alice after) 0
            _ -> Spec.assertFailure s "fixture should give alice a Tovolar and a Wolves, bob a Piker"
        -- CR 109.5's "you control", which is what makes this a bystander's
        -- condition rather than the board's: bob has a Tovolar of his own,
        -- watching alice's two connect. He controls neither, so he draws nothing
        -- -- an arm that read the event's damager without the Filter's
        -- ControlledBy would have him draw twice.
        Spec.it s "CR 109.5 the defender's own Tovolar sees no Wolf of his connect" $ do
          (gs, mine, theirs) <- board ["Tovolar, Dire Overlord", "Russet Wolves"] ["Tovolar, Dire Overlord"]
          piker <- S.printingOf s registry "Goblin Piker"
          case (mine, theirs) of
            ([tovolar, wolves], [_]) -> do
              let after = S.runCombat (plan [tovolar, wolves]) (stock piker 4 S.bob (stock piker 4 S.alice gs))
              Spec.assertEqWith s "both of alice's connected, for six" (S.lifeOf S.bob after) (Just 14)
              Spec.assertEqWith s "she drew two" (handSize S.alice after) 2
              Spec.assertEqWith s "and he drew none" (handSize S.bob after) 0
            _ -> Spec.assertFailure s "fixture should give alice a Tovolar and a Wolves, bob a Tovolar"

-- What the filtered condition is FOR: a payload that aims at the creature that
-- dealt the damage (Pawl.Engine.Binding.combatDamager) rather than at the bearer
-- -- Aragorn, Hornburg Hero {1}{R}{G}{W} Legendary Creature -- Human Soldier 4/4,
-- "attacking creatures you control have first strike and renown 1" and "whenever
-- a renowned creature you control deals combat damage to a player, double the
-- number of +1/+1 counters on it".
--
-- Three capabilities meet here, and each has a way to fail that the counts below
-- tell apart: the slot naming the damager (aim it at the source and Aragorn takes
-- the counters), Quantity.AgainstSlot reading the damager's counters (read the
-- source's and the number is 0), and Filter.HasDesignation rejecting a candidate that
-- is not renowned yet (drop it and the Piker doubles too).
--
-- Aurelia, the Warleader supplies the second combat phase, as she does in
-- renownSpec: the doubling needs a creature that was ALREADY renowned when it
-- connected, and CR 603.2 checks this condition against the damage event itself,
-- where renown's own counters arrive only as ITS trigger resolves -- so one
-- connection can never both renown a creature and double it.
--
-- Aragorn arrives BETWEEN the two combats so the Maulers takes its two counters
-- from printed renown 2 alone: with him out on the first swing the Maulers would
-- hold renown 2 and a granted renown 1 at once, and CR 702.112c leaves which
-- resolves first to its controller.
aragornSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aragornSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      renownedness oid gs = fmap (Set.member Designation.Renowned . Object.designations) (Game.lookupObject oid gs)
   in Spec.describe s "Doubling a damager's counters" $ do
        -- The proving test. 2 -> 4 rather than 2 -> 3, which is what separates
        -- "double" from "add one", and 4 rather than 2, which is what separates
        -- reading the damager's counters from reading the bearer's.
        Spec.it s "CR 702.112b whole card: a renowned creature's counters double when it connects" $ do
          (gs, mine, _) <- board ["Rhox Maulers", "Aurelia, the Warleader", "Goblin Piker"] []
          aragorn <- S.printingOf s registry "Aragorn, Hornburg Hero"
          case mine of
            [maulers, aurelia, piker] -> do
              let first = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers, aurelia]) gs
                  (hero, staged) = S.addCreature aragorn S.alice first
                  loaded = S.addCounter CounterKind.PlusOnePlusOne 3 piker staged
                  second = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (plan [maulers, piker]) loaded
                  after = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers, piker]) second
              Spec.assertEqWith s "the first combat connected for seven" (S.lifeOf S.bob first) (Just 13)
              Spec.assertEqWith s "renown 2 alone renowned the Maulers" (countersOn maulers first) (Map.singleton CounterKind.PlusOnePlusOne 2)
              Spec.assertEqWith s "the second phase really ran a declaration" (GameState.phase second) (Phase.Combat CombatStep.DeclareAttackers)
              Spec.assertEqWith s "and the second combat connected for eleven" (S.lifeOf S.bob after) (Just 2)
              Spec.assertEqWith s "so the Maulers doubled to four, not three" (countersOn maulers after) (Map.singleton CounterKind.PlusOnePlusOne 4)
              Spec.assertEqWith s "making it an 8/8" (S.powerToughnessOf maulers after) (Just (8, 8))
              -- CR 702.112b's designation read of a CANDIDATE: the Piker carries
              -- three counters from outside renown and was NOT renowned when it
              -- dealt its damage, so Aragorn's ability never triggered for it. The
              -- fourth counter is the renown 1 he granted it, which becomes
              -- renowned only as that trigger resolves -- 4 rather than the 7 a
              -- filter without the designation conjunct would leave.
              Spec.assertEqWith s "the Piker was renowned by that same damage" (renownedness piker after) (Just True)
              Spec.assertEqWith s "but took one counter rather than doubling" (countersOn piker after) (Map.singleton CounterKind.PlusOnePlusOne 4)
              -- The slot: had the payload aimed at CR 113.7a's source, these
              -- counters would be here instead.
              Spec.assertEqWith s "and Aragorn himself took none" (countersOn hero after) Map.empty
            _ -> Spec.assertFailure s "fixture should give alice a Maulers, an Aurelia and a Piker"
        -- The other half of the same static ability, which the case above only
        -- passes through: CR 702.7b's first strike, granted to an ATTACKING
        -- creature. Two identical 2/1s meet, and only the attacker's controller
        -- has an Aragorn -- so the blocker is dead before it assigns (CR 510.4),
        -- where without the grant both would die.
        Spec.it s "CR 702.7b the same static grants first strike to attackers" $ do
          (gs, mine, theirs) <- board ["Aragorn, Hornburg Hero", "Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([_, piker], [blocker]) -> do
              let after = S.runCombat (plan [piker]) gs
              Spec.assertEqWith s "the blocker is dead" (Game.lookupObject blocker after) Nothing
              Spec.assertEqWith s "the attacker survived, unrenowned" (renownedness piker after) (Just False)
              Spec.assertEqWith s "alice keeps both creatures" (S.creaturesInPlay S.alice after) 2
              Spec.assertEqWith s "bob none" (S.creaturesInPlay S.bob after) 0
              Spec.assertEqWith s "and nothing reached bob" (S.lifeOf S.bob after) (Just 20)
            _ -> Spec.assertFailure s "fixture should give alice an Aragorn and a Piker, bob a Piker"

-- The same filtered condition's OTHER half of the event: HOW MUCH it carried,
-- read back through Pawl.Engine.Binding.eventAmount -- Shroofus Sproutsire
-- {2}{G} Legendary Creature -- Saproling 1/1, "trample" and "whenever a Saproling
-- you control deals combat damage to a player, create that many 1/1 green
-- Saproling creature tokens".
--
-- Trample is what makes "that many" a different number from anything else on the
-- board: CR 702.19b lets the attacker's controller hold damage back on the
-- blocker, so a 5/5 trampler can deal 4 to the player. Four tokens is therefore
-- not the damager's power (5), not a fixed count (1), and not one per damager (2
-- creatures connect). The power reading is the one that needs the trample: an
-- unblocked trampler deals its whole power, and no board without a blocker tells
-- the two apart.
--
-- Shroofus is himself a Saproling, so the filter admits the watcher, and the
-- Goblin Piker (Goblin Warrior 2/1) beside him is the creature it must reject.
--
-- No library stocking: nothing here draws, and runCombat stops at end of combat,
-- so CR 104.3c never comes up.
shroofusSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
shroofusSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1 / 509.1 / 510.1c: the named attackers, the named blocks, and the
      -- trampler's division pinned rather than left to the fixture -- the division
      -- is the one thing the two cases below differ in.
      swing ::
        [ObjectId.ObjectId] ->
        Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId) ->
        [(ObjectId.ObjectId, Map.Map Recipient.Recipient Natural)] ->
        Prompt.Prompt r ->
        r
      swing attackers blocks divisions p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.DeclareBlockers {} -> blocks
        Prompt.AssignCombatDamage _ _ damager _ _ -> Maybe.fromMaybe Map.empty (List.lookup damager divisions)
        _ -> S.aggressiveAnswer p
      -- CR 508.1b's choice, which only the three-seat board raises: bob is
      -- attacked, carol is the opponent who is neither the damaged player nor the
      -- damager's controller.
      atBob :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      atBob attackers p = case p of
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer S.bob) (NonEmpty.toList options))
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
   in Spec.describe s "Tokens counted off a combat damage event" $ do
        -- The proving case. A 5/5 trampler holds 1 back on the blocker and spills
        -- 4, while a Goblin Piker connects unblocked for 2 alongside it.
        Spec.it s "CR 510.2 whole card: that many is the damage dealt, not the damager's power" $ do
          (gs, mine, theirs) <- board ["Shroofus Sproutsire", "Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([shroofus, piker], [blocker]) -> do
              let loaded = S.addCounter CounterKind.PlusOnePlusOne 4 shroofus gs
                  blocks = Map.singleton blocker (Set.singleton shroofus)
                  spilling = [(shroofus, Map.fromList [(Recipient.ToCreature blocker, 1), (Recipient.ToPlayer S.bob, 4)])]
                  after = S.runCombat (swing [shroofus, piker] blocks spilling) loaded
              Spec.assertEqWith s "the counters make Shroofus a 5/5" (S.powerToughnessOf shroofus loaded) (Just (5, 5))
              Spec.assertEqWith s "4 spilled past the blocker, plus the Piker's 2" (S.lifeOf S.bob after) (Just 14)
              Spec.assertBool s (not (Set.member blocker (GameState.battlefield after))) "CR 704.5g: the blocker took its 1"
              -- 6 = Shroofus, the Goblin Piker, and four tokens. A count read off
              -- his POWER would be 7, a fixed count of one 3, and a filter that
              -- admitted the Goblin Piker's 2 as well 8.
              Spec.assertEqWith s "so four tokens, not five and not one and not six" (S.creaturesInPlay S.alice after) 6
              Spec.assertEqWith s "and bob keeps nothing" (S.creaturesInPlay S.bob after) 0
            _ -> Spec.assertFailure s "fixture should give alice a Shroofus and a Piker, bob a Piker"
        -- The negative, on the SAME board with the same seats, the same creatures
        -- and the same declaration -- only the division differs. CR 702.19b lets
        -- the whole 5 stay on the blocker, so the Saproling deals its
        -- combat damage to a creature rather than to a player and the condition's
        -- recipient half rejects the event. The Goblin Piker still connects for 2,
        -- which the filter rejects -- so both halves answer at once, and neither
        -- makes a token.
        Spec.it s "CR 510.1c the same trampler soaking its blocker makes nothing" $ do
          (gs, mine, theirs) <- board ["Shroofus Sproutsire", "Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([shroofus, piker], [blocker]) -> do
              let loaded = S.addCounter CounterKind.PlusOnePlusOne 4 shroofus gs
                  blocks = Map.singleton blocker (Set.singleton shroofus)
                  soaking = [(shroofus, Map.fromList [(Recipient.ToCreature blocker, 5), (Recipient.ToPlayer S.bob, 0)])]
                  after = S.runCombat (swing [shroofus, piker] blocks soaking) loaded
              Spec.assertEqWith s "only the Goblin Piker's 2 reached bob" (S.lifeOf S.bob after) (Just 18)
              Spec.assertBool s (not (Set.member blocker (GameState.battlefield after))) "CR 704.5g: the blocker took all 5"
              Spec.assertEqWith s "and alice keeps her two creatures, with no token beside them" (S.creaturesInPlay S.alice after) 2
            _ -> Spec.assertFailure s "fixture should give alice a Shroofus and a Piker, bob a Piker"
        -- CR 109.5's "you control", on three seats so that the damager's
        -- controller, the damaged player and a bystanding opponent are three
        -- different people. alice's Shroofus connects unblocked for 5; bob's and
        -- carol's watch it and make nothing.
        Spec.it s "CR 109.5 an opponent's Shroofus counts no Saproling of hers" $ do
          shroofus <- S.printingOf s registry "Shroofus Sproutsire"
          let (gs, mine, theirs, hers) = S.threePlayerCombat [shroofus] [shroofus] [shroofus]
          case (mine, theirs, hers) of
            ([attacker], [_], [_]) -> do
              let loaded = S.addCounter CounterKind.PlusOnePlusOne 4 attacker gs
                  after = S.runCombat (atBob [attacker]) loaded
              Spec.assertEqWith s "unblocked, all 5 reached bob" (S.lifeOf S.bob after) (Just 15)
              Spec.assertEqWith s "carol was not attacked" (S.lifeOf S.carol after) (Just 20)
              Spec.assertEqWith s "alice took five tokens" (S.creaturesInPlay S.alice after) 6
              Spec.assertEqWith s "bob none" (S.creaturesInPlay S.bob after) 1
              Spec.assertEqWith s "and carol none" (S.creaturesInPlay S.carol after) 1
            _ -> Spec.assertFailure s "fixture should give each seat a Shroofus"

-- CR 702.25a's flanking, which rule 702 states as a triggered
-- ability, and with it CR 509.3d -- "becomes blocked by a creature", the one
-- block-trigger form that fires once per BLOCKER and names it.
--
-- Benalish Cavalry {1}{W} Creature -- Human Knight 2/2 is the card: flanking and
-- nothing else, so every number below is the keyword's. Its blockers are drawn
-- from the pool's vanilla creatures for the same reason.
--
-- Every reading is taken at the COMBAT DAMAGE step, before damage is dealt (CR
-- 509.2a puts these triggers on the stack in the declare blockers step), so the
-- -1/-1 is read directly rather than through what survives combat.
flankingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
flankingSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
   in Spec.describe s "Flanking" $ do
        -- The proving test, and its control: the same 2/2 attacker WITHOUT
        -- flanking (Icehide Golem) against the same 2/1 blocker. The flanker's
        -- Piker is 1/0 and already dead when damage would be dealt, so the
        -- flanker survives; the Golem trades with it.
        Spec.it s "CR 702.25a whole card: the blocking Piker is -1/-1 and dies before damage" $ do
          (gs, mine, theirs) <- board ["Benalish Cavalry"] ["Goblin Piker"]
          (control, controlMine, controlTheirs) <- board ["Icehide Golem"] ["Goblin Piker"]
          case (mine, theirs, controlMine, controlTheirs) of
            ([cavalry], [piker], [golem], [otherPiker]) -> do
              let struck = atDamage gs
                  traded = S.runCombat S.aggressiveAnswer gs
                  controlStruck = atDamage control
                  controlTraded = S.runCombat S.aggressiveAnswer control
              Spec.assertBool s (not (S.onBattlefield piker struck)) "the 2/1 Piker went to 1/0 and CR 704.5f buried it"
              Spec.assertEqWith s "the Cavalry itself is untouched" (S.powerToughnessOf cavalry struck) (Just (2, 2))
              Spec.assertBool s (S.onBattlefield cavalry traded) "so nothing was left to deal it damage"
              Spec.assertEqWith s "control leg: a 2/2 without flanking leaves the Piker at 2/1" (S.powerToughnessOf otherPiker controlStruck) (Just (2, 1))
              Spec.assertBool s (not (S.onBattlefield golem controlTraded)) "and the Piker's 2 kills it"
              Spec.assertBool s (not (S.onBattlefield otherPiker controlTraded)) "both die, where the flanker died alone"
            _ -> Spec.assertFailure s "fixture should give each seat one creature"
        -- CR 509.3d's arity, and the whole difference from CR 509.3c: "triggers
        -- once for each creature that blocks the specified creature". Two
        -- blockers, two triggers, and each -1/-1 lands on its OWN blocker.
        --
        -- The Hill Giant is the load-bearing reading: a condition matched against
        -- the GROUPED GameEvent.AttackerBlocked fires once and leaves it 3/3,
        -- and a binding that named the bearer instead moves the Cavalry's own
        -- 2/2.
        Spec.it s "CR 509.3d two blockers are two triggers, each on its own blocker" $ do
          (gs, mine, theirs) <- board ["Benalish Cavalry"] ["Goblin Piker", "Hill Giant"]
          case (mine, theirs) of
            ([cavalry], [piker, giant]) -> do
              let struck = atDamage gs
              -- One assertion over all three readings, so a mutation cannot hide
              -- behind whichever of them is checked first.
              Spec.assertEqWith
                s
                "the 3/3 Giant is 2/2, the 2/1 Piker is gone, and the Cavalry took neither -1/-1"
                (S.powerToughnessOf giant struck, S.onBattlefield piker struck, S.powerToughnessOf cavalry struck)
                (Just (2, 2), False, Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give bob two blockers"
        -- CR 702.25a's "without flanking", read as CR 509.3f asks -- the blocker's
        -- characteristics as it becomes a blocking creature. A second Benalish
        -- Cavalry blocking is 2/2 still; the Icehide Golem, the same 2/2 without
        -- the keyword, is 1/1. The two boards differ in nothing else.
        Spec.it s "CR 702.25a a blocker WITH flanking is spared and one without is not" $ do
          (withIt, _, theirs) <- board ["Benalish Cavalry"] ["Benalish Cavalry"]
          (without, _, others) <- board ["Benalish Cavalry"] ["Icehide Golem"]
          case (theirs, others) of
            ([blockingCavalry], [golem]) -> do
              Spec.assertEqWith s "the flanking blocker is untouched" (S.powerToughnessOf blockingCavalry (atDamage withIt)) (Just (2, 2))
              Spec.assertEqWith s "the one without takes -1/-1" (S.powerToughnessOf golem (atDamage without)) (Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give bob one blocker on each board"
        -- CR 702.25b: each instance triggers separately, which is abilitiesFor's
        -- replicate. No card in the pool prints flanking twice, so this is
        -- asserted of the MINT rather than of a board -- as bushido's, prowess'
        -- and battle cry's are.
        Spec.it s "CR 702.25b two instances mint two abilities, both CR 509.3d" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Flanking 2
              expected =
                TriggerCondition.SelfBecomesBlockedBy
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not (Filter.Type.HasKeyword Keyword.Type.Flanking)])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith s "each watching CR 509.3d, filtered on the blocker's own flanking" (fmap TriggeredAbility.condition abilities) [expected, expected]

-- CR 702.45a: "'Bushido N' means 'Whenever this creature blocks or becomes
-- blocked, it gets +N/+N until end of turn.'" Rule 702 states it as a triggered
-- ability, as it does CR 702.70a, CR 702.86a, CR
-- 702.91a and CR 702.108a, and it is the only one that names TWO events: "blocks" is
-- CR 509.3a and "becomes blocked" is CR 509.3c, so Pawl.Engine.Keyword.bushido
-- mints two abilities and the two cases below fire one each.
--
-- Inner-Chamber Guard, {1}{W} Creature -- Human Samurai 0/2 with bushido 2 and
-- nothing else. Chosen for its numbers: 0/2 becoming 2/4 is unmistakable, an
-- asymmetric base means no reading of the rule lands on the same pair, and
-- bushido 2 rather than 1 keeps +N/+N apart from a hardcoded +1/+1. Goblin Piker
-- 2/1 is the other side, and the two flip TOGETHER on the pump: at 2/4 the Guard
-- kills the Piker and lives, at 0/2 it kills nothing and dies. Those two survival
-- assertions are regression fences rather than proofs -- every mutation tried
-- against this group tripped the power/toughness assertion above them first --
-- and what they fence is the TIMING: a pump that landed after CR 510's damage
-- would leave both creatures where an unpumped Guard does.
bushidoSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bushidoSpec s registry =
  let noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- The board as combat damage is about to be dealt, so the pump is readable
      -- before it decides anything.
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
   in Spec.describe s "Bushido" $ do
        -- CR 509.3a's half, whole card: bob's Guard blocks alice's Piker.
        Spec.it s "CR 702.45a whole card: blocking makes Inner-Chamber Guard 2/4" $ do
          (gs, attackers, blockers) <- board ["Goblin Piker"] ["Inner-Chamber Guard"]
          case (attackers, blockers) of
            ([piker], [guard]) -> do
              let pumped = atDamage gs
                  fought = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "0/2 before blockers are declared" (S.powerToughnessOf guard gs) (Just (0, 2))
              Spec.assertEqWith s "and 2/4 once the trigger has resolved" (S.powerToughnessOf guard pumped) (Just (2, 4))
              Spec.assertBool s (not (S.onBattlefield piker fought)) "the pumped Guard's 2 killed the 2/1 Piker"
              Spec.assertBool s (S.onBattlefield guard fought) "and its 4 toughness survived the Piker's 2"
            _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Guard"
        -- The control leg for the case above, on the same board: nothing blocks,
        -- so CR 509.3a's event never happens and the Guard stays 0/2. Without it
        -- an ability that pumped on any combat event at all would pass.
        Spec.it s "CR 509.3a a Guard that does not block is not pumped" $ do
          (gs, _, blockers) <- board ["Goblin Piker"] ["Inner-Chamber Guard"]
          case blockers of
            [guard] -> do
              let after = S.runCombat noBlocks gs
              Spec.assertEqWith s "still 0/2" (S.powerToughnessOf guard after) (Just (0, 2))
              Spec.assertEqWith s "and the unblocked Piker's 2 reached bob" (S.lifeOf S.bob after) (Just 18)
            _ -> Spec.assertFailure s "fixture should give bob a Guard"
        -- CR 509.3c's half, the other arm of the same printed sentence: now the
        -- Guard is alice's and attacks, and bob's Piker blocks it. An
        -- implementation with only the CR 509.3a arm passes every case above and
        -- fails this one.
        Spec.it s "CR 702.45a whole card: becoming blocked makes Inner-Chamber Guard 2/4" $ do
          (gs, attackers, blockers) <- board ["Inner-Chamber Guard"] ["Goblin Piker"]
          case (attackers, blockers) of
            ([guard], [piker]) -> do
              let pumped = atDamage gs
                  fought = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "2/4 once the trigger has resolved" (S.powerToughnessOf guard pumped) (Just (2, 4))
              Spec.assertBool s (not (S.onBattlefield piker fought)) "the pumped Guard's 2 killed the 2/1 Piker"
              Spec.assertBool s (S.onBattlefield guard fought) "and its 4 toughness survived the Piker's 2"
            _ -> Spec.assertFailure s "fixture should give alice a Guard and bob a Piker"
        -- The control leg for CR 509.3c, on the same board as the case above:
        -- attacking is not becoming blocked, so an unblocked Guard stays 0/2 and
        -- takes nothing from bob.
        Spec.it s "CR 509.3c a Guard that goes unblocked is not pumped" $ do
          (gs, attackers, _) <- board ["Inner-Chamber Guard"] ["Goblin Piker"]
          case attackers of
            [guard] -> do
              let after = S.runCombat noBlocks gs
              Spec.assertEqWith s "still 0/2" (S.powerToughnessOf guard after) (Just (0, 2))
              Spec.assertEqWith s "and 0 power took nothing from bob" (S.lifeOf S.bob after) (Just 20)
            _ -> Spec.assertFailure s "fixture should give alice a Guard"
        -- CR 702.45b: "If a creature has multiple instances of bushido, each
        -- triggers separately." Asked of the mint rather than of a board, as
        -- prowess' and battle cry's are: no card in this pool prints bushido twice
        -- and nothing here grants it. The count is FOUR rather than two because
        -- one instance is already two abilities -- rule 702.45a's one sentence,
        -- CR 509.3a's event and CR 509.3c's.
        Spec.it s "CR 702.45b each instance of bushido is its own ability" $ do
          Spec.assertEqWith s "bushido 2 held once is its two halves" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Bushido 2) 1)) (Keyword.bushido 2)
          Spec.assertEqWith s "and held twice is four abilities" (length (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Bushido 2) 2))) 4

-- CR 702.130a: "'Afflict N' means 'Whenever this creature becomes blocked,
-- defending player loses N life.'" Rule 702 states it as a triggered ability,
-- and it is the first to put CR 509.3c's event and CR
-- 508.5's defending player in one sentence.
--
-- Khenra Eternal {1}{B} Creature -- Zombie Jackal Warrior 2/2 with afflict 1 and
-- nothing else printed on it, so every number below is the keyword's.
--
-- THREE SEATS, for annihilatorSpec's reason: at two players "the defending
-- player" and "the attacker's one opponent" are the same seat.
--
-- Afflict 1 is the only N a card in this pool puts on a board, so no case below
-- can tell the keyword's N from a hardcoded 1. The mint inequality in the last
-- case is what does, and it is there for that and no other reason.
afflictSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
afflictSpec s registry =
  let -- Attacks `who` with everything and lets them block with everything.
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        _ -> S.aggressiveAnswer p
      -- The same, with CR 509.1's declaration switched off -- the control leg, and
      -- the only difference between the two answerers.
      unblocked :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      unblocked who p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> attacking who p
      -- alice fields the Khenra; bob and carol each field a Goblin Piker, so
      -- either can block and neither is the only possible defender.
      board = do
        khenra <- S.printingOf s registry "Khenra Eternal"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat [khenra] [piker] [piker])
      -- All three life totals as one reading, so no mutation can hide behind the
      -- order the assertions happen to be written in.
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "Afflict" $ do
        -- The proving test. alice attacks bob, bob blocks, and CR 508.5 makes bob
        -- the defending player, so bob alone loses 1. No combat damage reaches a
        -- player: the Khenra is blocked, and its 2 and the Piker's 2 trade.
        Spec.it s "CR 702.130a whole card: a blocked Khenra Eternal costs the defending player 1 life" $ do
          (gs, ours, yours, _) <- board
          case (ours, yours) of
            ([khenra], [piker]) -> do
              let after = S.runCombat (attacking S.bob) gs
              Spec.assertEqWith s "bob, and nobody else, is down 1" (lives after) (Just 20, Just 19, Just 20)
              Spec.assertBool s (not (S.onBattlefield khenra after)) "the 2/2 Khenra died to the Piker's 2"
              Spec.assertBool s (not (S.onBattlefield piker after)) "and the 2/1 Piker to the Khenra's"
            _ -> Spec.assertFailure s "fixture should give alice a Khenra and bob a Piker"
        -- CR 508.5a: the defending player is one SPECIFIC player, determined per
        -- attacking creature. The only difference from the case above is the
        -- answer to Prompt.ChooseDefender, so an implementation that bound the
        -- attacker's controller, or "an opponent", or a fixed seat cannot pass
        -- both.
        Spec.it s "CR 508.5 the life follows whichever opponent was attacked" $ do
          (gs, _, _, _) <- board
          Spec.assertEqWith s "carol, attacked this time, is the one down 1" (lives (S.runCombat (attacking S.carol) gs)) (Just 20, Just 20, Just 19)
        -- The control leg, on the same board: no block, so CR 509.3c's event never
        -- happens and no life is lost to afflict. bob is down TWO instead of one,
        -- the Khenra's combat damage -- distinct from 1, so the two legs cannot be
        -- read as each other.
        Spec.it s "CR 509.3c an unblocked Khenra Eternal afflicts nobody" $ do
          (gs, _, _, _) <- board
          Spec.assertEqWith s "bob took 2 combat damage and no afflict" (lives (S.runCombat (unblocked S.bob) gs)) (Just 20, Just 18, Just 20)
        -- CR 603.2 through CR 508.5: the becomes-blocked event carries the
        -- defending player, and the scan stamps them under the reserved slot rule
        -- 702.130a's "defending player" reads. The falsifier is an arm that binds
        -- the attacking side, or none at all.
        Spec.it s "CR 603.2 the defending player rides the becomes-blocked event in the reserved slot" $ do
          let bindings = Event.eventBindings TriggerCondition.SelfBecomesBlocked (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked (ObjectId.MkObjectId 9) S.carol))
          Spec.assertEqWith s "carol is bound under thatPlayer" (Binding.targetsOf bindings) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.carol)))
        -- CR 702.130b: "If a creature has multiple instances of afflict, each
        -- triggers separately." Asked of the mint rather than of a board, as
        -- bushido's and prowess' are: no card in this pool prints afflict twice
        -- and nothing here grants it.
        --
        -- The inequality is the second half of the case and a separate claim: N
        -- reaches the minted ability at all. Afflict 1 is the only N a board in
        -- this pool can show, so nothing above would go red if the mint hardcoded
        -- its 1.
        Spec.it s "CR 702.130b each instance of afflict is its own ability, and N reaches it" $ do
          Spec.assertEqWith s "afflict 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afflict 1) 2)) [Keyword.afflict 1, Keyword.afflict 1]
          Spec.assertEqWith s "and afflict 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afflict 3) 1)) [Keyword.afflict 3]
          Spec.assertBool s (Keyword.afflict 1 /= Keyword.afflict 3) "and the two differ, so N is in the ability"

-- CR 702.121 melee, whose rule text is a triggered ability,
-- and the first whose payload is a number read off game state rather than a
-- literal, with Wings of the Guard ({1}{W} Creature -- Bird 1/1, flying and
-- melee, and nothing else).
--
-- THREE SEATS throughout, and here that is load-bearing rather than tidy: at two
-- players "each opponent you attacked" and "each opponent" are the same number,
-- so a bonus that ignored the combat record entirely would pass every case.
--
-- CR 802 is unavailable (#175), so one combat phase has ONE defending player and
-- the bonus pawl can reach is 0 or 1. What separates the two is CR 506.3's other
-- attackable permanents: a creature that attacked only a planeswalker attacked no
-- opponent.
meleeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
meleeSpec s registry =
  let -- Attacks `who` with everything, aiming every attack at the player.
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        _ -> S.aggressiveAnswer p
      isPlaneswalker target = case target of
        AttackTarget.OfPlaneswalker _ -> True
        AttackTarget.OfPlayer _ -> False
        AttackTarget.OfBattle _ -> False
      -- The same, with `these` creatures aimed at a planeswalker instead (CR
      -- 508.1b). Falls back to the head, so a board with no planeswalker offered
      -- runs exactly as the answerer above.
      aimingAtJace :: [ObjectId.ObjectId] -> PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimingAtJace these who p = case p of
        Prompt.ChooseAttackTarget _ _ oid options
          | elem oid these -> case filter isPlaneswalker (NonEmpty.toList options) of
              target : _ -> target
              [] -> NonEmpty.head options
        _ -> attacking who p
      -- alice fields the Bird (plus whatever else `mine` names); bob and carol
      -- each field a Goblin Piker, so either is a legal defending player.
      board mine = do
        wings <- S.printingOf s registry "Wings of the Guard"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat (wings : mine) [piker] [piker])
      -- The same, with bob fielding Jace Beleren at loyalty 3 as well, which is
      -- what lets an attack be declared at something that is not an opponent.
      jaceBoard mine = do
        wings <- S.printingOf s registry "Wings of the Guard"
        piker <- S.printingOf s registry "Goblin Piker"
        jace <- S.printingOf s registry "Jace Beleren"
        pure $ case S.threePlayerCombat (wings : mine) [piker, jace] [piker] of
          (gs, ours, theirs@(_ : jaceId : _), others) -> (S.addCounter CounterKind.Loyalty 3 jaceId gs, ours, theirs, others)
          done -> done
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
   in Spec.describe s "Melee" $ do
        -- The proving test. One opponent attacked, so +1/+1 on a 1/1. The
        -- falsifier three seats buy: a bonus counting alice's OPPONENTS rather
        -- than the ones she attacked reads 2 here and cannot pass.
        Spec.it s "CR 702.121a whole card: Wings of the Guard attacking one of two opponents is 2/2" $ do
          (gs, ours, _, _) <- board []
          case ours of
            [wings] -> Spec.assertEqWith s "1/1 plus one opponent attacked" (S.powerToughnessOf wings (atBlockers (attacking S.bob) gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Bird"
        -- Rule 702.121a counts OPPONENTS, not creatures: a second attacker at the
        -- same seat adds nothing. The falsifier is a bonus read off the size of
        -- the declaration, which reads 2 here and 1 above.
        Spec.it s "CR 702.121a a second attacker at the same opponent does not raise the bonus" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          (gs, ours, _, _) <- board [piker]
          case ours of
            [wings, _] -> Spec.assertEqWith s "still one opponent attacked" (S.powerToughnessOf wings (atBlockers (attacking S.bob) gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice a Bird and a Piker"
        -- CR 508.4's sibling reading, from the other side: attacking an
        -- opponent's PLANESWALKER is not attacking that opponent, so the bonus is
        -- 0 and the Bird stays a 1/1. The attack record is asserted first, so a
        -- run where the Bird failed to attack at all fails there rather than
        -- passing this vacuously.
        Spec.it s "CR 506.3 a creature that attacked only a planeswalker gets +0/+0" $ do
          (gs, ours, theirs, _) <- jaceBoard []
          case (ours, theirs) of
            ([wings], [_, jaceId]) -> do
              let after = atBlockers (aimingAtJace [wings] S.bob) gs
              Spec.assertEqWith s "CR 508.1b the Bird really did attack Jace" (Map.lookup wings (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlaneswalker jaceId))
              Spec.assertEqWith s "and no opponent was attacked, so it is still a 1/1" (S.powerToughnessOf wings after) (Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Bird and bob a Jace"
        -- Melee still TRIGGERS when its bearer attacks a planeswalker -- what the
        -- planeswalker changes is the bonus. Same board as above plus a Piker
        -- sent at bob, so the record holds one opponent and the Bird is pumped
        -- although it attacked nobody.
        Spec.it s "CR 702.121a the bearer's own attack need not be the one that counts" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          (gs, ours, _, _) <- jaceBoard [piker]
          case ours of
            [wings, _] -> Spec.assertEqWith s "the Piker's attack on bob is the +1/+1" (S.powerToughnessOf wings (atBlockers (aimingAtJace [wings] S.bob) gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice a Bird and a Piker"
        -- CR 611.2d: the bonus is fixed as the ability resolves, and CR 511.3
        -- clears the combat record at end of combat -- so a pump that re-read the
        -- record live would shrink back to +0/+0 the moment combat ended, while
        -- the printed duration runs to end of turn.
        Spec.it s "CR 611.2d the +1/+1 outlives the combat record it was computed from" $ do
          (gs, ours, _, _) <- board []
          case ours of
            [wings] -> do
              let after = S.runToStep Phase.PostcombatMain (attacking S.bob) gs
              Spec.assertEqWith s "CR 511.3 the record is cleared" (Combat.Type.declaredAttacked (GameState.combat after)) Set.empty
              Spec.assertEqWith s "and the Bird is still a 2/2" (S.powerToughnessOf wings after) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Bird"
        -- CR 702.121b: two instances are two abilities, so two triggers and two
        -- bonuses. Asserted at the mint, no card in the pool having melee twice.
        Spec.it s "CR 702.121b each instance triggers separately" $ do
          Spec.assertEqWith s "melee held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Melee 2)) [Keyword.melee, Keyword.melee]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Melee 1)) [Keyword.melee]

-- CR 702.105 dethrone, whose CONDITION is the whole keyword -- the first minted
-- trigger narrowed by a fact about the game rather than about the declaration --
-- with Enraged Revolutionary ({2}{R} Creature -- Human Warrior 2/1, dethrone and
-- nothing else). The counter is read as power and toughness, so 2/1 is "did not
-- trigger" and 3/2 is "did".
--
-- THREE SEATS throughout, and load-bearing twice over: at two players "the player
-- with the most life" and "the defending player" coincide whenever the attacker's
-- controller is behind, and there is no second opponent to be the wrong one.
--
-- Life totals are all distinct except where a tie is the point, so no reading of
-- the rule produces the same board twice.
dethroneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dethroneSpec s registry =
  let at pid n gs = gs {GameState.players = Map.adjust (\pl -> pl {Player.life = n}) pid (GameState.players gs)}
      lives a b c gs = at S.alice a (at S.bob b (at S.carol c gs))
      -- alice fields the Revolutionary; bob and carol each field a Piker, so
      -- either is a legal defending player.
      board = do
        rev <- S.printingOf s registry "Enraged Revolutionary"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat [rev] [piker] [piker])
      -- The same with bob fielding Jace Beleren at loyalty 3, so that the only
      -- attackable permanent on his side is not a player.
      jaceBoard = do
        rev <- S.printingOf s registry "Enraged Revolutionary"
        piker <- S.printingOf s registry "Goblin Piker"
        jace <- S.printingOf s registry "Jace Beleren"
        pure $ case S.threePlayerCombat [rev] [piker, jace] [piker] of
          (gs, ours, theirs@(_ : jaceId : _), others) -> (S.addCounter CounterKind.Loyalty 3 jaceId gs, ours, theirs, others)
          done -> done
      isPlaneswalker target = case target of
        AttackTarget.OfPlaneswalker _ -> True
        AttackTarget.OfPlayer _ -> False
        AttackTarget.OfBattle _ -> False
      aimingAtJace :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimingAtJace who p = case p of
        Prompt.ChooseAttackTarget _ _ _ options -> case filter isPlaneswalker (NonEmpty.toList options) of
          target : _ -> target
          [] -> NonEmpty.head options
        _ -> attacking who p
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
   in Spec.describe s "Dethrone" $ do
        -- The proving test. bob is on 20 against alice's 15 and carol's 10, so the
        -- creature grows.
        Spec.it s "CR 702.105a whole card: attacking the player with the most life is a +1/+1 counter" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> Spec.assertEqWith s "2/1 plus one counter" (S.powerToughnessOf rev (atBlockers (attacking S.bob) (lives 15 20 10 gs))) (Just (3, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- The same board attacked the other way. carol is the LOWEST, so nothing
        -- triggers -- the falsifier for a condition that fired on any attack.
        Spec.it s "CR 702.105a attacking a player who is not on the most life does nothing" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> do
              let after = atBlockers (attacking S.carol) (lives 15 20 10 gs)
              Spec.assertEqWith s "CR 508.1b the Revolutionary really did attack carol" (Map.lookup rev (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlayer S.carol))
              Spec.assertEqWith s "and it is still a 2/1" (S.powerToughnessOf rev after) (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- Rule 702.105a says "the player with the most life", not "the opponent
        -- with the most life", so the attacker's OWN controller is compared too:
        -- alice on 25 makes bob's 20 not the most, and the same attack that grew
        -- the creature above now does nothing.
        Spec.it s "CR 702.105a the attacking creature's controller counts as a player" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> do
              let after = atBlockers (attacking S.bob) (lives 25 20 10 gs)
              Spec.assertEqWith s "CR 508.1b the Revolutionary really did attack bob" (Map.lookup rev (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlayer S.bob))
              Spec.assertEqWith s "and alice is ahead, so it is still a 2/1" (S.powerToughnessOf rev after) (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- "Or tied for most life": bob and carol both on 20, and attacking either
        -- triggers. The falsifier is a strict comparison, which fires on neither.
        Spec.it s "CR 702.105a a tie for most life still triggers" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> Spec.assertEqWith s "tied at 20 against alice's 15" (S.powerToughnessOf rev (atBlockers (attacking S.carol) (lives 15 20 20 gs))) (Just (3, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- CR 702.105a names THE PLAYER, and CR 508.1b lets a creature attack a
        -- planeswalker instead. The defending player is bob either way, so this is
        -- the case that separates reading Combat.attackers from reading the
        -- declaration event's CR 508.5 field.
        Spec.it s "CR 508.1b attacking the leader's planeswalker is not attacking the leader" $ do
          (gs, ours, theirs, _) <- jaceBoard
          case (ours, theirs) of
            ([rev], [_, jaceId]) -> do
              let after = atBlockers (aimingAtJace S.bob) (lives 15 20 10 gs)
              Spec.assertEqWith s "CR 508.1b the Revolutionary really did attack Jace" (Map.lookup rev (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlaneswalker jaceId))
              Spec.assertEqWith s "and no player was attacked, so it is still a 2/1" (S.powerToughnessOf rev after) (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Revolutionary and bob a Jace"
        -- CR 702.105b: two instances are two abilities, so two triggers and two
        -- counters. Asserted at the mint, no card in the pool having dethrone twice.
        Spec.it s "CR 702.105b each instance triggers separately" $ do
          Spec.assertEqWith s "dethrone held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Dethrone 2)) [Keyword.dethrone, Keyword.dethrone]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Dethrone 1)) [Keyword.dethrone]

-- CR 508.3a's second sentence, the first condition to read WHOM a declaration
-- attacked from a bystander's seat rather than from the attacker's.
--
-- Marchesa's Decree {3}{B} Enchantment is the card: "whenever a creature attacks
-- you or a planeswalker you control, that creature's controller loses 1 life".
-- CR 508.5/508.5a make that one test on GameEvent.AttackerDeclared's defending
-- player, so the condition never reads the board.
--
-- THREE SEATS, and load-bearing: at two players the defending player, "an
-- opponent" and the attacking creature's controller are all one seat, so a
-- condition that fired on every declaration and a payload that hit the wrong
-- player would both pass. alice is active and attacks, bob holds the Decree, and
-- carol is the second opponent whose leg separates the two readings.
--
-- Every leg reads all three life totals, since "that creature's controller"
-- (alice) and CR 109.5's "you" (bob) are different seats on this board.
--
-- TWO attackers, because CR 508.3a's arity is per declared attacker: one life per
-- creature, not one per declaration (CR 508.3b, gap #538).
marchesasDecreeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
marchesasDecreeSpec s registry =
  let board = do
        decree <- S.printingOf s registry "Marchesa's Decree"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat [piker, piker] [decree] [])
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        _ -> S.aggressiveAnswer p
      -- The same board with the declaration itself declined, which is the leg
      -- that separates "a creature attacked you" from "the step began".
      standingStill :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      standingStill who p = case p of
        Prompt.DeclareAttackers {} -> []
        _ -> attacking who p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "Marchesa's Decree" $ do
        -- The proving test. Two of alice's Pikers attack bob, so the Decree fires
        -- twice and ALICE is down 2 -- bob, whose enchantment it is, loses nothing.
        Spec.it s "CR 508.3a/508.5 whole card: each creature attacking you costs its controller 1 life" $ do
          (gs, _, _, _) <- board
          Spec.assertEqWith s "alice lost 1 per attacker, bob and carol nothing" (lives (atBlockers (attacking S.bob) gs)) (Just 18, Just 20, Just 20)
        -- The same declaration aimed at the other opponent. CR 508.5 makes carol
        -- the defending player, so the Decree is silent -- the falsifier for a
        -- condition that fired on any declaration, which a two-seat board cannot
        -- see.
        Spec.it s "CR 508.5 a creature attacking the other opponent does not trigger it" $ do
          (gs, ours, _, _) <- board
          let after = atBlockers (attacking S.carol) gs
          case ours of
            piker : _ -> Spec.assertEqWith s "CR 508.1b the Piker really did attack carol" (Map.lookup piker (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlayer S.carol))
            _ -> Spec.assertFailure s "fixture should give alice two Goblin Pikers"
          Spec.assertEqWith s "and nobody lost life" (lives after) (Just 20, Just 20, Just 20)
        -- No declaration at all, on the same board and against the same defending
        -- player: the falsifier for a condition that fired on the step rather than
        -- on CR 508.1a's declaration.
        Spec.it s "CR 508.3a a declare attackers step with no attackers triggers nothing" $ do
          (gs, _, _, _) <- board
          let after = atBlockers (standingStill S.bob) gs
          Spec.assertEqWith s "nothing was declared" (Combat.Type.attackers (GameState.combat after)) Map.empty
          Spec.assertEqWith s "and nobody lost life" (lives after) (Just 20, Just 20, Just 20)

-- CR 702.23 rampage, whose rule text is a triggered ability,
-- and the first whose bonus multiplies a printed N by a number read off the
-- board.
--
-- Wolverine Pack {2}{G}{G} Creature -- Wolverine 2/4 is the card: rampage 2 and
-- nothing else. Its numbers are chosen so no two readings of rule 702.23a agree
-- -- an asymmetric 2/4 base, and N = 2 rather than 1, so "+N per blocker" (6/8 at
-- two blockers), "+1 per blocker beyond the first" (3/5) and the rule's own
-- reading (4/6) are three different pairs.
--
-- Horrible Hordes {3} Artifact Creature -- Spirit 2/2, rampage 1, is the second
-- producer and is what pins N: the same three blockers give it +2/+2 where the
-- Pack gets +4/+4.
--
-- Every reading but the last is taken at the COMBAT DAMAGE step, before damage is
-- dealt -- CR 509.2a puts the trigger on the stack in the declare blockers step,
-- so the bonus is already applied there.
rampageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rampageSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
   in Spec.describe s "Rampage" $ do
        -- The proving test: two blockers is one beyond the first, so rampage 2 is
        -- +2/+2 and the 2/4 Pack is a 4/6.
        Spec.it s "CR 702.23a whole card: Wolverine Pack blocked by two creatures is 4/6" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker", "Hill Giant"]
          case mine of
            [pack] -> Spec.assertEqWith s "2/4 plus one blocker beyond the first, twice" (S.powerToughnessOf pack (atDamage gs)) (Just (4, 6))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- A THIRD blocker is a second creature beyond the first, so the bonus
        -- doubles rather than growing by one. The falsifier is a bonus that adds 1
        -- per creature beyond the first instead of N, which reads 4/6 here.
        Spec.it s "CR 702.23a a third blocker is a second +2/+2" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker", "Hill Giant", "Icehide Golem"]
          case mine of
            [pack] -> Spec.assertEqWith s "2/4 plus two beyond the first, twice" (S.powerToughnessOf pack (atDamage gs)) (Just (6, 8))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- "BEYOND THE FIRST": one blocker is a trigger with a bonus of 0, not a
        -- bonus of N. The falsifier is a bonus counting blockers outright, which
        -- reads 4/6 here.
        Spec.it s "CR 702.23a one blocker leaves the Pack a 2/4" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker"]
          case mine of
            [pack] -> Spec.assertEqWith s "the first blocker is not beyond the first" (S.powerToughnessOf pack (atDamage gs)) (Just (2, 4))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- CR 509.3c is the event, so an UNBLOCKED attacker never triggers at all.
        -- Asserted on the LOG and not on power and toughness, which cannot tell
        -- the two apart: a trigger that fired with no blockers would be +0/+0 and
        -- leave the same 2/4. The blocked leg is the same board with the block
        -- taken, so the pair differs in nothing but CR 509.1's declaration.
        Spec.it s "CR 509.3c an unblocked Pack never triggers" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker"]
          case mine of
            [pack] -> do
              let fired after = elem (GameEvent.AbilityTriggered (AbilityTriggered.MkAbilityTriggered pack S.alice TriggerCondition.SelfBecomesBlocked)) (S.eventsOf after)
              Spec.assertBool s (not (fired (S.runToStep (Phase.Combat CombatStep.CombatDamage) noBlocks gs))) "nothing blocked, so nothing triggered"
              Spec.assertBool s (fired (atDamage gs)) "and the same board with the block taken does trigger"
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- N is the card's, not the engine's: rampage 1 against the same three
        -- blockers is +2/+2 where rampage 2 was +4/+4.
        Spec.it s "CR 702.23a rampage 1 on the same board is half the bonus" $ do
          (gs, mine, _) <- board ["Horrible Hordes"] ["Goblin Piker", "Hill Giant", "Icehide Golem"]
          case mine of
            [hordes] -> Spec.assertEqWith s "2/2 plus two beyond the first, once" (S.powerToughnessOf hordes (atDamage gs)) (Just (4, 4))
            _ -> Spec.assertFailure s "fixture should give alice one Hordes"
        -- CR 702.23b: the bonus is calculated as the ability RESOLVES and does not
        -- move afterwards. CR 511.3 clears Combat.blockers at end of combat, so a
        -- bonus that re-read the declaration live would fall back to +0/+0 the
        -- moment combat ended, while the printed duration runs to end of turn. The
        -- Pack is a 6/8 taking 7, so it survives to be read.
        Spec.it s "CR 702.23b the bonus outlives the blockers it was counted from" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker", "Hill Giant", "Icehide Golem"]
          case mine of
            [pack] -> do
              let after = S.runToStep Phase.PostcombatMain S.aggressiveAnswer gs
              Spec.assertEqWith s "CR 511.3 the declaration is cleared" (Combat.Type.blockers (GameState.combat after)) Map.empty
              Spec.assertEqWith s "and the Pack is still a 6/8" (S.powerToughnessOf pack after) (Just (6, 8))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- CR 702.23c: each instance triggers separately. Asserted at the MINT, no
        -- printing in the pool carrying rampage twice, and the second assertion is
        -- what puts N inside the ability rather than beside it.
        Spec.it s "CR 702.23c each instance triggers separately" $ do
          Spec.assertEqWith s "rampage 2 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Rampage 2) 2)) [Keyword.rampage 2, Keyword.rampage 2]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Rampage 2) 1)) [Keyword.rampage 2]
          Spec.assertBool s (Keyword.rampage 1 /= Keyword.rampage 2) "and the two differ, so N is in the ability"

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
  selfBlocksCreatureSpec s registry
  selfBecomesBlockedSpec s registry
  selfAttacksUnblockedSpec s registry
  frenzySpec s registry
  bushidoSpec s registry
  flankingSpec s registry
  exaltedSpec s registry
  mentorSpec s registry
  mentorsTriggerSpec s registry
  trainingSpec s registry
  saviorOfOllenbockSpec s registry
  decayedSpec s registry
  provokeSpec s registry
  trygonPredatorSpec s registry
  questingBeastSpec s registry
  larcenySpec s registry
  evolveSpec s registry
  krasisSpec s registry
  renownSpec s registry
  arborColossusSpec s registry
  vanishingSpec s registry
  fadingSpec s registry
  modularSpec s registry
  tovolarSpec s registry
  aragornSpec s registry
  shroofusSpec s registry
  afflictSpec s registry
  meleeSpec s registry
  dethroneSpec s registry
  marchesasDecreeSpec s registry
  rampageSpec s registry
