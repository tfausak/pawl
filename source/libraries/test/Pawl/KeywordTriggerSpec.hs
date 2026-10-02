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
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Event.Binding as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection.View
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Soulbond as Soulbond
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
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
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
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Subtype as Subtype
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
              bindings = Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing Map.empty (ObjectId.MkObjectId 0) S.alice (TriggerCondition.SelfDealsCombatDamageToPlayer PlayerRelation.AnyPlayer) ev
          Spec.assertEqWith s "bob is bound under thatPlayer" (Binding.targetsOf bindings) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.bob)))
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
  let -- alice fields Slivdrazi Monstrosity and a Slaughter Drone; bob and carol
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
          let bindings = Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing Map.empty (ObjectId.MkObjectId 0) S.alice (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime) (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared (ObjectId.MkObjectId 7) S.carol (AttackTarget.OfPlayer S.carol) 1 S.alice))
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
prowessSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
prowessSpec s _ =
  Spec.describe s "Prowess" $ do
    -- CR 702.108b: "If a creature has multiple instances of prowess, each
    -- triggers separately." Asked of the mint rather than of a board, as
    -- battle cry's is: no card in this pool prints prowess twice and nothing
    -- here grants it, so a second instance is unreachable through play.
    Spec.it s "CR 702.108b each instance of prowess is its own ability" $ do
      Spec.assertEqWith s "prowess held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Prowess 2)) [Keyword.prowess, Keyword.prowess]
      Spec.assertEqWith s "and held once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Prowess 1)) [Keyword.prowess]

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
selfBlocksSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBlocksSpec s registry =
  let -- Attacks with everything and declines every block. aggressiveAnswer's
      -- control leg: the same game with CR 509.1's declaration switched off, and
      -- the only difference between the two answerers.
      noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
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
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
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

-- CR 509.3c: "Whenever [a creature] becomes blocked, . . ." -- the ATTACKING
-- side of the same declaration selfBlocksSpec reads, matched against
-- GameEvent.AttackerBlocked.
--
-- Sacred Prey {G} Creature -- Horse 1/1, "Whenever this creature becomes blocked,
-- you gain 1 life", is the card: the cheapest producer in the pool, and its
-- payload names nothing about the blockers, so these cases isolate the
-- CONDITION. The gain lands on the ATTACKING seat (alice), which is the seat
-- combat damage never moves here, so every number below is the trigger's alone.
selfBecomesBlockedSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
selfAttacksUnblockedSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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

  -- CR 613.1f reaching a spell on the stack: Imoti, Celebrant of Bounty
  -- {3}{G}{U} Legendary Creature -- Snake Druid 3/1 -- "Cascade / Spells you
  -- cast with mana value 6 or greater have cascade." (Oracle text checked
  -- 2026-09-30). Durkwood Baloth {4}{G}{G} prints no cascade and has mana value
  -- 6 exactly, so the grant's "6 or greater" is met at its edge. The two boards
  -- differ in Imoti alone; the cast trigger is read off the spell's projection
  -- rather than its printed face.
  Spec.it s "CR 613.1f Imoti grants a six-drop spell cascade" $ do
    imoti <- S.printingOf s registry "Imoti, Celebrant of Bounty"
    baloth <- S.printingOf s registry "Durkwood Baloth"
    piker <- S.printingOf s registry "Goblin Piker"
    think <- S.printingOf s registry "Think Twice"
    forest <- S.printingOf s registry "Forest"
    let board withImoti =
          let base = Setup.emptyGame S.bothPlayers
              (_, g1) = S.addLibraryCard think S.alice base
              (_, g2) = S.addLibraryCard piker S.alice g1
              g3 = foldr (\_ g -> snd (S.addPermanent forest S.alice g)) g2 [1 :: Int .. 6]
              g4 = if withImoti then snd (S.addPermanent imoti S.alice g3) else g3
              (_, g5) = S.addHandCard baloth S.alice g4
           in g5
                { GameState.activePlayer = S.alice,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.alice
                }
        isIn zone name gs = elem (CardName.MkCardName (Text.pack name)) (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone S.alice gs))
        without = S.runPure cascading (board False) Engine.priorityLoop
    Spec.assertBool s (isIn Zone.Battlefield "Goblin Piker" (S.runPure cascading (board True) Engine.priorityLoop)) "with Imoti, the Baloth's granted cascade cast the Goblin Piker free"
    Spec.assertEqWith s "without Imoti, the Baloth resolved with no cascade and the Piker stayed in the library" (isIn Zone.Battlefield "Durkwood Baloth" without, isIn Zone.Library "Goblin Piker" without) (True, True)

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
stormSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
stormSpec s _ = Spec.describe s "Storm" $ do
  Spec.it s "CR 702.40a storm is minted for a spell on the stack and nowhere else" $ do
    Spec.assertEqWith s "the stack roster mints it" (Keyword.stackTriggeredAbilitiesOf (Map.singleton Keyword.Type.Storm 1)) [Keyword.storm]
    Spec.assertEqWith s "and the battlefield roster does not" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Storm 1)) []

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
replicateSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
replicateSpec s _ = Spec.describe s "Replicate" $ do
  Spec.it s "CR 702.56a replicate's trigger is minted for a spell on the stack and nowhere else" $ do
    let keyword = Keyword.Type.Replicate (Cost.MkCost Nothing [])
    Spec.assertEqWith s "the stack roster mints one" (length (Keyword.stackTriggeredAbilitiesOf (Map.singleton keyword 1))) 1
    Spec.assertEqWith s "and the battlefield roster mints none" (Keyword.triggeredAbilitiesOf (Map.singleton keyword 1)) []

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

  -- CR 702.153a / 613.1f: Silverquill, the Disputant's "each instant and sorcery
  -- spell you cast has casualty 1" is a keyword the spell HAS, so Lightning Bolt,
  -- which prints none, is offered the sacrifice at CR 601.2b and copied by the
  -- cast trigger (Cost.spellKeywords). Two boards differing in ONE answer: alice
  -- sacrifices Jedit Ojanen, a 5/5, and bob takes the copy's 3 beside the
  -- original's; unpaid, the original's 3 alone.
  Spec.it s "CR 702.153a a granted casualty copies Lightning Bolt when paid; unpaid, it resolves once" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    silverquill <- S.printingOf s registry "Silverquill, the Disputant"
    let (_, withSilverquill) = S.addPermanent silverquill S.alice (S.landsFor mountain S.alice 1 (Setup.emptyGame S.bothPlayers))
        (jeditId, withJedit) = S.addPermanent jedit S.alice withSilverquill
        (spellId, g1) = S.addHandCard bolt S.alice withJedit
        board =
          g1
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        answer :: Natural.Natural -> Prompt.Prompt r -> r
        answer times p = case p of
          Prompt.ChooseSacrifices _ _ _ candidates _ _ -> Set.fromList (filter (== jeditId) candidates)
          _ -> paidTimesAt times (Recipient.ToPlayer S.bob) p
        step times gs action = snd (Engine.runGamePure (answer times) gs (action >> Engine.settleForPriority))
        resolveAll times gs = if null (GameState.stack gs) then gs else resolveAll times (step times gs Stack.resolveTop)
        after :: Natural.Natural -> GameState.GameState
        after times = resolveAll times (step times board (S.cast S.alice spellId))
    Spec.assertEqWith s "bob took the original's 3 and the copy's 3" (S.lifeOf S.bob (after 1)) (Just 14)
    Spec.assertEqWith s "and Jedit was the sacrifice, Silverquill still standing" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Jedit Ojanen")) S.alice (after 1), S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Silverquill, the Disputant")) S.alice (after 1)) (0, 1)
    Spec.assertEqWith s "CR 603.4 unpaid, the original's 3 alone beside Jedit" (S.lifeOf S.bob (after 0), S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Jedit Ojanen")) S.alice (after 0)) (Just 17, 1)

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
gravestormSpec :: (Monad m) => Spec.Spec m n -> n ()
gravestormSpec s = Spec.describe s "Gravestorm" $ do
  Spec.it s "CR 702.69a gravestorm is minted for a spell on the stack and nowhere else" $ do
    Spec.assertEqWith s "the stack roster mints it" (Keyword.stackTriggeredAbilitiesOf (Map.singleton Keyword.Type.Gravestorm 1)) [Keyword.gravestorm]
    Spec.assertEqWith s "and the battlefield roster does not" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Gravestorm 1)) []

-- CR 702.144a's demonstrate, on Incarnation Technique {4}{B} Sorcery --
-- "Demonstrate / Mill five cards, then return a creature card from your
-- graveyard to the battlefield." (Oracle text checked on Scryfall, 2026-09-18).
--
-- THREE SEATS, so that "choose an opponent" is a choice rather than an elision
-- (Pawl.Engine.PlayerEffect elides a one-candidate ChoosePlayer): alice picks
-- bob, and carol is the seat that shows the pick was read rather than assumed.
--
-- ONE creature card in each player's graveyard, each a different name, and
-- libraries of Swamps so the mill adds none: which card each resolution returns
-- is then forced, and WHOSE graveyard was consulted is the whole of what the
-- board says.
demonstrateSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
demonstrateSpec s _ = Spec.describe s "Demonstrate" $ do
  Spec.it s "CR 702.144a demonstrate is minted for a spell on the stack and nowhere else" $ do
    Spec.assertEqWith s "the stack roster mints it" (Keyword.stackTriggeredAbilitiesOf (Map.singleton Keyword.Type.Demonstrate 1)) [Keyword.demonstrate]
    Spec.assertEqWith s "and the battlefield roster does not" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Demonstrate 1)) []

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
conspireSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
conspireSpec s _ = Spec.describe s "Conspire" $ do
  Spec.it s "CR 702.78a conspire's trigger is minted for a spell on the stack and nowhere else" $ do
    Spec.assertEqWith s "the stack roster mints one" (length (Keyword.stackTriggeredAbilitiesOf (Map.singleton Keyword.Type.Conspire 1))) 1
    Spec.assertEqWith s "and the battlefield roster mints none" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Conspire 1)) []

-- CR 601.2b's optional additional cost answered `times` times, with every target
-- prompt -- the spell's own and CR 707.10c's per-copy offer -- pinned to one
-- recipient: data/scenarios/cast's paid-times answer crossed with `pinTarget`
-- below.
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

-- `paysFor` with ONE held card cast the moment CR 117.1 offers its holder
-- priority, aimed at the named permanent. The cast happens exactly once without
-- the answerer counting: the card leaves the hand, so no later round offers it.
--
-- The target is FILTERED out of the offered set rather than built, S.preferring's
-- posture: an answerer that searched for some other legal recipient -- Lightning
-- Bolt reaches a player too -- would repair the assertion a mutation broke.
respondingWith :: ObjectId.ObjectId -> ObjectId.ObjectId -> PlayerId.PlayerId -> Prompt.Prompt r -> r
respondingWith inHand victim who p = case p of
  Prompt.ChooseAction _ _ actions -> Maybe.fromMaybe A.Pass (List.find (S.isCastOf inHand) actions)
  Prompt.ChooseTargets _ _ _ offered -> S.preferring (\recipient -> Recipient.objectOf recipient == Just victim) offered
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
        -- CR 702.30a prints no "if this permanent is on the battlefield" -- that
        -- clause is rule 702.24a's, and cumulativeUpkeep's -- so a Jaguar bolted
        -- in response to its own echo trigger still resolves it and still owes
        -- alice the choice CR 118.12a's "unless" puts to her. CR 608.2a re-reads
        -- the "if" against CR 608.2h's record, which is where the clock CR 400.7
        -- deleted with the permanent still is.
        --
        -- ONE thing apart from the first leg above: bob holds a Bolt and casts
        -- it. alice's three Forests, her window and her answer are unchanged.
        Spec.it s "CR 608.2a a Jaguar killed in response still asks its controller the cost" $ do
          (oid, gs0) <- jaguarBoard 3
          mountain <- S.printingOf s registry "Mountain"
          bolt <- S.printingOf s registry "Lightning Bolt"
          let (held, staged) = S.addHandCard bolt S.bob (S.landsFor mountain S.bob 1 gs0)
              ((_, after), log') = ranUpkeep (respondingWith held oid S.alice) S.alice staged
          Spec.assertBool s (not (S.onBattlefield oid after)) "the Bolt really killed the Jaguar before its echo resolved"
          Spec.assertEqWith s "CR 702.30a a Forest of alice's paid echo for a permanent already gone" (S.tappedCount S.alice after) 1
          Spec.assertEqWith s "she being offered it exactly once" (length (payResponses log')) 1
        -- CR 603.3a fixes the ability's controller when it triggered, so rule
        -- 702.30a's "you" is alice at BOTH reads of the "if" however control moves
        -- in between -- and the window the clock holds for her is untouched by
        -- bob's coming to control the Jaguar, which opens one of his own beside it.
        --
        -- Ray of Command rather than a kill, so the source is still there and the
        -- only thing that moved is the seat: the leg above already covers a source
        -- that is gone.
        Spec.it s "CR 603.3a control moving in response still asks the player whose ability it is" $ do
          (oid, gs0) <- jaguarBoard 3
          island <- S.printingOf s registry "Island"
          ray <- S.printingOf s registry "Ray of Command"
          let (held, staged) = S.addHandCard ray S.bob (S.landsFor island S.bob 4 gs0)
              ((_, after), log') = ranUpkeep (respondingWith held oid S.alice) S.alice staged
          Spec.assertEqWith s "the Ray really took the Jaguar" (Projection.View.controllerOf oid after) (Just S.bob)
          Spec.assertEqWith s "CR 702.30a a Forest of alice's paid echo all the same" (S.tappedCount S.alice after) 1
          Spec.assertEqWith s "she, and not the thief, being offered it" (length (payResponses log')) 1
          Spec.assertEqWith s "and bob spent nothing but the Ray's four Islands" (S.tappedCount S.bob after) 4
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
-- you gain 2 life". Every exploit printing pairs the keyword with an
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
      -- Takes rule 702.110a's offer, sacrifices the first offered creature the
      -- predicate admits -- the Sadist is on the battlefield too, so a fixture
      -- taking the first would prove nothing about which creature the choice
      -- reached -- and aims rule 702.110b's trigger at bob.
      exploiting :: (ObjectId.ObjectId -> Bool) -> Prompt.Prompt r -> r
      exploiting wanted p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChoosePermanent _ _ _ offered ->
          Maybe.fromMaybe (NonEmpty.head offered) (List.find wanted (NonEmpty.toList offered))
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
            (pikerId, withPiker) = S.addPermanent piker S.alice base
            (giantId, withGiant) = S.addPermanent giant S.alice withPiker
            (_, staged) = S.spellOnStack sadist S.alice withGiant
        pure (pikerId, giantId, staged)
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
          let after = played (exploiting (== giantId)) staged
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
        -- The same board, differing in the victim: the Sadist sacrifices ITSELF,
        -- and CR 603.10a's look-back at the sacrifice still fires its trigger
        -- (the Colonel Autumn ruling).
        Spec.it s "CR 702.110b exploiting itself still fires the printed exploits trigger" $ do
          (pikerId, giantId, staged) <- sadistBoard
          let after = played (exploiting (\oid -> oid /= pikerId && oid /= giantId)) staged
          Spec.assertEqWith s "CR 603.10a bob lost 2 life to the self-exploit's trigger" (S.lifeOf S.bob after) (Just 18)
          Spec.assertEqWith s "CR 702.110a the Sadist itself was sacrificed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Qarsi Sadist")) S.alice after) 0
          Spec.assertBool s (S.onBattlefield giantId after) "and the Hill Giant stayed"

-- CR 702.72 champion, whose rule is a PAIR of triggered abilities linked through
-- the exile pile (CR 702.72b, CR 607.2k): "When this permanent enters, sacrifice
-- it unless you exile another [object] you control" and "When this permanent
-- leaves the battlefield, return the exiled card to the battlefield under its
-- owner's control."
--
-- Wanderwine Prophets {4}{U}{U} Creature -- Merfolk Wizard 4\/4 is the printing,
-- "Champion a Merfolk" plus a combat-damage trigger that trades a Merfolk for an
-- extra turn. Only the champion half is driven here; the extra turn is the card's
-- own printed text and not rule 702.72's.
--
-- TWO BOARDS DIFFERING IN THE QUALITY ALONE: alice controls a Goblin Piker and
-- two more creatures, which are Merfolk on one board and not on the other. The
-- Piker is on both, so "the Piker was not exiled" is what says the [object]
-- filter narrowed rather than the entry ability exiling whatever it found.
--
-- The champion'd Merfolk is BOB's card under alice's control, so rule 702.72a's
-- "under its owner's control" cannot collapse onto the controller that exiled it.
championSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
championSpec s registry =
  let prophetsName = CardName.MkCardName $ Text.pack "Wanderwine Prophets"
      abolisherName = CardName.MkCardName $ Text.pack "Razorfin Abolisher"
      pikerName = CardName.MkCardName $ Text.pack "Goblin Piker"
      -- Takes rule 702.72a's offer and exiles the NAMED candidate, which is the
      -- second one offered -- a fixture taking the first would prove nothing
      -- about which permanent the choice reached.
      exiling :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      exiling victim p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChoosePermanent _ _ _ offered ->
          Maybe.fromMaybe (NonEmpty.head offered) (List.find (== victim) (NonEmpty.toList offered))
        _ -> S.identityAnswer p
      -- alice's Goblin Piker, alice's `first`, and bob's `second` under alice's
      -- control; the Prophets on the stack over them.
      prophetsBoard first second = do
        prophets <- S.printingOf s registry "Wanderwine Prophets"
        piker <- S.printingOf s registry "Goblin Piker"
        firstP <- S.printingOf s registry first
        secondP <- S.printingOf s registry second
        let base = Setup.emptyGame S.bothPlayers
            (_, withPiker) = S.addPermanent piker S.alice base
            (_, withFirst) = S.addPermanent firstP S.alice withPiker
            (secondId, withSecond) = S.addPermanent secondP S.bob withFirst
            lent = S.giveControl secondId S.alice withSecond
            (spell, staged) = S.spellOnStack prophets S.alice lent
        pure (spell, secondId, staged)
      -- The spell resolves, rule 702.72a's entry trigger is placed (CR 603.3) and
      -- resolves.
      played :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      played answer staged =
        S.runPure answer staged (Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop)
      namesIn zone pid gs =
        Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs)
      battlefieldNamed name pid gs =
        List.find (\oid -> fmap Face.name (Game.faceOf oid gs) == Just name) (Game.zoneMembers Zone.Battlefield pid gs)
   in Spec.describe s "Champion" $ do
        -- The proving case: the Merfolk alice named goes to exile and the Prophet
        -- stays, which is rule 702.72a's "unless" taken.
        Spec.it s "CR 702.72a exiling another Merfolk keeps the Prophet on the battlefield" $ do
          (_, abolisherId, staged) <- prophetsBoard "Tidal Warrior" "Razorfin Abolisher"
          let after = played (exiling abolisherId) staged
          Spec.assertEqWith s "CR 702.72a the Prophet is on the battlefield" (S.countOnBattlefieldByName prophetsName S.alice after) 1
          Spec.assertBool s (notElem prophetsName (namesIn Zone.Graveyard S.alice after)) "and not in the graveyard"
          Spec.assertEqWith s "CR 702.72a the Merfolk alice named is in its owner's exile" (namesIn Zone.Exile S.bob after) [abolisherName]
          Spec.assertBool s (Maybe.isNothing (battlefieldNamed abolisherName S.bob after)) "and off the battlefield"
          -- The other Merfolk she did not name, and the Piker the [object] filter
          -- never offered, both stay: one exile, and a narrowed one.
          Spec.assertEqWith s "the Tidal Warrior she did not name stayed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Tidal Warrior")) S.alice after) 1
          Spec.assertEqWith s "and the Goblin Piker, which is no Merfolk, stayed" (S.countOnBattlefieldByName pikerName S.alice after) 1
        -- THE PAIR. The same board with the two companions' creature types
        -- changed and nothing else: with no other Merfolk there is nothing to
        -- exile, so rule 702.72a's sacrifice is what happens.
        Spec.it s "CR 702.72a with no other Merfolk to exile the Prophet is sacrificed" $ do
          (_, wolvesId, staged) <- prophetsBoard "Hill Giant" "Russet Wolves"
          let after = played (exiling wolvesId) staged
          Spec.assertEqWith s "CR 702.72a the Prophet is in its owner's graveyard" (filter (== prophetsName) (namesIn Zone.Graveyard S.alice after)) [prophetsName]
          Spec.assertEqWith s "and not on the battlefield" (S.countOnBattlefieldByName prophetsName S.alice after) 0
          Spec.assertEqWith s "CR 702.72a nothing was exiled" (namesIn Zone.Exile S.bob after) []
          Spec.assertEqWith s "the Russet Wolves alice would have exiled stayed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Russet Wolves")) S.bob after) 1
          Spec.assertEqWith s "and so did the Goblin Piker" (S.countOnBattlefieldByName pikerName S.alice after) 1
        -- CR 612.1 / 613.7 through the MINT: two Artificial Evolutions ({U}
        -- Instant, "Change the text of target spell or permanent by replacing
        -- all instances of one creature type with another. The new creature
        -- type can't be Wall." -- checked against Scryfall 2026-09-24) on the
        -- Prophet, Merfolk -> Elf and then Elf -> Merfolk. The keyword ends
        -- Champion a Merfolk, and the entry ability rule 702.72a mints from it
        -- has to say Merfolk too, not the Elf a first-match lookup of the raw
        -- swaps gives.
        Spec.it s "CR 613.7 two Evolutions on Wanderwine Prophets champion a Merfolk" $ do
          island <- S.printingOf s registry "Island"
          prophets <- S.printingOf s registry "Wanderwine Prophets"
          evolution <- S.printingOf s registry "Artificial Evolution"
          let (prophetId, b0) = S.addPermanent prophets S.alice (S.landsInPlay island 2)
              (b1, firstId) = S.handOne evolution b0
              (b2, secondId) = S.handOne evolution b1
              evolving :: (Subtype.Subtype, Subtype.Subtype) -> Prompt.Prompt r -> r
              evolving swap p = case p of
                Prompt.ChooseTargets _ _ _ offers -> S.preferring ((== Just prophetId) . Recipient.objectOf) offers
                Prompt.ChooseCreatureTypeSwap {} -> swap
                _ -> S.identityAnswer p
              toElf = (Subtype.Merfolk, Subtype.Elf)
              toMerfolk = (Subtype.Elf, Subtype.Merfolk)
              first = S.runPure (evolving toElf) b2 (S.cast S.alice firstId >> Stack.resolveTop)
              both = S.runPure (evolving toMerfolk) first (S.cast S.alice secondId >> Stack.resolveTop)
              championOf quality = Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Champion (Filter.Type.HasSubtype quality)) 1)
              mintedOn gs = Projection.mintedTriggeredAbilitiesOf (Projection.project prophetId gs)
          Spec.assertBool s (mintedOn both == championOf Subtype.Merfolk) "CR 613.7 the minted entry ability exiles another Merfolk"
          -- The anti-vacuity check, after the behaviour: the first Evolution
          -- alone really moved the ability onto Elves.
          Spec.assertBool s (mintedOn first == championOf Subtype.Elf) "after the first Evolution alone it exiles another Elf"

        -- Rule 702.72a's SECOND ability, off the first's board: the Prophet dies
        -- and the linked pile empties back onto the battlefield -- under BOB's
        -- control, he owning the card alice championed.
        Spec.it s "CR 702.72a the Prophet leaving returns the exiled card under its owner's control" $ do
          (_, abolisherId, staged) <- prophetsBoard "Tidal Warrior" "Razorfin Abolisher"
          let championed = played (exiling abolisherId) staged
          case battlefieldNamed prophetsName S.alice championed of
            Nothing -> Spec.assertFailure s "the Prophet did not reach the battlefield"
            Just prophetId -> do
              let after =
                    S.runPure
                      S.identityAnswer
                      championed
                      (Event.changeZone prophetId Zone.Graveyard >> Engine.settleForPriority >> Stack.resolveTop)
              case battlefieldNamed abolisherName S.bob after of
                Nothing -> Spec.assertFailure s "CR 702.72a the championed Merfolk did not return to the battlefield"
                Just returned ->
                  Spec.assertEqWith s "CR 702.72a it returned under its OWNER's control, not alice's" (Projection.View.controllerOf returned after) (Just S.bob)
              Spec.assertEqWith s "and the exile pile is empty" (namesIn Zone.Exile S.bob after) []

-- CR 702.58 graft, whose rule is a static ability and a triggered one: "graft N"
-- means "this permanent enters with N +1/+1 counters on it" and "whenever another
-- creature enters, if this permanent has a +1/+1 counter on it, you may move a
-- +1/+1 counter from this permanent onto that creature."
--
-- Two printings, and the pair is what makes rule 702.58a's clauses observable:
--
--   * Llanowar Reborn (Land: "This land enters tapped. {T}: Add {G}. Graft 1")
--     bears the ability while being no creature itself, so the counter it hands
--     over cannot come back to it and its own power and toughness cannot absorb
--     the move;
--   * Simic Initiate ({G} Creature -- Human Mutant 0/0, "Graft 1" and nothing
--     else) is the ENTRANT, and being a graft permanent itself is what tests
--     "ANOTHER creature": its own entry must not fire its own ability.
--
-- (Names, costs, type lines, P/T and Oracle text checked against
-- api.scryfall.com 2026-09-17; each card is transcribed whole.)
--
-- Every permanent ENTERS rather than being placed: S.addPermanent would stock the
-- counter by fixture and leave rule 702.58a's CR 614.1c row unproven.
--
-- Numbers all distinct: the graft is 1, the Goblin Piker a printed 2/1, the Hill
-- Giant a 3/3, and the Initiate a 0/0 that holds 2 counters once the move lands.

-- CR 702.95 soulbond, whose rule 702.95a is two triggered abilities, rule 702.95c
-- and rule 702.95d a gate on the pairing, and rule 702.95e three endings.
--
-- Wolfir Silverheart {3}{G}{G} Creature -- Wolf Warrior 4\/4 is the printing:
-- soulbond, plus "As long as this creature is paired with another creature, each
-- of those creatures gets +4\/+4" -- the clause that makes the pairing OBSERVABLE,
-- with nothing else on the card. (Name, cost, type line, P\/T and Oracle text
-- checked against api.scryfall.com 2026-09-20; the card is transcribed whole.)
--
-- Numbers all distinct, so that no two readings coincide: the Wolfir is 4\/4 and
-- 8\/8 paired, the Hill Giant 3\/3 and 7\/7 paired, and the Goblin Piker a 2\/1
-- that is never paired at all. The Piker is on every board, which is what says
-- the grant reached the PAIR rather than every creature alice controls -- and
-- being a second candidate, it also keeps rule 702.95a's choice a real one rather
-- than a prompt the engine could elide.
soulbondSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulbondSpec s registry =
  let wolfirName = CardName.MkCardName $ Text.pack "Wolfir Silverheart"
      giantName = CardName.MkCardName $ Text.pack "Hill Giant"
      -- Rule 702.95a's "you may", taken, and its partner named by identity. Pinned
      -- to the Giant rather than to the first option offered, so an answerer
      -- cannot repair a mutation by finding whatever is legal.
      pairingWith :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      pairingWith partner p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChoosePermanent _ _ _ offered ->
          Maybe.fromMaybe (NonEmpty.head offered) (List.find (== partner) (NonEmpty.toList offered))
        _ -> S.identityAnswer p
      -- Rule 702.95a's second ability offers the "may" and nothing else, the
      -- entrant being the partner already.
      taking :: Prompt.Prompt r -> r
      taking p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      onBattlefield name gs =
        List.find (\oid -> fmap Face.name (Game.faceOf oid gs) == Just name) (Game.zoneMembers Zone.Battlefield S.alice gs)
      -- alice's Hill Giant and Goblin Piker, with the Wolfir on the stack over
      -- them: rule 702.95a's FIRST ability is what fires when it enters.
      wolfirEntering = do
        wolfir <- S.printingOf s registry "Wolfir Silverheart"
        giant <- S.printingOf s registry "Hill Giant"
        piker <- S.printingOf s registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            (giantId, withGiant) = S.addPermanent giant S.alice base
            (pikerId, withPiker) = S.addPermanent piker S.alice withGiant
            (_, staged) = S.spellOnStack wolfir S.alice withPiker
        pure (giantId, pikerId, staged)
      -- The spell resolves, rule 702.95a's trigger is placed (CR 603.3) and
      -- resolves.
      played :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      played answer staged =
        S.runPure answer staged (Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
   in Spec.describe s "Soulbond" $ do
        -- The proving case for rule 702.95a's first ability and for the grant the
        -- pairing turns on.
        Spec.it s "CR 702.95a pairing on its own entry gives both creatures +4/+4" $ do
          (giantId, pikerId, staged) <- wolfirEntering
          let after = played (pairingWith giantId) staged
          case onBattlefield wolfirName after of
            Nothing -> Spec.assertFailure s "the Wolfir did not reach the battlefield"
            Just wolfirId -> do
              Spec.assertEqWith s "CR 702.95a the Wolfir is 8/8 while paired" (S.powerToughnessOf wolfirId after) (Just (8, 8))
              Spec.assertEqWith s "CR 702.95a and the creature alice paired it with is 7/7" (S.powerToughnessOf giantId after) (Just (7, 7))
              -- The other candidate, which the choice did not reach: the grant is
              -- the pair's and not every creature's.
              Spec.assertEqWith s "the Goblin Piker alice did not name is its printed 2/1" (S.powerToughnessOf pikerId after) (Just (2, 1))
        -- Rule 702.95e's third ending, off the same board: the partner leaves and
        -- the pairing goes with it, which the Wolfir's own size reports.
        Spec.it s "CR 702.95e the partner leaving the battlefield unpairs the Wolfir" $ do
          (giantId, _, staged) <- wolfirEntering
          let paired = played (pairingWith giantId) staged
          case onBattlefield wolfirName paired of
            Nothing -> Spec.assertFailure s "the Wolfir did not reach the battlefield"
            Just wolfirId -> do
              let after = S.runPure S.identityAnswer paired (Event.changeZone giantId Zone.Graveyard >> Engine.settleForPriority)
              Spec.assertEqWith s "CR 702.95e the Wolfir is its printed 4/4 again" (S.powerToughnessOf wolfirId after) (Just (4, 4))
              Spec.assertEqWith s "CR 702.95e and it is paired with nothing" (Game.lookupObject wolfirId after >>= Object.paired) Nothing
        -- Rule 702.95a's SECOND ability: the Wolfir is already on the battlefield
        -- and unpaired when another creature alice controls enters, so the entrant
        -- is the partner with nothing to choose.
        Spec.it s "CR 702.95a another creature entering pairs with the Wolfir" $ do
          wolfir <- S.printingOf s registry "Wolfir Silverheart"
          giant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          let base = Setup.emptyGame S.bothPlayers
              (wolfirId, withWolfir) = S.addPermanent wolfir S.alice base
              (pikerId, withPiker) = S.addPermanent piker S.alice withWolfir
              (_, staged) = S.spellOnStack giant S.alice withPiker
              after = played taking staged
          Spec.assertEqWith s "CR 702.95a the Wolfir is 8/8 while paired" (S.powerToughnessOf wolfirId after) (Just (8, 8))
          case onBattlefield giantName after of
            Nothing -> Spec.assertFailure s "the Hill Giant did not reach the battlefield"
            Just giantId -> Spec.assertEqWith s "CR 702.95a and the creature that entered is 7/7" (S.powerToughnessOf giantId after) (Just (7, 7))
          Spec.assertEqWith s "the Goblin Piker, which was on the battlefield already, is its printed 2/1" (S.powerToughnessOf pikerId after) (Just (2, 1))
        -- CR 603.4's SECOND check, which is about the PROMPT rather than the
        -- board: the condition is re-read as the ability resolves, and an entrant
        -- alice no longer controls makes it false, so the ability leaves the stack
        -- without asking her anything. Soulbond.pair would refuse the pairing
        -- either way (CR 702.95c), so the board cannot tell the two apart -- only
        -- the count of "you may" asks can.
        --
        -- A pair of boards differing in exactly one thing, whether bob takes the
        -- entrant while the trigger waits. Counted through State rather than a
        -- pure answerer, which cannot report what it was asked.
        Spec.it s "CR 603.4 an entrant alice no longer controls is not asked about" $ do
          wolfir <- S.printingOf s registry "Wolfir Silverheart"
          giant <- S.printingOf s registry "Hill Giant"
          let counting :: Prompt.Prompt r -> State.State Int r
              counting p = case p of
                Prompt.ChooseOptional {} -> do
                  State.modify' (+ 1)
                  pure OptionalDecision.Exercises
                _ -> pure (S.identityAnswer p)
              gs0 = Setup.emptyGame S.bothPlayers
              (wolfirId, withWolfir) = S.addPermanent wolfir S.alice gs0
              (_, staged) = S.spellOnStack giant S.alice withWolfir
              -- The Giant has entered and rule 702.95a's second trigger is on the
              -- stack, unresolved.
              waiting = S.runPure S.identityAnswer staged (Stack.resolveTop >> Engine.settleForPriority)
              asks board = State.execState (Engine.runGame counting board (Stack.resolveTop >> Engine.settleForPriority)) 0
          case onBattlefield giantName waiting of
            Nothing -> Spec.assertFailure s "the Hill Giant did not reach the battlefield"
            Just giantId -> do
              Spec.assertEqWith s "CR 603.4 bob having taken the entrant, alice is asked nothing" (asks (S.giveControl giantId S.bob waiting)) 0
              -- The same board with the entrant left alone: one real "may", which
              -- is what says the zero above is the condition failing and not a
              -- trigger that never fired.
              Spec.assertEqWith s "and with the entrant still hers, one" (asks waiting) 1
              Spec.assertEqWith s "CR 702.95c the Wolfir stays its printed 4/4 either way" (S.powerToughnessOf wolfirId (snd (State.evalState (Engine.runGame counting (S.giveControl giantId S.bob waiting) (Stack.resolveTop >> Engine.settleForPriority)) 0))) (Just (4, 4))
        -- CR 702.95b's "the creature another creature is paired with", asked of
        -- the partner: Flowering Lumberknot {3}{G} Creature -- Treefolk 5\/5, "This
        -- creature can't attack or block unless it's paired with a creature with
        -- soulbond" (Oracle checked against api.scryfall.com 2026-09-26). A pair of
        -- boards differing only in the partner: a Clone of bob's Wolfir, whose
        -- soulbond is a copiable value (CR 707.2), or alice's Hill Giant, which has
        -- none -- the partner a Wolfir has after losing its abilities, since CR
        -- 702.95e ends no pairing for that. The Clone stays on both boards, so
        -- neither "Lumberknot is paired" nor "alice controls a soulbond creature"
        -- separates them; and bob's Wolfir is paired with his Goblin Piker on both,
        -- so neither does "some soulbond creature is paired".
        Spec.it s "CR 702.95b Flowering Lumberknot fights only beside a partner with soulbond" $ do
          wolfir <- S.printingOf s registry "Wolfir Silverheart"
          clone <- S.printingOf s registry "Clone"
          lumberknot <- S.printingOf s registry "Flowering Lumberknot"
          giant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          let gs0 = Setup.emptyGame S.bothPlayers
              (wolfirId, withWolfir) = S.addPermanent wolfir S.bob gs0
              (pikerId, withPiker) = S.addPermanent piker S.bob withWolfir
              (lumberknotId, withLumberknot) = S.addPermanent lumberknot S.alice withPiker
              (giantId, withGiant) = S.addPermanent giant S.alice withLumberknot
              (_, staged) = S.spellOnStack clone S.alice withGiant
              -- The Clone copies bob's Wolfir, pinned by id, and declines its own
              -- soulbond "may": the pairing below is the one thing the boards vary.
              copying :: Prompt.Prompt r -> r
              copying p = case p of
                Prompt.ChooseCopyTarget _ _ _ legal -> List.find (== wolfirId) legal
                Prompt.ChooseOptional {} -> OptionalDecision.Declines
                _ -> S.identityAnswer p
              resolved = Soulbond.pair S.bob wolfirId pikerId (S.runPure copying staged (Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority))
              alices = Set.toList (Set.filter (\oid -> Projection.View.controllerOf oid resolved == Just S.alice) (GameState.battlefield resolved))
          case filter (`notElem` [lumberknotId, giantId]) alices of
            [cloneId] -> do
              let withClone = Soulbond.pair S.alice lumberknotId cloneId resolved
                  withGiantPartner = Soulbond.pair S.alice lumberknotId giantId resolved
              Spec.assertEqWith s "CR 508.1c paired with the Clone of the Wolfir it can attack" (Combat.canAttack S.alice lumberknotId withClone) True
              Spec.assertEqWith s "CR 508.1c paired with the Hill Giant it cannot" (Combat.canAttack S.alice lumberknotId withGiantPartner) False
              Spec.assertEqWith s "CR 509.1b and it can block beside the Clone and not beside the Giant" (fmap (Combat.canBlock S.alice lumberknotId) [withClone, withGiantPartner]) [True, False]
            other -> Spec.assertFailure s ("expected the Clone as alice's one new permanent, got " <> show (length other))

graftSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graftSpec s registry =
  let newestOnBattlefield name gs =
        Maybe.listToMaybe (reverse (filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just name) (Game.zoneMembers Zone.Battlefield S.alice gs)))
      plusOnes oid gs = Map.findWithDefault 0 CounterKind.PlusOnePlusOne (maybe Map.empty Object.counters (Game.lookupObject oid gs))
      -- Rule 702.58a's "you may", taken and declined. Pinned to the decision
      -- rather than searched for, so a mutation cannot be repaired by an answerer
      -- looking for a legal move.
      taking :: Prompt.Prompt r -> r
      taking p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      declining :: Prompt.Prompt r -> r
      declining p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Declines
        _ -> S.identityAnswer p
      -- alice's hand holds the two creatures and a Plains; the Reborn has already
      -- entered, so rule 702.58a's replacement has run and its counter is on the
      -- land rather than put there by hand.
      grafted = do
        land <- S.printingOf s registry "Llanowar Reborn"
        plains <- S.printingOf s registry "Plains"
        piker <- S.printingOf s registry "Goblin Piker"
        giant <- S.printingOf s registry "Hill Giant"
        initiate <- S.printingOf s registry "Simic Initiate"
        let (landCard, g0) = S.addHandCard land S.alice (Setup.emptyGame S.bothPlayers)
            (pikerCard, g1) = S.addHandCard piker S.alice g0
            (giantCard, g2) = S.addHandCard giant S.alice g1
            (plainsCard, g3) = S.addHandCard plains S.alice g2
            (initiateCard, g4) = S.addHandCard initiate S.alice g3
            entered = S.runPure S.identityAnswer g4 (Event.changeZone landCard Zone.Battlefield)
            -- RE-FOUND rather than tracked: CR 400.7 makes the permanent a NEW
            -- object, so the hand card's id names nothing on the battlefield. The
            -- Reborn is the only permanent there, and the first assertion below
            -- reads its counter, so a wrong id fails loudly rather than quietly.
            landId = Maybe.fromMaybe landCard (Maybe.listToMaybe (Game.zoneMembers Zone.Battlefield S.alice entered))
        pure (landId, pikerCard, giantCard, plainsCard, initiateCard, entered)
      -- The entrant arrives, CR 603.3 places what it triggered, and the ability
      -- resolves. `placed` stops one step short, where the CR 603.4 "if" has
      -- already decided whether anything reached the stack at all.
      placed :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      placed answer oid gs = S.runPure answer gs (Event.changeZone oid Zone.Battlefield >> Engine.settleForPriority)
      entersUnder :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      entersUnder answer oid gs = S.runPure answer (placed answer oid gs) Stack.resolveTop
   in Spec.describe s "Graft" $ do
        -- The proving case, and both halves of rule 702.58a at once: the land came
        -- in carrying the counter, and the counter leaves it for the creature that
        -- entered.
        Spec.it s "CR 702.58a the land enters with its counter and hands it to an entering creature" $ do
          (landId, pikerCard, _, _, _, board) <- grafted
          Spec.assertEqWith s "CR 702.58a the land entered with one +1/+1 counter" (plusOnes landId board) 1
          let after = entersUnder taking pikerCard board
          case newestOnBattlefield (CardName.MkCardName $ Text.pack "Goblin Piker") after of
            Nothing -> Spec.assertFailure s "the Goblin Piker did not reach the battlefield"
            Just pikerId -> do
              Spec.assertEqWith s "CR 702.58a the counter is on the creature that entered" (plusOnes pikerId after) 1
              Spec.assertEqWith s "CR 702.58a and off the land it was moved from" (plusOnes landId after) 0
              Spec.assertEqWith s "so the printed 2/1 is a 3/2" (Projection.powerOf pikerId after, Projection.toughnessOf pikerId after) (Just 3, Just 2)
        -- THE PAIR ON THE COUNTER. The same board once the counter has gone: rule
        -- 702.58a's intervening "if" has nothing to find, so a second creature
        -- entering takes nothing and the first keeps what it has.
        Spec.it s "CR 603.4 with no counter left a second creature entering takes nothing" $ do
          (landId, pikerCard, giantCard, _, _, board) <- grafted
          let once = entersUnder taking pikerCard board
              after = entersUnder taking giantCard once
          case (newestOnBattlefield (CardName.MkCardName $ Text.pack "Goblin Piker") after, newestOnBattlefield (CardName.MkCardName $ Text.pack "Hill Giant") after) of
            (Just pikerId, Just giantId) -> do
              Spec.assertEqWith s "CR 603.4 the Hill Giant got no counter" (plusOnes giantId after) 0
              Spec.assertEqWith s "so it is the printed 3/3" (Projection.powerOf giantId after, Projection.toughnessOf giantId after) (Just 3, Just 3)
              Spec.assertEqWith s "the Goblin Piker kept the one it took" (plusOnes pikerId after) 1
              Spec.assertEqWith s "and the land is still empty" (plusOnes landId after) 0
              -- The proxy, after the behaviour: the "if" is checked as the
              -- trigger would be PLACED, so nothing reached the stack to decline.
              Spec.assertEqWith s "CR 603.4 no trigger was placed at all" (length (GameState.stack (placed taking giantCard once))) 0
            _ -> Spec.assertFailure s "both creatures should have reached the battlefield"
        -- THE PAIR ON THE ANSWER. The first board with alice's one decision
        -- changed and nothing else: rule 702.58a's "you may" declined leaves the
        -- counter where it was.
        Spec.it s "CR 702.58a declining the may leaves the counter on the land" $ do
          (landId, pikerCard, _, _, _, board) <- grafted
          let after = entersUnder declining pikerCard board
          case newestOnBattlefield (CardName.MkCardName $ Text.pack "Goblin Piker") after of
            Nothing -> Spec.assertFailure s "the Goblin Piker did not reach the battlefield"
            Just pikerId -> do
              Spec.assertEqWith s "CR 702.58a the creature got nothing" (plusOnes pikerId after) 0
              Spec.assertEqWith s "and the land kept its counter" (plusOnes landId after) 1
              Spec.assertEqWith s "the trigger still reached the stack -- a declined may is not a fizzle" (length (GameState.stack (placed declining pikerCard board))) 1
        -- THE PAIR ON THE ENTRANT'S TYPE. The first board with a LAND entering
        -- instead of a creature: rule 702.58a says "another CREATURE".
        Spec.it s "CR 702.58a a land entering does not fire it" $ do
          (landId, _, _, plainsCard, _, board) <- grafted
          let after = entersUnder taking plainsCard board
          Spec.assertEqWith s "CR 702.58a the land kept its counter" (plusOnes landId after) 1
          Spec.assertEqWith s "and nothing was placed" (length (GameState.stack (placed taking plainsCard board))) 0
        -- "ANOTHER" doing real work. The entrant is itself a graft permanent, so
        -- two abilities see one entry: the Reborn's fires, and the Initiate's own
        -- does not, it being no "another" to itself. The counters cannot tell
        -- those apart -- CR 122.5 forbids a move from an object to itself either
        -- way -- so the count of what alice was asked is the discriminator, and
        -- the board's reading of it is the 2/2.
        Spec.it s "CR 702.58a an entering graft permanent does not trigger its own ability" $ do
          (landId, _, _, _, initiateCard, board) <- grafted
          let after = entersUnder taking initiateCard board
          Spec.assertEqWith s "CR 702.58a exactly one ability triggered" (length (GameState.stack (placed taking initiateCard board))) 1
          case newestOnBattlefield (CardName.MkCardName $ Text.pack "Simic Initiate") after of
            Nothing -> Spec.assertFailure s "the Simic Initiate did not reach the battlefield"
            Just initiateId -> do
              Spec.assertEqWith s "CR 702.58a it holds its own counter and the land's" (plusOnes initiateId after) 2
              Spec.assertEqWith s "so the printed 0/0 is a 2/2" (Projection.powerOf initiateId after, Projection.toughnessOf initiateId after) (Just 2, Just 2)
              Spec.assertEqWith s "and the land handed its one over" (plusOnes landId after) 0

-- CR 702.165a: "Backup N" means "When this creature enters, put N +1/+1
-- counters on target creature. If that's another creature, it also gains the
-- non-backup abilities of this creature printed below this one until end of
-- turn."
--
-- Archpriest of Shadows, {3}{B}{B} Creature -- Human Warlock 4/4, is the
-- producer, and it is the one that makes BOTH halves of the grant observable:
-- what it prints below its backup line is a KEYWORD (deathtouch) and a
-- TRIGGERED ABILITY (its combat-damage reanimation), which travel through two
-- different arms of Pawl.Engine.Resolve.Effect's expandGrant. (Name, cost, type
-- line, P/T and Oracle text checked against api.scryfall.com 2026-09-20.)
--
-- Goblin Piker ({1}{R} Creature -- Goblin 2/1, no abilities) is the recipient,
-- so every keyword and every ability it shows below is one backup gave it.
--
-- Jedit Ojanen ({4}{G}{G} Creature -- Cat Warrior 5/5, no abilities) is bob's
-- blocker, and its toughness is what makes DEATHTOUCH the discriminator rather
-- than the +1/+1 counter: neither the printed 2 nor the backed-up 3 damage
-- destroys a 5/5 on its own.
--
-- Hill Giant ({3}{R} Creature -- Giant 3/3, no abilities) waits in alice's
-- graveyard as the thing the granted trigger returns.
--
-- Streetwise Negotiator ({1}{G} Creature -- Cat Citizen 0/2, "Backup 1 / This
-- creature assigns combat damage equal to its toughness rather than its
-- power", checked against api.scryfall.com 2026-09-23) grants a STATIC
-- ability. The backed-up Piker is a 3/2, so its toughness and its power are
-- different amounts of damage. Turn to Frog ({1}{U} Instant, loses all
-- abilities until end of turn) is the CR 613.1f removal whose timestamp,
-- before or after the grant's, decides whether the Piker keeps it.
--
-- Chomping Kavu ({3}{G} Creature -- Kavu 3/3, "Backup 1 / This creature can't
-- be blocked by creatures with power 2 or less", checked against
-- api.scryfall.com 2026-09-24) grants a RULE ability (CR 613.11). bob's Cabal
-- Evangel (2/2) is the blocker it bars and his War Mammoth (3/3) the one it
-- does not.
--
-- Saiba Cryptomancer ({1}{U} Creature -- Moonfolk Ninja 0/1, "Flash / Backup 1
-- / Hexproof", checked against api.scryfall.com 2026-09-26) prints a keyword on
-- EACH side of its backup line, so rule 702.165a's "printed below this one" is
-- what decides which one the Piker gains.
backupSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
backupSpec s registry =
  let pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      giantName = CardName.MkCardName (Text.pack "Hill Giant")
      -- Pinned by FILTERING the offered set rather than by building a
      -- Recipient, so CR 608.2b's re-read at resolution sees the recipient the
      -- prompt actually offered.
      targeting :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      targeting victim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, rs) -> Set.filter ((== Just victim) . Recipient.objectOf) rs) sets
        _ -> S.identityAnswer p
      -- The same pin, with combat switched on, for the granted trigger's own
      -- target -- the card in the graveyard, not the creature that connected.
      attacking :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      attacking victim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, rs) -> Set.filter ((== Just victim) . Recipient.objectOf) rs) sets
        _ -> S.aggressiveAnswer p
      plusOnes oid gs = Map.findWithDefault 0 CounterKind.PlusOnePlusOne (maybe Map.empty Object.counters (Game.lookupObject oid gs))
      -- The Archpriest ENTERS rather than being placed: rule 702.165a's ability
      -- is a CR 603.6a entry trigger, so a fixture that put the permanent there
      -- would prove nothing. CR 603.3 places what the entry triggered and the
      -- ability then resolves.
      entersTargeting :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      entersTargeting victim card gs =
        S.runPure (targeting victim) gs (Event.changeZone card Zone.Battlefield >> Engine.settleForPriority >> Stack.resolveTop)
      -- alice attacks with everything, then CR 509.1b's question: may bob's
      -- `blocker` alone block `attacker`?
      attacked gs = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice))
      mayBlock blocker attacker gs = Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton attacker)) (attacked gs)
   in Spec.describe s "Backup" $ do
        -- THE KEYWORD HALF, at gameplay level. The Piker connects with a 5/5 and
        -- destroys it, which only deathtouch (CR 702.2b) can do at three damage.
        Spec.it s "CR 702.165a the backed-up creature kills a 5/5 with the granted deathtouch" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          jedit <- S.printingOf s registry "Jedit Ojanen"
          archpriest <- S.printingOf s registry "Archpriest of Shadows"
          case S.combatBoardOf [piker] [jedit] of
            (gs0, [pikerId], [jeditId]) -> do
              let (card, staged) = S.addHandCard archpriest S.alice gs0
                  backed = entersTargeting pikerId card staged
                  after = S.runCombat S.aggressiveAnswer backed
              Spec.assertBool s (not (S.onBattlefield jeditId after)) "CR 702.165a the 5/5 blocker was destroyed by the granted deathtouch"
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Deathtouch pikerId backed) "and the Piker did hold deathtouch before damage"
              Spec.assertEqWith s "with rule 702.165a's one +1/+1 counter on it" (plusOnes pikerId backed) 1
            _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Jedit Ojanen"
        -- THE PAIR, differing in one thing: rule 702.165a's target. Aimed at the
        -- Archpriest itself the "if that's another creature" is false, so the
        -- Piker is the printed 2/1 with no deathtouch and the 5/5 lives.
        Spec.it s "CR 702.165a aiming the trigger at itself grants nothing" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          jedit <- S.printingOf s registry "Jedit Ojanen"
          archpriest <- S.printingOf s registry "Archpriest of Shadows"
          case S.combatBoardOf [piker] [jedit] of
            (gs0, [pikerId], [jeditId]) -> do
              let (card, staged) = S.addHandCard archpriest S.alice gs0
                  -- RE-FOUND after the entry, CR 400.7 having made the permanent
                  -- a new object; the hand card's id names nothing to target.
                  placed = S.runPure S.identityAnswer staged (Event.changeZone card Zone.Battlefield)
                  selfId = Maybe.fromMaybe card (Maybe.listToMaybe (reverse (Game.zoneMembers Zone.Battlefield S.alice placed)))
                  backed = S.runPure (targeting selfId) placed (Engine.settleForPriority >> Stack.resolveTop)
                  after = S.runCombat S.aggressiveAnswer backed
              -- CR 702.165a's "if that's another creature" doing the work: the
              -- Archpriest already prints deathtouch, so a grant it made to
              -- ITSELF would be a SECOND instance (CR 613.1f counts them), which
              -- is the one reading of this board a relaxed condition produces.
              Spec.assertEqWith s "CR 702.165a the Archpriest holds its one printed deathtouch and no granted second" (Map.lookup Keyword.Type.Deathtouch (Projection.keywordsOf selfId backed)) (Just 1)
              Spec.assertBool s (not (Projection.hasKeyword Keyword.Type.Deathtouch pikerId backed)) "and the Piker gained nothing"
              Spec.assertBool s (S.onBattlefield jeditId after) "so the 5/5 blocker survived the three damage"
              Spec.assertEqWith s "and the counters went on the Archpriest itself" (plusOnes selfId backed) 1
            _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Jedit Ojanen"
        -- "PRINTED BELOW THIS ONE", the pair on one board: the hexproof below
        -- Saiba Cryptomancer's backup line travels, the flash above it does not.
        Spec.it s "CR 702.165a the Piker gains the hexproof printed below the backup line and not the flash above it" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          saiba <- S.printingOf s registry "Saiba Cryptomancer"
          case S.combatBoardOf [piker] [] of
            (gs0, [pikerId], _) -> do
              let (card, staged) = S.addHandCard saiba S.alice gs0
                  backed = entersTargeting pikerId card staged
              Spec.assertBool s (not (Projection.hasKeyword Keyword.Type.Flash pikerId backed)) "CR 702.165a the Piker did not gain the flash printed above the backup line"
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Hexproof Nothing) pikerId backed) "and did gain the hexproof printed below it"
              Spec.assertEqWith s "with rule 702.165a's one +1/+1 counter on it" (plusOnes pikerId backed) 1
            _ -> Spec.assertFailure s "fixture should give alice a Piker"
        -- THE ABILITY HALF, at gameplay level, and CR 113.7 with it: the granted
        -- trigger is the PIKER's, so "this creature" is the Piker and "your
        -- graveyard" is the Piker's controller's. The Archpriest never attacks,
        -- being summoning sick, so the reanimation is the copy's.
        Spec.it s "CR 702.165a the backed-up creature's combat damage fires the granted trigger" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          giant <- S.printingOf s registry "Hill Giant"
          archpriest <- S.printingOf s registry "Archpriest of Shadows"
          case S.combatBoardOf [piker] [] of
            (gs0, [pikerId], _) -> do
              let (giantCard, buried) = S.addGraveyardCard giant S.alice gs0
                  (card, staged) = S.addHandCard archpriest S.alice buried
                  backed = entersTargeting pikerId card staged
                  after = S.runCombat (attacking giantCard) backed
              Spec.assertEqWith s "CR 702.165a the granted trigger returned the Hill Giant to the battlefield" (S.countOnBattlefieldByName giantName S.alice after) 1
              Spec.assertEqWith s "the Piker's three damage reached bob" (S.lifeOf S.bob after) (Just 17)
              Spec.assertEqWith s "and the Piker is still there to have dealt it" (S.countOnBattlefieldByName pikerName S.alice after) 1
            _ -> Spec.assertFailure s "fixture should give alice a Piker"
        -- CR 702.165d, the source GONE: Murder kills the Archpriest with its
        -- trigger on the stack. The grant was fixed as the trigger was put there,
        -- so the Piker still gains deathtouch and still kills the 5/5.
        Spec.it s "CR 702.165d the Archpriest killed in response still grants what it had as its trigger was put on the stack" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          jedit <- S.printingOf s registry "Jedit Ojanen"
          archpriest <- S.printingOf s registry "Archpriest of Shadows"
          murder <- S.printingOf s registry "Murder"
          swamp <- S.printingOf s registry "Swamp"
          case S.combatBoardOf [piker] [jedit] of
            (gs0, [pikerId], [jeditId]) -> do
              let (card, staged) = S.addHandCard archpriest S.alice (S.landsFor swamp S.alice 3 gs0)
                  (murderId, armed) = S.addHandCard murder S.alice staged
                  before = Game.zoneMembers Zone.Battlefield S.alice armed
                  placed = S.runPure (targeting pikerId) armed (Event.changeZone card Zone.Battlefield >> Engine.settleForPriority)
                  archId = List.find (`notElem` before) (Game.zoneMembers Zone.Battlefield S.alice placed)
              case archId of
                Nothing -> Spec.assertFailure s "the Archpriest did not reach the battlefield"
                Just selfId -> do
                  let killed = S.runPure (targeting selfId) placed (S.cast S.alice murderId >> Stack.resolveTop)
                      backed = S.runPure S.identityAnswer killed Stack.resolveTop
                      after = S.runCombat S.aggressiveAnswer backed
                  Spec.assertBool s (not (S.onBattlefield jeditId after)) "CR 702.165d the 5/5 blocker was destroyed by the deathtouch the dead Archpriest granted"
                  Spec.assertBool s (Projection.hasKeyword Keyword.Type.Deathtouch pikerId backed) "and the Piker did hold deathtouch before damage"
                  Spec.assertBool s (not (S.onBattlefield selfId killed)) "the Murder really did kill the Archpriest before its trigger resolved"
                  Spec.assertEqWith s "with rule 702.165a's one +1/+1 counter on the Piker" (plusOnes pikerId backed) 1
            _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Jedit Ojanen"
        -- CR 113.7a, the source gone BEFORE the trigger is put on the stack: the
        -- Archpriest is destroyed after it enters but before CR 603.3 places its
        -- trigger (a destruction later in the same resolution stands for it), so
        -- the values fixed are its last known information.
        Spec.it s "CR 113.7a an Archpriest gone before its trigger is put on the stack grants off its last known information" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          jedit <- S.printingOf s registry "Jedit Ojanen"
          archpriest <- S.printingOf s registry "Archpriest of Shadows"
          case S.combatBoardOf [piker] [jedit] of
            (gs0, [pikerId], [jeditId]) -> do
              let (card, staged) = S.addHandCard archpriest S.alice gs0
                  before = Game.zoneMembers Zone.Battlefield S.alice staged
                  entered = S.runPure S.identityAnswer staged (Event.changeZone card Zone.Battlefield)
                  archId = List.find (`notElem` before) (Game.zoneMembers Zone.Battlefield S.alice entered)
              case archId of
                Nothing -> Spec.assertFailure s "the Archpriest did not reach the battlefield"
                Just selfId -> do
                  let backed = S.runPure (targeting pikerId) entered (Event.destroy Regenerability.Regenerable [selfId] >> Engine.settleForPriority >> Stack.resolveTop)
                      after = S.runCombat S.aggressiveAnswer backed
                  Spec.assertBool s (not (S.onBattlefield jeditId after)) "CR 113.7a the 5/5 blocker was destroyed by the deathtouch granted off the Archpriest's last known information"
                  Spec.assertBool s (Projection.hasKeyword Keyword.Type.Deathtouch pikerId backed) "and the Piker did hold deathtouch before damage"
                  Spec.assertBool s (not (S.onBattlefield selfId backed)) "the Archpriest really was gone"
                  Spec.assertEqWith s "with rule 702.165a's one +1/+1 counter on the Piker" (plusOnes pikerId backed) 1
            _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Jedit Ojanen"
        -- CR 702.165d, the source COPIED: Mirrorweave turns every other
        -- creature, the Archpriest among them, into a Goblin Piker with the
        -- trigger on the stack. The Archpriest's copiable values no longer carry
        -- deathtouch, but the grant was fixed before they changed.
        Spec.it s "CR 702.165d the Archpriest becoming a copy in response still grants what it had as its trigger was put on the stack" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          jedit <- S.printingOf s registry "Jedit Ojanen"
          archpriest <- S.printingOf s registry "Archpriest of Shadows"
          mirrorweave <- S.printingOf s registry "Mirrorweave"
          plains <- S.printingOf s registry "Plains"
          case S.combatBoardOf [piker] [jedit] of
            (gs0, [pikerId], _) -> do
              let (card, staged) = S.addHandCard archpriest S.alice (S.landsFor plains S.alice 5 gs0)
                  (weaveId, armed) = S.addHandCard mirrorweave S.alice staged
                  before = Game.zoneMembers Zone.Battlefield S.alice armed
                  placed = S.runPure (targeting pikerId) armed (Event.changeZone card Zone.Battlefield >> Engine.settleForPriority)
                  archId = List.find (`notElem` before) (Game.zoneMembers Zone.Battlefield S.alice placed)
              case archId of
                Nothing -> Spec.assertFailure s "the Archpriest did not reach the battlefield"
                Just selfId -> do
                  let woven = S.runPure (targeting pikerId) placed (S.cast S.alice weaveId >> Stack.resolveTop)
                      backed = S.runPure S.identityAnswer woven Stack.resolveTop
                  Spec.assertBool s (Projection.hasKeyword Keyword.Type.Deathtouch pikerId backed) "CR 702.165d the Piker gained the deathtouch the Archpriest had as its trigger was put on the stack"
                  Spec.assertBool s (not (Projection.hasKeyword Keyword.Type.Deathtouch selfId woven)) "the Mirrorweave really did make the Archpriest a Piker before its trigger resolved"
                  Spec.assertEqWith s "with rule 702.165a's one +1/+1 counter on the Piker" (plusOnes pikerId backed) 1
            _ -> Spec.assertFailure s "fixture should give alice a Piker"
        -- THE STATIC HALF, at gameplay level, and CR 113.7 with it: "this
        -- creature" in the granted ability is the Piker, whose toughness of 2
        -- is what reaches bob rather than its power of 3.
        Spec.it s "CR 702.165a the backed-up creature assigns combat damage equal to its toughness" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          negotiator <- S.printingOf s registry "Streetwise Negotiator"
          case S.combatBoardOf [piker] [] of
            (gs0, [pikerId], _) -> do
              let (card, staged) = S.addHandCard negotiator S.alice gs0
                  backed = entersTargeting pikerId card staged
                  after = S.runCombat S.aggressiveAnswer backed
              Spec.assertEqWith s "CR 702.165a the Piker dealt bob its toughness of 2" (S.lifeOf S.bob after) (Just 18)
              Spec.assertEqWith s "where its power was 3" (Projection.powerOf pikerId backed) (Just 3)
              Spec.assertEqWith s "and its toughness 2" (Projection.toughnessOf pikerId backed) (Just 2)
            _ -> Spec.assertFailure s "fixture should give alice a Piker"
        -- CR 613.1f in CR 613.7 timestamp order, as a pair differing only in
        -- which resolves first. A removal applied BEFORE the grant has nothing
        -- to remove yet, so the Piker has the ability; one applied after takes
        -- it away. Turn to Frog's 1/1 plus the counter is a 2/2, so the pair is
        -- read off the projected flag rather than the damage.
        Spec.it s "CR 613.1f only a removal later than the grant takes the granted static ability away" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          negotiator <- S.printingOf s registry "Streetwise Negotiator"
          frog <- S.printingOf s registry "Turn to Frog"
          island <- S.printingOf s registry "Island"
          case S.combatBoardOf [piker] [] of
            (gs0, [pikerId], _) -> do
              let (card, staged) = S.addHandCard negotiator S.alice (S.landsFor island S.alice 2 gs0)
                  (frogId, armed) = S.addHandCard frog S.alice staged
                  frogged g = S.runPure (targeting pikerId) g (S.cast S.alice frogId >> Stack.resolveTop)
                  frogFirst = entersTargeting pikerId card (frogged armed)
                  frogLast = frogged (entersTargeting pikerId card armed)
                  toughnessFlag g = PC.assignsCombatDamageWithToughness (Projection.project pikerId g)
              Spec.assertBool s (toughnessFlag frogFirst) "CR 613.1f the grant after Turn to Frog left the Piker holding it"
              Spec.assertBool s (not (toughnessFlag frogLast)) "CR 613.1f Turn to Frog after the grant took it away"
              Spec.assertEqWith s "the Frog really resolved first, leaving a 1/1 with the counter" (Projection.powerOf pikerId frogFirst) (Just 2)
              Spec.assertEqWith s "and last" (Projection.powerOf pikerId frogLast) (Just 2)
            _ -> Spec.assertFailure s "fixture should give alice a Piker"
        -- CR 305.7's last clause: Blood Moon makes the nonbasic Dryad Arbor a
        -- Mountain and strips its rules text, but not an ability another
        -- effect granted. Aspirant's Ascent makes the backed-up Arbor a 3/5
        -- flier, so its toughness and power are different damage.
        Spec.it s "CR 305.7 a Blood Moon'd Dryad Arbor keeps the static ability backup granted it" $ do
          arbor <- S.printingOf s registry "Dryad Arbor"
          negotiator <- S.printingOf s registry "Streetwise Negotiator"
          bloodMoon <- S.printingOf s registry "Blood Moon"
          ascent <- S.printingOf s registry "Aspirant's Ascent"
          island <- S.printingOf s registry "Island"
          case S.combatBoardOf [arbor] [] of
            (gs0, [arborId], _) -> do
              let (_, mooned) = S.addPermanent bloodMoon S.bob (S.landsFor island S.alice 1 gs0)
                  (card, staged) = S.addHandCard negotiator S.alice mooned
                  (ascentId, armed) = S.addHandCard ascent S.alice staged
                  backed = entersTargeting arborId card armed
                  -- The Island pays; the Arbor is left untapped to attack.
                  sparing :: Prompt.Prompt r -> r
                  sparing p = case p of
                    Prompt.ChooseManaSource _ _ candidates -> List.find (/= arborId) (NonEmpty.toList candidates)
                    Prompt.ChooseExtraManaSource {} -> Nothing
                    _ -> targeting arborId p
                  pumped = S.runPure sparing backed (S.cast S.alice ascentId >> Stack.resolveTop)
                  after = S.runCombat S.aggressiveAnswer pumped
              Spec.assertEqWith s "CR 305.7 the Arbor dealt bob its toughness of 5" (S.lifeOf S.bob after) (Just 15)
              Spec.assertEqWith s "where its power was 3" (Projection.powerOf arborId pumped) (Just 3)
              Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf arborId pumped)) "and Blood Moon really had made it a Mountain"
            _ -> Spec.assertFailure s "fixture should give alice a Dryad Arbor"
        -- CR 613.1f in CR 613.7 timestamp order, the static half's pair over a
        -- rule ability: a Turn to Frog before the grant leaves the Piker barring
        -- the Evangel, one after takes the restriction away. Either way the
        -- Piker is a 2/2.
        Spec.it s "CR 613.1f only a removal later than the grant takes the granted restriction away" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          evangel <- S.printingOf s registry "Cabal Evangel"
          kavu <- S.printingOf s registry "Chomping Kavu"
          frog <- S.printingOf s registry "Turn to Frog"
          island <- S.printingOf s registry "Island"
          case S.combatBoardOf [piker] [evangel] of
            (gs0, [pikerId], [evangelId]) -> do
              let (card, staged) = S.addHandCard kavu S.alice (S.landsFor island S.alice 2 gs0)
                  (frogId, armed) = S.addHandCard frog S.alice staged
                  frogged g = S.runPure (targeting pikerId) g (S.cast S.alice frogId >> Stack.resolveTop)
                  frogFirst = entersTargeting pikerId card (frogged armed)
                  frogLast = frogged (entersTargeting pikerId card armed)
              Spec.assertBool s (not (mayBlock evangelId pikerId frogFirst)) "CR 613.1f the grant after Turn to Frog left the Piker unblockable by the 2/2"
              Spec.assertBool s (mayBlock evangelId pikerId frogLast) "CR 613.1f Turn to Frog after the grant took the restriction away"
              Spec.assertEqWith s "the Frog really resolved first, leaving a 1/1 with the counter" (Projection.powerOf pikerId frogFirst) (Just 2)
              Spec.assertEqWith s "and last" (Projection.powerOf pikerId frogLast) (Just 2)
            _ -> Spec.assertFailure s "fixture should give alice a Piker and bob an Evangel"
        -- CR 305.7's last clause over a rule ability: Blood Moon strips the
        -- Dryad Arbor's rules text but not the restriction backup granted it.
        Spec.it s "CR 305.7 a Blood Moon'd Dryad Arbor keeps the restriction backup granted it" $ do
          arbor <- S.printingOf s registry "Dryad Arbor"
          evangel <- S.printingOf s registry "Cabal Evangel"
          kavu <- S.printingOf s registry "Chomping Kavu"
          bloodMoon <- S.printingOf s registry "Blood Moon"
          case S.combatBoardOf [arbor] [evangel] of
            (gs0, [arborId], [evangelId]) -> do
              let (_, mooned) = S.addPermanent bloodMoon S.bob gs0
                  (card, staged) = S.addHandCard kavu S.alice mooned
                  backed = entersTargeting arborId card staged
              Spec.assertBool s (not (mayBlock evangelId arborId backed)) "CR 305.7 the Arbor still can't be blocked by the 2/2"
              Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf arborId backed)) "and Blood Moon really had made it a Mountain"
            _ -> Spec.assertFailure s "fixture should give alice a Dryad Arbor"

-- CR 702.101a: "Extort is a triggered ability. 'Extort' means 'Whenever you cast
-- a spell, you may pay {W/B}. If you do, each opponent loses 1 life and you gain
-- life equal to the total life lost this way.'"
--
-- Syndic of Tithes, {1}{W} Creature -- Human Cleric 2/2, whose whole text is
-- extort, so nothing else on the card can move a life total.
--
-- Its gameplay legs are data/scenarios/keyword-trigger/cr-702-101a-*.json.
extortSpec :: (Monad m) => Spec.Spec m n -> n ()
extortSpec s = Spec.describe s "Extort" $ do
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
incrementSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
incrementSpec s _ =
  Spec.describe s "Increment" $ do
    -- CR 702.191b: "If a creature has multiple instances of increment, each
    -- one triggers separately." Asked of the mint, as rule 702.101b's is.
    Spec.it s "CR 702.191b each instance of increment is its own ability" $ do
      Spec.assertEqWith s "increment held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Increment 2)) [Keyword.increment, Keyword.increment]
      Spec.assertEqWith s "and held once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Increment 1)) [Keyword.increment]

-- CR 702.59a: "Recover is a triggered ability that functions only while the card
-- with recover is in a player's graveyard. 'Recover [cost]' means 'When a
-- creature is put into your graveyard from the battlefield, you may pay [cost].
-- If you do, return this card from your graveyard to your hand. Otherwise, exile
-- this card.'"
--
-- Sun's Bounty, {1}{W} Instant, "You gain 4 life." / "Recover {1}{W}" -- the
-- cheapest printing by machinery: its whole non-keyword text is one GainLife, so
-- nothing but recover is under test.
--
-- Its gameplay legs are data/scenarios/keyword-trigger/cr-702-59a-*.json.
recoverSpec :: (Monad m) => Spec.Spec m n -> n ()
recoverSpec s = Spec.describe s "Recover" $ do
  -- The roster, asked directly: rule 702.59a states no per-instance clause,
  -- so the graveyard mint reads the DISTINCT keywords and a card holding
  -- recover once contributes one ability.
  Spec.it s "CR 702.59a the graveyard roster mints it" $ do
    let cost = Cost.MkCost Nothing []
    Spec.assertEqWith s "recover is on the graveyard roster" (Keyword.graveyardTriggeredAbilitiesOf Set.empty (Set.singleton (Keyword.Type.Recover cost))) [Keyword.recover cost]
    Spec.assertEqWith s "and on none of the others" (Keyword.handTriggeredAbilitiesOf (Set.singleton (Keyword.Type.Recover cost)) <> Keyword.exileTriggeredAbilitiesOf (Set.singleton (Keyword.Type.Recover cost))) []

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  cascadeSpec s registry
  rippleSpec s registry
  stormSpec s registry
  replicateSpec s registry
  casualtySpec s registry
  gravestormSpec s
  conspireSpec s registry
  demonstrateSpec s registry
  echoSpec s registry
  exploitSpec s registry
  championSpec s registry
  soulbondSpec s registry
  graftSpec s registry
  backupSpec s registry
  poisonousSpec s registry
  ingestSpec s registry
  annihilatorSpec s registry
  battleCrySpec s registry
  prowessSpec s registry
  extortSpec s
  incrementSpec s registry
  recoverSpec s
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
partnerWithSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
  Prompt.ChooseOptional _ pid _ _ _ _ -> if pid == S.carol then OptionalDecision.Exercises else OptionalDecision.Declines
  Prompt.Search _ pid matches cap -> if pid == S.carol then List.genericTake cap matches else []
  _ -> S.identityAnswer p
