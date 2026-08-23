{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Damage and Pawl.Engine.Sba: the damage funnel, who the
-- funnel credits as a damage event's source, its deal-time riders (deathtouch,
-- infect, wither, toxic, lifelink), trample, state-based actions, and the
-- look-back read of the damage the turn's event log records
-- (Filter.DealtDamageThisTurn).
module Pawl.DamageSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- Fill every target slot with the candidate that NAMES `oid`, whatever tag the
-- pool produced for it, falling back to the set's minimum so the interpreter
-- stays total. PlaneswalkerSpec's `aimedAt` one spec over, and deliberately
-- tag-blind: the group below is about how many entries CR 115.4's pool has for
-- one permanent, so the fixture must not assert which tag it carries.
aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    S.preferring (\r -> Recipient.objectOf r == Just oid) sets
  _ -> S.identityAnswer p

-- aimedAt's player-shaped twin, for CR 120.1's fourth recipient: a slot's offer
-- is FILTERED rather than answered with a hand-built recipient, so a candidate
-- the engine never offered cannot be smuggled past CR 608.2b's re-read.
aimedAtPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimedAtPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    S.preferring (== Recipient.ToPlayer pid) sets
  _ -> S.identityAnswer p

-- The permanent of a given name on the battlefield, found by NAME because CR
-- 400.7 mints a new object as a spell resolves and the hand's id does not
-- survive the cast.
permanentNamed :: String -> GameState.GameState -> ObjectId.ObjectId
permanentNamed name gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack name))
   in case filter named (Set.toList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> S.noSource

-- CR 120.3: damage "may have one or more of the following results, depending on
-- ... the characteristics of the damage's recipient". The results are therefore
-- per CARD TYPE and not per recipient, so a permanent that is both a creature and
-- a planeswalker is owed CR 120.3c (that many loyalty counters come off) AND CR
-- 120.3e (that much damage is marked) from one damage event -- while CR 115.4
-- still offers it as ONE candidate, since that rule lists what may be chosen
-- rather than how many ways there are to choose it.
--
-- Four cards already in the pool compose the board, chained through CR 205.1b's
-- retention clause:
--
--   * Liquimetal Coating: "{T}: Target permanent becomes an artifact IN ADDITION
--     to its other types until end of turn" -- CR 205.1b's first sentence, so
--     Jace Beleren keeps the planeswalker card type while gaining artifact.
--   * March of the Machines: "Each noncreature artifact is an artifact creature
--     with power and toughness each equal to its mana value" -- CR 205.1b's third
--     sentence ("some effects state that an object becomes an 'artifact
--     creature'; these effects also allow the object to retain all of its prior
--     card types"), so the planeswalker type survives again. Jace's mana value is
--     3, so he is a 3/3.
--   * Prodigal Sorcerer: "{T}: Prodigal Sorcerer deals 1 damage to any target" is
--     both CR 115.4's pool and the damage.
--
-- The whole sequence stays inside one turn, because the Coating's effect is
-- UntilEndOfTurn.
--
-- ONE damage, never three. Jace's loyalty is 3 and March makes him a 3/3, so at 3
-- damage CR 120.3c alone buries him (CR 704.5i) and CR 120.3e alone destroys him
-- (CR 704.5g) -- an assertion that he died would pass with EITHER result
-- implemented and neither is what this pins. At 1 the two observables are
-- independent and neither is lethal.
--
-- The positives are pinned FIRST: "exactly one candidate names Jace" is a
-- negative that passes for free on a board where the animation chain silently did
-- nothing and Jace is a plain planeswalker.
creaturePlaneswalkerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
creaturePlaneswalkerSpec s registry =
  Spec.describe s "CreatureAndPlaneswalker"
    . Spec.it s "CR 120.3c/120.3e one ping at an animated Jace Beleren takes a loyalty counter AND marks a damage"
    $ do
      island <- S.printingOf s registry "Island"
      jace <- S.printingOf s registry "Jace Beleren"
      march <- S.printingOf s registry "March of the Machines"
      coating <- S.printingOf s registry "Liquimetal Coating"
      sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      let (handGs, jaceInHand) = S.handOne jace (S.landsInPlay island 3)
          -- Cast rather than arranged, so the three loyalty counters come from CR
          -- 306.5b's replacement and not from a fixture.
          board = S.runPure S.identityAnswer handGs (do S.cast S.alice jaceInHand; Stack.resolveTop)
          jaceId = permanentNamed "Jace Beleren" board
          (_, withMarch) = S.addCreature march S.alice board
          (coatingId, withCoating) = S.addCreature coating S.alice withMarch
          (sorcererId, withSorcerer) = S.addCreature sorcerer S.alice withCoating
          ready = withSorcerer {GameState.priority = Just S.alice}
      case (Face.activatedAbilities (S.combinedFace coating), Face.activatedAbilities (S.combinedFace sorcerer)) of
        (coat : _, ping : _) -> do
          let coated = S.runPure (aimedAt jaceId) ready (do Activate.activateAbility S.alice coatingId coat; Stack.resolveTop)
          -- The positives.
          Spec.assertBool s (Set.member CardType.Artifact (Projection.cardTypesOf jaceId coated)) "CR 205.1b: the Coating made Jace an artifact"
          Spec.assertBool s (Projection.isCreatureOf jaceId coated) "so March animates him"
          Spec.assertBool s (Set.member CardType.Planeswalker (Projection.cardTypesOf jaceId coated)) "and he is STILL a planeswalker"
          Spec.assertEqWith s "a 3/3, his mana value" (S.powerToughnessOf jaceId coated) (Just (3, 3))
          Spec.assertEqWith s "CR 306.5b: three loyalty counters" (S.counterOf CounterKind.Loyalty jaceId coated) 3
          Spec.assertEqWith s "and nothing marked on him yet" (S.damageOf jaceId coated) (Just 0)
          -- CR 115.4 offers a PERMANENT, not a card type. Read off Prodigal
          -- Sorcerer's own committed target slot rather than a hand-built one.
          let slot = SlotName.MkSlotName (Text.pack "target")
              offered = Map.lookup slot (Modal.allTargetSlots (ActivatedAbility.modal ping))
              namingJace = fmap (\targetSlot -> Set.filter (\r -> Recipient.objectOf r == Just jaceId) (Target.legalRecipients (Just S.alice) sorcererId targetSlot coated)) offered
          Spec.assertEqWith s "CR 115.4: Jace is one candidate, not one per card type" (fmap Set.size namingJace) (Just 1)
          -- Both of CR 120.3's results, off one damage.
          let pinged = S.runPure (aimedAt jaceId) coated (do Activate.activateAbility S.alice sorcererId ping; Stack.resolveTop)
          Spec.assertEqWith s "CR 120.3c: loyalty 3 -> 2" (S.counterOf CounterKind.Loyalty jaceId pinged) 2
          Spec.assertEqWith s "CR 120.3e: and one damage marked" (S.damageOf jaceId pinged) (Just 1)
          Spec.assertBool s (Set.member jaceId (GameState.battlefield (S.settleSba pinged))) "CR 704.5g/704.5i: neither result was lethal"
        _ -> Spec.assertFailure s "Liquimetal Coating and Prodigal Sorcerer should each print an activated ability"

creatureSbaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
creatureSbaSpec s registry =
  Spec.describe s "CreatureSba" $ do
    Spec.it s "CR 704.5g a creature with lethal damage is destroyed" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          after = S.settleSba (S.markDamage oid 1 gs)
      Spec.assertEqWith s "off the battlefield" (Game.zoneMembers Zone.Battlefield S.alice after) []
      Spec.assertEqWith s "in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

    Spec.it s "CR 704.5g damage below toughness is not lethal" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          -- A Piker is 2/1, so 0 marked damage is survivable and 1 is not.
          after = S.settleSba (S.markDamage oid 0 gs)
      Spec.assertEqWith s "still there" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 1

    Spec.it s "CR 704.5g a Mountain with damage marked is not destroyed" $ do
      -- Not a creature: 704.5f/g do not apply. This is the classification
      -- doing its job -- the check never asks WHICH card it is.
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 1
      case Game.zoneMembers Zone.Battlefield S.alice gs of
        [] -> Spec.assertFailure s "fixture should have one Mountain"
        oid : _ ->
          Spec.assertEqWith
            s
            "survives"
            (length (Game.zoneMembers Zone.Battlefield S.alice (S.settleSba (S.markDamage oid 5 gs))))
            1

    Spec.it s "a destroyed creature conserves objects" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          marked = S.markDamage oid 1 gs
      Spec.assertEqWith
        s
        "conserved"
        (Game.objectCount (S.settleSba marked))
        (Game.objectCount marked)

    -- The deterministic successor to the retired green-black "some seed sends a
    -- creature to the graveyard" property: two 2/1 Pikers trade in combat and
    -- both die to the CR 704.5g state-based action.
    Spec.it s "a creature dies in a played-out combat" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, _) = S.combatBoard piker 1 1
          after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
      Spec.assertEqWith s "attacker died" (S.creaturesInPlay S.alice after) 0
      Spec.assertEqWith s "blocker died" (S.creaturesInPlay S.bob after) 0

    Spec.it s "CR 704.5d a token off the battlefield ceases to exist" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          goblinCard = Printing.card piker
          (tokId, gs) = S.addToken goblinCard S.alice base
          inGrave = S.runPure S.identityAnswer gs (Event.changeZone tokId Zone.Graveyard)
          -- The changeZone minted a new incarnation; find it in the graveyard.
          settled = S.settleSba inGrave
      Spec.assertEqWith s "before the SBA, a token sits in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice inGrave)) 1
      Spec.assertEqWith s "after the SBA, it has ceased to exist" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 0
      Spec.assertEqWith s "no token objects remain" (Game.objectCount settled) 0

    Spec.it s "CR 704.5d a token on the battlefield does NOT cease" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          goblinCard = Printing.card piker
          (_, gs) = S.addToken goblinCard S.alice base
          settled = S.settleSba gs
      Spec.assertEqWith s "the token survives on the battlefield" (Game.objectCount settled) 1

    Spec.it s "CR 704.5d/704.5g a 1/1 token dies to lethal damage and ceases to exist" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          goblinCard = Printing.card piker
          -- A real 2/1 Piker (bob's) is the damage source; alice's 1/1 token takes 2.
          (srcId, gs1) = S.addCreature piker S.bob base
          (tokId, gs2) = S.addToken goblinCard S.alice gs1
          damaged = S.runPure S.identityAnswer gs2 (Damage.applyDamage [DamageEvent.MkDamageEvent srcId (Recipient.ToCreature tokId) 2 False False False 0 Nothing DamageKind.Combat])
          settled = S.settleSba damaged
      Spec.assertEqWith s "the token is gone from the battlefield" (S.creaturesInPlay S.alice settled) 0
      Spec.assertEqWith s "and NOT sitting in a graveyard (the falsifier)" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 0

    Spec.it s "CR 704.5g regeneration saves a creature from lethal combat damage" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          (victim, gs0) = S.addCreature piker S.alice base -- 2/1
          shielded = S.addRegenShield victim gs0
          -- 2 combat damage is lethal to a 2/1; the shield replaces the CR 704.5g destruction.
          damaged = S.runPure S.identityAnswer shielded (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False False 0 Nothing DamageKind.Combat])
          settled = S.settleSba damaged
      Spec.assertEqWith s "survived (regenerated)" (Set.member victim (GameState.battlefield settled)) True
      case Game.lookupObject victim settled of
        Just obj -> do
          Spec.assertEqWith s "tapped" (Object.tapped obj) TapState.Tapped
          Spec.assertEqWith s "damage healed" (Object.damage obj) 0
        Nothing -> Spec.assertFailure s "victim vanished"

damageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageSpec s registry =
  Spec.describe s "Damage" $ do
    Spec.it s "a permanent starts with no damage marked" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertEqWith s "none" (S.damageOf oid gs) (Just 0)

    Spec.it s "CR 514.2 marked damage is removed" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertEqWith s "removed" (S.damageOf oid (Damage.removeAllDamage (S.markDamage oid 1 gs))) (Just 0)

    Spec.it s "CR 514.2 damage wears off at the cleanup step" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          marked = S.markDamage oid 1 gs
          after = snd (Engine.runGamePure S.identityAnswer marked (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
      Spec.assertEqWith s "worn off" (S.damageOf oid after) (Just 0)

    Spec.it s "CR 400.7 a new object carries no damage forward" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          marked = S.markDamage oid 1 gs
          after = S.runPure S.identityAnswer marked (Event.changeZone oid Zone.Graveyard)
      case Game.zoneMembers Zone.Graveyard S.alice after of
        [] -> Spec.assertFailure s "expected a card in the graveyard"
        new : _ -> Spec.assertEqWith s "fresh object, no damage" (S.damageOf new after) (Just 0)

    Spec.it s "CR 615 a prevention drops combat damage but spares Noncombat" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          (victim, gs0) = S.addCreature piker S.alice base
          shield =
            ActiveReplacement.MkActiveReplacement
              { ActiveReplacement.effect = ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat) (Filter.Type.And []) Nothing Nothing Nothing) DamageRewrite.PreventAll Seq.empty),
                ActiveReplacement.source = victim,
                ActiveReplacement.controller = S.alice,
                ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                ActiveReplacement.expiry = Expiry.Type.AtCleanup,
                ActiveReplacement.uses = Uses.Unlimited,
                ActiveReplacement.origin = ReplacementOrigin.Other,
                ActiveReplacement.condition = Nothing,
                ActiveReplacement.rider = Nothing,
                ActiveReplacement.slots = Map.empty
              }
          withShield = S.addReplacement shield gs0
          combat = S.runPure S.identityAnswer withShield (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False False 0 Nothing DamageKind.Combat])
          spell = S.runPure S.identityAnswer withShield (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False False 0 Nothing DamageKind.Noncombat])
      Spec.assertEqWith s "combat damage prevented -- none marked" (S.damageOf victim combat) (Just 0)
      Spec.assertEqWith s "combat damage prevented -- no event recorded" (S.damageEventsOf combat) []
      Spec.assertEqWith s "noncombat damage still dealt" (S.damageOf victim spell) (Just 2)

    Spec.it s "CR 514.2 an until-end-of-turn replacement wears off at cleanup" $
      let base = Setup.emptyGame S.bothPlayers
          shield =
            ActiveReplacement.MkActiveReplacement
              { ActiveReplacement.effect = ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat) (Filter.Type.And []) Nothing Nothing Nothing) DamageRewrite.PreventAll Seq.empty),
                ActiveReplacement.source = ObjectId.MkObjectId 900,
                ActiveReplacement.controller = S.alice,
                ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                ActiveReplacement.expiry = Expiry.Type.AtCleanup,
                ActiveReplacement.uses = Uses.Unlimited,
                ActiveReplacement.origin = ReplacementOrigin.Other,
                ActiveReplacement.condition = Nothing,
                ActiveReplacement.rider = Nothing,
                ActiveReplacement.slots = Map.empty
              }
          dropped = Expiry.dropAtCleanup (S.addReplacement shield base)
       in Spec.assertEqWith s "no replacements remain" (GameState.replacements dropped) []

infectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
infectSpec s registry =
  Spec.describe s "Infect" $ do
    Spec.it s "CR 120.3b infect damage to a player becomes poison, not life loss" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False True False 0 Nothing DamageKind.Combat
          after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
      Spec.assertEqWith s "bob has three poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
      Spec.assertEqWith s "bob's life unchanged" (S.lifeOf S.bob after) (Just 20)
      Spec.assertEqWith s "the source's controller gains no poison" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0

    Spec.it s "CR 120.3d infect damage to a creature becomes -1/-1 counters, not marked damage" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (victim, gs1) = S.addCreature piker S.bob gs0
          ev = DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) 2 False True False 0 Nothing DamageKind.Combat
          after = S.runPure S.identityAnswer gs1 (Damage.applyDamage [ev])
      Spec.assertEqWith s "two -1/-1 counters" (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject victim after)) (Just 2)
      Spec.assertEqWith s "no marked damage" (S.damageOf victim after) (Just 0)

    -- CR 120.3c AND NOT CR 120.3d: rule 120.3d names a CREATURE recipient, so an
    -- infect source damaging a planeswalker takes loyalty counters off it like any
    -- other source and gives it no -1/-1 counters. The results are the recipient's
    -- card types (CR 120.3), never the source's keyword.
    --
    -- Jace Beleren is CAST rather than arranged, so his three loyalty counters come
    -- from CR 306.5b's replacement and the removal has something to take.
    Spec.it s "CR 120.3c infect damage to a planeswalker takes loyalty, not -1/-1 counters" $ do
      island <- S.printingOf s registry "Island"
      jace <- S.printingOf s registry "Jace Beleren"
      ichorRats <- S.printingOf s registry "Ichor Rats"
      let (handGs, jaceInHand) = S.handOne jace (S.landsInPlay island 3)
          board = S.runPure S.identityAnswer handGs (do S.cast S.alice jaceInHand; Stack.resolveTop)
          walker = permanentNamed "Jace Beleren" board
          (src, withRats) = S.addCreature ichorRats S.alice board
          ev = DamageEvent.MkDamageEvent src (Recipient.ToPlaneswalker walker) 2 False True False 0 Nothing DamageKind.Combat
          after = S.runPure S.identityAnswer withRats (Damage.applyDamage [ev])
      Spec.assertEqWith s "CR 306.5b: three loyalty counters to start" (S.counterOf CounterKind.Loyalty walker board) 3
      Spec.assertEqWith s "CR 120.3c: loyalty 3 -> 1" (S.counterOf CounterKind.Loyalty walker after) 1
      Spec.assertEqWith s "CR 120.3d reaches no planeswalker" (S.counterOf CounterKind.MinusOneMinusOne walker after) 0
      Spec.assertEqWith s "and CR 120.3e reaches none either" (S.damageOf walker after) (Just 0)

    Spec.it s "CR 702.90 Glistener Elf poisons an unblocked player, drains no life" $ do
      glistenerElf <- S.printingOf s registry "Glistener Elf"
      let (gs, _, _) = S.combatBoardOf [glistenerElf] []
          after = S.fightWith S.aggressiveAnswer gs
      Spec.assertEqWith s "bob has one poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 1
      Spec.assertEqWith s "bob's life unchanged" (S.lifeOf S.bob after) (Just 20)
      Spec.assertEqWith s "alice (controller) has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0

    Spec.it s "CR 702.90c Glistener Elf shrinks and kills a blocker with -1/-1 counters" $ do
      glistenerElf <- S.printingOf s registry "Glistener Elf"
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, blockers) = S.combatBoardOf [glistenerElf] [piker]
          fought = S.fightWith S.aggressiveAnswer gs
          settled = S.settleSba fought
      case blockers of
        [] -> Spec.assertFailure s "fixture should have a blocker"
        blocker : _ -> do
          Spec.assertEqWith s "one -1/-1 counter before SBA" (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject blocker fought)) (Just 1)
          Spec.assertEqWith s "no marked damage on the blocker" (S.damageOf blocker fought) (Just 0)
          Spec.assertEqWith s "blocker buried by 704.5f" (length (Game.zoneMembers Zone.Graveyard S.bob settled)) 1

-- CR 702.80. Wither is infect's CREATURE half alone, so the pair below is the
-- whole unit: CR 120.3d's counters against a creature, CR 120.3a's ordinary life
-- loss against a player. Sickle Ripper ({1}{B} Creature -- Elemental Warrior
-- 2/1, "Wither") prints nothing else.
--
-- CR 702.80d's redundancy is not tested: the rider is a Bool read by Map.member,
-- so a second instance cannot change it, and there is nothing to mutate away.
witherSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
witherSpec s registry =
  Spec.describe s "Wither" $ do
    -- Deliberately mismatched numbers: 2 damage into 4 toughness, so "two -1/-1
    -- counters" and "two marked damage" leave DIFFERENT boards. A 2/2 blocker
    -- would die either way and prove nothing.
    Spec.it s "CR 702.80a Sickle Ripper shrinks its blocker with -1/-1 counters and takes ordinary damage back" $ do
      sickleRipper <- S.printingOf s registry "Sickle Ripper"
      giantSpider <- S.printingOf s registry "Giant Spider"
      let (gs, attackers, blockers) = S.combatBoardOf [sickleRipper] [giantSpider]
          fought = S.fightWith S.aggressiveAnswer gs
          settled = S.settleSba fought
      case (attackers, blockers) of
        (attacker : _, blocker : _) -> do
          Spec.assertEqWith s "two -1/-1 counters on the blocker" (S.counterOf CounterKind.MinusOneMinusOne blocker fought) 2
          Spec.assertEqWith s "no marked damage on the blocker" (S.damageOf blocker fought) (Just 0)
          Spec.assertEqWith s "the 0/2 blocker survives" (S.onBattlefield blocker settled) True
          -- CR 120.3e: the Spider has no wither, so ITS damage is marked as
          -- usual. Without this the change could be "damage is counters now".
          Spec.assertEqWith s "two damage marked on the Ripper" (S.damageOf attacker fought) (Just 2)
          Spec.assertEqWith s "no -1/-1 counters on the Ripper" (S.counterOf CounterKind.MinusOneMinusOne attacker fought) 0
          Spec.assertEqWith s "CR 704.5g buries the 2/1 Ripper in its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 1
        _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

    -- CR 120.3a's exception names infect and NOT wither, which is the one place
    -- wither differs from the keyword it otherwise copies.
    Spec.it s "CR 120.3a Sickle Ripper drains an unblocked player's life, gives no poison" $ do
      sickleRipper <- S.printingOf s registry "Sickle Ripper"
      let (gs, _, _) = S.combatBoardOf [sickleRipper] []
          after = S.fightWith S.aggressiveAnswer gs
      Spec.assertEqWith s "bob lost the two life" (S.lifeOf S.bob after) (Just 18)
      -- CR 119.2's EVENT and not only the total: lifeLostBy skips infect's
      -- diverted damage, and this is what keeps wither out of that skip.
      Spec.assertEqWith s "a life-loss event was recorded" (elem (GameEvent.LifeLost (LifeChange.MkLifeChange S.bob 2)) (S.eventsOf after)) True
      Spec.assertEqWith s "bob has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0
      Spec.assertEqWith
        s
        "nothing on the board gained a -1/-1 counter"
        (any (\obj -> Map.findWithDefault 0 CounterKind.MinusOneMinusOne (Object.counters obj) > (0 :: Natural.Natural)) (GameState.objects after))
        False

toxicSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
toxicSpec s registry =
  Spec.describe s "Toxic" $ do
    Spec.it s "CR 120.3g toxic poison is IN ADDITION to the damage, not instead of it" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False False False 2 Nothing DamageKind.Combat
          after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
      Spec.assertEqWith s "bob has two poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 2
      Spec.assertEqWith s "bob still lost the three life" (S.lifeOf S.bob after) (Just 17)
      Spec.assertEqWith s "the source's controller gains no poison" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0

    Spec.it s "CR 120.3g toxic gives no poison on NONCOMBAT damage" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False False False 2 Nothing DamageKind.Noncombat
          after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
      Spec.assertEqWith s "bob has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0
      Spec.assertEqWith s "bob lost the three life" (S.lifeOf S.bob after) (Just 17)

    -- CR 120.3b and 120.3g compose: infect REPLACES the damage with poison,
    -- toxic ADDS its own on top, so a source with both gives amount + N and
    -- still drains no life. No card in the pool has both, so the event is
    -- hand-built.
    Spec.it s "CR 120.3b/120.3g infect and toxic stack: poison is amount plus N, and no life is lost" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False True False 2 Nothing DamageKind.Combat
          after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
      Spec.assertEqWith s "bob has five poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 5
      Spec.assertEqWith s "bob's life unchanged" (S.lifeOf S.bob after) (Just 20)

    Spec.it s "CR 702.164c Branchblight Stalker poisons an unblocked player AND drains its life" $ do
      stalker <- S.printingOf s registry "Branchblight Stalker"
      let (gs, _, _) = S.combatBoardOf [stalker] []
          after = S.fightWith S.aggressiveAnswer gs
      Spec.assertEqWith s "bob has two poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 2
      Spec.assertEqWith s "bob took the three damage too" (S.lifeOf S.bob after) (Just 17)
      Spec.assertEqWith s "alice (controller) has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0

    -- CR 120.3g is scoped to combat damage dealt TO A PLAYER: a blocked toxic
    -- creature hands its poison to nobody, and marks its blocker normally --
    -- toxic is not infect, so the blocker takes damage, not -1/-1 counters.
    Spec.it s "CR 120.3g a blocked Branchblight Stalker gives no poison and marks its blocker" $ do
      stalker <- S.printingOf s registry "Branchblight Stalker"
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, blockers) = S.combatBoardOf [stalker] [piker]
          fought = S.fightWith S.aggressiveAnswer gs
      Spec.assertEqWith s "bob has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob fought) 0
      Spec.assertEqWith s "bob's life unchanged" (S.lifeOf S.bob fought) (Just 20)
      case blockers of
        [] -> Spec.assertFailure s "fixture should have a blocker"
        blocker : _ -> do
          Spec.assertEqWith s "three damage marked on the blocker" (S.damageOf blocker fought) (Just 3)
          Spec.assertEqWith s "no -1/-1 counters" (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject blocker fought)) (Just 0)

    -- CR 702.4b deals combat damage TWICE, and CR 120.3g fires per instance of
    -- combat damage, not once per combat: two waves, two lots of poison. The
    -- grant is a layer-6 GainKeyword rather than a card, since no printing in
    -- the pool has both double strike and toxic.
    Spec.it s "CR 702.4b/120.3g a double-striking Branchblight Stalker poisons twice" $ do
      stalker <- S.printingOf s registry "Branchblight Stalker"
      let (gs0, attackers, _) = S.combatBoardOf [stalker] []
      case attackers of
        [] -> Spec.assertFailure s "fixture should have an attacker"
        attacker : _ -> do
          let gs = S.withEffectAt attacker (Timestamp.MkTimestamp 100) (Modification.GainKeyword Keyword.DoubleStrike) gs0
              after = S.runCombat S.aggressiveAnswer gs
          Spec.assertEqWith s "toxic 2 twice is four poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 4
          Spec.assertEqWith s "and three damage twice" (S.lifeOf S.bob after) (Just 14)

    -- Two Aspirant's Ascents on one Branchblight Stalker: toxic 2 printed plus
    -- toxic 1 granted twice is a total toxic value of 4 (CR 702.164b sums N
    -- over every toxic ability, and rule 702.164 has no redundancy clause of
    -- the CR 702.3c/702.9c kind). The falsifier is a projection that keeps
    -- keywords in a set, where the second toxic 1 collapses into the first and
    -- bob takes 3 poison instead of 4.
    --
    -- The same two casts grant flying twice, which CR 702.9c DOES make
    -- redundant: the Stalker simply flies, and bob (with no creatures) is not
    -- blocking either way.
    Spec.it s "CR 702.164b two Aspirant's Ascents make Branchblight Stalker toxic 4" $ do
      stalker <- S.printingOf s registry "Branchblight Stalker"
      island <- S.printingOf s registry "Island"
      ascent <- S.printingOf s registry "Aspirant's Ascent"
      let (gs0, attackers, _) = S.combatBoardOf [stalker] []
      case attackers of
        [] -> Spec.assertFailure s "fixture should have an attacker"
        attacker : _ -> do
          let withIsland g = snd (S.addCreature island S.alice g)
              castAscent g =
                let (oid, g1) = S.addHandCard ascent S.alice g
                    g2 = g1 {GameState.priority = Just S.alice}
                 in S.runPure S.identityAnswer g2 (S.cast S.alice oid Monad.>> Stack.resolveTop)
              gs = castAscent (castAscent (withIsland (withIsland gs0)))
              after = S.fightWith S.aggressiveAnswer gs
          Spec.assertEqWith s "toxic 2 plus toxic 1 twice" (Projection.totalToxic attacker gs) 4
          Spec.assertBool s (Projection.hasKeyword Keyword.Flying attacker gs) "CR 702.9c: two flying grants still just fly"
          Spec.assertEqWith s "bob has four poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 4
          Spec.assertEqWith s "and took the 3/1 Stalker's twice-pumped five damage" (S.lifeOf S.bob after) (Just 15)

    -- CR 615.6: a prevented event never happens, so no combat damage was
    -- "dealt to a player" for CR 120.3g to hang poison off. The falsifier is a
    -- toxic implementation that reads the rider off the event batch instead of
    -- off the survivors.
    Spec.it s "CR 615.6 prevented combat damage gives no toxic poison" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          shield =
            ActiveReplacement.MkActiveReplacement
              { ActiveReplacement.effect = ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat) (Filter.Type.And []) Nothing Nothing Nothing) DamageRewrite.PreventAll Seq.empty),
                ActiveReplacement.source = oid,
                ActiveReplacement.controller = S.alice,
                ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                ActiveReplacement.expiry = Expiry.Type.AtCleanup,
                ActiveReplacement.uses = Uses.Unlimited,
                ActiveReplacement.origin = ReplacementOrigin.Other,
                ActiveReplacement.condition = Nothing,
                ActiveReplacement.rider = Nothing,
                ActiveReplacement.slots = Map.empty
              }
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False False False 2 Nothing DamageKind.Combat
          after = S.runPure S.identityAnswer (S.addReplacement shield gs0) (Damage.applyDamage [ev])
      Spec.assertEqWith s "no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0
      Spec.assertEqWith s "no life lost" (S.lifeOf S.bob after) (Just 20)

-- Aims every target slot at bob when he is a legal recipient, and falls back to
-- the lowest candidate otherwise. Prodigal Sorcerer's ping would otherwise land
-- on the lowest recipient, which is a creature on these boards.
pingsBob :: Prompt.Prompt r -> r
pingsBob p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    S.preferring (== Recipient.ToPlayer S.bob) sets
  _ -> S.identityAnswer p

lifelinkSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lifelinkSpec s registry =
  Spec.describe s "Lifelink" $ do
    -- CR 702.15b: "Damage dealt by a source with lifelink causes that source's
    -- controller ... to gain that much life (in addition to any other results
    -- that damage causes)." CR 120.3a is the other result here, and BOTH halves
    -- are asserted: an implementation that replaced the life loss with a life
    -- gain would pass a one-sided assertion.
    Spec.it s "CR 702.15b/120.3a an unblocked Child of Night drains bob AND gains alice two life" $ do
      childOfNight <- S.printingOf s registry "Child of Night"
      let (gs, _, _) = S.combatBoardOf [childOfNight] []
          after = S.fightWith S.aggressiveAnswer gs
      Spec.assertEqWith s "alice gained two" (S.lifeOf S.alice after) (Just 22)
      Spec.assertEqWith s "and bob still lost two" (S.lifeOf S.bob after) (Just 18)

    -- CR 120.3f's "in addition to the damage's other results", where the other
    -- result is CR 120.3e's marked damage. Wall of Stone is 0/8, so nothing dies
    -- and the mark is readable straight off the blocker.
    --
    -- Also the CONTROLLER axis, cheaply: bob controls the damaged permanent and
    -- gains nothing, so lifelink is not paying whoever the damage landed on.
    Spec.it s "CR 120.3f a blocked Child of Night marks its blocker AND still gains the life" $ do
      childOfNight <- S.printingOf s registry "Child of Night"
      wallOfStone <- S.printingOf s registry "Wall of Stone"
      let (gs, _, blockers) = S.combatBoardOf [childOfNight] [wallOfStone]
          after = S.fightWith S.aggressiveAnswer gs
      case blockers of
        [] -> Spec.assertFailure s "fixture should have a blocker"
        blocker : _ -> do
          Spec.assertEqWith s "two damage marked on the Wall" (S.damageOf blocker after) (Just 2)
          Spec.assertEqWith s "alice gained two" (S.lifeOf S.alice after) (Just 22)
          Spec.assertEqWith s "and bob gained nothing" (S.lifeOf S.bob after) (Just 20)

    -- CR 702.15d: "The lifelink rules function no matter what zone an object
    -- with lifelink deals damage from" -- and CR 120.3f is not scoped to combat
    -- either, so a Basilisk Collar'd Prodigal Sorcerer's ping gains life. The
    -- falsifier is an implementation that lives in Pawl.Engine.Combat.
    Spec.it s "CR 702.15d a Basilisk Collar'd Prodigal Sorcerer's ping gains life too" $ do
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      basiliskCollar <- S.printingOf s registry "Basilisk Collar"
      let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
          (collarId, g1) = S.addCreature basiliskCollar S.alice g0
          equipped = (S.attach collarId srcId g1) {GameState.priority = Just S.alice}
          bare = g1 {GameState.priority = Just S.alice}
          ping board ability = S.runPure pingsBob board (Activate.activateAbility S.alice srcId ability Monad.>> Stack.resolveTop)
      case Face.activatedAbilities (S.combinedFace prodigalSorcerer) of
        [] -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
        ability : _ -> do
          let withCollar = ping equipped ability
              without = ping bare ability
          Spec.assertBool s (Projection.hasKeyword Keyword.Lifelink srcId equipped) "the Collar really grants lifelink"
          Spec.assertEqWith s "alice gained one" (S.lifeOf S.alice withCollar) (Just 21)
          Spec.assertEqWith s "and bob took the ping anyway" (S.lifeOf S.bob withCollar) (Just 19)
          -- The control twin: the same ping off the same Sorcerer with no
          -- Collar gains nothing, so the life above came from the keyword.
          Spec.assertEqWith s "no Collar, no life" (S.lifeOf S.alice without) (Just 20)
          Spec.assertEqWith s "bob took it all the same" (S.lifeOf S.bob without) (Just 19)

    -- CR 702.15b names "that source's controller, or its owner if it has no
    -- controller" -- so a creature bob OWNS, attacking under a Ray of Command,
    -- pays alice. The falsifier is an implementation that reads Object.owner.
    Spec.it s "CR 702.15b a Ray of Command'd Child of Night pays the THIEF, not its owner" $ do
      childOfNight <- S.printingOf s registry "Child of Night"
      rayOfCommand <- S.printingOf s registry "Ray of Command"
      island <- S.printingOf s registry "Island"
      let (gs0, _, theirs) = S.combatBoardOf [] [childOfNight]
          withIslands = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) gs0 [1 :: Int .. 4]
          (rayId, withRay) = S.addHandCard rayOfCommand S.alice withIslands
          ready = withRay {GameState.priority = Just S.alice}
          stolen = S.runPure S.identityAnswer ready (S.cast S.alice rayId Monad.>> Stack.resolveTop)
          after = S.fightWith S.aggressiveAnswer stolen
      case theirs of
        [] -> Spec.assertFailure s "fixture should have given bob a Child of Night"
        vampire : _ -> do
          Spec.assertEqWith s "bob owns it" (fmap Object.owner (Game.lookupObject vampire stolen)) (Just S.bob)
          Spec.assertEqWith s "but alice controls it" (Projection.controllerOf vampire stolen) (Just S.alice)
          Spec.assertEqWith s "the thief gained two" (S.lifeOf S.alice after) (Just 22)
          Spec.assertEqWith s "and its owner only lost two" (S.lifeOf S.bob after) (Just 18)

    -- The rider itself, asserted on the CR 608.2i record rather than through a
    -- life total: CR 702.15b's answer is a PLAYER, so the event carries who
    -- rather than whether, and a Piker trading with the Vampire carries nobody.
    -- The negative half is what keeps a "gain life for every event" reading out.
    Spec.it s "CR 702.15b/702.15c the deal-time rider names alice for Child of Night and nobody for a Piker" $ do
      childOfNight <- S.printingOf s registry "Child of Night"
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, mine, theirs) = S.combatBoardOf [childOfNight] [piker]
          fought = S.fightWith S.aggressiveAnswer gs
          payeeOf src = fmap DamageEvent.dealtByLifelink (List.find (\ev -> DamageEvent.source ev == src) (S.damageEventsOf fought))
      case (mine, theirs) of
        (vampire : _, blocker : _) -> do
          Spec.assertEqWith s "the Vampire's damage pays alice" (payeeOf vampire) (Just (Just S.alice))
          Spec.assertEqWith s "the Piker's damage pays nobody" (payeeOf blocker) (Just Nothing)
        _ -> Spec.assertFailure s "fixture should have one creature per side"

    -- CR 615.6: "If damage that would be dealt is prevented, it never happens" --
    -- so there is no damage for CR 702.15b to hang a life gain off. The
    -- falsifier is a pass that reads the rider off the event BATCH instead of
    -- off the survivors, which is the same trip-wire toxic's prevention case
    -- above sets. Hand-built, because the shield is a fixture rather than a card.
    Spec.it s "CR 615.6 prevented damage gains no lifelink life" $ do
      childOfNight <- S.printingOf s registry "Child of Night"
      let (oid, gs0) = S.addCreature childOfNight S.alice (Setup.emptyGame S.bothPlayers)
          shield =
            ActiveReplacement.MkActiveReplacement
              { ActiveReplacement.effect = ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat) (Filter.Type.And []) Nothing Nothing Nothing) DamageRewrite.PreventAll Seq.empty),
                ActiveReplacement.source = oid,
                ActiveReplacement.controller = S.alice,
                ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                ActiveReplacement.expiry = Expiry.Type.AtCleanup,
                ActiveReplacement.uses = Uses.Unlimited,
                ActiveReplacement.origin = ReplacementOrigin.Other,
                ActiveReplacement.condition = Nothing,
                ActiveReplacement.rider = Nothing,
                ActiveReplacement.slots = Map.empty
              }
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 2 False False False 0 (Just S.alice) DamageKind.Combat
          after = S.runPure S.identityAnswer (S.addReplacement shield gs0) (Damage.applyDamage [ev])
      Spec.assertEqWith s "alice gained nothing" (S.lifeOf S.alice after) (Just 20)
      Spec.assertEqWith s "and bob lost nothing" (S.lifeOf S.bob after) (Just 20)

    -- Read through the PROJECTION, not off the printed card: Humility (layer 6)
    -- strips lifelink, and the 1/1 it leaves behind gains nobody anything.
    Spec.it s "CR 613 Humility removes lifelink, so Child of Night's damage gains no life" $ do
      childOfNight <- S.printingOf s registry "Child of Night"
      humility <- S.printingOf s registry "Humility"
      let (gs0, _, _) = S.combatBoardOf [childOfNight] []
          gs = S.withHumility humility gs0
          after = S.fightWith S.aggressiveAnswer gs
      Spec.assertEqWith s "alice gained nothing" (S.lifeOf S.alice after) (Just 20)
      Spec.assertEqWith s "and the 1/1 dealt just one" (S.lifeOf S.bob after) (Just 19)

-- CR 608.2h / 702.2e / 702.15c / 702.90d: a source that has already CEASED still
-- deals damage with the riders it last had. All three keyword rules carry the
-- same sentence -- "If an object is no longer in the zone it's expected to be in
-- as an effect causes it to deal damage, its last known information is used to
-- determine whether it had [the keyword]" -- and CR 608.2h is the general form.
--
-- Ghitu Fire-Eater is the producer: "{T}, Sacrifice this creature: It deals
-- damage equal to its power to any target" pays a cost that removes the very
-- object the riders are read off, so by resolution its id names nothing.
-- Pawl.ActivateSpec's LastKnownInformation group proves the AMOUNT already
-- survives that (Quantity.Power goes through Projection.viewWithLastKnown); this
-- group is the riders, which did not.
--
-- Basilisk Collar is what gives it riders to lose -- "Equipped creature has
-- deathtouch and lifelink" is two of the four in one card, and neither is
-- printed on the Fire-Eater, so an implementation reading the printed card
-- rather than last known information fails these too.
lastKnownRiderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lastKnownRiderSpec s registry =
  Spec.describe s "LastKnownRiders" $ do
    -- Both riders at once, because they are read at one site and a fix that
    -- reached only the keyword half (or only the controller half) would pass
    -- whichever test was written alone.
    Spec.it s "CR 702.15c/702.2e a sacrificed Basilisk Collar'd Fire-Eater still deals lifelink deathtouch damage" $ do
      ghituFireEater <- S.printingOf s registry "Ghitu Fire-Eater"
      basiliskCollar <- S.printingOf s registry "Basilisk Collar"
      case Face.activatedAbilities (S.combinedFace ghituFireEater) of
        [] -> Spec.assertFailure s "Ghitu Fire-Eater should declare one activated ability"
        ability : _ -> do
          let (srcId, g0) = S.addCreature ghituFireEater S.alice (Setup.emptyGame S.bothPlayers)
              (collarId, g1) = S.addCreature basiliskCollar S.alice g0
              equipped = (S.attach collarId srcId g1) {GameState.priority = Just S.alice}
              bare = g1 {GameState.priority = Just S.alice}
              fire board = S.runPure pingsBob board (Activate.activateAbility S.alice srcId ability Monad.>> Stack.resolveTop)
              withCollar = fire equipped
              without = fire bare
              eventOf board = List.find (\ev -> DamageEvent.source ev == srcId) (S.damageEventsOf board)
          -- The premise: the Collar really grants both while it is still there,
          -- and the cost really takes the source away before resolution.
          Spec.assertBool s (Projection.hasKeyword Keyword.Lifelink srcId equipped) "the Collar grants lifelink on the battlefield"
          Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch srcId equipped) "and deathtouch"
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject srcId withCollar)) "and by resolution the source's id names nothing"
          -- CR 702.15c: the life is gained even though nothing on the board has
          -- lifelink by the time the damage is dealt.
          Spec.assertEqWith s "alice gained the 2 it dealt" (S.lifeOf S.alice withCollar) (Just 22)
          Spec.assertEqWith s "the rider names alice, not nobody" (fmap DamageEvent.dealtByLifelink (eventOf withCollar)) (Just (Just S.alice))
          -- CR 702.2e: the same for deathtouch, which CR 704.5h then reads.
          Spec.assertEqWith s "and the damage is flagged deathtouch" (fmap DamageEvent.dealtByDeathtouch (eventOf withCollar)) (Just True)
          Spec.assertEqWith s "bob took it either way" (S.lifeOf S.bob withCollar) (Just 18)
          -- The control twin: the same sacrifice with no Collar carries neither
          -- rider, so the two above came from last known information rather than
          -- from a fallback that flags everything.
          Spec.assertEqWith s "no Collar, no life" (S.lifeOf S.alice without) (Just 20)
          Spec.assertEqWith s "no Collar, no lifelink rider" (fmap DamageEvent.dealtByLifelink (eventOf without)) (Just Nothing)
          Spec.assertEqWith s "no Collar, no deathtouch rider" (fmap DamageEvent.dealtByDeathtouch (eventOf without)) (Just False)

    -- CR 613.1b, the half the test above cannot see: alice both OWNS and
    -- controls her Fire-Eater, so LastKnown.controller and Object.owner give the
    -- same answer there and a reader that took the owner would pass. Here bob
    -- owns it and alice has stolen it, so CR 702.15b's "that source's
    -- CONTROLLER" pays the thief -- and the record has to keep control
    -- separately from the characteristics, which is why LastKnown does (CR 109.3
    -- says control is not a characteristic).
    Spec.it s "CR 702.15b/613.1b a stolen Fire-Eater's lifelink pays the THIEF, not its owner" $ do
      ghituFireEater <- S.printingOf s registry "Ghitu Fire-Eater"
      basiliskCollar <- S.printingOf s registry "Basilisk Collar"
      case Face.activatedAbilities (S.combinedFace ghituFireEater) of
        [] -> Spec.assertFailure s "Ghitu Fire-Eater should declare one activated ability"
        ability : _ -> do
          let (srcId, g0) = S.addCreature ghituFireEater S.bob (Setup.emptyGame S.bothPlayers)
              (collarId, g1) = S.addCreature basiliskCollar S.alice g0
              equipped = S.attach collarId srcId g1
              stolen = (S.giveControl srcId S.alice equipped) {GameState.priority = Just S.alice}
              after = S.runPure pingsBob stolen (Activate.activateAbility S.alice srcId ability Monad.>> Stack.resolveTop)
              rider = fmap DamageEvent.dealtByLifelink (List.find (\ev -> DamageEvent.source ev == srcId) (S.damageEventsOf after))
          Spec.assertEqWith s "bob owns it" (fmap Object.owner (Game.lookupObject srcId stolen)) (Just S.bob)
          Spec.assertEqWith s "but alice controls it as it is sacrificed" (Projection.controllerOf srcId stolen) (Just S.alice)
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject srcId after)) "and by resolution its id names nothing"
          Spec.assertEqWith s "the rider names the thief" rider (Just (Just S.alice))
          Spec.assertEqWith s "so alice gained the 2" (S.lifeOf S.alice after) (Just 22)
          Spec.assertEqWith s "and its owner gained nothing" (S.lifeOf S.bob after) (Just 18)

    -- The fallback is only a fallback: CR 608.2h's FIRST clause uses current
    -- information while the source is where it is expected to be, and only its
    -- second reaches for last known information.
    --
    -- The falsifier is a reader that consults the last-known map first, and
    -- reaching it needs a state the engine cannot build: Event.changeZone files
    -- a lastKnown entry in the same modify' that deletes the object, so `objects`
    -- and `lastKnown` are disjoint by construction and no real board has an entry
    -- for a LIVE id. So the entry is planted by hand -- a snapshot saying the
    -- Sorcerer had lifelink, filed under the id of a Sorcerer that is still on
    -- the battlefield and has just had the grant stripped by Humility.
    --
    -- Without lastKnownOf's liveness guard this test gains alice a life; with it,
    -- the live projection wins and she gains nothing. That guard is otherwise
    -- unfalsifiable, which is exactly why it is worth planting the state to
    -- falsify it -- the invariant it defends is an invariant of the CALLERS, not
    -- of the type.
    Spec.it s "CR 608.2h a live source reads LIVE, even with a last-known entry filed under its id" $ do
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      basiliskCollar <- S.printingOf s registry "Basilisk Collar"
      humility <- S.printingOf s registry "Humility"
      case Face.activatedAbilities (S.combinedFace prodigalSorcerer) of
        [] -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
        ability : _ -> do
          let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
              (collarId, g1) = S.addCreature basiliskCollar S.alice g0
              -- Read off the object rather than rebuilt, so the snapshot names
              -- the same printing entry its own object does.
              sorcererSource = fmap Object.source (Game.lookupObject srcId g1)
              equipped = S.attach collarId srcId g1
              -- The snapshot the guard must ignore, taken while the Collar's
              -- grant is still live, then filed under the still-live id.
              snapshot =
                LastKnown.MkLastKnown
                  { LastKnown.characteristics = Projection.project srcId equipped,
                    LastKnown.controller = S.alice,
                    -- The fallback is unreachable: srcId was just minted by
                    -- S.addCreature two bindings above.
                    LastKnown.source = Maybe.fromMaybe (Source.OfCard (PrintingId.MkPrintingId 0)) sorcererSource,
                    -- CR 122.1: nothing put a counter on the Sorcerer, and this
                    -- case is about a keyword grant rather than a counter.
                    LastKnown.counters = Map.empty,
                    -- Nothing here copies anything; the field is filled to
                    -- build the record.
                    LastKnown.copiable = Projection.copiableCharacteristics srcId equipped,
                    -- The Sorcerer wears the Collar rather than the other way
                    -- round, so the object this record is about is attached to
                    -- nothing.
                    LastKnown.attachedTo = Nothing
                  }
              humbled = S.withHumility humility equipped
              planted =
                humbled
                  { GameState.lastKnown = Map.insert srcId snapshot (GameState.lastKnown humbled),
                    GameState.priority = Just S.alice
                  }
              after = S.runPure pingsBob planted (Activate.activateAbility S.alice srcId ability Monad.>> Stack.resolveTop)
          Spec.assertBool s (Map.member Keyword.Lifelink (PC.keywords (LastKnown.characteristics snapshot))) "the planted snapshot really says lifelink"
          Spec.assertBool s (not (Projection.hasKeyword Keyword.Lifelink srcId planted)) "while Humility has stripped it live"
          Spec.assertBool s (Maybe.isJust (Game.lookupObject srcId after)) "and the Sorcerer is still there to be read live"
          Spec.assertEqWith s "so alice gains nothing -- the live read won" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "and bob took the ping" (S.lifeOf S.bob after) (Just 19)

    -- CR 704.8: a permanent that leaves the battlefield alongside other
    -- state-based actions has its last known information derived from the board
    -- before ANY of them was performed.
    --
    -- Opalescence animates Bad Moon into a creature, and Bad Moon's own +1/+1
    -- reaches itself and the Berserker alike, so alice has a 3/3 and a 3/3 with
    -- 3 damage marked on each: CR 704.5g destroys both in one pass. Deathknell
    -- Berserker's intervening if (CR 603.4) asks whether its power was 3 or
    -- more, and Bad Moon's grant is the only thing that gets it there -- so the
    -- Zombie token appears iff the Berserker was read against a board Bad Moon
    -- had not yet left.
    --
    -- BOTH ID ORDERS, for the reason Pawl.ZoneTriggerSpec's CR 603.10a case
    -- runs both: the two boards differ in nothing a rule can see, and Event
    -- reaches the lower id first. The Moon-first board is the one that answered
    -- no token.
    Spec.it s "CR 704.8 a creature destroyed beside Bad Moon is read with Bad Moon still there, in either id order" $ do
      opalescence <- S.printingOf s registry "Opalescence"
      badMoon <- S.printingOf s registry "Bad Moon"
      deathknellBerserker <- S.printingOf s registry "Deathknell Berserker"
      let board moonFirst =
            let withOpal = snd (S.addCreature opalescence S.alice sbaBase)
                (moon, berserker, placed) =
                  if moonFirst
                    then
                      let (m, g1) = S.addCreature badMoon S.alice withOpal
                          (b, g2) = S.addCreature deathknellBerserker S.alice g1
                       in (m, b, g2)
                    else
                      let (b, g1) = S.addCreature deathknellBerserker S.alice withOpal
                          (m, g2) = S.addCreature badMoon S.alice g1
                       in (m, b, g2)
             in (moon, berserker, S.markDamage moon 3 (S.markDamage berserker 3 placed))
          run moonFirst =
            let (moon, berserker, marked) = board moonFirst
                settled = S.runPure S.identityAnswer marked Engine.settleForPriority
             in (moon, berserker, marked, S.runPure S.identityAnswer settled Stack.resolveTop)
          (moonFirstMoon, moonFirstBerserker, moonFirstMarked, moonFirstAfter) = run True
          (berserkerFirstMoon, berserkerFirstBerserker, berserkerFirstMarked, berserkerFirstAfter) = run False
      Spec.assertEqWith s "a Zombie token with Bad Moon minted first" (length (S.tokensOf moonFirstAfter)) 1
      Spec.assertEqWith s "and one with the Berserker minted first" (length (S.tokensOf berserkerFirstAfter)) 1
      -- The premises the reading above rests on: the grant is really there
      -- before the pass, the two victims really are in one pass, and the two
      -- boards really do differ in which one Event reaches first.
      Spec.assertEqWith s "Opalescence makes Bad Moon a 3/3" (S.powerToughnessOf moonFirstMoon moonFirstMarked) (Just (3, 3))
      Spec.assertEqWith s "and Bad Moon makes the Berserker a 3/3" (S.powerToughnessOf moonFirstBerserker moonFirstMarked) (Just (3, 3))
      Spec.assertBool s (moonFirstMoon < moonFirstBerserker) "Bad Moon is reached first on the Moon-first board"
      Spec.assertBool s (berserkerFirstBerserker < berserkerFirstMoon) "and the Berserker first on the other"
      Spec.assertEqWith s "both cards reached the graveyard either way" (fmap (length . Game.zoneMembers Zone.Graveyard S.alice) [moonFirstAfter, berserkerFirstAfter]) [2, 2]
      Spec.assertEqWith s "leaving her Opalescence and the token on the battlefield" (fmap (length . Game.zoneMembers Zone.Battlefield S.alice) [moonFirstAfter, berserkerFirstAfter]) [2, 2]
      Spec.assertEqWith s "with the same board on the twin" (S.powerToughnessOf berserkerFirstBerserker berserkerFirstMarked) (Just (3, 3))

sbaBase :: GameState.GameState
sbaBase = Setup.emptyGame S.bothPlayers

-- Answers Prompt.ChooseLegend by keeping the candidate `wanted`, when it is on
-- offer. A pair of tests differing only in this argument proves the ANSWER
-- decides which legend survives, rather than the order Sba enumerates them.
keepsLegend :: ObjectId.ObjectId -> Prompt.Prompt r -> r
keepsLegend wanted p = case p of
  Prompt.ChooseLegend _ _ candidates ->
    if elem wanted (NonEmpty.toList candidates) then wanted else NonEmpty.head candidates
  _ -> S.identityAnswer p

-- Copies `target` when asked what to copy, and keeps `keep` when the legend rule
-- asks which same-named legend survives.
copiesAndKeeps :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
copiesAndKeeps target keep p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> if elem target legal then Just target else Nothing
  Prompt.ChooseLegend _ _ candidates ->
    if elem keep (NonEmpty.toList candidates) then keep else NonEmpty.head candidates
  _ -> S.identityAnswer p

-- Is this object still on the battlefield?
inPlay :: ObjectId.ObjectId -> GameState.GameState -> Bool
inPlay oid gs = fmap Object.zone (Game.lookupObject oid gs) == Just Zone.Battlefield

-- The two doors of Roaring Furnace // Steaming Sauna, and the halves a Room
-- permanent below is set up with.
furnaceName, saunaName :: CardName.CardName
furnaceName = CardName.MkCardName (Text.pack "Roaring Furnace")
saunaName = CardName.MkCardName (Text.pack "Steaming Sauna")

-- CR 709.5c: give a Room permanent these unlocked designations directly. The
-- names a Room projects are one per UNLOCKED door (CR 709.4a, via
-- Pawl.Engine.Card.roomNames), so this is what makes a permanent multi-named,
-- which is what two legend groups need to share a member set. Set
-- rather than cast-and-unlock because the doors cost {1}{R} and {3}{U}{U} apiece
-- and this group's subject is the grouping, not the unlocking; Pawl.RoomSpec
-- covers the end-to-end route and asserts against this same field.
unlockDoors :: Set.Set CardName.CardName -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
unlockDoors doors oid gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.unlockedHalves = doors}) oid (GameState.objects gs)}

-- Answers Prompt.ChooseLegend with a DIFFERENT candidate each time it is asked,
-- and counts the asks. Two identical prompts cannot be told apart by a pure
-- answerer, so this is what shows that asking twice about one set of legends
-- lets independent answers bury every member of it.
alternatingLegend :: Prompt.Prompt r -> State.State Natural.Natural r
alternatingLegend p = case p of
  Prompt.ChooseLegend _ _ candidates -> do
    n <- State.get
    State.modify' (+ 1)
    pure (if even n then NonEmpty.head candidates else NonEmpty.last candidates)
  _ -> pure (S.identityAnswer p)

-- Keeps `left` when it is on offer and `right` otherwise: the answer a
-- controller of three chained Rooms gives to keep both ends of the chain.
keepsEitherEnd :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
keepsEitherEnd left right p = case p of
  Prompt.ChooseLegend _ _ candidates
    | elem left (NonEmpty.toList candidates) -> left
    | elem right (NonEmpty.toList candidates) -> right
  _ -> S.identityAnswer p

legendRuleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
legendRuleSpec s registry =
  Spec.describe s "LegendRule" $ do
    -- CR 704.5j: "If two or more legendary permanents with the same name are
    -- controlled by the same player, that player chooses one of them, and the
    -- rest are put into their owners' graveyards."
    Spec.it s "CR 704.5j a second Thalia sends one of them to the graveyard" $ do
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      let (first, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (second, gs) = S.addCreature thalia S.alice g0
          kept = S.runPure (keepsLegend first) gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay first kept) "the chosen one stays"
      Spec.assertBool s (not (inPlay second kept)) "the other is gone"
      -- CR 400.7: the move mints a NEW incarnation, so the buried Thalia is not
      -- `second` any more. Count the graveyard rather than chase the dead id.
      Spec.assertEqWith s "exactly one Thalia was buried" (length (Game.zoneMembers Zone.Graveyard S.alice kept)) 1

    -- The discriminating twin: same board, opposite answer. This fails if the
    -- engine picks the survivor itself.
    Spec.it s "CR 704.5j which Thalia survives is the controller's choice" $ do
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      let (first, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (second, gs) = S.addCreature thalia S.alice g0
          keptSecond = S.runPure (keepsLegend second) gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay second keptSecond) "the second one stays this time"
      Spec.assertBool s (not (inPlay first keptSecond)) "the first is gone"

    -- CR 704.5j is per CONTROLLER: one legend each is legal, and the rule has
    -- nothing to say about the two of them.
    Spec.it s "CR 704.5j two players may each control a Thalia" $ do
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      let (hers, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (his, gs) = S.addCreature thalia S.bob g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay hers after) "alice keeps hers"
      Spec.assertBool s (inPlay his after) "bob keeps his"

    -- "Legendary" is half the condition; a duplicated ordinary creature is not
    -- the legend rule's business.
    Spec.it s "CR 704.5j two copies of a NON-legendary creature both survive" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (a, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (b, gs) = S.addCreature piker S.alice g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay a after && inPlay b after) "both stay"

    -- "With the same name" is the other half: two DIFFERENT legends coexist.
    Spec.it s "CR 704.5j a Thalia and an Urborg coexist under one controller" $ do
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
      let (t, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (u, gs) = S.addCreature urborg S.alice g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay t after && inPlay u after) "both stay"

    -- "Put into their OWNERS' graveyards" -- not the controller's. Alice
    -- controls both, but bob owns the one she stole, so that is where it goes.
    Spec.it s "CR 704.5j the loser goes to its OWNER's graveyard, not the controller's" $ do
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      let (hers, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (his, g1) = S.addCreature thalia S.bob g0
          stolen = S.giveControl his S.alice g1
          after = S.runPure (keepsLegend hers) stolen Sba.checkStateBasedActions
      Spec.assertBool s (inPlay hers after) "alice keeps her own"
      Spec.assertBool s (not (inPlay his after)) "the stolen one left the battlefield"
      -- The whole point: alice controlled it, but bob owns it, so bob's
      -- graveyard is where it lands. (CR 400.7 gives it a fresh id on the way,
      -- so this counts the zone rather than naming the old one.)
      Spec.assertEqWith s "one card in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
      Spec.assertEqWith s "and none in alice's" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

    -- The P2-reachable path the issue names, and the gameplay-level proof: a
    -- Clone is neither legendary nor named Thalia on its own, but CR 707.2 lists both
    -- name and supertype among the copiable values, so the copy copies
    -- name and supertype alike, so the copy IS a second Thalia and the rule
    -- fires on it.
    Spec.it s "CR 707.2/704.5j a Clone copying Thalia is a second Thalia and the rule fires" $ do
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      clone <- S.printingOf s registry "Clone"
      let (original, board) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (_, staged) = S.spellOnStack clone S.alice board
          settled = snd (Engine.runGamePure (copiesAndKeeps original original) staged (Stack.resolveTop >> Engine.settleForPriority))
      Spec.assertBool s (inPlay original settled) "the original survives, because alice chose it"
      Spec.assertEqWith s "and exactly one Thalia is left in play" (S.creaturesInPlay S.alice settled) 1

    -- CR 704.3: every applicable state-based action is performed
    -- "simultaneously as a single event". So a legend that CR 704.5f is already
    -- burying stays on CR 704.5j's ballot, and keeping THAT one is a legal
    -- choice which puts every other copy into the graveyard beside it.
    --
    -- Dropping such a member from the candidates would decide for the player
    -- and strand a copy alive that they chose to lose -- which is what this
    -- branch did before review caught it.
    Spec.it s "CR 704.3/704.5j keeping a Thalia that is already dying buries both" $ do
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      let (healthy, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (dying, g1) = S.addCreature thalia S.alice g0
          -- Thalia is 2/1, so -2/-1 makes this copy a 0/0: CR 704.5f applies to
          -- it and not to the other.
          gs = S.withEffect dying (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal (-2)) (Quantity.Literal (-1)))) g1
          keptDying = S.runPure (keepsLegend dying) gs Sba.checkStateBasedActions
          keptHealthy = S.runPure (keepsLegend healthy) gs Sba.checkStateBasedActions
      Spec.assertEqWith s "the 0/0 really is a 0/0" (Projection.toughnessOf dying gs) (Just 0)
      -- Keeping the dying copy: 704.5j buries the healthy one, 704.5f buries this
      -- one, and alice is left with no Thalia at all.
      Spec.assertBool s (not (inPlay healthy keptDying)) "the healthy Thalia went too"
      Spec.assertBool s (not (inPlay dying keptDying)) "and so did the dying one"
      Spec.assertEqWith s "two cards in the graveyard, so neither was moved twice" (length (Game.zoneMembers Zone.Graveyard S.alice keptDying)) 2
      -- The discriminating twin: keeping the healthy copy saves it, so the
      -- outcome above really is alice's choice and not a forced sweep.
      Spec.assertBool s (inPlay healthy keptHealthy) "keeping the healthy one saves it"
      Spec.assertEqWith s "and only the 0/0 was buried" (length (Game.zoneMembers Zone.Graveyard S.alice keptHealthy)) 1

    -- CR 613.1d layer 4 / CR 205.4b / CR 704.5j, at gameplay level and end to
    -- end: Leyline of Singularity's "All nonland permanents are legendary" is
    -- the pool's only printed grant of the LEGENDARY supertype, and the legend
    -- rule is what makes the grant observable on the board rather than only in a
    -- projection. Two Goblin Pikers are a legal board until the
    -- Leyline resolves; afterwards they are two same-named legends under one
    -- controller and CR 704.5j buries one.
    --
    -- The Leyline is CAST and resolved rather than placed, so the grant is read
    -- through the same layer fold a real game would run it through.
    Spec.it s "CR 613.1d/704.5j Leyline of Singularity makes two Goblin Pikers legendary, and the legend rule buries one" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      leyline <- S.printingOf s registry "Leyline of Singularity"
      let (a, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (b, g1) = S.addCreature piker S.alice g0
          -- The CONTROL, and the guard against a vacuous pass below: with no
          -- Leyline on the board these two live through a settled sweep, so the
          -- fixture really does hold two duplicates for the rule to find.
          before = S.runPure S.identityAnswer g1 Sba.performStateBasedActions
          (_, staged) = S.spellOnStack leyline S.alice g1
          after = S.runPure (keepsLegend a) staged (Stack.resolveTop >> Engine.settleForPriority)
      Spec.assertBool s (a /= b) "the two Pikers are separate objects, not one counted twice"
      Spec.assertBool s (inPlay a before && inPlay b before) "without the Leyline both survive"
      Spec.assertBool s (inPlay a after) "the Piker alice chose stays"
      Spec.assertBool s (not (inPlay b after)) "the other Piker is gone"
      Spec.assertEqWith s "and it is in its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

    -- The other half of Leyline of Singularity's affected set: "All NONLAND
    -- permanents". Two Forests share a name and would be a legend rule pair if
    -- the grant reached them, so this is what proves the Not (HasCardType Land)
    -- filter discriminates rather than being decoration.
    Spec.it s "CR 613.1d Leyline of Singularity leaves two Forests alone, because they are lands" $ do
      forest <- S.printingOf s registry "Forest"
      leyline <- S.printingOf s registry "Leyline of Singularity"
      let (a, g0) = S.addCreature forest S.alice (Setup.emptyGame S.bothPlayers)
          (b, g1) = S.addCreature forest S.alice g0
          (_, staged) = S.spellOnStack leyline S.alice g1
          after = S.runPure (keepsLegend a) staged (Stack.resolveTop >> Engine.settleForPriority)
      Spec.assertBool s (a /= b) "the two Forests are separate objects"
      Spec.assertBool s (inPlay a after && inPlay b after) "both Forests stay"
      Spec.assertEqWith s "nothing was buried" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

    -- CR 704.5j is ONE choice over ONE set of same-named legendary permanents,
    -- and CR 704.3 performs the whole pass as a single event -- so a permanent
    -- with two names is asked about once, not once per name.
    --
    -- CR 709.5 gives a Room permanent one name per unlocked door (CR 709.4a), so
    -- two Rooms with both doors open are same-named through BOTH names, which is
    -- what makes keying the groups by name and keying them by member set
    -- disagree. Leyline of Singularity supplies the supertype.
    --
    -- Asked twice about the same pair, the two answers are independent, and
    -- keeping one Room under the first name and the OTHER under the second
    -- buries both -- leaving alice with no Room at all, which "chooses one of
    -- them, and the rest" forbids. The alternating answerer is what makes that
    -- observable: two identical prompts are indistinguishable to a pure one.
    Spec.it s "CR 704.5j/709.4a two fully unlocked Rooms are one legend group, asked once" $ do
      room <- S.printingOf s registry "Roaring Furnace"
      leyline <- S.printingOf s registry "Leyline of Singularity"
      let bothDoors = Set.fromList [furnaceName, saunaName]
          (p, g0) = S.addCreature room S.alice (Setup.emptyGame S.bothPlayers)
          (q, g1) = S.addCreature room S.alice g0
          board = unlockDoors bothDoors q (unlockDoors bothDoors p g1)
          -- The CONTROL: without the Leyline neither Room is legendary, so the
          -- rule has nothing to say and the pass below cannot be vacuous.
          before = S.runPure S.identityAnswer board Sba.performStateBasedActions
          (_, staged) = S.spellOnStack leyline S.alice board
          ((_, after), asked) = State.runState (Engine.runGame alternatingLegend staged (Stack.resolveTop >> Engine.settleForPriority)) 0
      Spec.assertBool s (p /= q) "the two Rooms are separate objects, not one counted twice"
      Spec.assertEqWith s "the first Room projects BOTH door names" (Projection.namesOf p board) bothDoors
      Spec.assertEqWith s "and so does the second" (Projection.namesOf q board) bothDoors
      Spec.assertBool s (inPlay p before && inPlay q before) "without the Leyline both survive"
      Spec.assertBool s (inPlay p after || inPlay q after) "alice is left with a Room, whatever she answered"
      Spec.assertEqWith s "and exactly one Room was buried" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
      Spec.assertEqWith s "the legend rule asked exactly once" asked 1

    -- The other side of the same design call: groups that OVERLAP are still two
    -- groups. Three Rooms unlocked {Roaring Furnace}, both, {Steaming Sauna} are
    -- two distinct same-named sets under CR 704.5j, and keeping the first under
    -- one name and the third under the other is a legal pair of choices that
    -- buries only the middle one. Merging by shared name -- a transitive closure
    -- rather than the deduplication Sba.legendGroups does -- would make one
    -- group of three and bury two of them.
    Spec.it s "CR 704.5j overlapping legend groups stay two groups, so both ends survive" $ do
      room <- S.printingOf s registry "Roaring Furnace"
      leyline <- S.printingOf s registry "Leyline of Singularity"
      let (a, g0) = S.addCreature room S.alice (Setup.emptyGame S.bothPlayers)
          (b, g1) = S.addCreature room S.alice g0
          (c, g2) = S.addCreature room S.alice g1
          board =
            unlockDoors (Set.singleton saunaName) c
              . unlockDoors (Set.fromList [furnaceName, saunaName]) b
              $ unlockDoors (Set.singleton furnaceName) a g2
          (_, staged) = S.spellOnStack leyline S.alice board
          after = S.runPure (keepsEitherEnd a c) staged (Stack.resolveTop >> Engine.settleForPriority)
      -- The chain really is a chain: the ends share a name with the middle and
      -- none with each other.
      Spec.assertEqWith s "one end is named for one door only" (Projection.namesOf a board) (Set.singleton furnaceName)
      Spec.assertEqWith s "the middle has both names" (Projection.namesOf b board) (Set.fromList [furnaceName, saunaName])
      Spec.assertEqWith s "the other end is named for the other door only" (Projection.namesOf c board) (Set.singleton saunaName)
      Spec.assertBool s (inPlay a after) "alice keeps the first end"
      Spec.assertBool s (inPlay c after) "and the other end too"
      Spec.assertBool s (not (inPlay b after)) "the middle Room, named by both groups, is the only loser"
      Spec.assertEqWith s "exactly one Room was buried" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

-- The two world enchantments in the pool, fetched together: most tests below
-- want two DIFFERENTLY NAMED world permanents, since a rule that ignores names
-- is half of the contrast with the legend rule above.
worldPair :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Printing.Printing, Printing.Printing)
worldPair s registry = do
  crossroads <- S.printingOf s registry "Concordant Crossroads"
  livingPlane <- S.printingOf s registry "Living Plane"
  pure (crossroads, livingPlane)

-- One state-based-action pass with CR 704.5k's clock sampled first, which is what
-- Engine.performSettle does on every pass (sampleWorldSince, then the SBA check).
-- A test that called Sba directly would find every world permanent unstamped and
-- the rule with no candidates -- so this, and not the bare pass, is what these
-- single-pass tests run.
worldPass :: Game.Type.Game Bool
worldPass = Engine.sampleWorldSince >> Sba.performStateBasedActions

-- What Object.worldSince holds for one object, or Nothing if there is no such
-- object at all.
worldSinceOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Timestamp.Timestamp
worldSinceOf oid gs = Game.lookupObject oid gs >>= Object.worldSince

-- S.addCreature, plus the CR 704.5k stamp that the settle following a permanent's
-- entry would mint. A fixture that hand-places two world permanents with no settle
-- between them has them becoming world in the SAME pass, which CR 704.5k's second
-- sentence makes a TIE -- not the board these tests are about.
addWorld :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addWorld printing pid gs =
  let (oid, gs1) = S.addCreature printing pid gs
   in (oid, S.runPure S.identityAnswer gs1 Engine.sampleWorldSince)

worldRuleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
worldRuleSpec s registry =
  Spec.describe s "WorldRule" $ do
    -- CR 704.5k: "If two or more permanents have the supertype world, all
    -- except the one that has had the world supertype for the shortest amount
    -- of time are put into their owners' graveyards."
    --
    -- Shortest amount of time is the LAST one to become world, which for a
    -- PRINTED world supertype is the last one to enter -- so the second one to
    -- enter is the one that lives.
    Spec.it s "CR 704.5k the newer of two world permanents survives" $ do
      (crossroads, livingPlane) <- worldPair s registry
      let (older, g0) = addWorld crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (newer, gs) = addWorld livingPlane S.alice g0
          after = S.runPure S.identityAnswer gs worldPass
      Spec.assertBool s (inPlay newer after) "the newcomer stays"
      Spec.assertBool s (not (inPlay older after)) "the incumbent is gone"
      Spec.assertEqWith s "exactly one was buried" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

    -- The discriminating twin: the same two cards, entering in the opposite
    -- order, produce the opposite survivor. This fails if the rule is reading
    -- anything but the clock -- an object id, a name, or the order Sba happens
    -- to enumerate the battlefield in.
    Spec.it s "CR 704.5k which one survives is the entry order, not the card" $ do
      (crossroads, livingPlane) <- worldPair s registry
      let (older, g0) = addWorld livingPlane S.alice (Setup.emptyGame S.bothPlayers)
          (newer, gs) = addWorld crossroads S.alice g0
          after = S.runPure S.identityAnswer gs worldPass
      Spec.assertBool s (inPlay newer after) "the newcomer stays"
      Spec.assertBool s (not (inPlay older after)) "the incumbent is gone"

    -- Unlike CR 704.5j, the world rule is NOT scoped to one controller: it
    -- says "if two or more permanents", full stop. Two players each with a
    -- world permanent is exactly the board the legend rule leaves alone.
    Spec.it s "CR 704.5k two players may NOT each keep a world permanent" $ do
      (crossroads, livingPlane) <- worldPair s registry
      let (hers, g0) = addWorld crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (his, gs) = addWorld livingPlane S.bob g0
          after = S.runPure S.identityAnswer gs worldPass
      Spec.assertBool s (inPlay his after) "bob's newer one stays"
      Spec.assertBool s (not (inPlay hers after)) "alice's older one is gone"

    -- "All except the one" -- so a third arrival buries BOTH incumbents in the
    -- same pass, rather than peeling one off per pass.
    Spec.it s "CR 704.5k a third world permanent buries both incumbents at once" $ do
      (crossroads, livingPlane) <- worldPair s registry
      let (first, g0) = addWorld crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (second, g1) = addWorld livingPlane S.alice g0
          (third, gs) = addWorld crossroads S.alice g1
          after = S.runPure S.identityAnswer gs worldPass
      Spec.assertBool s (inPlay third after) "only the newest stays"
      Spec.assertBool s (not (inPlay first after)) "the first is gone"
      Spec.assertBool s (not (inPlay second after)) "the second is gone too"
      Spec.assertEqWith s "both were buried in ONE pass" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2

    -- "Two or more": one world permanent is nobody's business.
    Spec.it s "CR 704.5k a lone world permanent survives" $ do
      (crossroads, _) <- worldPair s registry
      let (only, gs) = addWorld crossroads S.alice (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs worldPass
      Spec.assertBool s (inPlay only after) "it stays"

    -- The other half of the condition: an ordinary enchantment alongside a
    -- world one is not a pair. Bad Moon is the control -- an enchantment in
    -- every way except the supertype.
    Spec.it s "CR 704.5k a world permanent and an ordinary enchantment coexist" $ do
      (crossroads, _) <- worldPair s registry
      badMoon <- S.printingOf s registry "Bad Moon"
      let (world, g0) = addWorld crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (ordinary, gs) = S.addCreature badMoon S.alice g0
          after = S.runPure S.identityAnswer gs worldPass
      Spec.assertBool s (inPlay world after) "the world enchantment stays"
      Spec.assertBool s (inPlay ordinary after) "so does Bad Moon"

    -- "Put into their OWNERS' graveyards" -- alice controls bob's card, but
    -- bob owns it, so bob's graveyard is where it lands. (CR 400.7 gives it a
    -- fresh id on the way, so this counts the zone rather than naming the old
    -- one.)
    Spec.it s "CR 704.5k the loser goes to its OWNER's graveyard" $ do
      (crossroads, livingPlane) <- worldPair s registry
      let (his, g0) = addWorld crossroads S.bob (Setup.emptyGame S.bothPlayers)
          stolen = S.giveControl his S.alice g0
          (hers, gs) = addWorld livingPlane S.alice stolen
          after = S.runPure S.identityAnswer gs worldPass
      Spec.assertBool s (inPlay hers after) "alice's newer one stays"
      Spec.assertBool s (not (inPlay his after)) "the stolen one left the battlefield"
      Spec.assertEqWith s "one card in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
      Spec.assertEqWith s "and none in alice's" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

    -- CR 704.3: every applicable state-based action is performed
    -- "simultaneously as a single event", so ONE pass settles both rules.
    -- Night of Souls' Betrayal is legendary and Concordant Crossroads is
    -- world, so alice's board is two of each: the legend rule asks her which
    -- Night to keep and the world rule keeps the newer Crossroads without
    -- asking, and both losers are in the graveyard when that single pass
    -- returns. (No printing is legendary AND world, so the two rules cannot
    -- name the same permanent; the deduplicated batch they share is pinned by
    -- the legend rule's own tests above.)
    Spec.it s "CR 704.3 the world rule and the legend rule share one pass" $ do
      (crossroads, _) <- worldPair s registry
      night <- S.printingOf s registry "Night of Souls' Betrayal"
      let (oldWorld, g0) = addWorld crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (firstNight, g1) = S.addCreature night S.alice g0
          (secondNight, g2) = S.addCreature night S.alice g1
          (newWorld, gs) = addWorld crossroads S.alice g2
          after = S.runPure (keepsLegend firstNight) gs worldPass
      Spec.assertBool s (inPlay newWorld after) "the newest world permanent stays"
      Spec.assertBool s (not (inPlay oldWorld after)) "the older world permanent is gone"
      Spec.assertBool s (inPlay firstNight after) "the chosen legend stays"
      Spec.assertBool s (not (inPlay secondNight after)) "the other legend is gone"
      Spec.assertEqWith s "two cards buried, neither moved twice" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2

    -- CR 704.3 again, this time where two state-based actions name the SAME
    -- permanent. Opalescence animates every other non-Aura enchantment at its
    -- mana value, so the {G} Concordant Crossroads is a 1/1; Night of Souls'
    -- Betrayal takes every creature down -1/-1, so it is a 0/0. CR 704.5f
    -- buries it for its toughness and CR 704.5k buries it for being the older
    -- world permanent -- and the deduplicated batch must move it once, not
    -- twice, or its zone change (and any dies-trigger watching) would fire
    -- again.
    Spec.it s "CR 704.5f/704.5k a permanent both rules name is moved once" $ do
      (crossroads, livingPlane) <- worldPair s registry
      opalescence <- S.printingOf s registry "Opalescence"
      night <- S.printingOf s registry "Night of Souls' Betrayal"
      let (_, g0) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
          (_, g1) = S.addCreature night S.alice g0
          (doomed, g2) = addWorld crossroads S.alice g1
          (newer, gs) = addWorld livingPlane S.alice g2
      Spec.assertEqWith s "the animated Crossroads really is a 0/0" (Projection.toughnessOf doomed gs) (Just 0)
      let after = S.runPure S.identityAnswer gs worldPass
      Spec.assertBool s (not (inPlay doomed after)) "it is gone"
      Spec.assertBool s (inPlay newer after) "the newer world permanent stays"
      Spec.assertEqWith s "and exactly one card was buried" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

    -- The whole cards, cast: alice has a Concordant Crossroads out and casts
    -- Living Plane for its printed {2}{G}{G}. Nothing targets, nobody is
    -- asked, and the incumbent is in the graveyard by the time she has
    -- priority again -- the world rule as a player would meet it.
    Spec.it s "CR 704.5k whole cards: resolving Living Plane buries the Concordant Crossroads already out" $ do
      (crossroads, livingPlane) <- worldPair s registry
      forest <- S.printingOf s registry "Forest"
      let base = S.landsInPlay forest 4 -- {2}{G}{G}
          (incumbent, withCrossroads) = addWorld crossroads S.alice base
          (withSpell, spellId) = S.handOne livingPlane withCrossroads
          cast = snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))
          after = snd (Engine.runGamePure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority))
      Spec.assertBool s (not (inPlay incumbent after)) "the incumbent is gone"
      Spec.assertEqWith s "one card in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
      -- The survivor is on the battlefield and working: its static ability has
      -- made every Forest a creature, which is how this test knows the world
      -- rule buried the OLD one rather than the new arrival.
      Spec.assertEqWith s "and the four Forests are creatures now" (length (filter (\oid -> Projection.isCreatureOf oid after) (Set.toList (GameState.battlefield after)))) 4

    -- THE discriminating board for CR 704.5k's clock: a permanent that entered
    -- EARLIER but became world LATER. The Forest was on the battlefield first and
    -- the Living Plane second, so the entry timestamp says the Living Plane has
    -- been world for the shortest time -- and the rule says the Forest has, since
    -- it only became world when the Charter resolved. The two readings pick
    -- opposite survivors, so both assertions flip if the clock goes back to
    -- Object.timestamp.
    --
    -- Synthetic World Charter ("Target permanent becomes world until end of turn",
    -- CR 205.4a, CR 613.1d) is the producer: no printing grants or removes the
    -- world supertype.
    Spec.it s "CR 704.5k the clock is when it became world, not when it entered" $ do
      (_, livingPlane) <- worldPair s registry
      forest <- S.printingOf s registry "Forest"
      charter <- S.printingOf s registry "Synthetic World Charter"
      let (old, g0) = S.addCreature forest S.alice (Setup.emptyGame S.bothPlayers)
          -- Two more Forests, which is the Charter's {1}{G}.
          (_, g1) = S.addCreature forest S.alice g0
          (_, g2) = S.addCreature forest S.alice g1
          (plane, g3) = S.addCreature livingPlane S.alice g2
          (g4, spell) = S.handOne charter g3
          -- The Living Plane is stamped on this settle; the Forest is not world
          -- yet, so nothing is buried.
          settled = S.runPure S.identityAnswer g4 Engine.settleForPriority
          resolved = S.runPure (aimedAt old) settled (S.cast S.alice spell >> Stack.resolveTop)
          after = S.runPure S.identityAnswer resolved Engine.settleForPriority
      -- Without this the whole group could pass on a Charter that grants nothing:
      -- one world permanent is no rule at all.
      Spec.assertBool s (Set.member Supertype.World (PC.supertypes (Projection.project old resolved))) "the Charter really did make the Forest world"
      Spec.assertBool s (inPlay old after) "the Forest became world last, so it survives"
      Spec.assertBool s (not (inPlay plane after)) "the Living Plane has been world longer, so it is buried"

    -- CR 704.5k's second sentence: "in the event of a tie for the shortest amount
    -- of time, all are put into their owners' graveyards." Two permanents that
    -- become world in the SAME settle pass share one stamp and so tie.
    --
    -- Built from a continuous effect on each rather than two Charter resolutions:
    -- two spells resolve one after another and each resolution is followed by its
    -- own settle, so that route cannot produce a genuine same-pass tie.
    Spec.it s "CR 704.5k two permanents that become world in one pass tie, and both are buried" $ do
      forest <- S.printingOf s registry "Forest"
      let (a, g0) = S.addCreature forest S.alice (Setup.emptyGame S.bothPlayers)
          (b, g1) = S.addCreature forest S.alice g0
          gs = S.withEffect b (Modification.AddSupertype Supertype.World) (S.withEffect a (Modification.AddSupertype Supertype.World) g1)
          sampled = S.runPure S.identityAnswer gs Engine.sampleWorldSince
          after = S.runPure S.identityAnswer gs Engine.settleForPriority
      Spec.assertBool s (Maybe.isJust (worldSinceOf a sampled)) "both were stamped at all"
      Spec.assertEqWith s "one stamp for the whole pass, so the two tie" (worldSinceOf a sampled) (worldSinceOf b sampled)
      Spec.assertBool s (not (inPlay a after) && not (inPlay b after)) "a tie buries every world permanent"

    -- CR 400.7: the returning permanent is a new object, so its clock starts when
    -- it returned. The Crossroads was world first, went to hand while the Living
    -- Plane took the stamp, and came back last -- so it is the survivor. A clock
    -- carried across the move would make it the older world permanent and bury it.
    Spec.it s "CR 400.7 a world permanent that leaves and returns is world only since it returned" $ do
      (crossroads, livingPlane) <- worldPair s registry
      let (first, g0) = S.addCreature crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, g1) = S.runPureWith S.identityAnswer g0 (Engine.settleForPriority >> Event.changeZoneReturning first Zone.Hand)
          (plane, g2) = S.addCreature livingPlane S.alice g1
      case inHand of
        Nothing -> Spec.assertFailure s "the Crossroads should have reached alice's hand"
        Just held -> do
          -- The Living Plane is stamped on this settle, before the Crossroads is
          -- back on the battlefield to be stamped after it.
          let (back, g3) = S.runPureWith S.identityAnswer g2 (Engine.settleForPriority >> Event.changeZoneReturning held Zone.Battlefield)
              after = S.runPure S.identityAnswer g3 Engine.settleForPriority
          case back of
            Nothing -> Spec.assertFailure s "the Crossroads should have returned to the battlefield"
            Just again -> do
              Spec.assertBool s (inPlay again after) "the returned Crossroads became world last, so it survives"
              Spec.assertBool s (not (inPlay plane after)) "the Living Plane is buried"

    -- The stamp is not a latch: when the permanent stops being world the next
    -- settle clears it, so becoming world again starts a new clock. CR 514.2's
    -- sweep (Expiry.dropAtCleanup) is what ends the "until end of turn" grant.
    Spec.it s "CR 704.5k the clock is cleared when the permanent stops being world" $ do
      forest <- S.printingOf s registry "Forest"
      let (land, g0) = S.addCreature forest S.alice (Setup.emptyGame S.bothPlayers)
          gs = S.withEffect land (Modification.AddSupertype Supertype.World) g0
          stamped = S.runPure S.identityAnswer gs Engine.settleForPriority
          after = S.runPure S.identityAnswer (Expiry.dropAtCleanup stamped) Engine.settleForPriority
      Spec.assertBool s (Maybe.isJust (worldSinceOf land stamped)) "world, and stamped"
      Spec.assertBool s (Maybe.isNothing (worldSinceOf land after)) "no longer world, so no longer stamped"

sbaSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
sbaSpec s =
  Spec.describe s "Sba" $ do
    Spec.it s "drew-from-empty loses" $
      let after = S.settleSba sbaBase {GameState.drewFromEmpty = Set.singleton S.alice}
       in Spec.assertEqWith s "alice lost" (fmap Player.status (Map.lookup S.alice (GameState.players after))) (Just (Status.Departed Departure.Type.Lost))

    Spec.it s "one remaining player wins" $
      let after = S.settleSba sbaBase {GameState.drewFromEmpty = Set.singleton S.alice}
       in Spec.assertEqWith s "bob won" (GameState.result after) (Just (Result.Won S.bob))

    Spec.it s "life <= 0 loses" $
      let gs = sbaBase {GameState.players = Map.insert S.alice (Player.MkPlayer {Player.life = 0, Player.status = Status.Playing, Player.counters = Map.empty, Player.ringTemptations = 0, Player.speed = Nothing, Player.commander = Nothing, Player.commanderCasts = 0, Player.commanderDamage = Map.empty, Player.dungeon = Nothing}) (GameState.players sbaBase)}
       in Spec.assertEqWith s "bob won" (GameState.result (S.settleSba gs)) (Just (Result.Won S.bob))

    Spec.it s "simultaneous last departures draw" $
      let after = S.settleSba sbaBase {GameState.drewFromEmpty = Set.fromList [S.alice, S.bob]}
       in Spec.assertEqWith s "draw" (GameState.result after) (Just Result.Drawn)

    Spec.it s "CR 704.5c ten poison counters lose the game" $
      let gs = S.addPlayerCounter PlayerCounterKind.Poison 10 S.bob (Setup.emptyGame S.bothPlayers)
          after = S.settleSba gs
       in Spec.assertEqWith s "bob lost" (fmap Player.status (Map.lookup S.bob (GameState.players after))) (Just (Status.Departed Departure.Type.Lost))

    Spec.it s "CR 704.5c nine poison counters do not" $
      let gs = S.addPlayerCounter PlayerCounterKind.Poison 9 S.bob (Setup.emptyGame S.bothPlayers)
          after = S.settleSba gs
       in Spec.assertEqWith s "bob still playing" (fmap Player.status (Map.lookup S.bob (GameState.players after))) (Just Status.Playing)

damageEventSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageEventSpec s registry =
  Spec.describe s "DamageEvent" $ do
    Spec.it s "a blocked 2/1 trade emits both damage events" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, mine, theirs) = S.combatBoard piker 1 1
          after = S.fightWith S.aggressiveAnswer gs
          events = S.damageEventsOf after
      case (mine, theirs) of
        (a : _, b : _) -> do
          Spec.assertEqWith s "two events" (length events) 2
          Spec.assertBool
            s
            (elem (DamageEvent.MkDamageEvent a (Recipient.ToCreature b) 2 False False False 0 Nothing DamageKind.Combat) events)
            "attacker hit blocker for 2"
          Spec.assertBool
            s
            (elem (DamageEvent.MkDamageEvent b (Recipient.ToCreature a) 2 False False False 0 Nothing DamageKind.Combat) events)
            "blocker hit attacker for 2"
        _ -> Spec.assertFailure s "fixture should have one creature per side"

    Spec.it s "an unblocked 2/1 emits a ToPlayer event" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, mine, _) = S.combatBoard piker 1 0
          after = S.fightWith S.aggressiveAnswer gs
      case mine of
        a : _ ->
          Spec.assertEqWith
            s
            "one player event"
            (S.damageEventsOf after)
            [DamageEvent.MkDamageEvent a (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat]
        _ -> Spec.assertFailure s "fixture should have an attacker"

deathtouchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
deathtouchSpec s registry =
  Spec.describe s "Deathtouch" $ do
    Spec.it s "CR 704.5h a 1/1 deathtoucher destroys a 3/3 it deals 1 to" $ do
      -- Typhoid Rats attacks, Ogre Sentry blocks. Rat deals 1 -> Ogre dies by
      -- 704.5h (toughness 3, not lethal by the numbers); Ogre's 3 kills the Rat.
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
          after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
      Spec.assertEqWith s "the Ogre is dead" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "the Rat is dead" (S.creaturesInPlay S.alice after) 0

    Spec.it s "CR 704.5g the control: a 2/1 without deathtouch leaves the 3/3 alive" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [piker] [ogreSentry]
          after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
      Spec.assertEqWith s "the Ogre survives" (S.creaturesInPlay S.bob after) 1

    Spec.it s "the SBA check consumes the damage events by watermark, not by draining" $ do
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
          after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
      Spec.assertEqWith s "nothing left unscanned" (Event.unscannedDamage after) []
      Spec.assertBool s (not (null (S.damageEventsOf after))) "the record survives (CR 608.2i)"

    Spec.it s "CR 702.2e the deal-time bit is true for a real deathtoucher, false for a plain source" $ do
      -- Typhoid Rats (deathtouch) and Ogre Sentry trade combat damage under
      -- aggressiveAnswer (which DOES declare attackers). fightWith runs no SBAs,
      -- so the wave is still unscanned in the turn log.
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs, rats, ogres) = S.combatBoardOf [typhoidRats] [ogreSentry]
          fought = S.fightWith S.aggressiveAnswer gs
          ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
          ogreId = case ogres of o : _ -> o; [] -> ObjectId.MkObjectId 999
          bitFor src = any (\ev -> DamageEvent.source ev == src && DamageEvent.dealtByDeathtouch ev) (S.damageEventsOf fought)
      Spec.assertBool s (bitFor ratId) "Rat's damage is flagged deathtouch"
      Spec.assertBool s (not (bitFor ogreId)) "Ogre's damage is not"

    Spec.it s "CR 702.2e Humility removes deathtouch, so the deal-time bit is false" $ do
      -- Under Humility the Rat loses deathtouch (layer 6); its combat-damage
      -- event's bit is false -- asserted directly on the event, not via a kill.
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      humility <- S.printingOf s registry "Humility"
      let (gs0, rats, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
          gs = S.withHumility humility gs0
          fought = S.fightWith S.aggressiveAnswer gs
          ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
          ratBit = any (\ev -> DamageEvent.source ev == ratId && DamageEvent.dealtByDeathtouch ev) (S.damageEventsOf fought)
      Spec.assertBool s (not ratBit) "no deathtouch at deal time under Humility"

-- Both halves of CR 510.1e for ONE attacker assigning by itself: the damage every
-- creature is assigning in the step is this creature's own answer, so the gates
-- read exactly what they read before the step's total became their comparand. The
-- cases where another creature's damage is what clears a gate are gameplay-level,
-- in Pawl.CombatSpec's TrampleOverPlaneswalkers and SharedBlocker groups.
--
-- No deathtouch anywhere in the step, so CR 702.2c's set is empty and every bar
-- is cleared by the numbers alone.
assignedAlone :: Map.Map Recipient.Recipient Natural.Natural -> Natural.Natural -> Map.Map Recipient.Recipient Natural.Natural -> Bool
assignedAlone thresholds power answer =
  Damage.wellFormedAssignment thresholds power answer
    && Damage.tiersCleared thresholds answer Set.empty answer

assignmentLegalitySpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
assignmentLegalitySpec s =
  Spec.describe s "AssignmentLegality" $ do
    Spec.it s "under-assignment with no overflow is legal (power below lethal)" $
      -- One blocker, lethal 3, power 2, defender present with threshold 0.
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 2)]
       in Spec.assertBool s (assignedAlone thresholds 2 answer) "accepted"

    Spec.it s "defender damage while a blocker is short is illegal" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 0),
                (Recipient.ToPlayer S.bob, 3)
              ]
       in Spec.assertBool s (not (assignedAlone thresholds 3 answer)) "rejected"

    Spec.it s "defender damage once the blocker has lethal is legal" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                (Recipient.ToPlayer S.bob, 2)
              ]
       in Spec.assertBool s (assignedAlone thresholds 3 answer) "accepted"

    Spec.it s "an answer that does not total power is illegal" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 0)]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 1)]
       in Spec.assertBool s (not (assignedAlone thresholds 2 answer)) "rejected"

    Spec.it s "an illegal recipient is rejected" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 0)]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 2), 2)]
       in Spec.assertBool s (not (assignedAlone thresholds 2 answer)) "rejected"

    -- The threshold gates the DEFENDER's share; it is not a cap on the
    -- blocker's. CR 702.19b lets the attacker assign past lethal and spill
    -- what is left, so an over-assigned blocker is no obstacle.
    Spec.it s "over-assigning a blocker and still spilling over is legal" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 2)
              ]
       in Spec.assertBool s (assignedAlone thresholds 5 answer) "accepted"

    -- CR 702.19b gates the defender on ALL blocking creatures having lethal,
    -- so this pair is the quantifier: one blocker short rejects the very same
    -- defender share that the twin below accepts once it is filled in. A gate
    -- reading "some blocker is at lethal" passes the first and is caught here.
    -- Two blockers is also the shape the prompt is actually reached in
    -- (Damage.attackerAssignment forces the single-blocker case unless the
    -- attacker tramples past its threshold).
    Spec.it s "two blockers: one short of lethal gates the defender" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 2),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 1),
                (Recipient.ToPlayer S.bob, 2)
              ]
       in Spec.assertBool s (not (assignedAlone thresholds 5 answer)) "rejected"

    Spec.it s "two blockers: both at lethal frees the defender" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 2),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 2),
                (Recipient.ToPlayer S.bob, 1)
              ]
       in Spec.assertBool s (assignedAlone thresholds 5 answer) "accepted"

    -- CR 702.19c's SECOND tier, the mirror of the blocker pair above: with
    -- trample over planeswalkers the map holds the attacked planeswalker at its
    -- loyalty AND that planeswalker's controller at 0, and the player's share is
    -- gated on the planeswalker having been assigned its whole loyalty. Power 7
    -- over loyalty 3, so the two answers below differ only in whether Jace is one
    -- short.
    Spec.it s "CR 702.19c the planeswalker one short of its loyalty gates its controller" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToPlaneswalker (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToPlaneswalker (ObjectId.MkObjectId 1), 2),
                (Recipient.ToPlayer S.bob, 5)
              ]
       in Spec.assertBool s (not (assignedAlone thresholds 7 answer)) "rejected"

    Spec.it s "CR 702.19c the planeswalker at its loyalty frees its controller" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToPlaneswalker (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToPlaneswalker (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 4)
              ]
       in Spec.assertBool s (assignedAlone thresholds 7 answer) "accepted"

    -- "At least equal to the loyalty" (CR 702.19c), so the threshold is a floor on
    -- the planeswalker's share and not a cap on it.
    Spec.it s "CR 702.19c over-paying the loyalty and still spilling over is legal" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToPlaneswalker (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToPlaneswalker (ObjectId.MkObjectId 1), 5),
                (Recipient.ToPlayer S.bob, 2)
              ]
       in Spec.assertBool s (assignedAlone thresholds 7 answer) "accepted"

    -- Without trample the defending player is not among the thresholds at all
    -- (Damage.attackerAssignment adds that entry only for a trampler), so a
    -- point aimed at them is an illegal RECIPIENT rather than a gated one.
    Spec.it s "with no trample the defending player is not a legal recipient" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 0),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 1),
                (Recipient.ToPlayer S.bob, 1)
              ]
       in Spec.assertBool s (not (assignedAlone thresholds 3 answer)) "rejected"

-- Sends the whole assignment to the FIRST blocker when the defending player is
-- asked, and to the SECOND when the attacker's controller is. Which creature
-- dies then reports who the CR 702.22j chooser was, without the test having to
-- inspect a prompt it cannot otherwise observe.
askedOf :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
askedOf blockers p = case p of
  Prompt.AssignCombatDamage _ asked _ _ n ->
    let target = case (asked == S.bob, blockers) of
          (True, first : _) -> Just first
          (False, _ : second : _) -> Just second
          _ -> Nothing
     in maybe Map.empty (\oid -> Map.singleton (Recipient.ToCreature oid) n) target
  _ -> S.aggressiveAnswer p

-- Assigns each blocker exactly its threshold, and every leftover point to the
-- defender. A legal trample division for these boards.
tramplingAnswer :: Prompt.Prompt r -> r
tramplingAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        toBlockers = Map.fromList (fmap (\(r, t) -> (r, t)) blockers)
        spent = sum (fmap snd blockers)
        leftover = if n >= spent then n - spent else 0
        defenders = filter (not . S.isCreatureRecipient) (Map.keys thresholds)
     in case defenders of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> S.aggressiveAnswer p

trampleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trampleSpec s registry =
  Spec.describe s "Trample" $ do
    Spec.it s "CR 702.19b a 3/3 trampler spills excess onto the defending player" $ do
      -- War Mammoth (3/3 trample) blocked by a Piker (2/1): 1 lethal to the
      -- Piker, 2 to bob. Mammoth survives (2 marked < 3).
      warMammoth <- S.printingOf s registry "War Mammoth"
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, _) = S.combatBoardOf [warMammoth] [piker]
          after = S.settleSba (S.fightWith tramplingAnswer gs)
      Spec.assertEqWith s "bob took the 2 overflow" (S.lifeOf S.bob after) (Just 18)
      Spec.assertEqWith s "the Piker is dead" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "the Mammoth survives" (S.creaturesInPlay S.alice after) 1

    -- CR 702.22j: with a banding creature blocking, the DEFENDING player divides
    -- the attacking creature's damage. The pair below differs only in which
    -- blocker has banding, so it pins the chooser rather than the arithmetic --
    -- the answerer sends the whole amount to a DIFFERENT creature depending on
    -- who it was asked of, and the board says which happened.
    Spec.it s "CR 702.22j a banding blocker moves the division to the defender" $ do
      warMammoth <- S.printingOf s registry "War Mammoth"
      benalishHero <- S.printingOf s registry "Benalish Hero"
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, blockers) = S.combatBoardOf [warMammoth] [benalishHero, piker]
          after = S.settleSba (S.fightWith (askedOf blockers) gs)
      Spec.assertEqWith s "bob was asked, so the FIRST blocker took it all and died" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Benalish Hero")) S.bob after) 0
      Spec.assertEqWith s "and the Piker, which took none, survives" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.bob after) 1

    Spec.it s "CR 510.1c without banding the attacker's controller still divides" $ do
      warMammoth <- S.printingOf s registry "War Mammoth"
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, blockers) = S.combatBoardOf [warMammoth] [piker, warMammoth]
          after = S.settleSba (S.fightWith (askedOf blockers) gs)
      Spec.assertEqWith s "alice was asked, so the SECOND blocker took it and died" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "War Mammoth")) S.bob after) 0
      Spec.assertEqWith s "and the Piker, the first blocker, survives" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.bob after) 1

    Spec.it s "CR 702.19b a non-trample control spills nothing" $ do
      -- Ogre Sentry is a 3/3 that cannot attack (defender), so use the Piker's
      -- existing behavior as the control: a blocked non-trample attacker deals
      -- nothing to the player. (combatDamageSpec already asserts bob = 20.)
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, _) = S.combatBoardOf [piker] [piker]
          after = S.fightWith tramplingAnswer gs
      Spec.assertEqWith s "bob untouched by a non-trampler" (S.lifeOf S.bob after) (Just 20)

    Spec.it s "CR 702.19b defender-short assignment is rejected" $ do
      -- A cheat responder gives bob 3 while the Piker gets 0. Illegal: the
      -- attacker deals nothing, bob untouched, Piker survives.
      warMammoth <- S.printingOf s registry "War Mammoth"
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, _) = S.combatBoardOf [warMammoth] [piker]
          cheat p = case p of
            Prompt.AssignCombatDamage _ _ _ thresholds n ->
              case filter (not . S.isCreatureRecipient) (Map.keys thresholds) of
                d : _ -> Map.singleton d n
                [] -> Map.empty
            _ -> S.aggressiveAnswer p
          after = S.settleSba (S.fightWith cheat gs)
      Spec.assertEqWith s "bob untouched" (S.lifeOf S.bob after) (Just 20)
      Spec.assertEqWith s "the Piker survives the rejected assignment" (S.creaturesInPlay S.bob after) 1

    Spec.it s "CR 702.19b under-assignment across two blockers spills nothing (power below total lethal)" $ do
      -- War Mammoth (3/3 trample) blocked by TWO Ogre Sentries (3/3 each): it
      -- cannot reach lethal on both (needs 6, has 3), so no overflow -- bob is
      -- untouched -- and the division among the Ogres is free. Real cards, for
      -- the under-assignment case assignmentLegalitySpec pins on the predicate.
      warMammoth <- S.printingOf s registry "War Mammoth"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [warMammoth] [ogreSentry, ogreSentry]
          dumpOne p = case p of
            Prompt.AssignCombatDamage _ _ _ thresholds n ->
              case filter S.isCreatureRecipient (Map.keys thresholds) of
                r : _ -> Map.singleton r n
                [] -> Map.empty
            _ -> S.aggressiveAnswer p
          after = S.settleSba (S.fightWith dumpOne gs)
      Spec.assertEqWith s "bob untouched (no overflow)" (S.lifeOf S.bob after) (Just 20)
      Spec.assertEqWith s "one Ogre took all 3 and died, the other lived" (S.creaturesInPlay S.bob after) 1

-- #29: a blocker declared in the declare blockers step can be gone by the combat
-- damage step -- since M3a the pool has instant-speed removal. CR 509.1h keeps the
-- attacker BLOCKED (the combat map is the record and is deliberately not mutated,
-- #28), but CR 510.1c assigns damage only to the creatures CURRENTLY blocking it.
-- Removal happens between declareBlockers and dealCombatDamage, which is exactly
-- when a Murder resolves.
killBlockerMidCombat :: ObjectId.ObjectId -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
killBlockerMidCombat victim answer gs =
  S.runPure answer gs $ do
    Combat.declareAttackers S.alice
    Combat.declareBlockers
    Event.destroy Regenerability.Regenerable [victim]
    Monad.void Damage.dealCombatDamage

-- Every DamageDealt event in the history addressed to `oid`, however much.
damageEventsTo :: ObjectId.ObjectId -> GameState.GameState -> [DamageEvent.DamageEvent]
damageEventsTo oid gs =
  let pick ev = case ev of
        GameEvent.DamageDealt de ->
          if DamageEvent.target de == Recipient.ToCreature oid then [de] else []
        _ -> []
   in concatMap pick (S.eventsOf gs)

-- Sinks the whole assignment into the first creature recipient offered. This is
-- the discriminating answer for #29: if the engine still offers a departed
-- blocker, the damage lands on the ghost and evaporates, so any assertion about
-- where the damage really went fails. An answer that routes by threshold (like
-- tramplingAnswer) would pass either way, because a departed blocker's threshold
-- computes to 0.
dumpOntoFirstCreature :: Prompt.Prompt r -> r
dumpOntoFirstCreature p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  _ -> S.aggressiveAnswer p

departedBlockerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
departedBlockerSpec s registry =
  Spec.describe s "Departed blockers (#29)" $ do
    Spec.it s "CR 702.19d a trampler whose only blocker left assigns everything to the player" $ do
      -- War Mammoth (3/3 trample) is blocked by a Piker (2/1); the Piker is
      -- destroyed before damage. "As though all blocking creatures have been
      -- assigned lethal damage" -- so all 3 hit bob, and there is nothing left
      -- to choose. dumpOntoFirstCreature sinks everything into a creature
      -- recipient if one is offered, so an offered ghost shows up as bob
      -- taking nothing.
      warMammoth <- S.printingOf s registry "War Mammoth"
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, theirs) = S.combatBoardOf [warMammoth] [piker]
      case theirs of
        blocker : _ ->
          let after = S.settleSba (killBlockerMidCombat blocker dumpOntoFirstCreature gs)
           in do
                Spec.assertEqWith s "bob took all 3" (S.lifeOf S.bob after) (Just 17)
                Spec.assertEqWith s "nothing was addressed to the departed blocker" (damageEventsTo blocker after) []
        [] -> Spec.assertFailure s "fixture did not build a blocker"

    Spec.it s "CR 510.1c a non-trampler whose only blocker left assigns no combat damage" $ do
      -- A Piker (2/1) blocked by a Piker that then dies. "If no creatures are
      -- currently blocking it ... it assigns no combat damage." bob is untouched
      -- either way, so the observable is the history: the engine must not record
      -- a DamageDealt addressed to a creature that is not there.
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, _, theirs) = S.combatBoardOf [piker] [piker]
      case theirs of
        blocker : _ ->
          let after = S.settleSba (killBlockerMidCombat blocker S.aggressiveAnswer gs)
           in do
                Spec.assertEqWith s "bob untouched" (S.lifeOf S.bob after) (Just 20)
                Spec.assertEqWith s "no phantom damage event" (damageEventsTo blocker after) []
        [] -> Spec.assertFailure s "fixture did not build a blocker"

    Spec.it s "CR 510.1c a partly-departed block assigns only among the survivors" $ do
      -- War Mammoth (3/3 trample) blocked by two Ogre Sentries (3/3); one dies
      -- before damage. One live blocker with lethal exactly 3 leaves nothing to
      -- divide, so this is forced: all 3 onto the survivor, which then dies.
      -- With the ghost still in the list the assignment is a free division and
      -- the whole 3 can sink into it, leaving the survivor untouched.
      warMammoth <- S.printingOf s registry "War Mammoth"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs, _, theirs) = S.combatBoardOf [warMammoth] [ogreSentry, ogreSentry]
      case theirs of
        dead : _ : _ ->
          let after = S.settleSba (killBlockerMidCombat dead dumpOntoFirstCreature gs)
           in do
                Spec.assertEqWith s "the surviving Ogre took the full 3 and died" (S.creaturesInPlay S.bob after) 0
                Spec.assertEqWith s "bob untouched (3 power, 3 lethal, no excess)" (S.lifeOf S.bob after) (Just 20)
        _ -> Spec.assertFailure s "fixture did not build two blockers"

-- CR 509.1h at whole-card level: alice attacks, bob blocks, and a REAL Lightning Bolt
-- -- cast, paid for, targeted, resolved off the stack, with the CR 704.5g SBA
-- doing the killing -- removes the blocker before the combat damage step. The
-- direct-call twin above (killBlockerMidCombat) reaches the same state through
-- Event.destroy; this one proves the door a player actually uses gets there too.
--
-- `blocks` routes the whole board's blockers at the first attacker (or declines,
-- for the control leg). The state is split at the SBA because S.settleSba is a
-- plain GameState -> GameState, not a Game action.
boltBlockerMidCombat :: Bool -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
boltBlockerMidCombat blocks bolt blocker gs =
  let aimedAtBlocker :: Prompt.Prompt r -> r
      aimedAtBlocker p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature blocker))) sets
        Prompt.DeclareBlockers {} | not blocks -> Map.empty
        _ -> S.aggressiveAnswer p
      declared =
        S.runPure aimedAtBlocker gs $ do
          Combat.declareAttackers S.alice
          Combat.declareBlockers
          S.cast S.alice bolt
          Monad.void Stack.resolveTop
   in S.runPure S.aggressiveAnswer (S.settleSba declared) (Monad.void Damage.dealCombatDamage)

-- CR 509.1h's last sentence -- "A creature remains blocked even if all the
-- creatures blocking it are removed from combat" -- is a STATUS the declaration
-- confers, not a running count of who is still blocking. Combat.blockers spells it
-- with the attacker's KEY, and the two ways the set behind that key can empty out
-- are covered here: the blocker destroyed (the key survives untouched, and
-- Damage.attackerAssignment's liveness filter drops it), and the blocker removed
-- from combat by regenerating (CR 506.4/701.19a: Game.removeFromCombat empties the
-- set but keeps the key).
--
-- Both end at the same observable: the attacker assigns no combat damage at all
-- (CR 510.1c), so the defending player takes nothing. Reading emptiness as
-- unblocked -- the bug this group pins -- lets the attacker through instead.
blockedStaysBlockedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blockedStaysBlockedSpec s registry =
  Spec.describe s "Blocked stays blocked (CR 509.1h)" $ do
    Spec.it s "CR 510.1c a blocker Bolted after blocks are declared leaves the attacker blocked, so the defender takes nothing" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      mountain <- S.printingOf s registry "Mountain"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (board, mine, theirs) = S.combatBoardOf [piker] [piker]
          (_, withLand) = S.addCreature mountain S.alice board
          (bolt, gs) = S.addHandCard lightningBolt S.alice withLand
      case (mine, theirs) of
        (attacker : _, blocker : _) -> do
          let after = boltBlockerMidCombat True bolt blocker gs
              -- The control leg: the SAME Bolt on the SAME blocker, but bob
              -- declines to block. Nothing is blocking either way, so this is
              -- what discriminates "blocked with no blockers left" (assigns
              -- nothing) from "never blocked" (assigns to the player) -- without
              -- it, an engine that simply never dealt this damage would pass.
              unblocked = boltBlockerMidCombat False bolt blocker gs
          Spec.assertBool s (not (Set.member blocker (GameState.battlefield after))) "the Bolt killed the blocker"
          Spec.assertBool s (Combat.isBlocked attacker after) "the attacker is still blocked"
          Spec.assertEqWith s "so bob takes nothing" (S.lifeOf S.bob after) (Just 20)
          Spec.assertBool s (not (Combat.isBlocked attacker unblocked)) "unblocked control leg: not blocked"
          Spec.assertEqWith s "unblocked control leg: bob takes the Piker's 2" (S.lifeOf S.bob unblocked) (Just 18)
        _ -> Spec.assertFailure s "fixture did not build an attacker and a blocker"

    Spec.it s "CR 701.19a a blocker that regenerates is removed from combat, and the attacker is STILL blocked" $ do
      -- Drudge Skeletons blocks, then regenerates off alice's Bolt. CR 701.19a's
      -- rewrite ends with "If it's an attacking or blocking creature, remove it
      -- from combat," so unlike the destroyed blocker above this one is still on
      -- the battlefield and is genuinely removed from combat rather than merely
      -- dead. The shield is seeded rather than activated: what is under test is
      -- CR 509.1h, and bob paying {B} for his own ability is ActivateSpec's
      -- subject.
      piker <- S.printingOf s registry "Goblin Piker"
      mountain <- S.printingOf s registry "Mountain"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
      let (board, mine, theirs) = S.combatBoardOf [piker] [drudgeSkeletons]
          (_, withLand) = S.addCreature mountain S.alice board
          (bolt, gs0) = S.addHandCard lightningBolt S.alice withLand
      case (mine, theirs) of
        (attacker : _, blocker : _) -> do
          let after = boltBlockerMidCombat True bolt blocker (S.addRegenShield blocker gs0)
          Spec.assertBool s (Set.member blocker (GameState.battlefield after)) "CR 701.19a: the Skeletons survived the Bolt"
          Spec.assertEqWith s "CR 701.19a: and its damage was removed" (S.damageOf blocker after) (Just 0)
          Spec.assertEqWith s "CR 506.4: it is no longer blocking anything" (Combat.blockersOf attacker after) Set.empty
          Spec.assertBool s (Combat.isBlocked attacker after) "CR 509.1h: but the attacker remains blocked"
          Spec.assertEqWith s "CR 510.1c: so it assigns no combat damage and bob takes nothing" (S.lifeOf S.bob after) (Just 20)
          Spec.assertEqWith s "CR 510.1d: and the regenerated blocker assigns nothing back" (S.damageOf attacker after) (Just 0)
          -- The Bolt's own 3 is in the history too, so this filters to combat
          -- damage: what must be absent is the attacker hitting a creature the
          -- rules say is no longer blocking it (CR 510.1c).
          Spec.assertEqWith s "and no COMBAT damage was addressed to it either" (filter (\ev -> DamageEvent.kind ev == DamageKind.Combat) (damageEventsTo blocker after)) []
        _ -> Spec.assertFailure s "fixture did not build an attacker and a blocker"

-- The mirror of killBlockerMidCombat: the ATTACKER is gone by the combat damage
-- step. CR 506.4 removes it from combat, so by CR 510.1d its blockers are
-- blocking nothing -- but Combat.blockers is keyed BY the attacker and that key
-- is never pruned (CR 509.1h, #28), so the stale key is what reaches
-- Damage.blockerAssignment.
killAttackerMidCombat :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
killAttackerMidCombat victim gs =
  S.runPure S.aggressiveAnswer gs $ do
    Combat.declareAttackers S.alice
    Combat.declareBlockers
    Event.destroy Regenerability.Regenerable [victim]
    Monad.void Damage.dealCombatDamage

departedAttackerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
departedAttackerSpec s registry =
  Spec.describe s "Departed attackers (CR 510.1d)" $ do
    Spec.it s "CR 510.1d a blocker whose attacker was destroyed mid-combat assigns no combat damage" $ do
      -- CR 510.1d: "A blocking creature assigns combat damage to the creatures
      -- it's blocking. If it isn't currently blocking any creatures (if, for
      -- example, they were destroyed or removed from combat), it assigns no
      -- combat damage." The attacker is destroyed between declare blockers and
      -- the combat damage step -- CR 506.4 removes it from combat.
      --
      -- What the assertion catches: an implementation that filters only the
      -- BLOCKER (Damage.blockerAssignment's Projection.powerOf reads the
      -- blocker, which is alive) and so emits a DamageEvent addressed to the
      -- dead attacker. Marking that damage is a no-op the moment the object is
      -- gone from GameState.objects, so the mark is NOT the observable -- the
      -- CR 608.2i history is. The control leg proves the assertion is not
      -- vacuous: with the attacker alive the same board records exactly that
      -- event.
      piker <- S.printingOf s registry "Goblin Piker"
      let (gs, mine, _) = S.combatBoardOf [piker] [piker]
      case mine of
        attacker : _ -> do
          Spec.assertEqWith s "nothing was addressed to the destroyed attacker" (damageEventsTo attacker (killAttackerMidCombat attacker gs)) []
          Spec.assertEqWith s "and with the attacker alive the blocker DOES hit it -- the filter is what did it" (fmap DamageEvent.amount (damageEventsTo attacker (S.fightWith S.aggressiveAnswer gs))) [2]
        [] -> Spec.assertFailure s "fixture did not build an attacker"

    Spec.it s "CR 510.1d a blocker whose attacker left the game assigns no combat damage" $ do
      -- The departure route to the same stale key. Three seats, because at two
      -- the concession ends the game (CR 104.2a). CR 800.4a's first clause
      -- deletes alice's attacker outright and drops its Combat.attackers entry,
      -- but the Combat.blockers KEY survives -- deliberately, since CR 509.1h's
      -- last sentence is about the blockers' side of that record -- so bob's
      -- blocker is still handed a dead attacker to hit.
      piker <- S.printingOf s registry "Goblin Piker"
      let (attacker, b1) = S.addCreature piker S.alice S.threePlayerGame
          (blocker, b2) = S.addCreature piker S.bob b1
          fighting =
            b2
              { GameState.combat =
                  Combat.Type.MkCombat
                    { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.bob),
                      Combat.Type.blockers = Map.singleton attacker (Set.singleton blocker),
                      Combat.Type.struckFirst = Nothing,
                      -- CR 508.1k / 509.1g: each joined combat under its own
                      -- controller, which is what a declaration would have
                      -- recorded and what CR 506.4 compares against.
                      Combat.Type.joinedUnder = Map.fromList [(attacker, S.alice), (blocker, S.bob)],
                      Combat.Type.attacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      Combat.Type.declaredAttacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      -- Empty, because this board stands after the declare attackers
                      -- step ended: CR 500.1 scopes this half to the step, and
                      -- Combat.clearAttackedThisStep empties it as one ends.
                      Combat.Type.declaredAttackedThisStep = Set.empty,
                      -- CR 508.1a / 509.1a: this board is hand-built rather
                      -- than declared, so nothing was declared on it.
                      Combat.Type.declaredAttackers = Set.empty,
                      Combat.Type.declaredBlockers = Set.empty,
                      -- CR 506.7b: the fixture stands after CR 509.1's
                      -- declaration, which is what put the entry in
                      -- Combat.blockers above.
                      Combat.Type.blockersDeclared = True,
                      Combat.Type.defender = Just S.bob
                    }
              }
          gone = S.departs Departure.Type.Conceded S.alice fighting
          (assignedAfter, _) = S.runPureWith S.identityAnswer gone (Damage.gatherCombatDamage (const True))
          (assignedBefore, _) = S.runPureWith S.identityAnswer fighting (Damage.gatherCombatDamage (const True))
      Spec.assertBool s (Map.member attacker (Combat.Type.blockers (GameState.combat gone))) "CR 509.1h: the blockers key really is still there, so this is the live path"
      Spec.assertEqWith s "no assignment names the departed attacker" (filter (\ev -> DamageEvent.target ev == Recipient.ToCreature attacker) assignedAfter) []
      Spec.assertEqWith s "and with alice still in the game the blocker's hit is assigned -- the filter is what did it" (fmap DamageEvent.amount (filter (\ev -> DamageEvent.target ev == Recipient.ToCreature attacker) assignedBefore)) [2]

-- CR 800.4e: "If combat damage would be assigned to a player who has left the
-- game, that damage isn't assigned." attackerAssignment reads the defender's
-- status at two independent sites -- the unblocked/trample-through toDefender
-- list, and the CR 702.19b threshold map the assignment prompt offers -- and
-- both need coverage.
--
-- S.identityAnswer's AssignCombatDamage arm dumps the WHOLE amount onto the
-- first CREATURE recipient it finds (Support.hs), never a player one, so it
-- cannot tell whether a ToPlayer entry is present in the threshold map at all:
-- guarded or not, a blocked trampler's excess lands on the blocker either way
-- under that answerer. It is fine for the unblocked path (no prompt is ever
-- issued there), but the trample threshold map needs an answerer that actually
-- spends the excess on a player recipient when one is offered.
-- defenderOrBlockerAnswer assigns each blocker exactly its threshold and routes
-- the leftover to a player recipient if the threshold map offers one, falling
-- back onto the blocker (over-lethal, still legal -- a threshold is a floor and
-- Damage.tiersCleared has no upper bound) when it does not. That is what actually
-- surfaces whether the departed defender was offered.
defenderOrBlockerAnswer :: Prompt.Prompt r -> r
defenderOrBlockerAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockerEntries = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        toBlockers = Map.fromList blockerEntries
        spent = sum (fmap snd blockerEntries)
        leftover = if n >= spent then n - spent else 0
        defenders = filter (not . S.isCreatureRecipient) (Map.keys thresholds)
     in case defenders of
          d : _ -> Map.insert d leftover toBlockers
          [] -> case blockerEntries of
            (r, _) : _ -> Map.insertWith (+) r leftover toBlockers
            [] -> toBlockers
  _ -> S.aggressiveAnswer p

departedDefenderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
departedDefenderSpec s registry =
  Spec.describe s "Departed defender (CR 800.4e)" $ do
    Spec.it s "CR 800.4e no combat damage is assigned to a player who has left the game" $ do
      -- CR 800.4e: "If combat damage would be assigned to a player who has left
      -- the game, that damage isn't assigned." Reachable: a defending player can
      -- concede between the declare-attackers step and the combat damage step.
      -- Three seats, because at two the concession ends the game (CR 104.2a).
      piker <- S.printingOf s registry "Goblin Piker"
      let (attacker, board) = S.addCreature piker S.alice S.threePlayerGame
          attacking =
            board
              { GameState.combat =
                  Combat.Type.MkCombat
                    { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.bob),
                      Combat.Type.blockers = Map.empty,
                      Combat.Type.struckFirst = Nothing,
                      Combat.Type.joinedUnder = Map.singleton attacker S.alice,
                      Combat.Type.attacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      Combat.Type.declaredAttacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      -- Empty, because this board stands after the declare attackers
                      -- step ended: CR 500.1 scopes this half to the step, and
                      -- Combat.clearAttackedThisStep empties it as one ends.
                      Combat.Type.declaredAttackedThisStep = Set.empty,
                      -- CR 508.1a / 509.1a: this board is hand-built rather
                      -- than declared, so nothing was declared on it.
                      Combat.Type.declaredAttackers = Set.empty,
                      Combat.Type.declaredBlockers = Set.empty,
                      -- CR 506.7b: attackers are declared and blockers are not,
                      -- which is the moment the comment above describes.
                      Combat.Type.blockersDeclared = False,
                      Combat.Type.defender = Just S.bob
                    }
              }
          gone = S.departs Departure.Type.Conceded S.bob attacking
          (assignedAfter, _) = S.runPureWith S.identityAnswer gone (Damage.gatherCombatDamage (const True))
          (assignedBefore, _) = S.runPureWith S.identityAnswer attacking (Damage.gatherCombatDamage (const True))
      Spec.assertEqWith s "nothing is assigned to the departed defender" assignedAfter []
      Spec.assertEqWith s "and with bob still in the game the same board assigns one hit -- the guard is what did it" (length assignedBefore) 1
      Spec.assertEqWith s "to bob" (fmap DamageEvent.target assignedBefore) [Recipient.ToPlayer S.bob]

    Spec.it s "CR 800.4e a departed defender is not offered as a trample recipient either" $ do
      -- CR 702.19b assigns trample's excess "as its controller chooses", and the
      -- defending player is one of the choices Prompt.AssignCombatDamage offers.
      -- CR 800.4e removes the damage, so the choice must not be offered: an
      -- assignment the engine then discards would silently take damage away from
      -- the blockers it could otherwise have gone to.
      --
      -- CAROL is the defender and the one who leaves, and BOB's Piker blocks, so
      -- the blocker survives the departure and the board stays in the prompt arm.
      -- (Making the defender the blocker's controller would work too, right up
      -- to the point where CR 800.4a's first clause removes their blocker and
      -- the board falls out of that arm entirely.) War Mammoth is a 3/3 with
      -- trample; the Piker is a 2/1, so there is excess and a real choice.
      --
      -- S.identityAnswer is not the discriminating answerer here (see the
      -- group comment above): it never picks a player recipient, so a blocked
      -- trampler's excess lands on the blocker whether the defender is offered
      -- or not. defenderOrBlockerAnswer is used for both legs instead.
      warMammoth <- S.printingOf s registry "War Mammoth"
      piker <- S.printingOf s registry "Goblin Piker"
      let (attacker, b1) = S.addCreature warMammoth S.alice S.threePlayerGame
          (blocker, b2) = S.addCreature piker S.bob b1
          attacking =
            b2
              { GameState.combat =
                  Combat.Type.MkCombat
                    { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.carol),
                      Combat.Type.blockers = Map.singleton attacker (Set.singleton blocker),
                      Combat.Type.struckFirst = Nothing,
                      Combat.Type.joinedUnder = Map.fromList [(attacker, S.alice), (blocker, S.bob)],
                      Combat.Type.attacked = Set.singleton (AttackTarget.OfPlayer S.carol),
                      Combat.Type.declaredAttacked = Set.singleton (AttackTarget.OfPlayer S.carol),
                      -- Empty, because this board stands after the declare attackers
                      -- step ended: CR 500.1 scopes this half to the step, and
                      -- Combat.clearAttackedThisStep empties it as one ends.
                      Combat.Type.declaredAttackedThisStep = Set.empty,
                      -- CR 508.1a / 509.1a: this board is hand-built rather
                      -- than declared, so nothing was declared on it.
                      Combat.Type.declaredAttackers = Set.empty,
                      Combat.Type.declaredBlockers = Set.empty,
                      Combat.Type.blockersDeclared = True,
                      Combat.Type.defender = Just S.carol
                    }
              }
          gone = S.departs Departure.Type.Conceded S.carol attacking
          (assignedAfter, _) = S.runPureWith defenderOrBlockerAnswer gone (Damage.gatherCombatDamage (const True))
          (assignedBefore, _) = S.runPureWith defenderOrBlockerAnswer attacking (Damage.gatherCombatDamage (const True))
      Spec.assertBool s (Maybe.isJust (Game.lookupObject blocker gone)) "the blocker survived carol's departure, so the board is still in the prompt arm"
      Spec.assertBool s (notElem (Recipient.ToPlayer S.carol) (fmap DamageEvent.target assignedAfter)) "no assignment names the departed defender"
      Spec.assertEqWith s "all three points land on the blocker instead" (fmap DamageEvent.amount (filter (\ev -> DamageEvent.target ev == Recipient.ToCreature blocker) assignedAfter)) [3]
      Spec.assertBool s (Maybe.isJust (List.find (\ev -> DamageEvent.target ev == Recipient.ToPlayer S.carol) assignedBefore)) "with carol still in the game the threshold map DOES offer her -- the guard is what did it"

-- Grant deathtouch to `oid` the way Serpent's Gift does: a stored continuous
-- effect over just that object. Timestamp is arbitrary (no competing layer-6
-- effect in these fixtures).
grantDeathtouch :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
grantDeathtouch oid gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 997,
            ContinuousEffect.timestamp = Timestamp.MkTimestamp 500,
            ContinuousEffect.expiry = Expiry.Type.AtCleanup,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Deathtouch,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

trampleDeathtouchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trampleDeathtouchSpec s registry =
  Spec.describe s "TrampleDeathtouch" $ do
    Spec.it s "CR 702.2c a deathtouch-granted trampler needs only 1 on the blocker, spilling the rest" $ do
      -- War Mammoth (3/3 trample) GRANTED deathtouch into Ogre Sentry (3/3):
      -- lethal collapses to 1, so 1 to the Ogre and 2 tramples to bob; the Ogre
      -- still dies (704.5h, via the deal-time bit). Real cards replace M2c's
      -- synthetic deathtrampler.
      warMammoth <- S.printingOf s registry "War Mammoth"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs0, mammoths, _) = S.combatBoardOf [warMammoth] [ogreSentry]
          mammothId = case mammoths of
            m : _ -> m
            [] -> ObjectId.MkObjectId 999
          gs = grantDeathtouch mammothId gs0
          after = S.settleSba (S.fightWith tramplingAnswer gs)
      Spec.assertEqWith s "bob took the 2 overflow" (S.lifeOf S.bob after) (Just 18)
      Spec.assertEqWith s "the Ogre is dead" (S.creaturesInPlay S.bob after) 0

    Spec.it s "CR 702.19b the control: plain trample into the same 3/3 spills nothing" $ do
      -- War Mammoth (3/3 trample, NO deathtouch) into Ogre Sentry (3/3): lethal
      -- is 3, all 3 go to the Ogre, 0 tramples. Only deathtouch changes the spill.
      warMammoth <- S.printingOf s registry "War Mammoth"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [warMammoth] [ogreSentry]
          after = S.settleSba (S.fightWith tramplingAnswer gs)
      Spec.assertEqWith s "bob untouched without deathtouch" (S.lifeOf S.bob after) (Just 20)

m2cPropertySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
m2cPropertySpec s registry =
  Spec.describe s "M2cProperties" $ do
    Spec.it s "a deathtoucher's victim with toughness > 0 is gone after the SBA" $ do
      -- The property in fixture form (the deck has no deathtoucher, so this is
      -- the M2c coverage; it becomes a random-game property when a deathtoucher
      -- joins a deck -- the castability work, #23). Every toughness we throw at
      -- the 1/1 deathtoucher dies to it.
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      piker <- S.printingOf s registry "Goblin Piker"
      nimbleBirdsticker <- S.printingOf s registry "Nimble Birdsticker"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let victims = [piker, nimbleBirdsticker, ogreSentry]
          killsIt v =
            let (gs, _, _) = S.combatBoardOf [typhoidRats] [v]
                after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
             in S.creaturesInPlay S.bob after == 0
      Spec.assertBool s (all killsIt victims) "deathtouch kills every toughness"

    Spec.it s "the deathtouch and trample reads never name a card" $
      -- A structural reminder, asserted by the interaction falsifier's outcome
      -- (TrampleDeathtouch) depending only on the keyword projection. This case
      -- documents the invariant; the real enforcement is code review of
      -- Damage.blockerThreshold and Sba.woundedByDeathtouch, which case on
      -- Keyword, never on a printing.
      Spec.assertBool s True "see TrampleDeathtouch and Deathtouch groups"

-- CR 120.1a: "Damage can't be dealt to an object that's not a battle, a
-- creature, or a planeswalker." Damage.damageRecipient is where a Recipient that
-- names a permanent GENERICALLY -- Pawl.Engine.Binding.became's entrant, which
-- Pawl.Engine.Event.eventBindings tags Recipient.ToObject because the trigger condition
-- says nothing about the entrant's card types -- gets classified before an
-- effect can build a damage event out of it.
--
-- All three clauses of that rule are live: the creature one has been since M3a, CR
-- 306.8's loyalty removal made the planeswalker one so (#494), and CR 120.3h's
-- defense-counter removal made the battle one so.
--
-- So the function has four answers for a generically named permanent --
-- ToCreature, ToPlaneswalker, ToBattle, or Nothing -- plus the pass-through for a
-- recipient that arrives already classified.
--
-- Two of them happen in real games. Aether Flash reaches both (TriggerSpec's
-- aetherFlashSpec): a creature entrant becomes ToCreature, and an entrant
-- already dead by the time the ability resolves (CR 608.2h) becomes Nothing.
-- Corrosive Gale reaches the first from the other direction (ResolveSpec's
-- CorrosiveGale group), since every member of an ObjectRef.EachMatching sweep
-- arrives here tagged ToObject.
--
-- The PLANESWALKER answer is pinned here and nowhere else, because no card in
-- the pool can produce it. A DealDamage takes its recipient from an AnyTarget,
-- Creatures or Players pool -- none of which tags a candidate ToObject -- or
-- from a bound slot, or from a swept set, and neither of the two that can name
-- a permanent generically admits a planeswalker: Aether Flash's `became` has a
-- trigger condition Filter that admits only creatures, and Corrosive Gale's
-- sweep filter says HasCardType Creature. A planeswalker or a land therefore
-- reaches this function's ToObject arm only from a direct call like these.
damageRecipientSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageRecipientSpec s registry =
  Spec.describe s "CR 120.1a which recipients damage can be dealt to" $ do
    Spec.it s "a generically named creature becomes CR 120.3e's creature recipient" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertEqWith
        s
        "retagged, not rejected"
        (Damage.damageRecipient gs (Recipient.ToObject oid))
        (Just (Recipient.ToCreature oid))

    -- CR 306.8's arm is what makes the wording here matter. "Noncreature
    -- permanent" is NOT the condition for taking nothing -- a planeswalker is
    -- one and takes damage (the case below) -- so what CR 120.1a rejects is a
    -- permanent that is none of its three card types. A land is the case the
    -- pool can build today; the battle case below is the other side of it.
    Spec.it s "a generically named permanent that is neither a creature nor a planeswalker can be dealt no damage" $ do
      plains <- S.printingOf s registry "Plains"
      let (oid, gs) = S.addCreature plains S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertBool s (Set.member oid (GameState.battlefield gs)) "the land is really there"
      Spec.assertEqWith s "and takes nothing" (Damage.damageRecipient gs (Recipient.ToObject oid)) Nothing

    -- CR 120.1a's planeswalker clause, and the retag CR 120.3c needs: the
    -- ToPlaneswalker tag is what tells Damage.applyDamage to remove loyalty
    -- counters instead of marking damage, so classifying a generically named
    -- planeswalker as ToCreature here would silently give it the wrong one of
    -- CR 120.3's results.
    --
    -- No card drives this: see the group's header. It is the same footing the
    -- Plains case above stands on -- a direct call, pinning an answer the pool
    -- cannot yet ask for. PlaneswalkerSpec's Lightning Bolt case is what proves
    -- the loyalty removal itself in a real game, through AnyTarget's own
    -- ToPlaneswalker tag rather than through this arm.
    Spec.it s "a generically named planeswalker becomes CR 120.3c's planeswalker recipient" $ do
      jace <- S.printingOf s registry "Jace Beleren"
      let (oid, gs) = S.addCreature jace S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertBool s (Set.member oid (GameState.battlefield gs)) "the planeswalker is really there"
      Spec.assertEqWith
        s
        "retagged, not rejected"
        (Damage.damageRecipient gs (Recipient.ToObject oid))
        (Just (Recipient.ToPlaneswalker oid))

    -- CR 120.1a's battle clause, and the retag CR 120.3h needs, on exactly the
    -- planeswalker case's footing above: the ToBattle tag is what tells
    -- Damage.applyDamage to remove DEFENSE counters, so classifying a generically
    -- named battle as anything else would give it the wrong one of CR 120.3's
    -- results.
    --
    -- No card drives this either, and for the same reason: nothing in the pool
    -- names a battle generically. Pawl.BattleSpec's Damage group is what proves the
    -- counter removal in a real game, through AnyTarget's own ToBattle tag rather
    -- than through this arm.
    Spec.it s "a generically named battle becomes CR 120.3h's battle recipient" $ do
      invasion <- S.printingOf s registry "Invasion of Dominaria"
      let (oid, gs) = S.addCreature invasion S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertBool s (Set.member oid (GameState.battlefield gs)) "the battle is really there"
      Spec.assertEqWith
        s
        "retagged, not rejected"
        (Damage.damageRecipient gs (Recipient.ToObject oid))
        (Just (Recipient.ToBattle oid))

    Spec.it s "an object that no longer exists takes nothing either (CR 608.2h)" $
      Spec.assertEqWith
        s
        "no recipient"
        (Damage.damageRecipient (Setup.emptyGame S.bothPlayers) (Recipient.ToObject (ObjectId.MkObjectId 99)))
        Nothing

    -- The pass-through half. A combat recipient (CR 510.1b-d) and a chosen
    -- target out of a typed Pool were classified when they were built, so this
    -- function is not a second, later reading of the same question.
    Spec.it s "a creature, planeswalker, battle or player recipient is unchanged" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      jace <- S.printingOf s registry "Jace Beleren"
      invasion <- S.printingOf s registry "Invasion of Dominaria"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (jaceId, gs1) = S.addCreature jace S.alice gs0
          (battleId, gs) = S.addCreature invasion S.alice gs1
      Spec.assertEqWith s "creature" (Damage.damageRecipient gs (Recipient.ToCreature oid)) (Just (Recipient.ToCreature oid))
      Spec.assertEqWith s "planeswalker" (Damage.damageRecipient gs (Recipient.ToPlaneswalker jaceId)) (Just (Recipient.ToPlaneswalker jaceId))
      Spec.assertEqWith s "battle" (Damage.damageRecipient gs (Recipient.ToBattle battleId)) (Just (Recipient.ToBattle battleId))
      Spec.assertEqWith s "player" (Damage.damageRecipient gs (Recipient.ToPlayer S.bob)) (Just (Recipient.ToPlayer S.bob))

-- Two Forests, one creature for alice, one for bob, and Rabid Bite cast from
-- alice's hand at the only legal target in each slot. Returns the dealer, the
-- victim and the board after the sorcery has resolved -- state-based actions
-- NOT yet checked, so a caller that wants CR 704.5h runs S.settleSba itself.
biteBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
biteBoard forest rabidBite dealerPrinting victimPrinting =
  let (dealerId, withDealer) = S.addCreature dealerPrinting S.alice (S.landsInPlay forest 2)
      (victimId, withVictim) = S.addCreature victimPrinting S.bob withDealer
      (gs, spellId) = S.handOne rabidBite withVictim
      after = S.runPure S.identityAnswer gs (S.cast S.alice spellId Monad.>> Stack.resolveTop)
   in (dealerId, victimId, after)

-- CR 120.1's "an object that deals damage is the source of that damage", where
-- the object is NOT the resolving one -- CR 120.2b: "the spell or ability will
-- specify which object deals that damage."
--
-- Rabid Bite {1}{G} Sorcery (data/cards/rabid-bite.json): "Target creature you
-- control deals damage equal to its power to target creature you don't
-- control." The sorcery is the instruction; the targeted creature is the source.
--
-- A source is invisible unless something READS it, so no case here stops at the
-- marked damage -- which would pass whichever object the engine credited. The
-- readers are the CR 608.2i record's own DamageEvent.source, CR 120.3f's
-- lifelink payee and CR 702.2b's deathtouch destruction. Rabid Bite itself has
-- none of those keywords, so every one of them answers only if the creature is
-- the source.
--
-- Two pairs, each differing in exactly one thing. Child of Night and Goblin
-- Piker are both 2/1, so the damage is 2 either way and only lifelink differs;
-- Typhoid Rats and Llanowar Elves are both 1/1, so the damage is 1 either way
-- and only deathtouch differs (the Elves' mana ability is never activated). The
-- two amounts differ from each other, and the victim is a 0/8 Wall of Stone
-- throughout, which survives both -- so its destruction can only be deathtouch.
damageSourceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageSourceSpec s registry = Spec.describe s "DamageSource" $ do
  Spec.it s "CR 120.1/120.2b the event credits the targeted creature, and CR 120.3f pays ITS controller" $ do
    forest <- S.printingOf s registry "Forest"
    rabidBite <- S.printingOf s registry "Rabid Bite"
    childOfNight <- S.printingOf s registry "Child of Night"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (vampire, wall, after) = biteBoard forest rabidBite childOfNight wallOfStone
    -- The OBSERVER first, deliberately: a mutation that credits the sorcery has
    -- to be caught by a rule reading the source, not only by the field itself.
    Spec.assertEqWith s "alice gained two off ITS lifelink" (S.lifeOf S.alice after) (Just 22)
    Spec.assertEqWith s "bob gained nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "two damage marked on the Wall" (S.damageOf wall after) (Just 2)
    Spec.assertBool s (S.onBattlefield wall (S.settleSba after)) "the 0/8 Wall survives two damage"
    Spec.assertEqWith s "and the Vampire dealt it, not the sorcery" (fmap DamageEvent.source (S.damageEventsOf after)) [vampire]
  -- The paired control: same seats, same mana, same 2 power, same recipient. A
  -- reading that gained life for the sorcery's own damage would gain it here too.
  Spec.it s "CR 120.3f a Goblin Piker dealing the same two gains nobody anything" $ do
    forest <- S.printingOf s registry "Forest"
    rabidBite <- S.printingOf s registry "Rabid Bite"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (pikerId, wall, after) = biteBoard forest rabidBite piker wallOfStone
    Spec.assertEqWith s "the Piker dealt it" (fmap DamageEvent.source (S.damageEventsOf after)) [pikerId]
    Spec.assertEqWith s "two damage marked on the Wall" (S.damageOf wall after) (Just 2)
    Spec.assertEqWith s "and no lifelink to pay" (S.lifeOf S.alice after) (Just 20)
    Spec.assertBool s (S.onBattlefield wall (S.settleSba after)) "the Wall survives"
  -- CR 702.2b reads the SOURCE's deathtouch, and one damage kills a 0/8 only if
  -- the source is the Rat. The rider is captured at deal time
  -- (DamageEvent.dealtByDeathtouch), so this also pins that the redirected id is
  -- what damageEvent read the keywords off.
  Spec.it s "CR 702.2b a Typhoid Rats' one damage destroys the 0/8 Wall" $ do
    forest <- S.printingOf s registry "Forest"
    rabidBite <- S.printingOf s registry "Rabid Bite"
    rats <- S.printingOf s registry "Typhoid Rats"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (ratId, wall, after) = biteBoard forest rabidBite rats wallOfStone
    -- The observer first, for the reason the lifelink case above puts it first.
    Spec.assertBool s (not (S.onBattlefield wall (S.settleSba after))) "CR 704.5h destroyed the Wall"
    Spec.assertEqWith s "one damage, not two" (S.damageOf wall after) (Just 1)
    Spec.assertEqWith s "the deal-time rider read the Rat" (fmap DamageEvent.dealtByDeathtouch (S.damageEventsOf after)) [True]
    Spec.assertEqWith s "and the Rat dealt it" (fmap DamageEvent.source (S.damageEventsOf after)) [ratId]
  -- The paired control for that one: another 1/1, so the same one damage, and
  -- the only difference is the keyword.
  Spec.it s "CR 702.2b Llanowar Elves' one damage leaves the Wall standing" $ do
    forest <- S.printingOf s registry "Forest"
    rabidBite <- S.printingOf s registry "Rabid Bite"
    elves <- S.printingOf s registry "Llanowar Elves"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (elfId, wall, after) = biteBoard forest rabidBite elves wallOfStone
    Spec.assertEqWith s "the Elf dealt it" (fmap DamageEvent.source (S.damageEventsOf after)) [elfId]
    Spec.assertEqWith s "one damage" (S.damageOf wall after) (Just 1)
    Spec.assertEqWith s "no deathtouch rider" (fmap DamageEvent.dealtByDeathtouch (S.damageEventsOf after)) [False]
    Spec.assertBool s (S.onBattlefield wall (S.settleSba after)) "the Wall survives"

-- Three Mountains and Flame Spill in alice's hand, one creature for bob, and the
-- instant cast at it -- the only creature on the board, so the target is forced
-- whatever `aimedAt` prefers. `prepare` is the one thing the three cases below
-- differ in, so each pair of boards differs in exactly one thing.
--
-- Returns the victim, the board BEFORE the cast (bob's life is read off it, so
-- the assertions say "three less than it was" rather than a number that also
-- depends on the starting total) and the board after resolution, with
-- state-based actions NOT settled: S.damageOf reads nothing off a creature the
-- CR 704.5g sweep has already destroyed.
spillBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId -> GameState.GameState -> GameState.GameState) ->
  (ObjectId.ObjectId, GameState.GameState, GameState.GameState)
spillBoard mountain flameSpill victimPrinting prepare =
  let (victimId, withVictim) = S.addCreature victimPrinting S.bob (S.landsInPlay mountain 3)
      (gs, spellId) = S.handOne flameSpill (prepare victimId withVictim)
      after = S.runPure (aimedAt victimId) gs (S.cast S.alice spellId Monad.>> Stack.resolveTop)
   in (victimId, gs, after)

-- CR 120.4a: "if an effect that's causing damage to be dealt states that excess
-- damage that would be dealt to a permanent is dealt to another permanent or
-- player instead, the damage event is modified accordingly."
--
-- Flame Spill {2}{R} Instant (data/cards/flame-spill.json): "Flame Spill deals 4
-- damage to target creature. Excess damage is dealt to that creature's
-- controller instead." The pool's producer for the rule, and the whole reason
-- DealDamage carries an excess destination.
--
-- The LIFE TOTAL is the assertion each case leads with. Marked damage alone
-- cannot tell the rewrite from its absence on the first board -- a 2/1 is
-- destroyed by 1 and by 4 alike -- so an assertion that the Piker died, or even
-- that some damage was marked, passes with CR 120.4a unimplemented.
--
-- Three boards, differing in one thing each:
--
--   * a Goblin Piker (2/1): 1 is lethal, so 1 is marked and 3 goes to bob.
--   * a Wall of Stone (0/8): nothing is excess, so all 4 stay on the Wall and
--     bob loses nothing. The negative, on the same board as the third case.
--   * that same Wall with 5 damage already marked: CR 120.6's bar is 3 rather
--     than 8, so 3 are marked and 1 goes to bob. This is the case that reads the
--     lethal-damage definition rather than the toughness -- on an undamaged
--     creature the two agree, and only "damage already marked on the creature"
--     separates them.
excessDamageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
excessDamageSpec s registry = Spec.describe s "ExcessDamage" $ do
  Spec.it s "CR 120.4a Flame Spill's excess goes to the creature's controller" $ do
    mountain <- S.printingOf s registry "Mountain"
    flameSpill <- S.printingOf s registry "Flame Spill"
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, before, after) = spillBoard mountain flameSpill piker (\_ gs -> gs)
    Spec.assertEqWith s "CR 120.4a: 3 of the 4 went to the Piker's controller" (S.lifeOf S.bob after) (fmap (subtract 3) (S.lifeOf S.bob before))
    Spec.assertEqWith s "CR 120.6: 1 was lethal, so 1 is what is marked" (S.damageOf pikerId after) (Just 1)
    Spec.assertEqWith s "and the spell's controller took none of it" (S.lifeOf S.alice after) (S.lifeOf S.alice before)
    Spec.assertBool s (not (S.onBattlefield pikerId (S.settleSba after))) "CR 704.5g: 1 is still lethal to a 2/1"
  Spec.it s "CR 120.4a nothing is excess on an undamaged Wall of Stone, so nothing is redirected" $ do
    mountain <- S.printingOf s registry "Mountain"
    flameSpill <- S.printingOf s registry "Flame Spill"
    wall <- S.printingOf s registry "Wall of Stone"
    let (wallId, before, after) = spillBoard mountain flameSpill wall (\_ gs -> gs)
    Spec.assertEqWith s "CR 120.4a: 4 is under the 0/8's bar, so bob loses nothing" (S.lifeOf S.bob after) (S.lifeOf S.bob before)
    Spec.assertEqWith s "and all 4 are marked on the Wall" (S.damageOf wallId after) (Just 4)
    Spec.assertBool s (S.onBattlefield wallId (S.settleSba after)) "CR 704.5g: 4 is not lethal to a 0/8"
  -- CR 120.4a's last clause: "if the first permanent has multiple card types
  -- from among the list of creature, planeswalker, and battle, the excess damage
  -- is the greatest of the calculated amounts for each of the card types it
  -- has." The animated Jace Beleren of the CreatureAndPlaneswalker group above
  -- is the board that HAS two of them, with one damage marked on him so that the
  -- two bars differ: CR 120.6's lethal damage is 3 - 1 = 2, and CR 306.5c's
  -- loyalty is 3. The greatest excess is therefore 2, off the LOWER bar -- and 1
  -- off the higher one, which is what reading the rule the other way round
  -- gives.
  Spec.it s "CR 120.4a the excess is the greatest across the card types the permanent has" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    jace <- S.printingOf s registry "Jace Beleren"
    march <- S.printingOf s registry "March of the Machines"
    coating <- S.printingOf s registry "Liquimetal Coating"
    flameSpill <- S.printingOf s registry "Flame Spill"
    let (handGs, jaceInHand) = S.handOne jace (S.landsInPlay island 3)
        board = S.runPure S.identityAnswer handGs (do S.cast S.alice jaceInHand; Stack.resolveTop)
        jaceId = permanentNamed "Jace Beleren" board
        (_, withMarch) = S.addCreature march S.alice board
        (coatingId, withCoating) = S.addCreature coating S.alice withMarch
        ready = (S.landsFor mountain S.alice 3 withCoating) {GameState.priority = Just S.alice}
    case Face.activatedAbilities (S.combinedFace coating) of
      coat : _ -> do
        let coated = S.runPure (aimedAt jaceId) ready (do Activate.activateAbility S.alice coatingId coat; Stack.resolveTop)
            (before, spellId) = S.handOne flameSpill (S.markDamage jaceId 1 coated)
            after = S.runPure (aimedAt jaceId) before (do S.cast S.alice spellId; Stack.resolveTop)
        Spec.assertEqWith s "CR 120.4a: 2 is excess, the greatest of the two card types' amounts" (S.lifeOf S.alice after) (fmap (subtract 2) (S.lifeOf S.alice before))
        Spec.assertEqWith s "so 2 of the 4 stayed, on top of the 1 already marked" (S.damageOf jaceId after) (Just 3)
        Spec.assertEqWith s "CR 120.3c: and those 2 took two loyalty counters off" (S.counterOf CounterKind.Loyalty jaceId after) 1
        -- The preconditions the arithmetic above rests on, asserted after it so
        -- that no mutation of the rewrite is absorbed here first.
        Spec.assertBool s (Projection.isCreatureOf jaceId before) "CR 205.1b: the Coating and March made him a creature"
        Spec.assertBool s (Set.member CardType.Planeswalker (Projection.cardTypesOf jaceId before)) "and he is still a planeswalker"
        Spec.assertEqWith s "a 3/3 with 1 marked, so CR 120.6's bar is 2" (S.powerToughnessOf jaceId before) (Just (3, 3))
        Spec.assertEqWith s "CR 306.5c: while his loyalty is 3" (S.counterOf CounterKind.Loyalty jaceId before) 3
      _ -> Spec.assertFailure s "Liquimetal Coating should print an activated ability"
  Spec.it s "CR 120.4a/120.6 the bar is lethal damage, not toughness" $ do
    mountain <- S.printingOf s registry "Mountain"
    flameSpill <- S.printingOf s registry "Flame Spill"
    wall <- S.printingOf s registry "Wall of Stone"
    -- The same 0/8, with 5 marked: the ONE difference from the case above.
    let (wallId, before, after) = spillBoard mountain flameSpill wall (\oid gs -> S.markDamage oid 5 gs)
    Spec.assertEqWith s "CR 120.4a/120.6: 3 more is lethal, so 1 goes to bob" (S.lifeOf S.bob after) (fmap (subtract 1) (S.lifeOf S.bob before))
    Spec.assertEqWith s "and the Wall is marked to exactly its toughness" (S.damageOf wallId after) (Just 8)
    Spec.assertBool s (not (S.onBattlefield wallId (S.settleSba after))) "CR 704.5g: 8 marked on a 0/8 is lethal"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Damage" $ do
  damageSpec s registry
  damageSourceSpec s registry
  damageRecipientSpec s registry
  legendRuleSpec s registry
  worldRuleSpec s registry
  damageEventSpec s registry
  deathtouchSpec s registry
  assignmentLegalitySpec s
  trampleSpec s registry
  departedBlockerSpec s registry
  blockedStaysBlockedSpec s registry
  departedAttackerSpec s registry
  departedDefenderSpec s registry
  trampleDeathtouchSpec s registry
  sbaSpec s
  creatureSbaSpec s registry
  infectSpec s registry
  witherSpec s registry
  toxicSpec s registry
  lifelinkSpec s registry
  lastKnownRiderSpec s registry
  creaturePlaneswalkerSpec s registry
  excessDamageSpec s registry
  fightSpec s registry
  m2cPropertySpec s registry
  dealtDamageThisTurnSpec s registry

-- Fill every target slot with whichever of the two named permanents that slot's
-- own filter admits. Prey Upon's slots are disjointly filtered by controller, so
-- one predicate over both ids aims each slot at exactly one candidate.
aimedAtEither :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAtEither a b p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    S.preferring (\r -> Recipient.objectOf r == Just a || Recipient.objectOf r == Just b) sets
  _ -> S.identityAnswer p

-- alice holds Prey Upon over one Forest, controls a Goblin Piker and faces bob's
-- Hill Giant, returned as (the board, the spell in hand, alice's fighter, bob's).
--
-- The two bodies are chosen so every reading of CR 701.14a produces a different
-- board. The powers differ (2 against 3) so a fight that ran one blow twice, or
-- swapped the dealers, reads wrong; the toughnesses differ (1 against 3) so
-- exactly one creature dies, and WHICH one is the rule's answer rather than a
-- coincidence. Two 2/2s could not tell any of that apart.
preyBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
preyBoard s registry = do
  prey <- S.printingOf s registry "Prey Upon"
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  let (g0, spell) = S.handOne prey (S.landsInPlay forest 1)
      (mine, g1) = S.addCreature piker S.alice g0
      (theirs, g2) = S.addCreature giant S.bob g1
  pure (g2, spell, mine, theirs)

-- CR 701.14, through Prey Upon: "target creature you control fights target
-- creature you don't control". The whole card is the keyword action, which is
-- why it is the producer -- Wolverine, Fierce Fighter drags CR 701.69's heal in
-- beside it.
--
-- Here rather than in Pawl.CombatSpec because CR 701.14d says the damage is not
-- combat damage, and this module owns the damage funnel.
fightSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fightSpec s registry = Spec.describe s "Fight (CR 701.14)" $ do
  Spec.it s "CR 701.14a each fighter deals damage equal to its power to the other" $ do
    (before, spell, mine, theirs) <- preyBoard s registry
    let after = S.settleSba (S.runPure (aimedAtEither mine theirs) before (S.cast S.alice spell >> Stack.resolveTop))
    -- The fixture's own numbers first, so neither assertion below can pass on a
    -- board whose bodies were not what this case claims.
    Spec.assertEqWith s "alice's 2/1 and bob's 3/3" (S.powerToughnessOf mine before, S.powerToughnessOf theirs before) (Just (2, 1), Just (3, 3))
    -- CR 701.14a's first blow. 2 and not 3: the Piker deals ITS power, not the
    -- Giant's.
    Spec.assertEqWith s "CR 701.14a the Piker dealt 2 to the Giant" (S.damageOf theirs after) (Just 2)
    -- CR 701.14a's second blow, and the assertion that only it can redden: the
    -- Giant's 3 is lethal to a 2/1 (CR 704.5g).
    Spec.assertBool s (not (S.onBattlefield mine after)) "CR 701.14a/704.5g the Giant's 3 back killed the Piker"
    -- The control: 2 is not lethal to a 3/3, so the Giant is still there and the
    -- death above is the rule's arithmetic rather than a board-wide wipe.
    Spec.assertBool s (S.onBattlefield theirs after) "CR 704.5g and 2 is not lethal to the Giant"

  -- CR 701.14b: "if one or both creatures instructed to fight are no longer on
  -- the battlefield or are no longer creatures, NEITHER of them fights or deals
  -- damage. If one or both creatures are illegal targets ... neither of them
  -- fights or deals damage."
  --
  -- THE PAIR with the case above, differing in one thing: bob takes control of
  -- the Piker (CR 108.4) after Prey Upon is on the stack, so the "mine" slot's
  -- target is illegal -- no longer a creature alice controls -- while the Piker
  -- itself is still on the battlefield and still a creature.
  --
  -- What this leg can and cannot separate. Prey Upon TARGETS both fighters, so CR
  -- 608.2b removes the Piker from the slot map in BOTH roles, and 701.14b's first
  -- sentence (gone from the battlefield, no longer a creature) is not
  -- independently observable off this card -- an implementation that dropped
  -- those two conjuncts would answer the same here. What it does separate is the
  -- PAIR shape: an implementation that fell back to the remaining fighter leaves
  -- the Giant fighting itself and marks 3 on it.
  --
  -- Prey Upon still has its other target, so CR 608.2b lets it resolve rather
  -- than countering it -- the graveyard assertion is what pins that, and without
  -- it this case would pass for a spell that never resolved at all.
  Spec.it s "CR 701.14b one illegal target and NEITHER creature deals damage" $ do
    (before, spell, mine, theirs) <- preyBoard s registry
    let cast = S.runPure (aimedAtEither mine theirs) before (S.cast S.alice spell)
        stolen = S.giveControl mine S.bob cast
        after = S.settleSba (S.runPure S.identityAnswer stolen Stack.resolveTop)
    Spec.assertEqWith s "CR 701.14b the Giant dealt nothing to the Piker" (S.damageOf mine after) (Just 0)
    Spec.assertBool s (S.onBattlefield mine after) "CR 701.14b so the Piker lived"
    -- The discriminator: an implementation that let the remaining fighter swing
    -- anyway marks the Giant's own 3 on the Giant.
    Spec.assertEqWith s "CR 701.14b and the Piker dealt nothing to the Giant" (S.damageOf theirs after) (Just 0)
    -- CR 608.2b: it RESOLVED, and was not countered for want of targets.
    Spec.assertEqWith s "the spell resolved into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and left the stack" (length (GameState.stack after)) 0

  -- CR 701.14d: "the damage dealt when a creature fights ISN'T COMBAT DAMAGE."
  --
  -- Read through a Fog-shaped shield -- CR 615.1's prevention, scoped to
  -- DamageKind.Combat and to no recipient in particular -- so the rule is a
  -- board on which combat damage cannot land and the fight's damage does anyway.
  -- THE PAIR with the first case, differing only in that shield: every number
  -- below is the same one that case asserts.
  Spec.it s "CR 701.14d fight damage is not combat damage, so a combat-only prevention misses it" $ do
    (base, spell, mine, theirs) <- preyBoard s registry
    let shield =
          ActiveReplacement.MkActiveReplacement
            { ActiveReplacement.effect = ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat) (Filter.Type.And []) Nothing Nothing Nothing) DamageRewrite.PreventAll Seq.empty),
              ActiveReplacement.source = theirs,
              ActiveReplacement.controller = S.alice,
              ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
              ActiveReplacement.expiry = Expiry.Type.AtCleanup,
              ActiveReplacement.uses = Uses.Unlimited,
              ActiveReplacement.origin = ReplacementOrigin.Other,
              ActiveReplacement.condition = Nothing,
              ActiveReplacement.rider = Nothing,
              ActiveReplacement.slots = Map.empty
            }
        before = S.addReplacement shield base
        after = S.settleSba (S.runPure (aimedAtEither mine theirs) before (S.cast S.alice spell >> Stack.resolveTop))
    Spec.assertEqWith s "CR 701.14d the Piker's 2 still landed on the Giant" (S.damageOf theirs after) (Just 2)
    Spec.assertBool s (not (S.onBattlefield mine after)) "CR 701.14d and the Giant's 3 still killed the Piker"

  -- CR 701.14c: "if a creature fights itself, it deals damage to itself equal to
  -- twice its power". ONE blow of 2P, not two of P.
  --
  -- The producer is Nightfall Predator, the back face of Daybreak Ranger: "{R},
  -- {T}: This creature fights target creature", whose second participant is a
  -- target with no control clause and no "another", so CR 115.3 lets the player
  -- name the Predator itself. Prey Upon cannot reach the rule -- its two slots are
  -- filtered by controller, disjointly.
  --
  -- Every observer that SUMS reads the same under both implementations, because
  -- 4 + 4 and 8 agree: marked damage, excess (CR 120.4a), lifelink, and an
  -- amount-based shield, whose rule says so itself ("such effects count only the
  -- amount of damage; the number of events or sources dealing it doesn't matter",
  -- CR 615.7). Only an observer that counts EVENTS separates them, so this case is
  -- written on CR 122.1c's shield counters: one counter is spent per prevented
  -- event, whatever the amount.
  Spec.it s "CR 701.14c a self-fight is ONE damage event, so one shield counter answers it" $ do
    (board, predator) <- predatorBoard s registry
    Spec.assertEqWith s "Moonmist flipped the Ranger to a 4/4 Nightfall Predator" (fmap Face.name (Game.faceOf predator board), S.powerToughnessOf predator board) (Just (CardName.MkCardName (Text.pack "Nightfall Predator")), Just (4, 4))
    let shielded = S.addCounter CounterKind.Shield 2 predator board
        after = selfFight predator shielded
    -- The gameplay-level assertion, and the only one the two readings disagree
    -- on: two blows of 4 spend both counters, one blow of 8 spends one.
    Spec.assertEqWith s "CR 701.14c one event, so one of the two counters is left" (S.counterOf CounterKind.Shield predator after) 1
    -- Supporting checks: both readings agree here, which is why they come after.
    Spec.assertEqWith s "CR 122.1c the blow was prevented whole, so nothing is marked" (S.damageOf predator after) (Just 0)
    Spec.assertBool s (S.onBattlefield predator after) "and the Predator survives"

  -- The AMOUNT, on the same board minus the shield counters: CR 701.14c's "twice
  -- its power". +0/+5 makes the Predator a 4/9, so 8 is marked rather than
  -- lethal and the number is readable.
  --
  -- The two-blow implementation this replaced marked 8 here too, so this case
  -- does not separate that bug; it separates a fix that collapses the two blows
  -- into one blow of P (which marks 4) from one that doubles.
  Spec.it s "CR 701.14c the one blow is TWICE its power" $ do
    (base, predator) <- predatorBoard s registry
    let board = S.withEffect predator (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 0) (Quantity.Literal 5))) base
        after = selfFight predator board
    Spec.assertEqWith s "the Predator is a 4/9 before it fights" (S.powerToughnessOf predator board) (Just (4, 9))
    Spec.assertEqWith s "CR 701.14c twice its power is marked on it" (S.damageOf predator after) (Just 8)
    Spec.assertBool s (S.onBattlefield predator after) "CR 704.5g 8 is not lethal to a 4/9"

  -- The same rule read as a DEATH rather than as a counter, on one shield counter
  -- instead of two: the single blow of 8 is prevented whole and the Predator
  -- lives, where two blows of 4 spend the counter on the first and let the second
  -- kill a 4/4. THE PAIR with the case above, differing only in the shield.
  Spec.it s "CR 701.14c one shield counter answers the whole self-fight" $ do
    (board, predator) <- predatorBoard s registry
    let shielded = S.addCounter CounterKind.Shield 1 predator board
        after = selfFight predator shielded
    Spec.assertBool s (S.onBattlefield predator after) "CR 701.14c the one event was prevented, so the Predator lives"
    Spec.assertEqWith s "nothing was marked on it" (S.damageOf predator after) (Just 0)
    Spec.assertEqWith s "and the counter paid for it" (S.counterOf CounterKind.Shield predator after) 0

-- alice controls a Daybreak Ranger that Moonmist has flipped to its back face,
-- plus an untapped Mountain for the back face's {R}. The Ranger is the only Human
-- on the board, so Moonmist's board-wide "transform all Humans" reaches it and
-- nothing else.
--
-- The Mountain arrives AFTER Moonmist resolves: paying {1}{G} off a board holding
-- one may tap it for the generic, and then the ability has no red mana to pay
-- with.
--
-- Moonmist rather than the card's own upkeep trigger: neither face's "if no
-- spells were cast last turn, transform this creature" is transcribed, because no
-- card can read that count (gap #1883).
predatorBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
predatorBoard s registry = do
  ranger <- S.printingOf s registry "Daybreak Ranger"
  moonmist <- S.printingOf s registry "Moonmist"
  forest <- S.printingOf s registry "Forest"
  mountain <- S.printingOf s registry "Mountain"
  let (rangerId, g1) = S.addCreature ranger S.alice (S.landsInPlay forest 2)
      (g2, spell) = S.handOne moonmist g1
      cast = S.runPure S.castAnswer g2 (S.cast S.alice spell)
      flipped = S.runPure S.castAnswer cast Stack.resolveTop
  pure (S.landsFor mountain S.alice 1 flipped, rangerId)

-- alice activates the back face's fight ability and answers its one target slot
-- with the Predator itself (CR 115.3: the same object may be chosen for each
-- instance of the word "target"), then the ability resolves and SBAs settle.
--
-- The ability is read off the object's CURRENT face (CR 712.8: each face has its
-- own characteristics), so a board whose Ranger never transformed would activate
-- the front face's ping instead and mark no damage on the Ranger at all. The id
-- survives the flip because CR 712.18 says a transformed permanent is not a new
-- object.
selfFight :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
selfFight oid gs = case Game.faceOf oid gs of
  Just face
    | ability : _ <- Face.activatedAbilities face ->
        let activated = S.runPure (aimedAt oid) gs (Activate.activateAbility S.alice oid ability)
         in S.settleSba (S.runPure (aimedAt oid) activated Stack.resolveTop)
  _ -> gs

-- CR 120.1 / 608.2i: Filter.DealtDamageThisTurn, the look-back read Fatal Blow's
-- "destroy target creature that was dealt damage this turn" asks for. Here
-- rather than in Pawl.FilterSpec, whose spec takes no registry and so holds
-- View-record units only, and rather than in a subsystem spec, Fatal Blow
-- belonging to none.
--
-- The CR defines no term for the phrase -- rule 702.54a is the one place it is
-- printed in the rules, as ordinary English -- so the citations here are rule
-- 120.1 for what damage is dealt to and rule 608.2i for reading it back.
dealtDamageThisTurnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dealtDamageThisTurnSpec s registry =
  Spec.describe s "DealtDamageThisTurn" $ do
    -- Two IDENTICAL Hill Giants, so no characteristic separates them and only
    -- the damage can decide the offer -- and 3/3s rather than the Goblin Piker a
    -- board like this usually uses, because a Piker is 2/1 and CR 704.5g would
    -- destroy the damaged one before the spell ever looked for a target, leaving
    -- an empty legal set under every implementation.
    Spec.it s "CR 120.1 Fatal Blow can be aimed at the pinged creature and not at its twin" $ do
      island <- S.printingOf s registry "Island"
      sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      hillGiant <- S.printingOf s registry "Hill Giant"
      fatalBlow <- S.printingOf s registry "Fatal Blow"
      case (Face.activatedAbilities (S.combinedFace sorcerer), S.spellTargetSlot fatalBlow) of
        (ping : _, Just theSlot) -> do
          let (sorcererId, gs1) = S.addCreature sorcerer S.alice (S.landsInPlay island 1)
              (hurtId, gs2) = S.addCreature hillGiant S.bob gs1
              (wholeId, gs3) = S.addCreature hillGiant S.bob gs2
              ready = gs3 {GameState.priority = Just S.alice}
              pinged = S.runPure (aimedAt hurtId) ready (do Activate.activateAbility S.alice sorcererId ping; Stack.resolveTop)
              board = S.settleSba pinged
          Spec.assertEqWith
            s
            "CR 120.1 / 608.2i: the pinged Giant is the only legal target"
            (Set.toList (Target.legalRecipients Nothing S.noSource theSlot board))
            [Recipient.ToCreature hurtId]
          -- The preconditions the offer rests on, asserted AFTER it so neither
          -- can absorb a mutation of the atom.
          Spec.assertEqWith s "CR 120.3e: one damage was marked on it" (S.damageOf hurtId board) (Just 1)
          Spec.assertBool s (Set.member hurtId (GameState.battlefield board)) "CR 704.5g: 1 is not lethal to a 3/3, so it is still there to target"
          Spec.assertBool s (Set.member wholeId (GameState.battlefield board)) "and its twin is on the battlefield too, undamaged"
        _ -> Spec.assertFailure s "Prodigal Sorcerer should print an activated ability, and Fatal Blow a 'target' slot"

    -- The board Object.damage cannot answer, and the rules question the issue
    -- names. CR 120.6: "All damage marked on a permanent is removed when it
    -- regenerates" -- so a regenerated Uthden Troll carries no marks and was
    -- still dealt damage this turn. A marks-reading implementation offers
    -- nothing here; the log reading offers the Troll.
    Spec.it s "CR 120.6 a regenerated creature carries no marked damage and is still a legal target" $ do
      mountain <- S.printingOf s registry "Mountain"
      uthdenTroll <- S.printingOf s registry "Uthden Troll"
      bolt <- S.printingOf s registry "Lightning Bolt"
      fatalBlow <- S.printingOf s registry "Fatal Blow"
      case (Face.activatedAbilities (S.combinedFace uthdenTroll), S.spellTargetSlot fatalBlow) of
        (regenerate : _, Just theSlot) -> do
          let (hurtId, gs1) = S.addCreature uthdenTroll S.alice (S.landsInPlay mountain 2)
              (wholeId, gs2) = S.addCreature uthdenTroll S.alice gs1
              -- {R}: Regenerate this creature -- really activated, so the shield
              -- comes from the card rather than from a fixture.
              armed = S.runPure S.identityAnswer gs2 (do Activate.activateAbility S.alice hurtId regenerate; Stack.resolveTop)
              (withBolt, boltId) = S.handOne bolt armed
              cast = S.runPure (aimedAt hurtId) withBolt (S.cast S.alice boltId)
              -- 3 damage to a 2/2 is lethal (CR 704.5g); CR 701.19a's shield
              -- replaces the destruction and removes the marks.
              board = S.settleSba (S.runPure (aimedAt hurtId) cast Stack.resolveTop)
          Spec.assertEqWith
            s
            "CR 120.1 / 120.6: the regenerated Troll is the only legal target"
            (Set.toList (Target.legalRecipients Nothing S.noSource theSlot board))
            [Recipient.ToCreature hurtId]
          Spec.assertEqWith s "CR 120.6: and it carries NO marked damage, so Object.damage cannot be what answered" (S.damageOf hurtId board) (Just 0)
          Spec.assertBool s (Set.member hurtId (GameState.battlefield board)) "CR 701.19a: the shield saved it"
          Spec.assertBool s (Set.member wholeId (GameState.battlefield board)) "and its twin, which nothing damaged, is standing too"
        _ -> Spec.assertFailure s "Uthden Troll should print an activated ability, and Fatal Blow a 'target' slot"

    -- A FENCE rather than a proof of the log reading: what it holds is that the
    -- atom is turn-scoped at all, Engine.beginTurnOf clearing the event log at
    -- the handoff. It does not discriminate the readings in the way the case
    -- above does -- CR 514.2 removes marked damage at cleanup too, so a real turn
    -- would leave both readings answering False, and this board reaches the
    -- handoff without passing through a cleanup step.
    Spec.it s "CR 514.2 the offer does not survive into the next turn" $ do
      island <- S.printingOf s registry "Island"
      sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      hillGiant <- S.printingOf s registry "Hill Giant"
      fatalBlow <- S.printingOf s registry "Fatal Blow"
      case (Face.activatedAbilities (S.combinedFace sorcerer), S.spellTargetSlot fatalBlow) of
        (ping : _, Just theSlot) -> do
          let (sorcererId, gs1) = S.addCreature sorcerer S.alice (S.landsInPlay island 1)
              (hurtId, gs2) = S.addCreature hillGiant S.bob gs1
              ready = gs2 {GameState.priority = Just S.alice}
              board = S.settleSba (S.runPure (aimedAt hurtId) ready (do Activate.activateAbility S.alice sorcererId ping; Stack.resolveTop))
              nextTurn = Engine.beginTurnOf S.bob board
          Spec.assertEqWith s "legal while the damage is this turn's" (Set.toList (Target.legalRecipients Nothing S.noSource theSlot board)) [Recipient.ToCreature hurtId]
          Spec.assertEqWith s "and no candidate at all once the turn has handed over" (Set.toList (Target.legalRecipients Nothing S.noSource theSlot nextTurn)) []
        _ -> Spec.assertFailure s "Prodigal Sorcerer should print an activated ability, and Fatal Blow a 'target' slot"

    -- The PLAYER half of the same atom: CR 120.1 has damage dealt to players as
    -- well as to the three permanent kinds, so "any target that was dealt damage
    -- this turn" (Needle Drop) has to answer for a seat and not only for a
    -- permanent (#2157).
    --
    -- THREE seats, because two would collapse the negative control onto the
    -- caster: alice controls the spell and carol controls nothing, so an
    -- implementation answering the atom True for every player, or for every
    -- player but "you", is caught by the offer below rather than passing on the
    -- one seat it happened to get right. The Prodigal Sorcerer that dealt the
    -- damage is a fourth undamaged candidate of the OTHER shape, which is what
    -- keeps the object half honest at the same time.
    Spec.it s "CR 120.1 Needle Drop can be aimed at the damaged player and not at the untouched seats" $ do
      mountain <- S.printingOf s registry "Mountain"
      island <- S.printingOf s registry "Island"
      sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      needleDrop <- S.printingOf s registry "Needle Drop"
      case (Face.activatedAbilities (S.combinedFace sorcerer), S.spellTargetSlot needleDrop) of
        (ping : _, Just theSlot) -> do
          let seats = S.landsFor mountain S.alice 1 (Setup.emptyGame S.threePlayers)
              (sorcererId, gs1) = S.addCreature sorcerer S.alice seats
              (withDrop, dropId) = S.handOne needleDrop gs1
              -- CR 121.3: the draw clause needs a card to take, and an empty
              -- library would lose alice the game to CR 104.3c instead.
              (_, stocked) = S.addLibraryCard island S.alice withDrop
              ready = stocked {GameState.priority = Just S.alice}
              board = S.settleSba (S.runPure (aimedAtPlayer S.bob) ready (do Activate.activateAbility S.alice sorcererId ping; Stack.resolveTop))
              resolved = S.settleSba (S.runPure (aimedAtPlayer S.bob) board (do S.cast S.alice dropId; Stack.resolveTop))
          Spec.assertEqWith
            s
            "CR 120.1 / 120.3a: bob took the ping and then Needle Drop's own damage"
            (S.lifeOf S.bob resolved)
            (Just 18)
          Spec.assertEqWith
            s
            "CR 120.1 / 608.2i: the damaged seat is the only legal target"
            (Set.toList (Target.legalRecipients Nothing S.noSource theSlot board))
            [Recipient.ToPlayer S.bob]
          -- The preconditions the two assertions above rest on, after them so
          -- neither can absorb a mutation of the atom.
          Spec.assertEqWith s "the ping alone had put bob at 19" (S.lifeOf S.bob board) (Just 19)
          Spec.assertEqWith s "and neither untouched seat lost any life" (fmap (`S.lifeOf` resolved) [S.alice, S.carol]) [Just 20, Just 20]
          Spec.assertEqWith s "CR 121.1: the second clause drew alice a card" (S.handSize S.alice resolved) 1
          Spec.assertBool s (Set.member sorcererId (GameState.battlefield board)) "and the undamaged Sorcerer is standing to be offered"
        _ -> Spec.assertFailure s "Prodigal Sorcerer should print an activated ability, and Needle Drop a 'target' slot"
