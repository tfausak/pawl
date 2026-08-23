{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Resolve and Pawl.Engine.Target: targeting legality and the
-- core of spell resolution. The rest of Pawl.Engine.Resolve is covered by the
-- sibling modules named in Main.hs, which all describe under the same name.
module Pawl.ResolveSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetCount as TargetCount
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.Zone as Zone

targetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
targetSpec s registry = Spec.describe s "Target" $ do
  Spec.it s "CR 115.4 AnyTarget offers every creature and every playing player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "creature and both players"
      (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.AnyTarget Nothing) gs)
      (Set.fromList [Recipient.ToCreature oid, Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob])
  Spec.it s "a departed player is not a legal target" $ do
    let gs = S.departs Departure.Type.Lost S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertBool
      s
      (not (Set.member (Recipient.ToPlayer S.bob) (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.AnyTarget Nothing) gs)))
      "bob gone"
  Spec.it s "CR 800.4b an object does not change to the control of a player who has left the game" $ do
    -- CR 800.4b: "If an object would change to the control of a player who has
    -- left the game, it doesn't." Resolve.applyEffect takes the controller
    -- explicitly, which is what makes this testable: the effect is asked to
    -- resolve on behalf of a player who is no longer in the game.
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (myr, board) = S.addCreature darksteelMyr S.carol S.threePlayerGame
        gone = S.departs Departure.Type.Conceded S.bob board
        slot = SlotName.MkSlotName (Text.pack "target")
        after =
          S.runPure S.identityAnswer gone $
            Resolve.applyEffect
              S.noSource
              S.noSource
              S.bob
              (Map.singleton slot (Set.singleton (Recipient.ToObject myr)))
              (Map.singleton slot (Set.singleton (Recipient.ToObject myr)))
              (Effect.GainControl (DurationRef.MkDurationRef Duration.Indefinite (ObjectRef.InSlot slot)))
        control =
          S.runPure S.identityAnswer board $
            Resolve.applyEffect
              S.noSource
              S.noSource
              S.bob
              (Map.singleton slot (Set.singleton (Recipient.ToObject myr)))
              (Map.singleton slot (Set.singleton (Recipient.ToObject myr)))
              (Effect.GainControl (DurationRef.MkDurationRef Duration.Indefinite (ObjectRef.InSlot slot)))
    Spec.assertEqWith s "no control effect is stored for a departed controller" (GameState.continuousEffects after) []
    Spec.assertEqWith s "and the Myr's controller is unchanged" (Projection.controllerOf myr after) (Just S.carol)
    Spec.assertEqWith s "the same call for a player still in the game DOES store one -- the guard is what did it" (length (GameState.continuousEffects control)) 1
    Spec.assertEqWith s "and takes control" (Projection.controllerOf myr control) (Just S.bob)
  Spec.it s "CR 608.2b a creature that left its zone is no longer legal" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
    Spec.assertBool s (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.AnyTarget Nothing) gs) "legal while fielded"
    Spec.assertBool s (not (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.AnyTarget Nothing) gone)) "illegal once moved"
  Spec.it s "legalSets maps each slot to its legal recipients" $ do
    let slots = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.AnyTarget Nothing)
        gs = Setup.emptyGame S.bothPlayers
    Spec.assertEqWith
      s
      "one slot, two players"
      (Target.legalSets Nothing Map.empty S.noSource slots gs)
      (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]))
  Spec.it s "CR 115.4 CreatureTarget offers creatures but no players" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "just the creature"
      (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Creatures Nothing) gs)
      (Set.singleton (Recipient.ToCreature oid))
  Spec.it s "CR 601.2c CreatureTarget has an empty legal set with no creatures" $ do
    Spec.assertBool
      s
      (Set.null (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Creatures Nothing) (Setup.emptyGame S.bothPlayers)))
      "nothing to target"
  Spec.it s "CR 608.2b a creature that left is no longer a legal CreatureTarget" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
    Spec.assertBool s (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.Creatures Nothing) gs) "legal while fielded"
    Spec.assertBool s (not (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.Creatures Nothing) gone)) "illegal once moved"
  Spec.it s "CR 115 SpellOrPermanentTarget offers battlefield permanents and stack spells" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (permId, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertBool
      s
      (Set.member (Recipient.ToObject permId) (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.SpellsAndPermanents Nothing) gs))
      "the permanent is a legal object target"
  Spec.it s "CR 115 SpellTarget offers a stack spell but not a battlefield permanent" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (permId, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (spellId, gs) = S.spellOnStack lightningBolt S.alice base
        legal = Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Spells Nothing) gs
    Spec.assertBool s (Set.member (Recipient.ToObject spellId) legal) "the stack spell is a legal target"
    Spec.assertBool s (not (Set.member (Recipient.ToObject permId) legal)) "the battlefield permanent is not a legal target"
  Spec.it s "LandTarget offers a land as an object target, not a creature or player" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
    Spec.assertBool s (Set.member (Recipient.ToObject landId) (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs)) "the land is legal"
    Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs))) "no players"
  Spec.it s "CR 115: PlayerTarget is exactly the players still in the game" $ do
    let gs = Setup.emptyGame S.bothPlayers
        expected = Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]
    Spec.assertEqWith s "both players, no creatures" (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Players Nothing) gs) expected
  -- CR 115.1a / 700.2c: "target Wall" (Chaos Charm) restricts CreatureTarget to
  -- creatures whose PROJECTED subtypes include Wall. Wall of Stone (a real 0/8
  -- Creature - Wall, M4g) is the Wall; a Piker is the non-Wall control.
  Spec.it s "CR 115.1a / 700.2c \"target Wall\" offers a Wall creature but not a non-Wall creature" $ do
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    piker <- S.printingOf s registry "Goblin Piker"
    let (wallId, base) = S.addCreature wallOfStone S.bob (Setup.emptyGame S.bothPlayers)
        (pikerId, gs) = S.addCreature piker S.alice base
        slot = SlotName.MkSlotName (Text.pack "target")
        legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing Map.empty S.noSource (Map.singleton slot (TargetSlot.required Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
    Spec.assertBool s (Set.member (Recipient.ToCreature wallId) legal) "the Wall is legal"
    Spec.assertBool s (not (Set.member (Recipient.ToCreature pikerId) legal)) "the non-Wall creature is not legal"
  -- The same "target Wall", against a Wall that Ashaya animated into a land
  -- and Blood Moon then set to Mountain. CR 305.7 retires the land's OLD LAND
  -- TYPES and nothing else on the subtype axis, and its fourth sentence keeps
  -- the card types -- so the Wall is still a creature, still a Wall, and still
  -- a legal target. This is the gameplay-level half of Pawl.ProjectionSpec's
  -- "a Blood Moon'd creature-land keeps its creature types".
  Spec.it s "CR 305.7 an Ashaya-animated, Blood Moon'd Wall of Stone is still a legal \"target Wall\"" $ do
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (wallId, g1) = S.addCreature wallOfStone S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addCreature ashaya S.alice g1
        (_, gs) = S.addCreature bloodMoon S.alice g2
        slot = SlotName.MkSlotName (Text.pack "target")
        legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing Map.empty S.noSource (Map.singleton slot (TargetSlot.required Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
    Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf wallId gs)) "it really is a Mountain"
    Spec.assertBool s (Projection.isCreatureOf wallId gs) "and still a creature (CR 305.7 removes no card types)"
    Spec.assertBool s (Set.member (Recipient.ToCreature wallId) legal) "so \"target Wall\" still offers it"
  Spec.it s "CR 115.1a ArtifactTarget is the battlefield's projected artifacts" $ do
    -- boardWithCreatureArtifactLand: alice has a Piker, a Mindslaver
    -- (Legendary Artifact) and a Mountain.
    piker <- S.printingOf s registry "Goblin Piker"
    mindslaver <- S.printingOf s registry "Mindslaver"
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.boardWithCreatureArtifactLand piker mindslaver mountain
        legal = Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact))) gs
    Spec.assertEqWith s "exactly the artifact" legal (Set.singleton (Recipient.ToObject (S.artifactId gs)))
    Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) legal)) "no players"
  Spec.it s "CR 115.1a / 109.5 OpponentCreatureTarget excludes the source's controller's creatures" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let gs0 = Setup.emptyGame S.bothPlayers
        (mine, gs1) = S.addCreature piker S.alice gs0
        (theirs, gs2) = S.addCreature warMammoth S.bob gs1
        legal = Target.legalRecipients (Just S.alice) mine (TargetSlot.required Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs2
    Spec.assertEqWith s "only the opponent's creature" legal (Set.singleton (Recipient.ToCreature theirs))
    Spec.assertBool s (not (Set.member (Recipient.ToCreature mine) legal)) "not the source's controller's own"
  -- CR 115.1 / 109.5: "target OPPONENT". Until Ravenous Rats there was no
  -- card in the pool that narrowed a PLAYER target, so Target.legalRecipients
  -- kept every player unconditionally (#168). Three seats, so "an opponent"
  -- is a real set rather than the only other player.
  Spec.it s "CR 115.1 a Players pool narrowed by IsPlayer Opponent excludes the source's controller" $ do
    ravenousRats <- S.printingOf s registry "Ravenous Rats"
    let (src, gs) = S.addCreature ravenousRats S.alice (Setup.emptyGame S.threePlayers)
        theSlot = TargetSlot.required Pool.Players (Just (Filter.Type.IsPlayer PlayerRelation.Opponent))
        legal = Target.legalRecipients (Just S.alice) src theSlot gs
    Spec.assertEqWith
      s
      "exactly bob and carol, never alice"
      legal
      (Set.fromList [Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol])
  -- The card itself, so the narrowing is proven through the real target slot
  -- the JSON carries rather than one hand-built in the test.
  Spec.it s "CR 115.1 Ravenous Rats' entry trigger may only target an opponent" $ do
    ravenousRats <- S.printingOf s registry "Ravenous Rats"
    let (src, gs) = S.addCreature ravenousRats S.bob (Setup.emptyGame S.threePlayers)
        -- The slot lives on the ENTRY TRIGGER, not the spell, so
        -- Card.allTargetSlots (which covers the spell and the enchant slot)
        -- is the wrong door -- read the ability the card actually prints.
        slots = fmap (Modal.allTargetSlots . TriggeredAbility.modal) (Face.triggeredAbilities (S.combinedFace ravenousRats))
    case concatMap Map.elems slots of
      [theSlot] ->
        Spec.assertEqWith
          s
          "bob is excluded from his own Rats' trigger"
          (Target.legalRecipients (Just S.bob) src theSlot gs)
          (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.carol])
      _ -> Spec.assertFailure s "Ravenous Rats should declare exactly one target slot"
  -- The gameplay-level proof design.md section 4 asks for: an opcode is not
  -- done until a card exercises it end to end. Ravenous Rats enters, its
  -- trigger is placed and targeted from the narrowed set, and an OPPONENT
  -- loses a card from hand -- not alice, who cast it.
  Spec.it s "CR 115.1 Ravenous Rats' entry trigger makes an opponent discard, never its own controller" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ravenousRats <- S.printingOf s registry "Ravenous Rats"
    let base0 = S.landsInPlay swamp 2
        -- Both players hold a card, so "whose hand shrank" is a real question.
        (_, base1) = S.addHandCard piker S.bob base0
        (gs, spellId) = S.handOne ravenousRats base1
        aliceBefore = S.handSize S.alice gs
        bobBefore = S.handSize S.bob gs
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
    Spec.assertEqWith s "bob discarded one" (S.handSize S.bob settled) (bobBefore - 1)
    Spec.assertEqWith s "alice lost only the Rats she cast" (S.handSize S.alice settled) (aliceBefore - 1)
    Spec.assertEqWith s "the Rats resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ravenous Rats") S.alice settled) 1
  Spec.it s "CR 806.1 at three seats a ControlledBy Opponent pool spans BOTH opponents' creatures" $ do
    -- Palace Jailer's second trigger targets a creature an opponent controls.
    -- At three seats that is a choice across two boards, and the engine must
    -- offer all of it. DISCRIMINATING: a relation resolved as "the next seat"
    -- offers only bob's, and carol is deliberately the far seat -- so an
    -- implementation that took one opponent fails on the set equality, not on
    -- a membership check that a superset would also satisfy.
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let gs0 = Setup.emptyGame S.threePlayers
        (mine, gs1) = S.addCreature piker S.alice gs0
        (bobs, gs2) = S.addCreature warMammoth S.bob gs1
        (carols, gs3) = S.addCreature piker S.carol gs2
        legal = Target.legalRecipients (Just S.alice) mine (TargetSlot.required Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs3
    Spec.assertEqWith
      s
      "exactly bob's and carol's, and nothing of alice's"
      legal
      (Set.fromList [Recipient.ToCreature bobs, Recipient.ToCreature carols])
  Spec.it s "CR 613.1b OpponentCreatureTarget follows PROJECTED control, not ownership" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (mine, gs1) = S.addCreature piker S.alice gs0
        (theirs, gs2) = S.addCreature warMammoth S.bob gs1
        (alsoTheirs, gs3) = S.addCreature typhoidRats S.bob gs2
        -- alice steals one of bob's creatures: it stops being "a creature an
        -- opponent controls" for alice's source, and becomes one for bob's.
        stolen = S.giveControl theirs S.alice gs3
    Spec.assertEqWith
      s
      "for alice's source, only the creature still under bob's control"
      (Target.legalRecipients (Projection.controllerOf mine stolen) mine (TargetSlot.required Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen)
      (Set.singleton (Recipient.ToCreature alsoTheirs))
    Spec.assertEqWith
      s
      "for bob's source, the two alice now controls"
      (Target.legalRecipients (Projection.controllerOf alsoTheirs stolen) alsoTheirs (TargetSlot.required Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen)
      (Set.fromList [Recipient.ToCreature mine, Recipient.ToCreature theirs])
  -- P9 (#40): the reshaped TargetSlot = Pool + Maybe Filter reproduces the
  -- retired hand-carved constructors as data. A black creature
  -- (Typhoid Rats, {B}) and a nonblack one (Goblin Piker, {1}{R}) exercise
  -- the Not (HasColor Black) filter that WAS NonblackCreatureTarget.
  Spec.it s "P9 Creatures + Not (HasColor Black) excludes a black creature" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
        (plainOid, gs) = S.addCreature piker S.alice gs1
        theSlot = TargetSlot.required Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))
        legal = Target.legalRecipients Nothing S.noSource theSlot gs
    Spec.assertBool s (not (Set.member (Recipient.ToCreature blackOid) legal)) "black creature illegal"
    Spec.assertBool s (Set.member (Recipient.ToCreature plainOid) legal) "nonblack creature legal"
  Spec.it s "P9 Creatures + Nothing narrows nothing" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
        (plainOid, gs) = S.addCreature piker S.alice gs1
        theSlot = TargetSlot.required Pool.Creatures Nothing
        expectedAllCreatures = Set.fromList [Recipient.ToCreature blackOid, Recipient.ToCreature plainOid]
    Spec.assertEqWith s "all creatures legal" (Target.legalRecipients Nothing S.noSource theSlot gs) expectedAllCreatures
  -- CR 601.2c "another" over a Creatures pool (#163). The pool tags its
  -- candidates ToCreature (CR 115.1a); a Not IsSource conjunct drops the
  -- source whatever tag the pool gave it, which the retired Exclusion field
  -- did not -- it deleted a ToObject recipient a Creatures pool never emits,
  -- so "another target creature" left the source legal.
  Spec.it s "another target creature excludes the source (CR 601.2c)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (srcId, gs1) = S.addCreature piker S.alice gs0
        (otherId, gs) = S.addCreature piker S.alice gs1
        slot = SlotName.MkSlotName (Text.pack "target")
        slots = Map.singleton slot (TargetSlot.required Pool.Creatures (Just (Filter.Type.Not Filter.Type.IsSource)))
    Spec.assertEqWith
      s
      "source excluded from its own set"
      (Target.legalSets Nothing Map.empty srcId slots gs)
      (Map.singleton slot (Set.singleton (Recipient.ToCreature otherId)))
  -- The other half of the same claim: a slot carrying no Not IsSource does
  -- not exclude, so Prodigal Sorcerer may still ping itself (CR 115.4).
  Spec.it s "a slot without Not IsSource still admits the source (CR 115.4)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (srcId, gs) = S.addCreature piker S.alice gs0
        slot = SlotName.MkSlotName (Text.pack "target")
        slots = Map.singleton slot (TargetSlot.required Pool.Creatures Nothing)
    Spec.assertEqWith
      s
      "source is its own legal target"
      (Target.legalSets Nothing Map.empty srcId slots gs)
      (Map.singleton slot (Set.singleton (Recipient.ToCreature srcId)))
  -- Gate cards for P9 Task 5: Terror and Reprisal. Both cards' printed text ends
  -- "It can't be regenerated.", which both card files carry as
  -- CantBeRegenerated; CR 701.19c's half is proved by Pawl.ReplacementSpec's
  -- "CR 701.19c a shield does not save a creature from a destruction that
  -- forbids regeneration", not here. The cases below are about the TARGET
  -- filters only.
  Spec.it s "Terror: And of Not(HasColor Black) and Not(HasCardType Artifact) excludes black and artifact creatures" $ do
    terror <- S.printingOf s registry "Terror"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSlot terror of
      Nothing -> Spec.assertFailure s "Terror's printing carries no 'target' slot"
      Just theSlot -> do
        let gs0 = Setup.emptyGame S.bothPlayers
            (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
            (artifactOid, gs2) = S.addCreature darksteelMyr S.bob gs1
            (plainOid, gs) = S.addCreature piker S.alice gs2
            legal = Target.legalRecipients Nothing S.noSource theSlot gs
        Spec.assertBool s (not (Set.member (Recipient.ToCreature blackOid) legal)) "black creature illegal"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature artifactOid) legal)) "artifact creature illegal"
        Spec.assertBool s (Set.member (Recipient.ToCreature plainOid) legal) "nonblack, nonartifact creature legal"
  Spec.it s "Reprisal: PowerAtLeast 4 legality tracks a projected power pump" $ do
    reprisal <- S.printingOf s registry "Reprisal"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSlot reprisal of
      Nothing -> Spec.assertFailure s "Reprisal's printing carries no 'target' slot"
      Just theSlot -> do
        let gs0 = Setup.emptyGame S.bothPlayers
            (smallOid, gs) = S.addCreature piker S.bob gs0 -- power 2, {1}{R}
            legalBefore = Target.legalRecipients Nothing S.noSource theSlot gs
            pumped = S.withEffect smallOid (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 2) (Quantity.Literal 0))) gs
            legalAfter = Target.legalRecipients Nothing S.noSource theSlot pumped
        Spec.assertBool s (not (Set.member (Recipient.ToCreature smallOid) legalBefore)) "power 2 is illegal (below the PowerAtLeast 4 floor)"
        Spec.assertBool s (Set.member (Recipient.ToCreature smallOid) legalAfter) "pumped to power 4 becomes legal"
  -- CR 508.1k: Kill Shot's IsAttacking narrowing, read off the committed card
  -- data. The defender is a creature in every other respect, so only combat
  -- status can be what separates the two.
  Spec.it s "Kill Shot: IsAttacking admits the attacker and rejects the untapped defender" $ do
    killShot <- S.printingOf s registry "Kill Shot"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSlot killShot of
      Nothing -> Spec.assertFailure s "Kill Shot's printing carries no 'target' slot"
      Just theSlot -> do
        let (board, mine, theirs) = S.combatBoardOf [piker] [piker]
            declared = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
            legal = Target.legalRecipients Nothing S.noSource theSlot declared
        case (mine, theirs) of
          (attacker : _, defender : _) -> do
            Spec.assertBool s (Map.member attacker (Combat.Type.attackers (GameState.combat declared))) "the fixture really did attack"
            Spec.assertBool s (Set.member (Recipient.ToCreature attacker) legal) "the attacker is legal"
            Spec.assertBool s (not (Set.member (Recipient.ToCreature defender) legal)) "the creature that stayed home is not"
          _ -> Spec.assertFailure s "fixture should have one creature a side"

resolveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
resolveSpec s registry = Spec.describe s "Resolve" $ do
  Spec.it s "CR 608 a resolved spell's damage is Noncombat" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.landsInPlay mountain 1
        (_target, gs0) = S.addCreature piker S.bob base
        (gs1, spellId) = S.handOne lightningBolt gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (S.cast S.alice spellId))
        -- resolveTop applies the damage but does NOT run SBAs, so the event persists.
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith
      s
      "the Bolt's damage event is Noncombat"
      (fmap DamageEvent.kind (S.damageEventsOf resolved))
      [DamageKind.Noncombat]
  Spec.it s "CR 608.3 / 704.5g a resolved Bolt kills a Piker" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
    Spec.assertEqWith s "no creature survives" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "Piker in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
  Spec.it s "CR 608.2n the resolved Bolt is in its owner's graveyard" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "one card" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  Spec.it s "CR 120.3a a Bolt at a player drains life without marking" $ do
    -- No creature on the battlefield, so identityAnswer's lookupMin picks
    -- ToPlayer alice: a self-Bolt, which is legal Magic.
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "seventeen" (S.lifeOf S.alice after) (Just 17)
  Spec.it s "the resolved damage flows through the event funnel" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "one event of amount 3" (fmap DamageEvent.amount (S.damageEventsOf after)) [3]
  Spec.it s "resolving a Bolt conserves objects" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
    Spec.assertEqWith s "conserved" (Game.objectCount (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))) (Game.objectCount cast)
  Spec.it s "CR 608.2b a Bolt whose only target died fizzles" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (base, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        -- Kill the Piker while the Bolt is on the stack, as Bolt B will in
        -- the integration test, then check state-based actions.
        dead = S.settleSba (S.markDamage (S.pikerOf base) 3 cast)
        after = snd (Engine.runGamePure S.identityAnswer dead Stack.resolveTop)
    Spec.assertEqWith s "Bolt in the graveyard, unresolved" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "no damage was dealt" (S.damageEventsOf after) []
    Spec.assertEqWith s "bob untouched" (S.lifeOf S.bob after) (Just 20)
  Spec.it s "CR 608.2b a fizzled spell applies none of its effects" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (base, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        dead = S.settleSba (S.markDamage (S.pikerOf base) 3 cast)
        after = snd (Engine.runGamePure S.identityAnswer dead Stack.resolveTop)
    Spec.assertEqWith s "life totals unchanged" (S.lifeOf S.alice after) (Just 20)
  -- The deterministic successor to the retired "instants happen" property: a
  -- Bolt cast in a game and resolved ends in its owner's graveyard.
  Spec.it s "a cast Bolt reaches its owner's graveyard" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "one card in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  Spec.it s "CR 612 slotsOf finds a ChangeText slot" $ do
    let slot = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "slotsOf" (Resolve.slotsOf (Effect.ChangeText (ChangeText.MkChangeText SubtypeFamily.CreatureType (Set.singleton Subtype.Wall) slot))) (Map.singleton slot SlotArity.One)
  -- The card lint's READ side for CR 106.4's recipient: Shizuko, Caller of
  -- Autumn's "that player" is a slot read, so a payload naming a slot no
  -- condition binds must look dangling. Asserted here because the pool cannot
  -- observe it -- Shizuko's own slot IS bound, so the lint passes either way and
  -- only a card written wrong would notice. A regression fence, not a proof of
  -- behaviour the pool exercises.
  Spec.it s "CR 106.4 slotsOf finds the slot an AddMana recipient names" $ do
    let slot = SlotName.MkSlotName (Text.pack "thatPlayer")
    Spec.assertEqWith s "a named recipient is a read" (Resolve.slotsOf (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.InSlot slot) ManaProduction.AnyColor ManaRetention.Ordinary Nothing))) (Map.singleton slot SlotArity.One)
    Spec.assertEqWith s "and CR 109.5's unwritten one names no slot" (Resolve.slotsOf (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) ManaProduction.AnyColor ManaRetention.Ordinary Nothing))) Map.empty
  Spec.it s "CR 605 manaProduced reads AddMana, nothing else" $ do
    Spec.assertEqWith s "add mana" (ManaAbility.manaProduced (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) (ManaProduction.OfType (ManaType.Colored Color.Green)) ManaRetention.Ordinary Nothing))) (Just (ManaProduction.OfType (ManaType.Colored Color.Green)))
    Spec.assertEqWith s "add mana of any color" (ManaAbility.manaProduced (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) ManaProduction.AnyColor ManaRetention.Ordinary Nothing))) (Just ManaProduction.AnyColor)
    -- CR 605.1a asks whether the ability could add mana to "a player's" pool, so a
    -- recipient the card names is dropped rather than disqualifying: an ability
    -- that adds to somebody else is still a mana ability.
    Spec.assertEqWith s "a named recipient is dropped" (ManaAbility.manaProduced (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) ManaProduction.AnyColor ManaRetention.Ordinary Nothing))) (Just ManaProduction.AnyColor)
    Spec.assertEqWith s "damage produces no mana" (ManaAbility.manaProduced (Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "x"))) (Quantity.Literal 1) Nothing Nothing))) Nothing
  Spec.it s "CR 612.1 a text change reaches a Filter carried by an effect" $ do
    -- Boil ("Destroy all Islands") is the first card whose effect selects by
    -- a BASIC LAND TYPE, so it is the first that can tell whether CR 612.1's
    -- "any words or symbols printed on that object" reaches inside an
    -- effect's Filter. The stored ChangeSubtypeWord is what a resolved
    -- Magical Hack leaves on the spell.
    --
    -- The Filter half of read-point 3 (Resolve.modesOf) rests on this case
    -- alone: no real instant or sorcery SETS a land's subtype. The Modification
    -- half of the same read-point is Turn to Frog's SetCreatureSubtype under an
    -- Artificial Evolution (Pawl.CounterspellSpec's ArtificialEvolution group), and
    -- Pawl.ActivateSpec's Tidal Warrior chain reaches the same
    -- Projection.rewriteEffect ModifyTarget arm through an ACTIVATED ability.
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    boil <- S.printingOf s registry "Boil"
    let base = Setup.emptyGame S.bothPlayers
        (islandId, g1) = S.addCreature island S.alice base
        (forestId, g2) = S.addCreature forest S.alice g1
        (boilPrintingId, g2b) = Game.intern boil g2
        (boilId, g3) = Game.freshObjectId g2b
        boilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfCard boilPrintingId,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              -- CR 700.2: Boil has one mode, and a directly-built stack object
              -- (bypassing Cast.castSpell) must stamp it chosen (mode 0), or
              -- Resolve.modesOf and Resolve.targetSlotsOf -- both scoped to the
              -- CHOSEN modes through Binding.modesOf -- would see no effects and
              -- no target slots at all.
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = Timestamp.MkTimestamp 0,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapNext = False,
              Object.exertedBy = Set.empty
            }
        g4 =
          g3
            { GameState.objects = Map.insert boilId boilObj (GameState.objects g3),
              GameState.stack = boilId : GameState.stack g3
            }
        resolve g = snd (Engine.runGamePure S.identityAnswer g (Resolve.resolveSpell boilId))
        onBattlefield oid g = Set.member oid (GameState.battlefield g)
        plain = resolve g4
        hacked = resolve (S.withEffectAt boilId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Island Subtype.Forest)) g4)
    -- The control: unhacked, Boil does what it prints.
    Spec.assertBool s (not (onBattlefield islandId plain)) "unhacked, the Island dies"
    Spec.assertBool s (onBattlefield forestId plain) "unhacked, the Forest lives"
    -- And hacked, the word swap moves which lands the filter admits.
    Spec.assertBool s (not (onBattlefield forestId hacked)) "hacked, the Forest dies"
    Spec.assertBool s (onBattlefield islandId hacked) "hacked, the Island lives"
  Spec.it s "CR 400.7a hacking Blood Moon on the stack carries onto the permanent" $ do
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (nonbasicId, g1) = S.addCreature urborg S.alice base
        (bloodMoonPrintingId, g1b) = Game.intern bloodMoon g1
        (bloodMoonSpellId, g2) = Game.freshObjectId g1b
        bmObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfCard bloodMoonPrintingId,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Map.empty,
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = Timestamp.MkTimestamp 0,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapNext = False,
              Object.exertedBy = Set.empty
            }
        g3 =
          g2
            { GameState.objects = Map.insert bloodMoonSpellId bmObj (GameState.objects g2),
              GameState.stack = bloodMoonSpellId : GameState.stack g2
            }
        hacked = S.withEffectAt bloodMoonSpellId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Mountain Subtype.Island)) g3
        after = snd (Engine.runGamePure S.identityAnswer hacked Stack.resolveTop)
    -- CR 400.7 mints a NEW object for the permanent, but CR 400.7a is the
    -- exception: an effect that changes a PERMANENT SPELL's characteristics
    -- keeps applying to the permanent that spell becomes, and rules text is a
    -- characteristic (CR 109.3). So the hacked Blood Moon reads "Nonbasic lands
    -- are Islands" on the battlefield, and Urborg -- a nonbasic land -- is an
    -- Island. Stack.carryOver is what re-keys the stored effect.
    Spec.assertEqWith s "hack carried over: nonbasic land is Island" (Projection.subtypesOf nonbasicId after) (Set.singleton Subtype.Island)
  Spec.it s "CR 608.2n a resolving ability deals its damage and ceases" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        ability = case Face.activatedAbilities (S.combinedFace prodigalSorcerer) of
          ab : _ -> ab
          [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)) [] Nothing Nothing
        (abilId, g1) = Game.freshObjectId g0
        (ts, g2) = Game.freshTimestamp g1
        slot = SlotName.MkSlotName (Text.pack "target")
        abilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source =
                Source.OfAbility
                  ActivatedAbilitySource.MkActivatedAbilitySource
                    { ActivatedAbilitySource.source = srcId,
                      ActivatedAbilitySource.ability = ability
                    },
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer S.bob))) Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapNext = False,
              Object.exertedBy = Set.empty
            }
        g3 =
          g2
            { GameState.objects = Map.insert abilId abilObj (GameState.objects g2),
              GameState.stack = abilId : GameState.stack g2
            }
        resolved = snd (Engine.runGamePure S.identityAnswer g3 Stack.resolveTop)
    Spec.assertEqWith s "bob took 1" (S.lifeOf S.bob resolved) (Just 19)
    Spec.assertEqWith s "ability object gone" (Game.lookupObject abilId resolved) Nothing
    Spec.assertEqWith s "stack empty" (GameState.stack resolved) []
  Spec.it s "CR 701.23 Search fetches a basic land to the battlefield tapped" $ do
    -- The fetched card gets a NEW object id (CR 400.7 changeZone), so assert by
    -- count/tapped-count, never by the library incarnation's id.
    mountain <- S.printingOf s registry "Mountain"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addLibraryCard mountain S.alice base
        ability =
          ActivatedAbility.MkActivatedAbility
            (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [])
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Just (Quantity.Literal 1), Search.filter = basicLandFilter, Search.upTo = False, Search.destination = SearchDestination.BattlefieldTapped}]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            []
            Nothing
            Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility ActivatedAbilitySource.MkActivatedAbilitySource {ActivatedAbilitySource.source = ObjectId.MkObjectId 0, ActivatedAbilitySource.ability = ability}) Zone.Stack TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False 0 (Mana.MkMana []) Nothing Set.empty Set.empty False Set.empty
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
    Spec.assertEqWith s "one permanent on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice resolved)) 1
    Spec.assertEqWith s "it is tapped" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "library empty" (Game.zoneMembers Zone.Library S.alice resolved) []
  Spec.it s "CR 701.23b Search may fail to find" $ do
    mountain <- S.printingOf s registry "Mountain"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addLibraryCard mountain S.alice base
        ability = ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Just (Quantity.Literal 1), Search.filter = basicLandFilter, Search.upTo = False, Search.destination = SearchDestination.BattlefieldTapped}]))) Map.empty)) (ModeSelection.ChooseExactly 1)) [] Nothing Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj = Object.MkObject S.alice Nothing (Source.OfAbility ActivatedAbilitySource.MkActivatedAbilitySource {ActivatedAbilitySource.source = ObjectId.MkObjectId 0, ActivatedAbilitySource.ability = ability}) Zone.Stack TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False 0 (Mana.MkMana []) Nothing Set.empty Set.empty False Set.empty
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findNothing g4 Stack.resolveTop)
    Spec.assertEqWith s "nothing entered the battlefield" (GameState.battlefield resolved) Set.empty
  Spec.it s "CR 701.23a Search (And [HasCardType Land, HasSupertype Basic]) offers a basic land, not a nonland" $ do
    -- P9: the Search filter reads each library card through its own CR 613
    -- projection (Projection.viewOfObject) -- the card is an object and has one
    -- there like any other. For a card no continuous effect reaches, that view
    -- is the printed card.
    -- With a Mountain (basic land) and a Piker (creature) both in the library,
    -- only the Mountain is a candidate: findFirst fetches it while the Piker
    -- stays put. The Piker is added SECOND, so it is the head of the library
    -- (Support.addLibraryCard prepends); a filter that matched everything would
    -- fetch the Piker and this test would fail.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (_, g0) = S.addLibraryCard mountain S.alice base
        (pikerId, g1) = S.addLibraryCard piker S.alice g0
        ability =
          ActivatedAbility.MkActivatedAbility
            (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [])
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Just (Quantity.Literal 1), Search.filter = basicLandFilter, Search.upTo = False, Search.destination = SearchDestination.BattlefieldTapped}]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            []
            Nothing
            Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility ActivatedAbilitySource.MkActivatedAbilitySource {ActivatedAbilitySource.source = ObjectId.MkObjectId 0, ActivatedAbilitySource.ability = ability}) Zone.Stack TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False 0 (Mana.MkMana []) Nothing Set.empty Set.empty False Set.empty
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
    Spec.assertEqWith s "the basic land is offered and fetched to the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Mountain") S.alice resolved) 1
    Spec.assertBool s (elem pikerId (Game.zoneMembers Zone.Library S.alice resolved)) "the nonland is not offered -- it remains in the library"
  -- #222: CR 701.23a's filter defines what the search may find. An
  -- interpreter that names a card the filter excluded must find nothing --
  -- "fails to find" is already a legal outcome, so rejecting needs no new
  -- branch. Same fixture as the test above, so the only variable is the answer.
  Spec.it s "#222 a search that names a card the filter excluded fetches nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (_, g0) = S.addLibraryCard mountain S.alice base
        (pikerId, g1) = S.addLibraryCard piker S.alice g0
        ability =
          ActivatedAbility.MkActivatedAbility
            (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [])
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Just (Quantity.Literal 1), Search.filter = basicLandFilter, Search.upTo = False, Search.destination = SearchDestination.BattlefieldTapped}]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            []
            Nothing
            Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility ActivatedAbilitySource.MkActivatedAbilitySource {ActivatedAbilitySource.source = ObjectId.MkObjectId 0, ActivatedAbilitySource.ability = ability}) Zone.Stack TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False 0 (Mana.MkMana []) Nothing Set.empty Set.empty False Set.empty
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure (findForbidden pikerId) g4 Stack.resolveTop)
    Spec.assertEqWith s "the Piker was NOT fetched to the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice resolved) 0
    Spec.assertBool s (elem pikerId (Game.zoneMembers Zone.Library S.alice resolved)) "it is still in the library"
    Spec.assertEqWith s "and nothing else was fetched either" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Mountain") S.alice resolved) 0
  -- Hoarding Dragon -- "Flying. When this creature enters, you may search your
  -- library for an artifact card, exile it, then shuffle." The whole-card proof
  -- of SearchDestination.Exile, cast and resolved rather than assembled.
  --
  -- The printed card's second half -- "When this creature dies, you may put the
  -- exiled card into its owner's hand" -- is CR 607.2a's linked ability, and the
  -- two cases at the end of this group are what prove the link picks out the
  -- right card.
  --
  -- The destination is the assertion, and three readings have to be told apart:
  -- exile, hand (RevealThenHand) and battlefield (BattlefieldTapped). So the
  -- Altar is asserted present in exile AND absent from both other zones, and the
  -- empty reveal log separates CR 701.23e's silent exile from the Sextant's
  -- "reveal that card". The Piker is in the library so the filter has a card to
  -- reject; it is added second, so Support.addLibraryCard makes it the head and a
  -- filter that admitted everything would exile it instead.
  Spec.it s "CR 701.23a/701.23e whole card: Hoarding Dragon exiles the artifact it finds, unrevealed" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    altar <- S.printingOf s registry "Ashnod's Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    let base0 = S.landsInPlay mountain 5
        (_, base1) = S.addLibraryCard altar S.alice base0
        (pikerId, base2) = S.addLibraryCard piker S.alice base1
        (gs, spellId) = S.handOne dragon base2
        cast = snd (Engine.runGamePure findFirstExercising gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure findFirstExercising cast Engine.priorityLoop)
    Spec.assertEqWith s "the Dragon resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Hoarding Dragon") S.alice settled) 1
    Spec.assertEqWith
      s
      "the Altar, and only the Altar, is in exile"
      (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Exile S.alice settled))
      [CardName.MkCardName $ Text.pack "Ashnod's Altar"]
    Spec.assertEqWith s "it did NOT go to her hand -- she cast her only card" (S.handSize S.alice settled) 0
    Spec.assertEqWith s "nor onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ashnod's Altar") S.alice settled) 0
    Spec.assertEqWith s "CR 701.23e: the card says only \"exile it\", so nothing was revealed" (S.revealsOf settled) []
    Spec.assertEqWith s "the nonartifact was no candidate and stayed in the library" (Game.zoneMembers Zone.Library S.alice settled) [pikerId]
  -- The paired negative: the SAME board, the same mana, the same answers, with
  -- CR 603.5's "may" declined instead of exercised. Nothing is searched and
  -- nothing is exiled, so an engine that exiled a card off some other path than
  -- this search would fail here.
  Spec.it s "CR 603.5 declining Hoarding Dragon's \"may\" exiles nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    altar <- S.printingOf s registry "Ashnod's Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    let base0 = S.landsInPlay mountain 5
        (altarId, base1) = S.addLibraryCard altar S.alice base0
        (pikerId, base2) = S.addLibraryCard piker S.alice base1
        (gs, spellId) = S.handOne dragon base2
        cast = snd (Engine.runGamePure findFirstDeclining gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure findFirstDeclining cast Engine.priorityLoop)
    Spec.assertEqWith s "the Dragon still resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Hoarding Dragon") S.alice settled) 1
    Spec.assertEqWith s "exile is empty" (Game.zoneMembers Zone.Exile S.alice settled) []
    Spec.assertEqWith s "both library cards are still there" (Set.fromList (Game.zoneMembers Zone.Library S.alice settled)) (Set.fromList [altarId, pikerId])
  -- CR 607.2a's linked set, on the board that can tell it from "every card in
  -- exile": TWO Hoarding Dragons, each of which exiled a different artifact, and
  -- one of them dies. The pair below runs the SAME board twice and differs in
  -- exactly one thing -- which Dragon takes the lethal damage -- so an engine
  -- whose "the exiled card" named all of exile, or the oldest entry, or the
  -- newest, fails one of the two.
  --
  -- Two objects of ONE NAME rather than two different exilers, because that is
  -- the reading CR 607.2a singles out: the link is per OBJECT, and a second copy
  -- of the same printing is a different object with its own set.
  --
  -- Each search is PINNED to a named card rather than taking the head of the
  -- offered list, so which Dragon holds which artifact is decided by the fixture
  -- and not by where a shuffle left the library.
  --
  -- The kill is marked damage plus CR 704.5g, which is a LEAVE-THE-BATTLEFIELD
  -- event: CR 603.10a makes the dies trigger look back, so its source is the
  -- permanent as it was on the battlefield -- the same id that did the exiling,
  -- and the whole reason the link survives its own object's death.
  Spec.it s "CR 607.2a: the dead Dragon returns the card IT exiled, not the other Dragon's" $ do
    board <- twoDragonBoard s registry
    Spec.assertEqWith s "the first Dragon's artifact came back to her hand" (handNames (kill (firstDragon board) board)) [firstArtifact board]
    Spec.assertEqWith s "and the surviving Dragon's is still in exile" (exileNames (kill (firstDragon board) board)) [secondArtifact board]
  Spec.it s "CR 607.2a: killing the OTHER Dragon returns the OTHER card" $ do
    board <- twoDragonBoard s registry
    Spec.assertEqWith s "the second Dragon's artifact came back to her hand" (handNames (kill (secondDragon board) board)) [secondArtifact board]
    Spec.assertEqWith s "and the first Dragon's is still in exile" (exileNames (kill (secondDragon board) board)) [firstArtifact board]
  -- Fertilid's Favor -- "Target player searches their library for a basic land
  -- card, puts it onto the battlefield tapped, then shuffles. Put two +1/+1
  -- counters on up to one target artifact or creature." The whole-card proof that
  -- a search reads the library the effect's PlayerRef names rather than its
  -- controller's, cast and resolved rather than assembled.
  --
  -- THREE seats, because two collapse "the targeted player" onto "the one
  -- opponent" and a controller-defaulting engine would still be caught only by
  -- luck. alice casts, carol is targeted, and bob is the seat neither role names.
  --
  -- A Mountain in EVERY library, because one library holding the only basic land
  -- cannot tell "read carol's library" from "read every library": the assertions
  -- below are which SEAT gained the land and which libraries kept theirs, and
  -- each of the three answers a different reading of the rule. The Piker in
  -- carol's library gives the filter a card to reject; it is added second, so
  -- Support.addLibraryCard makes it the head and a filter that admitted
  -- everything would fetch it instead.
  Spec.it s "CR 701.23a whole card: Fertilid's Favor searches the TARGET player's library, not its controller's" $ do
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    favor <- S.printingOf s registry "Fertilid's Favor"
    let withLands = List.foldl' (\g _ -> snd (S.addCreature forest S.alice g)) S.threePlayerGame [1 .. (4 :: Int)]
        (aliceCard, g1) = S.addLibraryCard mountain S.alice withLands
        (bobCard, g2) = S.addLibraryCard mountain S.bob g1
        (_, g3) = S.addLibraryCard mountain S.carol g2
        (carolPiker, g4) = S.addLibraryCard piker S.carol g3
        (gs, spellId) = S.handOne favor g4
        cast = snd (Engine.runGamePure atCarolFinding gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure atCarolFinding cast Engine.priorityLoop)
        mountainName = CardName.MkCardName $ Text.pack "Mountain"
    Spec.assertEqWith s "carol got the basic land" (S.countOnBattlefieldByName mountainName S.carol settled) 1
    Spec.assertEqWith s "and it entered tapped" (S.tappedCount S.carol settled) 1
    Spec.assertEqWith s "the spell's controller got nothing -- her own Mountain was no candidate" (S.countOnBattlefieldByName mountainName S.alice settled) 0
    Spec.assertEqWith s "nor did the third seat" (S.countOnBattlefieldByName mountainName S.bob settled) 0
    Spec.assertEqWith s "alice's library is untouched" (Game.zoneMembers Zone.Library S.alice settled) [aliceCard]
    Spec.assertEqWith s "bob's library is untouched" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
    Spec.assertEqWith s "carol's library kept only the card the filter rejected" (Game.zoneMembers Zone.Library S.carol settled) [carolPiker]
  -- Explosive Vegetation -- "Search your library for up to two basic land cards,
  -- put them onto the battlefield tapped, then shuffle." The whole-card proof
  -- that a search's count is a MAXIMUM the searcher chooses within (CR 701.23a),
  -- cast and resolved rather than assembled. Its whole printed text is
  -- expressible, so nothing about pawl's copy runs weaker than the card.
  --
  -- THREE basic lands in the library against a cap of two, so the count is
  -- observable rather than exhausted: a search that found three because only
  -- three were there proves nothing about the "up to two". All three are
  -- DIFFERENT basics, so which two were found is assertable -- and none of them
  -- is the Forest the mana came from, which would otherwise make a fetched land
  -- indistinguishable from one already in play. The Piker gives the filter a
  -- nonland to reject.
  --
  -- The find is PINNED to specific ids rather than "the first n offered":
  -- Support.addLibraryCard prepends, so the head of the library is the Piker and
  -- then the Plains, and an engine that took the head of the candidate list
  -- would fetch the Plains this answer never names.
  --
  -- The three cases below are the same board and the same mana, differing in
  -- exactly one thing: how many of the two the searcher takes.
  Spec.it s "CR 701.23a whole card: Explosive Vegetation finds both of its \"up to two\"" $ do
    board <- vegetationBoard s registry
    let settled = resolveVegetation (findPinned [vegetationMountain board, vegetationIsland board]) board
    Spec.assertEqWith
      s
      "the Mountain and the Island she named are on the battlefield, beside her four Forests"
      (List.sort (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Battlefield S.alice settled)))
      (List.sort (fmap (CardName.MkCardName . Text.pack) ["Forest", "Forest", "Forest", "Forest", "Island", "Mountain"]))
    -- Every Forest paid for the spell, so nothing on the battlefield is untapped
    -- unless a fetched land entered that way -- which is the destination's whole
    -- assertion. Rule 701.23 says only how to LOOK, so "onto the battlefield
    -- tapped" is the card's own instruction.
    Spec.assertEqWith s "and both entered TAPPED" (fmap (`S.soleFaceName` settled) (untappedOf S.alice settled)) []
    Spec.assertEqWith
      s
      "the third basic and the nonland stayed in the library"
      (Set.fromList (Game.zoneMembers Zone.Library S.alice settled))
      (Set.fromList [vegetationPlains board, vegetationPiker board])
  Spec.it s "CR 701.23b whole card: Explosive Vegetation may find FEWER than its \"up to two\"" $ do
    board <- vegetationBoard s registry
    let settled = resolveVegetation (findPinned [vegetationMountain board]) board
    Spec.assertEqWith
      s
      "only the one basic she named is on the battlefield"
      (List.sort (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Battlefield S.alice settled)))
      (List.sort (fmap (CardName.MkCardName . Text.pack) ["Forest", "Forest", "Forest", "Forest", "Mountain"]))
    Spec.assertEqWith s "and it entered tapped" (fmap (`S.soleFaceName` settled) (untappedOf S.alice settled)) []
    Spec.assertEqWith
      s
      "the two basics she passed over are still in the library"
      (Set.fromList (Game.zoneMembers Zone.Library S.alice settled))
      (Set.fromList [vegetationIsland board, vegetationPlains board, vegetationPiker board])
  Spec.it s "CR 701.23b whole card: Explosive Vegetation may decline to find at all" $ do
    board <- vegetationBoard s registry
    let settled = resolveVegetation (findPinned []) board
    Spec.assertEqWith
      s
      "nothing was fetched -- only the Forests she paid with are on the battlefield"
      (List.sort (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Battlefield S.alice settled)))
      (replicate 4 (CardName.MkCardName (Text.pack "Forest")))
    Spec.assertEqWith
      s
      "every library card is still there"
      (Set.fromList (Game.zoneMembers Zone.Library S.alice settled))
      (Set.fromList [vegetationMountain board, vegetationIsland board, vegetationPlains board, vegetationPiker board])
  -- Mana Severance -- "Search your library for any number of land cards, exile
  -- them, then shuffle." The whole-card proof that a search can state NO count
  -- (CR 701.23a: the find is bounded by what the zone holds, not by a number the
  -- card names), cast and resolved rather than assembled. Its whole printed text
  -- is expressible.
  --
  -- FOUR matching lands, all DIFFERENT basics, against a search that names no
  -- number: a cap that came from anywhere but the library's own contents would
  -- have to be a literal, and four distinct names make "all four" assertable
  -- rather than "one, four times" -- List.nub in the executor cannot repair it.
  -- None of them is the Island the mana came from. The Piker gives the filter a
  -- nonland to reject, so the offer is strictly larger than the largest legal
  -- answer and the prompt cannot short-circuit.
  --
  -- The first three cases are the same board and the same mana, differing in
  -- exactly how many of the four the searcher takes. The fourth changes the
  -- board instead: FIVE matching lands and the same card, which is what rules
  -- out a literal. A four-land board alone cannot -- a search for "up to four"
  -- would pass all three of the cases above.
  Spec.it s "CR 701.23a whole card: Mana Severance's \"any number of\" exiles every land she names" $ do
    board <- severanceBoard s registry severanceFour
    let settled = resolveSeverance (findPinned (severanceLands board)) board
    Spec.assertEqWith
      s
      "all four lands she named are in exile"
      (List.sort (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Exile S.alice settled)))
      (List.sort (fmap (CardName.MkCardName . Text.pack) ["Forest", "Mountain", "Plains", "Swamp"]))
    Spec.assertEqWith
      s
      "and only the nonland the filter rejected is left in the library"
      (Game.zoneMembers Zone.Library S.alice settled)
      [severancePiker board]
  Spec.it s "CR 701.23b whole card: Mana Severance may find FEWER than the library holds" $ do
    board <- severanceBoard s registry severanceFour
    let settled = resolveSeverance (findPinned (take 2 (severanceLands board))) board
    Spec.assertEqWith
      s
      "only the two she named are in exile"
      (List.sort (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Exile S.alice settled)))
      (List.sort (fmap (CardName.MkCardName . Text.pack) ["Mountain", "Swamp"]))
    Spec.assertEqWith
      s
      "the two lands she passed over are still in the library"
      (Set.fromList (Game.zoneMembers Zone.Library S.alice settled))
      (Set.fromList (severancePiker board : drop 2 (severanceLands board)))
  -- The case that separates "no count" from "as many as possible". CR 701.23d
  -- forces a search made "simply for a quantity of cards", which one stating no
  -- quantity is not, and CR 701.23b's fail-to-find covers the stated quality
  -- besides -- so an answer of zero stands and nothing is completed from the
  -- lands she passed over.
  Spec.it s "CR 701.23b whole card: Mana Severance may decline to find at all" $ do
    board <- severanceBoard s registry severanceFour
    let settled = resolveSeverance (findPinned []) board
    Spec.assertEqWith
      s
      "nothing was exiled"
      (Game.zoneMembers Zone.Exile S.alice settled)
      []
    Spec.assertEqWith
      s
      "every library card is still there"
      (Set.fromList (Game.zoneMembers Zone.Library S.alice settled))
      (Set.fromList (severancePiker board : severanceLands board))
  -- The same card over a bigger library. CR 701.23a bounds the find by the ZONE,
  -- so the count moves with the library and no number the card could have stated
  -- explains both this case and the four-land one above.
  Spec.it s "CR 701.23a whole card: Mana Severance's count is the library's, so five lands exile five" $ do
    board <- severanceBoard s registry (severanceFour <> ["Evolving Wilds"])
    let settled = resolveSeverance (findPinned (severanceLands board)) board
    Spec.assertEqWith
      s
      "all five lands she named are in exile"
      (List.sort (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Exile S.alice settled)))
      (List.sort (fmap (CardName.MkCardName . Text.pack) ["Evolving Wilds", "Forest", "Mountain", "Plains", "Swamp"]))
    Spec.assertEqWith
      s
      "and only the nonland the filter rejected is left in the library"
      (Game.zoneMembers Zone.Library S.alice settled)
      [severancePiker board]
  -- Extract -- "{U} Sorcery: Search target player's library for a card and exile
  -- it. Then that player shuffles." The whole-card proof that the player LOOKING
  -- and the player whose library is looked at can be different seats (CR 701.23a),
  -- and that a search stating no quality must find (CR 701.23d).
  --
  -- Its filter is `And []`, the trivial predicate: "a card" states nothing about
  -- what may be found, so every card in the library is a candidate and no
  -- assertion here can pass because a filter quietly rejected something.
  Spec.it s "CR 701.23a whole card: Extract's controller searches the TARGET player's library" $ do
    island <- S.printingOf s registry "Island"
    extract <- S.printingOf s registry "Extract"
    piker <- S.printingOf s registry "Goblin Piker"
    altar <- S.printingOf s registry "Ashnod's Altar"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let (gs, spellId, aliceLib, bobCard, carolLib) = extractBoard island extract piker altar mountain forest plains
        pinned = case carolLib of
          _ : middle : _ -> middle
          _ -> ObjectId.MkObjectId 0
        cast = snd (Engine.runGamePure (aliceFinding pinned) gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure (aliceFinding pinned) cast Engine.priorityLoop)
    Spec.assertEqWith s "carol's library starts in the order the pins assume" (Game.zoneMembers Zone.Library S.carol gs) carolLib
    Spec.assertEqWith
      s
      "the card alice named, and only it, is in exile"
      (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Exile S.carol settled))
      [CardName.MkCardName $ Text.pack "Forest"]
    Spec.assertEqWith s "carol keeps the other two, in order" (Game.zoneMembers Zone.Library S.carol settled) (filter (/= pinned) carolLib)
    Spec.assertEqWith s "the searcher's own library is untouched -- she read carol's" (Game.zoneMembers Zone.Library S.alice settled) aliceLib
    Spec.assertEqWith s "and so is the third seat's" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
    Spec.assertEqWith s "nothing of alice's was exiled" (Game.zoneMembers Zone.Exile S.alice settled) []
    Spec.assertEqWith s "nor of bob's" (Game.zoneMembers Zone.Exile S.bob settled) []
    Spec.assertEqWith s "the found card was exiled, not fetched to the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Forest") S.carol settled) 0
    Spec.assertEqWith s "nor put into a hand" (S.handSize S.carol settled) 0
  -- The same board, with the search answered "nothing". CR 701.23b's permission
  -- does NOT apply -- it is for a search stating a quality, and Extract states
  -- none -- so CR 701.23d makes the find mandatory and the answer is completed.
  -- The paired negative is "CR 701.23b Search may fail to find" above: same
  -- declining answer, a filter that states a quality, nothing found.
  Spec.it s "CR 701.23d whole card: Extract must find, so declining still exiles a card" $ do
    island <- S.printingOf s registry "Island"
    extract <- S.printingOf s registry "Extract"
    piker <- S.printingOf s registry "Goblin Piker"
    altar <- S.printingOf s registry "Ashnod's Altar"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let (gs, spellId, aliceLib, bobCard, carolLib) = extractBoard island extract piker altar mountain forest plains
        cast = snd (Engine.runGamePure aliceFindingNothing gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure aliceFindingNothing cast Engine.priorityLoop)
    Spec.assertEqWith
      s
      "a card was exiled anyway, and it is the head of the library the engine completed with"
      (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Exile S.carol settled))
      [CardName.MkCardName $ Text.pack "Plains"]
    Spec.assertEqWith s "carol's library lost exactly that one" (Game.zoneMembers Zone.Library S.carol settled) (drop 1 carolLib)
    Spec.assertEqWith s "alice's library is still untouched" (Game.zoneMembers Zone.Library S.alice settled) aliceLib
    Spec.assertEqWith s "and bob's" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
  -- The same board again, with the shuffle answered by reversing whatever library
  -- it is offered. Which library gets shuffled is the CARD's sentence -- "then
  -- that player shuffles", its target -- rather than rule 701.24's, which says
  -- only what shuffling does (CR 701.24a). So carol's order changes and nobody
  -- else's does. An engine that shuffled the SEARCHER's library instead would
  -- overwrite alice's with carol's cards, which the last two assertions read
  -- directly.
  Spec.it s "CR 701.24a whole card: Extract shuffles the library it searched, the TARGET player's" $ do
    island <- S.printingOf s registry "Island"
    extract <- S.printingOf s registry "Extract"
    piker <- S.printingOf s registry "Goblin Piker"
    altar <- S.printingOf s registry "Ashnod's Altar"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let (gs, spellId, aliceLib, bobCard, carolLib) = extractBoard island extract piker altar mountain forest plains
        pinned = case carolLib of
          _ : middle : _ -> middle
          _ -> ObjectId.MkObjectId 0
        cast = snd (Engine.runGamePure (aliceFindingReversing pinned) gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure (aliceFindingReversing pinned) cast Engine.priorityLoop)
    Spec.assertEqWith s "carol's remaining cards came back reversed" (Game.zoneMembers Zone.Library S.carol settled) (reverse (filter (/= pinned) carolLib))
    Spec.assertEqWith s "alice's library kept its order -- hers was never shuffled" (Game.zoneMembers Zone.Library S.alice settled) aliceLib
    Spec.assertEqWith s "nor was bob's" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
  -- Denying Wind -- "{7}{U}{U} Sorcery: Search target player's library for up to
  -- seven cards and exile them. Then that player shuffles." Extract's sentence
  -- with two numbers changed and "up to" added, which is the whole point of
  -- choosing it as the producer: its filter is `And []` too,
  -- so Filter.statesAQuality answers False for both and only Search.upTo can tell
  -- them apart. Under CR 701.23d alone the search would find seven; the card's own
  -- "up to" makes the count a ceiling the searcher chooses within.
  --
  -- Carol's library holds nine cards, so the cap (seven), the answer (two) and
  -- the library size are three different numbers and no assertion can pass on a
  -- coincidence.
  Spec.it s "CR 701.23a whole card: Denying Wind's \"up to seven\" honours an answer of two" $ do
    island <- S.printingOf s registry "Island"
    denyingWind <- S.printingOf s registry "Denying Wind"
    piker <- S.printingOf s registry "Goblin Piker"
    altar <- S.printingOf s registry "Ashnod's Altar"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    let (gs, spellId, bobCard, pinned) = denyingWindBoard island denyingWind piker altar forest mountain
        carolLib = Game.zoneMembers Zone.Library S.carol gs
        cast = snd (Engine.runGamePure (aliceFindingThese pinned) gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure (aliceFindingThese pinned) cast Engine.priorityLoop)
    Spec.assertEqWith s "carol's library starts with nine cards" (length carolLib) 9
    Spec.assertEqWith
      s
      "exactly the two cards alice named are in exile -- not the seven a quota would have completed"
      (List.sort (namesIn Zone.Exile S.carol settled))
      (List.sort (fmap (Just . CardName.MkCardName . Text.pack) ["Forest", "Mountain"]))
    Spec.assertEqWith s "so carol kept the other seven" (length (Game.zoneMembers Zone.Library S.carol settled)) 7
    Spec.assertEqWith s "and none of them is one of the two she lost" (namesIn Zone.Library S.carol settled) (replicate 7 (Just (CardName.MkCardName (Text.pack "Goblin Piker"))))
    Spec.assertEqWith s "the searcher's own library is untouched -- she read carol's" (Game.zoneMembers Zone.Exile S.alice settled) []
    Spec.assertEqWith s "and the third seat's" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
  -- The same board answered "nothing". CR 701.23b's fail-to-find is what "up to"
  -- grants at the bottom of its range, and Denying Wind states no quality, so
  -- this case passes only because the flag is read. Its control is Extract, three
  -- cases above: the same `And []` filter and the same declining answer, and it
  -- must still exile a card under CR 701.23d.
  Spec.it s "CR 701.23a whole card: Denying Wind may decline to find at all" $ do
    island <- S.printingOf s registry "Island"
    denyingWind <- S.printingOf s registry "Denying Wind"
    piker <- S.printingOf s registry "Goblin Piker"
    altar <- S.printingOf s registry "Ashnod's Altar"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    let (gs, spellId, _, _) = denyingWindBoard island denyingWind piker altar forest mountain
        carolLib = Game.zoneMembers Zone.Library S.carol gs
        cast = snd (Engine.runGamePure aliceFindingNothing gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure aliceFindingNothing cast Engine.priorityLoop)
    Spec.assertEqWith s "nothing was exiled" (Game.zoneMembers Zone.Exile S.carol settled) []
    Spec.assertEqWith s "carol keeps every card, shuffled" (Set.fromList (Game.zoneMembers Zone.Library S.carol settled)) (Set.fromList carolLib)
  -- Jungle Wayfinder -- "{2}{G} Creature -- Elf Warrior 3/3. When this creature
  -- enters, each player may search their library for a basic land card, reveal
  -- it, put it into their hand, then shuffle." Oracle text verified against
  -- api.scryfall.com. The whole-card proof that a search's searcher and its
  -- library's owner can be COUPLED: rule 701.23a says only how to look, and
  -- "each player searches THEIR library" is the card's own sentence naming one
  -- instruction applied per player, so the two seats are the same on every pass.
  -- The Extract cases above separate the two readings from the other side --
  -- there the searcher and the owner are deliberately different players.
  --
  -- Three seats, so a cross-product engine is off by a factor of three rather
  -- than of two, and each library holds a DIFFERENT basic (alice Islands, bob
  -- Mountains, carol Plains) so "how many did she find" and "whose library did
  -- it come from" are separable assertions. None of the three is the Forest the
  -- {2}{G} is paid with, so a fetched card cannot be confused with one already
  -- in play. Each library also holds a Goblin Piker for the filter to reject.
  --
  -- THREE basics per library, not one, and that is what makes the case
  -- non-vacuous. A found card goes to its OWNER's hand (CR 400.3, through
  -- Event.changeZone), so on a one-basic board the cross product is invisible:
  -- alice's Island reaches alice's hand whether she found it or bob did, and the
  -- later passes over her library find nothing left to take. With three, the
  -- cross product empties each library into its owner's hand -- three cards per
  -- seat -- where the coupled reading takes exactly one.
  --
  -- The printed "may" is each seat's own (CR 603.5), which is why every seat is
  -- answered Exercises here; the case below is the one that declines.
  Spec.it s "CR 701.23a whole card: Jungle Wayfinder has each player search THEIR OWN library" $ do
    (gs, spellId) <- wayfinderBoard s registry
    let cast = snd (Engine.runGamePure findFirstExercising gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure findFirstExercising cast Engine.priorityLoop)
        nameOf = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith s "alice found ONE of her three Islands -- her library was searched once, by her" (namesIn Zone.Hand S.alice settled) [nameOf "Island"]
    Spec.assertEqWith s "bob one of his three Mountains" (namesIn Zone.Hand S.bob settled) [nameOf "Mountain"]
    Spec.assertEqWith s "carol one of her three Plains" (namesIn Zone.Hand S.carol settled) [nameOf "Plains"]
    -- What each library kept: the two basics nobody took and the Piker the
    -- filter rejected. A search that read a library it had no business in shows
    -- up here as a shortfall.
    Spec.assertEqWith s "alice's library kept the other two Islands and the Piker" (List.sort (namesIn Zone.Library S.alice settled)) (List.sort [nameOf "Island", nameOf "Island", nameOf "Goblin Piker"])
    Spec.assertEqWith s "bob's the other two Mountains" (List.sort (namesIn Zone.Library S.bob settled)) (List.sort [nameOf "Mountain", nameOf "Mountain", nameOf "Goblin Piker"])
    Spec.assertEqWith s "and carol's the other two Plains" (List.sort (namesIn Zone.Library S.carol settled)) (List.sort [nameOf "Plains", nameOf "Plains", nameOf "Goblin Piker"])
    Spec.assertEqWith s "the Wayfinder itself resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Jungle Wayfinder")) S.alice settled) 1
  -- The same card and the same board, with the "may" answered three ways: alice
  -- takes it and finds, bob DECLINES, carol takes it and finds nothing (CR
  -- 701.23b's fail-to-find, which a search stating a quality always allows).
  --
  -- The discriminating pair is bob against carol, and the quantity that tells
  -- them apart is LIBRARY ORDER. Hand size cannot: CR 701.23b lets carol find
  -- nothing, so both seats end with an empty hand. Library CONTENTS cannot
  -- either, for the same reason. Only the shuffle separates them -- a player who
  -- declines performs none of the sentence, so no shuffle -- and the answerer
  -- REVERSES whatever library it is offered, so a shuffle that happened is
  -- visible as the reversed order and one that did not as the original. That is
  -- the whole reason the third seat is on the board: at two seats there is no
  -- way to hold "declined" and "accepted, found nothing" apart.
  --
  -- Walked to the end: nothing later in the resolution touches bob's library --
  -- alice's find goes to its OWNER's hand (CR 400.3) and the Wayfinder's own
  -- move to the battlefield is alice's -- and no state-based action reorders a
  -- library, so the divergence survives to the assertion.
  --
  -- What this board does NOT prove is CR 608.2e's APNAP ORDER of the asks: alice
  -- is the active player, the controller and the head of the roster at once, so
  -- APNAP order, roster order and controller-first coincide. An enters trigger
  -- cannot be made to resolve while its controller is nonactive, so no cheap
  -- board separates them; the ordering rides apnapPlayersOf, which CR 118.12a's
  -- gate already exercises.
  Spec.it s "CR 603.5 whole card: Jungle Wayfinder's may is each seat's own, and a decliner's library is not shuffled" $ do
    (gs, spellId) <- wayfinderBoard s registry
    let cast = snd (Engine.runGamePure findFirstExercising gs (S.cast S.alice spellId))
        -- Resolve the creature spell, so what is left on the stack is the enters
        -- trigger alone and the asks below are its.
        onStack = snd (Engine.runGamePure findFirstExercising cast (Stack.resolveTop >> Engine.settleForPriority))
        ((_, after), asked) = State.runState (Engine.runGame wayfinderAnswer onStack Stack.resolveTop) []
        nameOf = Just . CardName.MkCardName . Text.pack
        may pid = (Text.pack "may", pid)
        search pid = (Text.pack "search", pid)
    Spec.assertEqWith s "CR 603.6a: the enters trigger, and nothing else, is on the stack" (length (GameState.stack onStack)) 1
    Spec.assertEqWith s "each library starts with three basics and a Piker" (fmap (\pid -> length (namesIn Zone.Library pid onStack)) [S.alice, S.bob, S.carol]) [4, 4, 4]
    Spec.assertEqWith
      s
      "CR 603.5: bob declined, so his library was never shuffled -- it is in its original order"
      (namesIn Zone.Library S.bob after)
      (namesIn Zone.Library S.bob onStack)
    Spec.assertEqWith
      s
      "carol took the may and found nothing (CR 701.23b), so her library WAS shuffled"
      (namesIn Zone.Library S.carol after)
      (reverse (namesIn Zone.Library S.carol onStack))
    Spec.assertEqWith s "alice took it and found one Island" (namesIn Zone.Hand S.alice after) [nameOf "Island"]
    Spec.assertEqWith s "bob's hand is empty" (namesIn Zone.Hand S.bob after) []
    Spec.assertEqWith s "and carol's" (namesIn Zone.Hand S.carol after) []
    Spec.assertEqWith
      s
      "every seat was asked the may, and only the two who took it were asked to search"
      asked
      [may S.alice, may S.bob, may S.carol, search S.alice, search S.carol]
  -- The same board differing in exactly one thing: EVERY seat declines. The
  -- shuffle answerer is still the reversing one, so a library that came back in
  -- its original order is one nothing shuffled -- and the whole sentence,
  -- shuffle included, is what nobody performed.
  Spec.it s "CR 603.5 whole card: nobody takes Jungle Wayfinder's may, so no library moves at all" $ do
    (gs, spellId) <- wayfinderBoard s registry
    let cast = snd (Engine.runGamePure findFirstExercising gs (S.cast S.alice spellId))
        onStack = snd (Engine.runGamePure findFirstExercising cast (Stack.resolveTop >> Engine.settleForPriority))
        ((_, after), asked) = State.runState (Engine.runGame decliningWayfinderAnswer onStack Stack.resolveTop) []
        libraries g = fmap (\pid -> namesIn Zone.Library pid g) [S.alice, S.bob, S.carol]
    Spec.assertEqWith s "every library is untouched, in its original order" (libraries after) (libraries onStack)
    Spec.assertEqWith s "and every hand is empty" (fmap (\pid -> namesIn Zone.Hand pid after) [S.alice, S.bob, S.carol]) [[], [], []]
    Spec.assertEqWith s "three asks and no search" asked [(Text.pack "may", S.alice), (Text.pack "may", S.bob), (Text.pack "may", S.carol)]
    Spec.assertEqWith s "the Wayfinder itself still resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Jungle Wayfinder")) S.alice after) 1
  Spec.it s "CR 603/608.2n Rest in Peace's ETB exiles graveyards and ceases" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    let g0 = Setup.emptyGame S.bothPlayers
        (ripId, g1) = S.addCreature restInPeace S.alice g0
        (deadId, g2) = S.addLibraryCard piker S.bob g1
        -- move the Piker into bob's graveyard
        g3 = S.runPure S.identityAnswer g2 (Event.changeZone deadId Zone.Graveyard)
        ability =
          TriggeredAbility.MkTriggeredAbility
            TriggerCondition.SelfEnters
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.ExileAllGraveyards]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            Nothing
            TriggerLimit.Unlimited
        (abilId, g4) = Game.freshObjectId g3
        (ts, g5) = Game.freshTimestamp g4
        abilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source =
                Source.OfTrigger
                  TriggeredAbilitySource.MkTriggeredAbilitySource
                    { TriggeredAbilitySource.source = ripId,
                      TriggeredAbilitySource.ability = ability
                    },
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapNext = False,
              Object.exertedBy = Set.empty
            }
        g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
        resolved = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
    Spec.assertEqWith s "bob's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 0
    Spec.assertEqWith s "ability ceased" (Game.lookupObject abilId resolved) Nothing
  Spec.it s "CR 103.5b ExileHandThenDraw exiles the whole hand, then draws that many" $ do
    mountain <- S.printingOf s registry "Mountain"
    swamp <- S.printingOf s registry "Swamp"
    let g0 = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addHandCard mountain S.alice g0
        (_, g2) = S.addHandCard swamp S.alice g1
        g3 = List.foldl' (\g _ -> snd (S.addLibraryCard mountain S.alice g)) g2 (replicate 5 ())
        after =
          S.runPure S.identityAnswer g3 $
            Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty Effect.ExileHandThenDraw
    Spec.assertEqWith s "the hand is refilled to the size it had" (S.handSize S.alice after) 2
    Spec.assertEqWith s "both old cards went to exile" (length (Game.zoneMembers Zone.Exile S.alice after)) 2
    Spec.assertEqWith s "and the library is two shorter" (length (Game.zoneMembers Zone.Library S.alice after)) 3
  Spec.it s "CR 723.1: Mindslaver's ability installs pending control, promoted next turn" $ do
    mindslaver <- S.printingOf s registry "Mindslaver"
    let g0 = Setup.emptyGame S.bothPlayers
        (srcId, g1) = S.addCreature mindslaver S.alice g0
        slot = SlotName.MkSlotName (Text.pack "target")
        ability =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost =
                Cost.Type.MkCost
                  { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                    Cost.Type.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
                  },
              ActivatedAbility.modal =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.ControlPlayerNextTurn slot]))) (Map.singleton slot (TargetSlot.required Pool.Players Nothing))))
                  (ModeSelection.ChooseExactly 1),
              ActivatedAbility.restrictions = [],
              ActivatedAbility.condition = Nothing,
              ActivatedAbility.name = Nothing
            }
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source =
                Source.OfAbility
                  ActivatedAbilitySource.MkActivatedAbilitySource
                    { ActivatedAbilitySource.source = srcId,
                      ActivatedAbilitySource.ability = ability
                    },
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer S.bob))) Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapNext = False,
              Object.exertedBy = Set.empty
            }
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = abilId : GameState.stack g3}
        resolved = snd (Engine.runGamePure S.identityAnswer g4 Stack.resolveTop)
        bobsTurn = snd (Engine.runGamePure S.identityAnswer resolved Engine.handoffTurn)
        afterBob = snd (Engine.runGamePure S.identityAnswer bobsTurn Engine.handoffTurn)
    Spec.assertEqWith s "control pending for bob" (Map.lookup S.bob (GameState.pendingControl resolved)) (Just (Decider.MkDecider S.alice))
    Spec.assertEqWith s "promoted on bob's turn" (GameState.activeControl bobsTurn) (Just (Decider.MkDecider S.alice))
    Spec.assertEqWith s "bob's decisions route to alice" (Decide.deciderFor S.bob bobsTurn) (Decider.MkDecider S.alice)
    Spec.assertEqWith s "control expired after bob's turn" (Decide.deciderFor S.bob afterBob) (Decider.MkDecider S.bob)
  Spec.it s "CR 723.1a: a second player-controlling effect overwrites the first (last created wins)" $ do
    mindslaver <- S.printingOf s registry "Mindslaver"
    let base = Setup.emptyGame S.bothPlayers
        -- First: alice controls bob.
        afterAlice = installControlBy mindslaver S.alice S.bob base
        -- Then: bob controls bob (CR 723.9 self-control), created LATER.
        afterBob = installControlBy mindslaver S.bob S.bob afterAlice
    Spec.assertEqWith s "the first effect installed alice as bob's decider" (Map.lookup S.bob (GameState.pendingControl afterAlice)) (Just (Decider.MkDecider S.alice))
    Spec.assertEqWith s "CR 723.1a: the later effect overwrites — bob's own control wins" (Map.lookup S.bob (GameState.pendingControl afterBob)) (Just (Decider.MkDecider S.bob))
  Spec.it s "CR 727.1a: resolving a RestartGame ability restarts with its controller as starting player" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        -- alice owns a card on the battlefield; it must survive the restart.
        -- aliceId only threads into the ability's Source.OfAbility below --
        -- CR 400.7 mints a fresh id for this card on the opening draw's zone
        -- change (Event.changeZone), so the post-restart check is ownership-
        -- based (SetupSpec's CR 727.2 test uses the same idiom), not a
        -- lookup by this specific pre-restart id.
        (aliceId, g1) = S.addCreature mountain S.alice g0
        -- bob owns 8 cards (enough for a full opening hand, no CR 727.3 loss).
        g2 = addMany mountain 8 S.bob g1
        g3 = addMany mountain 7 S.alice g2
        -- Hand-build bob's ability object on the stack: one mode, effect
        -- RestartGame, no targets. Object.owner = bob is the resolving
        -- controller (Resolve.hs), which restartGame uses as the starter.
        (abilId, g4) = Game.freshObjectId g3
        (ts, g5) = Game.freshTimestamp g4
        ability =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost =
                Cost.Type.MkCost
                  { Cost.Type.mana = Just (ManaCost.MkManaCost []),
                    Cost.Type.components = []
                  },
              ActivatedAbility.modal =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.RestartGame Nothing)))) Map.empty))
                  (ModeSelection.ChooseExactly 1),
              ActivatedAbility.restrictions = [],
              ActivatedAbility.condition = Nothing,
              ActivatedAbility.name = Nothing
            }
        abilObj =
          Object.MkObject
            { Object.owner = S.bob,
              Object.enteredUnder = Nothing,
              Object.source =
                Source.OfAbility
                  ActivatedAbilitySource.MkActivatedAbilitySource
                    { ActivatedAbilitySource.source = aliceId,
                      ActivatedAbilitySource.ability = ability
                    },
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.bob,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapNext = False,
              Object.exertedBy = Set.empty
            }
        g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
        after = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
    Spec.assertEqWith s "the game restarted with bob as the starting player (CR 727.1a)" (GameState.activePlayer after) S.bob
    Spec.assertEqWith s "alice's 8 cards all survived the restart, still hers (CR 727.2)" (length (filter (\o -> Object.owner o == S.alice) (Map.elems (GameState.objects after)))) 8
    Spec.assertEqWith s "the resolving ability object ceased to exist (not a card)" (Game.lookupObject abilId after) Nothing
  Spec.it s "CR 729.1b: PlaySubgame binds the winner, a later LoseLife reads it (mid-resolution binding visible)" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let g0 = Setup.emptyGame S.bothPlayers
        -- a stub runner: no real subgame, just report alice won.
        stubRunner :: Game Result.Result
        stubRunner = pure (Result.Won S.alice)
        (spellId, g1) = subgameSpellOn lightningBolt "Subgame Test Spell" nonWinnersLose3 g0
        after = snd (Engine.runGamePure S.identityAnswer g1 (Resolve.resolveSpellWith stubRunner spellId))
    Spec.assertEqWith s "bob, the one player who did not win, lost 3 to the follow-on" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "alice won, so the exclusion kept her out of the set" (S.lifeOf S.alice after) (Just 20)
  Spec.it s "CR 729.1b: a DRAWN subgame binds no winner, so the whole table is in the non-winner set" $ do
    -- The won board one line over with EXACTLY ONE thing changed -- the stub's
    -- Result -- so what the two cases differ by is who won and nothing else.
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let g0 = Setup.emptyGame S.bothPlayers
        stubRunner :: Game Result.Result
        stubRunner = pure Result.Drawn
        (spellId, g1) = subgameSpellOn lightningBolt "Subgame Test Spell" nonWinnersLose3 g0
        after = snd (Engine.runGamePure S.identityAnswer g1 (Resolve.resolveSpellWith stubRunner spellId))
    Spec.assertEqWith s "bob did not win a drawn subgame, so he pays" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "and neither did alice -- a draw punishes everybody, not nobody" (S.lifeOf S.alice after) (Just 17)
  Spec.it s "CR 729.1b: the non-winner set is the players still in the game, so a departed seat is not in it" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    -- bob departed the MAIN game before this effect resolves, so bob was never
    -- seated for the subgame (Setup.subgameStateFrom seats only
    -- Game.stillPlayingInOrder) -- only alice and carol played it. The stub
    -- reports alice won, so carol is the whole non-winner set; bob still appears
    -- in the raw seating roster (GameState.turnOrder) and is the non-participant
    -- a roster bug would wrongly charge.
    let g0 = S.departs Departure.Type.Conceded S.bob S.threePlayerGame
        stubRunner :: Game Result.Result
        stubRunner = pure (Result.Won S.alice)
        (spellId, g1) = subgameSpellOn lightningBolt "Subgame Test Spell (Three Seats, One Departed)" nonWinnersLose3 g0
        after = snd (Engine.runGamePure S.identityAnswer g1 (Resolve.resolveSpellWith stubRunner spellId))
    Spec.assertEqWith s "carol, a genuine subgame participant who did not win, lost 3" (S.lifeOf S.carol after) (Just 17)
    Spec.assertEqWith s "bob departed before the subgame and never played it, so he pays nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "alice won" (S.lifeOf S.alice after) (Just 20)
  Spec.it s "CR 111 Dragon Fodder creates two 1/1 Goblin tokens" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let base = S.landsInPlay mountain 2
        (gs, spellId) = S.handOne dragonFodder base
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    -- Two Goblin tokens exist (count == 2 proves two distinct objects). The
    -- battlefield also holds alice's 2 Mountains, so filter by name/creature.
    -- CR 111.4: Dragon Fodder does not name its tokens, so each is named
    -- "Goblin Token" -- its subtype plus the word "Token".
    Spec.assertEqWith s "two Goblin tokens on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Token") S.alice after) 2
    Spec.assertEqWith s "alice controls two creatures (the tokens)" (S.creaturesInPlay S.alice after) 2
    Spec.assertEqWith s "Dragon Fodder went to the graveyard (CR 608.2n)" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    -- The control leg for Hanweir Garrison's "tapped and attacking" riders
    -- (CombatSpec's PutOntoBattlefieldAttacking group): a Create that says
    -- neither takes CR 110.5b's default and joins no combat, so the riders
    -- are the effect's and not something every token gets.
    Spec.assertEqWith s "CR 110.5b: the Goblins enter untapped" (Maybe.mapMaybe (\oid -> fmap Object.tapped (Game.lookupObject oid after)) (S.tokensOf after)) [TapState.Untapped, TapState.Untapped]
    Spec.assertEqWith s "and attacking nothing" (Combat.Type.attackers (GameState.combat after)) Map.empty
  Spec.it s "CR 615 Fog prevents combat damage but not spell damage (the gate)" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    fog <- S.printingOf s registry "Fog"
    let base = S.landsInPlay forest 1
        (victim, gs0) = S.addCreature piker S.bob base
        (gs1, fogId) = S.handOne fog gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (S.cast S.alice fogId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        combat = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False False 0 Nothing DamageKind.Combat])
        spell = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False False 0 Nothing DamageKind.Noncombat])
    Spec.assertEqWith s "Fog installed one replacement" (length (GameState.replacements resolved)) 1
    Spec.assertEqWith s "combat damage prevented (the cancel shape)" (S.damageOf victim combat) (Just 0)
    -- The falsifier: a tag-blind Fog would also blunt this spell damage.
    Spec.assertEqWith s "spell damage untouched (Noncombat)" (S.damageOf victim spell) (Just 2)
  -- Sudden Impact: "deals damage to target player equal to the number of
  -- cards in THAT player's hand." Cast through the real path (Cast.castSpell
  -- + resolveTop), not S.spellOnStack -- that helper sets Object.bindings =
  -- Map.empty and so does not fill the target slot the InSlot count reads.
  Spec.it s "Sudden Impact reads the TARGET's hand, not the caster's" $ do
    -- THE FALSIFIER for a perspective baked into the count: Alice holds
    -- five and Bob holds two, and Bob takes two. A count whose "you" were
    -- the resolving controller (Alice) would deal five instead.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    suddenImpact <- S.printingOf s registry "Sudden Impact"
    let gs0 = S.landsInPlay mountain 4
        fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
        gs1 = fill S.alice 5 (fill S.bob 2 gs0)
        (spellId, gs2) = S.addHandCard suddenImpact S.alice gs1
        cast = snd (Engine.runGamePure atBobAnswer gs2 (S.cast S.alice spellId))
        before = S.lifeOf S.bob cast
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "two damage" (S.lifeOf S.bob after) (fmap (subtract 2) before)
  Spec.it s "CR 608.2h the number is read as the effect is applied, not as the spell is cast" $ do
    -- Bob's hand grows AFTER Sudden Impact is on the stack and BEFORE it
    -- resolves; the damage follows the hand size at resolution.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    suddenImpact <- S.printingOf s registry "Sudden Impact"
    let gs0 = S.landsInPlay mountain 4
        fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
        gs1 = fill S.bob 2 gs0
        (spellId, gs2) = S.addHandCard suddenImpact S.alice gs1
        cast = snd (Engine.runGamePure atBobAnswer gs2 (S.cast S.alice spellId))
        (_, cast1) = S.addHandCard piker S.bob cast
        before = S.lifeOf S.bob cast1
        after = snd (Engine.runGamePure atBobAnswer cast1 Stack.resolveTop)
    Spec.assertEqWith s "three damage" (S.lifeOf S.bob after) (fmap (subtract 3) before)
  Spec.it s "the same count with Relative You reads the caster's hand" $ do
    -- The direct contrast: the SAME Count shape (InZone Hand, Members) that
    -- Sudden Impact scopes with PlayerRef.InSlot also serves Inner Calm,
    -- Outer Strength's PlayerRef.Relative You -- one shape, two
    -- perspectives, neither welded into a constructor.
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
        gs = fill S.alice 5 (fill S.bob 2 gs0)
        yourHand =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Hand (PlayerRef.Relative PlayerRelation.You)))
            (Filter.Type.And [])
            Aggregation.Members
    Spec.assertEqWith
      s
      "Alice's five"
      (S.countOf (\oid -> Just (Projection.viewOfObject oid gs)) (Filter.contextFor (Just S.alice) Nothing) gs yourHand)
      (Just 5)
  -- CR 205.4g, end to end: "any permanent with the supertype 'snow' is a
  -- snow permanent." Skred deals damage equal to the number of snow
  -- permanents YOU control, cast through the real path (Cast.castSpell +
  -- resolveTop) so the count is read at resolution off a real projection.
  --
  -- THE FALSIFIER, in both directions at once, which is why the board is
  -- lopsided. Alice has two Snow-Covered Mountains and two plain Mountains;
  -- Bob has one Snow-Covered Mountain and the Wall of Stone that takes the
  -- damage. The right answer is 2. A count blind to the supertype would see
  -- four permanents Alice controls and deal 4; a count blind to CR 109.5's
  -- controller would see three snow permanents and deal 3. All three numbers
  -- differ, so no single wrong reading can pass.
  --
  -- Wall of Stone is 0/8, so it survives and carries the damage as a mark
  -- (CR 120.3e, removed at CR 514.2's cleanup) that the assertion can read
  -- exactly -- a dead creature would only tell us the damage was at least
  -- its toughness.
  Spec.it s "CR 205.4g Skred counts the snow permanents YOU control, and nothing else" $ do
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    mountain <- S.printingOf s registry "Mountain"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    skred <- S.printingOf s registry "Skred"
    let gs0 = S.landsInPlay snowMountain 2
        gs1 = snd (S.addCreature mountain S.alice (snd (S.addCreature mountain S.alice gs0)))
        gs2 = snd (S.addCreature snowMountain S.bob gs1)
        (wall, gs3) = S.addCreature wallOfStone S.bob gs2
        (spellId, gs4) = S.addHandCard skred S.alice gs3
        cast = snd (Engine.runGamePure (atCreature wall) gs4 (S.cast S.alice spellId))
        after = snd (Engine.runGamePure (atCreature wall) cast Stack.resolveTop)
    Spec.assertEqWith s "no damage before it resolves" (S.damageOf wall cast) (Just 0)
    Spec.assertEqWith s "two snow permanents you control, so two damage" (S.damageOf wall after) (Just 2)
  -- CR 608.2h: the answer "is determined only once, when the effect is
  -- applied", so a quantity Projection.freezeQuantities cannot evaluate at
  -- that one moment has no later moment to be evaluated in. Storing the raw
  -- quantity would hand it to applyModification, which reads it against the
  -- AFFECTED object on every projection -- a wrong answer, not a deferred
  -- one. Nothing is stored instead, which is the posture CR 611.2b already
  -- gives this opcode when the duration never starts.
  --
  -- A bare Star is the unevaluable quantity here (CR 208.2: it has no value
  -- of its own); the literal leg is the control that keeps the empty result
  -- from passing vacuously.
  Spec.it s "CR 608.2h a modification that cannot be frozen is not stored at all" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        slot = SlotName.MkSlotName (Text.pack "target")
        store m =
          S.runPure S.identityAnswer gs $
            Resolve.applyEffect
              S.noSource
              S.noSource
              S.alice
              (Map.singleton slot (Set.singleton (Recipient.ToCreature pikerId)))
              (Map.singleton slot (Set.singleton (Recipient.ToCreature pikerId)))
              (Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.UntilEndOfTurn m (ObjectRef.InSlot slot)))
        refused = store (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) Quantity.Star))
        stored = store (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)))
    Spec.assertEqWith s "no effect is stored for an unevaluable quantity" (GameState.continuousEffects refused) []
    Spec.assertEqWith s "and the Piker is its printed 2/1" (Projection.powerOf pikerId refused, Projection.toughnessOf pikerId refused) (Just 2, Just 1)
    Spec.assertEqWith s "the same call with two Literals DOES store one -- the refusal is what did it" (length (GameState.continuousEffects stored)) 1
    Spec.assertEqWith s "and pumps the Piker to 5/4" (Projection.powerOf pikerId stored, Projection.toughnessOf pikerId stored) (Just 5, Just 4)

-- Add n Mountains to pid's battlefield, discarding the ids (used to bulk up a
-- pool of owned cards). replicate n () avoids a list comprehension (CLAUDE.md).
addMany :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addMany mountain n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature mountain pid g)) gs (replicate n ())

-- Build a Mindslaver-shaped ControlPlayerNextTurn ability owned by `controller`,
-- targeting `target`, put it on the stack, and resolve it. Returns the resulting
-- state. Object.owner is the resolving ability's controller (Resolve.hs), so this
-- installs pendingControl[target] = MkDecider controller.
installControlBy :: Printing.Printing -> PlayerId.PlayerId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
installControlBy mindslaver controller target gs0 =
  let (srcId, gs1) = S.addCreature mindslaver controller gs0
      slot = SlotName.MkSlotName (Text.pack "target")
      ability =
        ActivatedAbility.MkActivatedAbility
          { ActivatedAbility.cost =
              Cost.Type.MkCost
                { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                  Cost.Type.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
                },
            ActivatedAbility.modal =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.ControlPlayerNextTurn slot]))) (Map.singleton slot (TargetSlot.required Pool.Players Nothing))))
                (ModeSelection.ChooseExactly 1),
            ActivatedAbility.restrictions = [],
            ActivatedAbility.condition = Nothing,
            ActivatedAbility.name = Nothing
          }
      (abilId, gs2) = Game.freshObjectId gs1
      (ts, gs3) = Game.freshTimestamp gs2
      abilObj =
        Object.MkObject
          { Object.owner = controller,
            Object.enteredUnder = Nothing,
            Object.source =
              Source.OfAbility
                ActivatedAbilitySource.MkActivatedAbilitySource
                  { ActivatedAbilitySource.source = srcId,
                    ActivatedAbilitySource.ability = ability
                  },
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled controller,
            Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer target))) Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
      gs4 = gs3 {GameState.objects = Map.insert abilId abilObj (GameState.objects gs3), GameState.stack = abilId : GameState.stack gs3}
   in snd (Engine.runGamePure S.identityAnswer gs4 Stack.resolveTop)

-- CR 205.4c / 701.23a: a basic land card is one with the Land card type and the
-- Basic supertype -- Evolving Wilds' search filter, the printed-card predicate
-- that replaced CardCriterion.BasicLandCard.
basicLandFilter :: Filter.Type.Filter Keyword.Keyword
basicLandFilter =
  Filter.Type.And
    [ Filter.Type.HasCardType CardType.Land,
      Filter.Type.HasSupertype Supertype.Basic
    ]

-- Explosive Vegetation's board, built once and shared by its three cases so
-- they differ in the ANSWER alone. Four Forests pay the {3}{G} -- all of them,
-- which is what makes "nothing untapped" an assertion about the fetch -- and the
-- library holds three DIFFERENT basic lands against a cap of two, plus a nonland
-- for the filter to reject.
data VegetationBoard = MkVegetationBoard
  { vegetationState :: GameState.GameState,
    vegetationSpell :: ObjectId.ObjectId,
    vegetationMountain :: ObjectId.ObjectId,
    vegetationIsland :: ObjectId.ObjectId,
    vegetationPlains :: ObjectId.ObjectId,
    vegetationPiker :: ObjectId.ObjectId
  }

vegetationBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m VegetationBoard
vegetationBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  vegetation <- S.printingOf s registry "Explosive Vegetation"
  let (mountainId, g1) = S.addLibraryCard mountain S.alice (S.landsInPlay forest 4)
      (islandId, g2) = S.addLibraryCard island S.alice g1
      (plainsId, g3) = S.addLibraryCard plains S.alice g2
      (pikerId, g4) = S.addLibraryCard piker S.alice g3
      (gs, spellId) = S.handOne vegetation g4
  pure (MkVegetationBoard gs spellId mountainId islandId plainsId pikerId)

resolveVegetation :: (forall r. Prompt.Prompt r -> r) -> VegetationBoard -> GameState.GameState
resolveVegetation answer board =
  let cast = snd (Engine.runGamePure answer (vegetationState board) (S.cast S.alice (vegetationSpell board)))
   in snd (Engine.runGamePure answer cast Engine.priorityLoop)

-- Mana Severance's board. Two Islands pay the {1}{U}, and the library holds the
-- land cards NAMED -- all different, and none of them an Island -- against a
-- search that states no count, plus a nonland for the filter to reject.
--
-- Parameterised by the lands rather than fixed, because a board of one size
-- cannot tell an unbounded search from a literal that happens to equal it: the
-- four-land cases below and the five-land case together admit no literal.
data SeveranceBoard = MkSeveranceBoard
  { severanceState :: GameState.GameState,
    severanceSpell :: ObjectId.ObjectId,
    -- | The library's land cards, in the order they were named.
    severanceLands :: [ObjectId.ObjectId],
    severancePiker :: ObjectId.ObjectId
  }

severanceBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [String] -> m SeveranceBoard
severanceBoard s registry names = do
  island <- S.printingOf s registry "Island"
  lands <- mapM (S.printingOf s registry) names
  piker <- S.printingOf s registry "Goblin Piker"
  severance <- S.printingOf s registry "Mana Severance"
  let place (ids, g) printing = let (oid, g') = S.addLibraryCard printing S.alice g in (ids <> [oid], g')
      (landIds, g1) = List.foldl' place ([], S.landsInPlay island 2) lands
      (pikerId, g2) = S.addLibraryCard piker S.alice g1
      (gs, spellId) = S.handOne severance g2
  pure (MkSeveranceBoard gs spellId landIds pikerId)

-- The four the three shared cases use.
severanceFour :: [String]
severanceFour = ["Mountain", "Swamp", "Plains", "Forest"]

resolveSeverance :: (forall r. Prompt.Prompt r -> r) -> SeveranceBoard -> GameState.GameState
resolveSeverance answer board =
  let cast = snd (Engine.runGamePure answer (severanceState board) (S.cast S.alice (severanceSpell board)))
   in snd (Engine.runGamePure answer cast Engine.priorityLoop)

-- Finds exactly the cards named and nothing else, whatever the engine offers.
-- PINNED rather than picked out of the candidate list: an answerer that went
-- looking for a legal choice would find one again after a mutation, and the
-- assertion would stay green while the engine's own count was broken.
findPinned :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
findPinned wanted p = case p of
  Prompt.SearchLibrary {} -> wanted
  _ -> S.identityAnswer p

untappedOf :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
untappedOf pid gs =
  let isUntapped oid = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
   in filter isUntapped (Game.zoneMembers Zone.Battlefield pid gs)

-- Finds as many as the search allows, taking them off the head of the offered
-- list -- one card for the searches that ask for one.
findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches cap -> List.genericTake cap matches
  _ -> S.identityAnswer p

-- Names a card the search filter did NOT admit -- the lying interpreter #222 is
-- about. Parameterised so the test can point it at a specific nonland.
findForbidden :: ObjectId.ObjectId -> Prompt.Prompt r -> r
findForbidden wanted p = case p of
  Prompt.SearchLibrary {} -> [wanted]
  _ -> S.identityAnswer p

-- findFirst, plus CR 603.5's printed "may" taken. The pair below it declines the
-- same "may" and answers every other prompt identically, so a board run through
-- both differs in exactly that one decision.
findFirstExercising :: Prompt.Prompt r -> r
findFirstExercising p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> findFirst p

findFirstDeclining :: Prompt.Prompt r -> r
findFirstDeclining p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  _ -> findFirst p

-- The board the two Jungle Wayfinder cases share: alice casts it off three
-- Forests, and every seat's library holds three of ONE basic plus a Goblin Piker
-- for the filter to reject. See the first case for why three seats, three basics
-- each, and a different basic per seat.
wayfinderBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
wayfinderBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  wayfinder <- S.printingOf s registry "Jungle Wayfinder"
  island <- S.printingOf s registry "Island"
  mountain <- S.printingOf s registry "Mountain"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  let stock printing pid g = List.foldl' (\h _ -> snd (S.addLibraryCard printing pid h)) g [1 :: Int .. 3]
      g0 = S.landsFor forest S.alice 3 S.threePlayerGame
      (_, g1) = S.addLibraryCard piker S.alice g0
      g2 = stock island S.alice g1
      (_, g3) = S.addLibraryCard piker S.bob g2
      g4 = stock mountain S.bob g3
      (_, g5) = S.addLibraryCard piker S.carol g4
      g6 = stock plains S.carol g5
  pure (S.handOne wayfinder g6)

-- Three different answers to one clause's "may", plus a log of WHO was asked
-- what -- a pure Prompt -> r answerer could report the decisions but not the
-- seats they were put to, and "bob was never asked to search" is half of what
-- the case proves.
--
-- The shuffle is answered by REVERSING whatever library it is offered, which is
-- what makes a shuffle that happened visible at all (Game.honourShuffle honours
-- any permutation). Everything else falls through to S.identityAnswer, which
-- DECLINES a CR 603.5 may -- so the accepting arm is written out rather than
-- left to it.
wayfinderAnswer :: Prompt.Prompt r -> State.State [(Text.Text, PlayerId.PlayerId)] r
wayfinderAnswer p = case p of
  Prompt.ChooseOptional _ pid _ _ _ -> do
    State.modify' (<> [(Text.pack "may", pid)])
    pure (if pid == S.bob then OptionalDecision.Declines else OptionalDecision.Exercises)
  -- CR 701.23b's fail-to-find for carol; the filter states a quality, so an
  -- empty answer stands rather than being completed.
  Prompt.SearchLibrary _ pid matches cap -> do
    State.modify' (<> [(Text.pack "search", pid)])
    pure (if pid == S.carol then [] else List.genericTake cap matches)
  Prompt.Shuffle offered -> pure (reverse offered)
  _ -> pure (S.identityAnswer p)

-- wayfinderAnswer with every seat declining, and the SAME reversing shuffle, so
-- a board run through both differs in exactly the decisions.
decliningWayfinderAnswer :: Prompt.Prompt r -> State.State [(Text.Text, PlayerId.PlayerId)] r
decliningWayfinderAnswer p = case p of
  Prompt.ChooseOptional _ pid _ _ _ -> do
    State.modify' (<> [(Text.pack "may", pid)])
    pure OptionalDecision.Declines
  _ -> wayfinderAnswer p

-- findFirstExercising with the FIND pinned to one named card. The two Dragons of
-- the CR 607.2a pair have to exile DIFFERENT artifacts for the linked set to be
-- provable at all, and the head of the offered list is where a shuffle left it
-- rather than something the fixture chose.
findPinnedExercising :: ObjectId.ObjectId -> Prompt.Prompt r -> r
findPinnedExercising wanted p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.SearchLibrary {} -> [wanted]
  _ -> S.identityAnswer p

-- The board CR 607.2a's two cases share: two Hoarding Dragons of alice's, each
-- having exiled a different artifact, plus what an assertion needs to tell the
-- two halves apart.
data TwoDragons = MkTwoDragons
  { dragonBoard :: GameState.GameState,
    firstDragon :: ObjectId.ObjectId,
    secondDragon :: ObjectId.ObjectId,
    firstArtifact :: CardName.CardName,
    secondArtifact :: CardName.CardName
  }

-- Cast one spell and settle, so the entry trigger has resolved by the time the
-- next Dragon is cast.
settleCast :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
settleCast answer spellId gs =
  let cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
   in snd (Engine.runGamePure answer cast Engine.priorityLoop)

dragonsOf :: GameState.GameState -> [ObjectId.ObjectId]
dragonsOf gs =
  filter
    (\oid -> S.soleFaceName oid gs == CardName.MkCardName (Text.pack "Hoarding Dragon"))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

-- The Dragons are cast one at a time so that the SECOND one's id is the
-- battlefield Dragon the first cast did not leave behind. Every claim the board
-- makes is asserted here rather than assumed, since both cases below read the
-- board's own answer back: an exile that held one card, or a hand that already
-- held one, would make them pass for the wrong reason.
twoDragonBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m TwoDragons
twoDragonBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  dragon <- S.printingOf s registry "Hoarding Dragon"
  altar <- S.printingOf s registry "Ashnod's Altar"
  sphere <- S.printingOf s registry "Chromatic Sphere"
  piker <- S.printingOf s registry "Goblin Piker"
  let base0 = S.landsInPlay mountain 10
      (altarId, base1) = S.addLibraryCard altar S.alice base0
      (sphereId, base2) = S.addLibraryCard sphere S.alice base1
      (_, base3) = S.addLibraryCard piker S.alice base2
      (base4, firstSpell) = S.handOne dragon base3
      (secondSpell, base5) = S.addHandCard dragon S.alice base4
      afterFirst = settleCast (findPinnedExercising altarId) firstSpell base5
      afterSecond = settleCast (findPinnedExercising sphereId) secondSpell afterFirst
      earlier = dragonsOf afterFirst
      later = filter (`notElem` earlier) (dragonsOf afterSecond)
  Spec.assertEqWith s "the first Dragon is alone on the battlefield" (length earlier) 1
  Spec.assertEqWith s "the second Dragon joined it" (length later) 1
  Spec.assertEqWith s "her hand is empty, so anything in it later was returned" (S.handSize S.alice afterSecond) 0
  Spec.assertEqWith
    s
    "each Dragon exiled a different artifact"
    (Set.fromList (exileNames afterSecond))
    (Set.fromList [S.printingName altar, S.printingName sphere])
  pure
    MkTwoDragons
      { dragonBoard = afterSecond,
        firstDragon = Maybe.fromMaybe S.noSource (Maybe.listToMaybe earlier),
        secondDragon = Maybe.fromMaybe S.noSource (Maybe.listToMaybe later),
        firstArtifact = S.printingName altar,
        secondArtifact = S.printingName sphere
      }

-- CR 704.5g: lethal damage on a 4/4, swept by the state-based actions the
-- priority loop runs before it hands anybody priority, which is what fires the
-- dies trigger and resolves it.
kill :: ObjectId.ObjectId -> TwoDragons -> GameState.GameState
kill oid board =
  snd (Engine.runGamePure findFirstExercising (S.markDamage oid 4 (dragonBoard board)) Engine.priorityLoop)

handNames :: GameState.GameState -> [CardName.CardName]
handNames gs = fmap (`S.soleFaceName` gs) (Game.zoneMembers Zone.Hand S.alice gs)

exileNames :: GameState.GameState -> [CardName.CardName]
exileNames gs = fmap (`S.soleFaceName` gs) (Game.zoneMembers Zone.Exile S.alice gs)

findNothing :: Prompt.Prompt r -> r
findNothing p = case p of
  Prompt.SearchLibrary {} -> []
  _ -> S.identityAnswer p

-- Fertilid's Favor's answerer, in three parts. CR 601.2c is announced at its
-- FLOOR, so the Favor's "up to one target artifact or creature" takes no target
-- at all and the searching player is the only slot left to fill; what remains is
-- aimed at carol wherever she is offered (line 4536's idiom, a preference rather
-- than a filter, so a slot she is no candidate for still gets a legal answer).
--
-- The find is PINNED to carol: an engine that asked the spell's controller to
-- search instead finds nothing at all, rather than helpfully finding a card in
-- whichever library it was handed.
atCarolFinding :: Prompt.Prompt r -> r
atCarolFinding p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (TargetCount.least . fst) offers
  Prompt.ChooseTargets _ _ _ sets ->
    fmap
      (\(n, legal) -> Set.fromList (take (Natural.toIntSaturating n) (List.nub (filter (== Recipient.ToPlayer S.carol) (Set.toAscList legal) <> Set.toAscList legal))))
      sets
  Prompt.SearchLibrary _ pid matches cap ->
    if pid == S.carol
      then List.genericTake cap matches
      else []
  _ -> S.identityAnswer p

-- Extract's board, built once so the three cases below differ in exactly one
-- thing: the answer. Three seats, because two collapse "target player" onto "the
-- one opponent" and the whole point of the card is that the searcher and the
-- library's owner are different players.
--
-- Every library is stocked, and alice's with TWO cards, so "which library was
-- read" and "which library was shuffled" are both observable: a single-card
-- library cannot show a reordering. The printings, in argument order: the Island
-- alice taps, the Extract in her hand, the Goblin Piker she has two of, bob's
-- Ashnod's Altar, then carol's Mountain, Forest and Plains -- added in that
-- order, so Support.addLibraryCard's prepending leaves the Plains at the head and
-- the Forest in the middle. The first case below asserts that order before
-- casting anything, since the pins rest on it.
extractBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], ObjectId.ObjectId, [ObjectId.ObjectId])
extractBoard island extract piker altar mountain forest plains =
  let g0 = S.landsFor island S.alice 1 S.threePlayerGame
      (aliceOne, g1) = S.addLibraryCard piker S.alice g0
      (aliceTwo, g2) = S.addLibraryCard piker S.alice g1
      (bobCard, g3) = S.addLibraryCard altar S.bob g2
      (carolMountain, g4) = S.addLibraryCard mountain S.carol g3
      (carolForest, g5) = S.addLibraryCard forest S.carol g4
      (carolPlains, g6) = S.addLibraryCard plains S.carol g5
      (gs, spellId) = S.handOne extract g6
   in (gs, spellId, [aliceTwo, aliceOne], bobCard, [carolPlains, carolForest, carolMountain])

-- Extract's answerers, sharing one targeting half with atCarolFinding above:
-- carol is preferred wherever a target is offered, so the spell's one slot names
-- her. They differ ONLY in what the search is answered and how the shuffle is,
-- so the three boards below are one board with one variable each.
--
-- The find is pinned to a card id AND to the asking seat. Asking carol -- the
-- library's owner rather than the spell's controller -- gets an empty answer,
-- which under CR 701.23d is not "nothing happens" but "the head is completed in",
-- so the pinned card is deliberately NOT the head.
atCarolTargeted :: Prompt.Prompt r -> r
atCarolTargeted p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (TargetCount.least . fst) offers
  Prompt.ChooseTargets _ _ _ sets ->
    fmap
      (\(n, legal) -> Set.fromList (take (Natural.toIntSaturating n) (List.nub (filter (== Recipient.ToPlayer S.carol) (Set.toAscList legal) <> Set.toAscList legal))))
      sets
  _ -> S.identityAnswer p

aliceFinding :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aliceFinding wanted p = case p of
  Prompt.SearchLibrary _ pid _ _ -> if pid == S.alice then [wanted] else []
  _ -> atCarolTargeted p

aliceFindingNothing :: Prompt.Prompt r -> r
aliceFindingNothing p = case p of
  Prompt.SearchLibrary {} -> []
  _ -> atCarolTargeted p

-- Denying Wind's board. Extract's, with the mana its {7}{U}{U} needs and a
-- library big enough that "up to seven" is a real ceiling: nine cards, so the
-- cap, the answer and the library size are all different numbers. Alice's own
-- library is stocked too, so a search that read the wrong seat's would find
-- something rather than nothing and the difference would be visible.
-- Seven of carol's nine are Goblin Pikers and the other two are a Forest and a
-- Mountain, which is what makes the find READABLE: an exiled card is a fresh
-- incarnation with a fresh id (CR 400.7), so the two named cards can only be
-- identified by name, and the seven the engine would complete with under CR
-- 701.23d are named something else.
denyingWindBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId])
denyingWindBoard island denyingWind piker altar forest mountain =
  let g0 = S.landsFor island S.alice 9 S.threePlayerGame
      (_, g1) = S.addLibraryCard piker S.alice g0
      (bobCard, g2) = S.addLibraryCard altar S.bob g1
      g3 = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.carol g)) g2 [1 :: Int .. 7]
      (carolForest, g4) = S.addLibraryCard forest S.carol g3
      (carolMountain, g5) = S.addLibraryCard mountain S.carol g4
      (gs, spellId) = S.handOne denyingWind g5
   in (gs, spellId, bobCard, [carolForest, carolMountain])

-- aliceFinding for a several-card find: the search is answered with exactly the
-- ids named, which is how an answer SHORTER than the cap gets in front of the
-- engine. Pinned by id rather than chosen from what is offered, so a mutation
-- cannot repair the answer by finding a different legal one.
aliceFindingThese :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
aliceFindingThese wanted p = case p of
  Prompt.SearchLibrary _ pid _ _ -> if pid == S.alice then wanted else []
  _ -> atCarolTargeted p

-- aliceFinding, plus a shuffle that REVERSES the library it is offered. Game
-- .honourShuffle accepts any permutation of what was offered, so the reversal is
-- honoured and names which library the shuffle read and wrote.
aliceFindingReversing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aliceFindingReversing wanted p = case p of
  Prompt.Shuffle offered -> reverse offered
  _ -> aliceFinding wanted p

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Answers ChooseTargets by pointing every slot at bob (the opponent), otherwise
-- behaves like identityAnswer. Used to aim a player-targeting spell at bob.
atBobAnswer :: Prompt.Prompt r -> r
atBobAnswer p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

-- atBobAnswer's creature counterpart: aim every target slot at one named
-- creature, rather than at whatever Set.lookupMin happens to offer first.
atCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
atCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- CR 729.1b's plumbing, as a card face would write it: run the subgame, then make
-- every player who did not win lose 3. A flat 3 rather than Shahrazad's half-life
-- rider because what these three cases pin is WHO is in the set, and a per-player
-- amount would let a wrong set and a wrong amount cancel out.
nonWinnersLose3 :: [Effect.Effect Card.Type.Card]
nonWinnersLose3 =
  let slot = SlotName.MkSlotName (Text.pack "winner")
   in [ Effect.PlaySubgame slot,
        Effect.LoseLife (PlayerQuantity.MkPlayerQuantity (PlayerRef.EachPlayerExcept slot) (Quantity.Literal 3))
      ]

-- A hand-built {0} sorcery of alice's on the stack, one chosen mode holding
-- `effects` and no target slots. The NARROWEST path to the Effect.PlaySubgame arm:
-- Resolve.resolveSpellWith takes the subgame runner as an argument, so a stub
-- Result decides the outcome outright and no nested game runs -- which is what
-- lets a test name a DRAWN subgame at all. `borrowed` supplies the type line, so
-- the object is a spell like any other.
subgameSpellOn :: Printing.Printing -> String -> [Effect.Effect Card.Type.Card] -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
subgameSpellOn borrowed name effects gs0 =
  let (spellPrintingId, gs0b) = Game.intern (Printing.MkPrinting card) gs0
      (spellId, gs1) = Game.freshObjectId gs0b
      (ts, gs2) = Game.freshTimestamp gs1
      card = Card.Type.MkCard {Card.Type.layout = Layout.Normal, Card.Type.faces = NonEmpty.singleton face}
      face =
        Face.MkFace
          { Face.name = CardName.MkCardName $ Text.pack name,
            Face.manaCost = Nothing,
            Face.typeLine = Face.typeLine (S.combinedFace borrowed),
            Face.power = Nothing,
            Face.toughness = Nothing,
            Face.loyalty = Nothing,
            Face.defense = Nothing,
            Face.keywords = Set.empty,
            Face.colorIndicator = Set.empty,
            Face.staticAbilities = [],
            Face.spell =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) Map.empty))
                (ModeSelection.ChooseExactly 1),
            Face.activatedAbilities = [],
            Face.replacementEffects = [],
            Face.triggeredAbilities = [],
            Face.delayedAbilities = Map.empty,
            Face.rooms = Seq.empty,
            Face.castingPermissions = [],
            Face.castingRestrictions = [],
            Face.characteristicPT = Nothing,
            Face.playerAbilities = [],
            Face.blockRequirements = [],
            Face.blockPermissions = [],
            Face.attackRequirements = [],
            Face.combatRestrictions = [],
            Face.sacrificeRestrictions = [],
            Face.untapRestrictions = [],
            Face.attachRestrictions = [],
            Face.entryRestrictions = [],
            Face.attackCosts = [],
            Face.blockCosts = [],
            Face.mulliganActions = [],
            Face.openingHandActions = [],
            Face.specialActions = [],
            Face.additionalCosts = [],
            Face.maximumX = Nothing,
            Face.alternativeCosts = [],
            Face.costReductions = [],
            Face.enchant = [],
            Face.counterability = Counterability.Counterable
          }
      spellObj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfToken spellPrintingId,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.alice,
            Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in (spellId, gs2 {GameState.objects = Map.insert spellId spellObj (GameState.objects gs2), GameState.stack = spellId : GameState.stack gs2})

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  targetSpec s registry
  resolveSpec s registry
