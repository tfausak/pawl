{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EffectSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SourceRelation as SourceRelation
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- | The `card` parameter is instantiated at 'Text.Text' throughout (and at
-- 'Int' in the parametricity case). 'Effect.toJson'/'Effect.fromJson' reach it
-- only through the supplied codec, so any type proves the shape.
cardToJson :: Text.Text -> Value.Value
cardToJson = Common.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: Effect.Effect Text.Text -> Value.Value
toJson = Effect.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (Effect.Effect Text.Text)
fromJson = Effect.fromJson cardFromJson

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Effect" $ do
  -- ObjectRef is untagged, so both arms have to survive.
  Spec.it s "DealDamage round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3))
      """ {"type":"DealDamage","value":["target",{"type":"Literal","value":3}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (ObjectRef.EachMatching (Filter.HasKeyword Keyword.Flying)) (Quantity.Literal 1))
      """ {"type":"DealDamage","value":[{"type":"HasKeyword","value":{"type":"Flying"}},{"type":"Literal","value":1}]} """
  -- ModifyTarget's ObjectRef is untagged, so both arms have to survive.
  Spec.it s "ModifyTarget round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))))
      """ {"type":"ModifyTarget","value":[{"type":"UntilEndOfTurn"},{"type":"GainKeyword","value":{"type":"Trample"}},"t"]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.EachMatching (Filter.And [Filter.HasCardType CardType.Creature, Filter.IsAttacking])))
      """ {"type":"ModifyTarget","value":[{"type":"UntilEndOfTurn"},{"type":"GainKeyword","value":{"type":"Trample"}},{"type":"And","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"IsAttacking"}]}]} """
  Spec.it s "ChangeText, a basic land type swap that forbids nothing" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChangeText SubtypeFamily.BasicLandType Set.empty (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"ChangeText","value":[{"type":"BasicLandType"},[],"target"]} """
  Spec.it s "ChangeText, a creature type swap whose new word can't be Wall" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChangeText SubtypeFamily.CreatureType (Set.singleton Subtype.Wall) (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"ChangeText","value":[{"type":"CreatureType"},[{"type":"Wall"}],"target"]} """
  Spec.it s "AddMana, a fixed type and any color" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green)))
      """ {"type":"AddMana","value":{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddMana ManaProduction.AnyColor)
      """ {"type":"AddMana","value":{"type":"AnyColor"}} """
  Spec.it s "Search" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Search (Filter.HasCardType CardType.Land) SearchDestination.BattlefieldTapped)
      """ {"type":"Search","value":[{"type":"HasCardType","value":{"type":"Land"}},{"type":"BattlefieldTapped"}]} """
  Spec.it s "ExileAllGraveyards" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.ExileAllGraveyards
      """ {"type":"ExileAllGraveyards"} """
  Spec.it s "RestartGame" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.RestartGame
      """ {"type":"RestartGame"} """
  Spec.it s "ControlPlayerNextTurn" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ControlPlayerNextTurn (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"ControlPlayerNextTurn","value":"target"} """
  -- Both ObjectRef arms, plus the two shapes CR 701.19c's regeneration rider
  -- takes. The two-element literal pins the elided (Nothing) arm of the
  -- bound-count slot below.
  Spec.it s "Destroy carries its CR 701.19c rider both ways" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.Regenerable Nothing)
      """ {"type":"Destroy","value":["t",{"type":"Regenerable"}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.CantBeRegenerated Nothing)
      """ {"type":"Destroy","value":["t",{"type":"CantBeRegenerated"}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing)
      """ {"type":"Destroy","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"Regenerable"}]} """
  -- The third element is the slot the sweep binds its count into, ELIDED when
  -- absent, so a Destroy already on disk keeps its two-element payload.
  Spec.it s "Destroy's bound-count slot round-trips and is written only when present" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Artifact)) Regenerability.Regenerable (Just (SlotName.MkSlotName (Text.pack "destroyed"))))
      """ {"type":"Destroy","value":[{"type":"HasCardType","value":{"type":"Artifact"}},{"type":"Regenerable"},"destroyed"]} """
  Spec.it s "Sacrifice" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self")))
      """ {"type":"Sacrifice","value":"self"} """
  Spec.it s "Attach" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Attach (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"Attach","value":"target"} """
  -- CR 701.3: the destination Filter travels in the payload, distinguishing
  -- this arm's wire format from Attach's bare slot above.
  Spec.it s "AttachTarget" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AttachTarget (SlotName.MkSlotName (Text.pack "target")) (Filter.HasCardType CardType.Creature))
      """ {"type":"AttachTarget","value":["target",{"type":"HasCardType","value":{"type":"Creature"}}]} """
  -- MoveToZone's payload is the slot and the destination zone, then three
  -- independently elided extras -- the EntryRiders, the bound slot and CR
  -- 113.6m's origin zone -- so it is told apart by JSON TYPE alone, at every
  -- length. A string is the bound slot; an object is the origin zone if it
  -- decodes as a zone and the riders otherwise, which is why the last two cases
  -- put a zone and a riders object side by side.
  Spec.it s "MoveToZone round-trips every shape, and elides the defaults" $ do
    let slot = SlotName.MkSlotName (Text.pack "target")
        bound = SlotName.MkSlotName (Text.pack "exiled")
        attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.transformed = False}
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing Nothing)
      """ {"type":"MoveToZone","value":["target",{"type":"Hand"}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone slot Zone.Exile EntryRiders.defaultValue (Just bound) Nothing)
      """ {"type":"MoveToZone","value":["target",{"type":"Exile"},"exiled"]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone bound Zone.Battlefield attacking Nothing Nothing)
      """ {"type":"MoveToZone","value":["exiled",{"type":"Battlefield"},{"tapped":{"type":"Tapped"},"attacking":true}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone bound Zone.Battlefield attacking (Just bound) Nothing)
      """ {"type":"MoveToZone","value":["exiled",{"type":"Battlefield"},{"tapped":{"type":"Tapped"},"attacking":true},"exiled"]} """
    -- CR 113.6m's origin zone alone, the shape a card states when its effect
    -- moves its own source out of a named zone with nothing else to say.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing (Just Zone.Graveyard))
      """ {"type":"MoveToZone","value":["target",{"type":"Hand"},{"type":"Graveyard"}]} """
    -- Reassembling Skeleton's own shape: riders AND an origin, two objects in a
    -- row, which only the type-directed read tells apart.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone slot Zone.Battlefield attacking Nothing (Just Zone.Graveyard))
      """ {"type":"MoveToZone","value":["target",{"type":"Battlefield"},{"tapped":{"type":"Tapped"},"attacking":true},{"type":"Graveyard"}]} """
    -- All three extras at once, so the encoder's order is pinned and the reader
    -- is shown to need none of it.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone slot Zone.Battlefield attacking (Just bound) (Just Zone.Exile))
      """ {"type":"MoveToZone","value":["target",{"type":"Battlefield"},{"tapped":{"type":"Tapped"},"attacking":true},"exiled",{"type":"Exile"}]} """
  -- Both of Draw's PlayerRef shapes: a controller draw and a targeted one.
  Spec.it s "Draw" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
      """ {"type":"Draw","value":[{"type":"Relative","value":{"type":"You"}},{"type":"Literal","value":2}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3))
      """ {"type":"Draw","value":[{"type":"InSlot","value":"target"},{"type":"Literal","value":3}]} """
  Spec.it s "Mill" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Mill (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2) Nothing)
      """ {"type":"Mill","value":[{"type":"InSlot","value":"target"},{"type":"Literal","value":2}]} """
  -- CR 728.1's mill, which counts the nonland cards it put in the graveyard.
  Spec.it s "Mill, with a tally" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.Mill
          (PlayerRef.Relative PlayerRelation.You)
          (Quantity.Literal 2)
          ( Just
              MillTally.MkMillTally
                { MillTally.slot = SlotName.MkSlotName (Text.pack "milled"),
                  MillTally.filter = Filter.Not (Filter.HasCardType CardType.Land)
                }
          )
      )
      """ {"type":"Mill","value":[{"type":"Relative","value":{"type":"You"}},{"type":"Literal","value":2},{"slot":"milled","filter":{"type":"Not","value":{"type":"HasCardType","value":{"type":"Land"}}}}]} """
  Spec.it s "Discard" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Discard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 1))
      """ {"type":"Discard","value":["target",{"type":"Literal","value":1}]} """
  Spec.it s "LoseLife" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.LoseLife (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2))
      """ {"type":"LoseLife","value":[{"type":"InSlot","value":"target"},{"type":"Literal","value":2}]} """
  Spec.it s "GainLife" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))
      """ {"type":"GainLife","value":[{"type":"Relative","value":{"type":"You"}},{"type":"Literal","value":1}]} """
  -- Create's EntryRiders and bound slot are each ELIDED when they are the
  -- default, exactly like MoveToZone above: four emitted forms, the middle two
  -- told apart at decode by JSON TYPE.
  Spec.it s "Create round-trips all four shapes, and elides the defaults" $ do
    let attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.transformed = False}
        plain = EntryRiders.defaultValue
        slot = SlotName.MkSlotName (Text.pack "token")
        card = Text.pack "Goblin Piker"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Quantity.Literal 2) card plain Nothing)
      """ {"type":"Create","value":[{"type":"Literal","value":2},"Goblin Piker"]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Quantity.Literal 1) card plain (Just slot))
      """ {"type":"Create","value":[{"type":"Literal","value":1},"Goblin Piker","token"]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Quantity.Literal 2) card attacking Nothing)
      """ {"type":"Create","value":[{"type":"Literal","value":2},"Goblin Piker",{"tapped":{"type":"Tapped"},"attacking":true}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Quantity.Literal 1) card attacking (Just slot))
      """ {"type":"Create","value":[{"type":"Literal","value":1},"Goblin Piker",{"tapped":{"type":"Tapped"},"attacking":true},"token"]} """
  Spec.it s "Replace" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Replace Duration.UntilEndOfTurn Uses.Once ReplacementOrigin.Other Nothing (ReplacementEffect.DestructionR DestructionRewrite.Regenerate))
      """ {"type":"Replace","value":[{"type":"UntilEndOfTurn"},{"type":"Once"},{"type":"Other"},null,{"type":"DestructionR","value":{"type":"Regenerate"}}]} """
  -- CR 614.15 / 616.1a: a self-replacement gated on a nonzero threshold.
  -- CR 702's ability words have no rules meaning, so "Metalcraft" itself
  -- encodes nothing.
  Spec.it s "Replace (a conditional self-replacement)" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.Replace
          Duration.UntilEndOfTurn
          Uses.Once
          ReplacementOrigin.SelfReplacement
          (Just (Condition.MkCondition (Quantity.Count threeArtifacts) Comparison.AtLeast (Quantity.Literal 3)))
          (ReplacementEffect.DamageR (DamagePattern.MkDamagePattern Nothing SourceRelation.TheSource Nothing) (DamageRewrite.SetAmount 4))
      )
      """ {"type":"Replace","value":[{"type":"UntilEndOfTurn"},{"type":"Once"},{"type":"SelfReplacement"},{"measured":{"type":"Count","value":{"scope":{"type":"InZone","value":[{"type":"Battlefield"},{"type":"EachPlayer"}]},"filter":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Artifact"}},{"type":"ControlledBy","value":{"type":"You"}}]},"aggregation":{"type":"Objects"}}},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":3}},{"type":"DamageR","value":[{"whichSource":{"type":"TheSource"}},{"type":"SetAmount","value":4}]}]} """
  -- CR 614.10a: a slot read, plus the whole-phase selector -- the arm a Phase
  -- alone cannot spell (CR 500.1).
  Spec.it s "SkipNextPhase" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep)))
      """ {"type":"SkipNextPhase","value":[{"type":"InSlot","value":"target"},{"type":"Step","value":{"type":"Beginning","value":{"type":"DrawStep"}}}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) PhaseSelector.CombatPhase)
      """ {"type":"SkipNextPhase","value":[{"type":"InSlot","value":"target"},{"type":"CombatPhase"}]} """
  -- CR 615.7.
  Spec.it s "PreventNextDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PreventNextDamage Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 4))
      """ {"type":"PreventNextDamage","value":[{"type":"UntilEndOfTurn"},"target",{"type":"Literal","value":4}]} """
  -- CR 615.1: the same shield with no amount to spend (Selfless Squire).
  Spec.it s "PreventAllDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PreventAllDamage Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you"))))
      """ {"type":"PreventAllDamage","value":[{"type":"UntilEndOfTurn"},"you"]} """
  -- CR 113.9: this opcode counters an ability as well as a spell, with the type
  -- unchanged, so the wire shape is too.
  Spec.it s "Counter" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Counter (SlotName.MkSlotName (Text.pack "spell")))
      """ {"type":"Counter","value":"spell"} """
  -- CR 701.24: a bare slot name, not an array -- the library is derived from
  -- the object the slot names (CR 400.3), so there is no second field to write.
  Spec.it s "ShuffleIntoLibrary" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ShuffleIntoLibrary (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"ShuffleIntoLibrary","value":"target"} """
  Spec.it s "PutCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (SlotName.MkSlotName (Text.pack "creature")))
      """ {"type":"PutCounters","value":[{"type":"PlusOnePlusOne"},{"type":"Literal","value":1},"creature"]} """
  -- Every PlayerRef shape the opcode accepts: the self-scoped one, and the slot
  -- read CR 702.70a needs.
  Spec.it s "GainPlayerCounters" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2))
      """ {"type":"GainPlayerCounters","value":[{"type":"Relative","value":{"type":"You"}},{"type":"Energy"},{"type":"Literal","value":2}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainPlayerCounters (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) PlayerCounterKind.Poison (Quantity.Literal 3))
      """ {"type":"GainPlayerCounters","value":[{"type":"InSlot","value":"thatPlayer"},{"type":"Poison"},{"type":"Literal","value":3}]} """
  -- The mirror opcode, on the same wire shape and a DIFFERENT tag: CR 728.1's
  -- removal must never decode as a gain.
  Spec.it s "RemovePlayerCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemovePlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Rad (Quantity.InSlot (SlotName.MkSlotName (Text.pack "milled"))))
      """ {"type":"RemovePlayerCounters","value":[{"type":"Relative","value":{"type":"You"}},{"type":"Rad"},{"type":"InSlot","value":"milled"}]} """
  -- CR 701.26a's Tap is Untap's mirror and shares its wire shape, so the two
  -- must not collapse into one tag.
  Spec.it s "Tap round-trips, and is not Untap" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"Tap","value":"target"} """
    Spec.assertBool
      s
      ( toJson (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
          /= toJson (Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      )
      "Tap and Untap of the same slot encode differently"
  Spec.it s "Untap round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"Untap","value":"target"} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Untap (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      """ {"type":"Untap","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  -- CR 701.27a. Both ObjectRef arms, since the pool prints one of each shape's
  -- twin: Thraben Gargoyle's "transform this creature" is the slot, and a
  -- "transform all X" sweep is the filter.
  Spec.it s "Transform round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Transform (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      """ {"type":"Transform","value":"self"} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Transform (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      """ {"type":"Transform","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  Spec.it s "RemoveFromCombat" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemoveFromCombat (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"RemoveFromCombat","value":"target"} """
  -- Both shapes in the pool: a pair, and a repeated phase.
  Spec.it s "AddPhases round-trips the pair and a repeated phase" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain])
      """ {"type":"AddPhases","value":[{"type":"ExtraCombat"},{"type":"ExtraMain"}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraCombat])
      """ {"type":"AddPhases","value":[{"type":"ExtraCombat"},{"type":"ExtraCombat"}]} """
  Spec.it s "GainControl round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainControl Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"GainControl","value":[{"type":"UntilEndOfTurn"},"target"]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainControl Duration.Indefinite (ObjectRef.EachMatching (Filter.HasCardType CardType.Enchantment)))
      """ {"type":"GainControl","value":[{"type":"Indefinite"},{"type":"HasCardType","value":{"type":"Enchantment"}}]} """
  -- The shapes the encoder can emit, told apart by LENGTH: a bare ability name
  -- (CR 603.7a/b's defaults), a two-element form (a stated duration, onset
  -- still the default), and a three-element form (a stated onset, whose last
  -- element is the duration or null).
  Spec.it s "ArmDelayedTrigger round-trips all three shapes, and elides the default onset" $ do
    let sacrificeIt = AbilityName.MkAbilityName (Text.pack "sacrifice it")
        eachCombat = AbilityName.MkAbilityName (Text.pack "each combat")
        returnIt = AbilityName.MkAbilityName (Text.pack "return it")
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ArmDelayedTrigger sacrificeIt Onset.Immediately Nothing)
      """ {"type":"ArmDelayedTrigger","value":"sacrifice it"} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ArmDelayedTrigger eachCombat Onset.Immediately (Just Duration.UntilEndOfTurn))
      """ {"type":"ArmDelayedTrigger","value":["each combat",{"type":"UntilEndOfTurn"}]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ArmDelayedTrigger returnIt Onset.FromYourNextTurn Nothing)
      """ {"type":"ArmDelayedTrigger","value":["return it",{"type":"FromYourNextTurn"},null]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ArmDelayedTrigger returnIt Onset.FromYourNextTurn (Just Duration.UntilEndOfTurn))
      """ {"type":"ArmDelayedTrigger","value":["return it",{"type":"FromYourNextTurn"},{"type":"UntilEndOfTurn"}]} """
  Spec.it s "AffectPlayers" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells)
      """ {"type":"AffectPlayers","value":[{"type":"UntilEndOfTurn"},{"type":"Opponents"},{"type":"CantCastSpells"}]} """
  Spec.it s "CreateEmblem" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateEmblem (Text.pack "Goblin Piker"))
      """ {"type":"CreateEmblem","value":"Goblin Piker"} """
  -- Two different `card` values through the SAME constant codec, so a leak
  -- straight to the constructor (bypassing the codec argument) fails this
  -- rather than merely coincides.
  Spec.it s "CreateEmblem reaches its card only through the supplied codec" $
    Spec.assertEqWith
      s
      "the emblem payload comes from the argument, not the card"
      (Effect.toJson (const sentinel) (Effect.CreateEmblem (Text.pack "a wholly different card type")))
      (Effect.toJson (const sentinel) (Effect.CreateEmblem (0 :: Int)))
  Spec.it s "BecomeMonarch" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.BecomeMonarch MonarchTarget.TheController)
      """ {"type":"BecomeMonarch","value":{"type":"TheController"}} """
  Spec.it s "ItBecomes" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ItBecomes Daytime.Night)
      """ {"type":"ItBecomes","value":{"type":"Night"}} """
  Spec.it s "ExileUntilMonarch" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ExileUntilMonarch (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"ExileUntilMonarch","value":"target"} """
  Spec.it s "PlaySubgame" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PlaySubgame (SlotName.MkSlotName (Text.pack "loser")))
      """ {"type":"PlaySubgame","value":"loser"} """
  Spec.it s "ExileHandThenDraw" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.ExileHandThenDraw
      """ {"type":"ExileHandThenDraw"} """
  Spec.it s "Proliferate" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.Proliferate
      """ {"type":"Proliferate"} """
  -- CR 701.54a: nullary, because rule 701.54 fixes the chooser, the count and the
  -- qualification, leaving an author nothing to write.
  Spec.it s "TemptWithTheRing" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.TemptWithTheRing
      """ {"type":"TemptWithTheRing"} """
  Spec.it s "PlayerSacrifices" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PlayerSacrifices (SlotName.MkSlotName (Text.pack "t")) (Filter.HasCardType CardType.Creature) (Quantity.Literal 1))
      """ {"type":"PlayerSacrifices","value":["t",{"type":"HasCardType","value":{"type":"Creature"}},{"type":"Literal","value":1}]} """
  -- CR 500.7: a slot read with an empty skip set, a self-scoped arm carrying
  -- CR 500.11's skip of one step, and a two-member set.
  Spec.it s "TakeExtraTurn" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Set.empty)
      """ {"type":"TakeExtraTurn","value":[{"type":"InSlot","value":"target"},[]]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (PlayerRef.Relative PlayerRelation.You) (Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap))))
      """ {"type":"TakeExtraTurn","value":[{"type":"Relative","value":{"type":"You"}},[{"type":"Step","value":{"type":"Beginning","value":{"type":"Untap"}}}]]} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn PlayerRef.EachPlayer (Set.fromList [PhaseSelector.Step (Phase.Beginning BeginningStep.Untap), PhaseSelector.CombatPhase]))
      """ {"type":"TakeExtraTurn","value":[{"type":"EachPlayer"},[{"type":"Step","value":{"type":"Beginning","value":{"type":"Untap"}}},{"type":"CombatPhase"}]]} """

-- The stand-in a parametricity test hands over in place of a real card codec:
-- any Value at all, so long as both instantiations are given the same one.
sentinel :: Value.Value
sentinel = Common.text (Text.pack "SENTINEL")

-- The artifact count the conditional self-replacement above (CR 614.15 / 616.1a)
-- reads; its "three or more" threshold lives in the Condition at the use site.
threeArtifacts :: Count.Count Quantity.Quantity
threeArtifacts =
  Count.MkCount
    (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
    (Filter.And [Filter.HasCardType CardType.Artifact, Filter.ControlledBy PlayerRelation.You])
    Aggregation.Objects
