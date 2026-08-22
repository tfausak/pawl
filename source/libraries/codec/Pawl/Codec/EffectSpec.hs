module Pawl.Codec.EffectSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Amass as Amass
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastObligation as CastObligation
import qualified Pawl.Types.CastOffer as CastOffer.Type
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamageDirection as DamageDirection
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.Fight as Fight
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- | The `card` parameter is instantiated at 'Text.Text' throughout (and at
-- 'Int' in the parametricity case). 'Effect.codec' reaches it only through the
-- supplied codec, so any type proves the shape.
cardCodec :: Codec.Codec Text.Text
cardCodec = Common.text

codec :: Codec.Codec (Effect.Effect Text.Text)
codec = Effect.codec cardCodec

toJson :: Effect.Effect Text.Text -> Value.Value
toJson = Codec.encode codec

fromJson :: Value.Value -> Either Text.Text (Effect.Effect Text.Text)
fromJson = Codec.decode codec

-- | A card codec that ignores its argument, at whatever type the use site
-- demands. Only its encoder is exercised: the parametricity case below asks
-- whether the payload comes from THIS codec rather than from the constructor.
constCodec :: Value.Value -> Codec.Codec a
constCodec v =
  Codec.MkCodec
    { Codec.encode = const v,
      Codec.decode = const (Left (Text.pack "constCodec does not decode")),
      -- Borrowed rather than built: naming a Schema here would make this spec
      -- depend on pawl:json-schema, which nothing else under Pawl.Codec does.
      Codec.schema = Codec.schema Common.text
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Effect" $ do
  -- Every ObjectRef arm has to survive the trip through the payload.
  Spec.it s "DealDamage round-trips all three ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3) Nothing Nothing))
      " {\"type\":\"DealDamage\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":3}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.EachMatching (Filter.HasKeyword Keyword.Flying)) (Quantity.Literal 1) Nothing Nothing))
      " {\"type\":\"DealDamage\",\"value\":{\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasKeyword\",\"value\":{\"type\":\"Flying\"}}},\"quantity\":{\"type\":\"Literal\",\"value\":1}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (DealDamage.MkDealDamage ObjectRef.EachPlayer (Quantity.Literal 2) Nothing Nothing))
      " {\"type\":\"DealDamage\",\"value\":{\"ref\":{\"type\":\"EachPlayer\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
  -- Both ObjectRef arms have to survive the trip through the payload.
  Spec.it s "ModifyTarget round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t")))))
      " {\"type\":\"ModifyTarget\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"modification\":{\"type\":\"GainKeyword\",\"value\":{\"type\":\"Trample\"}},\"ref\":{\"type\":\"InSlot\",\"value\":\"t\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.EachMatching (Filter.And [Filter.HasCardType CardType.Creature, Filter.IsAttacking]))))
      " {\"type\":\"ModifyTarget\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"modification\":{\"type\":\"GainKeyword\",\"value\":{\"type\":\"Trample\"}},\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"IsAttacking\"}]}}}} "
  Spec.it s "ChangeText, a basic land type swap that forbids nothing" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChangeText (ChangeText.MkChangeText SubtypeFamily.BasicLandType Set.empty (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"ChangeText\",\"value\":{\"family\":{\"type\":\"BasicLandType\"},\"forbidden\":[],\"slot\":\"target\"}} "
  Spec.it s "ChangeText, a creature type swap whose new word can't be Wall" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChangeText (ChangeText.MkChangeText SubtypeFamily.CreatureType (Set.singleton Subtype.Wall) (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"ChangeText\",\"value\":{\"family\":{\"type\":\"CreatureType\"},\"forbidden\":[{\"type\":\"Wall\"}],\"slot\":\"target\"}} "
  Spec.it s "AddMana, a fixed type and any color" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) (ManaProduction.OfType (ManaType.Colored Color.Green)) ManaRetention.Ordinary Nothing))
      " {\"type\":\"AddMana\",\"value\":{\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) ManaProduction.AnyColor ManaRetention.Ordinary Nothing))
      " {\"type\":\"AddMana\",\"value\":{\"production\":{\"type\":\"AnyColor\"}}} "
  -- CR 106.4's other half: Shizuko, Caller of Autumn's "that player adds", where
  -- the recipient is written because CR 109.5's "you" is somebody else.
  Spec.it s "AddMana, a recipient the card names" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) (ManaProduction.OfType (ManaType.Colored Color.Green)) ManaRetention.Ordinary Nothing))
      " {\"type\":\"AddMana\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}}} "
  Spec.it s "Search" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.Search
          Search.MkSearch
            { Search.searcher = PlayerRef.Relative PlayerRelation.You,
              Search.owner = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
              Search.quantity = Quantity.Literal 2,
              Search.filter = Filter.HasCardType CardType.Land,
              Search.upTo = False,
              Search.destination = SearchDestination.BattlefieldTapped
            }
      )
      " {\"type\":\"Search\",\"value\":{\"searcher\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"owner\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":2},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"destination\":{\"type\":\"BattlefieldTapped\"}}} "
  Spec.it s "ExileAllGraveyards" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.ExileAllGraveyards
      " {\"type\":\"ExileAllGraveyards\"} "
  Spec.it s "RestartGame" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RestartGame Nothing)
      " {\"type\":\"RestartGame\"} "
  -- CR 727.5's exemption, the shape Karn Liberated writes.
  Spec.it s "RestartGame exempting cards" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RestartGame (Just (ObjectRef.EachCardExiledWithSource Nothing)))
      " {\"type\":\"RestartGame\",\"value\":{\"type\":\"EachCardExiledWithSource\"}} "
  Spec.it s "ControlPlayerNextTurn" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ControlPlayerNextTurn (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"ControlPlayerNextTurn\",\"value\":\"target\"} "
  -- Both ObjectRef arms, plus the two shapes CR 701.19c's regeneration rider
  -- takes. The two-element literal pins the elided (Nothing) arm of the
  -- bound-count slot below.
  Spec.it s "Destroy carries its CR 701.19c rider both ways" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (Destroy.MkDestroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.Regenerable Nothing Nothing Nothing))
      " {\"type\":\"Destroy\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"t\"},\"regenerability\":{\"type\":\"Regenerable\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (Destroy.MkDestroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.CantBeRegenerated Nothing Nothing Nothing))
      " {\"type\":\"Destroy\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"t\"},\"regenerability\":{\"type\":\"CantBeRegenerated\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing Nothing Nothing))
      " {\"type\":\"Destroy\",\"value\":{\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"regenerability\":{\"type\":\"Regenerable\"}}} "
  -- The slot the sweep binds its count into is ELIDED when absent, so the case
  -- above writes two keys and this one writes three. Since #1305 that is
  -- Fields.defaulted omitting a key, not a trailing array element recovered by
  -- JSON type.
  Spec.it s "Destroy's bound-count slot round-trips and is written only when present" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Artifact)) Regenerability.Regenerable (Just (SlotName.MkSlotName (Text.pack "destroyed"))) Nothing Nothing))
      " {\"type\":\"Destroy\",\"value\":{\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}},\"regenerability\":{\"type\":\"Regenerable\"},\"slot\":\"destroyed\"}} "
  Spec.it s "PayAnyEnergy" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PayAnyEnergy (SlotName.MkSlotName (Text.pack "paid")))
      " {\"type\":\"PayAnyEnergy\",\"value\":\"paid\"} "
  Spec.it s "Sacrifice" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self")))
      " {\"type\":\"Sacrifice\",\"value\":\"self\"} "
  Spec.it s "Attach" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Attach (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"Attach\",\"value\":\"target\"} "
  -- CR 701.3: the destination Filter travels in the payload, distinguishing
  -- this arm's wire format from Attach's bare slot above.
  Spec.it s "AttachTarget" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AttachTarget (AttachTarget.MkAttachTarget (SlotName.MkSlotName (Text.pack "target")) (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"AttachTarget\",\"value\":{\"slot\":\"target\",\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- CR 303.4d / CR 301.5c: the same payload as AttachTarget above, so only the
  -- tag tells the two opcodes apart on the wire.
  Spec.it s "AttachTargetToEach" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AttachTargetToEach (AttachTarget.MkAttachTarget (SlotName.MkSlotName (Text.pack "target")) (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"AttachTargetToEach\",\"value\":{\"slot\":\"target\",\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- MoveToZone's payload is the ObjectRef and the destination zone, then four
  -- independently elided extras -- the EntryRiders, the bound slot, CR 113.6m's
  -- origin zone and CR 401.2's library position -- so it is told apart by JSON
  -- TYPE alone, at every length. A string is the bound slot; an object is the
  -- origin zone or the library position if it decodes as one and the riders
  -- otherwise, which is why the cases below put those objects side by side.
  --
  -- The ObjectRef in first position is a tagged object at every arm since
  -- #1304, so it takes no part in that reckoning: it is positional, and the
  -- tail begins after it.
  Spec.it s "MoveToZone round-trips every shape, and elides the defaults" $ do
    let slot = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
        bound = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled"))
        boundSlot = SlotName.MkSlotName (Text.pack "exiled")
        attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = False}
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Hand\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Exile EntryRiders.defaultValue (Just boundSlot) Nothing LibraryPlacement.defaultValue))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Exile\"},\"slot\":\"exiled\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone bound Zone.Battlefield attacking Nothing Nothing LibraryPlacement.defaultValue))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"zone\":{\"type\":\"Battlefield\"},\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone bound Zone.Battlefield attacking (Just boundSlot) Nothing LibraryPlacement.defaultValue))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"zone\":{\"type\":\"Battlefield\"},\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true},\"slot\":\"exiled\"}} "
    -- CR 113.6m's origin zone alone, the shape a card states when its effect
    -- moves its own source out of a named zone with nothing else to say.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing (Just Zone.Graveyard) LibraryPlacement.defaultValue))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Hand\"},\"origin\":{\"type\":\"Graveyard\"}}} "
    -- Reassembling Skeleton's own shape: riders AND an origin, two objects in a
    -- row, which only the type-directed read tells apart.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Battlefield attacking Nothing (Just Zone.Graveyard) LibraryPlacement.defaultValue))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Battlefield\"},\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true},\"origin\":{\"type\":\"Graveyard\"}}} "
    -- All four extras at once, so the encoder's order is pinned and the reader
    -- is shown to need none of it. The origin zone and the library position sit
    -- next to each other here, which is the pair only their disjoint tags tell
    -- apart.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Battlefield attacking (Just boundSlot) (Just Zone.Exile) (LibraryPlacement.Stated LibraryPosition.Top)))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Battlefield\"},\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true},\"slot\":\"exiled\",\"origin\":{\"type\":\"Exile\"},\"placement\":{\"type\":\"Stated\",\"value\":{\"type\":\"Top\"}}}} "
    -- Griptide's shape: a library destination with the end it arrives at, and
    -- nothing else. The position is the only extra, so this is what proves it is
    -- not read positionally as the riders.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Library EntryRiders.defaultValue Nothing Nothing (LibraryPlacement.Stated LibraryPosition.Top)))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Library\"},\"placement\":{\"type\":\"Stated\",\"value\":{\"type\":\"Top\"}}}} "
    -- And the default end is ELIDED, so Unsummon's two-element payload is
    -- unchanged by the field's arrival. Decoding that payload is what fills it
    -- back in.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Library EntryRiders.defaultValue Nothing Nothing (LibraryPlacement.Stated LibraryPosition.Bottom)))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Library\"}}} "
    -- Evacuation's shape: an EachMatching ref in first position, which is an
    -- object where every case above is a string.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"zone\":{\"type\":\"Hand\"}}} "
  -- Both of Draw's PlayerRef shapes: a controller draw and a targeted one.
  Spec.it s "Draw" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)))
      " {\"type\":\"Draw\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3)))
      " {\"type\":\"Draw\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":3}}} "
  -- No-Regrets Egret's "you may reveal No-Regrets Egret", which names itself
  -- through CR 113.7's reserved self slot.
  Spec.it s "Reveal" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Reveal (Reveal.MkReveal (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))) Nothing))
      " {\"type\":\"Reveal\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"self\"}}} "
  -- Into the Wilds' "look at the top card of your library", whose slot the next
  -- clause reads back.
  Spec.it s "LookAt" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.LookAt
          ( LookAt.MkLookAt
              (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)))
              (SlotName.MkSlotName (Text.pack "looked"))
          )
      )
      " {\"type\":\"LookAt\",\"value\":{\"ref\":{\"type\":\"TopOfLibrary\",\"value\":{\"count\":{\"type\":\"Literal\",\"value\":1},\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}}},\"slot\":\"looked\"}} "
  -- Both of Scry's PlayerRef shapes: Crystal Ball's controller scry and
  -- Kozilek's Command's "target player scries 2".
  Spec.it s "Scry" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Scry (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)))
      " {\"type\":\"Scry\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Scry (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2)))
      " {\"type\":\"Scry\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
  -- Curate's "Surveil 2", and a distinct tag from Scry's above: the two carry the
  -- same payload and differ only in where the unwanted cards go, so a shared tag
  -- would decode one card's text as the other's.
  Spec.it s "Surveil" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Surveil (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)))
      " {\"type\":\"Surveil\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
  -- Spin into Myth's "then fateseal 2". The PlayerRef is the FATESEALER, so the
  -- controller spelling is the one a card writes; the opponent whose library is
  -- looked at is chosen as the effect applies and appears nowhere in the data.
  Spec.it s "Fateseal" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Fateseal (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)))
      " {\"type\":\"Fateseal\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
  -- Merfolk Branchwalker's "it explores", against the trigger-source slot, and
  -- the swept-set shape the ObjectRef also admits.
  Spec.it s "Explore" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Explore (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Explore\",\"value\":{\"type\":\"InSlot\",\"value\":\"self\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Explore (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"Explore\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  Spec.it s "Mill" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Mill (Mill.MkMill (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2) Nothing Nothing))
      " {\"type\":\"Mill\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
  -- CR 728.1's mill, which counts the nonland cards it put in the graveyard.
  Spec.it s "Mill, with a tally" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.Mill
          ( Mill.MkMill
              (PlayerRef.Relative PlayerRelation.You)
              (Quantity.Literal 2)
              ( Just
                  MillTally.MkMillTally
                    { MillTally.slot = SlotName.MkSlotName (Text.pack "milled"),
                      MillTally.filter = Filter.Not (Filter.HasCardType CardType.Land)
                    }
              )
              Nothing
          )
      )
      " {\"type\":\"Mill\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2},\"tally\":{\"slot\":\"milled\",\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}}}} "
  Spec.it s "Discard" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Discard (Discard.Counted (CountedDiscard.MkCountedDiscard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 1))))
      " {\"type\":\"Discard\",\"value\":{\"type\":\"Counted\",\"value\":{\"slot\":\"target\",\"quantity\":{\"type\":\"Literal\",\"value\":1}}}} "
  Spec.it s "LoseLife" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.LoseLife (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2)))
      " {\"type\":\"LoseLife\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
  Spec.it s "GainLife" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainLife (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)))
      " {\"type\":\"GainLife\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":1}}} "
  Spec.it s "ExchangeLifeTotals" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ExchangeLifeTotals (ExchangeSides.WithController (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"ExchangeLifeTotals\",\"value\":{\"type\":\"WithController\",\"value\":\"target\"}} "
  Spec.it s "ExchangeLifeTotals between two targets" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ExchangeLifeTotals (ExchangeSides.BetweenTargets (SlotName.MkSlotName (Text.pack "players"))))
      " {\"type\":\"ExchangeLifeTotals\",\"value\":{\"type\":\"BetweenTargets\",\"value\":\"players\"}} "
  Spec.it s "SetLifeTotal" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 10)))
      " {\"type\":\"SetLifeTotal\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":1e1}}} "
  -- Nullary: the whole choice is the resolving controller's, so there is nothing
  -- for card data to carry.
  Spec.it s "RedistributeLifeTotals" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.RedistributeLifeTotals
      " {\"type\":\"RedistributeLifeTotals\"} "
  -- Create's EntryRiders, bound slot and CR 111.2 creator are each ELIDED when
  -- they are the default, exactly like MoveToZone above; the riders and the slot
  -- were once the middle two of four emitted forms, told apart at decode by JSON
  -- TYPE.
  Spec.it s "Create round-trips every combination of its three elided keys" $ do
    let attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = False}
        plain = EntryRiders.defaultValue
        slot = SlotName.MkSlotName (Text.pack "token")
        card = Text.pack "Goblin Piker"
        you = PlayerRef.Relative PlayerRelation.You
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Create.MkCreate (Quantity.Literal 2) card plain Nothing you))
      " {\"type\":\"Create\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":2},\"card\":\"Goblin Piker\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Create.MkCreate (Quantity.Literal 1) card plain (Just slot) you))
      " {\"type\":\"Create\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":1},\"card\":\"Goblin Piker\",\"slot\":\"token\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Create.MkCreate (Quantity.Literal 2) card attacking Nothing you))
      " {\"type\":\"Create\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":2},\"card\":\"Goblin Piker\",\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Create.MkCreate (Quantity.Literal 1) card attacking (Just slot) you))
      " {\"type\":\"Create\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":1},\"card\":\"Goblin Piker\",\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true},\"slot\":\"token\"}} "
    -- CR 111.2's creator, the one key of the three that is not a card's own
    -- name: Rampage of the Clans writes the controller of the permanent the
    -- loop around it bound.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Create.MkCreate (Quantity.Literal 1) card plain Nothing (PlayerRef.ControllerOfBound (SlotName.MkSlotName (Text.pack "victim")))))
      " {\"type\":\"Create\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":1},\"card\":\"Goblin Piker\",\"creator\":{\"type\":\"ControllerOfBound\",\"value\":\"victim\"}}} "
  -- Both ObjectRef arms have to survive. A count of one is elided, so both of
  -- these write the ref alone.
  Spec.it s "CreateCopy round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Literal 1) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
      " {\"type\":\"CreateCopy\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Literal 1) (ObjectRef.EachMatching (Filter.HasKeyword Keyword.Flying))))
      " {\"type\":\"CreateCopy\",\"value\":{\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasKeyword\",\"value\":{\"type\":\"Flying\"}}}}} "
  -- Kicked Rite of Replication's five. Before #1305 this was a second payload
  -- SHAPE told apart by length; it is now the same shape with the defaulted key
  -- present.
  Spec.it s "CreateCopy round-trips a count above one" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Literal 5) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
      " {\"type\":\"CreateCopy\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":5},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
  -- Unstable Shapeshifter's own pair. The two refs take DIFFERENT shapes on
  -- purpose: they are not interchangeable, and a codec that swapped them would
  -- round-trip a symmetric fixture unnoticed.
  Spec.it s "BecomeCopy" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.BecomeCopy (BecomeCopy.MkBecomeCopy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "became"))) (ObjectRef.EachMatching Filter.IsSource)))
      " {\"type\":\"BecomeCopy\",\"value\":{\"original\":{\"type\":\"InSlot\",\"value\":\"became\"},\"subject\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"IsSource\"}}}} "
  Spec.it s "Replace" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.Replace
          Replace.MkReplace
            { Replace.duration = Duration.UntilEndOfTurn,
              Replace.uses = Uses.Once,
              Replace.origin = ReplacementOrigin.Other,
              Replace.condition = Nothing,
              Replace.effect = ReplacementEffect.DestructionR DestructionRewrite.Regenerate
            }
      )
      " {\"type\":\"Replace\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"uses\":{\"type\":\"Once\"},\"origin\":{\"type\":\"Other\"},\"effect\":{\"type\":\"DestructionR\",\"value\":{\"type\":\"Regenerate\"}}}} "
  -- CR 614.15 / 616.1a: a self-replacement gated on a nonzero threshold.
  -- CR 702's ability words have no rules meaning, so "Metalcraft" itself
  -- encodes nothing.
  Spec.it s "Replace (a conditional self-replacement)" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.Replace
          Replace.MkReplace
            { Replace.duration = Duration.UntilEndOfTurn,
              Replace.uses = Uses.Once,
              Replace.origin = ReplacementOrigin.SelfReplacement,
              Replace.condition = Just (Condition.Compares (Compares.MkCompares (Quantity.Count threeArtifacts) Comparison.AtLeast (Quantity.Literal 3))),
              Replace.effect = ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern Nothing Filter.IsSource Nothing Nothing Nothing) (DamageRewrite.SetAmount 4) Seq.empty)
            }
      )
      " {\"type\":\"Replace\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"uses\":{\"type\":\"Once\"},\"origin\":{\"type\":\"SelfReplacement\"},\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Count\",\"value\":{\"scope\":{\"type\":\"InZone\",\"value\":{\"zone\":{\"type\":\"Battlefield\"},\"player\":{\"type\":\"EachPlayer\"}}},\"filter\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}},{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}]},\"aggregation\":{\"type\":\"Members\"}}},\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":3}}},\"effect\":{\"type\":\"DamageR\",\"value\":{\"matching\":{\"whatSource\":{\"type\":\"IsSource\"}},\"rewrite\":{\"type\":\"SetAmount\",\"value\":4}}}}} "
  -- CR 614.10a: a slot read, plus the whole-phase selector -- the arm a Phase
  -- alone cannot spell (CR 500.1).
  Spec.it s "SkipNextPhase" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep))))
      " {\"type\":\"SkipNextPhase\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"selector\":{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"DrawStep\"}}}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) PhaseSelector.CombatPhase))
      " {\"type\":\"SkipNextPhase\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"selector\":{\"type\":\"CombatPhase\"}}} "
  -- CR 615.7.
  Spec.it s "PreventNextDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage Duration.UntilEndOfTurn Nothing (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Nothing (Quantity.Literal 4) Seq.empty))
      " {\"type\":\"PreventNextDamage\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":4}}} "
  -- CR 615.5's additional effect (Test of Faith) and CR 510.2's kind (Decorated
  -- Griffin's "the next 1 COMBAT damage"). Both elided above; written here, so
  -- the defaulted keys are proven to DECODE and the recursion into an effect
  -- inside an effect is round-tripped.
  Spec.it s "PreventNextDamage with a kind and a CR 615.5 rider" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.PreventNextDamage
          PreventNextDamage.MkPreventNextDamage
            { PreventNextDamage.duration = Duration.UntilEndOfTurn,
              PreventNextDamage.kind = Just DamageKind.Combat,
              PreventNextDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
              PreventNextDamage.chosenSource = Nothing,
              PreventNextDamage.quantity = Quantity.Literal 3,
              PreventNextDamage.riders =
                Seq.singleton (Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.InSlot (SlotName.MkSlotName (Text.pack "thatMuch"))) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
            }
      )
      " {\"type\":\"PreventNextDamage\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"kind\":{\"type\":\"Combat\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":3},\"riders\":[{\"type\":\"PutCounters\",\"value\":{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"quantity\":{\"type\":\"InSlot\",\"value\":\"thatMuch\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}}]}} "
  -- CR 608.2f: Soulfire Eruption's per-object body, the other nesting of an
  -- effect inside an effect -- a DealDamage reading the mana value of the card
  -- an earlier body instruction exiled for THIS member.
  Spec.it s "ForEach" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.ForEach
          ForEach.MkForEach
            { ForEach.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "victims")),
              ForEach.slot = SlotName.MkSlotName (Text.pack "victim"),
              ForEach.body =
                Seq.singleton (Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "victim"))) (Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot (SlotName.MkSlotName (Text.pack "exiled")) Quantity.ManaValue)) Nothing Nothing))
            }
      )
      " {\"type\":\"ForEach\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"victims\"},\"slot\":\"victim\",\"body\":[{\"type\":\"DealDamage\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"victim\"},\"quantity\":{\"type\":\"AgainstSlot\",\"value\":{\"slot\":\"exiled\",\"quantity\":{\"type\":\"ManaValue\"}}}}}]}} "
  -- CR 615.1: the same shield with no amount to spend (Selfless Squire).
  Spec.it s "PreventAllDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage Duration.UntilEndOfTurn Nothing (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you"))) DamageDirection.DealtTo Nothing Seq.empty))
      " {\"type\":\"PreventAllDamage\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"you\"}}} "
  -- The same shield with both defaulted keys written: CR 510.2's kind
  -- (Inkshield's "all COMBAT damage") and CR 615.5's rider (Brace for Impact's
  -- +1/+1 counter). Elided above, so this is what proves they decode.
  Spec.it s "PreventAllDamage with a kind and a CR 615.5 rider" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.PreventAllDamage
          PreventAllDamage.MkPreventAllDamage
            { PreventAllDamage.duration = Duration.UntilEndOfTurn,
              PreventAllDamage.kind = Just DamageKind.Combat,
              PreventAllDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
              PreventAllDamage.direction = DamageDirection.DealtTo,
              PreventAllDamage.chosenSource = Nothing,
              PreventAllDamage.riders =
                Seq.singleton (Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.InSlot (SlotName.MkSlotName (Text.pack "thatMuch"))) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
            }
      )
      " {\"type\":\"PreventAllDamage\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"kind\":{\"type\":\"Combat\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"riders\":[{\"type\":\"PutCounters\",\"value\":{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"quantity\":{\"type\":\"InSlot\",\"value\":\"thatMuch\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}}]}} "
  -- CR 614.9: Turn the Tables, whose kind field is PRINTED ("all combat
  -- damage") and whose two refs are the source side then the destination.
  Spec.it s "RedirectDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.RedirectDamage
          RedirectDamage.MkRedirectDamage
            { RedirectDamage.duration = Duration.UntilEndOfTurn,
              RedirectDamage.kind = Just DamageKind.Combat,
              RedirectDamage.from = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you")),
              RedirectDamage.to = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
            }
      )
      " {\"type\":\"RedirectDamage\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"kind\":{\"type\":\"Combat\"},\"from\":{\"type\":\"InSlot\",\"value\":\"you\"},\"to\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
  -- CR 113.9: this opcode counters an ability as well as a spell, with the type
  -- unchanged, so the wire shape is too.
  Spec.it s "Counter" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Counter (Counter.MkCounter (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) Nothing))
      " {\"type\":\"Counter\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"}}} "
  -- Swift Silence's "counter all other spells. Draw a card for each spell
  -- countered this way": the swept set and the slot the count is bound at, which
  -- is the pair a targeted counterspell writes neither of.
  Spec.it s "Counter over a swept set, binding what it countered" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Counter (Counter.MkCounter (ObjectRef.EachSpell (Filter.Not Filter.IsSource)) (Just (SlotName.MkSlotName (Text.pack "countered")))))
      " {\"type\":\"Counter\",\"value\":{\"ref\":{\"type\":\"EachSpell\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}},\"slot\":\"countered\"}} "
  -- CR 701.24: an ObjectRef, tagged InSlot around the slot name.
  -- Riftsweeper's shape -- the library is derived from the objects it names (CR
  -- 400.3), so there is no second field to write.
  Spec.it s "ShuffleIntoLibrary" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.ShuffleIntoLibrary
          ShuffleIntoLibrary.MkShuffleIntoLibrary
            { ShuffleIntoLibrary.library = Nothing,
              ShuffleIntoLibrary.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
            }
      )
      " {\"type\":\"ShuffleIntoLibrary\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
  -- CR 701.24c's named library (Dwell on the Past's "their library"): the pair
  -- form, an ARRAY where a lone ObjectRef is a tagged object -- which is what
  -- tells the two apart.
  Spec.it s "ShuffleIntoLibrary naming the library" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.ShuffleIntoLibrary
          ShuffleIntoLibrary.MkShuffleIntoLibrary
            { ShuffleIntoLibrary.library = Just (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "player"))),
              ShuffleIntoLibrary.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "cards"))
            }
      )
      " {\"type\":\"ShuffleIntoLibrary\",\"value\":{\"library\":{\"type\":\"InSlot\",\"value\":\"player\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"cards\"}}} "
  -- CR 608.2g, in the shape rule 310.12b's battles and rule 702's keywords mint
  -- in the engine (Pawl.Engine.Battle, Pawl.Engine.Keyword): both defaulted keys
  -- elided. Wild Evocation is the one card that writes them, and
  -- Pawl.Codec.OfferCastSpec covers that spelling.
  Spec.it s "OfferCast" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.OfferCast
          OfferCast.MkOfferCast
            { OfferCast.slot = SlotName.MkSlotName (Text.pack "exiled"),
              OfferCast.caster = PlayerRef.Relative PlayerRelation.You,
              OfferCast.optionality = CastObligation.Optional,
              OfferCast.offer = CastOffer.defaultValue
            }
      )
      " {\"type\":\"OfferCast\",\"value\":{\"slot\":\"exiled\"}} "
    -- CR 310.12b's two riders, which is what stops the offer being elided.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.OfferCast
          OfferCast.MkOfferCast
            { OfferCast.slot = SlotName.MkSlotName (Text.pack "exiled"),
              OfferCast.caster = PlayerRef.Relative PlayerRelation.You,
              OfferCast.optionality = CastObligation.Optional,
              OfferCast.offer =
                CastOffer.Type.MkCastOffer
                  { CastOffer.Type.transformed = True,
                    CastOffer.Type.withoutPayingManaCost = True,
                    CastOffer.Type.payingInstead = Nothing
                  }
            }
      )
      " {\"type\":\"OfferCast\",\"value\":{\"slot\":\"exiled\",\"offer\":{\"transformed\":true,\"withoutPayingManaCost\":true}}} "
  Spec.it s "PutCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "creature")))))
      " {\"type\":\"PutCounters\",\"value\":{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"ref\":{\"type\":\"InSlot\",\"value\":\"creature\"}}} "
  -- The ObjectRef's other arm: a Filter is an object where a slot is a string, so
  -- the widening left every card's spelling alone (Pawl.Codec.ObjectRef).
  Spec.it s "PutCounters over a swept set" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (ObjectRef.EachMatching (Filter.HasDesignation Designation.Renowned))))
      " {\"type\":\"PutCounters\",\"value\":{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasDesignation\",\"value\":{\"type\":\"Renowned\"}}}}} "
  -- CR 122: PutCounters' mirror, and a distinct tag -- a signed amount under one
  -- tag would make the two indistinguishable in a card file.
  Spec.it s "RemoveCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.MinusOneMinusOne (Quantity.Literal 1) (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"RemoveCounters\",\"value\":{\"kind\":{\"type\":\"MinusOneMinusOne\"},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"slot\":\"target\"}} "
  -- Every PlayerRef shape the opcode accepts: the self-scoped one, and the slot
  -- read CR 702.70a needs.
  Spec.it s "GainPlayerCounters" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2)))
      " {\"type\":\"GainPlayerCounters\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"kind\":{\"type\":\"Energy\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) PlayerCounterKind.Poison (Quantity.Literal 3)))
      " {\"type\":\"GainPlayerCounters\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"kind\":{\"type\":\"Poison\"},\"quantity\":{\"type\":\"Literal\",\"value\":3}}} "
  -- The mirror opcode, on the same wire shape and a DIFFERENT tag: CR 728.1's
  -- removal must never decode as a gain.
  Spec.it s "RemovePlayerCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Rad (Quantity.InSlot (SlotName.MkSlotName (Text.pack "milled")))))
      " {\"type\":\"RemovePlayerCounters\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"kind\":{\"type\":\"Rad\"},\"quantity\":{\"type\":\"InSlot\",\"value\":\"milled\"}}} "
  -- CR 701.26a's Tap is Untap's mirror and shares its wire shape, so the two
  -- must not collapse into one tag.
  Spec.it s "Tap round-trips, and is not Untap" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"Tap\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
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
      " {\"type\":\"Untap\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Untap (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"Untap\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- CR 701.35a. Both ObjectRef arms, since the pool prints one of each: Azorius
  -- Arrester's "detain target creature an opponent controls" is the slot, and
  -- Lavinia of the Tenth's "detain each nonland permanent your opponents control"
  -- is the filter. It shares Tap's and Untap's wire shape, so it must not collapse
  -- into either tag.
  Spec.it s "Detain round-trips both ObjectRef arms, and is not Tap" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Detain (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"Detain\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Detain (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"Detain\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
    Spec.assertBool
      s
      ( toJson (Effect.Detain (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
          /= toJson (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      )
      "Detain and Tap of the same slot encode differently"
  -- CR 701.15a, which shares Detain's wire shape down to the field: both are a
  -- bare ObjectRef whose duration and whose actor the rulebook fixes, so the tag
  -- is the only thing telling them apart. data/cards prints the slot arm (Jeering
  -- Homunculus); the filter arm costs nothing.
  Spec.it s "Goad round-trips both ObjectRef arms, and is not Detain" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Goad (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"Goad\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Goad (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"Goad\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
    Spec.assertBool
      s
      ( toJson (Effect.Goad (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
          /= toJson (Effect.Detain (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      )
      "Goad and Detain of the same slot encode differently"
  -- CR 502.3's one-shot prohibition, which shares Tap's and Untap's wire shape
  -- and must not collapse into either: a card printing "tap target creature. That
  -- creature doesn't untap ..." writes two effects over the same slot.
  Spec.it s "DoesNotUntapNext round-trips, and is neither Tap nor Untap" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DoesNotUntapNext (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"DoesNotUntapNext\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
    Spec.assertBool
      s
      ( toJson (Effect.DoesNotUntapNext (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
          /= toJson (Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      )
      "DoesNotUntapNext and Untap of the same slot encode differently"
    Spec.assertBool
      s
      ( toJson (Effect.DoesNotUntapNext (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
          /= toJson (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      )
      "DoesNotUntapNext and Tap of the same slot encode differently"
  -- CR 701.27a. Both ObjectRef arms, since the pool prints one of each shape's
  -- twin: Thraben Gargoyle's "transform this creature" is the slot, and a
  -- "transform all X" sweep is the filter.
  Spec.it s "Transform round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Transform (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Transform\",\"value\":{\"type\":\"InSlot\",\"value\":\"self\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Transform (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"Transform\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- CR 702.26b. Both ObjectRef arms, since the pool prints one of each: Reality
  -- Ripple's "target artifact, creature, or land phases out" is the slot, and
  -- Teferi's Protection's "all permanents you control phase out" is the filter. It
  -- shares Tap's, Untap's and Transform's wire shape, so it must not collapse into
  -- any of their tags.
  Spec.it s "PhaseOut round-trips both ObjectRef arms, and is not Transform" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PhaseOut (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"PhaseOut\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PhaseOut (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"PhaseOut\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
    Spec.assertBool
      s
      ( toJson (Effect.PhaseOut (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
          /= toJson (Effect.Transform (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      )
      "PhaseOut and Transform of the same slot encode differently"
  -- CR 708.2. One slot and no ObjectRef, since Backslide names a target and
  -- nothing in the pool sweeps a set face down; the listed characteristics are
  -- CR 708.2a's here, so that key is absent.
  Spec.it s "TurnFaceDown" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown (SlotName.MkSlotName (Text.pack "target")) FaceDownCharacteristics.defaultValue))
      " {\"type\":\"TurnFaceDown\",\"value\":{\"slot\":\"target\"}} "
  -- CR 708, and no payload beyond the slot: the permanent regains its own
  -- copiable values (CR 708.8), so there is nothing to list.
  Spec.it s "TurnFaceUp" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TurnFaceUp (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"TurnFaceUp\",\"value\":\"target\"} "
  -- CR 701.14a's pair, Prey Upon's two slots.
  Spec.it s "Fight" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Fight (Fight.MkFight (SlotName.MkSlotName (Text.pack "mine")) (SlotName.MkSlotName (Text.pack "theirs"))))
      " {\"type\":\"Fight\",\"value\":{\"first\":\"mine\",\"second\":\"theirs\"}} "
  Spec.it s "RemoveFromCombat" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemoveFromCombat (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"RemoveFromCombat\",\"value\":\"target\"} "
  Spec.it s "BecomesBlocked" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.BecomesBlocked (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"BecomesBlocked\",\"value\":\"target\"} "
  -- Both shapes in the pool: a pair, and a repeated phase.
  Spec.it s "AddPhases round-trips the pair and a repeated phase" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain])
      " {\"type\":\"AddPhases\",\"value\":[{\"type\":\"ExtraCombat\"},{\"type\":\"ExtraMain\"}]} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraCombat])
      " {\"type\":\"AddPhases\",\"value\":[{\"type\":\"ExtraCombat\"},{\"type\":\"ExtraCombat\"}]} "
  -- CR 724.1: nullary for TemptWithTheRing's reason -- rule 724.1's six steps
  -- fix the whole procedure, leaving an author nothing to write.
  Spec.it s "EndTurn" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.EndTurn
      " {\"type\":\"EndTurn\"} "
  Spec.it s "GainControl round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainControl (DurationRef.MkDurationRef Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
      " {\"type\":\"GainControl\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainControl (DurationRef.MkDurationRef Duration.Indefinite (ObjectRef.EachMatching (Filter.HasCardType CardType.Enchantment))))
      " {\"type\":\"GainControl\",\"value\":{\"duration\":{\"type\":\"Indefinite\"},\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Enchantment\"}}}}} "
  Spec.it s "GrantPlayFromExile round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled"))) ManaSpending.AsProduced))
      " {\"type\":\"GrantPlayFromExile\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile Duration.Indefinite (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) ManaSpending.AsProduced))
      " {\"type\":\"GrantPlayFromExile\",\"value\":{\"duration\":{\"type\":\"Indefinite\"},\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}}} "
  -- CR 118.14's rider, through the ARM rather than through the payload codec
  -- alone: the Effect layer is what a card's JSON actually goes through, and
  -- Dire Fleet Daredevil's clause has to survive it.
  Spec.it s "GrantPlayFromExile round-trips CR 118.14's spending rider" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled"))) ManaSpending.AnyType))
      " {\"type\":\"GrantPlayFromExile\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"spending\":{\"type\":\"AnyType\"}}} "
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
      ( Effect.ArmDelayedTrigger
          ArmDelayedTrigger.MkArmDelayedTrigger
            { ArmDelayedTrigger.name = sacrificeIt,
              ArmDelayedTrigger.onset = Onset.Immediately,
              ArmDelayedTrigger.duration = Nothing
            }
      )
      " {\"type\":\"ArmDelayedTrigger\",\"value\":{\"name\":\"sacrifice it\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.ArmDelayedTrigger
          ArmDelayedTrigger.MkArmDelayedTrigger
            { ArmDelayedTrigger.name = eachCombat,
              ArmDelayedTrigger.onset = Onset.Immediately,
              ArmDelayedTrigger.duration = Just Duration.UntilEndOfTurn
            }
      )
      " {\"type\":\"ArmDelayedTrigger\",\"value\":{\"name\":\"each combat\",\"duration\":{\"type\":\"UntilEndOfTurn\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.ArmDelayedTrigger
          ArmDelayedTrigger.MkArmDelayedTrigger
            { ArmDelayedTrigger.name = returnIt,
              ArmDelayedTrigger.onset = Onset.FromYourNextTurn,
              ArmDelayedTrigger.duration = Nothing
            }
      )
      " {\"type\":\"ArmDelayedTrigger\",\"value\":{\"name\":\"return it\",\"onset\":{\"type\":\"FromYourNextTurn\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.ArmDelayedTrigger
          ArmDelayedTrigger.MkArmDelayedTrigger
            { ArmDelayedTrigger.name = returnIt,
              ArmDelayedTrigger.onset = Onset.FromYourNextTurn,
              ArmDelayedTrigger.duration = Just Duration.UntilEndOfTurn
            }
      )
      " {\"type\":\"ArmDelayedTrigger\",\"value\":{\"name\":\"return it\",\"onset\":{\"type\":\"FromYourNextTurn\"},\"duration\":{\"type\":\"UntilEndOfTurn\"}}} "
  Spec.it s "AffectPlayers" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AffectPlayers (AffectPlayers.MkAffectPlayers Duration.UntilEndOfTurn (AffectedPlayers.Scoped PlayerScope.Opponents) PlayerEffect.CantCastSpells))
      " {\"type\":\"AffectPlayers\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"players\":{\"type\":\"Scoped\",\"value\":{\"type\":\"Opponents\"}},\"effect\":{\"type\":\"CantCastSpells\"}}} "
  -- The targeted seat, which is the arm no scope can say (Cease-Fire).
  Spec.it s "AffectPlayers at a named slot" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AffectPlayers (AffectPlayers.MkAffectPlayers Duration.UntilEndOfTurn (AffectedPlayers.Named (SlotName.MkSlotName (Text.pack "target"))) PlayerEffect.CantCastSpells))
      " {\"type\":\"AffectPlayers\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"players\":{\"type\":\"Named\",\"value\":\"target\"},\"effect\":{\"type\":\"CantCastSpells\"}}} "
  Spec.it s "RequireBlock" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RequireBlock (RequireBlock.MkRequireBlock Duration.UntilEndOfCombat (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature))))
      " {\"type\":\"RequireBlock\",\"value\":{\"duration\":{\"type\":\"UntilEndOfCombat\"},\"blocker\":{\"type\":\"InSlot\",\"value\":\"target\"},\"attacker\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}}} "
  Spec.it s "RequireAttack" $
    Common.assertCodec
      s
      codec
      (Effect.RequireAttack (RequireAttack.MkRequireAttack Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (PlayerRef.Relative PlayerRelation.You)))
      " {\"type\":\"RequireAttack\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"attacker\":{\"type\":\"InSlot\",\"value\":\"target\"},\"defender\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}}} "
  Spec.it s "CreateEmblem" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateEmblem (Text.pack "Goblin Piker"))
      " {\"type\":\"CreateEmblem\",\"value\":\"Goblin Piker\"} "
  -- Two different `card` values through the SAME constant codec, so a leak
  -- straight to the constructor (bypassing the codec argument) fails this
  -- rather than merely coincides.
  Spec.it s "CreateEmblem reaches its card only through the supplied codec" $
    Spec.assertEqWith
      s
      "the emblem payload comes from the argument, not the card"
      (Codec.encode (Effect.codec (constCodec sentinel)) (Effect.CreateEmblem (Text.pack "a wholly different card type")))
      (Codec.encode (Effect.codec (constCodec sentinel)) (Effect.CreateEmblem (0 :: Int)))
  Spec.it s "BecomeMonarch" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.BecomeMonarch MonarchTarget.TheController)
      " {\"type\":\"BecomeMonarch\",\"value\":{\"type\":\"TheController\"}} "
  Spec.it s "Designate Renowned" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Designate (Designate.MkDesignate Designation.Renowned (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Designate\",\"value\":{\"designation\":{\"type\":\"Renowned\"},\"slot\":\"self\"}} "
  Spec.it s "Designate Monstrous" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Designate (Designate.MkDesignate Designation.Monstrous (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Designate\",\"value\":{\"designation\":{\"type\":\"Monstrous\"},\"slot\":\"self\"}} "
  Spec.it s "Designate Suspected" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Designate (Designate.MkDesignate Designation.Suspected (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Designate\",\"value\":{\"designation\":{\"type\":\"Suspected\"},\"slot\":\"self\"}} "
  -- CR 716.2a's first half. Designate's shape with a number where that has a
  -- designation tag.
  Spec.it s "SetClassLevel" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SetClassLevel (SetClassLevel.MkSetClassLevel (ClassLevel.MkClassLevel 2) (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"SetClassLevel\",\"value\":{\"level\":2,\"slot\":\"self\"}} "
  -- CR 701.60a's ending, with an ObjectRef on the wire where Designate above
  -- writes a slot name directly: Eliminate the Impossible names a set rather
  -- than one permanent.
  Spec.it s "Unsuspect" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Unsuspect (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"Unsuspect\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  Spec.it s "Evolve" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Evolve (SlotName.MkSlotName (Text.pack "self")))
      " {\"type\":\"Evolve\",\"value\":\"self\"} "
  -- CR 702.134a's counter and CR 702.134c's marker. The slot is the ability's
  -- chosen target rather than "self", which is what parts it from Evolve above.
  Spec.it s "Mentor" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Mentor (SlotName.MkSlotName (Text.pack "mentored")))
      " {\"type\":\"Mentor\",\"value\":\"mentored\"} "
  -- CR 702.149a's counter and CR 702.149c's marker. Back to Evolve's "self": rule
  -- 702.149a puts its counter on the training creature itself.
  Spec.it s "Train" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Train (SlotName.MkSlotName (Text.pack "self")))
      " {\"type\":\"Train\",\"value\":\"self\"} "
  Spec.it s "ItBecomes" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ItBecomes Daytime.Night)
      " {\"type\":\"ItBecomes\",\"value\":{\"type\":\"Night\"}} "
  Spec.it s "ExileUntilMonarch" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ExileUntilMonarch (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"ExileUntilMonarch\",\"value\":\"target\"} "
  -- CR 702.55a's two ids: the card that is exiled, and the slot naming the
  -- creature it haunts.
  Spec.it s "ExileHaunting" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ExileHaunting (ExileHaunting.MkExileHaunting (SlotName.MkSlotName (Text.pack "became")) (SlotName.MkSlotName (Text.pack "haunted"))))
      " {\"type\":\"ExileHaunting\",\"value\":{\"card\":\"became\",\"host\":\"haunted\"}} "
  Spec.it s "PlaySubgame" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PlaySubgame (SlotName.MkSlotName (Text.pack "loser")))
      " {\"type\":\"PlaySubgame\",\"value\":\"loser\"} "
  Spec.it s "ChooseOpponent" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChooseOpponent (SlotName.MkSlotName (Text.pack "opponent")))
      " {\"type\":\"ChooseOpponent\",\"value\":\"opponent\"} "
  Spec.it s "ChooseOpponentAtRandom" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChooseOpponentAtRandom (SlotName.MkSlotName (Text.pack "opponent")))
      " {\"type\":\"ChooseOpponentAtRandom\",\"value\":\"opponent\"} "
  Spec.it s "ExileHandThenDraw" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.ExileHandThenDraw
      " {\"type\":\"ExileHandThenDraw\"} "
  Spec.it s "Proliferate" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.Proliferate
      " {\"type\":\"Proliferate\"} "
  -- CR 701.39a: the count alone, because rule 701.39a fixes the chooser, the kind
  -- of counter and the candidate pool, leaving an author only N to write.
  Spec.it s "Bolster" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Bolster (Quantity.Literal 3))
      " {\"type\":\"Bolster\",\"value\":{\"type\":\"Literal\",\"value\":3}} "
  -- CR 701.47a: the subtype and the count, which are rule 701.47a's two printed
  -- variables. The Army type and the token's other characteristics are the
  -- rulebook's and appear nowhere in card data.
  Spec.it s "Amass" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Amass (Amass.MkAmass (Quantity.Literal 3) Subtype.Zombie))
      " {\"type\":\"Amass\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":3},\"subtype\":{\"type\":\"Zombie\"}}} "
  -- CR 701.68a: who and how many, Draw's shape -- rule 701.68a fixes the kind of
  -- counter and the candidate pool, leaving an author the blighter and N.
  Spec.it s "Blight" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Blight (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)))
      " {\"type\":\"Blight\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":1}}} "
  -- CR 701.68a's "you" is whoever the instruction ADDRESSES: High Perfect Morcant's
  -- "each opponent blights 1" is the arm that needs the reference to be writable.
  Spec.it s "Blight for an opponent" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Blight (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.Opponent) (Quantity.Literal 2)))
      " {\"type\":\"Blight\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"Opponent\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
  -- CR 701.54a: nullary, because rule 701.54 fixes the chooser, the count and the
  -- qualification, leaving an author nothing to write.
  Spec.it s "TemptWithTheRing" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.TemptWithTheRing
      " {\"type\":\"TemptWithTheRing\"} "
  -- CR 701.49: nullary for TemptWithTheRing's reason -- rule 701.49 fixes the
  -- venturer and which dungeon, leaving an author nothing to write.
  Spec.it s "Venture" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.Venture
      " {\"type\":\"Venture\"} "
  Spec.it s "PlayerSacrifices" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices (SlotName.MkSlotName (Text.pack "t")) (Filter.HasCardType CardType.Creature) (Quantity.Literal 1)))
      " {\"type\":\"PlayerSacrifices\",\"value\":{\"slot\":\"t\",\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"quantity\":{\"type\":\"Literal\",\"value\":1}}} "
  -- CR 500.7: a slot read with an empty skip set, a self-scoped arm carrying
  -- CR 500.11's skip of one step, and a two-member set.
  Spec.it s "TakeExtraTurn" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Set.empty))
      " {\"type\":\"TakeExtraTurn\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"skips\":[]}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn (PlayerRef.Relative PlayerRelation.You) (Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap)))))
      " {\"type\":\"TakeExtraTurn\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"skips\":[{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Untap\"}}}]}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn PlayerRef.EachPlayer (Set.fromList [PhaseSelector.Step (Phase.Beginning BeginningStep.Untap), PhaseSelector.CombatPhase])))
      " {\"type\":\"TakeExtraTurn\",\"value\":{\"player\":{\"type\":\"EachPlayer\"},\"skips\":[{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Untap\"}}},{\"type\":\"CombatPhase\"}]}} "

-- The stand-in a parametricity test hands over in place of a real card codec:
-- any Value at all, so long as both instantiations are given the same one.
sentinel :: Value.Value
sentinel = Value.text (Text.pack "SENTINEL")

-- The artifact count the conditional self-replacement above (CR 614.15 / 616.1a)
-- reads; its "three or more" threshold lives in the Condition at the use site.
threeArtifacts :: Count.Count Quantity.Quantity
threeArtifacts =
  Count.MkCount
    (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
    (Filter.And [Filter.HasCardType CardType.Artifact, Filter.ControlledBy PlayerRelation.You])
    Aggregation.Members
