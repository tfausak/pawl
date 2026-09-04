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
import qualified Pawl.Types.AttachBound as AttachBound
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastObligation as CastObligation
import qualified Pawl.Types.CastOffer as CastOffer.Type
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.CoinReading as CoinReading
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamageDirection as DamageDirection
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePart as DamagePart
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
import qualified Pawl.Types.Draw as Draw
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
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ForbidBlock as ForbidBlock
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.InitiativeTarget as InitiativeTarget
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.MovedKinds as MovedKinds
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
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Sacrificer as Sacrificer
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
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

-- | And the `ability` parameter likewise: it too is reached only through the
-- codec supplied here, so any type proves the shape.
abilityCodec :: Codec.Codec Text.Text
abilityCodec = Common.text

codec :: Codec.Codec (Effect.Effect Text.Text Text.Text)
codec = Effect.codec cardCodec abilityCodec

toJson :: Effect.Effect Text.Text Text.Text -> Value.Value
toJson = Codec.encode codec

fromJson :: Value.Value -> Either Text.Text (Effect.Effect Text.Text Text.Text)
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
      (Effect.DealDamage (DealDamage.MkDealDamage (Seq.singleton (DamagePart.MkDamagePart (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3))) Nothing Nothing))
      " {\"type\":\"DealDamage\",\"value\":{\"parts\":[{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":3}}]}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (DealDamage.MkDealDamage (Seq.singleton (DamagePart.MkDamagePart (ObjectRef.EachMatching (Filter.HasKeyword Keyword.Flying)) (Quantity.Literal 1))) Nothing Nothing))
      " {\"type\":\"DealDamage\",\"value\":{\"parts\":[{\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasKeyword\",\"value\":{\"type\":\"Flying\"}}},\"quantity\":{\"type\":\"Literal\",\"value\":1}}]}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (DealDamage.MkDealDamage (Seq.singleton (DamagePart.MkDamagePart ObjectRef.EachPlayer (Quantity.Literal 2))) Nothing Nothing))
      " {\"type\":\"DealDamage\",\"value\":{\"parts\":[{\"ref\":{\"type\":\"EachPlayer\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}}]}} "
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
      (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) (ManaProduction.OfType (ManaType.Colored Color.Green)) ManaRetention.Ordinary Nothing Nothing))
      " {\"type\":\"AddMana\",\"value\":{\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) ManaProduction.AnyColor ManaRetention.Ordinary Nothing Nothing))
      " {\"type\":\"AddMana\",\"value\":{\"production\":{\"type\":\"AnyColor\"}}} "
  -- CR 106.4's other half: Shizuko, Caller of Autumn's "that player adds", where
  -- the recipient is written because CR 109.5's "you" is somebody else.
  Spec.it s "AddMana, a recipient the card names" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) (ManaProduction.OfType (ManaType.Colored Color.Green)) ManaRetention.Ordinary Nothing Nothing))
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
              Search.zones = Set.singleton Zone.Library,
              Search.quantity = Just (Quantity.Literal 2),
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
      (Effect.Sacrifice SacrificeEffect.MkSacrificeEffect {SacrificeEffect.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self")), SacrificeEffect.sacrificer = Sacrificer.EffectController})
      " {\"type\":\"Sacrifice\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"self\"}}} "
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
  -- CR 701.3a's third arrangement: two slot names and no Filter, since the
  -- destination is targeted rather than found as the effect resolves.
  Spec.it s "AttachBound" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AttachBound (AttachBound.MkAttachBound (SlotName.MkSlotName (Text.pack "became")) (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"AttachBound\",\"value\":{\"subject\":\"became\",\"destination\":\"target\"}} "
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
        attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Hand\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Exile EntryRiders.defaultValue (Just boundSlot) Nothing LibraryPlacement.defaultValue Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Exile\"},\"slot\":\"exiled\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone bound Zone.Battlefield attacking Nothing Nothing LibraryPlacement.defaultValue Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"zone\":{\"type\":\"Battlefield\"},\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone bound Zone.Battlefield attacking (Just boundSlot) Nothing LibraryPlacement.defaultValue Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"zone\":{\"type\":\"Battlefield\"},\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true},\"slot\":\"exiled\"}} "
    -- CR 113.6m's origin zone alone, the shape a card states when its effect
    -- moves its own source out of a named zone with nothing else to say.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing (Just Zone.Graveyard) LibraryPlacement.defaultValue Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Hand\"},\"origin\":{\"type\":\"Graveyard\"}}} "
    -- Reassembling Skeleton's own shape: riders AND an origin, two objects in a
    -- row, which only the type-directed read tells apart.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Battlefield attacking Nothing (Just Zone.Graveyard) LibraryPlacement.defaultValue Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Battlefield\"},\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true},\"origin\":{\"type\":\"Graveyard\"}}} "
    -- All four extras at once, so the encoder's order is pinned and the reader
    -- is shown to need none of it. The origin zone and the library position sit
    -- next to each other here, which is the pair only their disjoint tags tell
    -- apart.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Battlefield attacking (Just boundSlot) (Just Zone.Exile) (LibraryPlacement.Stated LibraryPosition.Top) Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Battlefield\"},\"riders\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true},\"slot\":\"exiled\",\"origin\":{\"type\":\"Exile\"},\"placement\":{\"type\":\"Stated\",\"value\":{\"type\":\"Top\"}}}} "
    -- Griptide's shape: a library destination with the end it arrives at, and
    -- nothing else. The position is the only extra, so this is what proves it is
    -- not read positionally as the riders.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Library EntryRiders.defaultValue Nothing Nothing (LibraryPlacement.Stated LibraryPosition.Top) Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Library\"},\"placement\":{\"type\":\"Stated\",\"value\":{\"type\":\"Top\"}}}} "
    -- And the default end is ELIDED, so Unsummon's two-element payload is
    -- unchanged by the field's arrival. Decoding that payload is what fills it
    -- back in.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Library EntryRiders.defaultValue Nothing Nothing (LibraryPlacement.Stated LibraryPosition.Bottom) Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"zone\":{\"type\":\"Library\"}}} "
    -- Evacuation's shape: an EachMatching ref in first position, which is an
    -- object where every case above is a string.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue Nothing))
      " {\"type\":\"MoveToZone\",\"value\":{\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"zone\":{\"type\":\"Hand\"}}} "
  -- Both of Draw's PlayerRef shapes: a controller draw and a targeted one.
  Spec.it s "Draw" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2) Nothing))
      " {\"type\":\"Draw\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (Draw.MkDraw (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3) Nothing))
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
      (Effect.Discard (Discard.Counted (CountedDiscard.MkCountedDiscard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 1) Nothing)))
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
  -- CR 702.179's speed, up and down. Synthetic Speed Boost's own value.
  Spec.it s "IncreaseSpeed" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity PlayerRef.EachPlayer (Quantity.Literal 2)))
      " {\"type\":\"IncreaseSpeed\",\"value\":{\"player\":{\"type\":\"EachPlayer\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}}} "
  -- Its own payload rather than the PlayerQuantity above, over the floor
  -- Spikeshell Harrier prints; see Pawl.Types.SpeedDecrease.
  Spec.it s "DecreaseSpeed" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DecreaseSpeed (SpeedDecrease.MkSpeedDecrease (PlayerRef.ControllerOfBound (SlotName.MkSlotName (Text.pack "permanent"))) (Quantity.Literal 1) 1))
      " {\"type\":\"DecreaseSpeed\",\"value\":{\"player\":{\"type\":\"ControllerOfBound\",\"value\":\"permanent\"},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"floor\":1}} "
  -- Create's EntryRiders, bound slot and CR 111.2 creator are each ELIDED when
  -- they are the default, exactly like MoveToZone above; the riders and the slot
  -- were once the middle two of four emitted forms, told apart at decode by JSON
  -- TYPE.
  Spec.it s "Create round-trips every combination of its three elided keys" $ do
    let attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}
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
  -- The count elided at one and the destination stated, which is what "conjure a
  -- card named Ornithopter into your hand" prints.
  Spec.it s "Conjure" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Conjure (Conjure.MkConjure Conjure.defaultQuantity (Text.pack "Ornithopter") ConjureDestination.Hand))
      " {\"type\":\"Conjure\",\"value\":{\"card\":\"Ornithopter\",\"destination\":{\"type\":\"Hand\"}}} "
  -- Toralf's Disciple's form: a stated count and a library.
  Spec.it s "Conjure with a stated count" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Conjure (Conjure.MkConjure (Quantity.Literal 4) (Text.pack "Lightning Bolt") ConjureDestination.Library))
      " {\"type\":\"Conjure\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":4},\"card\":\"Lightning Bolt\",\"destination\":{\"type\":\"Library\"}}} "
  -- Both ObjectRef arms have to survive. A count of one is elided, so both of
  -- these write the ref alone.
  Spec.it s "CreateCopy round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Literal 1) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) EntryRiders.defaultValue))
      " {\"type\":\"CreateCopy\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Literal 1) (ObjectRef.EachMatching (Filter.HasKeyword Keyword.Flying)) EntryRiders.defaultValue))
      " {\"type\":\"CreateCopy\",\"value\":{\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasKeyword\",\"value\":{\"type\":\"Flying\"}}}}} "
  -- Kicked Rite of Replication's five. Before #1305 this was a second payload
  -- SHAPE told apart by length; it is now the same shape with the defaulted key
  -- present.
  Spec.it s "CreateCopy round-trips a count above one" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Literal 5) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) EntryRiders.defaultValue))
      " {\"type\":\"CreateCopy\",\"value\":{\"quantity\":{\"type\":\"Literal\",\"value\":5},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
  -- Twincast's second sentence and Zada, Hedron Grinder's, against CR 707.10
  -- alone. The three fixtures differ in exactly the key that carries the
  -- answer, which is elided when the card prints none.
  Spec.it s "CopyStackObject round-trips each of CR 707.10's answers" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CopyStackObject (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) CopyTargets.ChosenByController))
      " {\"type\":\"CopyStackObject\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"},\"targets\":{\"type\":\"ChosenByController\"}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CopyStackObject (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) (CopyTargets.ForEach (ObjectRef.EachMatching (Filter.ControlledBy PlayerRelation.You)))))
      " {\"type\":\"CopyStackObject\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"},\"targets\":{\"type\":\"ForEach\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}}}}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CopyStackObject (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) CopyTargets.Copied))
      " {\"type\":\"CopyStackObject\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"}}} "
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
              Replace.effect = ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern Nothing Filter.IsSource Nothing Nothing Nothing Nothing) (DamageRewrite.SetAmount 4) Seq.empty)
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
      (Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage Duration.UntilEndOfTurn Nothing (Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))) Nothing Nothing Nothing (Quantity.Literal 4) Seq.empty))
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
              PreventNextDamage.ref = Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
              PreventNextDamage.whatRecipient = Nothing,
              PreventNextDamage.whoRecipient = Nothing,
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
                Seq.singleton (Effect.DealDamage (DealDamage.MkDealDamage (Seq.singleton (DamagePart.MkDamagePart (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "victim"))) (Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot (SlotName.MkSlotName (Text.pack "exiled")) Quantity.ManaValue)))) Nothing Nothing))
            }
      )
      " {\"type\":\"ForEach\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"victims\"},\"slot\":\"victim\",\"body\":[{\"type\":\"DealDamage\",\"value\":{\"parts\":[{\"ref\":{\"type\":\"InSlot\",\"value\":\"victim\"},\"quantity\":{\"type\":\"AgainstSlot\",\"value\":{\"slot\":\"exiled\",\"quantity\":{\"type\":\"ManaValue\"}}}}]}}]}} "
  -- CR 615.1: the same shield with no amount to spend (Selfless Squire).
  Spec.it s "PreventAllDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage Duration.UntilEndOfTurn Nothing (Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you")))) Nothing DamageDirection.DealtTo Nothing (Filter.And []) Seq.empty))
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
              PreventAllDamage.ref = Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
              PreventAllDamage.whatRecipient = Nothing,
              PreventAllDamage.direction = DamageDirection.DealtTo,
              PreventAllDamage.chosenSource = Nothing,
              PreventAllDamage.whatSource = Filter.And [],
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
              RedirectDamage.amount = Nothing,
              RedirectDamage.from = Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you"))),
              RedirectDamage.whatRecipient = Nothing,
              RedirectDamage.whoRecipient = Nothing,
              RedirectDamage.to = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
              RedirectDamage.chosenSource = Nothing
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
      (Effect.Counter (Counter.MkCounter (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) Nothing Nothing))
      " {\"type\":\"Counter\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"}}} "
  -- Swift Silence's "counter all other spells. Draw a card for each spell
  -- countered this way": the swept set and the slot the count is bound at, which
  -- is the pair a targeted counterspell writes neither of.
  Spec.it s "Counter over a swept set, binding what it countered" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Counter (Counter.MkCounter (ObjectRef.EachSpell (Filter.Not Filter.IsSource)) (Just (SlotName.MkSlotName (Text.pack "countered"))) Nothing))
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
  -- form: both keys written, where the case above elides the absent one.
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
  -- CR 701.24a on its own: the arm above with no ref at all, so the payload is a
  -- bare PlayerRef. Undercity's "then shuffle" is what writes it.
  Spec.it s "Shuffle" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Shuffle (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"Shuffle\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
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
  -- CR 122.5: a third tag, not the other two in sequence -- the move is atomic
  -- where the pair is not, so a card file must be able to say which it printed.
  Spec.it s "MoveCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveCounters (MoveCounters.MkMoveCounters (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))) (MovedKinds.Chosen (Quantity.Literal 1)) Nothing (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "became")))))
      " {\"type\":\"MoveCounters\",\"value\":{\"from\":{\"type\":\"InSlot\",\"value\":\"self\"},\"to\":{\"type\":\"InSlot\",\"value\":\"became\"}}} "
  -- CR 122.8: a fourth tag, and the only counter opcode whose payload names
  -- neither a kind nor a count -- the whole tally the read object had is what
  -- crosses.
  Spec.it s "PutCountersFrom" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom (SlotName.MkSlotName (Text.pack "self")) Nothing (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "creature")))))
      " {\"type\":\"PutCountersFrom\",\"value\":{\"from\":\"self\",\"ref\":{\"type\":\"InSlot\",\"value\":\"creature\"}}} "
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
  -- CR 701.28a. Transform's wire shape exactly, and the assertion that matters is
  -- that the two do not share a TAG: they resolve through one code path, so a
  -- collapsed encoding would be invisible everywhere else.
  Spec.it s "Convert round-trips both ObjectRef arms, and is not Transform" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Convert (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Convert\",\"value\":{\"type\":\"InSlot\",\"value\":\"self\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Convert (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"Convert\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
    Spec.assertBool
      s
      ( toJson (Effect.Convert (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
          /= toJson (Effect.Transform (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      )
      "Convert and Transform of the same slot encode differently"
  -- CR 701.42a. The slot Hanweir Battlements' own exile bound, plus the combined
  -- back face inline -- through the card codec, which here writes a bare name.
  -- Pawl.Codec.Effect is Arm.tagged, so a missing arm compiles with no round-trip
  -- test at all (#2262): this case is what catches one.
  Spec.it s "Meld" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Meld (Meld.MkMeld (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "melding"))) (Text.pack "Hanweir, the Writhing Township")))
      " {\"type\":\"Meld\",\"value\":{\"objects\":{\"type\":\"InSlot\",\"value\":\"melding\"},\"result\":\"Hanweir, the Writhing Township\"}} "
    -- The combined face reaches the wire only through the SUPPLIED card codec,
    -- CreateEmblem's case one payload over: two different `card` types encode
    -- alike through a constant codec, which a leak straight to the constructor
    -- would fail.
    Spec.assertEqWith
      s
      "the result payload comes from the argument, not the card"
      (Codec.encode (Effect.codec (constCodec sentinel) abilityCodec) (Effect.Meld (Meld.MkMeld (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "melding"))) (Text.pack "a wholly different card type"))))
      (Codec.encode (Effect.codec (constCodec sentinel) abilityCodec) (Effect.Meld (Meld.MkMeld (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "melding"))) (0 :: Int))))
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
      (Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) FaceDownCharacteristics.defaultValue))
      " {\"type\":\"TurnFaceDown\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
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
  -- CR 506.4. Both ObjectRef arms, since the pool prints one of each: Labyrinth
  -- of Skophos' "remove target creature from combat" is the slot, and Save
  -- Point's "remove each creature from combat" the CR 109.2 sweep.
  Spec.it s "RemoveFromCombat round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemoveFromCombat (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"RemoveFromCombat\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemoveFromCombat (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"RemoveFromCombat\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
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
  -- CR 724.2: nullary for the same reason, and hand-written for the reason every
  -- arm here is -- Arm.tagged's list ends in `_ -> Nothing`, so a constructor
  -- with no arm encodes to nothing and decodes from nothing without a warning
  -- (#2262).
  Spec.it s "EndCombatPhase" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.EndCombatPhase
      " {\"type\":\"EndCombatPhase\"} "
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
  -- CR 702.170c, a BARE ObjectRef where its neighbour above carries a record: rule
  -- 702.170d fixes the beneficiary and the duration this one would otherwise
  -- state. The two must not collapse into each other on the wire, since they
  -- write different fields of the same exiled object. data/cards prints the slot
  -- arm (Kellan Joins Up); the filter arm costs nothing.
  Spec.it s "MakePlotted round-trips both ObjectRef arms, and is not GrantPlayFromExile" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MakePlotted (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "plotted"))))
      " {\"type\":\"MakePlotted\",\"value\":{\"type\":\"InSlot\",\"value\":\"plotted\"}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MakePlotted (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      " {\"type\":\"MakePlotted\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
    Spec.assertBool
      s
      ( toJson (Effect.MakePlotted (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "plotted"))))
          /= toJson (Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile Duration.Indefinite (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "plotted"))) ManaSpending.AsProduced))
      )
      "MakePlotted and GrantPlayFromExile of the same slot encode differently"
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
  Spec.it s "CantBeRegenerated" $
    Common.assertCodec
      s
      codec
      (Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
      " {\"type\":\"CantBeRegenerated\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
  Spec.it s "RequireAttack" $
    Common.assertCodec
      s
      codec
      (Effect.RequireAttack (RequireAttack.MkRequireAttack Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (PlayerRef.Relative PlayerRelation.You)))
      " {\"type\":\"RequireAttack\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"attacker\":{\"type\":\"InSlot\",\"value\":\"target\"},\"defender\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}}} "
  Spec.it s "ForbidBlock" $
    Common.assertCodec
      s
      codec
      (Effect.ForbidBlock (ForbidBlock.MkForbidBlock Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
      " {\"type\":\"ForbidBlock\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
  Spec.it s "ForbidAttack" $
    Common.assertCodec
      s
      codec
      (Effect.ForbidAttack (ForbidAttack.MkForbidAttack Duration.UntilEndOfTurn (RestrictedCreatures.Named (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))) Nothing))
      " {\"type\":\"ForbidAttack\",\"value\":{\"duration\":{\"type\":\"UntilEndOfTurn\"},\"affected\":{\"type\":\"Named\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}}}} "
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
      (Codec.encode (Effect.codec (constCodec sentinel) abilityCodec) (Effect.CreateEmblem (Text.pack "a wholly different card type")))
      (Codec.encode (Effect.codec (constCodec sentinel) abilityCodec) (Effect.CreateEmblem (0 :: Int)))
  Spec.it s "BecomeMonarch" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.BecomeMonarch MonarchTarget.TheController)
      " {\"type\":\"BecomeMonarch\",\"value\":{\"type\":\"TheController\"}} "
  Spec.it s "TakeTheInitiative" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeTheInitiative InitiativeTarget.ControllerOfSource)
      " {\"type\":\"TakeTheInitiative\",\"value\":{\"type\":\"ControllerOfSource\"}} "
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
  -- CR 709.5g's lock. Keys to the House prints both settings, so the one wire
  -- key that tells them apart is what a round trip has to carry.
  Spec.it s "SetHalfLocked" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked False True (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"SetHalfLocked\",\"value\":{\"every\":false,\"locked\":true,\"slot\":\"target\"}} "
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
  Spec.it s "RollDie" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RollDie RollDie.MkRollDie {RollDie.sides = 20, RollDie.modifier = Nothing, RollDie.slot = SlotName.MkSlotName (Text.pack "result")})
      " {\"type\":\"RollDie\",\"value\":{\"sides\":20,\"slot\":\"result\"}} "
  -- CR 706.2's modifier, so the elided field above is not the only shape this
  -- arm round-trips.
  Spec.it s "RollDie with a modifier" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RollDie RollDie.MkRollDie {RollDie.sides = 20, RollDie.modifier = Just (Quantity.Literal 3), RollDie.slot = SlotName.MkSlotName (Text.pack "result")})
      " {\"type\":\"RollDie\",\"value\":{\"modifier\":{\"type\":\"Literal\",\"value\":3},\"sides\":20,\"slot\":\"result\"}} "
  Spec.it s "FlipCoin" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.FlipCoin FlipCoin.MkFlipCoin {FlipCoin.count = Quantity.Literal 1, FlipCoin.reading = CoinReading.Wins, FlipCoin.slot = SlotName.MkSlotName (Text.pack "flip"), FlipCoin.misses = Nothing})
      " {\"type\":\"FlipCoin\",\"value\":{\"slot\":\"flip\"}} "
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
  -- CR 201.4a's restriction on which names may be chosen, and nothing else: rule
  -- 109.5 fixes the chooser and rule 201.4 the count.
  Spec.it s "ChooseCardName" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChooseCardName (Filter.Not (Filter.HasCardType CardType.Land)))
      " {\"type\":\"ChooseCardName\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}} "
  -- CR 400.11c: the filter and CR 701.20a's reveal, Burning Wish's "reveal a
  -- sorcery card". Everything else about the sentence is the rule's -- the pool is
  -- the resolving controller's own (CR 108.3b) and the destination their hand.
  Spec.it s "FromOutsideTheGame" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame (Filter.HasCardType CardType.Sorcery) True))
      " {\"type\":\"FromOutsideTheGame\",\"value\":{\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Sorcery\"}},\"reveal\":true}} "
  -- CR 608.2n: no payload at all -- the spell exiling itself is the whole of it.
  Spec.it s "ExileThisSpell" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.ExileThisSpell
      " {\"type\":\"ExileThisSpell\"} "
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
  -- CR 701.49: the plain keyword action, whose payload is absent -- rule 701.49
  -- fixes the venturer, and CR 701.49a lets the player choose from every dungeon
  -- card they own, leaving an author nothing to write.
  Spec.it s "Venture" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Venture Nothing)
      " {\"type\":\"Venture\"} "
  -- CR 701.49d: the "venture into [quality]" variant, whose payload is CR 205.3p's
  -- dungeon type.
  Spec.it s "Venture into a quality" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Venture (Just Subtype.Undercity))
      " {\"type\":\"Venture\",\"value\":{\"type\":\"Undercity\"}} "
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
      (Effect.TakeExtraTurn TakeExtraTurn.MkTakeExtraTurn {TakeExtraTurn.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target")), TakeExtraTurn.skips = Set.empty, TakeExtraTurn.count = Quantity.Literal 1})
      " {\"type\":\"TakeExtraTurn\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"skips\":[]}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn TakeExtraTurn.MkTakeExtraTurn {TakeExtraTurn.player = PlayerRef.Relative PlayerRelation.You, TakeExtraTurn.skips = Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap)), TakeExtraTurn.count = Quantity.Literal 1})
      " {\"type\":\"TakeExtraTurn\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"skips\":[{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Untap\"}}}]}} "
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn TakeExtraTurn.MkTakeExtraTurn {TakeExtraTurn.player = PlayerRef.EachPlayer, TakeExtraTurn.skips = Set.fromList [PhaseSelector.Step (Phase.Beginning BeginningStep.Untap), PhaseSelector.CombatPhase], TakeExtraTurn.count = Quantity.Literal 1})
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
