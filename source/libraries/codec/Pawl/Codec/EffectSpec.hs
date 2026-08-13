{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EffectSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
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
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
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
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- | The `card` parameter is instantiated at 'Text.Text' throughout (and at
-- 'Int' in the parametricity case). 'Effect.toJson'/'Effect.fromJson' reach it
-- only through the supplied codec, so any type proves the shape.
cardToJson :: Text.Text -> Value.Value
cardToJson = Value.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: Effect.Effect Text.Text -> Value.Value
toJson = Effect.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (Effect.Effect Text.Text)
fromJson = Effect.fromJson cardFromJson

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Effect" $ do
  -- Every ObjectRef arm has to survive the trip through the payload.
  Spec.it s "DealDamage round-trips all three ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3)))
      """ {"type":"DealDamage","value":{"ref":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":3}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.EachMatching (Filter.HasKeyword Keyword.Flying)) (Quantity.Literal 1)))
      """ {"type":"DealDamage","value":{"ref":{"type":"EachMatching","value":{"type":"HasKeyword","value":{"type":"Flying"}}},"quantity":{"type":"Literal","value":1}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (DealDamage.MkDealDamage ObjectRef.EachPlayer (Quantity.Literal 2)))
      """ {"type":"DealDamage","value":{"ref":{"type":"EachPlayer"},"quantity":{"type":"Literal","value":2}}} """
  -- Both ObjectRef arms have to survive the trip through the payload.
  Spec.it s "ModifyTarget round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t")))))
      """ {"type":"ModifyTarget","value":{"duration":{"type":"UntilEndOfTurn"},"modification":{"type":"GainKeyword","value":{"type":"Trample"}},"ref":{"type":"InSlot","value":"t"}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.EachMatching (Filter.And [Filter.HasCardType CardType.Creature, Filter.IsAttacking]))))
      """ {"type":"ModifyTarget","value":{"duration":{"type":"UntilEndOfTurn"},"modification":{"type":"GainKeyword","value":{"type":"Trample"}},"ref":{"type":"EachMatching","value":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"IsAttacking"}]}}}} """
  Spec.it s "ChangeText, a basic land type swap that forbids nothing" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChangeText (ChangeText.MkChangeText SubtypeFamily.BasicLandType Set.empty (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"ChangeText","value":{"family":{"type":"BasicLandType"},"forbidden":[],"slot":"target"}} """
  Spec.it s "ChangeText, a creature type swap whose new word can't be Wall" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChangeText (ChangeText.MkChangeText SubtypeFamily.CreatureType (Set.singleton Subtype.Wall) (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"ChangeText","value":{"family":{"type":"CreatureType"},"forbidden":[{"type":"Wall"}],"slot":"target"}} """
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
      (Effect.Search (PlayerRef.Relative PlayerRelation.You) (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2) (Filter.HasCardType CardType.Land) SearchDestination.BattlefieldTapped)
      """ {"type":"Search","value":[{"type":"Relative","value":{"type":"You"}},{"type":"InSlot","value":"target"},{"type":"Literal","value":2},{"type":"HasCardType","value":{"type":"Land"}},{"type":"BattlefieldTapped"}]} """
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
      (Effect.Destroy (Destroy.MkDestroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.Regenerable Nothing))
      """ {"type":"Destroy","value":{"ref":{"type":"InSlot","value":"t"},"regenerability":{"type":"Regenerable"}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (Destroy.MkDestroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.CantBeRegenerated Nothing))
      """ {"type":"Destroy","value":{"ref":{"type":"InSlot","value":"t"},"regenerability":{"type":"CantBeRegenerated"}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing))
      """ {"type":"Destroy","value":{"ref":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}},"regenerability":{"type":"Regenerable"}}} """
  -- The slot the sweep binds its count into is ELIDED when absent, so the case
  -- above writes two keys and this one writes three. Since #1305 that is
  -- Fields.defaulted omitting a key, not a trailing array element recovered by
  -- JSON type.
  Spec.it s "Destroy's bound-count slot round-trips and is written only when present" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Artifact)) Regenerability.Regenerable (Just (SlotName.MkSlotName (Text.pack "destroyed")))))
      """ {"type":"Destroy","value":{"ref":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Artifact"}}},"regenerability":{"type":"Regenerable"},"slot":"destroyed"}} """
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
      (Effect.AttachTarget (AttachTarget.MkAttachTarget (SlotName.MkSlotName (Text.pack "target")) (Filter.HasCardType CardType.Creature)))
      """ {"type":"AttachTarget","value":{"slot":"target","filter":{"type":"HasCardType","value":{"type":"Creature"}}}} """
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
        attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False}
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue))
      """ {"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Hand"}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Exile EntryRiders.defaultValue (Just boundSlot) Nothing LibraryPlacement.defaultValue))
      """ {"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Exile"},"slot":"exiled"}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone bound Zone.Battlefield attacking Nothing Nothing LibraryPlacement.defaultValue))
      """ {"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"exiled"},"zone":{"type":"Battlefield"},"riders":{"tapped":{"type":"Tapped"},"attacking":true}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone bound Zone.Battlefield attacking (Just boundSlot) Nothing LibraryPlacement.defaultValue))
      """ {"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"exiled"},"zone":{"type":"Battlefield"},"riders":{"tapped":{"type":"Tapped"},"attacking":true},"slot":"exiled"}} """
    -- CR 113.6m's origin zone alone, the shape a card states when its effect
    -- moves its own source out of a named zone with nothing else to say.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing (Just Zone.Graveyard) LibraryPlacement.defaultValue))
      """ {"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Hand"},"origin":{"type":"Graveyard"}}} """
    -- Reassembling Skeleton's own shape: riders AND an origin, two objects in a
    -- row, which only the type-directed read tells apart.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Battlefield attacking Nothing (Just Zone.Graveyard) LibraryPlacement.defaultValue))
      """ {"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Battlefield"},"riders":{"tapped":{"type":"Tapped"},"attacking":true},"origin":{"type":"Graveyard"}}} """
    -- All four extras at once, so the encoder's order is pinned and the reader
    -- is shown to need none of it. The origin zone and the library position sit
    -- next to each other here, which is the pair only their disjoint tags tell
    -- apart.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Battlefield attacking (Just boundSlot) (Just Zone.Exile) (LibraryPlacement.Stated LibraryPosition.Top)))
      """ {"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Battlefield"},"riders":{"tapped":{"type":"Tapped"},"attacking":true},"slot":"exiled","origin":{"type":"Exile"},"placement":{"type":"Stated","value":{"type":"Top"}}}} """
    -- Griptide's shape: a library destination with the end it arrives at, and
    -- nothing else. The position is the only extra, so this is what proves it is
    -- not read positionally as the riders.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Library EntryRiders.defaultValue Nothing Nothing (LibraryPlacement.Stated LibraryPosition.Top)))
      """ {"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Library"},"placement":{"type":"Stated","value":{"type":"Top"}}}} """
    -- And the default end is ELIDED, so Unsummon's two-element payload is
    -- unchanged by the field's arrival. Decoding that payload is what fills it
    -- back in.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone slot Zone.Library EntryRiders.defaultValue Nothing Nothing (LibraryPlacement.Stated LibraryPosition.Bottom)))
      """ {"type":"MoveToZone","value":{"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Library"}}} """
    -- Evacuation's shape: an EachMatching ref in first position, which is an
    -- object where every case above is a string.
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue))
      """ {"type":"MoveToZone","value":{"ref":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}},"zone":{"type":"Hand"}}} """
  -- Both of Draw's PlayerRef shapes: a controller draw and a targeted one.
  Spec.it s "Draw" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)))
      """ {"type":"Draw","value":{"player":{"type":"Relative","value":{"type":"You"}},"quantity":{"type":"Literal","value":2}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3)))
      """ {"type":"Draw","value":{"player":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":3}}} """
  -- Both of Scry's PlayerRef shapes: Crystal Ball's controller scry and
  -- Kozilek's Command's "target player scries 2".
  Spec.it s "Scry" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Scry (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)))
      """ {"type":"Scry","value":{"player":{"type":"Relative","value":{"type":"You"}},"quantity":{"type":"Literal","value":2}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Scry (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2)))
      """ {"type":"Scry","value":{"player":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":2}}} """
  -- Curate's "Surveil 2", and a distinct tag from Scry's above: the two carry the
  -- same payload and differ only in where the unwanted cards go, so a shared tag
  -- would decode one card's text as the other's.
  Spec.it s "Surveil" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Surveil (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)))
      """ {"type":"Surveil","value":{"player":{"type":"Relative","value":{"type":"You"}},"quantity":{"type":"Literal","value":2}}} """
  -- Spin into Myth's "then fateseal 2". The PlayerRef is the FATESEALER, so the
  -- controller spelling is the one a card writes; the opponent whose library is
  -- looked at is chosen as the effect applies and appears nowhere in the data.
  Spec.it s "Fateseal" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Fateseal (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)))
      """ {"type":"Fateseal","value":{"player":{"type":"Relative","value":{"type":"You"}},"quantity":{"type":"Literal","value":2}}} """
  -- Merfolk Branchwalker's "it explores", against the trigger-source slot, and
  -- the swept-set shape the ObjectRef also admits.
  Spec.it s "Explore" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Explore (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      """ {"type":"Explore","value":{"type":"InSlot","value":"self"}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Explore (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      """ {"type":"Explore","value":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  Spec.it s "Mill" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Mill (Mill.MkMill (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2) Nothing))
      """ {"type":"Mill","value":{"player":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":2}}} """
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
          )
      )
      """ {"type":"Mill","value":{"player":{"type":"Relative","value":{"type":"You"}},"quantity":{"type":"Literal","value":2},"tally":{"slot":"milled","filter":{"type":"Not","value":{"type":"HasCardType","value":{"type":"Land"}}}}}} """
  Spec.it s "Discard" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Discard (Discard.MkDiscard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 1)))
      """ {"type":"Discard","value":{"slot":"target","quantity":{"type":"Literal","value":1}}} """
  Spec.it s "LoseLife" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.LoseLife (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2)))
      """ {"type":"LoseLife","value":{"player":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":2}}} """
  Spec.it s "GainLife" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainLife (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)))
      """ {"type":"GainLife","value":{"player":{"type":"Relative","value":{"type":"You"}},"quantity":{"type":"Literal","value":1}}} """
  Spec.it s "ExchangeLifeTotals" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ExchangeLifeTotals (ExchangeSides.WithController (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"ExchangeLifeTotals","value":{"type":"WithController","value":"target"}} """
  Spec.it s "ExchangeLifeTotals between two targets" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ExchangeLifeTotals (ExchangeSides.BetweenTargets (SlotName.MkSlotName (Text.pack "players"))))
      """ {"type":"ExchangeLifeTotals","value":{"type":"BetweenTargets","value":"players"}} """
  Spec.it s "SetLifeTotal" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 10)))
      """ {"type":"SetLifeTotal","value":{"player":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":1e1}}} """
  -- Nullary: the whole choice is the resolving controller's, so there is nothing
  -- for card data to carry.
  Spec.it s "RedistributeLifeTotals" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.RedistributeLifeTotals
      """ {"type":"RedistributeLifeTotals"} """
  -- Create's EntryRiders and bound slot are each ELIDED when they are the
  -- default, exactly like MoveToZone above: four emitted forms, the middle two
  -- told apart at decode by JSON TYPE.
  Spec.it s "Create round-trips all four shapes, and elides the defaults" $ do
    let attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False}
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
  -- Both ObjectRef arms have to survive. A count of one is elided, so both of
  -- these write the ref alone.
  Spec.it s "CreateCopy round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Literal 1) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
      """ {"type":"CreateCopy","value":{"ref":{"type":"InSlot","value":"target"}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Literal 1) (ObjectRef.EachMatching (Filter.HasKeyword Keyword.Flying))))
      """ {"type":"CreateCopy","value":{"ref":{"type":"EachMatching","value":{"type":"HasKeyword","value":{"type":"Flying"}}}}} """
  -- Kicked Rite of Replication's five. Before #1305 this was a second payload
  -- SHAPE told apart by length; it is now the same shape with the defaulted key
  -- present.
  Spec.it s "CreateCopy round-trips a count above one" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Literal 5) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
      """ {"type":"CreateCopy","value":{"quantity":{"type":"Literal","value":5},"ref":{"type":"InSlot","value":"target"}}} """
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
          (Just (Condition.Compares (Compares.MkCompares (Quantity.Count threeArtifacts) Comparison.AtLeast (Quantity.Literal 3))))
          (ReplacementEffect.DamageR (DamagePattern.MkDamagePattern Nothing Filter.IsSource Nothing) (DamageRewrite.SetAmount 4))
      )
      """ {"type":"Replace","value":[{"type":"UntilEndOfTurn"},{"type":"Once"},{"type":"SelfReplacement"},{"type":"Compares","value":{"measured":{"type":"Count","value":{"scope":{"type":"InZone","value":[{"type":"Battlefield"},{"type":"EachPlayer"}]},"filter":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Artifact"}},{"type":"ControlledBy","value":{"type":"You"}}]},"aggregation":{"type":"Members"}}},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":3}}},{"type":"DamageR","value":[{"whatSource":{"type":"IsSource"}},{"type":"SetAmount","value":4}]}]} """
  -- CR 614.10a: a slot read, plus the whole-phase selector -- the arm a Phase
  -- alone cannot spell (CR 500.1).
  Spec.it s "SkipNextPhase" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep))))
      """ {"type":"SkipNextPhase","value":{"player":{"type":"InSlot","value":"target"},"selector":{"type":"Step","value":{"type":"Beginning","value":{"type":"DrawStep"}}}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) PhaseSelector.CombatPhase))
      """ {"type":"SkipNextPhase","value":{"player":{"type":"InSlot","value":"target"},"selector":{"type":"CombatPhase"}}} """
  -- CR 615.7.
  Spec.it s "PreventNextDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PreventNextDamage Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 4) Seq.empty)
      """ {"type":"PreventNextDamage","value":[{"type":"UntilEndOfTurn"},{"type":"InSlot","value":"target"},{"type":"Literal","value":4}]} """
  -- CR 615.5's additional effect, the fourth element (Test of Faith). Elided
  -- above, where it is empty; nested here, so the recursion into an effect
  -- inside an effect is round-tripped.
  Spec.it s "PreventNextDamage with a CR 615.5 rider" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.PreventNextDamage
          Duration.UntilEndOfTurn
          (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
          (Quantity.Literal 3)
          (Seq.singleton (Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.InSlot (SlotName.MkSlotName (Text.pack "thatMuch"))) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))))
      )
      """ {"type":"PreventNextDamage","value":[{"type":"UntilEndOfTurn"},{"type":"InSlot","value":"target"},{"type":"Literal","value":3},[{"type":"PutCounters","value":{"kind":{"type":"PlusOnePlusOne"},"quantity":{"type":"InSlot","value":"thatMuch"},"ref":{"type":"InSlot","value":"target"}}}]]} """
  -- CR 615.1: the same shield with no amount to spend (Selfless Squire).
  Spec.it s "PreventAllDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PreventAllDamage (DurationRef.MkDurationRef Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you")))))
      """ {"type":"PreventAllDamage","value":{"duration":{"type":"UntilEndOfTurn"},"ref":{"type":"InSlot","value":"you"}}} """
  -- CR 614.9: Turn the Tables, whose kind field is PRINTED ("all combat
  -- damage") and whose two refs are the source side then the destination.
  Spec.it s "RedirectDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RedirectDamage Duration.UntilEndOfTurn (Just DamageKind.Combat) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you"))) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"RedirectDamage","value":[{"type":"UntilEndOfTurn"},{"type":"Combat"},{"type":"InSlot","value":"you"},{"type":"InSlot","value":"target"}]} """
  -- CR 113.9: this opcode counters an ability as well as a spell, with the type
  -- unchanged, so the wire shape is too.
  Spec.it s "Counter" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Counter (SlotName.MkSlotName (Text.pack "spell")))
      """ {"type":"Counter","value":"spell"} """
  -- CR 701.24: an ObjectRef, tagged InSlot around the slot name.
  -- Riftsweeper's shape -- the library is derived from the objects it names (CR
  -- 400.3), so there is no second field to write.
  Spec.it s "ShuffleIntoLibrary" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ShuffleIntoLibrary Nothing (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"ShuffleIntoLibrary","value":{"type":"InSlot","value":"target"}} """
  -- CR 701.24c's named library (Dwell on the Past's "their library"): the pair
  -- form, an ARRAY where a lone ObjectRef is a tagged object -- which is what
  -- tells the two apart.
  Spec.it s "ShuffleIntoLibrary naming the library" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Effect.ShuffleIntoLibrary
          (Just (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "player"))))
          (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "cards")))
      )
      """ {"type":"ShuffleIntoLibrary","value":[{"type":"InSlot","value":"player"},{"type":"InSlot","value":"cards"}]} """
  Spec.it s "PutCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "creature")))))
      """ {"type":"PutCounters","value":{"kind":{"type":"PlusOnePlusOne"},"quantity":{"type":"Literal","value":1},"ref":{"type":"InSlot","value":"creature"}}} """
  -- The ObjectRef's other arm: a Filter is an object where a slot is a string, so
  -- the widening left every card's spelling alone (Pawl.Codec.ObjectRef).
  Spec.it s "PutCounters over a swept set" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (ObjectRef.EachMatching (Filter.HasDesignation Designation.Renowned))))
      """ {"type":"PutCounters","value":{"kind":{"type":"PlusOnePlusOne"},"quantity":{"type":"Literal","value":1},"ref":{"type":"EachMatching","value":{"type":"HasDesignation","value":{"type":"Renowned"}}}}} """
  -- CR 122: PutCounters' mirror, and a distinct tag -- a signed amount under one
  -- tag would make the two indistinguishable in a card file.
  Spec.it s "RemoveCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.MinusOneMinusOne (Quantity.Literal 1) (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"RemoveCounters","value":{"kind":{"type":"MinusOneMinusOne"},"quantity":{"type":"Literal","value":1},"slot":"target"}} """
  -- Every PlayerRef shape the opcode accepts: the self-scoped one, and the slot
  -- read CR 702.70a needs.
  Spec.it s "GainPlayerCounters" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2)))
      """ {"type":"GainPlayerCounters","value":{"player":{"type":"Relative","value":{"type":"You"}},"kind":{"type":"Energy"},"quantity":{"type":"Literal","value":2}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) PlayerCounterKind.Poison (Quantity.Literal 3)))
      """ {"type":"GainPlayerCounters","value":{"player":{"type":"InSlot","value":"thatPlayer"},"kind":{"type":"Poison"},"quantity":{"type":"Literal","value":3}}} """
  -- The mirror opcode, on the same wire shape and a DIFFERENT tag: CR 728.1's
  -- removal must never decode as a gain.
  Spec.it s "RemovePlayerCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Rad (Quantity.InSlot (SlotName.MkSlotName (Text.pack "milled")))))
      """ {"type":"RemovePlayerCounters","value":{"player":{"type":"Relative","value":{"type":"You"}},"kind":{"type":"Rad"},"quantity":{"type":"InSlot","value":"milled"}}} """
  -- CR 701.26a's Tap is Untap's mirror and shares its wire shape, so the two
  -- must not collapse into one tag.
  Spec.it s "Tap round-trips, and is not Untap" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"Tap","value":{"type":"InSlot","value":"target"}} """
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
      """ {"type":"Untap","value":{"type":"InSlot","value":"target"}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Untap (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      """ {"type":"Untap","value":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  -- CR 701.27a. Both ObjectRef arms, since the pool prints one of each shape's
  -- twin: Thraben Gargoyle's "transform this creature" is the slot, and a
  -- "transform all X" sweep is the filter.
  Spec.it s "Transform round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Transform (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      """ {"type":"Transform","value":{"type":"InSlot","value":"self"}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Transform (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      """ {"type":"Transform","value":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  -- CR 708.2a. One slot and no ObjectRef, since Backslide names a target and
  -- nothing in the pool sweeps a set face down.
  Spec.it s "TurnFaceDown" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TurnFaceDown (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"TurnFaceDown","value":"target"} """
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
      (Effect.GainControl (DurationRef.MkDurationRef Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))))
      """ {"type":"GainControl","value":{"duration":{"type":"UntilEndOfTurn"},"ref":{"type":"InSlot","value":"target"}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainControl (DurationRef.MkDurationRef Duration.Indefinite (ObjectRef.EachMatching (Filter.HasCardType CardType.Enchantment))))
      """ {"type":"GainControl","value":{"duration":{"type":"Indefinite"},"ref":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Enchantment"}}}}} """
  Spec.it s "GrantPlayFromExile round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GrantPlayFromExile (DurationRef.MkDurationRef Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")))))
      """ {"type":"GrantPlayFromExile","value":{"duration":{"type":"UntilEndOfTurn"},"ref":{"type":"InSlot","value":"exiled"}}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GrantPlayFromExile (DurationRef.MkDurationRef Duration.Indefinite (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature))))
      """ {"type":"GrantPlayFromExile","value":{"duration":{"type":"Indefinite"},"ref":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}}}} """
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
      (Effect.AffectPlayers (AffectPlayers.MkAffectPlayers Duration.UntilEndOfTurn (AffectedPlayers.Scoped PlayerScope.Opponents) PlayerEffect.CantCastSpells))
      """ {"type":"AffectPlayers","value":{"duration":{"type":"UntilEndOfTurn"},"players":{"type":"Scoped","value":{"type":"Opponents"}},"effect":{"type":"CantCastSpells"}}} """
  -- The targeted seat, which is the arm no scope can say (Cease-Fire).
  Spec.it s "AffectPlayers at a named slot" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AffectPlayers (AffectPlayers.MkAffectPlayers Duration.UntilEndOfTurn (AffectedPlayers.Named (SlotName.MkSlotName (Text.pack "target"))) PlayerEffect.CantCastSpells))
      """ {"type":"AffectPlayers","value":{"duration":{"type":"UntilEndOfTurn"},"players":{"type":"Named","value":"target"},"effect":{"type":"CantCastSpells"}}} """
  Spec.it s "RequireBlock" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RequireBlock (RequireBlock.MkRequireBlock Duration.UntilEndOfCombat (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature))))
      """ {"type":"RequireBlock","value":{"duration":{"type":"UntilEndOfCombat"},"blocker":{"type":"InSlot","value":"target"},"attacker":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}}}} """
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
  Spec.it s "Designate Renowned" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Designate (Designate.MkDesignate Designation.Renowned (SlotName.MkSlotName (Text.pack "self"))))
      """ {"type":"Designate","value":{"designation":{"type":"Renowned"},"slot":"self"}} """
  Spec.it s "Designate Monstrous" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Designate (Designate.MkDesignate Designation.Monstrous (SlotName.MkSlotName (Text.pack "self"))))
      """ {"type":"Designate","value":{"designation":{"type":"Monstrous"},"slot":"self"}} """
  Spec.it s "Designate Suspected" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Designate (Designate.MkDesignate Designation.Suspected (SlotName.MkSlotName (Text.pack "self"))))
      """ {"type":"Designate","value":{"designation":{"type":"Suspected"},"slot":"self"}} """
  -- CR 701.60a's ending, with an ObjectRef on the wire where Designate above
  -- writes a slot name directly: Eliminate the Impossible names a set rather
  -- than one permanent.
  Spec.it s "Unsuspect" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Unsuspect (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      """ {"type":"Unsuspect","value":{"type":"EachMatching","value":{"type":"HasCardType","value":{"type":"Creature"}}}} """
  Spec.it s "Evolve" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Evolve (SlotName.MkSlotName (Text.pack "self")))
      """ {"type":"Evolve","value":"self"} """
  -- CR 702.134a's counter and CR 702.134c's marker. The slot is the ability's
  -- chosen target rather than "self", which is what parts it from Evolve above.
  Spec.it s "Mentor" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Mentor (SlotName.MkSlotName (Text.pack "mentored")))
      """ {"type":"Mentor","value":"mentored"} """
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
  -- CR 702.55a's two ids: the card that is exiled, and the slot naming the
  -- creature it haunts.
  Spec.it s "ExileHaunting" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ExileHaunting (ExileHaunting.MkExileHaunting (SlotName.MkSlotName (Text.pack "became")) (SlotName.MkSlotName (Text.pack "haunted"))))
      """ {"type":"ExileHaunting","value":{"card":"became","host":"haunted"}} """
  Spec.it s "PlaySubgame" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PlaySubgame (SlotName.MkSlotName (Text.pack "loser")))
      """ {"type":"PlaySubgame","value":"loser"} """
  Spec.it s "ChooseOpponent" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChooseOpponent (SlotName.MkSlotName (Text.pack "opponent")))
      """ {"type":"ChooseOpponent","value":"opponent"} """
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
  -- CR 701.49: nullary for TemptWithTheRing's reason -- rule 701.49 fixes the
  -- venturer and which dungeon, leaving an author nothing to write.
  Spec.it s "Venture" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Effect.Venture
      """ {"type":"Venture"} """
  Spec.it s "PlayerSacrifices" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices (SlotName.MkSlotName (Text.pack "t")) (Filter.HasCardType CardType.Creature) (Quantity.Literal 1)))
      """ {"type":"PlayerSacrifices","value":{"slot":"t","filter":{"type":"HasCardType","value":{"type":"Creature"}},"quantity":{"type":"Literal","value":1}}} """
  -- CR 500.7: a slot read with an empty skip set, a self-scoped arm carrying
  -- CR 500.11's skip of one step, and a two-member set.
  Spec.it s "TakeExtraTurn" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Set.empty))
      """ {"type":"TakeExtraTurn","value":{"player":{"type":"InSlot","value":"target"},"skips":[]}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn (PlayerRef.Relative PlayerRelation.You) (Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap)))))
      """ {"type":"TakeExtraTurn","value":{"player":{"type":"Relative","value":{"type":"You"}},"skips":[{"type":"Step","value":{"type":"Beginning","value":{"type":"Untap"}}}]}} """
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn PlayerRef.EachPlayer (Set.fromList [PhaseSelector.Step (Phase.Beginning BeginningStep.Untap), PhaseSelector.CombatPhase])))
      """ {"type":"TakeExtraTurn","value":{"player":{"type":"EachPlayer"},"skips":[{"type":"Step","value":{"type":"Beginning","value":{"type":"Untap"}}},{"type":"CombatPhase"}]}} """

-- The stand-in a parametricity test hands over in place of a real card codec:
-- any Value at all, so long as both instantiations are given the same one.
sentinel :: Value.Value
sentinel = Value.text (Text.pack "SENTINEL")

-- The artifact count the conditional self-replacement above (CR 614.15 / 616.1a)
-- reads; its "three or more" threshold lives in the Condition at the use site.
threeArtifacts :: Count.Count Quantity.Quantity
threeArtifacts =
  Count.MkCount
    (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
    (Filter.And [Filter.HasCardType CardType.Artifact, Filter.ControlledBy PlayerRelation.You])
    Aggregation.Members
