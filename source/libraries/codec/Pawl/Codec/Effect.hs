module Pawl.Codec.Effect where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.ExtraPhase as ExtraPhase
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaProduction as ManaProduction
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.Codec.MonarchTarget as MonarchTarget
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Onset as Onset
import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.Regenerability as Regenerability
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.SearchDestination as SearchDestination
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.Uses as Uses
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Onset as Onset

toJson :: (card -> Value.Value) -> Effect.Effect card -> Value.Value
toJson codec e = case e of
  Effect.DealDamage r q -> Common.tagged "DealDamage" (Just (Common.array [ObjectRef.toJson r, Quantity.toJson q]))
  Effect.ModifyTarget d m r -> Common.tagged "ModifyTarget" (Just (Common.array [Duration.toJson d, Modification.toJson m, ObjectRef.toJson r]))
  Effect.ChangeText s -> Common.tagged "ChangeText" (Just (SlotName.toJson s))
  Effect.AddMana production -> Common.tagged "AddMana" (Just (ManaProduction.toJson production))
  Effect.Search f d -> Common.tagged "Search" (Just (Common.array [Filter.toJson Keyword.toJson f, SearchDestination.toJson d]))
  Effect.ExileAllGraveyards -> Common.nullary "ExileAllGraveyards"
  Effect.Proliferate -> Common.nullary "Proliferate"
  Effect.ExileHandThenDraw -> Common.nullary "ExileHandThenDraw"
  Effect.PlayerSacrifices slot f q -> Common.tagged "PlayerSacrifices" (Just (Common.array [SlotName.toJson slot, Filter.toJson Keyword.toJson f, Quantity.toJson q]))
  Effect.RestartGame -> Common.nullary "RestartGame"
  Effect.ControlPlayerNextTurn s -> Common.tagged "ControlPlayerNextTurn" (Just (SlotName.toJson s))
  -- The bound-count slot is ELIDED when absent, the posture Create's EntryRiders
  -- and ArmDelayedTrigger's duration both take, so every card that says nothing about
  -- counting its sweep stays byte-for-byte as it was written.
  Effect.Destroy s r ms ->
    Common.tagged "Destroy" . Just . Common.array $
      [ObjectRef.toJson s, Regenerability.toJson r] <> fmap SlotName.toJson (Maybe.maybeToList ms)
  Effect.Sacrifice s -> Common.tagged "Sacrifice" (Just (SlotName.toJson s))
  Effect.RemoveFromCombat s -> Common.tagged "RemoveFromCombat" (Just (SlotName.toJson s))
  Effect.Counter s -> Common.tagged "Counter" (Just (SlotName.toJson s))
  -- MoveToZone's payload is positional and takes Create's shape, for the same
  -- reason and with the same two elisions: the EntryRiders are dropped when they
  -- are the CR 110.5b default and the bound slot when there is none, so every
  -- card file written before either existed stays byte-for-byte as it was. The
  -- three-element form is two shapes, told apart on decode by JSON TYPE -- a
  -- slot name is a string, riders are an object.
  Effect.MoveToZone s z riders ms ->
    Common.tagged "MoveToZone" . Just . Common.array $
      [SlotName.toJson s, Zone.toJson z]
        <> (if riders == EntryRiders.defaultValue then [] else [EntryRiders.toJson riders])
        <> fmap SlotName.toJson (Maybe.maybeToList ms)
  Effect.Draw r q -> Common.tagged "Draw" (Just (Common.array [PlayerRef.toJson r, Quantity.toJson q]))
  Effect.Mill s q -> Common.tagged "Mill" (Just (Common.array [SlotName.toJson s, Quantity.toJson q]))
  Effect.Discard s q -> Common.tagged "Discard" (Just (Common.array [SlotName.toJson s, Quantity.toJson q]))
  Effect.LoseLife r q -> Common.tagged "LoseLife" (Just (Common.array [PlayerRef.toJson r, Quantity.toJson q]))
  Effect.GainLife r q -> Common.tagged "GainLife" (Just (Common.array [PlayerRef.toJson r, Quantity.toJson q]))
  -- Create's payload is positional, and the EntryRiders are ELIDED when they are
  -- the CR 110.5b default (EntryRiders.defaultValue) -- the same posture
  -- `counterability` takes, so a card file that says nothing about how its
  -- tokens enter stays exactly as it was written. The three-element form is
  -- therefore two shapes, told apart on decode by JSON TYPE rather than by
  -- position: a slot name is a string (SlotName.toJson) and riders are an object,
  -- so the two can never be confused.
  Effect.Create q c te ms ->
    Common.tagged "Create" . Just . Common.array $
      [Quantity.toJson q, codec c]
        <> (if te == EntryRiders.defaultValue then [] else [EntryRiders.toJson te])
        <> fmap SlotName.toJson (Maybe.maybeToList ms)
  Effect.Replace d u re -> Common.tagged "Replace" (Just (Common.array [Duration.toJson d, Uses.toJson u, ReplacementEffect.toJson re]))
  Effect.SkipNextPhase r sel -> Common.tagged "SkipNextPhase" (Just (Common.array [PlayerRef.toJson r, PhaseSelector.toJson sel]))
  Effect.PutCounters k q s -> Common.tagged "PutCounters" (Just (Common.array [CounterKind.toJson k, Quantity.toJson q, SlotName.toJson s]))
  Effect.GainPlayerCounters r k q -> Common.tagged "GainPlayerCounters" (Just (Common.array [PlayerRef.toJson r, PlayerCounterKind.toJson k, Quantity.toJson q]))
  Effect.Tap r -> Common.tagged "Tap" (Just (ObjectRef.toJson r))
  Effect.Untap r -> Common.tagged "Untap" (Just (ObjectRef.toJson r))
  Effect.AddPhases ps -> Common.tagged "AddPhases" (Just (Common.array (fmap ExtraPhase.toJson ps)))
  Effect.GainControl d r -> Common.tagged "GainControl" (Just (Common.array [Duration.toJson d, ObjectRef.toJson r]))
  -- The duration is ELIDED when absent, which is CR 603.7b's default -- so
  -- Tidal Wave's one-shot entry stays a bare ability name and only a card that
  -- states a duration writes the two-element form. The ONSET is elided when it is CR 603.7a's default (the ability is armed the
  -- moment it is created), so Tidal Wave's bare name and Full Throttle's
  -- two-element form both stay exactly as they were written. A stated onset
  -- takes the THREE-element form, whose last element is the duration or null --
  -- and LENGTH, not JSON type, is what tells the forms apart, because an onset
  -- and a duration are both tagged objects.
  Effect.ArmDelayedTrigger n o md -> Common.tagged "ArmDelayedTrigger" . Just $ case (o, md) of
    (Onset.Immediately, Nothing) -> AbilityName.toJson n
    (Onset.Immediately, Just d) -> Common.array [AbilityName.toJson n, Duration.toJson d]
    _ -> Common.array [AbilityName.toJson n, Onset.toJson o, Common.encodeMaybe Duration.toJson md]
  Effect.AffectPlayers d s pe -> Common.tagged "AffectPlayers" (Just (Common.array [Duration.toJson d, PlayerScope.toJson s, PlayerEffect.toJson pe]))
  Effect.CreateEmblem c -> Common.tagged "CreateEmblem" (Just (codec c))
  Effect.BecomeMonarch t -> Common.tagged "BecomeMonarch" (Just (MonarchTarget.toJson t))
  Effect.ExileUntilMonarch s -> Common.tagged "ExileUntilMonarch" (Just (SlotName.toJson s))
  Effect.Attach s -> Common.tagged "Attach" (Just (SlotName.toJson s))
  Effect.AttachTarget s f -> Common.tagged "AttachTarget" (Just (Common.array [SlotName.toJson s, Filter.toJson Keyword.toJson f]))
  Effect.PlaySubgame s -> Common.tagged "PlaySubgame" (Just (SlotName.toJson s))
  Effect.TakeExtraTurn r skips -> Common.tagged "TakeExtraTurn" (Just (Common.array [PlayerRef.toJson r, Common.encodeSet PhaseSelector.toJson skips]))
  -- A bare slot name, not an array: whose library is DERIVED from the object
  -- that slot names (CR 701.24 / "its owner"), so there is no second field for
  -- a card file to write. The Counter and Sacrifice shape.
  Effect.ShuffleIntoLibrary s -> Common.tagged "ShuffleIntoLibrary" (Just (SlotName.toJson s))

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Effect.Effect card)
fromJson decode value = do
  (t, mv) <- Common.asTagged value
  case t of
    "DealDamage" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.DealDamage <$> ObjectRef.fromJson r <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "DealDamage expects [objectRef, quantity]"
    "ModifyTarget" -> case mv of
      Just (Value.Array (Array.MkArray [d, m, r])) -> Effect.ModifyTarget <$> Duration.fromJson d <*> Modification.fromJson m <*> ObjectRef.fromJson r
      _ -> Left . Text.pack $ "ModifyTarget expects [duration, modification, objectRef]"
    "ChangeText" -> Common.withValue mv (fmap Effect.ChangeText . SlotName.fromJson)
    "AddMana" -> Common.withValue mv (fmap Effect.AddMana . ManaProduction.fromJson)
    "Search" -> case mv of
      Just (Value.Array (Array.MkArray [f, d])) -> Effect.Search <$> Filter.fromJson Keyword.fromJson f <*> SearchDestination.fromJson d
      _ -> Left . Text.pack $ "Search expects [filter, destination]"
    "ExileAllGraveyards" -> Right Effect.ExileAllGraveyards
    "Proliferate" -> Right Effect.Proliferate
    "ExileHandThenDraw" -> Right Effect.ExileHandThenDraw
    "PlayerSacrifices" -> case mv of
      Just (Value.Array (Array.MkArray [sv, fv, qv])) -> Effect.PlayerSacrifices <$> SlotName.fromJson sv <*> Filter.fromJson Keyword.fromJson fv <*> Quantity.fromJson qv
      _ -> Left . Text.pack $ "PlayerSacrifices expects [slot, filter, quantity]"
    "RestartGame" -> Right Effect.RestartGame
    "ControlPlayerNextTurn" -> Common.withValue mv (fmap Effect.ControlPlayerNextTurn . SlotName.fromJson)
    "Destroy" -> case mv of
      Just (Value.Array (Array.MkArray [sv, rv])) -> Effect.Destroy <$> ObjectRef.fromJson sv <*> Regenerability.fromJson rv <*> pure Nothing
      Just (Value.Array (Array.MkArray [sv, rv, nv])) -> Effect.Destroy <$> ObjectRef.fromJson sv <*> Regenerability.fromJson rv <*> (Just <$> SlotName.fromJson nv)
      _ -> Left . Text.pack $ "Destroy expects [objectRef, regenerability], optionally with a slot"
    "Sacrifice" -> Common.withValue mv (fmap Effect.Sacrifice . SlotName.fromJson)
    "RemoveFromCombat" -> Common.withValue mv (fmap Effect.RemoveFromCombat . SlotName.fromJson)
    "Counter" -> Common.withValue mv (fmap Effect.Counter . SlotName.fromJson)
    -- The four shapes the encoder above can emit, read exactly as Create's are:
    -- an Object in third position is the EntryRiders, anything else is the bound
    -- slot name (a string).
    "MoveToZone" -> case mv of
      Just (Value.Array (Array.MkArray [s, z])) -> Effect.MoveToZone <$> SlotName.fromJson s <*> Zone.fromJson z <*> pure EntryRiders.defaultValue <*> pure Nothing
      Just (Value.Array (Array.MkArray [s, z, e@(Value.Object _)])) -> Effect.MoveToZone <$> SlotName.fromJson s <*> Zone.fromJson z <*> EntryRiders.fromJson e <*> pure Nothing
      Just (Value.Array (Array.MkArray [s, z, b])) -> Effect.MoveToZone <$> SlotName.fromJson s <*> Zone.fromJson z <*> pure EntryRiders.defaultValue <*> (Just <$> SlotName.fromJson b)
      Just (Value.Array (Array.MkArray [s, z, e, b])) -> Effect.MoveToZone <$> SlotName.fromJson s <*> Zone.fromJson z <*> EntryRiders.fromJson e <*> (Just <$> SlotName.fromJson b)
      _ -> Left . Text.pack $ "MoveToZone expects [slot, zone], optionally with EntryRiders and/or a slot"
    "Draw" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.Draw <$> PlayerRef.fromJson r <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "Draw expects [playerRef, quantity]"
    "Mill" -> case mv of
      Just (Value.Array (Array.MkArray [s, q])) -> Effect.Mill <$> SlotName.fromJson s <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "Mill expects [slot, quantity]"
    "Discard" -> case mv of
      Just (Value.Array (Array.MkArray [s, q])) -> Effect.Discard <$> SlotName.fromJson s <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "Discard expects [slot, quantity]"
    "LoseLife" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.LoseLife <$> PlayerRef.fromJson r <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "LoseLife expects [playerRef, quantity]"
    "GainLife" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.GainLife <$> PlayerRef.fromJson r <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "GainLife expects [playerRef, quantity]"
    -- The four shapes the encoder above can emit. The three-element one is read
    -- by JSON type: an Object is the EntryRiders, anything else is the slot name
    -- (a string), which is what lets the entry be elided when it is the default
    -- without a hole in the array.
    "Create" -> case mv of
      Just (Value.Array (Array.MkArray [q, c])) -> Effect.Create <$> Quantity.fromJson q <*> decode c <*> pure EntryRiders.defaultValue <*> pure Nothing
      Just (Value.Array (Array.MkArray [q, c, e@(Value.Object _)])) -> Effect.Create <$> Quantity.fromJson q <*> decode c <*> EntryRiders.fromJson e <*> pure Nothing
      Just (Value.Array (Array.MkArray [q, c, s])) -> Effect.Create <$> Quantity.fromJson q <*> decode c <*> pure EntryRiders.defaultValue <*> (Just <$> SlotName.fromJson s)
      Just (Value.Array (Array.MkArray [q, c, e, s])) -> Effect.Create <$> Quantity.fromJson q <*> decode c <*> EntryRiders.fromJson e <*> (Just <$> SlotName.fromJson s)
      _ -> Left . Text.pack $ "Create expects [Quantity, Card], optionally with EntryRiders and/or a slot"
    -- The three shapes the encoder above can emit, told apart by LENGTH.
    "ArmDelayedTrigger" -> case mv of
      Just (Value.Array (Array.MkArray [n, o, d])) ->
        Effect.ArmDelayedTrigger <$> AbilityName.fromJson n <*> Onset.fromJson o <*> Common.decodeMaybe Duration.fromJson d
      Just (Value.Array (Array.MkArray [n, d])) ->
        Effect.ArmDelayedTrigger <$> AbilityName.fromJson n <*> pure Onset.Immediately <*> fmap Just (Duration.fromJson d)
      _ -> Common.withValue mv (fmap (\n -> Effect.ArmDelayedTrigger n Onset.Immediately Nothing) . AbilityName.fromJson)
    "Replace" -> case mv of
      Just (Value.Array (Array.MkArray [d, u, re])) -> do
        duration <- Duration.fromJson d
        uses <- Uses.fromJson u
        effect <- ReplacementEffect.fromJson re
        pure (Effect.Replace duration uses effect)
      _ -> Left . Text.pack $ "Replace expects [Duration, Uses, ReplacementEffect]"
    "SkipNextPhase" -> case mv of
      Just (Value.Array (Array.MkArray [r, sel])) -> Effect.SkipNextPhase <$> PlayerRef.fromJson r <*> PhaseSelector.fromJson sel
      _ -> Left . Text.pack $ "SkipNextPhase expects [playerRef, phaseSelector]"
    "PutCounters" -> case mv of
      Just (Value.Array (Array.MkArray [k, q, s])) -> Effect.PutCounters <$> CounterKind.fromJson k <*> Quantity.fromJson q <*> SlotName.fromJson s
      _ -> Left . Text.pack $ "PutCounters expects [counterKind, quantity, slot]"
    "GainPlayerCounters" -> case mv of
      Just (Value.Array (Array.MkArray [r, k, q])) -> Effect.GainPlayerCounters <$> PlayerRef.fromJson r <*> PlayerCounterKind.fromJson k <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "GainPlayerCounters expects [playerRef, playerCounterKind, quantity]"
    "Tap" -> Common.withValue mv (fmap Effect.Tap . ObjectRef.fromJson)
    "Untap" -> Common.withValue mv (fmap Effect.Untap . ObjectRef.fromJson)
    "AddPhases" -> case mv of
      Just (Value.Array (Array.MkArray ps)) -> Effect.AddPhases <$> traverse ExtraPhase.fromJson ps
      _ -> Left . Text.pack $ "AddPhases expects [ExtraPhase]"
    "GainControl" -> case mv of
      Just (Value.Array (Array.MkArray [d, r])) -> Effect.GainControl <$> Duration.fromJson d <*> ObjectRef.fromJson r
      _ -> Left . Text.pack $ "GainControl expects [duration, objectRef]"
    "AffectPlayers" -> case mv of
      Just (Value.Array (Array.MkArray [d, s, pe])) -> Effect.AffectPlayers <$> Duration.fromJson d <*> PlayerScope.fromJson s <*> PlayerEffect.fromJson pe
      _ -> Left . Text.pack $ "AffectPlayers expects [Duration, PlayerScope, PlayerEffect]"
    "CreateEmblem" -> Common.withValue mv (fmap Effect.CreateEmblem . decode)
    "BecomeMonarch" -> Common.withValue mv (fmap Effect.BecomeMonarch . MonarchTarget.fromJson)
    "ExileUntilMonarch" -> Common.withValue mv (fmap Effect.ExileUntilMonarch . SlotName.fromJson)
    "Attach" -> Common.withValue mv (fmap Effect.Attach . SlotName.fromJson)
    "AttachTarget" -> case mv of
      Just (Value.Array (Array.MkArray [s, f])) -> Effect.AttachTarget <$> SlotName.fromJson s <*> Filter.fromJson Keyword.fromJson f
      _ -> Left . Text.pack $ "AttachTarget expects [slot, filter]"
    "PlaySubgame" -> Common.withValue mv (fmap Effect.PlaySubgame . SlotName.fromJson)
    "TakeExtraTurn" -> case mv of
      Just (Value.Array (Array.MkArray [r, skips])) -> Effect.TakeExtraTurn <$> PlayerRef.fromJson r <*> Common.decodeSet PhaseSelector.fromJson skips
      _ -> Left . Text.pack $ "TakeExtraTurn expects [playerRef, phaseSelectors]"
    "ShuffleIntoLibrary" -> Common.withValue mv (fmap Effect.ShuffleIntoLibrary . SlotName.fromJson)
    _ -> Left . Text.pack $ "unknown Effect: " <> t
