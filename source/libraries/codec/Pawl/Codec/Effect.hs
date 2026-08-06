module Pawl.Codec.Effect where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Daytime as Daytime
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
import qualified Pawl.Codec.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Codec.SearchDestination as SearchDestination
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.SubtypeFamily as SubtypeFamily
import qualified Pawl.Codec.Uses as Uses
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Effect as Effect
-- These type modules share an alias with their codec module, the posture Onset
-- already took here: the names never collide, since a codec module exports
-- functions and a type module exports the type moveTail's signature needs.
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

toJson :: (card -> Value.Value) -> Effect.Effect card -> Value.Value
toJson codec e = case e of
  Effect.DealDamage r q -> Common.tagged "DealDamage" (Just (Common.array [ObjectRef.toJson r, Quantity.toJson q]))
  Effect.ModifyTarget d m r -> Common.tagged "ModifyTarget" (Just (Common.array [Duration.toJson d, Modification.toJson m, ObjectRef.toJson r]))
  Effect.ChangeText family forbidden s ->
    Common.tagged "ChangeText" . Just . Common.array $
      [SubtypeFamily.toJson family, Common.encodeSet Subtype.toJson forbidden, SlotName.toJson s]
  Effect.AddMana production -> Common.tagged "AddMana" (Just (ManaProduction.toJson production))
  Effect.Search f d -> Common.tagged "Search" (Just (Common.array [Filter.toJson Keyword.toJson f, SearchDestination.toJson d]))
  Effect.ExileAllGraveyards -> Common.nullary "ExileAllGraveyards"
  Effect.Proliferate -> Common.nullary "Proliferate"
  Effect.TemptWithTheRing -> Common.nullary "TemptWithTheRing"
  Effect.ExileHandThenDraw -> Common.nullary "ExileHandThenDraw"
  Effect.PlayerSacrifices slot f q -> Common.tagged "PlayerSacrifices" (Just (Common.array [SlotName.toJson slot, Filter.toJson Keyword.toJson f, Quantity.toJson q]))
  Effect.RestartGame -> Common.nullary "RestartGame"
  Effect.ControlPlayerNextTurn s -> Common.tagged "ControlPlayerNextTurn" (Just (SlotName.toJson s))
  -- The bound-count slot is ELIDED when absent, so a card that says nothing
  -- about counting its sweep keeps its two-element payload.
  Effect.Destroy s r ms ->
    Common.tagged "Destroy" . Just . Common.array $
      [ObjectRef.toJson s, Regenerability.toJson r] <> fmap SlotName.toJson (Maybe.maybeToList ms)
  Effect.Sacrifice s -> Common.tagged "Sacrifice" (Just (SlotName.toJson s))
  Effect.RemoveFromCombat s -> Common.tagged "RemoveFromCombat" (Just (SlotName.toJson s))
  Effect.Counter s -> Common.tagged "Counter" (Just (SlotName.toJson s))
  -- MoveToZone's payload is the slot and the destination zone, then three
  -- independently elided extras where Create has two: the EntryRiders are
  -- dropped when they are the CR 110.5b default, the bound slot when there is
  -- none, and CR 113.6m's origin zone when the effect states none. Everything
  -- after the destination is
  -- therefore optional, and told apart on decode by JSON TYPE rather than by
  -- position -- a slot name is a string, a zone is a tagged object, and the
  -- riders are an object that is not one. `moveTail` is the decoding half.
  Effect.MoveToZone s z riders ms mo ->
    Common.tagged "MoveToZone" . Just . Common.array $
      [SlotName.toJson s, Zone.toJson z]
        <> (if riders == EntryRiders.defaultValue then [] else [EntryRiders.toJson riders])
        <> fmap SlotName.toJson (Maybe.maybeToList ms)
        <> fmap Zone.toJson (Maybe.maybeToList mo)
  Effect.Draw r q -> Common.tagged "Draw" (Just (Common.array [PlayerRef.toJson r, Quantity.toJson q]))
  Effect.Mill s q -> Common.tagged "Mill" (Just (Common.array [SlotName.toJson s, Quantity.toJson q]))
  Effect.Discard s q -> Common.tagged "Discard" (Just (Common.array [SlotName.toJson s, Quantity.toJson q]))
  Effect.LoseLife r q -> Common.tagged "LoseLife" (Just (Common.array [PlayerRef.toJson r, Quantity.toJson q]))
  Effect.GainLife r q -> Common.tagged "GainLife" (Just (Common.array [PlayerRef.toJson r, Quantity.toJson q]))
  Effect.IncreaseSpeed r q -> Common.tagged "IncreaseSpeed" (Just (Common.array [PlayerRef.toJson r, Quantity.toJson q]))
  -- Create's payload is positional, and the EntryRiders are ELIDED when they
  -- are the CR 110.5b default. The three-element form is therefore two shapes,
  -- told apart on decode by JSON TYPE rather than by position: a slot name is a
  -- string and riders are an object, so the two can never be confused.
  Effect.Create q c te ms ->
    Common.tagged "Create" . Just . Common.array $
      [Quantity.toJson q, codec c]
        <> (if te == EntryRiders.defaultValue then [] else [EntryRiders.toJson te])
        <> fmap SlotName.toJson (Maybe.maybeToList ms)
  Effect.Replace d u o c re ->
    Common.tagged "Replace" . Just . Common.array $
      [Duration.toJson d, Uses.toJson u, ReplacementOrigin.toJson o, Common.encodeMaybe Condition.toJson c, ReplacementEffect.toJson re]
  Effect.SkipNextPhase r sel -> Common.tagged "SkipNextPhase" (Just (Common.array [PlayerRef.toJson r, PhaseSelector.toJson sel]))
  Effect.PreventNextDamage d r q -> Common.tagged "PreventNextDamage" (Just (Common.array [Duration.toJson d, ObjectRef.toJson r, Quantity.toJson q]))
  Effect.PreventAllDamage d r -> Common.tagged "PreventAllDamage" (Just (Common.array [Duration.toJson d, ObjectRef.toJson r]))
  Effect.PutCounters k q s -> Common.tagged "PutCounters" (Just (Common.array [CounterKind.toJson k, Quantity.toJson q, SlotName.toJson s]))
  Effect.GainPlayerCounters r k q -> Common.tagged "GainPlayerCounters" (Just (Common.array [PlayerRef.toJson r, PlayerCounterKind.toJson k, Quantity.toJson q]))
  Effect.Tap r -> Common.tagged "Tap" (Just (ObjectRef.toJson r))
  Effect.Untap r -> Common.tagged "Untap" (Just (ObjectRef.toJson r))
  Effect.Transform r -> Common.tagged "Transform" (Just (ObjectRef.toJson r))
  Effect.AddPhases ps -> Common.tagged "AddPhases" (Just (Common.array (fmap ExtraPhase.toJson ps)))
  Effect.GainControl d r -> Common.tagged "GainControl" (Just (Common.array [Duration.toJson d, ObjectRef.toJson r]))
  -- The duration is ELIDED when absent (CR 603.7b's default) and the onset when
  -- it is CR 603.7a's default, so a one-shot entry stays a bare ability name
  -- and a stated duration alone writes the two-element form. A stated onset
  -- takes the THREE-element form, whose last element is the duration or null.
  -- LENGTH, not JSON type, tells the forms apart: an onset and a duration are
  -- both tagged objects.
  Effect.ArmDelayedTrigger n o md -> Common.tagged "ArmDelayedTrigger" . Just $ case (o, md) of
    (Onset.Immediately, Nothing) -> AbilityName.toJson n
    (Onset.Immediately, Just d) -> Common.array [AbilityName.toJson n, Duration.toJson d]
    _ -> Common.array [AbilityName.toJson n, Onset.toJson o, Common.encodeMaybe Duration.toJson md]
  Effect.AffectPlayers d s pe -> Common.tagged "AffectPlayers" (Just (Common.array [Duration.toJson d, PlayerScope.toJson s, PlayerEffect.toJson pe]))
  Effect.CreateEmblem c -> Common.tagged "CreateEmblem" (Just (codec c))
  Effect.BecomeMonarch t -> Common.tagged "BecomeMonarch" (Just (MonarchTarget.toJson t))
  Effect.ItBecomes d -> Common.tagged "ItBecomes" (Just (Daytime.toJson d))
  Effect.ExileUntilMonarch s -> Common.tagged "ExileUntilMonarch" (Just (SlotName.toJson s))
  Effect.Attach s -> Common.tagged "Attach" (Just (SlotName.toJson s))
  Effect.AttachTarget s f -> Common.tagged "AttachTarget" (Just (Common.array [SlotName.toJson s, Filter.toJson Keyword.toJson f]))
  Effect.PlaySubgame s -> Common.tagged "PlaySubgame" (Just (SlotName.toJson s))
  Effect.TakeExtraTurn r skips -> Common.tagged "TakeExtraTurn" (Just (Common.array [PlayerRef.toJson r, Common.encodeSet PhaseSelector.toJson skips]))
  -- A bare slot name, not an array: the library is derived from the object that
  -- slot names (CR 701.24), so there is no second field to write.
  Effect.ShuffleIntoLibrary s -> Common.tagged "ShuffleIntoLibrary" (Just (SlotName.toJson s))

-- Everything a MoveToZone payload may carry after its slot and its destination
-- zone: the EntryRiders (CR 110.5b), the slot binding the destination
-- incarnation (CR 400.7), and the origin zone the effect names (CR 113.6m).
-- Each is optional, so they are read by JSON TYPE rather than by position -- a
-- string is the slot, and an object is the origin zone if it decodes as one and
-- the riders otherwise.
--
-- ZONE FIRST is what makes that order-independent rather than merely ordered:
-- EntryRiders.fromJson defaults every field it does not find, so it would accept
-- a zone object and silently return the default riders, while Zone.fromJson
-- accepts nothing but its own tagged shape.
--
-- A REPEATED element is an error rather than last-one-wins. Two origin zones is
-- a card file saying something CR 113.6m's "a particular zone" cannot mean, and
-- two of anything else is as likely a typo.
moveTail :: [Value.Value] -> Either Text.Text (EntryRiders.EntryRiders, Maybe SlotName.SlotName, Maybe Zone.Zone)
moveTail = go Nothing Nothing Nothing
  where
    go mRiders mSlot mOrigin values = case values of
      [] -> Right (Maybe.fromMaybe EntryRiders.defaultValue mRiders, mSlot, mOrigin)
      v : rest -> case v of
        Value.String _ -> case mSlot of
          Just _ -> Left . Text.pack $ "MoveToZone names two bound slots"
          Nothing -> do
            slot <- SlotName.fromJson v
            go mRiders (Just slot) mOrigin rest
        _ -> case Zone.fromJson v of
          Right zone -> case mOrigin of
            Just _ -> Left . Text.pack $ "MoveToZone names two origin zones"
            Nothing -> go mRiders mSlot (Just zone) rest
          Left _ -> case mRiders of
            Just _ -> Left . Text.pack $ "MoveToZone names two sets of entry riders"
            Nothing -> do
              riders <- EntryRiders.fromJson v
              go (Just riders) mSlot mOrigin rest

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
    "ChangeText" -> case mv of
      Just (Value.Array (Array.MkArray [fv, xv, sv])) ->
        Effect.ChangeText <$> SubtypeFamily.fromJson fv <*> Common.decodeSet Subtype.fromJson xv <*> SlotName.fromJson sv
      _ -> Left . Text.pack $ "ChangeText expects [subtypeFamily, forbiddenSubtypes, slot]"
    "AddMana" -> Common.withValue mv (fmap Effect.AddMana . ManaProduction.fromJson)
    "Search" -> case mv of
      Just (Value.Array (Array.MkArray [f, d])) -> Effect.Search <$> Filter.fromJson Keyword.fromJson f <*> SearchDestination.fromJson d
      _ -> Left . Text.pack $ "Search expects [filter, destination]"
    "ExileAllGraveyards" -> Right Effect.ExileAllGraveyards
    "Proliferate" -> Right Effect.Proliferate
    "TemptWithTheRing" -> Right Effect.TemptWithTheRing
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
    -- Read by JSON TYPE and not by position, which is `moveTail`'s whole job:
    -- Create's third-position rule would not survive CR 113.6m's origin zone,
    -- since that zone and the EntryRiders are both objects.
    "MoveToZone" -> case mv of
      Just (Value.Array (Array.MkArray (s : z : rest))) -> do
        (riders, mSlot, mOrigin) <- moveTail rest
        Effect.MoveToZone <$> SlotName.fromJson s <*> Zone.fromJson z <*> pure riders <*> pure mSlot <*> pure mOrigin
      _ -> Left . Text.pack $ "MoveToZone expects [slot, zone], optionally with EntryRiders, a slot and/or an origin zone"
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
    "IncreaseSpeed" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.IncreaseSpeed <$> PlayerRef.fromJson r <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "IncreaseSpeed expects [playerRef, quantity]"
    -- The three-element form is read by JSON type: an Object is the
    -- EntryRiders, anything else is the slot name, which is what lets the
    -- riders be elided without leaving a hole in the array.
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
      Just (Value.Array (Array.MkArray [d, u, o, c, re])) -> do
        duration <- Duration.fromJson d
        uses <- Uses.fromJson u
        origin <- ReplacementOrigin.fromJson o
        condition <- Common.decodeMaybe Condition.fromJson c
        effect <- ReplacementEffect.fromJson re
        pure (Effect.Replace duration uses origin condition effect)
      _ -> Left . Text.pack $ "Replace expects [Duration, Uses, ReplacementOrigin, Maybe Condition, ReplacementEffect]"
    "SkipNextPhase" -> case mv of
      Just (Value.Array (Array.MkArray [r, sel])) -> Effect.SkipNextPhase <$> PlayerRef.fromJson r <*> PhaseSelector.fromJson sel
      _ -> Left . Text.pack $ "SkipNextPhase expects [playerRef, phaseSelector]"
    "PreventNextDamage" -> case mv of
      Just (Value.Array (Array.MkArray [d, r, q])) -> Effect.PreventNextDamage <$> Duration.fromJson d <*> ObjectRef.fromJson r <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "PreventNextDamage expects [Duration, ObjectRef, Quantity]"
    "PreventAllDamage" -> case mv of
      Just (Value.Array (Array.MkArray [d, r])) -> Effect.PreventAllDamage <$> Duration.fromJson d <*> ObjectRef.fromJson r
      _ -> Left . Text.pack $ "PreventAllDamage expects [Duration, ObjectRef]"
    "PutCounters" -> case mv of
      Just (Value.Array (Array.MkArray [k, q, s])) -> Effect.PutCounters <$> CounterKind.fromJson k <*> Quantity.fromJson q <*> SlotName.fromJson s
      _ -> Left . Text.pack $ "PutCounters expects [counterKind, quantity, slot]"
    "GainPlayerCounters" -> case mv of
      Just (Value.Array (Array.MkArray [r, k, q])) -> Effect.GainPlayerCounters <$> PlayerRef.fromJson r <*> PlayerCounterKind.fromJson k <*> Quantity.fromJson q
      _ -> Left . Text.pack $ "GainPlayerCounters expects [playerRef, playerCounterKind, quantity]"
    "Tap" -> Common.withValue mv (fmap Effect.Tap . ObjectRef.fromJson)
    "Untap" -> Common.withValue mv (fmap Effect.Untap . ObjectRef.fromJson)
    "Transform" -> Common.withValue mv (fmap Effect.Transform . ObjectRef.fromJson)
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
    "ItBecomes" -> Common.withValue mv (fmap Effect.ItBecomes . Daytime.fromJson)
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
