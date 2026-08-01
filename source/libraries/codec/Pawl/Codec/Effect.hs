-- | The @Effect ⇆ Json@ codec (#481).
module Pawl.Codec.Effect where

import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import Pawl.Codec.CounterKind (counterKindToJson, jsonToCounterKind)
import Pawl.Codec.Duration (durationToJson, jsonToDuration)
import Pawl.Codec.EntryRiders (defaultEntryRiders, entryRidersToJson, jsonToEntryRiders)
import qualified Pawl.Codec.ExtraPhase as ExtraPhase
import Pawl.Codec.Filter (filterToJson, jsonToFilter)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ManaProduction (jsonToManaProduction, manaProductionToJson)
import Pawl.Codec.Modification (jsonToModification, modificationToJson)
import qualified Pawl.Codec.MonarchTarget as MonarchTarget
import Pawl.Codec.ObjectRef (jsonToObjectRef, objectRefToJson)
import Pawl.Codec.Onset (jsonToOnset, onsetToJson)
import Pawl.Codec.PhaseSelector (jsonToPhaseSelector, phaseSelectorToJson)
import Pawl.Codec.PlayerCounterKind (jsonToPlayerCounterKind, playerCounterKindToJson)
import Pawl.Codec.PlayerEffect (jsonToPlayerEffect, playerEffectToJson)
import Pawl.Codec.PlayerRef (jsonToPlayerRef, playerRefToJson)
import Pawl.Codec.PlayerScope (jsonToPlayerScope, playerScopeToJson)
import Pawl.Codec.Quantity (jsonToQuantity, quantityToJson)
import Pawl.Codec.Regenerability (jsonToRegenerability, regenerabilityToJson)
import Pawl.Codec.ReplacementEffect (jsonToReplacementEffect, replacementEffectToJson)
import Pawl.Codec.SearchDestination (jsonToSearchDestination, searchDestinationToJson)
import Pawl.Codec.SlotName (jsonToSlotName, slotNameToJson)
import Pawl.Codec.Uses (jsonToUses, usesToJson)
import Pawl.Codec.Zone (jsonToZone, zoneToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array, Object))
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Onset as Onset

effectToJson :: (card -> Value) -> Effect.Effect card -> Value
effectToJson codec e = case e of
  Effect.DealDamage s q -> Json.tagged (Text.pack "DealDamage") (Just (Array (MkArray [slotNameToJson s, quantityToJson q])))
  Effect.ModifyTarget d m r -> Json.tagged (Text.pack "ModifyTarget") (Just (Array (MkArray [durationToJson d, modificationToJson m, objectRefToJson r])))
  Effect.ChangeText s -> Json.tagged (Text.pack "ChangeText") (Just (slotNameToJson s))
  Effect.AddMana production -> Json.tagged (Text.pack "AddMana") (Just (manaProductionToJson production))
  Effect.Search f d -> Json.tagged (Text.pack "Search") (Just (Array (MkArray [filterToJson f, searchDestinationToJson d])))
  Effect.ExileAllGraveyards -> Json.nullary (Text.pack "ExileAllGraveyards")
  Effect.Proliferate -> Json.nullary (Text.pack "Proliferate")
  Effect.ExileHandThenDraw -> Json.nullary (Text.pack "ExileHandThenDraw")
  Effect.PlayerSacrifices slot f q -> Json.tagged (Text.pack "PlayerSacrifices") (Just (Array (MkArray [slotNameToJson slot, filterToJson f, quantityToJson q])))
  Effect.RestartGame -> Json.nullary (Text.pack "RestartGame")
  Effect.ControlPlayerNextTurn s -> Json.tagged (Text.pack "ControlPlayerNextTurn") (Just (slotNameToJson s))
  -- The bound-count slot is ELIDED when absent, the posture Create's EntryRiders
  -- and ArmDelayedTrigger's duration both take, so every card that says nothing about
  -- counting its sweep stays byte-for-byte as it was written.
  Effect.Destroy s r ms ->
    Json.tagged (Text.pack "Destroy") . Just . Array . MkArray $
      [objectRefToJson s, regenerabilityToJson r] <> fmap slotNameToJson (Maybe.maybeToList ms)
  Effect.Sacrifice s -> Json.tagged (Text.pack "Sacrifice") (Just (slotNameToJson s))
  Effect.RemoveFromCombat s -> Json.tagged (Text.pack "RemoveFromCombat") (Just (slotNameToJson s))
  Effect.Counter s -> Json.tagged (Text.pack "Counter") (Just (slotNameToJson s))
  -- MoveToZone's payload is positional and takes Create's shape, for the same
  -- reason and with the same two elisions: the EntryRiders are dropped when they
  -- are the CR 110.5b default and the bound slot when there is none, so every
  -- card file written before either existed stays byte-for-byte as it was. The
  -- three-element form is two shapes, told apart on decode by JSON TYPE -- a
  -- slot name is a string, riders are an object.
  Effect.MoveToZone s z riders ms ->
    Json.tagged (Text.pack "MoveToZone") . Just . Array . MkArray $
      [slotNameToJson s, zoneToJson z]
        <> (if riders == defaultEntryRiders then [] else [entryRidersToJson riders])
        <> fmap slotNameToJson (Maybe.maybeToList ms)
  Effect.Draw r q -> Json.tagged (Text.pack "Draw") (Just (Array (MkArray [playerRefToJson r, quantityToJson q])))
  Effect.Mill s q -> Json.tagged (Text.pack "Mill") (Just (Array (MkArray [slotNameToJson s, quantityToJson q])))
  Effect.Discard s q -> Json.tagged (Text.pack "Discard") (Just (Array (MkArray [slotNameToJson s, quantityToJson q])))
  Effect.LoseLife r q -> Json.tagged (Text.pack "LoseLife") (Just (Array (MkArray [playerRefToJson r, quantityToJson q])))
  Effect.GainLife r q -> Json.tagged (Text.pack "GainLife") (Just (Array (MkArray [playerRefToJson r, quantityToJson q])))
  -- Create's payload is positional, and the EntryRiders are ELIDED when they are
  -- the CR 110.5b default (defaultEntryRiders) -- the same posture
  -- `counterability` takes, so a card file that says nothing about how its
  -- tokens enter stays exactly as it was written. The three-element form is
  -- therefore two shapes, told apart on decode by JSON TYPE rather than by
  -- position: a slot name is a string (slotNameToJson) and riders are an object,
  -- so the two can never be confused.
  Effect.Create q c te ms ->
    Json.tagged (Text.pack "Create") . Just . Array . MkArray $
      [quantityToJson q, codec c]
        <> (if te == defaultEntryRiders then [] else [entryRidersToJson te])
        <> fmap slotNameToJson (Maybe.maybeToList ms)
  Effect.Replace d u re -> Json.tagged (Text.pack "Replace") (Just (Array (MkArray [durationToJson d, usesToJson u, replacementEffectToJson re])))
  Effect.SkipNextPhase r sel -> Json.tagged (Text.pack "SkipNextPhase") (Just (Array (MkArray [playerRefToJson r, phaseSelectorToJson sel])))
  Effect.PutCounters k q s -> Json.tagged (Text.pack "PutCounters") (Just (Array (MkArray [counterKindToJson k, quantityToJson q, slotNameToJson s])))
  Effect.GainPlayerCounters r k q -> Json.tagged (Text.pack "GainPlayerCounters") (Just (Array (MkArray [playerRefToJson r, playerCounterKindToJson k, quantityToJson q])))
  Effect.Tap r -> Json.tagged (Text.pack "Tap") (Just (objectRefToJson r))
  Effect.Untap r -> Json.tagged (Text.pack "Untap") (Just (objectRefToJson r))
  Effect.AddPhases ps -> Json.tagged (Text.pack "AddPhases") (Just (Array (MkArray (fmap ExtraPhase.toJson ps))))
  Effect.GainControl d r -> Json.tagged (Text.pack "GainControl") (Just (Array (MkArray [durationToJson d, objectRefToJson r])))
  -- The duration is ELIDED when absent, which is CR 603.7b's default -- so
  -- Tidal Wave's one-shot entry stays a bare ability name and only a card that
  -- states a duration writes the two-element form. The ONSET is elided when it is CR 603.7a's default (the ability is armed the
  -- moment it is created), so Tidal Wave's bare name and Full Throttle's
  -- two-element form both stay exactly as they were written. A stated onset
  -- takes the THREE-element form, whose last element is the duration or null --
  -- and LENGTH, not JSON type, is what tells the forms apart, because an onset
  -- and a duration are both tagged objects.
  Effect.ArmDelayedTrigger n o md -> Json.tagged (Text.pack "ArmDelayedTrigger") . Just $ case (o, md) of
    (Onset.Immediately, Nothing) -> AbilityName.toJson n
    (Onset.Immediately, Just d) -> Array (MkArray [AbilityName.toJson n, durationToJson d])
    _ -> Array (MkArray [AbilityName.toJson n, onsetToJson o, Json.maybeTo durationToJson md])
  Effect.AffectPlayers d s pe -> Json.tagged (Text.pack "AffectPlayers") (Just (Array (MkArray [durationToJson d, playerScopeToJson s, playerEffectToJson pe])))
  Effect.CreateEmblem c -> Json.tagged (Text.pack "CreateEmblem") (Just (codec c))
  Effect.BecomeMonarch t -> Json.tagged (Text.pack "BecomeMonarch") (Just (MonarchTarget.toJson t))
  Effect.ExileUntilMonarch s -> Json.tagged (Text.pack "ExileUntilMonarch") (Just (slotNameToJson s))
  Effect.Attach s -> Json.tagged (Text.pack "Attach") (Just (slotNameToJson s))
  Effect.AttachTarget s f -> Json.tagged (Text.pack "AttachTarget") (Just (Array (MkArray [slotNameToJson s, filterToJson f])))
  Effect.PlaySubgame s -> Json.tagged (Text.pack "PlaySubgame") (Just (slotNameToJson s))
  Effect.TakeExtraTurn r skips -> Json.tagged (Text.pack "TakeExtraTurn") (Just (Array (MkArray [playerRefToJson r, Json.setTo phaseSelectorToJson skips])))

jsonToEffect :: (Value -> Either Text card) -> Value -> Either Text (Effect.Effect card)
jsonToEffect decode value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "DealDamage" -> case mv of
      Just (Array (MkArray [s, q])) -> Effect.DealDamage <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "DealDamage expects [slot, quantity]")
    "ModifyTarget" -> case mv of
      Just (Array (MkArray [d, m, r])) -> Effect.ModifyTarget <$> jsonToDuration d <*> jsonToModification m <*> jsonToObjectRef r
      _ -> Left (Text.pack "ModifyTarget expects [duration, modification, objectRef]")
    "ChangeText" -> Json.withValue mv (fmap Effect.ChangeText . jsonToSlotName)
    "AddMana" -> Json.withValue mv (fmap Effect.AddMana . jsonToManaProduction)
    "Search" -> case mv of
      Just (Array (MkArray [f, d])) -> Effect.Search <$> jsonToFilter f <*> jsonToSearchDestination d
      _ -> Left (Text.pack "Search expects [filter, destination]")
    "ExileAllGraveyards" -> Right Effect.ExileAllGraveyards
    "Proliferate" -> Right Effect.Proliferate
    "ExileHandThenDraw" -> Right Effect.ExileHandThenDraw
    "PlayerSacrifices" -> case mv of
      Just (Array (MkArray [sv, fv, qv])) -> Effect.PlayerSacrifices <$> jsonToSlotName sv <*> jsonToFilter fv <*> jsonToQuantity qv
      _ -> Left (Text.pack "PlayerSacrifices expects [slot, filter, quantity]")
    "RestartGame" -> Right Effect.RestartGame
    "ControlPlayerNextTurn" -> Json.withValue mv (fmap Effect.ControlPlayerNextTurn . jsonToSlotName)
    "Destroy" -> case mv of
      Just (Array (MkArray [sv, rv])) -> Effect.Destroy <$> jsonToObjectRef sv <*> jsonToRegenerability rv <*> pure Nothing
      Just (Array (MkArray [sv, rv, nv])) -> Effect.Destroy <$> jsonToObjectRef sv <*> jsonToRegenerability rv <*> (Just <$> jsonToSlotName nv)
      _ -> Left (Text.pack "Destroy expects [objectRef, regenerability], optionally with a slot")
    "Sacrifice" -> Json.withValue mv (fmap Effect.Sacrifice . jsonToSlotName)
    "RemoveFromCombat" -> Json.withValue mv (fmap Effect.RemoveFromCombat . jsonToSlotName)
    "Counter" -> Json.withValue mv (fmap Effect.Counter . jsonToSlotName)
    -- The four shapes the encoder above can emit, read exactly as Create's are:
    -- an Object in third position is the EntryRiders, anything else is the bound
    -- slot name (a string).
    "MoveToZone" -> case mv of
      Just (Array (MkArray [s, z])) -> Effect.MoveToZone <$> jsonToSlotName s <*> jsonToZone z <*> pure defaultEntryRiders <*> pure Nothing
      Just (Array (MkArray [s, z, e@(Object _)])) -> Effect.MoveToZone <$> jsonToSlotName s <*> jsonToZone z <*> jsonToEntryRiders e <*> pure Nothing
      Just (Array (MkArray [s, z, b])) -> Effect.MoveToZone <$> jsonToSlotName s <*> jsonToZone z <*> pure defaultEntryRiders <*> (Just <$> jsonToSlotName b)
      Just (Array (MkArray [s, z, e, b])) -> Effect.MoveToZone <$> jsonToSlotName s <*> jsonToZone z <*> jsonToEntryRiders e <*> (Just <$> jsonToSlotName b)
      _ -> Left (Text.pack "MoveToZone expects [slot, zone], optionally with EntryRiders and/or a slot")
    "Draw" -> case mv of
      Just (Array (MkArray [r, q])) -> Effect.Draw <$> jsonToPlayerRef r <*> jsonToQuantity q
      _ -> Left (Text.pack "Draw expects [playerRef, quantity]")
    "Mill" -> case mv of
      Just (Array (MkArray [s, q])) -> Effect.Mill <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Mill expects [slot, quantity]")
    "Discard" -> case mv of
      Just (Array (MkArray [s, q])) -> Effect.Discard <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Discard expects [slot, quantity]")
    "LoseLife" -> case mv of
      Just (Array (MkArray [r, q])) -> Effect.LoseLife <$> jsonToPlayerRef r <*> jsonToQuantity q
      _ -> Left (Text.pack "LoseLife expects [playerRef, quantity]")
    "GainLife" -> case mv of
      Just (Array (MkArray [r, q])) -> Effect.GainLife <$> jsonToPlayerRef r <*> jsonToQuantity q
      _ -> Left (Text.pack "GainLife expects [playerRef, quantity]")
    -- The four shapes the encoder above can emit. The three-element one is read
    -- by JSON type: an Object is the EntryRiders, anything else is the slot name
    -- (a string), which is what lets the entry be elided when it is the default
    -- without a hole in the array.
    "Create" -> case mv of
      Just (Array (MkArray [q, c])) -> Effect.Create <$> jsonToQuantity q <*> decode c <*> pure defaultEntryRiders <*> pure Nothing
      Just (Array (MkArray [q, c, e@(Object _)])) -> Effect.Create <$> jsonToQuantity q <*> decode c <*> jsonToEntryRiders e <*> pure Nothing
      Just (Array (MkArray [q, c, s])) -> Effect.Create <$> jsonToQuantity q <*> decode c <*> pure defaultEntryRiders <*> (Just <$> jsonToSlotName s)
      Just (Array (MkArray [q, c, e, s])) -> Effect.Create <$> jsonToQuantity q <*> decode c <*> jsonToEntryRiders e <*> (Just <$> jsonToSlotName s)
      _ -> Left (Text.pack "Create expects [Quantity, Card], optionally with EntryRiders and/or a slot")
    -- The three shapes the encoder above can emit, told apart by LENGTH.
    "ArmDelayedTrigger" -> case mv of
      Just (Array (MkArray [n, o, d])) ->
        Effect.ArmDelayedTrigger <$> AbilityName.fromJson n <*> jsonToOnset o <*> Json.maybeFrom jsonToDuration d
      Just (Array (MkArray [n, d])) ->
        Effect.ArmDelayedTrigger <$> AbilityName.fromJson n <*> pure Onset.Immediately <*> fmap Just (jsonToDuration d)
      _ -> Json.withValue mv (fmap (\n -> Effect.ArmDelayedTrigger n Onset.Immediately Nothing) . AbilityName.fromJson)
    "Replace" -> case mv of
      Just (Array (MkArray [d, u, re])) -> do
        duration <- jsonToDuration d
        uses <- jsonToUses u
        effect <- jsonToReplacementEffect re
        pure (Effect.Replace duration uses effect)
      _ -> Left (Text.pack "Replace expects [Duration, Uses, ReplacementEffect]")
    "SkipNextPhase" -> case mv of
      Just (Array (MkArray [r, sel])) -> Effect.SkipNextPhase <$> jsonToPlayerRef r <*> jsonToPhaseSelector sel
      _ -> Left (Text.pack "SkipNextPhase expects [playerRef, phaseSelector]")
    "PutCounters" -> case mv of
      Just (Array (MkArray [k, q, s])) -> Effect.PutCounters <$> jsonToCounterKind k <*> jsonToQuantity q <*> jsonToSlotName s
      _ -> Left (Text.pack "PutCounters expects [counterKind, quantity, slot]")
    "GainPlayerCounters" -> case mv of
      Just (Array (MkArray [r, k, q])) -> Effect.GainPlayerCounters <$> jsonToPlayerRef r <*> jsonToPlayerCounterKind k <*> jsonToQuantity q
      _ -> Left (Text.pack "GainPlayerCounters expects [playerRef, playerCounterKind, quantity]")
    "Tap" -> Json.withValue mv (fmap Effect.Tap . jsonToObjectRef)
    "Untap" -> Json.withValue mv (fmap Effect.Untap . jsonToObjectRef)
    "AddPhases" -> case mv of
      Just (Array (MkArray ps)) -> Effect.AddPhases <$> traverse ExtraPhase.fromJson ps
      _ -> Left (Text.pack "AddPhases expects [ExtraPhase]")
    "GainControl" -> case mv of
      Just (Array (MkArray [d, r])) -> Effect.GainControl <$> jsonToDuration d <*> jsonToObjectRef r
      _ -> Left (Text.pack "GainControl expects [duration, objectRef]")
    "AffectPlayers" -> case mv of
      Just (Array (MkArray [d, s, pe])) -> Effect.AffectPlayers <$> jsonToDuration d <*> jsonToPlayerScope s <*> jsonToPlayerEffect pe
      _ -> Left (Text.pack "AffectPlayers expects [Duration, PlayerScope, PlayerEffect]")
    "CreateEmblem" -> Json.withValue mv (fmap Effect.CreateEmblem . decode)
    "BecomeMonarch" -> Json.withValue mv (fmap Effect.BecomeMonarch . MonarchTarget.fromJson)
    "ExileUntilMonarch" -> Json.withValue mv (fmap Effect.ExileUntilMonarch . jsonToSlotName)
    "Attach" -> Json.withValue mv (fmap Effect.Attach . jsonToSlotName)
    "AttachTarget" -> case mv of
      Just (Array (MkArray [s, f])) -> Effect.AttachTarget <$> jsonToSlotName s <*> jsonToFilter f
      _ -> Left (Text.pack "AttachTarget expects [slot, filter]")
    "PlaySubgame" -> Json.withValue mv (fmap Effect.PlaySubgame . jsonToSlotName)
    "TakeExtraTurn" -> case mv of
      Just (Array (MkArray [r, skips])) -> Effect.TakeExtraTurn <$> jsonToPlayerRef r <*> Json.setFrom jsonToPhaseSelector skips
      _ -> Left (Text.pack "TakeExtraTurn expects [playerRef, phaseSelectors]")
    _ -> Left (Text.pack "unknown Effect: " <> t)
