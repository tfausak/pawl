module Pawl.Codec.Effect where

import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.AffectedPlayers as AffectedPlayers
import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.Daytime as Daytime
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.ExchangeSides as ExchangeSides
import qualified Pawl.Codec.ExtraPhase as ExtraPhase
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.LibraryPlacement as LibraryPlacement
import qualified Pawl.Codec.ManaProduction as ManaProduction
import qualified Pawl.Codec.MillTally as MillTally
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.Codec.MonarchTarget as MonarchTarget
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Onset as Onset
import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.Codec.PlayerRef as PlayerRef
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
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Effect as Effect
-- These type modules share an alias with their codec module, the posture Onset
-- already took here: the names never collide, since a codec module exports
-- functions and a type module exports the type moveTail's signature needs.
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

toJson :: (card -> Value.Value) -> Effect.Effect card -> Value.Value
toJson codec e = case e of
  Effect.DealDamage r q -> Common.tagged "DealDamage" (Just (Value.array [Codec.encode ObjectRef.codec r, Codec.encode Quantity.codec q]))
  Effect.ModifyTarget d m r -> Common.tagged "ModifyTarget" (Just (Value.array [Duration.toJson d, Modification.toJson m, Codec.encode ObjectRef.codec r]))
  Effect.ChangeText family forbidden s ->
    Common.tagged "ChangeText" . Just . Value.array $
      [Codec.encode SubtypeFamily.codec family, Common.encodeSet (Codec.encode Subtype.codec) forbidden, Codec.encode SlotName.codec s]
  Effect.AddMana production -> Common.tagged "AddMana" (Just (Codec.encode ManaProduction.codec production))
  Effect.Search s o q f d -> Common.tagged "Search" (Just (Value.array [Codec.encode PlayerRef.codec s, Codec.encode PlayerRef.codec o, Codec.encode Quantity.codec q, Codec.encode (Filter.codec Keyword.codec) f, Codec.encode SearchDestination.codec d]))
  Effect.ExileAllGraveyards -> Common.nullary "ExileAllGraveyards"
  Effect.Proliferate -> Common.nullary "Proliferate"
  Effect.TemptWithTheRing -> Common.nullary "TemptWithTheRing"
  Effect.Venture -> Common.nullary "Venture"
  Effect.ExileHandThenDraw -> Common.nullary "ExileHandThenDraw"
  Effect.PlayerSacrifices slot f q -> Common.tagged "PlayerSacrifices" (Just (Value.array [Codec.encode SlotName.codec slot, Codec.encode (Filter.codec Keyword.codec) f, Codec.encode Quantity.codec q]))
  Effect.RestartGame -> Common.nullary "RestartGame"
  Effect.ControlPlayerNextTurn s -> Common.tagged "ControlPlayerNextTurn" (Just (Codec.encode SlotName.codec s))
  -- The bound-count slot is ELIDED when absent, so a card that says nothing
  -- about counting its sweep keeps its two-element payload.
  Effect.Destroy s r ms ->
    Common.tagged "Destroy" . Just . Value.array $
      [Codec.encode ObjectRef.codec s, Codec.encode Regenerability.codec r] <> fmap (Codec.encode SlotName.codec) (Maybe.maybeToList ms)
  Effect.Sacrifice s -> Common.tagged "Sacrifice" (Just (Codec.encode SlotName.codec s))
  Effect.TurnFaceDown s -> Common.tagged "TurnFaceDown" (Just (Codec.encode SlotName.codec s))
  Effect.RemoveFromCombat s -> Common.tagged "RemoveFromCombat" (Just (Codec.encode SlotName.codec s))
  Effect.Counter s -> Common.tagged "Counter" (Just (Codec.encode SlotName.codec s))
  -- MoveToZone's payload is the ObjectRef and the destination zone, then four
  -- independently elided extras where Create has two: the EntryRiders are
  -- dropped when they are the CR 110.5b default, the bound slot when there is
  -- none, CR 113.6m's origin zone when the effect states none, and the library
  -- placement when it is the default end, stated. Everything after the
  -- destination is therefore optional, and told apart on decode by JSON TYPE
  -- rather than by position -- a slot name is a string, a zone and a library
  -- placement are tagged objects with disjoint tags, and the riders are an
  -- object that is neither. `moveTail` is the decoding half.
  --
  -- The two POSITIONAL elements are exempt from that, and since #1304 the
  -- ObjectRef among them is a tagged object like any other sum -- which is what
  -- keeps it out of the tail's reckoning entirely rather than merely distinct
  -- from it.
  Effect.MoveToZone r z riders ms mo p ->
    Common.tagged "MoveToZone" . Just . Value.array $
      [Codec.encode ObjectRef.codec r, Codec.encode Zone.codec z]
        <> (if riders == EntryRiders.defaultValue then [] else [EntryRiders.toJson riders])
        <> fmap (Codec.encode SlotName.codec) (Maybe.maybeToList ms)
        <> fmap (Codec.encode Zone.codec) (Maybe.maybeToList mo)
        <> (if p == LibraryPlacement.defaultValue then [] else [Codec.encode LibraryPlacement.codec p])
  Effect.Draw r q -> Common.tagged "Draw" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode Quantity.codec q]))
  Effect.Scry r q -> Common.tagged "Scry" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode Quantity.codec q]))
  Effect.Surveil r q -> Common.tagged "Surveil" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode Quantity.codec q]))
  Effect.Fateseal r q -> Common.tagged "Fateseal" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode Quantity.codec q]))
  Effect.Explore r -> Common.tagged "Explore" (Just (Codec.encode ObjectRef.codec r))
  -- The tally is ELIDED when absent, as Destroy's bound-count slot is, so a mill
  -- nothing looks back at keeps its two-element payload.
  Effect.Mill r q mt ->
    Common.tagged "Mill" . Just . Value.array $
      [Codec.encode PlayerRef.codec r, Codec.encode Quantity.codec q] <> fmap MillTally.toJson (Maybe.maybeToList mt)
  Effect.Discard s q -> Common.tagged "Discard" (Just (Value.array [Codec.encode SlotName.codec s, Codec.encode Quantity.codec q]))
  Effect.LoseLife r q -> Common.tagged "LoseLife" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode Quantity.codec q]))
  Effect.GainLife r q -> Common.tagged "GainLife" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode Quantity.codec q]))
  Effect.ExchangeLifeTotals sides -> Common.tagged "ExchangeLifeTotals" (Just (ExchangeSides.toJson sides))
  Effect.IncreaseSpeed r q -> Common.tagged "IncreaseSpeed" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode Quantity.codec q]))
  -- Create's payload is positional, and the EntryRiders are ELIDED when they
  -- are the CR 110.5b default. The three-element form is therefore two shapes,
  -- told apart on decode by JSON TYPE rather than by position: a slot name is a
  -- string and riders are an object, so the two can never be confused.
  Effect.Create q c te ms ->
    Common.tagged "Create" . Just . Value.array $
      [Codec.encode Quantity.codec q, codec c]
        <> (if te == EntryRiders.defaultValue then [] else [EntryRiders.toJson te])
        <> fmap (Codec.encode SlotName.codec) (Maybe.maybeToList ms)
  -- The count is ELIDED when it is one, which is Mill's tally posture and
  -- Create's riders': every card that mints a single copy keeps the bare
  -- ObjectRef it has always had, and only a card that mints several spells the
  -- pair out.
  Effect.CreateCopy q r
    | q == Quantity.Literal 1 -> Common.tagged "CreateCopy" (Just (Codec.encode ObjectRef.codec r))
    | otherwise -> Common.tagged "CreateCopy" (Just (Value.array [Codec.encode Quantity.codec q, Codec.encode ObjectRef.codec r]))
  Effect.Replace d u o c re ->
    Common.tagged "Replace" . Just . Value.array $
      [Duration.toJson d, Codec.encode Uses.codec u, Codec.encode ReplacementOrigin.codec o, Common.encodeMaybe (Codec.encode Condition.codec) c, ReplacementEffect.toJson re]
  Effect.SkipNextPhase r sel -> Common.tagged "SkipNextPhase" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode PhaseSelector.codec sel]))
  -- CR 615.5's additional effect is ELIDED when it is empty, which is Create's
  -- posture above and every other prevention in the corpus: a shield with no
  -- rider stays the three-element form it has always had.
  Effect.PreventNextDamage d r q rider ->
    Common.tagged "PreventNextDamage" . Just . Value.array $
      [Duration.toJson d, Codec.encode ObjectRef.codec r, Codec.encode Quantity.codec q]
        <> [Common.encodeSeq (toJson codec) rider | not (Seq.null rider)]
  Effect.PreventAllDamage d r -> Common.tagged "PreventAllDamage" (Just (Value.array [Duration.toJson d, Codec.encode ObjectRef.codec r]))
  Effect.RedirectDamage d k f t -> Common.tagged "RedirectDamage" (Just (Value.array [Duration.toJson d, Common.encodeMaybe (Codec.encode DamageKind.codec) k, Codec.encode ObjectRef.codec f, Codec.encode ObjectRef.codec t]))
  Effect.PutCounters k q r -> Common.tagged "PutCounters" (Just (Value.array [Codec.encode (CounterKind.codec Keyword.codec) k, Codec.encode Quantity.codec q, Codec.encode ObjectRef.codec r]))
  Effect.RemoveCounters k q s -> Common.tagged "RemoveCounters" (Just (Value.array [Codec.encode (CounterKind.codec Keyword.codec) k, Codec.encode Quantity.codec q, Codec.encode SlotName.codec s]))
  Effect.GainPlayerCounters r k q -> Common.tagged "GainPlayerCounters" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode PlayerCounterKind.codec k, Codec.encode Quantity.codec q]))
  Effect.RemovePlayerCounters r k q -> Common.tagged "RemovePlayerCounters" (Just (Value.array [Codec.encode PlayerRef.codec r, Codec.encode PlayerCounterKind.codec k, Codec.encode Quantity.codec q]))
  Effect.Tap r -> Common.tagged "Tap" (Just (Codec.encode ObjectRef.codec r))
  Effect.Untap r -> Common.tagged "Untap" (Just (Codec.encode ObjectRef.codec r))
  Effect.Transform r -> Common.tagged "Transform" (Just (Codec.encode ObjectRef.codec r))
  Effect.AddPhases ps -> Common.tagged "AddPhases" (Just (Value.array (fmap (Codec.encode ExtraPhase.codec) ps)))
  Effect.GainControl d r -> Common.tagged "GainControl" (Just (Value.array [Duration.toJson d, Codec.encode ObjectRef.codec r]))
  -- The duration is ELIDED when absent (CR 603.7b's default) and the onset when
  -- it is CR 603.7a's default, so a one-shot entry stays a bare ability name
  -- and a stated duration alone writes the two-element form. A stated onset
  -- takes the THREE-element form, whose last element is the duration or null.
  -- LENGTH, not JSON type, tells the forms apart: an onset and a duration are
  -- both tagged objects.
  Effect.ArmDelayedTrigger n o md -> Common.tagged "ArmDelayedTrigger" . Just $ case (o, md) of
    (Onset.Immediately, Nothing) -> Codec.encode AbilityName.codec n
    (Onset.Immediately, Just d) -> Value.array [Codec.encode AbilityName.codec n, Duration.toJson d]
    _ -> Value.array [Codec.encode AbilityName.codec n, Codec.encode Onset.codec o, Common.encodeMaybe Duration.toJson md]
  Effect.AffectPlayers d s pe -> Common.tagged "AffectPlayers" (Just (Value.array [Duration.toJson d, AffectedPlayers.toJson s, PlayerEffect.toJson pe]))
  Effect.RequireBlock d b a -> Common.tagged "RequireBlock" (Just (Value.array [Duration.toJson d, Codec.encode ObjectRef.codec b, Codec.encode ObjectRef.codec a]))
  Effect.CreateEmblem c -> Common.tagged "CreateEmblem" (Just (codec c))
  Effect.BecomeMonarch t -> Common.tagged "BecomeMonarch" (Just (MonarchTarget.toJson t))
  Effect.Designate d s -> Common.tagged "Designate" (Just (Value.array [Designation.toJson d, Codec.encode SlotName.codec s]))
  Effect.Unsuspect r -> Common.tagged "Unsuspect" (Just (Codec.encode ObjectRef.codec r))
  Effect.Evolve s -> Common.tagged "Evolve" (Just (Codec.encode SlotName.codec s))
  Effect.Mentor s -> Common.tagged "Mentor" (Just (Codec.encode SlotName.codec s))
  Effect.ItBecomes d -> Common.tagged "ItBecomes" (Just (Codec.encode Daytime.codec d))
  Effect.ExileUntilMonarch s -> Common.tagged "ExileUntilMonarch" (Just (Codec.encode SlotName.codec s))
  Effect.Attach s -> Common.tagged "Attach" (Just (Codec.encode SlotName.codec s))
  Effect.AttachTarget s f -> Common.tagged "AttachTarget" (Just (Value.array [Codec.encode SlotName.codec s, Codec.encode (Filter.codec Keyword.codec) f]))
  Effect.PlaySubgame s -> Common.tagged "PlaySubgame" (Just (Codec.encode SlotName.codec s))
  Effect.TakeExtraTurn r skips -> Common.tagged "TakeExtraTurn" (Just (Value.array [Codec.encode PlayerRef.codec r, Common.encodeSet (Codec.encode PhaseSelector.codec) skips]))
  -- The library-naming PlayerRef is ELIDED when absent, Mill's tally posture:
  -- Riftsweeper's derived owner (CR 701.24) keeps the bare ObjectRef it has
  -- always had, and only a card naming the library spells the pair out.
  --
  -- Told apart on decode by JSON TYPE, as Create's riders are: the pair form is
  -- an ARRAY and a lone ObjectRef is a tagged object, so neither can be read as
  -- the other. Before #1304 an ObjectRef could itself be a two-element array,
  -- and the decode below had to guard on its head being an object to tell the
  -- two apart; now that no ObjectRef is ever an array, the arity alone decides.
  Effect.ShuffleIntoLibrary mp r -> case mp of
    Nothing -> Common.tagged "ShuffleIntoLibrary" (Just (Codec.encode ObjectRef.codec r))
    Just p -> Common.tagged "ShuffleIntoLibrary" (Just (Value.array [Codec.encode PlayerRef.codec p, Codec.encode ObjectRef.codec r]))
  -- The slot alone when the offer carries neither of CR 310.11b's riders, which
  -- is an ordinary cast of the card; the pair otherwise. Elided the way
  -- MoveToZone's EntryRiders are, and told apart on decode by JSON TYPE.
  Effect.OfferCast s offer ->
    Common.tagged "OfferCast" . Just $
      if offer == CastOffer.defaultValue
        then Codec.encode SlotName.codec s
        else Value.array [Codec.encode SlotName.codec s, Codec.encode CastOffer.codec offer]
  Effect.GrantPlayFromExile d r -> Common.tagged "GrantPlayFromExile" (Just (Value.array [Duration.toJson d, Codec.encode ObjectRef.codec r]))

-- Everything a MoveToZone payload may carry after its ObjectRef and its
-- destination zone: the EntryRiders (CR 110.5b), the slot binding the destination
-- incarnation (CR 400.7), the origin zone the effect names (CR 113.6m), and how a
-- library destination's end is settled (CR 401.2). Each is optional, so they are
-- read by JSON TYPE rather than by position -- a string is the slot, and an
-- object is the origin zone or the library placement if it decodes as one and the
-- riders otherwise.
--
-- ZONE AND PLACEMENT FIRST is what makes that order-independent rather than
-- merely ordered: EntryRiders.fromJson defaults every field it does not find, so
-- it would accept either tagged object and silently return the default riders,
-- while the zone codec and the placement codec accept nothing but their own
-- tagged shapes. Those two shapes cannot be confused with each other either --
-- no zone is named Stated or OwnerChooses.
--
-- A REPEATED element is an error rather than last-one-wins. Two origin zones is
-- a card file saying something CR 113.6m's "a particular zone" cannot mean, and
-- two of anything else is as likely a typo.
moveTail :: [Value.Value] -> Either Text.Text (EntryRiders.EntryRiders, Maybe SlotName.SlotName, Maybe Zone.Zone, LibraryPlacement.LibraryPlacement)
moveTail = go Nothing Nothing Nothing Nothing
  where
    go mRiders mSlot mOrigin mPlacement values = case values of
      [] ->
        Right
          ( Maybe.fromMaybe EntryRiders.defaultValue mRiders,
            mSlot,
            mOrigin,
            Maybe.fromMaybe LibraryPlacement.defaultValue mPlacement
          )
      v : rest -> case v of
        Value.String _ -> case mSlot of
          Just _ -> Left . Text.pack $ "MoveToZone names two bound slots"
          Nothing -> do
            slot <- Codec.decode SlotName.codec v
            go mRiders (Just slot) mOrigin mPlacement rest
        _ -> case Codec.decode Zone.codec v of
          Right zone -> case mOrigin of
            Just _ -> Left . Text.pack $ "MoveToZone names two origin zones"
            Nothing -> go mRiders mSlot (Just zone) mPlacement rest
          Left _ -> case Codec.decode LibraryPlacement.codec v of
            Right placement -> case mPlacement of
              Just _ -> Left . Text.pack $ "MoveToZone names two library placements"
              Nothing -> go mRiders mSlot mOrigin (Just placement) rest
            Left _ -> case mRiders of
              Just _ -> Left . Text.pack $ "MoveToZone names two sets of entry riders"
              Nothing -> do
                riders <- EntryRiders.fromJson v
                go (Just riders) mSlot mOrigin mPlacement rest

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Effect.Effect card)
fromJson decode value = do
  (t, mv) <- Common.asTagged value
  case t of
    "DealDamage" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.DealDamage <$> Codec.decode ObjectRef.codec r <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "DealDamage expects [objectRef, quantity]"
    "ModifyTarget" -> case mv of
      Just (Value.Array (Array.MkArray [d, m, r])) -> Effect.ModifyTarget <$> Duration.fromJson d <*> Modification.fromJson m <*> Codec.decode ObjectRef.codec r
      _ -> Left . Text.pack $ "ModifyTarget expects [duration, modification, objectRef]"
    "ChangeText" -> case mv of
      Just (Value.Array (Array.MkArray [fv, xv, sv])) ->
        Effect.ChangeText <$> Codec.decode SubtypeFamily.codec fv <*> Common.decodeSet (Codec.decode Subtype.codec) xv <*> Codec.decode SlotName.codec sv
      _ -> Left . Text.pack $ "ChangeText expects [subtypeFamily, forbiddenSubtypes, slot]"
    "AddMana" -> Common.withValue mv (fmap Effect.AddMana . Codec.decode ManaProduction.codec)
    "Search" -> case mv of
      Just (Value.Array (Array.MkArray [s, o, q, f, d])) -> Effect.Search <$> Codec.decode PlayerRef.codec s <*> Codec.decode PlayerRef.codec o <*> Codec.decode Quantity.codec q <*> Codec.decode (Filter.codec Keyword.codec) f <*> Codec.decode SearchDestination.codec d
      _ -> Left . Text.pack $ "Search expects [searcher, libraryOwner, quantity, filter, destination]"
    "ExileAllGraveyards" -> Right Effect.ExileAllGraveyards
    "Proliferate" -> Right Effect.Proliferate
    "TemptWithTheRing" -> Right Effect.TemptWithTheRing
    "Venture" -> Right Effect.Venture
    "ExileHandThenDraw" -> Right Effect.ExileHandThenDraw
    "PlayerSacrifices" -> case mv of
      Just (Value.Array (Array.MkArray [sv, fv, qv])) -> Effect.PlayerSacrifices <$> Codec.decode SlotName.codec sv <*> Codec.decode (Filter.codec Keyword.codec) fv <*> Codec.decode Quantity.codec qv
      _ -> Left . Text.pack $ "PlayerSacrifices expects [slot, filter, quantity]"
    "RestartGame" -> Right Effect.RestartGame
    "ControlPlayerNextTurn" -> Common.withValue mv (fmap Effect.ControlPlayerNextTurn . Codec.decode SlotName.codec)
    "Destroy" -> case mv of
      Just (Value.Array (Array.MkArray [sv, rv])) -> Effect.Destroy <$> Codec.decode ObjectRef.codec sv <*> Codec.decode Regenerability.codec rv <*> pure Nothing
      Just (Value.Array (Array.MkArray [sv, rv, nv])) -> Effect.Destroy <$> Codec.decode ObjectRef.codec sv <*> Codec.decode Regenerability.codec rv <*> (Just <$> Codec.decode SlotName.codec nv)
      _ -> Left . Text.pack $ "Destroy expects [objectRef, regenerability], optionally with a slot"
    "Sacrifice" -> Common.withValue mv (fmap Effect.Sacrifice . Codec.decode SlotName.codec)
    "TurnFaceDown" -> Common.withValue mv (fmap Effect.TurnFaceDown . Codec.decode SlotName.codec)
    "RemoveFromCombat" -> Common.withValue mv (fmap Effect.RemoveFromCombat . Codec.decode SlotName.codec)
    "Counter" -> Common.withValue mv (fmap Effect.Counter . Codec.decode SlotName.codec)
    -- Read by JSON TYPE and not by position, which is `moveTail`'s whole job:
    -- Create's third-position rule would not survive CR 113.6m's origin zone,
    -- since that zone and the EntryRiders are both objects.
    "MoveToZone" -> case mv of
      Just (Value.Array (Array.MkArray (r : z : rest))) -> do
        (riders, mSlot, mOrigin, placement) <- moveTail rest
        Effect.MoveToZone <$> Codec.decode ObjectRef.codec r <*> Codec.decode Zone.codec z <*> pure riders <*> pure mSlot <*> pure mOrigin <*> pure placement
      _ -> Left . Text.pack $ "MoveToZone expects [objectRef, zone], optionally with EntryRiders, a slot, an origin zone and/or a library placement"
    "Draw" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.Draw <$> Codec.decode PlayerRef.codec r <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "Draw expects [playerRef, quantity]"
    "Scry" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.Scry <$> Codec.decode PlayerRef.codec r <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "Scry expects [playerRef, quantity]"
    "Surveil" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.Surveil <$> Codec.decode PlayerRef.codec r <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "Surveil expects [playerRef, quantity]"
    "Fateseal" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.Fateseal <$> Codec.decode PlayerRef.codec r <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "Fateseal expects [playerRef, quantity]"
    "Explore" -> Common.withValue mv (fmap Effect.Explore . Codec.decode ObjectRef.codec)
    "Mill" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.Mill <$> Codec.decode PlayerRef.codec r <*> Codec.decode Quantity.codec q <*> pure Nothing
      Just (Value.Array (Array.MkArray [r, q, tv])) -> Effect.Mill <$> Codec.decode PlayerRef.codec r <*> Codec.decode Quantity.codec q <*> (Just <$> MillTally.fromJson tv)
      _ -> Left . Text.pack $ "Mill expects [playerRef, quantity], optionally with a tally"
    "Discard" -> case mv of
      Just (Value.Array (Array.MkArray [s, q])) -> Effect.Discard <$> Codec.decode SlotName.codec s <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "Discard expects [slot, quantity]"
    "LoseLife" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.LoseLife <$> Codec.decode PlayerRef.codec r <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "LoseLife expects [playerRef, quantity]"
    "GainLife" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.GainLife <$> Codec.decode PlayerRef.codec r <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "GainLife expects [playerRef, quantity]"
    "ExchangeLifeTotals" -> Common.withValue mv (fmap Effect.ExchangeLifeTotals . ExchangeSides.fromJson)
    "IncreaseSpeed" -> case mv of
      Just (Value.Array (Array.MkArray [r, q])) -> Effect.IncreaseSpeed <$> Codec.decode PlayerRef.codec r <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "IncreaseSpeed expects [playerRef, quantity]"
    -- The three-element form is read by JSON type: an Object is the
    -- EntryRiders, anything else is the slot name, which is what lets the
    -- riders be elided without leaving a hole in the array.
    "Create" -> case mv of
      Just (Value.Array (Array.MkArray [q, c])) -> Effect.Create <$> Codec.decode Quantity.codec q <*> decode c <*> pure EntryRiders.defaultValue <*> pure Nothing
      Just (Value.Array (Array.MkArray [q, c, e@(Value.Object _)])) -> Effect.Create <$> Codec.decode Quantity.codec q <*> decode c <*> EntryRiders.fromJson e <*> pure Nothing
      Just (Value.Array (Array.MkArray [q, c, s])) -> Effect.Create <$> Codec.decode Quantity.codec q <*> decode c <*> pure EntryRiders.defaultValue <*> (Just <$> Codec.decode SlotName.codec s)
      Just (Value.Array (Array.MkArray [q, c, e, s])) -> Effect.Create <$> Codec.decode Quantity.codec q <*> decode c <*> EntryRiders.fromJson e <*> (Just <$> Codec.decode SlotName.codec s)
      _ -> Left . Text.pack $ "Create expects [Quantity, Card], optionally with EntryRiders and/or a slot"
    -- Told apart by LENGTH. Not implemented: a ref whose own encoding is a
    -- two-element array is read as this pair and fails on the quantity, so
    -- ObjectRef.TopOfLibrary has no bare spelling here (#1311). A three-element
    -- one -- ObjectRef.EachCardInGraveyard -- falls through to the bare-ref
    -- branch and is unaffected.
    "CreateCopy" -> case mv of
      Just (Value.Array (Array.MkArray [q, r])) -> Effect.CreateCopy <$> Codec.decode Quantity.codec q <*> Codec.decode ObjectRef.codec r
      _ -> Common.withValue mv (fmap (Effect.CreateCopy (Quantity.Literal 1)) . Codec.decode ObjectRef.codec)
    -- The three shapes the encoder above can emit, told apart by LENGTH.
    "ArmDelayedTrigger" -> case mv of
      Just (Value.Array (Array.MkArray [n, o, d])) ->
        Effect.ArmDelayedTrigger <$> Codec.decode AbilityName.codec n <*> Codec.decode Onset.codec o <*> Common.decodeMaybe Duration.fromJson d
      Just (Value.Array (Array.MkArray [n, d])) ->
        Effect.ArmDelayedTrigger <$> Codec.decode AbilityName.codec n <*> pure Onset.Immediately <*> fmap Just (Duration.fromJson d)
      _ -> Common.withValue mv (fmap (\n -> Effect.ArmDelayedTrigger n Onset.Immediately Nothing) . Codec.decode AbilityName.codec)
    "Replace" -> case mv of
      Just (Value.Array (Array.MkArray [d, u, o, c, re])) -> do
        duration <- Duration.fromJson d
        uses <- Codec.decode Uses.codec u
        origin <- Codec.decode ReplacementOrigin.codec o
        condition <- Common.decodeMaybe (Codec.decode Condition.codec) c
        effect <- ReplacementEffect.fromJson re
        pure (Effect.Replace duration uses origin condition effect)
      _ -> Left . Text.pack $ "Replace expects [Duration, Uses, ReplacementOrigin, Maybe Condition, ReplacementEffect]"
    "SkipNextPhase" -> case mv of
      Just (Value.Array (Array.MkArray [r, sel])) -> Effect.SkipNextPhase <$> Codec.decode PlayerRef.codec r <*> Codec.decode PhaseSelector.codec sel
      _ -> Left . Text.pack $ "SkipNextPhase expects [playerRef, phaseSelector]"
    "PreventNextDamage" -> case mv of
      Just (Value.Array (Array.MkArray [d, r, q])) -> Effect.PreventNextDamage <$> Duration.fromJson d <*> Codec.decode ObjectRef.codec r <*> Codec.decode Quantity.codec q <*> pure Seq.empty
      Just (Value.Array (Array.MkArray [d, r, q, rider])) ->
        Effect.PreventNextDamage <$> Duration.fromJson d <*> Codec.decode ObjectRef.codec r <*> Codec.decode Quantity.codec q <*> Common.decodeSeq (fromJson decode) rider
      _ -> Left . Text.pack $ "PreventNextDamage expects [Duration, ObjectRef, Quantity], optionally with a CR 615.5 rider"
    "PreventAllDamage" -> case mv of
      Just (Value.Array (Array.MkArray [d, r])) -> Effect.PreventAllDamage <$> Duration.fromJson d <*> Codec.decode ObjectRef.codec r
      _ -> Left . Text.pack $ "PreventAllDamage expects [Duration, ObjectRef]"
    "RedirectDamage" -> case mv of
      Just (Value.Array (Array.MkArray [d, k, from, to])) -> Effect.RedirectDamage <$> Duration.fromJson d <*> Common.decodeMaybe (Codec.decode DamageKind.codec) k <*> Codec.decode ObjectRef.codec from <*> Codec.decode ObjectRef.codec to
      _ -> Left . Text.pack $ "RedirectDamage expects [Duration, Maybe DamageKind, ObjectRef, ObjectRef]"
    "PutCounters" -> case mv of
      Just (Value.Array (Array.MkArray [k, q, r])) -> Effect.PutCounters <$> Codec.decode (CounterKind.codec Keyword.codec) k <*> Codec.decode Quantity.codec q <*> Codec.decode ObjectRef.codec r
      _ -> Left . Text.pack $ "PutCounters expects [counterKind, quantity, objectRef]"
    "RemoveCounters" -> case mv of
      Just (Value.Array (Array.MkArray [k, q, s])) -> Effect.RemoveCounters <$> Codec.decode (CounterKind.codec Keyword.codec) k <*> Codec.decode Quantity.codec q <*> Codec.decode SlotName.codec s
      _ -> Left . Text.pack $ "RemoveCounters expects [counterKind, quantity, slot]"
    "GainPlayerCounters" -> case mv of
      Just (Value.Array (Array.MkArray [r, k, q])) -> Effect.GainPlayerCounters <$> Codec.decode PlayerRef.codec r <*> Codec.decode PlayerCounterKind.codec k <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "GainPlayerCounters expects [playerRef, playerCounterKind, quantity]"
    "RemovePlayerCounters" -> case mv of
      Just (Value.Array (Array.MkArray [r, k, q])) -> Effect.RemovePlayerCounters <$> Codec.decode PlayerRef.codec r <*> Codec.decode PlayerCounterKind.codec k <*> Codec.decode Quantity.codec q
      _ -> Left . Text.pack $ "RemovePlayerCounters expects [playerRef, playerCounterKind, quantity]"
    "Tap" -> Common.withValue mv (fmap Effect.Tap . Codec.decode ObjectRef.codec)
    "Untap" -> Common.withValue mv (fmap Effect.Untap . Codec.decode ObjectRef.codec)
    "Transform" -> Common.withValue mv (fmap Effect.Transform . Codec.decode ObjectRef.codec)
    "AddPhases" -> case mv of
      Just (Value.Array (Array.MkArray ps)) -> Effect.AddPhases <$> traverse (Codec.decode ExtraPhase.codec) ps
      _ -> Left . Text.pack $ "AddPhases expects [ExtraPhase]"
    "GainControl" -> case mv of
      Just (Value.Array (Array.MkArray [d, r])) -> Effect.GainControl <$> Duration.fromJson d <*> Codec.decode ObjectRef.codec r
      _ -> Left . Text.pack $ "GainControl expects [duration, objectRef]"
    "AffectPlayers" -> case mv of
      Just (Value.Array (Array.MkArray [d, s, pe])) -> Effect.AffectPlayers <$> Duration.fromJson d <*> AffectedPlayers.fromJson s <*> PlayerEffect.fromJson pe
      _ -> Left . Text.pack $ "AffectPlayers expects [Duration, AffectedPlayers, PlayerEffect]"
    "RequireBlock" -> case mv of
      Just (Value.Array (Array.MkArray [d, b, a])) -> Effect.RequireBlock <$> Duration.fromJson d <*> Codec.decode ObjectRef.codec b <*> Codec.decode ObjectRef.codec a
      _ -> Left . Text.pack $ "RequireBlock expects [Duration, ObjectRef, ObjectRef]"
    "CreateEmblem" -> Common.withValue mv (fmap Effect.CreateEmblem . decode)
    "BecomeMonarch" -> Common.withValue mv (fmap Effect.BecomeMonarch . MonarchTarget.fromJson)
    "Designate" -> case mv of
      Just (Value.Array (Array.MkArray [d, s])) -> Effect.Designate <$> Designation.fromJson d <*> Codec.decode SlotName.codec s
      _ -> Left . Text.pack $ "Designate expects [designation, slot]"
    "Unsuspect" -> Common.withValue mv (fmap Effect.Unsuspect . Codec.decode ObjectRef.codec)
    "Evolve" -> Common.withValue mv (fmap Effect.Evolve . Codec.decode SlotName.codec)
    "Mentor" -> Common.withValue mv (fmap Effect.Mentor . Codec.decode SlotName.codec)
    "ItBecomes" -> Common.withValue mv (fmap Effect.ItBecomes . Codec.decode Daytime.codec)
    "ExileUntilMonarch" -> Common.withValue mv (fmap Effect.ExileUntilMonarch . Codec.decode SlotName.codec)
    "Attach" -> Common.withValue mv (fmap Effect.Attach . Codec.decode SlotName.codec)
    "AttachTarget" -> case mv of
      Just (Value.Array (Array.MkArray [s, f])) -> Effect.AttachTarget <$> Codec.decode SlotName.codec s <*> Codec.decode (Filter.codec Keyword.codec) f
      _ -> Left . Text.pack $ "AttachTarget expects [slot, filter]"
    "PlaySubgame" -> Common.withValue mv (fmap Effect.PlaySubgame . Codec.decode SlotName.codec)
    "TakeExtraTurn" -> case mv of
      Just (Value.Array (Array.MkArray [r, skips])) -> Effect.TakeExtraTurn <$> Codec.decode PlayerRef.codec r <*> Common.decodeSet (Codec.decode PhaseSelector.codec) skips
      _ -> Left . Text.pack $ "TakeExtraTurn expects [playerRef, phaseSelectors]"
    "ShuffleIntoLibrary" -> case mv of
      Just (Value.Array (Array.MkArray [p, r])) ->
        Effect.ShuffleIntoLibrary <$> fmap Just (Codec.decode PlayerRef.codec p) <*> Codec.decode ObjectRef.codec r
      _ -> Common.withValue mv (fmap (Effect.ShuffleIntoLibrary Nothing) . Codec.decode ObjectRef.codec)
    "OfferCast" -> case mv of
      Just (Value.Array (Array.MkArray [s, o])) -> Effect.OfferCast <$> Codec.decode SlotName.codec s <*> Codec.decode CastOffer.codec o
      _ -> Common.withValue mv (fmap (\s -> Effect.OfferCast s CastOffer.defaultValue) . Codec.decode SlotName.codec)
    "GrantPlayFromExile" -> case mv of
      Just (Value.Array (Array.MkArray [d, r])) -> Effect.GrantPlayFromExile <$> Duration.fromJson d <*> Codec.decode ObjectRef.codec r
      _ -> Left . Text.pack $ "GrantPlayFromExile expects [duration, objectRef]"
    _ -> Left . Text.pack $ "unknown Effect: " <> t
