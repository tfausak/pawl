module Pawl.Codec.Effect where

import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.AffectPlayers as AffectPlayers
import qualified Pawl.Codec.AttachTarget as AttachTarget
import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.ChangeText as ChangeText
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.CreateCopy as CreateCopy
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.Daytime as Daytime
import qualified Pawl.Codec.DealDamage as DealDamage
import qualified Pawl.Codec.Designate as Designate
import qualified Pawl.Codec.Destroy as Destroy
import qualified Pawl.Codec.Discard as Discard
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.DurationRef as DurationRef
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.ExchangeSides as ExchangeSides
import qualified Pawl.Codec.ExileHaunting as ExileHaunting
import qualified Pawl.Codec.ExtraPhase as ExtraPhase
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaProduction as ManaProduction
import qualified Pawl.Codec.Mill as Mill
import qualified Pawl.Codec.ModifyTarget as ModifyTarget
import qualified Pawl.Codec.MonarchTarget as MonarchTarget
import qualified Pawl.Codec.MoveToZone as MoveToZone
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Onset as Onset
import qualified Pawl.Codec.PlayerCounters as PlayerCounters
import qualified Pawl.Codec.PlayerQuantity as PlayerQuantity
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Codec.PutCounters as PutCounters
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.RemoveCounters as RemoveCounters
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Codec.RequireBlock as RequireBlock
import qualified Pawl.Codec.SearchDestination as SearchDestination
import qualified Pawl.Codec.SkipNextPhase as SkipNextPhase
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.SpeedDecrease as SpeedDecrease
import qualified Pawl.Codec.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Codec.Uses as Uses
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Effect as Effect
-- These type modules share an alias with their codec module, the posture Onset
-- already took here: the names never collide, since a codec module exports
-- functions and a type module exports the type a signature here names.
import qualified Pawl.Types.Onset as Onset

toJson :: (card -> Value.Value) -> Effect.Effect card -> Value.Value
toJson codec e = case e of
  Effect.DealDamage x -> Common.tagged "DealDamage" . Just $ Codec.encode DealDamage.codec x
  Effect.ModifyTarget x -> Common.tagged "ModifyTarget" . Just $ Codec.encode ModifyTarget.codec x
  Effect.ChangeText x -> Common.tagged "ChangeText" . Just $ Codec.encode ChangeText.codec x
  Effect.AddMana production -> Common.tagged "AddMana" (Just (Codec.encode ManaProduction.codec production))
  Effect.Search s o q f d -> Common.tagged "Search" (Just (Value.array [Codec.encode PlayerRef.codec s, Codec.encode PlayerRef.codec o, Codec.encode Quantity.codec q, Codec.encode (Filter.codec Keyword.codec) f, Codec.encode SearchDestination.codec d]))
  Effect.ExileAllGraveyards -> Common.nullary "ExileAllGraveyards"
  Effect.Proliferate -> Common.nullary "Proliferate"
  Effect.TemptWithTheRing -> Common.nullary "TemptWithTheRing"
  Effect.Venture -> Common.nullary "Venture"
  Effect.ExileHandThenDraw -> Common.nullary "ExileHandThenDraw"
  Effect.PlayerSacrifices x -> Common.tagged "PlayerSacrifices" . Just $ Codec.encode PlayerSacrifices.codec x
  Effect.RestartGame -> Common.nullary "RestartGame"
  Effect.ControlPlayerNextTurn s -> Common.tagged "ControlPlayerNextTurn" (Just (Codec.encode SlotName.codec s))
  Effect.Destroy d -> Common.tagged "Destroy" . Just $ Codec.encode Destroy.codec d
  Effect.Sacrifice s -> Common.tagged "Sacrifice" (Just (Codec.encode SlotName.codec s))
  Effect.TurnFaceDown s -> Common.tagged "TurnFaceDown" (Just (Codec.encode SlotName.codec s))
  Effect.RemoveFromCombat s -> Common.tagged "RemoveFromCombat" (Just (Codec.encode SlotName.codec s))
  Effect.Counter s -> Common.tagged "Counter" (Just (Codec.encode SlotName.codec s))
  Effect.MoveToZone m -> Common.tagged "MoveToZone" . Just $ Codec.encode MoveToZone.codec m
  Effect.Draw x -> Common.tagged "Draw" . Just $ Codec.encode PlayerQuantity.codec x
  Effect.Scry x -> Common.tagged "Scry" . Just $ Codec.encode PlayerQuantity.codec x
  Effect.Surveil x -> Common.tagged "Surveil" . Just $ Codec.encode PlayerQuantity.codec x
  Effect.Fateseal x -> Common.tagged "Fateseal" . Just $ Codec.encode PlayerQuantity.codec x
  Effect.Explore r -> Common.tagged "Explore" (Just (Codec.encode ObjectRef.codec r))
  Effect.Mill m -> Common.tagged "Mill" . Just $ Codec.encode Mill.codec m
  Effect.Discard x -> Common.tagged "Discard" . Just $ Codec.encode Discard.codec x
  Effect.LoseLife x -> Common.tagged "LoseLife" . Just $ Codec.encode PlayerQuantity.codec x
  Effect.GainLife x -> Common.tagged "GainLife" . Just $ Codec.encode PlayerQuantity.codec x
  Effect.ExchangeLifeTotals sides -> Common.tagged "ExchangeLifeTotals" (Just (Codec.encode ExchangeSides.codec sides))
  Effect.SetLifeTotal x -> Common.tagged "SetLifeTotal" . Just $ Codec.encode PlayerQuantity.codec x
  Effect.RedistributeLifeTotals -> Common.nullary "RedistributeLifeTotals"
  Effect.IncreaseSpeed x -> Common.tagged "IncreaseSpeed" . Just $ Codec.encode PlayerQuantity.codec x
  Effect.DecreaseSpeed x -> Common.tagged "DecreaseSpeed" . Just $ Codec.encode SpeedDecrease.codec x
  -- Create's payload is positional, and the EntryRiders are ELIDED when they
  -- are the CR 110.5b default. The three-element form is therefore two shapes,
  -- told apart on decode by JSON TYPE rather than by position: a slot name is a
  -- string and riders are an object, so the two can never be confused.
  --
  -- The LAST arm still shaped this way. Its record waits on this module becoming
  -- a bundle, since Create is parametric in the card and a record codec needs a
  -- whole Codec for it (#1305, and #1306's constraint one level up).
  Effect.Create q c te ms ->
    Common.tagged "Create" . Just . Value.array $
      [Codec.encode Quantity.codec q, codec c]
        <> (if te == EntryRiders.defaultValue then [] else [Codec.encode EntryRiders.codec te])
        <> fmap (Codec.encode SlotName.codec) (Maybe.maybeToList ms)
  Effect.CreateCopy c -> Common.tagged "CreateCopy" . Just $ Codec.encode CreateCopy.codec c
  Effect.Replace d u o c re ->
    Common.tagged "Replace" . Just . Value.array $
      [Codec.encode Duration.codec d, Codec.encode Uses.codec u, Codec.encode ReplacementOrigin.codec o, Common.encodeMaybe (Codec.encode Condition.codec) c, Codec.encode ReplacementEffect.codec re]
  Effect.SkipNextPhase x -> Common.tagged "SkipNextPhase" . Just $ Codec.encode SkipNextPhase.codec x
  -- CR 615.5's additional effect is ELIDED when it is empty, which is Create's
  -- posture above and every other prevention in the corpus: a shield with no
  -- rider stays the three-element form it has always had.
  Effect.PreventNextDamage d r q rider ->
    Common.tagged "PreventNextDamage" . Just . Value.array $
      [Codec.encode Duration.codec d, Codec.encode ObjectRef.codec r, Codec.encode Quantity.codec q]
        <> [Common.encodeSeq (toJson codec) rider | not (Seq.null rider)]
  Effect.PreventAllDamage x -> Common.tagged "PreventAllDamage" . Just $ Codec.encode DurationRef.codec x
  Effect.RedirectDamage d k f t -> Common.tagged "RedirectDamage" (Just (Value.array [Codec.encode Duration.codec d, Common.encodeMaybe (Codec.encode DamageKind.codec) k, Codec.encode ObjectRef.codec f, Codec.encode ObjectRef.codec t]))
  Effect.PutCounters x -> Common.tagged "PutCounters" . Just $ Codec.encode PutCounters.codec x
  Effect.RemoveCounters x -> Common.tagged "RemoveCounters" . Just $ Codec.encode RemoveCounters.codec x
  Effect.GainPlayerCounters x -> Common.tagged "GainPlayerCounters" . Just $ Codec.encode PlayerCounters.codec x
  Effect.RemovePlayerCounters x -> Common.tagged "RemovePlayerCounters" . Just $ Codec.encode PlayerCounters.codec x
  Effect.Tap r -> Common.tagged "Tap" (Just (Codec.encode ObjectRef.codec r))
  Effect.Untap r -> Common.tagged "Untap" (Just (Codec.encode ObjectRef.codec r))
  Effect.Transform r -> Common.tagged "Transform" (Just (Codec.encode ObjectRef.codec r))
  Effect.AddPhases ps -> Common.tagged "AddPhases" (Just (Value.array (fmap (Codec.encode ExtraPhase.codec) ps)))
  Effect.GainControl x -> Common.tagged "GainControl" . Just $ Codec.encode DurationRef.codec x
  -- The duration is ELIDED when absent (CR 603.7b's default) and the onset when
  -- it is CR 603.7a's default, so a one-shot entry stays a bare ability name
  -- and a stated duration alone writes the two-element form. A stated onset
  -- takes the THREE-element form, whose last element is the duration or null.
  -- LENGTH, not JSON type, tells the forms apart: an onset and a duration are
  -- both tagged objects.
  Effect.ArmDelayedTrigger n o md -> Common.tagged "ArmDelayedTrigger" . Just $ case (o, md) of
    (Onset.Immediately, Nothing) -> Codec.encode AbilityName.codec n
    (Onset.Immediately, Just d) -> Value.array [Codec.encode AbilityName.codec n, Codec.encode Duration.codec d]
    _ -> Value.array [Codec.encode AbilityName.codec n, Codec.encode Onset.codec o, Common.encodeMaybe (Codec.encode Duration.codec) md]
  Effect.AffectPlayers x -> Common.tagged "AffectPlayers" . Just $ Codec.encode AffectPlayers.codec x
  Effect.RequireBlock x -> Common.tagged "RequireBlock" . Just $ Codec.encode RequireBlock.codec x
  Effect.CreateEmblem c -> Common.tagged "CreateEmblem" (Just (codec c))
  Effect.BecomeMonarch t -> Common.tagged "BecomeMonarch" (Just (Codec.encode MonarchTarget.codec t))
  Effect.Designate x -> Common.tagged "Designate" . Just $ Codec.encode Designate.codec x
  Effect.Unsuspect r -> Common.tagged "Unsuspect" (Just (Codec.encode ObjectRef.codec r))
  Effect.Evolve s -> Common.tagged "Evolve" (Just (Codec.encode SlotName.codec s))
  Effect.Mentor s -> Common.tagged "Mentor" (Just (Codec.encode SlotName.codec s))
  Effect.ItBecomes d -> Common.tagged "ItBecomes" (Just (Codec.encode Daytime.codec d))
  Effect.ExileUntilMonarch s -> Common.tagged "ExileUntilMonarch" (Just (Codec.encode SlotName.codec s))
  Effect.ExileHaunting x -> Common.tagged "ExileHaunting" . Just $ Codec.encode ExileHaunting.codec x
  Effect.Attach s -> Common.tagged "Attach" (Just (Codec.encode SlotName.codec s))
  Effect.AttachTarget x -> Common.tagged "AttachTarget" . Just $ Codec.encode AttachTarget.codec x
  Effect.PlaySubgame s -> Common.tagged "PlaySubgame" (Just (Codec.encode SlotName.codec s))
  Effect.ChooseOpponent s -> Common.tagged "ChooseOpponent" (Just (Codec.encode SlotName.codec s))
  Effect.TakeExtraTurn x -> Common.tagged "TakeExtraTurn" . Just $ Codec.encode TakeExtraTurn.codec x
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
  Effect.GrantPlayFromExile x -> Common.tagged "GrantPlayFromExile" . Just $ Codec.encode DurationRef.codec x

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Effect.Effect card)
fromJson decode value = do
  (t, mv) <- Common.asTagged value
  case t of
    "DealDamage" -> Common.withValue mv (fmap Effect.DealDamage . Codec.decode DealDamage.codec)
    "ModifyTarget" -> Common.withValue mv (fmap Effect.ModifyTarget . Codec.decode ModifyTarget.codec)
    "ChangeText" -> Common.withValue mv (fmap Effect.ChangeText . Codec.decode ChangeText.codec)
    "AddMana" -> Common.withValue mv (fmap Effect.AddMana . Codec.decode ManaProduction.codec)
    "Search" -> case mv of
      Just (Value.Array (Array.MkArray [s, o, q, f, d])) -> Effect.Search <$> Codec.decode PlayerRef.codec s <*> Codec.decode PlayerRef.codec o <*> Codec.decode Quantity.codec q <*> Codec.decode (Filter.codec Keyword.codec) f <*> Codec.decode SearchDestination.codec d
      _ -> Left . Text.pack $ "Search expects [searcher, libraryOwner, quantity, filter, destination]"
    "ExileAllGraveyards" -> Right Effect.ExileAllGraveyards
    "Proliferate" -> Right Effect.Proliferate
    "TemptWithTheRing" -> Right Effect.TemptWithTheRing
    "Venture" -> Right Effect.Venture
    "ExileHandThenDraw" -> Right Effect.ExileHandThenDraw
    "PlayerSacrifices" -> Common.withValue mv (fmap Effect.PlayerSacrifices . Codec.decode PlayerSacrifices.codec)
    "RestartGame" -> Right Effect.RestartGame
    "ControlPlayerNextTurn" -> Common.withValue mv (fmap Effect.ControlPlayerNextTurn . Codec.decode SlotName.codec)
    "Destroy" -> Common.withValue mv (fmap Effect.Destroy . Codec.decode Destroy.codec)
    "Sacrifice" -> Common.withValue mv (fmap Effect.Sacrifice . Codec.decode SlotName.codec)
    "TurnFaceDown" -> Common.withValue mv (fmap Effect.TurnFaceDown . Codec.decode SlotName.codec)
    "RemoveFromCombat" -> Common.withValue mv (fmap Effect.RemoveFromCombat . Codec.decode SlotName.codec)
    "Counter" -> Common.withValue mv (fmap Effect.Counter . Codec.decode SlotName.codec)
    "MoveToZone" -> Common.withValue mv (fmap Effect.MoveToZone . Codec.decode MoveToZone.codec)
    "Draw" -> Common.withValue mv (fmap Effect.Draw . Codec.decode PlayerQuantity.codec)
    "Scry" -> Common.withValue mv (fmap Effect.Scry . Codec.decode PlayerQuantity.codec)
    "Surveil" -> Common.withValue mv (fmap Effect.Surveil . Codec.decode PlayerQuantity.codec)
    "Fateseal" -> Common.withValue mv (fmap Effect.Fateseal . Codec.decode PlayerQuantity.codec)
    "Explore" -> Common.withValue mv (fmap Effect.Explore . Codec.decode ObjectRef.codec)
    "Mill" -> Common.withValue mv (fmap Effect.Mill . Codec.decode Mill.codec)
    "Discard" -> Common.withValue mv (fmap Effect.Discard . Codec.decode Discard.codec)
    "LoseLife" -> Common.withValue mv (fmap Effect.LoseLife . Codec.decode PlayerQuantity.codec)
    "GainLife" -> Common.withValue mv (fmap Effect.GainLife . Codec.decode PlayerQuantity.codec)
    "ExchangeLifeTotals" -> Common.withValue mv (fmap Effect.ExchangeLifeTotals . Codec.decode ExchangeSides.codec)
    "SetLifeTotal" -> Common.withValue mv (fmap Effect.SetLifeTotal . Codec.decode PlayerQuantity.codec)
    "RedistributeLifeTotals" -> Right Effect.RedistributeLifeTotals
    "IncreaseSpeed" -> Common.withValue mv (fmap Effect.IncreaseSpeed . Codec.decode PlayerQuantity.codec)
    "DecreaseSpeed" -> Common.withValue mv (fmap Effect.DecreaseSpeed . Codec.decode SpeedDecrease.codec)
    -- The three-element form is read by JSON type: an Object is the
    -- EntryRiders, anything else is the slot name, which is what lets the
    -- riders be elided without leaving a hole in the array.
    "Create" -> case mv of
      Just (Value.Array (Array.MkArray [q, c])) -> Effect.Create <$> Codec.decode Quantity.codec q <*> decode c <*> pure EntryRiders.defaultValue <*> pure Nothing
      Just (Value.Array (Array.MkArray [q, c, e@(Value.Object _)])) -> Effect.Create <$> Codec.decode Quantity.codec q <*> decode c <*> Codec.decode EntryRiders.codec e <*> pure Nothing
      Just (Value.Array (Array.MkArray [q, c, s])) -> Effect.Create <$> Codec.decode Quantity.codec q <*> decode c <*> pure EntryRiders.defaultValue <*> (Just <$> Codec.decode SlotName.codec s)
      Just (Value.Array (Array.MkArray [q, c, e, s])) -> Effect.Create <$> Codec.decode Quantity.codec q <*> decode c <*> Codec.decode EntryRiders.codec e <*> (Just <$> Codec.decode SlotName.codec s)
      _ -> Left . Text.pack $ "Create expects [Quantity, Card], optionally with EntryRiders and/or a slot"
    "CreateCopy" -> Common.withValue mv (fmap Effect.CreateCopy . Codec.decode CreateCopy.codec)
    -- The three shapes the encoder above can emit, told apart by LENGTH.
    "ArmDelayedTrigger" -> case mv of
      Just (Value.Array (Array.MkArray [n, o, d])) ->
        Effect.ArmDelayedTrigger <$> Codec.decode AbilityName.codec n <*> Codec.decode Onset.codec o <*> Common.decodeMaybe (Codec.decode Duration.codec) d
      Just (Value.Array (Array.MkArray [n, d])) ->
        Effect.ArmDelayedTrigger <$> Codec.decode AbilityName.codec n <*> pure Onset.Immediately <*> fmap Just (Codec.decode Duration.codec d)
      _ -> Common.withValue mv (fmap (\n -> Effect.ArmDelayedTrigger n Onset.Immediately Nothing) . Codec.decode AbilityName.codec)
    "Replace" -> case mv of
      Just (Value.Array (Array.MkArray [d, u, o, c, re])) -> do
        duration <- Codec.decode Duration.codec d
        uses <- Codec.decode Uses.codec u
        origin <- Codec.decode ReplacementOrigin.codec o
        condition <- Common.decodeMaybe (Codec.decode Condition.codec) c
        effect <- Codec.decode ReplacementEffect.codec re
        pure (Effect.Replace duration uses origin condition effect)
      _ -> Left . Text.pack $ "Replace expects [Duration, Uses, ReplacementOrigin, Maybe Condition, ReplacementEffect]"
    "SkipNextPhase" -> Common.withValue mv (fmap Effect.SkipNextPhase . Codec.decode SkipNextPhase.codec)
    "PreventNextDamage" -> case mv of
      Just (Value.Array (Array.MkArray [d, r, q])) -> Effect.PreventNextDamage <$> Codec.decode Duration.codec d <*> Codec.decode ObjectRef.codec r <*> Codec.decode Quantity.codec q <*> pure Seq.empty
      Just (Value.Array (Array.MkArray [d, r, q, rider])) ->
        Effect.PreventNextDamage <$> Codec.decode Duration.codec d <*> Codec.decode ObjectRef.codec r <*> Codec.decode Quantity.codec q <*> Common.decodeSeq (fromJson decode) rider
      _ -> Left . Text.pack $ "PreventNextDamage expects [Duration, ObjectRef, Quantity], optionally with a CR 615.5 rider"
    "PreventAllDamage" -> Common.withValue mv (fmap Effect.PreventAllDamage . Codec.decode DurationRef.codec)
    "RedirectDamage" -> case mv of
      Just (Value.Array (Array.MkArray [d, k, from, to])) -> Effect.RedirectDamage <$> Codec.decode Duration.codec d <*> Common.decodeMaybe (Codec.decode DamageKind.codec) k <*> Codec.decode ObjectRef.codec from <*> Codec.decode ObjectRef.codec to
      _ -> Left . Text.pack $ "RedirectDamage expects [Duration, Maybe DamageKind, ObjectRef, ObjectRef]"
    "PutCounters" -> Common.withValue mv (fmap Effect.PutCounters . Codec.decode PutCounters.codec)
    "RemoveCounters" -> Common.withValue mv (fmap Effect.RemoveCounters . Codec.decode RemoveCounters.codec)
    "GainPlayerCounters" -> Common.withValue mv (fmap Effect.GainPlayerCounters . Codec.decode PlayerCounters.codec)
    "RemovePlayerCounters" -> Common.withValue mv (fmap Effect.RemovePlayerCounters . Codec.decode PlayerCounters.codec)
    "Tap" -> Common.withValue mv (fmap Effect.Tap . Codec.decode ObjectRef.codec)
    "Untap" -> Common.withValue mv (fmap Effect.Untap . Codec.decode ObjectRef.codec)
    "Transform" -> Common.withValue mv (fmap Effect.Transform . Codec.decode ObjectRef.codec)
    "AddPhases" -> case mv of
      Just (Value.Array (Array.MkArray ps)) -> Effect.AddPhases <$> traverse (Codec.decode ExtraPhase.codec) ps
      _ -> Left . Text.pack $ "AddPhases expects [ExtraPhase]"
    "GainControl" -> Common.withValue mv (fmap Effect.GainControl . Codec.decode DurationRef.codec)
    "AffectPlayers" -> Common.withValue mv (fmap Effect.AffectPlayers . Codec.decode AffectPlayers.codec)
    "RequireBlock" -> Common.withValue mv (fmap Effect.RequireBlock . Codec.decode RequireBlock.codec)
    "CreateEmblem" -> Common.withValue mv (fmap Effect.CreateEmblem . decode)
    "BecomeMonarch" -> Common.withValue mv (fmap Effect.BecomeMonarch . Codec.decode MonarchTarget.codec)
    "Designate" -> Common.withValue mv (fmap Effect.Designate . Codec.decode Designate.codec)
    "Unsuspect" -> Common.withValue mv (fmap Effect.Unsuspect . Codec.decode ObjectRef.codec)
    "Evolve" -> Common.withValue mv (fmap Effect.Evolve . Codec.decode SlotName.codec)
    "Mentor" -> Common.withValue mv (fmap Effect.Mentor . Codec.decode SlotName.codec)
    "ItBecomes" -> Common.withValue mv (fmap Effect.ItBecomes . Codec.decode Daytime.codec)
    "ExileUntilMonarch" -> Common.withValue mv (fmap Effect.ExileUntilMonarch . Codec.decode SlotName.codec)
    "ExileHaunting" -> Common.withValue mv (fmap Effect.ExileHaunting . Codec.decode ExileHaunting.codec)
    "Attach" -> Common.withValue mv (fmap Effect.Attach . Codec.decode SlotName.codec)
    "AttachTarget" -> Common.withValue mv (fmap Effect.AttachTarget . Codec.decode AttachTarget.codec)
    "PlaySubgame" -> Common.withValue mv (fmap Effect.PlaySubgame . Codec.decode SlotName.codec)
    "ChooseOpponent" -> Common.withValue mv (fmap Effect.ChooseOpponent . Codec.decode SlotName.codec)
    "TakeExtraTurn" -> Common.withValue mv (fmap Effect.TakeExtraTurn . Codec.decode TakeExtraTurn.codec)
    "ShuffleIntoLibrary" -> case mv of
      Just (Value.Array (Array.MkArray [p, r])) ->
        Effect.ShuffleIntoLibrary <$> fmap Just (Codec.decode PlayerRef.codec p) <*> Codec.decode ObjectRef.codec r
      _ -> Common.withValue mv (fmap (Effect.ShuffleIntoLibrary Nothing) . Codec.decode ObjectRef.codec)
    "OfferCast" -> case mv of
      Just (Value.Array (Array.MkArray [s, o])) -> Effect.OfferCast <$> Codec.decode SlotName.codec s <*> Codec.decode CastOffer.codec o
      _ -> Common.withValue mv (fmap (\s -> Effect.OfferCast s CastOffer.defaultValue) . Codec.decode SlotName.codec)
    "GrantPlayFromExile" -> Common.withValue mv (fmap Effect.GrantPlayFromExile . Codec.decode DurationRef.codec)
    _ -> Left . Text.pack $ "unknown Effect: " <> t
