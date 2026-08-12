module Pawl.Codec.EntryRewrite where

import qualified Data.Text as Text
import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.EntryOption as EntryOption
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EntryRewrite as EntryRewrite

toJson :: EntryRewrite.EntryRewrite -> Value.Value
toJson r = case r of
  -- CR 707.9: the exceptions are omitted when there are none, the posture
  -- Common.optionalPair takes on a record field -- a plain Clone's rewrite stays
  -- the nullary tag it has always been.
  EntryRewrite.AsCopy [] -> Common.nullary "AsCopy"
  EntryRewrite.AsCopy exceptions -> Common.tagged "AsCopy" . Just $ Common.encodeList (Codec.encode CopyException.codec) exceptions
  EntryRewrite.ChoiceOf options -> Common.tagged "ChoiceOf" . Just $ Common.encodeList (Codec.encode EntryOption.codec) options
  EntryRewrite.WithCounters kind n -> Common.tagged "WithCounters" . Just . Value.array $ [Codec.encode (CounterKind.codec Keyword.codec) kind, Common.encodeNatural n]
  EntryRewrite.ChooseColor -> Common.nullary "ChooseColor"
  EntryRewrite.ChooseBasicLandType -> Common.nullary "ChooseBasicLandType"
  EntryRewrite.ChooseCardNames f -> Common.tagged "ChooseCardNames" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  EntryRewrite.UnderSourceControl -> Common.nullary "UnderSourceControl"
  EntryRewrite.Riot -> Common.nullary "Riot"
  EntryRewrite.Unleash -> Common.nullary "Unleash"
  EntryRewrite.Tapped -> Common.nullary "Tapped"
  EntryRewrite.PayLifeOrTapped n -> Common.tagged "PayLifeOrTapped" . Just $ Common.encodeNatural n
  EntryRewrite.SacrificeAnyNumber f kind -> Common.tagged "SacrificeAnyNumber" . Just . Value.array $ [Codec.encode (Filter.codec Keyword.codec) f, Common.encodeMaybe (Codec.encode (CounterKind.codec Keyword.codec)) kind]

fromJson :: Value.Value -> Either Text.Text EntryRewrite.EntryRewrite
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("AsCopy", Nothing) -> Right (EntryRewrite.AsCopy [])
    ("AsCopy", Just v) -> EntryRewrite.AsCopy <$> Common.decodeList (Codec.decode CopyException.codec) v
    ("ChooseColor", _) -> Right EntryRewrite.ChooseColor
    ("ChooseBasicLandType", _) -> Right EntryRewrite.ChooseBasicLandType
    ("UnderSourceControl", _) -> Right EntryRewrite.UnderSourceControl
    ("Riot", _) -> Right EntryRewrite.Riot
    ("Unleash", _) -> Right EntryRewrite.Unleash
    ("Tapped", _) -> Right EntryRewrite.Tapped
    ("PayLifeOrTapped", Just v) -> EntryRewrite.PayLifeOrTapped <$> Common.decodeNatural v
    ("ChoiceOf", Just v) -> EntryRewrite.ChoiceOf <$> Common.decodeList (Codec.decode EntryOption.codec) v
    ("ChooseCardNames", Just v) -> EntryRewrite.ChooseCardNames <$> Codec.decode (Filter.codec Keyword.codec) v
    ("SacrificeAnyNumber", Just (Value.Array (Array.MkArray [f, k]))) -> do
      criterion <- Codec.decode (Filter.codec Keyword.codec) f
      kind <- Common.decodeMaybe (Codec.decode (CounterKind.codec Keyword.codec)) k
      pure (EntryRewrite.SacrificeAnyNumber criterion kind)
    ("WithCounters", Just (Value.Array (Array.MkArray [k, n]))) -> do
      kind <- Codec.decode (CounterKind.codec Keyword.codec) k
      count <- Common.decodeNatural n
      pure (EntryRewrite.WithCounters kind count)
    _ -> Left . Text.pack $ "unknown EntryRewrite: " <> t
