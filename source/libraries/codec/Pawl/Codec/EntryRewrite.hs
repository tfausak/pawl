module Pawl.Codec.EntryRewrite where

import qualified Data.Text as Text
import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.EntryOption as EntryOption
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EntryRewrite as EntryRewrite

toJson :: EntryRewrite.EntryRewrite -> Value.Value
toJson r = case r of
  -- CR 707.9: the exceptions are omitted when there are none, the posture
  -- Common.optionalPair takes on a record field -- a plain Clone's rewrite stays
  -- the nullary tag it has always been.
  EntryRewrite.AsCopy [] -> Common.nullary "AsCopy"
  EntryRewrite.AsCopy exceptions -> Common.tagged "AsCopy" . Just $ Common.encodeList CopyException.toJson exceptions
  EntryRewrite.ChoiceOf options -> Common.tagged "ChoiceOf" . Just $ Common.encodeList EntryOption.toJson options
  EntryRewrite.WithCounters kind n -> Common.tagged "WithCounters" . Just . Value.array $ [CounterKind.toJson Keyword.toJson kind, Common.encodeNatural n]
  EntryRewrite.ChooseColor -> Common.nullary "ChooseColor"
  EntryRewrite.ChooseBasicLandType -> Common.nullary "ChooseBasicLandType"
  EntryRewrite.ChooseCardNames f -> Common.tagged "ChooseCardNames" . Just $ Filter.toJson Keyword.toJson f
  EntryRewrite.UnderSourceControl -> Common.nullary "UnderSourceControl"
  EntryRewrite.Riot -> Common.nullary "Riot"
  EntryRewrite.Unleash -> Common.nullary "Unleash"
  EntryRewrite.Tapped -> Common.nullary "Tapped"
  EntryRewrite.PayLifeOrTapped n -> Common.tagged "PayLifeOrTapped" . Just $ Common.encodeNatural n
  EntryRewrite.SacrificeAnyNumber f kind -> Common.tagged "SacrificeAnyNumber" . Just . Value.array $ [Filter.toJson Keyword.toJson f, Common.encodeMaybe (CounterKind.toJson Keyword.toJson) kind]

fromJson :: Value.Value -> Either Text.Text EntryRewrite.EntryRewrite
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("AsCopy", Nothing) -> Right (EntryRewrite.AsCopy [])
    ("AsCopy", Just v) -> EntryRewrite.AsCopy <$> Common.decodeList CopyException.fromJson v
    ("ChooseColor", _) -> Right EntryRewrite.ChooseColor
    ("ChooseBasicLandType", _) -> Right EntryRewrite.ChooseBasicLandType
    ("UnderSourceControl", _) -> Right EntryRewrite.UnderSourceControl
    ("Riot", _) -> Right EntryRewrite.Riot
    ("Unleash", _) -> Right EntryRewrite.Unleash
    ("Tapped", _) -> Right EntryRewrite.Tapped
    ("PayLifeOrTapped", Just v) -> EntryRewrite.PayLifeOrTapped <$> Common.decodeNatural v
    ("ChoiceOf", Just v) -> EntryRewrite.ChoiceOf <$> Common.decodeList EntryOption.fromJson v
    ("ChooseCardNames", Just v) -> EntryRewrite.ChooseCardNames <$> Filter.fromJson Keyword.fromJson v
    ("SacrificeAnyNumber", Just (Value.Array (Array.MkArray [f, k]))) -> do
      criterion <- Filter.fromJson Keyword.fromJson f
      kind <- Common.decodeMaybe (CounterKind.fromJson Keyword.fromJson) k
      pure (EntryRewrite.SacrificeAnyNumber criterion kind)
    ("WithCounters", Just (Value.Array (Array.MkArray [k, n]))) -> do
      kind <- CounterKind.fromJson Keyword.fromJson k
      count <- Common.decodeNatural n
      pure (EntryRewrite.WithCounters kind count)
    _ -> Left . Text.pack $ "unknown EntryRewrite: " <> t
