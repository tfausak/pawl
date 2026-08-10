module Pawl.Codec.Filter where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.KeywordFamily as KeywordFamily
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Filter as Filter

-- | CR 702.29e's typecycling filter, absent for plain cycling: null rather than
-- an omitted key, because this rides inside a positional pair.
optional :: (Value.Value -> Either Text.Text keyword) -> Value.Value -> Either Text.Text (Maybe (Filter.Filter keyword))
optional decode = Common.decodeMaybe (fromJson decode)

-- | Recursive, mirroring Quantity's toJson/fromJson: And/Or carry their
-- operands as a JSON Array, Not as a single nested object, and each atom
-- delegates to the leaf-enum codec for the characteristic it cases on.
--
-- The keyword codec is a PARAMETER: Pawl.Codec.Keyword imports this module for
-- CR 702.29e's typecycling filter, so a direct call to Keyword.toJson here
-- would close a module cycle. Every caller passes Pawl.Codec.Keyword.toJson.
-- Pawl.Codec.KeywordFamily is called DIRECTLY below and needs no parameter,
-- mirroring the types it encodes: a family carries no filter, so that module
-- imports neither this one nor Pawl.Codec.Keyword.
toJson :: (keyword -> Value.Value) -> Filter.Filter keyword -> Value.Value
toJson encode filter_ = case filter_ of
  Filter.HasCardType t -> Common.tagged "HasCardType" . Just $ CardType.toJson t
  Filter.HasSupertype sup -> Common.tagged "HasSupertype" . Just $ Supertype.toJson sup
  Filter.HasColor c -> Common.tagged "HasColor" . Just $ Color.toJson c
  Filter.HasSubtype sub -> Common.tagged "HasSubtype" . Just $ Subtype.toJson sub
  Filter.HasKeyword k -> Common.tagged "HasKeyword" . Just $ encode k
  Filter.HasKeywordFamily f -> Common.tagged "HasKeywordFamily" . Just $ KeywordFamily.toJson f
  Filter.PowerAtLeast n -> Common.tagged "PowerAtLeast" . Just $ Common.integer n
  Filter.PowerAtMost n -> Common.tagged "PowerAtMost" . Just $ Common.integer n
  Filter.PowerLessThanSource -> Common.nullary "PowerLessThanSource"
  Filter.PowerGreaterThanSource -> Common.nullary "PowerGreaterThanSource"
  Filter.ControlledByDefendingPlayer -> Common.nullary "ControlledByDefendingPlayer"
  Filter.ManaValueAtMost n -> Common.tagged "ManaValueAtMost" . Just $ Common.integer n
  Filter.ControlledBy r -> Common.tagged "ControlledBy" . Just $ PlayerRelation.toJson r
  Filter.OwnedBy r -> Common.tagged "OwnedBy" . Just $ PlayerRelation.toJson r
  Filter.IsPlayer r -> Common.tagged "IsPlayer" . Just $ PlayerRelation.toJson r
  Filter.IsSource -> Common.nullary "IsSource"
  Filter.IsAttacking -> Common.nullary "IsAttacking"
  Filter.IsBlocking -> Common.nullary "IsBlocking"
  Filter.AttackedThisTurn -> Common.nullary "AttackedThisTurn"
  Filter.IsAttachedToCreature -> Common.nullary "IsAttachedToCreature"
  Filter.IsAttachedToPermanent -> Common.nullary "IsAttachedToPermanent"
  Filter.CanHostSubject -> Common.nullary "CanHostSubject"
  Filter.IsToken -> Common.nullary "IsToken"
  Filter.IsTapped -> Common.nullary "IsTapped"
  Filter.IsRingBearer -> Common.nullary "IsRingBearer"
  Filter.And fs -> Common.tagged "And" . Just . Common.array $ fmap (toJson encode) fs
  Filter.Or fs -> Common.tagged "Or" . Just . Common.array $ fmap (toJson encode) fs
  Filter.Not f -> Common.tagged "Not" . Just $ toJson encode f

fromJson :: (Value.Value -> Either Text.Text keyword) -> Value.Value -> Either Text.Text (Filter.Filter keyword)
fromJson decode value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("HasCardType", Just v) -> Filter.HasCardType <$> CardType.fromJson v
    ("HasSupertype", Just v) -> Filter.HasSupertype <$> Supertype.fromJson v
    ("HasColor", Just v) -> Filter.HasColor <$> Color.fromJson v
    ("HasSubtype", Just v) -> Filter.HasSubtype <$> Subtype.fromJson v
    ("HasKeyword", Just v) -> Filter.HasKeyword <$> decode v
    ("HasKeywordFamily", Just v) -> Filter.HasKeywordFamily <$> KeywordFamily.fromJson v
    ("PowerAtLeast", Just v) -> Filter.PowerAtLeast <$> Common.asInteger v
    ("PowerAtMost", Just v) -> Filter.PowerAtMost <$> Common.asInteger v
    ("PowerLessThanSource", _) -> Right Filter.PowerLessThanSource
    ("PowerGreaterThanSource", _) -> Right Filter.PowerGreaterThanSource
    ("ControlledByDefendingPlayer", _) -> Right Filter.ControlledByDefendingPlayer
    ("ManaValueAtMost", Just v) -> Filter.ManaValueAtMost <$> Common.asInteger v
    ("ControlledBy", Just v) -> Filter.ControlledBy <$> PlayerRelation.fromJson v
    ("OwnedBy", Just v) -> Filter.OwnedBy <$> PlayerRelation.fromJson v
    ("IsPlayer", Just v) -> Filter.IsPlayer <$> PlayerRelation.fromJson v
    ("IsSource", _) -> Right Filter.IsSource
    ("IsAttacking", _) -> Right Filter.IsAttacking
    ("IsBlocking", _) -> Right Filter.IsBlocking
    ("AttackedThisTurn", _) -> Right Filter.AttackedThisTurn
    ("IsAttachedToCreature", _) -> Right Filter.IsAttachedToCreature
    ("IsAttachedToPermanent", _) -> Right Filter.IsAttachedToPermanent
    ("CanHostSubject", _) -> Right Filter.CanHostSubject
    ("IsToken", _) -> Right Filter.IsToken
    ("IsTapped", _) -> Right Filter.IsTapped
    ("IsRingBearer", _) -> Right Filter.IsRingBearer
    ("And", Just (Value.Array (Array.MkArray vs))) -> Filter.And <$> traverse (fromJson decode) vs
    ("Or", Just (Value.Array (Array.MkArray vs))) -> Filter.Or <$> traverse (fromJson decode) vs
    ("Not", Just v) -> Filter.Not <$> fromJson decode v
    _ -> Left . Text.pack $ "unknown Filter: " <> t
