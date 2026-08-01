module Pawl.Codec.Filter where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Filter as Filter

-- | CR 702.29e's typecycling filter, absent for plain cycling: null rather than an
-- omitted key, because this rides inside a positional pair.
optional :: Value.Value -> Either Text.Text (Maybe Filter.Filter)
optional = Common.decodeMaybe fromJson

-- | Recursive, mirroring Quantity's toJson/fromJson: And/Or carry their operands
-- as a JSON Array, Not as a single nested object, and each atom delegates to the
-- leaf-enum codec for the characteristic it cases on.
toJson :: Filter.Filter -> Value.Value
toJson filter_ = case filter_ of
  Filter.HasCardType t -> Common.tagged "HasCardType" . Just $ CardType.toJson t
  Filter.HasSupertype sup -> Common.tagged "HasSupertype" . Just $ Supertype.toJson sup
  Filter.HasColor c -> Common.tagged "HasColor" . Just $ Color.toJson c
  Filter.HasSubtype sub -> Common.tagged "HasSubtype" . Just $ Subtype.toJson sub
  Filter.PowerAtLeast n -> Common.tagged "PowerAtLeast" . Just $ Common.integer n
  Filter.ControlledBy r -> Common.tagged "ControlledBy" . Just $ PlayerRelation.toJson r
  Filter.IsPlayer r -> Common.tagged "IsPlayer" . Just $ PlayerRelation.toJson r
  Filter.IsSource -> Common.nullary "IsSource"
  Filter.IsAttacking -> Common.nullary "IsAttacking"
  Filter.IsBlocking -> Common.nullary "IsBlocking"
  Filter.AttackedThisTurn -> Common.nullary "AttackedThisTurn"
  Filter.IsAttachedToCreature -> Common.nullary "IsAttachedToCreature"
  Filter.IsAttachedToPermanent -> Common.nullary "IsAttachedToPermanent"
  Filter.CanHostSubject -> Common.nullary "CanHostSubject"
  Filter.IsToken -> Common.nullary "IsToken"
  Filter.And fs -> Common.tagged "And" . Just . Common.array $ fmap toJson fs
  Filter.Or fs -> Common.tagged "Or" . Just . Common.array $ fmap toJson fs
  Filter.Not f -> Common.tagged "Not" . Just $ toJson f

fromJson :: Value.Value -> Either Text.Text Filter.Filter
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("HasCardType", Just v) -> Filter.HasCardType <$> CardType.fromJson v
    ("HasSupertype", Just v) -> Filter.HasSupertype <$> Supertype.fromJson v
    ("HasColor", Just v) -> Filter.HasColor <$> Color.fromJson v
    ("HasSubtype", Just v) -> Filter.HasSubtype <$> Subtype.fromJson v
    ("PowerAtLeast", Just v) -> Filter.PowerAtLeast <$> Common.asInteger v
    ("ControlledBy", Just v) -> Filter.ControlledBy <$> PlayerRelation.fromJson v
    ("IsPlayer", Just v) -> Filter.IsPlayer <$> PlayerRelation.fromJson v
    ("IsSource", _) -> Right Filter.IsSource
    ("IsAttacking", _) -> Right Filter.IsAttacking
    ("IsBlocking", _) -> Right Filter.IsBlocking
    ("AttackedThisTurn", _) -> Right Filter.AttackedThisTurn
    ("IsAttachedToCreature", _) -> Right Filter.IsAttachedToCreature
    ("IsAttachedToPermanent", _) -> Right Filter.IsAttachedToPermanent
    ("CanHostSubject", _) -> Right Filter.CanHostSubject
    ("IsToken", _) -> Right Filter.IsToken
    ("And", Just (Value.Array (Array.MkArray vs))) -> Filter.And <$> traverse fromJson vs
    ("Or", Just (Value.Array (Array.MkArray vs))) -> Filter.Or <$> traverse fromJson vs
    ("Not", Just v) -> Filter.Not <$> fromJson v
    _ -> Left . Text.pack $ "unknown Filter: " <> t
