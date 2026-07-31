-- | The @Filter ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Filter where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.CardType (cardTypeToJson, jsonToCardType)
import Pawl.Codec.Color (colorToJson, jsonToColor)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.PlayerRelation (jsonToPlayerRelation, playerRelationToJson)
import Pawl.Codec.Subtype (jsonToSubtype, subtypeToJson)
import Pawl.Codec.Supertype (jsonToSupertype, supertypeToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array, Null))
import qualified Pawl.Types.Filter as Filter

-- CR 702.29e's typecycling filter, absent for plain cycling: null rather than an
-- omitted key, because this rides inside a positional pair.
optionalFilter :: Value -> Either Text (Maybe Filter.Filter)
optionalFilter value = case value of
  Null _ -> Right Nothing
  _ -> fmap Just (jsonToFilter value)

-- Recursive, mirroring quantityToJson/jsonToQuantity: And/Or carry their
-- operands as a JSON Array, Not as a single nested object, and each atom
-- delegates to the leaf-enum codec for the characteristic it cases on.
filterToJson :: Filter.Filter -> Value
filterToJson filter_ = case filter_ of
  Filter.HasCardType t -> Json.tagged (Text.pack "HasCardType") (Just (cardTypeToJson t))
  Filter.HasSupertype s -> Json.tagged (Text.pack "HasSupertype") (Just (supertypeToJson s))
  Filter.HasColor c -> Json.tagged (Text.pack "HasColor") (Just (colorToJson c))
  Filter.HasSubtype s -> Json.tagged (Text.pack "HasSubtype") (Just (subtypeToJson s))
  Filter.PowerAtLeast n -> Json.tagged (Text.pack "PowerAtLeast") (Just (Json.jInt n))
  Filter.ControlledBy r -> Json.tagged (Text.pack "ControlledBy") (Just (playerRelationToJson r))
  Filter.IsPlayer r -> Json.tagged (Text.pack "IsPlayer") (Just (playerRelationToJson r))
  Filter.IsSource -> Json.nullary (Text.pack "IsSource")
  Filter.IsAttacking -> Json.nullary (Text.pack "IsAttacking")
  Filter.IsBlocking -> Json.nullary (Text.pack "IsBlocking")
  Filter.AttackedThisTurn -> Json.nullary (Text.pack "AttackedThisTurn")
  Filter.IsAttachedToCreature -> Json.nullary (Text.pack "IsAttachedToCreature")
  Filter.IsAttachedToPermanent -> Json.nullary (Text.pack "IsAttachedToPermanent")
  Filter.CanHostSubject -> Json.nullary (Text.pack "CanHostSubject")
  Filter.IsToken -> Json.nullary (Text.pack "IsToken")
  Filter.And fs -> Json.tagged (Text.pack "And") (Just (Array (MkArray (fmap filterToJson fs))))
  Filter.Or fs -> Json.tagged (Text.pack "Or") (Just (Array (MkArray (fmap filterToJson fs))))
  Filter.Not f -> Json.tagged (Text.pack "Not") (Just (filterToJson f))

jsonToFilter :: Value -> Either Text Filter.Filter
jsonToFilter value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("HasCardType", Just v) -> Filter.HasCardType <$> jsonToCardType v
    ("HasSupertype", Just v) -> Filter.HasSupertype <$> jsonToSupertype v
    ("HasColor", Just v) -> Filter.HasColor <$> jsonToColor v
    ("HasSubtype", Just v) -> Filter.HasSubtype <$> jsonToSubtype v
    ("PowerAtLeast", Just v) -> Filter.PowerAtLeast <$> Json.asInteger v
    ("ControlledBy", Just v) -> Filter.ControlledBy <$> jsonToPlayerRelation v
    ("IsPlayer", Just v) -> Filter.IsPlayer <$> jsonToPlayerRelation v
    ("IsSource", _) -> Right Filter.IsSource
    ("IsAttacking", _) -> Right Filter.IsAttacking
    ("IsBlocking", _) -> Right Filter.IsBlocking
    ("AttackedThisTurn", _) -> Right Filter.AttackedThisTurn
    ("IsAttachedToCreature", _) -> Right Filter.IsAttachedToCreature
    ("IsAttachedToPermanent", _) -> Right Filter.IsAttachedToPermanent
    ("CanHostSubject", _) -> Right Filter.CanHostSubject
    ("IsToken", _) -> Right Filter.IsToken
    ("And", Just (Array (MkArray vs))) -> Filter.And <$> traverse jsonToFilter vs
    ("Or", Just (Array (MkArray vs))) -> Filter.Or <$> traverse jsonToFilter vs
    ("Not", Just v) -> Filter.Not <$> jsonToFilter v
    _ -> Left (Text.pack "unknown Filter: " <> t)
