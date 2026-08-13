module Pawl.Codec.CombatRestriction where

import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Affected as Affected.Type
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Keyword as Keyword.Type

-- | TAGGED, where the requirement codecs are objects with one named key: this
-- type is a sum over which declaration the restriction forbids and in what
-- shape, while the two requirement types are newtypes over one field each.
--
-- The tag's payload is an OBJECT rather than the Affected itself, because every
-- arm carries at least two things and the last is CR 508.1c's "unless" gate.
-- Named keys and not a positional array, for Pawl.Codec.Condition's reason:
-- "affected" and "unless" cannot be swapped by accident. The subject-carrying
-- arms share one payload, the SIZE-BOUNDING ones share another, which spells its
-- first key "limit" instead of "affected", and the PAIRWISE one has its own,
-- which adds "blockers" -- so the tag plus the key set is the whole of what
-- distinguishes them.
toJson :: CombatRestriction.CombatRestriction -> Value.Value
toJson cr = case cr of
  CombatRestriction.CantAttack a c -> Common.tagged "CantAttack" . Just $ payload a c
  CombatRestriction.CantBlock a c -> Common.tagged "CantBlock" . Just $ payload a c
  CombatRestriction.CantBeBlockedBy a f c -> Common.tagged "CantBeBlockedBy" . Just $ blockerPayload a f c
  CombatRestriction.CantAttackAlone a c -> Common.tagged "CantAttackAlone" . Just $ payload a c
  CombatRestriction.CantAttackMoreThan n c -> Common.tagged "CantAttackMoreThan" . Just $ boundPayload n c
  CombatRestriction.CantBlockMoreThan n c -> Common.tagged "CantBlockMoreThan" . Just $ boundPayload n c

-- | "unless" is OMITTED when the restriction is unconditional, rather than
-- written as null: an absent key is how every optional field in this codec
-- spells "the card does not say this".
payload :: Affected.Type.Affected -> Maybe Condition.Type.Condition -> Value.Value
payload a c =
  Value.object
    ( Common.requiredPair "affected" Affected.toJson a
        <> Common.optionalPair "unless" Nothing (Common.encodeMaybe (Codec.encode Condition.codec)) c
    )

-- | The PAIRWISE arm's payload: `payload` plus the object it names. "blockers"
-- and not a second "affected", because the two Filter positions are not
-- interchangeable -- one is the set of attackers restricted, the other describes
-- the blockers barred from them.
blockerPayload :: Affected.Type.Affected -> Filter.Type.Filter Keyword.Type.Keyword -> Maybe Condition.Type.Condition -> Value.Value
blockerPayload a f c =
  Value.object
    ( Common.requiredPair "affected" Affected.toJson a
        <> Common.requiredPair "blockers" (Codec.encode (Filter.codec Keyword.codec)) f
        <> Common.optionalPair "unless" Nothing (Common.encodeMaybe (Codec.encode Condition.codec)) c
    )

-- | The SIZE-BOUNDING arms' payload. "limit" and not "affected", because a bound
-- names no creature: the key set is what tells a reader of the card file which
-- shape it is looking at without consulting the tag.
boundPayload :: Natural.Natural -> Maybe Condition.Type.Condition -> Value.Value
boundPayload n c =
  Value.object
    ( Common.requiredPair "limit" Common.encodeNatural n
        <> Common.optionalPair "unless" Nothing (Common.encodeMaybe (Codec.encode Condition.codec)) c
    )

-- | This dispatch is a STRING match, so a missing arm here compiles and fails
-- only when a card file is loaded. Pawl.Codec.CombatRestrictionSpec round-trips
-- every arm, which is what turns that into a test failure.
fromJson :: Value.Value -> Either Text.Text CombatRestriction.CombatRestriction
fromJson value = do
  (t, mv) <- Common.asTagged value
  case t of
    "CantAttack" -> Common.withValue mv (payloadFromJson CombatRestriction.CantAttack)
    "CantBlock" -> Common.withValue mv (payloadFromJson CombatRestriction.CantBlock)
    "CantBeBlockedBy" -> Common.withValue mv blockerFromJson
    "CantAttackAlone" -> Common.withValue mv (payloadFromJson CombatRestriction.CantAttackAlone)
    "CantAttackMoreThan" -> Common.withValue mv (boundFromJson CombatRestriction.CantAttackMoreThan)
    "CantBlockMoreThan" -> Common.withValue mv (boundFromJson CombatRestriction.CantBlockMoreThan)
    _ -> Left . Text.pack $ "unknown CombatRestriction: " <> t

payloadFromJson ::
  (Affected.Type.Affected -> Maybe Condition.Type.Condition -> CombatRestriction.CombatRestriction) ->
  Value.Value ->
  Either Text.Text CombatRestriction.CombatRestriction
payloadFromJson mk value = do
  ps <- Common.asObject value
  a <- Common.field "affected" ps >>= Affected.fromJson
  c <- Common.defaultedField "unless" Nothing (Common.decodeMaybe (Codec.decode Condition.codec)) ps
  pure $ mk a c

blockerFromJson :: Value.Value -> Either Text.Text CombatRestriction.CombatRestriction
blockerFromJson value = do
  ps <- Common.asObject value
  a <- Common.field "affected" ps >>= Affected.fromJson
  f <- Common.field "blockers" ps >>= Codec.decode (Filter.codec Keyword.codec)
  c <- Common.defaultedField "unless" Nothing (Common.decodeMaybe (Codec.decode Condition.codec)) ps
  pure $ CombatRestriction.CantBeBlockedBy a f c

boundFromJson ::
  (Natural.Natural -> Maybe Condition.Type.Condition -> CombatRestriction.CombatRestriction) ->
  Value.Value ->
  Either Text.Text CombatRestriction.CombatRestriction
boundFromJson mk value = do
  ps <- Common.asObject value
  n <- Common.field "limit" ps >>= Common.decodeNatural
  c <- Common.defaultedField "unless" Nothing (Common.decodeMaybe (Codec.decode Condition.codec)) ps
  pure $ mk n c
