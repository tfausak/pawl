module Pawl.Codec.CombatRestriction where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Affected as Affected.Type
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Condition as Condition.Type

-- | TAGGED, where the requirement codecs are objects with one named key: this
-- type is a sum over which declaration the restriction forbids and in what
-- shape, while the two requirement types are newtypes over one field each.
--
-- The tag's payload is an OBJECT rather than the Affected itself, because each
-- arm carries two things and the second is CR 508.1c's "unless" gate. Named
-- keys and not a positional array, for Pawl.Codec.Condition's reason:
-- "affected" and "unless" cannot be swapped by accident. Every arm shares that
-- payload, so the tag is the whole of what distinguishes them.
toJson :: CombatRestriction.CombatRestriction -> Value.Value
toJson cr = case cr of
  CombatRestriction.CantAttack a c -> Common.tagged "CantAttack" . Just $ payload a c
  CombatRestriction.CantBlock a c -> Common.tagged "CantBlock" . Just $ payload a c
  CombatRestriction.CantAttackAlone a c -> Common.tagged "CantAttackAlone" . Just $ payload a c

-- | "unless" is OMITTED when the restriction is unconditional, rather than
-- written as null: an absent key is how every optional field in this codec
-- spells "the card does not say this".
payload :: Affected.Type.Affected -> Maybe Condition.Type.Condition -> Value.Value
payload a c =
  Common.object
    ( Common.requiredPair "affected" Affected.toJson a
        <> Common.optionalPair "unless" Nothing (Common.encodeMaybe Condition.toJson) c
    )

fromJson :: Value.Value -> Either Text.Text CombatRestriction.CombatRestriction
fromJson value = do
  (t, mv) <- Common.asTagged value
  case t of
    "CantAttack" -> Common.withValue mv (payloadFromJson CombatRestriction.CantAttack)
    "CantBlock" -> Common.withValue mv (payloadFromJson CombatRestriction.CantBlock)
    "CantAttackAlone" -> Common.withValue mv (payloadFromJson CombatRestriction.CantAttackAlone)
    _ -> Left . Text.pack $ "unknown CombatRestriction: " <> t

payloadFromJson ::
  (Affected.Type.Affected -> Maybe Condition.Type.Condition -> CombatRestriction.CombatRestriction) ->
  Value.Value ->
  Either Text.Text CombatRestriction.CombatRestriction
payloadFromJson mk value = do
  ps <- Common.asObject value
  a <- Common.field "affected" ps >>= Affected.fromJson
  c <- Common.defaultedField "unless" Nothing (Common.decodeMaybe Condition.fromJson) ps
  pure $ mk a c
