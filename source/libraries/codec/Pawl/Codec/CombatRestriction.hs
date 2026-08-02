module Pawl.Codec.CombatRestriction where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.CombatRestriction as CombatRestriction

-- | TAGGED, where the requirement codecs are objects with one named key: this type
-- is a sum over which declaration the restriction forbids, and the two
-- requirement types are newtypes over one field each
-- (Pawl.Types.CombatRestriction says why the shapes differ).
toJson :: CombatRestriction.CombatRestriction -> Value.Value
toJson cr = case cr of
  CombatRestriction.CantAttack a -> Common.tagged "CantAttack" . Just $ Affected.toJson a
  CombatRestriction.CantBlock a -> Common.tagged "CantBlock" . Just $ Affected.toJson a

fromJson :: Value.Value -> Either Text.Text CombatRestriction.CombatRestriction
fromJson value = do
  (t, mv) <- Common.asTagged value
  case t of
    "CantAttack" -> Common.withValue mv (fmap CombatRestriction.CantAttack . Affected.fromJson)
    "CantBlock" -> Common.withValue mv (fmap CombatRestriction.CantBlock . Affected.fromJson)
    _ -> Left . Text.pack $ "unknown CombatRestriction: " <> t
