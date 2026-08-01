-- | The @CombatRestriction ⇆ Json@ codec (#481).
module Pawl.Codec.CombatRestriction where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Affected (affectedToJson, jsonToAffected)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.CombatRestriction as CombatRestriction

-- TAGGED, where the requirement codecs are objects with one named key: this type
-- is a sum over which declaration the restriction forbids, and the two
-- requirement types are newtypes over one field each
-- (Pawl.Types.CombatRestriction says why the shapes differ).
combatRestrictionToJson :: CombatRestriction.CombatRestriction -> Value
combatRestrictionToJson cr = case cr of
  CombatRestriction.CantAttack a -> Json.tagged (Text.pack "CantAttack") (Just (affectedToJson a))
  CombatRestriction.CantBlock a -> Json.tagged (Text.pack "CantBlock") (Just (affectedToJson a))

jsonToCombatRestriction :: Value -> Either Text CombatRestriction.CombatRestriction
jsonToCombatRestriction value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "CantAttack" -> Json.withValue mv (fmap CombatRestriction.CantAttack . jsonToAffected)
    "CantBlock" -> Json.withValue mv (fmap CombatRestriction.CantBlock . jsonToAffected)
    _ -> Left (Text.pack "unknown CombatRestriction: " <> t)
