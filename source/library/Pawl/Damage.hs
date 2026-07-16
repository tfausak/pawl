module Pawl.Damage where

import qualified Data.Map.Strict as Map
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object

-- CR 514.2: during the cleanup step, all damage marked on permanents is removed.
--
-- Every object, not just battlefield ones: the field exists on all of them, and
-- CR 514.2 says "all damage marked on permanents (including phased-out
-- permanents)" -- there is no reason to be selective, and being selective is how
-- a stale mark survives.
removeAllDamage :: GameState -> GameState
removeAllDamage gs =
  let clear obj = obj {Object.damage = 0}
   in gs {GameState.objects = Map.map clear (GameState.objects gs)}
