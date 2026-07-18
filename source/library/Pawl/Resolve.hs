module Pawl.Resolve where

import Data.Set (Set)
import qualified Data.Set as Set
import Pawl.Type.Effect (Effect)
import qualified Pawl.Type.Effect as Effect
import Pawl.Type.SlotName (SlotName)

-- THE ONE LEGITIMATE HOME of `case effect of`: this module is the VM's opcode
-- semantics (design.md section 1). Everything else asks classifications. The
-- executor itself arrives with resolution; slotsOf is the read half of the
-- dataflow lint.
slotsOf :: Effect -> Set SlotName
slotsOf effect = case effect of
  Effect.DealDamage slot _ -> Set.singleton slot
