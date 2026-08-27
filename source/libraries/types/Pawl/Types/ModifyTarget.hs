module Pawl.Types.ModifyTarget where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's ModifyTarget arm (#1305): apply this
-- modification to the objects the ObjectRef names, for this duration.
--
-- Parametric in `ability` for Pawl.Types.Modification's reason, and it is the
-- same variable Pawl.Types.Effect threads down to here: this module cannot NAME
-- an ability, because an ability carries Effects and an Effect carries one of
-- these, so a concrete Pawl.Types.GrantedAbility here would close a module
-- cycle. Pawl.Types.GrantedAbility ties the knot instead, exactly as
-- Pawl.Types.Card ties the `card` one -- which is what lets a card worded
-- "target creature gains '[ability]' until end of turn" resolve into the same
-- Modification a printed static ability grants.
data ModifyTarget ability = MkModifyTarget
  { duration :: Duration.Duration,
    modification :: Modification.Modification ability,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
