module Pawl.Types.ModifyTarget where

import qualified Data.Void as Void
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's ModifyTarget arm (#1305): apply this
-- modification to the objects the ObjectRef names, for this duration.
--
-- The Modification is instantiated at Void, which says in the type that a
-- modification created by a RESOLUTION cannot grant a quoted ability. That is a
-- module-graph fact and not a rules one: Modification's grant carries a whole
-- Pawl.Types.ActivatedAbility, an ability carries Effects, and an Effect carrying
-- an ability back would close a cycle that no type parameter can open. A card
-- worded "target creature gains '[ability]' until end of turn" therefore has no
-- home here yet (#1642).
--
-- The widening in the other direction is total and lives at
-- Pawl.Engine.Projection.widenModification: what a resolution stores can go
-- anywhere a card's grant can.
data ModifyTarget = MkModifyTarget
  { duration :: Duration.Duration,
    modification :: Modification.Modification Void.Void,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
