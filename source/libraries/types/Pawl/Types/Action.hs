module Pawl.Types.Action where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.ObjectId as ObjectId

-- | Grows: special actions beyond Play, …
data Action
  = Pass
  | Play ObjectId.ObjectId
  | Cast ObjectId.ObjectId
  | -- | CR 602: activate the source permanent's ability. Carries the ability
    -- VALUE (validated by membership in Projection.abilitiesOf), never an
    -- index.
    Activate ObjectId.ObjectId (ActivatedAbility.ActivatedAbility Card.Card)
  deriving (Eq, Ord, Show)
