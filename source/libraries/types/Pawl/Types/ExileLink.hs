module Pawl.Types.ExileLink where

import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 607.2a: what an exiled card is linked to -- the object whose ability
-- exiled it, and that ability's name when it has one, so a reference naming one
-- of two exiling abilities reads only its own pile.
--
-- Nothing where the exiling ability is unnamed, and for CR 607.2b's and CR
-- 614.14's links, which no reference names an ability of.
data ExileLink = MkExileLink
  { source :: ObjectId.ObjectId,
    ability :: Maybe AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
