{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CantBlockCreatures where

import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CantBlockCreatures as CantBlockCreatures

-- | Pawl.Codec.CantBeBlockedBy's shape, with "attackers" in place of
-- "blockers".
codec :: Codec.Codec CantBlockCreatures.CantBlockCreatures
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec CantBlockCreatures.affected
  attackers <- Fields.required "attackers" (Filter.codec Keyword.codec) CantBlockCreatures.attackers
  unless <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) CantBlockCreatures.unless
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) CantBlockCreatures.name
  pure
    CantBlockCreatures.MkCantBlockCreatures
      { CantBlockCreatures.affected = affected,
        CantBlockCreatures.attackers = attackers,
        CantBlockCreatures.unless = unless,
        CantBlockCreatures.name = name
      }
