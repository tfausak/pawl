{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ControllerBecomesTarget where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.StackObjectKind as StackObjectKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ControllerBecomesTarget as ControllerBecomesTarget

-- | A bare object keyed by the record's field names. The relation is required --
-- a card that narrows to an opponent is a different card, not a defaulted one --
-- while the kind is elided when absent, because "a spell or ability" is the wider
-- and unnarrowed reading of CR 601.2c.
codec :: Codec.Codec ControllerBecomesTarget.ControllerBecomesTarget
codec = Fields.object $ do
  relation <- Fields.required "relation" PlayerRelation.codec ControllerBecomesTarget.relation
  kind <- Fields.defaulted "kind" Nothing (Common.maybe StackObjectKind.codec) ControllerBecomesTarget.kind
  pure
    ControllerBecomesTarget.MkControllerBecomesTarget
      { ControllerBecomesTarget.relation = relation,
        ControllerBecomesTarget.kind = kind
      }
