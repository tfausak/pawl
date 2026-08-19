{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.StaticAbility where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.StaticAbility as StaticAbility

-- | CR 613.6: the parts of one ability's effect travel together, so the wire
-- format is one affected set and an ARRAY of modifications -- never one entry
-- per layer.
--
-- The CR 604.2 "as long as" gate is OPTIONAL, and absent means unconditional:
-- every card written before it existed encodes byte-for-byte as it did. The CR
-- 604.2 override beside it -- "if this leaves the battlefield, this effect
-- continues until end of turn" -- is optional for the same reason, and absent
-- means the effect ends with its permanent.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (StaticAbility.StaticAbility card)
codec cardCodec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec StaticAbility.affected
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) StaticAbility.condition
  lingers <- Fields.defaulted "lingers" Nothing (Common.maybe Duration.codec) StaticAbility.lingers
  modifications <- Fields.required "modifications" (Common.nonEmpty (Modification.codec (ActivatedAbility.codec cardCodec))) StaticAbility.modifications
  pure
    StaticAbility.MkStaticAbility
      { StaticAbility.affected = affected,
        StaticAbility.condition = condition,
        StaticAbility.lingers = lingers,
        StaticAbility.modifications = modifications
      }
