{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BlockPermission where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BlockPermission as BlockPermission

-- | An object with named keys, never a tagged sum: the type has one shape.
-- "additional" is REQUIRED rather than defaulted to one, because a permission
-- that adds nothing is not a thing any card prints and a missing key is far
-- likelier to be a typo than a deliberate zero. It is a whole Quantity, so the
-- common case reads {"type":"Literal","value":1} rather than a bare 1 -- the
-- price of Kemba's Legion's counted permission being the same field.
--
-- Which is also why "any number of creatures" is written as an explicit NULL
-- rather than by omitting the key: it is a positive statement the card makes,
-- and the required key keeps it distinguishable from a typo. "while" is the
-- ordinary optional field, omitted when the card states no gate, exactly as
-- Pawl.Codec.CombatRestriction spells "unless".
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec BlockPermission.BlockPermission
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec BlockPermission.affected
  additional <- Fields.required "additional" (Common.maybe Quantity.codec) BlockPermission.additional
  while <- Fields.defaulted "while" Nothing (Common.maybe Condition.codec) BlockPermission.while
  pure
    BlockPermission.MkBlockPermission
      { BlockPermission.affected = affected,
        BlockPermission.additional = additional,
        BlockPermission.while = while
      }
