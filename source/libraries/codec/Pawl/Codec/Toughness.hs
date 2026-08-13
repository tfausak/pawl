module Pawl.Codec.Toughness where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Toughness as Toughness

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- a @$defs@ entry under this newtype's own name.
codec :: Codec.Codec Toughness.Toughness
codec = Common.wrapper Quantity.codec Toughness.MkToughness Toughness.unwrap
