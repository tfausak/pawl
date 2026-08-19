module Pawl.Codec.Printing where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Printing as Printing

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- a @$defs@ entry under this newtype's own name.
codec :: Codec.Codec Printing.Printing
codec = Common.wrapper Card.codec Printing.MkPrinting Printing.card
