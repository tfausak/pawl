module Pawl.Codec.CardName where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CardName as CardName

codec :: Codec.Codec CardName.CardName
codec = Common.wrapper Common.text CardName.MkCardName CardName.unwrap
