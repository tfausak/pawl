module Pawl.Codec.Emperors where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TeamId as TeamId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Emperors as Emperors

-- | Keyed by a TeamId, a Natural newtype, so it takes 'Common.naturalMap' --
-- Pawl.Codec.Teams' shape with key and value swapped.
codec :: Codec.Codec Emperors.Emperors
codec = Common.wrapper (Common.naturalMap TeamId.codec PlayerId.codec) Emperors.MkEmperors Emperors.unwrap
