{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlanarDieRolled where

import qualified Pawl.Codec.PlanarDieFace as PlanarDieFace
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlanarDieRolled as PlanarDieRolled

codec :: Codec.Codec PlanarDieRolled.PlanarDieRolled
codec = Fields.object $ do
  roller <- Fields.required "roller" PlayerId.codec PlanarDieRolled.roller
  face <- Fields.required "face" PlanarDieFace.codec PlanarDieRolled.face
  pure PlanarDieRolled.MkPlanarDieRolled {PlanarDieRolled.roller = roller, PlanarDieRolled.face = face}
