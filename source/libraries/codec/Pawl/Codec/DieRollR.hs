{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DieRollR where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.DieRollRewrite as DieRollRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DieRollR as DieRollR

codec :: Codec.Codec DieRollR.DieRollR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec DieRollR.whose
  rewrite <- Fields.required "rewrite" DieRollRewrite.codec DieRollR.rewrite
  pure
    DieRollR.MkDieRollR
      { DieRollR.whose = whose,
        DieRollR.rewrite = rewrite
      }
