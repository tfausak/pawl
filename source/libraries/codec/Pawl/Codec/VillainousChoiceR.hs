{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.VillainousChoiceR where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.VillainousChoiceRewrite as VillainousChoiceRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.VillainousChoiceR as VillainousChoiceR

codec :: Codec.Codec VillainousChoiceR.VillainousChoiceR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec VillainousChoiceR.whose
  rewrite <- Fields.required "rewrite" VillainousChoiceRewrite.codec VillainousChoiceR.rewrite
  pure
    VillainousChoiceR.MkVillainousChoiceR
      { VillainousChoiceR.whose = whose,
        VillainousChoiceR.rewrite = rewrite
      }
