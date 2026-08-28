module Pawl.Codec.LifeLossRSpec where

import qualified Pawl.Codec.LifeLossR as LifeLossR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.LifeLossCause as LifeLossCause
import qualified Pawl.Types.LifeLossPattern as LifeLossPattern
import qualified Pawl.Types.LifeLossR as LifeLossR
import qualified Pawl.Types.LifeLossRewrite as LifeLossRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LifeLossR" $ do
  -- CR 614.1a / 120.4c: Worship.
  Spec.it s "MkLifeLossR" $
    Common.assertCodec
      s
      LifeLossR.codec
      ( LifeLossR.MkLifeLossR
          LifeLossPattern.MkLifeLossPattern
            { LifeLossPattern.whose = ControllerRelation.Yours,
              LifeLossPattern.whichCause = Just LifeLossCause.ByDamage
            }
          (LifeLossRewrite.LeaveAtLeast 1)
      )
      " {\"matching\":{\"whose\":{\"type\":\"Yours\"},\"whichCause\":{\"type\":\"ByDamage\"}},\"rewrite\":{\"type\":\"LeaveAtLeast\",\"value\":1}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeLossR.codec
