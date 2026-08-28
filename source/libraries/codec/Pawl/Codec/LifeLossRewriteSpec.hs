module Pawl.Codec.LifeLossRewriteSpec where

import qualified Pawl.Codec.LifeLossRewrite as LifeLossRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LifeLossRewrite as LifeLossRewrite
import qualified Pawl.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LifeLossRewrite" $ do
  -- Worship's "reduces it to 1 instead".
  Spec.it s "LeaveAtLeast" $
    Common.assertCodec
      s
      LifeLossRewrite.codec
      (LifeLossRewrite.LeaveAtLeast 1)
      " {\"type\":\"LeaveAtLeast\",\"value\":1} "
  -- Bloodletter of Aclazotz' "they lose twice that much life instead".
  Spec.it s "Scaled" $
    Common.assertCodec
      s
      LifeLossRewrite.codec
      (LifeLossRewrite.Scaled (Scaling.Multiply 2))
      " {\"type\":\"Scaled\",\"value\":{\"type\":\"Multiply\",\"value\":2}} "
  -- Ashiok, Wicked Manipulator's "exile that many cards from the top of your
  -- library instead".
  Spec.it s "ExileFromTopOfYourLibrary" $
    Common.assertCodec
      s
      LifeLossRewrite.codec
      LifeLossRewrite.ExileFromTopOfYourLibrary
      " {\"type\":\"ExileFromTopOfYourLibrary\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeLossRewrite.codec
