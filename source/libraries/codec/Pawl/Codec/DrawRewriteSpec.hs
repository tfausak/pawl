module Pawl.Codec.DrawRewriteSpec where

import qualified Pawl.Codec.DrawRewrite as DrawRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DrawRewrite as DrawRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DrawRewrite" $ do
  -- CR 614.1a: Words of Worship.
  Spec.it s "GainLife" $
    Common.assertCodec
      s
      DrawRewrite.codec
      (DrawRewrite.GainLife 5)
      " {\"type\":\"GainLife\",\"value\":5} "
  -- CR 400.11c: Ring of Ma'rûf, whose sentence states no quality and prints no
  -- reveal.
  Spec.it s "FromOutsideTheGame" $
    Common.assertCodec
      s
      DrawRewrite.codec
      ( DrawRewrite.FromOutsideTheGame
          FromOutsideTheGame.MkFromOutsideTheGame
            { FromOutsideTheGame.filter = Filter.And [],
              FromOutsideTheGame.reveal = False
            }
      )
      " {\"type\":\"FromOutsideTheGame\",\"value\":{\"filter\":{\"type\":\"And\",\"value\":[]},\"reveal\":false}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DrawRewrite.codec
