module Pawl.Codec.DrawRewriteSpec where

import qualified Pawl.Codec.DrawRewrite as DrawRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DrawRewrite as DrawRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.Types.OutsideDestination as OutsideDestination

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
            { FromOutsideTheGame.count = 1,
              FromOutsideTheGame.upTo = False,
              FromOutsideTheGame.destination = OutsideDestination.Hand,
              FromOutsideTheGame.filter = Filter.And [],
              FromOutsideTheGame.reveal = False
            }
      )
      " {\"type\":\"FromOutsideTheGame\",\"value\":{\"count\":1,\"destination\":{\"type\":\"Hand\"},\"filter\":{\"type\":\"And\",\"value\":[]},\"reveal\":false,\"upTo\":false}} "
  -- CR 702.52a: Darkblast's dredge 3.
  Spec.it s "Dredge" $
    Common.assertCodec
      s
      DrawRewrite.codec
      (DrawRewrite.Dredge 3)
      " {\"type\":\"Dredge\",\"value\":3} "
  -- CR 614.10: Plagiarize.
  Spec.it s "YouDraw" $
    Common.assertCodec
      s
      DrawRewrite.codec
      DrawRewrite.YouDraw
      " {\"type\":\"YouDraw\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DrawRewrite.codec
