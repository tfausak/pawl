module Pawl.Codec.TurnUpRSpec where

import qualified Pawl.Codec.TurnUpR as TurnUpR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.WithCounters as WithCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TurnUpR" $ do
  -- Bubble Smuggler's clause, the shape a CARD writes: no `requiring`, so the row
  -- applies down every road CR 614.1e reaches, and the field stays off the wire.
  Spec.it s "MkTurnUpR" $
    Common.assertCodec
      s
      TurnUpR.codec
      ( TurnUpR.MkTurnUpR
          { TurnUpR.matching = Filter.IsSource,
            TurnUpR.requiring = Nothing,
            TurnUpR.rewrite =
              TurnUpRewrite.WithCounters
                (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.Literal 4))
          }
      )
      " {\"matching\":{\"type\":\"IsSource\"},\"rewrite\":{\"type\":\"WithCounters\",\"value\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":4}}]}} "
  -- The other shape, which only Pawl.Engine.Keyword.mintedReplacementsFor's
  -- megamorph arm builds: CR 702.37b's counter, conditional on CR 702.37e's
  -- procedure having paid the megamorph cost; see #987.
  Spec.it s "megamorph's baked requiring (CR 702.37b)" $
    Common.assertCodec
      s
      TurnUpR.codec
      ( TurnUpR.MkTurnUpR
          { TurnUpR.matching = Filter.IsSource,
            TurnUpR.requiring = Just TurnUpProcedure.Morph,
            TurnUpR.rewrite =
              TurnUpRewrite.WithCounters
                (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.Literal 1))
          }
      )
      " {\"matching\":{\"type\":\"IsSource\"},\"requiring\":{\"type\":\"Morph\"},\"rewrite\":{\"type\":\"WithCounters\",\"value\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":1}}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TurnUpR.codec
