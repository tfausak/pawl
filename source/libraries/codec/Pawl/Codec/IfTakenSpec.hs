module Pawl.Codec.IfTakenSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Codec.IfTaken as IfTaken
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.IfTaken as IfTaken

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.IfTaken" $ do
  -- CR 608.2c: Worms of the Earth's "if a player does either", both ordinals.
  Spec.it s "AnyTaken" $
    Common.assertCodec
      s
      IfTaken.codec
      (IfTaken.AnyTaken (ClauseIndex.MkClauseIndex 0 NonEmpty.:| [ClauseIndex.MkClauseIndex 1]))
      " {\"type\":\"AnyTaken\",\"value\":[0,1]} "
  -- CR 608.2c / 118.12a: Development's "unless any opponent has you draw a card".
  Spec.it s "NoneTaken" $
    Common.assertCodec
      s
      IfTaken.codec
      (IfTaken.NoneTaken (NonEmpty.singleton (ClauseIndex.MkClauseIndex 2)))
      " {\"type\":\"NoneTaken\",\"value\":[2]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s IfTaken.codec
