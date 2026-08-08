{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ClauseIndexSpec where

import qualified Pawl.Codec.ClauseIndex as ClauseIndex
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ClauseIndex as ClauseIndex

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.ClauseIndex" . Spec.it s "MkClauseIndex" $
    Common.assertJsonCodec
      s
      ClauseIndex.toJson
      ClauseIndex.fromJson
      (ClauseIndex.MkClauseIndex 2)
      """ 2 """
