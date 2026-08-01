module Pawl.Codec.TargetSpecSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TargetSpec as TargetSpec
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.TargetSpec as TargetSpec

-- Filter's own per-constructor and nested-And/Or/Not coverage lives in
-- Pawl.Codec.FilterSpec; a TargetSpec is Pool + Maybe Filter, so these cases
-- exercise the Filter arm above only in its embedded position. One
-- constructor (MkTargetSpec), so these three cover a bare pool (Nothing
-- filter, omitted key), a filtered pool, and the Not IsSource conjunct that
-- carries CR 601.2c's "another" (#163).
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TargetSpec" $ do
  Spec.it s "MkTargetSpec bare pool" $
    Common.assertJsonCodec
      s
      TargetSpec.toJson
      TargetSpec.fromJson
      (TargetSpec.MkTargetSpec Pool.Creatures Nothing)
      "{\"pool\":{\"type\":\"Creatures\"}}"
  Spec.it s "MkTargetSpec filtered pool" $
    Common.assertJsonCodec
      s
      TargetSpec.toJson
      TargetSpec.fromJson
      (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.HasCardType CardType.Artifact)))
      "{\"pool\":{\"type\":\"Permanents\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}}"
  Spec.it s "MkTargetSpec \"another\" (Not IsSource)" $
    Common.assertJsonCodec
      s
      TargetSpec.toJson
      TargetSpec.fromJson
      (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.And [Filter.Not (Filter.HasCardType CardType.Land), Filter.Not Filter.IsSource])))
      "{\"pool\":{\"type\":\"Permanents\"},\"filter\":{\"type\":\"And\",\"value\":[{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}},{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}]}}"
