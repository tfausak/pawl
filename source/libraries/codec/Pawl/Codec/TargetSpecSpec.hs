{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TargetSpecSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TargetSpec as TargetSpec
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.TargetSpec as TargetSpec

-- Filter's own coverage lives in Pawl.Codec.FilterSpec, so these cases exercise
-- it only in its embedded position: a bare pool (Nothing filter, omitted key),
-- a filtered pool, and the Not IsSource conjunct carrying CR 601.2c's "another"
-- (#163).
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TargetSpec" $ do
  Spec.it s "MkTargetSpec bare pool" $
    Common.assertJsonCodec
      s
      TargetSpec.toJson
      TargetSpec.fromJson
      (TargetSpec.MkTargetSpec Pool.Creatures Nothing)
      """ {"pool":{"type":"Creatures"}} """
  Spec.it s "MkTargetSpec filtered pool" $
    Common.assertJsonCodec
      s
      TargetSpec.toJson
      TargetSpec.fromJson
      (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.HasCardType CardType.Artifact)))
      """ {"pool":{"type":"Permanents"},"filter":{"type":"HasCardType","value":{"type":"Artifact"}}} """
  Spec.it s "MkTargetSpec \"another\" (Not IsSource)" $
    Common.assertJsonCodec
      s
      TargetSpec.toJson
      TargetSpec.fromJson
      (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.And [Filter.Not (Filter.HasCardType CardType.Land), Filter.Not Filter.IsSource])))
      """ {"pool":{"type":"Permanents"},"filter":{"type":"And","value":[{"type":"Not","value":{"type":"HasCardType","value":{"type":"Land"}}},{"type":"Not","value":{"type":"IsSource"}}]}} """
  -- CR 115.2 clause (a): a target spec over a graveyard, tagged with WHOSE.
  Spec.it s "MkTargetSpec over a graveyard" $
    Common.assertJsonCodec
      s
      TargetSpec.toJson
      TargetSpec.fromJson
      (TargetSpec.MkTargetSpec (Pool.CardsInGraveyard PlayerScope.You) (Just (Filter.HasCardType CardType.Creature)))
      """ {"pool":{"type":"CardsInGraveyard","value":{"type":"You"}},"filter":{"type":"HasCardType","value":{"type":"Creature"}}} """
  -- CR 115.2 clause (a)'s other zone: no PlayerScope (CR 400.1's shared zone)
  -- and no Filter.
  Spec.it s "MkTargetSpec over exile, scopeless and unfiltered" $
    Common.assertJsonCodec
      s
      TargetSpec.toJson
      TargetSpec.fromJson
      (TargetSpec.MkTargetSpec Pool.CardsInExile Nothing)
      """ {"pool":{"type":"CardsInExile"}} """
