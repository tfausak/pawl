{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.AsCopySpec where

import qualified Pawl.Codec.AsCopy as AsCopy
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.SetPowerToughness as SetPowerToughness

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AsCopy" $ do
  -- CR 707.5: Clone's "any creature", excepting nothing -- the empty exception
  -- list is defaulted away.
  Spec.it s "MkAsCopy, no exceptions: the key is omitted" $
    Common.assertCodec
      s
      AsCopy.codec
      (AsCopy.MkAsCopy (Filter.HasCardType CardType.Creature) [])
      """ {"eligible":{"type":"HasCardType","value":{"type":"Creature"}}} """
  -- CR 707.9d: Quicksilver Gargantuan's "except it's 7/7", beside the same
  -- eligible set.
  Spec.it s "MkAsCopy, an exception: both keys" $
    Common.assertCodec
      s
      AsCopy.codec
      (AsCopy.MkAsCopy (Filter.HasCardType CardType.Creature) [CopyException.SetPowerToughness (SetPowerToughness.MkSetPowerToughness 7 7)])
      """ {"eligible":{"type":"HasCardType","value":{"type":"Creature"}},"exceptions":[{"type":"SetPowerToughness","value":{"power":7,"toughness":7}}]} """
  Spec.it s "has a schema" $ Common.assertHasSchema s AsCopy.codec
