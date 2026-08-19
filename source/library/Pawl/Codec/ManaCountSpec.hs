module Pawl.Codec.ManaCountSpec where

import qualified Pawl.Codec.ManaCount as ManaCount
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaCount as ManaCount
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaCount" $ do
  -- Omnath, Locus of Mana's "each unspent green mana you have".
  Spec.it s "MkManaCount, your own green" $
    Common.assertCodec
      s
      ManaCount.codec
      (ManaCount.MkManaCount (PlayerRef.Relative PlayerRelation.You) (ManaFilter.OfType (ManaType.Colored Color.Green)))
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}} "
  -- The other end of both fields, so a codec that dropped either payload would
  -- round-trip one of these and not both.
  Spec.it s "MkManaCount, every player's whole pool" $
    Common.assertCodec
      s
      ManaCount.codec
      (ManaCount.MkManaCount PlayerRef.EachPlayer ManaFilter.Any)
      " {\"player\":{\"type\":\"EachPlayer\"},\"filter\":{\"type\":\"Any\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaCount.codec
