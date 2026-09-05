module Pawl.Codec.CopyExceptionSpec where

import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SetPowerToughness as SetPowerToughness

-- CR 707.9: the "except ..." clause of a copy effect. Quicksilver Gargantuan,
-- the printed card CR 707.9b's power/toughness arm comes from, is square, so the
-- asymmetric case below is what actually pins the pair's order -- as Dack's
-- Duplicate's two unequal keywords pin the CR 707.9a arm's ascending array, and
-- two unequal card types the type arm's, where Phyrexian Metamorph names one.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CopyException" $ do
  Spec.it s "SetPowerToughness round-trips" $
    Common.assertCodec
      s
      CopyException.codec
      (CopyException.SetPowerToughness (SetPowerToughness.MkSetPowerToughness 7 7))
      " {\"type\":\"SetPowerToughness\",\"value\":{\"power\":7,\"toughness\":7}} "

  Spec.it s "SetPowerToughness writes power before toughness" $
    Common.assertCodec
      s
      CopyException.codec
      (CopyException.SetPowerToughness (SetPowerToughness.MkSetPowerToughness 4 5))
      " {\"type\":\"SetPowerToughness\",\"value\":{\"power\":4,\"toughness\":5}} "

  Spec.it s "GainKeywords round-trips, ascending by keyword" $
    Common.assertCodec
      s
      CopyException.codec
      (CopyException.GainKeywords (Set.fromList [Keyword.Dethrone, Keyword.Haste]))
      " {\"type\":\"GainKeywords\",\"value\":[{\"type\":\"Haste\"},{\"type\":\"Dethrone\"}]} "

  Spec.it s "AddCardTypes round-trips, ascending by card type" $
    Common.assertCodec
      s
      CopyException.codec
      (CopyException.AddCardTypes (Set.fromList [CardType.Enchantment, CardType.Artifact]))
      " {\"type\":\"AddCardTypes\",\"value\":[{\"type\":\"Artifact\"},{\"type\":\"Enchantment\"}]} "

  Spec.it s "rejects a payload of the wrong length" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"SetPowerToughness\",\"value\":[4]} ") >>= Codec.decode CopyException.codec))
      "expected a decode failure"

  Spec.it s "has a schema" $
    Common.assertHasSchema s CopyException.codec
