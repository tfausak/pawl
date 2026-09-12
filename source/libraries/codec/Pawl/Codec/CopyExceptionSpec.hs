module Pawl.Codec.CopyExceptionSpec where

import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SetPowerToughness as SetPowerToughness
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

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
      (CopyException.codec Common.text)
      (CopyException.SetPowerToughness (SetPowerToughness.MkSetPowerToughness 7 7))
      " {\"type\":\"SetPowerToughness\",\"value\":{\"power\":7,\"toughness\":7}} "

  Spec.it s "SetPowerToughness writes power before toughness" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      (CopyException.SetPowerToughness (SetPowerToughness.MkSetPowerToughness 4 5))
      " {\"type\":\"SetPowerToughness\",\"value\":{\"power\":4,\"toughness\":5}} "

  Spec.it s "GainKeywords round-trips, ascending by keyword" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      (CopyException.GainKeywords (Set.fromList [Keyword.Dethrone, Keyword.Haste]))
      " {\"type\":\"GainKeywords\",\"value\":[{\"type\":\"Haste\"},{\"type\":\"Dethrone\"}]} "

  Spec.it s "AddCardTypes round-trips, ascending by card type" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      (CopyException.AddCardTypes (Set.fromList [CardType.Enchantment, CardType.Artifact]))
      " {\"type\":\"AddCardTypes\",\"value\":[{\"type\":\"Artifact\"},{\"type\":\"Enchantment\"}]} "

  Spec.it s "AddSubtypes round-trips, ascending by subtype" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      (CopyException.AddSubtypes (Set.fromList [Subtype.Shapeshifter, Subtype.Rogue]))
      " {\"type\":\"AddSubtypes\",\"value\":[{\"type\":\"Rogue\"},{\"type\":\"Shapeshifter\"}]} "

  Spec.it s "AddSupertypes round-trips, ascending by supertype" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      (CopyException.AddSupertypes (Set.fromList [Supertype.Snow, Supertype.Legendary]))
      " {\"type\":\"AddSupertypes\",\"value\":[{\"type\":\"Legendary\"},{\"type\":\"Snow\"}]} "

  Spec.it s "RemoveSupertypes round-trips, ascending by supertype" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      (CopyException.RemoveSupertypes (Set.fromList [Supertype.Snow, Supertype.Legendary]))
      " {\"type\":\"RemoveSupertypes\",\"value\":[{\"type\":\"Legendary\"},{\"type\":\"Snow\"}]} "

  Spec.it s "SetName round-trips as a bare string" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      (CopyException.SetName (CardName.MkCardName (Text.pack "Sakashima the Impostor")))
      " {\"type\":\"SetName\",\"value\":\"Sakashima the Impostor\"} "

  -- CR 707.9a's second arm is NULLARY, so the tag alone is the whole value.
  Spec.it s "GainThisAbility round-trips as a bare tag" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      CopyException.GainThisAbility
      " {\"type\":\"GainThisAbility\"} "

  Spec.it s "SetColors round-trips, ascending by colour" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      (CopyException.SetColors (Set.fromList [Color.Black, Color.White]))
      " {\"type\":\"SetColors\",\"value\":[{\"type\":\"White\"},{\"type\":\"Black\"}]} "

  Spec.it s "NoManaCost round-trips as a bare tag" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      CopyException.NoManaCost
      " {\"type\":\"NoManaCost\"} "

  -- CR 707.9c's decline-to-copy arm is NULLARY too, so the tag alone is the
  -- whole value.
  Spec.it s "DontCopyColors round-trips as a bare tag" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      CopyException.DontCopyColors
      " {\"type\":\"DontCopyColors\"} "

  -- CR 707.9a's quoted ability rides the ability codec it is handed; a string
  -- stands in for a Pawl.Types.GrantedAbility here.
  Spec.it s "GainAbility round-trips through the ability codec" $
    Common.assertCodec
      s
      (CopyException.codec Common.text)
      (CopyException.GainAbility (Text.pack "quoted"))
      " {\"type\":\"GainAbility\",\"value\":\"quoted\"} "

  Spec.it s "rejects a payload of the wrong length" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"SetPowerToughness\",\"value\":[4]} ") >>= Codec.decode (CopyException.codec Common.text)))
      "expected a decode failure"

  Spec.it s "has a schema" $
    Common.assertHasSchema s (CopyException.codec Common.text)
