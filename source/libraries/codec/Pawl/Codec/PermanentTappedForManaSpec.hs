module Pawl.Codec.PermanentTappedForManaSpec where

import qualified Pawl.Codec.PermanentTappedForMana as PermanentTappedForMana
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaSpecification as ManaSpecification
import qualified Pawl.Types.PermanentTappedForMana as PermanentTappedForMana
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Supertype as Supertype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentTappedForMana" $ do
  -- Autumn Willow, Harmony's payload: whose tap fires it, the quality the tapped
  -- permanent has to have, and the mana narrowing its printed sentence does not
  -- make.
  Spec.it s "MkPermanentTappedForMana, every key" $
    Common.assertCodec
      s
      PermanentTappedForMana.codec
      ( PermanentTappedForMana.MkPermanentTappedForMana
          { PermanentTappedForMana.player = PlayerRelation.You,
            PermanentTappedForMana.filter = Filter.HasCardType CardType.Land,
            PermanentTappedForMana.mana = ManaSpecification.AnyMana
          }
      )
      " {\"player\":{\"type\":\"You\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"mana\":{\"type\":\"AnyMana\"}} "
  -- Gauntlet of Power's, whose third key is the one that differs: a codec that
  -- defaulted the narrowing rather than reading it would encode the two alike.
  Spec.it s "MkPermanentTappedForMana, a narrowed one" $
    Common.assertCodec
      s
      PermanentTappedForMana.codec
      ( PermanentTappedForMana.MkPermanentTappedForMana
          { PermanentTappedForMana.player = PlayerRelation.AnyPlayer,
            PermanentTappedForMana.filter = Filter.HasSupertype Supertype.Basic,
            PermanentTappedForMana.mana = ManaSpecification.ChosenColor
          }
      )
      " {\"player\":{\"type\":\"AnyPlayer\"},\"filter\":{\"type\":\"HasSupertype\",\"value\":{\"type\":\"Basic\"}},\"mana\":{\"type\":\"ChosenColor\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentTappedForMana.codec
