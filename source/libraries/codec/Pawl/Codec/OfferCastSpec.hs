module Pawl.Codec.OfferCastSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.OfferCast as OfferCast
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CastOffer as CastOffer.Type
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.OfferCast" $ do
  -- CR 608.2g's bare offer: an ordinary cast of the card, which is the elided
  -- case.
  Spec.it s "MkOfferCast, offer elided" $
    Common.assertCodec
      s
      OfferCast.codec
      ( OfferCast.MkOfferCast
          { OfferCast.slot = SlotName.MkSlotName (Text.pack "exiled"),
            OfferCast.caster = PlayerRef.Relative PlayerRelation.You,
            OfferCast.optionality = Optionality.Optional,
            OfferCast.offer = CastOffer.defaultValue
          }
      )
      " {\"slot\":\"exiled\"} "
  -- CR 310.12b's two riders, which is what stops the offer being elided.
  Spec.it s "MkOfferCast, offer written" $
    Common.assertCodec
      s
      OfferCast.codec
      ( OfferCast.MkOfferCast
          { OfferCast.slot = SlotName.MkSlotName (Text.pack "exiled"),
            OfferCast.caster = PlayerRef.Relative PlayerRelation.You,
            OfferCast.optionality = Optionality.Optional,
            OfferCast.offer =
              CastOffer.Type.MkCastOffer
                { CastOffer.Type.transformed = True,
                  CastOffer.Type.withoutPayingManaCost = True,
                  CastOffer.Type.payingInstead = Nothing
                }
          }
      )
      " {\"slot\":\"exiled\",\"offer\":{\"transformed\":true,\"withoutPayingManaCost\":true}} "
  -- CR 608.2g's OTHER posture -- "instructs", to a player the effect names.
  -- Wild Evocation's, and the pair of keys nothing else in the pool writes.
  Spec.it s "MkOfferCast, caster and optionality written" $
    Common.assertCodec
      s
      OfferCast.codec
      ( OfferCast.MkOfferCast
          { OfferCast.slot = SlotName.MkSlotName (Text.pack "revealed"),
            OfferCast.caster = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer")),
            OfferCast.optionality = Optionality.Mandatory,
            OfferCast.offer =
              CastOffer.Type.MkCastOffer
                { CastOffer.Type.transformed = False,
                  CastOffer.Type.withoutPayingManaCost = True,
                  CastOffer.Type.payingInstead = Nothing
                }
          }
      )
      " {\"slot\":\"revealed\",\"caster\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"optionality\":{\"type\":\"Mandatory\"},\"offer\":{\"withoutPayingManaCost\":true}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s OfferCast.codec
