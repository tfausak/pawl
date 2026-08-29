module Pawl.Codec.DealDamageSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.DealDamage as DealDamage
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.ExcessDestination as ExcessDestination
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DealDamage" $ do
  -- CR 120.1: this much damage to the objects or players the one clause names,
  -- from the resolving object's own source (CR 113.7) when no dealer is written.
  Spec.it s "MkDealDamage, no dealer" $
    Common.assertCodec
      s
      DealDamage.codec
      ( DealDamage.MkDealDamage
          { DealDamage.parts =
              Seq.singleton
                DamagePart.MkDamagePart
                  { DamagePart.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
                    DamagePart.quantity = Quantity.Literal 3
                  },
            DealDamage.dealer = Nothing,
            DealDamage.excess = Nothing
          }
      )
      " {\"parts\":[{\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":3}}]} "
  -- CR 120.2b's dealer and CR 120.4a's excess destination, the two keys a card
  -- writes only when its sentence says them -- over the SEVERAL clauses one
  -- instruction may name, which CR 608.2f deals as one batch, and whose amounts
  -- may differ (Char).
  Spec.it s "MkDealDamage, two clauses at different amounts plus both optional keys" $
    Common.assertCodec
      s
      DealDamage.codec
      ( DealDamage.MkDealDamage
          { DealDamage.parts =
              Seq.fromList
                [ DamagePart.MkDamagePart
                    { DamagePart.ref = ObjectRef.EachMatching (Filter.HasKeyword Keyword.Flying),
                      DamagePart.quantity = Quantity.Literal 3
                    },
                  DamagePart.MkDamagePart
                    { DamagePart.ref = ObjectRef.EachPlayer,
                      DamagePart.quantity = Quantity.Literal 2
                    }
                ],
            DealDamage.dealer = Just (SlotName.MkSlotName (Text.pack "dealer")),
            DealDamage.excess = Just ExcessDestination.ToRecipientController
          }
      )
      " {\"parts\":[{\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasKeyword\",\"value\":{\"type\":\"Flying\"}}},\"quantity\":{\"type\":\"Literal\",\"value\":3}},{\"ref\":{\"type\":\"EachPlayer\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}}],\"dealer\":\"dealer\",\"excess\":{\"type\":\"ToRecipientController\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DealDamage.codec
