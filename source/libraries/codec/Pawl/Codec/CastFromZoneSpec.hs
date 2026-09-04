module Pawl.Codec.CastFromZoneSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CastFromZone as CastFromZone
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastFromZone as CastFromZone
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastFromZone" $ do
  -- Yawgmoth's Will's shape: your own graveyard, every card in it.
  Spec.it s "MkCastFromZone, both keys" $
    Common.assertCodec
      s
      CastFromZone.codec
      ( CastFromZone.MkCastFromZone
          { CastFromZone.from = InZone.MkInZone {InZone.zone = Zone.Graveyard, InZone.player = PlayerRef.Relative PlayerRelation.You},
            CastFromZone.matching = Filter.And []
          }
      )
      " {\"from\":{\"zone\":{\"type\":\"Graveyard\"},\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}},\"matching\":{\"type\":\"And\",\"value\":[]}} "
  -- Sen Triplets' shape: the hand of the player a target slot named.
  Spec.it s "a slot names whose zone" $
    Common.assertCodec
      s
      CastFromZone.codec
      ( CastFromZone.MkCastFromZone
          { CastFromZone.from = InZone.MkInZone {InZone.zone = Zone.Hand, InZone.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "opponent"))},
            CastFromZone.matching = Filter.HasCardType CardType.Creature
          }
      )
      " {\"from\":{\"zone\":{\"type\":\"Hand\"},\"player\":{\"type\":\"InSlot\",\"value\":\"opponent\"}},\"matching\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CastFromZone.codec
