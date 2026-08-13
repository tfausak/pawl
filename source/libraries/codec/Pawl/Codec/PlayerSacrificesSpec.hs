{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerSacrificesSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerSacrifices" $ do
  -- CR 701.21: the players the SLOT names each sacrifice this many permanents
  -- matching the filter. The slot is a player reference, not the sacrificed
  -- object -- the filter is what says which permanents qualify.
  Spec.it s "MkPlayerSacrifices, all three keys" $
    Common.assertCodec
      s
      PlayerSacrifices.codec
      ( PlayerSacrifices.MkPlayerSacrifices
          { PlayerSacrifices.slot = SlotName.MkSlotName (Text.pack "player"),
            PlayerSacrifices.filter = Filter.HasCardType CardType.Creature,
            PlayerSacrifices.quantity = Quantity.Literal 1
          }
      )
      """ {"slot":"player","filter":{"type":"HasCardType","value":{"type":"Creature"}},"quantity":{"type":"Literal","value":1}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerSacrifices.codec
