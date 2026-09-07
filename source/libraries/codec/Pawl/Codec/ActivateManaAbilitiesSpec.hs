module Pawl.Codec.ActivateManaAbilitiesSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ActivateManaAbilities as ActivateManaAbilities
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivateManaAbilities as ActivateManaAbilities
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActivateManaAbilities" $ do
  -- CR 605.3: the player the reference names activates a mana ability of each
  -- permanent they control that the filter matches -- Drain Power's "each land".
  Spec.it s "MkActivateManaAbilities, both keys" $
    Common.assertCodec
      s
      ActivateManaAbilities.codec
      ( ActivateManaAbilities.MkActivateManaAbilities
          { ActivateManaAbilities.filter = Filter.HasCardType CardType.Land,
            ActivateManaAbilities.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"player\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ActivateManaAbilities.codec
