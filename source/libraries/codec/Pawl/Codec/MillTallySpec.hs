{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.MillTallySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.MillTally as MillTally
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.MillTally"
    -- CR 728.1's own tally: "for each NONLAND card milled this way".
    . Spec.it s "MkMillTally, a slot and a filter"
    $ Common.assertJsonCodec
      s
      MillTally.toJson
      MillTally.fromJson
      MillTally.MkMillTally
        { MillTally.slot = SlotName.MkSlotName (Text.pack "milled"),
          MillTally.filter = Filter.Not (Filter.HasCardType CardType.Land)
        }
      """ {"slot":"milled","filter":{"type":"Not","value":{"type":"HasCardType","value":{"type":"Land"}}}} """
