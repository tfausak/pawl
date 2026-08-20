module Pawl.Codec.MillSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Mill as Mill
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Mill" $ do
  -- Every mill in the pool but rule 728.1's looks back at nothing, so the tally
  -- key is absent here and present below.
  Spec.it s "MkMill, no tally: the key is omitted" $
    Common.assertCodec
      s
      Mill.codec
      (Mill.MkMill (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 3) Nothing Nothing)
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":3}} "
  Spec.it s "MkMill, CR 728.1's tally: the key is written" $
    Common.assertCodec
      s
      Mill.codec
      ( Mill.MkMill
          (PlayerRef.Relative PlayerRelation.You)
          (Quantity.Literal 2)
          (Just (MillTally.MkMillTally (SlotName.MkSlotName (Text.pack "milled")) (Filter.Not (Filter.HasCardType CardType.Land))))
          Nothing
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2},\"tally\":{\"slot\":\"milled\",\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}}} "
  -- CR 701.17c's slot, which Midnight Tilling writes and the tally-carrying
  -- printing does not: the two keys are independent.
  Spec.it s "MkMill, CR 701.17c's slot: the key is written" $
    Common.assertCodec
      s
      Mill.codec
      ( Mill.MkMill
          (PlayerRef.Relative PlayerRelation.You)
          (Quantity.Literal 4)
          Nothing
          (Just (SlotName.MkSlotName (Text.pack "milled")))
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":4},\"slot\":\"milled\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Mill.codec
