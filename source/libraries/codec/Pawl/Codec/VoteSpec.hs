module Pawl.Codec.VoteSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Vote as Vote
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Vote as Vote

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Vote" $ do
  -- CR 701.38a: the starter is the seat the vote begins with, the filter is rule
  -- 701.38b's listed choices, and the slot is where the objects tied for most
  -- votes land.
  Spec.it s "MkVote, all three keys" $
    Common.assertCodec
      s
      Vote.codec
      ( Vote.MkVote
          { Vote.starter = PlayerRef.Relative PlayerRelation.You,
            Vote.filter = Filter.HasCardType CardType.Creature,
            Vote.slot = SlotName.MkSlotName (Text.pack "elected")
          }
      )
      " {\"starter\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"slot\":\"elected\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Vote.codec
