module Pawl.Codec.VoteSpec where

import qualified Data.List.NonEmpty as NonEmpty
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
import qualified Pawl.Types.VoteChoices as VoteChoices
import qualified Pawl.Types.VoteObjects as VoteObjects

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Vote" $ do
  -- CR 701.38a: the starter is the seat the vote begins with, and rule 701.38b's
  -- listed choices are the tagged half. Pawl.Codec.VoteChoices and
  -- Pawl.Codec.VoteObjects have no spec of their own; these two cases are it.
  Spec.it s "MkVote, Council's Judgment's object vote" $
    Common.assertCodec
      s
      Vote.codec
      ( Vote.MkVote
          { Vote.starter = PlayerRef.Relative PlayerRelation.You,
            Vote.choices =
              VoteChoices.Objects
                VoteObjects.MkVoteObjects
                  { VoteObjects.filter = Filter.HasCardType CardType.Creature,
                    VoteObjects.slot = SlotName.MkSlotName (Text.pack "elected")
                  }
          }
      )
      " {\"starter\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"choices\":{\"type\":\"Objects\",\"value\":{\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"slot\":\"elected\"}}} "
  Spec.it s "MkVote, Plea for Power's word vote" $
    Common.assertCodec
      s
      Vote.codec
      ( Vote.MkVote
          { Vote.starter = PlayerRef.Relative PlayerRelation.You,
            Vote.choices =
              VoteChoices.Words
                ( SlotName.MkSlotName (Text.pack "time")
                    NonEmpty.:| [SlotName.MkSlotName (Text.pack "knowledge")]
                )
          }
      )
      " {\"starter\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"choices\":{\"type\":\"Words\",\"value\":[\"time\",\"knowledge\"]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Vote.codec
