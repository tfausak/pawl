module Pawl.Codec.PreventionSpec where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Prevention as Prevention
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.FloatingCandidate as FloatingCandidate
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prevention as Prevention
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Prevention" $ do
  -- CR 615.13 with no CR 615.5 rider, which is what all but a handful of
  -- preventions look like.
  Spec.it s "a plain prevention" $
    Common.assertCodec
      s
      Prevention.codec
      Prevention.MkPrevention
        { Prevention.by =
            CandidateId.OfFloating
              FloatingCandidate.MkFloatingCandidate
                { FloatingCandidate.source = ObjectId.MkObjectId 3,
                  FloatingCandidate.timestamp = Timestamp.MkTimestamp 8
                },
          Prevention.source = ObjectId.MkObjectId 11,
          Prevention.recipient = Recipient.ToPlayer (PlayerId.MkPlayerId 1),
          Prevention.amount = 2,
          Prevention.rider = Nothing
        }
      " {\"by\":{\"type\":\"OfFloating\",\"value\":{\"source\":3,\"timestamp\":8}},\"source\":11,\"recipient\":{\"type\":\"ToPlayer\",\"value\":1},\"amount\":2,\"rider\":null} "
  -- CR 615.5: the rider rides the prevention rather than the row that fired it,
  -- because a CR 615.7 shield spent to zero is dropped in the same application.
  Spec.it s "a prevention carrying CR 615.5's additional effect" $
    Common.assertCodec
      s
      Prevention.codec
      Prevention.MkPrevention
        { Prevention.by =
            CandidateId.OfFloating
              FloatingCandidate.MkFloatingCandidate
                { FloatingCandidate.source = ObjectId.MkObjectId 5,
                  FloatingCandidate.timestamp = Timestamp.MkTimestamp 6
                },
          Prevention.source = ObjectId.MkObjectId 12,
          Prevention.recipient = Recipient.ToCreature (ObjectId.MkObjectId 7),
          Prevention.amount = 4,
          Prevention.rider =
            Just
              PreventionRider.MkPreventionRider
                { PreventionRider.effects = Seq.fromList [Effect.Proliferate],
                  PreventionRider.targets =
                    Map.singleton
                      (SlotName.MkSlotName (Text.pack "target"))
                      (Set.singleton (Recipient.ToCreature (ObjectId.MkObjectId 7))),
                  PreventionRider.controller = PlayerId.MkPlayerId 1,
                  PreventionRider.source = ObjectId.MkObjectId 9
                }
        }
      ( " {\"by\":{\"type\":\"OfFloating\",\"value\":{\"source\":5,\"timestamp\":6}}"
          <> ",\"source\":12,\"recipient\":{\"type\":\"ToCreature\",\"value\":7},\"amount\":4"
          <> ",\"rider\":{\"effects\":[{\"type\":\"Proliferate\"}],\"targets\":{\"target\":[{\"type\":\"ToCreature\",\"value\":7}]},\"controller\":1,\"source\":9}} "
      )
  Spec.it s "has a schema" $
    Common.assertHasSchema s Prevention.codec
