module Pawl.Codec.DamagePreventedSpec where

import qualified Pawl.Codec.DamagePrevented as DamagePrevented
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.FloatingCandidate as FloatingCandidate
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamagePrevented" $ do
  -- CR 615.1 / 615.13: which prevention effect stopped it, how much it stopped,
  -- who the damage was headed for, and what would have dealt it.
  Spec.it s "MkDamagePrevented, every key" $
    Common.assertCodec
      s
      DamagePrevented.codec
      ( DamagePrevented.MkDamagePrevented
          { DamagePrevented.by =
              CandidateId.OfFloating
                FloatingCandidate.MkFloatingCandidate
                  { FloatingCandidate.source = ObjectId.MkObjectId 7,
                    FloatingCandidate.timestamp = Timestamp.MkTimestamp 2
                  },
            DamagePrevented.source = ObjectId.MkObjectId 13,
            DamagePrevented.recipient = Recipient.ToPlayer (PlayerId.MkPlayerId 0),
            DamagePrevented.amount = 3
          }
      )
      " {\"by\":{\"type\":\"OfFloating\",\"value\":{\"source\":7,\"timestamp\":2}},\"source\":13,\"recipient\":{\"type\":\"ToPlayer\",\"value\":0},\"amount\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DamagePrevented.codec
