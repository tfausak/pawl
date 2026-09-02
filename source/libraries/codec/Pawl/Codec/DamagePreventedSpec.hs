module Pawl.Codec.DamagePreventedSpec where

import qualified Data.Map as Map
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
  -- CR 615.1 / 615.13: which prevention effect stopped it, how much it stopped
  -- for each recipient the damage was headed for, and what would have dealt it.
  --
  -- TWO recipients, which is the shape one application can have (Divine
  -- Deflection covers a player and the permanents they control): the map is a
  -- 'Common.multiset', so it is on the wire as key/count objects ascending by
  -- key, and ToCreature precedes ToPlayer.
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
            DamagePrevented.amounts =
              Map.fromList
                [ (Recipient.ToPlayer (PlayerId.MkPlayerId 0), 3),
                  (Recipient.ToCreature (ObjectId.MkObjectId 5), 1)
                ]
          }
      )
      ( " {\"by\":{\"type\":\"OfFloating\",\"value\":{\"source\":7,\"timestamp\":2}},\"source\":13"
          <> ",\"amounts\":[{\"key\":{\"type\":\"ToCreature\",\"value\":5},\"value\":1}"
          <> ",{\"key\":{\"type\":\"ToPlayer\",\"value\":0},\"value\":3}]} "
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s DamagePrevented.codec
