{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttackerBlocked where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec AttackerBlocked.AttackerBlocked
codec = Fields.object $ do
  attacker <- Fields.required "attacker" ObjectId.codec AttackerBlocked.attacker
  defender <- Fields.required "defender" PlayerId.codec AttackerBlocked.defender
  pure
    AttackerBlocked.MkAttackerBlocked
      { AttackerBlocked.attacker = attacker,
        AttackerBlocked.defender = defender
      }
