{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttackerDeclared where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared

-- | A bare object keyed by the record's field names, replacing the positional
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec AttackerDeclared.AttackerDeclared
codec = Fields.object $ do
  attacker <- Fields.required "attacker" ObjectId.codec AttackerDeclared.attacker
  defender <- Fields.required "defender" PlayerId.codec AttackerDeclared.defender
  count <- Fields.required "count" Common.natural AttackerDeclared.count
  pure
    AttackerDeclared.MkAttackerDeclared
      { AttackerDeclared.attacker = attacker,
        AttackerDeclared.defender = defender,
        AttackerDeclared.count = count
      }
