{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BecameAttacked where

import qualified Pawl.Codec.AttackTarget as AttackTarget
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BecameAttacked as BecameAttacked

-- | A bare object keyed by the record's field names, Pawl.Codec.AttackerDeclared's
-- shape. Runtime-only: GameEvent serialises transcripts, never card data.
codec :: Codec.Codec BecameAttacked.BecameAttacked
codec = Fields.object $ do
  attacker <- Fields.required "attacker" PlayerId.codec BecameAttacked.attacker
  target <- Fields.required "target" AttackTarget.codec BecameAttacked.target
  pure
    BecameAttacked.MkBecameAttacked
      { BecameAttacked.attacker = attacker,
        BecameAttacked.target = target
      }
