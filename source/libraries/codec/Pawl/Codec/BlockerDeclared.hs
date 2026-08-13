{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BlockerDeclared where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BlockerDeclared as BlockerDeclared

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec BlockerDeclared.BlockerDeclared
codec = Fields.object $ do
  blocker <- Fields.required "blocker" ObjectId.codec BlockerDeclared.blocker
  attacker <- Fields.required "attacker" ObjectId.codec BlockerDeclared.attacker
  pure
    BlockerDeclared.MkBlockerDeclared
      { BlockerDeclared.blocker = blocker,
        BlockerDeclared.attacker = attacker
      }
