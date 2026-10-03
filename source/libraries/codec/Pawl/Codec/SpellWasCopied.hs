{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SpellWasCopied where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SpellWasCopied as SpellWasCopied

-- | Pawl.Codec.Exploited's shape: a bare object keyed by the record's field
-- names. Runtime-only, GameEvent serialising transcripts rather than card data.
codec :: Codec.Codec SpellWasCopied.SpellWasCopied
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec SpellWasCopied.player
  copy <- Fields.required "copy" ObjectId.codec SpellWasCopied.copy
  pure
    SpellWasCopied.MkSpellWasCopied
      { SpellWasCopied.player = player,
        SpellWasCopied.copy = copy
      }
