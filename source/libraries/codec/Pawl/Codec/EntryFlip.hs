{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntryFlip where

import qualified Pawl.Codec.EntryOption as EntryOption
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EntryFlip as EntryFlip

-- | Both keys are REQUIRED: CR 705.1's coin comes up one way or the other, so a
-- card that named only one face would leave the engine nothing to apply on the
-- other.
codec :: Codec.Codec EntryFlip.EntryFlip
codec = Fields.object $ do
  heads <- Fields.required "heads" EntryOption.codec EntryFlip.heads
  tails <- Fields.required "tails" EntryOption.codec EntryFlip.tails
  pure
    EntryFlip.MkEntryFlip
      { EntryFlip.heads = heads,
        EntryFlip.tails = tails
      }
