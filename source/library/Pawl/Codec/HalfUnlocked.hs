{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.HalfUnlocked where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.HalfUnlocked as HalfUnlocked

-- | A bare object keyed by the record's field names, replacing the positional
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec HalfUnlocked.HalfUnlocked
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec HalfUnlocked.object
  name <- Fields.required "name" CardName.codec HalfUnlocked.name
  fully <- Fields.required "fully" Common.boolean HalfUnlocked.fully
  pure
    HalfUnlocked.MkHalfUnlocked
      { HalfUnlocked.object = object,
        HalfUnlocked.name = name,
        HalfUnlocked.fully = fully
      }
