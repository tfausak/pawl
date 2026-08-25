{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ClassLevelChange where

import qualified Pawl.Codec.ClassLevel as ClassLevel
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ClassLevelChange as ClassLevelChange

-- | A bare object keyed by the record's field names, Pawl.Codec.CounterChange's
-- shape. Runtime-only: GameEvent serialises transcripts, never card data.
codec :: Codec.Codec ClassLevelChange.ClassLevelChange
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec ClassLevelChange.object
  before <- Fields.required "before" ClassLevel.codec ClassLevelChange.before
  after <- Fields.required "after" ClassLevel.codec ClassLevelChange.after
  pure
    ClassLevelChange.MkClassLevelChange
      { ClassLevelChange.object = object,
        ClassLevelChange.before = before,
        ClassLevelChange.after = after
      }
