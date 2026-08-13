{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Mentored where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Mentored as Mentored

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec Mentored.Mentored
codec = Fields.object $ do
  mentor <- Fields.required "mentor" ObjectId.codec Mentored.mentor
  mentored <- Fields.required "mentored" ObjectId.codec Mentored.mentored
  pure
    Mentored.MkMentored
      { Mentored.mentor = mentor,
        Mentored.mentored = mentored
      }
