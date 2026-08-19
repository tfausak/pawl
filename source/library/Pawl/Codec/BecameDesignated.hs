{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BecameDesignated where

import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BecameDesignated as BecameDesignated

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec BecameDesignated.BecameDesignated
codec = Fields.object $ do
  designation <- Fields.required "designation" Designation.codec BecameDesignated.designation
  object <- Fields.required "object" ObjectId.codec BecameDesignated.object
  pure
    BecameDesignated.MkBecameDesignated
      { BecameDesignated.designation = designation,
        BecameDesignated.object = object
      }
