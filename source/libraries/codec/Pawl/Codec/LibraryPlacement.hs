module Pawl.Codec.LibraryPlacement where

import qualified Pawl.Codec.LibraryPosition as LibraryPosition
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement

-- | A stated placement writes its OWN tag and carries the position as the
-- payload, rather than passing the position's tag through as the placement's.
-- Passing it through read as though a placement were a widened position, and it
-- cost the schema the claim 'Arm.tagged' otherwise makes: a decoder dispatching
-- on the JSON type of what it was handed cannot be stated as @oneOf@ over
-- branches a reader can tell apart (#1304).
--
-- The two tags were also what told a placement from a zone in
-- 'Pawl.Codec.Effect'\'s @moveTail@. That function is gone (#1305): a placement
-- is now a NAMED KEY on 'Pawl.Types.MoveToZone', so nothing has to tell the two
-- apart by shape.
codec :: Codec.Codec LibraryPlacement.LibraryPlacement
codec =
  Arm.tagged
    [ Arm.payload "Stated" LibraryPosition.codec LibraryPlacement.Stated (\x -> case x of LibraryPlacement.Stated y -> Just y; _ -> Nothing),
      Arm.nullary "OwnerChooses" LibraryPlacement.OwnerChooses,
      Arm.payload "RandomOrder" LibraryPosition.codec LibraryPlacement.RandomOrder (\x -> case x of LibraryPlacement.RandomOrder y -> Just y; _ -> Nothing)
    ]
