module Pawl.Codec.LibraryPlacement where

import qualified Pawl.Codec.LibraryPosition as LibraryPosition
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement

-- | A stated placement writes its OWN tag and carries the position as the
-- payload, rather than passing the position's tag through as the placement's.
-- Passing it through read as though a placement were a widened position, and it
-- cost the schema the claim 'Arm.tagged' otherwise makes: a decoder dispatching
-- on the JSON type of what it was handed cannot be stated as @oneOf@ over
-- branches a reader can tell apart (#1304).
--
-- The two tags stay disjoint from every zone's, which is what
-- 'Pawl.Codec.Effect'\'s @moveTail@ needs to tell a placement from a zone.
codec :: Codec.Codec LibraryPlacement.LibraryPlacement
codec =
  Arm.tagged
    encode
    [ Arm.payload "Stated" LibraryPosition.codec LibraryPlacement.Stated,
      Arm.nullary "OwnerChooses" LibraryPlacement.OwnerChooses
    ]
  where
    encode p = case p of
      LibraryPlacement.Stated position ->
        Common.tagged "Stated" . Just $ Codec.encode LibraryPosition.codec position
      LibraryPlacement.OwnerChooses -> Common.nullary "OwnerChooses"
