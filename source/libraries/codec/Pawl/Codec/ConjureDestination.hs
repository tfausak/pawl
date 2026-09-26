module Pawl.Codec.ConjureDestination where

import qualified Data.Maybe as Maybe
import qualified Pawl.Codec.ConjureEntry as ConjureEntry
import qualified Pawl.Codec.LibraryDepth as LibraryDepth
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.ConjureEntry as ConjureEntry

-- | @Library@ is 'Arm.optionalPayload' so a conjure naming no place (Toralf\'s
-- Disciple) writes the bare tag.
--
-- @Battlefield@ is 'Arm.optionalPayload' rather than 'Arm.payload' for
-- Pawl.Codec.Conjure's reason: "onto the battlefield" states nothing about the
-- arrival, so it writes the bare tag and only "tapped" or "attacking" writes a
-- value. An explicit default still decodes, and encodes back to the bare tag,
-- which is the canonical spelling Pawl.CardSpec's corpus round trip holds every
-- card file to.
codec :: Codec.Codec ConjureDestination.ConjureDestination
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Hand" ConjureDestination.Hand,
      Arm.optionalPayload "Library" LibraryDepth.codec ConjureDestination.Library (\x -> case x of ConjureDestination.Library y -> Just y; _ -> Nothing),
      Arm.nullary "Graveyard" ConjureDestination.Graveyard,
      Arm.nullary "Exile" ConjureDestination.Exile,
      Arm.optionalPayload
        "Battlefield"
        ConjureEntry.codec
        (ConjureDestination.Battlefield . Maybe.fromMaybe ConjureEntry.defaultValue)
        ( \x -> case x of
            ConjureDestination.Battlefield entry
              | entry == ConjureEntry.defaultValue -> Just Nothing
              | otherwise -> Just (Just entry)
            _ -> Nothing
        )
    ]

-- | Total, which is what makes a new constructor an error here rather than a
-- silent @{}@ -- 'Arm.tagged' says why it takes this beside the arm list.
tagOf :: ConjureDestination.ConjureDestination -> String
tagOf x = case x of
  ConjureDestination.Hand -> "Hand"
  ConjureDestination.Library _ -> "Library"
  ConjureDestination.Graveyard -> "Graveyard"
  ConjureDestination.Battlefield _ -> "Battlefield"
  ConjureDestination.Exile -> "Exile"
