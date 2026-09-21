module Pawl.Codec.ConjureDestination where

import qualified Data.Maybe as Maybe
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.TapState as TapState

-- | @Battlefield@ is 'Arm.optionalPayload' rather than 'Arm.payload' for
-- Pawl.Codec.Conjure's reason: CR 110.5b's untapped is what the sentence does
-- NOT say, so "onto the battlefield" writes the bare tag and only "onto the
-- battlefield tapped" writes a value. An explicit @Untapped@ still decodes, and
-- encodes back to the bare tag, which is the canonical spelling
-- Pawl.CardSpec's corpus round trip holds every card file to.
codec :: Codec.Codec ConjureDestination.ConjureDestination
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Hand" ConjureDestination.Hand,
      Arm.nullary "Library" ConjureDestination.Library,
      Arm.nullary "Graveyard" ConjureDestination.Graveyard,
      Arm.optionalPayload
        "Battlefield"
        TapState.codec
        (ConjureDestination.Battlefield . Maybe.fromMaybe TapState.Untapped)
        ( \x -> case x of
            ConjureDestination.Battlefield TapState.Untapped -> Just Nothing
            ConjureDestination.Battlefield tapped -> Just (Just tapped)
            _ -> Nothing
        )
    ]

-- | Total, which is what makes a new constructor an error here rather than a
-- silent @{}@ -- 'Arm.tagged' says why it takes this beside the arm list.
tagOf :: ConjureDestination.ConjureDestination -> String
tagOf x = case x of
  ConjureDestination.Hand -> "Hand"
  ConjureDestination.Library -> "Library"
  ConjureDestination.Graveyard -> "Graveyard"
  ConjureDestination.Battlefield _ -> "Battlefield"
