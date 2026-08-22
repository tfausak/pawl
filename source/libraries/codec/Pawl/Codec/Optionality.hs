module Pawl.Codec.Optionality where

import qualified Data.Maybe as Maybe
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRef as PlayerRef.Type
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | The asker is ELIDED when it is the unmarked "you" (CR 405.4 / CR 113.8), so
-- @{"type":"Optional"}@ keeps meaning what it always meant and only a card whose
-- printed "may" names somebody else -- Jungle Wayfinder's "each player may" --
-- writes a value. 'Arm.optionalPayload' is what takes the tag with or without
-- one.
codec :: Codec.Codec Optionality.Optionality
codec =
  Arm.tagged
    [ Arm.nullary "Mandatory" Optionality.Mandatory,
      Arm.optionalPayload
        "Optional"
        PlayerRef.codec
        (Optionality.Optional . Maybe.fromMaybe defaultAsker)
        ( \x -> case x of
            Optionality.Optional ref -> Just (if ref == defaultAsker then Nothing else Just ref)
            _ -> Nothing
        )
    ]

-- | CR 603.5's "you may": who a clause's printed "may" asks unless it says
-- otherwise.
defaultAsker :: PlayerRef.Type.PlayerRef
defaultAsker = PlayerRef.Type.Relative PlayerRelation.You
