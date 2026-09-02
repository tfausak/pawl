module Pawl.Codec.RestrictedCreatures where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures

-- | Tagged, in Pawl.Codec.AffectedPlayers' shape and PARAMETRIC for its reason:
-- a card writes @RestrictedCreatures ObjectRef@ (Pawl.Codec.ForbidAttack passes
-- Pawl.Codec.ObjectRef's codec) and the store holds @RestrictedCreatures
-- ObjectId@ (Pawl.Codec.ActiveAttackProhibition passes Pawl.Codec.ObjectId's).
codec ::
  (Typeable.Typeable named) =>
  Codec.Codec named ->
  Codec.Codec (RestrictedCreatures.RestrictedCreatures named)
codec namedCodec =
  Arm.tagged
    [ Arm.payload "Named" namedCodec RestrictedCreatures.Named (\x -> case x of RestrictedCreatures.Named y -> Just y; _ -> Nothing),
      Arm.payload "Matching" (Filter.codec Keyword.codec) RestrictedCreatures.Matching (\x -> case x of RestrictedCreatures.Matching y -> Just y; _ -> Nothing)
    ]
