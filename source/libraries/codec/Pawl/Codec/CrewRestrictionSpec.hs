module Pawl.Codec.CrewRestrictionSpec where

import qualified Pawl.Codec.CrewRestriction as CrewRestriction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CrewRestriction as CrewRestriction
import qualified Pawl.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CrewRestriction" $ do
  -- Revoke Privileges' third clause (CR 702.122d / CR 101.2), written the way
  -- its own first two clauses are: an Aura's "enchanted creature" is
  -- HasAttached IsSource, Pacifism's spelling.
  Spec.it s "MkCrewRestriction" $
    Common.assertCodec
      s
      CrewRestriction.codec
      ( CrewRestriction.MkCrewRestriction
          ( Affected.Matching
              ( Filter.And
                  [ Filter.HasCardType CardType.Creature,
                    Filter.HasAttached Filter.IsSource
                  ]
              )
          )
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"HasAttached\",\"value\":{\"type\":\"IsSource\"}}]}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CrewRestriction.codec
