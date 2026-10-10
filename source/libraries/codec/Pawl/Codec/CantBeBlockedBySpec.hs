module Pawl.Codec.CantBeBlockedBySpec where

import qualified Pawl.Codec.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Measure as Measure
import qualified Pawl.Types.Measures as Measures
import qualified Pawl.Types.Operand as Operand

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CantBeBlockedBy" $ do
  -- CR 509.1b's pairwise restriction. Both creature-naming keys carry a Filter,
  -- so the fixture makes them differ on purpose: only an asymmetric case catches
  -- a codec that barred the attackers from themselves.
  Spec.it s "MkCantBeBlockedBy, unless elided" $
    Common.assertCodec
      s
      CantBeBlockedBy.codec
      ( CantBeBlockedBy.MkCantBeBlockedBy
          { CantBeBlockedBy.affected = Affected.Matching (Filter.HasCardType CardType.Creature),
            CantBeBlockedBy.blockers = Filter.Measures (Measures.MkMeasures Measure.Power Comparison.GreaterThan (Operand.OfSource Measure.Power)),
            CantBeBlockedBy.unless = Nothing,
            CantBeBlockedBy.name = Nothing
          }
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"blockers\":{\"type\":\"Measures\",\"value\":{\"measure\":{\"type\":\"Power\"},\"comparison\":{\"type\":\"GreaterThan\"},\"operand\":{\"type\":\"OfSource\",\"value\":{\"type\":\"Power\"}}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CantBeBlockedBy.codec
