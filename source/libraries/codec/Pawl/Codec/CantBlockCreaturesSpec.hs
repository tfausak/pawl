module Pawl.Codec.CantBlockCreaturesSpec where

import qualified Pawl.Codec.CantBlockCreatures as CantBlockCreatures
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CantBlockCreatures as CantBlockCreatures
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Measure as Measure
import qualified Pawl.Types.Measures as Measures
import qualified Pawl.Types.Operand as Operand

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CantBlockCreatures" $ do
  -- The two creature-naming keys differ on purpose, so a codec swapping them
  -- fails.
  Spec.it s "MkCantBlockCreatures, unless elided" $
    Common.assertCodec
      s
      CantBlockCreatures.codec
      ( CantBlockCreatures.MkCantBlockCreatures
          { CantBlockCreatures.affected = Affected.Matching (Filter.HasCardType CardType.Creature),
            CantBlockCreatures.attackers = Filter.Measures (Measures.MkMeasures Measure.Power Comparison.GreaterThan (Operand.OfSource Measure.Power)),
            CantBlockCreatures.unless = Nothing,
            CantBlockCreatures.name = Nothing
          }
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"attackers\":{\"type\":\"Measures\",\"value\":{\"measure\":{\"type\":\"Power\"},\"comparison\":{\"type\":\"GreaterThan\"},\"operand\":{\"type\":\"OfSource\",\"value\":{\"type\":\"Power\"}}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CantBlockCreatures.codec
