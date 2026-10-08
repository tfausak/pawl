module Pawl.Codec.CounterDestinationSpec where

import qualified Pawl.Codec.CounterDestination as CounterDestination
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterDestination as CounterDestination
import qualified Pawl.Types.CounteredEnd as CounteredEnd
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterDestination" $ do
  -- Remand's "put it into its owner's hand instead": every default elided.
  Spec.it s "MkCounterDestination, a bare zone" $
    Common.assertCodec
      s
      CounterDestination.codec
      (CounterDestination.MkCounterDestination Zone.Hand (CounteredEnd.Stated LibraryPosition.Bottom) Nothing Nothing)
      " {\"zone\":{\"type\":\"Hand\"}} "
  -- Memory Lapse's "put it on top of its owner's library instead".
  Spec.it s "MkCounterDestination, the top of a library" $
    Common.assertCodec
      s
      CounterDestination.codec
      (CounterDestination.MkCounterDestination Zone.Library (CounteredEnd.Stated LibraryPosition.Top) Nothing Nothing)
      " {\"zone\":{\"type\":\"Library\"},\"position\":{\"type\":\"Stated\",\"value\":{\"type\":\"Top\"}}} "
  -- Desertion's "if an artifact or creature spell is countered this way".
  Spec.it s "MkCounterDestination, narrowed to some spells" $
    Common.assertCodec
      s
      CounterDestination.codec
      (CounterDestination.MkCounterDestination Zone.Battlefield (CounteredEnd.Stated LibraryPosition.Bottom) (Just (Filter.Or [Filter.HasCardType CardType.Artifact, Filter.HasCardType CardType.Creature])) Nothing)
      " {\"zone\":{\"type\":\"Battlefield\"},\"only\":{\"type\":\"Or\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}},{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterDestination.codec
