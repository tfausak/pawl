module Pawl.Codec.CardArrivedInSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.CardArrivedIn as CardArrivedIn
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardArrivedIn as CardArrivedIn
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardArrivedIn" $ do
  -- The default: "from anywhere", which is the only shape the pool wrote before
  -- Dimir Strandcatcher, so the key is absent from the wire form rather than
  -- present and empty.
  Spec.it s "MkCardArrivedIn, no exclusion" $
    Common.assertCodec
      s
      CardArrivedIn.codec
      (CardArrivedIn.MkCardArrivedIn {CardArrivedIn.to = Zone.Graveyard, CardArrivedIn.excluding = Set.empty})
      " {\"to\":{\"type\":\"Graveyard\"}} "
  -- Asymmetric on purpose, Pawl.Codec.MovedBetweenSpec's reason: both keys carry
  -- a Zone, so only a case whose destination and exclusion differ catches a
  -- codec that read one out of the other.
  Spec.it s "MkCardArrivedIn, an excluded origin" $
    Common.assertCodec
      s
      CardArrivedIn.codec
      (CardArrivedIn.MkCardArrivedIn {CardArrivedIn.to = Zone.Graveyard, CardArrivedIn.excluding = Set.singleton Zone.Battlefield})
      " {\"excluding\":[{\"type\":\"Battlefield\"}],\"to\":{\"type\":\"Graveyard\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CardArrivedIn.codec
