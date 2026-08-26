module Pawl.Codec.CounterRestrictionSpec where

import qualified Pawl.Codec.CounterRestriction as CounterRestriction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterRestriction as CounterRestriction
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterRestriction" $ do
  -- Solemnity's second sentence (CR 101.2 / CR 122.6), which names no kind, so
  -- the key is absent -- the wire form data/cards/solemnity.json writes.
  Spec.it s "MkCounterRestriction, no kind named" $
    Common.assertCodec
      s
      CounterRestriction.codec
      ( CounterRestriction.MkCounterRestriction
          (Affected.Matching (Filter.HasCardType CardType.Creature))
          Nothing
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- Melira, Sylvok Outcast's second sentence, which names one.
  Spec.it s "MkCounterRestriction, a named kind" $
    Common.assertCodec
      s
      CounterRestriction.codec
      ( CounterRestriction.MkCounterRestriction
          (Affected.Matching (Filter.ControlledBy PlayerRelation.You))
          (Just CounterKind.MinusOneMinusOne)
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}},\"kind\":{\"type\":\"MinusOneMinusOne\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterRestriction.codec
