{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.UntapRestrictionSpec where

import qualified Pawl.Codec.UntapRestriction as UntapRestriction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.UntapRestriction as UntapRestriction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.UntapRestriction" $ do
  -- Tsabo's Web's second sentence (CR 502.3 / CR 101.2), which is also the pool's
  -- one writer of Filter.HasNonManaActivatedAbility -- so this round-trips that
  -- atom in the position a card actually writes it.
  Spec.it s "MkUntapRestriction" $
    Common.assertCodec
      s
      UntapRestriction.codec
      ( UntapRestriction.MkUntapRestriction
          ( Affected.Matching
              ( Filter.And
                  [ Filter.HasCardType CardType.Land,
                    Filter.HasNonManaActivatedAbility
                  ]
              )
          )
      )
      """ {"affected":{"type":"Matching","value":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Land"}},{"type":"HasNonManaActivatedAbility"}]}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s UntapRestriction.codec
