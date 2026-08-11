{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.UntapRestrictionSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.UntapRestriction as UntapRestriction
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.UntapRestriction as UntapRestriction

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  -- Tsabo's Web's second sentence (CR 502.1 / CR 101.2), which is also the pool's
  -- one writer of Filter.HasNonManaActivatedAbility -- so this round-trips that
  -- atom in the position a card actually writes it.
  Spec.describe s "Pawl.Codec.UntapRestriction" . Spec.it s "MkUntapRestriction" $
    Common.assertJsonCodec
      s
      UntapRestriction.toJson
      UntapRestriction.fromJson
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
