{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SacrificeRestrictionSpec where

import qualified Pawl.Codec.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SacrificeRestriction" $ do
  -- Garland, Royal Kidnapper's third clause (CR 701.21a / CR 101.2), which is
  -- also the one Affected in the pool that conjoins CR 109.5's controller with
  -- CR 108.3's owner -- so this round-trips the OwnedBy atom in the position a
  -- card actually writes it, not only as a bare filter.
  Spec.it s "MkSacrificeRestriction" $
    Common.assertCodec
      s
      SacrificeRestriction.codec
      ( SacrificeRestriction.MkSacrificeRestriction
          ( Affected.Matching
              ( Filter.And
                  [ Filter.HasCardType CardType.Creature,
                    Filter.ControlledBy PlayerRelation.You,
                    Filter.Not (Filter.OwnedBy PlayerRelation.You)
                  ]
              )
          )
      )
      """ {"affected":{"type":"Matching","value":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"ControlledBy","value":{"type":"You"}},{"type":"Not","value":{"type":"OwnedBy","value":{"type":"You"}}}]}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s SacrificeRestriction.codec
