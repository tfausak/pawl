{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.IncreaseSpellCostSpec where

import qualified Pawl.Codec.IncreaseSpellCost as IncreaseSpellCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.IncreaseSpellCost as IncreaseSpellCost

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.IncreaseSpellCost" $ do
  -- CR 601.2f, as Thalia taxes a noncreature spell.
  Spec.it s "MkIncreaseSpellCost" $
    Common.assertCodec
      s
      IncreaseSpellCost.codec
      ( IncreaseSpellCost.MkIncreaseSpellCost
          { IncreaseSpellCost.whichSpells = Filter.Not (Filter.HasCardType CardType.Creature),
            IncreaseSpellCost.amount = 1
          }
      )
      """ {"whichSpells":{"type":"Not","value":{"type":"HasCardType","value":{"type":"Creature"}}},"amount":1} """
  Spec.it s "has a schema" $ Common.assertHasSchema s IncreaseSpellCost.codec
