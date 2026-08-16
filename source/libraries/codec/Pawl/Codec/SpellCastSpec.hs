{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SpellCastSpec where

import qualified Pawl.Codec.SpellCast as SpellCast
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SpellCast" $ do
  -- Prowess's shape (Monastery Swiftspear): every turn.
  Spec.it s "MkSpellCast, an unscoped window" $
    Common.assertCodec
      s
      SpellCast.codec
      ( SpellCast.MkSpellCast
          { SpellCast.filter = Filter.ControlledBy PlayerRelation.You,
            SpellCast.scope = TurnScope.EachTurn,
            SpellCast.zone = Nothing
          }
      )
      """ {"filter":{"type":"ControlledBy","value":{"type":"You"}},"scope":{"type":"EachTurn"}} """
  -- Brineborn Cutthroat's "during an opponent's turn". The scope is REQUIRED,
  -- for the reason Pawl.Codec.StepBegins gives.
  Spec.it s "MkSpellCast, an opponent's-turn window" $
    Common.assertCodec
      s
      SpellCast.codec
      ( SpellCast.MkSpellCast
          { SpellCast.filter = Filter.ControlledBy PlayerRelation.You,
            SpellCast.scope = TurnScope.OpponentsTurn,
            SpellCast.zone = Nothing
          }
      )
      """ {"filter":{"type":"ControlledBy","value":{"type":"You"}},"scope":{"type":"OpponentsTurn"}} """
  -- Harness the Storm's "from your hand". The zone is ELIDED when absent, which
  -- the two cases above pin; here it is written, which is what pins the key.
  Spec.it s "MkSpellCast, a window naming the zone the cast came from" $
    Common.assertCodec
      s
      SpellCast.codec
      ( SpellCast.MkSpellCast
          { SpellCast.filter = Filter.ControlledBy PlayerRelation.You,
            SpellCast.scope = TurnScope.EachTurn,
            SpellCast.zone = Just Zone.Hand
          }
      )
      """ {"filter":{"type":"ControlledBy","value":{"type":"You"}},"scope":{"type":"EachTurn"},"zone":{"type":"Hand"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s SpellCast.codec
