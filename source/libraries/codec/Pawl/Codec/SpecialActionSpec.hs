{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SpecialActionSpec where

import qualified Pawl.Codec.SpecialAction as SpecialAction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.SpecialAction as SpecialAction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SpecialAction" $ do
  Spec.it s "DiscardThisAnyTime" $
    Common.assertJsonCodec
      s
      SpecialAction.toJson
      SpecialAction.fromJson
      SpecialAction.DiscardThisAnyTime
      """ {"type":"DiscardThisAnyTime"} """
  -- Leonin Arbiter's {2}, which is the whole of its ignore cost.
  Spec.it s "IgnoreThisUntilEndOfTurn" $
    Common.assertJsonCodec
      s
      SpecialAction.toJson
      SpecialAction.fromJson
      ( SpecialAction.IgnoreThisUntilEndOfTurn
          Cost.MkCost
            { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
              Cost.components = []
            }
      )
      """ {"type":"IgnoreThisUntilEndOfTurn","value":{"mana":[{"type":"Generic","value":2}]}} """
