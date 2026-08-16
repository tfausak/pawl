{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PrintedReplacementSpec where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.PrintedReplacement as PrintedReplacement
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PrintedReplacement" $ do
  -- CR 604.2's unconditional case, which is every producer in the pool but one.
  Spec.it s "MkPrintedReplacement, condition elided" $
    Common.assertCodec
      s
      codec
      ( PrintedReplacement.MkPrintedReplacement
          { PrintedReplacement.condition = Nothing,
            PrintedReplacement.effect = ReplacementEffect.DestructionR DestructionRewrite.Regenerate
          }
      )
      """ {"effect":{"type":"DestructionR","value":{"type":"Regenerate"}}} """
  -- Jared Carthalion, True Heir's "while you're the monarch", the case that
  -- writes the key.
  Spec.it s "MkPrintedReplacement, condition written" $
    Common.assertCodec
      s
      codec
      ( PrintedReplacement.MkPrintedReplacement
          { PrintedReplacement.condition =
              Just
                ( Condition.Compares
                    ( Compares.MkCompares
                        (Quantity.IsMonarch (PlayerRef.Relative PlayerRelation.You))
                        Comparison.AtLeast
                        (Quantity.Literal 1)
                    )
                ),
            PrintedReplacement.effect = ReplacementEffect.DestructionR DestructionRewrite.Regenerate
          }
      )
      """ {"condition":{"type":"Compares","value":{"measured":{"type":"IsMonarch","value":{"type":"Relative","value":{"type":"You"}}},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":1}}},"effect":{"type":"DestructionR","value":{"type":"Regenerate"}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
  where
    -- The effect codec the card boundary would pass in (CR 615.5's riders ride
    -- the DamageR arm underneath).
    codec = PrintedReplacement.codec (Effect.codec Card.codec)
