module Pawl.Codec.ReplaceSpec where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Replace as Replace
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Uses as Uses

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Replace" $ do
  -- CR 614.3: the unconditional case, which is most of them. The positional
  -- payload this replaces wrote the absent condition as an explicit null.
  Spec.it s "MkReplace, condition elided" $
    Common.assertCodec
      s
      codec
      ( Replace.MkReplace
          { Replace.duration = Duration.UntilEndOfTurn,
            Replace.uses = Uses.Once,
            Replace.origin = ReplacementOrigin.Other,
            Replace.condition = Nothing,
            Replace.effect = ReplacementEffect.DestructionR DestructionRewrite.Regenerate
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"uses\":{\"type\":\"Once\"},\"origin\":{\"type\":\"Other\"},\"effect\":{\"type\":\"DestructionR\",\"value\":{\"type\":\"Regenerate\"}}} "
  -- CR 614.15 / 614.1: Galvanic Blast's metalcraft clause, the case that writes
  -- the key.
  Spec.it s "MkReplace, condition written" $
    Common.assertCodec
      s
      codec
      ( Replace.MkReplace
          { Replace.duration = Duration.UntilEndOfTurn,
            Replace.uses = Uses.Once,
            Replace.origin = ReplacementOrigin.SelfReplacement,
            Replace.condition = Just (Condition.Compares (Compares.MkCompares (Quantity.Literal 3) Comparison.AtLeast (Quantity.Literal 3))),
            Replace.effect = ReplacementEffect.DestructionR DestructionRewrite.Regenerate
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"uses\":{\"type\":\"Once\"},\"origin\":{\"type\":\"SelfReplacement\"},\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":3},\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":3}}},\"effect\":{\"type\":\"DestructionR\",\"value\":{\"type\":\"Regenerate\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
  where
    -- The effect codec the card boundary would pass in (CR 615.5's riders ride
    -- the DamageR arm underneath).
    codec = Replace.codec (Effect.codec Card.codec)
