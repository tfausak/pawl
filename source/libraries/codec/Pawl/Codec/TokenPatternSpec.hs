module Pawl.Codec.TokenPatternSpec where

import qualified Pawl.Codec.TokenPattern as TokenPattern
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.TokenPattern as TokenPattern

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TokenPattern" $ do
  Spec.it s "MkTokenPattern" $
    Common.assertCodec
      s
      TokenPattern.codec
      TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours, TokenPattern.whatToken = Filter.And []}
      " {\"whose\":{\"type\":\"Yours\"}} "
  -- CR 111.1: Queen Allenal of Ruadach's "creature tokens".
  Spec.it s "a pattern naming what the token is" $
    Common.assertCodec
      s
      TokenPattern.codec
      TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours, TokenPattern.whatToken = Filter.HasCardType CardType.Creature}
      " {\"whose\":{\"type\":\"Yours\"},\"whatToken\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- CR 109.5: Anyones is what a pattern that says nothing about the
  -- controller means, so the sole field's key is omitted.
  Spec.it s "an all-default value omits every optional key" $
    Common.assertCodec
      s
      TokenPattern.codec
      TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Anyones, TokenPattern.whatToken = Filter.And []}
      " {} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TokenPattern.codec
