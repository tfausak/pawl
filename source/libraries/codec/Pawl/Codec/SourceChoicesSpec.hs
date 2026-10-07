module Pawl.Codec.SourceChoicesSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.SourceChoices as SourceChoices
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.SourceChoices as SourceChoices
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SourceChoices" $ do
  Spec.it s "nothing chosen" $
    Common.assertCodec
      s
      SourceChoices.codec
      SourceChoices.MkSourceChoices
        { SourceChoices.names = Set.empty,
          SourceChoices.colors = Set.empty,
          SourceChoices.subtype = Nothing
        }
      " {} "
  Spec.it s "a name, a colour and a subtype" $
    Common.assertCodec
      s
      SourceChoices.codec
      SourceChoices.MkSourceChoices
        { SourceChoices.names = Set.singleton (CardName.MkCardName (Text.pack "Shock")),
          SourceChoices.colors = Set.singleton Color.Red,
          SourceChoices.subtype = Just Subtype.Goblin
        }
      " {\"names\":[\"Shock\"],\"colors\":[{\"type\":\"Red\"}],\"subtype\":{\"type\":\"Goblin\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s SourceChoices.codec
