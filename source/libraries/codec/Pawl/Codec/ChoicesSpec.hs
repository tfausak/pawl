module Pawl.Codec.ChoicesSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Choices as Choices
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Choices as Choices.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.ModeIndex as ModeIndex.Type
import qualified Pawl.Types.Reference as Reference.Type
import qualified Pawl.Types.SlotName as SlotName.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Choices" $ do
  Spec.it s "no choices is an empty object" $
    Common.assertCodec s Choices.codec Choices.Type.none " {} "
  Spec.it s "targets, modes, X, a cost order and mana sources" $
    Common.assertCodec
      s
      Choices.codec
      Choices.Type.none
        { Choices.Type.targets = Just [Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "bob"))],
          Choices.Type.modes = Just (Seq.singleton (ModeIndex.Type.MkModeIndex 1)),
          Choices.Type.x = Just 3,
          Choices.Type.costOrder = Just [1, 0],
          Choices.Type.manaSources = Seq.fromList [Just (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "mountain"))), Nothing]
        }
      " {\"targets\":[\"$bob\"],\"modes\":[1],\"x\":3,\"costOrder\":[1,0],\"mana\":[\"$mountain\",null]} "
  Spec.it s "targets by slot" $
    Common.assertCodec
      s
      Choices.codec
      Choices.Type.none {Choices.Type.targetsBySlot = Map.singleton (SlotName.Type.MkSlotName (Text.pack "victim")) (Seq.singleton (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "bear"))))}
      " {\"targetsBySlot\":{\"victim\":[\"$bear\"]}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Choices.codec
