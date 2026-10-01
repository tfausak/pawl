module Pawl.Codec.MoveSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Move as Move
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Activation as Activation.Type
import qualified Pawl.Types.Casting as Casting.Type
import qualified Pawl.Types.Choices as Choices.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Move as Move.Type
import qualified Pawl.Types.OptionalDecision as OptionalDecision.Type
import qualified Pawl.Types.Reference as Reference.Type
import qualified Pawl.Types.SlotName as SlotName.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Move" $ do
  Spec.it s "Cast" $
    Common.assertCodec s Move.codec (Move.Type.Cast (Casting.Type.MkCasting (ref "bolt") Choices.Type.none)) " {\"Cast\":{\"object\":\"@bolt\"}} "
  Spec.it s "PlayLand" $
    Common.assertCodec s Move.codec (Move.Type.PlayLand (ref "land")) " {\"PlayLand\":\"@land\"} "
  Spec.it s "Activate" $
    Common.assertCodec s Move.codec (Move.Type.Activate (Activation.Type.MkActivation (ref "stone") Nothing Choices.Type.none)) " {\"Activate\":{\"object\":\"@stone\"}} "
  Spec.it s "Attack" $
    Common.assertCodec s Move.codec (Move.Type.Attack (Seq.fromList [ref "bear", ref "wolf"])) " {\"Attack\":[\"@bear\",\"@wolf\"]} "
  Spec.it s "Block is keyed by blocker" $
    Common.assertCodec s Move.codec (Move.Type.Block (Map.singleton (ref "wall") (Set.singleton (ref "bear")))) " {\"Block\":{\"@wall\":[\"@bear\"]}} "
  Spec.it s "AssignDamage is keyed by recipient" $
    Common.assertCodec s Move.codec (Move.Type.AssignDamage (Map.fromList [(ref "first", 1), (ref "second", 1)])) " {\"AssignDamage\":{\"@first\":1,\"@second\":1}} "
  Spec.it s "ChooseDefender" $
    Common.assertCodec s Move.codec (Move.Type.ChooseDefender (Label.Type.MkLabel (Text.pack "bob"))) " {\"ChooseDefender\":\"bob\"} "
  Spec.it s "ChooseAttackTarget" $
    Common.assertCodec s Move.codec (Move.Type.ChooseAttackTarget (ref "bob")) " {\"ChooseAttackTarget\":\"@bob\"} "
  Spec.it s "OrderTimestamps" $
    Common.assertCodec s Move.codec (Move.Type.OrderTimestamps (Seq.fromList [ref "first", ref "second"])) " {\"OrderTimestamps\":[\"@first\",\"@second\"]} "
  Spec.it s "ChooseOptional" $
    Common.assertCodec s Move.codec (Move.Type.ChooseOptional OptionalDecision.Type.Exercises) " {\"ChooseOptional\":{\"type\":\"Exercises\"}} "
  Spec.it s "ChooseTargets is keyed by slot" $
    Common.assertCodec s Move.codec (Move.Type.ChooseTargets (Map.singleton (SlotName.Type.MkSlotName (Text.pack "target")) (Seq.singleton (ref "moon")))) " {\"ChooseTargets\":{\"target\":[\"@moon\"]}} "
  Spec.it s "Concede" $
    Common.assertCodec s Move.codec Move.Type.Concede " \"Concede\" "
  Spec.it s "Pass" $
    Common.assertCodec s Move.codec Move.Type.Pass " \"Pass\" "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Move.codec

ref :: String -> Reference.Type.Reference
ref = Reference.Type.Labelled . Label.Type.MkLabel . Text.pack
