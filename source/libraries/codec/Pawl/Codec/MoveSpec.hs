module Pawl.Codec.MoveSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Move as Move
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Activation as Activation.Type
import qualified Pawl.Types.Answer as Answer.Type
import qualified Pawl.Types.Casting as Casting.Type
import qualified Pawl.Types.Choices as Choices.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Move as Move.Type
import qualified Pawl.Types.OptionalDecision as OptionalDecision.Type
import qualified Pawl.Types.Paying as Paying.Type
import qualified Pawl.Types.PaymentDecision as PaymentDecision.Type
import qualified Pawl.Types.Reference as Reference.Type
import qualified Pawl.Types.Reply as Reply.Type
import qualified Pawl.Types.SlotName as SlotName.Type
import qualified Pawl.Types.Subtype as Subtype.Type
import qualified Pawl.Types.Taking as Taking.Type
import qualified Pawl.Types.TypeSwap as TypeSwap.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Move" $ do
  Spec.it s "Cast" $
    Common.assertCodec s Move.codec (Move.Type.Cast (Casting.Type.MkCasting (ref "bolt") Choices.Type.none)) " {\"Cast\":{\"object\":\"$bolt\"}} "
  Spec.it s "PlayLand" $
    Common.assertCodec s Move.codec (Move.Type.PlayLand (ref "land")) " {\"PlayLand\":\"$land\"} "
  Spec.it s "Activate" $
    Common.assertCodec s Move.codec (Move.Type.Activate (Activation.Type.MkActivation (ref "stone") Nothing Choices.Type.none)) " {\"Activate\":{\"object\":\"$stone\"}} "
  Spec.it s "Attack" $
    Common.assertCodec s Move.codec (Move.Type.Attack (Seq.fromList [ref "bear", ref "wolf"])) " {\"Attack\":[\"$bear\",\"$wolf\"]} "
  Spec.it s "Block is keyed by blocker" $
    Common.assertCodec s Move.codec (Move.Type.Block (Map.singleton (ref "wall") (Set.singleton (ref "bear")))) " {\"Block\":{\"$wall\":[\"$bear\"]}} "
  Spec.it s "AssignDamage is keyed by recipient" $
    Common.assertCodec s Move.codec (Move.Type.AssignDamage (Map.fromList [(ref "first", 1), (ref "second", 1)])) " {\"AssignDamage\":{\"$first\":1,\"$second\":1}} "
  Spec.it s "ChooseDefender" $
    Common.assertCodec s Move.codec (Move.Type.ChooseDefender (Label.Type.MkLabel (Text.pack "bob"))) " {\"ChooseDefender\":\"bob\"} "
  Spec.it s "ChooseAttackTarget" $
    Common.assertCodec s Move.codec (Move.Type.ChooseAttackTarget (ref "bob")) " {\"ChooseAttackTarget\":\"$bob\"} "
  Spec.it s "OrderTimestamps" $
    Common.assertCodec s Move.codec (Move.Type.OrderTimestamps (Seq.fromList [ref "first", ref "second"])) " {\"OrderTimestamps\":[\"$first\",\"$second\"]} "
  Spec.it s "ChooseOptional" $
    Common.assertCodec s Move.codec (Move.Type.ChooseOptional OptionalDecision.Type.Exercises) " {\"ChooseOptional\":{\"type\":\"Exercises\"}} "
  Spec.it s "ChooseToPay" $
    Common.assertCodec s Move.codec (Move.Type.ChooseToPay (Paying.Type.MkPaying PaymentDecision.Type.Pays Choices.Type.none)) " {\"ChooseToPay\":{\"decision\":{\"type\":\"Pays\"}}} "
  Spec.it s "ChooseTargets is keyed by slot" $
    Common.assertCodec s Move.codec (Move.Type.ChooseTargets (Map.singleton (SlotName.Type.MkSlotName (Text.pack "target")) (Seq.singleton (ref "moon")))) " {\"ChooseTargets\":{\"target\":[\"$moon\"]}} "
  Spec.it s "OrderTriggers, null for a sourceless trigger" $
    Common.assertCodec s Move.codec (Move.Type.OrderTriggers (Seq.fromList [Just (ref "ghoul"), Nothing])) " {\"OrderTriggers\":[\"$ghoul\",null]} "
  Spec.it s "ChooseCopyTarget" $
    Common.assertCodec s Move.codec (Move.Type.ChooseCopyTarget (Just (ref "bear"))) " {\"ChooseCopyTarget\":\"$bear\"} "
  Spec.it s "ChooseCopyTarget declined" $
    Common.assertCodec s Move.codec (Move.Type.ChooseCopyTarget Nothing) " {\"ChooseCopyTarget\":null} "
  Spec.it s "Answer" $
    Common.assertCodec s Move.codec (Move.Type.Answer (Answer.Type.MkAnswer (Text.pack "ChooseDiscard") (Reply.Type.Array [Reply.Type.Text (Text.pack "@card")]) False)) " {\"Answer\":{\"prompt\":\"ChooseDiscard\",\"with\":[\"@card\"]}} "
  Spec.it s "ChooseTypeSwap" $
    Common.assertCodec s Move.codec (Move.Type.ChooseTypeSwap (TypeSwap.Type.MkTypeSwap Subtype.Type.Goblin Subtype.Type.Elf)) " {\"ChooseTypeSwap\":{\"from\":{\"type\":\"Goblin\"},\"to\":{\"type\":\"Elf\"}}} "
  Spec.it s "Take" $
    Common.assertCodec s Move.codec (Move.Type.Take (Taking.Type.MkTaking (Text.pack "TurnFaceUp $piker Manifest") Choices.Type.none)) " {\"Take\":{\"action\":\"TurnFaceUp $piker Manifest\"}} "

  Spec.it s "Concede" $
    Common.assertCodec s Move.codec Move.Type.Concede " \"Concede\" "
  Spec.it s "Pass" $
    Common.assertCodec s Move.codec Move.Type.Pass " \"Pass\" "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Move.codec

ref :: String -> Reference.Type.Reference
ref = Reference.Type.Labelled . Label.Type.MkLabel . Text.pack
