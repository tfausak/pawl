module Pawl.Codec.Move where

import qualified Pawl.Codec.Activation as Activation
import qualified Pawl.Codec.Answer as Answer
import qualified Pawl.Codec.Casting as Casting
import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.OptionalDecision as OptionalDecision
import qualified Pawl.Codec.Paying as Paying
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.Codec.Taking as Taking
import qualified Pawl.Codec.TypeSwap as TypeSwap
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Move as Move
import qualified Pawl.Types.SlotName as SlotName

-- | Keyed, so a move reads @{"Attack": ["$bear"]}@ and a pass @"Pass"@. Blocks
-- and assignments are objects keyed by reference: @{"$wall": ["$bear"]}@.
codec :: Codec.Codec Move.Move
codec =
  Arm.keyed
    tagOf
    [ Arm.payload "Cast" Casting.codec Move.Cast (\x -> case x of Move.Cast y -> Just y; _ -> Nothing),
      Arm.payload "PlayLand" Reference.codec Move.PlayLand (\x -> case x of Move.PlayLand y -> Just y; _ -> Nothing),
      Arm.payload "Activate" Activation.codec Move.Activate (\x -> case x of Move.Activate y -> Just y; _ -> Nothing),
      Arm.payload "Attack" (Common.seq Reference.codec) Move.Attack (\x -> case x of Move.Attack y -> Just y; _ -> Nothing),
      Arm.payload "Block" (Common.textMap Reference.toText Reference.fromText (Common.set Reference.codec)) Move.Block (\x -> case x of Move.Block y -> Just y; _ -> Nothing),
      Arm.payload "AssignDamage" (Common.textMap Reference.toText Reference.fromText Common.natural) Move.AssignDamage (\x -> case x of Move.AssignDamage y -> Just y; _ -> Nothing),
      Arm.payload "ChooseDefender" Label.codec Move.ChooseDefender (\x -> case x of Move.ChooseDefender y -> Just y; _ -> Nothing),
      Arm.payload "ChooseAttackTarget" Reference.codec Move.ChooseAttackTarget (\x -> case x of Move.ChooseAttackTarget y -> Just y; _ -> Nothing),
      Arm.payload "OrderTimestamps" (Common.seq Reference.codec) Move.OrderTimestamps (\x -> case x of Move.OrderTimestamps y -> Just y; _ -> Nothing),
      Arm.payload "ChooseOptional" OptionalDecision.codec Move.ChooseOptional (\x -> case x of Move.ChooseOptional y -> Just y; _ -> Nothing),
      Arm.payload "ChooseToPay" Paying.codec Move.ChooseToPay (\x -> case x of Move.ChooseToPay y -> Just y; _ -> Nothing),
      Arm.payload "ChooseCopyTarget" (Common.maybe Reference.codec) Move.ChooseCopyTarget (\x -> case x of Move.ChooseCopyTarget y -> Just y; _ -> Nothing),
      Arm.payload "Answer" Answer.codec Move.Answer (\x -> case x of Move.Answer y -> Just y; _ -> Nothing),
      Arm.payload "ChooseTypeSwap" TypeSwap.codec Move.ChooseTypeSwap (\x -> case x of Move.ChooseTypeSwap y -> Just y; _ -> Nothing),
      Arm.payload "ChooseTargets" (Common.textMap SlotName.unwrap (Right . SlotName.MkSlotName) (Common.seq Reference.codec)) Move.ChooseTargets (\x -> case x of Move.ChooseTargets y -> Just y; _ -> Nothing),
      Arm.payload "OrderTriggers" (Common.seq (Common.maybe Reference.codec)) Move.OrderTriggers (\x -> case x of Move.OrderTriggers y -> Just y; _ -> Nothing),
      Arm.payload "Take" Taking.codec Move.Take (\x -> case x of Move.Take y -> Just y; _ -> Nothing),
      Arm.nullary "Concede" Move.Concede,
      Arm.nullary "Pass" Move.Pass
    ]

tagOf :: Move.Move -> String
tagOf x = case x of
  Move.Cast {} -> "Cast"
  Move.PlayLand {} -> "PlayLand"
  Move.Activate {} -> "Activate"
  Move.Attack {} -> "Attack"
  Move.Block {} -> "Block"
  Move.AssignDamage {} -> "AssignDamage"
  Move.ChooseDefender {} -> "ChooseDefender"
  Move.ChooseAttackTarget {} -> "ChooseAttackTarget"
  Move.OrderTimestamps {} -> "OrderTimestamps"
  Move.ChooseOptional {} -> "ChooseOptional"
  Move.ChooseToPay {} -> "ChooseToPay"
  Move.ChooseTypeSwap {} -> "ChooseTypeSwap"
  Move.Answer {} -> "Answer"
  Move.ChooseCopyTarget {} -> "ChooseCopyTarget"
  Move.ChooseTargets {} -> "ChooseTargets"
  Move.OrderTriggers {} -> "OrderTriggers"
  Move.Take {} -> "Take"
  Move.Concede {} -> "Concede"
  Move.Pass {} -> "Pass"
