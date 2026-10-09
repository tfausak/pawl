module Pawl.Codec.PlayerRefSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackingPlayers as AttackingPlayers
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerRef" $ do
  -- CR 102.1's whole table has one spelling, the relation every reader judges.
  Spec.it s "Relative AnyPlayer is the whole table" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.Relative PlayerRelation.AnyPlayer)
      " {\"type\":\"Relative\",\"value\":{\"type\":\"AnyPlayer\"}} "
  Spec.it s "Relative" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.Relative PlayerRelation.You)
      " {\"type\":\"Relative\",\"value\":{\"type\":\"You\"}} "
  Spec.it s "InSlot" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"InSlot\",\"value\":\"target\"} "
  Spec.it s "EachInSlot" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.EachInSlot (SlotName.MkSlotName (Text.pack "thoseWhoMay")))
      " {\"type\":\"EachInSlot\",\"value\":\"thoseWhoMay\"} "
  Spec.it s "Specific" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.Specific (PlayerId.MkPlayerId 1))
      " {\"type\":\"Specific\",\"value\":1} "
  Spec.it s "Candidate" $
    Common.assertCodec
      s
      PlayerRef.codec
      PlayerRef.Candidate
      " {\"type\":\"Candidate\"} "
  Spec.it s "ControllerOfBound" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.ControllerOfBound (SlotName.MkSlotName (Text.pack "permanent")))
      " {\"type\":\"ControllerOfBound\",\"value\":\"permanent\"} "
  Spec.it s "OwnerOfBound" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.OwnerOfBound (SlotName.MkSlotName (Text.pack "permanent")))
      " {\"type\":\"OwnerOfBound\",\"value\":\"permanent\"} "
  Spec.it s "ControllerOfObject" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.ControllerOfObject (ObjectId.MkObjectId 7))
      " {\"type\":\"ControllerOfObject\",\"value\":7} "
  Spec.it s "OwnerOfObject" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.OwnerOfObject (ObjectId.MkObjectId 7))
      " {\"type\":\"OwnerOfObject\",\"value\":7} "
  -- CR 614.1c / CR 702.174b: ControllerOfBound's shape one record over.
  Spec.it s "ChosenPlayerOfBound" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.ChosenPlayerOfBound (SlotName.MkSlotName (Text.pack "permanent")))
      " {\"type\":\"ChosenPlayerOfBound\",\"value\":\"permanent\"} "
  Spec.it s "Attacking" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.Attacking (AttackingPlayers.MkAttackingPlayers PlayerRelation.Opponent (SlotName.MkSlotName (Text.pack "attackedPlayer"))))
      " {\"type\":\"Attacking\",\"value\":{\"relation\":{\"type\":\"Opponent\"},\"attacked\":\"attackedPlayer\"}} "
  Spec.it s "EachOpponentExcept" $
    Common.assertCodec
      s
      PlayerRef.codec
      (PlayerRef.EachOpponentExcept (SlotName.MkSlotName (Text.pack "defendingPlayer")))
      " {\"type\":\"EachOpponentExcept\",\"value\":\"defendingPlayer\"} "
  Spec.it s "an unknown tag is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"EachOpponent\"} ") >>= Codec.decode PlayerRef.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerRef.codec
