module Pawl.Codec.ScopeSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Scope as Scope
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.MovedBetween as MovedBetween
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Scope" $ do
  Spec.it s "InZone" $
    Common.assertCodec
      s
      Scope.codec
      (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
      " {\"type\":\"InZone\",\"value\":{\"zone\":{\"type\":\"Battlefield\"},\"player\":{\"type\":\"EachPlayer\"}}} "
  -- CR 608.2i's look-back-in-time domain.
  Spec.it s "InHistory" $
    Common.assertCodec
      s
      Scope.codec
      (Scope.InHistory (EventShape.MovedBetween (MovedBetween.MkMovedBetween Zone.Battlefield Zone.Graveyard)))
      " {\"type\":\"InHistory\",\"value\":{\"type\":\"MovedBetween\",\"value\":{\"from\":{\"type\":\"Battlefield\"},\"to\":{\"type\":\"Graveyard\"}}}} "
  -- CR 102.1's domain: the players a reference names, rather than their zones.
  Spec.it s "OverPlayers" $
    Common.assertCodec
      s
      Scope.codec
      (Scope.OverPlayers (PlayerRef.Relative PlayerRelation.Opponent))
      " {\"type\":\"OverPlayers\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"Opponent\"}}} "
  -- CR 400.7j's domain: the objects a slot of the surrounding announcement
  -- names, rather than a zone's residents.
  Spec.it s "OverBound" $
    Common.assertCodec
      s
      Scope.codec
      (Scope.OverBound (SlotName.MkSlotName (Text.pack "discarded")))
      " {\"type\":\"OverBound\",\"value\":\"discarded\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Scope.codec
