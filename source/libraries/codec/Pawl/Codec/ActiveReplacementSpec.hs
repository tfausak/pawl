module Pawl.Codec.ActiveReplacementSpec where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ActiveReplacement as ActiveReplacement
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- | Rest in Peace's rewrite, borrowed from Pawl.Codec.ReplacementEffectSpec: the
-- payload is not what this module has to prove, so the same value serves both
-- cases below.
effect :: ReplacementEffect.ReplacementEffect (Effect.Effect card)
effect =
  ReplacementEffect.ZoneChangeR
    ( ZoneChangeR.MkZoneChangeR
        ZoneChangePattern.MkZoneChangePattern
          { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
            ZoneChangePattern.whatObject = Filter.And [],
            ZoneChangePattern.whoseObject = ControllerRelation.Anyones
          }
        Zone.Exile
    )

effectJson :: String
effectJson = "{\"type\":\"ZoneChangeR\",\"value\":{\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"}},\"destination\":{\"type\":\"Exile\"}}}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveReplacement" $ do
  -- CR 614.3: the row most installers write -- no printed clause, no CR 615.5
  -- rider, and no CR 603.7c slot snapshot. `origin` is Other, which is CR
  -- 614.15's "other replacement effects".
  Spec.it s "a bare floating row" $
    Common.assertCodec
      s
      ActiveReplacement.codec
      ActiveReplacement.MkActiveReplacement
        { ActiveReplacement.effect = effect,
          ActiveReplacement.source = ObjectId.MkObjectId 1,
          ActiveReplacement.controller = PlayerId.MkPlayerId 2,
          ActiveReplacement.timestamp = Timestamp.MkTimestamp 3,
          ActiveReplacement.expiry = Expiry.AtCleanup,
          ActiveReplacement.uses = Uses.Unlimited,
          ActiveReplacement.origin = ReplacementOrigin.Other,
          ActiveReplacement.condition = Nothing,
          ActiveReplacement.rider = Nothing,
          ActiveReplacement.slots = Map.empty
        }
      ( " {\"effect\":"
          <> effectJson
          <> ",\"source\":1,\"controller\":2,\"timestamp\":3,\"expiry\":{\"type\":\"AtCleanup\"}"
          <> ",\"uses\":{\"type\":\"Unlimited\"},\"origin\":{\"type\":\"Other\"}"
          <> ",\"condition\":null,\"rider\":null,\"slots\":{}} "
      )
  -- Every optional axis at once, each away from the case above: CR 614.1's
  -- printed "if", CR 615.5's rider, CR 614.3's used-up count, CR 614.15's
  -- self-replacement, and CR 603.7c's snapshot. The snapshot's value is a SET of
  -- two ids, so an encoder writing only one is caught.
  Spec.it s "a row carrying a clause, a rider and a slot snapshot" $
    Common.assertCodec
      s
      ActiveReplacement.codec
      ActiveReplacement.MkActiveReplacement
        { ActiveReplacement.effect = effect,
          ActiveReplacement.source = ObjectId.MkObjectId 4,
          ActiveReplacement.controller = PlayerId.MkPlayerId 5,
          ActiveReplacement.timestamp = Timestamp.MkTimestamp 6,
          ActiveReplacement.expiry = Expiry.Never,
          ActiveReplacement.uses = Uses.Once,
          ActiveReplacement.origin = ReplacementOrigin.SelfReplacement,
          ActiveReplacement.condition =
            Just (Condition.Compares (Compares.MkCompares Quantity.Power Comparison.AtLeast (Quantity.Literal 7))),
          ActiveReplacement.rider =
            Just
              PreventionRider.MkPreventionRider
                { PreventionRider.effects = Seq.fromList [Effect.Proliferate],
                  PreventionRider.targets = Map.empty,
                  PreventionRider.controller = PlayerId.MkPlayerId 5,
                  PreventionRider.source = ObjectId.MkObjectId 4
                },
          ActiveReplacement.slots =
            Map.singleton
              (SlotName.MkSlotName (Text.pack "that spell"))
              (Set.fromList [ObjectId.MkObjectId 8, ObjectId.MkObjectId 9])
        }
      ( " {\"effect\":"
          <> effectJson
          <> ",\"source\":4,\"controller\":5,\"timestamp\":6,\"expiry\":{\"type\":\"Never\"}"
          <> ",\"uses\":{\"type\":\"Once\"},\"origin\":{\"type\":\"SelfReplacement\"}"
          <> ",\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Power\"},\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":7}}}"
          <> ",\"rider\":{\"effects\":[{\"type\":\"Proliferate\"}],\"targets\":{},\"controller\":5,\"source\":4}"
          <> ",\"slots\":{\"that spell\":[8,9]}} "
      )
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveReplacement.codec
