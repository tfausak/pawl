module Pawl.Codec.ActivePlayerEffectSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SourceChoices as SourceChoices
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActivePlayerEffect" $ do
  -- CR 611.1 / 613.11, Silence's row: the controller is STORED because the
  -- source is an instant already in a graveyard, so CR 109.5's "your opponents"
  -- has nothing live to resolve against. Its seat differs from every other
  -- number here.
  Spec.it s "a scoped effect that ends at cleanup" $
    Common.assertCodec
      s
      ActivePlayerEffect.codec
      ActivePlayerEffect.MkActivePlayerEffect
        { ActivePlayerEffect.source = ObjectId.MkObjectId 1,
          ActivePlayerEffect.controller = PlayerId.MkPlayerId 2,
          ActivePlayerEffect.choices = SourceChoices.MkSourceChoices {SourceChoices.names = Set.empty, SourceChoices.colors = Set.empty, SourceChoices.subtype = Nothing},
          ActivePlayerEffect.timestamp = Timestamp.MkTimestamp 3,
          ActivePlayerEffect.expiry = Expiry.AtCleanup,
          ActivePlayerEffect.scope = AffectedPlayers.Scoped (PlayerScope.Related PlayerRelation.Opponent),
          ActivePlayerEffect.effect = PlayerEffect.CantCastSpells
        }
      " {\"source\":1,\"controller\":2,\"choices\":{},\"timestamp\":3,\"expiry\":{\"type\":\"AtCleanup\"},\"scope\":{\"type\":\"Scoped\",\"value\":{\"type\":\"Opponent\"}},\"effect\":{\"type\":\"CantCastSpells\"}} "
  -- The Named arm, which is the half no printed carrier has: the seat CR 601.2c
  -- chose, baked as the effect began. It differs from `controller`, so an
  -- encoder writing the controller into both fields could not pass.
  Spec.it s "an effect baked onto the seat its slot named" $
    Common.assertCodec
      s
      ActivePlayerEffect.codec
      ActivePlayerEffect.MkActivePlayerEffect
        { ActivePlayerEffect.source = ObjectId.MkObjectId 5,
          ActivePlayerEffect.controller = PlayerId.MkPlayerId 6,
          ActivePlayerEffect.choices = SourceChoices.MkSourceChoices {SourceChoices.names = Set.empty, SourceChoices.colors = Set.empty, SourceChoices.subtype = Nothing},
          ActivePlayerEffect.timestamp = Timestamp.MkTimestamp 7,
          ActivePlayerEffect.expiry = Expiry.Never,
          ActivePlayerEffect.scope = AffectedPlayers.Named (PlayerId.MkPlayerId 8),
          ActivePlayerEffect.effect = PlayerEffect.NoMaximumHandSize
        }
      " {\"source\":5,\"controller\":6,\"choices\":{},\"timestamp\":7,\"expiry\":{\"type\":\"Never\"},\"scope\":{\"type\":\"Named\",\"value\":8},\"effect\":{\"type\":\"NoMaximumHandSize\"}} "
  -- CR 608.2h: the names baked as the effect began, Cheering Fanatic's row.
  Spec.it s "an effect carrying the names its source had chosen" $
    Common.assertCodec
      s
      ActivePlayerEffect.codec
      ActivePlayerEffect.MkActivePlayerEffect
        { ActivePlayerEffect.source = ObjectId.MkObjectId 9,
          ActivePlayerEffect.controller = PlayerId.MkPlayerId 10,
          ActivePlayerEffect.choices = SourceChoices.MkSourceChoices {SourceChoices.names = Set.singleton (CardName.MkCardName (Text.pack "Shock")), SourceChoices.colors = Set.empty, SourceChoices.subtype = Nothing},
          ActivePlayerEffect.timestamp = Timestamp.MkTimestamp 11,
          ActivePlayerEffect.expiry = Expiry.AtCleanup,
          ActivePlayerEffect.scope = AffectedPlayers.Scoped (PlayerScope.Related PlayerRelation.AnyPlayer),
          ActivePlayerEffect.effect = PlayerEffect.CantCastSpells
        }
      " {\"source\":9,\"controller\":10,\"choices\":{\"names\":[\"Shock\"]},\"timestamp\":11,\"expiry\":{\"type\":\"AtCleanup\"},\"scope\":{\"type\":\"Scoped\",\"value\":{\"type\":\"AnyPlayer\"}},\"effect\":{\"type\":\"CantCastSpells\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActivePlayerEffect.codec
