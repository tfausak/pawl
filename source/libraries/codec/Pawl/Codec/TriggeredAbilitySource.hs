{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TriggeredAbilitySource where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource

-- | The ability codec is built from the card codec the same way every other
-- parameterized codec in this library is.
codec :: Codec.Codec TriggeredAbilitySource.TriggeredAbilitySource
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec TriggeredAbilitySource.source
  ability <- Fields.required "ability" (TriggeredAbility.codec Card.codec) TriggeredAbilitySource.ability
  -- CR 603.7a: absent for an ability the source itself has, which is every
  -- trigger but a delayed one.
  createdAt <- Fields.defaulted "createdAt" Nothing (Common.maybe Timestamp.codec) TriggeredAbilitySource.createdAt
  pure
    TriggeredAbilitySource.MkTriggeredAbilitySource
      { TriggeredAbilitySource.source = source,
        TriggeredAbilitySource.ability = ability,
        TriggeredAbilitySource.createdAt = createdAt
      }
