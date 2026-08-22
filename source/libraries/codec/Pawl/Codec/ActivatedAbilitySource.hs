{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActivatedAbilitySource where

import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource

-- | The ability codec is built from the card codec the same way every other
-- parameterized codec in this library is.
codec :: Codec.Codec ActivatedAbilitySource.ActivatedAbilitySource
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActivatedAbilitySource.source
  ability <- Fields.required "ability" (ActivatedAbility.codec Card.codec) ActivatedAbilitySource.ability
  pure
    ActivatedAbilitySource.MkActivatedAbilitySource
      { ActivatedAbilitySource.source = source,
        ActivatedAbilitySource.ability = ability
      }
