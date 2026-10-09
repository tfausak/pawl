{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AbilitySticker where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.GrantedAbility as GrantedAbility
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AbilitySticker as AbilitySticker

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec AbilitySticker.AbilitySticker
codec = Fields.object $ do
  tickets <- Fields.required "tickets" Common.natural AbilitySticker.tickets
  keywords <- Fields.defaulted "keywords" Map.empty (Common.repeats Keyword.codec) AbilitySticker.keywords
  abilities <- Fields.defaulted "abilities" [] (Common.list (GrantedAbility.codec Card.codec)) AbilitySticker.abilities
  pure
    AbilitySticker.MkAbilitySticker
      { AbilitySticker.tickets = tickets,
        AbilitySticker.keywords = keywords,
        AbilitySticker.abilities = abilities
      }
