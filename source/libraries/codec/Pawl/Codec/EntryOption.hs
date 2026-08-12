{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntryOption where

import qualified Data.Set as Set
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EntryOption as EntryOption

-- | 'power' and 'toughness' stay REQUIRED (R5 of the omit-defaults design): a
-- token whose printed characteristics are 0/0 is a legal EntryOption, so an
-- absent power must not read as 0 when it means the file forgot to state one.
codec :: Codec.Codec EntryOption.EntryOption
codec = Fields.object $ do
  power <- Fields.required "power" Common.integer EntryOption.power
  toughness <- Fields.required "toughness" Common.integer EntryOption.toughness
  keywords <- Fields.defaulted "keywords" Set.empty (Common.set Keyword.codec) EntryOption.keywords
  pure
    EntryOption.MkEntryOption
      { EntryOption.power = power,
        EntryOption.toughness = toughness,
        EntryOption.keywords = keywords
      }
