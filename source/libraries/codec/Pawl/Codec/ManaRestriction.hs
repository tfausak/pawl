{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManaRestriction where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManaRestriction as ManaRestriction

-- | Every one of CR 106.6's payment kinds, each DEFAULTED to Nothing -- a card
-- that says "only to cast artifact spells" writes no other half, and one that
-- says "only to turn permanents face up" writes no cast half. The default is a
-- refusal rather than a permission, so an omitted key never widens what the
-- printing allows -- and it is also why adding a kind leaves every card file in
-- the pool spelled as it was.
--
-- An object and not a bare filter, which is what the field held before Omen
-- Hawker: the same predicate means different things depending on which payment
-- it is about, so the wire has to say which.
codec :: Codec.Codec ManaRestriction.ManaRestriction
codec = Fields.object $ do
  casts <- Fields.defaulted "casts" Nothing (Common.maybe (Filter.codec Keyword.codec)) ManaRestriction.casts
  activations <- Fields.defaulted "activations" Nothing (Common.maybe (Filter.codec Keyword.codec)) ManaRestriction.activations
  unlocks <- Fields.defaulted "unlocks" Nothing (Common.maybe (Filter.codec Keyword.codec)) ManaRestriction.unlocks
  turnsFaceUp <- Fields.defaulted "turnsFaceUp" Nothing (Common.maybe (Filter.codec Keyword.codec)) ManaRestriction.turnsFaceUp
  pure
    ManaRestriction.MkManaRestriction
      { ManaRestriction.casts = casts,
        ManaRestriction.activations = activations,
        ManaRestriction.unlocks = unlocks,
        ManaRestriction.turnsFaceUp = turnsFaceUp
      }
