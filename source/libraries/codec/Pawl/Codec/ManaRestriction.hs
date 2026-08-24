{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManaRestriction where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManaRestriction as ManaRestriction

-- | Both of CR 106.6's payment kinds, each DEFAULTED to Nothing -- a card that
-- says "only to cast artifact spells" writes no activation half, and one that
-- says "only to activate abilities" writes no cast half. The default is a
-- refusal rather than a permission, so an omitted key never widens what the
-- printing allows.
--
-- An object and not a bare filter, which is what the field held before Omen
-- Hawker: the same predicate means different things depending on which payment
-- it is about, so the wire has to say which.
codec :: Codec.Codec ManaRestriction.ManaRestriction
codec = Fields.object $ do
  casts <- Fields.defaulted "casts" Nothing (Common.maybe (Filter.codec Keyword.codec)) ManaRestriction.casts
  activations <- Fields.defaulted "activations" Nothing (Common.maybe (Filter.codec Keyword.codec)) ManaRestriction.activations
  pure
    ManaRestriction.MkManaRestriction
      { ManaRestriction.casts = casts,
        ManaRestriction.activations = activations
      }
