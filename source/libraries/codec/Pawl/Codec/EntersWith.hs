{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntersWith where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.WithCounters as WithCounters
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EntersWith as EntersWith

-- | A bare object keyed by the record's field names, Pawl.Codec.AsCopy's shape,
-- carrying that module's @Maybe WithCounters@ wire form for the same payload.
-- @counters@ is defaulted rather than required: a clause that grants a keyword
-- and places nothing omits the key.
--
-- The non-empty keyword set is 'Fields.objectWith''s check rather than a
-- field's, Pawl.Codec.Modal's posture: it is a rule about which arm a card must
-- write -- a clause granting no keyword is EntryRewrite.WithCounters -- and has
-- no schema representation.
codec :: Codec.Codec EntersWith.EntersWith
codec =
  let check e =
        if Set.null (EntersWith.keywords e)
          then Left (Text.pack "enters-with clause grants no keyword")
          else Right e
   in Fields.objectWith check $ do
        counters <- Fields.defaulted "counters" Nothing (Common.maybe WithCounters.codec) EntersWith.counters
        keywords <- Fields.required "keywords" (Common.set Keyword.codec) EntersWith.keywords
        pure EntersWith.MkEntersWith {EntersWith.counters = counters, EntersWith.keywords = keywords}
