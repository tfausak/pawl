{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntersWith where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.WithCounters as WithCounters
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EntersWith as EntersWith

-- | A bare object keyed by the record's field names, Pawl.Codec.AsCopy's shape,
-- carrying that module's @Maybe WithCounters@ wire form for the same payload.
-- Every field is defaulted: a clause omits the half it does not print.
--
-- That the clause grants SOMETHING is 'Fields.objectWith''s check rather than a
-- field's, Pawl.Codec.Modal's posture: it is a rule about which arm a card must
-- write -- a clause granting nothing is EntryRewrite.WithCounters -- and has no
-- schema representation.
--
-- The ability codec is a PARAMETER for Pawl.Codec.CopyException's reason.
codec :: (Typeable.Typeable ability, Eq ability) => Codec.Codec ability -> Codec.Codec (EntersWith.EntersWith ability)
codec abilityCodec =
  let check e =
        if Set.null (EntersWith.keywords e) && Seq.null (EntersWith.abilities e)
          then Left (Text.pack "enters-with clause grants no keyword and no ability")
          else Right e
   in Fields.objectWith check $ do
        counters <- Fields.defaulted "counters" Nothing (Common.maybe WithCounters.codec) EntersWith.counters
        keywords <- Fields.defaulted "keywords" Set.empty (Common.set Keyword.codec) EntersWith.keywords
        abilities <- Fields.defaulted "abilities" Seq.empty (Common.seq abilityCodec) EntersWith.abilities
        pure EntersWith.MkEntersWith {EntersWith.counters = counters, EntersWith.keywords = keywords, EntersWith.abilities = abilities}
