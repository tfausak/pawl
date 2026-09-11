{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntryR where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.EntryRewrite as EntryRewrite
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EntryR as EntryR

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
--
-- The effect codec is a PARAMETER rather than an import, for the reason
-- Pawl.Codec.DamageR gives.
codec ::
  (Typeable.Typeable ability, Eq ability, Typeable.Typeable effect, Eq effect) =>
  Codec.Codec ability ->
  Codec.Codec effect ->
  Codec.Codec (EntryR.EntryR ability effect)
codec abilityCodec effectCodec = Fields.object $ do
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) EntryR.matching
  rewrite <- Fields.required "rewrite" (EntryRewrite.codec abilityCodec effectCodec) EntryR.rewrite
  pure
    EntryR.MkEntryR
      { EntryR.matching = matching,
        EntryR.rewrite = rewrite
      }
