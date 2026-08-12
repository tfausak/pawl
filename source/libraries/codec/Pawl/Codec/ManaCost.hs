module Pawl.Codec.ManaCost where

import qualified Pawl.Codec.ManaSymbol as ManaSymbol
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaCost as ManaCost

-- | 'Common.wrapper', not 'Common.scalar': the latter takes a bare
-- 'Schema.Schema' rather than a 'Define.SchemaM', so it cannot express a schema
-- built from another codec's (recursive) 'Codec.schema'. Either way this stays
-- NAMED: 'ManaCost' is a domain type a card author sees named in the schema
-- ("mana" in @Pawl.Codec.Cost@) and should be able to look up, same as
-- 'PlayerId' gets a $defs entry despite wrapping a bare 'Natural.Natural'. The
-- distinction is named-domain-type vs. structural-wrapper, never what the type
-- wraps -- a bare 'Maybe' or array inlines (e.g. 'Common.list' itself), while a
-- domain type wrapping the exact same shape (this one) gets a $defs entry.
-- 'ManaSymbol''s $defs entry documents the ELEMENT; this one documents the
-- COLLECTION, so the two are not redundant with each other.
codec :: Codec.Codec ManaCost.ManaCost
codec =
  Common.wrapper
    (Common.list ManaSymbol.codec)
    ManaCost.MkManaCost
    ManaCost.unwrap
