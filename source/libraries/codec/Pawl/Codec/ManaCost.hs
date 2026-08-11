module Pawl.Codec.ManaCost where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ManaSymbol as ManaSymbol
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.Types.ManaCost as ManaCost

-- | NOT 'Common.scalar': its signature takes a bare 'Schema.Schema', not a
-- 'Define.SchemaM', so it cannot express a schema built from another codec's
-- (recursive) 'Codec.schema'. A hand-built 'Codec.MkCodec' instead, but still
-- NAMED: 'ManaCost' is a domain type a card author sees named in the schema
-- ("mana" in @Pawl.Codec.Cost@) and should be able to look up, same as
-- 'PlayerId' gets a $defs entry despite wrapping a bare 'Natural.Natural'. The
-- distinction 'Common.scalar' vs. inlining is named-domain-type vs.
-- structural-wrapper, never what the type wraps -- a bare 'Maybe' or array
-- inlines (e.g. 'Common.list' itself), while a domain type wrapping the exact
-- same shape (this one) gets a $defs entry. 'ManaSymbol''s $defs entry
-- documents the ELEMENT; this one documents the COLLECTION, so the two are not
-- redundant with each other.
codec :: Codec.Codec ManaCost.ManaCost
codec =
  Codec.MkCodec
    { Codec.encode = Codec.encode listCodec . ManaCost.unwrap,
      Codec.decode = fmap ManaCost.MkManaCost . Codec.decode listCodec,
      Codec.schema = Define.define (Name.typeName proxy) (Codec.schema listCodec)
    }
  where
    listCodec = Common.list ManaSymbol.codec
    proxy = Typeable.Proxy :: Typeable.Proxy ManaCost.ManaCost
