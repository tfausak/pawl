module Pawl.Codec.Expansion where

import qualified Data.Text as Text
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Expansion as Expansion

-- | A bare string on the wire -- "ARN" -- filed in @$defs@ under its own name,
-- exactly as 'Pawl.Codec.CardName' is.
--
-- The rejection lives in the INNER codec rather than in the wrap, because
-- 'Common.wrapper' takes a total injection and so cannot fail.
codec :: Codec.Codec Expansion.Expansion
codec = Common.wrapper resolvable Expansion.UnsafeMkExpansion Expansion.unwrap

-- | 'Common.text' with its decoder tightened to the codes CR 206.3 names -- a
-- record update, so the encoder and the schema stay shared. A card naming an
-- expansion 'Pawl.Types.Expansion.catalog' does not know fails to DECODE, which
-- is what keeps an unresolved code from reading as "this matches nothing".
--
-- The schema still says only "string", the treatment 'Pawl.Codec.CounterName'
-- gives its own tightening: an enumeration written into the schema is a second
-- copy of the catalog's keys.
resolvable :: Codec.Codec Text.Text
resolvable =
  Common.text
    { Codec.decode = \value -> Expansion.unwrap <$> (Common.asText value >>= make)
    }

-- | The only door into 'Pawl.Types.Expansion.Expansion'.
make :: Text.Text -> Either Text.Text Expansion.Expansion
make text =
  let expansion = Expansion.UnsafeMkExpansion text
   in case Expansion.names expansion of
        Just _ -> Right expansion
        Nothing ->
          Left
            ( Text.pack "Expansion: CR 206.3 names no expansion "
                <> text
            )
