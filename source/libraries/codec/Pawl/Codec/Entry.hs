module Pawl.Codec.Entry where

import qualified Data.Text as Text
import qualified Pawl.Codec.Check as Check
import qualified Pawl.Codec.Move as Move
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Types.Entry as Entry

codec :: Codec.Codec Entry.Entry
codec = Fields.object fields

-- | Exactly one of @do@, @refuse@ and @check@, which Pawl.Codec.Timed writes
-- into its own object. Written out rather than composed, because no optional
-- fields can say "exactly one"; the schema names all three and requires none,
-- and the decoder is the stricter.
fields :: Fields.Fields Entry.Entry Entry.Entry
fields =
  Fields.MkFields
    { Fields.encode = encodeFields,
      Fields.decode = decodeFields,
      Fields.schema = do
        move <- Codec.schema Move.codec
        check <- Codec.schema Check.codec
        pure ([Value.pair "do" (Schema.unwrap move), Value.pair "refuse" (Schema.unwrap move), Value.pair "check" (Schema.unwrap check)], [])
    }

encodeFields :: Entry.Entry -> [Pair.Pair Value.Value]
encodeFields x = case x of
  Entry.Do move -> [Value.pair "do" (Codec.encode Move.codec move)]
  Entry.Refuse move -> [Value.pair "refuse" (Codec.encode Move.codec move)]
  Entry.Expect check -> [Value.pair "check" (Codec.encode Check.codec check)]

decodeFields :: [Pair.Pair Value.Value] -> Either Text.Text Entry.Entry
decodeFields ps = case (Common.optionalField "do" ps, Common.optionalField "refuse" ps, Common.optionalField "check" ps) of
  (Just move, Nothing, Nothing) -> fmap Entry.Do (Codec.decode Move.codec move)
  (Nothing, Just move, Nothing) -> fmap Entry.Refuse (Codec.decode Move.codec move)
  (Nothing, Nothing, Just check) -> fmap Entry.Expect (Codec.decode Check.codec check)
  _ -> Left (Text.pack "expected exactly one of do, refuse and check")
