module Pawl.Codec.WithCountersSpec where

import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.WithCounters as WithCounters
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.WithCounters as WithCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.WithCounters" $ do
  -- CR 614.1c, as a modular or graft permanent enters.
  Spec.it s "one kind" $
    Common.assertCodec
      s
      WithCounters.codec
      (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.Literal 2))
      " [{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":2}}] "
  -- CR 614.1c with #2314: Agent's Toolkit's several kinds as one row, ascending
  -- by kind so the encoding is canonical.
  Spec.it s "several kinds, ascending by kind" $
    Common.assertCodec
      s
      WithCounters.codec
      (WithCounters.MkWithCounters (Map.fromList [(CounterKind.PlusOnePlusOne, Quantity.Literal 1), (CounterKind.Keyword Keyword.Flying, Quantity.Literal 1)]))
      " [{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":1}},{\"kind\":{\"type\":\"Keyword\",\"value\":{\"type\":\"Flying\"}},\"count\":{\"type\":\"Literal\",\"value\":1}}] "
  -- A row that places nothing is not a sentence any card prints, so the empty
  -- array is a decode failure rather than a no-op row (#2314).
  Spec.it s "rejects a row with no kinds" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode WithCounters.codec =<< Common.parse (Text.pack "[]")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s WithCounters.codec
