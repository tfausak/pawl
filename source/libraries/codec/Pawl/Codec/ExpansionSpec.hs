module Pawl.Codec.ExpansionSpec where

import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Expansion as Expansion
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Expansion as Expansion.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Expansion" $ do
  Spec.it s "UnsafeMkExpansion" $
    Common.assertCodec
      s
      Expansion.codec
      (Expansion.Type.UnsafeMkExpansion (Text.pack "ARN"))
      " \"ARN\" "

  Spec.it s "has a schema" $
    Common.assertHasSchema s Expansion.codec

  Spec.it s "rejects a non-string" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " 1 ") >>= Codec.decode Expansion.codec))
      "expected a decode failure"

  -- The whole reason the door exists: CR 206.3 names three expansions, and a
  -- fourth code has no name set at all. Refused here rather than read as "this
  -- matches nothing", which would make a typo a silently harmless card.
  Spec.it s "CR 206.3 rejects a code the rule does not name" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " \"LEA\" ") >>= Codec.decode Expansion.codec))
      "expected a decode failure"

  -- Case matters, for Pawl.Types.CounterName's reason: the catalog's keys are
  -- exact text, and nothing here folds case.
  Spec.it s "rejects a code that differs only in case" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " \"arn\" ") >>= Codec.decode Expansion.codec))
      "expected a decode failure"

  -- Every key the catalog holds goes through the door, so a code added there
  -- cannot be one card data may not write.
  Spec.it s "accepts every code the catalog names" $
    Spec.assertBool
      s
      (all (Either.isRight . Expansion.make . Expansion.Type.unwrap) (Map.keys Expansion.Type.catalog))
      "expected every catalog key to resolve"
