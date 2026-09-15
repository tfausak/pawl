module Pawl.Codec.EntersWithSpec where

import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.EntersWith as EntersWith
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntersWith as EntersWith
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.WithCounters as WithCounters

-- One constructor, so three cases: both halves of a printed sentence, the
-- `counters` key omitted, and the empty-keywords decode failure.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntersWith" $ do
  -- CR 614.1c, Faerie Squadron's "it enters with two +1/+1 counters on it and
  -- with flying" -- one sentence, one row.
  Spec.it s "counters and keywords" $
    Common.assertCodec
      s
      EntersWith.codec
      (EntersWith.MkEntersWith (Just (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.Literal 2))) (Set.singleton Keyword.Flying))
      " {\"counters\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":2}}],\"keywords\":[{\"type\":\"Flying\"}]} "
  -- CR 614.1c naming a keyword alone, which is the clause with its counter half
  -- unprinted.
  Spec.it s "keywords alone, the counters key omitted" $
    Common.assertCodec
      s
      EntersWith.codec
      (EntersWith.MkEntersWith Nothing (Set.singleton Keyword.Haste))
      " {\"keywords\":[{\"type\":\"Haste\"}]} "
  -- A clause granting no keyword is EntryRewrite.WithCounters, so this arm's
  -- empty set is a decode failure rather than a second spelling of that row
  -- (see #3288).
  Spec.it s "rejects a clause granting no keyword" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode EntersWith.codec =<< Common.parse (Text.pack "{\"keywords\":[]}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s EntersWith.codec
