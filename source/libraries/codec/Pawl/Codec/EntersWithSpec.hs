module Pawl.Codec.EntersWithSpec where

import qualified Data.Either as Either
import qualified Data.Sequence as Seq
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

-- One constructor, so four cases: counters and keywords of a printed sentence,
-- the `counters` key omitted, counters and a quoted ability with the `keywords`
-- key omitted, and the grants-nothing decode failure.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntersWith" $ do
  -- CR 614.1c, Faerie Squadron's "it enters with two +1/+1 counters on it and
  -- with flying" -- one sentence, one row.
  Spec.it s "counters and keywords" $
    Common.assertCodec
      s
      (EntersWith.codec Common.text)
      (EntersWith.MkEntersWith (Just (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.Literal 2))) (Set.singleton Keyword.Flying) Seq.empty)
      " {\"counters\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":2}}],\"keywords\":[{\"type\":\"Flying\"}]} "
  -- CR 614.1c naming a keyword alone, which is the clause with its counter half
  -- unprinted.
  Spec.it s "keywords alone, the counters key omitted" $
    Common.assertCodec
      s
      (EntersWith.codec Common.text)
      (EntersWith.MkEntersWith Nothing (Set.singleton Keyword.Haste) Seq.empty)
      " {\"keywords\":[{\"type\":\"Haste\"}]} "
  -- CR 614.1c, Degavolver's "it enters with two +1/+1 counters on it and with
  -- 'Pay 3 life: Regenerate this creature.'" -- a quoted ability, no keyword.
  Spec.it s "counters and a quoted ability, the keywords key omitted" $
    Common.assertCodec
      s
      (EntersWith.codec Common.text)
      (EntersWith.MkEntersWith (Just (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.Literal 2))) Set.empty (Seq.singleton (Text.pack "quoted")))
      " {\"abilities\":[\"quoted\"],\"counters\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":2}}]} "
  -- A clause granting nothing is EntryRewrite.WithCounters, so this arm's
  -- empty grant is a decode failure rather than a second spelling of that row
  -- (see #3288).
  Spec.it s "rejects a clause granting no keyword and no ability" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode (EntersWith.codec Common.text) =<< Common.parse (Text.pack "{\"keywords\":[],\"abilities\":[]}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s (EntersWith.codec Common.text)
