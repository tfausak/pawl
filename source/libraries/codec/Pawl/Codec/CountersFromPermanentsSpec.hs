module Pawl.Codec.CountersFromPermanentsSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.CountersFromPermanents as CountersFromPermanents
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CostAmount as CostAmount
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterSpread as CounterSpread
import qualified Pawl.Types.CountersFromPermanents as CountersFromPermanents
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.WhichCounters as WhichCounters

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (CountersFromPermanents.CountersFromPermanents Keyword.Keyword)
codec = CountersFromPermanents.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CountersFromPermanents" $ do
  -- Zameck Guildmage's one counter. The count is COUNTERS, and the key name is
  -- SINGULAR -- where TapPermanents' `whichPermanents` narrows a set of objects,
  -- this narrows the one permanent they come off.
  Spec.it s "MkCountersFromPermanents" $
    Common.assertCodec
      s
      codec
      ( CountersFromPermanents.MkCountersFromPermanents
          { CountersFromPermanents.count = CostAmount.Fixed 1,
            CountersFromPermanents.kind = WhichCounters.OfKind CounterKind.PlusOnePlusOne,
            CountersFromPermanents.whichPermanent = Filter.HasCardType CardType.Creature,
            CountersFromPermanents.spread = CounterSpread.FromOne
          }
      )
      " {\"count\":{\"type\":\"Fixed\",\"value\":1},\"kind\":{\"type\":\"OfKind\",\"value\":{\"type\":\"PlusOnePlusOne\"}},\"whichPermanent\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- Tayam, Luminous Enigma's three counters of any kind, divided among
  -- creatures: the spread is written only where it is not the one-permanent
  -- default.
  Spec.it s "FromAmong" $
    Common.assertCodec
      s
      codec
      ( CountersFromPermanents.MkCountersFromPermanents
          { CountersFromPermanents.count = CostAmount.Fixed 3,
            CountersFromPermanents.kind = WhichCounters.OfAnyKind,
            CountersFromPermanents.whichPermanent = Filter.HasCardType CardType.Creature,
            CountersFromPermanents.spread = CounterSpread.FromAmong
          }
      )
      " {\"count\":{\"type\":\"Fixed\",\"value\":3},\"kind\":{\"type\":\"OfAnyKind\"},\"whichPermanent\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"spread\":{\"type\":\"FromAmong\"}} "
  Spec.it s "rejects counters of any kind at a floor" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode codec =<< Common.parse (Text.pack "{\"count\":{\"type\":\"Fixed\",\"value\":1},\"kind\":{\"type\":\"OfAnyKind\"},\"whichPermanent\":{\"type\":\"IsSource\"},\"spread\":{\"type\":\"FromAmongAtLeast\"}}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
