module Pawl.Codec.CounterPatternSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.CounterPattern as CounterPattern
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterSubject as CounterSubject
import qualified Pawl.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterPattern" $ do
  -- A fixed kind, a real filter, and CR 614.1's passive subject -- the clause
  -- names neither an effect nor a player.
  Spec.it s "Hardened Scales (a fixed kind, a real filter)" $
    Common.assertCodec
      s
      CounterPattern.codec
      CounterPattern.MkCounterPattern
        { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
          CounterPattern.subject = CounterSubject.ByAnything,
          CounterPattern.whose = ControllerRelation.Yours,
          CounterPattern.onWhat = Filter.HasCardType CardType.Creature,
          CounterPattern.onWho = Nothing
        }
      " {\"whichKind\":{\"type\":\"PlusOnePlusOne\"},\"subject\":{\"type\":\"ByAnything\"},\"whose\":{\"type\":\"Yours\"},\"onWhat\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- whichKind = Nothing means ANY kind, never "no kind", and the trivial filter
  -- matches every permanent. An omitted key is what that Nothing means.
  Spec.it s "Doubling Season (any kind, the trivial filter)" $
    Common.assertCodec
      s
      CounterPattern.codec
      CounterPattern.MkCounterPattern
        { CounterPattern.whichKind = Nothing,
          CounterPattern.subject = CounterSubject.ByEffect,
          CounterPattern.whose = ControllerRelation.Yours,
          CounterPattern.onWhat = Filter.And [],
          CounterPattern.onWho = Nothing
        }
      " {\"subject\":{\"type\":\"ByEffect\"},\"whose\":{\"type\":\"Yours\"},\"onWhat\":{\"type\":\"And\",\"value\":[]}} "
  -- CR 109.5: whichKind's Nothing and whose's Anyones are both what a pattern
  -- that says nothing means, so only the two required keys survive.
  Spec.it s "an all-default value omits every optional key" $
    Common.assertCodec
      s
      CounterPattern.codec
      CounterPattern.MkCounterPattern
        { CounterPattern.whichKind = Nothing,
          CounterPattern.subject = CounterSubject.ByAnything,
          CounterPattern.whose = ControllerRelation.Anyones,
          CounterPattern.onWhat = Filter.And [],
          CounterPattern.onWho = Nothing
        }
      " {\"subject\":{\"type\":\"ByAnything\"},\"onWhat\":{\"type\":\"And\",\"value\":[]}} "
  -- CR 122.6: Vorinclex, Monstrous Raider's halving clause -- narrowed by who is
  -- PUTTING the counters, and reaching players as well as permanents.
  Spec.it s "Vorinclex (a putter relation, and players too)" $
    Common.assertCodec
      s
      CounterPattern.codec
      CounterPattern.MkCounterPattern
        { CounterPattern.whichKind = Nothing,
          CounterPattern.subject = CounterSubject.ByPlayer ControllerRelation.Opponents,
          CounterPattern.whose = ControllerRelation.Anyones,
          CounterPattern.onWhat = Filter.And [],
          CounterPattern.onWho = Just ControllerRelation.Anyones
        }
      " {\"subject\":{\"type\":\"ByPlayer\",\"value\":{\"type\":\"Opponents\"}},\"onWhat\":{\"type\":\"And\",\"value\":[]},\"onWho\":{\"type\":\"Anyones\"}} "
  -- The breaking format change, made to prove itself: `subject` is required, so
  -- the pre-#1232 wire shape -- the same object with the key absent -- does not
  -- decode to a defaulted subject, it does not decode at all.
  Spec.it s "rejects a CounterPattern with no subject key" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode CounterPattern.codec =<< Common.parse (Text.pack " {\"onWhat\":{\"type\":\"And\",\"value\":[]}} ")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterPattern.codec
