module Pawl.Codec.MovedKindsSpec where

import qualified Pawl.Codec.MovedKinds as MovedKinds
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MovedKinds" $ do
  -- Fate Transfer's "move all counters": no kind and no count, so the tag is the
  -- whole of it.
  Spec.it s "Every" $
    Common.assertCodec s MovedKinds.codec MovedKinds.Every " {\"type\":\"Every\"} "
  -- Explorer's Cache's "move a +1/+1 counter", where the count of one is what "a
  -- counter" means. The payload is the pair CR 122.6's entry riders write, so
  -- both spell it `count` and the schema carries one definition.
  Spec.it s "Named carries a kind and a count" $
    Common.assertCodec
      s
      MovedKinds.codec
      (MovedKinds.Named CounterKind.PlusOnePlusOne (Quantity.Literal 1))
      " {\"type\":\"Named\",\"value\":{\"count\":{\"type\":\"Literal\",\"value\":1},\"kind\":{\"type\":\"PlusOnePlusOne\"}}} "
  -- CR 122.1b's keyword counter reaches the named arm like any other kind.
  Spec.it s "Named carries a keyword counter too" $
    Common.assertCodec
      s
      MovedKinds.codec
      (MovedKinds.Named (CounterKind.Keyword Keyword.Flying) (Quantity.Literal 2))
      " {\"type\":\"Named\",\"value\":{\"count\":{\"type\":\"Literal\",\"value\":2},\"kind\":{\"type\":\"Keyword\",\"value\":{\"type\":\"Flying\"}}}} "
  -- Spike Cannibal's "move all +1/+1 counters": the kind is all there is to say,
  -- the count being the whole tally the first object bears. Arm.tagged compiles
  -- with no arm for a constructor and answers Nothing on encode (#2262), so this
  -- case is what forces one.
  Spec.it s "EveryOfKind carries only its kind" $
    Common.assertCodec
      s
      MovedKinds.codec
      (MovedKinds.EveryOfKind CounterKind.PlusOnePlusOne)
      " {\"type\":\"EveryOfKind\",\"value\":{\"type\":\"PlusOnePlusOne\"}} "
  -- Agent's Toolkit's "move a counter": the count is all there is to say, the
  -- kind being the player's to pick.
  Spec.it s "Chosen carries only its count" $
    Common.assertCodec
      s
      MovedKinds.codec
      (MovedKinds.Chosen (Quantity.Literal 1))
      " {\"type\":\"Chosen\",\"value\":{\"type\":\"Literal\",\"value\":1}} "
  -- Resourceful Defense's "move any number of counters": neither a kind nor a
  -- count, both being the player's to pick, so the tag is the whole of it.
  Spec.it s "AnyNumber" $
    Common.assertCodec s MovedKinds.codec MovedKinds.AnyNumber " {\"type\":\"AnyNumber\"} "
  -- Goldberry, River-Daughter's "move one or more counters": AnyNumber above with
  -- the empty answer excluded, and its own tag for that reason.
  Spec.it s "AtLeastOne" $
    Common.assertCodec s MovedKinds.codec MovedKinds.AtLeastOne " {\"type\":\"AtLeastOne\"} "
  -- Scrounging Bandar's "move any number of +1/+1 counters": the kind is all the
  -- card settles, the count being the player's to pick, so the payload is
  -- EveryOfKind's and the tag is what tells the two apart. Arm.tagged compiles
  -- with no arm for a constructor and answers Nothing on encode (#2262), so this
  -- case is what forces one.
  Spec.it s "AnyNumberOfKind carries only its kind" $
    Common.assertCodec
      s
      MovedKinds.codec
      (MovedKinds.AnyNumberOfKind CounterKind.PlusOnePlusOne)
      " {\"type\":\"AnyNumberOfKind\",\"value\":{\"type\":\"PlusOnePlusOne\"}} "
  -- Goldberry, River-Daughter's "move a counter of each kind not on Goldberry":
  -- the destination settles the kinds and the wording settles the count, so the
  -- tag is the whole of it. Arm.tagged compiles with no arm for a constructor
  -- and answers Nothing on encode (#2262), so this case is what forces one.
  Spec.it s "EachAbsentKind" $
    Common.assertCodec s MovedKinds.codec MovedKinds.EachAbsentKind " {\"type\":\"EachAbsentKind\"} "
  -- Takesies' "move up to one counter from each permanent": the player picks the
  -- kind and may pick none, so the tag is the whole of it. Arm.tagged compiles
  -- with no arm for a constructor and answers Nothing on encode (#2262), so this
  -- case is what forces one.
  Spec.it s "UpToOneChosen" $
    Common.assertCodec s MovedKinds.codec MovedKinds.UpToOneChosen " {\"type\":\"UpToOneChosen\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s MovedKinds.codec
