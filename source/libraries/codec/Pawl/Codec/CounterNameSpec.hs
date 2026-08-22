module Pawl.Codec.CounterNameSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.CounterName as CounterName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterName as CounterName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterName" $ do
  Spec.it s "UnsafeMkCounterName" $
    Common.assertCodec
      s
      CounterName.codec
      (CounterName.UnsafeMkCounterName (Text.pack "conqueror"))
      " \"conqueror\" "

  -- CR 122.1: the name IS the identity, so nothing folds case or punctuation.
  -- Two spellings a normalization would merge stay two kinds.
  Spec.it s "UnsafeMkCounterName, a name that differs only in case" $
    Common.assertCodec
      s
      CounterName.codec
      (CounterName.UnsafeMkCounterName (Text.pack "Conqueror"))
      " \"Conqueror\" "

  Spec.it s "has a schema" $
    Common.assertHasSchema s CounterName.codec

  Spec.it s "rejects a non-string" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " 1 ") >>= Codec.decode CounterName.codec))
      "expected a decode failure"

  -- CR 122.1's interchangeability sentence: a name Pawl.Types.CounterKind
  -- already has a constructor for would key the same rules object twice.
  Spec.it s "rejects a reserved spelling" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " \"shield\" ") >>= Codec.decode CounterName.codec))
      "expected a decode failure"

  Spec.it s "rejects the empty name" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " \"\" ") >>= Codec.decode CounterName.codec))
      "expected a decode failure"
