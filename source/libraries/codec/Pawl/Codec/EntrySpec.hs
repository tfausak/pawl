module Pawl.Codec.EntrySpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Entry as Entry
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Check as Check.Type
import qualified Pawl.Types.Entry as Entry.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.LifeIs as LifeIs.Type
import qualified Pawl.Types.Move as Move.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Entry" $ do
  Spec.it s "a move is under do" $
    Common.assertCodec s Entry.codec (Entry.Type.Do Move.Type.Pass) " {\"do\":\"Pass\"} "
  Spec.it s "a refused move is under refuse" $
    Common.assertCodec s Entry.codec (Entry.Type.Refuse Move.Type.Pass) " {\"refuse\":\"Pass\"} "
  Spec.it s "a check is under check" $
    Common.assertCodec s Entry.codec (Entry.Type.Expect (Check.Type.Life (LifeIs.Type.MkLifeIs (Label.Type.MkLabel (Text.pack "bob")) 18))) " {\"check\":{\"Life\":{\"player\":\"bob\",\"life\":18}}} "
  Spec.it s "rejects two and none" $
    Spec.assertBool
      s
      (all (\t -> Either.isLeft (Common.parse (Text.pack t) >>= Codec.decode Entry.codec)) ["{}", "{\"do\":\"Pass\",\"check\":{\"Life\":{\"player\":\"bob\",\"life\":18}}}", "{\"do\":\"Pass\",\"refuse\":\"Pass\"}"])
      "expected each to fail"
  Spec.it s "has a schema" $
    Common.assertHasSchema s Entry.codec
