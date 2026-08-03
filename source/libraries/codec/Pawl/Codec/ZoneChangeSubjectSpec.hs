module Pawl.Codec.ZoneChangeSubjectSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ZoneChangeSubject as ZoneChangeSubject
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ZoneChangeSubject" $ do
  Spec.it s "AnyObject" $
    Common.assertJsonCodec
      s
      ZoneChangeSubject.toJson
      ZoneChangeSubject.fromJson
      ZoneChangeSubject.AnyObject
      "{\"type\":\"AnyObject\"}"
  Spec.it s "TheSource" $
    Common.assertJsonCodec
      s
      ZoneChangeSubject.toJson
      ZoneChangeSubject.fromJson
      ZoneChangeSubject.TheSource
      "{\"type\":\"TheSource\"}"
