module Pawl.Codec.ZoneChangeSubject where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

toJson :: ZoneChangeSubject.ZoneChangeSubject -> Value.Value
toJson s = Common.nullary $ case s of
  ZoneChangeSubject.AnyObject -> "AnyObject"
  ZoneChangeSubject.TheSource -> "TheSource"

fromJson :: Value.Value -> Either Text.Text ZoneChangeSubject.ZoneChangeSubject
fromJson =
  Common.decodeNullary
    "ZoneChangeSubject"
    [ ("AnyObject", ZoneChangeSubject.AnyObject),
      ("TheSource", ZoneChangeSubject.TheSource)
    ]
