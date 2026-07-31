-- | The @ZoneChangeSubject ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.ZoneChangeSubject where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

zoneChangeSubjectToJson :: ZoneChangeSubject.ZoneChangeSubject -> Value
zoneChangeSubjectToJson s = Json.nullary . Text.pack $ case s of
  ZoneChangeSubject.AnyObject -> "AnyObject"
  ZoneChangeSubject.TheSource -> "TheSource"

jsonToZoneChangeSubject :: Value -> Either Text ZoneChangeSubject.ZoneChangeSubject
jsonToZoneChangeSubject =
  Json.decodeNullary
    (Text.pack "ZoneChangeSubject")
    [ (Text.pack "AnyObject", ZoneChangeSubject.AnyObject),
      (Text.pack "TheSource", ZoneChangeSubject.TheSource)
    ]
