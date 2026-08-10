module Pawl.Codec.KeywordFamily where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.KeywordFamily as KeywordFamily

-- | Nullary tags, unlike Pawl.Codec.Keyword's tagged pairs: the type is
-- payload-free, so `{"type":"Toxic"}` names the family and
-- `{"type":"Toxic","value":2}` the written instance. The two never collide,
-- because they sit under different Filter tags -- HasKeywordFamily and
-- HasKeyword.
toJson :: KeywordFamily.KeywordFamily -> Value.Value
toJson f = Common.nullary $ case f of
  KeywordFamily.Hexproof -> "Hexproof"
  KeywordFamily.Landwalk -> "Landwalk"
  KeywordFamily.Rampage -> "Rampage"
  KeywordFamily.Cycling -> "Cycling"
  KeywordFamily.Flashback -> "Flashback"
  KeywordFamily.Morph -> "Morph"
  KeywordFamily.Entwine -> "Entwine"
  KeywordFamily.Modular -> "Modular"
  KeywordFamily.Bushido -> "Bushido"
  KeywordFamily.Soulshift -> "Soulshift"
  KeywordFamily.Vanishing -> "Vanishing"
  KeywordFamily.Poisonous -> "Poisonous"
  KeywordFamily.Reinforce -> "Reinforce"
  KeywordFamily.Annihilator -> "Annihilator"
  KeywordFamily.Outlast -> "Outlast"
  KeywordFamily.Renown -> "Renown"
  KeywordFamily.Crew -> "Crew"
  KeywordFamily.Fabricate -> "Fabricate"
  KeywordFamily.Afflict -> "Afflict"
  KeywordFamily.Afterlife -> "Afterlife"
  KeywordFamily.Toxic -> "Toxic"

fromJson :: Value.Value -> Either Text.Text KeywordFamily.KeywordFamily
fromJson =
  Common.decodeNullary
    "KeywordFamily"
    [ ("Hexproof", KeywordFamily.Hexproof),
      ("Landwalk", KeywordFamily.Landwalk),
      ("Rampage", KeywordFamily.Rampage),
      ("Cycling", KeywordFamily.Cycling),
      ("Flashback", KeywordFamily.Flashback),
      ("Morph", KeywordFamily.Morph),
      ("Entwine", KeywordFamily.Entwine),
      ("Modular", KeywordFamily.Modular),
      ("Bushido", KeywordFamily.Bushido),
      ("Soulshift", KeywordFamily.Soulshift),
      ("Vanishing", KeywordFamily.Vanishing),
      ("Poisonous", KeywordFamily.Poisonous),
      ("Reinforce", KeywordFamily.Reinforce),
      ("Annihilator", KeywordFamily.Annihilator),
      ("Outlast", KeywordFamily.Outlast),
      ("Renown", KeywordFamily.Renown),
      ("Crew", KeywordFamily.Crew),
      ("Fabricate", KeywordFamily.Fabricate),
      ("Afflict", KeywordFamily.Afflict),
      ("Afterlife", KeywordFamily.Afterlife),
      ("Toxic", KeywordFamily.Toxic)
    ]
