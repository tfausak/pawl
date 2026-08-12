module Pawl.Codec.KeywordFamily where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.KeywordFamily as KeywordFamily

-- | Nullary tags, unlike Pawl.Codec.Keyword's tagged pairs: the type is
-- payload-free, so `{"type":"Toxic"}` names the family and
-- `{"type":"Toxic","value":2}` the written instance. The two never collide,
-- because they sit under different Filter tags -- HasKeywordFamily and
-- HasKeyword.
codec :: Codec.Codec KeywordFamily.KeywordFamily
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Hexproof" KeywordFamily.Hexproof,
      Arm.nullary "Landwalk" KeywordFamily.Landwalk,
      Arm.nullary "Rampage" KeywordFamily.Rampage,
      Arm.nullary "Cycling" KeywordFamily.Cycling,
      Arm.nullary "Kicker" KeywordFamily.Kicker,
      Arm.nullary "Flashback" KeywordFamily.Flashback,
      Arm.nullary "Morph" KeywordFamily.Morph,
      Arm.nullary "Entwine" KeywordFamily.Entwine,
      Arm.nullary "Modular" KeywordFamily.Modular,
      Arm.nullary "Bushido" KeywordFamily.Bushido,
      Arm.nullary "Soulshift" KeywordFamily.Soulshift,
      Arm.nullary "Vanishing" KeywordFamily.Vanishing,
      Arm.nullary "Poisonous" KeywordFamily.Poisonous,
      Arm.nullary "Reinforce" KeywordFamily.Reinforce,
      Arm.nullary "Annihilator" KeywordFamily.Annihilator,
      Arm.nullary "Outlast" KeywordFamily.Outlast,
      Arm.nullary "Renown" KeywordFamily.Renown,
      Arm.nullary "Crew" KeywordFamily.Crew,
      Arm.nullary "Fabricate" KeywordFamily.Fabricate,
      Arm.nullary "Afflict" KeywordFamily.Afflict,
      Arm.nullary "Afterlife" KeywordFamily.Afterlife,
      Arm.nullary "Toxic" KeywordFamily.Toxic
    ]
  where
    encode f = Common.nullary $ case f of
      KeywordFamily.Hexproof -> "Hexproof"
      KeywordFamily.Landwalk -> "Landwalk"
      KeywordFamily.Rampage -> "Rampage"
      KeywordFamily.Cycling -> "Cycling"
      KeywordFamily.Kicker -> "Kicker"
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
