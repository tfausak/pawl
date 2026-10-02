module Pawl.Types.Check where

import qualified Pawl.Types.AttackersAre as AttackersAre
import qualified Pawl.Types.BlockersAre as BlockersAre
import qualified Pawl.Types.CountIs as CountIs
import qualified Pawl.Types.CountersAre as CountersAre
import qualified Pawl.Types.DamageIs as DamageIs
import qualified Pawl.Types.DefendersAre as DefendersAre
import qualified Pawl.Types.KeywordsAre as KeywordsAre
import qualified Pawl.Types.LifeIs as LifeIs
import qualified Pawl.Types.MonarchIs as MonarchIs
import qualified Pawl.Types.NamesAre as NamesAre
import qualified Pawl.Types.PlayerCountersAre as PlayerCountersAre
import qualified Pawl.Types.PowerToughnessIs as PowerToughnessIs
import qualified Pawl.Types.SubtypesAre as SubtypesAre
import qualified Pawl.Types.TappedIs as TappedIs
import qualified Pawl.Types.TypesAre as TypesAre
import qualified Pawl.Types.ViewIs as ViewIs

-- | One fact a scenario asserts about the game, in the nouns a client would
-- display.
data Check
  = Life LifeIs.LifeIs
  | Count CountIs.CountIs
  | Damage DamageIs.DamageIs
  | Tapped TappedIs.TappedIs
  | Counters CountersAre.CountersAre
  | Types TypesAre.TypesAre
  | Attackers AttackersAre.AttackersAre
  | Blockers BlockersAre.BlockersAre
  | Defenders DefendersAre.DefendersAre
  | Monarch MonarchIs.MonarchIs
  | PowerToughness PowerToughnessIs.PowerToughnessIs
  | PlayerCounters PlayerCountersAre.PlayerCountersAre
  | Subtypes SubtypesAre.SubtypesAre
  | Names NamesAre.NamesAre
  | View ViewIs.ViewIs
  | Keywords KeywordsAre.KeywordsAre
  deriving (Eq, Ord, Show)
