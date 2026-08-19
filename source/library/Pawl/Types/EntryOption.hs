module Pawl.Types.EntryOption where

import qualified Data.Set as Set
import qualified Pawl.Types.Keyword as Keyword

-- | CR 208.2b / 614.1c: one of the shapes an "as this creature enters, it becomes
-- your choice of ..." ability offers. Primal Plasma's three are (3,3,{}),
-- (2,2,{Flying}) and (1,6,{Defender}).
--
-- An as-enters choice of a SUBTYPE is not expressible here (Convincing Mirage's
-- "as this Aura enters, choose a basic land type"): this record carries a P/T
-- and keywords only, and no Modification reads a subtype chosen at entry (#608).
--
-- The keywords are UNIONED into the object's copiable snapshot, never assigned
-- over it, per Primal Plasma's Gatherer ruling: a Clone of a 2/2-flying Plasma
-- that picks the third option is 1/6 with flying AND defender. P/T is SET
-- instead (CR 707.2).
data EntryOption = MkEntryOption
  { power :: Integer,
    toughness :: Integer,
    keywords :: Set.Set Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
