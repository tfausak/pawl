module Pawl.Types.ManaUnit where

import qualified Data.Set as Set
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ProductionTag as ProductionTag

-- | One unit of mana in a pool.
--
-- Grows TWO separate collections, and conflating them would be a mistake.
-- Production-time tags are the CLOSED half -- snow-ness, "this activation caused
-- you to lose life" -- observable facts about the production event that the
-- engine determines with no card knowledge. Spending restrictions are the OPEN
-- half: Cavern of Souls' "spend this mana only to cast a creature spell of the
-- chosen type" is a predicate over spells, so payment must ask a classification
-- and must NEVER case on the restriction itself. Only the first is here
-- (Pawl.Types.ProductionTag); spending restrictions are #252.
--
-- Deliberately no source ObjectId. Snow cares about a PROPERTY of the source, not
-- its identity, and a reference would dangle by construction: mana outlives its
-- source, and CR 400.7 mints a fresh id on every zone change. Properties are
-- stamped at production time by Pawl.Engine.Mana.manaOptionsOfGiven, the one place
-- that knows both the source and the mana.
data ManaUnit = MkManaUnit
  { manaType :: ManaType.ManaType,
    tags :: Set.Set ProductionTag.ProductionTag
  }
  deriving (Eq, Ord, Show)
