module Pawl.Types.RowSource where

import qualified Data.Set as Set
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 113.7: the source of a player-effect row, as
-- Pawl.Engine.PlayerEffect.applying hands it to every consumer.
--
-- Runtime-only and never written: no codec, because nothing stores one.
data RowSource = MkRowSource
  { -- | The object the row came from, Nothing for a row no object printed.
    object :: Maybe ObjectId.ObjectId,
    -- | CR 608.2h: a stored row's chosen names, baked as it began
    -- (Pawl.Types.ActivePlayerEffect.chosenNames). Nothing for a printed row,
    -- whose names CR 604.2 reads off the permanent live.
    chosenNames :: Maybe (Set.Set CardName.CardName)
  }
  deriving (Eq, Ord, Show)
