{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Seat where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.Placement as Placement
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.TeamId as TeamId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Seat as Seat

-- | Life defaults to CR 119.1's twenty.
codec :: Codec.Codec Seat.Seat
codec = Fields.object $ do
  name <- Fields.required "name" Label.codec Seat.name
  life <- Fields.defaulted "life" 20 Common.integer Seat.life
  counters <- Fields.defaulted "counters" Map.empty (Common.multiset PlayerCounterKind.codec) Seat.counters
  team <- Fields.defaulted "team" Nothing (Common.maybe TeamId.codec) Seat.team
  range <- Fields.defaulted "range" Nothing (Common.maybe Common.natural) Seat.range
  emperor <- Fields.defaulted "emperor" False Common.boolean Seat.emperor
  battlefield <- Fields.defaulted "battlefield" Seq.empty (Common.seq Placement.codec) Seat.battlefield
  hand <- Fields.defaulted "hand" Seq.empty (Common.seq Placement.codec) Seat.hand
  graveyard <- Fields.defaulted "graveyard" Seq.empty (Common.seq Placement.codec) Seat.graveyard
  library <- Fields.defaulted "library" Seq.empty (Common.seq Placement.codec) Seat.library
  exile <- Fields.defaulted "exile" Seq.empty (Common.seq Placement.codec) Seat.exile
  pure
    Seat.MkSeat
      { Seat.name = name,
        Seat.life = life,
        Seat.counters = counters,
        Seat.team = team,
        Seat.range = range,
        Seat.emperor = emperor,
        Seat.battlefield = battlefield,
        Seat.hand = hand,
        Seat.graveyard = graveyard,
        Seat.library = library,
        Seat.exile = exile
      }
