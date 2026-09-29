{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Seat where

import qualified Data.Sequence as Seq
import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.Placement as Placement
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Seat as Seat

-- | Life defaults to CR 119.1's twenty.
codec :: Codec.Codec Seat.Seat
codec = Fields.object $ do
  name <- Fields.required "name" Label.codec Seat.name
  life <- Fields.defaulted "life" 20 Common.integer Seat.life
  battlefield <- Fields.defaulted "battlefield" Seq.empty (Common.seq Placement.codec) Seat.battlefield
  hand <- Fields.defaulted "hand" Seq.empty (Common.seq Placement.codec) Seat.hand
  graveyard <- Fields.defaulted "graveyard" Seq.empty (Common.seq Placement.codec) Seat.graveyard
  library <- Fields.defaulted "library" Seq.empty (Common.seq Placement.codec) Seat.library
  pure
    Seat.MkSeat
      { Seat.name = name,
        Seat.life = life,
        Seat.battlefield = battlefield,
        Seat.hand = hand,
        Seat.graveyard = graveyard,
        Seat.library = library
      }
