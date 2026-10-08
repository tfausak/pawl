{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Seat where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.Placement as Placement
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.TeamId as TeamId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Seat as Seat

-- | Life defaults to CR 119.1's twenty, and the mana pool to empty.
codec :: Codec.Codec Seat.Seat
codec = Fields.object $ do
  name <- Fields.required "name" Label.codec Seat.name
  life <- Fields.defaulted "life" 20 Common.integer Seat.life
  counters <- Fields.defaulted "counters" Map.empty (Common.multiset PlayerCounterKind.codec) Seat.counters
  team <- Fields.defaulted "team" Nothing (Common.maybe TeamId.codec) Seat.team
  range <- Fields.defaulted "range" Nothing (Common.maybe Common.natural) Seat.range
  emperor <- Fields.defaulted "emperor" False Common.boolean Seat.emperor
  manaPool <- Fields.defaulted "manaPool" [] symbols Seat.manaPool
  battlefield <- Fields.defaulted "battlefield" Seq.empty (Common.seq Placement.codec) Seat.battlefield
  hand <- Fields.defaulted "hand" Seq.empty (Common.seq Placement.codec) Seat.hand
  graveyard <- Fields.defaulted "graveyard" Seq.empty (Common.seq Placement.codec) Seat.graveyard
  library <- Fields.defaulted "library" Seq.empty (Common.seq Placement.codec) Seat.library
  exile <- Fields.defaulted "exile" Seq.empty (Common.seq Placement.codec) Seat.exile
  command <- Fields.defaulted "command" Seq.empty (Common.seq Placement.codec) Seat.command
  ante <- Fields.defaulted "ante" Seq.empty (Common.seq Placement.codec) Seat.ante
  pure
    Seat.MkSeat
      { Seat.name = name,
        Seat.life = life,
        Seat.counters = counters,
        Seat.team = team,
        Seat.range = range,
        Seat.emperor = emperor,
        Seat.manaPool = manaPool,
        Seat.battlefield = battlefield,
        Seat.hand = hand,
        Seat.graveyard = graveyard,
        Seat.library = library,
        Seat.exile = exile,
        Seat.command = command,
        Seat.ante = ante
      }

-- | A pool as one letter per unit, CR 105.1's W, U, B, R and G and CR 106.1b's
-- C: @"RRC"@.
symbols :: Codec.Codec [ManaType.ManaType]
symbols =
  Common.scalar
    Schema.string
    (Codec.encode Common.text . Text.pack . fmap letterOf)
    (Codec.decode Common.text Monad.>=> (traverse typeOf . Text.unpack))
  where
    letterOf manaType = case manaType of
      ManaType.Colored Color.White -> 'W'
      ManaType.Colored Color.Blue -> 'U'
      ManaType.Colored Color.Black -> 'B'
      ManaType.Colored Color.Red -> 'R'
      ManaType.Colored Color.Green -> 'G'
      ManaType.Colorless -> 'C'
    typeOf letter = case letter of
      'W' -> Right (ManaType.Colored Color.White)
      'U' -> Right (ManaType.Colored Color.Blue)
      'B' -> Right (ManaType.Colored Color.Black)
      'R' -> Right (ManaType.Colored Color.Red)
      'G' -> Right (ManaType.Colored Color.Green)
      'C' -> Right ManaType.Colorless
      _ -> Left (Text.pack ("not a mana symbol: " <> [letter]))
