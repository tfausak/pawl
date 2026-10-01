{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LibraryArrival where

import qualified Pawl.Codec.LibraryPosition as LibraryPosition
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LibraryArrival as LibraryArrival

codec :: Codec.Codec LibraryArrival.LibraryArrival
codec = Fields.object $ do
  owner <- Fields.required "owner" PlayerId.codec LibraryArrival.owner
  position <- Fields.required "position" LibraryPosition.codec LibraryArrival.position
  cards <- Fields.required "cards" (Common.seq ObjectId.codec) LibraryArrival.cards
  pure
    LibraryArrival.MkLibraryArrival
      { LibraryArrival.owner = owner,
        LibraryArrival.position = position,
        LibraryArrival.cards = cards
      }
