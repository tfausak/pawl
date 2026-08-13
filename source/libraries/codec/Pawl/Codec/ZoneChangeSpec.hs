{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ZoneChangeSpec where

import qualified Pawl.Codec.ZoneChange as ZoneChange
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ZoneChange" $ do
  -- CR 400.7's move, with all four keys DISTINCT on purpose: 'departed' and
  -- 'object' are both an ObjectId and 'from' and 'to' are both a Zone, so a
  -- fixture that repeated either pair would round-trip a codec that swapped
  -- them. CR 400.7 gives a moved object a new id, which is the case here.
  Spec.it s "MkZoneChange, a new id on arrival" $
    Common.assertCodec
      s
      ZoneChange.codec
      ( ZoneChange.MkZoneChange
          { ZoneChange.departed = ObjectId.MkObjectId 1,
            ZoneChange.object = ObjectId.MkObjectId 2,
            ZoneChange.from = Zone.Battlefield,
            ZoneChange.to = Zone.Graveyard
          }
      )
      """ {"departed":1,"object":2,"from":{"type":"Battlefield"},"to":{"type":"Graveyard"}} """
  -- The same id on both sides, which is what a library-to-graveyard mill writes
  -- (CR 113.6k): the shape the engine reads when nothing "became" anything.
  Spec.it s "MkZoneChange, the same id throughout" $
    Common.assertCodec
      s
      ZoneChange.codec
      ( ZoneChange.MkZoneChange
          { ZoneChange.departed = ObjectId.MkObjectId 3,
            ZoneChange.object = ObjectId.MkObjectId 3,
            ZoneChange.from = Zone.Library,
            ZoneChange.to = Zone.Graveyard
          }
      )
      """ {"departed":3,"object":3,"from":{"type":"Library"},"to":{"type":"Graveyard"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s ZoneChange.codec
