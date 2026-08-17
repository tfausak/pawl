{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SearchSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Search as Search
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Search" $ do
  -- CR 701.23. Extract's shape, which is the one that needs the keys: the
  -- CONTROLLER looks and the TARGET owns the library, so the two PlayerRefs
  -- differ. Every other producer in the pool says the same ref twice, and a
  -- codec that swapped them would round-trip those unnoticed.
  Spec.it s "MkSearch, every key" $
    Common.assertCodec
      s
      Search.codec
      ( Search.MkSearch
          { Search.searcher = PlayerRef.Relative PlayerRelation.You,
            Search.owner = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "player")),
            Search.quantity = Quantity.Literal 1,
            Search.filter = Filter.HasCardType CardType.Land,
            Search.upTo = True,
            Search.destination = SearchDestination.BattlefieldTapped
          }
      )
      """ {"searcher":{"type":"Relative","value":{"type":"You"}},"owner":{"type":"InSlot","value":"player"},"quantity":{"type":"Literal","value":1},"filter":{"type":"HasCardType","value":{"type":"Land"}},"upTo":true,"destination":{"type":"BattlefieldTapped"}} """
  -- The other reading of the same count: no "upTo" key means the quantity is a
  -- quota, which is what every search in the pool but Denying Wind's prints.
  Spec.it s "an absent upTo decodes as False and is not written back" $
    Common.assertCodec
      s
      Search.codec
      ( Search.MkSearch
          { Search.searcher = PlayerRef.Relative PlayerRelation.You,
            Search.owner = PlayerRef.Relative PlayerRelation.You,
            Search.quantity = Quantity.Literal 1,
            Search.filter = Filter.HasCardType CardType.Land,
            Search.upTo = False,
            Search.destination = SearchDestination.BattlefieldTapped
          }
      )
      """ {"searcher":{"type":"Relative","value":{"type":"You"}},"owner":{"type":"Relative","value":{"type":"You"}},"quantity":{"type":"Literal","value":1},"filter":{"type":"HasCardType","value":{"type":"Land"}},"destination":{"type":"BattlefieldTapped"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s Search.codec
