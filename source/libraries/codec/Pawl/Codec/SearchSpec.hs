module Pawl.Codec.SearchSpec where

import qualified Data.Set as Set
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
import qualified Pawl.Types.Zone as Zone

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
            Search.zones = Set.fromList [Zone.Library, Zone.Graveyard],
            Search.quantity = Just (Quantity.Literal 1),
            Search.filter = Filter.HasCardType CardType.Land,
            Search.upTo = True,
            Search.destination = SearchDestination.BattlefieldTapped
          }
      )
      " {\"searcher\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"owner\":{\"type\":\"InSlot\",\"value\":\"player\"},\"zones\":[{\"type\":\"Library\"},{\"type\":\"Graveyard\"}],\"quantity\":{\"type\":\"Literal\",\"value\":1},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"upTo\":true,\"destination\":{\"type\":\"BattlefieldTapped\"}} "
  -- The other reading of the same count: no "upTo" key means the quantity is a
  -- quota. Paired with the case above so each key's absence is asserted, not
  -- just its presence -- a required key would have made every card file
  -- rewrite. "zones" is the same shape: absent means the library alone, which is
  -- what every card file written before Delivery Moogle says.
  Spec.it s "an absent upTo and an absent zones take their defaults and are not written back" $
    Common.assertCodec
      s
      Search.codec
      ( Search.MkSearch
          { Search.searcher = PlayerRef.Relative PlayerRelation.You,
            Search.owner = PlayerRef.Relative PlayerRelation.You,
            Search.zones = Set.singleton Zone.Library,
            Search.quantity = Just (Quantity.Literal 1),
            Search.filter = Filter.HasCardType CardType.Land,
            Search.upTo = False,
            Search.destination = SearchDestination.BattlefieldTapped
          }
      )
      " {\"searcher\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"owner\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"destination\":{\"type\":\"BattlefieldTapped\"}} "
  -- CR 701.23a's other reading of the count: Mana Severance's "any number of
  -- land cards" states none at all, which is a null on the wire rather than an
  -- absent key. Round-tripped explicitly because nothing else forces a case for
  -- it.
  Spec.it s "a null quantity is \"any number of\", and round-trips as null" $
    Common.assertCodec
      s
      Search.codec
      ( Search.MkSearch
          { Search.searcher = PlayerRef.Relative PlayerRelation.You,
            Search.owner = PlayerRef.Relative PlayerRelation.You,
            Search.zones = Set.singleton Zone.Library,
            Search.quantity = Nothing,
            Search.filter = Filter.HasCardType CardType.Land,
            Search.upTo = False,
            Search.destination = SearchDestination.Exile
          }
      )
      " {\"searcher\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"owner\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":null,\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"destination\":{\"type\":\"Exile\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Search.codec
