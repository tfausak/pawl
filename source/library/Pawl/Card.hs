-- Card-agnostic classifications only (CR 110.1 / 305.6, the closed half). The
-- ~30 hand-written card values moved out to the test suite's Pawl.Cards at M3.5:
-- the engine library can no longer name a card, so §1's invariant (the closed
-- half never depends on a card's identity) is enforced by the module graph.
module Pawl.Card where

import qualified Data.Set as Set
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.TypeLine as TypeLine

isLand :: Card.Card -> Bool
isLand c = Set.member CardType.Land (TypeLine.types (Card.typeLine c))

isCreature :: Card.Card -> Bool
isCreature c = Set.member CardType.Creature (TypeLine.types (Card.typeLine c))

-- CR 304.1: an instant is castable whenever its controller has priority. The
-- timing classification, shaped like isPermanent.
isInstant :: Card.Card -> Bool
isInstant c = Set.member CardType.Instant (TypeLine.types (Card.typeLine c))

-- CR 110.1: the permanent card types. An enumeration -- closed half, finite.
isPermanentType :: CardType.CardType -> Bool
isPermanentType cardType = case cardType of
  CardType.Land -> True
  CardType.Creature -> True
  CardType.Instant -> False
  CardType.Enchantment -> True
  CardType.Artifact -> True

-- The classification resolution dispatches on (CR 608.3). This is the whole
-- reason the engine never needs to know WHICH card is resolving.
isPermanent :: Card.Card -> Bool
isPermanent c = any isPermanentType (Set.toList (TypeLine.types (Card.typeLine c)))
