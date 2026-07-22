module Pawl.Quantity where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Game as Game
import qualified Pawl.Type.Card as Card
import Pawl.Type.CardType (CardType)
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CountSpec as CountSpec
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.ProjectedCharacteristics as PC
import Pawl.Type.Quantity (Quantity)
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange

-- Nothing when the value cannot be determined.
--
-- `you` is the player a player-scoped count is relative to (CR 608.2h). The
-- caller supplies it because this module cannot ask the projection who controls
-- what: Pawl.Projection imports Pawl.Quantity, so the arrow only points one way.
-- Resolve passes the resolving spell's controller; Projection passes the
-- object's own controller.
evaluate :: GameState -> ObjectId -> Maybe PlayerId -> Quantity -> Maybe Integer
evaluate gs oid you quantity = case quantity of
  Quantity.Literal n -> Just n
  Quantity.ManaValue -> fmap manaValueOf (Game.cardOf oid gs)
  -- CR 601.2b: read the chosen X from the source object's binding environment.
  Quantity.X -> case Game.lookupObject oid gs of
    Nothing -> Nothing
    Just obj -> fmap toInteger (Binding.amountOf Binding.variableX (Object.bindings obj))
  -- CR 208.2: a bare star has no value of its own. The projection substitutes
  -- the object's characteristic-defining quantity for it at the seed
  -- (Projection.baseCharacteristics), so reaching this arm means the star was
  -- never resolved -- honestly Nothing, not a hole.
  Quantity.Star -> Nothing
  Quantity.Plus a b -> case (evaluate gs oid you a, evaluate gs oid you b) of
    (Just x, Just y) -> Just (x + y)
    _ -> Nothing
  Quantity.Count spec -> countOf gs you spec

-- The one place a CountSpec is interpreted.
countOf :: GameState -> Maybe PlayerId -> CountSpec.CountSpec -> Maybe Integer
countOf gs you spec = case spec of
  -- CR 208.2a: Tarmogoyf counts card TYPES, so this is the size of the union,
  -- not the number of cards.
  CountSpec.CardTypesInAllGraveyards -> Just (toInteger (Set.size (typesInAllGraveyards gs)))
  CountSpec.CardsInYourHand -> case you of
    Nothing -> Nothing
    Just pid -> Just (toInteger (length (Game.zoneMembers Zone.Hand pid gs)))
  -- CR 700.4: "dies" means put into a graveyard FROM THE BATTLEFIELD.
  CountSpec.CreaturesDiedThisTurn ->
    Just (toInteger (length (filter died (Foldable.toList (GameState.events gs)))))

-- The distinct card types among the cards in every player's graveyard. Reads the
-- PRINTED type line (Game.cardOf), never the projection: nothing projects a
-- graveyard card today, and a projected read here would recurse into the layer
-- fold that calls this (#41).
typesInAllGraveyards :: GameState -> Set CardType
typesInAllGraveyards gs =
  let ids = concatMap Foldable.toList (Map.elems (GameState.graveyard gs))
      typesOf oid = case Game.cardOf oid gs of
        Nothing -> Set.empty
        Just card -> TypeLine.types (Card.typeLine card)
   in Set.unions (map typesOf ids)

-- CR 700.4 / 608.2h: did this event record a creature dying? Creature-ness comes
-- from the event's own snapshot -- the object as it last existed on the
-- battlefield -- so a land animated into a creature counts, and so does a token
-- (which has no printed card to consult, CR 111.1).
died :: GameEvent.GameEvent -> Bool
died event = case event of
  GameEvent.Moved zc snapshot ->
    ZoneChange.from zc == Zone.Battlefield
      && ZoneChange.to zc == Zone.Graveyard
      && Set.member CardType.Creature (PC.cardTypes snapshot)
  GameEvent.DamageDealt _ -> False
  GameEvent.StepBegan _ _ -> False
  GameEvent.SpellCast _ -> False

-- CR 208.2: resolve a printed star to the quantity a characteristic-defining
-- ability supplies, recursing through Plus so 1+* becomes 1+<the count>.
substituteStar :: Quantity -> Quantity -> Quantity
substituteStar star quantity = case quantity of
  Quantity.Star -> star
  Quantity.Plus a b -> Quantity.Plus (substituteStar star a) (substituteStar star b)
  Quantity.Literal _ -> quantity
  Quantity.ManaValue -> quantity
  Quantity.X -> quantity
  Quantity.Count _ -> quantity

-- CR 202.3: the mana value is the total amount of mana in the cost -- each
-- generic symbol contributes its number, each colored/typed symbol contributes
-- one. A land has no mana cost (CR 202.1), so its mana value is 0.
manaValueOf :: Card.Card -> Integer
manaValueOf card = case Card.manaCost card of
  Nothing -> 0
  Just (ManaCost.MkManaCost symbols) -> sum (map symbolValue symbols)

symbolValue :: ManaSymbol.ManaSymbol -> Integer
symbolValue symbol = case symbol of
  ManaSymbol.Generic n -> toInteger n
  ManaSymbol.OfType _ -> 1
  -- CR 202.3b: off the stack a variable's contribution to mana value is 0.
  ManaSymbol.Variable -> 0
