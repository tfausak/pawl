module Pawl.Types.Conjure where

import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.Quantity as Quantity

-- | Alchemy\'s conjure keyword action: create a card that was in nobody\'s deck
-- and put it into a zone.
--
-- Digital-only, so there is no rule to cite: @docs\/rules.txt@ contains no
-- "conjure", and the authority is Arena\'s own text -- "conjure a card named
-- Ornithopter into your hand". What the CR does settle is what the result is
-- NOT: it is no token (CR 111.1's are created by an effect and are not cards),
-- so the conjured object is an ordinary card, castable and shufflable, and
-- 'Pawl.Types.Source.OfCard' is what backs it.
--
-- The card is carried INLINE, 'Pawl.Types.Meld.Meld''s reason spelled out
-- there: no @Pawl.Engine@ module imports @Pawl.Registry@ and
-- 'Pawl.Types.GameState.GameState' holds no name-keyed map, so an opcode naming
-- its card by name would have nothing to resolve the name against. The printed
-- sentence names the card ("a card named Ornithopter"); pawl\'s card file
-- writes that card out.
--
-- Parametric in @card@ for 'Pawl.Types.Effect.Effect''s reason: the conjured
-- card is card DATA nested inside card data, and the parameter is what keeps
-- 'Pawl.Types.Effect' from naming a concrete card type.
--
-- Not implemented: a conjure whose card is not NAMED outright. A duplicate of an
-- object already in the game (Futurist Spellthief\'s "conjure a duplicate of
-- target spell into your hand") wants an
-- 'Pawl.Types.ObjectRef.ObjectRef' where this field holds text (#2643), and a
-- random pick from a printed spellbook (Tome of the Infinite\'s "conjure a random
-- card from Tome of the Infinite\'s spellbook") wants a list plus a choice the
-- engine makes (#3062). Neither is a card this field can hold.
--
-- Not implemented: a conjurer other than the resolving controller. That is a
-- SHAPE and not one card -- a chosen player (Juggernaut Peddler\'s "that player
-- exiles it and conjures a card named Juggernaut into their hand") and the
-- controller of another object (Thendar, the Overminer\'s "its controller
-- conjures a card named Wastes onto the battlefield tapped") are both printed --
-- so a 'Pawl.Types.PlayerRef.PlayerRef' is what this would carry, the field
-- 'Pawl.Types.Create.Create' already has. Also not implemented: a slot binding
-- the conjured card for a later clause to name (Kari Zev, Crew of Two\'s "if
-- that card is on the battlefield, return it to its owner\'s hand") (#2638).
data Conjure card = MkConjure
  { -- | How many copies of the card. Toralf\'s Disciple\'s "conjure four cards
    -- named Lightning Bolt"; a printed "a card" is one.
    quantity :: Quantity.Quantity,
    -- | The card conjured, written out in full.
    card :: card,
    -- | The zone it arrives in.
    destination :: ConjureDestination.ConjureDestination
  }
  deriving (Eq, Ord, Show)

-- | What a card conjuring one card writes, and the value the codec elides.
defaultQuantity :: Quantity.Quantity
defaultQuantity = Quantity.Literal 1
