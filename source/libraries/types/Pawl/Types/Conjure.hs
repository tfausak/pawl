module Pawl.Types.Conjure where

import qualified Pawl.Types.ConjureCards as ConjureCards
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.ConjureSelection as ConjureSelection
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

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
-- A card the sentence NAMES is carried INLINE, 'Pawl.Types.Meld.Meld''s reason
-- spelled out there: no @Pawl.Engine@ module imports @Pawl.Registry@ and
-- 'Pawl.Types.GameState.GameState' holds no name-keyed map, so an opcode naming
-- its card by name would have nothing to resolve the name against. The printed
-- sentence names the card ("a card named Ornithopter"); pawl\'s card file
-- writes that card out. A DUPLICATE names no card at all --
-- 'Pawl.Types.ConjureCards.Duplicate' names an object already in the game and
-- the resolver reads the card off it -- so that road needs no registry either.
-- A REFERENCE pick (Fear of Change\'s "a random creature card with mana value
-- X") asks the interpreter, which holds the registry, for the cards a filter
-- admits ('Pawl.Types.Prompt.ReferenceCards').
--
-- Parametric in @card@ for 'Pawl.Types.Effect.Effect''s reason: the conjured
-- card is card DATA nested inside card data, and the parameter is what keeps
-- 'Pawl.Types.Effect' from naming a concrete card type.
--
-- Not implemented: a conjurer other than the resolving controller. That is a
-- SHAPE and not one card -- a chosen player (Juggernaut Peddler\'s "that player
-- exiles it and conjures a card named Juggernaut into their hand") and the
-- controller of another object (Thendar, the Overminer\'s "its controller
-- conjures a card named Wastes onto the battlefield tapped") are both printed --
-- so a 'Pawl.Types.PlayerRef.PlayerRef' is what this would carry, the field
-- 'Pawl.Types.Create.Create' already has (#3970).
data Conjure card = MkConjure
  { -- | How many copies of the card. Toralf\'s Disciple\'s "conjure four cards
    -- named Lightning Bolt"; a printed "a card" is one.
    quantity :: Quantity.Quantity,
    -- | WHAT is conjured -- a list written out in the card file, or a duplicate
    -- of an object already in the game.
    cards :: ConjureCards.ConjureCards card,
    -- | Which question the sentence asks of the candidates above -- "a random
    -- card" or "a card of your choice". Unobservable on a one-candidate list
    -- and on a duplicate, where neither question is asked.
    selection :: ConjureSelection.ConjureSelection,
    -- | The zone it arrives in.
    destination :: ConjureDestination.ConjureDestination,
    -- | The slot the conjured cards are bound to, for a later clause to name --
    -- Kari Zev, Crew of Two\'s "if that card is on the battlefield, return it
    -- to its owner\'s hand" (CR 603.7c). 'Pawl.Types.Create.slot''s shape.
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)

-- | What a card conjuring one card writes, and the value the codec elides.
defaultQuantity :: Quantity.Quantity
defaultQuantity = Quantity.Literal 1

-- | What a card naming one card outright writes, and the value the codec
-- elides. Randomness rather than choice: the printings that say neither are the
-- one-candidate ones, where the two are indistinguishable.
defaultSelection :: ConjureSelection.ConjureSelection
defaultSelection = ConjureSelection.AtRandom
