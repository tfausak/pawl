module Pawl.Types.ConjureCards where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.FromReference as FromReference
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | WHICH CARD an Alchemy conjure creates -- the half of the sentence that says
-- what is conjured, as 'Pawl.Types.ConjureDestination' is the half that says
-- where it goes.
--
-- Conjure is a DIGITAL-ONLY keyword action and is in no rule of the CR --
-- @docs\/rules.txt@ does not contain the word -- so the authority for this type
-- is the printed sentence. Three of them are printed and they ask different
-- questions: "conjure a card named Ornithopter" names a card outright,
-- "conjure a duplicate of target spell" names an OBJECT ALREADY IN THE GAME and
-- takes its card from there, and "conjure a random creature card with mana value
-- X" names neither and picks from the Oracle card reference.
--
-- A sum rather than optional fields on 'Pawl.Types.Conjure.Conjure': no
-- printing says two of them, and 'Pawl.Types.Conjure.selection' is a question
-- only the written and reference roads ask.
data ConjureCards card
  = -- | The candidates, each written out in full in the card file. ONE is a card
    -- the sentence names outright (Emporium Thopterist\'s "a card named
    -- Ornithopter"); more than one is the printed spellbook the sentence names
    -- instead (Tome of the Infinite\'s "a random card from Tome of the
    -- Infinite\'s spellbook"), and the conjure picks one of them,
    -- 'Pawl.Types.Conjure.selection' saying how. One list rather than a card and
    -- an optional spellbook beside it: a named card is the pick over a
    -- one-candidate pool, which no board can tell from taking it outright, and a
    -- prompt is raised only for two or more.
    Written (NonEmpty.NonEmpty card)
  | -- | A duplicate of each object the ref names (Sinister Reflections\'s
    -- "conjure a duplicate of each of up to two target nontoken creatures you
    -- control into your hand").
    --
    -- The card conjured is minted from the named object\'s COPIABLE values,
    -- @docs\/design.md@ section 2.8\'s settlement, so a Clone that is a copy of
    -- an Ornithopter duplicates as an Ornithopter rather than as a Clone. That
    -- is what keeps the duplicate from being a second name for
    -- 'Pawl.Types.Effect.CreateCopy': that opcode mints a TOKEN whose copiable
    -- values point at an original still on the battlefield, where this mints a
    -- CARD that outlives the original.
    --
    -- 'Pawl.Types.Conjure.quantity' still counts: Gyox, Brutal Carnivora\'s
    -- "conjure X duplicates of it into exile" names one object and makes X
    -- cards.
    Duplicate ObjectRef.ObjectRef
  | -- | A card of the Oracle card reference (CR 108.1) the filter admits (Fear
    -- of Change\'s "conjure a duplicate of a random creature card with mana value
    -- X"). A "duplicate" of a card outside the game is that card: nothing of the
    -- game\'s is on it to copy.
    Reference FromReference.FromReference
  deriving (Eq, Ord, Show)

-- | The candidates written out in the card file, and none for a duplicate, whose
-- card is read off the board, or for a reference pick, whose card is not card
-- data at all. The channel every walk over an opcode\'s NESTED
-- CARD DATA needs -- the round trip, CR 612.1\'s text change, and the lints that
-- ask which faces a card mints -- and a plain list rather than a derived
-- 'Foldable', which Haskell2010 has no deriving clause for.
written :: ConjureCards card -> [card]
written cards = case cards of
  Written xs -> NonEmpty.toList xs
  Duplicate _ -> []
  Reference _ -> []
