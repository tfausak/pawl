module Pawl.Types.Source where

import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Types.MeldSource as MeldSource
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource

-- | What is behind an object. The card-shaped constructors name their printing
-- by id rather than carrying it (#1592), so the rules distinction between them
-- is the only distinction left: a card, a melded permanent, a token, an emblem
-- and a copy of a spell are different things under CR 108, CR 701.42a, CR 111,
-- CR 114 and CR 707.10, and the engine cases on which of them it has.
data Source
  = -- | CR 108: a card, named by its entry in GameState.printings.
    OfCard PrintingId.PrintingId
  | -- | CR 701.42a: a melded permanent -- "a single object represented by two
    -- cards", which Pawl.Types.MeldSource carries and documents. CR 712.8g gives
    -- it "only the characteristics of the combined back face", so the payload's
    -- result printing is what every characteristic read resolves through, and CR
    -- 202.3c reads its components' front faces for the mana value instead.
    --
    -- ITS OWN CONSTRUCTOR rather than OfCard, for the reason OfSpellCopy is not
    -- OfCard either: the arm has to be able to answer a question the payload of a
    -- bare printing cannot. CR 712.21 splits this object back into the cards
    -- representing it as it leaves the battlefield, and CR 701.27g excludes an
    -- object represented by more than one card from being a transformed
    -- permanent -- both of which ask WHICH cards, and OfCard names one printing
    -- that is not even a card in a deck.
    --
    -- Still a card for CR 108.2's purposes, so every classifier that sorts a
    -- source into card and not-a-card answers for this arm the way it answers for
    -- OfCard: CR 108.2b excludes tokens and nothing excludes a melded permanent,
    -- whose components are both Magic cards.
    OfMeld MeldSource.MeldSource
  | -- | CR 111.3/111.6: a token -- a permanent not represented by a card. Its
    -- characteristics ARE a Card (CR 111.3: effect-defined values are functionally
    -- equivalent to printed ones), interned like any other printing, and carrying
    -- no print-level data because a token is not a card (CR 111.6).
    OfToken PrintingId.PrintingId
  | -- | CR 602: an activated ability on the stack, which
    -- Pawl.Types.ActivatedAbilitySource carries and documents.
    OfAbility ActivatedAbilitySource.ActivatedAbilitySource
  | -- | CR 603.3: a triggered ability on the stack, which
    -- Pawl.Types.TriggeredAbilitySource carries and documents.
    OfTrigger TriggeredAbilitySource.TriggeredAbilitySource
  | -- | CR 114: an emblem -- an object in the command zone whose only
    -- characteristics are its abilities (CR 114.3). Its characteristics ARE a
    -- Card and are interned like a token's; unlike a token it is never a permanent
    -- (CR 114.5) and never on the battlefield. Owned and controlled by the player
    -- who created it (CR 114.2 / 109.4c).
    OfEmblem PrintingId.PrintingId
  | -- | CR 707.10 / 112.1a: a copy of a spell -- itself a spell, with no card
    -- associated with it. Names the copied spell's printing the way the three
    -- card-shaped constructors do, so every characteristic read resolves as
    -- theirs does; the copiable values themselves come from the snapshot
    -- Pawl.Engine.Binding.setCopy stamps, exactly as they do for a token copy.
    --
    -- ITS OWN CONSTRUCTOR, and neither of the two it resembles: OfCard would make
    -- CR 704.5e's "a copy of a spell in a zone other than the stack ceases to
    -- exist" unaskable, and OfToken is CR 111.3 token-ness, which a copy of a
    -- spell acquires only as CR 707.10f's permanent is put onto the battlefield
    -- (Pawl.Engine.Event's zone-change funnel rewrites it there).
    OfSpellCopy PrintingId.PrintingId
  | -- | CR 725.2 / CR 702.179d: a triggered ability with no object source, which
    -- Pawl.Types.InherentTriggerSource carries and documents.
    OfInherentTrigger InherentTriggerSource.InherentTriggerSource
  deriving (Eq, Ord, Show)
