module Pawl.Types.Source where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Types.MeldSource as MeldSource
import qualified Pawl.Types.MergeComponent as MergeComponent
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
  | -- | CR 730.2: a merged permanent -- "represented by the card or copy that
    -- represented that object in addition to any other components that were
    -- representing it". The components in TOP-TO-BOTTOM order, so the head is
    -- what CR 730.2a's "only the characteristics of its topmost component"
    -- names and every characteristic read resolves through.
    --
    -- ITS OWN CONSTRUCTOR rather than OfMeld, whose payload answers a different
    -- question: a melded permanent reads an interned combined face that is no
    -- component of it (CR 712.8g) and sums its components' front faces for the
    -- mana value (CR 202.3c), where a merged permanent reads one component and
    -- CR 730.2a gives it that component's mana value like every other
    -- characteristic. The two share only what Pawl.Engine.Game.componentsOf
    -- answers -- CR 730.3 restates CR 712.21 -- which is exactly what that
    -- classifier is for.
    --
    -- Still a card for CR 108.2's purposes when its topmost component is one,
    -- OfMeld's reason, and a token when that component is one: rule 730.2d says
    -- which, and Pawl.Engine.Game.sourceIsToken reads the head for it.
    --
    -- CR 730.2e gives the permanent its topmost component's STATUS, which is
    -- Object.facing and not a field here: every component of a pawl permanent
    -- shares that one status, which is CR 730.2f's "each face-up component that
    -- represents it is turned face down" said once.
    --
    -- CR 730.2h's flip components are Object.flipped and not a field here
    -- either: what the flip reaches is decided at the merge, which stamps a
    -- second reading of it beside the first (Pawl.Engine.Binding.setMergeCopy).
    --
    -- CR 730.2i's double-faced component is Object.face, and the merge stamps a
    -- third reading for it (Pawl.Engine.Binding.turnMergeCopy).
    --
    -- Not implemented: CR 730.2g's instant or sorcery component, which keeps a
    -- face-down merged permanent from turning face up (#3392).
    OfMerge (NonEmpty.NonEmpty MergeComponent.MergeComponent)
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
  | -- | CR 707.10a's other copy: a copy of a CARD, which CR 722.3c is the only
    -- rule in pawl that mints -- "its controller creates a copy of that object in
    -- exile, except that copy has only the characteristics of that permanent's
    -- prepare spell". Names an interned printing holding those characteristics as
    -- its one face, that rule's "those characteristics become the copy's normal
    -- characteristics" being why the printing is a normal one-faced card rather
    -- than the preparation card again.
    --
    -- ITS OWN CONSTRUCTOR and not OfSpellCopy, though the two share a printing
    -- payload and a projection road, because CR 704.5e states them as two
    -- sentences with different zones: a copy of a SPELL ceases to exist outside
    -- the stack, where a copy of a CARD survives on the battlefield too -- so a
    -- copy of a card that resolves as a permanent stays, where CR 707.10f turns a
    -- copy of a permanent spell into a token. Not OfCard either, which would make
    -- the cast copy a card in a graveyard for ever.
    --
    -- CR 722.3c's exception to rule 704.5e is what keeps one in EXILE, and
    -- Pawl.Engine.Sba reads Object.preparedCopyOf for it.
    OfCardCopy PrintingId.PrintingId
  | -- | CR 725.2 / CR 702.179d: a triggered ability with no object source, which
    -- Pawl.Types.InherentTriggerSource carries and documents.
    OfInherentTrigger InherentTriggerSource.InherentTriggerSource
  deriving (Eq, Ord, Show)
