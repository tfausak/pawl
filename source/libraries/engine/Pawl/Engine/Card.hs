-- Card-agnostic classifications only (CR 110.1 / 305.6, the closed half). The
-- engine library cannot name a card -- the hand-written card values live in the
-- test suite's Pawl.Cards -- so design.md §1's invariant is enforced by the
-- module graph.
--
-- Also where a Card is resolved to the Face whose characteristics are live,
-- since CR 709.4 / 712.8a / 715.4 make that a question about the card's layout
-- and never about which card it is.
module Pawl.Engine.Card where

import Control.Applicative ((<|>))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import Pawl.Types.TargetSpec (TargetSpec)
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.TypeLine as TypeLine

-- The face a card shows where nothing has singled out one half for itself. WHICH
-- face that is, is exactly what the layout decides, and the three rules disagree:
-- CR 709.4 gives a split card its two halves COMBINED, CR 712.8a gives a
-- double-faced card its FRONT face alone, CR 715.4 gives an adventurer card its
-- NORMAL characteristics alone. Under Normal there is one face and all three name
-- it; the layouts that make them differ each have their arm below.
--
-- TOTAL, which is what Card.faces being NonEmpty buys: every characteristic read
-- in the engine funnels through here, and a Maybe would spread to all of them.
combined :: Card.Card -> Face.Face Card.Card
combined card = case Card.layout card of
  Layout.Normal -> NonEmpty.head (Card.faces card)
  -- CR 709.4: "In every zone except the stack, the characteristics of a split
  -- card are those of its two halves combined." Written over the whole
  -- NonEmpty rather than over a pair because docs/design.md §2.11 is a
  -- standing rule against baking arity into the card model: every
  -- tournament-legal split card has exactly two faces, but a fold over N costs
  -- nothing to write and so also covers Who // What // When // Where // Why, a
  -- five-part split card from a silver-border Un-set, entirely outside the CR.
  Layout.Split -> foldSplit (Card.faces card)
  -- CR 715.4: "In every zone except the stack, and while on the stack not as an
  -- Adventure, an adventurer card has only its normal characteristics." The
  -- alternative characteristics of CR 715.2 are reached ONLY through
  -- Object.face, which CR 715.3b writes for the stack incarnation and CR 715.3a's
  -- offer writes speculatively (Cast.asProposed) --
  -- so the face this returns is the normal one, and the same expression Normal
  -- takes is a different claim: that a card with TWO printed sets of
  -- characteristics shows one of them, rather than that a card with one shows
  -- it.
  Layout.Adventure -> NonEmpty.head (Card.faces card)
  -- CR 712.8a: "While a double-faced card is outside the game or in a zone other
  -- than the battlefield or stack, it has only the characteristics of its front
  -- face", and CR 712.8d says the same of a permanent showing that face. The
  -- front face is the first (Pawl.Types.Layout.Transforming), so this is
  -- Adventure's expression again over a different claim: what a nonmodal
  -- double-faced card shows where nothing has turned it over.
  --
  -- The BACK face is reached only through Object.face, which CR 701.27a writes.
  Layout.Transforming -> NonEmpty.head (Card.faces card)
  -- CR 712.8a again, whose scope is every double-faced card and not just the
  -- nonmodal one above: a modal double-faced card in a hand, a graveyard or a
  -- library has only its front face's characteristics. So this is one card that
  -- offers two casts (castableFaces below) while showing exactly one set of
  -- characteristics where it sits -- which is what makes it neither Split nor
  -- Adventure.
  --
  -- The BACK face is reached only through Object.face, which CR 712.11b's choice
  -- writes as the spell is put onto the stack and CR 712.13 carries onto the
  -- permanent (enteringFace below).
  Layout.ModalDoubleFaced -> NonEmpty.head (Card.faces card)

-- CR 709.4, one pair at a time. Left-associated over the NonEmpty, so printed
-- order decides the joined name and the concatenated mana cost.
foldSplit :: NonEmpty.NonEmpty (Face.Face Card.Card) -> Face.Face Card.Card
foldSplit faces = List.foldl' merge2 (NonEmpty.head faces) (NonEmpty.tail faces)

merge2 :: Face.Face Card.Card -> Face.Face Card.Card -> Face.Face Card.Card
merge2 l r =
  l
    { -- CR 709.4a gives the card BOTH names and no joined one; a single
      -- CardName cannot carry that, so this is the form docs/rules.txt's own
      -- Examples write, unspaced -- "Fire//Ice" (lines 3882, 5747) and
      -- "Assault//Battery" (line 5746). #650 carries the plural axis.
      Face.name = CardName.join (Face.name l NonEmpty.:| [Face.name r]),
      -- CR 709.4b: "the combined mana costs of its two halves", from which
      -- colours and mana value fall out with no further arm.
      Face.manaCost = concatCosts (Face.manaCost l) (Face.manaCost r),
      -- CR 709.4c: "each card type specified on either of its halves" -- see
      -- unionTypeLines for where the other two sets of the type line come from.
      Face.typeLine = unionTypeLines (Face.typeLine l) (Face.typeLine r),
      -- CR 709.4c again: a keyword is the NAME of an ability the object has (CR
      -- 702.1), so "each ability in the text box of each half" is what unions
      -- these.
      Face.keywords = Set.union (Face.keywords l) (Face.keywords r),
      -- CR 709.4: the colour indicator is a characteristic (CR 109.3), and no
      -- subrule narrows it the way 709.4b narrows the mana cost.
      Face.colorIndicator = Set.union (Face.colorIndicator l) (Face.colorIndicator r),
      -- CR 709.4c: "each ability in the text box of each half".
      Face.staticAbilities = Face.staticAbilities l <> Face.staticAbilities r,
      Face.activatedAbilities = Face.activatedAbilities l <> Face.activatedAbilities r,
      Face.replacementEffects = Face.replacementEffects l <> Face.replacementEffects r,
      Face.triggeredAbilities = Face.triggeredAbilities l <> Face.triggeredAbilities r,
      -- Left-biased: two halves that arm a delayed ability under the same
      -- AbilityName do not both survive here, and the right half's is the one
      -- lost (#652).
      Face.delayedAbilities = Map.union (Face.delayedAbilities l) (Face.delayedAbilities r),
      Face.castingPermissions = Face.castingPermissions l <> Face.castingPermissions r,
      Face.castingRestrictions = Face.castingRestrictions l <> Face.castingRestrictions r,
      Face.additionalCosts = Face.additionalCosts l <> Face.additionalCosts r,
      Face.alternativeCosts = Face.alternativeCosts l <> Face.alternativeCosts r,
      Face.playerAbilities = Face.playerAbilities l <> Face.playerAbilities r,
      Face.blockRequirements = Face.blockRequirements l <> Face.blockRequirements r,
      Face.attackRequirements = Face.attackRequirements l <> Face.attackRequirements r,
      Face.combatRestrictions = Face.combatRestrictions l <> Face.combatRestrictions r,
      Face.attackCosts = Face.attackCosts l <> Face.attackCosts r,
      Face.mulliganActions = Face.mulliganActions l <> Face.mulliganActions r,
      Face.openingHandActions = Face.openingHandActions l <> Face.openingHandActions r,
      -- CR 709.4c again, and CR 702.5a: an enchant ability IS an ability in a
      -- half's text box, so both halves' survive here -- and CR 702.5c says what
      -- a combined view carrying two of them means, which is Card.enchantSpec's
      -- conjunction. Concatenated rather than left-biased for that reason, unlike
      -- the four boxes below.
      Face.enchant = Face.enchant l <> Face.enchant r,
      -- The first half that has one. CR 709.4 does not say how two printed
      -- power/toughness/loyalty/defense boxes combine, and taking the left half's
      -- is not implemented as anything the rule sanctions (#658).
      Face.power = firstJust (Face.power l) (Face.power r),
      Face.toughness = firstJust (Face.toughness l) (Face.toughness r),
      Face.loyalty = firstJust (Face.loyalty l) (Face.loyalty r),
      Face.defense = firstJust (Face.defense l) (Face.defense r),
      Face.characteristicPT = firstJust (Face.characteristicPT l) (Face.characteristicPT r)
      -- Face.counterability is NOT listed: record update keeps the left half's,
      -- and writing `Face.counterability l` here would be a no-op. CR 113.6g is
      -- a per-half ability, so the combined view taking the left half's is a
      -- placeholder no split card exercises.
      --
      -- Face.spell is deliberately NOT merged either: it stays the left half's, and
      -- nothing ever casts it. CR 709.3b means the thing on the stack is always
      -- ONE half, so the combined view is never the payload that resolves --
      -- Task 4's castableFaces is what a cast reads. Merging the modes here
      -- would invent a spell that has no printing.
    }

-- CR 202.1b: "Some objects have no mana cost. This normally includes all land
-- cards." So Nothing is no mana cost at all, not a zero one, and two Nothings
-- stay Nothing. Either half present making the combined cost Just is this
-- function's own design choice, not something the rule states -- the symbol
-- lists are concatenated left to right, which is what CR 709.4b's "combined
-- mana costs" means for a pair of Maybes.
concatCosts :: Maybe ManaCost.ManaCost -> Maybe ManaCost.ManaCost -> Maybe ManaCost.ManaCost
concatCosts l r = case (l, r) of
  (Nothing, Nothing) -> Nothing
  _ -> Just (ManaCost.MkManaCost (costSymbols l <> costSymbols r))

-- concatCosts' shared halves: the symbol list of a printed cost, or none for a
-- face with no mana cost at all.
costSymbols :: Maybe ManaCost.ManaCost -> [ManaSymbol.ManaSymbol]
costSymbols = foldMap ManaCost.unwrap

-- CR 709.4c names the card types: "A split card has each card type specified on
-- either of its halves". The supertypes and subtypes come from CR 709.4 itself
-- -- "the characteristics of a split card are those of its two halves combined"
-- -- since a supertype or subtype is a characteristic (CR 109.3) and 709.4c does
-- not narrow the type line to its middle set.
unionTypeLines :: TypeLine.TypeLine -> TypeLine.TypeLine -> TypeLine.TypeLine
unionTypeLines l r =
  TypeLine.MkTypeLine
    { TypeLine.supertypes = Set.union (TypeLine.supertypes l) (TypeLine.supertypes r),
      TypeLine.types = Set.union (TypeLine.types l) (TypeLine.types r),
      TypeLine.subtypes = Set.union (TypeLine.subtypes l) (TypeLine.subtypes r)
    }

-- Maybe's Alternative instance, read left to right: the left half's value if
-- it has one, the right half's (Just or Nothing) otherwise.
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust l r = l <|> r

-- CR 709.3a: the faces a player may propose to cast. Under Normal the sole
-- face; under Split every half, because CR 709.3 says "A player chooses which
-- half of a split card they are casting before putting it onto the stack."
--
-- A LIST of options and never a choice: which half is cast is the player's, and
-- offering each as its own legal action is how the engine avoids making it.
--
-- `combined` is deliberately not among them. CR 709.4's view is what the card
-- has in every zone but the stack; CR 709.3a puts only the CHOSEN half on the
-- stack, so a cast is never priced or evaluated against the pair.
castableFaces :: Card.Card -> [Face.Face Card.Card]
castableFaces card = case Card.layout card of
  Layout.Normal -> [NonEmpty.head (Card.faces card)]
  Layout.Split -> NonEmpty.toList (Card.faces card)
  -- CR 715.3: "As a player plays an adventurer card, the player chooses whether
  -- they play the card normally or as an Adventure." Both, for Split's reason:
  -- the choice is the player's, and offering each half as its own legal action
  -- is how the engine avoids making it. WHICH of them a given zone allows is not
  -- this function's question -- CR 715.3d's exile permission excludes the
  -- Adventure half, and Pawl.Engine.Cast is where that is read.
  Layout.Adventure -> NonEmpty.toList (Card.faces card)
  -- CR 712.11: "A double-faced spell is cast with its front face up by default."
  -- ONE option, and no choice to leave the player: unlike Split's halves and
  -- Adventure's two ways to play, a nonmodal double-faced card's back face is not
  -- something a player may elect to cast. Only an effect allowing the card to be
  -- cast "transformed" or "converted" puts a back face on the stack (CR 712.8c /
  -- 712.11a), and pawl has none: the convert wording is #698, and the wordings
  -- that reach a back face without one on the STACK put the card onto the
  -- battlefield transformed instead -- CR 712.13a for a double-faced spell
  -- already on the stack, CR 712.14a for a card put there without being cast
  -- (#70).
  Layout.Transforming -> [NonEmpty.head (Card.faces card)]
  -- CR 712.11b: "A player casting a modal double-faced card or a copy of a modal
  -- double-faced card as a spell chooses which face they are casting before
  -- putting it onto the stack." EVERY face, for Split's and Adventure's reason:
  -- the choice is the player's, and offering each as its own legal action is how
  -- the engine avoids making it. CR 712.11c is what makes each offer stand on
  -- its own -- "Only the face that will be face up on the stack is evaluated to
  -- determine if it can be cast" -- which is exactly how Pawl.Engine.Cast prices
  -- and gates a proposal.
  --
  -- A LAND face is among them, and needs no arm of its own to stay uncastable:
  -- CR 202.1b gives it no mana cost, which CR 118.6 makes an unpayable one, so
  -- Pawl.Engine.Cost.canPay drops it at the affordability gate the way it drops
  -- any other land (see Cost.costsFor's own note). CR 712.12's other half -- a
  -- player PLAYING such a face as a land -- is landFaces below.
  Layout.ModalDoubleFaced -> NonEmpty.toList (Card.faces card)

-- CR 305.1 / 712.12: the faces of this card a player may PLAY as a land, each
-- paired with the name the play carries -- the castableFaces of the special
-- action rather than of the cast, and the same shape for the same reason. A
-- LIST of options and never a choice: CR 712.12 has the player choose "one of
-- its faces that's a land", and offering each as its own legal action is how
-- the engine avoids choosing for them.
--
-- The pairing is with a MAYBE name because the two halves of the answer are not
-- the same kind of thing. A chosen face is named, which is what CR 712.12's
-- "with that face up" needs written onto the permanent; the default view is not
-- a chosen face at all, and for CR 709.4's split card it is not any single
-- face's name either. See Pawl.Types.Action's Play.
--
-- FILTERED to lands here rather than by the caller, so CR 305.1's "land card"
-- and CR 712.12's "faces that's a land" are asked once. What is NOT asked here
-- is anything about the player or the board -- prohibitions and CR 305.2's
-- allowance are Pawl.Engine.Action's, which is what keeps this a fact about a
-- card.
landFaces :: Card.Card -> [(Maybe CardName.CardName, Face.Face Card.Card)]
landFaces card =
  let -- CR 712.8a's default view, which every layout but the modal one plays as:
      -- a card with one face plays as that face, and CR 709.4's split card has no
      -- printing whose halves are lands, so nothing here can tell the combined
      -- view from a chosen half.
      byDefault = [(Nothing, combined card)]
      named face = (Just (Face.name face), face)
      offered = case Card.layout card of
        Layout.Normal -> byDefault
        Layout.Split -> byDefault
        Layout.Adventure -> byDefault
        -- CR 712.8a again, and CR 712.12 names the MODAL kind alone: a nonmodal
        -- double-faced card in a hand is only its front face, so a back face that
        -- is a land is not something its controller may elect to play. Westvale
        -- Abbey // Ormendahl, Profane Prince is the shape that makes the arm say
        -- something -- a land front over a creature back, played as the front
        -- face like any other land.
        Layout.Transforming -> byDefault
        Layout.ModalDoubleFaced -> fmap named (NonEmpty.toList (Card.faces card))
   in filter (isLand . snd) offered

-- CR 712.14b: "If a player is instructed to put a modal double-faced card onto
-- the battlefield and its front face isn't a permanent card, the card stays in
-- its current zone." True when this card is one the instruction does nothing to.
--
-- A layout classification, like every other function here, and NOT a question
-- about the effect doing the instructing: the rule turns on the card's front
-- face alone. Pawl.Engine.Event.changeZoneEntering is the one door it gates,
-- since that is the door an effect that puts an object onto the battlefield goes
-- through. CR 712.12's land play deliberately does not: playing a land is a
-- special action the player takes (CR 305.1), not an instruction to put a card
-- onto the battlefield, and CR 712.12 permits it in as many words.
--
-- False for every other layout, and that is the rule rather than a gap. CR
-- 712.14b names the modal double-faced card and nothing else; a nonmodal one is
-- covered by CR 712.14's default (front face up) and by CR 712.14a's
-- "transformed" wording, which pawl has no producer for (#70).
staysWhenPutOntoBattlefield :: Card.Card -> Bool
staysWhenPutOntoBattlefield card = case Card.layout card of
  Layout.Normal -> False
  Layout.Split -> False
  Layout.Adventure -> False
  Layout.Transforming -> False
  Layout.ModalDoubleFaced -> not (isPermanent (NonEmpty.head (Card.faces card)))

-- CR 701.27a: "To transform a permanent, turn it over so that its other face is
-- up." WHICH face that leaves up, by NAME -- the form Object.face stores, and
-- the whole of what a transform writes.
--
-- Takes the face the permanent shows now (Nothing being its front face, CR
-- 712.8a) and answers the one it would show after. Nothing wherever the rules
-- say nothing happens instead:
--
--   * CR 701.27c / 712.9: "If a spell or ability instructs a player to transform
--     ... any permanent that isn't represented by a double-faced token or a
--     double-faced card, nothing happens" -- every layout that is not a
--     double-faced card, which here is every one but Transforming and
--     ModalDoubleFaced. CR 712.9's first sentence is what puts the modal kind on
--     the permitted side: "Only permanents represented by double-faced tokens and
--     double-faced cards that are not meld cards can transform or convert", and
--     CR 712.3 says the same from the card's side ("they may have an ability that
--     allows them to 'transform' or 'convert' on either face"). A Clone that
--     copied a double-faced permanent is one of the forbidden ones (CR 712.9's own
--     first Example), and falls out of the layout being read off the object's
--     stored card rather than off the projection.
--   * CR 701.27d / 712.10: "the face that permanent would transform into is an
--     instant or sorcery face" -- read off the face this would land on. No card
--     in the pool has such a back face, so that guard is written for the cost of
--     one type-line read rather than exercised.
--
-- Not read here, and the reason this answers a NAME rather than performing the
-- turn: CR 701.27f ignores the instruction outright when the permanent has
-- already turned over since the asking ability was put on the stack, which is a
-- question about two OBJECTS rather than about a card. Pawl.Engine.Resolve's
-- alreadyTurnedFor asks it, and a Just from here is still only a candidate.
--
-- The SUCCESSOR in printed order, wrapping -- "its other face" for the two faces
-- CR 712.1 gives a double-faced card, and a rotation for any longer list, which
-- is docs/design.md section 2.11's standing rule against baking arity into the
-- card model. A one-faced card so labelled rotates back onto itself, which is
-- the same permanent it already was.
turnedOver :: Maybe CardName.CardName -> Card.Card -> Maybe CardName.CardName
turnedOver mName card = case Card.layout card of
  Layout.Normal -> Nothing
  Layout.Split -> Nothing
  Layout.Adventure -> Nothing
  Layout.Transforming -> nextFace mName card
  Layout.ModalDoubleFaced -> nextFace mName card

-- turnedOver's rotation, for the two layouts CR 712.9 lets a permanent turn
-- over. Shared rather than written twice because the two differ about which
-- face is live and about which faces may be cast, and about this not at all:
-- "its other face" is one sentence (CR 701.27a) and reads the same whichever
-- kind of double-faced card is turning.
nextFace :: Maybe CardName.CardName -> Card.Card -> Maybe CardName.CardName
nextFace mName card =
  let faces = NonEmpty.toList (Card.faces card)
      -- Nothing, and a name that resolves to no face, both mean the front face
      -- -- Game.resolveFace's own fallback, so the two agree about which face
      -- the permanent is being turned FROM.
      showing = Maybe.fromMaybe 0 (mName >>= \n -> List.findIndex ((== n) . Face.name) faces)
      after = drop (showing + 1) faces <> take (showing + 1) faces
   in case after of
        [] -> Nothing
        next : _ -> if isInstant next || isSorcery next then Nothing else Just (Face.name next)

-- CR 712.13: "By default, a resolving double-faced spell that becomes a
-- permanent is put onto the battlefield with the same face up that was face up
-- on the stack." Takes the face the SPELL showed on the stack (Object.face) and
-- answers the one the permanent enters showing.
--
-- Nothing for every layout the rule does not name, and that is a claim rather
-- than a shrug: CR 400.7 makes the resolving spell a new object, so the default
-- is that nothing about the stack incarnation survives, and CR 709.4 and CR
-- 715.4 say outright that a split card and an adventurer card have their
-- combined and normal characteristics respectively in every zone but the stack.
-- Dropping the face is what gives them that.
--
-- CR 709.5d asks for the same carry-through the other rule does -- a permanent
-- with a shared type line is unlocked on the half that was cast -- and is NOT
-- implemented here: Rooms have no Layout arm at all (#892).
enteringFace :: Card.Card -> Maybe CardName.CardName -> Maybe CardName.CardName
enteringFace card shown = case Card.layout card of
  Layout.Normal -> Nothing
  Layout.Split -> Nothing
  Layout.Adventure -> Nothing
  -- Indistinguishable from Nothing for every nonmodal card pawl can build today,
  -- and written as the rule reads anyway: CR 712.11 casts one with its front
  -- face up, so `shown` IS the front face, and CR 712.8a resolves Nothing to
  -- that same face. The two answers part only once a card can be cast
  -- "transformed" (CR 712.11a), which is #698's wording plus #70's.
  Layout.Transforming -> shown
  -- The arm CR 712.13 is written for in this pool: CR 712.11b let the caster
  -- choose either face, so the face that was up on the stack is genuinely a
  -- choice and dropping it would put the OTHER face's permanent onto the
  -- battlefield.
  Layout.ModalDoubleFaced -> shown

-- CR 202.3b / 712.8e: the face a MANA VALUE is read from, which is not always
-- the face whose other characteristics are live. "While a nonmodal double-faced
-- permanent has its back face up, it has only the characteristics of its back
-- face. However, its mana value is calculated using the mana cost of its front
-- face" -- and CR 202.3a says the same from the other side, exempting that back
-- face from the mana value of 0 an object with no mana cost otherwise has.
--
-- Takes the live face rather than deriving it, so the two questions stay one
-- call apart: every layout but Transforming answers with exactly what it was
-- given, and the reader (Pawl.Engine.Game.manaCostFaceOf) is the only place that
-- has to know the difference.
--
-- CR 202.3b's second sentence -- a permanent COPYING the back face of a nonmodal
-- double-faced object has mana value 0 -- is not implemented: this answers the
-- front face's cost for every Transforming card, copy or not (#699).
manaCostFace :: Card.Card -> Face.Face Card.Card -> Face.Face Card.Card
manaCostFace card live = case Card.layout card of
  Layout.Normal -> live
  Layout.Split -> live
  Layout.Adventure -> live
  Layout.Transforming -> NonEmpty.head (Card.faces card)
  -- The live face, and NOT the front one: CR 712.8e's mana-value exception is
  -- written about a nonmodal double-faced permanent alone, and CR 712.8f states
  -- the modal rule with no exception at all -- "While a modal double-faced spell
  -- is on the stack or a modal double-faced permanent is on the battlefield, it
  -- has only the characteristics of the face that's up." Mana value is a
  -- characteristic (CR 109.3), so the face that's up is where it is read from.
  Layout.ModalDoubleFaced -> live

-- The face of this card with the given name, if it has one. CR 709.4a: a card's
-- faces are referred to BY NAME, which is what a player names in paper and what
-- survives in a DecisionLog; the Ord on Card.faces is printed order and carries
-- no identity.
--
-- Nothing when no face is so named, and the FIRST match otherwise -- so a hit is
-- unique only where a card's face names are pairwise distinct. That is a
-- requirement on card DATA rather than something this function can check: the
-- corpus lint in Pawl.CardSpec is what holds it, and since the pool now prints a
-- two-faced card (Wax // Wane) that lint compares two names rather than passing
-- vacuously over a pool of one-face cards.
faceNamed :: CardName.CardName -> Card.Card -> Maybe (Face.Face Card.Card)
faceNamed n card = List.find (\f -> Face.name f == n) (NonEmpty.toList (Card.faces card))

-- Every effect across all of a face's modes, in printed (mode, then written)
-- order. CR 608.2c/700.2: the face's whole text spans its modes, and both the
-- dataflow lint and the text-change scan range over all of them regardless of
-- what is chosen.
--
-- Face.mulliganActions is deliberately NOT included: it is not part of the
-- spell, and CR 103.5b's action is performed from the hand rather than cast
-- (#184).
allEffects :: Face.Face Card.Card -> [Effect Card.Card]
allEffects face = Modal.allEffects (Face.spell face)

-- The union of every mode's target specs, plus the enchant slot. CR 303.4a: an
-- Aura spell's target is defined by its enchant ability rather than by a mode,
-- and merging here is what puts that slot in front of Cast's prompt and
-- Resolve's CR 608.2b re-validation without either learning what an Aura is.
--
-- Union is left-biased, and the CardSpec lint holds that no mode declares this
-- slot name, so the bias is never exercised.
allTargetSpecs :: Face.Face Card.Card -> Map SlotName TargetSpec
allTargetSpecs face = Map.union (enchantSpecs face) (Modal.allTargetSpecs (Face.spell face))

-- The target specs of one mode by index (CR 700.2c: only the chosen mode's
-- slots). Nothing if the index is out of range (total). The enchant slot is
-- NOT part of this -- it answers "what does mode i declare", and CR 303.4a's
-- slot is declared by the card, not by any mode.
modeTargetSpecs :: ModeIndex.ModeIndex -> Face.Face Card.Card -> Maybe (Map SlotName TargetSpec)
modeTargetSpecs idx face = Modal.modeTargetSpecs idx (Face.spell face)

-- CR 608.2c/700.2: the CHOSEN modes only, each with its index, in printed order
-- -- the Set is already sorted by ModeIndex's Ord. Out-of-range indices
-- contribute nothing (total via Seq.lookup). Modes rather than a flat effect
-- list, for the reason Modal.chosenModes gives: the mode is the unit CR 603.5's
-- "may" covers.
chosenModes :: Set.Set ModeIndex.ModeIndex -> Face.Face Card.Card -> [(ModeIndex.ModeIndex, Mode.Mode Card.Card)]
chosenModes chosen face = Modal.chosenModes chosen (Face.spell face)

-- CR 601.2c/700.2c: the target specs of the CHOSEN modes only (union), plus
-- the card's enchant slot (CR 303.4a) if it has one. Only these slots are
-- prompted at cast and re-validated at CR 608.2b.
modesTargetSpecs :: Set.Set ModeIndex.ModeIndex -> Face.Face Card.Card -> Map SlotName TargetSpec
modesTargetSpecs chosen face = Map.union (enchantSpecs face) (Modal.modesTargetSpecs chosen (Face.spell face))

isLand :: Face.Face Card.Card -> Bool
isLand f = Set.member CardType.Land (TypeLine.types (Face.typeLine f))

isCreature :: Face.Face Card.Card -> Bool
isCreature f = Set.member CardType.Creature (TypeLine.types (Face.typeLine f))

-- CR 304.1: an instant is castable whenever its controller has priority. The
-- timing classification, shaped like isPermanent.
isInstant :: Face.Face Card.Card -> Bool
isInstant f = Set.member CardType.Instant (TypeLine.types (Face.typeLine f))

-- CR 307.1: a sorcery is cast only in a main phase of its controller's own
-- turn. The other half of the timing classification isInstant is, and the other
-- card type CR 205.4e's casting restriction names.
isSorcery :: Face.Face Card.Card -> Bool
isSorcery f = Set.member CardType.Sorcery (TypeLine.types (Face.typeLine f))

-- CR 205.4a: does the printed type line carry the "legendary" supertype? The
-- supertype half of the same closed-half classification isInstant is. Two rules
-- turn on it: CR 205.4d's legend rule (CR 704.5j, Pawl.Engine.Sba) and CR
-- 205.4e's casting restriction (Pawl.Engine.Cast).
--
-- PRINTED, and only ever asked of a face rather than of a permanent: CR
-- 704.5j's reading has to see a Clone's COPIED supertype, so Sba goes through
-- the projection instead of calling this.
isLegendary :: Face.Face Card.Card -> Bool
isLegendary f = Set.member Supertype.Legendary (TypeLine.supertypes (Face.typeLine f))

-- | CR 110.4
isPermanentType :: CardType.CardType -> Bool
isPermanentType cardType = case cardType of
  CardType.Artifact -> True
  CardType.Battle -> True
  CardType.Conspiracy -> False
  CardType.Creature -> True
  CardType.Dungeon -> False
  CardType.Enchantment -> True
  CardType.Instant -> False
  CardType.Kindred -> False
  CardType.Land -> True
  CardType.Phenomenon -> False
  CardType.Plane -> False
  CardType.Planeswalker -> True
  CardType.Scheme -> False
  CardType.Sorcery -> False
  CardType.Vanguard -> False

-- The classification resolution dispatches on (CR 608.3). This is the whole
-- reason the engine never needs to know WHICH card is resolving.
isPermanent :: Face.Face Card.Card -> Bool
isPermanent f = any isPermanentType (Set.toList (TypeLine.types (Face.typeLine f)))

-- CR 205.3h / 303.4: is this face an Aura? A SUBTYPE read off the printed type
-- line, the same kind of closed-half classification isPermanent is -- NOT a
-- case on the card's identity. Pawl.Engine.Stack dispatches on it, which is the
-- one place an Aura differs from any other enchantment by a rule.
isAura :: Face.Face Card.Card -> Bool
isAura f = Set.member Subtype.Aura (TypeLine.subtypes (Face.typeLine f))

-- CR 205.3k: is this face the ADVENTURE half of an adventurer card? The same
-- printed-subtype read isAura is, and the reason "cast as an Adventure" is
-- answerable at all: CR 715.3b puts one named face on the stack, and this is
-- what says which kind of face that is.
--
-- A subtype and not a position, though CR 715.2's frames are positional and
-- `combined` above does read the position. The two questions differ: which face
-- is NORMAL is a fact about the card's layout, while "was this spell cast as an
-- Adventure" is a fact about the object on the stack, and CR 715.3d asks the
-- second of a face that has already been chosen.
isAdventure :: Face.Face Card.Card -> Bool
isAdventure f = Set.member Subtype.Adventure (TypeLine.subtypes (Face.typeLine f))

-- CR 303.4a: the slot an Aura spell's required target is bound under. A genuine
-- target, so it lives in the ordinary target namespace rather than among
-- Pawl.Engine.Binding's reserved names. The CardSpec lint holds that no mode
-- declares this name, which is what makes the merge above collision-free.
enchantSlot :: SlotName
enchantSlot = SlotName.MkSlotName (Text.pack "enchant")

-- CR 303.4a / 702.5a: the enchant abilities' target spec as a one-entry slot
-- map, empty for every non-Aura. ONE slot however many instances of enchant the
-- face has, since CR 303.4a gives an Aura spell a single target and CR 702.5c
-- makes the instances narrow it together (enchantSpec below). Merged into the two
-- functions above, and passed to Target.fillableModes by Pawl.Engine.Cast so
-- castability accounts for it.
enchantSpecs :: Face.Face Card.Card -> Map SlotName TargetSpec
enchantSpecs face = case enchantSpec face of
  Nothing -> Map.empty
  Just spec -> Map.singleton enchantSlot spec

-- CR 702.5c: "If an Aura has multiple instances of enchant, all of them apply.
-- The Aura's target must follow the restrictions from all the instances of
-- enchant." This is where that rule lives -- the ONE spec every reader of an
-- enchant ability gets, so CR 601.2c's target legality (Pawl.Engine.Cast), CR
-- 303.4c's admission re-check (Pawl.Engine.Sba.fallsOff) and CR 701.3a's attach
-- (Pawl.Engine.Resolve.attachmentFor) cannot disagree about what "all of them"
-- means. Nothing for a face with no enchant ability at all, which is every
-- non-Aura (the CardSpec lint holds the biconditional).
--
-- The conjunction is Filter.And, which already means "matches all of these", so
-- no new predicate vocabulary is involved. A spec with no Filter narrows nothing
-- (TargetSpec's own note), so it contributes no conjunct rather than an empty
-- one -- which keeps a lone bare "enchant creature" folding to ITSELF, and that
-- matters: Sba.stillLegalEnchant's fast arm matches on exactly that shape.
--
-- The POOL is taken from the first instance. CR 702.5c's last sentence conjoins
-- the pools too -- "The Aura can enchant only objects or players that match all
-- of its enchant abilities" -- and what that asks for is their INTERSECTION,
-- which Pool is not closed under: only a NESTED pair has a Pool naming it
-- (Creatures against Permanents is Creatures), Creatures against Players is
-- empty, and AnyTarget against Permanents is creatures-and-planeswalkers. So
-- taking the first instance is not merely inexact, it is ORDER-DEPENDENT even
-- where the answer could be written down. No card can reach it: the CardSpec lint
-- rejects a face whose enchant abilities disagree about their pool, and states
-- the rule (#797).
enchantSpec :: Face.Face card -> Maybe TargetSpec
enchantSpec face = case Face.enchant face of
  [] -> Nothing
  spec : specs ->
    Just
      TargetSpec.MkTargetSpec
        { TargetSpec.pool = TargetSpec.pool spec,
          TargetSpec.filter = case Maybe.mapMaybe TargetSpec.filter (spec : specs) of
            [] -> Nothing
            [one] -> Just one
            many -> Just (Filter.And many)
        }
