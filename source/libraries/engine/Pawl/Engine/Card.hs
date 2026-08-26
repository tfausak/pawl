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
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Counterability as Counterability
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeInstance as ModeInstance
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import Pawl.Types.TargetSlot (TargetSlot)
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TypeLine as TypeLine

-- CR 708.2: what a face-down object's characteristics ARE -- "no characteristics
-- other than those LISTED by the ability or rules that allowed the spell or
-- permanent to be face down". The list is the argument, and CR 708.2a is the
-- list for an ability that names none: "a 2/2 face-down creature with no text,
-- no name, no subtypes, and no mana cost". CR 702.37a words the morph cast's
-- version identically, so FaceDownCharacteristics.defaultValue serves both.
--
-- A SUBSTITUTION and not a layer, which is CR 708.2's own reading: "Any listed
-- characteristics are the COPIABLE VALUES of that object's characteristics."
-- Copiable values are where the CR 613 fold starts, so this replaces the printed
-- face at Pawl.Engine.Game.faceOf and every layer then runs on top of it. CR
-- 708.10 says as much from the other side -- a face-down permanent that becomes
-- a copy of another permanent still has these characteristics.
--
-- A function of the LIST and never of the card underneath, which is the whole
-- point of the rule: nothing about Ainok Tracker survives into what the
-- face-down permanent is. That is also why nothing here can leak the card's
-- identity -- the argument comes from Object.facing, which records what allowed
-- the object to be face down rather than what it was.
--
-- "No characteristics other than those listed" reaches past the fields rule
-- 708.2a names: no colour (the empty colour indicator and absent mana cost make
-- it colourless by CR 105.2c), no supertypes, no abilities of any kind, no
-- casting permissions and no additional or alternative costs. Every field below
-- is that reading, written out rather than inherited, so a field added to
-- Pawl.Types.Face has to be decided here rather than defaulting to the card's.
-- The four the argument reaches are the ones a listing names -- see
-- Pawl.Types.FaceDownCharacteristics for which listings pawl cannot yet carry.
--
-- The KEYWORDS are the exception to "no abilities of any kind", and CR 702.168b
-- is why: disguise's listing is "a 2/2 face-down creature card WITH WARD {2}",
-- so rule 708.2's "no characteristics other than those listed" leaves that one
-- ability standing. It arrives here as an ordinary Face.keywords set, so CR
-- 702.21a's trigger is minted from the projection by the same road every printed
-- ward takes; nothing about being face down is special-cased downstream.
--
-- The one field that is not empty besides the type line and the P/T is `spell`:
-- a face-down creature spell still has to RESOLVE, and Face.defaultSpell is
-- exactly "one mode, no effects, no targets" -- what CR 708.2a's "no text"
-- means for a permanent spell, whose resolution is CR 608.3's move to the
-- battlefield rather than anything the text says.
--
-- The empty name is CR 708.2a's "no name" and is the same value
-- Pawl.Engine.Projection.baseCharacteristics gives an object with no card behind
-- it. No listing in the pool names one. It matches no printed card, which is
-- what makes CR 708.4's "effects that care about the characteristics of a spell
-- will see only the face-down spell's characteristics" true for a prohibition
-- that names a card (Pawl.Engine.PlayerEffect.prohibitsCasting).
faceDownFace :: FaceDownCharacteristics.FaceDownCharacteristics -> Face.Face Card.Card
faceDownFace listed =
  Face.MkFace
    { Face.name = CardName.MkCardName Text.empty,
      Face.manaCost = Nothing,
      Face.typeLine = FaceDownCharacteristics.typeLine listed,
      Face.power = FaceDownCharacteristics.power listed,
      Face.toughness = FaceDownCharacteristics.toughness listed,
      Face.loyalty = Nothing,
      Face.defense = Nothing,
      Face.keywords = FaceDownCharacteristics.keywords listed,
      Face.colorIndicator = Set.empty,
      Face.characteristicPT = Nothing,
      Face.staticAbilities = [],
      Face.spell = Face.defaultSpell,
      Face.activatedAbilities = [],
      Face.replacementEffects = [],
      Face.triggeredAbilities = [],
      Face.delayedAbilities = Map.empty,
      Face.rooms = Seq.empty,
      Face.castingPermissions = [],
      Face.castingRestrictions = [],
      Face.enchant = [],
      Face.counterability = Counterability.Counterable,
      Face.additionalCosts = [],
      Face.maximumX = Nothing,
      Face.alternativeCosts = [],
      Face.costReductions = [],
      Face.playerAbilities = [],
      Face.blockRequirements = [],
      Face.blockPermissions = [],
      Face.attackRequirements = [],
      Face.combatRestrictions = [],
      Face.sacrificeRestrictions = [],
      Face.untapRestrictions = [],
      Face.attachRestrictions = [],
      Face.counterRestrictions = [],
      Face.entryRestrictions = [],
      Face.attackCosts = [],
      Face.blockCosts = [],
      Face.mulliganActions = [],
      Face.openingHandActions = [],
      Face.specialActions = []
    }

-- The face a card shows where nothing has singled out one half for itself. WHICH
-- face that is, is exactly what the layout decides, and the three rules disagree:
-- CR 709.4 gives a split card its two halves COMBINED, CR 712.8a gives a
-- double-faced card its FRONT face alone, CR 715.4 gives an adventurer card its
-- NORMAL characteristics alone. Under Normal there is one face and all three name
-- it; the layouts that make them differ each have their arm below.
--
-- A ROOM PERMANENT is the one object this does not answer for, and CR 709.5 is
-- why: it singles out no half either, but its LOCKED halves are subtracted from
-- the combined view rather than folded into it. `roomFace` below is that answer,
-- built from the same foldSplit this function's Room arm takes; the seam that
-- chooses between the two is Pawl.Engine.Game.resolveFaceFor. The Room arm here
-- is the card OFF the battlefield, where CR 709.5c leaves it no designations to
-- subtract by.
--
-- TOTAL, which is what Card.faces being NonEmpty buys: every characteristic read
-- in the engine funnels through here or through roomFace, and a Maybe would
-- spread to all of them.
--
-- The layout case analysis is combinedFaces below, so that the NAMES this view
-- has (CR 709.4a, combinedNames) and the characteristics it has cannot come to
-- disagree about which halves contribute.
combined :: Card.Card -> Face.Face Card.Card
combined = foldSplit . combinedFaces

-- CR 709.4a: the names CR 709.4's combined view has, one per contributing half.
-- Plural for a split card and for a Room off the battlefield, singular for
-- everything else -- and never the "//"-joined string Face.name carries, which
-- is a rendering of the two and not a name the object has.
combinedNames :: Card.Card -> Set.Set CardName.CardName
combinedNames = Set.fromList . fmap Face.name . NonEmpty.toList . combinedFaces

-- Which halves `combined` folds -- the layout's own answer to "what does a card
-- show where nothing has singled out one half for itself".
combinedFaces :: Card.Card -> NonEmpty.NonEmpty (Face.Face Card.Card)
combinedFaces card = case Card.layout card of
  Layout.Normal -> pure (NonEmpty.head (Card.faces card))
  -- CR 709.4: "In every zone except the stack, the characteristics of a split
  -- card are those of its two halves combined." Written over the whole
  -- NonEmpty rather than over a pair because docs/design.md §2.11 is a
  -- standing rule against baking arity into the card model: every
  -- tournament-legal split card has exactly two faces, but a fold over N costs
  -- nothing to write and so also covers Who // What // When // Where // Why, a
  -- five-part split card from a silver-border Un-set, entirely outside the CR.
  Layout.Split -> Card.faces card
  -- CR 709.4 again, and the same expression for a different reason. A Room is a
  -- split card (CR 709.5's own first words, "Some split cards are permanent
  -- cards with a single shared type line"), so its off-the-battlefield view is
  -- the two halves combined exactly as CR 709.4 says. What CR 709.5 adds only
  -- bites on the battlefield: its two static abilities subtract a LOCKED half's
  -- name, mana cost and rules text, and CR 709.5c makes the unlocked
  -- designations something "a permanent on the battlefield can have". A Room in a
  -- hand, a graveyard or a library has no designations because it is not a
  -- permanent, so no half is locked and nothing is taken back out -- see
  -- roomFace below, which is this value with the locked halves subtracted.
  Layout.Room -> Card.faces card
  -- CR 715.4: "In every zone except the stack, and while on the stack not as an
  -- Adventure, an adventurer card has only its normal characteristics." The
  -- alternative characteristics of CR 715.2 are reached ONLY through
  -- Object.face, which CR 715.3b writes for the stack incarnation and CR 715.3a's
  -- offer writes speculatively (Cast.asProposed) --
  -- so the face this returns is the normal one, and the same expression Normal
  -- takes is a different claim: that a card with TWO printed sets of
  -- characteristics shows one of them, rather than that a card with one shows
  -- it.
  Layout.Adventure -> pure (NonEmpty.head (Card.faces card))
  -- CR 712.8a: "While a double-faced card is outside the game or in a zone other
  -- than the battlefield or stack, it has only the characteristics of its front
  -- face", and CR 712.8d says the same of a permanent showing that face. The
  -- front face is the first (Pawl.Types.Layout.Transforming), so this is
  -- Adventure's expression again over a different claim: what a nonmodal
  -- double-faced card shows where nothing has turned it over.
  --
  -- The BACK face is reached only through Object.face, which CR 701.27a writes.
  Layout.Transforming -> pure (NonEmpty.head (Card.faces card))
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
  Layout.ModalDoubleFaced -> pure (NonEmpty.head (Card.faces card))

-- CR 709.4, one pair at a time. Left-associated over the NonEmpty, so printed
-- order decides the joined name and the concatenated mana cost.
--
-- Also the engine of CR 709.5's subtraction: roomFace below folds the SAME way
-- over halves it has already emptied, so a Room's open doors combine exactly as a
-- split card's two halves do and the locked ones contribute nothing.
foldSplit :: NonEmpty.NonEmpty (Face.Face Card.Card) -> Face.Face Card.Card
foldSplit faces = List.foldl' merge2 (NonEmpty.head faces) (NonEmpty.tail faces)

merge2 :: Face.Face Card.Card -> Face.Face Card.Card -> Face.Face Card.Card
merge2 l r =
  l
    { -- CR 709.4a gives the card BOTH names and no joined one, and a single
      -- CardName cannot carry that: this field is a RENDERING of the two, in
      -- the form docs/rules.txt's own Examples write, unspaced -- "Fire//Ice"
      -- (lines 3882, 5747) and "Assault//Battery" (line 5746). What the object
      -- has for rules purposes is combinedNames above, which every name
      -- question goes through (Pawl.Engine.Projection.hasName).
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
      -- Unreachable rather than merged: CR 709.4's combined view is a Room's, and
      -- CR 309.2c keeps a dungeon card out of every zone but the command zone, so
      -- no card has both halves and rooms. Concatenating would be wrong if one ever
      -- did -- a RoomIndex counts within one face, so the right half's arrows would
      -- point into the left half's rooms -- but with one side always empty this
      -- picks whichever side has them.
      Face.rooms = Face.rooms l <> Face.rooms r,
      Face.castingPermissions = Face.castingPermissions l <> Face.castingPermissions r,
      Face.castingRestrictions = Face.castingRestrictions l <> Face.castingRestrictions r,
      Face.additionalCosts = Face.additionalCosts l <> Face.additionalCosts r,
      Face.alternativeCosts = Face.alternativeCosts l <> Face.alternativeCosts r,
      -- CR 709.4c again: a cost reduction a half prints about itself is an
      -- ability in that half's text box, so the combined view has both. Never
      -- exercised -- CR 709.3b puts ONE half on the stack and
      -- Pawl.Engine.Cost.costsFor prices that half's own face -- but written
      -- for its neighbours' reason: a record UPDATE would otherwise keep the
      -- left half's silently.
      Face.costReductions = Face.costReductions l <> Face.costReductions r,
      Face.playerAbilities = Face.playerAbilities l <> Face.playerAbilities r,
      Face.blockRequirements = Face.blockRequirements l <> Face.blockRequirements r,
      Face.blockPermissions = Face.blockPermissions l <> Face.blockPermissions r,
      Face.attackRequirements = Face.attackRequirements l <> Face.attackRequirements r,
      Face.combatRestrictions = Face.combatRestrictions l <> Face.combatRestrictions r,
      Face.sacrificeRestrictions = Face.sacrificeRestrictions l <> Face.sacrificeRestrictions r,
      Face.untapRestrictions = Face.untapRestrictions l <> Face.untapRestrictions r,
      Face.attachRestrictions = Face.attachRestrictions l <> Face.attachRestrictions r,
      Face.counterRestrictions = Face.counterRestrictions l <> Face.counterRestrictions r,
      Face.entryRestrictions = Face.entryRestrictions l <> Face.entryRestrictions r,
      Face.attackCosts = Face.attackCosts l <> Face.attackCosts r,
      Face.blockCosts = Face.blockCosts l <> Face.blockCosts r,
      Face.mulliganActions = Face.mulliganActions l <> Face.mulliganActions r,
      Face.openingHandActions = Face.openingHandActions l <> Face.openingHandActions r,
      -- CR 709.4c: "each ability in the text box of each half", so a permission
      -- printed on either half survives into the combined view. No printed split
      -- card grants a CR 116.2 special action; the line is here for the reason
      -- its neighbours are -- a record UPDATE would otherwise keep the left
      -- half's silently.
      Face.specialActions = Face.specialActions l <> Face.specialActions r,
      -- CR 709.4c again, and CR 702.5a: an enchant ability IS an ability in a
      -- half's text box, so both halves' survive here -- and CR 702.5c says what
      -- a combined view carrying two of them means, which is
      -- Card.enchantTargetSlot's conjunction. Concatenated rather than
      -- left-biased for that reason, unlike the printed boxes below.
      Face.enchant = Face.enchant l <> Face.enchant r,
      -- The half that prints the box. Where exactly ONE does, this is CR 709.4
      -- read rather than guessed: "the characteristics of a split card are those
      -- of its two halves combined", so the combined view has the power the one
      -- half has, exactly as CR 709.4c's card types and abilities are had. That
      -- is the case Pawl.CardSpec's SplitBox group proves, over synthetic split
      -- cards carrying each box on the RIGHT half alone -- a record update over
      -- `l` answers Nothing there, and the permanent is a 0/0, a planeswalker
      -- with no loyalty counters or a battle with no defense counters.
      --
      -- Where BOTH halves print one, no rule answers and the left bias below is
      -- arbitrary rather than wrong. CR 709.4's three worked readings of
      -- "combined" -- 709.4a's two names, 709.4b's concatenated mana cost,
      -- 709.4c's card types and abilities -- each give the combined object what
      -- both halves have AT ONCE, and none of them picks a half. A printed box
      -- cannot be had twice: CR 208.1 gives a creature one power, CR 208.5 speaks
      -- of a creature having "no value for its power" and never of two, and CR
      -- 613.4 has one number to work on. SUMMING was the alternative, generalising
      -- the mana value that falls out of CR 709.4b -- rejected because 709.4b adds
      -- symbol LISTS and states no arithmetic over numbers, and because addition
      -- is not idempotent: a characteristic printed once and belonging to both
      -- halves, which is exactly what CR 709.5a's shared type line is, survives
      -- unionTypeLines and would be doubled here.
      Face.power = firstJust (Face.power l) (Face.power r),
      Face.toughness = firstJust (Face.toughness l) (Face.toughness r),
      Face.loyalty = firstJust (Face.loyalty l) (Face.loyalty r),
      Face.defense = firstJust (Face.defense l) (Face.defense r),
      -- CR 709.4c reaches this one where it does not reach the four above: a
      -- characteristic-defining ability IS "an ability in the text box" (CR
      -- 604.3), so the combined view has BOTH halves'. One slot holds one of
      -- them. Not implemented: a combined view carrying the P/T-defining
      -- abilities of two halves at once (#2019) -- which CR 613.4a would then
      -- have no order to apply them in anyway, since CR 613.7a gives both the
      -- object's own timestamp.
      Face.characteristicPT = firstJust (Face.characteristicPT l) (Face.characteristicPT r),
      -- CR 709.4c: a sentence bounding X is an ability in a half's text box, so
      -- the combined view keeps whichever half prints one. The first, the printed
      -- boxes above's rule -- and unreachable for the same reason
      -- Face.costReductions is, CR 709.3b putting ONE half on the stack for
      -- Pawl.Engine.Cost to price and Pawl.Engine.Cast to announce against. Here
      -- so that a record UPDATE does not keep the left half's silently.
      Face.maximumX = firstJust (Face.maximumX l) (Face.maximumX r)
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
  -- CR 709.3 reaches a Room unchanged: it is a split card, so its caster chooses
  -- which half before putting it onto the stack, and every half is offered for
  -- Split's reason -- the choice is the player's. The reminder text on the
  -- printings says the same ("You may cast either half").
  --
  -- CR 709.5b is what makes the resulting SPELL differ from an ordinary split
  -- card's: "The existence of each half of an object with a shared type line is
  -- part of that object's copiable values, even if that object is a spell on the
  -- stack. This is an exception to rule 709.3b." That exception is about what the
  -- spell still HAS, not about what may be cast, so it changes nothing here --
  -- CR 709.3a still evaluates and prices only the chosen half.
  Layout.Room -> NonEmpty.toList (Card.faces card)
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
  -- 712.11a), and such an effect names the face itself rather than reaching this
  -- list -- Effect.OfferCast carries CR 310.12b's "transformed" rider and
  -- Pawl.Engine.Resolve answers it with `backFace` below. CR 712.14a's wording
  -- reaches a back face without one on the stack, and likewise does not come
  -- through here: Pawl.Types.EntryRiders carries it and
  -- Pawl.Engine.Event.changeZoneEntering applies it. CR 712.13a's ability causing
  -- a double-faced spell already on the stack to enter transformed does not
  -- either -- it is a replacement effect, EntryRewrite.EntersTransformed. What is
  -- still absent is the CONVERT wording (#698).
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
        -- CR 709.5 names a Room among the split cards that are "permanent
        -- cards", and every printing's shared type line reads
        -- "Enchantment - Room", so the filter below drops the pair whichever
        -- view is offered. That last clause is a fact about the pool and not
        -- something the rule requires (see hasSharedTypeLine below); a Room with
        -- a land in its type line would want the arm castableFaces has.
        Layout.Room -> byDefault
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
-- "transformed" wording, whose own refusal -- a card that isn't double-faced
-- stays put -- is the same door's, read off `backFace` rather than off this.
staysWhenPutOntoBattlefield :: Card.Card -> Bool
staysWhenPutOntoBattlefield card = case Card.layout card of
  Layout.Normal -> False
  Layout.Split -> False
  Layout.Room -> False
  Layout.Adventure -> False
  Layout.Transforming -> False
  Layout.ModalDoubleFaced -> not (isPermanent (NonEmpty.head (Card.faces card)))

-- CR 712.8a / 712.11: the FRONT face of a card -- the face a double-faced card
-- shows wherever nothing has turned it over, and the face CR 712.11 makes a
-- double-faced spell be cast with by default.
--
-- The first, which is the ordering Pawl.Types.Layout's Transforming and
-- ModalDoubleFaced both document, and TOTAL for the reason `combined` is:
-- Card.faces is a NonEmpty, so every layout has one. NOT `combined`, and only
-- Split tells them apart -- CR 709.4's combined view is what a split card has
-- off the stack, and its FIRST HALF is a different object.
frontFace :: Card.Card -> Face.Face Card.Card
frontFace = NonEmpty.head . Card.faces

-- CR 712.1 / 712.11a: the BACK face of a double-faced card -- the one "if a
-- double-faced card ... is cast as a spell 'transformed' ... it's put on the
-- stack with its back face up" names, and whose characteristics the resulting
-- spell then has (CR 712.8c for the nonmodal kind, CR 712.8f for the modal one).
--
-- Nothing for every layout that has no back face. CR 712.14a states the same
-- answer one zone over -- "if a player is instructed to put a card that isn't a
-- double-faced card onto the battlefield transformed or converted, that card
-- stays in its current zone" -- so an offer to cast such a card transformed
-- offers nothing rather than offering its front face under another name.
--
-- The SUCCESSOR of the front face, wrapping, which is `turnedOver`'s expression
-- read from the front: rule 712.1 gives a double-faced card two faces, and
-- docs/design.md section 2.11's standing rule against baking arity in makes a
-- longer list rotate rather than fail. Written separately from `turnedOver`
-- because that function answers CR 701.27a's question about a PERMANENT already
-- showing a face, and this one answers a question about a CARD in a zone where
-- CR 712.8a says it shows only its front.
backFace :: Card.Card -> Maybe (Face.Face Card.Card)
backFace card =
  let successor = case NonEmpty.tail (Card.faces card) of
        [] -> Nothing
        next : _ -> Just next
   in case Card.layout card of
        Layout.Normal -> Nothing
        Layout.Split -> Nothing
        -- A Room's halves are not faces of a double-faced card: CR 712.1 lists
        -- the kinds and a shared type line is not among them, so CR 712.14a's
        -- "a card that isn't a double-faced card ... stays in its current zone"
        -- is the answer, and Nothing is how this function says it.
        Layout.Room -> Nothing
        Layout.Adventure -> Nothing
        Layout.Transforming -> successor
        -- The SAME answer, because CR 712.11a says "a double-faced card" and CR
        -- 712.1 counts the modal kind among them: a card cast "transformed" is
        -- put on the stack with its back face up whichever kind it is, and CR
        -- 712.9 lets a modal double-faced permanent turn over for the same
        -- reason.
        --
        -- NOT how a modal double-faced card's back face is ordinarily reached,
        -- and that distinction is the whole point of the arm. CR 712.11b --
        -- "chooses which face they are casting before putting it onto the
        -- stack" -- makes that a CHOICE the caster makes at every cast, which is
        -- `castableFaces` offering every face above; routing it through a
        -- "transformed" rider instead would turn the player's choice into an
        -- effect's instruction and make the front face the only ordinary offer.
        -- The two rules answer different questions and this arm answers only CR
        -- 712.11a's.
        --
        -- No printing reaches it today: the pool's only producer of a cast
        -- "transformed" is CR 310.12b's defeated Siege, and no battle is a modal
        -- double-faced card. The wording that could is CR 701.28's "converted"
        -- (#698), which CR 712.3 names on exactly this layout.
        Layout.ModalDoubleFaced -> successor

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
  -- CR 701.27c / 712.9: a Room is not represented by a double-faced token or a
  -- double-faced card, so an instruction to transform one does nothing. Its two
  -- halves are both on the front, which is what a shared type line means.
  Layout.Room -> Nothing
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
-- answers the half that survives the move onto the battlefield.
--
-- WHAT THE ANSWER IS FOR differs by layout, which is why it is worded as a half
-- surviving rather than as the face the permanent shows. For a double-faced card
-- CR 712.13 leaves the permanent showing it, and Object.face is where
-- Pawl.Engine.Event.changeZoneAttaching writes it. For a Room, CR 709.5d spends
-- it on an unlocked DESIGNATION instead -- see the Room arm below, and
-- hasSharedTypeLine, which is what that move reads to tell the two apart.
--
-- Nothing for every layout neither rule names, and that is a claim rather than a
-- shrug: CR 400.7 makes the resolving spell a new object, so the default is that
-- nothing about the stack incarnation survives, and CR 709.4 and CR 715.4 say
-- outright that a split card and an adventurer card have their combined and
-- normal characteristics respectively in every zone but the stack. Dropping the
-- face is what gives them that.
--
enteringFace :: Card.Card -> Maybe CardName.CardName -> Maybe CardName.CardName
enteringFace card shown = case Card.layout card of
  Layout.Normal -> Nothing
  Layout.Split -> Nothing
  -- CR 709.5d asks for the same carry-through CR 712.13 does -- "A permanent
  -- with a shared type line is given the 'left half unlocked' designation as it
  -- enters the battlefield if its left half was cast as a spell" -- so the half
  -- the spell showed has to survive the move, and this is the one channel that
  -- carries it.
  --
  -- What the permanent does with it is where the two rules part. CR 712.13
  -- leaves a double-faced permanent SHOWING the face, which Object.face records;
  -- CR 709.5c's answer is a DESIGNATION, and a Room shows both halves at once
  -- whichever doors are open. So Pawl.Engine.Event.changeZoneAttaching spends
  -- this answer on Object.unlockedHalves rather than on Object.face, gated by
  -- hasSharedTypeLine below -- see the `face` note in its mkObj.
  Layout.Room -> shown
  Layout.Adventure -> Nothing
  -- CR 712.11 casts one with its front face up, so for an ordinary cast `shown`
  -- IS the front face and CR 712.8a would resolve Nothing to that same face. The
  -- two answers part where CR 712.11a's "transformed" cast does put a back face
  -- on the stack, which CR 310.12b's defeated Siege reaches (Pawl.BattleSpec's
  -- "she may then cast it TRANSFORMED and FREE" is the proof). The CONVERT
  -- spelling of the same permission is still absent (#698).
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
-- Only CR 202.3b's FIRST sentence, which is about the object itself. Its second
-- -- a copy of that back face has mana value 0 -- is a question about copying
-- that this function has no way to ask, so `showsBackFace` below answers the
-- classification and Pawl.Engine.Event's AsCopy arm spends it on the snapshot.
manaCostFace :: Card.Card -> Face.Face Card.Card -> Face.Face Card.Card
manaCostFace card live = case Card.layout card of
  Layout.Normal -> live
  Layout.Split -> live
  -- The live face, which for a Room permanent is roomFace's subtracted view: CR
  -- 709.5 takes a locked half's MANA COST away along with its name and rules
  -- text, so the mana value of a Room with one door open is that door's alone.
  -- CR 712.8e's exception is written about a nonmodal double-faced permanent and
  -- reaches nothing here.
  Layout.Room -> live
  Layout.Adventure -> live
  Layout.Transforming -> NonEmpty.head (Card.faces card)
  -- The live face, and NOT the front one: CR 712.8e's mana-value exception is
  -- written about a nonmodal double-faced permanent alone, and CR 712.8f states
  -- the modal rule with no exception at all -- "While a modal double-faced spell
  -- is on the stack or a modal double-faced permanent is on the battlefield, it
  -- has only the characteristics of the face that's up." Mana value is a
  -- characteristic (CR 109.3), so the face that's up is where it is read from.
  Layout.ModalDoubleFaced -> live

-- CR 202.3b, second sentence: is this card, showing this face, "the back face of
-- a nonmodal double-faced object"? "If a permanent or spell is a copy of the
-- back face of a nonmodal double-faced object (even if the card representing
-- that copy is itself a double-faced card), the mana value of the copy is 0."
--
-- The classification only. WHOSE mana value that makes 0 is a question about
-- copying, which is Pawl.Engine.Event's AsCopy arm -- the one place a copiable
-- snapshot is stamped -- and this answers about one card and the face it shows,
-- the way every other function in this module does.
--
-- NONMODAL is CR 712.2's kind, which in this pool is Layout.Transforming alone.
-- The modal layout is excluded by the rule's own word and not by an oversight:
-- CR 712.8f gives a modal double-faced permanent only "the characteristics of
-- the face that's up", its back face prints a mana cost of its own, and CR
-- 202.3b's first sentence never reached it either.
--
-- CR 202.3c states the identical rule for a copy of a MELDED permanent. Melding
-- is unmodelled (#369), so no object can be one for this to answer about.
--
-- The face is matched by NAME against the card's printed order, the way nextFace
-- above does it, so a name that resolves to no face falls back to the front
-- face -- Pawl.Engine.Game.resolveFace's own fallback, which is what keeps the
-- two from disagreeing about which face is up.
showsBackFace :: Card.Card -> Maybe CardName.CardName -> Bool
showsBackFace card mName = case Card.layout card of
  Layout.Normal -> False
  Layout.Split -> False
  Layout.Room -> False
  Layout.Adventure -> False
  Layout.Transforming ->
    case mName >>= \name -> List.findIndex ((== name) . Face.name) (NonEmpty.toList (Card.faces card)) of
      Nothing -> False
      Just index -> index /= 0
  Layout.ModalDoubleFaced -> False

-- CR 709.5: does this card have a SHARED TYPE LINE -- is it a Room? "Some split
-- cards are permanent cards with a single shared type line", and everything the
-- rule adds over CR 709.4 hangs off that one fact: the two static abilities that
-- subtract a locked half (roomFace below), CR 709.5c's designations, CR 709.5d's
-- entering designation and CR 709.5e's unlock cost.
--
-- A LAYOUT read, like every other classification in this module, and never a
-- question about which card it is. Named for the rule's own phrase rather than
-- `isRoom`, because "Room" is the SUBTYPE the printings happen to share (CR
-- 205.3) and a shared type line is what CR 709.5 actually turns on; nothing in
-- the rule requires the type line it shares to say Room.
hasSharedTypeLine :: Card.Card -> Bool
hasSharedTypeLine card = case Card.layout card of
  Layout.Normal -> False
  Layout.Split -> False
  Layout.Room -> True
  Layout.Adventure -> False
  Layout.Transforming -> False
  Layout.ModalDoubleFaced -> False

-- CR 709.5: what a Room permanent's characteristics ARE, given which of its
-- halves are unlocked (CR 709.5c). The shared type line "represents two static
-- abilities that function on the battlefield" -- "As long as this permanent
-- doesn't have the 'left half unlocked' designation, it doesn't have the name,
-- mana cost, or rules text of this object's left half", and its mirror -- so this
-- is CR 709.4's combined view (`combined` above) with every LOCKED half's name,
-- mana cost and rules text taken back out.
--
-- A SUBSTITUTION and not a pair of real static abilities, which is the reading CR
-- 709.5's own last sentence gives: "These abilities, as well as which half of that
-- permanent a characteristic is in, are part of that object's copiable values."
-- Copiable values are where the CR 613 fold STARTS, so the subtraction has to be
-- done before layer 1 rather than inside layer 3 or 6 -- and doing it as layers
-- would be wrong twice over. It would let a later timestamp or a dependency
-- reorder it, which copiable values cannot be; and CR 707.2 would then copy a
-- Room's whole text and leave the copy's doors unsubtractable, where the rule
-- says the abilities and the halves are copied. This is exactly the position CR
-- 708.2 puts a face-down permanent's characteristics in, and faceDownFace above
-- is substituted at the same seam (Pawl.Engine.Game.faceOf).
--
-- The type line survives a locked half, and that is the rule rather than an
-- omission: CR 709.5 subtracts the name, the mana cost and the rules text, and
-- nothing else. CR 709.5a says why -- "Each half of a split card with a shared
-- type line shares the types and subtypes listed on that card's shared type
-- line" -- so a Room with both doors locked is still an Enchantment Room. The
-- printed P/T boxes and colour indicator stay for the same reason: neither is
-- rules text, and no printing has either.
--
-- pawl stores that shared line on every face rather than once, so what holds the
-- copies to being copies is a corpus lint -- Pawl.CardSpec's "CR 709.5a a Room's
-- faces agree on their shared type line". Without it unionTypeLines above would
-- merge two disagreeing halves into a line neither prints.
--
-- The NAME is rebuilt rather than folded, because CR 709.4a's join is over the
-- names the object HAS: a Room with one door open has one name, and joining an
-- emptied one would leave the "Roaring Furnace//" of a half that was subtracted.
-- With no door open it has no name at all, which is the empty CardName
-- faceDownFace uses for CR 708.2a's "no name" -- the same value
-- Pawl.Engine.Projection.baseCharacteristics gives an object with no card behind
-- it, and one that matches no printing.
roomFace :: Set.Set CardName.CardName -> Card.Card -> Face.Face Card.Card
roomFace unlocked card =
  let folded = foldSplit (fmap (\face -> if Set.member (Face.name face) unlocked then face else subtractHalf face) (Card.faces card))
   in folded
        { Face.name = case unlockedFaces unlocked card of
            [] -> CardName.MkCardName Text.empty
            face : faces -> CardName.join (Face.name face NonEmpty.:| fmap Face.name faces)
        }

-- CR 709.5c: the halves of this Room permanent that ARE unlocked -- the halves
-- roomFace above keeps whole, in printed order. Pawl.Engine.Room.lockedHalves is
-- its complement, and asks the object rather than a designation set because its
-- caller needs the object anyway.
unlockedFaces :: Set.Set CardName.CardName -> Card.Card -> [Face.Face Card.Card]
unlockedFaces unlocked card = filter (\face -> Set.member (Face.name face) unlocked) (NonEmpty.toList (Card.faces card))

-- CR 709.4a / 709.5: the names a Room permanent has -- one per UNLOCKED door,
-- since the shared type line's static abilities take a locked half's name away.
-- None with both doors shut, which is CR 708.2a's "no name" read off a different
-- rule: an empty set, and never the empty name Face.name has to fall back on.
roomNames :: Set.Set CardName.CardName -> Card.Card -> Set.Set CardName.CardName
roomNames unlocked card = Set.fromList (fmap Face.name (unlockedFaces unlocked card))

-- roomFace's per-half half: one LOCKED half of a Room, emptied of everything CR
-- 709.5's static abilities take away -- "the name, mana cost, or rules text".
--
-- Written out field by field rather than derived from faceDownFace, for the
-- reason that value's own note gives: a field added to Pawl.Types.Face has to be
-- DECIDED here rather than defaulting to the printed half's. The two lists differ
-- because the two rules differ -- CR 708.2a replaces the whole face, where CR
-- 709.5 subtracts three things from it and leaves the rest standing.
--
-- Face.spell is emptied even though nothing reads it: merge2 keeps the LEFT
-- half's spell unconditionally, so a locked left half would otherwise donate its
-- text to a permanent the rule says does not have it. Face.counterability is
-- emptied for the same reason -- CR 113.6g is a per-half ability, so it is rules
-- text of that half.
subtractHalf :: Face.Face Card.Card -> Face.Face Card.Card
subtractHalf face =
  face
    { Face.name = CardName.MkCardName Text.empty,
      Face.manaCost = Nothing,
      Face.keywords = Set.empty,
      Face.staticAbilities = [],
      Face.spell = Face.defaultSpell,
      Face.activatedAbilities = [],
      Face.replacementEffects = [],
      Face.triggeredAbilities = [],
      Face.delayedAbilities = Map.empty,
      Face.rooms = Seq.empty,
      Face.castingPermissions = [],
      Face.castingRestrictions = [],
      Face.enchant = [],
      Face.counterability = Counterability.Counterable,
      Face.additionalCosts = [],
      Face.maximumX = Nothing,
      Face.alternativeCosts = [],
      Face.costReductions = [],
      Face.playerAbilities = [],
      Face.blockRequirements = [],
      Face.blockPermissions = [],
      Face.attackRequirements = [],
      Face.combatRestrictions = [],
      Face.sacrificeRestrictions = [],
      Face.untapRestrictions = [],
      Face.attachRestrictions = [],
      Face.counterRestrictions = [],
      Face.entryRestrictions = [],
      Face.attackCosts = [],
      Face.blockCosts = [],
      Face.mulliganActions = [],
      Face.openingHandActions = [],
      Face.specialActions = []
    }

-- The face of this card with the given name, if it has one. CR 709.4a: a card's
-- faces are referred to BY NAME, which is what a player names in paper and what
-- survives in a DecisionLog; the Ord on Card.faces is printed order and carries
-- no identity.
--
-- Nothing when no face is so named, and the FIRST match otherwise -- so a hit is
-- unique only where a card's face names are pairwise distinct. That is a
-- requirement on card DATA rather than something this function can check: the
-- corpus lint in Pawl.CardSpec is what holds it, and since the pool prints
-- two-faced cards (Wax // Wane, Roaring Furnace // Steaming Sauna) that lint
-- compares two names rather than passing vacuously over a pool of one-face cards.
--
-- CR 709.5 is what raised the stakes on that uniqueness: an unlocked designation
-- (Object.unlockedHalves) and CR 709.5h's trigger both pick a half out by name,
-- so two halves sharing one name would open and fire the wrong door rather than
-- merely reading the wrong characteristics.
faceNamed :: CardName.CardName -> Card.Card -> Maybe (Face.Face Card.Card)
faceNamed n card = List.find (\f -> Face.name f == n) (NonEmpty.toList (Card.faces card))

-- Every effect across all of a face's modes, in printed (mode, then written)
-- order. CR 608.2c/700.2: the face's whole text spans its modes, and both the
-- dataflow lint and the text-change scan range over all of them regardless of
-- what is chosen.
--
-- Face.mulliganActions and Face.openingHandActions are deliberately NOT
-- included: neither is part of the spell, and CR 103.5b's and CR 103.6's actions
-- are performed from the hand rather than cast. Pawl.CardSpec's handActions
-- reaches them instead, and holds them to the dataflow rule on their own terms
-- -- they declare no target slots -- so leaving them out here costs no lint
-- coverage.
allEffects :: Face.Face Card.Card -> [Effect Card.Card]
allEffects face = Modal.allEffects (Face.spell face)

-- The union of every mode's target slots, plus the enchant slot. CR 303.4a: an
-- Aura spell's target is defined by its enchant ability rather than by a mode,
-- and merging here is what puts that slot in front of Cast's prompt and
-- Resolve's CR 608.2b re-validation without either learning what an Aura is.
--
-- Union is left-biased, and the CardSpec lint holds that no mode declares this
-- slot name, so the bias is never exercised.
allTargetSlots :: Face.Face Card.Card -> Map SlotName TargetSlot
allTargetSlots face = Map.union (enchantSlotMap face) (Modal.allTargetSlots (Face.spell face))

-- The target slots of one mode by index (CR 700.2c: only the chosen mode's
-- slots). Nothing if the index is out of range (total). The enchant slot is
-- NOT part of this -- it answers "what does mode i declare", and CR 303.4a's
-- slot is declared by the card, not by any mode.
modeTargetSlots :: ModeIndex.ModeIndex -> Face.Face Card.Card -> Maybe (Map SlotName TargetSlot)
modeTargetSlots idx face = Modal.modeTargetSlots idx (Face.spell face)

-- CR 608.2c/700.2: the CHOSEN modes only, each with the instance naming which
-- mode and which occurrence of it, in printed order -- the Seq is kept sorted by
-- the casting path. Out-of-range indices contribute nothing (total via
-- Seq.lookup). Modes rather than a flat effect list, for the reason
-- Modal.chosenModes gives: the mode is the unit CR 603.5's "may" covers.
chosenModes :: Seq.Seq ModeIndex.ModeIndex -> Face.Face Card.Card -> [(ModeInstance.ModeInstance, Mode.Mode Card.Card)]
chosenModes chosen face = Modal.chosenModes chosen (Face.spell face)

-- CR 601.2c/700.2c: the target slots of the CHOSEN modes only (union), plus
-- the card's enchant slot (CR 303.4a) if it has one. Only these slots are
-- prompted at cast and re-validated at CR 608.2b.
modesTargetSlots :: Seq.Seq ModeIndex.ModeIndex -> Face.Face Card.Card -> Map SlotName TargetSlot
modesTargetSlots chosen face = modesTargetSlotsGiven (Face.enchant face) chosen face

-- modesTargetSlots with the enchant INSTANCES handed in rather than read off the
-- printed face -- what an object's projection holds (CR 613.1f), which is where a
-- granted instance lives (Modification.GainEnchant, CR 702.103b's bestow). The
-- two callers that HAVE an object are Pawl.Engine.Cast's CR 601.2c prompt and
-- Pawl.Engine.Resolve's CR 608.2b re-check, and both take this one, so a spell
-- whose enchant ability it was granted is offered and re-judged on that ability
-- rather than on the empty printed list. modesTargetSlots above is the same
-- function fed the printed instances, which is what a caller holding only a face
-- can supply.
modesTargetSlotsGiven :: [TargetSlot] -> Seq.Seq ModeIndex.ModeIndex -> Face.Face Card.Card -> Map SlotName TargetSlot
modesTargetSlotsGiven enchants chosen face = Map.union (enchantSlotMapGiven enchants) (Modal.modesTargetSlots chosen (Face.spell face))

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

-- CR 303.4a / 702.5a: the enchant abilities' target slot as a one-entry map,
-- empty for every non-Aura. ONE slot however many instances of enchant the
-- face has, since CR 303.4a gives an Aura spell a single target and CR 702.5c
-- makes the instances narrow it together (enchantTargetSlot below). Merged into
-- the two functions above, and passed to Target.fillableModes by Pawl.Engine.Cast
-- so castability accounts for it.
enchantSlotMap :: Face.Face Card.Card -> Map SlotName TargetSlot
enchantSlotMap = enchantSlotMapGiven . Face.enchant

-- enchantSlotMap over the instances themselves, so a PROJECTED list reaches the
-- same fold a printed one does; see modesTargetSlotsGiven above.
enchantSlotMapGiven :: [TargetSlot] -> Map SlotName TargetSlot
enchantSlotMapGiven enchants = case foldEnchant enchants of
  Nothing -> Map.empty
  Just slot -> Map.singleton enchantSlot slot

-- CR 702.5c: "If an Aura has multiple instances of enchant, all of them apply.
-- The Aura's target must follow the restrictions from all the instances of
-- enchant." This is where that rule lives -- the ONE target slot every reader of
-- an enchant ability gets, so CR 601.2c's target legality (Pawl.Engine.Cast), CR
-- 303.4c's admission re-check (Pawl.Engine.Sba.fallsOff) and CR 701.3a's attach
-- (Pawl.Engine.Attach.attachmentFor) cannot disagree about what "all of them"
-- means. Nothing for a face with no enchant ability at all, which is every
-- non-Aura (the CardSpec lint holds the biconditional).
--
-- The conjunction is Filter.And, which already means "matches all of these", so
-- no new predicate vocabulary is involved. A slot with no Filter narrows nothing
-- (TargetSlot's own note), so it contributes no conjunct rather than an empty
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
-- the rule (#797). A record update over the first instance's slot, so its pool
-- and its CR 115.6 requirement ride along; CR 702.5a's "Enchant [object or
-- player]" has no "up to" in it, so an enchant slot is always required.
enchantTargetSlot :: Face.Face card -> Maybe TargetSlot
enchantTargetSlot = foldEnchant . Face.enchant

-- The fold itself, over the instances rather than over a face: CR 702.5c's
-- conjunction has to answer the same way whether the instances were PRINTED or
-- GRANTED (Modification.GainEnchant), and this is the one place either
-- reaches. Pawl.Engine.Projection seeds ProjectedCharacteristics.enchant from
-- Face.enchant and appends grants to it, so a projected object's list is what
-- Pawl.Engine.Attach and Pawl.Engine.Sba fold. The cast and resolve paths take
-- the projected list too, through modesTargetSlotsGiven above: CR 702.103b's
-- bestow grants "enchant creature" to a spell ON THE STACK, which is exactly what
-- CR 601.2c and CR 608.2b then judge. enchantSlotMap's printed reading is what is
-- left for a caller holding a face and no object.
foldEnchant :: [TargetSlot] -> Maybe TargetSlot
foldEnchant slots = case slots of
  [] -> Nothing
  first : rest ->
    Just
      first
        { TargetSlot.filter = case Maybe.mapMaybe TargetSlot.filter (first : rest) of
            [] -> Nothing
            [one] -> Just one
            many -> Just (Filter.And many)
        }
