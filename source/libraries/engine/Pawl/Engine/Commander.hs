-- | CR 903, the Commander variant, as far as a commander in the command zone
-- reaches it: CR 903.3's designation, CR 903.6's starting zone, CR 903.8's cost
-- increase, CR 903.9's replacement sending a commander back to the command zone
-- instead of anywhere else, and CR 903.10a's twenty-one-damage loss.
--
-- A FORMAT and not a card, which is what makes this module the right home for
-- all five. No card's printed text says "cast this from the command zone" or
-- "this costs {2} more" -- rule 903 says both, of whatever card the player
-- designated. So none of this is a Pawl.Types.CastingPermission, a
-- Pawl.Types.PlayerEffect or a printed replacement: those are things a CARD
-- carries, and reading them here would be the rules core learning from data a
-- rule it already knows. Pawl.Engine.Cast.legendaryRestrictionOk makes the same
-- argument for CR 205.4e.
--
-- The designation is a PRINTING on the player, for the reason
-- Pawl.Types.Player.commander gives: CR 400.7 mints a fresh object id on every
-- zone change and a commander crosses zones constantly, so nothing keyed to an
-- object could survive its first cast.
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * CR 903.4's colour identity and CR 903.5's singleton deck construction
--     (#940) -- both are deck-legality rules, and pawl validates no deck.
--   * CR 903.12b, CR 903.12d and CR 903.12e's Brawl deck construction, and CR
--     903.13's Commander Draft -- deck-building rules, like rule 903.5's (#940).
module Pawl.Engine.Commander where

import qualified Control.Monad as Monad
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Subtype as Subtype.Engine
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PartnerText as PartnerText
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | CR 903.3 \/ CR 702.124: the cards this deck may designate as its
-- commanders. One designation is rule 903.3's; two are rule 702.124h's, and
-- only when EACH of them has partner -- "you can have two commanders if both
-- have partner" -- or rule 702.124i's, and only when both have the SAME
-- partner—[text] ability, or rule 702.124j's, and only when each names the
-- other, or rule 702.124k's, and only when one has "choose a
-- Background" and the other is a legendary Background enchantment card, or rule
-- 702.124m's, and only when one has "Doctor's companion" and the other is a
-- legendary Time Lord Doctor creature card with no other creature types.
--
-- Read off the FRONT FACE's printed keywords and type line rather than through
-- the projection, which is CR 702.124a: a partner ability "modifies the rules
-- for deck construction ... and functions before the game begins", so there is
-- no object for a continuous effect to have touched yet. Pawl.Engine.Vanguard
-- reads rule 902.4's card the same way and for the same reason.
--
-- More than two is empty for CR 702.124g: "no partner ability or combination of
-- partner abilities can ever let a player have more than two commanders". CR
-- 702.124f is why the limbs are separate disjuncts rather than one predicate:
-- "different partner abilities are distinct from one another and cannot be
-- combined", so a card with partner beside a Background is no pair, and neither
-- is partner beside partner—Friends forever.
--
-- Rule 702.124k's exclusion runs BOTH ways and both halves are here. A card with
-- "choose a Background" paired with anything that is not a legendary Background
-- enchantment card designates nothing, and a legendary Background enchantment
-- card is no commander "unless you have also designated a commander with 'choose
-- a Background'" -- which is why the ONE-card case below is gated too. Rule
-- 702.124k's second clause admits no exception for a Background named on its
-- own. That arm is FIRST, ahead of `soleCommander`: rule 702.124k's prohibition
-- is absolute, so a Background that also said it could be your commander (CR
-- 903.3a) would still be refused.
--
-- Empty is also what an ILLEGAL pair gets, which is the closest pawl can come to
-- rejecting the deck: nothing validates a deck (#940) and Pawl.Engine.Setup has
-- no channel to refuse one, so a pair rule 702.124 does not allow designates
-- neither card and starts no commander in the command zone.
--
-- Rule 702.124m's requirement is one-sided where rule 702.124k's is not. Rule
-- 702.124k forbids a Background named alone; rule 702.124m says nothing of the
-- kind about a Doctor, and a Doctor is a legendary creature card CR 903.3
-- designates on its own like any other. So the ONE-card case is gated for a
-- Background and not for a Doctor.
--
-- CR 903.3's own restriction on the card -- "a creature card, a Vehicle card, or
-- a Spacecraft card with one or more power\/toughness boxes" -- is asked of the
-- ONE-card case alone, and `soleCommander` is it. CR 702.124a is why no partner
-- limb shares it: "each partner ability has its own requirements for those two
-- commanders", and rule 702.124h, rule 702.124i and rule 702.124j ask for two
-- legendary CARDS, rule 702.124k for a Background enchantment and rule 702.124m
-- for two legendary creature cards. So Rowan Kenrith beside Will Kenrith is a
-- legal pair of planeswalkers on rule 702.124j's own words.
designations :: GameSettings.GameSettings -> Deck.Deck -> Set.Set Printing.Printing
designations settings deck =
  let named = Deck.commander deck
   in case Set.toList named of
        [] -> named
        [one] | isBackground one -> Set.empty
        [one] | soleCommander settings one -> named
        [_] -> Set.empty
        [_, _] | all hasPartner named -> named
        [a, b] | sharesPartnerText a b -> named
        [a, b] | namesEachOther a b -> named
        [a, b] | choosesBackground a && isBackground b -> named
        [a, b] | choosesBackground b && isBackground a -> named
        [a, b] | isDoctorsCompanion a && isTheDoctor b -> named
        [a, b] | isDoctorsCompanion b && isTheDoctor a -> named
        _ -> Set.empty

-- | CR 903.3's requirement of a deck's one commander: a legendary card that is
-- "either (a) a creature card, (b) a Vehicle card, or (c) a Spacecraft card with
-- one or more power\/toughness boxes" -- or, CR 903.3a, a card whose own ability
-- says it can be your commander.
--
-- Brawl adds a planeswalker card to those three, which is the whole of CR
-- 903.12c's difference from rule 903.3 and the reason this takes the settings. Pawl
-- validates no deck (#940), so a card this refuses is designated as nothing and
-- starts nowhere, which is `designations`' posture for every illegal deck.
--
-- Clauses (b) and (c) are REGRESSION FENCES and not proved: no legendary Vehicle
-- and no legendary Spacecraft is in the corpus (Dawnsire, Sunstar Dreadnought and
-- U.S.S. Enterprise-D, Galaxy-Class are what would prove them), so no test tells
-- either from the rule's absence. Clause (a), rule 903.3a and rule 903.12c are
-- each proved by a Pawl.CommanderSpec case.
soleCommander :: GameSettings.GameSettings -> Printing.Printing -> Bool
soleCommander settings printing =
  legendary printing
    && ( creatureCard printing
           || hasType CardType.Artifact Subtype.Vehicle printing
           || (hasType CardType.Artifact Subtype.Spacecraft printing && hasPowerToughnessBox printing)
           || (GameSettings.brawl settings && Set.member CardType.Planeswalker (printedTypes printing))
           || Face.canBeYourCommander (Card.frontFace (Printing.card printing))
       )

-- | CR 903.3(c)'s "with one or more power\/toughness boxes", which The Eternity
-- Elevator -- a legendary Spacecraft printing none -- is the card that conjunct
-- excludes.
--
-- Reads a station symbol's base-setting modification beside Face.power, because
-- CR 721.1 puts the box inside a striation and CR 721.2b makes it a static
-- ability: Lumen-Class Frigate's 3\/5 is transcribed as the {12+} ability's
-- Modification.SetBasePowerToughness, and Face.power is empty for CR 721.2c.
hasPowerToughnessBox :: Printing.Printing -> Bool
hasPowerToughnessBox printing =
  let face = Card.frontFace (Printing.card printing)
      sets ability = any isBaseSet (NonEmpty.toList (StaticAbility.modifications ability))
      isBaseSet modification = case modification of
        Modification.SetBasePowerToughness {} -> True
        _ -> False
   in Maybe.isJust (Face.power face) || any sets (Face.staticAbilities face)

-- | CR 903.3(a)'s "a creature card", asked of the card as it is in the COMMAND
-- ZONE rather than of its printed type line. Rule 903.3 judges the card before
-- the game begins, where CR 113.6c functions an ability that states which zones
-- it does NOT function in, and CR 903.6 puts the card it designates into the
-- command zone as the game begins. Grist, the Hunger Tide is the printing that
-- makes the two readings differ -- "as long as Grist isn't on the battlefield,
-- it's a 1\/1 Insect creature", which is why a planeswalker card is a legal
-- commander -- and Pawl.CommanderSpec's "CR 903.3 a legendary planeswalker that
-- is a creature card off the battlefield is designated" is the proof.
--
-- The command zone stands in for "before the game begins" because
-- Pawl.Types.StaticAbility.functionsFrom cannot tell rule 113.6b's positive zone
-- statement from rule 113.6c's negative one: both are a Set of Zone, so Anger's
-- graveyard and Grist's every-zone-but-the-battlefield have the same shape. A
-- card printed "as long as this card isn't in the command zone" would tell the
-- two apart; Scryfall o:"isn't in the command zone", 2026-09-18, has none.
--
-- Reads the modifications without asking `affected` or `condition`, which is
-- wider than the rule: an ability making some OTHER object a creature from the
-- command zone would count here. Grist's is the corpus's only ability that adds
-- a card type from that zone at all, and its affected set is its own source
-- (checked over data\/cards, 2026-09-18), so nothing is wrongly designated today.
creatureCard :: Printing.Printing -> Bool
creatureCard printing =
  let face = Card.frontFace (Printing.card printing)
      added =
        [ cardType
        | ability <- Face.staticAbilities face,
          Set.member Zone.Command (StaticAbility.functionsFrom ability),
          Modification.AddCardType cardType <- NonEmpty.toList (StaticAbility.modifications ability)
        ]
   in Set.member CardType.Creature (printedTypes printing)
        || elem CardType.Creature added

-- | CR 903.3(b) and CR 903.3(c)'s card kinds: a card type and one of its
-- subtypes, both off the printed front face for the reason `designations` gives.
--
-- The card TYPE is asked beside the subtype because CR 205.3g makes Vehicle and
-- Spacecraft artifact types, and CR 205.3n prints Spacecraft as a planar type as
-- well -- a subtype on its own would not tell the two apart.
hasType :: CardType.CardType -> Subtype.Subtype -> Printing.Printing -> Bool
hasType cardType subtype printing =
  Set.member cardType (printedTypes printing)
    && Set.member subtype (TypeLine.subtypes (Face.typeLine (Card.frontFace (Printing.card printing))))

-- | The card types printed on this card's front face, for the reason
-- `designations` gives.
printedTypes :: Printing.Printing -> Set.Set CardType.CardType
printedTypes printing = TypeLine.types (Face.typeLine (Card.frontFace (Printing.card printing)))

-- | CR 702.124h's requirement of one card of a pair.
hasPartner :: Printing.Printing -> Bool
hasPartner = printedKeyword Keyword.Partner

-- | CR 702.124i's requirement of the pair: the same partner—[text] ability on
-- both. An intersection rather than one ability each, for CR 702.124g: a card
-- with two partner abilities may use either.
sharesPartnerText :: Printing.Printing -> Printing.Printing -> Bool
sharesPartnerText a b = not (Set.null (Set.intersection (partnerTexts a) (partnerTexts b)))

-- | The partner—[text] abilities printed on this card's front face, for the
-- reason `designations` gives.
partnerTexts :: Printing.Printing -> Set.Set PartnerText.PartnerText
partnerTexts printing =
  Set.fromList [text | Keyword.PartnerText text <- Set.toList (Face.keywordSet (Card.frontFace (Printing.card printing)))]

-- | CR 702.124j's requirement of the pair: "two legendary cards ... if each has
-- a 'partner with [name]' ability with the other's name". Both memberships,
-- because the rule asks it of EACH card. Every printed pair names both ways, so
-- the two are proved only as a conjunction; no board tells them apart.
--
-- The legendary test is rule 702.124j's own word and is not idle the way it
-- would be under CR 702.124h: Ley Weaver and Lore Weaver name each other and are
-- not legendary, so they are the pair this conjunct alone refuses.
-- Pawl.CommanderSpec's "CR 702.124j the pair admits only two legendary cards
-- that name each other" is the proof.
namesEachOther :: Printing.Printing -> Printing.Printing -> Bool
namesEachOther a b =
  legendary a
    && legendary b
    && Set.member (printedName b) (partnerWithNames a)
    && Set.member (printedName a) (partnerWithNames b)

-- | The names this card's front face has a partner with ability for, for the
-- reason `designations` gives. A set because CR 702.124g lets one card carry
-- more than one partner ability.
partnerWithNames :: Printing.Printing -> Set.Set CardName.CardName
partnerWithNames printing =
  Set.fromList [name | Keyword.PartnerWith name <- Set.toList (Face.keywordSet (Card.frontFace (Printing.card printing)))]

-- | CR 702.124j's "[name]", matched against the printed front face's name -- CR
-- 201.2a's identity, and the same face every other limb here reads.
printedName :: Printing.Printing -> CardName.CardName
printedName = Face.name . Card.frontFace . Printing.card

-- | CR 702.124j's "two LEGENDARY cards". Narrower than `legendaryCreature`,
-- which CR 702.124m's limb needs: rule 702.124j asks only for a legendary card.
legendary :: Printing.Printing -> Bool
legendary printing =
  Set.member Supertype.Legendary (TypeLine.supertypes (Face.typeLine (Card.frontFace (Printing.card printing))))

-- | CR 702.124k's requirement of the card that names the other.
choosesBackground :: Printing.Printing -> Bool
choosesBackground = printedKeyword Keyword.ChooseABackground

-- | CR 702.124k's requirement of the card that is named: "a legendary Background
-- enchantment card". All three of legendary, enchantment and Background, since
-- rule 702.124k names all three -- Faceless One is the printing that would tell
-- an enchantment CREATURE apart, and it is legal because rule 702.124k asks for
-- an enchantment card and not for a card that is ONLY an enchantment.
isBackground :: Printing.Printing -> Bool
isBackground printing =
  let face = Card.frontFace (Printing.card printing)
      typeLine = Face.typeLine face
   in Set.member Supertype.Legendary (TypeLine.supertypes typeLine)
        && Set.member CardType.Enchantment (TypeLine.types typeLine)
        && Set.member Subtype.Background (TypeLine.subtypes typeLine)

-- | CR 702.124m's requirement of the card that names the other: "this card",
-- which rule 702.124m also has be a legendary creature card, since it designates
-- "two legendary creature cards".
isDoctorsCompanion :: Printing.Printing -> Bool
isDoctorsCompanion printing = legendaryCreature printing && printedKeyword Keyword.DoctorsCompanion printing

-- | CR 702.124m's requirement of the card that is named: "a legendary Time Lord
-- Doctor creature card that has no other creature types".
--
-- The last clause is why this counts the creature types rather than testing two
-- memberships: Susan Foreman is a legendary Time Lord that is no Doctor, and a
-- legendary Time Lord Doctor Warrior would satisfy both memberships and still be
-- refused. Subtype.isCreatureType is what keeps an artifact creature's Equipment
-- or a Vehicle's subtypes out of the count -- rule 702.124m bounds the CREATURE
-- types, not the subtypes.
isTheDoctor :: Printing.Printing -> Bool
isTheDoctor printing =
  let face = Card.frontFace (Printing.card printing)
      creatureTypes = Set.filter Subtype.Engine.isCreatureType (TypeLine.subtypes (Face.typeLine face))
   in legendaryCreature printing && creatureTypes == Set.fromList [Subtype.TimeLord, Subtype.Doctor]

-- | CR 702.124m's "two legendary CREATURE cards", which both halves of that
-- rule's pair must be. Rule 702.124h asks only for a legendary card and rule
-- 702.124k for a legendary enchantment, so neither limb above shares this.
legendaryCreature :: Printing.Printing -> Bool
legendaryCreature printing =
  legendary printing
    && Set.member CardType.Creature (TypeLine.types (Face.typeLine (Card.frontFace (Printing.card printing))))

-- | CR 702.124a: a partner ability is read off the printed front face, for the
-- reason `designations` gives.
printedKeyword :: Keyword.Keyword -> Printing.Printing -> Bool
printedKeyword keyword printing =
  Set.member keyword (Face.keywordSet (Card.frontFace (Printing.card printing)))

-- | CR 903.3: record which cards this player designated as their commanders.
-- Called once per designation by Pawl.Engine.Setup.createDeck, from the Deck.
designate :: PlayerId -> PrintingId.PrintingId -> GameState -> GameState
designate pid printingId gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.commander = Set.insert printingId (Player.commander p)}) pid (GameState.players gs)}

-- | CR 903.3: is this object its owner's commander?
--
-- Asked of the OWNER and never the controller, because rule 903.3 designates a
-- card from the deck its owner brought -- a stolen commander is still its owner's
-- commander, which is also what makes CR 903.9 send it to its owner's command
-- zone rather than the thief's.
isCommander :: ObjectId -> GameState -> Bool
isCommander oid gs = Maybe.isJust (commanderPrintingOf oid gs)

-- | CR 903.3's designation, read off whichever card represents this object: the
-- printing of the owner's commander when one of them is it, and Nothing
-- otherwise.
--
-- Matches on the PRINTING ID, which is exact under CR 903.5's singleton rule and
-- is the only thing that survives CR 400.7's fresh id. Setup.createDeck is what
-- makes the comparison sound: it interns a deck's distinct printings once, so
-- the designation and the object name the same entry. Pawl does not enforce rule
-- 903.5 (#940), so a deck holding two copies of its commander would have both
-- answer True here.
--
-- CARDS in the plural, which is CR 903.9c: "if a commander is a melded permanent
-- or a merged permanent" -- so the rules do call such a permanent a commander,
-- and the designation sits on one of the cards representing it rather than on
-- the permanent. Game.componentsOf is the plural read, a classifier over Source
-- and never a case on Source.OfMeld, so CR 730.3's merged permanent arrives
-- through the same door.
--
-- What this answers FOR a melded permanent is the component card, which is
-- exactly what CR 903.9c's procedure needs to single out -- see
-- `commandZoneComponent`. Rule 903.10a's tally is keyed by that same answer, so
-- a melded commander's combat damage lands under the card that is the
-- commander; rule 903.8's cast permission wants only the Bool, and cannot be
-- reached by a melded permanent at all, since none is ever in the command zone
-- to be cast from.
commanderPrintingOf :: ObjectId -> GameState -> Maybe PrintingId.PrintingId
commanderPrintingOf oid gs = do
  obj <- Game.lookupObject oid gs
  designated <- fmap Player.commander (Map.lookup (Object.owner obj) (GameState.players gs))
  let cards = case Object.source obj of
        Source.OfCard printingId -> Seq.singleton printingId
        -- CR 903.9c names "the card that represents it and is a commander", so a
        -- merged permanent's component that is no card is filtered out rather
        -- than compared: neither a token (CR 111.6) nor CR 730.2's copy of a
        -- spell (CR 707.10) is one, and either interned to the same printing as a
        -- designated card would otherwise match.
        --
        -- The COPY half of that is a regression fence: `Foldable.find` below
        -- answers the same printing whether or not a copy interned under it is in
        -- the list, so only Pawl.Engine.Event's rule 903.9c split observes the
        -- filter -- Pawl.MutateSpec's "CR 903.9c/111.6/707.10 a merged
        -- commander's token and copy components are not split off to the command
        -- zone" is that case.
        source -> fmap Game.printingOfComponent (Seq.filter Game.componentIsCard (Game.componentsOf source))
  -- CR 702.124e: with two designations this answers WHICH of them this object
  -- is, and no object can be both -- rule 903.5b's singleton deck gives each
  -- designation a distinct printing.
  Foldable.find (\printingId -> Set.member printingId designated) cards

-- | CR 903.9c: the one card of a melded or merged permanent that goes to the
-- command zone when its owner accepts CR 903.9b's offer -- "the card that
-- represents it and is a commander" -- or Nothing when the object is a single
-- card and the whole of it moves instead.
--
-- Gated on the object HAVING components rather than on `commanderPrintingOf`
-- alone: an ordinary commander card is its own designation, and rule 903.9c's
-- split has nothing to divide there.
commandZoneComponent :: ObjectId -> GameState -> Maybe PrintingId.PrintingId
commandZoneComponent oid gs = do
  obj <- Game.lookupObject oid gs
  Monad.guard (not (Seq.null (Game.componentsOf (Object.source obj))))
  commanderPrintingOf oid gs

-- | CR 903.10a / CR 704.6c: "a player who's been dealt 21 or more combat damage
-- by the same commander over the course of the game loses the game". The
-- predicate Pawl.Engine.Sba.losesNow reads, kept here for the reason this
-- module's header gives -- Sba owns WHEN a state-based action is checked, not
-- what each one means, the same split it takes with CR 704.5aa and
-- Pawl.Engine.Speed.
--
-- The MAXIMUM over the tally and never its sum, which is the whole of "by the
-- SAME commander": two commanders that between them dealt 24 have killed
-- nobody. Player.commanderDamage is keyed per commander (CR 702.124d), so that
-- holds of a partner deck's pair as much as of two opponents'.
-- Pawl.CommanderSpec's "CR 702.124d two partners dealing eleven each kill
-- nobody" is the proof.
--
-- ">= 21" and not "== 21", because rule 903.10a says "21 or more" and one
-- damage event can carry the difference on its own.
--
-- CR 903.12h removes the rule outright in a Brawl game -- "Brawl games do not
-- use the state-based action described in rule 704.6c" -- so the option is
-- tested before the tally rather than folded into the threshold: there is no
-- number of commander damage that loses a Brawl game, and a higher threshold
-- would be a different rule. The tally itself is still kept (CR 903.10a's
-- "over the course of the game" is not what 903.12h switches off), so a card
-- that reads it still can.
lethalDamage :: PlayerId -> GameState -> Bool
lethalDamage pid gs
  | GameSettings.brawl (GameState.settings gs) = False
  | otherwise = case Map.lookup pid (GameState.players gs) of
      Nothing -> False
      Just player -> any (>= 21) (Map.elems (Player.commanderDamage player))

-- | CR 903.8: "a commander's owner may cast it from the command zone for its mana
-- cost plus {2} for each previous time they cast it from the command zone this
-- game". This is the {2}-per-previous-cast part, as an amount of GENERIC mana.
--
-- Zero when the object is not a commander, when its owner is not the caster, or
-- when it has never been cast from there -- so every ordinary spell in every
-- ordinary game gets 0 and the tax costs nothing to ask about.
--
-- Read as a CR 601.2f cost INCREASE by Pawl.Engine.Cost, alongside the ones cards
-- generate. It belongs on that side of rule 601.2f rather than being folded into
-- the mana cost, because rule 903.8 says "plus {2}" -- an increase applied after
-- the cost is determined, so a cost reduction applies to the total afterwards
-- exactly as CR 601.2f orders them.
tax :: PlayerId -> ObjectId -> GameState -> Natural
tax pid oid gs
  | not (canCastFromCommandZone pid oid gs) = 0
  -- Rule 903.8 taxes casting it FROM THE COMMAND ZONE, so the object has to be
  -- there NOW. Without this the tax would follow the card everywhere: a commander
  -- returned to its owner's hand and cast from there is cast for its printed cost,
  -- and so is one flickered back onto the battlefield. It also keeps every
  -- ACTIVATED ability off the tax, since Pawl.Engine.Cost.total is asked about
  -- those too and a commander on the battlefield is not in the command zone.
  | not (Set.member oid (GameState.command gs)) = 0
  -- CR 702.124d: "when casting a commander with partner, ignore how many times
  -- your other commander has been cast", so the count is the one kept against
  -- THIS designation. `commanderPrintingOf` cannot be Nothing past the guard
  -- above, which is `isCommander`.
  | otherwise = 2 * maybe 0 (\printingId -> castCount pid printingId gs) (commanderPrintingOf oid gs)

-- | CR 903.8's permission half: may this player cast this object from the command
-- zone? Only its OWNER may, and only if it is their commander -- rule 903.8 says
-- "a commander's owner may cast it from the command zone", and nothing else in
-- the pool is castable from there at all.
canCastFromCommandZone :: PlayerId -> ObjectId -> GameState -> Bool
canCastFromCommandZone pid oid gs =
  isCommander oid gs && fmap Object.owner (Game.lookupObject oid gs) == Just pid

-- | CR 903.8's "each previous time they cast IT from the command zone this
-- game", asked of one designation: CR 702.124d has a partner deck's other
-- commander ignore this one's casts.
castCount :: PlayerId -> PrintingId.PrintingId -> GameState -> Natural
castCount pid printingId gs =
  maybe 0 (Map.findWithDefault 0 printingId . Player.commanderCasts) (Map.lookup pid (GameState.players gs))

-- | CR 903.8's tax as a function on ONE candidate cost: add {2} per previous cast
-- to its mana part, or leave it alone when there is no tax to add.
--
-- Takes the PRE-MOVE object, because `tax` asks whether it is in the command zone
-- and CR 601.2a's move has not happened yet at the only call site
-- (Pawl.Engine.Cast.castSpell), which is the whole reason this exists rather than
-- the tax being left to Cost.total.
--
-- A cost with no mana part (Nothing, CR 118.6a's unpayable) stays unpayable: fmap
-- leaves it alone rather than inventing a {2} cost for it.
taxCandidates :: PlayerId -> ObjectId -> GameState -> Cost.Cost Keyword.Keyword -> Cost.Cost Keyword.Keyword
taxCandidates pid oid gs cost =
  let owed = tax pid oid gs
      add (ManaCost.MkManaCost symbols) = ManaCost.MkManaCost (symbols <> [ManaSymbol.Generic owed])
   in if owed == 0 then cost else cost {Cost.mana = fmap add (Cost.mana cost)}

-- | CR 903.9a: the commanders their owners may put into the command zone right
-- now, paired with the owner to ask.
--
-- Rule 903.9a has two conditions and both are here. The object must be IN a
-- graveyard or in exile, and it must have arrived there "since the last time
-- state-based actions were checked" -- which is what keeps a declining owner from
-- being asked again on every subsequent check, and what makes the offer land once
-- per arrival. The watermark is GameState.damageScannedThrough, which is not a
-- damage-only mark despite its name: it is how far the STATE-BASED ACTION CHECK
-- has consumed the event log, the same boundary CR 704.5h reads and the same one
-- rule 903.9a's "since the last time" names.
--
-- Keyed off the move's ARRIVALS, because those are the incarnations now sitting
-- in the graveyard; ZoneChange.departed named the one that left the battlefield
-- and no longer exists.
--
-- Moved.arrivals and not ZoneChange.object alone, which is CR 903.9a read over
-- CR 712.21's split rather than CR 903.9c: rule 903.9c governs only the CR 903.9b
-- replacement, while this is the state-based action, and a melded
-- permanent's departure puts TWO cards into the graveyard or exile. Rule 903.9a
-- asks its question of each object in that zone, so each arrival is asked, and the
-- one that is a commander may be either of them. Reading the first alone would
-- offer the return when the commander half melded first and silently not when it
-- melded second. `isCommander` then keeps exactly the commander card.
--
-- NOT a replacement effect. Rule 903.9a says "this is a state-based action" in so
-- many words, which is why it is classified in Pawl.Engine.Sba beside CR 704.5's
-- own list rather than installed as a Pawl.Types.ReplacementEffect. Its sibling CR
-- 903.9b -- hand and library, which IS a replacement -- is `commandZoneOffer`
-- below, asked from the zone-change funnel instead.
--
-- Takes the unscanned events rather than reading them off the state, which is
-- what keeps this module free of Pawl.Engine.Event: rule 903.9b's half is called
-- FROM that module, so the dependency runs the other way now. Pawl.Engine.Sba
-- passes Event.unscannedSbaEvents, the watermark it owns anyway.
returnable :: [GameEvent] -> GameState -> [(PlayerId, ObjectId)]
returnable events gs =
  let arrivals = concatMap arrivalsOf events
      arrivalsOf event = case event of
        GameEvent.Moved m
          | elem (ZoneChange.to (Moved.change m)) [Zone.Graveyard, Zone.Exile] -> Foldable.toList (Moved.arrivals m)
        _ -> []
      stillThere oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> elem (Object.zone obj) [Zone.Graveyard, Zone.Exile]
      offer oid = case Game.lookupObject oid gs of
        Just obj | isCommander oid gs && stillThere oid -> Just (Object.owner obj, oid)
        _ -> Nothing
   in Maybe.mapMaybe offer (ListUtils.nubOrd arrivals)

-- | CR 903.9b: "if a commander would be put into its owner's hand or library from
-- anywhere, its owner may put it into the command zone instead". This is the
-- CONDITION half -- the owner to ask, or Nothing when the rule has nothing to say
-- about this move -- and Pawl.Engine.Event.offerCommandZone is the question.
--
-- Asked of ZoneChange.departed, the id that still exists: the proposed event has
-- not moved anything yet, so `departed` and `object` are the same value there (see
-- Pawl.Types.ZoneChange), and naming the departing one is what keeps this correct
-- if that ever stops being true. `returnable` above takes the opposite id for the
-- opposite reason -- rule 903.9a asks about an object that has ALREADY arrived.
--
-- Both destinations, because rule 903.9b names both and a fix handling only the
-- hand would leave "put target creature on top of its owner's library" (Griptide)
-- unoffered. CR 400.3 is why no owner check on the destination is needed: an
-- object headed for any hand or library goes to its OWNER's, so every move this
-- admits is already the rule's.
--
-- CR 903.9b's "may apply more than once to the same event", its named exception
-- to CR 614.5, needs nothing here: this is asked once per proposed zone change,
-- and CR 608.2f's batch reaches the funnel one member at a time, so a spell
-- returning two commanders in one event puts the question to each owner.
-- Pawl.CommanderSpec's "the offer applies once per commander in the same event"
-- is the proof, over Evacuation.
--
-- CR 903.9c's melded or merged commander is the ANSWER's other shape rather than
-- a second question, and Pawl.Engine.Event.offerCommandZone is where it is read.
--
-- Not implemented: a place for the offer in the CR 616.1 ordering, where CR
-- 616.1e leaves the affected player free to pick among applicable effects (#2266).
commandZoneOffer :: ZoneChange.ZoneChange -> GameState -> Maybe PlayerId
commandZoneOffer zc gs
  | notElem (ZoneChange.to zc) [Zone.Hand, Zone.Library] = Nothing
  | not (isCommander (ZoneChange.departed zc) gs) = Nothing
  | otherwise = fmap Object.owner (Game.lookupObject (ZoneChange.departed zc) gs)

-- | CR 903.8's counter, bumped as the cast is announced. Called by
-- Pawl.Engine.Cast only when the spell left the COMMAND ZONE -- a commander cast
-- from a hand or a graveyard makes no later cast dearer, which is what rule
-- 903.8's "from the command zone" restricts.
--
-- Counted against THIS commander (CR 702.124d), which is why it takes an object
-- rather than the player alone. The object is the SPELL on the stack, CR 601.2a
-- having already moved the card there: rule 903.3's designation is a printing,
-- and Source.OfCard carries it across CR 400.7's fresh incarnation, so the spell
-- answers `commanderPrintingOf` exactly as the card in the command zone did.
--
-- Does nothing when the object is not a commander, which its one caller has
-- already ruled out by the zone it was cast from.
recordCast :: PlayerId -> ObjectId -> GameState -> GameState
recordCast pid oid gs = case commanderPrintingOf oid gs of
  Nothing -> gs
  Just printingId ->
    gs
      { GameState.players =
          Map.adjust (\p -> p {Player.commanderCasts = Map.insertWith (+) printingId 1 (Player.commanderCasts p)}) pid (GameState.players gs)
      }
