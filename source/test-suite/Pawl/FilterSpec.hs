-- Covers Pawl.Types.Filter, Pawl.Types.PlayerRelation, Pawl.Engine.Filter.
module Pawl.FilterSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
-- Aliased Filter.Type, not Type, because the evaluator module Pawl.Engine.Filter
-- already claims the alias Filter (a documented exception to alias-to-last-
-- component, per the M4.5 P9 plan's global constraints).
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- A projected black creature controlled by player 0.
blackCreature :: Filter.View
blackCreature =
  Filter.MkView
    { Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.singleton Color.Black,
      Filter.subtypes = Set.singleton Subtype.Zombie,
      Filter.keywords = Set.singleton Keyword.Flying,
      Filter.power = Just 2,
      Filter.manaValue = Just 3,
      Filter.controller = Just (PlayerId.MkPlayerId 0),
      Filter.identity = Just (ObjectId.MkObjectId 7),
      Filter.playerIdentity = Nothing,
      Filter.attacking = False,
      Filter.blocking = False,
      Filter.attackedThisTurn = False,
      Filter.attachedToCreature = False,
      Filter.attachedToPermanent = False,
      Filter.canHostSubject = False,
      Filter.token = False,
      Filter.tapped = False,
      Filter.counters = Map.empty
    }

-- A colourless (devoid) creature with power 5, no controller recorded.
devoidBigCreature :: Filter.View
devoidBigCreature =
  Filter.MkView
    { Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.empty,
      Filter.subtypes = Set.empty,
      Filter.keywords = Set.empty,
      Filter.power = Just 5,
      Filter.manaValue = Just 5,
      Filter.controller = Nothing,
      Filter.identity = Nothing,
      Filter.playerIdentity = Nothing,
      Filter.attacking = False,
      Filter.blocking = False,
      Filter.attackedThisTurn = False,
      Filter.attachedToCreature = False,
      Filter.attachedToPermanent = False,
      Filter.canHostSubject = False,
      Filter.token = False,
      Filter.tapped = False,
      Filter.counters = Map.empty
    }

-- A creature whose only ability is the given keyword -- the toxic N and landwalk
-- views the family/instance cases below are asked about. Off blackCreature, so
-- every other axis is a fixed background and only the keyword set varies.
withKeyword :: Keyword.Keyword -> Filter.View
withKeyword keyword = blackCreature {Filter.keywords = Set.singleton keyword}

self :: Filter.Context
self = Filter.MkContext (Just (PlayerId.MkPlayerId 0)) Nothing

other :: Filter.Context
other = Filter.MkContext (Just (PlayerId.MkPlayerId 1)) Nothing

noPerspective :: Filter.Context
noPerspective = Filter.MkContext Nothing Nothing

-- The player candidate every "vacuously false" case below is asked about.
aPlayer :: Filter.View
aPlayer = Filter.playerView (PlayerId.MkPlayerId 0)

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Engine.Filter" $ do
  Spec.it s "HasCardType matches when present" $ do
    Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.HasCardType CardType.Creature)) "creature"

  Spec.it s "HasCardType fails when absent" $ do
    Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.HasCardType CardType.Land))) "not land"

  Spec.it s "HasColor matches Black creature" $ do
    Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.HasColor Color.Black)) "black"

  Spec.it s "Not HasColor Black is Doom Blade's narrowing" $ do
    Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.Not (Filter.Type.HasColor Color.Black)))) "black is illegal"
    Spec.assertBool s (Filter.matches self devoidBigCreature (Filter.Type.Not (Filter.Type.HasColor Color.Black))) "devoid is legal"

  Spec.it s "And [] is the trivial predicate (matches everything)" $ do
    Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.And [])) "trivial"

  Spec.it s "Terror: And of two negated atoms" $ do
    let terror = Filter.Type.And [Filter.Type.Not (Filter.Type.HasColor Color.Black), Filter.Type.Not (Filter.Type.HasCardType CardType.Artifact)]
    Spec.assertBool s (not (Filter.matches self blackCreature terror)) "black creature fails"
    Spec.assertBool s (Filter.matches self devoidBigCreature terror) "devoid creature passes"

  Spec.it s "Or matches when either arm matches" $ do
    Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasCardType CardType.Enchantment])) "creature or enchantment"

  -- CR 702.1 gives an ability a NAME and rule 702.164a says toxic "is written
  -- 'toxic N'". The two atoms ask about those two things, and this group is what
  -- keeps them apart: widening HasKeyword to cover both would make the first
  -- three assertions below fail, and dropping HasKeywordFamily would make the
  -- rest unwritable. Both readings have a producer -- Quagmire names swampwalk,
  -- Flensing Raptor names toxic -- so neither may absorb the other (#522).
  Spec.describe s "the keyword family and the written instance" $ do
    -- CR 702.164a. `HasKeyword (Toxic 2)` is toxic 2 and no other N: the
    -- projection is keyed by the whole keyword, so toxic 1 and toxic 3 are
    -- different keys and neither is this one.
    Spec.it s "HasKeyword asks about one written instance" $ do
      Spec.assertBool s (Filter.matches self (withKeyword (Keyword.Toxic 2)) (Filter.Type.HasKeyword (Keyword.Toxic 2))) "toxic 2 is toxic 2"
      Spec.assertBool s (not (Filter.matches self (withKeyword (Keyword.Toxic 1)) (Filter.Type.HasKeyword (Keyword.Toxic 2)))) "toxic 1 is not toxic 2"
      Spec.assertBool s (not (Filter.matches self (withKeyword (Keyword.Toxic 3)) (Filter.Type.HasKeyword (Keyword.Toxic 2)))) "toxic 3 is not toxic 2"

    -- The same three creatures, the other question. This is the issue's own
    -- success criterion: ONE filter reaching toxic 1, toxic 2 and toxic 3 alike.
    Spec.it s "HasKeywordFamily asks about the ability, whatever its N" $ do
      Spec.assertBool s (Filter.matches self (withKeyword (Keyword.Toxic 1)) (Filter.Type.HasKeywordFamily KeywordFamily.Toxic)) "toxic 1 has toxic"
      Spec.assertBool s (Filter.matches self (withKeyword (Keyword.Toxic 2)) (Filter.Type.HasKeywordFamily KeywordFamily.Toxic)) "toxic 2 has toxic"
      Spec.assertBool s (Filter.matches self (withKeyword (Keyword.Toxic 3)) (Filter.Type.HasKeywordFamily KeywordFamily.Toxic)) "toxic 3 has toxic"

    -- Not a predicate that says yes to everything: a nullary keyword has no
    -- family constructor at all, so flying can only ever answer this False.
    Spec.it s "a family matches nothing outside it" $ do
      Spec.assertBool s (not (Filter.matches self (withKeyword Keyword.Flying) (Filter.Type.HasKeywordFamily KeywordFamily.Toxic))) "flying is not toxic"
      Spec.assertBool s (not (Filter.matches self (withKeyword (Keyword.Poisonous 2)) (Filter.Type.HasKeywordFamily KeywordFamily.Toxic))) "CR 702.70a poisonous 2 is not toxic 2"

    -- CR 702.14a's generic term, and the reason the exact atom had to survive:
    -- Quagmire is "creatures with SWAMPWALK", not creatures with landwalk, and
    -- CR 702.14d says landwalk abilities don't cancel one another -- so an
    -- islandwalker is a different ability, not a different value of the same one.
    Spec.it s "CR 702.14a swampwalk is not islandwalk, but both are landwalk" $ do
      let swampwalk = Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Swamp)
          islandwalk = Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Island)
      Spec.assertBool s (Filter.matches self (withKeyword swampwalk) (Filter.Type.HasKeyword swampwalk)) "Quagmire reaches swampwalk"
      Spec.assertBool s (not (Filter.matches self (withKeyword islandwalk) (Filter.Type.HasKeyword swampwalk))) "and not islandwalk"
      Spec.assertBool s (Filter.matches self (withKeyword swampwalk) (Filter.Type.HasKeywordFamily KeywordFamily.Landwalk)) "Staff of the Ages reaches swampwalk"
      Spec.assertBool s (Filter.matches self (withKeyword islandwalk) (Filter.Type.HasKeywordFamily KeywordFamily.Landwalk)) "and islandwalk too"

  Spec.it s "PowerAtLeast compares projected power" $ do
    Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.PowerAtLeast 4))) "power 2 < 4"
    Spec.assertBool s (Filter.matches self devoidBigCreature (Filter.Type.PowerAtLeast 4)) "power 5 >= 4"

  Spec.it s "PowerAtLeast is False when power is Nothing" $ do
    let noPower = blackCreature {Filter.power = Nothing}
    Spec.assertBool s (not (Filter.matches self noPower (Filter.Type.PowerAtLeast 1))) "no power"

  -- CR 208.1 read as a ceiling: Ezuri, Claw of Progress' "power 2 or less".
  -- The bound is INCLUSIVE, which is the printed "or less", so the 2 that
  -- PowerAtLeast 4 declines is admitted here and the 5 is not.
  Spec.it s "PowerAtMost compares projected power" $ do
    Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.PowerAtMost 2)) "power 2 <= 2"
    Spec.assertBool s (not (Filter.matches self devoidBigCreature (Filter.Type.PowerAtMost 2))) "power 5 > 2"

  -- NOT the negation of PowerAtLeast, which is the whole reason it is a separate
  -- atom: an object with no power answers False to both, where `Not (PowerAtLeast
  -- 3)` would admit it.
  Spec.it s "PowerAtMost is False when power is Nothing" $ do
    let noPower = blackCreature {Filter.power = Nothing}
    Spec.assertBool s (not (Filter.matches self noPower (Filter.Type.PowerAtMost 99))) "no power"
    Spec.assertBool s (Filter.matches self noPower (Filter.Type.Not (Filter.Type.PowerAtLeast 99))) "where the negation of PowerAtLeast admits it"

  -- CR 202.3, a ceiling on a different characteristic: Ojutai's Command's
  -- "mana value 2 or less".
  Spec.it s "ManaValueAtMost compares the mana value" $ do
    Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.ManaValueAtMost 3)) "mana value 3 <= 3"
    Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.ManaValueAtMost 2))) "mana value 3 > 2"

  -- CR 202.3a: a mana value of 0 is a real answer, not a missing one, so the
  -- bound holds at zero rather than falling through to the Nothing arm below.
  Spec.it s "ManaValueAtMost holds for a mana value of 0" $ do
    let free = blackCreature {Filter.manaValue = Just 0}
    Spec.assertBool s (Filter.matches self free (Filter.Type.ManaValueAtMost 0)) "0 <= 0"

  Spec.it s "ManaValueAtMost is False when the mana value is Nothing" $ do
    let noCost = blackCreature {Filter.manaValue = Nothing}
    Spec.assertBool s (not (Filter.matches self noCost (Filter.Type.ManaValueAtMost 99))) "no mana value"

  Spec.it s "ManaValueAtMost is False for a player" $ do
    Spec.assertBool s (not (Filter.matches self aPlayer (Filter.Type.ManaValueAtMost 99))) "player"

  Spec.it s "ControlledBy You holds for own object" $ do
    Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.ControlledBy PlayerRelation.You)) "you"

  Spec.it s "ControlledBy You fails from an opponent's perspective" $ do
    Spec.assertBool s (not (Filter.matches other blackCreature (Filter.Type.ControlledBy PlayerRelation.You))) "not you"

  Spec.it s "ControlledBy Opponent holds across differing players" $ do
    Spec.assertBool s (Filter.matches other blackCreature (Filter.Type.ControlledBy PlayerRelation.Opponent)) "opponent"

  Spec.it s "ControlledBy is False when the object has no controller" $ do
    Spec.assertBool s (not (Filter.matches self devoidBigCreature (Filter.Type.ControlledBy PlayerRelation.Opponent))) "no controller"

  Spec.it s "ControlledBy is False when the context has no perspective" $ do
    Spec.assertBool s (not (Filter.matches noPerspective blackCreature (Filter.Type.ControlledBy PlayerRelation.You))) "no perspective"

  Spec.describe s "IsSource" $ do
    Spec.it s "matches the context's source" $ do
      Spec.assertBool
        s
        (Filter.matches (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7))) blackCreature Filter.Type.IsSource)
        "is the source"

    Spec.it s "does not match a different object" $ do
      Spec.assertBool
        s
        (not (Filter.matches (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 8))) blackCreature Filter.Type.IsSource))
        "not the source"

    Spec.it s "no source in context is vacuously false" $ do
      Spec.assertBool
        s
        (not (Filter.matches (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) Nothing) blackCreature Filter.Type.IsSource))
        "no source"

    Spec.it s "no identity in view is vacuously false" $ do
      Spec.assertBool
        s
        (not (Filter.matches (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7))) devoidBigCreature Filter.Type.IsSource))
        "no identity"

  Spec.describe s "IsAttacking" $ do
    Spec.it s "matches a view whose combat status says so" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.attacking = True}) Filter.Type.IsAttacking) "attacking"

    Spec.it s "does not match a creature that is not attacking" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.IsAttacking)) "not attacking"

    -- CR 109.3: combat status is not a characteristic, so no other axis of
    -- the view can stand in for it. A 5-power creature is no more attacking
    -- than a 2-power one.
    Spec.it s "is independent of every characteristic axis" $ do
      Spec.assertBool s (not (Filter.matches self devoidBigCreature Filter.Type.IsAttacking)) "power does not imply attacking"

    -- CR 506.3: only a creature can attack, and a player is not one.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.IsAttacking)) "player"

  Spec.describe s "IsBlocking" $ do
    Spec.it s "matches a view whose combat status says so" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.blocking = True}) Filter.Type.IsBlocking) "blocking"

    Spec.it s "does not match a creature that is not blocking" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.IsBlocking)) "not blocking"

    -- The two combat roles are independent in BOTH directions, which is
    -- why Labyrinth of Skophos' "attacking or blocking" needs two atoms
    -- rather than one: CR 508.1k confers the first at the attacker
    -- declaration and CR 509.1g the second at the blocker declaration,
    -- and neither says anything about the other.
    Spec.it s "is not implied by attacking" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.attacking = True}) Filter.Type.IsBlocking)) "attacking does not imply blocking"

    Spec.it s "does not imply attacking" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.blocking = True}) Filter.Type.IsAttacking)) "blocking does not imply attacking"

    -- Labyrinth of Skophos' own filter, over each role in turn.
    Spec.it s "Or [IsAttacking, IsBlocking] admits either role and rejects a creature in neither" $ do
      let both = Filter.Type.Or [Filter.Type.IsAttacking, Filter.Type.IsBlocking]
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.attacking = True}) both) "attacker"
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.blocking = True}) both) "blocker"
      Spec.assertBool s (not (Filter.matches self blackCreature both)) "neither"

    -- CR 509.1a: only a creature can be chosen to block, and a player is
    -- not one.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.IsBlocking)) "player"

  -- CR 612.1: a text-changing effect applies to "any words or symbols
  -- printed on that object", and HasSubtype is the only atom that can
  -- carry a basic land type. Threaded into effects by Resolve.rewriteEffect.
  Spec.describe s "rewrite" $ do
    Spec.it s "swaps the named subtype word" $ do
      Spec.assertEqWith
        s
        "Island became Forest"
        (Filter.rewrite [(Subtype.Island, Subtype.Forest)] (Filter.Type.HasSubtype Subtype.Island))
        (Filter.Type.HasSubtype Subtype.Forest)

    Spec.it s "leaves an unnamed subtype word alone" $ do
      Spec.assertEqWith
        s
        "Wall untouched"
        (Filter.rewrite [(Subtype.Island, Subtype.Forest)] (Filter.Type.HasSubtype Subtype.Wall))
        (Filter.Type.HasSubtype Subtype.Wall)

    Spec.it s "recurses through And, Or and Not" $ do
      let before = Filter.Type.And [Filter.Type.Not (Filter.Type.HasSubtype Subtype.Island), Filter.Type.Or [Filter.Type.HasSubtype Subtype.Island, Filter.Type.IsAttacking]]
          after = Filter.Type.And [Filter.Type.Not (Filter.Type.HasSubtype Subtype.Forest), Filter.Type.Or [Filter.Type.HasSubtype Subtype.Forest, Filter.Type.IsAttacking]]
      Spec.assertEqWith s "every occurrence" (Filter.rewrite [(Subtype.Island, Subtype.Forest)] before) after

    -- CR 612.1 changes WORDS, and a card type is not a subtype word.
    Spec.it s "leaves an atom that names no subtype alone" $ do
      Spec.assertEqWith
        s
        "card type untouched"
        (Filter.rewrite [(Subtype.Island, Subtype.Forest)] (Filter.Type.HasCardType CardType.Creature))
        (Filter.Type.HasCardType CardType.Creature)

    -- CR 702.14a: landwalk "appears within an object's rules text as
    -- '[type]walk'", so "creature with islandwalk" holds a land-type word one
    -- level down. No card in the pool filters by a landwalk yet; the
    -- gameplay-level proof of the same descent is CombatSpec's
    -- TextChangedLandwalk, which reaches rewriteKeyword through a GRANT.
    Spec.it s "descends into a keyword's own land type" $ do
      Spec.assertEqWith
        s
        "islandwalk became forestwalk"
        (Filter.rewrite [(Subtype.Island, Subtype.Forest)] (Filter.Type.HasKeyword (Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Island))))
        (Filter.Type.HasKeyword (Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Forest)))

    -- CR 702.14a's second clause: the [type] "can also be the card type land
    -- plus any combination of land types, card types, and/or supertypes". Two
    -- of CR 702.14c's four shapes carry no subtype word at all, and one carries
    -- a subtype beside a supertype -- so the swap must reach the Swamp in snow
    -- swampwalk and nothing in nonbasic landwalk.
    Spec.it s "leaves a landwalk criterion that names no land type alone" $ do
      Spec.assertEqWith
        s
        "nonbasic landwalk untouched"
        (Filter.rewrite [(Subtype.Swamp, Subtype.Island)] (Filter.Type.HasKeyword (Keyword.Landwalk (Filter.Type.Not (Filter.Type.HasSupertype Supertype.Basic)))))
        (Filter.Type.HasKeyword (Keyword.Landwalk (Filter.Type.Not (Filter.Type.HasSupertype Supertype.Basic))))
    Spec.it s "swaps only the land type of a snow swampwalk" $ do
      Spec.assertEqWith
        s
        "snow islandwalk"
        (Filter.rewrite [(Subtype.Swamp, Subtype.Island)] (Filter.Type.HasKeyword (Keyword.Landwalk (Filter.Type.And [Filter.Type.HasSupertype Supertype.Snow, Filter.Type.HasSubtype Subtype.Swamp]))))
        (Filter.Type.HasKeyword (Keyword.Landwalk (Filter.Type.And [Filter.Type.HasSupertype Supertype.Snow, Filter.Type.HasSubtype Subtype.Island])))

    -- CR 702.29e's "[Type]cycling" is rule 702's OTHER "[type]", and the rule's
    -- own example is a basic land type: "usually a subtype (as in
    -- 'mountaincycling')". Nothing in the pool prints typecycling; the arm
    -- exists because rewriteKeyword classifies every keyword by whether it holds
    -- a word rather than naming landwalk.
    Spec.it s "descends into a typecycling criterion too" $ do
      let cost = Cost.MkCost {Cost.mana = Nothing, Cost.components = []}
      Spec.assertEqWith
        s
        "mountaincycling became islandcycling"
        (Filter.rewrite [(Subtype.Mountain, Subtype.Island)] (Filter.Type.HasKeyword (Keyword.Cycling cost (Just (Filter.Type.HasSubtype Subtype.Mountain)))))
        (Filter.Type.HasKeyword (Keyword.Cycling cost (Just (Filter.Type.HasSubtype Subtype.Island))))

    -- CR 702.11d's "[quality]" is rule 702's THIRD carrier of a word, and CR
    -- 612.2 reaches it on the same terms as the two above: the rule changes "a
    -- creature type word used as a creature type", and a quality naming a
    -- creature type is one. The pool's one hexproof (Slippery Bogle) carries no
    -- quality at all, so nothing here changes an answer a card can ask for
    -- today; the arm exists because rewriteKeyword classifies a keyword by
    -- whether it holds a word, and after #726 this one does (#733).
    Spec.it s "descends into a hexproof quality" $ do
      Spec.assertEqWith
        s
        "hexproof from Goblins became hexproof from Zombies"
        (Filter.rewrite [(Subtype.Goblin, Subtype.Zombie)] (Filter.Type.HasKeyword (Keyword.Hexproof (Just (Filter.Type.HasSubtype Subtype.Goblin)))))
        (Filter.Type.HasKeyword (Keyword.Hexproof (Just (Filter.Type.HasSubtype Subtype.Zombie))))

    -- Elenda, Saint of Dusk's "hexproof from instants": a card type is not a
    -- subtype word, so the swap reaches nothing inside it.
    Spec.it s "leaves a hexproof quality that names no subtype alone" $ do
      Spec.assertEqWith
        s
        "hexproof from instants untouched"
        (Filter.rewrite [(Subtype.Goblin, Subtype.Zombie)] (Filter.Type.HasKeyword (Keyword.Hexproof (Just (Filter.Type.HasCardType CardType.Instant)))))
        (Filter.Type.HasKeyword (Keyword.Hexproof (Just (Filter.Type.HasCardType CardType.Instant))))

    -- CR 702.11b's plain hexproof carries no quality at all, which is the
    -- Nothing the descent has to leave standing rather than invent a filter for.
    Spec.it s "leaves an unqualified hexproof alone" $ do
      Spec.assertEqWith
        s
        "plain hexproof untouched"
        (Filter.rewrite [(Subtype.Goblin, Subtype.Zombie)] (Filter.Type.HasKeyword (Keyword.Hexproof Nothing)))
        (Filter.Type.HasKeyword (Keyword.Hexproof Nothing))

  Spec.describe s "AttackedThisTurn" $ do
    Spec.it s "matches a view whose history says so" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.attackedThisTurn = True}) Filter.Type.AttackedThisTurn) "attacked"

    Spec.it s "does not match a creature that never attacked" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.AttackedThisTurn)) "did not attack"

    -- The two axes are independent in BOTH directions, which is the whole
    -- reason this atom exists. A creature attacking right now may not have
    -- been declared this turn (CR 508.4 puts one onto the battlefield
    -- attacking without it ever having attacked), and one that attacked
    -- earlier this turn is no longer attacking once CR 511.3 has removed it
    -- from combat -- which is Relentless Assault's whole case.
    Spec.it s "is not implied by attacking right now" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.attacking = True}) Filter.Type.AttackedThisTurn)) "attacking does not imply attacked"

    Spec.it s "does not imply attacking right now" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.attackedThisTurn = True}) Filter.Type.IsAttacking)) "attacked does not imply attacking"

    -- CR 506.3: only a creature can be declared as an attacker, and a
    -- player is not one.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.AttackedThisTurn)) "player"

  Spec.describe s "IsAttachedToCreature" $ do
    Spec.it s "matches a view whose attachment says so" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.attachedToCreature = True}) Filter.Type.IsAttachedToCreature) "attached to a creature"

    Spec.it s "does not match a permanent attached to nothing" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.IsAttachedToCreature)) "unattached"

    -- CR 109.3 names "what an Aura enchants" among the things that are not
    -- characteristics, so no characteristic axis can stand in for it: being
    -- an Aura by subtype says nothing about whether it is on a creature.
    Spec.it s "is independent of every characteristic axis" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.subtypes = Set.singleton Subtype.Aura}) Filter.Type.IsAttachedToCreature)) "subtype does not imply attachment"

    -- CR 303.4b: a player is enchanted BY an Aura, never attached to
    -- anything -- Object.attachedTo is a field of the attached permanent,
    -- and a player is not one.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.IsAttachedToCreature)) "player"

  Spec.describe s "IsAttachedToPermanent" $ do
    Spec.it s "matches a view whose attachment says so" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.attachedToPermanent = True}) Filter.Type.IsAttachedToPermanent) "attached to a permanent"

    Spec.it s "does not match a permanent attached to nothing" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.IsAttachedToPermanent)) "unattached"

    -- The pair that makes this a separate atom rather than a synonym: CR
    -- 303.4 attaches an Aura to "an object or player", so being attached to a
    -- permanent is strictly wider than being attached to a creature and the
    -- implication runs one way only. Pawl.Engine.Projection fills both fields off the
    -- same Object.attachedTo, so the views a real board produces never carry
    -- the impossible combination -- but the matcher folds whatever it is
    -- given, and each atom must read its own field.
    Spec.it s "is a wider question than IsAttachedToCreature" $ do
      let onLand = blackCreature {Filter.attachedToPermanent = True}
      Spec.assertBool s (Filter.matches self onLand Filter.Type.IsAttachedToPermanent) "on a land: attached to a permanent"
      Spec.assertBool s (not (Filter.matches self onLand Filter.Type.IsAttachedToCreature)) "but not to a creature"

    -- CR 303.4b: a player is enchanted BY an Aura and is not itself attached
    -- to anything, which is the case this atom exists to exclude -- Curse of
    -- Death's Hold is attached to a player, so it is not a legal target for
    -- Aura Graft.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.IsAttachedToPermanent)) "player"

  Spec.describe s "CanHostSubject" $ do
    Spec.it s "matches a view the caller marked as a legal destination" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.canHostSubject = True}) Filter.Type.CanHostSubject) "can host"

    -- CR 701.3a asks about the SUBJECT, so no fact about the candidate can
    -- settle it: a creature is exactly what an Aura usually enchants and
    -- still answers False until the caller that knows what is moving says
    -- otherwise.
    Spec.it s "is independent of every characteristic axis" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.CanHostSubject)) "being a creature does not make it a legal host"

    -- Vacuously False wherever no attach frames the match, which is every
    -- view but the ones Pawl.Engine.Resolve's AttachTarget arm builds.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.CanHostSubject)) "player"

  Spec.describe s "IsToken" $ do
    Spec.it s "matches a view whose object is a token" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.token = True}) Filter.Type.IsToken) "token"

    -- Ashaya's "nontoken creatures you control" is spelled `Not IsToken`, the
    -- way CR 601.2c's "another" is spelled `Not IsSource` (#163).
    Spec.it s "Not IsToken is how 'nontoken' is written" $ do
      Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.Not Filter.Type.IsToken)) "a card permanent is nontoken"
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.token = True}) (Filter.Type.Not Filter.Type.IsToken))) "a token is not"

    -- CR 111.3: a token's characteristics are effect-defined and are
    -- "functionally equivalent" to printed ones, so no characteristic axis
    -- distinguishes a token from the card it copies.
    Spec.it s "is independent of every characteristic axis" $ do
      Spec.assertBool s (not (Filter.matches self devoidBigCreature Filter.Type.IsToken)) "power does not imply token"

    -- CR 111.1: a token is a marker used to represent a PERMANENT; a player
    -- is not one.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.IsToken)) "player"
