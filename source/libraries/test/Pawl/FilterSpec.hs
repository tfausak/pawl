-- Covers Pawl.Types.Filter, Pawl.Types.PlayerRelation, Pawl.Engine.Filter.
module Pawl.FilterSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Cycling as Cycling
-- Aliased Filter.Type, not Type, because the evaluator module Pawl.Engine.Filter
-- already claims the alias Filter (a documented exception to alias-to-last-
-- component, per the M4.5 P9 plan's global constraints).
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Zone as Zone

-- A projected black creature controlled by player 0.
blackCreature :: Filter.View
blackCreature =
  Filter.MkView
    { Filter.names = Set.empty,
      Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.singleton Color.Black,
      Filter.subtypes = Set.singleton Subtype.Zombie,
      Filter.keywords = Set.singleton Keyword.Flying,
      Filter.power = Just 2,
      Filter.toughness = Just 2,
      Filter.manaValue = Just 3,
      Filter.controller = Just (PlayerId.MkPlayerId 0),
      -- CR 108.3: OWNED by player 1 while CONTROLLED by player 0, the one board
      -- shape that tells OwnedBy and ControlledBy apart (Garland, Royal
      -- Kidnapper's stolen creature). A view where the two agreed would satisfy
      -- either reading of the atom.
      Filter.owner = Just (PlayerId.MkPlayerId 1),
      -- CR 400.1: a permanent, so the battlefield -- which is what lets the
      -- IsInZone cases below tell a matching zone from a non-matching one off one
      -- view.
      Filter.zone = Just Zone.Battlefield,
      Filter.identity = Just (ObjectId.MkObjectId 7),
      Filter.playerIdentity = Nothing,
      Filter.attacking = False,
      Filter.blocking = False,
      Filter.blocked = False,
      Filter.attackedThisTurn = False,
      Filter.declaredAttackerThisCombat = False,
      Filter.declaredBlockerThisCombat = False,
      Filter.milledThisTurn = False,
      Filter.dealtDamageThisTurn = False,
      Filter.attachedToView = Nothing,
      Filter.attachedViews = [],
      Filter.attachedTo = Nothing,
      Filter.canHostSubject = False,
      Filter.canAttachToSubject = False,
      Filter.token = False,
      Filter.tapped = False,
      Filter.transformed = False,
      Filter.counters = Map.empty,
      Filter.ringBearerFor = Nothing,
      Filter.designations = Set.empty,
      Filter.classLevel = Nothing,
      Filter.kicked = False,
      Filter.manaSpentTags = Set.empty,
      -- CR 602.1 / 605.1a: a vanilla creature as far as this axis goes, so the
      -- atom's own cases below say which view they want rather than inheriting it.
      Filter.nonManaActivatedAbility = False
    }

-- A colourless (devoid) creature with power 5, no controller recorded.
devoidBigCreature :: Filter.View
devoidBigCreature =
  Filter.MkView
    { Filter.names = Set.empty,
      Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.empty,
      Filter.subtypes = Set.empty,
      Filter.keywords = Set.empty,
      Filter.power = Just 5,
      Filter.toughness = Just 5,
      Filter.manaValue = Just 5,
      Filter.controller = Nothing,
      Filter.owner = Nothing,
      Filter.zone = Nothing,
      Filter.identity = Nothing,
      Filter.playerIdentity = Nothing,
      Filter.attacking = False,
      Filter.blocking = False,
      Filter.blocked = False,
      Filter.attackedThisTurn = False,
      Filter.declaredAttackerThisCombat = False,
      Filter.declaredBlockerThisCombat = False,
      Filter.milledThisTurn = False,
      Filter.dealtDamageThisTurn = False,
      Filter.attachedToView = Nothing,
      Filter.attachedViews = [],
      Filter.attachedTo = Nothing,
      Filter.canHostSubject = False,
      Filter.canAttachToSubject = False,
      Filter.token = False,
      Filter.tapped = False,
      Filter.transformed = False,
      Filter.counters = Map.empty,
      Filter.ringBearerFor = Nothing,
      Filter.designations = Set.empty,
      Filter.classLevel = Nothing,
      Filter.kicked = False,
      Filter.manaSpentTags = Set.empty,
      Filter.nonManaActivatedAbility = False
    }

-- A creature whose only ability is the given keyword -- the toxic N and landwalk
-- views the family/instance cases below are asked about. Off blackCreature, so
-- every other axis is a fixed background and only the keyword set varies.
withKeyword :: Keyword.Keyword -> Filter.View
withKeyword keyword = blackCreature {Filter.keywords = Set.singleton keyword}

-- blackCreature carrying player 0's Ring-bearer designation (CR 701.54a). Off
-- blackCreature, so the designation is the only axis that varies.
ringBearer :: Filter.View
ringBearer = blackCreature {Filter.ringBearerFor = Just (PlayerId.MkPlayerId 0)}

-- CR 702.112b's designation, which belongs to no player -- so unlike ringBearer
-- above there is no second view for "somebody else's".
renownedCreature :: Filter.View
renownedCreature = blackCreature {Filter.designations = Set.singleton Designation.Renowned}

-- CR 701.60b's designation, which belongs to no player either. Carries MENACE as
-- well, because CR 701.60c gives a suspected permanent menace: a view with the
-- designation and not the keyword could not tell an atom reading the designation
-- apart from one reading what hangs off it, and `withKeyword Keyword.Menace` is
-- the other half of that pair.
suspectedCreature :: Filter.View
suspectedCreature =
  blackCreature
    { Filter.designations = Set.singleton Designation.Suspected,
      Filter.keywords = Set.singleton Keyword.Menace
    }

-- CR 122.1's markers, two kinds at once so an atom that read the map's emptiness
-- rather than its key would pass the positive and fail nothing.
counteredCreature :: Filter.View
counteredCreature =
  blackCreature
    { Filter.counters = Map.fromList [(CounterKind.PlusOnePlusOne, 1), (CounterKind.Lore, 2)]
    }

-- The HOST a candidate's attachment points at (CR 701.3a): an ordinary creature
-- controlled by player 0, so AttachedTo's nest has both a card type and a
-- controller to read. Off blackCreature, and with its own id, so the host is
-- distinguishable from the candidate.
aHost :: Filter.View
aHost = blackCreature {Filter.identity = Just (ObjectId.MkObjectId 8)}

-- The same host under the OTHER seat. Control is the only axis that varies, which
-- is what makes "a creature you control" and "a creature an opponent controls"
-- tellable apart by control and by nothing else (CR 109.5).
theirHost :: Filter.View
theirHost = aHost {Filter.controller = Just (PlayerId.MkPlayerId 1)}

-- A host that is a LAND rather than a creature, for the card-type conjunct, and
-- for the "on a permanent is wider than on a creature" pair (CR 303.4).
aLandHost :: Filter.View
aLandHost = aHost {Filter.cardTypes = Set.singleton CardType.Land}

-- blackCreature attached to the given host. Off blackCreature for withKeyword's
-- reason: the attachment is the only axis that varies.
onHost :: Filter.View -> Filter.View
onHost host = blackCreature {Filter.attachedToView = Just host}

self :: Filter.Context
self = Filter.contextFor (Just (PlayerId.MkPlayerId 0)) Nothing

other :: Filter.Context
other = Filter.contextFor (Just (PlayerId.MkPlayerId 1)) Nothing

noPerspective :: Filter.Context
noPerspective = Filter.contextFor Nothing Nothing

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

  -- CR 702.134a's comparison, whose bound is the Context's source power rather
  -- than a literal the atom carries. blackCreature is power 2 and
  -- devoidBigCreature power 5, so one source power between them tells the two
  -- apart in both directions.
  Spec.describe s "PowerLessThanSource" $ do
    let sourced n = self {Filter.sourcePower = Just n}
    Spec.it s "holds below the source's power and fails above it" $ do
      Spec.assertBool s (Filter.matches (sourced 3) blackCreature Filter.Type.PowerLessThanSource) "2 < 3"
      Spec.assertBool s (not (Filter.matches (sourced 3) devoidBigCreature Filter.Type.PowerLessThanSource)) "5 is not < 3"

    -- STRICTLY less, which is what keeps a mentor from targeting itself: rule
    -- 702.134a says "less than", not "no greater than".
    Spec.it s "is False at equal power" $ do
      Spec.assertBool s (not (Filter.matches (sourced 2) blackCreature Filter.Type.PowerLessThanSource)) "2 is not < 2"

    -- The two vacuity postures, PowerAtMost's on the candidate side and
    -- ControlledBy's on the context side.
    Spec.it s "is False when either power is absent" $ do
      let noPower = blackCreature {Filter.power = Nothing}
      Spec.assertBool s (not (Filter.matches (sourced 3) noPower Filter.Type.PowerLessThanSource)) "no candidate power"
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.PowerLessThanSource)) "no source power"

    Spec.it s "is False for a player" $ do
      Spec.assertBool s (not (Filter.matches (sourced 3) aPlayer Filter.Type.PowerLessThanSource)) "player"

  -- CR 702.149a's comparison, the same Context field read the other way -- and NOT
  -- the negation of its sibling, which is why it is a separate atom: equal power
  -- and an absent power both answer False here and True to `Not
  -- PowerLessThanSource`.
  Spec.describe s "PowerGreaterThanSource" $ do
    let sourced n = self {Filter.sourcePower = Just n}
    Spec.it s "holds above the source's power and fails below it" $ do
      Spec.assertBool s (Filter.matches (sourced 3) devoidBigCreature Filter.Type.PowerGreaterThanSource) "5 > 3"
      Spec.assertBool s (not (Filter.matches (sourced 3) blackCreature Filter.Type.PowerGreaterThanSource)) "2 is not > 3"

    -- STRICTLY greater, which is what keeps a training creature from counting a
    -- companion its own size: rule 702.149a says "greater", not "no less".
    Spec.it s "is False at equal power, where the negation of its sibling is True" $ do
      Spec.assertBool s (not (Filter.matches (sourced 2) blackCreature Filter.Type.PowerGreaterThanSource)) "2 is not > 2"
      Spec.assertBool s (Filter.matches (sourced 2) blackCreature (Filter.Type.Not Filter.Type.PowerLessThanSource)) "where the negation admits it"

    Spec.it s "is False when either power is absent" $ do
      let noPower = blackCreature {Filter.power = Nothing}
      Spec.assertBool s (not (Filter.matches (sourced 3) noPower Filter.Type.PowerGreaterThanSource)) "no candidate power"
      Spec.assertBool s (not (Filter.matches self devoidBigCreature Filter.Type.PowerGreaterThanSource)) "no source power"

    Spec.it s "is False for a player" $ do
      Spec.assertBool s (not (Filter.matches (sourced 3) aPlayer Filter.Type.PowerGreaterThanSource)) "player"
  -- CR 702.39a's "defending player controls", whose player comes from the
  -- Context rather than from the perspective. blackCreature is controlled by
  -- player 0 and OWNED by player 1, so a reading that consulted the wrong field
  -- would not agree with either arm below.
  Spec.describe s "ControlledByDefendingPlayer" $ do
    let defended n = self {Filter.defendingPlayer = Just (PlayerId.MkPlayerId n)}
    Spec.it s "holds only for the defending player's creature" $ do
      Spec.assertBool s (Filter.matches (defended 0) blackCreature Filter.Type.ControlledByDefendingPlayer) "controller is the defender"
      Spec.assertBool s (not (Filter.matches (defended 1) blackCreature Filter.Type.ControlledByDefendingPlayer)) "owner is not the controller"

    -- NOT ControlledBy Opponent: `self`'s perspective is player 0, so that atom
    -- would answer the opposite of this one on the very same view. CR 506.2a is
    -- what makes them different questions.
    Spec.it s "is not ControlledBy Opponent" $ do
      Spec.assertBool s (not (Filter.matches (defended 0) blackCreature (Filter.Type.ControlledBy PlayerRelation.Opponent))) "player 0 is the perspective"

    -- ControlledBy's vacuity posture on both sides.
    Spec.it s "is False when either player is absent" $ do
      let noController = blackCreature {Filter.controller = Nothing}
      Spec.assertBool s (not (Filter.matches (defended 0) noController Filter.Type.ControlledByDefendingPlayer)) "no candidate controller"
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.ControlledByDefendingPlayer)) "no defending player"

  -- CR 119.5's "they control", whose player comes from the Context as
  -- ControlledByDefendingPlayer's does -- Biorhythm asks it once per recipient.
  -- blackCreature is controlled by player 0 and OWNED by player 1, so a reading
  -- off the wrong field agrees with neither arm below.
  Spec.describe s "ControlledByRecipient" $ do
    let reached n = self {Filter.recipient = Just (PlayerId.MkPlayerId n)}
    Spec.it s "holds only for the reached recipient's creature" $ do
      Spec.assertBool s (Filter.matches (reached 0) blackCreature Filter.Type.ControlledByRecipient) "controller is the recipient"
      Spec.assertBool s (not (Filter.matches (reached 1) blackCreature Filter.Type.ControlledByRecipient)) "owner is not the controller"

    -- Perspective-free, which is why the atom exists: `noPerspective` names no
    -- "you" at all and the answer is unchanged, where ControlledBy You is
    -- vacuously False there. That is the difference between "they control" and
    -- "you control" on one board.
    Spec.it s "is not ControlledBy You, and needs no perspective" $ do
      Spec.assertBool s (Filter.matches (noPerspective {Filter.recipient = Just (PlayerId.MkPlayerId 0)}) blackCreature Filter.Type.ControlledByRecipient) "no perspective needed"
      Spec.assertBool s (not (Filter.matches noPerspective blackCreature (Filter.Type.ControlledBy PlayerRelation.You))) "where ControlledBy You cannot answer"
      Spec.assertBool s (Filter.matches (reached 1) blackCreature (Filter.Type.ControlledBy PlayerRelation.You)) "and disagrees with it on the very same view: the perspective still controls this creature where the recipient does not"

    -- ControlledBy's vacuity posture on both sides. No recipient at all is every
    -- position but a per-recipient effect's quantity.
    Spec.it s "is False when either player is absent" $ do
      let noController = blackCreature {Filter.controller = Nothing}
      Spec.assertBool s (not (Filter.matches (reached 0) noController Filter.Type.ControlledByRecipient)) "no candidate controller"
      Spec.assertBool s (not (Filter.matches (reached 0) aPlayer Filter.Type.ControlledByRecipient)) "a player controls nothing"
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.ControlledByRecipient)) "no recipient"

  -- CR 603.2's "that player controls", the pair a trigger's binding is baked
  -- through. blackCreature is controlled by player 0 and owned by player 1, so a
  -- reading off the wrong field disagrees with both arms below.
  Spec.describe s "ControlledByBound and ControlledByPlayer" $ do
    let slot = SlotName.MkSlotName (Text.pack "thatPlayer")
        player = PlayerId.MkPlayerId
    Spec.it s "the baked atom compares the candidate's controller" $ do
      Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.ControlledByPlayer (player 0))) "controller is player 0"
      Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.ControlledByPlayer (player 1)))) "the owner is not the controller"

    -- Perspective-free, which is the whole point of baking a player in: the same
    -- answer under a context that names nobody.
    Spec.it s "the baked atom needs no perspective" $
      Spec.assertBool s (Filter.matches noPerspective blackCreature (Filter.Type.ControlledByPlayer (player 0))) "no perspective needed"

    Spec.it s "the baked atom is False for a player and for a candidate with no controller" $ do
      let noController = blackCreature {Filter.controller = Nothing}
      Spec.assertBool s (not (Filter.matches self noController (Filter.Type.ControlledByPlayer (player 0)))) "no candidate controller"
      Spec.assertBool s (not (Filter.matches self aPlayer (Filter.Type.ControlledByPlayer (player 0)))) "player"

    -- UNBAKED is False, always: the substitution happens where the bindings are,
    -- so an atom that survives to a match named a slot nothing bound.
    Spec.it s "the unbaked atom admits nothing" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.ControlledByBound slot))) "unbaked"
      Spec.assertBool s (not (Filter.matches noPerspective blackCreature (Filter.Type.ControlledByBound slot))) "unbaked, no perspective"

    -- bakeBound is what turns the first into the second, under every combinator
    -- -- an implementation that looked only at the top of a Filter would leave
    -- this one standing -- and it leaves a slot the environment does not name
    -- alone rather than guessing.
    Spec.it s "bakeBound substitutes the bound player, buried and at the top" $ do
      let buried f = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not f]]
          bound = Map.singleton slot (player 0)
      Spec.assertEqWith
        s
        "the atom is replaced in place"
        (Filter.bakeBound bound (buried (Filter.Type.ControlledByBound slot)))
        (buried (Filter.Type.ControlledByPlayer (player 0)))
      Spec.assertEqWith
        s
        "and left standing when the slot names nobody"
        (Filter.bakeBound Map.empty (Filter.Type.ControlledByBound slot))
        (Filter.Type.ControlledByBound slot)
      Spec.assertBool
        s
        (Filter.matches self blackCreature (Filter.bakeBound bound (Filter.Type.ControlledByBound slot)))
        "so the baked filter matches where the unbaked one did not"
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

  -- CR 202.3 read for parity: Void Winnower's "spells with even mana values",
  -- whose reminder text settles the boundary -- "(Zero is even.)"
  Spec.it s "ManaValueIsEven splits the mana values by parity" $ do
    let at n = blackCreature {Filter.manaValue = Just n}
    Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.ManaValueIsEven)) "mana value 3 is odd"
    Spec.assertBool s (Filter.matches self (at 2) Filter.Type.ManaValueIsEven) "2 is even"
    Spec.assertBool s (Filter.matches self (at 0) Filter.Type.ManaValueIsEven) "and zero is even"

  Spec.it s "ManaValueIsEven is False when there is no mana value at all" $ do
    Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.manaValue = Nothing}) Filter.Type.ManaValueIsEven)) "no mana value"
    Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.ManaValueIsEven)) "player"

  -- CR 601.3a's bound (Pawl.Engine.PlayerEffect.choiceCouldEscape): the literals
  -- the criterion compares against, and nothing else. A parity atom contributes
  -- none -- it is the tail past the greatest literal that covers it.
  Spec.it s "manaValueThresholds collects every literal at any depth" $ do
    Spec.assertEqWith
      s
      "both bounds, from inside an Or under a Not"
      (Filter.manaValueThresholds (Filter.Type.Not (Filter.Type.Or [Filter.Type.ManaValueAtMost 5, Filter.Type.And [Filter.Type.ManaValueIsEven, Filter.Type.ManaValueAtMost 2]])))
      [5, 2]
    Spec.assertEqWith
      s
      "and none from a criterion with no literal in it"
      (Filter.manaValueThresholds Filter.Type.ManaValueIsEven)
      []

  -- CR 110.2's board comparison is answered by Pawl.Engine.Count.bakePerspective,
  -- which holds the game state; this module holds none, so the atom is vacuously
  -- False wherever it reaches a match unbaked. ControlledByBound's posture, pinned
  -- here for the same reason it is pinned there -- Pawl.CountSpec's Oreskos
  -- Explorer group is what proves the BAKED answer.
  Spec.it s "ControlsMoreThanYou is False unbaked, for a player and for an object alike" $ do
    let lands = Filter.Type.ControlsMoreThanYou (Filter.Type.HasCardType CardType.Land)
    Spec.assertBool s (not (Filter.matches self aPlayer lands)) "player"
    Spec.assertBool s (not (Filter.matches self blackCreature lands)) "object"

  -- CR 400.1's per-player graveyard, ControlsMoreThanYou's posture one atom over
  -- and for its reason plus one: an OBJECT has no graveyard to size at all (CR
  -- 109.3 counts no zone among its characteristics), so False is the answer for
  -- both candidates rather than only for the unbaked position. Pawl.TriggerSpec's
  -- The Master of Lake-town death group proves the BAKED answer.
  Spec.it s "CardsInGraveyardAtLeast is False unbaked, for a player and for an object alike" $ do
    let bin = Filter.Type.CardsInGraveyardAtLeast 7
    Spec.assertBool s (not (Filter.matches self aPlayer bin)) "player"
    Spec.assertBool s (not (Filter.matches self blackCreature bin)) "object"

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

  -- CR 108.3 against CR 109.5's controller, on the ONE view where the two
  -- disagree: blackCreature is controlled by player 0 and owned by player 1, so
  -- each of these four answers the OPPOSITE of the ControlledBy case above it.
  -- An implementation reading the controller for the owner flips all four.
  Spec.describe s "OwnedBy" $ do
    Spec.it s "CR 108.3 OwnedBy You fails for an object its controller does not own" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.OwnedBy PlayerRelation.You))) "controlled, not owned"

    Spec.it s "CR 108.3 OwnedBy You holds from the OWNER's perspective" $ do
      Spec.assertBool s (Filter.matches other blackCreature (Filter.Type.OwnedBy PlayerRelation.You)) "player 1 owns it"

    Spec.it s "CR 108.3 OwnedBy Opponent holds from the controller's perspective" $ do
      Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.OwnedBy PlayerRelation.Opponent)) "someone else owns it"

    -- The two vacuity postures, matching ControlledBy's: no object behind the
    -- view, and no "you" for the relation to be about.
    Spec.it s "CR 108.3 OwnedBy is False when the view has no owner" $ do
      Spec.assertBool s (not (Filter.matches self devoidBigCreature (Filter.Type.OwnedBy PlayerRelation.Opponent))) "no owner"

    Spec.it s "CR 108.3 OwnedBy is False when the context has no perspective" $ do
      Spec.assertBool s (not (Filter.matches noPerspective blackCreature (Filter.Type.OwnedBy PlayerRelation.Opponent))) "no perspective"

  Spec.describe s "IsSource" $ do
    Spec.it s "matches the context's source" $ do
      Spec.assertBool
        s
        (Filter.matches (Filter.contextFor (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7))) blackCreature Filter.Type.IsSource)
        "is the source"

    Spec.it s "does not match a different object" $ do
      Spec.assertBool
        s
        (not (Filter.matches (Filter.contextFor (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 8))) blackCreature Filter.Type.IsSource))
        "not the source"

    Spec.it s "no source in context is vacuously false" $ do
      Spec.assertBool
        s
        (not (Filter.matches (Filter.contextFor (Just (PlayerId.MkPlayerId 0)) Nothing) blackCreature Filter.Type.IsSource))
        "no source"

    Spec.it s "no identity in view is vacuously false" $ do
      Spec.assertBool
        s
        (not (Filter.matches (Filter.contextFor (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7))) devoidBigCreature Filter.Type.IsSource))
        "no identity"

  -- CR 115.10a: what the RESOLUTION named, in either shape. blackCreature is
  -- object 7 throughout, so a slot naming 7 among others is the group read and a
  -- slot naming 6 and 8 is the negative built off the same board.
  Spec.describe s "IsBound" $ do
    let slot = SlotName.MkSlotName (Text.pack "milled")
        bound oids = Filter.contextWithSlots (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7)) (Map.singleton slot (Set.fromList (fmap ObjectId.MkObjectId oids)))
    Spec.it s "matches the one object the slot names" $ do
      Spec.assertBool s (Filter.matches (bound [7]) blackCreature (Filter.Type.IsBound slot)) "the bound object"

    -- The group read: CR 701.17c's "from among them" is a question about every
    -- member, so a batch naming the candidate among others admits it.
    Spec.it s "CR 115.10a matches every member of a slot bound to a group" $ do
      Spec.assertBool s (Filter.matches (bound [6, 7, 8]) blackCreature (Filter.Type.IsBound slot)) "a member of the group"

    Spec.it s "does not match an object the slot does not name" $ do
      Spec.assertBool s (not (Filter.matches (bound [6, 8]) blackCreature (Filter.Type.IsBound slot))) "not in the group"

    Spec.it s "a slot naming nothing is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches (bound []) blackCreature (Filter.Type.IsBound slot))) "nothing bound"

    -- The other half of the same map, and the reason it is a set: a reader that
    -- can point at one object and no more declines a group rather than taking
    -- whichever member sorts first.
    Spec.it s "CR 601.2c a singular reader takes the single binding and declines the group" $ do
      Spec.assertEqWith s "the singleton is answerable" (Filter.slotOneObject slot (bound [7])) (Just (ObjectId.MkObjectId 7))
      Spec.assertEqWith s "and the group is not" (Filter.slotOneObject slot (bound [6, 7, 8])) Nothing

  -- CR 201.4: what the SOURCE named, against what the candidate is called. The
  -- two sides are set intersection, so CR 201.4g's interchangeable names and CR
  -- 709.4a's two-named objects each fall out.
  Spec.describe s "HasChosenName" $ do
    let named ns = self {Filter.sourceChosenNames = Set.fromList (fmap (CardName.MkCardName . Text.pack) ns)}
        called ns = blackCreature {Filter.names = Set.fromList (fmap (CardName.MkCardName . Text.pack) ns)}
    Spec.it s "matches a candidate whose name the source chose" $ do
      Spec.assertBool s (Filter.matches (named ["Chromatic Star"]) (called ["Chromatic Star"]) Filter.Type.HasChosenName) "the chosen name"

    Spec.it s "does not match a candidate with some other name" $ do
      Spec.assertBool s (not (Filter.matches (named ["Chromatic Star"]) (called ["Crucible of Worlds"]) Filter.Type.HasChosenName)) "a different name"

    -- CR 709.4a's membership at the candidate's end: a split card off the stack
    -- shows two names, and sharing either is sharing one.
    Spec.it s "CR 709.4a matches a candidate showing the chosen name among others" $ do
      Spec.assertBool s (Filter.matches (named ["Wax"]) (called ["Wax", "Wane"]) Filter.Type.HasChosenName) "one of two names"

    -- The posture every context-relative atom takes, and the reason the position
    -- lint exists: outside the one context that fills the field this is False
    -- rather than an error.
    Spec.it s "a source that chose nothing is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self (called ["Chromatic Star"]) Filter.Type.HasChosenName)) "contextFor leaves it empty"

    -- CR 708.2a: a face-down object has no name at all, so it shares none.
    Spec.it s "a nameless candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches (named ["Chromatic Star"]) blackCreature Filter.Type.HasChosenName)) "no names"

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
        (Filter.rewrite [(Subtype.Mountain, Subtype.Island)] (Filter.Type.HasKeyword (Keyword.Cycling (Cycling.MkCycling cost (Just (Filter.Type.HasSubtype Subtype.Mountain))))))
        (Filter.Type.HasKeyword (Keyword.Cycling (Cycling.MkCycling cost (Just (Filter.Type.HasSubtype Subtype.Island)))))

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

    -- CR 702.16a's "[quality]" is the same carrier one rule along, and CR 612.2
    -- reaches it on the same terms. Apostle of Purifying Light's quality is a
    -- colour, so nothing in the pool changes an answer here today; the arm exists
    -- because rewriteKeyword classifies a keyword by whether it holds a word, and
    -- this one does. It is a REGRESSION FENCE and not a proof: nothing else in the
    -- tree observes it, which the PR that landed protection says outright.
    Spec.it s "descends into a protection quality" $ do
      Spec.assertEqWith
        s
        "protection from Goblins became protection from Zombies"
        (Filter.rewrite [(Subtype.Goblin, Subtype.Zombie)] (Filter.Type.HasKeyword (Keyword.Protection (Filter.Type.HasSubtype Subtype.Goblin))))
        (Filter.Type.HasKeyword (Keyword.Protection (Filter.Type.HasSubtype Subtype.Zombie)))

    -- The colour Apostle of Purifying Light actually prints: no subtype word
    -- inside it, so the swap reaches nothing.
    Spec.it s "leaves a protection quality that names no subtype alone" $ do
      Spec.assertEqWith
        s
        "protection from black untouched"
        (Filter.rewrite [(Subtype.Goblin, Subtype.Zombie)] (Filter.Type.HasKeyword (Keyword.Protection (Filter.Type.HasColor Color.Black))))
        (Filter.Type.HasKeyword (Keyword.Protection (Filter.Type.HasColor Color.Black)))

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

  Spec.describe s "DeclaredAttackerThisCombat" $ do
    Spec.it s "matches a view whose combat record says so" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.declaredAttackerThisCombat = True}) Filter.Type.DeclaredAttackerThisCombat) "declared"

    Spec.it s "does not match a creature nobody declared" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.DeclaredAttackerThisCombat)) "not declared"

    -- Independent of IsAttacking in BOTH directions, which is why the atom
    -- exists rather than being spelled Not IsAttacking. CR 508.1k makes a
    -- chosen creature attacking only after CR 508.1j's payment, so mid-payment
    -- it is declared and not attacking; CR 508.4's creature put onto the
    -- battlefield attacking is attacking and never declared, and so is one CR
    -- 506.4 has not yet removed from combat.
    Spec.it s "is not implied by attacking right now" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.attacking = True}) Filter.Type.DeclaredAttackerThisCombat)) "attacking does not imply declared"

    Spec.it s "does not imply attacking right now" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.declaredAttackerThisCombat = True}) Filter.Type.IsAttacking)) "declared does not imply attacking"

    -- And independent of the TURN-scoped atom, the distinction CR 500.8's
    -- additional combat phase makes observable: CR 511.3 empties the combat
    -- record while the turn's event log stands.
    Spec.it s "is not the same axis as AttackedThisTurn" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.attackedThisTurn = True}) Filter.Type.DeclaredAttackerThisCombat)) "attacked this turn does not imply declared this combat"
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.declaredAttackerThisCombat = True}) Filter.Type.AttackedThisTurn)) "and the other way round"

    -- CR 506.3: only a creature is ever declared as an attacker.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.DeclaredAttackerThisCombat)) "player"

  Spec.describe s "DeclaredBlockerThisCombat" $ do
    Spec.it s "matches a view whose combat record says so" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.declaredBlockerThisCombat = True}) Filter.Type.DeclaredBlockerThisCombat) "declared"

    Spec.it s "does not match a creature nobody declared" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.DeclaredBlockerThisCombat)) "not declared"

    -- CR 509.1g against CR 509.1f: the pair above's independence, on the
    -- blocking side.
    Spec.it s "is not implied by blocking right now" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.blocking = True}) Filter.Type.DeclaredBlockerThisCombat)) "blocking does not imply declared"

    Spec.it s "does not imply blocking right now" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.declaredBlockerThisCombat = True}) Filter.Type.IsBlocking)) "declared does not imply blocking"

    -- CR 508 and CR 509 are two turn-based actions, so the two atoms are two
    -- axes -- which is what makes Hollow Warrior's criterion a conjunction of
    -- two Nots rather than one atom meaning "in combat".
    Spec.it s "is a separate axis from the attacking declaration" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.declaredAttackerThisCombat = True}) Filter.Type.DeclaredBlockerThisCombat)) "attacker does not imply blocker"
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.declaredBlockerThisCombat = True}) Filter.Type.DeclaredAttackerThisCombat)) "and the other way round"

    -- CR 506.3 again, for CR 509.1a's half.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.DeclaredBlockerThisCombat)) "player"

  Spec.describe s "MilledThisTurn" $ do
    Spec.it s "matches a view whose history says so" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.milledThisTurn = True}) Filter.Type.MilledThisTurn) "milled"

    Spec.it s "does not match a card that reached the graveyard another way" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.MilledThisTurn)) "not milled"

    -- Independent of the OTHER look-back atom, which reads the same log for a
    -- different entry: a milled card was never declared as an attacker.
    Spec.it s "is not implied by having attacked" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.attackedThisTurn = True}) Filter.Type.MilledThisTurn)) "attacked does not imply milled"

    -- CR 701.17a mills CARDS, and a player is not one.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.MilledThisTurn)) "player"

  Spec.describe s "DealtDamageThisTurn" $ do
    Spec.it s "matches a view whose history says so" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.dealtDamageThisTurn = True}) Filter.Type.DealtDamageThisTurn) "damaged"

    Spec.it s "does not match a creature nothing has damaged" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.DealtDamageThisTurn)) "not damaged"

    -- Independent of the two other look-back atoms, which read the same log for
    -- different entries: CR 510.1a has an attacker deal combat damage, but being
    -- declared as one is not being dealt any.
    Spec.it s "is not implied by having attacked" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.attackedThisTurn = True}) Filter.Type.DealtDamageThisTurn)) "attacked does not imply damaged"

    -- CR 120.1 lets a player be dealt damage, so this atom is not vacuous for a
    -- player the way the two above are: the fold reads the same field for either
    -- shape of candidate, and Filter.playerView's False is a default a caller
    -- with a board overwrites (Pawl.Engine.Count.playerView). Pawl.DamageSpec's
    -- Needle Drop case is where a real board fills it.
    Spec.it s "reads the same field for a player candidate" $ do
      Spec.assertBool s (Filter.matches self (aPlayer {Filter.dealtDamageThisTurn = True}) Filter.Type.DealtDamageThisTurn) "a damaged player"
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.DealtDamageThisTurn)) "and an undamaged one"

  Spec.describe s "AttachedTo" $ do
    -- Miracle Worker's "target Aura attached to a creature you control", which is
    -- the composition no nullary atom expresses (CR 109.5, CR 303.4b).
    let onYourCreature =
          Filter.Type.AttachedTo
            (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You])
        -- Crown of the Ages' "attached to a creature" and Aura Graft's "attached
        -- to a permanent" respectively, both spelled in the general form.
        onACreature = Filter.Type.AttachedTo (Filter.Type.HasCardType CardType.Creature)
        onAPermanent = Filter.Type.AttachedTo (Filter.Type.And [])

    Spec.it s "matches an attachment whose host satisfies the nest" $ do
      Spec.assertBool s (Filter.matches self (onHost aHost) onYourCreature) "on a creature you control"

    -- The pair that makes the CONTROL conjunct do work: one candidate, one nest,
    -- two hosts differing in controller and in nothing else. CR 109.5 makes "you"
    -- the perspective, so the SAME board answers the other way from the other
    -- seat -- without that a nest reading the CANDIDATE's controller would pass
    -- the positive above, the candidate being player 0's too.
    Spec.it s "does not match when the host is an opponent's" $ do
      Spec.assertBool s (not (Filter.matches self (onHost theirHost) onYourCreature)) "on their creature"
      Spec.assertBool s (Filter.matches other (onHost theirHost) onYourCreature) "and the other way round from their seat"

    -- The pair that makes the CARD TYPE conjunct do work, on a host the
    -- perspective does control.
    Spec.it s "does not match when the host is not a creature" $ do
      Spec.assertBool s (not (Filter.matches self (onHost aLandHost) onYourCreature)) "on a land you control"
      Spec.assertBool s (Filter.matches self (onHost aLandHost) onAPermanent) "but it IS attached to something"

    -- CR 109.3 names "what an Aura enchants" among the things that are not
    -- characteristics, so no characteristic axis can stand in for it: being an
    -- Aura by subtype says nothing about whether it is on anything.
    Spec.it s "is independent of every characteristic axis" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.subtypes = Set.singleton Subtype.Aura}) onACreature)) "subtype does not imply attachment"

    -- CR 110.1: the host view is filled only for an object ON THE BATTLEFIELD, so
    -- even the trivial nest is False for a candidate attached to nothing. This is
    -- what makes `AttachedTo (And [])` Aura Graft's "attached to a permanent"
    -- rather than "attached to anything".
    Spec.it s "does not match a candidate attached to nothing" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature onACreature)) "unattached"
      Spec.assertBool s (not (Filter.matches self blackCreature onAPermanent)) "not even the trivial nest"

    -- The old nullary pair's discriminating case, kept in the general form: CR
    -- 303.4 attaches an Aura to an object or a player, so "on a permanent" is
    -- strictly wider than "on a creature" and the implication runs one way only.
    Spec.it s "on a permanent is a wider question than on a creature" $ do
      Spec.assertBool s (Filter.matches self (onHost aLandHost) onAPermanent) "on a land: attached to a permanent"
      Spec.assertBool s (not (Filter.matches self (onHost aLandHost) onACreature)) "but not to a creature"

    -- CR 303.4b: a player is enchanted BY an Aura and is not itself attached to
    -- anything -- Object.attachedTo is a field of the attached permanent, and a
    -- player is not one. Curse of Death's Hold is therefore not a legal target for
    -- Aura Graft.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer onYourCreature)) "player, narrow nest"
      Spec.assertBool s (not (Filter.matches self aPlayer onAPermanent)) "player, trivial nest"

    -- The nest reads the HOST and never the candidate: the candidate is black and
    -- the host is not, so an evaluator that forgot to switch views would answer
    -- the other way.
    Spec.it s "reads the host and not the candidate" $ do
      let colorlessHost = aHost {Filter.colors = Set.empty}
      Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.HasColor Color.Black)) "the candidate is black"
      Spec.assertBool s (not (Filter.matches self (onHost colorlessHost) (Filter.Type.AttachedTo (Filter.Type.HasColor Color.Black)))) "its host is not"

    -- Pawl.Engine.Filter.boundSlots must pair with bakeBound, both of which descend
    -- into the nest. Nothing in the pool nests a ControlledByBound under an
    -- attachment, so this unit case is the only observer that arm has.
    Spec.it s "reports a bound slot nested under the attachment" $ do
      let slot = SlotName.MkSlotName (Text.pack "victim")
      Spec.assertEqWith s "the slot the host's description names" (Filter.boundSlots (Filter.Type.AttachedTo (Filter.Type.ControlledByBound slot))) (Set.singleton slot)

    -- CR 612.1's word swap reaches the host's description too, and nothing in
    -- data/cards/ observes it: grep the corpus for ChangeSubtypeWord and it is
    -- absent, so no card drives Filter.rewrite at all. This is that arm's only
    -- observer.
    Spec.it s "CR 612.1 rewrites a subtype inside the nest" $ do
      let swapped = Filter.rewrite [(Subtype.Swamp, Subtype.Island)] (Filter.Type.AttachedTo (Filter.Type.HasSubtype Subtype.Swamp))
      Spec.assertEqWith s "the host's subtype word is swapped" swapped (Filter.Type.AttachedTo (Filter.Type.HasSubtype Subtype.Island))

  Spec.describe s "HasAttached" $ do
    -- A Tale for the Ages' "enchanted creatures", which CR 303.4b makes a
    -- question about an AURA specifically, plus the trivial nest and the control
    -- conjunct Archon of the Wild Rose adds to it.
    let anAura = aHost {Filter.subtypes = Set.singleton Subtype.Aura, Filter.cardTypes = Set.singleton CardType.Enchantment}
        theirAura = anAura {Filter.controller = Just (PlayerId.MkPlayerId 1)}
        someGear = anAura {Filter.subtypes = Set.singleton Subtype.Equipment, Filter.cardTypes = Set.singleton CardType.Artifact}
        carrying attachers = blackCreature {Filter.attachedViews = attachers}
        byAnAura = Filter.Type.HasAttached (Filter.Type.HasSubtype Subtype.Aura)
        byAnything = Filter.Type.HasAttached (Filter.Type.And [])
        byYourAura = Filter.Type.HasAttached (Filter.Type.And [Filter.Type.HasSubtype Subtype.Aura, Filter.Type.ControlledBy PlayerRelation.You])

    Spec.it s "matches a candidate something matching the nest is attached to" $ do
      Spec.assertBool s (Filter.matches self (carrying [anAura]) byAnAura) "an Aura is on it"

    -- CR 303.4b's shape, stated on its own: a creature is enchanted once an Aura
    -- is attached to it, whatever else is attached alongside. An `all` would
    -- answer False here.
    Spec.it s "asks ANY of what is attached, not all" $ do
      Spec.assertBool s (Filter.matches self (carrying [someGear, anAura]) byAnAura) "an Equipment as well as an Aura"

    -- The pair that makes the nest do work, and the pair CR 303.4b and CR 301.5a
    -- keep apart: an equipped creature is not an enchanted one.
    Spec.it s "does not match when nothing attached satisfies the nest" $ do
      Spec.assertBool s (not (Filter.matches self (carrying [someGear]) byAnAura)) "only an Equipment"
      Spec.assertBool s (Filter.matches self (carrying [someGear]) byAnything) "but it DOES have something attached"

    Spec.it s "does not match a candidate carrying nothing" $ do
      Spec.assertBool s (not (Filter.matches self (carrying []) byAnAura)) "nothing attached"
      Spec.assertBool s (not (Filter.matches self (carrying []) byAnything)) "not even the trivial nest"

    -- The MIRROR stated as a pair: one view is attached to an Aura and the other
    -- has an Aura attached, and each atom answers for exactly one of them. Neither
    -- atom expresses the other, which is the whole reason both exist.
    Spec.it s "is not AttachedTo with the nest rewritten" $ do
      Spec.assertBool s (Filter.matches self (onHost anAura) (Filter.Type.AttachedTo (Filter.Type.HasSubtype Subtype.Aura))) "on an Aura"
      Spec.assertBool s (not (Filter.matches self (onHost anAura) byAnAura)) "but nothing is attached to it"
      Spec.assertBool s (not (Filter.matches self (carrying [anAura]) (Filter.Type.AttachedTo (Filter.Type.HasSubtype Subtype.Aura)))) "and the enchanted creature is on nothing"

    -- CR 109.5 makes the nest's "you" the ability's controller and never the
    -- candidate's: the candidate belongs to player 0 in both readings, so an
    -- evaluator reading the CANDIDATE's controller would answer the same way
    -- twice.
    Spec.it s "the nest's you is the perspective, not the candidate's controller" $ do
      Spec.assertBool s (not (Filter.matches self (carrying [theirAura]) byYourAura)) "their Aura, from your seat"
      Spec.assertBool s (Filter.matches other (carrying [theirAura]) byYourAura) "and the other way round from their seat"

    -- The nest reads the ATTACHER and never the candidate: the candidate is black
    -- and the Aura on it is not, so an evaluator that forgot to switch views would
    -- answer the other way.
    Spec.it s "reads the attacher and not the candidate" $ do
      let colorlessAura = anAura {Filter.colors = Set.empty}
      Spec.assertBool s (Filter.matches self (carrying [colorlessAura]) (Filter.Type.HasColor Color.Black)) "the candidate is black"
      Spec.assertBool s (not (Filter.matches self (carrying [colorlessAura]) (Filter.Type.HasAttached (Filter.Type.HasColor Color.Black)))) "the Aura on it is not"

    -- CR 109.3 keeps attachment off the characteristics, so no characteristic axis
    -- stands in for it -- being an Aura says nothing about carrying one.
    Spec.it s "is independent of every characteristic axis" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.subtypes = Set.singleton Subtype.Aura}) byAnAura)) "subtype does not imply carrying one"

    -- CR 303.4b does let an Aura enchant a PLAYER, so unlike AttachedTo's player
    -- case this False is a limitation rather than the rule.
    --
    -- Not implemented: an enchanted player (#2030).
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer byAnAura)) "player, narrow nest"
      Spec.assertBool s (not (Filter.matches self aPlayer byAnything)) "player, trivial nest"

    -- boundSlots pairs with bakeBound here for AttachedTo's reason, and nothing in
    -- the pool nests a ControlledByBound under this atom either.
    Spec.it s "reports a bound slot nested under the attachment" $ do
      let slot = SlotName.MkSlotName (Text.pack "victim")
      Spec.assertEqWith s "the slot the attacher's description names" (Filter.boundSlots (Filter.Type.HasAttached (Filter.Type.ControlledByBound slot))) (Set.singleton slot)

    -- CR 612.1's word swap reaches the attacher's description, and A Tale for
    -- the Ages does narrow the atom by a subtype for one to reach -- but this
    -- stays the arm's only observer, for the reason the AttachedTo case above
    -- gives.
    Spec.it s "CR 612.1 rewrites a subtype inside the nest" $ do
      let swapped = Filter.rewrite [(Subtype.Aura, Subtype.Equipment)] byAnAura
      Spec.assertEqWith s "the attacher's subtype word is swapped" swapped (Filter.Type.HasAttached (Filter.Type.HasSubtype Subtype.Equipment))

  Spec.describe s "IsAttachedToSource" $ do
    -- CR 701.3a / 301.5a: the candidate's HOST against the match's source. Object
    -- 7 is the source throughout, so the three cases below differ only in what the
    -- candidate is attached to.
    let framed = Filter.contextFor (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7))
    Spec.it s "matches a candidate attached to the source" $ do
      Spec.assertBool s (Filter.matches framed (blackCreature {Filter.attachedTo = Just (ObjectId.MkObjectId 7)}) Filter.Type.IsAttachedToSource) "on the source"

    -- The discriminating case, and the whole reason the field is an id rather than
    -- a Bool: an Equipment attached to SOME creature is not one attached to this
    -- creature, so Kemba's Legion does not count the Bonesplitter on another
    -- creature.
    Spec.it s "does not match a candidate attached to another object" $ do
      let elsewhere = (onHost aHost) {Filter.attachedTo = Just (ObjectId.MkObjectId 8)}
      Spec.assertBool s (not (Filter.matches framed elsewhere Filter.Type.IsAttachedToSource)) "on another creature"
      Spec.assertBool s (Filter.matches framed elsewhere (Filter.Type.AttachedTo (Filter.Type.HasCardType CardType.Creature))) "still attached to a creature"

    Spec.it s "does not match a candidate attached to nothing" $ do
      Spec.assertBool s (not (Filter.matches framed blackCreature Filter.Type.IsAttachedToSource)) "unattached"

    -- Vacuously False where nothing frames the match, IsSource's own posture.
    Spec.it s "no source in context is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.attachedTo = Just (ObjectId.MkObjectId 7)}) Filter.Type.IsAttachedToSource)) "no source"

    -- CR 303.4b: a player is enchanted BY an attachment and is attached to
    -- nothing itself, so there is no host id to compare.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches framed aPlayer Filter.Type.IsAttachedToSource)) "player"

  Spec.describe s "IsHostOfSource" $ do
    -- CR 303.4b: the SOURCE's host against the candidate, the direction the two
    -- atoms above cannot say. `blackCreature` is object 7 and `aHost` is object 8,
    -- so the cases below differ only in which id the context reports the source as
    -- attached to.
    let enchanting oid = (Filter.contextFor (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 9))) {Filter.sourceAttachedTo = Just (ObjectId.MkObjectId oid)}
    Spec.it s "matches the object the source is attached to" $ do
      Spec.assertBool s (Filter.matches (enchanting 7) blackCreature Filter.Type.IsHostOfSource) "the host itself"

    -- The discriminating case: an atom answering "is the source attached to
    -- anything" rather than "to THIS" would admit both candidates here.
    Spec.it s "does not match another object" $ do
      Spec.assertBool s (not (Filter.matches (enchanting 8) blackCreature Filter.Type.IsHostOfSource)) "not the host"
      Spec.assertBool s (Filter.matches (enchanting 8) aHost Filter.Type.IsHostOfSource) "the host is"

    -- The direction it is NOT: object 7 is attached to nothing, and the source is
    -- attached to object 7, so IsAttachedToSource and this atom disagree on the
    -- same board -- which is what makes the third direction a third atom.
    Spec.it s "is not IsAttachedToSource in disguise" $ do
      Spec.assertBool s (not (Filter.matches (enchanting 7) blackCreature Filter.Type.IsAttachedToSource)) "the candidate's own host is not the source"

    -- Vacuously False wherever the position does not supply the field, which is
    -- every context but the four -- contextFor's own posture.
    Spec.it s "an unsupplied host is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches (Filter.contextFor (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 9))) blackCreature Filter.Type.IsHostOfSource)) "no host supplied"

    -- CR 303.4's other destination: a source attached to a PLAYER names no object,
    -- and a player candidate has no id to be named, so both ends answer False.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches (enchanting 7) aPlayer Filter.Type.IsHostOfSource)) "player"

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

  -- CR 701.3a's other side, whose fixed object is the HOST rather than the
  -- permanent being moved. Structurally the atom above -- a Bool the caller that
  -- knows the fixed object supplies -- which is why the three cases mirror it.
  Spec.describe s "CanAttachToSubject" $ do
    Spec.it s "matches a view the caller marked as attachable to the fixed host" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.canAttachToSubject = True}) Filter.Type.CanAttachToSubject) "can attach"

    -- The two halves of rule 701.3a are INDEPENDENT: a view the caller framed as
    -- a legal destination says nothing about whether the candidate could itself
    -- be attached to something else, so neither field may be read for the other.
    Spec.it s "is independent of CanHostSubject" $ do
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.canHostSubject = True}) Filter.Type.CanAttachToSubject)) "hosting is not attaching"

    -- Vacuously False wherever no search frames the match, which is every view
    -- but the ones Pawl.Engine.Resolve's Effect.Search arm builds.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.CanAttachToSubject)) "player"

  -- CR 400.1. The gameplay-level proof that this is the zone a spell is CAST
  -- FROM -- CR 601.2's "from where it is" -- is Pawl.CastSpec's Drannith
  -- Magistrate group; these cases pin the atom itself.
  Spec.describe s "IsInZone" $ do
    Spec.it s "matches the zone the candidate is in and no other" $ do
      Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.IsInZone Zone.Battlefield)) "the view's own zone"
      Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.IsInZone Zone.Graveyard))) "and not a different one"

    -- Drannith Magistrate's "from anywhere other than their hands" is spelled
    -- `Not (IsInZone Hand)`, the one-relation-one-spelling posture IsToken's
    -- cases below state (#163). The pair is what makes it discriminating: the
    -- negation must be true of the battlefield view and false of a hand one, off
    -- boards differing in the zone alone.
    Spec.it s "Not IsInZone is how 'anywhere other than' is written" $ do
      Spec.assertBool s (Filter.matches self blackCreature (Filter.Type.Not (Filter.Type.IsInZone Zone.Hand))) "a battlefield card is somewhere other than a hand"
      Spec.assertBool s (not (Filter.matches self (blackCreature {Filter.zone = Just Zone.Hand}) (Filter.Type.Not (Filter.Type.IsInZone Zone.Hand)))) "and a card in a hand is not"

    -- Vacuously False with no object to ask: CR 400.1 puts OBJECTS in zones, and
    -- CR 109.1 makes a player none. `Not` is then vacuously TRUE there, which is
    -- why the prohibition reading it is asked only of a spell's own object.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer (Filter.Type.IsInZone Zone.Hand))) "player"
      Spec.assertBool s (not (Filter.matches self devoidBigCreature (Filter.Type.IsInZone Zone.Hand))) "and so is a view with no zone recorded"

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

  -- CR 701.27g. The atom itself is a bare field read; both of the rule's
  -- exclusions live in the BUILDER that fills the field, so the gameplay-level
  -- proof is Pawl.TransformSpec's TransformedPermanent group.
  Spec.describe s "Transformed" $ do
    Spec.it s "matches a view whose permanent has its back face up" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.transformed = True}) Filter.Type.Transformed) "transformed"
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.Transformed)) "front face up"

    -- CR 712.8d/e: which face is up is what the characteristics are read OFF,
    -- not one of them, so nothing on the characteristic axes implies it.
    Spec.it s "is independent of every characteristic axis" $ do
      Spec.assertBool s (not (Filter.matches self devoidBigCreature Filter.Type.Transformed)) "power does not imply transformed"

    -- CR 701.27g asks about a PERMANENT, and CR 109.1 makes a player none.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.Transformed)) "player"

  -- CR 701.54e's designation conjunct. Context-relative, so the same VIEW answers
  -- differently under `self` and `other`, which is the pair that pins "YOUR
  -- Ring-bearer" apart from "a Ring-bearer".
  Spec.describe s "IsRingBearer" $ do
    Spec.it s "matches a permanent designated for the perspective player" $ do
      Spec.assertBool s (Filter.matches self ringBearer Filter.Type.IsRingBearer) "designated for player 0"

    -- CR 701.54a's "YOUR Ring-bearer": the designation names a player, and an
    -- opponent's Ring-bearer is not yours. The discriminating case -- an atom that
    -- ignored the perspective and only asked "is anything designated" passes the
    -- case above and fails this one.
    Spec.it s "does not match another player's Ring-bearer" $ do
      Spec.assertBool s (not (Filter.matches other ringBearer Filter.Type.IsRingBearer)) "designated for someone else"

    Spec.it s "does not match an undesignated permanent" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.IsRingBearer)) "no designation"

    -- The posture ControlledBy and IsPlayer take: "your Ring-bearer" is
    -- unanswerable where there is no "you" (an off-battlefield search).
    Spec.it s "is vacuously false with no perspective" $ do
      Spec.assertBool s (not (Filter.matches noPerspective ringBearer Filter.Type.IsRingBearer)) "no perspective"

    -- CR 701.54b: Ring-bearer is a designation A PERMANENT can have.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.IsRingBearer)) "player"

  -- CR 702.112b's designation asked of a CANDIDATE. Perspective-free, unlike the
  -- atom above: rule 702.112b's marker names no player, so `self` and `other`
  -- must answer alike.
  Spec.describe s "HasDesignation Renowned" $ do
    Spec.it s "matches a renowned permanent" $ do
      Spec.assertBool s (Filter.matches self renownedCreature (Filter.Type.HasDesignation Designation.Renowned)) "renowned"

    Spec.it s "does not match an undesignated permanent" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.HasDesignation Designation.Renowned))) "no designation"

    -- The discriminating pair for "asks nothing of the perspective": an atom
    -- written on IsRingBearer's model would answer False here.
    Spec.it s "answers the same for another player's perspective" $ do
      Spec.assertBool s (Filter.matches other renownedCreature (Filter.Type.HasDesignation Designation.Renowned)) "not yours, still renowned"

    Spec.it s "and with no perspective at all" $ do
      Spec.assertBool s (Filter.matches noPerspective renownedCreature (Filter.Type.HasDesignation Designation.Renowned)) "no perspective"

    -- CR 702.112b: "only permanents can be or become renowned".
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer (Filter.Type.HasDesignation Designation.Renowned))) "player"

  -- CR 701.60b's designation asked of a CANDIDATE, renowned's shape above.
  Spec.describe s "HasDesignation Suspected" $ do
    Spec.it s "matches a suspected permanent" $ do
      Spec.assertBool s (Filter.matches self suspectedCreature (Filter.Type.HasDesignation Designation.Suspected)) "suspected"

    Spec.it s "does not match an undesignated permanent" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.HasDesignation Designation.Suspected))) "no designation"

    -- The pair that separates the designation from what CR 701.60c hangs off it:
    -- an atom reading the menace grant instead of Object.designations passes the
    -- positive above and this one.
    Spec.it s "does not match a permanent that merely has menace" $ do
      Spec.assertBool s (not (Filter.matches self (withKeyword Keyword.Menace) (Filter.Type.HasDesignation Designation.Suspected))) "menace is not the designation"

    -- And the pair that separates it from the OTHER designation on the same view
    -- record, which an arm ignoring the designation payload would read.
    Spec.it s "does not match a renowned permanent" $ do
      Spec.assertBool s (not (Filter.matches self renownedCreature (Filter.Type.HasDesignation Designation.Suspected))) "renowned is a different designation"

    -- Perspective-free for renowned's reason: rule 701.60b's marker names no
    -- player.
    Spec.it s "answers the same for another player's perspective, and for none" $ do
      Spec.assertBool s (Filter.matches other suspectedCreature (Filter.Type.HasDesignation Designation.Suspected)) "not yours, still suspected"
      Spec.assertBool s (Filter.matches noPerspective suspectedCreature (Filter.Type.HasDesignation Designation.Suspected)) "no perspective"

    -- CR 701.60b: "only permanents can have the suspected designation".
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer (Filter.Type.HasDesignation Designation.Suspected))) "player"

  -- CR 122.1's presence read, per KIND. Perspective-free for HasDesignation's reason:
  -- a counter belongs to no player.
  Spec.describe s "HasCounters" $ do
    Spec.it s "matches a permanent bearing that kind" $ do
      Spec.assertBool s (Filter.matches self counteredCreature (Filter.Type.HasCounters CounterKind.PlusOnePlusOne)) "+1/+1"

    -- The kind is not decoration: the same view bears lore counters and no -1/-1.
    Spec.it s "does not match on a kind it does not bear" $ do
      Spec.assertBool s (not (Filter.matches self counteredCreature (Filter.Type.HasCounters CounterKind.MinusOneMinusOne))) "-1/-1"

    Spec.it s "does not match a permanent with no counters at all" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature (Filter.Type.HasCounters CounterKind.PlusOnePlusOne))) "none"

    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer (Filter.Type.HasCounters CounterKind.PlusOnePlusOne))) "player"

  -- CR 602.1 / 605.1a, read off the one field the builders fill. Whether an
  -- ability is a mana ability is decided there, not here, so these cases are the
  -- atom's plumbing; Pawl.UntapRestrictionSpec is where the classification itself
  -- is proved against two real lands.
  Spec.describe s "HasNonManaActivatedAbility" $ do
    Spec.it s "matches a permanent whose view records one" $ do
      Spec.assertBool s (Filter.matches self (blackCreature {Filter.nonManaActivatedAbility = True}) Filter.Type.HasNonManaActivatedAbility) "has one"

    Spec.it s "does not match one whose view records none" $ do
      Spec.assertBool s (not (Filter.matches self blackCreature Filter.Type.HasNonManaActivatedAbility)) "vanilla"

    -- CR 109.1: a player is not an object and has no abilities.
    Spec.it s "a player candidate is vacuously false" $ do
      Spec.assertBool s (not (Filter.matches self aPlayer Filter.Type.HasNonManaActivatedAbility)) "player"
