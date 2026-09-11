-- Reading an object's characteristics: the printed and copiable
-- characteristics a projection starts from, the last-known view, control
-- grants and the controller question. Sits below Pawl.Engine.Projection,
-- which layers the continuous effects on top. Split out of it for size.
module Pawl.Engine.Projection.View where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Defender as Defender
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Star as Star
import qualified Pawl.Engine.Subtype as Subtype
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CharacteristicPT as CharacteristicPT
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Crewing as Crewing
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.HybridPhyrexian as HybridPhyrexian
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Milled as Milled
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Power as Power
import Pawl.Types.ProjectedCharacteristics (ProjectedCharacteristics)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prototype as Prototype
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RuleAbilities as RuleAbilities
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpecialAction as SpecialAction
import qualified Pawl.Types.StaticAbility as StaticAbility
import Pawl.Types.Timestamp (Timestamp)
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Zone as Zone

-- The view CR 608.2h's record answers with: viewWithLastKnownAnywhere's body,
-- shared with the ability-source read in viewOfCharacteristics, which needs it
-- at ITS caller's depth rather than fullView's.
lastKnownView :: Count.ViewOf -> ObjectId -> GameState -> LastKnown.LastKnown -> Filter.View
lastKnownView peers oid gs lk =
  (viewOfCharacteristics peers oid (LastKnown.characteristics lk) (Just (LastKnown.controller lk)) (LastKnown.counters lk) gs)
    { Filter.owner = Just (LastKnown.owner lk),
      Filter.token = Game.sourceIsToken (LastKnown.source lk),
      Filter.attacking = LastKnown.attacking lk,
      Filter.blocking = LastKnown.blocking lk
    }

-- The characteristics view of a printed card, from the FACE alone. The axes that
-- only an OBJECT can have are Nothing or empty, and each says so at its field.
--
-- Its readers are Pawl.ProjectionSpec's, which ask about a printed face with no
-- game around it, and Pawl.Engine.Replacement.matchesTokenLot, which asks about
-- a token that is proposed and not yet minted (CR 614.12). A reader that holds
-- an OBJECT takes viewOfObject instead, in whatever zone the object sits -- see
-- #1911, which moved the last of them.
viewOfCard :: Face.Face Card.Type.Card -> Filter.View
viewOfCard face =
  let typeLine = Face.typeLine face
   in Filter.MkView
        { -- CR 201.1 off the printed FACE. A multi-faced card's combined view
          -- carries the halves joined for rendering (Engine.Card.merge2); CR
          -- 709.4a's set is viewOfCharacteristics', which has an id to ask
          -- Game.namesOf about.
          Filter.names = Set.singleton (Face.name face),
          Filter.cardTypes = TypeLine.types typeLine,
          Filter.supertypes = TypeLine.supertypes typeLine,
          -- CR 604.3 / 702.114a: a CDA functions in all zones, and this view
          -- enters no CR 613 fold, so devoid is applied here.
          Filter.colors =
            if definesColorless (Face.keywords face)
              then Set.empty
              else printedColorsOf face,
          -- CR 604.3 / 702.73a, the same one layer down: changeling "works
          -- everywhere".
          Filter.subtypes =
            if definesEveryCreatureType (Face.keywords face)
              then Set.union Subtype.everyCreatureType (TypeLine.subtypes typeLine)
              else TypeLine.subtypes typeLine,
          -- CR 702: read off the printed face, like the type line above.
          Filter.keywords = Face.keywords face,
          -- CR 208.1 read off the PRINTED power box -- see printedPower below.
          Filter.power = printedPower face,
          -- CR 208.1's other half off the printed toughness box.
          Filter.toughness = printedToughness face,
          -- CR 202.3: printed on the card, and rule 202.3 names no zone.
          Filter.manaValue = Just (Quantity.manaValueOf face),
          Filter.controller = Nothing,
          -- CR 108.3 gives an owner to a card IN THE GAME; this builder describes
          -- a printed FACE, so there is nothing to read Object.owner off.
          -- viewOfCharacteristics is the view that holds an id and answers, in
          -- whatever zone the object sits.
          Filter.owner = Nothing,
          -- CR 400.1 / 109.1: a printed card being matched by a search is not an
          -- object, so there is no zone for IsInZone to read either.
          Filter.zone = Nothing,
          -- CR 601.2a: a printed FACE was never cast, so WasCastFrom is vacuously
          -- False against it -- `zone` above's reason.
          Filter.castFrom = Nothing,
          -- CR 115.1: a printed face is on no stack and targets nothing.
          Filter.targets = Set.empty,
          Filter.targetViews = Map.empty,
          -- Not an object, so no identity for IsSource to compare.
          Filter.identity = Nothing,
          Filter.playerIdentity = Nothing,
          -- CR 506.3 / 509.1a: a card off the battlefield never attacked or
          -- blocked; CR 303.4b: nor is it attached to anything.
          Filter.attacking = False,
          -- CR 508.1b: a printed face attacks nothing, for the reason above.
          Filter.attackingPlayer = Nothing,
          -- CR 508.1b: nor any planeswalker, for the same reason.
          Filter.attackingPlaneswalkerController = Nothing,
          -- CR 310.9d: nor any battle, for the same reason.
          Filter.attackingBattleProtector = Nothing,
          Filter.blocking = False,
          Filter.blocked = False,
          Filter.attackedThisTurn = False,
          -- CR 508.1a / 509.1a: a printed face is in no combat, for the reason
          -- `attacking` above is False.
          Filter.declaredAttackerThisCombat = False,
          -- CR 508.3b's other half, False for the same reason: a printed face is
          -- in no combat, so nothing was declared attacking it.
          Filter.declaredAttackedThisCombat = False,
          Filter.declaredBlockerThisCombat = False,
          -- CR 701.17a mills an OBJECT; this builder describes a printed FACE.
          -- viewOfCharacteristics is the view that holds an id and answers.
          Filter.milledThisTurn = False,
          -- CR 120.1a: damage is dealt to a battle, a creature or a
          -- planeswalker, and this builder describes a printed FACE rather than
          -- a permanent. viewOfCharacteristics is the view that holds an id and
          -- answers.
          Filter.dealtDamageThisTurn = False,
          -- CR 702.122c relates a creature to a Vehicle it crewed, and this
          -- builder describes a printed FACE rather than either --
          -- `milledThisTurn` above's reason.
          Filter.crewedThisTurn = Set.empty,
          -- CR 302.6 asks about an OBJECT a player controls; this builder
          -- describes a printed FACE, which is none -- `milledThisTurn` above's
          -- reason.
          Filter.controlledSinceTurnBegan = False,
          Filter.attachedToView = Nothing,
          -- CR 303.4b's mirror, and Nothing for the same reason: a printed face
          -- is not an object, so no permanent's Object.attachedTo names it.
          Filter.attachedViews = [],
          Filter.attachedTo = Nothing,
          -- CR 701.3a: only Pawl.Engine.Resolve's AttachTarget arm fills this
          -- field, and its candidates are battlefield permanents.
          Filter.canHostSubject = False,
          -- CR 701.3a's other side: only Pawl.Engine.Resolve's Effect.Search arm
          -- fills this field, and it overlays it onto viewOfObject rather than
          -- reaching this builder, which holds a printed FACE and no board.
          Filter.canAttachToSubject = False,
          -- CR 111.6: "A token isn't a card." CR 704.5d already made a token in
          -- any zone this builder describes cease to exist.
          Filter.token = False,
          -- CR 903.3's designation is made of a card in a DECK and answered of an
          -- object; a printed face carries neither, so IsCommander is vacuously
          -- False here, `token` above's posture.
          Filter.commander = False,
          -- CR 113.3b: an ability on the stack is never a printed face, so this
          -- builder's candidate cannot be one.
          Filter.activatedAbility = False,
          -- CR 113.1c, the line above widened to CR 113.3's other kind: a printed
          -- face is no ability on the stack either.
          Filter.ability = False,
          -- CR 114.5: an emblem is not a card (CR 114.3 leaves it no printed
          -- face), so this builder's candidate cannot be one.
          Filter.emblem = False,
          -- CR 113.7, for the line above's reason: no ability, so no source.
          Filter.abilitySource = Nothing,
          Filter.tapped = False,
          -- CR 110.5d: only permanents have status, and this is a printed FACE
          -- with no object behind it -- the rule's own answer rather than an
          -- unknown, `transformed` below's reason one status category over.
          Filter.faceDown = False,
          -- Nothing rather than this very view: CR 708.12's subject is the card
          -- representing an object, and this builder IS a printed face, so a
          -- self-reference would only recur. Filter.representedCard says so.
          Filter.representedCard = Nothing,
          -- CR 406.3 writes its rider onto an object in exile, and this is a
          -- printed FACE with no object behind it -- the line above's reason,
          -- one rule over.
          Filter.exiledFaceDown = False,
          -- CR 701.27g asks about a permanent on the battlefield; this is a
          -- printed FACE with no object behind it, so the rule's own answer is
          -- False rather than an unknown.
          Filter.transformed = False,
          -- CR 122.1a-b: a counter can sit on a card off the battlefield, but this
          -- builder describes a printed FACE, so there is nothing to be on.
          Filter.counters = Map.empty,
          -- CR 701.54b: the designation rides an OBJECT, and CR 701.54a gives it
          -- only to a battlefield permanent.
          Filter.ringBearerFor = Nothing,
          -- The designations ride an OBJECT, and each of those rules gives its
          -- designation only to a permanent.
          Filter.designations = Set.empty,
          -- CR 701.37c's X is set on a PERMANENT -- `designations` above, same
          -- sentence.
          Filter.designationValues = Map.empty,
          -- CR 716.2b gives a level to a PERMANENT, and this builder describes a
          -- printed face.
          Filter.classLevel = Nothing,
          Filter.kicked = Map.empty,
          -- CR 601.2h pays the cost of a SPELL, and this builder describes a
          -- printed face.
          Filter.manaSpentTags = Set.empty,
          -- CR 602.1 / 605.1a off the PRINTED face: the card's printed abilities
          -- plus rule 702's HAND ones (CR 702.29b, CR 702.77b) and GRAVEYARD ones
          -- (CR 702.84a), not the battlefield ones, which are minted from the
          -- post-layer keyword map.
          Filter.nonManaActivatedAbility =
            not
              ( all
                  ManaAbility.isManaAbility
                  (Face.activatedAbilities face <> Keyword.handAbilitiesOf (Face.keywords face) <> Keyword.graveyardAbilitiesOf (Face.keywords face))
              ),
          -- CR 602.1 over the same three lists, without CR 605.1a's exclusion --
          -- Zirda, the Dawnwaker's companion condition is read here, since a card
          -- outside the game has no object to project.
          --
          -- CR 305.6's intrinsic ability is the disjunct, through the reader every
          -- view builder shares: a Mountain must answer the same here as it does
          -- on the battlefield.
          Filter.hasActivatedAbility =
            not (null (Face.activatedAbilities face <> Keyword.handAbilitiesOf (Face.keywords face) <> Keyword.graveyardAbilitiesOf (Face.keywords face)))
              || Subtype.intrinsicManaAbility (TypeLine.types typeLine) (TypeLine.subtypes typeLine),
          -- CR 702.184c reaches a permanent's CONTROLLER; this builder describes
          -- a printed FACE with no controller and no board to grant it one.
          Filter.grantsStationToughness = False
        }

-- CR 208.1's PRINTED power box, for a card off the battlefield. Nothing for a
-- face with no power box, since CR 208.1 gives power only to creature cards.
--
-- CR 208.2b's zero is the STAR's answer here, and only here. Deliberately NOT
-- Quantity.evaluate's Star arm, which stays Nothing: there a star that survived
-- baseCharacteristics is a hole rather than a zero. A face with a characteristicPT
-- answers Nothing here, since CR 208.2a's number is applyCharacteristicPT's, in
-- every zone (CR 604.3).
printedPower :: Face.Face Card.Type.Card -> Maybe Integer
printedPower face = case Face.characteristicPT face of
  Just _ -> Nothing
  Nothing -> case fmap Power.unwrap (Face.power face) of
    Just (Quantity.Type.Literal n) -> Just n
    Just Quantity.Type.Star -> Just 0
    -- Every other shape: no power box at all, or a box holding neither a number
    -- nor CR 208.2's bare star. The latter is unreachable -- a composite box like
    -- 1+* comes with a characteristicPT and left through the arm above.
    _ -> Nothing

-- printedPower's mirror, arm for arm, on the printed toughness box. CR 208.2b's
-- sentence names power and toughness together, so the star reads 0 here too.
printedToughness :: Face.Face Card.Type.Card -> Maybe Integer
printedToughness face = case Face.characteristicPT face of
  Just _ -> Nothing
  Nothing -> case fmap Toughness.unwrap (Face.toughness face) of
    Just (Quantity.Type.Literal n) -> Just n
    Just Quantity.Type.Star -> Just 0
    _ -> Nothing

-- CR 508.3a: does this event record THIS object being declared as an attacker?
-- Only Combat.declareAttackers appends one, so CR 508.4's creature put onto the
-- battlefield attacking stays out.
declaredIt :: ObjectId -> GameEvent.GameEvent -> Bool
declaredIt oid event = case event of
  GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared declared _ _ _) -> declared == oid
  _ -> False

-- CR 701.17a: does this event record THIS object as one of a mill's cards? Only
-- Resolve's Mill arm appends one, so a surveil's or an explore's bin stays out.
milledIt :: ObjectId -> GameEvent.GameEvent -> Bool
milledIt oid event = case event of
  GameEvent.Milled (Milled.MkMilled _ cards) -> Foldable.elem oid cards
  _ -> False

-- CR 702.122c: if this event records a crewing THIS object paid for, which
-- Vehicle it crewed. GameEvent.Crewed, written as the cost is paid, so a crew
-- ability that never resolves still leaves the relation behind.
crewedByIt :: ObjectId -> GameEvent.GameEvent -> Maybe ObjectId
crewedByIt oid event = case event of
  GameEvent.Crewed crewed
    | Set.member oid (Crewing.crewedBy crewed) -> Just (Crewing.vehicle crewed)
  _ -> Nothing

-- CR 302.6: has `controller` had this object under their control continuously
-- since their most recent turn began? Object.sickness is the engine's record of
-- rule 302.6's condition, and it names a PLAYER because the rule's subject is
-- one: Pawl.Engine.Engine.checkControlContinuity drops a `Settled p` the moment p stops
-- controlling the object, so the comparison is what makes a creature stolen this
-- turn answer False under its NEW controller as well as its old one.
settledUnder :: ObjectId -> Maybe PlayerId.PlayerId -> GameState -> Bool
settledUnder oid controller gs = case (Game.lookupObject oid gs, controller) of
  (Just object, Just pid) -> Object.sickness object == Sickness.Settled pid
  _ -> False

-- Shared assembly: fill a View from a projection's characteristics, a supplied
-- controller and supplied counters.
--
-- Counters and `peers` come in as arguments because CR 109.3's characteristic
-- list holds neither: only the caller knows whether it reads a live object or CR
-- 608.2h's record of one, and only the caller knows how deep the fold it stands
-- in has got. `peers` must be a bounded reader -- a full projection taken here
-- re-enters gather, whose CR 604.2 gate is object-independent, and loops.
--
-- A FENCE the compiler cannot keep: filterReadsPeers enumerates the fields filled
-- through `peers` below, so filling a new one -- or repointing an existing one at
-- `peers` -- means giving the atom that reads it a True arm there. Nothing warns
-- if you do not; CR 613.8a's cheap arm in projectDeciding would just start
-- skipping real dependencies.
viewOfCharacteristics :: Count.ViewOf -> ObjectId -> ProjectedCharacteristics -> Maybe PlayerId.PlayerId -> Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural -> GameState -> Filter.View
viewOfCharacteristics peers oid pc controller counters gs =
  Filter.MkView
    { -- CR 201.1 / 709.4a off the PROJECTION: names are copiable (CR 707.2), so a
      -- Clone answers to what it copied.
      Filter.names = PC.names pc,
      Filter.cardTypes = PC.cardTypes pc,
      -- CR 205.4 / 613.1d off the PROJECTION: layer 4 writes supertypes too.
      Filter.supertypes = PC.supertypes pc,
      Filter.colors = PC.colors pc,
      Filter.subtypes = PC.subtypes pc,
      -- CR 109.3 / 613.1f: abilities are characteristics and layer 6 writes them.
      -- keysSet because PC.keywords counts instances and HasKeyword asks membership.
      Filter.keywords = Map.keysSet (PC.keywords pc),
      Filter.power = PC.power pc,
      Filter.toughness = PC.toughness pc,
      -- CR 202.3 / 707.2 off the PROJECTION: mana cost is copiable, so layer 1
      -- replaces it. The printed cost is read in baseCharacteristics.
      Filter.manaValue = PC.manaValue pc,
      Filter.controller = controller,
      -- CR 108.3 / 110.2 / 111.2: read off the OBJECT rather than through the
      -- `controller` parameter, since layer 2 has already moved control and
      -- nothing moves ownership. Nothing for an id naming nothing, which CR 608.2b
      -- wants of a gone target; viewWithLastKnownAnywhere writes CR 608.2h's answer
      -- over it for the readers owed one (see #1069, whose remaining half is
      -- Count.viewOfSnapshot's SpellCast arm).
      Filter.owner = fmap Object.owner (Game.lookupObject oid gs),
      -- CR 400.1 off the OBJECT beside its owner, and for `owner`'s reason: CR
      -- 109.3 counts no zone among the characteristics, so no projection carries
      -- one. Nothing for an id naming nothing (CR 608.2h, #1069).
      Filter.zone = fmap Object.zone (Game.lookupObject oid gs),
      -- CR 601.2a off the OBJECT beside its zone, and for that field's reason
      -- squared: no projection carries a zone, and CR 400.7 leaves the spell no
      -- memory of the one it came from either, so Pawl.Engine.Cast's two stamps
      -- are the only place the answer exists. Nothing for an id naming nothing
      -- and for every object that was never cast.
      Filter.castFrom = Game.lookupObject oid gs >>= Object.castFrom,
      -- CR 115.1 off the OBJECT's bindings, live: CR 601.2c fixed the targets and
      -- CR 115.7 can move them, so nothing here is a stamp. Empty for an id
      -- naming nothing and for everything off the stack.
      Filter.targets = maybe Set.empty (targetsOfStackObject gs) (Game.lookupObject oid gs),
      -- CR 115.1's targets one indirection along, filled beside them and lazily:
      -- nothing forces this map but Filter.TargetsOnlyOne's nest.
      Filter.targetViews = maybe Map.empty (targetViewsOfStackObject peers gs) (Game.lookupObject oid gs),
      Filter.identity = Just oid,
      Filter.playerIdentity = Nothing,
      -- CR 508.1k: a combat status, not a characteristic (CR 109.3). CR 506.4 takes
      -- a departed creature out of the record, so lastKnownView above writes CR
      -- 608.2h's answer over this one.
      Filter.attacking = Game.isAttacking oid gs,
      -- CR 508.1b: the same map's VALUE, kept only when it names a player. A
      -- creature attacking a planeswalker or a battle answers Nothing here and
      -- True above, which is CR 509.1a's and CR 802.4a's own three-way split --
      -- deliberately NOT Pawl.Engine.Defender.playerOfAttacker, which answers CR
      -- 508.5 and would fold all three into one player.
      Filter.attackingPlayer = case Map.lookup oid (Combat.attackers (GameState.combat gs)) of
        Just (AttackTarget.OfPlayer pid) -> Just pid
        _ -> Nothing,
      -- CR 508.1b: the SAME map's value, kept only when it names a planeswalker,
      -- and then followed to that planeswalker's CONTROLLER -- which CR 613.1b
      -- lets layer 2 move, so a Confiscated planeswalker answers the Aura's
      -- controller and not CR 108.3's owner.
      --
      -- Battlefield membership guards the read, as the battle arm below and
      -- Defender.playerOf's do: CR 506.4 removes a planeswalker from combat when it
      -- phases out or leaves the battlefield, while CR 506.4c keeps the creature
      -- attacking with nothing to read -- and Game.removeFromCombat deletes only the
      -- departed permanent's own key, exactly as rule 506.4c demands, so the entry
      -- naming it survives. Neither half is wrong; the composition was. Without the
      -- guard controllerOf still answers, off GameState.objects, which CR 702.26d
      -- keeps a phased-out permanent in -- the read Pawl.Types.GameState's phasedOut
      -- field already names as the wrong one there. Pawl.CombatEffectSpec's "CR
      -- 506.4c a planeswalker that phases out stops being attacked" pair is the board.
      --
      -- The two clauses of rule 506.4 that leave the object on the battlefield under
      -- the same id are the other two conjuncts, and each is its own leg of
      -- Pawl.CombatEffectSpec's Aura Graft pair:
      --
      -- CONTROLLER, compared against the seat recorded as this creature joined
      -- combat (Pawl.Types.Combat's attackedUnder), which is rule 506.4's own
      -- comparand. Defender.defendingPlayers is the fallback where nothing was
      -- recorded -- a combat record built by hand -- and only an approximation of
      -- it: CR 506.2 admits only a defending player's planeswalkers into a
      -- declaration, so at TWO seats "its controller changes" and "its controller
      -- is no longer a defending player" name the same planeswalkers, while CR
      -- 802.2's several defending players tell them apart. MEMBERSHIP rather than
      -- Maybe equality on that path, which two Nothings satisfy; the arm answers
      -- Nothing there either way.
      --
      -- A REGRESSION FENCE rather than a proven behavior, the battle arm's
      -- posture below: THE RECORD already answers at every moment CR 117.5
      -- samples, so mutating this conjunct to True leaves the suite green. Not
      -- implemented: a board that separates the two readings (#2839).
      --
      -- CARD TYPE, through `peers`, which is what makes rule 506.4's planeswalker
      -- clause reachable without Projection.isPlaneswalkerOf: that one calls project,
      -- the re-entry `peers` exists to keep out of this function, while `peers` is
      -- the caller's own bounded reader -- so a Song of the Dryads in layer 4 is seen
      -- by every caller whose depth has passed layer 4, and by no caller that has
      -- not. Only PC.cardTypes is forced, and only for an id in Combat.attackers'
      -- VALUES: no permanent is ever both, since a declaration wants the attacker
      -- under the active player and the attacked planeswalker under the defending
      -- one, so this cannot re-enter itself.
      --
      -- THE RECORD, first and above all three: rule 506.4 lists EVENTS, so a
      -- controller who changes and changes back inside one combat leaves the
      -- planeswalker removed while every conjunct below re-derives it back in.
      -- Combat.attackingNothing is what makes the removal stick, keyed by this
      -- attacker; Pawl.Types.Combat says why it is stored, and
      -- Pawl.Engine.Combat.noteAttackingNothing is its one writer. The conjuncts
      -- stay because that record is sampled at CR 117.5's moments and this
      -- function is asked at every other one.
      --
      -- controllerOf is the lean fold rather than a projection, which is what
      -- makes it safe here: `viewUpTo` already calls it for the candidate's own
      -- controller from INSIDE the CR 613 layer fold, so asking it for one more
      -- object re-enters nothing that `peers` guards against.
      Filter.attackingPlaneswalkerController = case Map.lookup oid (Combat.attackers (GameState.combat gs)) of
        Just (AttackTarget.OfPlaneswalker pw)
          | Set.notMember oid (Combat.attackingNothing (GameState.combat gs)),
            Set.member pw (GameState.battlefield gs),
            Maybe.maybe
              (List.any (\defending -> controllerOf pw gs == Just defending) (Defender.defendingPlayers gs))
              (\seat -> controllerOf pw gs == Just seat)
              (Map.lookup oid (Combat.attackedUnder (GameState.combat gs))),
            any (Set.member CardType.Planeswalker . Filter.cardTypes) (peers pw) ->
              controllerOf pw gs
        _ -> Nothing,
      -- CR 310.9d: the SAME map's last arm, followed to the attacked battle's
      -- PROTECTOR -- the seat that rule substitutes for the defending player while
      -- the battle is being attacked, and not the battle's controller.
      --
      -- Battlefield membership guards the read the way Defender.playerOf's battle
      -- arm does: CR 506.4 stops a departed battle being attacked while CR 506.4c
      -- keeps the creature attacking, so the honest answer there is Nothing rather
      -- than a protector.
      --
      -- The GUARD is a regression fence rather than a proven behavior. CR 400.7
      -- leaves the object that reaches the new zone with no designation to read
      -- either (Pawl.BattleSpec, "CR 400.7 a battle that leaves the battlefield
      -- forgets its protector"), so dropping it changes no board pawl can build
      -- and mutating it to True leaves the suite green.
      --
      -- Battle.protectorOf is an Object.protector lookup and reads no projection,
      -- so unlike controllerOf above it re-enters nothing at all.
      --
      -- The other two conjuncts are rule 506.4's battle clauses, arm for arm with the
      -- planeswalker field above and with Combat.stillAttackedBattle's own list: the
      -- PROTECTOR compared against Defender.defendingPlayers -- membership, for the
      -- planeswalker field's reason -- which CR 310.9d makes the
      -- defending player while a battle is attacked, and the CARD TYPE through
      -- `peers`. The type conjunct is load-bearing precisely because CR 310.9g keeps
      -- the designation when a permanent stops being a battle, so Battle.protectorOf
      -- goes on answering; Pawl.BattleSpec's "CR 506.4 a battle that stops being a
      -- battle" pair is the board.
      --
      -- The PROTECTOR conjunct is a regression fence rather than a proven behavior:
      -- mutating it away leaves the suite green, CR 310.9f's change needing an
      -- effect that moves a designation. Not implemented: any such effect (#2980).
      --
      -- Combat.attackingNothing leads here as it does above, and for the same
      -- reason: it is the only one of the four that remembers a removal rather
      -- than re-deriving it.
      Filter.attackingBattleProtector = case Map.lookup oid (Combat.attackers (GameState.combat gs)) of
        Just (AttackTarget.OfBattle battle)
          | Set.notMember oid (Combat.attackingNothing (GameState.combat gs)),
            Set.member battle (GameState.battlefield gs),
            List.any (\defending -> Battle.protectorOf battle gs == Just defending) (Defender.defendingPlayers gs),
            any (Set.member CardType.Battle . Filter.cardTypes) (peers battle) ->
              Battle.protectorOf battle gs
        _ -> Nothing,
      -- CR 509.1g: likewise. Combat.blockers is keyed by ATTACKER, so blocking is
      -- membership in some attacker's set rather than a key lookup. CR 506.4 takes
      -- a departed creature out of the record, so viewWithLastKnownAnywhere writes
      -- CR 608.2h's answer over this one too.
      Filter.blocking = Game.isBlocking oid gs,
      -- CR 509.1h: the key lookup the line above is careful not to be. Stays True
      -- once every creature blocking it has left combat.
      Filter.blocked = Map.member oid (Combat.blockers (GameState.combat gs)),
      -- CR 608.2i: from the turn's event log, which CR 511.3 does not clear.
      Filter.attackedThisTurn = any (declaredIt oid . LoggedEvent.event) (GameState.events gs),
      -- CR 508.1a / 509.1a: from the COMBAT record, which CR 511.3 does clear --
      -- and not from that same log, which cannot answer it. CR 508.1k and CR
      -- 509.1g put the AttackerDeclared and BecameBlocking events after the
      -- payment these two are read during, so a fold over them would be False
      -- for exactly the creatures being declared.
      Filter.declaredAttackerThisCombat = Set.member oid (Combat.declaredAttackers (GameState.combat gs)),
      -- CR 508.3b: the same record's other half, indexed by TARGET rather than by
      -- attacker. A permanent is named as AttackTarget.OfPlaneswalker or
      -- AttackTarget.OfBattle; Pawl.Engine.Count.playerView answers CR 508.3b's
      -- third subject off the same set.
      Filter.declaredAttackedThisCombat =
        Set.member (AttackTarget.OfPlaneswalker oid) (Combat.declaredAttacked (GameState.combat gs))
          || Set.member (AttackTarget.OfBattle oid) (Combat.declaredAttacked (GameState.combat gs)),
      Filter.declaredBlockerThisCombat = Set.member oid (Combat.declaredBlockers (GameState.combat gs)),
      -- CR 701.17a / 608.2i: the same log, read for the mills.
      Filter.milledThisTurn = any (milledIt oid . LoggedEvent.event) (GameState.events gs),
      -- CR 120.1 / 608.2i: the same log again, read for the damage. Never
      -- Object.damage -- CR 120.6 removes the marks on a regeneration and CR
      -- 120.3d/120.3e mark none at all for wither or infect, and either creature
      -- was still dealt damage this turn.
      Filter.dealtDamageThisTurn = any ((== Just oid) . Game.damagedObject . LoggedEvent.event) (GameState.events gs),
      -- CR 702.122c / 608.2i: the same log once more, read for the crewings this
      -- candidate paid for. The VEHICLES, which is the half of the relation a
      -- candidate can answer; Pawl.Engine.Filter's CrewedSourceThisTurn compares
      -- them against the source it is evaluating for.
      Filter.crewedThisTurn = Set.fromList (Maybe.mapMaybe (crewedByIt oid . LoggedEvent.event) (Foldable.toList (GameState.events gs))),
      -- CR 302.6: Object.sickness, compared against the PROJECTED controller
      -- rather than read as a bare flag -- rule 302.6's subject is a player, so
      -- `Settled` names one, and Pawl.Engine.Engine.checkControlContinuity drops a
      -- settle whose player stopped controlling the object.
      Filter.controlledSinceTurnBegan = settledUnder oid controller gs,
      -- CR 701.3a: not a characteristic, so the attachment comes off
      -- Object.attachedTo -- but the HOST's characteristics are projected, so it
      -- arrives as a view of its own read through `peers` (CR 613.1). CR 303.4 /
      -- 110.1: narrowed to a host on the battlefield, which is what makes
      -- `AttachedTo (And [])` mean "attached to a permanent". The view under the
      -- Just must stay lazy -- `peers` will project again.
      Filter.attachedToView =
        Game.lookupObject oid gs
          >>= Object.attachedTo
          >>= Recipient.objectOf
          >>= \host -> if Set.member host (GameState.battlefield gs) then peers host else Nothing,
      -- CR 303.4b / 301.5a with the arrow turned round: the permanents attached TO
      -- this candidate, Pawl.Engine.Game.attachments' sweep. Each attacher's own
      -- characteristics come through `peers`, at this caller's depth, which is
      -- what keeps a HasAttached reached from inside the layer fold out of a loop.
      -- The list must stay lazy in its spine: nothing forces the sweep unless a
      -- Filter names the atom.
      Filter.attachedViews = Maybe.mapMaybe peers (Set.toList (Game.attachments oid gs)),
      -- CR 701.3a / 301.5a: the same attachment as the HOST'S ID -- IsAttachedToSource
      -- compares it against the match's source, which this builder does not know.
      -- Not narrowed to the battlefield the way `attachedToView` is.
      Filter.attachedTo = hostOf oid gs,
      -- CR 701.3a: filled only by Resolve's AttachTarget arm, the one place that
      -- knows what is being moved.
      Filter.canHostSubject = False,
      -- CR 701.3a's other side: filled only by Resolve's Effect.Search arm, the
      -- one place that can resolve Pawl.Types.Search.subject to the host the
      -- instruction fixed. Overlaid onto this
      -- builder's result rather than passed in, so a search pays for it only when
      -- its filter names the atom.
      Filter.canAttachToSubject = False,
      -- CR 111.6: fixed for the life of the object (CR 400.7). False for an id
      -- naming nothing, which CR 608.2b wants of a gone TARGET;
      -- viewWithLastKnownAnywhere writes CR 608.2h's answer over it for the
      -- readers owed one, exactly as `owner` above has it.
      Filter.token = Game.isToken oid gs,
      -- CR 903.3, read off the owner's designation for `token` above's reason: it
      -- is fixed before the game begins (CR 702.124a) and no rule rewrites it.
      -- False for an id naming nothing.
      Filter.commander = Commander.isCommander oid gs,
      -- CR 113.3b, read off Object.source for `token` above's reason: which of CR
      -- 113.3's kinds an ability is is fixed for the life of the object. False
      -- for an id naming nothing, and for every object that is not an ability on
      -- the stack.
      Filter.activatedAbility = Game.isActivatedAbility oid gs,
      -- CR 113.1c, the line above widened to take CR 113.3c's triggered kind as
      -- well, and read off Object.source in the same way.
      Filter.ability = Game.isAbility oid gs,
      -- CR 114.5, read off Object.source for the two lines above's reason. False
      -- for an id naming nothing, and for every object that is not an emblem.
      Filter.emblem = Game.isEmblem oid gs,
      -- CR 113.7: the source's view, through `peers` at this caller's depth while
      -- it exists (attachedViews' reason) and through CR 113.7a's last known
      -- information once it has left -- Green Slime countering the ability of an
      -- artifact sacrificed to activate it still reads "from an artifact source".
      -- Nothing for anything that is not an ability on the stack. Lazy, so a
      -- Filter that never names the atom pays nothing.
      Filter.abilitySource =
        Game.abilitySourceOf oid gs >>= \src ->
          if Map.member src (GameState.objects gs)
            then peers src
            else fmap (lastKnownView peers src gs) (Map.lookup src (GameState.lastKnown gs)),
      Filter.tapped = Game.isTapped oid gs,
      -- CR 110.5's other status, and the only site that fills the field. Read off
      -- Object.facing, never off the projection: CR 110.5a says status is not a
      -- characteristic in as many words. Never Object.exiledFaceDown beside it,
      -- which CR 110.5d says has no correlation to this.
      --
      -- The battlefield conjunct is CR 110.5d's own -- only permanents have
      -- status -- and is a REGRESSION FENCE rather than a proved behaviour, for
      -- `transformed` below's reason: the object it excludes is a face-down SPELL
      -- on the stack (CR 708.4), which is reachable, but every pool that reaches
      -- the atom today is already scoped to the battlefield, so dropping the
      -- conjunct leaves the suite green.
      Filter.faceDown = Set.member oid (GameState.battlefield gs) && maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid gs),
      -- CR 708.12's "ignoring any continuous effects", and the only site that
      -- fills the field: the card representing this object, read off
      -- Game.faceUpFaceOf so that CR 708.2a's substitution in Game.faceOf does not
      -- reach it. Nothing where no card is behind the object -- a token, an
      -- ability on the stack -- and Filter.RepresentedByCard is False there.
      --
      -- NOT scoped to the battlefield, unlike `faceDown` above: CR 708.12's read
      -- is of a card, which an object in any zone either has or has not.
      Filter.representedCard = fmap viewOfCard (Game.faceUpFaceOf oid gs),
      -- CR 406.3's rider, and the only site that fills the field. NO zone
      -- conjunct beside it, unlike the line above: Object.exiledFaceDown is
      -- per-incarnation state that only the move into exile writes, and CR 400.7
      -- mints a fresh incarnation on the way out, so the object it is True of is
      -- in exile by construction.
      Filter.exiledFaceDown = maybe False Object.exiledFaceDown (Game.lookupObject oid gs),
      -- CR 701.27g's three conjuncts, and the only site that fills the field. The
      -- face is read CURRENT -- Game.isFrontFaceUp reads Object.face, never the
      -- Object.turnedOverAt beside it -- which is the rule's first exclusion, a
      -- permanent front face up being untransformed however it got there. The
      -- second is that "an object represented by more than one card, such as a
      -- melded or merged permanent, is never considered a transformed permanent,
      -- even if it has components that are back face up", which Game.componentsOf
      -- answers for a melded permanent and for a merged one alike.
      --
      -- That conjunct is LOAD-BEARING, and the first exclusion does not stand in
      -- for it. A melded permanent is stamped Object.face = Nothing
      -- (Pawl.Engine.Event.meld) and Game.turnFaceOver refuses to turn it over (CR
      -- 712.4c), but neither is the only writer of that field: CR 616.1's entry
      -- loop runs over a melded permanent like any other entry, and
      -- Pawl.Engine.Event's EntryRewrite.EntersTransformed arm writes Object.face
      -- outright -- so a combined face printing daybound enters back face up at
      -- night (CR 702.145b, ranked by CR 616.1d; not CR 712.13a, which is the
      -- stack road alone) and this conjunct is the only thing answering.
      -- Pawl.MeldSpec's "CR 701.27g a melded permanent that entered with its back
      -- face up is still not one" is that board, and dropping the conjunct
      -- reddens it. That board melds into a daybound double-faced card, which no
      -- printed meld pair combines into -- the combined face is card data the
      -- opcode carries, the same stand-in Game.turnFaceOver's CR 712.4c arm is
      -- proved by -- so the rule is read where the printings cannot reach it.
      --
      -- The battlefield conjunct is a REGRESSION FENCE rather than a proved
      -- behaviour: every Count that reaches the atom is already scoped to a
      -- zone, so dropping it leaves the suite green. It is CR 701.27g's own
      -- wording, and the object it excludes -- a double-faced spell on the stack
      -- with its back face up (CR 712.11a) -- is reachable, so the conjunct
      -- stays.
      Filter.transformed =
        Set.member oid (GameState.battlefield gs)
          && not (Game.isFrontFaceUp oid gs)
          && maybe True (Seq.null . Game.componentsOf . Object.source) (Game.lookupObject oid gs),
      Filter.counters = counters,
      -- CR 701.54b: a designation rather than a characteristic. Nothing for an id
      -- naming no object -- a designation dies with the permanent (CR 400.7).
      Filter.ringBearerFor = Game.lookupObject oid gs >>= Object.ringBearerFor,
      -- Designations rather than characteristics: ringBearerFor's posture above.
      Filter.designations = maybe Set.empty Object.designations (Game.lookupObject oid gs),
      -- CR 701.37c: the number the mark was set with, read live off the object
      -- beside the mark itself -- `designations`' posture above.
      Filter.designationValues = foldMap Object.designationValues (Game.lookupObject oid gs),
      -- CR 716.2b: a designation too, so `designations`' posture again -- and
      -- Nothing for an id naming no object leaves CR 716.2d to answer level 1 at
      -- the read, which is what a CR 608.2h asker gets for a permanent that is
      -- gone.
      Filter.classLevel = Game.lookupObject oid gs >>= Object.classLevel,
      -- CR 702.33d: read live off the object, so the CR 608.2h path answers "not
      -- kicked" for a spell that has left the stack.
      Filter.kicked = foldMap Object.kicked (Game.lookupObject oid gs),
      -- CR 400.7d / CR 107.4h: read live off the object like `kicked`, and
      -- flattened to the tags here because that is the whole of what the
      -- vocabulary asks (see the field's own comment in Pawl.Engine.Filter). The
      -- object may be a CR 602.2a ability on the stack as well as a spell or the
      -- permanent one became; every one of them carries the field.
      Filter.manaSpentTags = foldMap (foldMap ManaUnit.tags . Mana.unwrap . Object.manaSpent) (Game.lookupObject oid gs),
      -- CR 602.1 / 605.1a off the PROJECTION like `keywords`: abilities are
      -- characteristics (CR 109.3) written by layer 6. The whole list the object
      -- HAS, not the list it can activate here. LAZY -- see the field's own comment
      -- in Pawl.Engine.Filter.
      Filter.nonManaActivatedAbility = not (all ManaAbility.isManaAbility (abilitiesFromCharacteristics peers pc oid gs)),
      -- CR 602.1 off the same list, without CR 605.1a's exclusion. LAZY for its
      -- neighbour's reason.
      --
      -- CR 305.6's intrinsic ability is a disjunct here and not a member of that
      -- list, because abilitiesFromCharacteristics mints rule 702's abilities and
      -- not rule 305's: what it folds is PC.activatedAbilities plus the keyword
      -- map, and no layer writes "{T}: Add {R}" onto a Mountain. Read off the
      -- PROJECTION, so a land that lost its basic land type in layer 4 has lost
      -- the ability with it (CR 305.7) and one that lost all its abilities in
      -- layer 6 has too (CR 613.1f).
      --
      -- The neighbour above needs no such disjunct and is right without one: CR
      -- 605.1a makes the intrinsic ability a mana ability, which that field
      -- excludes.
      Filter.hasActivatedAbility =
        not (null (abilitiesFromCharacteristics peers pc oid gs))
          || Subtype.intrinsicManaAbilityOf pc,
      -- CR 702.184c off the PROJECTION, applyModification's GrantsStationToughness
      -- arm being layer 6's only writer.
      Filter.grantsStationToughness = PC.grantsStationToughness pc
    }

-- CR 122.1: the counters on an object right now, and none for an id naming nothing.
countersOf :: ObjectId -> GameState -> Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural
countersOf oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)

-- CR 707.2 / 613.1a: an object's layer-1 (copy) result -- its stamped copy
-- snapshot when it has one, the printed base otherwise. Base-or-snapshot only, so
-- counters, pumps, control and ability grants are never part of a copiable value.
-- Not a recursion: a copy of a copy stored resolved values when it was stamped.
--
-- CR 730.2a's merge arrives on the SAME arm as a copy, and deliberately: the two
-- share layer 1a (CR 613.2a), Pawl.Engine.Event.merge stamps the record
-- withMergedAbilities below builds, and whichever of them stamped LAST is what
-- CR 613.7's timestamp order leaves. So no reader has to know that a merged
-- permanent is one, and an earlier copy effect on the target cannot outrank the
-- merge -- which is exactly what it did before #3371.
copiableCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
copiableCharacteristics oid gs = case copiableSnapshotOf oid gs of
  Just snapshot -> snapshot
  -- CR 709.5's last sentence puts a copy of a Room on this arm rather than the
  -- one above, copiableSnapshotOf below answering Nothing for it: the rule copies
  -- the shared type line's two static abilities and which half each
  -- characteristic is in, not the subtraction they produced on the copied
  -- permanent, and the designations those abilities read are CR 709.5c's, which
  -- belong to this object. So the copiable values are rebuilt from the copied
  -- card's halves against THIS object's doors, which is what baseCharacteristics
  -- does with Game.halvesOf underneath it.
  --
  -- Not implemented: a CR 707.9 exception applied to a copy of a Room, which
  -- Replacement.applyCopyExceptions stamps into the snapshot this arm goes
  -- around (#3249). No card in data/cards/ pairs an exception with a
  -- Room-eligible copy.
  -- Not implemented: CR 707.2's "status ... [is] not copied" for CR 110.5's
  -- flipped status, which baseCharacteristics reads through Game.faceOf and so
  -- folds a flipped permanent's alternative half into the value a copy freezes
  -- (#3364). Pawl.Engine.Event.copiedSnapshot is the reader that spends it.
  Nothing -> baseCharacteristics oid gs

-- CR 613.2a / 613.2b: an object's LAYER 1a value -- `copiableCharacteristics`
-- above with layer 1b's face-down substitution not yet applied. CR 708.8 names
-- the same value from the other side: it is what a permanent's copiable values
-- revert to when it turns face up.
--
-- ONE caller and one rule. CR 730.2a makes a merge a copiable effect that CR
-- 613.2a puts in layer 1a, so what Pawl.Engine.Event.merge folds is what layer
-- 1a had left each side -- never what layer 1b afterwards did to the permanent.
-- Read through `copiableCharacteristics` instead and a merge onto a face-down
-- permanent freezes CR 708.2a's nameless 2/2 into the stamp, so the merged
-- permanent goes on being a nameless 2/2 once it turns face up.
--
-- Written as a COUNTERFACTUAL BOARD rather than as a second seed, which is
-- Pawl.Engine.Game.faceUpFaceOf's own posture one module over: the question is
-- "what would this object's layer-1a value be if it were face up", and each of
-- the three forks that answers it -- `copiableSnapshotOf` below, and
-- Game.faceOf and Game.namesOf under `baseCharacteristics` -- reads
-- Object.facing off the board rather than taking it as an argument. One map
-- insert, on the merge road only.
--
-- No CR 613.7f timestamp rides the rewrite, and none should: nothing here turns
-- anything over, and the board is thrown away with the read.
copiableCharacteristicsFaceUp :: ObjectId -> GameState -> ProjectedCharacteristics
copiableCharacteristicsFaceUp oid gs =
  copiableCharacteristics
    oid
    gs {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.FaceUp}) oid (GameState.objects gs)}

-- CR 730.2h: `copiableCharacteristicsFaceUp` above asked of a side that is
-- FLIPPED -- "that component's alternative characteristics are used instead of
-- its normal characteristics if the merged permanent is flipped". Equal to the
-- face-up read for a side that is not a flip card, Game.resolveFaceFor's flipped
-- arm needing a Card.flippedFace to fire, so Pawl.Engine.Event.merge asks it of
-- BOTH sides and the rule's "if a merged permanent contains a flip card" falls
-- out rather than being tested.
--
-- The same COUNTERFACTUAL BOARD the neighbour above is written as, and stacked
-- on it: CR 110.5 makes flipped and face-down two of one rule's status
-- categories, and the question here is what layer 1a would say of this side with
-- both of them settled the way the merge needs. On a side that is already a
-- MERGED permanent the counterfactual reaches `stampedSnapshotOf` below instead,
-- which answers with the flipped reading that merge stamped -- so a merge onto a
-- merged permanent carries the older merge's flip component forward.
copiableCharacteristicsFlipped :: ObjectId -> GameState -> ProjectedCharacteristics
copiableCharacteristicsFlipped oid gs =
  copiableCharacteristicsFaceUp
    oid
    gs {GameState.objects = Map.adjust (\o -> o {Object.flipped = True}) oid (GameState.objects gs)}

-- CR 707.3: the copy snapshot an object's copiable RULES TEXT is read from --
-- Nothing for an object that is copying nothing, and Nothing again for one whose
-- snapshot copied HALVES, whose rules text every reader must re-derive against
-- this object's own unlocked designations instead (copiableCharacteristics
-- above).
--
-- THE accessor all seven of those readers share -- the record above,
-- staticAbilitiesOf and specialActionsOf below, Pawl.Engine.Projection's
-- copiableReplacementsOf, anyCopiableKeyword and copiableMintsType, and
-- Pawl.Engine.PlayerEffect's playerAbilitiesOf -- so that no two of them can
-- answer differently about one object. Four of the seven are disjuncts of ONE
-- predicate (Pawl.Engine.Projection.replacementsAffecting's baseHas), which is
-- what makes a per-reader fork a bug waiting rather than a style question: that
-- predicate's
-- False SKIPS the projection, so a reader left behind hides a copy's ability
-- rather than reporting it.
--
-- Two of those seven have a producer over a copied door -- copiableReplacementsOf
-- (Torture Pit, proved by Pawl.RoomSpec's "CR 707.2a a copy of a Room gathers the
-- replacement effect behind the door IT unlocked") and playerAbilitiesOf
-- (Steaming Sauna). anyCopiableKeyword, copiableMintsType and specialActionsOf
-- have none and are REGRESSION FENCES: no printed Room half prints a keyword,
-- none is a Saga, a planeswalker or a battle, and none grants a CR 116.2
-- permission (Scryfall `t:room`, 2026-09-05), so nothing can redden if they are
-- left behind again.
--
-- FACE DOWN answers Nothing before either of those questions is asked, so all
-- seven readers fall through to the printed read and Game.faceOf hands them
-- Card.faceDownFace. CR 613.2b orders layer 1b after layer 1a, so CR 708.2's
-- listed characteristics replace what a copy effect stamped rather than losing to
-- it -- Game.halvesOf's and Game.prepareSpellOf's fork, one field over, and here
-- rather than in any one reader because a per-reader fork is what this accessor
-- exists to prevent. The stamp itself rides through untouched, which is what CR
-- 708.8 reverts to.
copiableSnapshotOf :: ObjectId -> GameState -> Maybe ProjectedCharacteristics
copiableSnapshotOf oid gs
  | maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid gs) = Nothing
  | derivesFromCopiedHalves oid gs = Nothing
  | otherwise = stampedSnapshotOf oid gs

-- CR 709.5 / 709.5c: is this object's copiable rules text a set of HALVES it
-- copied, to be read against its own unlocked designations? The one fork
-- copiableSnapshotOf above makes, so no reader has to know it exists.
--
-- The BATTLEFIELD conjunct is Game.resolveFaceFor's own, which is what makes the
-- two agree: only there do CR 709.5c's designations exist for the subtraction to
-- read, so a card in a graveyard that became a copy of a Room keeps reading the
-- frozen snapshot, which is CR 709.4's combined view of the copied card.
derivesFromCopiedHalves :: ObjectId -> GameState -> Bool
derivesFromCopiedHalves oid gs = case stampedSnapshotOf oid gs of
  Nothing -> False
  Just snapshot ->
    Maybe.isJust (PC.halves snapshot)
      && fmap Object.zone (Game.lookupObject oid gs) == Just Zone.Battlefield

-- CR 707.3's raw stamp: what Pawl.Engine.Resolve and Pawl.Engine.Event wrote to
-- Binding.copyOf, before rule 709.5's question is asked of it. The projection's
-- ONE read of that binding, and only copiableSnapshotOf and derivesFromCopiedHalves
-- above call it; Pawl.Engine.Game.halvesCardOf makes the other read in the
-- engine, off the object it already holds.
--
-- CR 730.2h is the one fork: a FLIPPED merged permanent reads the flipped
-- stamp its merge left beside the ordinary one, so each flip component
-- contributes its alternative characteristics. CR 110.5a keeps status out of the
-- characteristics, which is why this is a choice between two stamped readings
-- rather than a CR 613 layer -- Pawl.Types.Object.flipped says the same of the
-- unmerged road, where Game.resolveFaceFor makes the substitution at the face
-- seam a merged permanent's stamp goes around.
stampedSnapshotOf :: ObjectId -> GameState -> Maybe ProjectedCharacteristics
stampedSnapshotOf oid gs = do
  object <- Game.lookupObject oid gs
  let bindings = Object.bindings object
  if Object.flipped object
    then case Binding.flippedCopyOf bindings of
      Just flipped -> Just flipped
      Nothing -> Binding.copyOf bindings
    else Binding.copyOf bindings

-- CR 707.2a: the static abilities this object's copiable rules text gives it --
-- its copy snapshot's when it has one, its printed face's otherwise. Equal to
-- PC.staticAbilities (copiableCharacteristics oid gs) by construction, since
-- that is what both arms of baseCharacteristics seed the field from.
--
-- Written as its own read rather than through copiableCharacteristics for two
-- reasons, both structural. It stays PROJECTION-FREE, which controlGrants below
-- requires of everything it touches -- baseCharacteristics asks controllerOf,
-- and controllerOf is built on controlGrants. And it costs one map lookup on
-- the ordinary permanent, where the seed spends Game.namesOf and two
-- Quantity.evaluates, so the readers below stay as cheap as the printed read
-- they replace.
-- CR 702.161a's living metal is APPENDED rather than read at a use site, and
-- appended HERE rather than at the one caller that folds these into layers: the
-- list's index is CR 613.6's memo key and Pawl.Engine.Event's departure handover
-- indexes the same list, so the two walks must agree on what is at each position.
-- Printed abilities keep their indices; a minted one takes the position after.
--
-- Off the COPIABLE keywords, like the rest of this function. So a living metal
-- another object's ability grants is not expanded (#2523): what a keyword MEANS
-- would otherwise have to be known before layer 6 has decided who holds it.
-- Devoid and changeling take the other road out of that -- grantedDefiningParts
-- emits their defining half as a second PART of whatever grants the keyword, so
-- neither needs an ability minted here.
staticAbilitiesOf :: ObjectId -> GameState -> [StaticAbility.StaticAbility Card.Type.Card]
staticAbilitiesOf oid gs = case copiableSnapshotOf oid gs of
  Just snapshot -> PC.staticAbilities snapshot <> Keyword.mintedStaticAbilitiesOf (Map.keysSet (PC.keywords snapshot))
  -- The printed read reaches a copied Room too, and has to: copiableSnapshotOf
  -- above answers Nothing for one, and Game.faceOf answers with the copied card's
  -- halves subtracted by THIS object's designations (CR 709.5).
  Nothing -> foldMap (\face -> Face.staticAbilities face <> Keyword.mintedStaticAbilitiesOf (Face.keywords face)) (Game.faceOf oid gs)

-- CR 116.2: the special actions this object's copiable rules text grants -- its
-- copy snapshot's when it has one, its printed face's otherwise.
-- staticAbilitiesOf above in every respect, and split from it for the same two
-- structural reasons; the one caller is Pawl.Engine.Ignore.ignoreGrants.
--
-- CR 707.2a is what puts the snapshot first: the permission is derived from the
-- copied object's rules text, so a permanent that became a copy of Leonin
-- Arbiter offers the {2} its own printed face never mentions, beside the
-- prohibition Pawl.Engine.PlayerEffect.playerAbilitiesOf already reads off that
-- same snapshot. Pawl.SpecialActionSpec's "CR 707.2a a copy of the Arbiter
-- carries the offer with the ban" proves it.
--
-- CR 116.2e's discard is not asked of this: that permission is read of a card in
-- HAND (Pawl.Engine.Action.discardableCards, off Pawl.Engine.Card.combined), a
-- zone in which nothing in data/cards/ can make an object a copy.
specialActionsOf :: ObjectId -> GameState -> [SpecialAction.SpecialAction]
specialActionsOf oid gs = case copiableSnapshotOf oid gs of
  Just snapshot -> PC.specialActions snapshot
  -- The printed read reaches a copied Room too, for staticAbilitiesOf's reason.
  Nothing -> foldMap Face.specialActions (Game.faceOf oid gs)

-- CR 613.11: the twelve rule-affecting ability families this object's copiable
-- rules text gives it -- its copy snapshot's when it has one, its printed face's
-- otherwise. specialActionsOf above in every respect, and split out for the same
-- two structural reasons.
--
-- THE reader for all twelve gatherer modules (Pawl.Engine.CombatRestriction and
-- its eleven siblings), which is what makes the two rules it settles settle once:
-- CR 707.2a, so a permanent that became a copy of Silent Arbiter limits attackers
-- and its own printed face is never consulted, and CR 702.140e with CR 730.2a, so
-- a merged permanent answers with every component's -- Pawl.Engine.Event.merge
-- having folded them into the snapshot at the merge's timestamp, which is why the
-- components' printed faces are not walked here. A component that was itself a
-- copy therefore contributes what it was a copy OF, which is what rule 730.2a's
-- "copiable" asks for and what a per-component face walk would have got wrong.
--
-- Pawl.MutateSpec's "CR 702.140e a Silent Arbiter under a Cubwarden still holds
-- alice to one attacker" is what proves the merged read.
ruleAbilitiesOf :: ObjectId -> GameState -> RuleAbilities.RuleAbilities
ruleAbilitiesOf oid gs = case copiableSnapshotOf oid gs of
  Just snapshot -> PC.ruleAbilities snapshot
  -- The printed read reaches a copied Room too, for staticAbilitiesOf's reason.
  Nothing -> foldMap ruleAbilitiesOfFace (Game.faceOf oid gs)

-- The thirteen lists a printed face declares. Its own function so that the seed
-- above and ruleAbilitiesOf's fallback cannot drift on what a face contributes.
ruleAbilitiesOfFace :: Face.Face Card.Type.Card -> RuleAbilities.RuleAbilities
ruleAbilitiesOfFace face =
  RuleAbilities.MkRuleAbilities
    { RuleAbilities.activationProhibitions = Face.activationProhibitions face,
      RuleAbilities.attachRestrictions = Face.attachRestrictions face,
      RuleAbilities.attackCosts = Face.attackCosts face,
      RuleAbilities.attackRequirements = Face.attackRequirements face,
      RuleAbilities.blockCosts = Face.blockCosts face,
      RuleAbilities.blockPermissions = Face.blockPermissions face,
      RuleAbilities.blockRequirements = Face.blockRequirements face,
      RuleAbilities.combatRestrictions = Face.combatRestrictions face,
      RuleAbilities.counterRestrictions = Face.counterRestrictions face,
      RuleAbilities.crewRestrictions = Face.crewRestrictions face,
      RuleAbilities.entryRestrictions = Face.entryRestrictions face,
      RuleAbilities.sacrificeRestrictions = Face.sacrificeRestrictions face,
      RuleAbilities.untapRestrictions = Face.untapRestrictions face
    }

-- CR 208.2 / 604.3: the card's characteristic-defining P/T, with the printed star
-- resolved to what the CDA counts. Nothing unless the card declares a CDA *and*
-- has a printed power and toughness box (CR 208.1) for the star to sit in.
--
-- PER BOX: a printed face declares one ability and Pawl.Codec.Face writes it into
-- both slots, but CR 709.4c's combined view of a split card can hold one half's
-- ability in each (Pawl.Engine.Card.definedBox).
seedCharacteristicPT :: Face.Face Card.Type.Card -> Maybe CharacteristicPT.CharacteristicPT
seedCharacteristicPT face =
  case (Face.characteristicPT face, Face.power face, Face.toughness face) of
    (Just star, Just (Power.MkPower p), Just (Toughness.MkToughness t)) ->
      Just
        CharacteristicPT.MkCharacteristicPT
          { CharacteristicPT.power = Star.substituteStar (CharacteristicPT.power star) p,
            CharacteristicPT.toughness = Star.substituteStar (CharacteristicPT.toughness star) t
          }
    _ -> Nothing

-- Printed characteristics before any effect: CR 613.1's starting point.
baseCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
baseCharacteristics oid gs = case Game.faceOf oid gs of
  Nothing ->
    PC.MkProjectedCharacteristics
      { -- No card behind this object (an ability on the stack).
        PC.names = Set.empty,
        PC.supertypes = Set.empty,
        PC.keywords = Map.empty,
        PC.colors = Set.empty,
        -- CR 202.1 reads a mana cost off a card, and there is none to read.
        PC.manaCost = Nothing,
        -- CR 202.3a all the same: CR 109.1 makes an ability on the stack an
        -- OBJECT, and an object with no mana cost has mana value 0 -- neither of
        -- that rule's two exceptions (a nonmodal double-faced back face, a melded
        -- permanent) is an object with no card behind it. CR 603.3's "no other
        -- characteristics" does not take it back: CR 109.3 lists mana COST as a
        -- characteristic and not mana value, which rule 202.3 derives from it.
        -- Pawl.CounterspellSpec's Synthetic Weigh the Trigger group is what
        -- proves the number.
        PC.manaValue = Just 0,
        PC.power = Nothing,
        PC.toughness = Nothing,
        PC.loyalty = Nothing,
        PC.defense = Nothing,
        PC.characteristicPT = Nothing,
        PC.cardTypes = Set.empty,
        PC.subtypes = Set.empty,
        PC.staticAbilities = [],
        PC.playerAbilities = [],
        -- CR 116.2: no card behind the object, so no printed permission either.
        PC.specialActions = [],
        PC.activatedAbilities = [],
        PC.replacementEffects = [],
        PC.triggeredAbilities = [],
        PC.enchant = [],
        -- CR 613.11: no card behind the object, so none of the twelve families
        -- either.
        PC.ruleAbilities = mempty,
        PC.lostAllAbilities = False,
        PC.subtypeWordChanges = [],
        PC.textChangedKeywords = Map.empty,
        PC.assignsCombatDamageWithToughness = False,
        -- CR 702.184c: an ability on the stack grants nothing to its
        -- controller's station abilities of its own.
        PC.grantsStationToughness = False,
        -- CR 709.5: no card behind the object, so no halves either.
        PC.halves = Nothing,
        -- CR 722.2b, for the same reason one line up.
        PC.prepare = Nothing
      }
  Just face ->
    -- CR 718.3b's swap sits OUTSIDE the record rather than in four of its fields,
    -- so that CR 718.5's "remain the same" is visible as the default: everything
    -- the seed builds is the printed value, and withPrototype below rewrites exactly
    -- the four characteristics rule 718.3b names.
    withPrototype oid gs face $
      -- The seed predates every layer, so it can describe no object: every view is
      -- Nothing. That silences the printed box's board-reading shapes only where
      -- they go through this view at all -- Pawl.Engine.Count.evaluate reads a
      -- Scope.InHistory snapshot and a Scope.OverPlayers player directly, so a Count
      -- over either would read LIVE state here. What keeps both out is CR 208.1 /
      -- 208.2: a printed box is a number or a star, and Pawl.CardSpec's "every
      -- printed power and toughness box is a number or a star" lint holds every
      -- card's own faces to it. A MINTED face may print a computed box (CR 111.3),
      -- which Resolve.bakeTokenCharacteristics stamps into a Literal as the token is
      -- created -- undeterminable ones included, so the only quantity that reaches
      -- this seed from a token is CR 208.2's star. Pawl.CountSpec's Miming Slime
      -- group is what proves that.
      let seedViewOf = const Nothing
          seedContext = Filter.contextFor (Game.teams gs) (controllerOf oid gs) (Just oid)
       in PC.MkProjectedCharacteristics
            { -- CR 709.4a: the names the object shows, which `face` cannot carry --
              -- Game.namesOf decides which halves show.
              PC.names = Game.namesOf oid gs,
              PC.supertypes = TypeLine.supertypes (Face.typeLine face),
              -- CR 702: a printed keyword appears once; layer 6 adds multiplicity.
              PC.keywords = Map.fromSet (const 1) (Face.keywords face),
              PC.colors = printedColorsOf face,
              -- CR 202.1: the printed cost of the face the object is showing, so CR
              -- 708.2a's face-down substitution leaves a face-down object with none.
              -- `face` rather than Game.manaCostFacesOf below: CR 712.8e lends a
              -- transformed permanent its front face's mana VALUE and not its cost,
              -- and CR 202.3c's melded sum is a number no single cost states.
              PC.manaCost = Face.manaCost face,
              -- CR 202.3, derived here so the rest of the fold reads a number.
              -- Game.manaCostFacesOf rather than `face`: CR 712.8e reads a transformed
              -- permanent's mana value off its FRONT face's cost, and CR 708.2a's
              -- face-down face has no mana cost (so CR 202.3a's 0). SUMMED because CR
              -- 202.3c gives a melded permanent "the combined mana cost of the front
              -- faces of each card that represents it"; every other object answers
              -- with one face, whose sum is itself. An object with no card behind it
              -- never arrives here -- Game.faceOf answers Nothing for it and the arm
              -- above carries CR 202.3a's 0 -- so an empty Seq here is a printing the
              -- game does not know (Game.frontFaceOfPrinting), which is Nothing.
              PC.manaValue = case Game.manaCostFacesOf oid gs of
                faces | Seq.null faces -> Nothing
                faces -> Just (sum (fmap Quantity.manaValueOf faces)),
              -- Quantity.evaluate, not Quantity.determine: CR 208.2a's "use 0
              -- instead" belongs to a CDA, so a printed star with none behind it is
              -- Nothing. A star given its value by CR 208.2b reports Nothing off the
              -- battlefield; one with a CDA is filled at layer 7a.
              PC.power = case Face.power face of
                Nothing -> Nothing
                Just (Power.MkPower q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
              PC.toughness = case Face.toughness face of
                Nothing -> Nothing
                Just (Toughness.MkToughness q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
              -- CR 306.5a: a literal number, copied through rather than evaluated.
              PC.loyalty = Face.loyalty face,
              -- CR 310.4a: a literal number, likewise.
              PC.defense = Face.defense face,
              PC.characteristicPT = seedCharacteristicPT face,
              PC.cardTypes = TypeLine.types (Face.typeLine face),
              PC.subtypes = TypeLine.subtypes (Face.typeLine face),
              -- CR 604.1 / 613.10: the two ability lists the layer fold never
              -- rewrites. In the SEED for enchant's reason below -- CR 707.2
              -- names rules text among the copiable values -- which is what puts
              -- a copied permanent's static and player abilities where
              -- staticAbilitiesOf and Pawl.Engine.PlayerEffect can find them
              -- instead of on the copier's printed face (CR 707.2a).
              PC.staticAbilities = Face.staticAbilities face,
              PC.playerAbilities = Face.playerAbilities face,
              -- CR 116.2, in the seed for the same reason and read by
              -- specialActionsOf below: CR 707.2a copies the abilities a face's
              -- rules text derives, and CR 116.2d's permission is one of them.
              PC.specialActions = Face.specialActions face,
              PC.activatedAbilities = Face.activatedAbilities face,
              PC.replacementEffects = Face.replacementEffects face,
              PC.triggeredAbilities = Face.triggeredAbilities face,
              -- CR 702.5a's printed instances. In the SEED rather than folded in
              -- later, so they ride copiableCharacteristics: CR 707.2 names rules
              -- text among the copiable values, and a granted instance is not
              -- copiable precisely because applyModification writes it after the
              -- seed. Read off `face`, so CR 708.2a's face-down substitution leaves
              -- a face-down permanent with none.
              PC.enchant = Face.enchant face,
              -- CR 613.11: the twelve families CR 613.11 applies after the layer
              -- system, off `face` for enchant's reason -- and copiable for
              -- enchant's reason too, which is what carries them to a copy and,
              -- through withMergedAbilities below, to a merged permanent's other
              -- components (CR 702.140e).
              PC.ruleAbilities = ruleAbilitiesOfFace face,
              -- CR 613.1's starting point, before layer 6 has run:
              -- applyModification's LoseAllAbilities arm is the only writer.
              PC.lostAllAbilities = False,
              -- The seed is CR 613.1's starting point, before layer 3 has run.
              PC.subtypeWordChanges = [],
              PC.textChangedKeywords = Map.empty,
              -- CR 613.1's starting point, before rules-changing effects.
              PC.assignsCombatDamageWithToughness = False,
              -- CR 613.1's starting point, before layer 6 has run:
              -- applyModification's GrantsStationToughness arm is the only writer.
              PC.grantsStationToughness = False,
              -- CR 709.5 / 709.5b: the halves this object has, which -- like the
              -- names above -- `face` cannot carry, a Face being one half's worth
              -- of characteristics. Game.halvesOf decides, and it reads the copy
              -- snapshot first, so a copy of a copy of a Room goes on carrying the
              -- doors.
              PC.halves = Game.halvesOf oid gs,
              -- CR 722.2b: the prepare spell this object has, which `face` cannot
              -- carry either -- CR 722.4 leaves the normal half alone in every
              -- zone, so the inset frame is nowhere in it. Game.prepareSpellOf
              -- decides, and it reads the copy snapshot first for halves' reason.
              PC.prepare = Game.prepareSpellOf oid gs
            }

-- CR 718.3b: "both a prototyped spell and the permanent it becomes have only its
-- alternative set of power, toughness, and mana cost characteristics. If that
-- mana cost includes one or more colored mana symbols, the spell and the
-- permanent it becomes are also that color or colors." The four characteristics
-- rule 718.3b names, swapped into the seed and nothing else swapped, which is CR
-- 718.5.
--
-- IN THE SEED, which is what makes CR 718.2a fall out with no work of its own:
-- "the existence and values of these alternative characteristics are part of the
-- object's copiable values", and a copy freezes exactly this record
-- (Pawl.Engine.Event.copiedSnapshot over copiableCharacteristics). CR 718.3c's
-- copy reads the frozen values for that reason and needs nothing of its own,
-- which Pawl.PrototypeSpec's "CR 718.2a / 718.3c a copy of the prototyped spell
-- is a white 2/2 too" proves: the token Lithoform Engine leaves has
-- Object.prototyped False by construction, so its colours and box can only have
-- come from the snapshot. CR 718.3d's copy of a prototyped PERMANENT rides the
-- same read and has no producer in data/cards/, so it is a regression fence.
--
-- CR 718.4 is not re-asked here. Object.prototyped is stamped only as the card
-- is cast (Pawl.Engine.Cast.stampPrototyped) and survives only the
-- stack-to-battlefield move (Pawl.Engine.Event.changeZoneAttaching), so a
-- prototype card in every other zone answers False and keeps its printed
-- characteristics, which is rule 718.4's sentence exactly.
--
-- The FIRST inset frame, and CR 718.1 is why one is enough: it gives a prototype
-- card "a second set of power, toughness, and mana cost characteristics",
-- singular, so no printing offers two for Object.prototyped's Bool to have to
-- choose between.
withPrototype :: ObjectId -> GameState -> Face.Face Card.Type.Card -> ProjectedCharacteristics -> ProjectedCharacteristics
withPrototype oid gs face pc = case (maybe False Object.prototyped (Game.lookupObject oid gs), Keyword.prototypes (Face.keywords face)) of
  (True, frame : _) ->
    pc
      { PC.manaCost = Just (Prototype.cost frame),
        -- CR 202.3 off the inset cost, exactly as the printed field is derived
        -- from the printed one.
        PC.manaValue = Just (Quantity.manaCostValue (Prototype.cost frame)),
        PC.power = Just (Prototype.power frame),
        PC.toughness = Just (Prototype.toughness frame),
        -- CR 202.2 with rule 718.3b's second sentence. printedColorsOf's own
        -- shape, with the inset cost in place of the printed one: a colour
        -- indicator is a separate characteristic (CR 204.2) that rule 718.3b does
        -- not name, so it survives.
        PC.colors = Set.union (Face.colorIndicator face) (manaCostColors (Just (Prototype.cost frame)))
      }
  _ -> pc

-- CR 702.140e / 730.2a: the copiable values a MERGE leaves -- rule 730.2a's
-- "only the characteristics of its topmost component" as the base, plus rule
-- 702.140e's "all abilities of each card and token that represents it" folded in
-- from the other side. `base` is the topmost side's record and `donor` the
-- other's; which is which is CR 702.140c's over-or-under choice, made by
-- Pawl.Engine.Event.merge, the one caller.
--
-- OVER RECORDS rather than over printed faces, which is the whole of the fix for
-- the defect this replaced, see #3371: each side contributes what LAYER 1a had
-- already left it, so a component that was itself a copy contributes the copy's
-- rules text (CR 707.2a) and a component that was itself merged contributes its
-- own merged values. Read off the printed faces instead, a merge under a Clone
-- would take Clone's own 0/0 box and its copy ability.
--
-- CR 613.2a is why the result is STAMPED at the merge rather than derived on
-- every projection: layer 1a holds copy effects and merges together, CR 613.7
-- orders them by timestamp, and CR 730.2a fixes this one's timestamp at "the
-- time the objects merged". Freezing what layer 1a said at that moment is CR
-- 707.2c's own posture for the copy effects it shares the sublayer with
-- ("determined only at the time that effect first starts to apply"), and it is
-- what makes an EARLIER copy effect on the target lose to the merge instead of
-- silently replacing it. A LATER copy effect still wins, by re-stamping.
--
-- The permanent's TIMESTAMP is untouched: CR 730.2b keeps it the same object and
-- CR 730.2c keeps every continuous effect that applied to it applying, so a
-- merge that minted a new one would reorder every layer already on it.
--
-- The ability fields and no other. PC.characteristicPT is deliberately NOT among
-- them although CR 604.3 makes a CDA an ability: rule 730.2a keeps the POWER AND
-- TOUGHNESS characteristic the topmost component's, so a donor CDA that set the
-- box would override the very sentence this function's base implements. No card
-- in data/cards/ prints a CDA box on a creature a mutate spell can target, so
-- nothing tells the two readings apart today.
--
-- Each side arrives at LAYER 1a, through `copiableCharacteristicsFaceUp` above,
-- so a FACE-DOWN component contributes its card's abilities rather than CR
-- 708.2a's none: CR 730.2e gives a component a status, CR 109.3 keeps status out
-- of the characteristics, and CR 613.2b applies rule 708.2's substitution to the
-- resulting permanent one sublayer later.
--
-- CR 730.2h's flip component arrives here already read the way that rule asks
-- for, Pawl.Engine.Event.merge folding a SECOND result out of both sides'
-- flipped records; this function is the same fold either way.
withMergedAbilities :: ProjectedCharacteristics -> ProjectedCharacteristics -> ProjectedCharacteristics
withMergedAbilities donor base =
  base
    { -- CR 702.1: a keyword IS an ability, and the map counts instances, so a
      -- component carrying a keyword the topmost one also has contributes a
      -- second instance (CR 702.1b's redundancy is then the layer fold's).
      PC.keywords = Map.unionWith (+) (PC.keywords base) (PC.keywords donor),
      PC.staticAbilities = PC.staticAbilities base <> PC.staticAbilities donor,
      PC.playerAbilities = PC.playerAbilities base <> PC.playerAbilities donor,
      PC.specialActions = PC.specialActions base <> PC.specialActions donor,
      PC.activatedAbilities = PC.activatedAbilities base <> PC.activatedAbilities donor,
      PC.replacementEffects = PC.replacementEffects base <> PC.replacementEffects donor,
      PC.triggeredAbilities = PC.triggeredAbilities base <> PC.triggeredAbilities donor,
      PC.enchant = PC.enchant base <> PC.enchant donor,
      -- CR 613.11's twelve families, whose Semigroup is the same concatenation
      -- the eight fields above are written out with; Pawl.Types.RuleAbilities
      -- carries it so a thirteenth family cannot be added and left out here.
      PC.ruleAbilities = PC.ruleAbilities base <> PC.ruleAbilities donor
    }

-- CR 202.2 / 204.2 / 202.2b: an object's printed colours, from its mana cost's
-- coloured symbols and its colour indicator. No devoid here: CR 702.114a makes it
-- a CDA, which CR 613.3 puts at the start of layer 5 (applyColorDefining).
printedColorsOf :: Face.Face Card.Type.Card -> Set Color.Color
printedColorsOf face =
  Set.union
    (Face.colorIndicator face)
    (manaCostColors (Face.manaCost face))

-- CR 702.114a. The one place that decides what devoid means.
definesColorless :: Set Keyword -> Bool
definesColorless = Set.member Keyword.Type.Devoid

-- CR 702.73a, definesColorless' twin: the one place changeling is decided.
definesEveryCreatureType :: Set Keyword -> Bool
definesEveryCreatureType = Set.member Keyword.Type.Changeling

-- CR 202.1b: a land has no mana cost at all, so it contributes no colours.
manaCostColors :: Maybe ManaCost.ManaCost -> Set Color.Color
manaCostColors mc = case mc of
  Nothing -> Set.empty
  Just (ManaCost.MkManaCost symbols) -> Set.fromList (concatMap symbolColors symbols)

-- CR 202.2b: only a coloured mana symbol carries a colour; colourless is not a
-- colour (CR 105.2c). A list, since a hybrid is all of its colours (CR 107.4e).
symbolColors :: ManaSymbol.ManaSymbol -> [Color.Color]
symbolColors symbol = case symbol of
  ManaSymbol.OfType (ManaType.Colored c) -> [c]
  ManaSymbol.OfType ManaType.Colorless -> []
  ManaSymbol.Hybrid (Hybrid.MkHybrid a b) -> Maybe.mapMaybe colorOfManaType [a, b]
  -- CR 107.4b/107.4e: a monocolored hybrid's other half is generic, so the named
  -- half is the whole contribution.
  ManaSymbol.MonocoloredHybrid t -> Maybe.maybeToList (colorOfManaType t)
  -- CR 107.4f / 202.2d: Phyrexian symbols are coloured mana symbols. Total `[c]`
  -- since Phyrexian carries a Color -- there is no colourless Phyrexian symbol.
  ManaSymbol.Phyrexian c -> [c]
  -- CR 107.4f: "a hybrid Phyrexian mana symbol is BOTH of its component
  -- colors", which CR 202.2d makes the object. Tamiyo, Compleated Sage is green
  -- and blue whichever of her {G/U/P}'s three ways paid for her.
  ManaSymbol.HybridPhyrexian (HybridPhyrexian.MkHybridPhyrexian l r) -> [l, r]
  -- CR 107.4h: snow is neither a colour nor a type of mana.
  ManaSymbol.Snow -> []
  ManaSymbol.Generic _ -> []
  ManaSymbol.Variable -> []

-- CR 105.2c: colourless is not a colour, so a colourless hybrid half adds none.
colorOfManaType :: ManaType.ManaType -> Maybe Color.Color
colorOfManaType manaType = case manaType of
  ManaType.Colored c -> Just c
  ManaType.Colorless -> Nothing

-- CR 113.6 / 614.12: the battlefield permanents whose static abilities FUNCTION
-- right now. Everything on the battlefield, minus the permanents entering beside
-- the one whose entry loop is running (GameState.enteringBeside), which this
-- engine has already materialized but the rules have not let in yet: CR 614.12
-- admits only the entering permanent's own static abilities and "continuous
-- effects that already exist", and a sibling's are neither.
--
-- Empty of exclusions at every priority window, so outside an entry loop this is
-- the battlefield. Three walks read it -- this module's static-ability gather,
-- its CR 305.7 set-subtype scan and its layer-2 control grants -- which together
-- are every place a permanent's own static ability becomes a continuous effect.
-- All three read that ability list through staticAbilitiesOf, so a copy's rules
-- text reaches each of them (CR 707.2a). Only the FIRST has an observer: Pawl.ReplacementSpec's "a Wood Elemental
-- reanimated beside Ashaya sacrifices nothing" goes red when it is widened back to
-- the whole battlefield, and neither of the other two moves a case, because
-- nothing in `data/cards/` puts a control-changer or a Blood Moon-shaped subtype
-- setter into a batch. Those two are regression fences, kept because a projection
-- that suppressed a sibling's ability in one walk and not the next would disagree
-- with itself.
--
-- anyConditional deliberately does NOT narrow by it: a superset costs a second
-- walk where a subset would leave a CR 604.2 clause wired open. Nor does
-- Pawl.Engine.Replacement.replacementsAffecting, whose siblings Event.loop
-- already drops by SOURCE (channel 2 of applyReplacementsIn's note).
abilitySources :: GameState -> [ObjectId]
abilitySources gs = Set.toList (Set.difference (GameState.battlefield gs) (GameState.enteringBeside gs))

-- CR 113.6b: does this static ability function from `zone`? THE zone
-- classification -- one question, asked by each of gatherGiven's static-ability
-- walks that HAS a CR 113.6 default to fall back on and by the two battlefield
-- walks beside them
-- (setLandSubtypeEffectsGiven and controlGrants), so a permanent's printed
-- ability cannot become a continuous effect through one of those three and not
-- the others.
--
-- An EMPTY set is an ability that states no zone, and then CR 113.6's own
-- defaults stand -- which is what makes the caller's zone argument the whole
-- answer for nearly every ability in the pool. A stated set is CR 113.6b's
-- "only", so it replaces those defaults rather than adding to them;
-- `fromGraveyardCard` above is the one caller that has to tell the two cases
-- apart, because the default it would otherwise override is CR 113.6f's
-- classification rather than a bare zone. The hand and library walks ask
-- `statesZone` below instead, having no default at all to fall back on.
--
-- Structural, and deliberately not a Condition: CR 604.2's clause is asked of an
-- ability some walk has already kept, so it could narrow a gather but never
-- widen one.
functionsFromZone :: Zone.Zone -> StaticAbility.StaticAbility card -> Bool
functionsFromZone zone sa =
  let zones = StaticAbility.functionsFrom sa
   in Set.null zones || Set.member zone zones

-- abilitiesGiven with the projection already in hand -- the half
-- viewOfCharacteristics calls.
--
-- CR 702.29b and CR 702.77b are why handAbilitiesOf is in this list, and CR
-- 702.84a why graveyardAbilitiesOf is beside it: a cycling, reinforce or unearth
-- ability exists in every zone, so the object HAS it here; it just cannot be
-- activated here (CR 113.6m).
--
-- CR 613.1: the gate's board comes in as a parameter. Taking fullView here would
-- not terminate for a caller inside the fold -- it re-enters `gather`, with no
-- memo and no descending bound.
abilitiesFromCharacteristics :: Count.ViewOf -> ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
abilitiesFromCharacteristics peers pc oid gs =
  let granted ability = case ActivatedAbility.condition ability of
        Nothing -> True
        Just cond -> Condition.holds peers (Filter.contextFor (Game.teams gs) (controllerOf oid gs) (Just oid)) gs oid cond
   in -- Rule 702's own activated abilities are appended here, minted from the
      -- POST-LAYER keyword map, so Humility takes crew away with the rest.
      --
      -- Unlike mintedTriggeredAbilitiesOf, no CR 612.2a rewrite is applied here,
      -- and that is right in both directions. A word from a keyword PAYLOAD --
      -- CR 702.6c's equip quality is the one the pool writes -- has already
      -- taken the swap once, because layer 3 rewrites the keyword MAP through
      -- Pawl.Engine.Filter.rewriteKeyword and this reads that map; rewriting
      -- again here would double-apply it under two chained changes.
      --
      -- Not implemented: a rewrite of the words rule 702's OWN TEXT writes into
      -- an activated ability it mints, which is the half a second pass would be
      -- for. No rule-702 keyword whose ability functions on the battlefield
      -- writes a creature-type word today (gap #2495).
      filter
        granted
        ( PC.activatedAbilities pc
            <> Keyword.battlefieldAbilitiesOf (PC.keywords pc)
            <> Keyword.handAbilitiesOf (Map.keysSet (PC.keywords pc))
            <> Keyword.graveyardAbilitiesOf (Map.keysSet (PC.keywords pc))
        )

-- CR 115.1: what a stack object TARGETS -- the recipients under its declared
-- target slots, which is CR 601.2c's (602.2b's, 603.3d's) announcement read live
-- off Object.bindings, so a CR 115.7 change of target is seen at once. What
-- Pawl.Engine.Filter.View's `targets` is filled from.
--
-- RESTRICTED to the declared slots, because Binding.targets is not only a
-- target: Pawl.Engine.Binding.toRecipients writes a recipient set for CR
-- 115.10a's non-targets too (a paid slot, a drawn pile). The declared slots are
-- the chosen modes' (CR 700.2) under the instance names Pawl.Engine.Modal gives
-- them -- the names Pawl.Engine.Cast, Activate and Engine bind under -- plus CR
-- 303.4a's enchant slot for a card-backed spell, by its reserved NAME rather than
-- off the projection: a bestowed grant (CR 702.103b) binds under the same name,
-- and reading `enchantOf` here would run a projection inside every view.
--
-- Empty for anything off the stack: CR 115.1 makes a target a property of a
-- spell or ability, and a permanent's bindings are its own resolution's. Empty
-- for the three sources that are never on the stack at all.
--
-- Both the restriction and the zone guard are FENCES rather than proven
-- behaviour: no board in the suite puts a non-target recipient binding on a
-- stack object and then asks a target atom of it, nor asks one of a permanent
-- carrying bindings, so dropping either leaves the suite green.
targetsOfStackObject :: GameState -> Object.Object -> Set Recipient.Recipient
targetsOfStackObject gs obj
  | Object.zone obj /= Zone.Stack = Set.empty
  | otherwise =
      let bindings = Object.bindings obj
          chosen = Binding.modesOf bindings
          ofModal = Map.keysSet . Modal.modesTargetSlots chosen
          -- CR 303.4a's enchant slot and CR 702.140a's mutate slot are declared
          -- unconditionally: both are named by a RULE rather than by the face,
          -- so a face read cannot see them, and the restriction below drops
          -- either where the spell never filled it.
          ofFace face = Set.insert Card.mutateSlot (Set.insert Card.enchantSlot (Map.keysSet (Card.modesTargetSlots chosen face)))
          declared = case Object.source obj of
            Source.OfCard _ -> maybe Set.empty ofFace (Game.faceOfObject gs obj)
            Source.OfSpellCopy _ -> maybe Set.empty ofFace (Game.faceOfObject gs obj)
            -- CR 722.3c's cast copy is card-backed the way the two arms above
            -- are: its interned printing has one face, whose target slots are the
            -- prepare spell's.
            Source.OfCardCopy _ -> maybe Set.empty ofFace (Game.faceOfObject gs obj)
            Source.OfAbility src -> ofModal (ActivatedAbility.modal (ActivatedAbilitySource.ability src))
            Source.OfTrigger src -> ofModal (TriggeredAbility.modal (TriggeredAbilitySource.ability src))
            Source.OfInherentTrigger src -> ofModal (TriggeredAbility.modal (InherentTriggerSource.ability src))
            Source.OfMeld _ -> Set.empty
            Source.OfMerge _ -> Set.empty
            Source.OfToken _ -> Set.empty
            Source.OfEmblem _ -> Set.empty
       in Set.unions (Map.elems (Map.restrictKeys (Binding.targetsOf bindings) declared))

-- CR 115.1 one indirection along: a VIEW of each thing the stack object above
-- targets, which is what Filter.TargetsOnlyOne's nest is matched against. What
-- Pawl.Engine.Filter.View's `targetViews` is filled from.
--
-- Through `peers` for an object target, exactly as `attachedToView` is: the
-- reader is bounded, and taking a full projection here would re-enter the fold
-- viewOfCharacteristics stands in. A player target answers Count.playerView,
-- which no projection reaches at all.
--
-- A recipient whose object is gone is DROPPED rather than mapped to a blank
-- view: CR 608.2b leaves nothing to describe, and Pawl.Engine.Filter answers the
-- atom False for a missing key.
targetViewsOfStackObject :: Count.ViewOf -> GameState -> Object.Object -> Map Recipient.Recipient Filter.View
targetViewsOfStackObject peers gs obj =
  Map.fromList
    (Maybe.mapMaybe (\r -> fmap ((,) r) (viewOfRecipient peers gs r)) (Set.toList (targetsOfStackObject gs obj)))

-- The view one recipient answers, splitting CR 115.1's two kinds of target: a
-- player is Count.playerView's candidate, and an object one the bounded reader
-- above answers for. Nothing for CR 406.4's pile, which is neither and which CR
-- 601.2c never leaves on a stack object anyway.
viewOfRecipient :: Count.ViewOf -> GameState -> Recipient.Recipient -> Maybe Filter.View
viewOfRecipient peers gs r = case Recipient.playerOf r of
  Just pid -> Just (Count.playerView gs pid)
  Nothing -> Recipient.objectOf r >>= peers

-- One control-granting static ability, flattened: the source and the timestamp
-- its effect takes (CR 613.7a).
data ControlGrant = MkControlGrant
  { cgSource :: ObjectId,
    cgAffected :: Affected.Affected,
    cgTimestamp :: Timestamp
  }
  deriving (Eq, Ord, Show)

-- Every layer-2 control-granting STATIC ability on the battlefield, gathered
-- once. NOT `gather`, and PROJECTION-FREE throughout: affects reads controllerOf
-- to supply CR 109.5's "you", so a controlGrants that consulted the layers would
-- be mutually recursive with it. That is why controlNames below reads copiable
-- values rather than the projection, and why no CR 305.7 gate is applied here.
-- Hoisted for the same reason setLandSubtypeEffects is: `controls` calls
-- controllerOf once per battlefield object.
--
-- The ability list is staticAbilitiesOf, so a copy's control-granting text is
-- read (CR 707.2a) -- which staticAbilitiesOf can supply without breaking the
-- rule above, being projection-free itself. No case observes that: the pool's
-- attachment grants (Confiscate, Control Magic, Synthetic Puppeteer's Yoke) are
-- Auras, and a copy of an Aura would enter attached to nothing and be put into a
-- graveyard by CR 704.5m, while no test stamps a copy binding on Synthetic
-- Goblin Dominion, the one that is not an Aura. A regression fence, kept because
-- the three walks over abilitySources must agree on which list they read.
--
-- Not implemented: CR 604.2's "as long as" gate, which setLandSubtypeEffects
-- does ask -- the same mutual recursion rules it out here (#1529).
controlGrants :: GameState -> [ControlGrant]
controlGrants gs =
  let grantsOf permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj ->
          let isControl sa = any isControlOp (StaticAbility.modifications sa)
              isControlOp m = case m of
                Modification.SetControllerToSource -> True
                _ -> False
              toGrant sa =
                MkControlGrant
                  { cgSource = permId,
                    cgAffected = StaticAbility.affected sa,
                    cgTimestamp = Object.timestamp permObj
                  }
           in fmap toGrant (filter (\sa -> isControl sa && functionsFromZone Zone.Battlefield sa) (staticAbilitiesOf permId gs))
   in concatMap grantsOf (abilitySources gs)

-- CR 303.4b: WHICH object this one is attached to -- what an Aura "enchants".
-- Nothing where it is attached to nothing, and where it is attached to a PLAYER
-- (CR 303.4's other destination), which is why Affected.Attached and
-- Affected.AttachedPlayerControls are two arms. No projection at all, so a
-- caller inside the layer fold may ask it -- which is what lets
-- Filter.IsHostOfSource be answered anywhere a source and a GameState are in hand.
hostOf :: ObjectId -> GameState -> Maybe ObjectId
hostOf oid gs = Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf

-- CR 303.4b's other destination: WHICH PLAYER this object is attached to --
-- what an enchant-player Aura "enchants". hostOf's twin, Nothing where the
-- object is attached to nothing or to an object. Like hostOf, no projection,
-- read live off Object.attachedTo, so controlNames below may ask it.
enchantedPlayerOf :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
enchantedPlayerOf oid gs = Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.playerOf

-- CR 108.4 / 613.1b: an object's controller is its owner, overridden by layer-2
-- control effects, last timestamp wins (CR 613.7). Stored continuous effects and
-- control-granting static abilities both carry a Timestamp and merge into one
-- maximum. A lean fold rather than the full projection: control precedes P/T.
controllerOf :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOf oid gs = controllerOfGiven (controlGrants gs) Set.empty oid gs

-- controllerOf with the grant list PRECOMPUTED and a visited set. The visited set
-- is a CR 613.8b loop-escape analog, not an implementation of it (#946):
-- deriving a grant's player asks for its SOURCE's controller, which can re-enter
-- this function, and re-entering an object already under question returns its
-- owner so a cycle grants nothing. It is not the only escape: controlNames'
-- AttachedPlayerControls arm shortens the GRANT list instead, so that a
-- candidate's earlier control effects stay visible.
controllerOfGiven :: [ControlGrant] -> Set ObjectId -> ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOfGiven grants visited oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj ->
    if Set.member oid visited
      then Just (defaultControllerOf obj)
      else
        let visited2 = Set.insert oid visited
            -- Does an affected set carried by `source` name `oid`? controlNames
            -- below is the enumeration this membership test reads off.
            namesFrom source a = Set.member oid (controlNames grants visited2 gs source a)
            storedSetter eff = case ContinuousEffect.modification eff of
              Modification.SetController pid
                | namesFrom (ContinuousEffect.source eff) (ContinuousEffect.affected eff) ->
                    Just (ContinuousEffect.timestamp eff, pid)
              _ -> Nothing
            stored = Maybe.mapMaybe storedSetter (GameState.continuousEffects gs)
            fromGrant g =
              if not (namesFrom (cgSource g) (cgAffected g))
                then Nothing
                else case controllerOfGiven grants visited2 (cgSource g) gs of
                  Nothing -> Nothing
                  Just who -> Just (cgTimestamp g, who)
            derived = Maybe.mapMaybe fromGrant grants
         in case stored <> derived of
              [] -> Just (defaultControllerOf obj)
              setters -> Just (snd (List.maximumBy (Ord.comparing fst) setters))

-- Which objects an affected set NAMES, for the CR 613.1b layer-2 control fold.
-- Parameterized by the source because Affected.Attached asks about the SOURCE's
-- state, and by the grant list and the caller's visited set because a PREDICATE
-- set has to answer control questions of its own -- CR 109.5's "you" for the
-- filter's perspective, and the candidate's own controller for a filter that
-- asks. Both go back through controllerOfGiven, never through the projection
-- (see controlGrants).
--
-- MatchingAnywhere and MatchingOffBattlefield stay empty and so grant nothing,
-- which is what the rule asks rather than an elision: CR 109.4 gives a
-- controller only to an object on the battlefield or the stack, so the part of
-- either set a control grant could observe is the part those two arms share
-- with Matching's own.
controlNames :: [ControlGrant] -> Set ObjectId -> GameState -> ObjectId -> Affected.Affected -> Set ObjectId
controlNames grants visited gs source a = case a of
  Affected.TheseObjects s -> s
  -- CR 303.4m: the source's own attachment, with no projection needed.
  Affected.Attached -> maybe Set.empty Set.singleton (hostOf source gs)
  -- CR 611.3a: a static ability's effect is not locked in, so the set is
  -- re-derived from the battlefield at every projection and a permanent that
  -- enters later is in it. CR 613.1a/613.2c: layer 2 reads an object's COPIABLE
  -- values, since layer 1 is the only layer before it and CR 613.8a confines
  -- dependency to one layer -- so no layer-4 type change feeds this test, which
  -- is what lets it run without projecting.
  Affected.Matching f -> Set.filter (matchesLeanly grants visited gs source f) (GameState.battlefield gs)
  Affected.MatchingAnywhere _ -> Set.empty
  Affected.MatchingOffBattlefield _ -> Set.empty
  -- CR 303.4b / 303.4m through a PLAYER: what the enchanted player controls,
  -- read as Matching's battlefield walk narrowed by the candidate's own
  -- controller. Both halves are dynamic (CR 611.3a), and the controller half is
  -- the lean fold's, never the projection's.
  --
  -- That half is asked under the grant list MINUS this source's own grants, and
  -- under a FRESH visited set rather than the caller's. CR 613.8a: applying
  -- another layer-2 control effect changes what this one applies to, so this one
  -- depends on it and CR 613.8b makes it wait until just after, whatever the two
  -- timestamps are (CR 613.7). Carrying the caller's visited set instead would
  -- answer the candidate under question its DEFAULT controller and hand over a
  -- permanent Control Magic had already taken; Pawl.AuraSpec's "CR 613.8a/613.8b
  -- a permanent already stolen from the enchanted player is not handed over
  -- again" proves it does not. The reverse dependency is what is absent: an
  -- attachment grant (CR 303.4m) changes who controls a candidate here, while
  -- nothing this effect does changes whose attachment that grant reads.
  --
  -- Dropping the source's own grants is also the loop escape, at the granularity
  -- the loop actually has: the nested read runs on a STRICTLY shorter grant
  -- list, so the recursion is well founded even with the visited set reset. Two
  -- Yokes, each enchanting the other's controller, terminate by that peeling
  -- rather than by CR 613.8b's dependency-loop clause, which would apply them in
  -- timestamp order (#946). The fold's STORED half cannot reach this arm at all:
  -- the one producer of a Modification.SetController writes an
  -- Affected.TheseObjects set (Pawl.Engine.Resolve.Effect) and no card may
  -- author one (#199), so no stored effect keeps the list from shrinking.
  --
  -- The battlefield bound is the arm's own, and it is what answers CR 702.26b
  -- for a phased-out permanent, which Pawl.Engine.Phasing.phaseOut removes from
  -- that set. Pawl.PhasingSpec's "CR 702.26b a phased-out permanent leaves a
  -- control grant reached through its controller" proves it.
  Affected.AttachedPlayerControls f -> case enchantedPlayerOf source gs of
    Nothing -> Set.empty
    Just pid ->
      let others = filter (\g -> cgSource g /= source) grants
       in Set.filter
            (\oid -> controllerOfGiven others Set.empty oid gs == Just pid && matchesLeanly grants visited gs source f oid)
            (GameState.battlefield gs)

-- Does `oid` match a layer-2 affected set's Filter, read at the copiable values
-- controlNames explains and with CR 109.5's "you" bound to the SOURCE's
-- controller? Projection-free throughout: every controller it needs comes from
-- controllerOfGiven carrying the caller's visited set, which answers an object's
-- owner once it revisits that object, so a control-dependent conjunct terminates
-- rather than re-entering the fold that is asking (#946). Termination is
-- structural here, not a matter of Filter.View's laziness -- which is what
-- separates this path from the liveness gate #197 describes.
--
-- Both of those controller reads are REGRESSION FENCES rather than proved
-- behaviour: a filter only forces either one by asking about control, and
-- neither of `data/cards/`'s two predicate control grants asks -- Synthetic
-- Goblin Dominion's "You control all Goblins" and Synthetic Puppeteer's Yoke's
-- filterless "all permanents". Blanking the perspective leaves the suite green.
-- The card that would prove them is #197's shape, a control-dependent conjunct
-- under a control grant.
--
-- The recursion this opens -- each candidate whose controller is forced
-- re-enters the fold, which walks the battlefield again -- is already reached by
-- controlNames' AttachedPlayerControls arm, which asks every battlefield
-- candidate's controller outside this function. That arm escapes the loop by
-- shortening the GRANT list instead, which is why it can see an earlier control
-- effect on the candidate under question and this function cannot: here the
-- collapse to the object's owner is all that stands between a control-dependent
-- conjunct and the fold that is asking. A filter that had to see CR 613.8b's
-- ordering would need the same treatment; the card that would ask for it is the
-- #197 shape named above, which the pool does not have.
matchesLeanly :: [ControlGrant] -> Set ObjectId -> GameState -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> Bool
matchesLeanly grants visited gs source f oid =
  Filter.matches
    (Filter.contextFor (Game.teams gs) (controllerOfGiven grants visited source gs) (Just source))
    (leanViewOf grants visited gs oid)
    f

-- The Filter.View a layer-2 affected set reads: copiable characteristics
-- (CR 613.2c) and a controller from the lean fold. The counterpart to
-- viewOfObjectGiven for a caller that must not project at all; a host is read
-- the same way, which is finite for the reason viewOfObjectGiven gives.
leanViewOf :: [ControlGrant] -> Set ObjectId -> GameState -> ObjectId -> Filter.View
leanViewOf grants visited gs oid =
  viewOfCharacteristics
    (Just . leanViewOf grants visited gs)
    oid
    (copiableCharacteristics oid gs)
    (controllerOfGiven grants visited oid gs)
    (countersOf oid gs)
    gs

-- CR 110.2 / 108.4a: the controller a CR 613.1b layer-2 effect OVERRIDES. A
-- permanent's default controller is whoever it entered under (CR 110.2), and
-- Object.enteredUnder is Nothing outside the two zones CR 109.4 gives a
-- controller, leaving CR 108.4a's owner.
defaultControllerOf :: Object.Object -> PlayerId.PlayerId
defaultControllerOf obj = Maybe.fromMaybe (Object.owner obj) (Object.enteredUnder obj)
