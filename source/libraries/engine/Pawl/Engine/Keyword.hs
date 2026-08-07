module Pawl.Engine.Keyword where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import Pawl.Types.ActivatedAbility (ActivatedAbility)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import Pawl.Types.Card (Card)
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.CastingPermission (CastingPermission)
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import Pawl.Types.Filter (Filter)
import qualified Pawl.Types.Filter as Filter
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import Pawl.Types.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern

-- Rule 702 in its OTHER voice. Most keywords this pool has are read where they
-- matter -- Projection.hasKeyword for an evasion or combat bit,
-- Pawl.Engine.Damage for infect's and toxic's damage riders -- because the rule
-- states them as static abilities some rules-core reader already asks about.
-- Rules 702.70 and 702.91 do not: they spell poisonous and battle cry out as
-- TRIGGERED abilities, so those have to be MINTED and handed to the ordinary CR
-- 603 machinery rather than merely consulted.
--
-- Casing on Keyword here is legitimate for the reason Pawl.Types.Keyword's own
-- comment gives: a keyword is a numbered rule, not an effect's identity. What
-- this module must never do is grow an arm for a CARD.
--
-- triggeredAbilitiesOf derives its abilities from a projection's POST-LAYER
-- keyword counts, so Humility takes rule 702.70a's and rule 702.91a's abilities
-- away for free and an Aura's layer-6 grant adds them. Its one caller is
-- Pawl.Engine.Event's EVENT scan; rule 702 has no state-triggered (CR 603.8) or
-- delayed (CR 603.7) keyword ability, so the first keyword that needs one must
-- widen those two scans.
--
-- Rule 702.34a's flashback shows how wide this voice is: ONE keyword becomes a
-- cost (flashbackCost), a casting permission (castingPermissionsOf) and a
-- replacement effect (flashbackExile). Those three readers get ordinary rules
-- objects and never learn that flashback produced them. All three are derived
-- from a card's PRINTED keywords rather than a projection's post-layer ones,
-- because all three function in the graveyard or on the stack (CR 113.6),
-- neither of which pawl's projection reaches (#160). entwineCost is read the
-- same way and for the same reason (CR 702.42a).

-- CR 702.70b: multiple instances of poisonous each trigger separately, so this
-- returns one ability PER INSTANCE -- `Poisonous 1` twice is two abilities and
-- two poison counters, not one ability for 2. (Contrast CR 702.164b, where
-- toxic's N values are SUMMED into a single rider -- Projection.totalToxic.) CR
-- 702.91b says the same of battle cry, so the two minting arms below are the
-- same shape.
--
-- Order is the Map's, which is Keyword's Ord -- rule-number order, and stable.
-- The CR 603.3b ordering prompt indexes into the scan's canonical order, so this
-- being deterministic is what keeps that prompt reproducible.
triggeredAbilitiesOf :: Map Keyword Natural -> [TriggeredAbility Card]
triggeredAbilitiesOf counts = concatMap (uncurry abilitiesFor) (Map.toAscList counts)

-- The abilities one keyword, held `count` times, contributes.
abilitiesFor :: Keyword -> Natural -> [TriggeredAbility Card]
abilitiesFor keyword count = case keyword of
  Keyword.Poisonous n -> List.genericReplicate count (poisonous n)
  Keyword.BattleCry -> List.genericReplicate count battleCry
  Keyword.Crew _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Banding -> []
  Keyword.Fear -> []
  Keyword.Morph _ -> []
  Keyword.Menace -> []
  Keyword.Cycling _ _ -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Riot -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Toxic _ -> []
  Keyword.StartYourEngines -> []

-- CR 602.1: the ACTIVATED abilities rule 702 gives a card while it sits in its
-- owner's hand, and the first sibling here that mints something a player takes
-- an action with.
--
-- Named for the ZONE rather than for cycling, because that is the classification
-- its one reader wants: Pawl.Engine.Activate.abilitiesFor asks "what can be
-- activated from here" and never learns that rule 702.29 produced any of them.
-- Rule 702 has more hand abilities to come (forecast, CR 702.57, is the next),
-- and each joins this list without its reader changing.
--
-- Printed keywords rather than a projection's post-layer ones, the same rules
-- fact castingPermissionsOf records: CR 113.6b confines an ability to the zones
-- it states, and rule 702.29a states the hand -- which pawl's projection does not
-- reach (#160).
handAbilitiesOf :: Set Keyword -> [ActivatedAbility Card]
handAbilitiesOf = concatMap handAbilitiesFor . Set.toAscList

-- Exhaustive for the reason permissionsFor is: rule 702 keeps adding abilities
-- that function from a hand, so the next one must break this build rather than
-- silently produce nothing.
handAbilitiesFor :: Keyword -> [ActivatedAbility Card]
handAbilitiesFor keyword = case keyword of
  Keyword.Cycling cost searchFor -> [cycling cost searchFor]
  Keyword.Crew _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Banding -> []
  Keyword.Fear -> []
  Keyword.Morph _ -> []
  Keyword.Menace -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Poisonous _ -> []
  Keyword.BattleCry -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Riot -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Toxic _ -> []
  Keyword.StartYourEngines -> []

-- CR 702.29a: cycling means paying its cost and discarding the card to draw a
-- card. The whole ability, minted from the one cost the keyword carries.
--
-- The discard is a COMPONENT of the activation cost and not an effect, because
-- rule 702.29a puts it before the colon. Three things follow that would all be
-- wrong the other way round: an activation the player backs out of discards
-- nothing (Pawl.Engine.Cost.pay restores the entry state), the card is already
-- in the graveyard while the draw is still on the stack, and CR 702.29c's "when
-- you cycle this card" has a cost payment to trigger off rather than a
-- resolution.
--
-- The card's own data carries only what is PRINTED on it, and rule 702.29a's
-- discard is added here. That is the split the whole module exists for: the card
-- says which keyword, the rule says what it means.
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability
-- is activated. AnyTime because rule 702.29a states no timing restriction, which
-- leaves CR 117.1b's default.
cycling :: Cost Keyword -> Maybe (Filter Keyword) -> ActivatedAbility Card
cycling cost searchFor =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost {Cost.components = Cost.components cost <> [CostComponent.DiscardThis]},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty Optionality.Mandatory Nothing))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.timing = ActivationTiming.AnyTime,
      -- CR 702.29a gives the card this ability outright, with no "as long as".
      ActivatedAbility.condition = Nothing
    }
  where
    -- The only difference between rule 702.29a and rule 702.29e: what the
    -- ability does once its cost is paid. Everything above is shared, which is CR
    -- 702.29f holding by construction.
    --
    -- CR 702.29a draws for the ability's controller, which CR 113.8 makes the
    -- player who activated it -- so You, the perspective Pawl.Engine.Resolve
    -- evaluates a PlayerRef against.
    --
    -- CR 702.29e searches instead. The reveal is part of the destination because
    -- it is part of that same sentence -- see Pawl.Types.SearchDestination, and CR
    -- 701.23e for why a search does not reveal on its own.
    effect = case searchFor of
      Nothing -> Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)
      Just filter_ -> Effect.Search filter_ SearchDestination.RevealThenHand

-- CR 602.1: the ACTIVATED abilities rule 702 gives a PERMANENT, handAbilitiesOf's
-- sibling one zone over. Named for the zone for that function's reason, and read
-- by Pawl.Engine.Projection.abilitiesGiven, which appends them to the projection's
-- own list and never learns that rule 702 produced any of them.
--
-- POST-LAYER keywords, unlike handAbilitiesOf's printed ones, and the contrast is
-- CR 113.6 again: this ability functions on the battlefield, which the projection
-- does reach. So Humility takes crew away at CR 613.1f layer 6 for free, and an
-- effect that grants crew adds it.
--
-- One ability PER INSTANCE, rule 702.70b's reading rather than rule 702.164b's:
-- CR 702.122a states a whole self-contained ability, so a permanent with crew
-- twice has two of them to activate and two thresholds, and nothing is summed.
--
-- Order is the Map's, which is Keyword's Ord -- rule-number order, and stable, for
-- triggeredAbilitiesOf's reason.
battlefieldAbilitiesOf :: Map Keyword Natural -> [ActivatedAbility Card]
battlefieldAbilitiesOf counts = concatMap (uncurry battlefieldAbilitiesFor) (Map.toAscList counts)

-- Exhaustive, exactly as handAbilitiesFor is, and for the same reason.
battlefieldAbilitiesFor :: Keyword -> Natural -> [ActivatedAbility Card]
battlefieldAbilitiesFor keyword count = case keyword of
  Keyword.Crew n -> List.genericReplicate count (crew n)
  Keyword.Cycling _ _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Banding -> []
  Keyword.Fear -> []
  -- CR 702.37e: turning a face-down permanent face up is a SPECIAL ACTION and
  -- doesn't use the stack (CR 116), so morph gives a permanent no activated
  -- ability. Pawl.Engine.Keyword.morphCost serves that action instead.
  Keyword.Morph _ -> []
  Keyword.Menace -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Poisonous _ -> []
  Keyword.BattleCry -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Riot -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Toxic _ -> []
  Keyword.StartYourEngines -> []

-- CR 702.122a: "Crew N" means "Tap any number of other untapped creatures you
-- control with total power N or greater: This permanent becomes an artifact
-- creature until end of turn." The whole ability, minted from the one number the
-- keyword carries -- the card says which keyword, the rule says what it means.
--
-- THE COST. Every word of rule 702.122a's criterion is written in the existing
-- Filter vocabulary rather than baked into the component: "other" is
-- `Not IsSource` (#163's one-relation-one-spelling), "untapped" is
-- `Not IsTapped`, "creatures" is `HasCardType Creature` and "you control" is
-- `ControlledBy You`. Only the AGGREGATE is the component's own, total power
-- being a property of the chosen set rather than of any candidate -- see
-- Pawl.Types.CostComponent.TapForTotalPower.
--
-- `Not IsSource` is load-bearing and not decoration: a Vehicle that has already
-- become a creature -- by an earlier crew, or by Opalescence -- would otherwise
-- be an untapped creature its controller controls, and could crew itself.
--
-- CR 302.6 does NOT reach this cost, in either direction. The Vehicle needs no
-- haste, because the tap symbol is not in the cost (Cost.requiresSicknessCheck
-- tests for CostComponent.TapThis and this is not one); and a creature that
-- arrived this turn may still be tapped to crew, because rule 302.6 gates only a
-- creature's OWN activated ability with the tap symbol in it. The Vehicle it
-- crews is still subject to rule 302.6's second sentence when it attacks.
--
-- THE EFFECT. "Becomes an artifact creature" ADDS two card types and sets
-- nothing, which is CR 205.1b naming this exact phrase: "some effects state that
-- an object becomes an 'artifact creature'; these effects also allow the object
-- to retain all of its prior card types and subtypes". So the Vehicle stays a
-- Vehicle, and Modification.AddCardType -- which adds and has no setting sibling
-- -- is the right opcode rather than a near miss. TWO of them, artifact and
-- creature being separate card types (CR 300.1), in one mode rather than one
-- opcode over a set: AddCardType carries a single type by design, and CR 613.7b
-- stamps both at the moment this one resolution creates them, so nothing in CR
-- 613.7's ordering can come between them.
--
-- Layer 4 either way (CR 613.1d).
--
-- ObjectRef.InSlot Binding.triggerSource -- the engine-reserved "self" slot -- so
-- the Vehicle is named and never TARGETED (CR 115.10a): rule 702.122a says "this
-- permanent", and a targeted crew would fizzle to shroud and fire "becomes the
-- target" triggers that the printed ability does not.
--
-- CR 208.3 is what makes this observable at all, and it needs no clause here: the
-- Vehicle's printed power and toughness are gated on its being a creature at
-- Pawl.Engine.Projection's read points, so adding the type is the whole of CR
-- 301.7b's "it immediately has its printed power and toughness".
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability is
-- activated, cycling's posture. AnyTime because rule 702.122a states no timing
-- restriction, which leaves CR 117.1b's default -- and CR 702.122a's "any number
-- of times" needs no expression, an activated ability having no once-per-turn
-- limit unless one is printed.
crew :: Natural -> ActivatedAbility Card
crew n =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost =
        Cost.MkCost
          { -- CR 118.5: rule 702.122a's cost has no mana part, which is
            -- `Just` an empty one and not the Nothing that means unpayable.
            Cost.mana = Just (ManaCost.MkManaCost []),
            Cost.components = [CostComponent.TapForTotalPower n criterion]
          },
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.fromList [becomes CardType.Artifact, becomes CardType.Creature]) Map.empty Optionality.Mandatory Nothing))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.timing = ActivationTiming.AnyTime,
      -- CR 702.122a gives the permanent this ability outright, with no "as long
      -- as", cycling's answer.
      ActivatedAbility.condition = Nothing
    }
  where
    criterion =
      Filter.And
        [ Filter.HasCardType CardType.Creature,
          Filter.Not Filter.IsTapped,
          Filter.ControlledBy PlayerRelation.You,
          Filter.Not Filter.IsSource
        ]
    becomes cardType =
      Effect.ModifyTarget
        Duration.UntilEndOfTurn
        (Modification.AddCardType cardType)
        (ObjectRef.InSlot Binding.triggerSource)

-- CR 601.3: the casting permissions rule 702 gives a card for holding a keyword.
-- A card's own printed permissions (Face.castingPermissions) are a separate,
-- additive list; Pawl.Engine.Cast reads both.
--
-- Taken over the card's PRINTED keywords rather than a projection's post-layer
-- ones: this permission functions in the GRAVEYARD (CR 113.6), which pawl's
-- projection does not reach (#160).
--
-- The card types come along because rule 702.34a's permission is CONDITIONAL on
-- them. They are the types of the one FACE being proposed, which is the caller's
-- doing -- see Pawl.Engine.Cast.permissionsOf for why that is the right face.
castingPermissionsOf :: Set CardType.CardType -> Set Keyword -> [CastingPermission]
castingPermissionsOf cardTypes = concatMap (permissionsFor cardTypes) . Set.toAscList

-- Exhaustive, exactly as abilitiesFor is, and for the same reason: rule 702 is
-- full of keywords that grant a zone permission (madness, retrace, escape,
-- disturb), so the next one added must break this build rather than silently
-- grant nothing.
permissionsFor :: Set CardType.CardType -> Keyword -> [CastingPermission]
permissionsFor cardTypes keyword = case keyword of
  -- CR 702.34a: "You may cast this card from your graveyard IF THE RESULTING
  -- SPELL IS AN INSTANT OR SORCERY SPELL by paying [cost] rather than paying its
  -- mana cost." The clause gates the permission itself, so a card that fails it
  -- gets no permission at all rather than a permission it cannot use --
  -- Pawl.CastSpec's "FlashbackCardType" group proves both directions.
  --
  -- Gated HERE rather than in Pawl.Engine.Cast.permitsCastFromGraveyard because
  -- the condition belongs to rule 702.34a, not to CastFromGraveyard: a card that
  -- PRINTS the same permission need not restrict itself to instants and
  -- sorceries, and must not inherit flashback's clause.
  Keyword.Flashback _
    | Set.member CardType.Instant cardTypes || Set.member CardType.Sorcery cardTypes ->
        [CastingPermission.CastFromGraveyard]
    | otherwise -> []
  -- CR 702.29a is an ACTIVATED ability, not a casting permission: cycling
  -- discards the card, it never casts it. See handAbilitiesOf above.
  Keyword.Cycling _ _ -> []
  -- CR 702.122a is an activated ability too, and one that functions on the
  -- battlefield -- see battlefieldAbilitiesOf above.
  Keyword.Crew _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  -- CR 702.8a grants no permission either, and it is the near miss flashback's
  -- neighbour makes worth stating: its SECOND sentence widens the TIME a cast may
  -- be proposed at (Pawl.Engine.Cast.instantSpeed) and names no zone, while its
  -- first names the zones the ABILITY functions in rather than the zones the card
  -- may be cast from. So a card with flash is castable from exactly the zones it
  -- was castable from without it.
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Banding -> []
  Keyword.Fear -> []
  Keyword.Morph _ -> []
  Keyword.Menace -> []
  -- CR 702.42a grants no permission: entwine widens a MODE choice and adds a
  -- cost to a cast that some other rule already allowed; it never allows one.
  Keyword.Entwine _ -> []
  Keyword.Poisonous _ -> []
  Keyword.BattleCry -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Riot -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Toxic _ -> []
  Keyword.StartYourEngines -> []

-- CR 702.8a: does this card's keyword set let it be played any time its
-- controller could cast an instant? Its one reader is
-- Pawl.Engine.Cast.instantSpeed, which turns it into the CR 302.1 / 307.1 window
-- being lifted for that one card.
--
-- MEMBERSHIP, not a count: CR 702.8b makes multiple instances of flash on the
-- same object redundant, so a second one has nothing left to widen.
--
-- Two separate facts make reading a Set of PRINTED keywords right here, and
-- neither of them is the other.
--
-- WHERE the ability functions is the rules half: rule 702.8a's first sentence
-- and CR 113.6e put it in any zone the card could be played from, and on the
-- stack. So a hand and a graveyard are zones this must be readable in at all,
-- which is why the caller asks a card rather than a permanent.
--
-- WHETHER printed is the right source is the engine half, and the rules do NOT
-- say it is: CR 613.1 names no zone, and CR 122.1b's keyword counter reaches a
-- card outside the battlefield explicitly. What makes the printed read safe is
-- that it is INDISTINGUISHABLE from a projected one today, which is a claim about
-- what pawl CANNOT EXPRESS rather than about Magic. Nothing can put a
-- keyword-changing effect on a card in a hand, and that takes all four of
-- Pawl.Types.Affected:
--
--   * Matching and AttachedPlayerControls are gated on battlefield membership,
--     structurally, inside Projection.affects.
--   * Attached names the object the SOURCE is attached to, which an Aura only
--     ever has while both are on the battlefield.
--   * TheseObjects is the one that could in principle reach elsewhere -- it is
--     CR 611.2c's frozen set, and Magical Hack's ChangeText already stores one
--     naming a spell on the STACK. What stops it here is the pool: no
--     Pawl.Types.Pool arm names a card in a hand at all, and every
--     Modification.GainKeyword producer in the pool is aimed at creatures on the
--     battlefield.
--
-- So a card in a hand projects exactly its printed keywords, and nothing can
-- grant or remove flash there (#160). The first effect that changes a
-- non-battlefield card's characteristics is what parts the two, and this becomes
-- a projected read then. The same posture castingPermissionsOf and
-- handAbilitiesOf above take (#567).
--
-- A membership test rather than an exhaustive case: this asks about ONE named
-- constructor rather than classifying every keyword, so a new arm has nothing to
-- say here.
hasFlash :: Set Keyword -> Bool
hasFlash = Set.member Keyword.Flash

-- CR 702.34a: the cost this card may be cast from the graveyard for, or Nothing
-- when it has no flashback. Read by Pawl.Engine.Cost.costsFor, which offers it
-- ONLY while the object is in a graveyard -- the zone half of the same sentence.
--
-- A wildcard rather than an exhaustive case: this asks about ONE named
-- constructor rather than classifying every keyword, so a new arm has nothing to
-- say here.
--
-- Nothing beyond the FIRST flashback cost is reachable. A card printing two
-- flashback abilities is expressible and unrepresented in what this returns; no
-- printing does it (#294).
flashbackCost :: Set Keyword -> Maybe (Cost Keyword)
flashbackCost keywords =
  let costOf keyword = case keyword of
        Keyword.Flashback cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.37a / 702.37e: the MORPH cost -- what a face-down permanent's
-- controller pays to turn it face up as CR 116.2b's special action -- or Nothing
-- when the card has no morph ability. Read by Pawl.Engine.FaceDown.
--
-- NOT the cost of the morph CAST: rule 702.37a writes that one into the rule
-- itself ("by paying {3}"), so it comes from Pawl.Engine.Cost.faceDownCost and
-- never from a card.
--
-- Asked of the card's PRINTED keywords, which for morph is the rule's own scope:
-- CR 702.37a says the ability "functions in any zone from which you could play
-- the card it's on", and CR 702.37e reads it off "the permanent's morph cost
-- WOULD BE IF IT WERE FACE UP" -- a face-down permanent projects no keywords at
-- all (CR 708.2a), so a projected read would find nothing to pay.
--
-- A wildcard rather than an exhaustive case, exactly as flashbackCost above.
--
-- Nothing beyond the FIRST morph cost is reachable: a card printing two morph
-- abilities is expressible and unrepresented, as for flashback and entwine, and
-- no printing does it.
morphCost :: Set Keyword -> Maybe (Cost Keyword)
morphCost keywords =
  let costOf keyword = case keyword of
        Keyword.Morph cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.42a: the ADDITIONAL cost this card's controller may pay to choose all
-- of its modes, or Nothing when it has no entwine. Read by Pawl.Engine.Cast,
-- which offers it at CR 601.2b and adds it to whichever candidate cost was
-- announced (CR 601.2f).
--
-- A wildcard rather than an exhaustive case, exactly as flashbackCost above.
--
-- Nothing beyond the FIRST entwine cost is reachable: a card printing two
-- entwine abilities is expressible and unrepresented (#474).
entwineCost :: Set Keyword -> Maybe (Cost Keyword)
entwineCost keywords =
  let costOf keyword = case keyword of
        Keyword.Entwine cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.34a's SECOND static ability, the one functioning while the card is on
-- the stack: exile it instead of putting it anywhere else as it leaves.
-- Filter.IsSource, because the rule says "this card" -- the spell itself and no
-- other object. Evaluated against the spell's own projected view, which exists
-- for as long as the spell does; Pawl.Engine.Event proposes the move before it
-- performs one, so the object is still on the stack when this is asked.
--
-- The destination is Graveyard rather than "anywhere else": a
-- Pawl.Types.ZoneChangePattern names ONE destination, and the graveyard is the
-- only place a spell leaves the stack for in this pool (CR 608.2n, the CR 608.2b
-- fizzle, CR 701.6a's counter) (#293).
--
-- Not gated on rule 702.34a's "if the flashback cost was paid": nothing here can
-- see which cost was paid (#101). Pawl.Engine.Cast installs this only for a
-- spell cast FROM THE GRAVEYARD, where the two coincide.
--
-- The door Pawl.Engine.Cast uses, so that module installs a REPLACEMENT EFFECT
-- it never inspects rather than asking whether a card has flashback.
castFromGraveyardReplacementsOf :: Set Keyword -> [ReplacementEffect]
castFromGraveyardReplacementsOf keywords =
  [flashbackExile | Maybe.isJust (flashbackCost keywords)]

flashbackExile :: ReplacementEffect
flashbackExile =
  ReplacementEffect.ZoneChangeR
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = Zone.Graveyard,
        ZoneChangePattern.whoseObject = ControllerRelation.Anyones,
        ZoneChangePattern.whatObject = Filter.IsSource
      }
    Zone.Exile

-- CR 702.136a: the AS-ENTERS REPLACEMENT rule 702 gives a permanent for holding
-- riot -- "You may have this permanent enter with an additional +1/+1 counter on
-- it. If you don't, it gains haste." The same voice the minted triggered
-- abilities (rule 702.70a), the minted hand ability (rule 702.29a) and
-- flashback's exile replacement (rule 702.34a) speak in: the card says which
-- keyword, the rule says what it means.
--
-- The FIRST minted replacement that functions on the battlefield, where
-- flashbackExile's is installed by Pawl.Engine.Cast on a spell -- which is why
-- this one is gathered by the projection and that one is not.
--
-- POST-LAYER keyword COUNTS, like triggeredAbilitiesOf and unlike
-- handAbilitiesOf's printed set -- rule 702.136a functions on the battlefield, so
-- Humility takes it away and a static ability that grants riot (Spider-Punk's
-- "other Spiders you control have riot") adds it, both for free.
--
-- ONE ROW PER INSTANCE, because CR 702.136b says each instance works separately:
-- a creature with riot twice should be asked twice, and may take a counter for
-- one instance and haste for the other.
--
-- Not implemented: the two rows are EQUAL VALUES, and CR 614.5's identity here is
-- (source, effect value), so a second instance gets no second opportunity and the
-- second ask never happens (#75). The replication is written the rule's way
-- anyway, so that closing #75 makes rule 702.136b right with no change here.
--
-- The pattern is Filter.IsSource: CR 614.1c's ability is the entering object's
-- own.
entryReplacementsOf :: Map Keyword Natural -> [ReplacementEffect]
entryReplacementsOf counts = concatMap (uncurry entryReplacementsFor) (Map.toAscList counts)

-- Exhaustive for abilitiesFor's reason: rule 702 keeps adding abilities that
-- rewrite an entry, and the next one must break this build rather than silently
-- produce nothing.
entryReplacementsFor :: Keyword -> Natural -> [ReplacementEffect]
entryReplacementsFor keyword count = case keyword of
  Keyword.Riot -> List.genericReplicate count (ReplacementEffect.EntryR Filter.IsSource EntryRewrite.Riot)
  Keyword.Crew _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Banding -> []
  Keyword.Fear -> []
  Keyword.Morph _ -> []
  Keyword.Menace -> []
  Keyword.Cycling _ _ -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Poisonous _ -> []
  Keyword.BattleCry -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Toxic _ -> []
  Keyword.StartYourEngines -> []

-- CR 702.136a again, in the SHORT-CIRCUIT's voice:
-- Pawl.Engine.Projection.replacementsAffecting skips the whole board when no
-- permanent's BASE card could hold a replacement effect, and a riot row is minted
-- from the projection rather than printed in a face's list -- so, like the
-- planeswalker disjunct beside it, the gate has to be told which keywords mint
-- one.
--
-- Membership rather than a count, because the gate asks whether there is any.
mintsEntryReplacement :: Keyword -> Bool
mintsEntryReplacement keyword = not (null (entryReplacementsFor keyword 1))

-- CR 702.70a: a creature with poisonous N gives a player it deals combat damage
-- to that many poison counters.
--
-- "That player" is the player the trigger's own event named, which
-- Pawl.Engine.Event.eventBindings stamps under the reserved Binding.triggerPlayer
-- slot as the trigger is gathered -- so the payload is an ordinary slot read and
-- this ability needs no opcode of its own. NOT the ability's controller: CR
-- 603.3a makes that the creature's controller, and the poison goes to their
-- victim.
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability
-- is placed -- rule 702.70a leaves nothing to choose, and has no "if" clause, so
-- intervening = Nothing.
poisonous :: Natural -> TriggeredAbility Card
poisonous n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDealsCombatDamageToPlayer,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty Optionality.Mandatory Nothing))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }
  where
    effect =
      Effect.GainPlayerCounters
        (PlayerRef.InSlot Binding.triggerPlayer)
        PlayerCounterKind.Poison
        (Quantity.Literal (toInteger n))

-- CR 702.91a: whenever this creature attacks, each other attacking creature gets
-- +1/+0 until end of turn. Rule 702.70a's poisonous above is the only other
-- keyword in this pool stated as a triggered ability, and this is built the same
-- way: minted here and handed to the ordinary CR 603 machinery, which never
-- learns a keyword produced it.
--
-- CR 508.3a is what "attacks" means -- being declared as an attacker -- so the
-- condition is the self-scoped SelfAttacks, EveryTime: rule 702.91a states no
-- "for the first time each turn" narrowing.
--
-- "EACH OTHER ATTACKING CREATURE" is a SET, swept at resolution and then frozen
-- (CR 611.2c), which is exactly Trumpet Blast's ObjectRef.EachMatching -- so this
-- needs no opcode of its own either. The three conjuncts are the three printed
-- words: a creature (CR 109.2 draws the set from the battlefield), attacking,
-- and OTHER, which is `Not IsSource` -- the spelling Filter.IsSource fixes for
-- "another" (#163), and the reason a battle-crying creature never pumps itself.
--
-- The tokens Hero of Bladehold's SECOND ability puts onto the battlefield
-- attacking are in the set or not according to WHEN this resolves, and that is
-- the whole of CR 603.3b's ordering choice: CR 611.2c fixes the affected set as
-- the effect begins, so a token that arrives afterwards is not pumped.
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability
-- is placed -- rule 702.91a leaves nothing to choose, and has no "if" clause, so
-- intervening = Nothing.
battleCry :: TriggeredAbility Card
battleCry =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty Optionality.Mandatory Nothing))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }
  where
    effect =
      Effect.ModifyTarget
        Duration.UntilEndOfTurn
        (Modification.ModifyPowerToughness (Quantity.Literal 1) (Quantity.Literal 0))
        ( ObjectRef.EachMatching
            (Filter.And [Filter.HasCardType CardType.Creature, Filter.IsAttacking, Filter.Not Filter.IsSource])
        )
