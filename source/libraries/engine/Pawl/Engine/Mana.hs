module Pawl.Engine.Mana where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Summoning as Summoning
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HybridPayment as HybridPayment
import Pawl.Types.Mana (Mana)
import qualified Pawl.Types.Mana as Mana
import Pawl.Types.ManaCost (ManaCost)
import qualified Pawl.Types.ManaCost as ManaCost
import Pawl.Types.ManaProduction (ManaProduction)
import qualified Pawl.Types.ManaProduction as ManaProduction
import Pawl.Types.ManaSymbol (ManaSymbol)
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import Pawl.Types.ManaType (ManaType)
import qualified Pawl.Types.ManaType as ManaType
import Pawl.Types.ManaUnit (ManaUnit)
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.Subtype (Subtype)
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState

-- | CR 305.6
subtypeMana :: Subtype -> Maybe ManaType
subtypeMana subtype = case subtype of
  Subtype.Mountain -> Just (ManaType.Colored Color.Red)
  Subtype.Swamp -> Just (ManaType.Colored Color.Black)
  Subtype.Forest -> Just (ManaType.Colored Color.Green)
  Subtype.Island -> Just (ManaType.Colored Color.Blue)
  Subtype.Plains -> Just (ManaType.Colored Color.White)
  _ -> Nothing

-- CR 105.4: a player asked to choose a color must choose one of the five;
-- multicolored and colorless are not colors. So an any-colour producer offers
-- exactly five options and never {C} -- which is also why AnyColor cannot be
-- spelled as "every ManaType".
--
-- Written out rather than derived from a Bounded Color: the five are CR 105.1's
-- closed enumeration, and spelling them here keeps the rule citation next to the
-- list.
producedTypes :: ManaProduction -> [ManaType]
producedTypes production = case production of
  ManaProduction.OfType manaType -> [manaType]
  ManaProduction.AnyColor ->
    fmap
      ManaType.Colored
      [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green]

-- Every ROUTE by which this object could be activated for mana, as the mana ONE
-- activation of it adds: its intrinsic subtype mana (CR 305.6), one route per
-- basic land type, PLUS one route per MODE (CR 700.2) of every projected
-- activated ability that is a mana ability (CR 605.1a), resolved inline at
-- payment and never on the stack (CR 605.3b).
--
-- CR 106.12 narrows "tap for mana" to a mana ability that includes {T} in its
-- activation cost, and NOTHING here reads a cost -- not this function and not
-- isManaAbility. Every mana ability in the pool costs exactly {T}
-- (manaSourcesGiven leans on the same fact for CR 302.6), so the filter would
-- change no answer; one that did not would be enumerated here as though it
-- tapped (#238).
--
-- The nesting is the whole point. The OUTER list is the options -- which ability
-- of this permanent, and which of its modes. The INNER list is that one
-- activation's YIELD, its AddMana effects in printed order (CR 608.2c). Sol
-- Ring's "{T}: Add {C}{C}" is one option adding two mana; an Urborg'd Mountain
-- is two options of one mana each.
--
-- Read through the projection (abilitiesOf), so Humility (layer 6) strips a
-- creature's mana ability too -- and so does CR 305.7 at layer 4, which is what
-- swaps a Blood Moon'd Reliquary Tower's printed "{T}: Add {C}" for the
-- Mountain's {R} rather than adding to it.
--
-- One route per mode, rather than one per combination of modes, because CR
-- 700.2's selection is "choose exactly one" for every mana ability in the pool.
-- An ability that chose two modes at once would make a route the CONCATENATION
-- of the chosen modes' yields (#449). A mode adding no mana contributes an empty
-- route, which the supply model ignores.
--
-- Takes a PRE-PROJECTED board, because every caller asks this of every source a
-- player controls, and each of the two projection reads here was a fresh gather
-- per source (#200).
manaRoutesOfGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [[ManaProduction]]
manaRoutesOfGiven pcs oid gs =
  let fromSubtypes =
        fmap
          (\manaType -> [ManaProduction.OfType manaType])
          (Maybe.mapMaybe subtypeMana (Set.toList (Projection.subtypesGiven pcs oid gs)))
      modeRoutes ability =
        fmap (Maybe.mapMaybe ManaAbility.manaProduced) (Modal.modeEffects (ActivatedAbility.modal ability))
      fromAbilities = concatMap modeRoutes (filter isManaAbility (Projection.abilitiesGiven pcs oid gs))
   in fromSubtypes <> fromAbilities

-- What tapping this object for mana could actually put in a pool: every route
-- above with each ManaProduction resolved to a concrete type, so one entry per
-- (route, colour choice) pair. `traverse` over the list applicative is that
-- product -- Birds of Paradise's one route becomes CR 105.4's five one-mana
-- yields.
--
-- A Mana rather than a list of types because that is what a yield IS: some mana,
-- headed for a pool (CR 106.4). tapForMana adds the chosen one whole.
--
-- Deduplicated by the WHOLE yield. Two routes producing identical mana (an
-- Urborg'd Swamp is a Swamp twice over) are indistinguishable options -- no cost
-- and no rider tells two of this permanent's mana abilities apart yet (#238) --
-- so collapsing them elides a prompt with no content. Deliberately order-
-- sensitive: {R} then {B} and {B} then {R} stay two options, which can only ever
-- ASK where a set-valued dedup would not.
manaYieldsOf :: ObjectId -> GameState -> [Mana]
manaYieldsOf = manaYieldsOfGiven Map.empty

-- The same yields against a pre-projected board, which is manaRoutesOfGiven's
-- argument and carries its reason (#200).
manaYieldsOfGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [Mana]
manaYieldsOfGiven pcs oid gs =
  let tags = productionTagsGiven pcs oid gs
      asMana manaTypes =
        Mana.MkMana (fmap (\manaType -> ManaUnit.MkManaUnit {ManaUnit.manaType = manaType, ManaUnit.tags = tags}) manaTypes)
   in List.nub (fmap asMana (concatMap (traverse producedTypes) (manaRoutesOfGiven pcs oid gs)))

-- The production-time tags (Pawl.Types.ProductionTag) every mana this object
-- adds will carry. THE one place they are decided; manaYieldsOfGiven just above
-- is the one place they are stamped onto a unit. They are captured rather than
-- looked up later because the mana outlives the source (Pawl.Types.ManaUnit).
--
-- CR 106.3 is what makes reading the SOURCE the right question: mana produced by
-- an ability has that ability's source (CR 113.7) -- for everything here, the
-- permanent being tapped. CR 107.4h then asks whether that source is a snow one,
-- and the rules define no separate term for it: a snow source is a source that
-- is snow, which for a permanent is CR 205.4g's supertype and nothing else.
--
-- CR 205.4g is PERMANENT-scoped and CR 106.3's first clause is wider, so this
-- read would be too narrow for a source that is not a permanent. Nothing reaches
-- it with one: the two engine paths in are tapForMana and payableResolutions,
-- and both take their oid from manaSourcesGiven, which filters the battlefield.
productionTagsGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> Set.Set ProductionTag.ProductionTag
productionTagsGiven pcs oid gs =
  if Set.member Supertype.Snow (Projection.supertypesGiven pcs oid gs)
    then Set.singleton ProductionTag.Snow
    else Set.empty

-- The units of one yield, in printed order.
unitsOf :: Mana -> [ManaUnit]
unitsOf (Mana.MkMana units) = units

-- The types in one yield, in printed order.
typesOf :: Mana -> [ManaType]
typesOf = fmap ManaUnit.manaType . unitsOf

-- CR 106.7's shape: every mana type this object COULD produce, flattened across
-- its yields and deduplicated. A strictly weaker question than manaYieldsOf --
-- it says nothing about how MUCH a single activation adds, so a Sol Ring answers
-- [{C}] here and yields {C}{C} there. Kept apart deliberately: conflating them
-- is what made Sol Ring read as a choice between two singles (#238).
manaTypesOf :: ObjectId -> GameState -> [ManaType]
manaTypesOf oid gs = List.nub (concatMap typesOf (manaYieldsOf oid gs))

-- CR 605.1a: an activated ability is a mana ability if it could add mana AND
-- doesn't target and is not itself a loyalty ability (CR 606.2, which
-- Pawl.Engine.Cost.isLoyaltyCost answers; no loyalty ability in the pool adds
-- mana, so the clause is inert rather than checked here). Read at two sites:
-- manaRoutesOfGiven includes a mana ability as a source, and
-- Action.legalActions excludes it from the stack.
--
-- Asked of the WHOLE ability, across every mode -- CR 605.1a's "could add mana"
-- is satisfied by any mode that does, and CR 605.2 keeps it a mana ability even
-- where the game state stops it producing.
isManaAbility :: ActivatedAbility.ActivatedAbility Card.Card -> Bool
isManaAbility ab =
  not (null (Maybe.mapMaybe ManaAbility.manaProduced (Modal.allEffects (ActivatedAbility.modal ab))))
    && Map.null (Modal.allTargetSpecs (ActivatedAbility.modal ab))

setPool :: PlayerId -> Mana -> GameState -> GameState
setPool pid pool gs = gs {GameState.manaPool = Map.insert pid pool (GameState.manaPool gs)}

addMana :: PlayerId -> [ManaUnit] -> GameState -> GameState
addMana pid units gs =
  let Mana.MkMana existing = Game.poolOf pid gs
   in setPool pid (Mana.MkMana (existing <> units)) gs

-- CR 500.5: as a step or phase ends, any unspent mana left in a player's mana
-- pool empties -- a turn-based action that does not use the stack (CR 703.4q).
-- CR 106.4 supplies the wording every card that stops it is templated on: the
-- player is said to LOSE this mana.
--
-- Being a turn-based action does not put it in Engine.runTurnBasedActions, which
-- handles a step's OPENING: CR 703.4q's own moment is the step's end, so
-- Engine.runStep calls this there instead.
--
-- The RETENTION check lives here rather than at that call site, because it is
-- part of the turn-based action and not part of the moment: CR 500.5 names one
-- action, and which mana it takes belongs to the action. Asked PER PLAYER and
-- then PER UNIT, off the CR 613.11 player-axis carrier, through a typed question
-- (PlayerEffect.keepsUnspentMana) that never reveals which effect answered it.
-- Per unit because a card may name only some of the mana: Upwelling keeps every
-- type, Omnath, Locus of Mana only green. The per-player question is asked ONCE
-- and its predicate applied to that player's units -- the shape
-- keepsUnspentMana's argument order is built for.
--
-- A player left with nothing is DROPPED from the map rather than left holding an
-- empty pool: absent already means an empty pool (Game.poolOf), so keeping the
-- key would give the same state two spellings.
--
-- `gs` is the state as the step ends, so the effect is read at that moment -- an
-- Upwelling that left the battlefield during the step is simply not there.
emptyManaPools :: GameState -> GameState
emptyManaPools gs =
  let retain pid pool = case filter (PlayerEffect.keepsUnspentMana pid gs) (Mana.unwrap pool) of
        [] -> Nothing
        kept -> Just (Mana.MkMana kept)
   in gs {GameState.manaPool = Map.mapMaybeWithKey retain (GameState.manaPool gs)}

-- Untapped permanents this player controls that can produce mana (CR 109.4a:
-- a mana ability's controller is determined as though it were on the stack --
-- i.e. the permanent's controller, CR 110.2 -- not the object's owner).
--
-- ONE control-grant walk and ONE whole-board projection for the whole sweep,
-- threaded into every question asked of every permanent -- the hoist
-- Sba.performStateBasedActions takes for the CR 704.3 sweep and
-- Projection.controls takes for the grant list. Unhoisted,
-- each candidate cost several fresh Projection.gathers plus a fresh grant walk,
-- which made a function the priority loop reaches at every boundary quadratic in
-- the battlefield (#200).
--
-- Projection.projectGiven carries the snapshot argument. It holds here because
-- this is a pure function of one GameState: nothing can move between the
-- projection and its uses, and payCost's loop -- the one caller that DOES change
-- the state, by tapping -- takes a fresh State.get on every pass.
manaSources :: PlayerId -> GameState -> [ObjectId]
manaSources pid gs = manaSourcesGiven (Projection.controlGrants gs) (Projection.projectAll gs) pid gs

manaSourcesGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
manaSourcesGiven grants pcs pid gs =
  let -- CR 302.6: a sick creature can't use a {T} mana ability, and every mana
      -- ability in the pool costs {T}; a land is never sick-gated. Keyed to
      -- `pid`: the creature must have settled under the player reaching for the
      -- mana, not under whoever held it before (#198) -- and CR 702.10c's haste
      -- exemption applies as it does to any other {T} ability.
      notSickCreature oid =
        not (Set.member CardType.Creature (Projection.cardTypesGiven pcs oid gs))
          || Summoning.settledOrHastyGiven pcs pid oid gs
      isSource oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> Object.tapped obj == TapState.Untapped && not (null (manaRoutesOfGiven pcs oid gs)) && notSickCreature oid
   in filter isSource (Projection.controlsGiven grants pid gs)

-- CR 106.12's "tap [a permanent] for mana" -- add the mana one activation of one
-- of its mana abilities yields, and tap it. CR 605.3b: a mana ability does not
-- use the stack, so this is immediate -- which is also why the colour choice is
-- made HERE and not by Resolve.
--
-- Monadic because of that choice. A Mountain offers one yield and is never
-- asked; Birds of Paradise (CR 105.4) and an Urborg'd Mountain (CR 305.6/305.7)
-- offer several, and the engine never picks for the player. Which SOURCE to tap
-- is a separate question, and payCost asks it.
--
-- The whole yield lands, so Sol Ring's "{T}: Add {C}{C}" adds two units from one
-- activation. What this still does NOT do is run the ability's own activation
-- cost or its riders: CR 602.2b routes an activation through CR 601.2b-i, and
-- this taps directly instead (#238).
tapForMana :: ObjectId -> Game ()
tapForMana oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case manaYieldsOf oid gs of
      [] -> pure ()
      first : rest -> do
        -- CR 109.4a/110.2: mana goes to the mana ability's controller, which is
        -- the permanent's controller (CR 106.4 only says it lands in "a player's
        -- mana pool", not whose) -- and that same player makes the colour
        -- choice. Falls back to owner in the impossible case lookupObject just
        -- proved oid exists but controllerOf returns Nothing.
        let controller = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
        chosen <- chooseManaYield controller oid (first NonEmpty.:| rest) gs
        let tapped = obj {Object.tapped = TapState.Tapped}
            gs1 = gs {GameState.objects = Map.insert oid tapped (GameState.objects gs)}
        case chosen of
          Mana.MkMana produced -> State.put (addMana controller produced gs1)

-- Which mana this source produces -- which of its mana abilities, in which mode,
-- and which colour each of that mode's AddMana effects makes, asked as ONE
-- question because the answer is one yield.
--
-- Elided exactly when the source offers ONE yield, where no choice exists --
-- manaYieldsOf has already collapsed routes producing identical mana, so a
-- remaining list of two is two genuinely different yields.
--
-- FILTERED, NOT TRUSTED, the posture chooseSource and Cost.payComponents take.
-- Here that is not merely hygiene -- honouring a yield the source cannot make
-- would mint mana out of nothing.
chooseManaYield :: PlayerId -> ObjectId -> NonEmpty.NonEmpty Mana -> GameState -> Game Mana
chooseManaYield pid oid candidates gs = case candidates of
  only NonEmpty.:| [] -> pure only
  _ -> do
    answer <- Game.choose (Prompt.ChooseManaYield (Decide.deciderFor pid gs) pid oid candidates)
    pure $
      if List.elem answer (NonEmpty.toList candidates)
        then answer
        else NonEmpty.head candidates

-- What ONE mana must be to satisfy one typed symbol of a cost: one of these mana
-- types, carrying at least these production-time tags.
--
-- A SET of types rather than a single one, because CR 107.4e's hybrid symbol can
-- be paid in one of two ways. A plain `{R}` is the singleton case, so every
-- payment path below reads one shape and none cases on hybrid-ness.
--
-- The TAGS are CR 107.4h's {S}, a second axis rather than more types: the symbol
-- constrains how the mana was PRODUCED and not what it is. Empty for every other
-- symbol, which is what keeps them all indifferent to how their mana was made.
data Demand = MkDemand
  { demandTypes :: Set.Set ManaType,
    demandTags :: Set.Set ProductionTag.ProductionTag
  }
  deriving (Eq, Ord, Show)

-- ONE mana the player could put toward a cost, as what it could be: the types it
-- might have, and the tags it would carry. A pool unit is the settled case (its
-- type is already fixed); an untapped source is the open one, where the choice
-- has not been made yet.
data Supply = MkSupply
  { supplyTypes :: Set.Set ManaType,
    supplyTags :: Set.Set ProductionTag.ProductionTag
  }
  deriving (Eq, Ord, Show)

-- Could this one mana serve that one demand? The ONE relation both payment and
-- payability read, so the two can never disagree about it.
--
-- Types INTERSECT -- the supply need only be able to be one of the demanded
-- types -- while tags are a SUPERSET: CR 107.4h demands mana from a snow source,
-- and a mana that is not from one cannot become so. A supply carrying a tag
-- nothing asked for is no worse for it.
serves :: Supply -> Demand -> Bool
serves supply demand =
  not (Set.disjoint (supplyTypes supply) (demandTypes demand))
    && Set.isSubsetOf (demandTags demand) (supplyTags supply)

-- A pool unit as a supply. Its type is settled, so the option set is a
-- singleton, and its tags are the ones production stamped on it
-- (manaYieldsOfGiven).
supplyOf :: ManaUnit -> Supply
supplyOf unit =
  MkSupply
    { supplyTypes = Set.singleton (ManaUnit.manaType unit),
      supplyTags = ManaUnit.tags unit
    }

-- A demand for one mana of one of these types, however it was produced -- every
-- TYPED symbol but CR 107.4h's {S}. A symbol demanding no particular mana
-- (Generic, and either symbol's second way) builds no demand at all.
ofTypes :: Set.Set ManaType -> Demand
ofTypes types = MkDemand {demandTypes = types, demandTags = Set.empty}

-- CR 106.1b: the six types of mana. Written out rather than derived, for the
-- reason producedTypes writes out CR 105.1's five: the enumeration IS the rule.
--
-- This is the whole type side of CR 107.4h's {S}, which narrows nothing about
-- what the mana is and everything about where it came from.
everyManaType :: Set.Set ManaType
everyManaType =
  Set.fromList
    ( ManaType.Colorless
        : fmap ManaType.Colored [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green]
    )

-- Every way ONE symbol can be paid: a typed demand, and Nothing for a symbol that
-- demands no particular mana at all -- paired with the generic mana that way adds
-- and the LIFE it costs.
--
-- A LIST of ways, because CR 107.4e's other half is not a wider set but a
-- different SHAPE: a monocolored hybrid {2/B} is one black mana or two mana of
-- any type, and one mana and two mana cannot be the same demand. CR 107.4f's
-- Phyrexian symbol is the other two-way symbol, for the different reason the
-- LIFE field below gives; every other symbol, {S} included, offers exactly one.
--
-- The LIFE field is CR 107.4f's Phyrexian symbol, and it is the one way that is
-- not a mana payment at all, so it could not be folded into either of the other
-- two: 2 life is not 2 generic mana (CR 118.7a's reductions never touch it, and
-- CR 202.3g values the symbol at 1 rather than 2), and it is no typed demand
-- because it consumes no supply. Zero for every other symbol.
--
-- CR 107.4f's ways, and CR 107.4e's monocolored hybrid ways, are collapsed to ONE
-- before payment by `announce`, which is what CR 118.13a and CR 601.2b call for --
-- so those enumerations reach payment only where nothing announced (a cost paid
-- during a resolution or for a special action, CR 118.13b/c, #373). CR 107.4e's
-- COLOUR/COLOUR hybrid is the one still settled at payment (#729), and its single
-- way here is why: both halves are one mana of a stated type, so they are one
-- demand over two types rather than two ways.
waysOf :: ManaSymbol -> [(Maybe Demand, Natural, Natural)]
waysOf symbol = case symbol of
  ManaSymbol.OfType t -> [(Just (ofTypes (Set.singleton t)), 0, 0)]
  -- CR 107.4e: a colour/colour hybrid is paid with one mana of a stated type
  -- either way, so it contributes nothing to the generic count.
  ManaSymbol.Hybrid a b -> [(Just (ofTypes (Set.fromList [a, b])), 0, 0)]
  -- CR 107.4e's two ways, and the one-mana way is FIRST -- see resolutions.
  ManaSymbol.MonocoloredHybrid t -> [(Just (ofTypes (Set.singleton t)), 0, 0), (Nothing, monocoloredHybridGeneric, 0)]
  -- CR 107.4f's two ways: one mana of its colour, or 2 life. The colour is a
  -- ManaType here because that is what a demand is made of; CR 107.4f admits no
  -- colourless Phyrexian symbol, so ManaType.Colored is total rather than a case.
  --
  -- The mana way is FIRST, which resolutions' sort then keeps -- see there.
  ManaSymbol.Phyrexian c -> [(Just (ofTypes (Set.singleton (ManaType.Colored c))), 0, 0), (Nothing, 0, 2)]
  ManaSymbol.Generic n -> [(Nothing, n, 0)]
  -- CR 107.4h: one mana of any type produced by a snow source. ONE way, and a
  -- typed demand rather than a generic count -- the same rule's next sentence
  -- made structural, since generic-mana reductions don't affect {S} costs. Every
  -- type (CR 106.1b) and one tag, so nothing about the mana's identity is asked
  -- and everything about its provenance is.
  ManaSymbol.Snow -> [(Just (MkDemand everyManaType (Set.singleton ProductionTag.Snow)), 0, 0)]
  -- Unreachable in payment: substituteX removes every Variable before canPay.
  -- The match must be total, so a bare {X} demands nothing and counts as 0
  -- generic.
  ManaSymbol.Variable -> [(Nothing, 0, 0)]

-- CR 601.2b's "nonhybrid equivalent cost", enumerated: every way the whole cost
-- resolves into typed demands, an amount of generic mana and an amount of life,
-- one entry per combination of per-symbol ways. `traverse` over the list
-- applicative is that product.
--
-- This is what lets everything below it keep the ONE-SUPPLY-PER-DEMAND shape
-- spendDemands and canPay's Hall condition both rest on. CR 107.4e's {2/B} is the
-- only symbol two mana can pay, and rather than teach those two about a demand
-- that consumes two units -- which would break the counting each of them does --
-- the choice is made here, above them. CR 107.4f's {G/P} is absorbed the same
-- way: a symbol payable with no mana at all would be a demand nothing serves.
--
-- Finite and small, which is what keeps the search terminating: the product over
-- symbols of their ways, so 2^(number of monocolored hybrid and Phyrexian
-- symbols), and exactly one entry for every cost without one.
--
-- ORDERED, and the order is a choice pawl makes for the player, because unlike a
-- colour/colour hybrid these ways are DISTINGUISHABLE -- they spend different
-- amounts of mana, or mana against life. Two rules, in this priority:
--
--   1. LEAST LIFE first, by the sort. CR 107.4f's life is the resource that does
--      not come back: an unspent pool empties every step (CR 500.5) and a land
--      untaps. Conservative, and still pawl choosing -- which is why a cast or an
--      activation announces first (`announce`, CR 118.13a) and reaches this sort
--      with no Phyrexian symbol left to order. A cost paid during a resolution or
--      for a special action has no such announcement (#373).
--   2. Among equal life, FEWEST UNITS, which waysOf's per-symbol order already
--      gives and a STABLE sort preserves. Pawl choosing again, and reached on the
--      same terms: a cast or an activation announces its monocolored hybrids away
--      first, so only CR 118.13b/c's costs arrive here with a {2/X} to order
--      (#373).
--
-- The sort is what makes rule 1 hold across symbols rather than within one: for
-- {2/R}{G/P} the product alone would offer a 2-life way before a 0-life one.
resolutions :: ManaCost -> [([Demand], Natural, Natural)]
resolutions (ManaCost.MkManaCost symbols) =
  let collect ways =
        ( Maybe.mapMaybe (\(demand, _, _) -> demand) ways,
          sum (fmap (\(_, generic, _) -> generic) ways),
          sum (fmap (\(_, _, life) -> life) ways)
        )
   in List.sortOn (\(_, _, life) -> life) (fmap collect (traverse waysOf symbols))

-- CR 601.2f: the total cost with X resolved -- each Variable symbol becomes
-- Generic n, every other symbol unchanged, order preserved (ManaCost is a list,
-- never fixed arity). Applied before any payment, so a Variable never reaches
-- spend/canPay.
substituteX :: Natural -> ManaCost -> ManaCost
substituteX x (ManaCost.MkManaCost symbols) =
  ManaCost.MkManaCost (fmap sub symbols)
  where
    sub symbol = case symbol of
      ManaSymbol.Variable -> ManaSymbol.Generic x
      other -> other

-- Every way to remove one unit that satisfies `wanted`, one result per candidate
-- unit. The branching point of the search below.
removals :: Demand -> [ManaUnit] -> [[ManaUnit]]
removals wanted units = Maybe.mapMaybe without (zip [0 :: Int ..] units)
  where
    without (i, u) =
      if serves (supplyOf u) wanted
        then Just (take i units <> drop (i + 1) units)
        else Nothing

-- Spend one unit per demand, or Nothing if no assignment covers them all.
--
-- EXACT, by search. A greedy left-to-right match is correct only while every
-- demand has exactly one option; CR 107.4e's hybrid breaks that -- with pool
-- {R}{G} and cost {R/G}{R}, greedy hands the {R} unit to the hybrid and the {R}
-- demand then fails with a {G} still in the pool. So this backtracks: try each
-- unit that could serve the first demand, recurse, and keep the first assignment
-- that covers the rest. A mana cost is a handful of symbols, so the search is
-- trivially small, and being exact means canPay's Hall condition and this never
-- disagree about whether a cost is payable.
spendDemands :: [ManaUnit] -> [Demand] -> Maybe [ManaUnit]
spendDemands units demands = case demands of
  [] -> Just units
  wanted : rest ->
    Maybe.listToMaybe (Maybe.mapMaybe (\left -> spendDemands left rest) (removals wanted units))

takeAny :: [ManaUnit] -> a -> Maybe [ManaUnit]
takeAny units _ = case units of
  _ : rest -> Just rest
  [] -> Nothing

-- Spend a pool against a cost, within a budget of `budget` life. Nothing when no
-- resolution fits; otherwise the pool that is left and the life to pay for it.
--
-- Typed symbols are matched FIRST because they are the constrained ones: generic
-- takes any unit, so paying it first could consume the only red and strand a
-- {R} that nothing else can satisfy.
--
-- The FIRST resolution that fits wins. For every cost without a monocolored
-- hybrid or a Phyrexian symbol there is only one; where there are several,
-- `resolutions` has already put the least-life, fewest-units one first.
--
-- The BUDGET is a cap and not a target, and it is what keeps that ordering
-- meaningful during payCost's loop. Left uncapped, a {G/P} would take the 2-life
-- resolution on the first pass -- the pool is empty before any source is tapped,
-- so the mana resolution cannot fit yet -- and the Forest would never be tapped
-- at all. payCost passes `lifeNeeded`, the least life any PAYABLE resolution
-- costs, so a cost the board can pay with mana is capped at zero life and the
-- loop is forced to tap for it.
spend :: Natural -> ManaCost -> Mana -> Maybe (Mana, Natural)
spend budget cost (Mana.MkMana units) =
  let attempt (demands, generic, life) = do
        Monad.guard (life <= budget)
        afterTyped <- spendDemands units demands
        left <- Monad.foldM takeAny afterTyped [1 .. generic]
        pure (Mana.MkMana left, life)
   in Maybe.listToMaybe (Maybe.mapMaybe attempt (resolutions cost))

-- CR 601.2g: if the total cost includes a mana payment, the player then has a
-- chance to activate mana abilities. Reached from an ability too, by CR 602.2b.
--
-- Returns whether it was paid; on failure nothing is spent, which is CR 601.2h's
-- bar on partial payments rather than mere tidiness. The prompts themselves are
-- NOT rolled back -- they live in the Program, outside the state -- so a failed
-- payment still asked its questions.
--
-- Failure is REACHABLE, and a source offering a colour choice is what makes it
-- so. canPay asks whether SOME sequence of choices pays the cost; this asks the
-- player to make them. A player who taps their only Birds of Paradise for green
-- cannot then pay {B}, and the engine must let them: choosing badly is a choice,
-- and second-guessing it here would be the engine playing the game.
--
-- One prompt per source tapped, against a shrinking candidate list, rather than
-- one prompt for a whole subset: a cost needing {G}{G} is two decisions, and the
-- second is made knowing the first.
--
-- The life budget only ever binds a cost NOTHING ANNOUNCED for: a cast (CR
-- 601.2b) and an activation (CR 602.2b) both run `announce` first, so the cost
-- arriving here holds no Phyrexian symbol and the budget is 0. What is left
-- under it is CR 118.13b/c, a cost paid during a resolution or for a special
-- action, where pawl still chooses (#373).
--
-- It is recomputed on EVERY pass rather than fixed at entry, because a tap can
-- change it: a Birds of Paradise tapped for blue takes the mana way to an
-- unannounced {G/P} off the board, and the cost is then payable only by CR
-- 107.4f's 2 life. Recomputing means pawl pays it, rather than failing the
-- payment the way the paragraph above lets a mis-tapped {B} fail -- the same MORE
-- PERMISSIVE posture, and reachable only where nothing announced (#373). Zero
-- when the cost is unpayable outright.
payCost :: PlayerId -> ManaCost -> Game Bool
payCost pid cost = do
  before <- State.get
  paid <- tapUntilPaid
  Monad.unless paid (State.put before)
  pure paid
  where
    tapUntilPaid = do
      gs <- State.get
      let budget = Maybe.fromMaybe 0 (lifeNeeded pid cost gs)
      case spend budget cost (Game.poolOf pid gs) of
        Just (left, life) -> do
          State.put (payLife pid life (setPool pid left gs))
          pure True
        Nothing -> case manaSources pid gs of
          [] -> pure False
          candidate : rest -> do
            oid <- chooseSource pid (candidate NonEmpty.:| rest) gs
            tapForMana oid
            tapUntilPaid

-- Which source to tap next.
--
-- Asked whenever there is more than one candidate, and elided ONLY when there is
-- exactly one -- where no choice exists to make. A deliberately blunt reading of
-- "eliding a prompt is legitimate only for indistinguishable options": it never
-- has to be right about what indistinguishable MEANS.
--
-- The obvious cheaper rule -- elide when every candidate is a copy of the same
-- card -- is unsound in this pool, and the counterexamples are ordinary. Two
-- Llanowar Elves are one card, but one may be equipped or enchanted, one may
-- carry +1/+1 counters, one may be borrowed until end of turn, and one may be
-- blocking (CR 506.4 does not remove a creature from combat for tapping).
-- `Game.cardOf` compares PRINTED identity and cannot see any of it, so that rule
-- would suppress exactly the prompts the invariant exists to force (#217).
--
-- FILTERED, NOT TRUSTED, the posture Combat.declareAttackers and
-- Cost.payComponents already take. That is not only hygiene -- tapForMana is a
-- no-op on an unknown or mana-less id, so honouring a bogus answer would leave
-- the state unchanged and loop forever.
chooseSource :: PlayerId -> NonEmpty.NonEmpty ObjectId -> GameState -> Game ObjectId
chooseSource pid candidates gs = case candidates of
  only NonEmpty.:| [] -> pure only
  _ -> do
    answer <- Game.choose (Prompt.ChooseManaSource (Decide.deciderFor pid gs) pid candidates)
    pure $
      if List.elem answer (NonEmpty.toList candidates)
        then answer
        else NonEmpty.head candidates

-- CR 107.4f's 2 life. The one place that number is written.
phyrexianLife :: Natural
phyrexianLife = 2

-- CR 107.4e's {2/X} half: the generic mana its OTHER way costs. The one place
-- that number is written, and it is fixed rather than carried for the reason
-- Pawl.Types.ManaSymbol.MonocoloredHybrid gives -- of CR 107.4's monocolored
-- hybrid symbols that constructor says only the five {2/W}..{2/G}, and every one
-- of those is a two.
monocoloredHybridGeneric :: Natural
monocoloredHybridGeneric = 2

-- CR 601.2b: a cost paid as a spell is cast has its Phyrexian and its hybrid
-- symbols announced -- "the player announces whether they intend to pay 2 life or
-- a corresponding colored mana cost for each of those symbols" for CR 107.4f's,
-- and "the player announces the nonhybrid equivalent cost they intend to pay" for
-- CR 107.4e's. CR 118.13a places both announcements as the controller PROPOSES
-- the spell or ability, NOT when the cost is paid; CR 602.2b sends an activated
-- ability's cost through the same rule.
--
-- Returns CR 601.2b's own phrase -- the "nonhybrid equivalent cost", a mana cost
-- with no Phyrexian and no monocolored hybrid symbol left in it -- and the life
-- the announcement committed. Pawl.Engine.Cost.announce turns that life into CR
-- 119.4's payment, so nothing below this function ever sees a mana symbol that
-- spends no mana.
--
-- CR 107.4e's COLOUR/COLOUR hybrid is the one "paid in multiple ways" symbol
-- still not announced here, so it rides through and is settled by
-- Pawl.Engine.Mana.spend's search at payment time (#729).
--
-- ONE SYMBOL AT A TIME, in printed order, each question asked knowing the answers
-- before it. What makes that sound is the OFFER: a route is offered only if the
-- whole cost is still payable after taking it, so an earlier answer may narrow a
-- later one's options but can never strand the payment -- CR 601.2b's last
-- sentence, arriving as a board rather than as a rule.
--
-- Measured through `total`, the caller's CR 601.2f totalling, because the cost
-- that decides whether a route is payable is the one that will actually be paid,
-- not the printed one. CR 601.2b comes first and 601.2f second, so the
-- ANNOUNCEMENT stays here; only the payability probe reaches forward. Get that
-- wrong and a cost reduction hides a route the player was entitled to and this
-- function elides the prompt, which is the engine choosing. That matters most for
-- CR 107.4e's monocolored hybrid, whose generic half IS the component CR 118.7a's
-- reductions come off: Flame Javelin's own ruling is that a generic cost
-- reduction applies to it only where the announced payment includes generic mana.
-- Every remaining announcement is COMPLETED before totalling (`completions`),
-- because CR 601.2f is defined over a nonhybrid equivalent cost: leaving the
-- tail's symbols unannounced would silently withhold reductions a later answer is
-- entitled to.
--
-- Measured against the BOARD and not the pool: canPayCommitting counts an
-- untapped source as the mana it could make (payableResolutions), so a Forest
-- still in play offers the mana route before anything is tapped. A player who
-- then taps it for something else fails the payment, which is CR 601.2h's
-- business and not this function's -- announcing is not producing.
--
-- FILTERED, NOT TRUSTED, the chooseSource posture.
announce :: PlayerId -> ObjectId -> (ManaCost -> ManaCost) -> ManaCost -> Game (ManaCost, Natural)
announce pid oid total (ManaCost.MkManaCost symbols) = go [] 0 symbols
  where
    -- "Payable" here means SOME completion of the remaining announcements pays
    -- it, which is what CR 601.2b's last sentence makes the question. Enumerated
    -- here rather than left to canPay's own `resolutions` so that each completion
    -- is an announcement-free cost by the time `total` sees it.
    --
    -- `done` is the reversed prefix already announced, `ways` the symbols this
    -- route would leave in place of the one being asked about, and `extra` the
    -- life it would commit on top of what is committed already.
    stillPayable done rest gs extra ways =
      let candidate (tail_, life) =
            canPayCommitting pid (extra + life) (total (ManaCost.MkManaCost (reverse done <> ways <> tail_))) gs
       in any candidate (completions rest)
    -- One symbol's announcement. Asked only where two routes are payable, and
    -- FILTERED, NOT TRUSTED where it is asked.
    --
    -- With NO payable route the cost is unpayable and there is nothing to
    -- announce: the payment fails (CR 118.6, CR 601.2h). `fallback` is returned
    -- only because this must return SOMETHING; it is not a choice, because there
    -- is no payable option for it to choose between.
    --
    -- UNREACHABLE from a caller that gated on payability first, and it is `total`
    -- above that makes that true rather than merely plausible: the gate and this
    -- offer measure payability through the same totalling, so no completion
    -- payable here can be one the gate refused. The gate is the STRICTER of the
    -- two for a monocolored hybrid, because it measures a cost whose {2/X} symbols
    -- CR 118.7a's reductions cannot see into (#730) -- which is the safe
    -- direction: it can only refuse a cast this function would have had an offer
    -- for. {X} used to be a wedge in that -- a gate at X=0 while this runs on the
    -- value the player named -- and BOTH callers now close it the same way, by
    -- re-asking their own payability predicate at the announced value before
    -- calling Cost.announce at all: Cast.castSpell (#417) and
    -- Activate.activateAbility (#544).
    choose :: (Eq a) => a -> [a] -> (NonEmpty.NonEmpty a -> Prompt.Prompt a) -> Game a
    choose fallback offers mkPrompt = case offers of
      [] -> pure fallback
      [only] -> pure only
      first : others -> do
        answer <- Game.choose (mkPrompt (first NonEmpty.:| others))
        pure (if List.elem answer offers then answer else first)
    go done committed remaining = case remaining of
      [] -> pure (ManaCost.MkManaCost (reverse done), committed)
      ManaSymbol.Phyrexian color : rest -> do
        gs <- State.get
        let asMana = ManaSymbol.OfType (ManaType.Colored color)
            offers =
              [PhyrexianPayment.PaysMana | stillPayable done rest gs committed [asMana]]
                <> [PhyrexianPayment.PaysLife | stillPayable done rest gs (committed + phyrexianLife) []]
        announced <-
          choose PhyrexianPayment.PaysMana offers $
            Prompt.AnnouncePhyrexianPayment (Decide.deciderFor pid gs) pid oid color
        case announced of
          PhyrexianPayment.PaysMana -> go (asMana : done) committed rest
          PhyrexianPayment.PaysLife -> go done (committed + phyrexianLife) rest
      -- CR 107.4e: "a monocolored hybrid symbol such as {2/B} can be paid with
      -- either one black mana or two mana of any type." Neither way commits life,
      -- so both are measured at the life already committed; what they differ in is
      -- the NUMBER of mana demanded, which is what makes this a real choice rather
      -- than a relabelling -- and what makes the {2} route reducible.
      ManaSymbol.MonocoloredHybrid manaType : rest -> do
        gs <- State.get
        let asTyped = ManaSymbol.OfType manaType
            asGeneric = ManaSymbol.Generic monocoloredHybridGeneric
            offers =
              [HybridPayment.PaysTyped | stillPayable done rest gs committed [asTyped]]
                <> [HybridPayment.PaysGeneric | stillPayable done rest gs committed [asGeneric]]
        announced <-
          choose HybridPayment.PaysTyped offers $
            Prompt.AnnounceHybridPayment (Decide.deciderFor pid gs) pid oid manaType
        case announced of
          HybridPayment.PaysTyped -> go (asTyped : done) committed rest
          HybridPayment.PaysGeneric -> go (asGeneric : done) committed rest
      other : rest -> go (other : done) committed rest

-- Every way the announcements of a cost's UNANNOUNCED tail could go, as the
-- symbols that leaves and the life those choices commit -- CR 601.2b's own
-- "nonhybrid equivalent cost", one entry per combination. Exactly the product
-- `resolutions` takes over CR 107.4e's and CR 107.4f's ways, lifted to the SYMBOL
-- level so that a caller can hand each completion to CR 601.2f's totalling before
-- asking whether it is payable.
--
-- One entry for a tail with nothing to announce, so this is the identity case for
-- every cost `announce` leaves untouched, and 2^(number of Phyrexian and
-- monocolored hybrid symbols) otherwise. Other symbols ride through in place --
-- CR 107.4e's colour/colour hybrid among them, since it is not announced (#729)
-- -- which is what keeps a completion a cost and not merely a set of choices.
completions :: [ManaSymbol] -> [([ManaSymbol], Natural)]
completions symbols = case symbols of
  [] -> [([], 0)]
  ManaSymbol.Phyrexian color : rest ->
    let asMana = ManaSymbol.OfType (ManaType.Colored color)
     in [(asMana : tail_, life) | (tail_, life) <- completions rest]
          <> [(tail_, life + phyrexianLife) | (tail_, life) <- completions rest]
  -- CR 107.4e's two ways, neither of which commits life. The {2} is a Generic
  -- symbol and not a demand for two mana of the stated type: CR 107.4e says "two
  -- mana of any type", and CR 107.4b says a numerical symbol represents generic
  -- mana, which "can be paid with any type of mana" -- the same permission, which
  -- is why the substitution loses nothing.
  ManaSymbol.MonocoloredHybrid manaType : rest ->
    [(ManaSymbol.OfType manaType : tail_, life) | (tail_, life) <- completions rest]
      <> [(ManaSymbol.Generic monocoloredHybridGeneric : tail_, life) | (tail_, life) <- completions rest]
  other : rest -> [(other : tail_, life) | (tail_, life) <- completions rest]

-- CR 118.3: can this cost be paid at all? Pure, because Action.legalActions asks
-- it while merely ENUMERATING actions, where prompting would be absurd -- so it
-- cannot simply walk tapForMana, which now asks a question.
--
-- A cost is payable exactly when SOME resolution of it is: `resolutions` has
-- already turned CR 107.4e's {2/B} and CR 107.4f's {G/P} into their nonhybrid
-- equivalents, so this asks nothing about hybrid-ness. payableResolutions is
-- where the per-resolution test lives.
canPay :: PlayerId -> ManaCost -> GameState -> Bool
canPay pid = canPayCommitting pid 0

-- The same question asked mid-announcement, where CR 118.13a's choices -- both
-- those already made and those a `completions` entry is standing in for -- have
-- committed `committed` life that CR 119.4's floor must still admit alongside
-- whatever the rest of the cost costs. Zero everywhere else, which is what
-- `canPay` is.
canPayCommitting :: PlayerId -> Natural -> ManaCost -> GameState -> Bool
canPayCommitting pid committed cost gs = not (null (payableResolutions pid committed cost gs))

-- One untapped source's contribution to the supply side, as the OPTIONS it
-- offers: one option per yield, and each option is that yield read as one supply
-- per mana it adds. payableResolutions picks exactly ONE option per source.
--
-- One and not several, because CR 106.12 makes "tap for mana" an activation of a
-- mana ability including {T} in its activation cost, and CR 107.5 bars tapping an
-- already-tapped permanent to pay that cost. Every mana ability in this pool
-- costs exactly {T} (manaSourcesGiven leans on the same fact for CR 302.6), so an
-- untapped source is tapped for mana at most once and adds what exactly one
-- activation adds. Mixing its yields -- the first mana from one, the second from
-- another -- describes a board no sequence of activations reaches.
--
-- The COLLAPSE is the one place several yields become one option, and it is
-- exact rather than a shortcut. Where every yield adds at most one mana, each
-- option is at most one supply, and one supply is already "one mana that could be
-- any of these types": the union over the yields is precisely what an Urborg'd
-- Mountain or a Birds of Paradise puts on the table. A yield adding NO mana is
-- dropped by the same union, which changes no answer, since both of
-- payableResolutions' board clauses only ever grow as supplies are added.
--
-- It is also what keeps the search below small: Birds of Paradise's five yields
-- collapse to one option, so five Birds are one board rather than 5^5. A source
-- with ONE yield needs no collapse, so Sol Ring takes the other branch.
--
-- The TAGS mix by union, and there too the union is exact: manaYieldsOfGiven
-- stamps one tag set on every unit of every yield of a source, because CR 106.3
-- makes them all facts about that one source.
sourceOptions :: [Mana] -> [[Supply]]
sourceOptions yields =
  let unitLists = fmap unitsOf yields
   in if all (\units -> length units <= 1) unitLists
        then [collapsed (concat unitLists)]
        else fmap (fmap supplyOf) unitLists
  where
    collapsed units =
      if null units
        then []
        else
          [ MkSupply
              { supplyTypes = Set.fromList (fmap ManaUnit.manaType units),
                supplyTags = Set.unions (fmap ManaUnit.tags units)
              }
          ]

-- The resolutions of `cost` this player could actually pay right now, in
-- `resolutions`' order -- so the head costs the least life of any of them, which
-- is what lifeNeeded reads. NOT the resolution `spend` will take: `spend` walks
-- the same list against the POOL alone, where a resolution this one admits on the
-- strength of an untapped source may not yet fit. What the two agree on is the
-- life, because the budget is what carries between them.
--
-- The mana part must not be simulated greedily: once a source can produce more
-- than one type, WHICH type each makes decides affordability -- a Forest and a
-- Birds of Paradise pay {G}{B}, but only if the Birds makes black.
--
-- So it is an assignment question, and it is answered exactly. Model each
-- available mana as a SUPPLY carrying the set of types it could be and the
-- production-time tags it would carry (CR 106.3). An untapped source is a CHOICE
-- among such supplies, one option per yield it could add (sourceOptions above),
-- because CR 107.5 lets it be tapped for mana once. Each typed symbol of the cost
-- is a DEMAND, which `serves` matches against a supply; generic symbols demand a
-- count and nothing more.
--
-- A BOARD is the pool plus one option taken from every source -- the mana the
-- player would actually have in front of them after tapping everything. A
-- RESOLVED cost is payable when clause 3 holds and SOME board satisfies clauses
-- 1 and 2:
--
--   1. every typed demand can be met at once -- a matching of demands into
--      supplies that saturates the demand side;
--   2. enough supplies are left over for the generic part. Every full typed
--      matching consumes exactly one supply per typed symbol, so the leftover
--      count does not depend on WHICH matching, and this is a plain comparison;
--      and
--   3. CR 119.4's floor admits the life -- this resolution's own, PLUS whatever
--      an announcement in progress has already committed (`committed`, zero for
--      every caller but `announce`). The clause that reads the PLAYER
--      rather than the board, and the only one a Phyrexian-free cost can never
--      fail: every resolution of such a cost costs 0 life, and CR 119.4b lets
--      anyone pay that -- see canPayLife.
--
-- Clauses 1 and 2 are asked of ONE board at a time, and that is the whole of
-- #450's fix. Asking them of a per-source union instead lets one source's first
-- mana come from one yield and its second from another, which is not a board any
-- sequence of activations reaches: Ashaya, Soul of the Wild makes a Palladium Myr
-- ({T}: Add {C}{C}) a Forest as well, so the union read it as able to make two
-- mana one of which is green.
--
-- The SEARCH over boards is the product of the sources' options, exponential in
-- the number of sources offering more than one -- the reason sourceOptions'
-- collapse matters. After it, a source offers more than one option only if it has
-- several yields AND one of them adds more than one mana, which in this pool
-- takes a multi-mana ability on a permanent some effect has ALSO given a basic
-- land type (Palladium Myr under Ashaya), so the ordinary board is one board.
-- `any` short-circuits, so a payable cost stops at the first board that pays it;
-- an unpayable one walks every board, and neither a domination prune nor a cheap
-- necessary-condition prefilter is implemented (#595).
--
-- Clause 1 is Hall's condition: a saturating matching exists iff no set of
-- demands outruns the supplies that could serve it. Checked directly rather than
-- by running a matching algorithm, so there is no gap between what this says and
-- what it does. Enumerated over subsets of the DISTINCT demands, which is enough
-- for every subset: equal demands have the same supplies, so for any violating
-- set take every demand equal to one of its members -- no smaller, same
-- supplies, still violating. At most 2^(distinct typed symbols) subsets. Over
-- DEMANDS rather than over TYPES because CR 107.4h's {S} demands every type (CR
-- 106.1b) narrowed by a production tag, so two demands can name the same types
-- and still be served by different supplies.
--
-- Clauses 1 and 2 count one supply per typed demand, which is exactly why CR
-- 107.4e's {2/B} is resolved AWAY before either is asked: neither can be fooled
-- into charging {2/B} a single mana. CR 107.4f's {G/P} rides on the same
-- enumeration: its life way is a resolution with one fewer demand, so neither
-- has to learn about a symbol that consumes no supply at all.
payableResolutions :: PlayerId -> Natural -> ManaCost -> GameState -> [([Demand], Natural, Natural)]
payableResolutions pid committed cost gs =
  let Mana.MkMana units = Game.poolOf pid gs
      -- The SAME board manaSources is judged against serves the per-source
      -- yields too, rather than a fresh projection per source on top of the sweep
      -- (#200); see manaSources above for the hoist and its snapshot argument.
      grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
      pooled = fmap supplyOf units
      options = fmap (\oid -> sourceOptions (manaYieldsOfGiven pcs oid gs)) (manaSourcesGiven grants pcs pid gs)
      -- One option taken from each source, appended to the pool: `sequenceA` over
      -- the list applicative is that product, and it is [[]] -- one board, the
      -- pool alone -- when the player controls no source at all.
      boards = fmap (\taken -> pooled <> concat taken) (sequenceA options)
      payable (demands, generic, life) =
        let subsets = List.subsequences (List.nub demands)
            fits supplies =
              let -- "The supplies that could serve this set of demands" and "the
                  -- demands in it", the two sides of Hall's condition for one
                  -- subset.
                  couldServe wanted = length (filter (\supply -> any (serves supply) wanted) supplies)
                  demandedIn wanted = length (filter (`elem` wanted) demands)
                  hallHolds wanted = demandedIn wanted <= couldServe wanted
               in Natural.length supplies >= Natural.length demands + generic
                    && all hallHolds subsets
         in canPayLife pid (committed + life) gs && any fits boards
   in filter payable (resolutions cost)

-- The least life any payable resolution of this cost costs, or Nothing when none
-- is payable. `resolutions` is sorted by life ascending and payableResolutions
-- keeps that order, so the head is the minimum.
--
-- This is the budget payCost pays under. A cast or an activation has already
-- announced its Phyrexian symbols away (`announce`, CR 118.13a), so this answers
-- 0 for them; it decides anything only where nothing announced (#373).
lifeNeeded :: PlayerId -> ManaCost -> GameState -> Maybe Natural
lifeNeeded pid cost gs = case payableResolutions pid 0 cost gs of
  (_, _, life) : _ -> Just life
  [] -> Nothing

-- CR 119.4: a player may pay an amount of life greater than 0 only if their life
-- total is at least that amount.
--
-- Lives here rather than in Pawl.Engine.Cost so that CR 107.4f's Phyrexian symbol
-- and CR 119.4's own PayLife component share one reading of the rule;
-- Pawl.Engine.Cost imports this module, so the dependency only goes one way.
--
-- CR 119.4b is answered BEFORE the lookup, not by the `>=` that would usually
-- absorb it: players can ALWAYS pay 0 life, whatever their total and even where
-- an effect says they can't pay life. So a player the map does not hold must not
-- turn a zero payment into an unpayable one -- and every resolution of a cost
-- with no Phyrexian symbol is a zero payment, so this is also what keeps
-- payableResolutions' life clause unable to change any answer such a cost used
-- to give.
canPayLife :: PlayerId -> Natural -> GameState -> Bool
canPayLife pid n gs =
  n == 0 || case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Player.life player >= toInteger n

-- CR 119.4: the payment is subtracted from the player's life total. A direct
-- subtraction, and the CR 704.5a state-based action that may follow is the
-- existing one in Pawl.Engine.Sba -- paying to exactly 0 is a legal payment, not
-- a barred one.
payLife :: PlayerId -> Natural -> GameState -> GameState
payLife pid n gs =
  gs
    { GameState.players =
        Map.adjust (\p -> p {Player.life = Player.life p - toInteger n}) pid (GameState.players gs)
    }
