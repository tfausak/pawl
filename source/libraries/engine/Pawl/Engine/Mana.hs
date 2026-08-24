module Pawl.Engine.Mana where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Claim as Claim
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.ManaFilter as ManaFilter
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.Activations as Activations
import Pawl.Types.Claim (Claim)
import qualified Pawl.Types.Color as Color
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.HybridPayment as HybridPayment
import qualified Pawl.Types.HybridPhyrexian as HybridPhyrexian
import qualified Pawl.Types.Keyword as Keyword
import Pawl.Types.Mana (Mana)
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaAddition as ManaAddition
import Pawl.Types.ManaCost (ManaCost)
import qualified Pawl.Types.ManaCost as ManaCost
import Pawl.Types.ManaOption (ManaOption)
import qualified Pawl.Types.ManaOption as ManaOption
import Pawl.Types.ManaProduction (ManaProduction)
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRestriction as ManaRestriction
import qualified Pawl.Types.ManaRetention as ManaRetention
import Pawl.Types.ManaSpending (ManaSpending)
import qualified Pawl.Types.ManaSpending as ManaSpending
import Pawl.Types.ManaSymbol (ManaSymbol)
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import Pawl.Types.ManaType (ManaType)
import qualified Pawl.Types.ManaType as ManaType
import Pawl.Types.ManaUnit (ManaUnit)
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PhaseSelector (PhaseSelector)
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.SpendManaAsThough as SpendManaAsThough
import Pawl.Types.Subtype (Subtype)
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- | Asked of a mana ability's OWN activation cost: HOW MANY TIMES may this
-- player activate it, for the ability on this object? CR 602.2b is why the
-- supply model has to ask -- activating a mana ability means paying its cost --
-- so a route answered 0 is neither an offer nor a supply.
--
-- A COUNT and not a yes-or-no, because a cost with nothing in it that CR 107.5
-- or CR 701.21a spends only once can be paid again: Ashnod's Altar beside two
-- creatures is two activations, and counting it once reads a cost only two could
-- pay as unpayable (#1128).
--
-- EVERY restriction on the activation rides here, not just CR 118.3's
-- payability: CR 605.3b keeps a mana ability off the stack, so there is no other
-- window to apply one in. That is why the ability's printed "activate only ..."
-- rider (CR 602.5) is an argument -- CR 605.1 keeps a timing rider from making an
-- ability any less a mana ability, so the rider has to be asked HERE or nowhere.
-- It takes the pre-projected board because CR 302.6's reads want it (#200).
--
-- And WHAT ONE ACTIVATION SPENDS besides, because the count is a fact about one
-- source asked alone and the supply model has to add several of them up: two
-- sources that each sacrifice a creature both answer 1 beside one creature, and
-- two that each pay 3 life both answer 2 at 6 life. The claims and the life are
-- what stop either pair being counted twice over (payableResolutionsGiven, #1126).
-- Unscaled -- one activation's, whatever the count.
--
-- A CALLBACK rather than a call, for Pawl.Engine.Count.ViewOf's reason: those
-- are Pawl.Engine.Cost's questions, and that module imports this one.
-- Pawl.Engine.Cost.manaActivations is the only answer the engine passes.
--
-- The Measure is WHICH of the two questions below is being asked, and it is an
-- argument rather than two capacities because only Pawl.Engine.Cost can tell
-- them apart: the difference is whether the route's own mana part is asked about
-- (`supplyCapacity`), and this module cannot reach the function that asks.
type Capacity = Measure -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> Cost Keyword.Keyword -> [ActivationRestriction.ActivationRestriction] -> GameState -> Activations.Activations

-- WHICH reader is asking a Capacity: CR 605.3a's windows, which offer a route
-- only when its whole cost is payable right now, or the supply walk, which
-- models the route's own mana as a DEMAND instead and so must not ask.
--
-- Every site that APPLIES a capacity passes ForOffer; `supplyCapacity` below is
-- the one thing that substitutes ForSupply, which is what keeps the supply
-- model a property of the walk rather than of who called it.
data Measure
  = ForOffer
  | ForSupply
  deriving (Bounded, Enum, Eq, Ord, Show)

-- The same question asked by the SUPPLY WALK rather than at the offer: what one
-- activation of this route puts on a board, with the mana CR 602.2b makes that
-- activation PAY left for manaSuppliesGiven to carry alongside it.
--
-- A route whose own cost holds mana is a supply that also CONSUMES a demand.
-- Transmogrant Altar's "{B}, {T}, Sacrifice a creature: Add {C}{C}{C}" puts
-- three colorless on the supply side and its own {B} on the demand side, and the
-- net is two mana. Counting the yield gross would be an OVERSTATEMENT -- a
-- supply too large offers a cast that then cannot be paid -- and counting such a
-- route as nothing at all was the understatement this replaced (#1120).
--
-- WHAT THIS DOES is ask `capacity` the ForSupply question, which is the whole of
-- the difference: Pawl.Engine.Cost.manaActivationsGiven then skips its CR 118.3
-- read of the route's OWN mana part and answers about everything else -- the
-- components, CR 302.6's settle, CR 701.35a's detain, CR 602.5's rider. The
-- demand itself is read off the route's printed cost, one level up.
--
-- Skipping that read is also what makes the recursion terminate, and statically.
-- The CR 118.3 read asks canPayCommitting whether its own route's mana part is
-- payable, and the walk under THAT question runs on this capacity again -- so a
-- single Altar would ask about itself forever. ForSupply cuts the question, so
-- the nesting bottoms out at once whatever is on the board and whatever the pool
-- prints.
--
-- Nothing is CR 118.6's unpayable cost and gets 0, which is the answer the offer
-- capacity would have given anyway.
--
-- ONE activation, and the cost it hands down is the PRINTED one, which is what
-- keeps that true: Pawl.Engine.Cost.repeatsOf reads a non-empty mana part and
-- answers 1, repeating a mana part being a way to spend mana that function has
-- not measured.
--
-- APPLIED BY THE WALK, not by its callers: manaSuppliesGiven wraps whatever
-- capacity it is handed, and canPayCommitting and payableResolutions wrap theirs
-- before building a source list, so every entry point measures the same thing
-- whoever called it. `capacity` is Pawl.Engine.Cost.manaActivations at every one.
-- The incoming Measure is DISCARDED for that reason: a caller cannot ask the
-- supply walk for the offer's answer.
supplyCapacity :: Capacity -> Capacity
supplyCapacity capacity _measure pcs pid oid cost restrictions gs = case Cost.mana cost of
  Nothing -> noActivations
  Just _ -> capacity ForSupply pcs pid oid cost restrictions gs

-- No activation at all: the answer a Capacity gives for a route this player
-- cannot take.
noActivations :: Activations.Activations
noActivations = Activations.MkActivations {Activations.times = 0, Activations.claims = [], Activations.life = 0}

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
--
-- CR 106.11 is a rewrite and this is where it happens: an effect that would add
-- mana represented by a snow mana symbol adds colorless mana instead, one per
-- symbol. ONE option and not a choice, so nothing here prompts -- and no snow
-- tag, because CR 107.4h reads the SOURCE (productionTagsGiven) and never the
-- symbol the effect was written with.
--
-- CR 607.2d's linked pair is resolved by reading Object.chosenColor off the
-- SOURCE, which is why this takes one: an ability referring to "the chosen
-- color" means the colour its own object was told to choose. One option, so it
-- offers no choice; none at all when nothing has been chosen, which for
-- Coldsteel Heart cannot happen on a permanent that entered (CR 614.1c) and is
-- not a colour for the engine to invent when it does.
producedTypes :: ObjectId -> GameState -> ManaProduction -> [ManaType]
producedTypes oid gs production = case production of
  ManaProduction.OfType manaType -> [manaType]
  ManaProduction.AnyColor ->
    fmap
      ManaType.Colored
      [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green]
  ManaProduction.Chosen ->
    fmap ManaType.Colored (Maybe.maybeToList (Game.lookupObject oid gs >>= Object.chosenColor))
  ManaProduction.SnowSymbol -> [ManaType.Colorless]

-- Every ROUTE by which this object could be activated for mana, as the mana ONE
-- activation of it adds: its intrinsic subtype mana (CR 305.6), one route per
-- basic land type, PLUS one route per MODE (CR 700.2) of every projected
-- activated ability that is a mana ability (CR 605.1a), resolved inline at
-- payment and never on the stack (CR 605.3b).
--
-- CR 106.12 narrows "tap for mana" to a mana ability that includes {T} in its
-- activation cost, and this function does not filter on that, deliberately: a
-- route is every way the object could be activated for mana, {T} or not. The
-- narrowing is applied where the tap rules are, by the Capacity reading each
-- route's own cost (manaSourcesGiven).
--
-- The nesting is the whole point. The OUTER list is the options -- which ability
-- of this permanent, and which of its modes -- and each carries the COST CR
-- 602.2b makes that option's activation pay, together with the ability's printed
-- "activate only ..." rider (CR 602.5): CR 605.3b gives a mana ability no stack
-- window to be gated in, so the rider has to travel with the route to the
-- Capacity, which is where both of CR 605.3a's windows ask. CR 305.6's intrinsic
-- ability is printed on no card and so carries none. The INNER list is that one
-- activation's YIELD, its AddMana effects in printed order (CR 608.2c). Sol
-- Ring's "{T}: Add {C}{C}" is one option adding two mana; an Urborg'd Mountain
-- is two options of one mana each.
--
-- Read through the projection (abilitiesOf), so Humility (layer 6) strips a
-- creature's mana ability too -- and so does CR 305.7 at layer 4, which is what
-- swaps a Blood Moon'd Reliquary Tower's printed "{T}: Add {C}" for the
-- Mountain's {R} rather than adding to it.
--
-- One route per SELECTION (Modal.selectionEffects), not per mode: CR 700.2's
-- selection is what a player actually makes, so a choose-two ability's route is
-- the CONCATENATION of a legal pair of modes' yields and its options are the
-- pairs. For "choose exactly one", which is every printed mana ability, that is
-- one route per mode. A mode adding no mana contributes an empty route, which
-- the supply model ignores.
--
-- Takes a PRE-PROJECTED board, because every caller asks this of every source a
-- player controls, and each of the two projection reads here was a fresh gather
-- per source (#200).
--
-- Not implemented: a route that reads its clauses' CR 701.46a-shaped gates.
-- Modal.selectionEffects flattens every clause of a mode whatever
-- Clause.condition says, so an ability written as "Add {C}. If ..., instead add
-- one mana of any color" would route both manas at once, which is weaker than
-- printed. Gemstone Caverns says that sentence as two abilities whose
-- ActivatedAbility.conditions are complements instead -- a gate abilitiesGiven
-- does apply, with the board in hand (#1924).
manaRoutesOfGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [(Cost Keyword.Keyword, [ActivationRestriction.ActivationRestriction], [ManaAddition.ManaAddition])]
manaRoutesOfGiven pcs oid gs =
  let fromSubtypes =
        fmap
          (\manaType -> (intrinsicManaCost, [], [intrinsicManaAddition manaType]))
          (Maybe.mapMaybe subtypeMana (Set.toList (Projection.subtypesGiven pcs oid gs)))
      selectionRoutes ability =
        fmap
          (\effects -> (ActivatedAbility.cost ability, ActivatedAbility.restrictions ability, Maybe.mapMaybe ManaAbility.manaProduced effects))
          (Modal.selectionEffects (ActivatedAbility.modal ability))
      fromAbilities = concatMap selectionRoutes (filter ManaAbility.isManaAbility (Projection.abilitiesGiven pcs oid gs))
   in fromSubtypes <> fromAbilities

-- CR 305.6's intrinsic mana ability is "{T}: Add [the type]", printed on no card
-- and so minted here rather than read off one: {T} and nothing else. What makes
-- the cost worth carrying at all is that a PRINTED mana ability's need not be
-- (Mana Confluence's is "{T}, Pay 1 life"), and CR 602.2b makes an activation
-- pay whichever it has.
intrinsicManaCost :: Cost Keyword.Keyword
intrinsicManaCost =
  Cost.MkCost
    { Cost.mana = Just (ManaCost.MkManaCost []),
      Cost.components = [CostComponent.TapThis]
    }

-- The instruction the ability above gives: CR 305.6 writes it out as "{T}: Add
-- [mana symbol]" and nothing more, so every field beyond the type takes the
-- value an unwritten clause means -- CR 109.5's "you", CR 106.4's ordinary loss
-- as the step ends, and no CR 106.6 restriction.
intrinsicManaAddition :: ManaType -> ManaAddition.ManaAddition
intrinsicManaAddition manaType =
  ManaAddition.MkManaAddition
    { ManaAddition.player = PlayerRef.Relative PlayerRelation.You,
      ManaAddition.production = ManaProduction.OfType manaType,
      ManaAddition.retention = ManaRetention.Ordinary,
      ManaAddition.restriction = Nothing
    }

-- What tapping this object for mana could actually put in a pool: every route
-- above with each ManaProduction resolved to a concrete type, so one entry per
-- (route, colour choice) pair. `traverse` over the list applicative is that
-- product -- Birds of Paradise's one route becomes CR 105.4's five one-mana
-- yields.
--
-- A Mana rather than a list of types because that is what a yield IS: some mana,
-- headed for a pool (CR 106.4). Pawl.Engine.Cost.tapForMana adds the chosen one
-- whole.
--
-- Deduplicated by the WHOLE yield, and by it ALONE -- which is what separates
-- this from manaOptionsOf below, where the cost rides along. The supply model
-- (sourceOptions) is the reader that wants the yields on their own: what a source
-- could put in a pool is a question about mana, not about what it charges.
-- Deliberately order-sensitive: {R} then {B} and {B} then {R} stay two options,
-- which can only ever ASK where a set-valued dedup would not.
manaYieldsOf :: ObjectId -> GameState -> [Mana]
manaYieldsOf = manaYieldsOfGiven Map.empty

-- The same yields against a pre-projected board, which is manaRoutesOfGiven's
-- argument and carries its reason (#200).
manaYieldsOfGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [Mana]
manaYieldsOfGiven pcs oid gs = List.nub (fmap ManaOption.yield (manaOptionsOfGiven pcs oid gs))

-- The same yields narrowed to the ones this player could actually get, each with
-- HOW MANY TIMES they could get it, WHAT ONE of those activations spends, and
-- the MANA that activation itself pays (CR 602.2b): an activation answered 0 --
-- CR 118.3's unpayable cost, CR 302.6's sick creature -- yields nothing, so it is
-- no supply (payableResolutionsGiven) and no offer
-- (Pawl.Engine.Cost.tapForMana), and one answered 2 is two activations' worth of
-- mana (#1128).
--
-- The mana part is the route's PRINTED one, carried out whole rather than
-- measured here, because the board is what has to serve it: the {B} Transmogrant
-- Altar eats is a demand alongside the cost being paid, and only
-- payableResolutionsGiven sees both at once. Empty for every other route in the
-- pool, which is what makes it free supply.
--
-- The count rides on the YIELD rather than the route because that is what the
-- supply model consumes (sourceOptions). Two routes with the same yield collapse
-- to the LARGER count and not to their sum: they are separate activations, so
-- the sum would be exact, but no card in the pool prints two, and understating
-- supply only ever refuses a cost the payment loop could have paid -- where
-- overstating it offers a cast that cannot be paid. What survives the collapse is
-- that one route's whole answer, claims, life and mana part included, so the
-- count and what it spends describe one activation of one route rather than a
-- blend of two. A TIE is broken toward the mana-FREE route, the same direction:
-- two routes alike in count and yield but not in what they charge are one option
-- either way, and the one charging nothing burdens the board with no demand. No
-- printing in `data/cards/` puts two routes on one yield at all.
--
-- Separate from manaYieldsOf above rather than replacing it, and CR 106.7 is why
-- rather than tidiness: "the type of mana a permanent could produce" is defined
-- to IGNORE whether the ability's costs could be paid, so manaTypesOf has to go
-- on answering the ungated question.
manaSuppliesGiven :: Capacity -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> GameState -> [(Activations.Activations, Mana, ManaCost)]
manaSuppliesGiven capacity pcs pid oid gs =
  let -- Applied HERE and not left to the caller: this is the one reader of a
      -- route's yield on the supply side, so wrapping it once is what makes the
      -- model a property of the walk rather than of who called it. Idempotent,
      -- forcing ForSupply twice being forcing it once, so a caller that has
      -- already wrapped its own copy (to build the matching source list) loses
      -- nothing.
      supply = supplyCapacity capacity
      measured option =
        ( supply ForOffer pcs pid oid (ManaOption.cost option) (ManaOption.restrictions option) gs,
          ManaOption.yield option,
          -- CR 118.6's Nothing never survives the filter below, supplyCapacity
          -- answering 0 for it, so the empty stand-in is unreachable rather than
          -- a claim that such a route costs nothing.
          Maybe.fromMaybe (ManaCost.MkManaCost []) (Cost.mana (ManaOption.cost option))
        )
      counted = fmap measured (manaOptionsOfGiven pcs oid gs)
      available = filter (\(activations, _, _) -> Activations.times activations > 0) counted
      yieldOf (_, yield, _) = yield
      -- Ordered so `maximumBy` prefers the larger count, and a mana-free route
      -- over a mana-eating one at equal counts.
      rankOf (activations, _, manaCost) = (Activations.times activations, null (ManaCost.unwrap manaCost))
   in fmap
        (\yield -> List.maximumBy (Ord.comparing rankOf) (filter ((==) yield . yieldOf) available))
        (List.nub (fmap yieldOf available))

-- Every way this object could be tapped for mana, as the COST CR 602.2b makes
-- that activation pay paired with the mana it adds -- one entry per (route,
-- colour choice) pair. `traverse` over the list applicative is that product:
-- Birds of Paradise's one route becomes CR 105.4's five one-mana options.
--
-- Deduplicated by the WHOLE option. Two routes producing identical mana for an
-- identical cost (an Urborg'd Swamp is a Swamp twice over) are indistinguishable
-- options, so collapsing them elides a prompt with no content; two that charge
-- differently are not, and survive as two -- which is why what survives here is
-- what Pawl.Engine.Cost.chooseManaYield offers whole.
manaOptionsOf :: ObjectId -> GameState -> [ManaOption]
manaOptionsOf = manaOptionsOfGiven Map.empty

-- The same options against a pre-projected board (#200).
manaOptionsOfGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [ManaOption]
manaOptionsOfGiven pcs oid gs =
  let tags = productionTagsGiven pcs oid gs
      -- CR 106.6, stamped from the instruction that adds the unit: the
      -- restriction is the addition's (CR 106.6a), so every unit one AddMana
      -- produces carries it and a route mixing a restricted addition with an
      -- unrestricted one yields units that differ. Pawl.Engine.Cost.payMana is
      -- what then refuses to spend it on the wrong thing (spendableFor), and
      -- payableResolutionsGiven is what keeps the offer in step with that.
      --
      -- Not implemented: the retention the same instruction may carry
      -- (Pawl.Types.ManaRetention). This is CR 605.3b's inline payment, and
      -- Ordinary is stamped here whatever the addition says, so a mana ability
      -- that said its mana is kept would be paid Ordinary while the same clause
      -- on a stack-using ability works (#1808). Exact for data/cards/, where
      -- every retaining printing is a static or a trigger.
      unitFor addition manaType =
        ManaUnit.MkManaUnit
          { ManaUnit.manaType = manaType,
            ManaUnit.tags = tags,
            ManaUnit.retention = ManaRetention.Ordinary,
            ManaUnit.restriction = ManaAddition.restriction addition
          }
      expand (cost, restrictions, additions) =
        fmap
          (\units -> ManaOption.MkManaOption {ManaOption.cost = cost, ManaOption.restrictions = restrictions, ManaOption.yield = Mana.MkMana units})
          (traverse (\addition -> fmap (unitFor addition) (producedTypes oid gs (ManaAddition.production addition))) additions)
   in List.nub (concatMap expand (manaRoutesOfGiven pcs oid gs))

-- The production-time tags (Pawl.Types.ProductionTag) every mana this object
-- adds will carry. THE one place they are decided; manaOptionsOfGiven just above
-- is where the payment path stamps them onto a unit, and Pawl.Engine.Resolve's
-- Effect.AddMana arm is where a resolving ability's own producer does (CR
-- 605.1b). They are captured rather than looked up later because the mana
-- outlives the source (Pawl.Types.ManaUnit).
--
-- CR 106.3 is what makes reading the SOURCE the right question: mana produced by
-- an ability has that ability's source (CR 113.7) -- the permanent being tapped
-- on the payment path, and the object a resolving ability came from on the other.
-- CR 107.4h then asks whether that source is a snow one, and the rules define no
-- separate term for it: a snow source is a source that is snow, which for a
-- permanent is CR 205.4g's supertype and nothing else.
--
-- CR 205.4g is PERMANENT-scoped and CR 106.3's first clause is wider, so this
-- read would be too narrow for a source that is not a permanent. Nothing reaches
-- it with one. Pawl.Engine.Cost.tapForMana and payableResolutions take their oid
-- from manaSourcesGiven, which filters the battlefield; Pawl.Engine.Resolve's
-- Effect.AddMana arm takes the resolving ability's source, which is a permanent
-- for every producer in the pool and need not be one in general.
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
-- makes Sol Ring read as a choice between two singles, which Pawl.ManaSpec's
-- solRingSpec is the standing proof against.
manaTypesOf :: ObjectId -> GameState -> [ManaType]
manaTypesOf oid gs = List.nub (concatMap typesOf (manaYieldsOf oid gs))

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
-- action, and which mana it takes belongs to the action.
--
-- TWO CARRIERS, and a unit either of them keeps is kept. The CR 613.11
-- player-axis one is asked per player and then per unit, through a typed
-- question (PlayerEffect.keepsUnspentMana) that never reveals which effect
-- answered it -- per unit because a card may name only some of the mana:
-- Upwelling keeps every type, Omnath, Locus of Mana only green. The per-player
-- question is asked ONCE and its predicate applied to that player's units,
-- which is the shape keepsUnspentMana's argument order is built for. The other
-- carrier is the UNIT itself (Pawl.Types.ManaRetention), which is where a
-- clause naming the mana one ability just added has to live -- Shizuko, Caller
-- of Autumn's "they don't lose THIS mana" says different things about two manas
-- of one pool, so no widening of the player-axis filter can express it.
--
-- A DISJUNCTION over the two, for keepsUnspentMana's own reason: two retention
-- effects that name different mana are not in conflict, and CR 613.11's
-- timestamp order has nothing to order here.
--
-- A player left with nothing is DROPPED from the map rather than left holding an
-- empty pool: absent already means an empty pool (Game.poolOf), so keeping the
-- key would give the same state two spellings.
--
-- `gs` is the state as the step ends, so the effect is read at that moment -- an
-- Upwelling that left the battlefield during the step is simply not there.
emptyManaPools :: GameState -> GameState
emptyManaPools gs =
  let keptByUnit unit = ManaUnit.retention unit /= ManaRetention.Ordinary
      keeps pid unit = PlayerEffect.keepsUnspentMana pid gs unit || keptByUnit unit
      retain pid pool = case filter (keeps pid) (Mana.unwrap pool) of
        [] -> Nothing
        kept -> Just (Mana.MkMana kept)
   in gs {GameState.manaPool = Map.mapMaybeWithKey retain (GameState.manaPool gs)}

-- CR 514.2: "all 'until end of turn' and 'this turn' effects end", during the
-- cleanup step. A unit's retention is such an effect, so it ends here -- and the
-- mana itself does NOT: this only clears the duration, and the cleanup step's
-- own CR 500.5 sweep at the step's END is what then takes the mana. Engine.hs
-- calls this beside Damage.removeAllDamage, which is CR 514.2's other half.
--
-- Here rather than in Pawl.Engine.Expiry, which sweeps the carriers keyed by a
-- Pawl.Types.Expiry: a mana unit carries none, and the pool is this module's.
-- The two are one simultaneous turn-based action either way, and nothing runs
-- between them.
--
-- Rewrites every unit rather than filtering: CR 514.2 ends the retention, it
-- does not remove the mana, so a retained unit becomes an ordinary one and stays
-- in the pool for the rest of the step.
--
-- BLANKET over the arms, not ManaRetention.UntilEndOfTurn-specific, and that is
-- a decision rather than an oversight: CR 514.2 is the backstop for any
-- retention still standing at cleanup, and an UntilEndOfCombat unit whose combat
-- phase never reported its end would otherwise sit in the pool for ever;
-- Turn.phaseEndingAt names the one such hole, see #2126. -Werror cannot see the
-- arm list here, the record update naming no constructor.
endManaRetention :: GameState -> GameState
endManaRetention gs =
  let ordinary unit = unit {ManaUnit.retention = ManaRetention.Ordinary}
      ended pool = Mana.MkMana (fmap ordinary (Mana.unwrap pool))
   in gs {GameState.manaPool = fmap ended (GameState.manaPool gs)}

-- CR 500.5a, repeated by CR 511.2: an "until end of combat" retention ends as
-- the combat PHASE ends. endManaRetention's shape one rule over -- it ends the
-- retention and takes no mana, and the CR 500.5 sweep (emptyManaPools) that
-- every caller runs after this is what then empties the pool.
--
-- Takes the window that is ending rather than being a bare
-- endCombatManaRetention, so its three callers spell it exactly as they spell
-- Expiry.dropAtEndOf beside it: Engine.runStepThatBegan for a combat phase whose
-- last step ended, and Pawl.Engine.Resolve's CR 724.1d and CR 724.2e arms for
-- one ended part-way through with no last step to ask.
--
-- EQUALITY on the selector, not Turn.inWindow's containment, for exactly
-- dropAtEndOf's reason: containment would end the retention as the first combat
-- STEP ended, which is the reading CR 500.5a exists to deny.
--
-- Only ManaRetention.UntilEndOfCombat, so a CR 514.2 retention outlives a combat
-- phase in the same turn.
endRetentionAtEndOf :: PhaseSelector -> GameState -> GameState
endRetentionAtEndOf ending gs =
  let ends retention = case retention of
        ManaRetention.UntilEndOfCombat -> ending == PhaseSelector.CombatPhase
        ManaRetention.UntilEndOfTurn -> False
        ManaRetention.Ordinary -> False
      ordinary unit = if ends (ManaUnit.retention unit) then unit {ManaUnit.retention = ManaRetention.Ordinary} else unit
      ended pool = Mana.MkMana (fmap ordinary (Mana.unwrap pool))
   in gs {GameState.manaPool = fmap ended (GameState.manaPool gs)}

-- Permanents this player controls with a mana ability they could activate right
-- now (CR 109.4a: a mana ability's controller is determined as though it were on
-- the stack -- i.e. the permanent's controller, CR 110.2 -- not the object's
-- owner).
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
-- projection and its uses, and Pawl.Engine.Cost.payMana's loop -- the one
-- caller that DOES change the state, by tapping -- takes a fresh State.get on
-- every pass.
manaSources :: Capacity -> PlayerId -> GameState -> [ObjectId]
manaSources capacity pid gs = manaSourcesGiven capacity (Projection.controlGrants gs) (Projection.projectAll gs) pid gs

manaSourcesGiven :: Capacity -> [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
manaSourcesGiven capacity grants pcs pid gs =
  let -- A route answered 0 is no route at all, so a permanent whose every
      -- route is refused is not a source -- Phyrexian Tower's "{T}, Sacrifice a
      -- creature" with no creature to give (CR 118.3), a tapped Forest's "{T}"
      -- (CR 107.5), a sick Llanowar Elves' (CR 302.6). Asked of the ROUTES
      -- rather than the options, since a colour choice cannot change what the
      -- activation charges.
      --
      -- PER ROUTE and not once for the permanent, which is CR 106.12's
      -- narrowing: "tap for mana" is an activation whose cost includes {T}, and
      -- the tap and sickness rules reach only such a cost. A Blood Pet is a
      -- black source while tapped and on the turn it arrives, because
      -- "Sacrifice this creature: Add {B}" is neither (#1116).
      isSource oid = any (\(cost, restrictions, _) -> Activations.times (capacity ForOffer pcs pid oid cost restrictions gs) > 0) (manaRoutesOfGiven pcs oid gs)
   in filter isSource (Projection.controlsGiven grants pid gs)

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
-- might have, the tags it would carry, and whether CR 106.6 restricts what it
-- may be spent on. A pool unit is the settled case (its type is already fixed);
-- an untapped source is the open one, where the choice has not been made yet.
--
-- The RESTRICTION is a Bool and not the restriction itself, because the units
-- reaching here have already been asked about the object THIS payment is for
-- (`spendableAmong`): what is left to know is whether this mana is admissible to
-- a payment that is a different one -- a nested mana ability's own activation
-- cost -- and `payableResolutionsGiven`'s `admits` answers no for every
-- restricted unit, exactly and over-strictly as its own comment argues.
data Supply = MkSupply
  { supplyTypes :: Set.Set ManaType,
    supplyTags :: Set.Set ProductionTag.ProductionTag,
    supplyRestricted :: Bool
  }
  deriving (Eq, Ord, Show)

-- Could this one mana serve that one demand? The ONE relation both payment and
-- payability read, so the two can never disagree about it.
--
-- Types INTERSECT -- the supply need only be able to be one of the demanded
-- types -- while tags are a SUPERSET: CR 107.4h demands mana from a snow source,
-- and a mana that is not from one cannot become so. A supply carrying a tag
-- nothing asked for is no worse for it.
--
-- The RESTRICTION is not read here: a demand carries no record of which payment
-- it belongs to, and that is what CR 106.6 asks about.
-- payableResolutionsGiven's `admits` is where the two meet.
serves :: Supply -> Demand -> Bool
serves supply demand =
  not (Set.disjoint (supplyTypes supply) (demandTypes demand))
    && Set.isSubsetOf (demandTags demand) (supplyTags supply)

-- CR 106.6, split over one player's pool: the units this payment may draw on,
-- and the ones it may not. Built on `spendableAmong` below, which is THE one reader
-- of Pawl.Types.ManaUnit.restriction, so payment and payability cannot disagree
-- about which mana is available -- the reason `serves` just above gives for
-- being one relation.
--
-- `subject` is WHAT the payment is for (Pawl.Types.PaymentSubject), which is the
-- question CR 106.6's restrictions ask: Mishra's Workshop's mana admits CR
-- 601.2h's cast and Omen Hawker's admits CR 602.2b's activation, each under its
-- own predicate. A payment that is neither -- a special action's cost, a combat
-- toll, CR 118.12's resolution-time payment -- can spend no restricted mana at
-- all, since no clause in the vocabulary names it.
--
-- A SPLIT rather than a filter, because the withheld units are still in the pool
-- (CR 106.4): Pawl.Engine.Cost.payMana puts them back beside whatever the
-- payment left, so mana a cost could not use is mana the next cost still has.
--
-- The perspective is the PAYER (CR 109.5's "you"), which is who the spell's
-- controller is at CR 601.2h and the ability's at CR 602.2b. Not implemented: a
-- restriction that reads the SOURCE that produced the mana. Pawl.Types.ManaUnit carries no source id by
-- construction, so the context has none and a source-relative atom would be
-- vacuously False; Cavern of Souls' "of the chosen type" is the printing that
-- wants one (#1978).
spendableFor :: PaymentSubject.PaymentSubject -> PlayerId -> GameState -> ([ManaUnit], [ManaUnit])
spendableFor subject pid gs = spendableAmong subject pid gs (unitsOf (Game.poolOf pid gs))

-- The same split asked of units that are NOT in the pool: what an untapped
-- source's yield would be worth to this payment. CR 605.3b's mana ability adds
-- its mana inline, restriction and all (manaOptionsOfGiven's stamp), so
-- payableResolutionsGiven has to ask this of a yield before counting it as
-- supply -- otherwise the offer admits a payment it then refuses.
--
-- THE one reader of Pawl.Types.ManaUnit.restriction, which is what keeps the two
-- roads' answers the same.
--
-- The subject's view is built ONCE per call and shared across the units, the
-- restriction being a question about the object being paid for rather than about
-- the mana. WHICH half of Pawl.Types.ManaRestriction is read is settled by the
-- subject for the same reason, once for the whole call.
spendableAmong :: PaymentSubject.PaymentSubject -> PlayerId -> GameState -> [ManaUnit] -> ([ManaUnit], [ManaUnit])
spendableAmong subject pid gs units =
  let paidFor = case subject of
        PaymentSubject.ForNeither -> Nothing
        PaymentSubject.Casting oid -> Just (ManaRestriction.casts, oid)
        PaymentSubject.Activating oid -> Just (ManaRestriction.activations, oid)
      asked = fmap (\(half, oid) -> (half, Filter.contextFor (Just pid) Nothing, Projection.viewOfObject oid gs)) paidFor
      admits unit = case ManaUnit.restriction unit of
        Nothing -> True
        Just restriction -> case asked of
          Nothing -> False
          Just (half, context, view) -> case half restriction of
            Nothing -> False
            Just wanted -> Filter.matches context view wanted
   in List.partition admits units

-- A pool unit as a supply. Its type is settled, so the option set is a
-- singleton, and its tags are the ones production stamped on it
-- (manaOptionsOfGiven).
supplyOf :: ManaUnit -> Supply
supplyOf unit =
  MkSupply
    { supplyTypes = Set.singleton (ManaUnit.manaType unit),
      supplyTags = ManaUnit.tags unit,
      supplyRestricted = Maybe.isJust (ManaUnit.restriction unit)
    }

-- CR 609.4b, applied to ONE mana type: what a mana of this type may be spent as
-- under a player's continuous effects (Celestial Dawn). The mana's own type when
-- nothing speaks about it.
--
-- THE SUPPLY SIDE, which is what makes this a different function from `relax`
-- rather than a constructor of ManaSpending. Rule 118.14's permission is about a
-- COST and so is applied to that cost's demands; this one is about a MANA, and
-- says different things about two manas of one pool -- "you may spend white mana
-- as though it were mana of any color. You may spend other mana only as though it
-- were colorless mana". No transform of a demand can depend on which unit is
-- being spent.
--
-- CR 609.4b's limit is respected the same way `relax` respects it: the pool is
-- not rewritten, so what was actually spent is still whatever the units are, and
-- the ManaCost is not rewritten either. Only the question "could this one mana
-- serve that one demand?" changes.
--
-- The clause's own set REPLACES the mana's type when the clause says "only", and
-- is added to it otherwise (Pawl.Types.SpendManaAsThough.only). Celestial Dawn's
-- restriction is the first case -- a red mana under it can pay {1} and {C} and no
-- longer {R} -- and its permission is the second, though nothing observes the
-- difference there, since "mana of any color" already contains white.
--
-- A UNION over the applicable clauses, with no CR 613.11 timestamp ordering,
-- because a union has nothing to order: two clauses that permit different types
-- are not in conflict, so neither running first changes the answer. Two clauses
-- that both said "only" of the same mana would each be permitting what the other
-- forbids, and the union is the reading that keeps CR 101.2 out of it -- an
-- "only" clause names types the mana may be spent as rather than saying "can't".
-- No printing pairs them, so no board tells that reading from another.
spendableAs :: [SpendManaAsThough.SpendManaAsThough] -> ManaType -> Set.Set ManaType
spendableAs clauses manaType =
  case filter (\clause -> ManaFilter.matchesType (SpendManaAsThough.which clause) manaType) clauses of
    [] -> Set.singleton manaType
    applicable ->
      Set.unions
        ( [Set.singleton manaType | not (any SpendManaAsThough.only applicable)]
            <> fmap SpendManaAsThough.asThough applicable
        )

-- The same, applied to a whole supply: every type it could be becomes every type
-- that one could be spent as.
--
-- Per TYPE and then unioned, which is exact for the open case `sourceOptions`
-- builds: a source that could make any colour is a real choice, so under
-- Celestial Dawn it can make white and spend it as any colour, or make red and
-- spend it as colorless, and the union is the set of demands it could serve.
--
-- The TAGS ride through untouched, CR 107.4h's reason and the same one `relax`
-- gives: a clause about which types a mana may be spent as says nothing about
-- where it came from, so a snow demand still wants a snow supply.
rewriteSupply :: [SpendManaAsThough.SpendManaAsThough] -> Supply -> Supply
rewriteSupply clauses supply = case clauses of
  [] -> supply
  _ -> supply {supplyTypes = Set.unions (fmap (spendableAs clauses) (Set.toList (supplyTypes supply)))}

-- A demand for one mana of one of these types, however it was produced -- every
-- TYPED symbol but CR 107.4h's {S}. A symbol demanding no particular mana
-- (Generic, and either symbol's second way) builds no demand at all.
ofTypes :: Set.Set ManaType -> Demand
ofTypes types = MkDemand {demandTypes = types, demandTags = Set.empty}

-- A GENERIC symbol of a SOURCE's own activation cost, said as a demand: CR
-- 106.1b's six types and no tag, so any supply's TYPE serves it.
--
-- A demand rather than a count, unlike the generic part of the cost being paid,
-- because a source's own mana is the half CR 605.3c and CR 106.6 both narrow --
-- only what an earlier position supplies, and never a restricted unit
-- (payableResolutionsGiven's `admits`) -- and a plain count cannot say which
-- supplies it may draw on. Chromatic Star's "{1}, {T}, Sacrifice this artifact"
-- and Coal Golem's "{3}, Sacrifice this creature" are the printings that reach
-- it.
anyTypeDemand :: Demand
anyTypeDemand = ofTypes everyManaType

-- CR 118.14, applied to ONE demand: under a permission to spend mana of any
-- type, a demand for red mana is served by any of CR 106.1b's six types.
--
-- THE DEMAND SIDE and not the supply side, which is CR 609.4b's limit made
-- structural: the pool is untouched, so what was actually spent to pay the cost
-- is still whatever mana the units are, and `resolutions` still reads the
-- unmodified ManaCost, so the cost is unchanged too. A model that recoloured the
-- units instead would have answered CR 500.5's leftover pool and a "spend only
-- black mana" restriction wrongly.
--
-- The TAGS ride through untouched, which is what keeps CR 107.4h's {S} honest:
-- rule 118.14 permits mana of any TYPE to be spent and says nothing about where
-- that mana came from, so a snow demand still wants a snow supply. It is also
-- why this widens rather than deleting the demand outright -- a demand nothing
-- constrains would spend a non-snow mana on {S}.
relax :: ManaSpending -> Demand -> Demand
relax spending demand = case spending of
  ManaSpending.AsProduced -> demand
  ManaSpending.AnyType -> demand {demandTypes = everyManaType}

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
-- CR 107.4f's ways, and BOTH of CR 107.4e's hybrids' ways, are collapsed to ONE
-- before payment by `announce`, which is what CR 601.2b calls for at CR 118.13a's
-- moment and Pawl.Engine.Resolve.payGatePaidBy at CR 118.13b's -- so those
-- enumerations reach payment only where nothing announced: a special action's
-- cost (CR 118.13c, #1990) and a cost to attack (CR 508.1j, #1991). The
-- colour/colour hybrid still gets a single way here rather than two, because both
-- halves are one mana of a stated type: unannounced, they are one demand over two
-- types, which is exactly the permission the symbol grants.
waysOf :: ManaSymbol -> [(Maybe Demand, Natural, Natural)]
waysOf symbol = case symbol of
  ManaSymbol.OfType t -> [(Just (ofTypes (Set.singleton t)), 0, 0)]
  -- CR 107.4e: a colour/colour hybrid is paid with one mana of a stated type
  -- either way, so it contributes nothing to the generic count.
  ManaSymbol.Hybrid (Hybrid.MkHybrid a b) -> [(Just (ofTypes (Set.fromList [a, b])), 0, 0)]
  -- CR 107.4e's two ways, and the one-mana way is FIRST -- see resolutions.
  ManaSymbol.MonocoloredHybrid t -> [(Just (ofTypes (Set.singleton t)), 0, 0), (Nothing, monocoloredHybridGeneric, 0)]
  -- CR 107.4f's two ways: one mana of its colour, or 2 life. The colour is a
  -- ManaType here because that is what a demand is made of; CR 107.4f admits no
  -- colourless Phyrexian symbol, so ManaType.Colored is total rather than a case.
  --
  -- The mana way is FIRST, which resolutions' sort then keeps -- see there.
  ManaSymbol.Phyrexian c -> [(Just (ofTypes (Set.singleton (ManaType.Colored c))), 0, 0), (Nothing, 0, 2)]
  -- CR 107.4f's hybrid Phyrexian symbol, and the union of the two arms around
  -- it: ONE mana of either component colour, which is the Hybrid arm's shape
  -- because a demand is a SET of admissible types, or the same 2 life the
  -- Phyrexian arm offers. Three printed ways, two rows, because the two mana
  -- ways differ only in which member of one demand set pays.
  --
  -- The mana way is FIRST here too, for the arm above's reason.
  ManaSymbol.HybridPhyrexian (HybridPhyrexian.MkHybridPhyrexian l r) ->
    [(Just (ofTypes (Set.fromList [ManaType.Colored l, ManaType.Colored r])), 0, 0), (Nothing, 0, 2)]
  ManaSymbol.Generic n -> [(Nothing, n, 0)]
  -- CR 107.4h: one mana of any type produced by a snow source. ONE way, and a
  -- typed demand rather than a generic count -- the same rule's next sentence
  -- made structural, since generic-mana reductions don't affect {S} costs. Every
  -- type (CR 106.1b) and one tag, so nothing about the mana's identity is asked
  -- and everything about its provenance is.
  ManaSymbol.Snow -> [(Just (MkDemand everyManaType (Set.singleton ProductionTag.Snow)), 0, 0)]
  -- Unreachable in payment: substituteX removes every Variable before canPay --
  -- at CR 601.2b for a cast or an activation, and at
  -- Pawl.Engine.Resolve.payGatePaid for CR 118.12's cost paid on resolution.
  -- The match must be total, so a bare {X} demands nothing and counts as 0
  -- generic.
  ManaSymbol.Variable -> [(Nothing, 0, 0)]

-- CR 601.2b's "nonhybrid equivalent cost", enumerated: every way the whole cost
-- resolves into typed demands, an amount of generic mana and an amount of life,
-- one entry per combination of per-symbol ways. `traverse` over the list
-- applicative is that product.
--
-- CR 118.14's permission is applied HERE, to the demands and to nothing else --
-- one place, so payment (`spend`) and payability (`payableResolutions`) cannot
-- come to disagree about what a widened cost asks for, exactly as `serves` is
-- one relation for both. See `relax` for why the cost itself is left alone.
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
--      untaps. Conservative, and still pawl choosing -- which is why a cast, an
--      activation (`announce`, CR 118.13a) and a cost paid on resolution
--      (Pawl.Engine.Resolve.payGatePaidBy, CR 118.13b) all announce first and
--      reach this sort with no Phyrexian symbol left to order. A special action's
--      cost (#1990) and a cost to attack (#1991) have no such announcement.
--   2. Among equal life, FEWEST UNITS, which waysOf's per-symbol order already
--      gives and a STABLE sort preserves. Pawl choosing again, and reached on the
--      same terms: every announcing site above clears its monocolored hybrids
--      first, so only those same two unannounced costs arrive here with a {2/X}
--      to order (#1990, #1991).
--
-- The sort is what makes rule 1 hold across symbols rather than within one: for
-- {2/R}{G/P} the product alone would offer a 2-life way before a 0-life one.
resolutions :: ManaSpending -> ManaCost -> [([Demand], Natural, Natural)]
resolutions spending (ManaCost.MkManaCost symbols) =
  let collect ways =
        ( fmap (relax spending) (Maybe.mapMaybe (\(demand, _, _) -> demand) ways),
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
--
-- The CLAUSES are the payer's CR 609.4b permissions, threaded from the caller
-- rather than read off a board because this helper has neither a player nor a
-- state. Pawl.Engine.Cost.payMana resolves them from the board once per pass and
-- hands them down through `spend`; empty is the ordinary board.
removals :: [SpendManaAsThough.SpendManaAsThough] -> Demand -> [ManaUnit] -> [[ManaUnit]]
removals clauses wanted units = Maybe.mapMaybe without (zip [0 :: Int ..] units)
  where
    without (i, u) =
      if serves (rewriteSupply clauses (supplyOf u)) wanted
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
spendDemands :: [SpendManaAsThough.SpendManaAsThough] -> [ManaUnit] -> [Demand] -> Maybe [ManaUnit]
spendDemands clauses units demands = case demands of
  [] -> Just units
  wanted : rest ->
    Maybe.listToMaybe (Maybe.mapMaybe (\left -> spendDemands clauses left rest) (removals clauses wanted units))

takeAny :: [ManaUnit] -> a -> Maybe [ManaUnit]
takeAny units _ = case units of
  _ : rest -> Just rest
  [] -> Nothing

-- A payment as the sequence of ONE-MANA choices it is: a typed symbol's demand,
-- then one entry per generic mana, which CR 107.4b lets any type pay.
--
-- Typed first for the reason `spend` gives: generic takes any unit, so paying it
-- first could strand a demand nothing else serves.
paymentSteps :: [Demand] -> Natural -> [Maybe Demand]
paymentSteps demands generic = fmap Just demands <> replicate (Natural.toIntSaturating generic) Nothing

-- Could this one unit pay that one step?
paysStep :: [SpendManaAsThough.SpendManaAsThough] -> Maybe Demand -> ManaUnit -> Bool
paysStep clauses step unit = case step of
  Nothing -> True
  Just wanted -> serves (rewriteSupply clauses (supplyOf unit)) wanted

-- Every pool these steps could leave behind, as a SET of sorted pools -- which is
-- what makes it a count of outcomes a player could tell apart rather than of
-- assignments. Equal units collapse, and so do two orders of the same spend.
--
-- Set-at-a-time rather than a search per assignment, so the work is bounded by
-- the distinct sub-pools rather than by the permutations of them; see #595.
leftovers :: [SpendManaAsThough.SpendManaAsThough] -> [Maybe Demand] -> [ManaUnit] -> Set.Set [ManaUnit]
leftovers clauses steps units = List.foldl' advance (Set.singleton (List.sort units)) steps
  where
    advance pools step =
      Set.fromList
        [ List.delete unit pool
        | pool <- Set.toList pools,
          unit <- Set.toAscList (Set.fromList pool),
          paysStep clauses step unit
        ]

-- The distinct units that could pay the next step and still leave the rest of the
-- payment possible. The offer `spendChosen` asks over; a unit that pays this step
-- and strands a later one is no option at all (CR 601.2h forbids a partial
-- payment).
spendable :: [SpendManaAsThough.SpendManaAsThough] -> [Maybe Demand] -> [ManaUnit] -> [ManaUnit]
spendable clauses steps units = case steps of
  [] -> []
  step : rest ->
    [ unit
    | unit <- Set.toAscList (Set.fromList units),
      paysStep clauses step unit,
      not (Set.null (leftovers clauses rest (List.delete unit units)))
    ]

-- The resolution a payment out of this pool will take -- the one `spend` settles
-- on -- as the steps it decomposes into and the life it commits.
plan :: [SpendManaAsThough.SpendManaAsThough] -> ManaSpending -> Natural -> ManaCost -> Mana -> Maybe ([Maybe Demand], Natural)
plan clauses spending budget cost (Mana.MkMana units) =
  Maybe.listToMaybe
    [ (steps, life)
    | (demands, generic, life) <- resolutions spending cost,
      life <= budget,
      let steps = paymentSteps demands generic,
      not (Set.null (leftovers clauses steps units))
    ]

-- CR 601.2h: the PLAYER pays the cost, so which mana leaves their pool is theirs
-- to choose, CR 107.4b's generic symbol included. Answers the pool that is left.
--
-- Asked only where the choice is observable: the payment must be able to leave
-- more than one pool, and the candidates are deduplicated, so two units that are
-- equal are one option and a payment that empties the pool however it is made
-- asks nothing.
--
-- FILTERED, NOT TRUSTED, the Pawl.Engine.Cost.chooseSource posture.
--
-- BOTH HALVES come back: the pool that is left, and the units that went. CR
-- 107.4h's third sentence asks about the second one after the fact, and the
-- caller cannot recover it by subtraction -- two equal units are the same value,
-- so a pool that shrank by one says nothing about which.
spendChosen :: PlayerId -> [SpendManaAsThough.SpendManaAsThough] -> [Maybe Demand] -> Mana -> Game (Mana, Mana)
spendChosen pid clauses steps0 (Mana.MkMana units0) = fmap (Bifunctor.bimap Mana.MkMana Mana.MkMana) (go steps0 units0)
  where
    go steps units = case steps of
      [] -> pure (units, [])
      _ : rest -> case spendable clauses steps units of
        -- Unreachable from `plan`, which offers these steps only where some
        -- payment completes them.
        [] -> pure (units, [])
        first : others -> do
          chosen <-
            if null others || Set.size (leftovers clauses steps units) < 2
              then pure first
              else do
                gs <- State.get
                answer <- Game.choose (Prompt.ChooseManaToSpend (Decide.deciderFor pid gs) pid (first NonEmpty.:| others))
                pure (if List.elem answer (first : others) then answer else first)
          fmap (fmap (chosen :)) (go rest (List.delete chosen units))

-- Spend a pool against a cost, within a budget of `budget` life. Nothing when no
-- resolution fits; otherwise the pool that is left and the life to pay for it.
--
-- ONE assignment of the several that may fit, so this answers WHETHER the pool
-- pays rather than what the payment leaves. The payment itself is `plan` and
-- `spendChosen`, where CR 601.2h's choice is the player's.
--
-- Typed symbols are matched FIRST because they are the constrained ones: generic
-- takes any unit, so paying it first could consume the only red and strand a
-- {R} that nothing else can satisfy.
--
-- The FIRST resolution that fits wins. For every cost without a monocolored
-- hybrid or a Phyrexian symbol there is only one; where there are several,
-- `resolutions` has already put the least-life, fewest-units one first.
--
-- The SPENDING permission is the payer's (CR 118.14), threaded in from the
-- caller rather than read off a board: what a spell may be paid with is fixed by
-- the effect that permitted the cast, and by the time the payment happens CR
-- 601.2a has already moved the card to a new incarnation that holds no
-- permission (Pawl.Engine.Cast.castSpellWith captures it before the move).
--
-- The BUDGET is a cap and not a target, and it is what keeps that ordering
-- meaningful during Pawl.Engine.Cost.payMana's loop. Left uncapped, a {G/P}
-- would take the 2-life resolution on the first pass -- the pool is empty
-- before any source is tapped, so the mana resolution cannot fit yet -- and the
-- Forest would never be tapped at all. payMana passes `lifeNeeded`, the least
-- life any PAYABLE resolution costs, so a cost the board can pay with mana is
-- capped at zero life and the loop is forced to tap for it.
spend :: [SpendManaAsThough.SpendManaAsThough] -> ManaSpending -> Natural -> ManaCost -> Mana -> Maybe (Mana, Natural)
spend clauses spending budget cost (Mana.MkMana units) =
  let attempt (demands, generic, life) = do
        Monad.guard (life <= budget)
        afterTyped <- spendDemands clauses units demands
        left <- Monad.foldM takeAny afterTyped [1 .. generic]
        pure (Mana.MkMana left, life)
   in Maybe.listToMaybe (Maybe.mapMaybe attempt (resolutions spending cost))

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
-- ability's cost through the same rule. CR 118.13b asks the same two questions of
-- a cost paid during a resolution, immediately before that payment, and reaches
-- this function through the same Pawl.Engine.Cost.announce seam.
--
-- Returns CR 601.2b's own phrase -- the "nonhybrid equivalent cost", a mana cost
-- with no Phyrexian symbol and neither of CR 107.4e's hybrids left in it -- the
-- life the announcement committed, and HOW MANY of CR 107.4f's symbols committed
-- it. The last is not the first divided by two even though it always equals it:
-- rule 702.150a counts SYMBOLS, and deriving that from an amount of life would
-- make a rule about compleated planeswalkers depend on nothing recording the
-- number. Pawl.Engine.Cost.announce turns that life
-- into CR 119.4's payment, so nothing below this function ever sees a mana symbol
-- that spends no mana.
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
-- `total` answers a LIST, and a route counts as payable when ANY of its totals
-- pays: CR 118.7e's choice of which half a hybrid REDUCTION takes is made at CR
-- 601.2f, one step after this, so withholding the resolution that pays would
-- hide a route here for a choice the player has not made yet. That keeps this
-- offer exactly as permissive as Pawl.Engine.Cost.canPaySomeCompletion, which
-- quantifies over the same two enumerations. What no test observes is the
-- difference, since it is only whether this function ASKS (#1076).
--
-- Measured against the BOARD and not the pool: canPayCommitting counts an
-- untapped source as the mana it could make (payableResolutions), so a Forest
-- still in play offers the mana route before anything is tapped. A player who
-- then taps it for something else fails the payment, which is CR 601.2h's
-- business and not this function's -- announcing is not producing.
--
-- `outside` is the life the REST of the cost owes -- CR 119.4 payments that are
-- no part of this mana cost, which Pawl.Engine.Cost.lifeOwedBy sums off the
-- components. CR 118.3 makes them one demand on one life total, so a route this
-- function offers has to be payable alongside them: a {G/P} beside an additional
-- cost of 2 life is a 4-life route and must not be offered at 3. Zero for a cost
-- whose components spend no life, which is every cost that reached here before
-- one did.
--
-- `claimed` is the same thing for objects: the components' own claims on a zone's
-- contents or on the untapped permanents (Pawl.Types.ClaimAxis), which every route
-- offered here has to be payable alongside (#1134). Empty for a cost whose
-- components spend no object.
--
-- `spending` is CR 118.14's permission, riding through to the payability
-- question below for the reason `outside` and `claimed` do: a route this
-- function refuses is a choice taken away from the player, and under a
-- permission to spend mana of any type the off-colour route is a real one.
--
-- FILTERED, NOT TRUSTED, the chooseSource posture.
announce :: PaymentSubject.PaymentSubject -> Capacity -> ManaSpending -> PlayerId -> ObjectId -> (ManaCost -> [ManaCost]) -> Natural -> [Claim] -> ManaCost -> Game (ManaCost, Natural, Natural)
announce subject capacity spending pid oid total outside claimed (ManaCost.MkManaCost symbols) = go [] 0 0 symbols
  where
    -- "Payable" here means SOME completion of the remaining announcements pays
    -- it, which is what CR 601.2b's last sentence makes the question. Enumerated
    -- here rather than left to canPay's own `resolutions` so that each completion
    -- is an announcement-free cost by the time `total` sees it.
    --
    -- `done` is the reversed prefix already announced, `ways` the symbols this
    -- route would leave in place of the one being asked about, and `extra` the
    -- life it would commit on top of what is committed already. `outside` rides
    -- on every one of them, for the reason the haddock gives.
    stillPayable done rest gs extra ways =
      let candidate (tail_, life) =
            any
              (\totalled -> canPayCommitting subject capacity spending pid (outside + extra + life) claimed totalled gs)
              (total (ManaCost.MkManaCost (reverse done <> ways <> tail_)))
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
    -- payable here can be one the gate refused. Unreachable BY CONSTRUCTION and
    -- not by conservatism, because the gate is now the same predicate over the
    -- same completions -- Pawl.Engine.Cost.canPaySomeCompletion is this
    -- `stillPayable` with nothing yet announced, which is what closed the
    -- monocolored hybrid's disagreement about CR 118.7a's reductions. {X} used to
    -- be a wedge in that -- a gate at X=0 while this runs on the value the player
    -- named -- and BOTH callers now close it the same way, by
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
    -- `paidWithLife` counts the symbols the third accumulator's haddock
    -- describes; it moves in lockstep with `committed` and is carried separately
    -- for the reason given there.
    go done committed paidWithLife remaining = case remaining of
      [] -> pure (ManaCost.MkManaCost (reverse done), committed, paidWithLife)
      symbol@(ManaSymbol.Phyrexian color) : rest -> do
        gs <- State.get
        let asMana = ManaSymbol.OfType (ManaType.Colored color)
            offers =
              [PhyrexianPayment.PaysMana | stillPayable done rest gs committed [asMana]]
                <> [PhyrexianPayment.PaysLife | stillPayable done rest gs (committed + phyrexianLife) []]
        announced <-
          choose PhyrexianPayment.PaysMana offers $
            Prompt.AnnouncePhyrexianPayment (Decide.deciderFor pid gs) pid oid symbol
        case announced of
          PhyrexianPayment.PaysMana -> go (asMana : done) committed paidWithLife rest
          PhyrexianPayment.PaysLife -> go done (committed + phyrexianLife) (paidWithLife + 1) rest
      -- CR 107.4f's hybrid Phyrexian symbol, whose three ways are the arm above's
      -- two with the mana one split in half. TWO PROMPTS, in that order: mana or
      -- life, then which colour -- and the second is asked only of the mana
      -- route, so the pair reaches exactly rule 107.4f's three announcements.
      --
      -- Both prompts are FILTERED BY PAYABILITY on the same `stillPayable` the
      -- arms around them use, which is what keeps the split from offering a
      -- colour the board cannot produce, and what elides the second prompt
      -- outright where one colour is payable. The mana route is offered at all
      -- only where some colour is: a PaysMana with no payable half would strand
      -- the payment CR 601.2b's last sentence promises.
      symbol@(ManaSymbol.HybridPhyrexian (HybridPhyrexian.MkHybridPhyrexian l r)) : rest -> do
        gs <- State.get
        let halves = hybridHalves (ManaType.Colored l) (ManaType.Colored r)
            payableHalves = filter (\half -> stillPayable done rest gs committed [ManaSymbol.OfType half]) halves
            offers =
              [PhyrexianPayment.PaysMana | not (null payableHalves)]
                <> [PhyrexianPayment.PaysLife | stillPayable done rest gs (committed + phyrexianLife) []]
        announced <-
          choose PhyrexianPayment.PaysMana offers $
            Prompt.AnnouncePhyrexianPayment (Decide.deciderFor pid gs) pid oid symbol
        case announced of
          PhyrexianPayment.PaysLife -> go done (committed + phyrexianLife) (paidWithLife + 1) rest
          PhyrexianPayment.PaysMana -> do
            half <-
              choose (ManaType.Colored l) payableHalves $
                Prompt.AnnounceHybridHalf (Decide.deciderFor pid gs) pid oid symbol
            go (ManaSymbol.OfType half : done) committed paidWithLife rest
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
          HybridPayment.PaysTyped -> go (asTyped : done) committed paidWithLife rest
          HybridPayment.PaysGeneric -> go (asGeneric : done) committed paidWithLife rest
      -- CR 107.4e: "a hybrid symbol such as {W/U} can be paid with either white
      -- or blue mana." Both ways spend ONE mana and commit no life, so this
      -- announcement moves neither CR 601.2f's total nor any reduction -- which
      -- is what sets it apart from the monocolored hybrid just above. What it
      -- decides is WHICH mana of an oversupplied pool is spent, and so what is
      -- left floating afterwards.
      symbol@(ManaSymbol.Hybrid (Hybrid.MkHybrid a b)) : rest -> do
        gs <- State.get
        let offers = filter (\half -> stillPayable done rest gs committed [ManaSymbol.OfType half]) (hybridHalves a b)
        announced <-
          choose a offers $
            Prompt.AnnounceHybridHalf (Decide.deciderFor pid gs) pid oid symbol
        go (ManaSymbol.OfType announced : done) committed paidWithLife rest
      other : rest -> go (other : done) committed paidWithLife rest

-- Every way the announcements of a cost's UNANNOUNCED tail could go, as the
-- symbols that leaves and the life those choices commit -- CR 601.2b's own
-- "nonhybrid equivalent cost", one entry per combination. Exactly the product
-- `resolutions` takes over CR 107.4e's and CR 107.4f's ways, lifted to the SYMBOL
-- level so that a caller can hand each completion to CR 601.2f's totalling before
-- asking whether it is payable.
--
-- One entry for a tail with nothing to announce, so this is the identity case for
-- every cost `announce` leaves untouched, and 2^(number of Phyrexian and hybrid
-- symbols) otherwise. Every other symbol rides through in place, which is what
-- keeps a completion a cost and not merely a set of choices.
completions :: [ManaSymbol] -> [([ManaSymbol], Natural)]
completions symbols = case symbols of
  [] -> [([], 0)]
  ManaSymbol.Phyrexian color : rest ->
    let asMana = ManaSymbol.OfType (ManaType.Colored color)
     in [(asMana : tail_, life) | (tail_, life) <- completions rest]
          <> [(tail_, life + phyrexianLife) | (tail_, life) <- completions rest]
  -- CR 107.4f's hybrid Phyrexian symbol: three ways rather than two, the arm
  -- above's life route beside one nonhybrid equivalent per component colour.
  -- `hybridHalves` collapses the degenerate pair for the reason it does above.
  --
  -- A REGRESSION FENCE rather than proven behaviour, unlike every other arm
  -- here: deleting it leaves the whole suite green. Both of this function's
  -- callers reach `waysOf` for a symbol that rides through unexpanded, and
  -- waysOf's rows say the same three things, so the two agree on every board --
  -- and neither cost in `data/cards/` that prints this symbol prints two of
  -- them, so `announce`'s tail is announcement-free by the time it asks. What the
  -- arm buys is the CONTRACT in the haddock above: a completion that still holds
  -- one is not a nonhybrid equivalent cost, which is what CR 601.2f is defined
  -- over. The card that would make it observable is one printing two
  -- announceable symbols with a hybrid Phyrexian symbol among them; none does.
  ManaSymbol.HybridPhyrexian (HybridPhyrexian.MkHybridPhyrexian l r) : rest ->
    concatMap
      (\half -> [(ManaSymbol.OfType half : tail_, life) | (tail_, life) <- completions rest])
      (hybridHalves (ManaType.Colored l) (ManaType.Colored r))
      <> [(tail_, life + phyrexianLife) | (tail_, life) <- completions rest]
  -- CR 107.4e's two ways, neither of which commits life. The {2} is a Generic
  -- symbol and not a demand for two mana of the stated type: CR 107.4e says "two
  -- mana of any type", and CR 107.4b says a numerical symbol represents generic
  -- mana, which "can be paid with any type of mana" -- the same permission, which
  -- is why the substitution loses nothing.
  ManaSymbol.MonocoloredHybrid manaType : rest ->
    [(ManaSymbol.OfType manaType : tail_, life) | (tail_, life) <- completions rest]
      <> [(ManaSymbol.Generic monocoloredHybridGeneric : tail_, life) | (tail_, life) <- completions rest]
  -- CR 107.4e's colour/colour half: one mana of either component type, and
  -- neither way commits life. Expanded here rather than ridden through so that
  -- every completion really is CR 601.2b's NONHYBRID equivalent cost -- the
  -- thing CR 601.2f is defined over.
  ManaSymbol.Hybrid (Hybrid.MkHybrid a b) : rest ->
    concatMap
      (\half -> [(ManaSymbol.OfType half : tail_, life) | (tail_, life) <- completions rest])
      (hybridHalves a b)
  other : rest -> [(other : tail_, life) | (tail_, life) <- completions rest]

-- CR 107.4e's two ways for a colour/colour hybrid, as the mana types they name.
-- The ONE place `Hybrid t t` is collapsed: that symbol is degenerate rather than
-- illegal (Pawl.Types.ManaSymbol) and means `OfType t`, so it is one way and not
-- two -- which keeps `announce` from asking a question with one answer written
-- twice, and keeps `completions` from returning the same cost twice.
hybridHalves :: ManaType -> ManaType -> [ManaType]
hybridHalves a b = if a == b then [a] else [a, b]

-- CR 118.3: can this MANA cost be paid at all, with nothing else claiming the
-- resources? Pure, because Action.legalActions reaches this family of predicates
-- (through Pawl.Engine.Cost.canPaySomeCompletionGiven, which asks
-- canPayCommittingGiven below) while merely ENUMERATING actions, where prompting
-- would be absurd -- so it cannot simply walk tapForMana, which now asks a
-- question.
--
-- A cost is payable exactly when SOME resolution of it is: `resolutions` has
-- already turned CR 107.4e's {2/B} and CR 107.4f's {G/P} into their nonhybrid
-- equivalents, so this asks nothing about hybrid-ness. payableResolutions is
-- where the per-resolution test lives.
--
-- A mana cost that is part of a whole COST goes through canPayCommitting below
-- instead, because its components may want life too (CR 118.3 again). What is
-- left here is the mana cost asked about on its own -- with nothing committed,
-- nothing claimed and no CR 118.14 permission, which is the spending rule every
-- cost takes when no effect has spoken about it.
--
-- NEITHER A CAST NOR AN ACTIVATION either, so CR 106.6-restricted mana is no
-- supply for it (spendableFor). Every caller is asking about a cost that is
-- neither -- Pawl.Engine.Cost.canPay's own callers, and the specs -- and a
-- caller that WAS one would want canPayCommitting with its subject.
canPay :: Capacity -> PlayerId -> ManaCost -> GameState -> Bool
canPay capacity pid = canPayCommitting PaymentSubject.ForNeither capacity ManaSpending.AsProduced pid 0 []

-- The same question with the payer's CR 118.14 permission and with resources
-- already spoken for: `spending`, which `relax` applies to the demands;
-- `committed` life, which CR 119.4's floor must still admit alongside whatever
-- the rest of this cost costs; and `claimed`, the objects the rest of this cost
-- will spend, whether by taking them out of a zone or by tapping them
-- (Pawl.Types.ClaimAxis).
--
-- `subject` is spendableFor's, and it is what makes this the function a CAST or
-- an ACTIVATION asks: the wrapper above hard-codes ForNeither.
--
-- Two callers commit life: `announce`, for CR 118.13a's choices -- both those
-- already made and those a `completions` entry is standing in for -- and
-- Pawl.Engine.Cost's canPay and canPaySomeCompletion(Given), for the CR 119.4
-- payments the cost's COMPONENTS owe.
--
-- The same callers claim objects, and only from the components (Cost.claimOf):
-- Village Rites' "sacrifice a creature" and Phyrexian Tower's are one demand on
-- one creature under CR 118.3, exactly as two sources' are (#1134). Zero and
-- empty everywhere else, which is what `canPay` is.
canPayCommitting :: PaymentSubject.PaymentSubject -> Capacity -> ManaSpending -> PlayerId -> Natural -> [Claim] -> ManaCost -> GameState -> Bool
canPayCommitting subject capacity spending pid committed claimed cost gs =
  let pcs = Projection.projectAll gs
   in canPayCommittingGiven subject capacity spending (manaSourcesGiven (supplyCapacity capacity) (Projection.controlGrants gs) pcs pid gs) pcs pid committed claimed cost gs

-- The same question given a board already walked -- see payableResolutionsGiven
-- for what `sources` and `pcs` are and why handing them in changes no answer.
canPayCommittingGiven :: PaymentSubject.PaymentSubject -> Capacity -> ManaSpending -> [ObjectId] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> Natural -> [Claim] -> ManaCost -> GameState -> Bool
canPayCommittingGiven subject capacity spending sources pcs pid committed claimed cost gs = not (null (payableResolutionsGiven subject capacity spending sources pcs pid committed claimed cost gs))

-- One source's contribution to the supply side, as the OPTIONS it offers: one
-- option per group of yields (see the collapse below), and each option is that
-- group -- read as one supply per mana it adds -- repeated as many times as it is
-- taken, paired with what those activations SPEND: the claims they make on objects
-- (Pawl.Types.ClaimAxis), and the life they pay (manaSuppliesGiven).
-- payableResolutions picks exactly ONE option per source.
--
-- How many times is enumerated, 0 up to the ceiling, for an option that SPENDS
-- something a board is measured against -- a CONTENDED claim on objects, or CR
-- 119.4's life -- and fixed at the ceiling for one that spends neither. That
-- asymmetry is the whole of #1126: the clauses payableResolutions asks of a board
-- only ever grow as supplies are added, so a free source is never worth taking
-- fewer times -- but a claiming or a life-paying one is, since the objects and the
-- life it leaves alone are what another source's cost, or the cost being paid, can
-- then have. Ashnod's Altar and Phyrexian Tower beside one creature buy one
-- sacrifice between them, and whose is the player's to choose; a Treasonous Ogre
-- activated fewer times than it could be is the life half of the same thing.
--
-- `contended` is the CALLER's answer to "do this source's claims meet any other
-- group's?", which is a fact about the whole board and not about this source
-- (payableResolutionsGiven builds it). Where they meet nothing, declining an
-- activation frees an object no other claim wants, so the maximum is never worth
-- undercutting -- and taking the maximum is always satisfiable, `times` being
-- Claim.repeats' own answer for these claims alone. LIFE is never isolated, one
-- life total serving every source, so a life-paying source widens whatever its
-- claims do.
--
-- That is what keeps CR 107.5's {T} affordable: every land and every mana
-- creature claims itself, so without the test a board of n sources would be 2^n
-- boards below; see #1725. It does not bound the case where they genuinely meet --
-- a Springleaf Drum beside n mana creatures widens all n -- which is the same
-- unpruned search #595 tracks.
--
-- The COLLAPSE is where several yields become one option, and it is exact rather
-- than a shortcut. Where a yield adds at most one mana, its option is at most one
-- supply, and one supply is already "one mana that could be any of these types":
-- the union over such yields is precisely what an Urborg'd Mountain or a Birds of
-- Paradise puts on the table. A yield adding NO mana is dropped by the same
-- union, which changes no answer, since both of payableResolutions' board clauses
-- only ever grow as supplies are added.
--
-- It is also what keeps the search below small: Birds of Paradise's five yields
-- collapse to one option, so five Birds are one board rather than 5^5. A yield
-- adding SEVERAL mana cannot join a union -- "one mana of any of these types"
-- cannot speak for Sol Ring's {C}{C} -- so it stays its own option.
--
-- Collapsed PER whole answer (Pawl.Types.Activations: the count, the claims and
-- the life), which is what lets a repeatable source MIX its yields across
-- activations: each activation of Phyrexian Altar ("Sacrifice a creature: Add one
-- mana of any color") chooses a colour on its own, so two creatures buy one red
-- and one green, and the union repeated twice says exactly that where one option
-- per yield said "two of one colour" (#1131). Exact because the KEY is what an
-- activation costs: every yield in a group is reachable by the same number of
-- activations spending the same objects and the same life, so k of them are k
-- supplies of the union, claim Claim.scale k, and pay k times the life.
--
-- Two GROUPS are still never mixed, since a source offers one option: a
-- multi-mana yield cannot be taken on one activation and a one-mana yield on the
-- next, nor can two groups that differ in what an activation costs. Not
-- implemented; no card in the pool has either shape (#1137).
--
-- The TAGS mix by union, and there too the union is exact: manaOptionsOfGiven
-- stamps one tag set on every unit of every yield of a source, because CR 106.3
-- makes them all facts about that one source.
-- ONE option a source offers a board: the mana it would add, the mana its own
-- activations would eat (CR 602.2b), and what those activations spend besides.
--
-- The demands and the supplies are ONE record because they are one fact -- k
-- activations of one route -- and separating them would let a board take the
-- yield of an activation whose cost it never served.
data SourceOption = MkSourceOption
  { optionSupplies :: [Supply],
    optionDemands :: [Demand],
    optionClaims :: [Claim],
    optionLife :: Natural
  }
  deriving (Eq, Ord, Show)

sourceOptions :: [SpendManaAsThough.SpendManaAsThough] -> Bool -> [(Activations.Activations, Mana, ManaCost)] -> [SourceOption]
sourceOptions clauses contended supplies =
  let -- CR 106.6 joins the key the narrow yields are grouped by, so the collapse
      -- below never unions a restricted mana with an unrestricted one into a
      -- supply that is neither. Such a source offers the two as separate
      -- OPTIONS, which is what they are: one activation makes one or the other.
      unitLists = [((activations, manaCost, any (Maybe.isJust . ManaUnit.restriction) (unitsOf yield)), unitsOf yield) | (activations, yield, manaCost) <- supplies]
      (narrow, wide) = List.partition (\(_, units) -> length units <= 1) unitLists
      grouped = [(key, collapsed restricted units) | (key@(_, _, restricted), units) <- Map.toList (Map.fromListWith (<>) narrow)]
      apart = [(key, fmap (rewriteSupply clauses . supplyOf) units) | (key, units) <- wide]
   in List.nub (concatMap optionsFor (grouped <> apart))
  where
    optionsFor ((activations, manaCost, _), offered) =
      let claims = Activations.claims activations
          life = Activations.life activations
          eats = not (null (ManaCost.unwrap manaCost))
          -- A mana-EATING option is always worth taking fewer times, for the same
          -- reason a claiming or a life-paying one is: what it does not activate
          -- is mana the rest of the board no longer owes. The free-and-uncontended
          -- shortcut below rests on a board's clauses only ever GROWING as
          -- supplies are added, which stops being true the moment an option adds
          -- a demand as well.
          counts =
            if not eats && (null claims || not contended) && life == 0
              then [Activations.times activations]
              else [0 .. Activations.times activations]
          -- CR 107.4e's hybrid and CR 107.4f's Phyrexian inside a mana ability's
          -- OWN cost, resolved the way the cost being paid is: one option per
          -- resolution, so the board picks. ManaSpending.AsProduced because rule
          -- 118.14's permission is granted for a CAST, and this is an activation.
          resolved = if eats then resolutions ManaSpending.AsProduced manaCost else [([], 0, 0)]
       in [ MkSourceOption
              { optionSupplies = concat (List.genericReplicate k offered),
                optionDemands = concat (List.genericReplicate k (demands <> List.genericReplicate generic anyTypeDemand)),
                optionClaims = Claim.scale k claims,
                optionLife = k * (life + owed)
              }
          | k <- counts,
            (demands, generic, owed) <- resolved
          ]
    collapsed restricted units =
      if null units
        then []
        else
          [ rewriteSupply
              clauses
              MkSupply
                { supplyTypes = Set.fromList (fmap ManaUnit.manaType units),
                  supplyTags = Set.unions (fmap ManaUnit.tags units),
                  supplyRestricted = restricted
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
-- production-time tags it would carry (CR 106.3). A source is a CHOICE among such
-- supplies, one option per yield it could add and each option as wide as the
-- number of activations that yield admits (sourceOptions above). Each typed
-- symbol of the cost is a DEMAND, which `serves` matches against a supply;
-- generic symbols demand a count and nothing more.
--
-- A BOARD is the pool plus one option taken from every source -- the mana the
-- player would actually have in front of them after tapping everything -- and it
-- is a board only if the activations it took are JOINTLY payable alongside
-- `claimed`, since two of them, or one of them and the cost being paid, may claim
-- one object (payableResolutionsGiven below). A RESOLVED cost is payable when SOME
-- board satisfies all three clauses:
--
--   1. every typed demand can be met at once -- a matching of demands into
--      supplies that saturates the demand side;
--   2. enough supplies are left over for the generic part. Every full typed
--      matching consumes exactly one supply per typed symbol, so the leftover
--      count does not depend on WHICH matching, and this is a plain comparison;
--      and
--   3. CR 119.4's floor admits the life: this resolution's own, PLUS the life the
--      BOARD's own activations pay -- Treasonous Ogre's "Pay 3 life: Add {R}" taken
--      three times is 9 -- PLUS whatever is already spoken for (`committed`: an
--      announcement in progress, or the life the whole cost's components owe; see
--      canPayCommitting). One total on one life total, which is CR 118.3's "fully"
--      read over the whole payment, exactly as clause 1's claims are: paying k
--      amounts in turn is affordable iff their sum is, since the total only falls.
--      Where neither the cost nor any source on the board spends life, the total is
--      0 and CR 119.4b lets anyone pay that, so the clause changes no answer such a
--      board used to give -- see Event.canPayLife.
--
-- All three clauses are asked of ONE board at a time, and that is the whole of
-- #450's fix. Asking them of a per-source union instead lets one source's first
-- mana come from one yield and its second from another, which is not a board any
-- sequence of activations reaches: Ashaya, Soul of the Wild makes a Palladium Myr
-- ({T}: Add {C}{C}) a Forest as well, so the union read it as able to make two
-- mana one of which is green.
--
-- The SEARCH over boards is the product of the sources' options, exponential in
-- the number of sources offering more than one -- the reason sourceOptions'
-- collapse matters. After it, a source offers more than one option only if it has
-- yields the collapse keeps apart -- one adding more than one mana, which in this
-- pool takes a multi-mana ability on a permanent some effect has ALSO given a
-- basic land type (Palladium Myr under Ashaya) -- or if its activation claims
-- something ANOTHER group also wants, or pays life, either of which costs one
-- option per number of times it could be taken. So the ordinary board -- lands,
-- and a mana creature, each claiming only itself -- is still one board, and a
-- repeatable source's colour choices cost options in its repeat count and not in
-- the number of colours.
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
payableResolutions :: PaymentSubject.PaymentSubject -> Capacity -> ManaSpending -> PlayerId -> Natural -> [Claim] -> ManaCost -> GameState -> [([Demand], Natural, Natural)]
payableResolutions subject capacity spending pid committed claimed cost gs =
  let pcs = Projection.projectAll gs
   in payableResolutionsGiven subject capacity spending (manaSourcesGiven (supplyCapacity capacity) (Projection.controlGrants gs) pcs pid gs) pcs pid committed claimed cost gs

-- The same list given a board the CALLER has already walked, which is the half
-- Action.legalActions' enumeration wants: the wrapper above takes one
-- control-grant walk and one whole-board projection per CALL, and the caller
-- there is a loop over the battlefield, so an activation cost measured through
-- the wrapper costs a whole-board sweep per permanent (#716).
--
-- `sources` is manaSourcesGiven's answer for this player, handed in rather than
-- taken, because that sweep is itself a walk of everything the player controls
-- asking manaRoutesOfGiven of each -- identical for every ability of every
-- permanent in one enumeration, and so one more per-permanent O(N) walk when it
-- is taken here (#1073).
--
-- IT MUST BE `supplyCapacity capacity`'s OWN LIST, which is what
-- Pawl.Engine.Cost.supplyManaSourcesGiven builds. Nothing in the type says so,
-- and the two plain wrappers above are what a caller with no list of its own
-- uses. A source listed here whose every route the supply capacity refuses
-- contributes an EMPTY option list, and `sequenceA options` below turns one of
-- those into no board at all -- the whole player's mana unpayable. The offer
-- list (Cost.activationManaSourcesGiven) is what would do it: it admits a
-- permanent whose only mana route holds mana in its own cost, and one whose only
-- mana route CR 601.2a's move puts out of reach (Cost.stackedManaActivations).
-- Pawl.ManaSpec's "CR 106.4 the Ignus's own yield is no supply for the {R} its
-- activation eats" is what holds the pairing: unstacking the source list alone
-- reddens its second assertion, a Mountain no longer paying a {1}.
--
-- The SAME board manaSources is judged against serves the per-source yields
-- too, rather than a fresh projection per source on top of the sweep (#200);
-- see manaSources above for the hoist and its snapshot argument. That argument
-- is what makes handing the board in from outside change no answer: it is a
-- snapshot of one GameState, and this is a pure function of the same one.
--
-- A board is JOINTLY PAYABLE or it is no board: the activations it is made of
-- are several claims on one pool, so CR 118.3's "fully" reaches across the
-- sources exactly as it reaches across one cost's components -- one creature
-- cannot be sacrificed to both Ashnod's Altar and Phyrexian Tower, and counting
-- each source alone against the untouched board said it could (#1126). One
-- untapped creature cannot pay for two Springleaf Drums either, which is the same
-- reading on Pawl.Types.ClaimAxis' tapping axis.
--
-- `claimed` joins them, which is the same rule one level up: the cost being paid
-- has components of its own that spend objects, and CR 601.2g's mana window comes
-- BEFORE CR 601.2h's payment, so both happen and both need their own object --
-- unless they spend on different axes, which is what lets a creature tapped for
-- the Drum's mana still be sacrificed. Village Rites beside Phyrexian Tower and
-- one creature is the printed case -- the creature buys the {B} or pays the
-- additional cost, not both
-- (#1134). It is the whole cost's claims and not the remainder's, which is
-- exact: `Cost.canPay` asks this before any part of the cost is paid.
payableResolutionsGiven :: PaymentSubject.PaymentSubject -> Capacity -> ManaSpending -> [ObjectId] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> Natural -> [Claim] -> ManaCost -> GameState -> [([Demand], Natural, Natural)]
payableResolutionsGiven subject capacity spending sources pcs pid committed claimed cost gs =
  let -- CR 106.6, resolved for BOTH halves of the board: mana this payment may
      -- not spend is no supply for it, whether it is already in the pool or
      -- would be added by tapping a source (`spendableSupply` below). The two
      -- halves have to be filtered by the same question, or the offer and the
      -- payment disagree -- Mishra's Workshop's three colourless would buy a
      -- creature spell the payment then cannot pay for.
      (units, _) = spendableFor subject pid gs
      -- CR 609.4b, resolved ONCE for this whole question and applied to both
      -- halves of the board: a rewrite reaching the pool but not the untapped
      -- sources (or the other way round) would make this disagree with `spend`
      -- about what is payable, and the symptom is an action the engine offers and
      -- then cannot pay.
      clauses = PlayerEffect.spendManaAsThough pid gs
      pooled = fmap (rewriteSupply clauses . supplyOf) units
      suppliesPer = fmap (\oid -> fmap spendableSupply (manaSuppliesGiven capacity pcs pid oid gs)) sources
      -- One activation's yield narrowed to the units this payment could spend,
      -- keeping its COUNT and its own mana cost: a restricted yield is still an
      -- activation the board may take (its other units may serve), and taking it
      -- still costs what it costs.
      --
      -- What survives is admitted for the CAST and no further: CR 106.6's
      -- restriction is asked a SECOND time of the demands a source's own
      -- activation makes, which are no cast, and `Supply.supplyRestricted`
      -- carries the answer down to `admits` below. Pawl.ManaSpec's "CR 106.6 the
      -- restricted red cannot pay the Star's {1}" is what proves it.
      spendableSupply (activations, yield, manaCost) =
        (activations, Mana.MkMana (fst (spendableAmong subject pid gs (unitsOf yield))), manaCost)
      activationsOf (activations, _, _) = Activations.claims activations
      -- WHICH sources are worth taking fewer times: the ones whose claims meet
      -- another source's, or the cost's own. Asked GROUPWISE, one group per
      -- source plus `claimed`, so a source's own claims meeting each other is
      -- not contention -- Cost.repeatsOf has already measured that, and it is
      -- what `times` is.
      contested = Claim.contested (claimed : fmap (concatMap activationsOf) suppliesPer)
      options = fmap (\supplies -> sourceOptions clauses (Claim.contends contested (concatMap activationsOf supplies)) supplies) suppliesPer
      -- One option taken from each source, appended to the pool: `sequenceA` over
      -- the list applicative is that product, and it is [[]] -- one board, the
      -- pool alone -- when the player controls no source at all. Each board
      -- carries the LIFE its activations pay, which its CR 119.4 clause below is
      -- then asked about, and the MANA they eat, which its Hall clause does.
      --
      -- ORDERED by CR 605.3c, and that ordering is the whole of the acyclicity:
      -- once a player begins to activate a mana ability it cannot be activated
      -- again until it has resolved, so the mana-eating options a board takes
      -- happen one after another and never at once. The yield of one reaches the
      -- pool on resolution (CR 106.4), which is before the next one's cost is
      -- paid (CR 601.2g then CR 601.2h) -- so option i's supplies may serve
      -- option j's demand for i < j, and no option's supplies may serve its own.
      --
      -- Said as a POSITION on every supply and every demand, so the one Hall
      -- check below reads it: the pool and the mana-FREE options sit at 0, the
      -- k-th eating option puts both its supplies and its demands at k, and the
      -- cost being paid demands at `costPosition`, past every one of them. A
      -- supply serves a demand only from strictly earlier, which gives all three
      -- readings at once.
      --
      -- The SEARCH grows by the permutations of the eating options a board takes,
      -- and `orderings` walks exactly one where there are fewer than two -- which
      -- is every board a single mana-eating source can build.
      orderings taken = case taken of
        [] -> [[]]
        [_] -> [taken]
        _ -> List.permutations taken
      boards =
        [ ( fmap ((,) (0 :: Natural)) (pooled <> concatMap optionSupplies free)
              <> concat [fmap ((,) k) (optionSupplies option) | (k, option) <- ranked],
            concat [fmap ((,) k) (optionDemands option) | (k, option) <- ranked],
            costPosition,
            sum (fmap optionLife taken)
          )
        | taken <- sequenceA options,
          Claim.satisfiable (claimed <> concatMap optionClaims taken),
          let (eating, free) = List.partition (not . null . optionDemands) taken,
          order <- orderings eating,
          let ranked = zip [1 :: Natural ..] order,
          let costPosition = 1 + Natural.length eating
        ]
      payable (demands, generic, life) =
        let fits (supplies, eaten, costPosition, spent) =
              let -- Both sides tagged with their position, so Hall's condition
                  -- reads ONE bipartite graph over the whole board: the cost's
                  -- own demands sit past every option's, and an option's supplies
                  -- and demands share a position.
                  wanted_ = fmap ((,) costPosition) demands <> eaten
                  -- CR 106.6 asked of the demand rather than of the mana: a
                  -- restricted unit pays the payment this walk is about and
                  -- nothing else, so it serves the cost's own demands and never a
                  -- nested mana ability's activation cost.
                  --
                  -- Exact for a CAST and for a payment that is neither: a mana
                  -- restricted to casts can never pay an activation cost, and
                  -- one restricted at all can never pay for a special action.
                  -- Not implemented: mana whose restriction admits ACTIVATIONS
                  -- paying a nested mana ability's own cost, which CR 602.2b
                  -- makes an activation like any other -- Omen Hawker's {C} into
                  -- Chromatic Star's {1} is refused here, where
                  -- Pawl.Engine.Cost.payActivation's own subject would admit it.
                  -- The disagreement is the STRICT way round, so the gate offers
                  -- less than the payment would take rather than more (#2239).
                  admits (from, supply) (wantedAt, demand) =
                    serves supply demand
                      && from < wantedAt
                      && (not (supplyRestricted supply) || wantedAt == costPosition)
                  -- "The supplies that could serve this set of demands" and "the
                  -- demands in it", the two sides of Hall's condition for one
                  -- subset.
                  couldServe subset = length (filter (\supply -> any (admits supply) subset) supplies)
                  demandedIn subset = length (filter (`elem` subset) wanted_)
                  hallHolds subset = demandedIn subset <= couldServe subset
               in Event.canPayLife pid (committed + life + spent) gs
                    -- Clause 2 is untouched by the positions: every supply sits
                    -- strictly before `costPosition`, so every one of them can
                    -- serve a generic symbol of the cost.
                    && Natural.length supplies >= Natural.length wanted_ + generic
                    && all hallHolds (List.subsequences (List.nub wanted_))
         in any fits boards
   in filter payable (resolutions spending cost)

-- The least life any payable resolution of this cost costs, or Nothing when none
-- is payable. `resolutions` is sorted by life ascending and payableResolutions
-- keeps that order, so the head is the minimum.
--
-- This is the budget Pawl.Engine.Cost.payMana pays under. A cast, an activation
-- (`announce`, CR 118.13a) and a cost paid on resolution (CR 118.13b) have all
-- announced their Phyrexian symbols away by now, so this answers 0 for them; it
-- decides anything only where nothing announced (#1990, #1991).
--
-- Nothing committed and nothing claimed, unlike the gates: this runs DURING the
-- payment, where the cost's components may already have been paid, so the whole
-- cost's claims would be partly spent ones. Refusing an unpayable cost is the
-- gates' job (Pawl.Engine.Cost.canPay); all this picks is which resolution the
-- mana is spent under.
lifeNeeded :: PaymentSubject.PaymentSubject -> Capacity -> ManaSpending -> PlayerId -> ManaCost -> GameState -> Maybe Natural
lifeNeeded subject capacity spending pid cost gs = case payableResolutions subject capacity spending pid 0 [] cost gs of
  (_, _, life) : _ -> Just life
  [] -> Nothing
