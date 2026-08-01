module Pawl.Engine.Mana where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Summoning as Summoning
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
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
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.Subtype (Subtype)
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState

-- CR 305.6: a basic land's mana ability is granted intrinsically by its subtype,
-- not printed in its text box. This is a classification of the type line -- it
-- never reads the card's identity.
subtypeMana :: Subtype -> Maybe ManaType
subtypeMana subtype = case subtype of
  Subtype.Mountain -> Just (ManaType.Colored Color.Red)
  Subtype.Swamp -> Just (ManaType.Colored Color.Black)
  Subtype.Forest -> Just (ManaType.Colored Color.Green)
  Subtype.Island -> Just (ManaType.Colored Color.Blue)
  Subtype.Plains -> Just (ManaType.Colored Color.White)
  Subtype.Goblin -> Nothing
  Subtype.Warrior -> Nothing
  Subtype.Human -> Nothing
  Subtype.Bird -> Nothing
  Subtype.Ogre -> Nothing
  Subtype.Centaur -> Nothing
  Subtype.Cat -> Nothing
  Subtype.Dinosaur -> Nothing
  Subtype.Beast -> Nothing
  Subtype.Rat -> Nothing
  Subtype.Elephant -> Nothing
  Subtype.Myr -> Nothing
  Subtype.Skeleton -> Nothing
  Subtype.Wall -> Nothing
  Subtype.Wizard -> Nothing
  Subtype.Shapeshifter -> Nothing
  Subtype.Lhurgoyf -> Nothing
  Subtype.Arcane -> Nothing
  Subtype.Barbarian -> Nothing
  Subtype.Zombie -> Nothing
  Subtype.Fungus -> Nothing
  -- CR 205.3m: Elemental is a creature type, not a basic land type, so CR
  -- 305.6's intrinsic mana ability never applies to it.
  Subtype.Elemental -> Nothing
  -- CR 205.3m: Rogue is a creature type, not a basic land type, so CR 305.6's
  -- intrinsic mana ability never applies to it.
  Subtype.Rogue -> Nothing
  -- CR 205.3m: Hag is a creature type, not a basic land type, so CR 305.6's
  -- intrinsic mana ability never applies to it.
  Subtype.Hag -> Nothing
  -- CR 205.3m: Warlock is a creature type, not a basic land type, so CR
  -- 305.6's intrinsic mana ability never applies to it.
  Subtype.Warlock -> Nothing
  -- CR 205.3m: Soldier is a creature type, not a basic land type, so CR 305.6's
  -- intrinsic mana ability never applies to it.
  Subtype.Soldier -> Nothing
  -- CR 205.3m: Phyrexian is a creature type, not a basic land type, so CR
  -- 305.6's intrinsic mana ability never applies to it.
  Subtype.Phyrexian -> Nothing
  -- CR 205.3m: Elf is a creature type, not a basic land type, so CR 305.6's
  -- intrinsic mana ability never applies to it.
  Subtype.Elf -> Nothing
  -- CR 205.3m: Nightmare is a creature type, not a basic land type, so CR
  -- 305.6's intrinsic mana ability never applies to it.
  Subtype.Nightmare -> Nothing
  -- CR 205.3m: Horse is a creature type, not a basic land type, so CR 305.6's
  -- intrinsic mana ability never applies to it.
  Subtype.Horse -> Nothing
  -- CR 205.3h: Aura is an enchantment type. CR 305.6's intrinsic mana ability is
  -- a property of BASIC LAND types only, so this is Nothing for the same reason
  -- every creature type above is.
  Subtype.Aura -> Nothing
  -- CR 301.5: Equipment is an ARTIFACT type, so CR 305.6's intrinsic mana ability
  -- never applies to it either.
  Subtype.Equipment -> Nothing
  Subtype.Scout -> Nothing
  Subtype.Artificer -> Nothing
  Subtype.Troll -> Nothing
  Subtype.Nomad -> Nothing
  Subtype.Shaman -> Nothing
  Subtype.Demon -> Nothing
  Subtype.Cleric -> Nothing
  Subtype.Illusion -> Nothing
  -- CR 205.3m: Spirit is a creature type, not a basic land type, so CR 305.6's
  -- intrinsic mana ability never applies to it.
  Subtype.Spirit -> Nothing
  Subtype.Angel -> Nothing
  Subtype.Insect -> Nothing
  Subtype.Berserker -> Nothing
  Subtype.Thopter -> Nothing
  Subtype.Dragon -> Nothing
  Subtype.Unicorn -> Nothing
  Subtype.Curse -> Nothing
  -- CR 205.3i: Desert IS a land type, and the first one here that is not a BASIC
  -- land type -- "Of that list, Forest, Island, Mountain, Plains, and Swamp are
  -- the basic land types." CR 305.6 grants its intrinsic ability to "an object
  -- with the land card type and a basic land type", so a Desert gets nothing
  -- from its type line and prints its own "{T}: Add {C}" instead. This is the
  -- constructor Pawl.Engine.Subtype.isLandType answers True for and this one Nothing:
  -- the two questions finally differ.
  Subtype.Desert -> Nothing
  -- CR 205.3m: Faerie is a creature type (and, per CR 308.2, a kindred subtype),
  -- not a basic land type, so CR 305.6's intrinsic mana ability never applies to
  -- it.
  Subtype.Faerie -> Nothing
  Subtype.Rhino -> Nothing
  -- CR 205.3j: a planeswalker type, so CR 305.6's intrinsic mana ability --
  -- which reaches "an object with the land card type and a basic land type" --
  -- never applies to it.
  Subtype.Jace -> Nothing
  Subtype.Wraith -> Nothing
  Subtype.Mongoose -> Nothing

-- CR 105.4: "If a player is asked to choose a color, they must choose one of the
-- five colors. 'Multicolored' is not a color. Neither is 'colorless.'" So an
-- any-colour producer offers exactly five options and never {C} -- which is also
-- why AnyColor cannot be spelled as "every ManaType".
--
-- Written out rather than derived from a Bounded Color: the five are CR 105.1's
-- closed enumeration, and spelling them here keeps the rule citation next to the
-- list a reader has to check.
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
-- CR 106.12 narrows the phrase "tap for mana" to a mana ability "that includes
-- the {T} symbol in its activation cost", and NOTHING here reads a cost -- not
-- this function and not isManaAbility. Every mana ability in the pool costs
-- exactly {T} (manaSourcesGiven leans on the same fact for CR 302.6), so the
-- filter would change no answer; one that did not would be enumerated here as
-- though it tapped, which is the cost half of the same shortcut tapForMana takes
-- (#238).
--
-- The nesting is the whole point. The OUTER list is the options -- which ability
-- of this permanent, and which of its modes. The INNER list is that one
-- activation's YIELD, its AddMana effects in printed order (CR 608.2c). Sol
-- Ring's "{T}: Add {C}{C}" is one option adding two mana; Birds of Paradise is
-- one option adding one mana of a colour still to be chosen; an Urborg'd
-- Mountain is two options of one mana each.
--
-- Read through the projection (abilitiesOf), so Humility (layer 6) strips a
-- creature's mana ability too -- and so does CR 305.7 at layer 4, which is what
-- swaps a Blood Moon'd Reliquary Tower's printed "{T}: Add {C}" for the
-- Mountain's {R} rather than adding to it.
--
-- One route per mode, rather than one per combination of modes, because CR
-- 700.2's selection is "choose exactly one" for every mana ability in the pool.
-- An ability that chose two modes at once would make a route the CONCATENATION
-- of the chosen modes' yields, and the options the size-n subsets of its modes
-- (#449). A mode adding no mana contributes an empty route, which is a legal but
-- pointless activation and costs the reader nothing below: it is one more option
-- offering no mana, and the supply model ignores it.
--
-- Takes a PRE-PROJECTED board, because every caller asks this of every source a
-- player controls -- manaSources below and payableResolutions both do, and each
-- of the two projection reads here was a fresh gather per source (#200). See
-- Projection.projectGiven for what the board is and why passing Map.empty (which
-- manaYieldsOf does) is the same answer.
manaRoutesOfGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [[ManaProduction]]
manaRoutesOfGiven pcs oid gs =
  let fromSubtypes =
        fmap
          (\manaType -> [ManaProduction.OfType manaType])
          (Maybe.mapMaybe subtypeMana (Set.toList (Projection.subtypesGiven pcs oid gs)))
      modeRoutes ability =
        fmap (Maybe.mapMaybe Resolve.manaProduced) (Modal.modeEffects (ActivatedAbility.modal ability))
      fromAbilities = concatMap modeRoutes (filter isManaAbility (Projection.abilitiesGiven pcs oid gs))
   in fromSubtypes <> fromAbilities

-- What tapping this object for mana could actually put in a pool: every route
-- above with each of its ManaProduction choices resolved to a concrete type, so
-- one entry per (route, colour choice) pair. `traverse` over the list
-- applicative is that product -- Birds of Paradise's one route becomes CR
-- 105.4's five one-mana yields.
--
-- A Mana rather than a list of types because that is what a yield IS: some mana,
-- headed for a pool (CR 106.4). tapForMana adds the chosen one whole.
--
-- Deduplicated by the WHOLE yield. Two routes producing identical mana (an
-- Urborg'd Swamp is a Swamp twice over) are indistinguishable options -- no cost
-- and no rider tells two of this permanent's mana abilities apart yet (#238) --
-- so collapsing them elides a prompt with no content. Deliberately order-
-- sensitive: {R} then {B} and {B} then {R} are left as two options rather than
-- merged, which can only ever ASK where a set-valued dedup would not, and no
-- card in the pool produces either.
manaYieldsOf :: ObjectId -> GameState -> [Mana]
manaYieldsOf = manaYieldsOfGiven Map.empty

-- The same yields against a pre-projected board, which is manaRoutesOfGiven's
-- argument and carries its reason (#200).
manaYieldsOfGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [Mana]
manaYieldsOfGiven pcs oid gs =
  let asMana manaTypes = Mana.MkMana (fmap (\manaType -> ManaUnit.MkManaUnit {ManaUnit.manaType = manaType}) manaTypes)
   in List.nub (fmap asMana (concatMap (traverse producedTypes) (manaRoutesOfGiven pcs oid gs)))

-- The types in one yield, in printed order. Reading a Mana rather than spending
-- or adding one, which is what every other unwrap in this module does.
typesOf :: Mana -> [ManaType]
typesOf (Mana.MkMana units) = fmap ManaUnit.manaType units

-- CR 106.7's shape: every mana type this object COULD produce, flattened across
-- its yields and deduplicated. A strictly weaker question than manaYieldsOf --
-- it says nothing about how MUCH a single activation adds, so a Sol Ring answers
-- [{C}] here and yields {C}{C} there. Kept apart deliberately: the two were one
-- function while every source added exactly one mana, and conflating them is
-- what made Sol Ring read as a choice between two singles (#238).
manaTypesOf :: ObjectId -> GameState -> [ManaType]
manaTypesOf oid gs = List.nub (concatMap typesOf (manaYieldsOf oid gs))

-- CR 605.1a: an activated ability is a mana ability if it could add mana AND
-- doesn't target and is not itself a loyalty ability (CR 606.2, which
-- Pawl.Engine.Cost.isLoyaltyCost answers; no loyalty ability in the pool adds
-- mana, so the clause is inert rather than checked here). The ABI
-- predicate read at two sites: manaRoutesOfGiven includes a mana ability as a
-- source (Task 6); Action.legalActions excludes it from the stack (Task 5).
--
-- Asked of the WHOLE ability, across every mode -- CR 605.1a's "could add mana"
-- is satisfied by any mode that does, and CR 605.2 keeps it a mana ability even
-- where the game state stops it producing.
isManaAbility :: ActivatedAbility.ActivatedAbility Card.Card -> Bool
isManaAbility ab =
  not (null (Maybe.mapMaybe Resolve.manaProduced (Modal.allEffects (ActivatedAbility.modal ab))))
    && Map.null (Modal.allTargetSpecs (ActivatedAbility.modal ab))

-- CR 106.4. Absent from the map means an empty pool.
poolOf :: PlayerId -> GameState -> Mana
poolOf pid gs = Map.findWithDefault (Mana.MkMana []) pid (GameState.manaPool gs)

setPool :: PlayerId -> Mana -> GameState -> GameState
setPool pid pool gs = gs {GameState.manaPool = Map.insert pid pool (GameState.manaPool gs)}

addMana :: PlayerId -> [ManaUnit] -> GameState -> GameState
addMana pid units gs =
  let Mana.MkMana existing = poolOf pid gs
   in setPool pid (Mana.MkMana (existing <> units)) gs

-- CR 500.5: as a step or phase ends, any unspent mana left in a player's mana
-- pool empties -- a turn-based action that does not use the stack (CR 703.4q).
-- CR 106.4 supplies the wording for what that does to the player: they are said
-- to LOSE this mana, which is how every card that stops it is templated.
--
-- Being a turn-based action does not put it in Engine.runTurnBasedActions, which
-- handles a step's OPENING: CR 703.4q's own moment is the step's end, so
-- Engine.runStep calls this there instead.
--
-- The RETENTION check lives here rather than at that call site, because it is
-- part of the turn-based action and not part of the moment: CR 500.5 names one
-- action, "any unspent mana left in a player's mana pool empties", and which
-- mana that is belongs to the action. Engine.runStepThatBegan stays a line that
-- says only WHEN.
--
-- Asked PER PLAYER, off the CR 613.11 player-axis carrier, through a typed
-- question (PlayerEffect.keepsUnspentMana) that never reveals which effect
-- answered it. Absent from the map already means an empty pool (poolOf), so
-- filtering the map is the whole action: a player who keeps their mana keeps the
-- entry, and everyone else's is dropped.
--
-- `gs` is the state as the step ends, so the effect is read at that moment and
-- never captured earlier -- an Upwelling that left the battlefield during the
-- step is simply not there to find.
emptyManaPools :: GameState -> GameState
emptyManaPools gs =
  gs {GameState.manaPool = Map.filterWithKey (\pid _ -> PlayerEffect.keepsUnspentMana pid gs) (GameState.manaPool gs)}

-- Untapped permanents this player controls that can produce mana (CR 109.4a:
-- a mana ability's controller is determined as though it were on the stack --
-- i.e. the permanent's controller, CR 110.2 -- not the object's owner).
--
-- ONE control-grant walk and ONE whole-board projection for the whole sweep,
-- threaded into every question asked of every permanent -- the hoist
-- Sba.performStateBasedActions takes for the CR 704.3 sweep and
-- Projection.controls takes for the grant list. Unhoisted, each candidate cost as
-- many as four fresh Projection.gathers -- two for its mana routes alone
-- (manaRoutesOfGiven reads subtypes and abilities separately), one for the
-- card-type test and one for the haste read behind it -- plus a fresh grant
-- walk, which made a function the priority loop reaches at every boundary
-- quadratic in the battlefield (#200).
--
-- Projection.projectGiven carries the snapshot argument. It holds here because
-- this is a pure function of one GameState: nothing can move between the
-- projection and its uses, and payCost's loop -- the one caller that DOES change
-- the state, by tapping -- takes a fresh State.get and so a fresh board on every
-- pass.
manaSources :: PlayerId -> GameState -> [ObjectId]
manaSources pid gs = manaSourcesGiven (Projection.controlGrants gs) (Projection.projectAll gs) pid gs

manaSourcesGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
manaSourcesGiven grants pcs pid gs =
  let -- CR 302.6: a sick creature can't use a {T} mana ability. A land is never
      -- sick-gated. (M3e mana abilities all cost {T}.) Keyed to `pid`: the
      -- creature must have settled under the player reaching for the mana, not
      -- under whoever held it before (#198) -- and CR 702.10c's haste exemption
      -- applies here exactly as it does to any other {T} ability, which is what
      -- makes Act of Treason's rider pay off.
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
-- Monadic because of that choice. A Mountain offers exactly one yield and is
-- never asked; a Birds of Paradise (CR 105.4) and an Urborg'd Mountain (CR
-- 305.6/305.7) both offer several, and the engine never picks for the player.
-- Which SOURCE to tap is a separate question, and payCost asks it.
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
-- FILTERED, NOT TRUSTED, the posture chooseSource and Cost.payComponents take:
-- an answer outside the offered set is rejected and the head used instead. Here
-- that is not merely hygiene -- honouring a yield the source cannot make would
-- mint mana out of nothing.
chooseManaYield :: PlayerId -> ObjectId -> NonEmpty.NonEmpty Mana -> GameState -> Game Mana
chooseManaYield pid oid candidates gs = case candidates of
  only NonEmpty.:| [] -> pure only
  _ -> do
    answer <- Trans.lift (Program.prompt (Prompt.ChooseManaYield (Decide.deciderFor pid gs) pid oid candidates))
    pure $
      if List.elem answer (NonEmpty.toList candidates)
        then answer
        else NonEmpty.head candidates

-- Every way ONE symbol can be paid: a typed demand -- the SET of mana types one
-- unit of which satisfies it, and Nothing for a symbol that demands no particular
-- type -- paired with the generic mana that way adds and the LIFE it costs.
--
-- A set rather than a single type, because CR 107.4e's hybrid symbol "can be paid
-- in one of two ways". A plain `{R}` is the singleton case, so both payment paths
-- below read one shape and never case on hybrid-ness.
--
-- A LIST of ways, because CR 107.4e's other half is not a wider set but a
-- different SHAPE: "a monocolored hybrid symbol such as {2/B} can be paid with
-- either one black mana or two mana of any type", and one mana and two mana
-- cannot be the same demand. Every other symbol offers exactly one way, so the
-- list is a singleton everywhere else.
--
-- The LIFE field is CR 107.4f's Phyrexian symbol, and it is the one way that is
-- not a mana payment at all, so it could not be folded into either of the other
-- two: 2 life is not 2 generic mana (CR 118.7a's reductions never touch it, and
-- CR 202.3g values the symbol at 1 rather than 2), and it is no typed demand
-- because it consumes no supply. Zero for every other symbol.
--
-- CR 107.4f's ways are collapsed to ONE before payment, by announcePhyrexian,
-- which is what CR 118.13a and CR 601.2b call for -- so this enumeration reaches
-- payment only where nothing announced. CR 107.4e's hybrid ways do survive to
-- payment, which is the elision that remains (#261).
waysOf :: ManaSymbol -> [(Maybe (Set.Set ManaType), Natural, Natural)]
waysOf symbol = case symbol of
  ManaSymbol.OfType t -> [(Just (Set.singleton t), 0, 0)]
  -- CR 107.4e: a colour/colour hybrid is paid with one mana of a stated type
  -- either way, so it contributes nothing to the generic count.
  ManaSymbol.Hybrid a b -> [(Just (Set.fromList [a, b]), 0, 0)]
  -- CR 107.4e's two ways, and the one-mana way is FIRST -- see resolutions.
  ManaSymbol.MonocoloredHybrid t -> [(Just (Set.singleton t), 0, 0), (Nothing, 2, 0)]
  -- CR 107.4f's two ways: "a cost that can be paid either with one mana of its
  -- color or by paying 2 life." The colour is a ManaType here because that is
  -- what a demand is made of; CR 107.4f admits no colourless Phyrexian symbol,
  -- so ManaType.Colored is total rather than a case.
  --
  -- The mana way is FIRST, which resolutions' sort then keeps -- see there.
  ManaSymbol.Phyrexian c -> [(Just (Set.singleton (ManaType.Colored c)), 0, 0), (Nothing, 0, 2)]
  ManaSymbol.Generic n -> [(Nothing, n, 0)]
  -- Unreachable in payment: substituteX removes every Variable before canPay
  -- (Task 4). The match must be total, so a bare {X} demands nothing and counts
  -- as 0 generic.
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
-- the choice is made here, above them. Each of them still sees a cost in which
-- every typed demand is exactly one mana. CR 107.4f's {G/P} is absorbed the same
-- way and for the same reason: a symbol payable with no mana at all would be a
-- demand nothing serves, so the life is lifted out here and neither of them ever
-- sees it.
--
-- Finite and small, which is what keeps the search terminating: the product over
-- symbols of their ways, so 2^(number of monocolored hybrid and Phyrexian
-- symbols), and exactly one entry for every cost without one. Flame Javelin
-- ({2/R}{2/R}{2/R}) is 8; Reaper King ({2/W}{2/U}{2/B}{2/R}{2/G}) is 32;
-- Mutagenic Growth ({G/P}) is 2.
--
-- ORDERED, and the order is a choice pawl makes for the player, because unlike a
-- colour/colour hybrid these ways are DISTINGUISHABLE -- they spend different
-- amounts of mana, or mana against life, and so leave a different board behind.
-- Two rules, in this priority:
--
--   1. LEAST LIFE first, by the sort. CR 107.4f's life is the resource that does
--      not come back: an unspent pool empties every step (CR 500.5) and a land
--      untaps, so preferring mana can never cost the player something they keep.
--      Conservative, and still pawl choosing -- which is why a cast or an
--      activation announces first (announcePhyrexian, CR 118.13a) and reaches
--      this sort with no Phyrexian symbol left to order. A cost paid during a
--      resolution or for a special action has no such announcement (#373).
--   2. Among equal life, FEWEST UNITS, which waysOf's per-symbol order already
--      gives and a STABLE sort preserves -- so a monocolored hybrid's behaviour
--      is exactly what it was (#261).
--
-- The sort is what makes rule 1 hold across symbols rather than within one: for
-- {2/R}{G/P} the product alone would offer a 2-life way before a 0-life one.
resolutions :: ManaCost -> [([Set.Set ManaType], Natural, Natural)]
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
removals :: Set.Set ManaType -> [ManaUnit] -> [[ManaUnit]]
removals wanted units = Maybe.mapMaybe without (zip [0 :: Int ..] units)
  where
    without (i, u) =
      if Set.member (ManaUnit.manaType u) wanted
        then Just (take i units <> drop (i + 1) units)
        else Nothing

-- Spend one unit per demand, or Nothing if no assignment covers them all.
--
-- EXACT, by search, and not the fold this replaced. A greedy left-to-right match
-- is correct only while every demand has exactly one option, because then the
-- order it consumes units in cannot matter. CR 107.4e's hybrid breaks that:
--
--   pool {R}{G}, cost {R/G}{R} -- greedy hands the {R} unit to the hybrid, then
--   the {R} demand fails with a {G} still in the pool. The assignment that works
--   gives the hybrid the {G}.
--
-- So this backtracks: try each unit that could serve the first demand, recurse,
-- and keep the first assignment that covers the rest. A mana cost is a handful of
-- symbols, so the search is trivially small, and being exact means canPay's Hall
-- condition and this never disagree about whether a cost is payable.
spendDemands :: [ManaUnit] -> [Set.Set ManaType] -> Maybe [ManaUnit]
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
-- hybrid or a Phyrexian symbol there is only one, so this is the plain spend it
-- always was; where there are several, `resolutions` has already put the
-- least-life, fewest-units one first.
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

-- CR 601.2g: "If the total cost includes a mana payment, the player then has a
-- chance to activate mana abilities." Reached from an ability too, by CR 602.2b
-- ("the remainder of the process ... is identical to ... 601.2b-i").
--
-- Returns whether it was paid; on failure nothing is spent, which is CR 601.2h's
-- "Partial payments are not allowed" rather than mere tidiness. The prompts
-- themselves are NOT rolled back -- they live in the Program, outside the state --
-- so a failed payment still asked its questions.
--
-- Failure is REACHABLE, and a source offering a colour choice is what makes it
-- so. canPay asks whether SOME sequence of choices pays the cost; this asks the
-- player to make them. A player who taps their only Birds of Paradise for green
-- cannot then pay {B}, and the engine must let them: choosing badly is a choice,
-- and second-guessing it here would be the engine playing the game. While every
-- source produced one fixed type the two questions coincided, and this comment
-- claimed the failure was unreachable. Proved reachable by ManaSpec's "a Birds
-- tapped for green does not pay {B}".
--
-- One prompt per source tapped, against a shrinking candidate list, rather than
-- one prompt for a whole subset: a cost needing {G}{G} is two decisions, and the
-- second is made knowing the first.
--
-- The life budget only ever binds a cost NOTHING ANNOUNCED for. A cast (CR
-- 601.2b) and an activation (CR 602.2b) both run announcePhyrexian first, so the
-- cost arriving here holds no Phyrexian symbol, every resolution of it costs 0
-- life, and the budget is 0 -- CR 107.4f's life is a Pawl.Engine.Cost component by then.
-- What is left under the budget is CR 118.13b/c, a cost paid during a resolution
-- or for a special action, where pawl still chooses (#373).
--
-- It is recomputed on EVERY pass rather than fixed at entry, because a tap can
-- change it: a Birds of Paradise tapped for blue takes the mana way to an
-- unannounced {G/P} off the board, and the cost is then payable only by CR
-- 107.4f's 2 life. Recomputing means pawl pays it, rather than failing the
-- payment the way the paragraph above lets a mis-tapped {B} fail -- the same MORE
-- PERMISSIVE posture #261 records. Zero when the cost is unpayable outright,
-- which leaves the loop exactly as it was -- spend fails, sources are exhausted
-- one prompt at a time, and the answer is False.
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
      case spend budget cost (poolOf pid gs) of
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
-- exactly one -- where no choice exists to make. That is a deliberately blunt
-- reading of CLAUDE.md's "eliding a prompt is legitimate only for
-- indistinguishable options": it never has to be right about what
-- indistinguishable MEANS.
--
-- The obvious cheaper rule -- elide when every candidate is a copy of the same
-- card -- is unsound in this pool, and the counterexamples are ordinary. Two
-- Llanowar Elves are one card, but one may be equipped with Bonesplitter or
-- enchanted, one may carry +1/+1 counters (Battlegrowth, Longtusk Cub), one may
-- be borrowed until end of turn by Act of Treason, and one may be blocking (CR
-- 506.4 does not remove a creature from combat for tapping). Each of those makes
-- the choice real. `Game.cardOf` compares PRINTED identity and cannot see any of
-- it, so that rule would suppress exactly the prompts the invariant exists to
-- force. An extra question is cheap; a missing one is the engine deciding (#217).
--
-- FILTERED, NOT TRUSTED, the posture Combat.declareAttackers and
-- Cost.payComponents already take: an answer outside the offered set is rejected
-- and the head is used instead. That is not only hygiene -- tapForMana is a no-op
-- on an unknown or mana-less id, so honouring a bogus answer would leave the
-- state unchanged and loop forever.
chooseSource :: PlayerId -> NonEmpty.NonEmpty ObjectId -> GameState -> Game ObjectId
chooseSource pid candidates gs = case candidates of
  only NonEmpty.:| [] -> pure only
  _ -> do
    answer <- Trans.lift (Program.prompt (Prompt.ChooseManaSource (Decide.deciderFor pid gs) pid candidates))
    pure $
      if List.elem answer (NonEmpty.toList candidates)
        then answer
        else NonEmpty.head candidates

-- CR 107.4f: "...or by paying 2 life." The one place that number is written.
phyrexianLife :: Natural
phyrexianLife = 2

-- CR 601.2b: "If a cost that will be paid as the spell is being cast includes
-- Phyrexian mana symbols, the player announces whether they intend to pay 2 life
-- or a corresponding colored mana cost for each of those symbols." CR 118.13a is
-- what places that announcement: it "is made as its controller proposes that
-- spell or ability", NOT when the cost is paid. CR 602.2b sends an activated
-- ability's cost through the same rule.
--
-- Returns CR 601.2b's own phrase -- the "nonhybrid equivalent cost", a mana cost
-- with no Phyrexian symbol left in it -- and the life the announcement committed.
-- Pawl.Engine.Cost.announce is what turns that life into CR 119.4's payment, so nothing
-- below this function ever sees a mana symbol that spends no mana.
--
-- ONE SYMBOL AT A TIME, in printed order, each question asked knowing the answers
-- before it -- the shape payCost's source prompts already take. What makes that
-- sound is the OFFER: a route is offered only if the whole cost is still payable
-- after taking it, so an earlier answer may narrow a later one's options but can
-- never strand the payment. That is also CR 601.2b's last sentence, arriving as a
-- board rather than as a rule: "previously made choices ... may restrict the
-- player's options when making these choices."
--
-- Measured through `total`, which is CR 601.2f's totalling supplied by the caller
-- -- because the cost that decides whether a route is payable is the one that will
-- actually be paid, not the printed one. CR 601.2b comes first and 601.2f second,
-- so the ANNOUNCEMENT stays here; only the payability probe reaches forward. Get
-- that wrong and a cost reduction hides a route the player was entitled to and
-- this function elides the prompt, which is the engine choosing -- proved by
-- ManaSpec's "CR 601.2f a reduction opens the coloured-mana route, so the
-- announcement is asked".
--
-- Every remaining announcement is COMPLETED before totalling (`completions`),
-- because CR 601.2f is defined over a nonhybrid equivalent cost: a reduction that
-- names a mana type can only cancel a symbol some announcement has committed to
-- mana, so leaving the tail's symbols Phyrexian would silently withhold reductions
-- a later answer is entitled to.
--
-- Measured against the BOARD and not the pool: canPayCommitting counts an
-- untapped source as the mana it could make (payableResolutions), so a Forest
-- still in play offers the mana route before anything is tapped. A player who
-- then taps it for something else fails the payment, which is CR 601.2h's
-- business and not this function's -- announcing is not producing.
--
-- FILTERED, NOT TRUSTED, the chooseSource posture: an answer outside the offered
-- set is rejected and the head used instead.
announcePhyrexian :: PlayerId -> ObjectId -> (ManaCost -> ManaCost) -> ManaCost -> Game (ManaCost, Natural)
announcePhyrexian pid oid total (ManaCost.MkManaCost symbols) = go [] 0 symbols
  where
    go done committed remaining = case remaining of
      [] -> pure (ManaCost.MkManaCost (reverse done), committed)
      ManaSymbol.Phyrexian color : rest -> do
        gs <- State.get
        let asMana = ManaSymbol.OfType (ManaType.Colored color)
            -- "Payable" here means "SOME completion of the remaining
            -- announcements pays it", which is what CR 601.2b's last sentence
            -- makes the question. Enumerated here rather than left to canPay's own
            -- `resolutions` so that each completion is a Phyrexian-free cost by
            -- the time `total` sees it.
            stillPayable extra ways =
              let candidate (tail_, life) =
                    canPayCommitting pid (extra + life) (total (ManaCost.MkManaCost (reverse done <> ways <> tail_))) gs
               in any candidate (completions rest)
            offers =
              [PhyrexianPayment.PaysMana | stillPayable committed [asMana]]
                <> [PhyrexianPayment.PaysLife | stillPayable (committed + phyrexianLife) []]
        announced <- case offers of
          -- Neither route pays, so the cost is unpayable and there is nothing to
          -- announce: the printed symbol stands and the payment fails (CR 118.6,
          -- CR 601.2h).
          --
          -- Reachable only from a caller announcing on a cost Cast.castable and
          -- Activate.activatable never admitted, and it is `total` above that makes
          -- that true rather than merely plausible: both gates measure payability
          -- through the same totalling, so no completion payable here can be one
          -- they refused, and none they admitted can be missing here. The one
          -- remaining wedge is {X}: Cast.payableCost gates at X=0 while the
          -- announcement runs on the value the player named, and no card in the
          -- pool prints {X} beside a Phyrexian symbol (#417).
          [] -> pure PhyrexianPayment.PaysMana
          [only] -> pure only
          first : others -> do
            let prompt = Prompt.AnnouncePhyrexianPayment (Decide.deciderFor pid gs) pid oid color (first NonEmpty.:| others)
            answer <- Trans.lift (Program.prompt prompt)
            pure (if List.elem answer offers then answer else first)
        case announced of
          PhyrexianPayment.PaysMana -> go (asMana : done) committed rest
          PhyrexianPayment.PaysLife -> go done (committed + phyrexianLife) rest
      other : rest -> go (other : done) committed rest

-- Every way the Phyrexian symbols of a cost's UNANNOUNCED tail could go, as the
-- symbols that leaves and the life those choices commit -- CR 601.2b's own
-- "nonhybrid equivalent cost", one entry per combination. Exactly the product
-- `resolutions` takes over CR 107.4f's ways, lifted to the SYMBOL level so that a
-- caller can hand each completion to CR 601.2f's totalling before asking whether
-- it is payable.
--
-- One entry for a Phyrexian-free tail, so this is the identity case for every cost
-- announcePhyrexian leaves untouched, and 2^(number of Phyrexian symbols)
-- otherwise -- the same small bound resolutions already carries. Non-Phyrexian
-- symbols ride through in place, which is what keeps a completion a cost and not
-- merely a set of choices.
completions :: [ManaSymbol] -> [([ManaSymbol], Natural)]
completions symbols = case symbols of
  [] -> [([], 0)]
  ManaSymbol.Phyrexian color : rest ->
    let asMana = ManaSymbol.OfType (ManaType.Colored color)
     in [(asMana : tail_, life) | (tail_, life) <- completions rest]
          <> [(tail_, life + phyrexianLife) | (tail_, life) <- completions rest]
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

-- One untapped source's contribution to the supply side, as a supply per mana it
-- would add: the Nth supply is every type the Nth mana of any of its yields
-- could be. `transpose` is exactly that read, and its ragged case -- yields of
-- different LENGTHS -- takes the longest, so a source is counted for the most
-- mana any one activation of it adds.
--
-- EXACT wherever the pool reaches it, and the two cases are worth separating. A
-- source with ONE yield (Sol Ring, Birds of Paradise, a Forest) has nothing to
-- mix, and a source whose yields are all ONE mana (an Urborg'd Mountain) has one
-- supply carrying their union, which is what the whole model was before Sol Ring.
-- What it OVER-counts is a source offering several multi-mana yields: transposing
-- lets the first mana come from one yield and the second from another, and
-- letting the longest yield set the count credits a source with mana the yield
-- the player actually picks may not add. Over-permissive, deliberately -- an
-- unpayable payment fails and rolls back (CR 601.2h, payCost), while
-- under-counting would withhold an action the player was entitled to. No card in
-- the pool has two yields of which either adds more than one mana (#450).
sourceSupplies :: [Mana] -> [Set.Set ManaType]
sourceSupplies yields = fmap Set.fromList (List.transpose (fmap typesOf yields))

-- The resolutions of `cost` this player could actually pay right now, in
-- `resolutions`' order -- so the head costs the least life of any of them, which
-- is what lifeNeeded reads. NOT the resolution `spend` will take: `spend` walks
-- the same list against the POOL alone, where a resolution this one admits on the
-- strength of an untapped source may not yet fit. What the two agree on is the
-- life, because the budget is what carries between them.
--
-- The mana part must not be simulated greedily. Once a source can produce more
-- than one type, WHICH type each source makes decides whether the cost is
-- affordable: a Forest and a Birds of Paradise pay {G}{B}, but only if the Birds
-- makes black, and a greedy walk that tapped the Forest for green and took the
-- Birds' first colour would call it unaffordable.
--
-- So it is an assignment question, and it is answered exactly. Model each
-- available mana as a SUPPLY carrying the set of types it could be -- a pool
-- unit is its own type; an untapped source contributes ONE SUPPLY PER MANA IT
-- ADDS, which is what sourceSupplies below reads off its yields. Each typed
-- symbol of the cost is a DEMAND for a specific type; generic symbols demand a
-- count and nothing more.
--
-- A RESOLVED cost is payable exactly when all three hold:
--
--   1. every typed demand can be met at once -- a matching of demands into
--      supplies that saturates the demand side;
--   2. enough supplies are left over for the generic part. Every full typed
--      matching consumes exactly one supply per typed symbol, so the leftover
--      count does not depend on WHICH matching, and this is a plain comparison;
--      and
--   3. CR 119.4's floor admits the life -- this resolution's own, PLUS whatever
--      an announcement in progress has already committed (`committed`, zero for
--      every caller but announcePhyrexian). This is the clause that reads the
--      PLAYER rather than the board, and the only one a Phyrexian-free cost can
--      never fail: every resolution of such a cost costs 0 life, and CR 119.4b
--      lets anyone pay that -- see canPayLife, which answers 0 without a lookup
--      precisely so that this holds for a player the map does not hold either.
--
-- Clause 1 is Hall's condition: a saturating matching exists iff no set of
-- demands outruns the supplies that could serve it. Demands of the same type
-- have identical options, so it is enough to check one demand set per SUBSET of
-- the types actually demanded -- at most 2^6 subsets by CR 106.1b, and in
-- practice a handful. Checked directly rather than by running a matching
-- algorithm: the condition IS the specification, so there is no gap between what
-- this says and what it does.
--
-- Clauses 1 and 2 count one supply per typed demand, which is exactly why CR
-- 107.4e's {2/B} is resolved AWAY before either is asked: `resolutions` turns the
-- cost into the nonhybrid equivalents, and the cost is payable when ANY of them
-- is. Neither clause has to learn about a demand two supplies satisfy, and
-- neither can be fooled into charging {2/B} a single mana. CR 107.4f's {G/P}
-- rides on the same enumeration: its life way is a resolution with one fewer
-- demand, so neither clause has to learn about a symbol that consumes no supply
-- at all.
payableResolutions :: PlayerId -> Natural -> ManaCost -> GameState -> [([Set.Set ManaType], Natural, Natural)]
payableResolutions pid committed cost gs =
  let Mana.MkMana units = poolOf pid gs
      -- The SAME board manaSources is judged against serves the per-source
      -- yields too, rather than a fresh projection per source on top of the sweep
      -- (#200); see manaSources above for the hoist and its snapshot argument.
      grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
      supplies =
        fmap (Set.singleton . ManaUnit.manaType) units
          <> concatMap (\oid -> sourceSupplies (manaYieldsOfGiven pcs oid gs)) (manaSourcesGiven grants pcs pid gs)
      couldServe wanted = length (filter (not . Set.disjoint wanted) supplies)
      payable (demands, generic, life) =
        let -- A demand belongs to the set W exactly when every type that could
            -- satisfy it is in W -- `isSubsetOf`, where a single-type demand only
            -- needed `member`. That is the whole generalization CR 107.4e's
            -- hybrid asks of Hall's condition: the demand side gained
            -- option-sets, and the condition is still "no set of demands outruns
            -- the supplies that could serve it".
            demandedIn wanted = length (filter (`Set.isSubsetOf` wanted) demands)
            hallHolds wanted = demandedIn wanted <= couldServe wanted
            -- Enumerated over TYPES, not over demands: taking W = the union of a
            -- demand set's options recovers the worst case for that set, so
            -- subsets of the demanded types cover every subset of demands. At
            -- most 2^6 by CR 106.1b, unchanged by hybrids.
            demandedTypes = Set.toList (Set.unions demands)
         in canPayLife pid (committed + life) gs
              && Natural.length supplies >= Natural.length demands + generic
              && all (hallHolds . Set.fromList) (List.subsequences demandedTypes)
   in filter payable (resolutions cost)

-- The least life any payable resolution of this cost costs, or Nothing when none
-- is payable. `resolutions` is sorted by life ascending and payableResolutions
-- keeps that order, so the head is the minimum.
--
-- This is the budget payCost pays under. A cast or an activation has already
-- announced its Phyrexian symbols away (announcePhyrexian, CR 118.13a), so this
-- answers 0 for them; it decides anything only where nothing announced (#373).
lifeNeeded :: PlayerId -> ManaCost -> GameState -> Maybe Natural
lifeNeeded pid cost gs = case payableResolutions pid 0 cost gs of
  (_, _, life) : _ -> Just life
  [] -> Nothing

-- CR 119.4: "If a cost or effect allows a player to pay an amount of life greater
-- than 0, the player may do so only if their life total is greater than or equal
-- to the amount of the payment."
--
-- Lives here rather than in Pawl.Engine.Cost so that CR 107.4f's Phyrexian symbol and
-- CR 119.4's own PayLife component share one reading of the rule; Pawl.Engine.Cost
-- imports this module, so the dependency only goes one way.
--
-- CR 119.4b is answered BEFORE the lookup, not by the `>=` that would usually
-- absorb it: "Players can always pay 0 life, no matter what their (or their
-- team's) life total is, and even if an effect says players can't pay life."
-- ALWAYS, so a player the map does not hold must not turn a zero payment into an
-- unpayable one -- and every resolution of a cost with no Phyrexian symbol is a
-- zero payment, so this is also what keeps payableResolutions' life clause
-- unable to change any answer such a cost used to give.
canPayLife :: PlayerId -> Natural -> GameState -> Bool
canPayLife pid n gs =
  n == 0 || case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Player.life player >= toInteger n

-- CR 119.4: "the payment is subtracted from their life total; in other words, the
-- player loses that much life." A direct subtraction, and the CR 704.5a
-- state-based action that may follow is the existing one in Pawl.Engine.Sba -- paying to
-- exactly 0 is a legal payment, not a barred one.
payLife :: PlayerId -> Natural -> GameState -> GameState
payLife pid n gs =
  gs
    { GameState.players =
        Map.adjust (\p -> p {Player.life = Player.life p - toInteger n}) pid (GameState.players gs)
    }
