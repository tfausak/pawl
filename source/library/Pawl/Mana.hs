module Pawl.Mana where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Decide as Decide
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Summoning as Summoning
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.Mana (Mana)
import qualified Pawl.Type.Mana as Mana
import Pawl.Type.ManaCost (ManaCost)
import qualified Pawl.Type.ManaCost as ManaCost
import Pawl.Type.ManaProduction (ManaProduction)
import qualified Pawl.Type.ManaProduction as ManaProduction
import Pawl.Type.ManaSymbol (ManaSymbol)
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import Pawl.Type.ManaType (ManaType)
import qualified Pawl.Type.ManaType as ManaType
import Pawl.Type.ManaUnit (ManaUnit)
import qualified Pawl.Type.ManaUnit as ManaUnit
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import Pawl.Type.Subtype (Subtype)
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState

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

-- Every mana type an object could produce: its intrinsic subtype mana (CR 305.6)
-- PLUS every projected activated ability that is a mana ability (CR 605.1a),
-- resolved inline at payment and never on the stack. Read through the projection
-- (abilitiesOf), so Humility (layer 6) strips a creature's mana ability too.
--
-- These are OPTIONS, not a yield: the object produces ONE of them when tapped
-- (tapForMana), so a five-entry list is Birds of Paradise offering five colours,
-- not a source adding five mana. An ability whose one mode adds mana TWICE is
-- therefore misread here, and under-produces (#238) -- no card in the pool does
-- it.
--
-- Deduplicated. Two routes to the same type (an Urborg'd Swamp is a Swamp twice
-- over) are indistinguishable options producing an identical unit, so collapsing
-- them elides a prompt that has no content -- the one such elision that needs no
-- judgement about what "indistinguishable" means.
manaTypesOf :: ObjectId -> GameState -> [ManaType]
manaTypesOf oid gs =
  let fromSubtypes = Maybe.mapMaybe subtypeMana (Set.toList (Projection.subtypesOf oid gs))
      fromAbilities =
        concatMap
          (concatMap producedTypes . Maybe.mapMaybe Resolve.manaProduced . Modal.allEffects . ActivatedAbility.modal)
          (filter isManaAbility (Projection.abilitiesOf oid gs))
   in List.nub (fromSubtypes <> fromAbilities)

-- CR 605.1a: an activated ability is a mana ability if it could add mana AND
-- doesn't target (the loyalty clause is vacuous -- no planeswalkers). The ABI
-- predicate read at two sites: manaTypesOf includes a mana ability as a source
-- (Task 6); Action.legalActions excludes it from the stack (Task 5).
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
emptyManaPools :: GameState -> GameState
emptyManaPools gs = gs {GameState.manaPool = Map.empty}

-- Untapped permanents this player controls that can produce mana (CR 109.4a:
-- a mana ability's controller is determined as though it were on the stack --
-- i.e. the permanent's controller, CR 110.2 -- not the object's owner).
manaSources :: PlayerId -> GameState -> [ObjectId]
manaSources pid gs =
  let -- CR 302.6: a sick creature can't use a {T} mana ability. A land is never
      -- sick-gated. (M3e mana abilities all cost {T}.) Keyed to `pid`: the
      -- creature must have settled under the player reaching for the mana, not
      -- under whoever held it before (#198) -- and CR 702.10c's haste exemption
      -- applies here exactly as it does to any other {T} ability, which is what
      -- makes Act of Treason's rider pay off.
      notSickCreature oid =
        not (Set.member CardType.Creature (Projection.cardTypesOf oid gs))
          || Summoning.settledOrHasty pid oid gs
      isSource oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> Object.tapped obj == TapState.Untapped && not (null (manaTypesOf oid gs)) && notSickCreature oid
   in filter isSource (Projection.controls pid gs)

-- Activate an object's intrinsic mana ability: tap it, add its mana. CR 605.3b:
-- a mana ability does not use the stack, so this is immediate -- which is also
-- why the colour choice is made HERE and not by Resolve.
--
-- Monadic because of that choice. A Mountain produces exactly one type and is
-- never asked; a Birds of Paradise (CR 105.4) and an Urborg'd Mountain (CR
-- 305.6/305.7) both offer several, and the engine never picks for the player.
-- Which SOURCE to tap is a separate question, and payCost asks it.
tapForMana :: ObjectId -> Game ()
tapForMana oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case manaTypesOf oid gs of
      [] -> pure ()
      first : rest -> do
        -- CR 109.4a/110.2: mana goes to the mana ability's controller, which is
        -- the permanent's controller (CR 106.4 only says it lands in "a player's
        -- mana pool", not whose) -- and that same player makes the colour
        -- choice. Falls back to owner in the impossible case lookupObject just
        -- proved oid exists but controllerOf returns Nothing.
        let controller = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
        produced <- chooseManaType controller oid (first NonEmpty.:| rest) gs
        let tapped = obj {Object.tapped = TapState.Tapped}
            gs1 = gs {GameState.objects = Map.insert oid tapped (GameState.objects gs)}
        State.put (addMana controller [ManaUnit.MkManaUnit {ManaUnit.manaType = produced}] gs1)

-- Which type this source produces.
--
-- Elided exactly when the source offers ONE type, where no choice exists --
-- manaTypesOf has already collapsed duplicate routes to the same type, so a
-- remaining list of two is two genuinely different mana.
--
-- FILTERED, NOT TRUSTED, the posture chooseSource and Cost.payComponents take:
-- an answer outside the offered set is rejected and the head used instead. Here
-- that is not merely hygiene -- honouring a type the source cannot make would
-- mint mana out of nothing.
chooseManaType :: PlayerId -> ObjectId -> NonEmpty.NonEmpty ManaType -> GameState -> Game ManaType
chooseManaType pid oid candidates gs = case candidates of
  only NonEmpty.:| [] -> pure only
  _ -> do
    answer <- Trans.lift (Program.prompt (Prompt.ChooseManaType (Decide.deciderFor pid gs) pid oid candidates))
    pure $
      if List.elem answer (NonEmpty.toList candidates)
        then answer
        else NonEmpty.head candidates

-- Every way ONE symbol can be paid: a typed demand -- the SET of mana types one
-- unit of which satisfies it, and Nothing for a symbol that demands no particular
-- type -- paired with the generic mana that way adds.
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
-- The ways survive all the way to payment rather than collapsing to one as the
-- spell is proposed, which is what CR 118.13a and CR 601.2b actually call for and
-- what pawl does not do (#261).
waysOf :: ManaSymbol -> [(Maybe (Set.Set ManaType), Natural)]
waysOf symbol = case symbol of
  ManaSymbol.OfType t -> [(Just (Set.singleton t), 0)]
  -- CR 107.4e: a colour/colour hybrid is paid with one mana of a stated type
  -- either way, so it contributes nothing to the generic count.
  ManaSymbol.Hybrid a b -> [(Just (Set.fromList [a, b]), 0)]
  -- CR 107.4e's two ways, and the one-mana way is FIRST -- see resolutions.
  ManaSymbol.MonocoloredHybrid t -> [(Just (Set.singleton t), 0), (Nothing, 2)]
  ManaSymbol.Generic n -> [(Nothing, n)]
  -- Unreachable in payment: substituteX removes every Variable before canPay
  -- (Task 4). The match must be total, so a bare {X} demands nothing and counts
  -- as 0 generic.
  ManaSymbol.Variable -> [(Nothing, 0)]

-- CR 601.2b's "nonhybrid equivalent cost", enumerated: every way the whole cost
-- resolves into typed demands plus an amount of generic mana, one entry per
-- combination of per-symbol ways. `traverse` over the list applicative is that
-- product.
--
-- This is what lets everything below it keep the ONE-SUPPLY-PER-DEMAND shape
-- spendDemands and canPay's Hall condition both rest on. CR 107.4e's {2/B} is the
-- only symbol two mana can pay, and rather than teach those two about a demand
-- that consumes two units -- which would break the counting each of them does --
-- the choice is made here, above them. Each of them still sees a cost in which
-- every typed demand is exactly one mana.
--
-- Finite and small, which is what keeps the search terminating: the product over
-- symbols of their ways, so 2^(number of monocolored hybrid symbols), and exactly
-- one entry for every cost without one. Flame Javelin ({2/R}{2/R}{2/R}) is 8;
-- Reaper King ({2/W}{2/U}{2/B}{2/R}{2/G}) is 32.
--
-- ORDERED, and the order is a choice pawl makes for the player, because unlike a
-- colour/colour hybrid the two ways spend DIFFERENT AMOUNTS of mana and so leave
-- different pools behind. waysOf puts the one-mana way first, so `spend` takes
-- the resolution that spends the fewest units (#261).
resolutions :: ManaCost -> [([Set.Set ManaType], Natural)]
resolutions (ManaCost.MkManaCost symbols) =
  let collect ways = (Maybe.mapMaybe fst ways, sum (fmap snd ways))
   in fmap collect (traverse waysOf symbols)

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

-- Spend a pool against a cost. Nothing when the pool cannot cover it.
--
-- Typed symbols are matched FIRST because they are the constrained ones: generic
-- takes any unit, so paying it first could consume the only red and strand a
-- {R} that nothing else can satisfy.
--
-- The FIRST resolution that works wins. For every cost without a monocolored
-- hybrid there is only one, so this is the plain spend it always was; where there
-- are several, `resolutions` has already put the cheapest first.
spend :: ManaCost -> Mana -> Maybe Mana
spend cost (Mana.MkMana units) =
  let attempt (demands, generic) = do
        afterTyped <- spendDemands units demands
        Monad.foldM takeAny afterTyped [1 .. generic]
   in fmap Mana.MkMana (Maybe.listToMaybe (Maybe.mapMaybe attempt (resolutions cost)))

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
payCost :: PlayerId -> ManaCost -> Game Bool
payCost pid cost = do
  before <- State.get
  paid <- tapUntilPaid
  Monad.unless paid (State.put before)
  pure paid
  where
    tapUntilPaid = do
      gs <- State.get
      case spend cost (poolOf pid gs) of
        Just left -> do
          State.put (setPool pid left gs)
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

-- CR 118.3: can this cost be paid at all? Pure, because Action.legalActions asks
-- it while merely ENUMERATING actions, where prompting would be absurd -- so it
-- cannot simply walk tapForMana, which now asks a question.
--
-- It must not simulate one greedily either. Once a source can produce more than
-- one type, WHICH type each source makes decides whether the cost is affordable:
-- a Forest and a Birds of Paradise pay {G}{B}, but only if the Birds makes
-- black, and a greedy walk that tapped the Forest for green and took the Birds'
-- first colour would call it unaffordable.
--
-- So this is an assignment question, and it is answered exactly. Model each
-- available mana as a SUPPLY carrying the set of types it could be -- a pool
-- unit is its own type; an untapped source is everything manaTypesOf offers,
-- because tapping it yields exactly one mana of one of them. Each typed symbol
-- of the cost is a DEMAND for a specific type; generic symbols demand a count
-- and nothing more.
--
-- A RESOLVED cost is payable exactly when both hold:
--
--   1. every typed demand can be met at once -- a matching of demands into
--      supplies that saturates the demand side; and
--   2. enough supplies are left over for the generic part. Every full typed
--      matching consumes exactly one supply per typed symbol, so the leftover
--      count does not depend on WHICH matching, and this is a plain comparison.
--
-- Clause 1 is Hall's condition: a saturating matching exists iff no set of
-- demands outruns the supplies that could serve it. Demands of the same type
-- have identical options, so it is enough to check one demand set per SUBSET of
-- the types actually demanded -- at most 2^6 subsets by CR 106.1b, and in
-- practice a handful. Checked directly rather than by running a matching
-- algorithm: the condition IS the specification, so there is no gap between what
-- this says and what it does.
--
-- Both clauses count one supply per typed demand, which is exactly why CR
-- 107.4e's {2/B} is resolved AWAY before either is asked: `resolutions` turns the
-- cost into the nonhybrid equivalents, and the cost is payable when ANY of them
-- is. Neither clause has to learn about a demand two supplies satisfy, and
-- neither can be fooled into charging {2/B} a single mana.
canPay :: PlayerId -> ManaCost -> GameState -> Bool
canPay pid cost gs =
  let Mana.MkMana units = poolOf pid gs
      supplies =
        fmap (Set.singleton . ManaUnit.manaType) units
          <> fmap (\oid -> Set.fromList (manaTypesOf oid gs)) (manaSources pid gs)
      couldServe wanted = length (filter (not . Set.disjoint wanted) supplies)
      payable (demands, generic) =
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
         in Natural.length supplies >= Natural.length demands + generic
              && all (hallHolds . Set.fromList) (List.subsequences demandedTypes)
   in any payable (resolutions cost)
