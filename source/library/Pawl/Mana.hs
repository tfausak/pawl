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

-- Every mana type an object could produce: its intrinsic subtype mana (CR 305.6)
-- PLUS every projected activated ability that is a mana ability (CR 605.1a),
-- resolved inline at payment and never on the stack. Read through the projection
-- (abilitiesOf), so Humility (layer 6) strips a creature's mana ability too.
manaTypesOf :: ObjectId -> GameState -> [ManaType]
manaTypesOf oid gs =
  let fromSubtypes = Maybe.mapMaybe subtypeMana (Set.toList (Projection.subtypesOf oid gs))
      fromAbilities =
        concatMap
          (Maybe.mapMaybe Resolve.manaProduced . Modal.allEffects . ActivatedAbility.modal)
          (filter isManaAbility (Projection.abilitiesOf oid gs))
   in fromSubtypes <> fromAbilities

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

-- CR 500.4: each player's mana pool empties at the end of every step and phase.
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

-- Activate an object's intrinsic mana ability: tap it, add its mana. CR 605.3:
-- a mana ability does not use the stack, so this is immediate.
--
-- Takes the first mana type the object can produce. A Mountain produces exactly
-- one, so there is no choice to make. A source that can produce more than one
-- TYPE (any dual land) makes that a real decision needing its own prompt (#124).
-- Which SOURCE to tap is a separate question, and payCost asks it.
tapForMana :: ObjectId -> GameState -> GameState
tapForMana oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj -> case manaTypesOf oid gs of
    [] -> gs
    produced : _ ->
      let tapped = obj {Object.tapped = TapState.Tapped}
          gs1 = gs {GameState.objects = Map.insert oid tapped (GameState.objects gs)}
       in -- CR 109.4a/110.2: mana goes to the mana ability's controller, which is
          -- the permanent's controller (CR 106.4 only says it lands in "a
          -- player's mana pool", not whose). Falls back to owner in the
          -- impossible case lookupObject just proved oid exists but
          -- controllerOf returns Nothing.
          addMana (Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)) [ManaUnit.MkManaUnit {ManaUnit.manaType = produced}] gs1

typedOf :: ManaSymbol -> Maybe ManaType
typedOf symbol = case symbol of
  ManaSymbol.OfType t -> Just t
  ManaSymbol.Generic _ -> Nothing
  ManaSymbol.Variable -> Nothing

genericOf :: ManaSymbol -> Natural
genericOf symbol = case symbol of
  ManaSymbol.Generic n -> n
  ManaSymbol.OfType _ -> 0
  -- Unreachable in payment: substituteX removes every Variable before canPay
  -- (Task 4). The match must be total, so a bare {X} counts as 0 generic.
  ManaSymbol.Variable -> 0

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

takeTyped :: [ManaUnit] -> ManaType -> Maybe [ManaUnit]
takeTyped units wanted = case List.break (\u -> ManaUnit.manaType u == wanted) units of
  (before, _ : after) -> Just (before <> after)
  (_, []) -> Nothing

takeAny :: [ManaUnit] -> a -> Maybe [ManaUnit]
takeAny units _ = case units of
  _ : rest -> Just rest
  [] -> Nothing

-- Spend a pool against a cost. Nothing when the pool cannot cover it.
--
-- Typed symbols are matched FIRST because they are the constrained ones: generic
-- takes any unit, so paying it first could consume the only red and strand a
-- {R} that nothing else can satisfy.
spend :: ManaCost -> Mana -> Maybe Mana
spend cost (Mana.MkMana units) =
  let ManaCost.MkManaCost symbols = cost
      typed = Maybe.mapMaybe typedOf symbols
      generic = sum (fmap genericOf symbols)
   in do
        afterTyped <- Monad.foldM takeTyped units typed
        afterGeneric <- Monad.foldM takeAny afterTyped [1 .. generic]
        pure (Mana.MkMana afterGeneric)

-- CR 601.2g: "If the total cost includes a mana payment, the player then has a
-- chance to activate mana abilities." Reached from an ability too, by CR 602.2b
-- ("the remainder of the process ... is identical to ... 601.2b-i").
--
-- Returns whether it was paid; on failure nothing is spent, which is CR 601.2h's
-- "Partial payments are not allowed" rather than mere tidiness. The prompts
-- themselves are NOT rolled back -- they live in the Program, outside the state --
-- so a failed payment still asked its questions. Unreachable in practice, because
-- every caller pre-checks the pure canPay.
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
            State.put (tapForMana oid gs)
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
-- it while merely ENUMERATING actions, where prompting would be absurd. The
-- greedy walk is sound for the question it answers: every mana source in this
-- pool produces exactly one mana of one type (a source offering a colour CHOICE
-- is #124), so the order they are tapped in cannot change WHETHER the cost is
-- affordable -- only which permanents end up tapped, which is payCost's business
-- and the player's choice.
canPay :: PlayerId -> ManaCost -> GameState -> Bool
canPay pid cost gs =
  let tapUntilPaid remaining state = case spend cost (poolOf pid state) of
        Just _ -> True
        Nothing -> case remaining of
          [] -> False
          oid : rest -> tapUntilPaid rest (tapForMana oid state)
   in tapUntilPaid (manaSources pid gs) gs
