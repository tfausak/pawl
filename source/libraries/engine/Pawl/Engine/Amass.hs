-- | CR 701.47, "amass [subtype] N": the Army token the rule prints, and the whole
-- of the keyword action.
--
-- Pawl.Engine.Ring's sibling, and standing on the same ground: rule 701 is a
-- keyword-action rule exactly as rule 702 is a keyword rule, so minting the token
-- from CR 701.47a here is Pawl.Engine.Keyword's standing to mint an ability from a
-- Keyword. The closed\/open invariant forbids the rules core casing on an EFFECT's
-- identity, and nothing here does: Pawl.Engine.Resolve's Effect.Amass arm calls
-- `amass` and asks nothing about which effect it came from.
module Pawl.Engine.Amass where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Subtype as Subtype
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.Modification as Modification
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Subtype as Subtype.Type
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TypeLine as TypeLine

-- | CR 701.47a's token: a 0\/0 black [subtype] Army creature. Minted here rather
-- than carried in card data, on Pawl.Engine.Keyword's terms -- its characteristics
-- are printed in the comprehensive rules, not on Relentless Advance.
--
-- CR 111.4 supplies the name from the subtypes, so a Zombie Army token is named
-- "Zombie Army". The word comes from Pawl.Engine.Subtype.creatureTypeWord, the same
-- list CR 205.3m holds, so a text-changed amass (CR 612.2a) mints a token named for
-- the NEW word -- Pawl.Engine.Projection.rewriteEffect has already swapped the
-- subtype by the time this is called. Nothing there means card data wrote a subtype
-- of another family, which no printing does; the Army half of the name stands
-- alone in that case rather than this being partial.
--
-- Black is a colorIndicator (CR 202.2e), the Servo token's shape with a colour in
-- it. The 0\/0 is exactly what rule 701.47a prints: the counters land before any
-- state-based action is checked (CR 704.3), so an amass of one or more leaves a
-- creature that survives.
armyToken :: Subtype.Type.Subtype -> Card.Card
armyToken subtype =
  Card.MkCard
    { Card.layout = Layout.Normal,
      Card.faces =
        NonEmpty.singleton
          Face.MkFace
            { Face.name =
                CardName.MkCardName
                  ( Text.unwords
                      (Maybe.maybeToList (Subtype.creatureTypeWord subtype) <> [Text.pack "Army"])
                  ),
              Face.manaCost = Nothing,
              Face.typeLine =
                TypeLine.MkTypeLine
                  Set.empty
                  (Set.singleton CardType.Creature)
                  (Set.fromList [subtype, Subtype.Type.Army]),
              Face.power = Just (Power.MkPower (Quantity.Literal 0)),
              Face.toughness = Just (Toughness.MkToughness (Quantity.Literal 0)),
              Face.loyalty = Nothing,
              Face.defense = Nothing,
              Face.keywords = Set.empty,
              Face.colorIndicator = Set.singleton Color.Black,
              Face.characteristicPT = Nothing,
              Face.staticAbilities = [],
              Face.spell = Face.defaultSpell,
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.rooms = Seq.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.enchant = [],
              Face.counterability = Counterability.Counterable,
              Face.additionalCosts = [],
              Face.alternativeCosts = [],
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.blockPermissions = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.sacrificeRestrictions = [],
              Face.untapRestrictions = [],
              Face.attackCosts = [],
              Face.mulliganActions = [],
              Face.openingHandActions = [],
              Face.specialActions = []
            }
    }

-- | CR 701.47a's "an Army creature you control": the pool the choice is made out
-- of, ascending so that both the single-candidate shortcut and a transcript are
-- deterministic (Pawl.Engine.Ring.tempt's posture).
--
-- BOTH conjuncts are the rule's. Creature-ness and the Army type are the PROJECTED
-- questions (CR 613.1d's layer 4), so an Army that has stopped being a creature is
-- not a candidate and a creature that has been made an Army by something else is.
armiesOf :: PlayerId -> GameState.GameState -> [ObjectId]
armiesOf pid gs =
  List.sort
    ( filter
        (\oid -> Projection.isCreatureOf oid gs && Set.member Subtype.Type.Army (Projection.subtypesOf oid gs))
        (Projection.controls pid gs)
    )

-- | CR 701.47a: a player amasses [subtype] N. The whole keyword action, in the
-- order rule 701.47a fixes -- the token BEFORE the choice, so a player who
-- controlled no Army is choosing among the one they now do.
--
-- Runs to the end whatever happens in the middle, which is CR 701.47b: a player
-- "amassed" once these actions are complete, "even if some or all of those actions
-- were impossible". Nothing here stops early, and the empty pool is a no-op rather
-- than a failure.
--
-- The four instructions in order:
--
-- 1. CR 701.47a's condition is on controlling an ARMY CREATURE, not on having
--    amassed before, so a player whose Army died gets a second token.
-- 2. The choice, via Prompt.ChooseAmass, and only when there is one to make.
-- 3. The counters, through Event.putCounters -- the single funnel (CR 122.6), so
--    CR 614.16's counter replacements (Hardened Scales, Doubling Season) get their
--    opportunity.
-- 4. CR 205.1b's type addition, as an ordinary timestamped layer-4 continuous
--    effect with Expiry.Never: rule 701.47a states no duration, so nothing ends
--    it. Skipped when the Army already has the subtype, which is the rule's own
--    "if it isn't a [subtype]" and not an optimisation -- the projected types are
--    what that reads.
--
-- Not implemented: nothing records that an amass happened, so CR 701.47c's "the
-- amassed Army" cannot be named by a later effect and CR 701.47b's completion is
-- not observable (#1484).
amass :: PlayerId -> ObjectId -> ObjectId -> Subtype.Type.Subtype -> Natural -> Game ()
amass pid source resolving subtype n = do
  gs0 <- State.get
  -- CR 701.47a, first: the token, and only if they control no Army creature.
  Monad.when (null (armiesOf pid gs0))
    . Monad.void
    $ Event.createTokens pid (armyToken subtype) Nothing 1 TapState.Untapped Map.empty
  gs1 <- State.get
  case armiesOf pid gs1 of
    -- CR 701.47b: an impossible choice is not a failed amass.
    [] -> pure ()
    first : rest -> do
      chosen <- case rest of
        -- One Army is the whole of "an Army creature you control", and rule
        -- 701.47a is not a "may" -- where the rules leave nothing to ask, don't
        -- prompt.
        [] -> pure first
        second : more -> do
          let offered = first NonEmpty.:| (second : more)
          -- FILTERED, NOT TRUSTED, Pawl.Engine.Ring.tempt's posture: an answer
          -- naming something never offered falls back to the first candidate,
          -- since the action is mandatory and must put its counters somewhere.
          answer <- Game.choose (Prompt.ChooseAmass (Decide.deciderFor pid gs1) pid resolving offered)
          pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
      Monad.when (n > 0)
        . Monad.void
        $ Event.putCounters (CounterCause.ByEffect pid) chosen CounterKind.PlusOnePlusOne n
      gs2 <- State.get
      Monad.unless (Set.member subtype (Projection.subtypesOf chosen gs2)) $ do
        let (ts, gs3) = Game.freshTimestamp gs2
            effect =
              ContinuousEffect.MkContinuousEffect
                { ContinuousEffect.source = source,
                  ContinuousEffect.timestamp = ts,
                  ContinuousEffect.expiry = Expiry.Never,
                  ContinuousEffect.modification = Modification.AddCreatureSubtype subtype,
                  ContinuousEffect.affected = Affected.TheseObjects (Set.singleton chosen)
                }
        State.put gs3 {GameState.continuousEffects = effect : GameState.continuousEffects gs3}
