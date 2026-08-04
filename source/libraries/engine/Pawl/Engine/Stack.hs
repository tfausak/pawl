module Pawl.Engine.Stack where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- The runner-aware resolve-the-top-of-stack: a resolving SPELL may play a
-- subgame (CR 729), so the spell branch takes the injected runner; abilities do
-- not (an ability-driven subgame is deferred). Engine.priorityLoop supplies
-- playSubgame.
--
-- CR 608.3: a resolving permanent spell becomes a permanent on the battlefield;
-- anything else resolves its effects and then goes to its owner's graveyard
-- (Resolve.resolveSpellWith -- the CR 608.2 executor).
--
-- THE INVARIANT: this dispatches on a CLASSIFICATION -- is-it-a-permanent, read
-- off the type line -- and never on the card's identity. There must never be a
-- `case card of ...` here; that is the fusion of the closed and open halves
-- that sinks the project.
resolveTopWith :: Game Result -> Game ()
resolveTopWith runSubgame = do
  gs <- State.get
  case GameState.stack gs of
    [] -> pure ()
    oid : rest -> case Game.lookupObject oid gs of
      -- A stack id that does not resolve is a bug elsewhere; drop it rather
      -- than wedging the loop.
      Nothing -> State.put gs {GameState.stack = rest}
      Just obj -> case Object.source obj of
        Source.OfCard printing ->
          -- CR 709.3b: if this spell has a face singled out, its classification
          -- is read off THAT half, not the two combined -- routed through
          -- Game.faceOf rather than Card.combined directly, so this
          -- is-it-a-permanent/is-it-an-Aura check narrows the same way every
          -- OTHER characteristic read of a stack object already does (#648
          -- still owes Cost.hs and Action.hs the same change). Falls back to
          -- the combined view for parity with faceOf's own fallback;
          -- unreachable here since `obj` already resolved via this same `oid`.
          let face = Maybe.fromMaybe (Card.combined (Printing.card printing)) (Game.faceOf oid gs)
           in if not (Card.isPermanent face)
                then Resolve.resolveSpellWith runSubgame oid
                else
                  if not (Card.isAura face)
                    then carryOver oid =<< Event.changeZoneReturning oid Zone.Battlefield
                    else -- CR 303.4a made this spell target, so CR 608.2b applies
                    -- to it. THE INVARIANT: is-it-an-Aura is a SUBTYPE read off
                    -- the type line (CR 205.3h), the same closed-half
                    -- classification as is-it-a-permanent above it.
                      if Resolve.targetsAllIllegal oid gs
                        then Event.changeZone oid Zone.Graveyard
                        else
                          -- CR 303.4: an Aura ENTERS attached, so the target is
                          -- seeded into the new incarnation rather than written
                          -- after the move (see Event.changeZoneAttaching).
                          carryOver oid =<< Event.changeZoneAttaching Nothing oid Zone.Battlefield (enchantedBy oid gs) TapState.Untapped
        -- A token is never on the stack (created onto the battlefield, never
        -- cast).
        Source.OfToken _ -> State.put gs {GameState.stack = rest}
        Source.OfAbility srcId ability -> do
          -- CR 601.3 (Panglacial): before resolving an ability that searches a
          -- library, offer its controller the chance to cast a
          -- castable-while-searching card from their library. The ability is
          -- still on the stack, so a cast lands on top of it. Offered at
          -- resolution start, not per-Search-effect within a multi-effect
          -- ability -- exact intra-resolution interleaving is not modelled
          -- (#57). CR 700.2c: only the CHOSEN modes are scanned.
          let chosen = Binding.modesOf (Object.bindings obj)
          Monad.when (any Resolve.searchesLibrary (Modal.modesEffects chosen (ActivatedAbility.modal ability))) $
            Cast.castWhileSearching (Object.owner obj)
          Resolve.resolveAbility oid srcId ability
        Source.OfTrigger srcId ability ->
          -- CR 608.2a: an intervening "if" is checked AGAIN as the ability
          -- resolves. Object.owner is the ability's controller, which is "you".
          --
          -- CR 608.2h supplies the view of `srcId`, for the reason
          -- Event.interveningHolds gives at the gather-time half of this rule:
          -- a leaves-the-battlefield ability's source is gone by construction
          -- (CR 603.10a, CR 400.7), and Projection.fullView would describe it
          -- as an object with no characteristics. The two checks must read
          -- alike, or a trigger that passed the gather would be removed here
          -- for no reason.
          case TriggeredAbility.intervening ability of
            Just cond
              | not (Condition.holds (Projection.viewWithLastKnown srcId gs) (Filter.MkContext (Just (Object.owner obj)) (Just srcId)) gs srcId cond) ->
                  State.modify' (Game.cease oid)
            _ ->
              let chosen = Binding.modesOf (Object.bindings obj)
                  modal = TriggeredAbility.modal ability
               in Resolve.resolveModes oid srcId (Modal.chosenModes chosen modal)
        -- CR 114.5: an emblem is never on the stack (created into the command
        -- zone, never cast). Drop it, like a token.
        Source.OfEmblem _ -> State.put gs {GameState.stack = rest}
        Source.OfInherentTrigger _ ability ->
          -- CR 725.2: an inherent monarch ability has no source object and no
          -- intervening "if" (intervening = Nothing); resolve its effects
          -- directly. Object.owner is the monarch (baked at placement) --
          -- "you".
          let chosen = Binding.modesOf (Object.bindings obj)
              modal = TriggeredAbility.modal ability
           in Resolve.resolveModes oid oid (Modal.chosenModes chosen modal)

-- CR 400.7a: effects that change a permanent spell's characteristics or
-- controller keep applying to the permanent it becomes. CR 400.7 mints a fresh
-- id for that permanent, so an effect stored against the SPELL's id would stop
-- naming it; this re-keys the ones that do. Nothing when the move did not
-- happen (a cancelled CR 616.1 replacement), in which case the spell's id is
-- still the live one.
--
-- THE INVARIANT: no case on any effect's identity. Every stored
-- ContinuousEffect qualifies by CLASSIFICATION -- Projection.layer puts each
-- modification in one of CR 613.1's layers, and every layer either changes a
-- characteristic (CR 109.3) or the controller (CR 613.1b), which are exactly
-- what this rule carries over.
--
-- Only Affected.TheseObjects is re-keyed, because that is the only arm a
-- resolution effect ever stores (CR 611.2c locks the set); the dynamic arms
-- belong to static abilities and are re-derived each projection.
--
-- Scoped to the two permanent-spell branches above. CR 400.7b (static-ability
-- ability grants) and CR 400.7c (prevention effects) are separate exceptions
-- with separate carriers and are not claimed here (#634).
carryOver :: ObjectId -> Maybe ObjectId -> Game ()
carryOver _ Nothing = pure ()
carryOver oldId (Just newId) = State.modify' $ \gs ->
  gs {GameState.continuousEffects = fmap (reanchor oldId newId) (GameState.continuousEffects gs)}

-- carryOver's per-effect half: swap oldId for newId in a locked affected set
-- that names it, and leave every other effect alone.
reanchor :: ObjectId -> ObjectId -> ContinuousEffect.ContinuousEffect -> ContinuousEffect.ContinuousEffect
reanchor oldId newId eff = case ContinuousEffect.affected eff of
  Affected.TheseObjects oids
    | Set.member oldId oids ->
        eff {ContinuousEffect.affected = Affected.TheseObjects (Set.insert newId (Set.delete oldId oids))}
  _ -> eff

-- The no-subgame resolve-top (every existing caller and test): a resolving
-- spell with a PlaySubgame effect would draw. Engine's live loop uses
-- resolveTopWith.
resolveTop :: Game ()
resolveTop = resolveTopWith Resolve.noSubgame

-- The object or player an Aura spell's enchant slot names (CR 303.4 / 303.4a).
-- Nothing when the slot is unbound, which CR 303.4a makes unreachable for a
-- cast Aura -- the slot is a required target.
--
-- The recipient is handed on UNCHANGED, tag and all, so CR 303.4c's re-check
-- (Sba.stillLegalEnchant) can compare the stored value against the same pool's
-- candidates without re-deriving how it is referenced.
enchantedBy :: ObjectId -> GameState.GameState -> Maybe Recipient.Recipient
enchantedBy oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> Map.lookup Card.enchantSlot (Binding.targetsOf (Object.bindings obj))
