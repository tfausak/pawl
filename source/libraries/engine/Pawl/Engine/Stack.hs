module Pawl.Engine.Stack where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
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
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- The runner-aware resolve-the-top-of-stack: a resolving SPELL may play a subgame
-- (CR 729), so the spell branch takes the injected runner; abilities do not (an
-- ability-driven subgame is deferred). Engine.priorityLoop supplies playSubgame.
--
-- CR 608.3: a resolving permanent spell becomes a permanent on the battlefield;
-- anything else resolves its effects and then goes to its owner's graveyard
-- (Resolve.resolveSpellWith -- the CR 608.2 executor).
--
-- THE INVARIANT: this dispatches on a CLASSIFICATION -- is-it-a-permanent, read
-- off the type line -- and never on the card's identity. There is no
-- `case card of Piker -> ...` here and there must never be one; that is the
-- fusion of the closed and open halves that sinks the project. The same shape as
-- is-it-a-mana-ability. Zero opcodes.
resolveTopWith :: Game Result -> Game ()
resolveTopWith runSubgame = do
  gs <- State.get
  case GameState.stack gs of
    [] -> pure ()
    oid : rest -> case Game.lookupObject oid gs of
      -- A stack id that does not resolve is a bug elsewhere; drop it rather than
      -- wedging the loop.
      Nothing -> State.put gs {GameState.stack = rest}
      Just obj -> case Object.source obj of
        Source.OfCard printing ->
          let card = Printing.card printing
           in if not (Card.isPermanent card)
                then Resolve.resolveSpellWith runSubgame oid
                else
                  if not (Card.isAura card)
                    then Event.changeZone oid Zone.Battlefield
                    else -- CR 303.4a made this spell target, so CR 608.2b applies to
                    -- it -- the first PERMANENT spell in this pool for which that
                    -- is true. THE INVARIANT: is-it-an-Aura is a SUBTYPE read off
                    -- the type line (CR 205.3h), the same closed-half
                    -- classification as is-it-a-permanent above it. Not a case on
                    -- the card's identity.
                      if Resolve.targetsAllIllegal oid gs
                        then Event.changeZone oid Zone.Graveyard
                        else
                          -- CR 303.4: an Aura ENTERS attached, so the target is
                          -- seeded into the new incarnation rather than written
                          -- after the move (see Event.changeZoneAttaching).
                          Monad.void (Event.changeZoneAttaching Nothing oid Zone.Battlefield (enchantedBy oid gs))
        -- A token is never on the stack (created onto the battlefield, never cast).
        Source.OfToken _ -> State.put gs {GameState.stack = rest}
        Source.OfAbility srcId ability -> do
          -- CR 601.3 (Panglacial): before resolving an ability that searches a
          -- library, offer its controller the chance to cast a
          -- castable-while-searching card from their library. The ability is still
          -- on the stack, so a cast lands on top of it (the ruling's sequence).
          -- Offered at resolution start, not per-Search-effect within a
          -- multi-effect ability -- exact intra-resolution interleaving is not
          -- modelled (#57); Evolving Wilds' only effect is the search.
          -- CR 700.2c/M4g: scanned over only the CHOSEN modes -- Evolving Wilds is
          -- single-mode, so chosen = {ModeIndex 0} and behavior is unchanged.
          let chosen = Binding.modesOf (Object.bindings obj)
          Monad.when (any Resolve.searchesLibrary (Modal.modesEffects chosen (ActivatedAbility.modal ability))) $
            Cast.castWhileSearching (Object.owner obj)
          Resolve.resolveAbility oid srcId ability
        Source.OfTrigger srcId ability ->
          -- CR 608.2a: an intervening "if" is checked AGAIN as the ability
          -- resolves; if it is no longer true the ability is removed from the
          -- stack and none of its effects happen. Object.owner is the ability's
          -- controller (Engine.placeOne stamps it), which is who "you" means.
          --
          -- CR 608.2h supplies the view of `srcId`, for the reason
          -- Event.interveningHolds spells out at the gather-time half of this
          -- same rule: a leaves-the-battlefield ability's source is gone by
          -- construction (CR 603.10a, CR 400.7), and Projection.fullView would
          -- describe it as an object with no characteristics rather than as the
          -- permanent it was. The two checks must read alike, or a trigger that
          -- passed the gather would be removed here for no reason a rule gives.
          case TriggeredAbility.intervening ability of
            Just cond
              | not (Condition.holds (Projection.viewWithLastKnown srcId gs) (Filter.MkContext (Just (Object.owner obj)) (Just srcId)) gs srcId cond) ->
                  State.modify' (Resolve.cease oid)
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
          -- directly. Object.owner is the monarch (baked at placement) -- "you".
          let chosen = Binding.modesOf (Object.bindings obj)
              modal = TriggeredAbility.modal ability
           in Resolve.resolveModes oid oid (Modal.chosenModes chosen modal)

-- The no-subgame resolve-top (every existing caller and test): a resolving spell
-- with a PlaySubgame effect would draw. Engine's live loop uses resolveTopWith.
resolveTop :: Game ()
resolveTop = resolveTopWith Resolve.noSubgame

-- The object or player an Aura spell's enchant slot names (CR 303.4a / 303.4:
-- "An Aura enters the battlefield attached to an object or player"). Nothing when
-- the slot is unbound, which CR 303.4a makes unreachable for a cast Aura -- the
-- slot is a required target.
--
-- The recipient is handed on UNCHANGED, tag and all: CR 702.5d's enchant-player
-- Auras are attached to the ToPlayer their Pool.Players spec produced, and every
-- other Aura to the ToCreature or ToObject its own pool produced. That is what
-- lets CR 303.4c's re-check (Pawl.Engine.Sba.stillLegalEnchant) compare the stored value
-- against the same pool's candidates without re-deriving how it is referenced.
enchantedBy :: ObjectId -> GameState.GameState -> Maybe Recipient.Recipient
enchantedBy oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> Map.lookup Card.enchantSlot (Binding.targetsOf (Object.bindings obj))
