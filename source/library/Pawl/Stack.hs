module Pawl.Stack where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Cast as Cast
import qualified Pawl.Condition as Condition
import qualified Pawl.Event as Event
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Printing as Printing
import Pawl.Type.Result (Result)
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.Zone as Zone

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
          if Card.isPermanent (Printing.card printing)
            then Event.changeZone oid Zone.Battlefield
            else Resolve.resolveSpellWith runSubgame oid
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
          case TriggeredAbility.intervening ability of
            Just cond
              | not (Condition.holds (\o -> Just (Projection.viewOfObject o gs)) (Filter.MkContext (Just (Object.owner obj)) (Just srcId)) gs srcId cond) ->
                  State.modify' (Resolve.cease oid)
            _ ->
              let chosen = Binding.modesOf (Object.bindings obj)
                  modal = TriggeredAbility.modal ability
               in Resolve.resolveEffects oid srcId (Modal.modesEffects chosen modal) (Modal.modesTargetSpecs chosen modal)
        -- CR 114.5: an emblem is never on the stack (created into the command
        -- zone, never cast). Drop it, like a token.
        Source.OfEmblem _ -> State.put gs {GameState.stack = rest}
        Source.OfInherentTrigger _ ability ->
          -- CR 725.2: an inherent monarch ability has no source object and no
          -- intervening "if" (intervening = Nothing); resolve its effects
          -- directly. Object.owner is the monarch (baked at placement) -- "you".
          let chosen = Binding.modesOf (Object.bindings obj)
              modal = TriggeredAbility.modal ability
           in Resolve.resolveEffects oid oid (Modal.modesEffects chosen modal) (Modal.modesTargetSpecs chosen modal)

-- The no-subgame resolve-top (every existing caller and test): a resolving spell
-- with a PlaySubgame effect would draw. Engine's live loop uses resolveTopWith.
resolveTop :: Game ()
resolveTop = resolveTopWith Resolve.noSubgame
