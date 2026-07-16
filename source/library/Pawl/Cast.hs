module Pawl.Cast where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Card as Card
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.ManaCost (ManaCost)
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone

-- Nothing when the object has no mana cost at all (CR 202.1: a land).
costOf :: ObjectId -> GameState -> Maybe ManaCost
costOf oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> case Object.source obj of
    Source.OfCard printing -> Card.manaCost (Printing.card printing)

-- CR 601.3a / 302.1: a creature spell may be cast only when its controller could
-- cast a sorcery -- a main phase of their own turn, with an empty stack. (The
-- priority requirement is implicit: the engine only offers actions to the player
-- who holds priority.)
--
-- M1a has nothing castable at instant speed, so this gate is the whole timing
-- story. It grows a per-card timing classification when instants arrive.
sorcerySpeed :: PlayerId -> GameState -> Bool
sorcerySpeed pid gs =
  Turn.isMainPhase (GameState.phase gs)
    && GameState.activePlayer gs == pid
    && null (GameState.stack gs)

-- Affordable and correctly timed, and actually in this player's hand.
castable :: PlayerId -> ObjectId -> GameState -> Bool
castable pid oid gs = case costOf oid gs of
  Nothing -> False
  Just cost ->
    sorcerySpeed pid gs
      && elem oid (Game.zoneMembers Zone.Hand pid gs)
      && Mana.canPay pid cost gs

castableSpells :: PlayerId -> GameState -> [ObjectId]
castableSpells pid gs =
  if sorcerySpeed pid gs
    then filter (\oid -> castable pid oid gs) (Game.zoneMembers Zone.Hand pid gs)
    else []

-- CR 601: the card moves hand -> stack, becoming a NEW object (CR 400.7), and
-- its costs are paid. The caster keeps priority (CR 117.3c) -- that is the
-- caller's job, in the priority loop.
--
-- CR 601.2 puts the card on the stack BEFORE costs are paid, rewinding the whole
-- cast if it turns out to be illegal. M1a pays first because legalActions only
-- ever offers an affordable cast, so there is nothing to rewind and the
-- observable result is identical. A no-op on failure keeps this total.
castSpell :: PlayerId -> ObjectId -> Game ()
castSpell pid oid = do
  gs <- State.get
  case costOf oid gs of
    Nothing -> pure ()
    Just cost -> case Mana.payCost pid cost gs of
      Nothing -> pure ()
      Just paid -> State.put (Game.changeZone oid Zone.Stack paid)
