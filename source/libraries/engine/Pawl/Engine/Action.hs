module Pawl.Engine.Action where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Room as Room
import qualified Pawl.Engine.Turn as Turn
import Pawl.Types.Action (Action)
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpecialAction as SpecialAction
import qualified Pawl.Types.Zone as Zone

-- The land plays this player may make right now: CR 305.1's land cards in their
-- hand, minus the ones an effect prohibits, each paired with CR 712.12's chosen
-- face (Nothing where no face is chosen -- see Pawl.Engine.Card.landFaces).
--
-- ONE CARD MAY APPEAR TWICE, in principle: CR 712.12 has the player choose among
-- "its faces that's a land", so a modal double-faced card with two land faces
-- would offer both. That shape falls out of landFaces returning a list; no
-- printing reaches it.
--
-- The PROHIBITION is asked here and not in legalActions' `canPlayLand` gate,
-- because it is per-CARD rather than per-turn: CR 305.2/305.3's limits are about
-- the player and settle the whole list at once, while Null Chamber's is about
-- the land's name and stops one card while leaving the rest playable.
--
-- Asked against the name of the CARD IN THE HAND, which is where the special
-- action is taken from, and not against the face chosen to be played: CR 712.8a
-- gives a double-faced card in a hand "only the characteristics of its front
-- face", so the land a player is playing is named by that face while they play
-- it. CR 712.19 does let the chooser name the OTHER face -- "the player may
-- choose the name of either face of a double-faced card but not both" -- and
-- naming it prohibits nothing here, which is the same reading from the other
-- side rather than a second decision. A land with several names would want a set
-- here rather than one name (#650).
playableLands :: PlayerId -> GameState -> [(ObjectId, Maybe CardName.CardName)]
playableLands pid gs =
  let cardOfHandCard oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Just (Printing.card printing)
          Source.OfToken card -> Just card
          Source.OfAbility _ _ -> Nothing
          Source.OfTrigger _ _ -> Nothing
          Source.OfEmblem _ -> Nothing
          Source.OfInherentTrigger _ _ -> Nothing
        Nothing -> Nothing
      playable oid = case cardOfHandCard oid of
        Nothing -> []
        Just card ->
          if PlayerEffect.prohibitsPlayingLand pid (Face.name (Card.combined card)) gs
            then []
            else fmap (\(mName, _) -> (oid, mName)) (Card.landFaces card)
   in concatMap playable (Game.zoneMembers Zone.Hand pid gs)

-- The cards in this player's hand whose own text grants CR 116.2e's special
-- action: Circling Vultures' "you may discard this card any time you could cast
-- an instant".
--
-- Read off the CARD (Card.combined) and never a projection, the field's own rule
-- in Pawl.Types.Face: the ability functions in the hand, which pawl's projection
-- does not reach (#160). A hand member with no card behind it -- a token, an
-- ability -- contributes nothing, playableLands' reading one function above.
--
-- The permission is a CLASSIFICATION and not an identity: this asks whether the
-- card data grants the operation, never which card it is.
discardableCards :: PlayerId -> GameState -> [ObjectId]
discardableCards pid gs =
  let grantsIt oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> granted (Printing.card printing)
          Source.OfToken card -> granted card
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
      granted card = List.elem SpecialAction.DiscardThisAnyTime (Face.specialActions (Card.combined card))
   in filter grantsIt (Game.zoneMembers Zone.Hand pid gs)

legalActions :: PlayerId -> GameState -> [Action]
legalActions pid gs =
  let -- CR 305.1 / 116.2a: the window is a main phase of this player's own turn
      -- with the stack empty, which is CR 307.5's "as a sorcery" window
      -- conjunct for conjunct -- so it is asked through the one predicate rather
      -- than a near-copy that can drift (see Turn.sorcerySpeedWindow).
      --
      -- CR 702.8a's window says "play this card", which reaches a land as well
      -- as a spell; this gate does not consult the keyword, so a land card with
      -- flash would still be playable only at sorcery speed (#566).
      canPlayLand =
        Turn.sorcerySpeedWindow pid gs
          -- CR 305.2a: compare the number of lands this player CAN play this
          -- turn with the number they HAVE already played; the play is legal
          -- only if the first is greater. A comparison of two counts and never
          -- a yes/no, because CR 305.2 lets a continuous effect raise the first
          -- one (Exploration, Azusa Lost but Seeking). Strictly greater is CR
          -- 305.2b read from the other side: it forbids the play once the
          -- allowance is EQUAL TO OR LESS THAN the tally, and less than is
          -- reachable -- Exploration destroyed after the second land leaves an
          -- allowance of one against a tally of two.
          && Map.findWithDefault 0 pid (GameState.landsPlayed gs) < PlayerEffect.landPlaysAllowed pid gs
      lands = if canPlayLand then fmap (uncurry Action.Play) (playableLands pid gs) else []
      -- CR 709.3: one action per castable HALF, so choosing a half is choosing
      -- an action and the engine never asks which one. CR 702.37d adds one more
      -- per half with morph, for the same reason: casting face down is a second
      -- cast of the same card, not a rider on the first.
      spells = fmap (\(oid, name, facing) -> Action.Cast oid name facing) (Cast.castableSpells pid gs)
      -- CR 116.2b / 702.37e: turning a face-down permanent face up is a special
      -- action a player may take any time they have priority, so it joins the
      -- menu beside CR 116.2a's land play rather than going through the stack.
      -- Ungated by phase or by an empty stack, which it shares with CR 116.2e's
      -- discard below: CR 116.2a and CR 116.2m both state those restrictions and
      -- CR 116.2b and CR 116.2e state neither.
      turnUps = fmap Action.TurnFaceUp (FaceDown.turnableFaceUp pid gs)
      -- CR 116.2m / 709.5e: the third special action, and the one whose window is
      -- CR 116.2a's rather than CR 116.2b's -- "any time they have priority and
      -- the stack is empty during a main phase of their turn". That gate is
      -- Turn.sorcerySpeedWindow, asked inside Room.canUnlock beside the cost, so
      -- the two clauses of one rule stay together.
      --
      -- ONE ACTION PER DOOR, for the reason CR 709.3's cast offers one per half:
      -- which door to open is the player's choice, and offering each as its own
      -- legal action is how the engine avoids making it.
      unlocks = fmap (uncurry Action.Unlock) (Room.unlockable pid gs)
      -- CR 116.2e: the fourth special action, and the SECOND one ungated by
      -- phase and by an empty stack -- "a player can take such an action any
      -- time they have priority", which is CR 116.2b's window rather than CR
      -- 116.2a's.
      --
      -- The card's own wording is "any time you could cast an instant", and this
      -- deliberately does not ask that: CR 116.2e's last sentence states the
      -- timing the rules use, so no casting permission or restriction is
      -- consulted. legalActions is only ever called for the player with
      -- priority, so the rule's condition is already met by being here.
      discards = fmap Action.DiscardFromHand (discardableCards pid gs)
      -- CR 702.29a: a HAND is a source of activations too, not just the
      -- battlefield -- cycling functions only while the card is in a player's
      -- hand. So is a GRAVEYARD, by CR 113.6m: Loxodon Surveyor's "{3}, Exile
      -- this card from your graveyard: Draw a card" functions only there. Which
      -- abilities an object offers from where is Activate.abilitiesFor's
      -- question; this list only says where to look, and the three zones are
      -- pairwise disjoint.
      --
      -- ONE control-grant walk and ONE whole-board projection for the whole
      -- enumeration, threaded into both halves of the loop. Each object would
      -- otherwise be projected three times plus a grant walk for its
      -- controller, at a boundary the priority loop reaches on every pass;
      -- sharing them rested on GHC's CSE before and is now stated (#200, #315,
      -- #316). The board is a snapshot of this one `gs` and this is a pure
      -- function of it, so nothing can move between the projection and its
      -- uses. Threaded into the cost and target gates too, which used to hoist
      -- a board apiece per call and so per permanent (#716).
      --
      -- Pawl.PerformanceSpec is what holds the line now, with a fixture per
      -- depth of the gate chain. A MANA ability stops at CR 605.1a and never
      -- reaches those two gates: that path is linear, and the ratio guard
      -- measures it over 64 and 256 permanents and fails if quadrupling the
      -- board costs more than 8x, which one projection per object does. A
      -- NON-mana ability runs the whole chain: that path is held by an absolute
      -- per-permanent ceiling instead, because what is left in it is still O(N)
      -- per permanent without being a projection (#1073).
      grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
      activations =
        let forObject oid =
              fmap (Action.Activate oid) (filter (\ab -> Activate.activatableGiven grants pcs pid oid ab gs) (Activate.abilitiesForGiven pcs oid gs))
         in concatMap forObject (Projection.controlsGiven grants pid gs <> Game.zoneMembers Zone.Hand pid gs <> Game.zoneMembers Zone.Graveyard pid gs)
      -- CR 605.3a's first window -- "a player may activate an activated mana
      -- ability whenever they have priority" -- which the activation list above
      -- cannot serve: Activate.activatableGiven refuses a mana ability outright,
      -- because CR 605.3b keeps it off the stack and that is the only thing an
      -- Action.Activate does with one.
      --
      -- Mana.manaSourcesGiven is the whole gate, and it is the SAME list CR
      -- 605.3a's other two windows are served from (Cost.payMana's candidates):
      -- untapped, controlled, offering some mana route, and past CR 302.6's
      -- sickness test. ONE sweep for the whole enumeration rather than one per
      -- permanent, on the board this function already walked.
      --
      -- Not implemented: CR 118.3 asked of the ability's own activation cost
      -- before it is offered, so a source whose cost cannot be paid is offered
      -- here, fails when taken, and -- having tapped nothing -- is offered again
      -- (#1119). The same gap the payment window has, where Cost.payMana's
      -- `refused` set is what stops it looping.
      manaActivations = fmap Action.ActivateManaAbility (Mana.manaSourcesGiven grants pcs pid gs)
   in Action.Pass : lands <> spells <> turnUps <> unlocks <> discards <> activations <> manaActivations
