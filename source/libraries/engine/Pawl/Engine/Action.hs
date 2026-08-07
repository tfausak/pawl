module Pawl.Engine.Action where

import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
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
      -- Ungated by phase or by an empty stack, which is the whole difference
      -- between the two special actions pawl offers -- CR 116.2a states both
      -- restrictions and CR 116.2b states neither.
      turnUps = fmap Action.TurnFaceUp (FaceDown.turnableFaceUp pid gs)
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
      -- uses. Pawl.PerformanceSpec is what holds the line now: it measures this
      -- enumeration over 64 and 256 permanents and fails if quadrupling the
      -- board costs more than 8x, which one projection per object does.
      activations =
        let grants = Projection.controlGrants gs
            pcs = Projection.projectAll gs
            forObject oid =
              fmap (Action.Activate oid) (filter (\ab -> Activate.activatableGiven grants pcs pid oid ab gs) (Activate.abilitiesForGiven pcs oid gs))
         in concatMap forObject (Projection.controlsGiven grants pid gs <> Game.zoneMembers Zone.Hand pid gs <> Game.zoneMembers Zone.Graveyard pid gs)
   in Action.Pass : lands <> spells <> turnUps <> activations
