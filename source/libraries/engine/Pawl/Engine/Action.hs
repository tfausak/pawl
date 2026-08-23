module Pawl.Engine.Action where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.EndEffect as EndEffect
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Foretell as Foretell
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Ignore as Ignore
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Plot as Plot
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Room as Room
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import Pawl.Types.Action (Action)
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.SpecialAction as SpecialAction
import qualified Pawl.Types.Zone as Zone

-- The land plays this player may make right now: CR 305.1's land cards in their
-- hand plus the ones an effect lets them play from elsewhere, minus the ones an
-- effect prohibits, each paired with CR 712.12's chosen face (Nothing where no
-- face is chosen -- see Pawl.Engine.Card.landFaces).
--
-- THREE SOURCES, because CR 305.1's "from their hand" is the rule's own
-- allowance and CR 101.1 lets a card widen it. The hand needs no permission; the
-- graveyard is a CR 613.11 player-axis grant (Crucible of Worlds, Yawgmoth's
-- Will), and exile is the object-borne permission CR 715.3d and
-- Effect.GrantPlayFromExile write. That split is the same one
-- Pawl.Engine.Cast.castableZones draws for the cast side, and the exile source
-- is read through that module's own zoneCandidates so the two can never disagree
-- about where to look -- CR 601.3's permission names a PLAYER, and exile is
-- filed by owner, so a land somebody else owns has to be reachable.
--
-- Reading the OBJECT-BORNE permission with Cast.permitsPlayFromExile and not
-- Cast.permitsCastFromExile is the whole of what makes this the PLAY side: the
-- plotted and foretold permissions that one also folds in each say "may cast it"
-- (CR 702.170d, CR 702.143a), where CR 601.1a makes playing a card either
-- playing it as a land or casting it -- and this is the first of those.
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
-- Asked against the name of the CARD IN THE ZONE it is played from, and not
-- against the face chosen to be played: CR 712.8a gives a double-faced card
-- "only the characteristics of its front face" while it is anywhere but the
-- battlefield or the stack -- which is every zone this list draws from -- so the
-- land a player is playing is named by that face while they play it. CR 712.19
-- does let the chooser name the OTHER face -- "the player may choose the name of
-- either face of a double-faced card but not both" -- and naming it prohibits
-- nothing here, which is the same reading from the other side rather than a
-- second decision. A land with SEVERAL names is asked as a set, CR 709.4a's "one
-- of its names": no printed land has two, but the prohibition is a membership
-- test rather than a comparison all the same.
playableLands :: PlayerId -> GameState -> [(ObjectId, Maybe CardName.CardName)]
playableLands pid gs =
  let playable oid = case Game.cardOfHandMember oid gs of
        Nothing -> []
        Just card ->
          if PlayerEffect.prohibitsPlayingLand pid (Card.combinedNames card) gs
            then []
            else fmap (\(mName, _) -> (oid, mName)) (Card.landFaces card)
      -- CR 305.1's own zone, which needs no permission.
      fromHand = Game.zoneMembers Zone.Hand pid gs
      -- The whole pile or none of it: the grant narrows no land (see
      -- Pawl.Types.PlayerEffect.PlayLandsFromGraveyard), so it is asked once
      -- rather than per card, and CR 400.1's per-player zone is the "your
      -- graveyard" both printings say.
      fromGraveyard =
        if PlayerEffect.mayPlayLandsFromGraveyard pid gs
          then Game.zoneMembers Zone.Graveyard pid gs
          else []
      -- Per card instead, because CR 715.3d's permission is state on ONE exiled
      -- incarnation naming ONE player.
      fromExile = filter (\oid -> Cast.permitsPlayFromExile pid oid gs) (Cast.zoneCandidates Zone.Exile pid gs)
   in concatMap playable (fromHand <> fromGraveyard <> fromExile)

-- The cards in this player's hand whose own text grants CR 116.2e's special
-- action: Circling Vultures' "you may discard this card any time you could cast
-- an instant".
--
-- Read off the CARD (Card.combined) and never a projection, the field's own rule
-- in Pawl.Types.Face: the ability functions in the hand, where this reader takes
-- the printed card (#1859). A hand member with no card behind it -- a token, an
-- ability -- contributes nothing, playableLands' reading one function above.
--
-- The permission is a CLASSIFICATION and not an identity: this asks whether the
-- card data grants the operation, never which card it is.
discardableCards :: PlayerId -> GameState -> [ObjectId]
discardableCards pid gs =
  let grantsIt oid = maybe False granted (Game.cardOfHandMember oid gs)
      granted card = List.elem SpecialAction.DiscardThisAnyTime (Face.specialActions (Card.combined card))
   in filter grantsIt (Game.zoneMembers Zone.Hand pid gs)

legalActions :: PlayerId -> GameState -> [Action]
legalActions pid gs =
  let -- CR 305.3: "a player can't play a land, for any reason, if it isn't their
      -- turn." The one conjunct of CR 116.2a's window that CR 702.8a does NOT
      -- lift, which is why it is asked here, per PLAYER, while the phase and the
      -- empty stack are asked per CARD in landTimingOk below. Dryad Arbor's own
      -- ruling reads the split the same way: a land granted flash "can't be
      -- played during another player's turn", and the land plays remaining still
      -- gate it.
      canPlayLand =
        GameState.activePlayer gs == pid
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
      -- CR 305.1 / 116.2a: the rest of the window is a main phase with the stack
      -- empty, which together with the active-player conjunct above is CR 307.5's
      -- "as a sorcery" window conjunct for conjunct -- so it is asked through the
      -- one predicate rather than a near-copy that can drift (see
      -- Turn.sorcerySpeedWindow).
      --
      -- Asked per CARD, because CR 702.8a's "you may play this card any time you
      -- could cast an instant" lifts it for the card the keyword is on and for no
      -- other -- CR 601.1a's "playing a card" being playing it as a land or
      -- casting it, whichever is appropriate. Read through Cast.flashOn, the same
      -- function Cast.instantSpeed reads for the cast half of that sentence, so
      -- the two halves cannot disagree about where flash is read from. The face
      -- is the one this play would put onto the battlefield (CR 712.12), resolved
      -- the way Cast.proposedFace resolves the half being cast.
      --
      -- Not implemented: the CR 613.11 player axis's "as though it had flash"
      -- (CR 601.3b, PlayerEffect.mayCastAsThoughItHadFlash), which Cast.timingOk
      -- reads beside Cast.instantSpeed for a cast. A printing whose permission
      -- says PLAY rather than cast -- Scout's Warning -- would move this window
      -- too, by CR 601.1a (#1938).
      landTimingOk (oid, mName) =
        Turn.sorcerySpeedWindow pid gs
          || maybe False (\card -> Cast.flashOn oid (Game.resolveFace mName card) gs) (Game.cardOf oid gs)
      lands = if canPlayLand then fmap (uncurry Action.Play) (filter landTimingOk (playableLands pid gs)) else []
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
      --
      -- ONE ACTION PER PROCEDURE, which is CR 701.40c's choice made the way CR
      -- 709.3's cast and CR 709.5e's unlock below make theirs: a manifested morph
      -- card may be turned face up by either rule's procedure, at two different
      -- prices, and the engine names both rather than one.
      turnUps = fmap (uncurry Action.TurnFaceUp) (FaceDown.turnableFaceUp pid gs)
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
      -- CR 116.2d: the fifth special action, and the THIRD ungated by phase and
      -- by an empty stack -- "a player can take such an action any time they
      -- have priority", CR 116.2b's window again.
      --
      -- ONE ACTION PER PERMANENT, not per ability: which of a permanent's static
      -- abilities is ignored is not a choice the rules leave open once the
      -- permanent is chosen, because no printed producer grants the permission on
      -- one of several -- Damping Engine's two abilities come from one sentence,
      -- and its "this effect" is that sentence (#1267).
      ignores = fmap Action.Ignore (Ignore.ignorable pid gs)
      -- CR 116.2c: the eighth special action, and the FOURTH ungated by phase and
      -- by an empty stack -- "a player can take such an action any time they have
      -- priority, unless that effect specifies another timing restriction", which
      -- is CR 116.2b's window plus an escape clause no producer uses.
      --
      -- ONE ACTION PER SOURCE, not per stored effect: CR 116.2c ends "that
      -- effect", the printed sentence, and Gliding Licid's one sentence stores
      -- four continuous effects that must end together. Nothing is left for the
      -- player to choose once the permanent is chosen.
      endings = fmap Action.EndEffect (EndEffect.endable pid gs)
      -- CR 116.2k / 702.170a: the sixth special action, and the second whose
      -- window is CR 116.2a's rather than CR 116.2b's -- "any time you have
      -- priority during your main phase while the stack is empty". That gate is
      -- Turn.sorcerySpeedWindow, asked inside Plot.canPlot beside the cost, so
      -- the clauses of one rule stay together.
      plots = fmap Action.Plot (Plot.plottable pid gs)
      -- CR 116.2h / 702.143a: the seventh special action, and a THIRD window --
      -- "any time a player has priority during their turn", which is neither CR
      -- 116.2b's (any priority at all) nor CR 116.2a's sorcery speed. The gate is
      -- asked inside Foretell.canForetell beside the {2}, for plots' reason.
      foretells = fmap Action.Foretell (Foretell.foretellable pid gs)
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
      -- Not implemented: nothing asserts that line. An allocation guard held it
      -- with a fixture per depth of the gate chain -- a mana ability stops at
      -- CR 605.1a and never reaches those two gates, a non-mana one runs the
      -- whole chain -- until measuring bytes was judged too compiler-specific
      -- to keep. A regression here fails nothing (gap #578). What is left in
      -- the non-mana path is still O(N) per permanent without being a
      -- whole-board structure to hoist -- the per-candidate filter over the
      -- shared pool below (#1448).
      grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
      -- ONE mana-source sweep of each kind for the whole enumeration. Each is a
      -- walk of everything this player controls, asking each what mana routes it
      -- offers, and neither depends on which permanent's ability is being gated
      -- -- so the cost gate took an identical one per ability until it was
      -- handed this one (#1073).
      --
      -- TWO sweeps and not one, because the two questions differ: CR 605.3a's
      -- offer at the end of this list is every source that could be tapped
      -- (`manaSources`), while the cost gate is judged against what the supply
      -- walk can count (`supplySources`, Mana.supplyCapacity). They part on
      -- exactly a permanent whose every mana route holds mana in its own cost,
      -- which in `data/cards/` is Transmogrant Altar; the gate must take the
      -- supply list or Mana.payableResolutionsGiven reads a source with no
      -- options and finds no payable board at all.
      manaSources = Cost.activationManaSourcesGiven grants pcs pid gs
      supplySources = Cost.supplyManaSourcesGiven grants pcs pid gs
      -- ONE set of base target pools for the whole enumeration, for the same
      -- reason: CR 115's candidate set for a Pool is a function of `gs` alone
      -- (Target.Pools), so building one per slot per ability walked the
      -- battlefield once per permanent. Lazy per pool, so an enumeration that
      -- targets only creatures pays for the creature walk alone (#1073).
      pools = Target.poolsGiven pcs gs
      activations =
        let forObject oid =
              fmap (Action.Activate oid) (filter (\ab -> Activate.activatableGiven grants pcs pools supplySources pid oid ab gs) (Activate.abilitiesForGiven pcs oid gs))
         in concatMap forObject (Projection.controlsGiven grants pid gs <> Game.zoneMembers Zone.Hand pid gs <> Game.zoneMembers Zone.Graveyard pid gs)
      -- CR 605.3a's first window -- "a player may activate an activated mana
      -- ability whenever they have priority" -- which the activation list above
      -- cannot serve: Activate.activatableGiven refuses a mana ability outright,
      -- because CR 605.3b keeps it off the stack and that is the only thing an
      -- Action.Activate does with one.
      --
      -- Cost.activationManaSourcesGiven is the whole gate, and it is the SAME
      -- list CR 605.3a's other two windows are served from (Cost.payMana's
      -- candidates, where a mana ability's own window narrows it further; see
      -- #2094): controlled, and offering some route Cost.manaActivations admits
      -- -- CR 118.3's payability of the ability's own cost (CR 602.2b), which
      -- carries CR 107.5's tapped permanent and CR 302.6's sick creature with
      -- it, plus CR 602.5's printed "activate only ..." rider, which CR 605.1
      -- leaves on an ability a timing restriction cannot disqualify. ONE sweep
      -- for the whole enumeration rather than one per permanent, on the board
      -- this function already walked.
      --
      -- That gate is also what keeps the offer from being one a player could
      -- take forever: taking it spends what the cost charged, and the gate
      -- refuses the source once it cannot charge it again -- a tapped permanent
      -- cannot pay {T} (CR 107.5), a sacrificed Blood Pet is gone. Cost.tapForMana
      -- picks among exactly the options this gate admitted -- one predicate,
      -- asked at the offer and at the payment, so the two cannot disagree.
      -- Pawl.ManaSpec's "the menu carries one activation per untapped source" is
      -- the proof.
      manaAbilityActivations = fmap Action.ActivateManaAbility manaSources
   in Action.Pass : lands <> spells <> turnUps <> unlocks <> discards <> ignores <> endings <> plots <> foretells <> activations <> manaAbilityActivations
