{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Target's per-player slots: CR 601.2c's "for each opponent,
-- ... up to one target ... that player controls", announced once per opponent
-- (Target.announcedSlots) and re-judged per copy at CR 608.2b
-- (Target.boundCopies). Riptide Gearhulk and Enigma Thief take the triggered
-- road, Dismantling Wave and Blatant Thievery the spell's, The Theorist, Jace
-- Beleren the activation's. Sepulchral Primordial and Afterlife from the Loam
-- reach "that player's graveyard" instead. Every board has three seats, so "each
-- opponent" and "that player" are never the same seat by accident.
module Pawl.TargetPerPlayerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural.Type
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Target" . Spec.describe s "PerPlayer" $ do
  let -- alice holds `creature`; bob controls a Goblin Piker and a Forest, carol
      -- a Glorious Anthem, alice an Ornithopter. bob's and carol's libraries
      -- hold three cards each (the LAST named is the top).
      board creature = do
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        anthem <- S.printingOf s registry "Glorious Anthem"
        ornithopter <- S.printingOf s registry "Ornithopter"
        card <- S.printingOf s registry creature
        seeded <- traverse (S.printingOf s registry) ["Lightning Bolt", "Unsummon", "Griptide"]
        let (pikerId, g1) = S.addPermanent piker S.bob S.threePlayerGame
            (_, g2) = S.addPermanent forest S.bob g1
            (anthemId, g3) = S.addPermanent anthem S.carol g2
            (_, g4) = S.addPermanent ornithopter S.alice g3
            stock pid g = List.foldl' (\g' p -> snd (S.addLibraryCard p pid g')) g seeded
            (gs, cardId) = S.handOne card (stock S.carol (stock S.bob g4))
        pure (gs, cardId, pikerId, anthemId)
      -- The trigger's announcement, recorded: every AnnounceTargets offer as
      -- the object ids each slot offers.
      enter :: Set.Set ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [[Set.Set ObjectId.ObjectId]])
      enter picks cardId gs =
        State.runState (fmap snd (Engine.runGame (aiming picks) gs (Event.changeZone cardId Zone.Battlefield >> Engine.placePendingTriggers))) []
      resolve :: GameState.GameState -> GameState.GameState
      resolve gs = State.evalState (fmap snd (Engine.runGame (aiming Set.empty) gs Stack.resolveTop)) []
      named = Just . CardName.MkCardName . Text.pack
      onBattlefield name = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack name))
  -- CR 601.2c: one slot per opponent, each offering only what THAT opponent
  -- controls -- not bob's Forest (nonland), not alice's own Ornithopter, and
  -- not the other opponent's permanent.
  Spec.it s "CR 601.2c Riptide Gearhulk asks each opponent's slot for that player's nonland permanents only" $ do
    (gs, cardId, pikerId, anthemId) <- board "Riptide Gearhulk"
    let (placed, offers) = enter (Set.fromList [pikerId, anthemId]) cardId gs
        after = resolve placed
    -- THE GAMEPLAY ASSERTIONS, ahead of the offer proxy.
    Spec.assertEqWith s "bob's Piker went third from the top of bob's library" (namesIn Zone.Library S.bob after) (fmap named ["Griptide", "Unsummon", "Goblin Piker", "Lightning Bolt"])
    Spec.assertEqWith s "carol's Anthem went third from the top of carol's library" (namesIn Zone.Library S.carol after) (fmap named ["Griptide", "Unsummon", "Glorious Anthem", "Lightning Bolt"])
    Spec.assertEqWith s "alice's own Ornithopter stayed" (onBattlefield "Ornithopter" S.alice after) 1
    Spec.assertEqWith s "one announcement, one slot per opponent, each its own player's" offers [[Set.singleton pikerId, Set.singleton anthemId]]
  -- CR 115.6: "up to one" per opponent, so one copy may be left empty while the
  -- other is filled.
  Spec.it s "CR 115.6 Riptide Gearhulk may leave carol's slot empty and still take bob's target" $ do
    (gs, cardId, pikerId, _) <- board "Riptide Gearhulk"
    let after = resolve (fst (enter (Set.singleton pikerId) cardId gs))
    Spec.assertEqWith s "bob's Piker went into his library" (onBattlefield "Goblin Piker" S.bob after) 0
    Spec.assertEqWith s "carol's Anthem stayed on the battlefield" (onBattlefield "Glorious Anthem" S.carol after) 1
  -- CR 608.2b per copy: the Piker was chosen for BOB's slot. Once carol
  -- controls it, it is still an opponent's nonland permanent -- so only the
  -- copy's own "that player" can make it illegal.
  Spec.it s "CR 608.2b the Piker changing hands to carol is illegal for bob's slot, and carol's target still resolves" $ do
    (gs, cardId, pikerId, anthemId) <- board "Riptide Gearhulk"
    let placed = fst (enter (Set.fromList [pikerId, anthemId]) cardId gs)
        after = resolve (S.giveControl pikerId S.carol placed)
    Spec.assertEqWith s "the Piker (bob owns it) stayed on the battlefield" (onBattlefield "Goblin Piker" S.bob after) 1
    Spec.assertEqWith s "carol's Anthem went third from the top of her library" (namesIn Zone.Library S.carol after) (fmap named ["Griptide", "Unsummon", "Glorious Anthem", "Lightning Bolt"])
  Spec.it s "CR 601.2c Enigma Thief returns each opponent's chosen permanent to its owner's hand" $ do
    (gs, cardId, pikerId, anthemId) <- board "Enigma Thief"
    let after = resolve (fst (enter (Set.fromList [pikerId, anthemId]) cardId gs))
    Spec.assertBool s (elem (named "Goblin Piker") (namesIn Zone.Hand S.bob after)) "the Piker is in bob's hand"
    Spec.assertBool s (elem (named "Glorious Anthem") (namesIn Zone.Hand S.carol after)) "the Anthem is in carol's hand"
    Spec.assertEqWith s "alice's Ornithopter stayed" (onBattlefield "Ornithopter" S.alice after) 1

  -- "that player's graveyard": each copy's POOL is its own player's graveyard
  -- (ZoneScope.BoundPlayer). bob's graveyard holds a Goblin Piker, an Ornithopter
  -- and a Forest; carol's a Llanowar Elves and a Glorious Anthem; alice's an
  -- Ornithopter. bob's and carol's libraries hold three cards each.
  let graveyards card = do
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        ornithopter <- S.printingOf s registry "Ornithopter"
        elves <- S.printingOf s registry "Llanowar Elves"
        anthem <- S.printingOf s registry "Glorious Anthem"
        swamp <- S.printingOf s registry "Swamp"
        printing <- S.printingOf s registry card
        seeded <- traverse (S.printingOf s registry) ["Lightning Bolt", "Unsummon", "Griptide"]
        let (pikerId, g1) = S.addGraveyardCard piker S.bob (S.landsFor swamp S.alice 8 S.threePlayerGame)
            (bobThopterId, g2) = S.addGraveyardCard ornithopter S.bob g1
            (_, g3) = S.addGraveyardCard forest S.bob g2
            (elvesId, g4) = S.addGraveyardCard elves S.carol g3
            (_, g5) = S.addGraveyardCard anthem S.carol g4
            (aliceThopterId, g6) = S.addGraveyardCard ornithopter S.alice g5
            stock pid g = List.foldl' (\g' p -> snd (S.addLibraryCard p pid g')) g seeded
            (gs, cardId) = S.handOne printing (stock S.carol (stock S.bob g6))
        pure (gs, cardId, (pikerId, bobThopterId, elvesId, aliceThopterId))
      -- Resolve the top of the stack, taking `takes` out of CR 608.2d's "you
      -- may" per member (FILTERED from the offer).
      resolveTaking :: Set.Set ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      resolveTaking takes gs = State.evalState (fmap snd (Engine.runGame (taking takes Set.empty) gs Stack.resolveTop)) []
      exile oid gs = State.evalState (fmap snd (Engine.runGame (aiming Set.empty) gs (Event.changeZone oid Zone.Exile))) []
      -- How many permanents named `name` `pid` CONTROLS (not owns).
      controls name pid gs =
        length
          [ o
          | o <- Set.toList (GameState.battlefield gs),
            fmap S.nameOf (Game.cardOf o gs) == named name,
            Projection.controllerOf o gs == Just pid
          ]
  Spec.it s "CR 601.2c Sepulchral Primordial asks each opponent's slot for that player's graveyard only" $ do
    (gs, cardId, (pikerId, bobThopterId, elvesId, aliceThopterId)) <- graveyards "Sepulchral Primordial"
    let (placed, offers) = enter (Set.fromList [pikerId, elvesId]) cardId gs
        after = resolveTaking (Set.fromList [pikerId, elvesId]) placed
    -- THE GAMEPLAY ASSERTIONS, ahead of the offer proxy.
    Spec.assertEqWith s "alice controls bob's Goblin Piker" (controls "Goblin Piker" S.alice after) 1
    Spec.assertEqWith s "alice controls carol's Llanowar Elves" (controls "Llanowar Elves" S.alice after) 1
    Spec.assertEqWith s "no Ornithopter entered" (controls "Ornithopter" S.alice after) 0
    Spec.assertEqWith s "one slot per opponent, each offering that player's creature cards" offers [[Set.fromList [pikerId, bobThopterId], Set.singleton elvesId]]
    Spec.assertBool s (not (any (any (Set.member aliceThopterId)) offers)) "alice's own graveyard is offered to no slot"
  -- CR 608.2d: "for each opponent, you may" is a choice per target, so alice can
  -- take carol's Elves and leave bob's targeted Piker where it is.
  Spec.it s "CR 608.2d Sepulchral Primordial may put carol's Elves onto the battlefield and not bob's Piker" $ do
    (gs, cardId, (pikerId, _, elvesId, _)) <- graveyards "Sepulchral Primordial"
    let after = resolveTaking (Set.singleton elvesId) (fst (enter (Set.fromList [pikerId, elvesId]) cardId gs))
    Spec.assertEqWith s "alice controls carol's Llanowar Elves" (controls "Llanowar Elves" S.alice after) 1
    Spec.assertBool s (elem (named "Goblin Piker") (namesIn Zone.Graveyard S.bob after)) "bob's Piker stayed in his graveyard"
  -- CR 115.6: "up to one" per opponent, so carol's copy may go unfilled.
  Spec.it s "CR 115.6 Sepulchral Primordial may leave carol's slot empty and still take bob's Piker" $ do
    (gs, cardId, (pikerId, _, elvesId, _)) <- graveyards "Sepulchral Primordial"
    let after = resolveTaking (Set.fromList [pikerId, elvesId]) (fst (enter (Set.singleton pikerId) cardId gs))
    Spec.assertEqWith s "alice controls bob's Goblin Piker" (controls "Goblin Piker" S.alice after) 1
    Spec.assertBool s (elem (named "Llanowar Elves") (namesIn Zone.Graveyard S.carol after)) "carol's Elves stayed in her graveyard"
  -- CR 400.7 / 608.2b: bob's Piker leaving his graveyard in response makes his
  -- copy's target illegal; carol's is still returned.
  Spec.it s "CR 608.2b Sepulchral Primordial returns carol's Elves after bob's targeted Piker is exiled" $ do
    (gs, cardId, (pikerId, _, elvesId, _)) <- graveyards "Sepulchral Primordial"
    let placed = fst (enter (Set.fromList [pikerId, elvesId]) cardId gs)
        after = resolveTaking (Set.fromList [pikerId, elvesId]) (exile pikerId placed)
    Spec.assertEqWith s "no Goblin Piker entered" (controls "Goblin Piker" S.alice after) 0
    Spec.assertEqWith s "alice controls carol's Llanowar Elves" (controls "Llanowar Elves" S.alice after) 1
  -- CR 614.12 and Ixidron's ruling ("If Ixidron and another creature are entering
  -- at the same time, the other creature enters face up") on the ForEach road:
  -- the Primordial returns its members one at a time inside one bracket, so the
  -- Soul Warden entering beside Ixidron stays face up in EITHER order. bob's Hill
  -- Giant and the Primordial itself, already on the battlefield, are what the
  -- sweep does reach.
  let primordialSweep wardenSeat ixidronSeat = do
        primordial <- S.printingOf s registry "Sepulchral Primordial"
        ixidron <- S.printingOf s registry "Ixidron"
        warden <- S.printingOf s registry "Soul Warden"
        giant <- S.printingOf s registry "Hill Giant"
        let (wardenId, g1) = S.addGraveyardCard warden wardenSeat S.threePlayerGame
            (ixidronId, g2) = S.addGraveyardCard ixidron ixidronSeat g1
            (_, g3) = S.addPermanent giant S.bob g2
            (gs, cardId) = S.handOne primordial g3
            picks = Set.fromList [wardenId, ixidronId]
            after = resolveTaking picks (fst (enter picks cardId gs))
            faceDown =
              [ fmap S.nameOf (Game.cardOf oid after)
              | oid <- Set.toList (GameState.battlefield after),
                maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid after)
              ]
        pure (List.sort faceDown, controls "Soul Warden" S.alice after + controls "Ixidron" S.alice after)
      swept = List.sort (fmap named ["Hill Giant", "Sepulchral Primordial"])
  Spec.it s "CR 614.12 Sepulchral Primordial returning a Soul Warden before Ixidron leaves the Warden face up" $ do
    (faceDown, returned) <- primordialSweep S.bob S.carol
    Spec.assertEqWith s "CR 614.12 the sweep turned over the Giant and the Primordial, not the Warden entering beside Ixidron" faceDown swept
    Spec.assertEqWith s "both creature cards entered under alice" returned 2
  Spec.it s "CR 614.12 Sepulchral Primordial returning Ixidron before a Soul Warden leaves the Warden face up" $ do
    (faceDown, returned) <- primordialSweep S.carol S.bob
    Spec.assertEqWith s "CR 614.12 the sweep turned over the Giant and the Primordial, not the Warden entering beside Ixidron" faceDown swept
    Spec.assertEqWith s "both creature cards entered under alice" returned 2

-- Announce one target for every slot offering one of `picks`, none elsewhere,
-- and take `picks` out of each offer -- FILTERED from the offer, so the
-- recipient is the one the pool tagged. Every AnnounceTargets offer is
-- recorded, in slot order, as object ids.
aiming :: Set.Set ObjectId.ObjectId -> Prompt.Prompt r -> State.State [[Set.Set ObjectId.ObjectId]] r
aiming picks p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> do
    State.modify (<> [fmap (objectsOf . snd) (Map.elems offers)])
    pure (fmap (\(_, legal) -> if Set.null (Set.intersection picks (objectsOf legal)) then 0 else 1 :: Natural.Type.Natural) offers)
  Prompt.ChooseTargets _ _ _ asked -> pure (fmap (Set.filter (maybe False (`Set.member` picks) . Recipient.objectOf) . snd) asked)
  _ -> pure (S.identityAnswer p)

-- `aiming picks`, and CR 608.2d's per-member "you may" answered with `takes`,
-- FILTERED from the swept members.
taking :: Set.Set ObjectId.ObjectId -> Set.Set ObjectId.ObjectId -> Prompt.Prompt r -> State.State [[Set.Set ObjectId.ObjectId]] r
taking takes picks p = case p of
  Prompt.ChooseLoopMembers _ _ _ swept -> pure (Set.fromList (filter (maybe False (`Set.member` takes) . Recipient.objectOf) swept))
  _ -> aiming picks p

objectsOf :: Set.Set Recipient.Recipient -> Set.Set ObjectId.ObjectId
objectsOf = Set.fromList . Maybe.mapMaybe Recipient.objectOf . Set.toList

-- Every card in a zone, by name, top first for a library.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (fmap S.nameOf . (`Game.cardOf` gs)) (Game.zoneMembers zone pid gs)
