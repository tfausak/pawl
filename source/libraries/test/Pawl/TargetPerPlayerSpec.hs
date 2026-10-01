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
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection.Engine
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Subtype as Subtype
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
  -- The spell's road: CR 601.2c's announcement as the spell is cast, and CR
  -- 608.2b's re-check as it resolves.
  Spec.it s "CR 601.2c Dismantling Wave destroys up to one artifact or enchantment of each opponent's" $ do
    plains <- S.printingOf s registry "Plains"
    ornithopter <- S.printingOf s registry "Ornithopter"
    anthem <- S.printingOf s registry "Glorious Anthem"
    wave <- S.printingOf s registry "Dismantling Wave"
    let (bobsId, g1) = S.addPermanent ornithopter S.bob (S.landsFor plains S.alice 3 S.threePlayerGame)
        (carolsId, g2) = S.addPermanent anthem S.carol g1
        (_, g3) = S.addPermanent ornithopter S.alice g2
        (gs, waveId) = S.handOne wave g3
        (after, offers) = State.runState (fmap snd (Engine.runGame (aiming (Set.fromList [bobsId, carolsId])) gs (S.cast S.alice waveId >> Stack.resolveTop))) []
    Spec.assertEqWith s "bob's Ornithopter was destroyed" (onBattlefield "Ornithopter" S.bob after) 0
    Spec.assertEqWith s "carol's Anthem was destroyed" (onBattlefield "Glorious Anthem" S.carol after) 0
    Spec.assertEqWith s "alice's own Ornithopter stayed" (onBattlefield "Ornithopter" S.alice after) 1
    Spec.assertEqWith s "one slot per opponent, each its own player's" offers [[Set.singleton bobsId, Set.singleton carolsId]]

  -- Exactly one target per opponent. alice holds seven Islands; bob controls a
  -- Goblin Piker and a Forest, carol an Ornithopter unless `carolEmpty`.
  let thievery carolEmpty = do
        island <- S.printingOf s registry "Island"
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        ornithopter <- S.printingOf s registry "Ornithopter"
        card <- S.printingOf s registry "Blatant Thievery"
        let (pikerId, g1) = S.addPermanent piker S.bob (S.landsFor island S.alice 7 S.threePlayerGame)
            (_, g2) = S.addPermanent forest S.bob g1
            (thopterId, g3) = if carolEmpty then (pikerId, g2) else S.addPermanent ornithopter S.carol g2
            (gs, spellId) = S.handOne card g3
        pure (gs, spellId, pikerId, thopterId)
      steal picks gs spellId = State.evalState (fmap snd (Engine.runGame (aiming picks) gs (S.cast S.alice spellId))) []
  -- Sylvan Primordial's ruling for the same template: a player with nothing to
  -- target gets no target, so carol controlling nothing does not stop the cast.
  -- The pair's other half: with NO opponent controlling anything the slot
  -- cannot be filled at all.
  Spec.it s "CR 601.2c Blatant Thievery with carol controlling nothing still takes bob's Piker" $ do
    (gs, spellId, pikerId, _) <- thievery True
    Spec.assertBool s (S.castable S.alice spellId gs) "castable with carol controlling nothing"
    Spec.assertEqWith s "alice controls bob's Piker" (Projection.controllerOf pikerId (resolve (steal (Set.singleton pikerId) gs spellId))) (Just S.alice)
    island <- S.printingOf s registry "Island"
    card <- S.printingOf s registry "Blatant Thievery"
    let (bare, bareSpellId) = S.handOne card (S.landsFor island S.alice 7 S.threePlayerGame)
    Spec.assertBool s (not (S.castable S.alice bareSpellId bare)) "and not castable with no opponent controlling anything"
  -- Blatant Thievery's own ruling: "If a permanent changes controller after
  -- being targeted but before this spell resolves, you won't gain control of
  -- that permanent."
  Spec.it s "CR 608.2b Blatant Thievery does not take the Piker once carol controls it" $ do
    (gs, spellId, pikerId, thopterId) <- thievery False
    let after = resolve (S.giveControl pikerId S.carol (steal (Set.fromList [pikerId, thopterId]) gs spellId))
    Spec.assertEqWith s "carol still controls the Piker" (Projection.controllerOf pikerId after) (Just S.carol)
    Spec.assertEqWith s "alice controls carol's Ornithopter" (Projection.controllerOf thopterId after) (Just S.alice)
  -- The activation's road (CR 602.2b's announcement), on a loyalty ability.
  Spec.it s "CR 602.2b The Theorist, Jace Beleren's -2 returns each opponent's chosen artifact or creature" $ do
    jace <- S.printingOf s registry "The Theorist, Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    ornithopter <- S.printingOf s registry "Ornithopter"
    anthem <- S.printingOf s registry "Glorious Anthem"
    case Face.activatedAbilities (S.combinedFace jace) of
      [_, minusTwo, _] -> do
        let (jaceId, g1) = S.addPermanent jace S.alice S.threePlayerGame
            (pikerId, g2) = S.addPermanent piker S.bob (S.addCounter CounterKind.Loyalty 3 jaceId g1)
            (thopterId, g3) = S.addPermanent ornithopter S.carol g2
            -- carol's Anthem is neither an artifact nor a creature: not offered.
            (_, g4) = S.addPermanent anthem S.carol g3
            (_, gs) = S.addPermanent piker S.alice g4
            (after, offers) = State.runState (fmap snd (Engine.runGame (aiming (Set.fromList [pikerId, thopterId])) gs (Activate.activateAbility S.alice jaceId minusTwo >> Stack.resolveTop))) []
        Spec.assertBool s (elem (named "Goblin Piker") (namesIn Zone.Hand S.bob after)) "bob's Piker is in his hand"
        Spec.assertBool s (elem (named "Ornithopter") (namesIn Zone.Hand S.carol after)) "carol's Ornithopter is in her hand"
        Spec.assertEqWith s "carol's Anthem stayed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Glorious Anthem")) S.carol after) 1
        Spec.assertEqWith s "alice's own Piker stayed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 1
        Spec.assertEqWith s "one slot per opponent, each its own player's" offers [[Set.singleton pikerId, Set.singleton thopterId]]
      _ -> Spec.assertFailure s "The Theorist has three loyalty abilities"

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
  -- "For each player": alice's own graveyard gets a copy too, and every returned
  -- card is a Zombie under alice's control.
  Spec.it s "CR 601.2c Afterlife from the Loam takes one creature card from each player's graveyard as Zombies" $ do
    (gs, spellId, (pikerId, bobThopterId, elvesId, aliceThopterId)) <- graveyards "Afterlife from the Loam"
    let picks = Set.fromList [pikerId, elvesId, aliceThopterId]
        (after, offers) = State.runState (fmap snd (Engine.runGame (taking Set.empty picks) gs (S.cast S.alice spellId >> Stack.resolveTop))) []
        entered = [o | o <- Set.toList (GameState.battlefield after), Projection.controllerOf o after == Just S.alice, fmap S.nameOf (Game.cardOf o after) /= named "Swamp"]
    Spec.assertEqWith s "alice controls bob's Piker" (controls "Goblin Piker" S.alice after) 1
    Spec.assertEqWith s "alice controls carol's Elves" (controls "Llanowar Elves" S.alice after) 1
    Spec.assertEqWith s "alice controls her own Ornithopter" (controls "Ornithopter" S.alice after) 1
    Spec.assertEqWith s "each is a Zombie" (fmap (\o -> Set.member Subtype.Zombie (Projection.Engine.subtypesOf o after)) entered) [True, True, True]
    Spec.assertEqWith s "one slot per player, each offering that player's creature cards" offers [[Set.singleton aliceThopterId, Set.fromList [pikerId, bobThopterId], Set.singleton elvesId]]

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
