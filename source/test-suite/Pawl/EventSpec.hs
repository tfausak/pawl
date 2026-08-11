module Pawl.EventSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- CR 701.6a: the counterings recorded so far this turn, in order. The sibling of
-- Support.zoneChangesOf, kept local because Event.counter is the only funnel that
-- appends one and this module is the only reader.
counteringsOf :: GameState.GameState -> [Countering.Countering]
counteringsOf gs =
  let counteringOf event = case event of
        GameEvent.SpellCountered c -> Just c
        _ -> Nothing
   in Maybe.mapMaybe counteringOf (S.eventsOf gs)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Event" $ do
  Spec.it s "CR 614: with Rest in Peace out, a creature sent to the graveyard is exiled" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (theCreature, g1) = S.addCreature piker S.bob g0
        after = S.runPure S.identityAnswer g1 (Event.changeZone theCreature Zone.Graveyard)
        inExile = Set.size (GameState.exile after)
        gyCount = length (Game.zoneMembers Zone.Graveyard S.bob after)
    Spec.assertEqWith s "exiled, not in graveyard" gyCount 0
    Spec.assertEqWith s "one object in exile" inExile 1

  Spec.it s "CR 603.2g: the emitted event records the RESOLVED destination (exile)" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (theCreature, g1) = S.addCreature piker S.bob g0
        after = S.runPure S.identityAnswer g1 (Event.changeZone theCreature Zone.Graveyard)
    case S.zoneChangesOf after of
      zc : _ -> Spec.assertEqWith s "event says exile" (ZoneChange.to zc) Zone.Exile
      [] -> Spec.assertFailure s "expected an emitted zone change"

  Spec.it s "without Rest in Peace, a creature goes to the graveyard" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (theCreature, g1) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer g1 (Event.changeZone theCreature Zone.Graveyard)
    Spec.assertEq s (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1

  Spec.it s "CR 608.2n: a resolving spell is exiled from the stack under Rest in Peace" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (bolt, g1) = S.addLibraryCard lightningBolt S.bob g0
        onStack = g1 {GameState.stack = bolt : GameState.stack g1, GameState.objects = Map.adjust (\o -> o {Object.zone = Zone.Stack}) bolt (GameState.objects g1)}
        after = S.runPure S.identityAnswer onStack (Event.changeZone bolt Zone.Graveyard)
    Spec.assertEqWith s "spell exiled, graveyard empty" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0

  Spec.it s "CR 701.6a Event.counter puts a countered spell into its owner's graveyard" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (spellId, onStack) = S.spellOnStack piker S.bob (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer onStack (Event.counter S.noSource S.alice spellId)
    Spec.assertEqWith s "off the stack" (GameState.stack after) []
    Spec.assertEqWith s "in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "not on the battlefield" (S.creaturesInPlay S.bob after) 0
    -- The distinct event, recorded ALONGSIDE the Moved event the same move
    -- files -- never instead of it. The Moved event is what CR 400.7 did;
    -- this one is what the move WAS, and CR 608.2n's resolved spell reaches
    -- the same graveyard by the same zone pair.
    Spec.assertEqWith
      s
      "and a countering event names the spell, the source and its controller"
      (counteringsOf after)
      [Countering.MkCountering spellId S.noSource S.alice]
    Spec.assertEqWith s "beside the zone change, not instead of it" (length (S.zoneChangesOf after)) 1

  -- CR 113.6g at the funnel itself, without a countering spell in the way:
  -- Event.counter is what CR 701.6a's "remove it from the stack" runs
  -- through, and the gate is there rather than in the Counter opcode so that
  -- every future counterer inherits it.
  Spec.it s "CR 113.6g Event.counter is a no-op on a spell that can't be countered" $ do
    rendingVolley <- S.printingOf s registry "Rending Volley"
    let (spellId, onStack) = S.spellOnStack rendingVolley S.bob (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer onStack (Event.counter S.noSource S.alice spellId)
    Spec.assertEqWith s "still on the stack, under its original id" (GameState.stack after) [spellId]
    Spec.assertEqWith s "nothing reached the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    -- CR 603.2g: "an event that's prevented or replaced won't trigger
    -- anything." A spell that can't be countered was never countered, so the
    -- gate must record nothing at all -- which is what keeps Baral, Chief of
    -- Compliance silent in Pawl.TriggerSpec's composition case.
    Spec.assertEqWith s "and no countering event was recorded" (counteringsOf after) []

  Spec.it s "CR 614 a countered spell is exiled under Rest in Peace" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (spellId, onStack) = S.spellOnStack piker S.bob g0
        after = S.runPure S.identityAnswer onStack (Event.counter S.noSource S.alice spellId)
    Spec.assertEqWith s "not in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "exiled instead" (length (Game.zoneMembers Zone.Exile S.bob after)) 1

  Spec.it s "CR 603/614 whole card: cast Rest in Peace, ETB exiles graveyards, then deaths are exiled" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    let base = S.landsInPlay plains 2
        (deadId, withDead) = S.addLibraryCard piker S.alice base
        g0 = S.runPure S.identityAnswer withDead (Event.changeZone deadId Zone.Graveyard) -- a card already in the graveyard
        (g1, ripId) = S.handOne restInPeace g0
        afterCast = snd (Engine.runGamePure S.identityAnswer g1 (S.cast S.alice ripId))
        -- run priority: both players pass, RiP resolves and enters, its ETB is
        -- placed (CR 117.5) and resolves, exiling the graveyard.
        settled = snd (Engine.runGamePure S.identityAnswer afterCast Engine.priorityLoop)
    Spec.assertEqWith s "alice's graveyard exiled by the ETB" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 0
    Spec.assertEqWith s "Rest in Peace is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Rest in Peace") S.alice settled) 1
    Spec.assertEqWith s "stack empty" (GameState.stack settled) []

  -- CR 800.4b, sentence 2: "If a token would be created under the control of
  -- a player who has left the game, no token is created." NOT "is created and
  -- then removed" -- so the assertion is that the object count never moved,
  -- which a create-then-clean-up implementation would fail.
  --
  -- Three seats, because CR 800.1 gates the departure machinery on a game
  -- that began with more than two players.
  Spec.it s "CR 800.4b no token is created under the control of a player who has left the game" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let goblinCard = Printing.card piker
        gone = Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame
        before = Game.objectCount gone
        after = S.runPure S.identityAnswer gone (Event.createTokens S.alice goblinCard Nothing 2 TapState.Untapped Map.empty)
    Spec.assertBool s (notElem S.alice (Game.stillPlaying gone)) "alice really has left"
    Spec.assertEqWith s "no object was ever minted" (Game.objectCount after) before
    Spec.assertEqWith s "and nothing reached the battlefield" (Set.size (GameState.battlefield after)) 0

  -- The other half of the guard: a player still in the game is unaffected, so
  -- this cannot pass by refusing every token.
  Spec.it s "CR 800.4b a player still in the game still gets their tokens" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let goblinCard = Printing.card piker
        gone = Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame
        before = Game.objectCount gone
        after = S.runPure S.identityAnswer gone (Event.createTokens S.bob goblinCard Nothing 2 TapState.Untapped Map.empty)
    Spec.assertEqWith s "bob's two tokens exist" (Game.objectCount after) (before + 2)
    Spec.assertEqWith s "both on the battlefield" (Set.size (GameState.battlefield after)) 2

  Spec.it s "CR 111.2 createTokens puts a token on the battlefield and emits an enters event" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        goblinCard = Printing.card piker
        before = Game.objectCount base
        after = S.runPure S.identityAnswer base (Event.createTokens S.alice goblinCard Nothing 1 TapState.Untapped Map.empty)
        newIds = Set.toList (GameState.battlefield after)
    Spec.assertEqWith s "one more object exists" (Game.objectCount after) (before + 1)
    Spec.assertEqWith s "exactly one battlefield object" (length newIds) 1
    case newIds of
      [tokId] -> do
        Spec.assertEqWith s "cardOf sees the token" (Game.cardOf tokId after) (Just goblinCard)
        Spec.assertEqWith s "owned by its creator (CR 111.2)" (Projection.controllerOf tokId after) (Just S.alice)
        case Game.lookupObject tokId after of
          Just obj -> Spec.assertEqWith s "summoning sick (CR 302.6)" (Object.sickness obj) Sickness.Sick
          Nothing -> Spec.assertFailure s "token vanished"
        Spec.assertEqWith
          s
          "one enters-battlefield event emitted"
          (fmap ZoneChange.to (S.zoneChangesOf after))
          [Zone.Battlefield]
      _ -> Spec.assertFailure s "expected exactly one token"

  Spec.it s "CR 614 + 704.5d a token dies under Rest in Peace: exiled, then ceases" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    let base = Setup.emptyGame S.bothPlayers
        goblinCard = Printing.card piker
        (_, withRip) = S.addCreature restInPeace S.bob base
        (tokId, gs) = S.addToken goblinCard S.alice withRip
        -- Kill the token: route it to the graveyard; RiP redirects to exile.
        dying = S.runPure S.identityAnswer gs (Event.changeZone tokId Zone.Graveyard)
        settled = S.settleSba dying
    Spec.assertEqWith s "the token never entered a graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice dying)) 0
    Spec.assertEqWith s "it was redirected to exile" (length (Game.zoneMembers Zone.Exile S.alice dying)) 1
    Spec.assertEqWith s "after the SBA it has ceased to exist (gone from exile)" (length (Game.zoneMembers Zone.Exile S.alice settled)) 0

  -- CR 614.1a: Anafenza, the Foremost -- "If a nontoken creature an opponent
  -- owns would die or a creature card not on the battlefield would be put into
  -- an opponent's graveyard, exile that card instead." The first redirect in the
  -- pool that narrows by WHAT the moving object is, which is what
  -- ZoneChangePattern.whatObject carries.
  --
  -- Anafenza is ALICE's throughout, so `whoseObject = Opponents` reads bob. Every
  -- case below moves an object to a graveyard the same way, and they differ only
  -- in whether the Filter admits it -- which is what makes the negative cases
  -- controls rather than decoration: a redirect that fired for every zone change
  -- would pass the first case and fail all three of the last.
  --
  -- Nothing here needs CR 608.2h. Pawl.Engine.Event proposes the move BEFORE it
  -- performs one, so a creature that "would die" is still on the battlefield when
  -- Replacement.matchesFiltered reads it, which is CR 400.7's "as it last
  -- existed" reached structurally rather than through last known information.
  Spec.it s "CR 614.1a Anafenza exiles an opponent's dying nontoken creature" $ do
    anafenza <- S.printingOf s registry "Anafenza, the Foremost"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature anafenza S.alice (Setup.emptyGame S.bothPlayers)
        (theirs, g1) = S.addCreature piker S.bob g0
        -- The CR 701.8a destroy funnel, so the redirect is read off the
        -- pre-batch board a CR 608.2f batch supplies.
        after = S.runPure S.identityAnswer g1 (Event.destroy Regenerability.CantBeRegenerated [theirs])
    Spec.assertEqWith s "it never reached bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "it was exiled instead" (length (Game.zoneMembers Zone.Exile S.bob after)) 1

  -- The second clause: "a creature card not on the battlefield would be put into
  -- an opponent's graveyard". Off the battlefield there is no projection, so the
  -- Filter reads the PRINTED type line (Projection.baseCharacteristics), which is
  -- the other half of the same field.
  Spec.it s "CR 614.1a Anafenza exiles an opponent's creature card headed for a graveyard from a library" $ do
    anafenza <- S.printingOf s registry "Anafenza, the Foremost"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature anafenza S.alice (Setup.emptyGame S.bothPlayers)
        (theirs, g1) = S.addLibraryCard piker S.bob g0
        after = S.runPure S.identityAnswer g1 (Event.changeZone theirs Zone.Graveyard)
    Spec.assertEqWith s "it never reached bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "it was exiled instead" (length (Game.zoneMembers Zone.Exile S.bob after)) 1

  -- CONTROL for Filter.Not Filter.IsToken. The same player, the same zone pair,
  -- the same funnel -- and a token, which Anafenza's "nontoken" excludes (CR
  -- 111.6). Rest in Peace, whose pattern admits every object, exiles exactly this
  -- token in the CR 704.5d case above.
  Spec.it s "CR 614.1a Anafenza does not exile an opponent's dying TOKEN creature" $ do
    anafenza <- S.printingOf s registry "Anafenza, the Foremost"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature anafenza S.alice (Setup.emptyGame S.bothPlayers)
        (theirs, g1) = S.addToken (Printing.card piker) S.bob g0
        after = S.runPure S.identityAnswer g1 (Event.destroy Regenerability.CantBeRegenerated [theirs])
    Spec.assertEqWith s "the token reached bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.bob after)) 0

  -- CONTROL for Filter.HasCardType CardType.Creature. A noncreature card of
  -- bob's, moved to his graveyard exactly as the creature card above was.
  Spec.it s "CR 614.1a Anafenza does not exile an opponent's NONCREATURE card" $ do
    anafenza <- S.printingOf s registry "Anafenza, the Foremost"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, g0) = S.addCreature anafenza S.alice (Setup.emptyGame S.bothPlayers)
        (theirs, g1) = S.addLibraryCard lightningBolt S.bob g0
        after = S.runPure S.identityAnswer g1 (Event.changeZone theirs Zone.Graveyard)
    Spec.assertEqWith s "the instant reached bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.bob after)) 0

  -- CONTROL for ControllerRelation.Opponents, which CR 400.3 makes an OWNER
  -- test: Anafenza's controller loses her own creatures to her own graveyard.
  Spec.it s "CR 614.1a Anafenza does not exile her own controller's dying creature" $ do
    anafenza <- S.printingOf s registry "Anafenza, the Foremost"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature anafenza S.alice (Setup.emptyGame S.bothPlayers)
        (ours, g1) = S.addCreature piker S.alice g0
        after = S.runPure S.identityAnswer g1 (Event.destroy Regenerability.CantBeRegenerated [ours])
    Spec.assertEqWith s "alice's own creature reached her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 0

  Spec.it s "CR 701.19a / 514.2 a regeneration shield is dropped at cleanup (this turn)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (oid, gs0) = S.addCreature piker S.alice base
        cleared = Expiry.dropAtCleanup (S.addRegenShield oid gs0)
    Spec.assertEqWith s "no shields remain" (GameState.replacements cleared) []

  Spec.it s "CR 701.19a Event.destroy consumes a shield and regenerates instead" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (oid, gs0) = S.addCreature piker S.alice base
        damaged = S.markDamage oid 1 gs0
        shielded = S.addRegenShield oid damaged
        after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [oid])
    Spec.assertEqWith s "still on the battlefield (regenerated, not destroyed)" (Set.member oid (GameState.battlefield after)) True
    Spec.assertEqWith s "shield spent" (GameState.replacements after) []
    case Game.lookupObject oid after of
      Just obj -> do
        Spec.assertEqWith s "tapped (CR 701.19a)" (Object.tapped obj) TapState.Tapped
        Spec.assertEqWith s "damage removed (CR 701.19a)" (Object.damage obj) 0
      Nothing -> Spec.assertFailure s "the creature vanished"

  Spec.it s "CR 701.19a a second destroy with no shield left kills it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (oid, gs0) = S.addCreature piker S.alice base
        once = S.runPure S.identityAnswer (S.addRegenShield oid gs0) (Event.destroy Regenerability.Regenerable [oid]) -- regenerated
        twice = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [oid]) -- no shield -> dies
    Spec.assertEqWith s "gone from the battlefield" (Set.member oid (GameState.battlefield twice)) False

  Spec.it s "CR 700.4 Event.destroy no-ops on an indestructible permanent" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let base = Setup.emptyGame S.bothPlayers
        (oid, gs0) = S.addCreature darksteelMyr S.alice base
        after = S.runPure S.identityAnswer gs0 (Event.destroy Regenerability.Regenerable [oid])
    Spec.assertEqWith s "indestructible survives" (Set.member oid (GameState.battlefield after)) True

  Spec.it s "CR 122.2 counters cease to exist when an object changes zones" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 0
        (oid, withCreature) = S.addCreature piker S.bob base
        withCounter = S.addCounter CounterKind.PlusOnePlusOne 2 oid withCreature
        -- Bounce to hand: changeZone mints a new incarnation (CR 400.7).
        bounced = S.runPure S.identityAnswer withCounter (Event.changeZone oid Zone.Hand)
        -- Total (no `head`): map over the hand zone; expect exactly one card, empty.
        handCounters = fmap (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h bounced)) (Game.zoneMembers Zone.Hand S.bob bounced)
    Spec.assertEqWith s "counter present before the move" (maybe Map.empty Object.counters (Game.lookupObject oid withCounter)) (Map.fromList [(CounterKind.PlusOnePlusOne, 2)])
    Spec.assertEqWith s "the one new incarnation in hand has no counters" handCounters [Map.empty]

  Spec.it s "CR 704.5q both counter kinds annihilate to zero (symmetric)" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 0
        (oid, gs0) = S.addCreature piker S.bob base
        gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
        gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs1
        after = S.settleSba gs2
    Spec.assertEqWith s "no counters remain" (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject oid after)) Map.empty

  Spec.it s "CR 704.5q annihilation removes N = min (asymmetric)" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 0
        (oid, gs0) = S.addCreature piker S.bob base
        gs1 = S.addCounter CounterKind.PlusOnePlusOne 2 oid gs0
        gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs1
        after = S.settleSba gs2
    Spec.assertEqWith s "one +1/+1 remains" (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid after)) (Just 1)
    Spec.assertEqWith s "no -1/-1 remains" (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject oid after)) (Just 0)
    -- Net P/T unchanged by annihilation: base 2/1 + net +1/+1 = 3/2.
    Spec.assertEqWith s "power still 3" (Projection.powerOf oid after) (Just 3)
    Spec.assertEqWith s "toughness still 2" (Projection.toughnessOf oid after) (Just 2)

  Spec.it s "CR 603.2 SelfDealsCombatDamageToPlayer matches the bearer's combat damage to a player" $ do
    let bearer = ObjectId.MkObjectId 1
        ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent bearer (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat)
    Spec.assertBool s (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev) "matches"

  Spec.it s "it does not match combat damage to a creature" $ do
    let bearer = ObjectId.MkObjectId 1
        ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent bearer (Recipient.ToCreature (ObjectId.MkObjectId 2)) 2 False False False 0 Nothing DamageKind.Combat)
    Spec.assertBool s (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev)) "no match"

  Spec.it s "it does not match noncombat damage to a player" $ do
    let bearer = ObjectId.MkObjectId 1
        ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent bearer (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Noncombat)
    Spec.assertBool s (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev)) "no match"

  Spec.it s "CR 400.7: a zone change forgets attachment" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay plains 1
        (host, withHost) = S.addCreature piker S.bob base
        (rider, withRider) = S.addCreature piker S.alice withHost
        attached = S.attach rider host withRider
        bounced = S.runPure S.identityAnswer attached (Event.changeZone rider Zone.Hand)
        moved = filter (\o -> Object.zone o == Zone.Hand) (Map.elems (GameState.objects bounced))
    Spec.assertEqWith s "attached before the move" (fmap Object.attachedTo (Game.lookupObject rider attached)) (Just (Just (Recipient.ToCreature host)))
    Spec.assertEqWith s "one card in hand" (length moved) 1
    Spec.assertEqWith s "the new incarnation is unattached" (fmap Object.attachedTo moved) [Nothing]
