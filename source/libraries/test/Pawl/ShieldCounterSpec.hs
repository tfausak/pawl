{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Replacement over shield counters (CR 122.1c) and the remaining
-- printed replacements after them: Dragonstorm Globe, Tidewalker, a redirected
-- permanent spell, Hurr Jackal, Queen Allenal, Chatterfang, Quina. Split out of
-- Pawl.ReplacementSpec, which keeps the machinery.
module Pawl.ShieldCounterSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import Pawl.PreventionSpec (answersFor, castAndResolve, countersOn, newestNamed, raceAnswer, settleDamage, theAbility, wasAskedToOrderDamage)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DestructionCause as DestructionCause
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

-- CR 122.1c: the replacement and the prevention effect one or more shield counters
-- create. Gameplay-level throughout: Swooping Protector is cast and enters with its
-- counter accumulated into the entry's own CR 616.1 pool, and every spell aimed at
-- it afterwards is a real card cast and resolved.
--
-- The two effects are proven SEPARATELY, and that separation is the point rather
-- than tidiness: a board where a shielded creature merely survives cannot tell "the
-- destruction was replaced" from "the damage was prevented". So the destruction
-- cases destroy without dealing damage (Doom Blade) and the damage cases deal damage
-- without destroying -- Lightning Bolt's 3 kills a 2/1 only through CR 704.5g, which
-- is a rule's destruction and reaches the shield through neither sentence.
shieldCounterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
shieldCounterSpec s registry = Spec.describe s "Shield counters (CR 122.1c)" $ do
  let protectorName = CardName.MkCardName (Text.pack "Swooping Protector")
      -- alice CASTS the bird rather than having it placed, so its counter arrives
      -- through Event.addEnteringCounters (CR 306.5b's as-it-enters clause) into
      -- the entry's own CR 616.1 pool, where a scaling replacement can reach it.
      -- Four Plains pay the {3}{W}; `extra` seats the
      -- lands whatever spell the case aims at the bird needs, and `scaler` seats a
      -- counter-scaling permanent under a named player.
      board extra scaler = do
        plains <- S.printingOf s registry "Plains"
        protector <- S.printingOf s registry "Swooping Protector"
        extras <- Monad.mapM (S.printingOf s registry) extra
        seat <- Monad.mapM (\(name, _) -> S.printingOf s registry name) scaler
        let landed = List.foldl' (\g p -> snd (S.addPermanent p S.alice g)) (S.landsInPlay plains 4) extras
            seated = case (seat, scaler) of
              (Just printing, Just (_, pid)) -> snd (S.addPermanent printing pid landed)
              _ -> landed
            (held, g1) = S.addHandCard protector S.alice seated
            after = S.runPure S.identityAnswer g1 (S.cast S.alice held >> Stack.resolveTop)
        pure (newestNamed protectorName after, after)
      shields = countersOn CounterKind.Shield
      -- One of alice's cards, cast at the bird from her hand and resolved.
      castAt victim printing gs =
        let (held, g1) = S.addHandCard printing S.alice gs
         in S.runPure (raceAnswer victim victim) g1 (S.cast S.alice held >> Stack.resolveTop)
      -- One noncombat damage event, from `src`, at `n`.
      hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing Nothing mempty False DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- alice's Palace Guard with `n` shield counters written onto it, bob's
      -- Spider-Punk beside it when `withPunk`, and two Mountains for the Bolt the
      -- CR 615.12 cases below aim at it. A 1/4 rather than the bird because a
      -- permanent that DIES to the unprevented damage reads 0 counters under
      -- either reading of the rule (CR 122.2), which tells them apart not at all.
      guardBoard withPunk n = do
        mountain <- S.printingOf s registry "Mountain"
        guardPrinting <- S.printingOf s registry "Palace Guard"
        punkPrinting <- S.printingOf s registry "Spider-Punk"
        let (guard_, g1) = S.addPermanent guardPrinting S.alice (S.landsInPlay mountain 2)
            shielded = S.addCounter CounterKind.Shield n guard_ g1
        pure (guard_, if withPunk then snd (S.addPermanent punkPrinting S.bob shielded) else shielded)
      -- Spend the counter on `src`'s hit first (CR 101.4c), keyed on the SOURCE
      -- id rather than on a batch position, so the assertion does not depend on
      -- the order the batch was gathered in.
      counterFirst :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      counterFirst src p = case p of
        Prompt.OrderDamage _ _ events ->
          let key e = (DamageEvent.source e /= src, DamageEvent.source e)
           in fmap fst (List.sortOn (key . snd) (zip [0 ..] events))
        _ -> S.identityAnswer p
  -- CR 306.5b / 614.16: the counter accumulates into the entry's own CR 616.1
  -- pool, so the two replacements that scale a placement reach it there. Three
  -- DISTINCT counts off one card -- doubled, unreplaced and halved -- which is
  -- what separates "the entry pool was used" from "the number was written onto
  -- the object".
  Spec.it s "CR 306.5b the shield counter the bird enters with reaches the entry's CR 616.1 pool" $ do
    (doubledBird, doubled) <- board [] (Just ("Doubling Season", S.alice))
    (plainBird, plain) <- board [] Nothing
    (halvedBird, halved) <- board [] (Just ("Vorinclex, Monstrous Raider", S.bob))
    case (doubledBird, plainBird, halvedBird) of
      (Just twice, Just once, Just half) -> do
        Spec.assertEqWith s "Doubling Season: twice one" (shields twice doubled) 2
        Spec.assertEqWith s "unreplaced: the printed one" (shields once plain) 1
        Spec.assertEqWith s "bob's praetor halves alice's placement, rounded down" (shields half halved) 0
      _ -> Spec.assertFailure s "the bird did not reach the battlefield"
  -- CR 122.1c's "as the result of an EFFECT", as a pair of boards differing in
  -- nothing but the destruction's cause. Through the two doors rather than through
  -- gameplay because that is the only way to hold everything else equal: reaching CR
  -- 704.5g against a SHIELDED permanent needs marked damage equal to its toughness,
  -- and the prevention half stops damage being marked for as long as a counter is
  -- there, so the gameplay route has to break the prevention half first. The
  -- Spider-Punk case below is that route, and it proves the same gate a second time
  -- at gameplay level.
  Spec.it s "CR 122.1c the counter does not save the bird from a rule's destruction" $ do
    (bird, entered) <- board [] Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let byEffect = S.runPure S.identityAnswer entered (Event.destroy Regenerability.Regenerable [oid])
            byRule = S.runPure S.identityAnswer entered (Event.destroyInBatch entered DestructionCause.ByRule Regenerability.Regenerable [oid])
        Spec.assertEqWith s "setup: one shield counter, on both boards" (shields oid entered) 1
        Spec.assertBool s (Set.member oid (GameState.battlefield byEffect)) "an effect's destruction is replaced"
        Spec.assertEqWith s "spending the counter" (shields oid byEffect) 0
        Spec.assertBool s (not (Set.member oid (GameState.battlefield byRule))) "the rule's destruction is not"
        -- CR 122.2: no assertion about the dead permanent's counters. They ceased to
        -- exist with the incarnation that held them, so the id reads 0 whether the
        -- shield was spent or ignored, which tells the two apart not at all.
        Spec.assertEqWith s "and it reached its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice byRule)) 1
  -- The gather's SHORT-CIRCUIT reads copiable rules text, and a shield counter is
  -- on none of it: Projection.replacementsAffecting would answer [] for a board whose only
  -- replacement is CR 122.1c's, so this case is what makes that disjunct
  -- load-bearing rather than a fence. Every producer in the pool is itself an entry
  -- replacement and so passes the short-circuit on its own printed text, which is
  -- why the counter here is written on directly -- a Goblin Piker prints nothing at
  -- all.
  Spec.it s "CR 122.1c a shield on a permanent that prints no replacement is still gathered" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (pikerId, g1) = S.addPermanent pikerPrinting S.alice (S.landsInPlay swamp 2)
        shielded = S.addCounter CounterKind.Shield 1 pikerId g1
        after = S.settleSba (castAt pikerId doomBlade shielded)
    Spec.assertBool s (Set.member pikerId (GameState.battlefield after)) "the Piker survived the Doom Blade"
    Spec.assertEqWith s "spending the counter" (shields pikerId after) 0
  -- CR 101.4c OVER CR 122.1c. One counter facing two simultaneous damage events is
  -- a resource that covers one of them and not the other, so which one it covers is
  -- a choice, and CR 101.4c gives it to the player making both CR 616.1 choices --
  -- "if no order is specified, the player chooses the order". The unit is the EVENT
  -- and not the amount: the counter prevents a whole event whatever its size, which
  -- is why the shield of CR 615.7 and this one are contested in different units.
  --
  -- The two answers leave DIFFERENT BOARDS, which is what makes the choice observable
  -- rather than bookkeeping: 5 and 2 at a 3/3 with one counter, so covering the 5
  -- leaves a survivor with 2 marked and covering the 2 leaves 5 marked on a creature
  -- CR 704.5g then destroys. Every number distinct -- 5, 2, toughness 3, one counter
  -- -- so no two readings of the rule land on the same board.
  --
  -- The counter is written onto a Hill Giant rather than carried by Swooping
  -- Protector because the bird's toughness of 1 makes "survived" unreachable, and
  -- survival is half of what tells the two answers apart. What is under test is
  -- which event the counter reaches and not how it got there; a real card putting
  -- it there is the CR 122.6 case at the top of this group.
  --
  -- The DAMAGE BATCH is hand-built and the shield is a real rule's, for
  -- mendingHandsSpec's reason -- and here the batch's gather order is itself the
  -- input the choice has to beat, which only a hand-built batch can state.
  Spec.it s "CR 101.4c one counter facing two simultaneous hits covers the one its controller says" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    giantPrinting <- S.printingOf s registry "Hill Giant"
    let (giant, g1) = S.addPermanent giantPrinting S.alice (S.landsInPlay plains 1)
        (big, g2) = S.addPermanent pikerPrinting S.bob g1
        (small, g3) = S.addPermanent pikerPrinting S.bob g2
        shielded = S.addCounter CounterKind.Shield 1 giant g3
        batch = [hit big (Recipient.ToCreature giant) 5, hit small (Recipient.ToCreature giant) 2]
        tookTheBig = settleDamage (counterFirst big) shielded batch
        tookTheSmall = settleDamage (counterFirst small) shielded batch
    Spec.assertEqWith s "setup: one counter, and two events it cannot both cover" (shields giant shielded) 1
    Spec.assertBool
      s
      (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch)))
      "alice was asked which damage the counter prevents"
    -- CR 615.6: a prevented event never happens, so the board says which of the two
    -- the counter reached twice over -- in what was marked and in what survived.
    Spec.assertEqWith s "the counter covers the 5: only the 2 happens" (amounts tookTheBig) [2]
    Spec.assertEqWith s "so 2 is marked on the 3/3" (S.damageOf giant tookTheBig) (Just 2)
    Spec.assertBool s (Set.member giant (GameState.battlefield (S.settleSba tookTheBig))) "and it survives"
    Spec.assertEqWith s "the counter covers the 2 instead: only the 5 happens" (amounts tookTheSmall) [5]
    Spec.assertEqWith s "so 5 is marked on the same 3/3" (S.damageOf giant tookTheSmall) (Just 5)
    Spec.assertBool s (not (Set.member giant (GameState.battlefield (S.settleSba tookTheSmall)))) "and CR 704.5g destroys it"
    -- CR 122.1c: one counter comes off per application either way, so the answer
    -- changes which event was covered and never how much the pair could cover.
    Spec.assertEqWith s "one counter spent either way" (shields giant tookTheBig) 0
    Spec.assertEqWith s "one counter spent either way" (shields giant tookTheSmall) 0
  -- The elision half, and the discriminating twin of the case above: two counters
  -- cover two events in any order, so there is nothing to decide and nothing is
  -- asked. One difference from that board -- the number of counters -- and the same
  -- seats, sources and amounts.
  Spec.it s "CR 122.1c two counters cover two simultaneous hits, and ask nothing" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    giantPrinting <- S.printingOf s registry "Hill Giant"
    let (giant, g1) = S.addPermanent giantPrinting S.alice (S.landsInPlay plains 1)
        (big, g2) = S.addPermanent pikerPrinting S.bob g1
        (small, g3) = S.addPermanent pikerPrinting S.bob g2
        shielded = S.addCounter CounterKind.Shield 2 giant g3
        batch = [hit big (Recipient.ToCreature giant) 5, hit small (Recipient.ToCreature giant) 2]
        after = settleDamage S.identityAnswer shielded batch
    Spec.assertBool
      s
      (not (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch))))
      "no OrderDamage was raised: two counters cover both events"
    Spec.assertEqWith s "neither event happened" (amounts after) []
    Spec.assertEqWith s "nothing is marked" (S.damageOf giant after) (Just 0)
    Spec.assertEqWith s "and both counters paid for it" (shields giant after) 0
    Spec.assertBool s (Set.member giant (GameState.battlefield (S.settleSba after))) "the Giant is untouched"
  -- CR 615.12's MIDDLE clause -- "those effects won't prevent any damage, but any
  -- additional effects they have will take place" -- over CR 122.1c's "prevent
  -- that damage and remove a shield counter from it". The removal is
  -- amount-INDEPENDENT, which is what tells this reading from "an inert prevention
  -- does nothing at all": the Bolt lands in full AND the counter comes off.
  --
  -- THE CONTROL is the same board minus Spider-Punk, one difference and nothing
  -- else, so no assertion here can pass on a board whose shield was inapplicable:
  -- the control's shield prevents the whole 3.
  Spec.it s "CR 615.12 an unpreventable Bolt is prevented not at all and takes the counter anyway" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (guard_, punked) <- guardBoard True 1
    (controlGuard, unpunked) <- guardBoard False 1
    let once = S.settleSba (castAt guard_ bolt punked)
        control = S.settleSba (castAt controlGuard bolt unpunked)
    Spec.assertEqWith s "setup: one shield counter on the 1/4, on both boards" (shields guard_ punked) 1
    Spec.assertEqWith s "the whole 3 is marked: the shield prevented none of it" (S.damageOf guard_ once) (Just 3)
    Spec.assertBool s (Set.member guard_ (GameState.battlefield once)) "and the 1/4 lived through it, so its counters are still readable"
    Spec.assertEqWith s "the counter came off anyway (CR 615.12's middle clause)" (shields guard_ once) 0
    Spec.assertEqWith s "control: without Spider-Punk the same Bolt is prevented whole" (S.damageOf controlGuard control) (Just 0)
    Spec.assertEqWith s "control: spending the same one counter" (shields controlGuard control) 0
  -- The same divergence where it reaches the BOARD rather than the bookkeeping,
  -- as its own case so that it fails on its own: a counter wrongly left on would
  -- go on to replace the next destruction (CR 122.1c's first sentence).
  Spec.it s "CR 122.1c the counter the unpreventable Bolt spent no longer replaces a destruction" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (guard_, punked) <- guardBoard True 1
    let once = S.settleSba (castAt guard_ bolt punked)
        destroyed = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [guard_])
    Spec.assertBool s (Set.member guard_ (GameState.battlefield once)) "setup: the 1/4 survived the Bolt"
    Spec.assertBool s (not (Set.member guard_ (GameState.battlefield destroyed))) "and an effect's destruction then goes unreplaced"
  -- CR 615.12a: "a prevention effect is applied to any particular unpreventable
  -- damage event just once". The inert application does not re-invoke itself, so
  -- one of the two counters comes off and not both -- the same "only one shield
  -- counter is removed" the preventing path obeys, which is what makes the CR
  -- 616.1 applied-set load-bearing here: the event survives the application, so
  -- the loop goes round again and re-collects this very row.
  --
  -- One difference from the pair of cases above -- the number of counters -- and
  -- the same seats, spell, lands and body.
  Spec.it s "CR 615.12a the unpreventable Bolt's one application takes one counter, not both" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (guard_, punked) <- guardBoard True 2
    let once = S.settleSba (castAt guard_ bolt punked)
        destroyed = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [guard_])
    Spec.assertEqWith s "setup: two shield counters" (shields guard_ punked) 2
    Spec.assertEqWith s "the whole 3 is still marked" (S.damageOf guard_ once) (Just 3)
    Spec.assertEqWith s "one counter came off, not both" (shields guard_ once) 1
    Spec.assertBool s (Set.member guard_ (GameState.battlefield destroyed)) "and the survivor still replaces a destruction"
  -- The pool's GAMEPLAY route to CR 122.1c's "as the result of an EFFECT", which
  -- the case above's counter arithmetic is what makes reachable: three counters
  -- and two unpreventable Bolts leave 6 marked on a 1/4 with a counter still on
  -- it, so CR 704.5g's state-based action destroys a SHIELDED permanent -- and a
  -- rule's destruction is not one the pair may replace. Reaching this any other
  -- way is impossible while a counter is there, since the prevention half stops
  -- the damage being marked; the door-pair case above proves the same gate
  -- without gameplay.
  --
  -- No settle between the two Bolts, or CR 704.5g would run on the first one's 3.
  Spec.it s "CR 122.1c a rule's destruction is not replaced, though a counter is still there" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (guard_, punked) <- guardBoard True 3
    let bolted = castAt guard_ bolt (castAt guard_ bolt punked)
        twice = S.settleSba bolted
    Spec.assertEqWith s "setup: one counter per Bolt came off, leaving one" (shields guard_ bolted) 1
    Spec.assertEqWith s "setup: and 6 is marked on the 1/4, which CR 704.5g calls lethal" (S.damageOf guard_ bolted) (Just 6)
    Spec.assertBool s (not (Set.member guard_ (GameState.battlefield twice))) "and CR 704.5g destroyed it anyway"
  -- CR 101.4c over CR 615.12: once an inert application spends a counter, an
  -- UNPREVENTABLE event competes for that counter exactly as a preventable one
  -- does, so the one counter facing one of each is contested and its controller
  -- says which event gets it. The CR 615.7 shield's opposite is excruciatorSpec's
  -- mixed batch, which asks nothing: that shield is not reduced by unpreventable
  -- damage at all (CR 615.12's last sentence), so the Excruciator's event is no
  -- claim on it.
  --
  -- The two answers leave DIFFERENT boards, which is what makes the choice
  -- observable: the counter on the Excruciator's 3 removes it and prevents
  -- nothing, so the Piker's 2 lands too and the 1/4 takes 5; the counter on the
  -- Piker's 2 prevents that event whole and leaves 3 marked on a survivor. Every
  -- number distinct -- 3, 2, toughness 4, one counter.
  Spec.it s "CR 101.4c an unpreventable event contests the shield counter it would spend" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    guardPrinting <- S.printingOf s registry "Palace Guard"
    excruciator <- S.printingOf s registry "Excruciator"
    let (guard_, g1) = S.addPermanent guardPrinting S.alice (S.landsInPlay plains 1)
        (avatar, g2) = S.addPermanent excruciator S.bob g1
        (piker, g3) = S.addPermanent pikerPrinting S.bob g2
        shielded = S.addCounter CounterKind.Shield 1 guard_ g3
        batch = [hit avatar (Recipient.ToCreature guard_) 3, hit piker (Recipient.ToCreature guard_) 2]
        tookTheAvatar = settleDamage (counterFirst avatar) shielded batch
        tookThePiker = settleDamage (counterFirst piker) shielded batch
    Spec.assertEqWith s "setup: one counter, and two events it cannot both reach" (shields guard_ shielded) 1
    Spec.assertBool
      s
      (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch)))
      "alice was asked which of the two the counter goes to"
    Spec.assertEqWith s "spent on the unpreventable 3, it prevents nothing and both events happen" (amounts tookTheAvatar) [3, 2]
    Spec.assertEqWith s "so the 1/4 takes 5" (S.damageOf guard_ tookTheAvatar) (Just 5)
    Spec.assertBool s (not (Set.member guard_ (GameState.battlefield (S.settleSba tookTheAvatar)))) "and CR 704.5g destroys it"
    Spec.assertEqWith s "spent on the Piker's 2 instead, that event never happens" (amounts tookThePiker) [3]
    Spec.assertEqWith s "so only the unpreventable 3 is marked" (S.damageOf guard_ tookThePiker) (Just 3)
    Spec.assertBool s (Set.member guard_ (GameState.battlefield (S.settleSba tookThePiker))) "and it survives"
    Spec.assertEqWith s "one counter spent either way" (shields guard_ tookTheAvatar) 0
    Spec.assertEqWith s "one counter spent either way" (shields guard_ tookThePiker) 0

-- Dragonstorm Globe {3} Artifact, whole text: "Each Dragon you control enters
-- with an additional +1/+1 counter on it. / {T}: Add one mana of any color."
-- (checked against Scryfall)
--
-- CR 612.1's REPLACEMENT-EFFECT carrier, and the first producer in the pool that
-- reaches it: a CR 604.2 replacement watching OTHER objects, so the permanent
-- holding it is on the battlefield for a text change to point at. Every earlier
-- replacement naming a subtype matches Filter.IsSource instead, and hacking the
-- SPELL that holds such a row is the shape data/scenarios/shield-counter proves -- so this is one
-- of the two shapes the rule reaches, not the only one.
--
-- CR 612.2 licenses the swap: "Dragon" here is a creature type word used as a
-- creature type, on an artifact that is not itself a Dragon. CR 613.1c puts the
-- change at layer 3, so Projection.replacementsOf hands the rewritten row to the
-- CR 616.1 entry loop.
--
-- The board: alice controls the Globe, an Island and six Mountains, and holds
-- Artificial Evolution ({U}) plus the card named by `entering`. The Globe is on
-- the battlefield BEFORE either spell is cast, which is what makes its row live
-- when the entry loop runs.
globeChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> String -> m (GameState.GameState, Maybe ObjectId.ObjectId)
globeChain s registry swap entering = do
  island <- S.printingOf s registry "Island"
  mountain <- S.printingOf s registry "Mountain"
  globe <- S.printingOf s registry "Dragonstorm Globe"
  evolution <- S.printingOf s registry "Artificial Evolution"
  creature <- S.printingOf s registry entering
  let base = S.landsFor mountain S.alice 6 (S.landsInPlay island 1)
      (globeId, g1) = S.addPermanent globe S.alice base
      (evolutionId, g2) = S.addHandCard evolution S.alice g1
      (creatureId, g3) = S.addHandCard creature S.alice g2
      ready =
        g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      hacked = case swap of
        Nothing -> ready
        Just (from, to) -> castAndResolve (evolveAt globeId from to) ready evolutionId
      after = castAndResolve S.identityAnswer hacked creatureId
  pure (after, newestNamed (S.printingName creature) after)

-- Aims every target set at one object and answers the creature-type swap, the
-- Pawl.ActivateSpec helper of the same name.
evolveAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
evolveAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  Prompt.ChooseCreatureTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- The four legs are two pairs differing in exactly one thing. Goblin Piker
-- (2/1 Creature -- Goblin Warrior) is the object the hacked word reaches and the
-- printed word does not; Hoarding Dragon (4/4 Creature -- Dragon) is the object
-- the printed word reaches and the hacked one does not. Distinct printed sizes,
-- so no reading of the rule produces the same number as another.
dragonstormGlobeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
dragonstormGlobeSpec s registry =
  Spec.describe s "Dragonstorm Globe (CR 612.1)" $ do
    Spec.it s "unhacked, that same Dragon does take the counter" $ do
      (after, entered) <- globeChain s registry Nothing "Hoarding Dragon"
      case entered of
        Nothing -> Spec.assertFailure s "the Hoarding Dragon did not reach the battlefield"
        Just dragonId -> do
          Spec.assertEqWith s "CR 614.1c the printed row applies to a Dragon" (Projection.powerOf dragonId after) (Just 5)
          Spec.assertEqWith s "through one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne dragonId after) 1

-- Hurr Jackal {R} Creature -- Jackal 1/1, whole text: "{T}: Target creature
-- can't be regenerated this turn." (oracle checked on Scryfall)
--
-- CR 701.19c's LASTING prohibition, a different carrier from Terror's: Terror
-- sets the Regenerability of the destruction it performs, where the Jackal knows
-- nothing about the destruction that eventually comes and so has to be read at
-- Event.resolveDestruction instead.
--
-- The destruction below is the CR 704.5g state-based action, deliberately. A
-- destruction any Effect.Destroy performed would carry its own Regenerability
-- and would kill the creature on today's tree too, proving nothing.
--
-- THE BOARD. alice's Jackal, and bob's TWO creatures -- a 2/1 Goblin Piker and a
-- 3/3 War Mammoth, each with a regeneration shield. bob owns both, so "it
-- reached a graveyard" is CR 400.3's owner's graveyard and cannot be confused
-- with a control-side move. TWO victims is what separates "prohibits the
-- creature named" from "prohibits every creature": only the Piker is targeted.
--
-- Returned twice: once with the Jackal's ability never activated, and once with
-- it resolved on the Piker. The pair differs in exactly that resolution.
jackalBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
jackalBoard jackal piker mammoth =
  let base = Setup.emptyGame S.bothPlayers
      (jackalId, g1) = S.addPermanent jackal S.alice base
      (victim, g2) = S.addPermanent piker S.bob g1
      (bystander, g3) = S.addPermanent mammoth S.bob g2
      control = (S.addRegenShield bystander (S.addRegenShield victim g3)) {GameState.priority = Just S.alice}
      activated = S.runPure (aimingAtObject victim) control (Activate.activateAbility S.alice jackalId (theAbility jackal) >> Stack.resolveTop)
   in (control, victim, bystander, activated)

-- CR 601.2c: aim the Jackal's ability at one particular creature. The offered
-- set is FILTERED rather than rebuilt, so the target the engine re-reads at
-- resolution (CR 608.2b) is the one it offered. Three creatures are on the
-- board, so the prompt is a real choice.
aimingAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- Exactly lethal to each (CR 704.5g): the Piker is a 2/1 and the Mammoth a 3/3,
-- so the two amounts differ and a fixture that damaged the wrong creature could
-- not be lethal to it by accident.
hurtBoth :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
hurtBoth victim bystander gs = S.markDamage bystander 3 (S.markDamage victim 1 gs)

-- The names of the cards in this player's graveyard. CR 400.7: a permanent that
-- is destroyed reaches the graveyard as a NEW object with a new id, so the id
-- the board was built with cannot be looked for there.
buriedNames :: PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
buriedNames pid gs =
  Maybe.mapMaybe
    ( \oid -> case fmap Object.source (Game.lookupObject oid gs) of
        Just (Source.OfCard printingId) -> fmap S.nameOf (Game.cardOfPrinting printingId gs)
        _ -> Nothing
    )
    (Game.zoneMembers Zone.Graveyard pid gs)

hurrJackalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hurrJackalSpec s registry = Spec.describe s "Hurr Jackal (CR 701.19c)" $ do
  let withBoard act = do
        jackal <- S.printingOf s registry "Hurr Jackal"
        piker <- S.printingOf s registry "Goblin Piker"
        mammoth <- S.printingOf s registry "War Mammoth"
        act (S.nameOf (Printing.card piker)) (jackalBoard jackal piker mammoth)
  Spec.it s "CR 701.19c / 704.5g the prohibited creature's shield does not save it from lethal damage"
    . withBoard
    $ \pikerName (_, victim, bystander, activated) -> do
      let settled = S.settleSba (hurtBoth victim bystander activated)
      -- The gameplay quantity first: what is in bob's graveyard. CR 400.7 mints
      -- a new object as the permanent moves, so the burial is asserted by NAME
      -- and the battlefield by id.
      Spec.assertEqWith s "the prohibited creature is in its owner's graveyard, and it alone" (buriedNames S.bob settled) [pikerName]
      Spec.assertBool s (not (Set.member victim (GameState.battlefield settled))) "and off the battlefield"
      -- THE CONTROL LEG, on this same board and this same CR 704.5g pass: the
      -- creature the ability never named regenerates. Without it the assertions
      -- above cannot tell a keyed prohibition from one that broke regeneration
      -- outright.
      Spec.assertBool s (Set.member bystander (GameState.battlefield settled)) "the creature the ability never named regenerates (CR 701.19a)"
      Spec.assertEqWith s "and CR 701.19a removed its damage" (S.damageOf bystander settled) (Just 0)
      -- CR 701.19c's sharp half: the prohibited creature's shield was never
      -- APPLIED, so it was never spent either. One of the two shields went, and
      -- it is the one that regenerated the Mammoth.
      Spec.assertEqWith s "the unapplied shield was not consumed" (length (GameState.replacements settled)) 1
  Spec.it s "CR 701.19a the same board without the ability: both shields hold"
    . withBoard
    $ \_ (control, victim, bystander, _) -> do
      let settled = S.settleSba (hurtBoth victim bystander control)
      Spec.assertBool s (Set.member victim (GameState.battlefield settled)) "the Piker regenerates when nothing forbade it"
      Spec.assertBool s (Set.member bystander (GameState.battlefield settled)) "and so does the Mammoth"
      Spec.assertEqWith s "nothing reached bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob settled)) 0

-- Chatterfang, Squirrel General (Oracle text checked against Scryfall
-- 2026-09-30): "If one or more tokens would be created under your control, those
-- tokens plus that many 1/1 green Squirrel creature tokens are created
-- instead." Queen Allenal's append (data/scenarios/shield-counter) sized by the
-- event (TokenPlus.ThatMany).
--
-- Dragon Fodder's two Goblins are the creation. Against Doubling Season the
-- two orders agree -- (2 + 2) * 2 and 2 * 2 + 4 -- where a one-token append
-- would answer two Squirrels and one.
chatterfangSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
chatterfangSpec s registry = Spec.describe s "Chatterfang, Squirrel General (CR 614.1a)" $ do
  let board mountain chatterfang = S.addPermanent chatterfang S.alice (S.landsInPlay mountain 2)
      squirrelName = CardName.MkCardName (Text.pack "Squirrel Token")
      goblinName = CardName.MkCardName (Text.pack "Goblin Token")
  Spec.it s "CR 614.1a two Goblins would be created, so two Goblins plus two Squirrels are" $ do
    mountain <- S.printingOf s registry "Mountain"
    chatterfang <- S.printingOf s registry "Chatterfang, Squirrel General"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let (_, g1) = board mountain chatterfang
        (g2, spellId) = S.handOne dragonFodder g1
        after = castAndResolve S.identityAnswer g2 spellId
    Spec.assertEqWith s "one Squirrel per token created" (S.countOnBattlefieldByName squirrelName S.alice after) 2
    Spec.assertEqWith s "and the two Goblins" (S.countOnBattlefieldByName goblinName S.alice after) 2
  Spec.it s "CR 616.1 racing Doubling Season: four Squirrels in either order" $ do
    mountain <- S.printingOf s registry "Mountain"
    chatterfang <- S.printingOf s registry "Chatterfang, Squirrel General"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let (chatterfangId, g1) = board mountain chatterfang
        (seasonId, g2) = S.addPermanent doublingSeason S.alice g1
        (g3, spellId) = S.handOne dragonFodder g2
        chatterfangFirst = castAndResolve (raceAnswer chatterfangId chatterfangId) g3 spellId
        seasonFirst = castAndResolve (raceAnswer seasonId chatterfangId) g3 spellId
    Spec.assertEqWith s "Chatterfang then Season: (2 Goblins + 2 Squirrels) * 2" (S.countOnBattlefieldByName squirrelName S.alice chatterfangFirst) 4
    Spec.assertEqWith s "Season then Chatterfang: 2 Goblins * 2, plus that many Squirrels" (S.countOnBattlefieldByName squirrelName S.alice seasonFirst) 4
    Spec.assertEqWith s "and four Goblins one way" (S.countOnBattlefieldByName goblinName S.alice chatterfangFirst) 4
    Spec.assertEqWith s "and four the other" (S.countOnBattlefieldByName goblinName S.alice seasonFirst) 4

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  chatterfangSpec s registry
  shieldCounterSpec s registry
  dragonstormGlobeSpec s registry
  hurrJackalSpec s registry
