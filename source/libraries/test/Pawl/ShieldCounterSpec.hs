{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Replacement over shield counters (CR 122.1c) and the remaining
-- printed replacements after them: Dragonstorm Globe, Tidewalker, a redirected
-- permanent spell, Hurr Jackal, Queen Allenal, Quina. Split out of
-- Pawl.ReplacementSpec, which keeps the machinery.
module Pawl.ShieldCounterSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import Pawl.EntryReplacementSpec (hackAt)
import Pawl.PreventionSpec (aimPlayer, answersFor, castAndResolve, countersOn, newestNamed, raceAnswer, settleDamage, theAbility, wasAskedToOrderDamage)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DestructionCause as DestructionCause
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
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
import qualified Pawl.Types.TapState as TapState
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
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
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
  -- CR 122.1c's SECOND sentence: "if damage would be dealt to this permanent,
  -- prevent that damage and remove a shield counter from it". Lightning Bolt's 3
  -- would be lethal to a 2/1, so an unprevented point of it is visible twice over --
  -- as marked damage and as a death.
  Spec.it s "CR 122.1c a Bolt at the bird is prevented and takes the counter" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (bird, entered) <- board ["Mountain"] Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid bolt entered)
        Spec.assertBool s (Set.member oid (GameState.battlefield once)) "it survived the Bolt"
        Spec.assertEqWith s "no damage was marked (CR 615.6)" (S.damageOf oid once) (Just 0)
        Spec.assertEqWith s "and the counter paid for it" (shields oid once) 0
  -- The discriminating twin, one difference from the case above: bob's praetor
  -- halves the entry placement to nothing, so the same bird faces the same Bolt on
  -- the same board with NO counter on it. Same mana, same seats, same spell.
  Spec.it s "CR 122.1c the same Bolt kills the same bird with no counter on it" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (bird, entered) <- board ["Mountain"] (Just ("Vorinclex, Monstrous Raider", S.bob))
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid bolt entered)
        Spec.assertEqWith s "setup: the halving left no shield" (shields oid entered) 0
        Spec.assertBool s (not (Set.member oid (GameState.battlefield once))) "so the Bolt killed it"
  -- CR 122.1c's FIRST sentence: "if this permanent would be destroyed as the result
  -- of an effect, instead remove a shield counter from it". Doom Blade destroys
  -- without dealing any damage, so nothing here can be mistaken for the prevention
  -- half -- and the bird is left untapped, which is how this also shows the removal
  -- is not a regeneration ("removing a shield counter in this way isn't the same as
  -- regenerating a creature"; CR 701.19a taps).
  Spec.it s "CR 122.1c Doom Blade is replaced by the counter, and the next one kills" $ do
    doomBlade <- S.printingOf s registry "Doom Blade"
    (bird, entered) <- board (replicate 4 "Swamp") Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid doomBlade entered)
            twice = S.settleSba (castAt oid doomBlade once)
        Spec.assertBool s (Set.member oid (GameState.battlefield once)) "it survived the first Doom Blade"
        Spec.assertEqWith s "the counter paid for it" (shields oid once) 0
        Spec.assertEqWith s "and it was not regenerated" (fmap Object.tapped (Game.lookupObject oid once)) (Just TapState.Untapped)
        Spec.assertBool s (not (Set.member oid (GameState.battlefield twice))) "and the second Doom Blade killed it"
  -- Two counters, two destructions, and the third kills: the count is how many
  -- events the pair may still replace, and one counter comes off per application
  -- however many are there ("if a permanent that would be dealt damage has more than
  -- one shield counter on it ... only one shield counter is removed").
  Spec.it s "CR 122.1c Doubling Season's two counters replace two destructions" $ do
    doomBlade <- S.printingOf s registry "Doom Blade"
    (bird, entered) <- board (replicate 6 "Swamp") (Just ("Doubling Season", S.alice))
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid doomBlade entered)
            twice = S.settleSba (castAt oid doomBlade once)
            thrice = S.settleSba (castAt oid doomBlade twice)
        Spec.assertEqWith s "one counter off, not both" (shields oid once) 1
        Spec.assertBool s (Set.member oid (GameState.battlefield twice)) "the second destruction is replaced too"
        Spec.assertEqWith s "and now there are none" (shields oid twice) 0
        Spec.assertBool s (not (Set.member oid (GameState.battlefield thrice))) "so the third kills it"
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
  -- CR 122.1c is a RULE rather than an ability the permanent has: "if a creature
  -- with a shield counter loses its abilities, the shield counter will still protect
  -- it as normal". So the pair survives layer 6, which is what minting it from
  -- Object.counters rather than from the projection's ability list buys. Humility
  -- arrives AFTER the bird, so what is under test is the shield outliving the
  -- abilities and not CR 614.12's question about an entry replacement under layer 6.
  Spec.it s "CR 613.1f a Humility'd bird keeps its shield" $ do
    doomBlade <- S.printingOf s registry "Doom Blade"
    humility <- S.printingOf s registry "Humility"
    (bird, entered) <- board (replicate 2 "Swamp") Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let humbled = S.withHumility humility entered
            once = S.settleSba (castAt oid doomBlade humbled)
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying oid entered) "setup: the bird has flying"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying oid humbled)) "setup: Humility took it away"
        Spec.assertEqWith s "setup: the counter is still there" (shields oid humbled) 1
        Spec.assertBool s (Set.member oid (GameState.battlefield once)) "and the shield still replaced the destruction"
        Spec.assertEqWith s "spending the counter" (shields oid once) 0
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
  -- CR 122.1c's "to THIS permanent": the pair protects the permanent its counters
  -- are on and no other recipient. Two Bolts off one board, one at bob and one at
  -- bob's Piker, so both shapes a wrongly scoped shield would reach are covered -- a
  -- damage event naming a PLAYER, whose recipient is no object at all, and one
  -- naming another creature.
  Spec.it s "CR 122.1c the shield covers its own permanent and no other recipient" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    (bird, entered) <- board ["Mountain", "Mountain"] Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let (pikerId, staged) = S.addPermanent pikerPrinting S.bob entered
            castAtBob gs =
              let (held, g1) = S.addHandCard bolt S.alice gs
               in S.runPure (aimPlayer S.bob) g1 (S.cast S.alice held >> Stack.resolveTop)
            hitBob = S.settleSba (castAtBob staged)
            hitPiker = S.settleSba (castAt pikerId bolt hitBob)
        Spec.assertEqWith s "bob took the Bolt" (S.lifeOf S.bob hitBob) (fmap (subtract 3) (S.lifeOf S.bob staged))
        Spec.assertEqWith s "and the bird's counter is untouched" (shields oid hitBob) 1
        Spec.assertBool s (not (Set.member pikerId (GameState.battlefield hitPiker))) "the second Bolt killed bob's Piker"
        Spec.assertEqWith s "and the counter is still untouched" (shields oid hitPiker) 1
        Spec.assertBool s (Set.member oid (GameState.battlefield hitPiker)) "setup: the bird sat there through both"
  -- CR 615.12 with CR 122.1c: the pair's prevention half says "prevent", so CR 615.1a
  -- makes it a prevention effect and unpreventable damage is still MET by it and
  -- still prevented none of -- the Bolt lands in full and kills the 2/1. The
  -- discriminating twin is the first Bolt case above: same bird, same Bolt, and the
  -- only difference is Spider-Punk on the board.
  --
  -- The rule's MIDDLE clause takes the bird's counter off with it ("if a permanent
  -- with a shield counter is dealt unpreventable damage, that damage will be dealt
  -- and a shield counter will still be removed"), so the bird reaches CR 704.5g
  -- with nothing left to replace anything. The counter is unreadable after the
  -- fact here -- CR 122.2 -- which is why the cases below use a body that
  -- survives; the GAMEPLAY route to CR 122.1c's "as the result of an effect" is
  -- the last of them, where two Bolts leave a still-shielded permanent facing CR
  -- 704.5g.
  Spec.it s "CR 615.12 an unpreventable Bolt kills the shielded bird" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (bird, entered) <- board ["Mountain"] (Just ("Spider-Punk", S.bob))
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid bolt entered)
        Spec.assertEqWith s "setup: the bird still entered with its counter" (shields oid entered) 1
        Spec.assertBool s (not (Set.member oid (GameState.battlefield once))) "and the Bolt killed it through the shield"
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
-- SPELL that holds such a row is tidewalkerSpec's shape below -- so this is one
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
dragonstormGlobeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dragonstormGlobeSpec s registry =
  Spec.describe s "Dragonstorm Globe (CR 612.1)" $ do
    Spec.it s "CR 612.1 a text change reaches a replacement effect: the hacked Globe swells an entering Goblin" $ do
      (after, entered) <- globeChain s registry (Just (Subtype.Dragon, Subtype.Goblin)) "Goblin Piker"
      case entered of
        Nothing -> Spec.assertFailure s "the Goblin Piker did not reach the battlefield"
        Just pikerId -> do
          Spec.assertEqWith s "CR 613.1c the layer-3 swap reaches the row the entry loop reads" (Projection.powerOf pikerId after) (Just 3)
          Spec.assertEqWith s "and the toughness with it" (Projection.toughnessOf pikerId after) (Just 2)
          Spec.assertEqWith s "through the one +1/+1 counter CR 614.1c put on it" (countersOn CounterKind.PlusOnePlusOne pikerId after) 1
    -- The control leg: the same board, the same Piker, no Artificial Evolution.
    Spec.it s "unhacked, the printed Dragon leaves that same Goblin alone" $ do
      (after, entered) <- globeChain s registry Nothing "Goblin Piker"
      case entered of
        Nothing -> Spec.assertFailure s "the Goblin Piker did not reach the battlefield"
        Just pikerId -> do
          Spec.assertEqWith s "printed 2/1, so the Globe's row did not apply" (Projection.powerOf pikerId after) (Just 2)
          Spec.assertEqWith s "printed 2/1, so the Globe's row did not apply" (Projection.toughnessOf pikerId after) (Just 1)
          Spec.assertEqWith s "no counter" (countersOn CounterKind.PlusOnePlusOne pikerId after) 0
    -- The converse: CR 612.1 REPLACES the word rather than adding one, so the
    -- Dragon the printed row named is no longer named.
    Spec.it s "CR 612.1 the printed word is gone: after the hack an entering Dragon gets nothing" $ do
      (after, entered) <- globeChain s registry (Just (Subtype.Dragon, Subtype.Goblin)) "Hoarding Dragon"
      case entered of
        Nothing -> Spec.assertFailure s "the Hoarding Dragon did not reach the battlefield"
        Just dragonId -> do
          Spec.assertEqWith s "printed 4/4" (Projection.powerOf dragonId after) (Just 4)
          Spec.assertEqWith s "no counter" (countersOn CounterKind.PlusOnePlusOne dragonId after) 0
    Spec.it s "unhacked, that same Dragon does take the counter" $ do
      (after, entered) <- globeChain s registry Nothing "Hoarding Dragon"
      case entered of
        Nothing -> Spec.assertFailure s "the Hoarding Dragon did not reach the battlefield"
        Just dragonId -> do
          Spec.assertEqWith s "CR 614.1c the printed row applies to a Dragon" (Projection.powerOf dragonId after) (Just 5)
          Spec.assertEqWith s "through one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne dragonId after) 1

-- Tidewalker {2}{U} Creature -- Elemental */*, whole text: "This creature enters
-- with a time counter on it for each Island you control. / Vanishing (At the
-- beginning of your upkeep, remove a time counter from this creature. When the
-- last is removed, sacrifice it.) / Tidewalker's power and toughness are each
-- equal to the number of time counters on it." (oracle checked on Scryfall
-- 2026-08-26)
--
-- The shape the Globe and the Beacon above route AROUND: the row is the ENTERING
-- permanent's own (Filter.IsSource), and the text change is on the SPELL it was a
-- moment earlier. CR 400.7a keeps that change applying to the permanent the spell
-- becomes, and CR 614.12 says the entry row is decided against "continuous
-- effects that already exist and would apply to the permanent" -- so the swapped
-- word is the one the CR 616.1 loop must read. The re-key runs inside
-- Event.changeZoneAttaching, before the entry loop, for exactly this.
--
-- THE TWO LAND COUNTS ARE UNEQUAL, four Islands and six Swamps, and both are
-- nonzero: a row read before the re-key answers four, a row read after answers
-- six, and neither is the other. Nonzero on both readings keeps CR 702.63b's
-- "when the last time counter is removed" and a CR 704.5f death out of the leg
-- -- the permanent arrives either way, and what the pair tells apart is which
-- word its own row named.
--
-- Four Islands rather than the one the mana needs: {2}{U} spends at most three
-- lands, so an Island is left untapped for the Hack however the payment picks,
-- and a leg cannot pass because the Hack was uncastable.
tidewalkerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tidewalkerSpec s registry = Spec.describe s "Tidewalker (CR 400.7a / 614.12)" $ do
  Spec.it s "CR 614.12 the hacked spell's own entry row counts the swapped land type" $ do
    (before, after, entered) <- tidewalkerChain s registry True
    case entered of
      Nothing -> Spec.assertFailure s "the Tidewalker did not reach the battlefield"
      Just tideId -> do
        Spec.assertEqWith s "six time counters, one for each Swamp, not four for the printed Islands" (countersOn CounterKind.Time tideId after) 6
        Spec.assertEqWith s "and CR 613.4a reads its power and toughness off that same tally" (S.powerToughnessOf tideId after) (Just (6, 6))
        Spec.assertEqWith s "the Hack was cast and resolved, leaving only the Tidewalker on the stack" (length (GameState.stack before)) 1
  -- The control: the same board and the same spell, no Magical Hack. It pins the
  -- printed reading, so the pair differs in exactly the text change.
  Spec.it s "unhacked, the printed Island is what its row counts" $ do
    (before, after, entered) <- tidewalkerChain s registry False
    case entered of
      Nothing -> Spec.assertFailure s "the Tidewalker did not reach the battlefield"
      Just tideId -> do
        Spec.assertEqWith s "four time counters, one for each Island" (countersOn CounterKind.Time tideId after) 4
        Spec.assertEqWith s "a 4/4" (S.powerToughnessOf tideId after) (Just (4, 4))
        Spec.assertEqWith s "the Tidewalker is the only thing on the stack" (length (GameState.stack before)) 1

-- alice controls four Islands and six Swamps and holds the Tidewalker and Magical
-- Hack ({U}). The Tidewalker is CAST and left ON THE STACK -- that is the whole
-- point, since hacking it after it resolved is the Globe's shape and reads the
-- same either way -- then the Hack is cast at the SPELL and resolved, and only
-- then does the Tidewalker resolve. Returns the state with the Tidewalker alone
-- on the stack, the state after it resolves, and the permanent it became.
tidewalkerChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (GameState.GameState, GameState.GameState, Maybe ObjectId.ObjectId)
tidewalkerChain s registry hack = do
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  tidewalker <- S.printingOf s registry "Tidewalker"
  magicalHack <- S.printingOf s registry "Magical Hack"
  let base = S.landsFor swamp S.alice 6 (S.landsInPlay island 4)
      (tideId, g1) = S.addHandCard tidewalker S.alice base
      (hackId, g2) = S.addHandCard magicalHack S.alice g1
      ready =
        g2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      onStack = S.runPure S.identityAnswer ready (S.cast S.alice tideId)
      -- The Hack's target is FILTERED out of the offered set rather than rebuilt,
      -- hackAt's reason: a hand-built recipient would be dropped at CR 608.2b's
      -- re-read with no error.
      before = case (hack, GameState.stack onStack) of
        (True, spellId : _) -> castAndResolve (hackAt spellId Subtype.Island Subtype.Swamp) onStack hackId
        _ -> onStack
      after = S.runPure S.identityAnswer before Stack.resolveTop
  pure (before, after, newestNamed (S.printingName tidewalker) after)

-- The OTHER side of the rule tidewalkerSpec above proves: CR 400.7a carries an
-- effect over to "the permanent that spell becomes", so a permanent spell a CR
-- 614.6 redirect sent somewhere other than the battlefield becomes no permanent
-- and carries nothing; see #2399. CR 608.3e is the rulebook stating the same shape
-- from the other end.
--
-- THE PRODUCER IS SYNTHETIC, and the search that says so: Scryfall's oracle text
-- for a replacement of a cast permanent spell's battlefield entry
-- (`o:"would enter" o:instead (o:exile or o:graveyard or o:hand or o:library or
-- o:"the stack")`, 2026-08-29; the older template `o:"would enter the
-- battlefield"` returns one card, and `o:"would be put onto the battlefield"`
-- and `o:"permanent spell would"` none) returns thirteen cards and not one
-- of them can be this board. Containment Priest, Hallowed Moonlight, Mistcaller
-- and Primeval Spawn all exclude a creature that WAS cast; Don't Blink reaches a
-- cast permanent spell but shuffles it into a library, where no characteristic
-- can be read; the seven Alliances-style lands (Lotus Vale is the shape) are
-- PLAYED rather than cast, so CR 400.7a never speaks to them and CR 400.7i's
-- carrier is a different one (gap #2398). Mox Diamond is the one real card that
-- sends a resolving permanent spell to a graveyard -- and its destination hangs
-- on an optional cost that Pawl.Types.ZoneChangeR, whose destination is a single
-- zone, cannot express. It could not discriminate here in any case: nothing in
-- the pool puts a continuous effect on a colorless artifact spell that a
-- graveyard can show.
--
-- Synthetic Entry Interdiction is "If a green spell would enter the battlefield,
-- put it into its owner's graveyard instead" -- the CR 614.1a replacement of
-- exactly the CR 608.3a zone change. `IsInZone Stack` is the "spell" half, and
-- it is load-bearing twice: the Queen's Bay Paladin below is cast on the same
-- board, and the card the Paladin returns FROM THE GRAVEYARD must not be
-- redirected back on its way in.
--
-- Giant Spider {3}{G} Creature -- Spider 2/4, whole text: "Reach". Artificial
-- Evolution {U} Instant: "Change the text of target spell or permanent by
-- replacing all instances of one creature type with another. The new creature
-- type can't be Wall." Queen's Bay Paladin {3}{B}{B} Creature -- Vampire Knight
-- 5/4: "Whenever this creature enters or attacks, return up to one target
-- Vampire card from your graveyard to the battlefield with a finality counter on
-- it. You lose life equal to its mana value." (oracle checked on Scryfall
-- 2026-08-29)
--
-- THE DISCRIMINATOR is the Paladin's target slot, a filter read of the
-- redirected card in the zone it was redirected TO. Spider -> Vampire is made on
-- the SPELL; if the redirect carried it over, the card in alice's graveyard is a
-- Vampire card the Paladin can return, and alice loses the Spider's four life.
-- If it does not, the graveyard holds a Spider and the Paladin's "up to one
-- target" finds nothing.
redirectedPermanentSpellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
redirectedPermanentSpellSpec s registry = Spec.describe s "a redirected permanent spell carries nothing over (CR 400.7a / 614.6)" $ do
  Spec.it s "CR 400.7a a spell that became no permanent carries no text change into its graveyard" $ do
    (spiderName, after) <- redirectedSpellChain s registry True
    Spec.assertEqWith s "the Paladin found no Vampire card, so nothing came back to the battlefield" (S.countOnBattlefieldByName spiderName S.alice after) 0
    Spec.assertEqWith s "and alice lost no life, which the returned card's mana value would have cost her" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "the redirect put the card in her graveyard, where it still sits" (length (filter ((==) (Just spiderName) . fmap Face.name . flip Game.faceOf after) (Game.zoneMembers Zone.Graveyard S.alice after))) 1
  -- The control, and the positive half of the same rule: the same board without
  -- the interdiction, so the spell DOES become a permanent and the text change
  -- rides across with it. The pair differs in exactly whether the redirect is on
  -- the battlefield.
  Spec.it s "uninterdicted, the same spell becomes a permanent and the text change rides across" $ do
    (spiderName, after) <- redirectedSpellChain s registry False
    Spec.assertEqWith s "the Spider resolved onto the battlefield" (S.countOnBattlefieldByName spiderName S.alice after) 1
    case newestNamed spiderName after of
      Nothing -> Spec.assertFailure s "the Giant Spider did not reach the battlefield"
      Just spiderId -> do
        Spec.assertEqWith s "CR 400.7a the permanent the spell became is a Vampire" (Set.member Subtype.Vampire (Projection.subtypesOf spiderId after)) True
        Spec.assertEqWith s "CR 612.1 replaced the word, so it is no longer a Spider" (Set.member Subtype.Spider (Projection.subtypesOf spiderId after)) False

-- alice holds a Giant Spider, an Artificial Evolution and a Queen's Bay Paladin,
-- and (when `interdict`) controls a Synthetic Entry Interdiction. She casts the
-- Spider, leaves it ON THE STACK -- that is the whole point, since evolving the
-- permanent afterwards is a different rule -- casts the Evolution at the SPELL
-- swapping Spider for Vampire, resolves the Spider, then casts the Paladin and
-- lets its enters trigger resolve. Returns the Spider's name and the final state.
--
-- FOUR Islands for a {U} spell, tidewalkerChain's reason at this board's size:
-- the Spider is the only spell cast before the Evolution and spends at most four
-- lands, so an Island survives however the payment picks. Seven Swamps then
-- cover the Paladin's {B}{B} whatever the two casts before it ate.
redirectedSpellChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (CardName.CardName, GameState.GameState)
redirectedSpellChain s registry interdict = do
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  spider <- S.printingOf s registry "Giant Spider"
  evolution <- S.printingOf s registry "Artificial Evolution"
  paladin <- S.printingOf s registry "Queen's Bay Paladin"
  interdiction <- S.printingOf s registry "Synthetic Entry Interdiction"
  let lands = S.landsFor swamp S.alice 7 (S.landsFor island S.alice 4 (S.landsInPlay forest 4))
      warded = if interdict then snd (S.addPermanent interdiction S.alice lands) else lands
      (spiderId, g1) = S.addHandCard spider S.alice warded
      (evolutionId, g2) = S.addHandCard evolution S.alice g1
      (paladinId, g3) = S.addHandCard paladin S.alice g2
      ready =
        g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      onStack = S.runPure S.identityAnswer ready (S.cast S.alice spiderId)
      -- The Evolution's target is FILTERED out of the offered set rather than
      -- rebuilt, hackAt's reason: a hand-built recipient would be dropped at CR
      -- 608.2b's re-read with no error.
      evolved = case GameState.stack onStack of
        spellId : _ -> S.runPure (evolvingAt spellId) onStack (S.cast S.alice evolutionId >> Stack.resolveTop)
        [] -> onStack
      resolved = S.runPure S.identityAnswer evolved Stack.resolveTop
      after = S.runPure takingEveryTarget resolved (S.cast S.alice paladinId >> Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop)
  pure (S.printingName spider, after)

-- Aims the Evolution at `oid` by narrowing the offered set to it, and answers CR
-- 612.1's word swap with Spider for Vampire.
evolvingAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
evolvingAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  Prompt.ChooseCreatureTypeSwap {} -> (Subtype.Spider, Subtype.Vampire)
  _ -> S.identityAnswer p

-- Takes every candidate the engine offers. S.identityAnswer declines, which
-- would answer the Paladin's "up to one target" with none on either board and
-- prove nothing; this answerer reads the engine's own candidate list back, so
-- the assertion is about what was LEGAL to target.
takingEveryTarget :: Prompt.Prompt r -> r
takingEveryTarget p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap snd sets
  _ -> S.identityAnswer p

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
  Spec.it s "CR 514.2 the prohibition lasts exactly the turn, and the same shield saves the same creature next turn"
    . withBoard
    $ \_ (_, victim, _, activated) -> do
      -- CR 514.2's own sweep, then the handoff into bob's turn. The shield is
      -- RE-ARMED after both: Support.addRegenShield arms Expiry.AtCleanup, so
      -- the one this turn put up is gone either way and a case that did not
      -- re-arm would pass for the wrong reason.
      let bobsTurn = S.runPure S.identityAnswer (Expiry.dropAtCleanup activated) Engine.handoffTurn
          settled = S.settleSba (S.markDamage victim 1 (S.addRegenShield victim bobsTurn))
      Spec.assertBool s (Set.member victim (GameState.battlefield settled)) "the same creature regenerates once the turn it was named in is over"
      Spec.assertEqWith s "CR 701.19a removed its damage" (S.damageOf victim settled) (Just 0)
      Spec.assertEqWith s "nothing reached bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob settled)) 0
      Spec.assertEqWith s "and the prohibition really is off the board" (GameState.unregeneratables bobsTurn) []
      Spec.assertEqWith s "bob's turn really did begin" (GameState.activePlayer bobsTurn) S.bob

-- Queen Allenal of Ruadach, {G}{W}{W} Legendary Creature -- Elf Noble */*: "If
-- one or more creature tokens would be created under your control, those tokens
-- plus a 1/1 white Soldier creature token are created instead." Two things one
-- card proves: a token replacement scoped by WHAT the token is
-- (Pawl.Types.TokenPattern.whatToken), and one that APPENDS a differently-shaped
-- token to the event rather than resizing it (Pawl.Types.TokenR.plus).
--
-- Dragon Fodder ({1}{R}, two 1/1 Goblins) is the creature-token maker, and
-- Eliminate the Impossible ({1}{U}, investigate) the negative: a Clue is a
-- token and not a creature token, so the same Queen on the same lands appends
-- nothing. Both boards carry both colours of mana so neither cast fails for
-- want of it.
queenAllenalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
queenAllenalSpec s registry = Spec.describe s "Queen Allenal of Ruadach (CR 614.1a)" $ do
  let board mountain island queen =
        let lands = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) (S.landsInPlay mountain 2) [1 .. (2 :: Int)]
         in S.addPermanent queen S.alice lands
      soldierName = CardName.MkCardName (Text.pack "Soldier Token")
      goblinName = CardName.MkCardName (Text.pack "Goblin Token")
      clueName = CardName.MkCardName (Text.pack "Clue Token")
      namedTokens name gs = filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just name) (S.tokensOf gs)
  Spec.it s "CR 614.1a two Goblins would be created, so two Goblins plus a Soldier are" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    queen <- S.printingOf s registry "Queen Allenal of Ruadach"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let (queenId, g1) = board mountain island queen
        (g2, spellId) = S.handOne dragonFodder g1
        after = castAndResolve S.identityAnswer g2 spellId
    Spec.assertEqWith s "one Soldier the spell never named" (S.countOnBattlefieldByName soldierName S.alice after) 1
    Spec.assertEqWith s "and the two Goblins it did" (S.countOnBattlefieldByName goblinName S.alice after) 2
    -- The appended token is the card the ROW printed, not a third Goblin.
    case namedTokens soldierName after of
      [soldier] -> do
        Spec.assertEqWith s "a 1/1" (S.powerToughnessOf soldier after) (Just (1, 1))
        Spec.assertEqWith s "and white" (Projection.colorsOf soldier after) (Set.singleton Color.White)
        Spec.assertEqWith s "under alice's control (CR 111.2)" (Projection.controllerOf soldier after) (Just S.alice)
      other -> Spec.assertFailure s ("expected exactly one Soldier, got " <> show (length other))
    -- The Queen's own CR 604.3 box, read live: herself and three tokens.
    Spec.assertEqWith s "the Queen counts the creatures she controls" (S.powerToughnessOf queenId after) (Just (4, 4))
  -- The negative, one thing different: the token is a Clue, and "creature
  -- tokens" does not say Clue.
  Spec.it s "CR 614.1a a Clue is not a creature token, so nothing is appended" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    queen <- S.printingOf s registry "Queen Allenal of Ruadach"
    eliminate <- S.printingOf s registry "Eliminate the Impossible"
    let (_, g1) = board mountain island queen
        (g2, spellId) = S.handOne eliminate g1
        after = castAndResolve S.identityAnswer g2 spellId
    Spec.assertEqWith s "no Soldier" (S.countOnBattlefieldByName soldierName S.alice after) 0
    Spec.assertEqWith s "the Clue was created, so the spell did resolve" (S.countOnBattlefieldByName clueName S.alice after) 1
  -- CR 616.1: the append is INSIDE the one creation event, which is what the
  -- order against Doubling Season observes. Queen first: two Goblins plus a
  -- Soldier, then doubled -- two Soldiers. Season first: four Goblins, then
  -- the Soldier joins them -- one Soldier. A rider that created the Soldier as a
  -- second event would answer one Soldier both ways.
  Spec.it s "CR 616.1 racing Doubling Season: the Soldier is doubled only when the Queen applies first" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    queen <- S.printingOf s registry "Queen Allenal of Ruadach"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let (queenId, g1) = board mountain island queen
        (seasonId, g2) = S.addPermanent doublingSeason S.alice g1
        (g3, spellId) = S.handOne dragonFodder g2
        queenFirst = castAndResolve (raceAnswer queenId queenId) g3 spellId
        seasonFirst = castAndResolve (raceAnswer seasonId queenId) g3 spellId
    Spec.assertEqWith s "Queen then Season: (2 Goblins + 1 Soldier) * 2 -- two Soldiers" (S.countOnBattlefieldByName soldierName S.alice queenFirst) 2
    Spec.assertEqWith s "and four Goblins" (S.countOnBattlefieldByName goblinName S.alice queenFirst) 4
    Spec.assertEqWith s "Season then Queen: 2 Goblins * 2, plus one Soldier" (S.countOnBattlefieldByName soldierName S.alice seasonFirst) 1
    Spec.assertEqWith s "and four Goblins" (S.countOnBattlefieldByName goblinName S.alice seasonFirst) 4
  -- The card's ruling: "Anything else specified in the effect creating the
  -- tokens (such as tapped, attacking, ...) applies to both the original tokens
  -- and the Soldier." Hero of Bladehold's "create two 1/1 white Soldier creature
  -- tokens that are tapped and attacking" is the effect; only the Hero is
  -- declared, so every attacker but the Hero arrived through the event.
  Spec.it s "the creating effect's riders reach the appended token: tapped and attacking" $ do
    hero <- S.printingOf s registry "Hero of Bladehold"
    queen <- S.printingOf s registry "Queen Allenal of Ruadach"
    case S.combatBoardOf [hero, queen] [] of
      (gs, [heroId, _], _) -> do
        let declareOnlyHero :: Prompt.Prompt r -> r
            declareOnlyHero p = case p of
              Prompt.DeclareAttackers _ _ ids -> filter (== heroId) ids
              _ -> S.identityAnswer p
            declared = S.runPure declareOnlyHero gs Engine.runStep
            soldiers = namedTokens soldierName declared
            attacking oid = Map.member oid (Combat.Type.attackers (GameState.combat declared))
            tapped oid = fmap Object.tapped (Game.lookupObject oid declared) == Just TapState.Tapped
        Spec.assertEqWith s "the Hero's two Soldiers and the Queen's one" (length soldiers) 3
        Spec.assertEqWith s "every one of them attacking (CR 508.4)" (length (filter attacking soldiers)) 3
        Spec.assertEqWith s "and every one of them tapped (CR 110.5b)" (length (filter tapped soldiers)) 3
      _ -> Spec.assertFailure s "fixture should give alice a Hero and a Queen"

-- Quina, Qu Gourmet, {2}{G} Legendary Creature -- Qu 2/3: "If one or more
-- tokens would be created under your control, those tokens plus a 1/1 green
-- Frog creature token are created instead." The append with NO kind: the same
-- Clue that slips past the Queen above brings a Frog here.
quinaSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
quinaSpec s registry = Spec.describe s "Quina, Qu Gourmet (CR 614.1a)" $ do
  Spec.it s "CR 614.1a any token at all: a Clue plus a Frog" $ do
    island <- S.printingOf s registry "Island"
    quina <- S.printingOf s registry "Quina, Qu Gourmet"
    eliminate <- S.printingOf s registry "Eliminate the Impossible"
    let (_, g1) = S.addPermanent quina S.alice (S.landsInPlay island 2)
        (g2, spellId) = S.handOne eliminate g1
        after = castAndResolve S.identityAnswer g2 spellId
    Spec.assertEqWith s "one Frog" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Frog Token")) S.alice after) 1
    Spec.assertEqWith s "beside the Clue" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Clue Token")) S.alice after) 1

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  queenAllenalSpec s registry
  quinaSpec s registry
  shieldCounterSpec s registry
  dragonstormGlobeSpec s registry
  tidewalkerSpec s registry
  redirectedPermanentSpellSpec s registry
  hurrJackalSpec s registry
