{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over triggers on life changing (CR 119, CR 603.7, CR
-- 702.15e): gaining and losing life, the amount bound, lifelink's gain events,
-- and the delayed triggers that read them. Split out of Pawl.EventTriggerSpec,
-- which keeps the machinery.
module Pawl.LifeTriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event.Binding as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- CR 119.9: "Some triggered abilities are written, 'Whenever [a player] gains
-- life, . . . .' Such abilities are treated as though they are written, 'Whenever
-- a source causes [a player] to gain life, . . . .' If a player gains 0 life, no
-- life gain event has occurred, and these abilities won't trigger."
--
-- Ajani's Pridemate, {1}{W} Creature -- Cat Soldier 2/2, "Whenever you gain life,
-- put a +1/+1 counter on this creature", the card that proves it. Its payload
-- names only its own source, so every case here isolates the CONDITION.
--
-- What makes the group a proof rather than a demonstration is that each positive
-- has a control differing in ONE thing:
--
--   * the same Soul Warden, the same entering creature, the same 1 life gained --
--     and the Warden under the OTHER player. Only the GAINER differs, and the
--     Pridemate is silent (CR 109.5 / 603.3a's "you").
--   * one combat damage event, two life totals moving in opposite directions, and
--     a Pridemate on each side. Only the DIRECTION differs, and only the gainer's
--     fires: CR 120.3f's lifelink gain is a life gain event and CR 119.2's damage
--     loss is not (GameEvent.LifeLost is a different constructor entirely).
--
-- The zero case is CR 119.9's own last sentence, asserted on the CR 608.2i record
-- rather than through a counter: a 0-damage lifelink event is a real damage event
-- that gains 0 life, so the log must hold no life gain for it to match.
lifeGainTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lifeGainTriggerSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- A Maybe rather than a defaulted 0, so a Pridemate that is no longer
      -- there reads as Nothing and cannot be mistaken for one that took no
      -- counter.
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- alice always holds the Pridemate and casts the creature; `wardenOwner`
      -- decides who gains the life the entering creature causes. That is the only
      -- difference between the two cases below.
      wardenBoard plains pridemate soulWarden wardenOwner =
        let (_, b0) = S.addCreature soulWarden wardenOwner (S.landsInPlay plains 1)
            (mateId, b1) = S.addCreature pridemate S.alice b0
            (gs, spellId) = S.handOne soulWarden b1
            cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
         in (mateId, resolveAll cast)
      -- Only `attacker` attacks, and nobody blocks, so the life totals move by
      -- exactly the one damage event under test. Declining the block is what puts
      -- the damage on the PLAYER: bob's own Pridemate would otherwise block, and
      -- CR 120.3e's marked damage would leave his life total alone -- costing the
      -- lifelink case its "and bob lost two" control.
      attacksWith :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      attacksWith attacker p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
   in Spec.describe s "PlayerGainsLife" $ do
        -- The gameplay-level proof, cast to resolution. alice's Soul Warden sees
        -- the second Warden enter (CR 603.6a), gains her 1 life on resolution (CR
        -- 119.3), and THAT is the event the Pridemate matches -- a second CR 117.5
        -- boundary later, off GameEvent.LifeGained.
        --
        -- Exactly one counter, not two: the newcomer's own "another" declines its
        -- own entry, so exactly one life gain event happened.
        Spec.it s "CR 119.9 whole cards: alice gains 1 life from Soul Warden and her Pridemate grows" $ do
          plains <- S.printingOf s registry "Plains"
          pridemate <- S.printingOf s registry "Ajani's Pridemate"
          soulWarden <- S.printingOf s registry "Soul Warden"
          let (mateId, settled) = wardenBoard plains pridemate soulWarden S.alice
          Spec.assertEqWith s "alice gained exactly 1" (S.lifeOf S.alice settled) (Just 21)
          Spec.assertEqWith s "the Pridemate took exactly one +1/+1 counter" (countersOn mateId settled) (Just 1)
        -- The control twin, differing in ONE thing: bob controls the Soul Warden,
        -- so bob is the one who gains. The same creature enters, the same 1 life
        -- is gained, the same log entry is written -- and CR 109.5's "you" is
        -- alice, so her Pridemate stays silent.
        --
        -- bob's gain is asserted too, or the case would pass for the wrong reason:
        -- an engine that recorded no event at all would also show no counter.
        Spec.it s "CR 109.5/603.3a the control: BOB gains the life, and alice's Pridemate stays silent" $ do
          plains <- S.printingOf s registry "Plains"
          pridemate <- S.printingOf s registry "Ajani's Pridemate"
          soulWarden <- S.printingOf s registry "Soul Warden"
          let (mateId, settled) = wardenBoard plains pridemate soulWarden S.bob
          Spec.assertEqWith s "bob really gained the life" (S.lifeOf S.bob settled) (Just 21)
          Spec.assertEqWith s "alice gained nothing" (S.lifeOf S.alice settled) (Just 20)
          Spec.assertEqWith s "so the Pridemate took no counter" (countersOn mateId settled) (Just 0)
        -- CR 120.3f: "damage dealt by a source with lifelink causes that source's
        -- controller to gain that much life, in addition to the damage's other
        -- results". The second producer, and the one CR 119.9's rewriting is aimed
        -- at -- no effect said "gain life"; a keyword did.
        --
        -- ONE board carries the control. bob has a Pridemate too, and the single
        -- combat damage event moves both life totals: alice's UP by 2 (CR 120.3f)
        -- and bob's DOWN by 2 (CR 119.2 / 120.3a). Only alice's fires, so the
        -- trigger is keyed on gaining life rather than on a life total moving.
        Spec.it s "CR 120.3f lifelink gains life, so the attacker's Pridemate grows and the defender's does not" $ do
          pridemate <- S.printingOf s registry "Ajani's Pridemate"
          childOfNight <- S.printingOf s registry "Child of Night"
          let (gs0, mine, _) = S.combatBoardOf [childOfNight] []
              (aliceMate, gs1) = S.addCreature pridemate S.alice gs0
              (bobMate, gs2) = S.addCreature pridemate S.bob gs1
          case mine of
            [] -> Spec.assertFailure s "fixture should have given alice a Child of Night"
            vampire : _ -> do
              let settled = resolveAll (S.fightWith (attacksWith vampire) gs2)
              Spec.assertEqWith s "alice gained two" (S.lifeOf S.alice settled) (Just 22)
              Spec.assertEqWith s "and bob lost two" (S.lifeOf S.bob settled) (Just 18)
              Spec.assertEqWith s "alice's Pridemate grew" (countersOn aliceMate settled) (Just 1)
              Spec.assertEqWith s "bob's Pridemate did not -- losing life is not gaining it" (countersOn bobMate settled) (Just 0)
        -- The AMOUNT, which the two cases above never read: they count one
        -- event, not the size of it. Sphinx of the Revelation's "whenever you
        -- gain life, you get that many {E}" reads it off the event through
        -- Pawl.Engine.Binding.eventAmount, so a 4-power lifelink hit has to
        -- arrive as four counters and not as one.
        Spec.it s "CR 119.9/120.3f whole card: the Sphinx's lifelink gain is worth that many energy counters" $ do
          sphinx <- S.printingOf s registry "Sphinx of the Revelation"
          let (gs0, mine, _) = S.combatBoardOf [sphinx] []
          case mine of
            [] -> Spec.assertFailure s "fixture should have given alice a Sphinx"
            flier : _ -> do
              let settled = resolveAll (S.fightWith (attacksWith flier) gs0)
              Spec.assertEqWith s "alice got four energy counters, the life she gained" (S.playerCounterOf PlayerCounterKind.Energy S.alice settled) 4
              Spec.assertEqWith s "off a gain of exactly four" (S.lifeOf S.alice settled) (Just 24)
              Spec.assertEqWith s "and bob has none of his own" (S.playerCounterOf PlayerCounterKind.Energy S.bob settled) 0
        -- CR 119.9's last sentence: "if a player gains 0 life, no life gain event
        -- has occurred".
        --
        -- Hand-built, and honestly so: no card in the pool can hand applyDamage a
        -- 0-amount event, CR 510.1a dropping a creature that assigns 0 or less and
        -- Resolve's DealDamage arm guarding its own quantity. What this pins is
        -- therefore applyDamage's own contract -- the door a future producer would
        -- come through -- rather than a board a player could sit at.
        --
        -- Asserted on the LOG rather than through a counter, because the claim is
        -- about the RECORD: a counter assertion would also pass for an engine that
        -- recorded the zero and then declined to match it, which is not what the
        -- rule says. The 2-damage half is the paired control, so an empty answer
        -- cannot pass for the wrong reason.
        Spec.it s "CR 119.9 a 0-damage lifelink event records no life gain at all" $ do
          childOfNight <- S.printingOf s registry "Child of Night"
          let (oid, gs0) = S.addCreature childOfNight S.alice (Setup.emptyGame S.bothPlayers)
              evOf n = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) n False False False 0 (Just S.alice) DamageKind.Combat
              gainsIn gs = [p | GameEvent.LifeGained (LifeChange.MkLifeChange p _) <- S.eventsOf gs]
              after n = S.runPure S.identityAnswer gs0 (Damage.applyDamage [evOf n])
          Spec.assertEqWith s "two damage records the gain" (gainsIn (after 2)) [S.alice]
          Spec.assertEqWith s "zero damage records nothing" (gainsIn (after 0)) []

-- CR 603.10's first sentence, asked of a permanent that never leaves: "objects
-- that exist immediately after an event are checked to see if the event matched
-- any trigger conditions, and continuous effects that exist at that time are used
-- to determine what the trigger conditions are". The abilities that decide are the
-- ones the permanent had AT THE EVENT, not the ones it has when the CR 117.5 scan
-- gets around to looking.
--
-- The two readings need an ability set that MOVES between an event and the scan,
-- with the event first. Synthetic Humbling Draught, {W} Instant, "You gain 2 life.
-- Until end of turn, target creature loses all abilities. You gain 3 life", is that
-- in one resolution: the first CR 119.3 life gain is recorded, the CR 613.1f
-- layer-6 removal lands after it, and the second gain after that -- all before any
-- player could have priority, so the whole thing is one CR 117.5 batch.
--
-- The SECOND gain is what makes the board tell the three readings apart, on the one
-- number every case asserts. Aimed at the Pridemate:
--
--   * abilities as of each event (the rule): the first gain triggers, the second
--     does not -- ONE counter.
--   * abilities as of the scan: neither triggers, the Pridemate having none by then
--     -- NO counter.
--   * abilities as of the batch's first event: both trigger, one sample being
--     stretched over two events that straddle the strip -- TWO counters.
--
-- SYNTHETIC, after searching the printings. Every card that says "loses all
-- abilities" and does anything else in the same resolution strips FIRST -- Day of
-- Black Sun, Patriar's Humiliation, Resolute Rejection, Snakeform, Abigale -- so
-- the ability set has already moved when the event happens and the readings agree.
-- The three printings that order it the other way each need a trigger condition
-- pawl does not have: Merfolk Trickster and The Wondrous Wasp tap first, and
-- nothing here watches a permanent becoming tapped, while Tishana's Tidebinder
-- counters an ability first. Nothing in the CR forbids the card as written; it is
-- Blossoming Calm's shape with Turn to Frog's one layer.
--
-- The pair differs in the TARGET and in nothing else: the same board, the same
-- mana, the same 5 life gained, the same two legal creatures to aim at. Aiming at
-- the Goblin Piker is the control that proves the plumbing -- the Pridemate takes
-- both counters -- and aiming at the Pridemate is the case.
abilitiesWhenTriggeredSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
abilitiesWhenTriggeredSpec s registry =
  let countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- Pinned to the one recipient rather than searched for among the legal ones:
      -- a searching answerer would find the other creature again once the case
      -- under test broke.
      aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimAt victimId p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victimId))) sets
        _ -> S.identityAnswer p
      board = do
        plains <- S.printingOf s registry "Plains"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        piker <- S.printingOf s registry "Goblin Piker"
        draught <- S.printingOf s registry "Synthetic Humbling Draught"
        let (mateId, b0) = S.addCreature pridemate S.alice (S.landsInPlay plains 1)
            (pikerId, b1) = S.addCreature piker S.alice b0
        pure (mateId, pikerId, S.handOne draught b1)
      drinkAt victimId (gs, spellId) =
        let cast = snd (Engine.runGamePure (aimAt victimId) gs (S.cast S.alice spellId))
         in snd (Engine.runGamePure (aimAt victimId) cast Engine.priorityLoop)
   in Spec.describe s "CR 603.10 abilities as of the event" $ do
        Spec.it s "CR 603.10 the control: the Draught strips the PIKER and both gains reach the Pridemate" $ do
          (mateId, pikerId, gs) <- board
          let after = drinkAt pikerId gs
          Spec.assertEqWith s "alice gained 2 and then 3" (S.lifeOf S.alice after) (Just 25)
          Spec.assertEqWith s "and her Pridemate, untouched, took a counter for each" (countersOn mateId after) (Just 2)
        Spec.it s "CR 603.10 stripping the PRIDEMATE takes the second gain from it and not the first" $ do
          (mateId, _, gs) <- board
          let after = drinkAt mateId gs
          Spec.assertEqWith s "the same 5 life, the strip changing neither gain" (S.lifeOf S.alice after) (Just 25)
          Spec.assertBool s (null (Projection.triggeredAbilitiesOf mateId after)) "and the Pridemate really has no abilities left"
          Spec.assertEqWith s "exactly one counter: the gain it still had the ability for" (countersOn mateId after) (Just 1)

-- CR 119.9 read for its NUMBER, which the group above never asks for: Ajani's
-- Pridemate's payload names no amount, so nothing there could tell a bound amount
-- from an unbound one.
--
-- Sanguine Bond, {3}{B}{B} Enchantment, "Whenever you gain life, target opponent
-- loses that much life." CR 603.2 makes the amount part of the event that fired
-- the trigger, and Pawl.Engine.Event.Binding.eventBindings stamps it under
-- Pawl.Engine.Binding.eventAmount, which the card's LoseLife reads as an ordinary
-- Quantity.InSlot.
--
-- What makes this a proof rather than a demonstration is that the two gameplay
-- cases carry DIFFERENT amounts, from different producers:
--
--   * Renewed Faith's "you gain 6 life" -- 6, a number nothing else on that board
--     is (three Plains, a mana value of 3, two life totals of 20).
--   * Radiant Fountain's entry trigger -- 2.
--
-- One constant bound in place of the real amount therefore fails one of the two,
-- and a binding that read the gainer's LIFE TOTAL rather than the gain (26 and 22
-- respectively) fails both.
lifeGainAmountSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lifeGainAmountSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      -- The same staging strippedTriggerSpec's entry fixture uses: the permanent
      -- is placed, its Moved event recorded, and CR 603.6a's scan run at the next
      -- settle.
      entering oid gs =
        let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
         in resolveAll (settle (S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project oid gs))] gs))
   in Spec.describe s "CR 119.9 that much life" $ do
        -- The gameplay-level proof, cast to resolution. alice's Renewed Faith
        -- gains her 6 (CR 119.3), the Bond's trigger matches that event, and its
        -- payload reads the SIX out of the slot -- bob's 20 becomes 14.
        --
        -- alice's own total is asserted too: an engine that made the Bond drain
        -- its controller would show the same 14 on bob only if it also failed
        -- here.
        Spec.it s "CR 603.2 whole cards: alice gains 6 from Renewed Faith and Sanguine Bond drains bob for 6" $ do
          plains <- S.printingOf s registry "Plains"
          sanguineBond <- S.printingOf s registry "Sanguine Bond"
          renewedFaith <- S.printingOf s registry "Renewed Faith"
          let (_, board) = S.addCreature sanguineBond S.alice (S.landsInPlay plains 3)
              (gs, spellId) = S.handOne renewedFaith board
              settled = resolveAll (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId)))
          Spec.assertEqWith s "alice gained exactly 6" (S.lifeOf S.alice settled) (Just 26)
          Spec.assertEqWith s "and bob lost exactly that much" (S.lifeOf S.bob settled) (Just 14)
        -- The SECOND amount, from the other producer, on a board where nothing is
        -- 6: a Bond that bound a constant, or bound the amount from the wrong
        -- event, cannot pass both this and the case above.
        Spec.it s "CR 603.2 a gain of 2 drains 2, not the previous case's 6" $ do
          sanguineBond <- S.printingOf s registry "Sanguine Bond"
          radiantFountain <- S.printingOf s registry "Radiant Fountain"
          let (_, withBond) = S.addCreature sanguineBond S.alice (Setup.emptyGame S.bothPlayers)
              (fountainId, gs) = S.addCreature radiantFountain S.alice withBond
              settled = entering fountainId gs
          Spec.assertEqWith s "alice gained exactly 2" (S.lifeOf S.alice settled) (Just 22)
          Spec.assertEqWith s "and bob lost exactly that much" (S.lifeOf S.bob settled) (Just 18)
        -- eventBindings in isolation, so the binding is pinned to the RULE rather
        -- than to one card's payload -- becameSlotSpec's shape. The 7 is neither
        -- life total nor any other number in reach, so an arm binding anything but
        -- the event's own amount fails here.
        --
        -- BOB's gain read under the You relation, which is what pins the gainer to
        -- the EVENT rather than to the relation: the arm binds whichever player the
        -- event names, and an arm that reached for the ability's controller instead
        -- would answer alice here.
        Spec.it s "CR 603.2 eventBindings binds the amount the event carries, and the player who gained it" $
          Spec.assertEqWith
            s
            "thatMuch is the gain and thatPlayer is the gainer"
            (Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.bob (TriggerCondition.PlayerGainsLife PlayerRelation.You) (GameEvent.LifeGained (LifeChange.MkLifeChange S.bob 7)))
            (Binding.setTriggerPlayer S.bob (Map.singleton Binding.eventAmount (Binding.toAmount 7)))

-- CR 119.9's event read for its PLAYER, which neither group above can ask for:
-- Ajani's Pridemate and Sanguine Bond both watch under CR 109.5's "you", where
-- the gainer and the ability's controller are one seat.
--
-- False Cure, {B}{B} Instant, "Until end of turn, whenever a player gains life,
-- that player loses 2 life for each 1 life they gained." Three things at once,
-- and the board is built so that each fails on its own:
--
--   * CR 102.1's bare "a player" (PlayerRelation.AnyPlayer), so a gain by
--     somebody who is not the caster fires it. bob's Radiant Fountain is that
--     gain.
--   * CR 603.2's gaining player, bound under Binding.triggerPlayer. THREE seats,
--     because a two-seat board collapses "that player" onto the one opponent --
--     carol sits there so that "an opponent" and "the player who gained" are not
--     the same reading.
--   * CR 603.7b's stated duration, which keeps the entry armed through firing:
--     the second gain, alice's own, is a different seat and a different amount.
--   * CR 603.2c's repeat WITHIN one batch, which the two gains above cannot show
--     because they arrive in batches of one. Centaur Peacemaker, on its own
--     board below, puts every seat's gain in a single batch.
--
-- The doubling is Quantity.Plus of the slot with itself, Pawl.Types.Quantity
-- having no multiply -- exact for "2 life for each 1 life", and what makes the
-- two amounts tell a bound amount from a constant.
falseCureSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
falseCureSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      -- lifeGainAmountSpec's entry staging: the permanent is already placed, its
      -- Moved event is recorded, and CR 603.6a's scan runs at the next settle.
      entering oid gs =
        let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
         in resolveAll (settle (S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project oid gs))] gs))
      -- alice casts the Cure off two Swamps on a three-seat board, and everything
      -- else is already on the battlefield: bob's Radiant Fountain (CR 119.3, "you
      -- gain 2 life" for BOB), alice's Soul Warden and a Goblin Piker under carol
      -- for the Warden to see enter.
      armed = do
        swamp <- S.printingOf s registry "Swamp"
        falseCure <- S.printingOf s registry "False Cure"
        fountain <- S.printingOf s registry "Radiant Fountain"
        soulWarden <- S.printingOf s registry "Soul Warden"
        piker <- S.printingOf s registry "Goblin Piker"
        let lands = S.landsFor swamp S.alice 2 S.threePlayerGame
            (fountainId, withFountain) = S.addCreature fountain S.bob lands
            (_, withWarden) = S.addCreature soulWarden S.alice withFountain
            (pikerId, withPiker) = S.addCreature piker S.carol withWarden
            (spellId, withSpell) = S.addHandCard falseCure S.alice withPiker
        pure (fountainId, pikerId, resolveAll (snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))))
      -- The Cure armed on `base` with Centaur Peacemaker, {1}{G}{W} Creature --
      -- Centaur Cleric 3/3, "When this creature enters, each player gains 4
      -- life." ONE resolution records a LifeGained per seat, so the whole board's
      -- gains reach the CR 117.5 settle as one batch. Its OWN minimal board: the
      -- Soul Warden above would see the Peacemaker enter and add a gain that is
      -- not part of the batch under test.
      peacemakerArmed base = do
        swamp <- S.printingOf s registry "Swamp"
        falseCure <- S.printingOf s registry "False Cure"
        peacemaker <- S.printingOf s registry "Centaur Peacemaker"
        let lands = S.landsFor swamp S.alice 2 base
            (peacemakerId, withPeacemaker) = S.addCreature peacemaker S.alice lands
            (spellId, withSpell) = S.addHandCard falseCure S.alice withPeacemaker
        pure (peacemakerId, resolveAll (snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))))
   in Spec.describe s "CR 603.7 False Cure" $ do
        -- The gain that is NOT the caster's. bob gains 2 and loses 4 -- and alice's
        -- and carol's totals are asserted untouched, which is the whole of #826: an
        -- arm that bound CR 109.5's "you" in place of the event's player would show
        -- alice at 16 and bob at 22 on this very board.
        Spec.it s "CR 102.1/603.2 bob gains 2 from his own Radiant Fountain and loses 4" $ do
          (fountainId, _, gs) <- armed
          let after = entering fountainId gs
          Spec.assertEqWith s "bob gained 2 and then lost 4" (S.lifeOf S.bob after) (Just 18)
          Spec.assertEqWith s "alice, who cast the Cure, is untouched" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "and so is carol" (S.lifeOf S.carol after) (Just 20)
        -- The SECOND firing, on a different seat and a different amount: CR 603.7b's
        -- stated duration keeps the entry armed, and the Soul Warden's 1 becomes 2
        -- rather than the 4 above. A doubling that bound a constant fails here; an
        -- entry spent by its first firing fires not at all.
        Spec.it s "CR 603.7b the same entry fires again for alice, whose gain is 1" $ do
          (fountainId, pikerId, gs) <- armed
          let afterBob = entering fountainId gs
              after = entering pikerId afterBob
          Spec.assertEqWith s "alice gained 1 from her Soul Warden and lost 2" (S.lifeOf S.alice after) (Just 19)
          Spec.assertEqWith s "bob is where the first firing left him" (S.lifeOf S.bob after) (Just 18)
          Spec.assertEqWith s "carol gained nothing and lost nothing" (S.lifeOf S.carol after) (Just 20)
        -- The control, through the narrowest path that ends the duration: CR 514.2's
        -- cleanup, which Expiry.dropAtCleanup is. The SAME gain on the SAME board
        -- afterwards costs bob nothing, so the two cases differ in exactly one thing.
        Spec.it s "CR 514.2 the entry is gone after cleanup, so the same gain costs nothing" $ do
          (fountainId, _, gs) <- armed
          let after = entering fountainId (Expiry.dropAtCleanup gs)
          Spec.assertEqWith s "bob gained his 2 and kept it" (S.lifeOf S.bob after) (Just 22)
          Spec.assertEqWith s "alice is untouched either way" (S.lifeOf S.alice after) (Just 20)
        -- CR 603.2c inside ONE batch, which the cases above cannot reach: the
        -- entry's trigger event occurs three times before the settle, and CR
        -- 603.7b's stated duration lifts the one shot, so 603.2c's "it can trigger
        -- repeatedly" applies and every seat pays 8. An entry taking the first
        -- match out of the batch drains alice alone and leaves bob and carol at 24.
        Spec.it s "CR 603.2c three seats gain in one batch, so the entry fires three times" $ do
          (peacemakerId, gs) <- peacemakerArmed S.threePlayerGame
          let after = entering peacemakerId gs
          Spec.assertEqWith s "alice starts at 20" (S.lifeOf S.alice gs) (Just 20)
          Spec.assertEqWith s "bob starts at 20" (S.lifeOf S.bob gs) (Just 20)
          Spec.assertEqWith s "carol starts at 20" (S.lifeOf S.carol gs) (Just 20)
          Spec.assertEqWith s "alice gained 4 and lost 8" (S.lifeOf S.alice after) (Just 16)
          Spec.assertEqWith s "bob gained 4 and lost 8" (S.lifeOf S.bob after) (Just 16)
          Spec.assertEqWith s "carol gained 4 and lost 8" (S.lifeOf S.carol after) (Just 16)
        -- The other half of the pair, differing in exactly one thing -- how many
        -- occurrences the batch holds. FOUR seats, so the firing count is four and
        -- not the three above: a fixed number of firings, or one per batch, passes
        -- at most one of the two boards. Four rather than two, since two seats
        -- would collapse "that player" onto the one opponent.
        Spec.it s "CR 603.2c a fourth seat in the batch is a fourth firing" $ do
          (peacemakerId, gs) <- peacemakerArmed S.fourPlayerGame
          let after = entering peacemakerId gs
          Spec.assertEqWith s "dave starts at 20" (S.lifeOf S.dave gs) (Just 20)
          Spec.assertEqWith s "alice gained 4 and lost 8" (S.lifeOf S.alice after) (Just 16)
          Spec.assertEqWith s "bob gained 4 and lost 8" (S.lifeOf S.bob after) (Just 16)
          Spec.assertEqWith s "carol gained 4 and lost 8" (S.lifeOf S.carol after) (Just 16)
          Spec.assertEqWith s "dave gained 4 and lost 8" (S.lifeOf S.dave after) (Just 16)
        -- The vacuity guard, not a prover: the same Peacemaker with NO Cure armed
        -- leaves every seat holding its 4 (CR 119.3). Without it a board where
        -- "each player gains 4" quietly gained nobody anything would read as a
        -- passing 16 above, the drains never having happened either.
        Spec.it s "CR 119.3 with no entry armed, each of the three seats keeps its 4" $ do
          peacemaker <- S.printingOf s registry "Centaur Peacemaker"
          let (peacemakerId, gs) = S.addCreature peacemaker S.alice S.threePlayerGame
              after = entering peacemakerId gs
          Spec.assertEqWith s "alice is at 24" (S.lifeOf S.alice after) (Just 24)
          Spec.assertEqWith s "bob is at 24" (S.lifeOf S.bob after) (Just 24)
          Spec.assertEqWith s "carol is at 24" (S.lifeOf S.carol after) (Just 24)

-- An answer to CR 603.7b's question below, pinned BY INDEX and by nothing else.
-- An answerer that searched the candidates for a legal one would find the earliest
-- again under any mutation, and Pawl.Engine.Replay.defaultAnswer -- what
-- S.identityAnswer and every other fallthrough answerer reaches -- IS the
-- earliest. A board answered by one of those cannot tell the rule from its
-- absence.
choosingGain :: Natural -> Prompt.Prompt r -> r
choosingGain n prompt = case prompt of
  Prompt.ChooseDelayedTriggerEvent {} -> n
  _ -> S.identityAnswer prompt

-- CR 603.7b's SECOND sentence, which the group above cannot reach: "if its
-- trigger event occurs more than once simultaneously and the ability doesn't
-- have a stated duration, the controller of the delayed triggered ability
-- chooses which event causes the ability to trigger."
--
-- False Cure states "until end of turn", which is exactly the duration that
-- sentence excludes: CR 603.2c then fires it once per occurrence and there is
-- nothing to choose. The producer has to be the same delayed entry WITHOUT one.
--
--   * Synthetic Singular Cure {B}{B} Instant
--     (data/cards/synthetic-singular-cure.json): "The next time a player gains
--     life, that player loses 2 life for each 1 life they gained."
--
-- WHY A SYNTHETIC. Scryfall o:/[Ww]hen(ever)? a player gains life/, 2026-08-26,
-- matches one printing -- False Cure, whose duration disqualifies it. Off the
-- life axis, o:/[Tt]he next time/ -o:"this turn" -o:"until end of turn"
-- -o:"each turn", same date, matches four: Five-Finger Discount, Ria Ivor, Spire
-- Phantasm and The Big Idea, each watching a single named object or a single die
-- roll, none of which can occur twice at once. So no printing arms a
-- duration-less delayed ability on an event one batch can hold two of, and a
-- printing that did -- "the next time a player gains life", with no duration --
-- would refute this. Nothing in the CR forbids one: 603.7b's second sentence is
-- written for exactly this shape.
--
-- The batch is Centaur Peacemaker's "each player gains 4 life" again, one
-- EventGroup across the seats (CR 608.2f). What the cases read is WHICH SEAT is
-- drained: the entry has no duration, so CR 603.7b's first sentence spends it on
-- one occurrence, exactly one seat pays 8 and the rest keep their 4. The
-- amounts are equal on purpose -- identity is then the only separator, so an
-- assertion about the drained seat cannot pass on an arithmetic coincidence.
singularCureSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
singularCureSpec s registry =
  let resolveAll n gs = snd (Engine.runGamePure (choosingGain n) gs Engine.priorityLoop)
      settle n gs = snd (Engine.runGamePure (choosingGain n) gs Engine.settleForPriority)
      -- falseCureSpec's staging: the permanent is already placed, its Moved
      -- event is recorded, and CR 603.6a's scan runs at the next settle.
      entering n oid gs =
        let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
         in resolveAll n (settle n (S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project oid gs))] gs))
      -- The distinct EventGroups the log's life gains carry. The precondition
      -- the whole group rests on, asserted rather than assumed: were the seats'
      -- gains not one group, the earliest-group step would already have picked a
      -- seat and CR 603.7b's second sentence would never be reached.
      gainsIn gs =
        Maybe.mapMaybe
          ( \logged -> case LoggedEvent.event logged of
              GameEvent.LifeGained _ -> Just (LoggedEvent.group logged)
              _ -> Nothing
          )
          (Foldable.toList (GameState.events gs))
      -- alice casts the Cure off two Swamps with a Centaur Peacemaker already on
      -- the battlefield, waiting to enter. Its own minimal board, peacemakerArmed's
      -- reason: any other life gain would be a second batch.
      armed base = do
        swamp <- S.printingOf s registry "Swamp"
        cure <- S.printingOf s registry "Synthetic Singular Cure"
        peacemaker <- S.printingOf s registry "Centaur Peacemaker"
        let lands = S.landsFor swamp S.alice 2 base
            (peacemakerId, withPeacemaker) = S.addCreature peacemaker S.alice lands
            (spellId, withSpell) = S.addHandCard cure S.alice withPeacemaker
        pure (peacemakerId, resolveAll 0 (snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))))
      -- The same Cure with a LONE gain to watch: bob's Radiant Fountain (CR
      -- 119.3, "you gain 2 life") entering instead of the Peacemaker.
      armedWithFountain = do
        swamp <- S.printingOf s registry "Swamp"
        cure <- S.printingOf s registry "Synthetic Singular Cure"
        fountain <- S.printingOf s registry "Radiant Fountain"
        let lands = S.landsFor swamp S.alice 2 S.threePlayerGame
            (fountainId, withFountain) = S.addCreature fountain S.bob lands
            (spellId, withSpell) = S.addHandCard cure S.alice withFountain
        pure (fountainId, resolveAll 0 (snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))))
   in Spec.describe s "CR 603.7b Synthetic Singular Cure" $ do
        -- The proving case. Three seats gain 4 in one event group and alice, who
        -- controls the entry, names the THIRD of them: carol pays 8 and nobody
        -- else pays anything. An engine that takes the earliest match drains
        -- alice and leaves carol at 24.
        Spec.it s "CR 603.7b the controller names carol's gain out of three simultaneous ones" $ do
          (peacemakerId, gs) <- armed S.threePlayerGame
          let after = entering 2 peacemakerId gs
          Spec.assertEqWith s "carol gained 4 and lost 8" (S.lifeOf S.carol after) (Just 16)
          Spec.assertEqWith s "alice gained 4 and kept it" (S.lifeOf S.alice after) (Just 24)
          Spec.assertEqWith s "so did bob" (S.lifeOf S.bob after) (Just 24)
          Spec.assertEqWith s "every seat started at 20" (fmap (\pid -> S.lifeOf pid gs) [S.alice, S.bob, S.carol]) [Just 20, Just 20, Just 20]
          Spec.assertEqWith s "three gains" (length (gainsIn after)) 3
          Spec.assertEqWith s "in one event group, so the choice is CR 603.7b's" (length (List.nub (gainsIn after))) 1
          Spec.assertEqWith s "and the entry is spent, having no stated duration" (Seq.length (GameState.delayedTriggers after)) 0
        -- The other half of the pair, differing in exactly one thing -- the
        -- answer. Same board, same batch, bob named instead: an engine that
        -- ignores the answer cannot pass both cases.
        Spec.it s "CR 603.7b the same batch answered differently drains bob instead" $ do
          (peacemakerId, gs) <- armed S.threePlayerGame
          let after = entering 1 peacemakerId gs
          Spec.assertEqWith s "bob gained 4 and lost 8" (S.lifeOf S.bob after) (Just 16)
          Spec.assertEqWith s "alice kept her 4" (S.lifeOf S.alice after) (Just 24)
          Spec.assertEqWith s "and so did carol" (S.lifeOf S.carol after) (Just 24)
        -- A FOURTH seat, so the candidate list is longer than any three-seat
        -- board can offer: naming dave is an answer no collapse onto "the last
        -- opponent" of the boards above reaches.
        Spec.it s "CR 603.7b a fourth seat is a fourth candidate" $ do
          (peacemakerId, gs) <- armed S.fourPlayerGame
          let after = entering 3 peacemakerId gs
          Spec.assertEqWith s "dave gained 4 and lost 8" (S.lifeOf S.dave after) (Just 16)
          Spec.assertEqWith s "alice kept her 4" (S.lifeOf S.alice after) (Just 24)
          Spec.assertEqWith s "bob kept his" (S.lifeOf S.bob after) (Just 24)
          Spec.assertEqWith s "carol kept hers" (S.lifeOf S.carol after) (Just 24)
          Spec.assertEqWith s "four gains in one event group" (length (gainsIn after), length (List.nub (gainsIn after))) (4, 1)
        -- The plumbing control, and the elision: ONE gain is not a choice, so no
        -- question is raised and the answer above cannot reach it. bob's own
        -- Radiant Fountain gains him 2 and the Cure takes 4, whatever index the
        -- answerer would have given.
        Spec.it s "CR 603.7b one occurrence is not a choice, so bob pays for his own gain" $ do
          (fountainId, gs) <- armedWithFountain
          let after = entering 2 fountainId gs
          Spec.assertEqWith s "bob gained 2 and lost 4" (S.lifeOf S.bob after) (Just 18)
          Spec.assertEqWith s "alice is untouched" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "and so is carol" (S.lifeOf S.carol after) (Just 20)
          Spec.assertEqWith s "one gain, one group" (length (gainsIn after), length (List.nub (gainsIn after))) (1, 1)
        -- The vacuity guard, falseCureSpec's: the same Peacemaker with NO entry
        -- armed leaves every seat holding its 4 (CR 119.3). Without it a board
        -- where nobody actually gained would read as a passing 24 above.
        Spec.it s "CR 119.3 with no entry armed, each of the three seats keeps its 4" $ do
          peacemaker <- S.printingOf s registry "Centaur Peacemaker"
          let (peacemakerId, gs) = S.addCreature peacemaker S.alice S.threePlayerGame
              after = entering 2 peacemakerId gs
          Spec.assertEqWith s "alice is at 24" (S.lifeOf S.alice after) (Just 24)
          Spec.assertEqWith s "bob is at 24" (S.lifeOf S.bob after) (Just 24)
          Spec.assertEqWith s "carol is at 24" (S.lifeOf S.carol after) (Just 24)

-- CR 101.4: two delayed entries with DIFFERENT controllers matched by one batch
-- are choices made at the same time, so the active player makes theirs first and
-- the nonactive players follow in turn order. CR 101.4b is what makes the order
-- load-bearing rather than cosmetic, the later chooser knowing what the earlier
-- one named, and CR 603.7d fixes each entry's chooser as the player who
-- controlled the spell that created it.
--
-- The two entries are two copies of Synthetic Singular Cure, one cast by each of
-- two seats; no second card is needed. GameState.delayedTriggers appends in
-- RESOLUTION order and the stack is last-in-first-out (CR 405.2 / 608.1), so the seat who
-- casts LAST arms FIRST -- which is how arming order and APNAP order come apart
-- on a two-seat board at all. The first case below asserts that gap on the board
-- rather than assuming it.
--
-- The batch is singularCureSpec's: Centaur Peacemaker entering, "each player
-- gains 4 life", one EventGroup across the seats (CR 608.2f), so each entry's
-- per-occurrence condition matches three times and each controller is asked.
--
-- The answerer has to be able to SEE the order or nothing here is observable: a
-- pure Prompt r -> r answers two structurally alike prompts alike whatever the
-- engine did, and both orders then drain the same seats. `laterKnows` is CR
-- 101.4b turned into an answer -- the seat asked FIRST names its own gain, and
-- the seat asked SECOND names carol's, carol controlling no entry of her own --
-- so which seat drains itself is exactly the ordering question.
apnapDelayedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
apnapDelayedSpec s registry =
  let -- Which candidate is the named seat's own gain. FILTERED rather than
      -- guessed: the answer indexes the offered list, so it is built from the
      -- candidates the engine handed over.
      indexOf pid candidates =
        let gained event = case event of
              GameEvent.LifeGained lc -> LifeChange.player lc == pid
              _ -> False
         in maybe 0 Int.toNaturalSaturating (List.findIndex gained (NonEmpty.toList candidates))
      -- The state is the sequence of controllers asked, which is both what the
      -- later answer depends on and what `asked` below reads.
      laterKnows :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
      laterKnows p = case p of
        Prompt.ChooseDelayedTriggerEvent _ controller _ candidates -> do
          earlier <- State.get
          State.put (earlier <> [controller])
          pure (indexOf (if null earlier then controller else S.carol) candidates)
        _ -> pure (S.identityAnswer p)
      -- The distinct EventGroups the log's life gains carry. singularCureSpec's
      -- precondition, for the same reason: were the seats' gains not one group,
      -- the earliest-group step would have picked a seat and CR 603.7b's second
      -- sentence -- and so the prompt this group orders -- would never be reached.
      gainsIn gs =
        Maybe.mapMaybe
          ( \logged -> case LoggedEvent.event logged of
              GameEvent.LifeGained _ -> Just (LoggedEvent.group logged)
              _ -> Nothing
          )
          (Foldable.toList (GameState.events gs))
      -- `first` casts a Cure, then `second` does, then both resolve: the stack
      -- being last-in-first-out, `second` arms first. alice is the active player
      -- either way, and the Peacemaker is already placed with its Moved event
      -- recorded, so the CR 603.6a scan runs at the next settle.
      armed first second = do
        swamp <- S.printingOf s registry "Swamp"
        cure <- S.printingOf s registry "Synthetic Singular Cure"
        peacemaker <- S.printingOf s registry "Centaur Peacemaker"
        let lands = S.landsFor swamp S.bob 2 (S.landsFor swamp S.alice 2 S.threePlayerGame)
            (peacemakerId, withPeacemaker) = S.addCreature peacemaker S.alice lands
            (firstSpell, withFirst) = S.addHandCard cure first withPeacemaker
            (secondSpell, withSecond) = S.addHandCard cure second withFirst
            cast = S.cast first firstSpell >> S.cast second secondSpell
            resolved = snd (Engine.runGamePure S.identityAnswer (snd (Engine.runGamePure S.identityAnswer withSecond cast)) Engine.priorityLoop)
            moved = ZoneChange.MkZoneChange peacemakerId peacemakerId Zone.Stack Zone.Battlefield
        pure (S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project peacemakerId resolved))] resolved)
      -- One run, read twice, so the controllers asked and the life totals cannot
      -- come from different games.
      played gs = State.runState (Engine.runGame laterKnows gs (Engine.settleForPriority >> Engine.priorityLoop)) []
      asked gs = snd (played gs)
      after gs = snd (fst (played gs))
      -- The store's own order, which the reordering must NOT disturb: it is the
      -- arming order every later gather reads. Read on the pre-batch board, where
      -- it is both this group's precondition -- arming order is not APNAP order,
      -- or nothing here is observable -- and the fence on that half of the change.
      armingOrder gs = fmap DelayedTrigger.controller (Foldable.toList (GameState.delayedTriggers gs))
   in Spec.describe s "CR 101.4 simultaneous delayed triggers" $ do
        -- The proving case. alice casts first, so BOB's entry is armed first, and
        -- an engine that walks the store asks bob before alice. Under CR 101.4 the
        -- active player is asked first: alice names her own gain and drains
        -- herself, and bob, knowing that, names carol's. Ask in arming order
        -- instead and it is bob who drains himself while alice keeps her 4.
        Spec.it s "CR 101.4 the active player is asked first even though the other seat armed first" $ do
          gs <- armed S.alice S.bob
          Spec.assertEqWith s "alice was asked first, so she drained her own gain" (S.lifeOf S.alice (after gs)) (Just 16)
          Spec.assertEqWith s "bob was asked second, so he kept his 4 and drained carol instead" (S.lifeOf S.bob (after gs)) (Just 24)
          Spec.assertEqWith s "carol paid for the second answer" (S.lifeOf S.carol (after gs)) (Just 16)
          Spec.assertEqWith s "and the two questions were raised in APNAP order" (asked gs) [S.alice, S.bob]
          Spec.assertEqWith s "bob's entry was armed first, so arming order is not APNAP order, and the store still holds it" (armingOrder gs) [S.bob, S.alice]
          Spec.assertEqWith s "setup: every seat started at 20" (fmap (\pid -> S.lifeOf pid gs) [S.alice, S.bob, S.carol]) [Just 20, Just 20, Just 20]
          Spec.assertEqWith s "three gains, in one event group" (length (gainsIn (after gs)), length (List.nub (gainsIn (after gs)))) (3, 1)
          Spec.assertEqWith s "and both entries are spent, neither having a stated duration" (Seq.length (GameState.delayedTriggers (after gs))) 0
        -- The other half of the pair, differing in exactly one thing -- which seat
        -- cast first, and so which entry was armed first. bob casts first, alice
        -- second, and the arming order is now APNAP order already: the same seats
        -- drain the same amounts. An engine that merely REVERSED the store passes
        -- the case above and fails this one; one that sorted the wrong way round
        -- fails both.
        Spec.it s "CR 101.4 arming the other way round changes nothing" $ do
          gs <- armed S.bob S.alice
          Spec.assertEqWith s "alice is still asked first and still drains herself" (S.lifeOf S.alice (after gs)) (Just 16)
          Spec.assertEqWith s "bob still keeps his 4" (S.lifeOf S.bob (after gs)) (Just 24)
          Spec.assertEqWith s "carol still pays" (S.lifeOf S.carol (after gs)) (Just 16)
          Spec.assertEqWith s "the same two questions in the same order" (asked gs) [S.alice, S.bob]
          Spec.assertEqWith s "setup: alice's entry was armed first this time" (armingOrder gs) [S.alice, S.bob]
        -- The vacuity guard: the same Peacemaker with NO entry armed asks nobody
        -- anything and leaves all three seats holding their 4 (CR 119.3). Without
        -- it an empty question list above would read as a passing order.
        Spec.it s "CR 119.3 with no entry armed, nobody is asked and each seat keeps its 4" $ do
          peacemaker <- S.printingOf s registry "Centaur Peacemaker"
          let (peacemakerId, placed) = S.addCreature peacemaker S.alice S.threePlayerGame
              moved = ZoneChange.MkZoneChange peacemakerId peacemakerId Zone.Stack Zone.Battlefield
              gs = S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project peacemakerId placed))] placed
          Spec.assertEqWith s "no question is raised" (asked gs) []
          Spec.assertEqWith s "alice is at 24" (S.lifeOf S.alice (after gs)) (Just 24)
          Spec.assertEqWith s "bob is at 24" (S.lifeOf S.bob (after gs)) (Just 24)
          Spec.assertEqWith s "carol is at 24" (S.lifeOf S.carol (after gs)) (Just 24)

-- CR 101.4c: a player making more than one choice at the same time picks the
-- order they make them in. The choices are CR 603.7b's -- one seat holding TWO
-- durationless delayed entries that one batch matches is asked twice, and
-- nothing in the rules specifies which question comes first, so the player does.
-- CR 603.3b's Prompt.OrderTriggers is the shape that asks it.
--
-- The two entries are Synthetic Singular Cure and Synthetic Singular Toll, both
-- cast by alice: the same trigger condition on the same batch with DIFFERENT
-- payloads, which is what makes the order observable on the board at all. Two
-- copies of one card cannot show it -- equal payloads drain the same seats by
-- the same amounts whichever entry named which gain -- so the second synthetic
-- is the observability, not the capability.
--
--   * Synthetic Singular Toll {B} Instant
--     (data/cards/synthetic-singular-toll.json): "The next time a player gains
--     life, that player loses 3 life."
--
-- WHY A SYNTHETIC. Its sibling's search, re-run: Scryfall o:/[Ww]hen(ever)? a
-- player gains life/, 2026-09-02, matches one printing -- False Cure, whose
-- stated duration is what CR 603.7b's second sentence excludes. A printing that
-- armed a duration-less delayed ability on a life gain would refute both cards;
-- nothing in the CR forbids one.
--
-- 3 against the Cure's 8 (twice the 4 gained) on purpose: the two entries must
-- be told apart by the AMOUNT a seat loses, so equal amounts would leave the two
-- orders indistinguishable however the questions were asked.
--
-- The batch is singularCureSpec's Centaur Peacemaker, "each player gains 4
-- life", one EventGroup across the seats (CR 608.2f), so each entry's
-- per-occurrence condition matches three times and each is asked once.
oneSeatDelayedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
oneSeatDelayedSpec s registry =
  let -- apnapDelayedSpec's: which candidate is the named seat's own gain,
      -- FILTERED out of the offered list rather than guessed.
      indexOf pid candidates =
        let gained event = case event of
              GameEvent.LifeGained lc -> LifeChange.player lc == pid
              _ -> False
         in maybe 0 Int.toNaturalSaturating (List.findIndex gained (NonEmpty.toList candidates))
      -- The state is the sources asked, in order, and every ordering prompt
      -- raised, tagged with how many per-occurrence questions preceded it.
      -- `picks` is the permutation alice answers the CR 101.4c one with, the one
      -- thing the two proving cases differ in.
      --
      -- TWO ordering prompts reach this board and the tag is what tells them
      -- apart: CR 101.4c's is raised as the batch is gathered, before any
      -- question, and CR 603.3b's is raised by Engine.orderPending afterwards,
      -- over the two triggers alice is putting on the stack. Only the first is
      -- this group's, so only it is answered with `picks`.
      --
      -- CR 101.4b turned into an answer, apnapDelayedSpec's `laterKnows`: the
      -- entry asked FIRST names alice's own gain and the one asked SECOND names
      -- carol's, carol controlling no entry. So which seat loses 8 and which
      -- loses 3 is exactly the ordering question.
      answering :: ([Natural] -> [Natural]) -> Prompt.Prompt r -> State.State ([ObjectId.ObjectId], [(Int, [TriggerSource.TriggerSource])]) r
      answering picks p = case p of
        Prompt.OrderTriggers _ _ entries -> do
          (questions, orderings) <- State.get
          State.put (questions, orderings <> [(length questions, fmap TriggerEntry.source entries)])
          let canonical = zipWith const [0 ..] entries
          pure (if null questions then picks canonical else canonical)
        Prompt.ChooseDelayedTriggerEvent _ _ source candidates -> do
          (questions, orderings) <- State.get
          State.put (questions <> [source], orderings)
          pure (indexOf (if null questions then S.alice else S.carol) candidates)
        _ -> pure (S.identityAnswer p)
      -- The distinct EventGroups the log's life gains carry, and how many. The
      -- precondition the group rests on: were the seats' gains not one group, the
      -- earliest-group step would have picked a seat and CR 603.7b's second
      -- sentence -- and so the questions this group orders -- never reached.
      gainsIn gs =
        Maybe.mapMaybe
          ( \logged -> case LoggedEvent.event logged of
              GameEvent.LifeGained _ -> Just (LoggedEvent.group logged)
              _ -> Nothing
          )
          (Foldable.toList (GameState.events gs))
      -- alice casts the Cure and then the Toll off three Swamps, with a Centaur
      -- Peacemaker already placed and its Moved event recorded, so the CR 603.6a
      -- scan runs at the next settle. The stack is last-in-first-out (CR 405.2 /
      -- 608.1), so the Toll resolves first and so arms first.
      armed = do
        swamp <- S.printingOf s registry "Swamp"
        cure <- S.printingOf s registry "Synthetic Singular Cure"
        toll <- S.printingOf s registry "Synthetic Singular Toll"
        peacemaker <- S.printingOf s registry "Centaur Peacemaker"
        let lands = S.landsFor swamp S.alice 3 S.threePlayerGame
            (peacemakerId, withPeacemaker) = S.addCreature peacemaker S.alice lands
            (cureSpell, withCure) = S.addHandCard cure S.alice withPeacemaker
            (tollSpell, withToll) = S.addHandCard toll S.alice withCure
            cast = S.cast S.alice cureSpell >> S.cast S.alice tollSpell
            resolved = snd (Engine.runGamePure S.identityAnswer (snd (Engine.runGamePure S.identityAnswer withToll cast)) Engine.priorityLoop)
            moved = ZoneChange.MkZoneChange peacemakerId peacemakerId Zone.Stack Zone.Battlefield
        pure (S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project peacemakerId resolved))] resolved)
      -- The Cure alone, for the control: one entry, so there is nothing to order.
      armedAlone = do
        swamp <- S.printingOf s registry "Swamp"
        cure <- S.printingOf s registry "Synthetic Singular Cure"
        peacemaker <- S.printingOf s registry "Centaur Peacemaker"
        let lands = S.landsFor swamp S.alice 3 S.threePlayerGame
            (peacemakerId, withPeacemaker) = S.addCreature peacemaker S.alice lands
            (cureSpell, withCure) = S.addHandCard cure S.alice withPeacemaker
            resolved = snd (Engine.runGamePure S.identityAnswer (snd (Engine.runGamePure S.identityAnswer withCure (S.cast S.alice cureSpell))) Engine.priorityLoop)
            moved = ZoneChange.MkZoneChange peacemakerId peacemakerId Zone.Stack Zone.Battlefield
        pure (S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project peacemakerId resolved))] resolved)
      -- One run, read three ways, so the questions asked and the life totals
      -- cannot come from different games.
      played picks gs = State.runState (Engine.runGame (answering picks) gs (Engine.settleForPriority >> Engine.priorityLoop)) ([], [])
      asked picks gs = fst (snd (played picks gs))
      offered picks gs = snd (snd (played picks gs))
      after picks gs = snd (fst (played picks gs))
      -- The store's own order, which the reordering must NOT disturb: it is the
      -- arming order every later gather reads.
      armingOrder gs = fmap DelayedTrigger.source (Foldable.toList (GameState.delayedTriggers gs))
   in Spec.describe s "CR 101.4c one seat's simultaneous delayed triggers" $ do
        -- The proving case. alice answers the ordering prompt with the reverse of
        -- the store, so the CURE is asked first: it names alice's own gain and
        -- drains her 8, and the Toll, asked second, takes 3 off carol. An engine
        -- that ignores the answer and asks in arming order drains alice 3 and
        -- carol 8 instead.
        Spec.it s "CR 101.4c the entry alice names first is asked first" $ do
          gs <- armed
          Spec.assertEqWith s "the Cure was asked first, so alice lost 8 of her own" (S.lifeOf S.alice (after reverse gs)) (Just 16)
          Spec.assertEqWith s "the Toll was asked second, so carol lost 3" (S.lifeOf S.carol (after reverse gs)) (Just 21)
          Spec.assertEqWith s "bob was named by neither" (S.lifeOf S.bob (after reverse gs)) (Just 24)
          Spec.assertEqWith s "and the two questions came in the order alice chose" (asked reverse gs) (reverse (armingOrder gs))
          -- CR 101.4c's prompt is the one raised before any question; CR 603.3b's
          -- follows the two answers, and its entries are still in STORE order --
          -- the half of this that must not move, since `outcomes` feeds it.
          Spec.assertEqWith s "one ordering prompt before the questions, over alice's two entries in store order" (offered reverse gs) [(0, fmap TriggerSource.OfObject (armingOrder gs)), (2, fmap TriggerSource.OfObject (armingOrder gs))]
          Spec.assertEqWith s "setup: every seat started at 20" (fmap (\pid -> S.lifeOf pid gs) [S.alice, S.bob, S.carol]) [Just 20, Just 20, Just 20]
          Spec.assertEqWith s "three gains, in one event group" (length (gainsIn (after reverse gs)), length (List.nub (gainsIn (after reverse gs)))) (3, 1)
          Spec.assertEqWith s "and both entries are spent, neither having a stated duration" (Seq.length (GameState.delayedTriggers (after reverse gs))) 0
        -- The other half of the pair, differing in exactly one thing -- the
        -- permutation alice answers with. The Toll is asked first now, so the
        -- amounts swap seats: alice pays 3 and carol pays 8.
        Spec.it s "CR 101.4c the other order swaps which seat pays which amount" $ do
          gs <- armed
          Spec.assertEqWith s "the Toll was asked first, so alice lost only 3" (S.lifeOf S.alice (after id gs)) (Just 21)
          Spec.assertEqWith s "the Cure was asked second, so carol lost 8" (S.lifeOf S.carol (after id gs)) (Just 16)
          Spec.assertEqWith s "bob is still untouched" (S.lifeOf S.bob (after id gs)) (Just 24)
          Spec.assertEqWith s "and the questions came in store order this time" (asked id gs) (armingOrder gs)
          Spec.assertEqWith s "the same CR 101.4c prompt was raised, ahead of the same two questions" (fmap fst (offered id gs)) [0, 2]
        -- The elision, and the control: ONE entry is not an order, so no ordering
        -- prompt is raised at all -- while the per-occurrence question still is,
        -- which is what keeps the case from passing on an empty board.
        Spec.it s "CR 101.4c one entry has nothing to order, so nobody is asked to" $ do
          gs <- armedAlone
          Spec.assertEqWith s "no ordering prompt" (offered reverse gs) []
          Spec.assertEqWith s "the Cure was still asked which gain, and named alice's" (S.lifeOf S.alice (after reverse gs)) (Just 16)
          Spec.assertEqWith s "one question, from the one entry" (asked reverse gs) (armingOrder gs)
          Spec.assertEqWith s "bob kept his 4" (S.lifeOf S.bob (after reverse gs)) (Just 24)
          Spec.assertEqWith s "and so did carol" (S.lifeOf S.carol (after reverse gs)) (Just 24)

-- CR 603.2c's FIRST sentence on the LIFE side, and the CR 608.2f bracket that
-- makes it reachable: "each player gains 4 life" is ONE action taken on several
-- players, processed simultaneously, so every seat's gain shares one
-- Pawl.Types.EventGroup and an ability watching "one or more players gain life"
-- finds one trigger event where the per-seat conditions above find one each.
--
-- Only a batch condition can tell the two apart: every other reader of a life
-- gain answers per occurrence, which is why the grouping went unobserved for as
-- long as it did.
--
--   * Synthetic Communal Vigil {1}{W} Enchantment
--     (data/cards/synthetic-communal-vigil.json): "Whenever one or more players
--     gain life, draw a card."
--
-- WHY A SYNTHETIC. Scryfall o:/one or more [^.]*gain(s)? life/, 2026-08-26,
-- matches one printing: Path of Bravery, whose "one or more" counts ATTACKING
-- CREATURES and whose life gain is the ability's effect rather than its trigger
-- event. So no printing reads a life gain in the batch scope, and a printing
-- that did -- "whenever one or more players gain life" -- would refute this.
-- Nothing in the CR forbids one: CR 603.2c's first sentence is written for
-- exactly this shape, and PermanentsDie is the same wording one event family
-- over.
--
-- The batch comes from Centaur Peacemaker, {1}{G}{W} Creature -- Centaur Cleric
-- 3/3, "When this creature enters, each player gains 4 life." Every seat's LIFE
-- TOTAL is the same under either reading, which is why what the cases read is
-- how many CARDS the Vigil drew: one for the batch, where a seat-at-a-time
-- reading draws one per seat.
communalVigilSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
communalVigilSpec s registry =
  let -- The distinct EventGroups the log's life gains carry, and how many gains
      -- there were. The precondition every case below rests on, asserted rather
      -- than assumed: were the seats' gains not one group, "once for the table"
      -- would be proving nothing about CR 608.2f.
      gainsIn gs =
        Maybe.mapMaybe
          ( \logged -> case LoggedEvent.event logged of
              GameEvent.LifeGained _ -> Just (LoggedEvent.group logged)
              _ -> Nothing
          )
          (Foldable.toList (GameState.events gs))
      -- Settle and resolve until the stack is empty: the Peacemaker's entry
      -- trigger gains the life, and the Vigil's own trigger only reaches the
      -- stack at the CR 117.5 scan after it. Resolving the top alone would leave
      -- the card undrawn.
      resolveEverything gs =
        let settled = S.runPure S.identityAnswer gs Engine.settleForPriority
         in if null (GameState.stack settled)
              then settled
              else resolveEverything (S.runPure S.identityAnswer settled Stack.resolveTop)
      -- alice's Vigil already on the battlefield and `gainer` entering with its
      -- CR 603.6a trigger settled onto the stack. Six Plains in her library, so
      -- the Vigil can draw four times over without CR 104.3c deciding a case for
      -- it: a board that could not answer a per-seat reading's four cards would
      -- pass the batch assertion for the wrong reason.
      vigilBoard gainer base = do
        plains <- S.printingOf s registry "Plains"
        vigil <- S.printingOf s registry "Synthetic Communal Vigil"
        entering <- S.printingOf s registry gainer
        let stocked = foldr (\_ g -> snd (S.addLibraryCard plains S.alice g)) base [1 .. 6 :: Int]
            withVigil = snd (S.addCreature vigil S.alice stocked)
            (_, entered) = S.entersWithTrigger entering S.alice withVigil
        pure (snd (Engine.runGamePure S.identityAnswer entered Engine.settleForPriority))
   in Spec.describe s "CR 603.2c Synthetic Communal Vigil" $ do
        -- The proving case. Three seats gain 4 apiece out of one resolution, and
        -- alice draws ONE card -- 3 is the seat-at-a-time reading and 0 is
        -- silence.
        Spec.it s "CR 608.2f three seats gaining at once are one event, so the Vigil draws one card" $ do
          board <- vigilBoard "Centaur Peacemaker" S.threePlayerGame
          let after = resolveEverything board
          Spec.assertEqWith s "alice drew one card for the whole batch" (S.handSize S.alice after) 1
          Spec.assertEqWith s "alice held nothing before" (S.handSize S.alice board) 0
          Spec.assertEqWith s "all three seats really gained their 4" (fmap (\pid -> S.lifeOf pid after) [S.alice, S.bob, S.carol]) [Just 24, Just 24, Just 24]
          Spec.assertEqWith s "and the three gains were one event group" (length (List.nub (gainsIn after))) 1
          Spec.assertEqWith s "three gains, not one" (length (gainsIn after)) 3
        -- The other half of the pair, differing in exactly one thing -- how many
        -- gains the batch holds. FOUR seats and still one card, where a
        -- per-occurrence reading now draws four.
        Spec.it s "CR 603.2c a fourth seat in the batch is not a fourth trigger" $ do
          board <- vigilBoard "Centaur Peacemaker" S.fourPlayerGame
          let after = resolveEverything board
          Spec.assertEqWith s "alice still drew exactly one" (S.handSize S.alice after) 1
          Spec.assertEqWith s "dave gained his 4 too" (S.lifeOf S.dave after) (Just 24)
          Spec.assertEqWith s "four gains in one event group" (length (gainsIn after), length (List.nub (gainsIn after))) (4, 1)
        -- The plumbing control: ONE gain, from Radiant Fountain's "you gain 2
        -- life", draws one card too -- the reading both implementations share.
        -- Without it a Vigil that fired on nothing at all would read as a passing
        -- 1 above.
        Spec.it s "CR 119.3 a lone gain draws one card as well" $ do
          board <- vigilBoard "Radiant Fountain" S.threePlayerGame
          let after = resolveEverything board
          Spec.assertEqWith s "alice drew her card" (S.handSize S.alice after) 1
          Spec.assertEqWith s "and she is the only seat that gained" (fmap (\pid -> S.lifeOf pid after) [S.alice, S.bob, S.carol]) [Just 22, Just 20, Just 20]
          Spec.assertEqWith s "one gain, one group" (length (gainsIn after), length (List.nub (gainsIn after))) (1, 1)
        -- The control that separates "once per event GROUP" from a dedup coarser
        -- than the group -- once ever, or once per turn -- which the boards above
        -- cannot tell apart, each holding one batch. A second Peacemaker enters,
        -- every seat gains again, and that second batch is an event group of its
        -- own: TWO cards, where a coarser dedup leaves the first card alone.
        --
        -- S.entersWithTrigger REWRITES the log, so the group count below reads
        -- the second batch alone rather than both.
        Spec.it s "CR 603.2c a second batch of gains is a second trigger event" $ do
          peacemaker <- S.printingOf s registry "Centaur Peacemaker"
          board <- vigilBoard "Centaur Peacemaker" S.threePlayerGame
          let after = resolveEverything board
              again = resolveEverything (snd (S.entersWithTrigger peacemaker S.alice after))
          Spec.assertEqWith s "alice drew a second card for the second batch" (S.handSize S.alice again) 2
          Spec.assertEqWith s "she held one after the first" (S.handSize S.alice after) 1
          Spec.assertEqWith s "every seat is 8 up over the two batches" (fmap (\pid -> S.lifeOf pid again) [S.alice, S.bob, S.carol]) [Just 28, Just 28, Just 28]
          Spec.assertEqWith s "and the second batch was one event group" (length (List.nub (gainsIn again))) 1

-- Only `attacker` attacks in the double-block case below, so S.aggressiveAnswer's
-- blockers all land on it (CR 509.1), and its CR 510.1c division puts one damage
-- on each blocker: two recipients, one source, one combat damage step. Both
-- halves are PINNED off what the prompt offered rather than built, so a mutation
-- cannot be repaired by an answerer that goes looking for a legal division.
lifelinkDivision :: ObjectId.ObjectId -> Prompt.Prompt r -> r
lifelinkDivision attacker p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
  Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (fmap (\r -> (r, 1)) (filter S.isCreatureRecipient (Map.keys thresholds)))
  _ -> S.aggressiveAnswer p

-- CR 702.15e against the CR 510.2 bracket: two lifelink attackers connecting in
-- one combat damage step deal ONE damage event and cause TWO life gain events,
-- so a batch reader of the gains fires twice in the same step where a batch
-- reader of the damage fires once. Pawl.Engine.Damage.dealWave brackets the
-- wave as one Pawl.Types.EventGroup, and Pawl.Engine.Damage.recordLifelinkGains
-- lands the gains AFTER the bracket closes, one group apiece; a bracket wide
-- enough to take the gains in fused them, which is what the step read as for a
-- while (related to #2814).
--
-- The other direction is the third case: ONE lifelink source dealing damage to
-- two recipients at once is one life gain event, since CR 702.15e separates the
-- events of multiple SOURCES and CR 119.9 hangs the trigger off the source
-- causing the gain. Pawl.Engine.Damage.recordLifelinkGains sums a source's
-- damage before it records, and a double-blocked Child of Night dividing its
-- power is the board that tells the two readings apart.
--
-- Synthetic Communal Vigil is the batch reader, and communalVigilSpec above says
-- why a synthetic. Child of Night {1}{B} Creature -- Vampire 2/1, lifelink, is
-- the source, twice over. Every life total is the same under either reading, so
-- what the cases read is how many CARDS the Vigil drew. Ajani's Pridemate stands
-- beside it as the per-occurrence control: "whenever you gain life" is CR 119.9's
-- per-source reading and takes a counter per gain under either grouping.
lifelinkGainEventsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lifelinkGainEventsSpec s registry =
  let groupsOf pick gs =
        Maybe.mapMaybe
          (\logged -> if pick (LoggedEvent.event logged) then Just (LoggedEvent.group logged) else Nothing)
          (Foldable.toList (GameState.events gs))
      gainGroups =
        groupsOf
          ( \event -> case event of
              GameEvent.LifeGained _ -> True
              _ -> False
          )
      combatDamageGroups =
        groupsOf
          ( \event -> case event of
              GameEvent.DamageDealt ev -> DamageEvent.kind ev == DamageKind.Combat
              _ -> False
          )
      combatDamageBy src gs =
        Maybe.mapMaybe
          ( \logged -> case LoggedEvent.event logged of
              GameEvent.DamageDealt ev
                | DamageEvent.source ev == src && DamageEvent.kind ev == DamageKind.Combat ->
                    Just (DamageEvent.target ev, DamageEvent.amount ev)
              _ -> Nothing
          )
          (Foldable.toList (GameState.events gs))
      gainAmounts gs =
        Maybe.mapMaybe
          ( \logged -> case LoggedEvent.event logged of
              GameEvent.LifeGained change -> Just (LifeChange.amount change)
              _ -> Nothing
          )
          (Foldable.toList (GameState.events gs))
      -- alice attacks with `mine` into an empty board, the Vigil and the
      -- Pridemate out and six Plains in her library, so the Vigil can draw for
      -- every reading without CR 104.3c deciding a case for it. The Pridemate
      -- attacks too under S.aggressiveAnswer and has no lifelink, so it adds two
      -- to bob's loss and nothing to the gains.
      board mine = do
        ours <- mapM (S.printingOf s registry) mine
        plains <- S.printingOf s registry "Plains"
        vigil <- S.printingOf s registry "Synthetic Communal Vigil"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        let (gs, _, _) = S.combatBoardOf ours []
            stocked = foldr (\_ g -> snd (S.addLibraryCard plains S.alice g)) gs [1 .. 6 :: Int]
            withVigil = snd (S.addCreature vigil S.alice stocked)
            (mate, staged) = S.addCreature pridemate S.alice withVigil
        pure (mate, S.runCombat S.aggressiveAnswer staged)
      -- The same board with two Goblin Pikers, {1}{R} 2/1, in the way: they both
      -- block the one Child, which divides its 2 power one apiece -- CR 510.1c's
      -- division is free. A Piker's 1 toughness makes each half lethal, so both
      -- leave the battlefield and the case reads the damage EVENTS rather than
      -- the marks they would otherwise carry.
      doubleBlockBoard = do
        child <- S.printingOf s registry "Child of Night"
        piker <- S.printingOf s registry "Goblin Piker"
        plains <- S.printingOf s registry "Plains"
        vigil <- S.printingOf s registry "Synthetic Communal Vigil"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        let (gs, ours, _) = S.combatBoardOf [child] [piker, piker]
            stocked = foldr (\_ g -> snd (S.addLibraryCard plains S.alice g)) gs [1 .. 6 :: Int]
            withVigil = snd (S.addCreature vigil S.alice stocked)
            (mate, staged) = S.addCreature pridemate S.alice withVigil
        pure (mate, ours, staged)
   in Spec.describe s "CR 702.15e lifelink gains beside the combat damage bracket" $ do
        -- The proving case: two lifelink sources connect at once, and the Vigil
        -- draws TWO -- 1 is the fused reading, 0 is silence.
        Spec.it s "CR 702.15e two lifelink attackers connecting at once are two life gain events" $ do
          (mate, after) <- board ["Child of Night", "Child of Night"]
          Spec.assertEqWith s "the Vigil drew once per lifelink source" (S.handSize S.alice after) 2
          Spec.assertEqWith s "alice gained 2 twice" (S.lifeOf S.alice after) (Just 24)
          Spec.assertEqWith s "bob took all three attackers" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "CR 702.15e: two gains, two event groups" (length (gainGroups after), length (List.nub (gainGroups after))) (2, 2)
          Spec.assertEqWith s "CR 510.2: the three damage events stayed one event group" (length (List.nub (combatDamageGroups after))) 1
          Spec.assertEqWith s "and the Pridemate took a counter per gain" (S.counterOf CounterKind.PlusOnePlusOne mate after) 2
        -- The board that differs in one lifelink source: one gain is one event
        -- under either reading, so this is the floor rather than a discrimination.
        Spec.it s "CR 119.9 one lifelink attacker connecting is one life gain event" $ do
          (mate, after) <- board ["Child of Night"]
          Spec.assertEqWith s "the Vigil drew once" (S.handSize S.alice after) 1
          Spec.assertEqWith s "alice gained 2" (S.lifeOf S.alice after) (Just 22)
          Spec.assertEqWith s "bob took both attackers" (S.lifeOf S.bob after) (Just 16)
          Spec.assertEqWith s "one gain, one group" (length (gainGroups after), length (List.nub (gainGroups after))) (1, 1)
          Spec.assertEqWith s "and the Pridemate took one counter" (S.counterOf CounterKind.PlusOnePlusOne mate after) 1
        -- The other direction, and the discriminating case: ONE source, two
        -- recipients at once. Two records of 1 is the per-recipient reading.
        Spec.it s "CR 119.9 one lifelink source damaging two blockers at once is one life gain event" $ do
          (mate, ours, staged) <- doubleBlockBoard
          case ours of
            [child] -> do
              let after = S.runCombat (lifelinkDivision child) staged
              Spec.assertEqWith s "the Pridemate took one counter for the one gain" (S.counterOf CounterKind.PlusOnePlusOne mate after) 1
              Spec.assertEqWith s "the Vigil drew once" (S.handSize S.alice after) 1
              Spec.assertEqWith s "CR 702.15b: one gain, of the source's whole damage" (gainAmounts after) [2]
              Spec.assertEqWith s "one gain, one event group" (length (gainGroups after), length (List.nub (gainGroups after))) (1, 1)
              Spec.assertEqWith s "alice gained 2 all told" (S.lifeOf S.alice after) (Just 22)
              -- The precondition the reading rests on: the Child's 2 power really
              -- did reach two recipients in the one step.
              Spec.assertEqWith s "the Child's 2 power went 1 apiece to two recipients" (length (List.nub (fmap fst (combatDamageBy child after))), fmap snd (combatDamageBy child after)) (2, [1, 1])
              Spec.assertEqWith s "CR 510.2: the damage stayed one event group" (length (List.nub (combatDamageGroups after))) 1
              Spec.assertEqWith s "bob was not damaged" (S.lifeOf S.bob after) (Just 20)
            _ -> Spec.assertFailure s "fixture should have one attacker"

-- CR 603.2c's FIRST sentence on the CR 603.7 DELAYED path, which the two groups
-- above cannot reach between them: Synthetic Communal Vigil proves the batch
-- reading for an object's own triggered ability, and False Cure proves the
-- per-occurrence reading for a delayed entry. Neither asks what a delayed entry
-- whose condition is BATCH-scoped does with a batch, and the answer used to be
-- "fire once per member" -- Event.eventTriggers consulted Event.batchScoped and
-- Event.delayedPending did not; see #2384.
--
--   * Forth Eorlingas! {X}{R}{W} Sorcery (data/cards/forth-eorlingas.json):
--     "Create X 2/2 red Human Knight creature tokens with trample and haste.
--     Whenever one or more creatures you control deal combat damage to one or
--     more players this turn, you become the monarch." Name, cost, type line
--     and Oracle text checked against Scryfall 2026-09-02.
--
-- It replaced Synthetic Communal Reckoning ("until end of turn, whenever one or
-- more players gain life, you lose 3 life") once batched combat damage was a
-- TriggerCondition; see #2940. communalRelapseSpec's synthetic stays: CR
-- 603.7b's second sentence turns on an entry having NO stated duration, and
-- "this turn" is one (#2955).
--
-- The board: alice casts it for X in her precombat main, and the hasty Knights
-- attack bob and connect in one combat damage step -- one Pawl.Types.EventGroup,
-- Pawl.Engine.Damage.dealWave bracketing the step (CR 510.2), which the group
-- count pins as the precondition. The monarch is a designation a player has or
-- lacks (CR 725.1), so crowning alice twice looks exactly like crowning her
-- once -- Monarch.crown records nothing for a player already crowned -- and the
-- COUNT of firings is read off the stack as the step's turn-based action
-- settles, the reading Megrim's group takes above: one trigger for the batch,
-- where a per-member gatherer stacks two.
--
-- "Creatures you control": on alice's own turn no creature she does not control
-- can deal combat damage to a player, so the ControlledBy half of the filter is
-- written as printed and unexercised here.
forthEorlingasSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
forthEorlingasSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      combatDamage = Phase.Combat CombatStep.CombatDamage
      declareAttackers = Phase.Combat CombatStep.DeclareAttackers
      -- CR 601.2b's X, and nothing else pinned: the answer is what makes X
      -- tokens rather than none.
      choosingX :: Natural -> Prompt.Prompt r -> r
      choosingX x p = case p of
        Prompt.ChooseX {} -> x
        _ -> S.identityAnswer p
      -- Attacks with nothing, so a turn's combat passes without the Knights
      -- connecting -- S.identityAnswer attacks with everything.
      passive :: Prompt.Prompt r -> r
      passive p = case p of
        Prompt.DeclareAttackers {} -> []
        _ -> S.identityAnswer p
      knights gs = filter (\oid -> Set.member Subtype.Knight (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- The distinct EventGroups the log's combat damage carries.
      combatDamageGroups gs =
        List.nub
          ( Maybe.mapMaybe
              ( \logged -> case LoggedEvent.event logged of
                  GameEvent.DamageDealt ev | DamageEvent.kind ev == DamageKind.Combat -> Just (LoggedEvent.group logged)
                  _ -> Nothing
              )
              (Foldable.toList (GameState.events gs))
          )
      -- Run whole steps until `done` holds of the board, bounded so a bug cannot
      -- loop forever; wide enough for the two turns the negative below plays.
      runUntil step done gs0 =
        let go n g =
              if n <= (0 :: Int) || done g || Maybe.isJust (GameState.result g)
                then g
                else go (n - 1) (step g)
         in go 40 gs0
      -- alice, in her precombat main with the four lands {2}{R}{W} needs at X=2,
      -- casts the spell for `x` and resolves it. Both libraries are stocked
      -- because the negative below plays through two draw steps (CR 104.3c).
      armed x = do
        mountain <- S.printingOf s registry "Mountain"
        plains <- S.printingOf s registry "Plains"
        spell <- S.printingOf s registry "Forth Eorlingas!"
        let lands = S.landsFor plains S.alice 2 (S.landsFor mountain S.alice 2 (Setup.emptyGame S.bothPlayers))
            stocked = List.foldl' (\g pid -> snd (S.addLibraryCard plains pid g)) lands (concat (replicate 4 [S.alice, S.bob]))
            (spellId, withSpell) = S.addHandCard spell S.alice stocked
            gs =
              withSpell
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice,
                  GameState.remaining = Seq.drop 1 (Seq.dropWhileL (/= Phase.PrecombatMain) (GameState.remaining withSpell))
                }
        pure (resolveAll (snd (Engine.runGamePure (choosingX x) gs (S.cast S.alice spellId))))
      -- From a board sitting at some turn's declare attackers step: the Knights
      -- attack bob, the damage step's turn-based action deals their damage, and
      -- the settle that follows puts whatever triggered onto the stack. Read
      -- there for the count, then resolved for the crown -- one run, read twice.
      connecting gs =
        let atDamage = S.runToStep combatDamage S.aggressiveAnswer gs
            placed = S.runPure S.aggressiveAnswer atDamage (Engine.runTurnBasedActions combatDamage >> Engine.settleForPriority)
         in (placed, resolveAll placed)
   in Spec.describe s "CR 603.2c Forth Eorlingas!" $ do
        Spec.it s "CR 111.3 / 601.2b cast for X=2, it mints two 2/2 red Human Knights with trample and haste and arms one entry" $ do
          gs <- armed 2
          case knights gs of
            tokens@[_, _] -> do
              Spec.assertEqWith s "each is 2/2" (fmap (\oid -> (Projection.powerOf oid gs, Projection.toughnessOf oid gs)) tokens) [(Just 2, Just 2), (Just 2, Just 2)]
              Spec.assertEqWith s "each is red" (fmap (\oid -> Projection.colorsOf oid gs) tokens) [Set.singleton Color.Red, Set.singleton Color.Red]
              Spec.assertBool s (all (\oid -> Set.member Subtype.Human (Projection.subtypesOf oid gs)) tokens) "each is a Human too"
              Spec.assertBool s (all (\oid -> Projection.hasKeyword Keyword.Type.Haste oid gs) tokens) "each has haste"
              Spec.assertBool s (all (\oid -> Projection.hasKeyword Keyword.Type.Trample oid gs) tokens) "each has trample"
              Spec.assertBool s (all (\oid -> Projection.controllerOf oid gs == Just S.alice) tokens) "and alice controls both"
            other -> Spec.assertFailure s ("expected exactly two Knight tokens, got " <> show (length other))
          Spec.assertEqWith s "one delayed ability waiting" (Seq.length (GameState.delayedTriggers gs)) 1
          Spec.assertEqWith s "nobody is the monarch yet" (GameState.monarch gs) Nothing
        -- The proving case. Two Knights connect in ONE combat damage step, which
        -- is one trigger event, so the entry fires ONCE: one trigger on the
        -- stack, and alice is crowned. A gatherer that fired per member stacks
        -- two -- and crowns alice all the same, which is why the count is read
        -- before the stack resolves.
        Spec.it s "CR 603.2c two Knights connecting in one step are one trigger event, so the entry fires once" $ do
          gs <- armed 2
          let (placed, after) = connecting (runUntil (\g -> S.runPure S.identityAnswer g Engine.runStep) ((== declareAttackers) . GameState.phase) gs)
          Spec.assertEqWith s "alice became the monarch" (GameState.monarch after) (Just S.alice)
          Spec.assertEqWith s "one trigger on the stack for the batch, not one per Knight" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "both Knights connected, for four" (S.lifeOf S.bob after) (Just 16)
          Spec.assertEqWith s "CR 510.2: the two damage events were one event group" (length (combatDamageGroups placed)) 1
          Spec.assertEqWith s "and CR 603.7b's stated duration kept the entry armed" (Seq.length (GameState.delayedTriggers after)) 1
        -- The floor the two readings share: one Knight is one occurrence either
        -- way, so a fixed count of one passes here and fails above.
        Spec.it s "CR 603.2c a lone Knight connecting is one trigger event as well" $ do
          gs <- armed 1
          let (placed, after) = connecting (runUntil (\g -> S.runPure S.identityAnswer g Engine.runStep) ((== declareAttackers) . GameState.phase) gs)
          Spec.assertEqWith s "alice became the monarch" (GameState.monarch after) (Just S.alice)
          Spec.assertEqWith s "one trigger on the stack" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "the one Knight connected, for two" (S.lifeOf S.bob after) (Just 18)
        -- The control that separates "once per event GROUP" from a collapse
        -- coarser than the group -- once per scan, the wrong-direction fix the
        -- boards above cannot tell apart, each holding one group. TWO combat
        -- damage groups reach ONE settle, and each is its own trigger event: two
        -- triggers. A per-scan collapse stacks one.
        --
        -- The log is written directly, S.withEvents giving each event its own
        -- group, because every funnel that deals a second batch also settles
        -- between them -- and then the two groups would be two scans, which is
        -- not the reading under test.
        Spec.it s "CR 704.3 two combat damage groups in one scan are two trigger events" $ do
          gs <- armed 2
          let hit knight = GameEvent.DamageDealt (DamageEvent.MkDamageEvent knight (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat)
              staged = S.withEvents (fmap hit (knights gs)) gs
              placed = S.runPure S.identityAnswer staged Engine.settleForPriority
          Spec.assertEqWith s "one trigger per group, so two" (length (GameState.stack placed)) 2
          Spec.assertEqWith s "two damage events, in two event groups" (length (combatDamageGroups staged)) 2
        -- "This turn", differing from the proving case in exactly one thing:
        -- which turn's combat the Knights attack in. The turn that armed the
        -- entry passes with no attack, bob takes his, and on alice's next turn
        -- the same two Knights connect for the same four -- and crown nobody,
        -- CR 514.2's cleanup having ended the entry's duration.
        Spec.it s "CR 603.7b / 514.2 the same Knights connecting on alice's next turn crown nobody" $ do
          gs <- armed 2
          let later = runUntil (\g -> S.runPure passive g Engine.runStep) (\g -> GameState.turnNumber g == 3 && GameState.phase g == declareAttackers) gs
              (placed, after) = connecting later
          Spec.assertEqWith s "nobody became the monarch" (GameState.monarch after) Nothing
          Spec.assertEqWith s "nothing triggered off the batch" (length (GameState.stack placed)) 0
          Spec.assertEqWith s "though both Knights connected, for four" (S.lifeOf S.bob after) (Just 16)
          Spec.assertEqWith s "it is alice's next turn" (GameState.turnNumber later, GameState.activePlayer later) (3, S.alice)
          Spec.assertEqWith s "the entry is gone, not masked" (Seq.length (GameState.delayedTriggers later)) 0
          Spec.assertEqWith s "bob took no damage before it" (S.lifeOf S.bob later) (Just 20)

-- CR 603.7b's SECOND sentence read the other way round: "if its trigger event
-- occurs MORE THAN ONCE simultaneously". A batch-scoped condition's trigger
-- event is the whole batch, which occurred ONCE, so there is nothing to choose
-- and the question must not be asked -- an engine that asks it invents a
-- decision the rules do not authorise, which is the elision bar design.md sets.
--
-- The observable is the PROMPT and nothing else, which is why this is its own
-- group. Walk the unfixed engine to the end: it raises the question, the answer
-- names one of the batch's members, and Event.eventBindings then binds NOTHING
-- off that member -- eventBindingSlots gives a batch condition no slots. So the
-- pending trigger, the life totals and the cards drawn are identical whichever
-- member was named, and singularCureSpec's `choosingGain` trick cannot separate
-- the seats here. Counting the questions is the only reading left.
--
-- BOTH LEGS, so the count cannot pass by the answerer never being reached: the
-- batch entry (Synthetic Communal Relapse) is asked NOTHING on the very board
-- where the per-occurrence entry (Synthetic Singular Cure) is asked once.
communalRelapseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
communalRelapseSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.ChooseDelayedTriggerEvent {} -> do
          State.modify (+ 1)
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
      -- falseCureSpec's Peacemaker board -- alice casts the named Instant off
      -- two Swamps with a Centaur Peacemaker already placed, waiting to enter --
      -- but run through Engine.runGame with a State answerer rather than the
      -- pure one, so the questions can be counted as the Peacemaker's entry
      -- trigger resolves and the delayed entry is gathered.
      staged name base = do
        swamp <- S.printingOf s registry "Swamp"
        spell <- S.printingOf s registry name
        peacemaker <- S.printingOf s registry "Centaur Peacemaker"
        let lands = S.landsFor swamp S.alice 2 base
            (peacemakerId, withPeacemaker) = S.addCreature peacemaker S.alice lands
            (spellId, withSpell) = S.addHandCard spell S.alice withPeacemaker
            gs = resolveAll (snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId)))
            moved = ZoneChange.MkZoneChange peacemakerId peacemakerId Zone.Stack Zone.Battlefield
        pure (S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project peacemakerId gs))] gs)
      played gs = State.runState (Engine.runGame counting gs (Engine.settleForPriority >> Engine.priorityLoop)) 0
      -- How many times the question was asked, and the board it was asked on --
      -- one run, read twice, so the count and the totals cannot come from
      -- different games.
      asks gs = snd (played gs)
      after gs = snd (fst (played gs))
   in Spec.describe s "CR 603.7b Synthetic Communal Relapse" $ do
        -- The proving case. Three seats gain 4 in one event group; the batch
        -- occurred once, so no question is raised and the entry fires on it.
        -- An engine that gathers per member asks alice which of the three gains
        -- triggered her ability.
        Spec.it s "CR 603.7b a batch occurred once, so its controller is asked nothing" $ do
          gs <- staged "Synthetic Communal Relapse" S.threePlayerGame
          Spec.assertEqWith s "no question is raised" (asks gs) 0
          Spec.assertEqWith s "and the entry fired once, alice paying 3 off her own 4" (S.lifeOf S.alice (after gs)) (Just 21)
          Spec.assertEqWith s "the entry is spent, having no stated duration" (Seq.length (GameState.delayedTriggers (after gs))) 0
        -- The other leg, differing in exactly one thing -- the entry's CONDITION.
        -- Synthetic Singular Cure watches "a player gains life" per occurrence, so
        -- on this same batch its trigger event occurred three times and CR 603.7b's
        -- second sentence applies: one question. Without this leg a count of zero
        -- above would pass on a board that never reached the answerer at all.
        Spec.it s "CR 603.7b a per-occurrence entry on the same batch is asked once" $ do
          gs <- staged "Synthetic Singular Cure" S.threePlayerGame
          Spec.assertEqWith s "exactly one question" (asks gs) 1
        -- A FOURTH seat, so the batch is bigger and the count still zero. The
        -- unfixed engine asked here too -- one question over four candidates
        -- rather than three -- so the pair is not a coincidence of size.
        Spec.it s "CR 603.7b a fourth seat in the batch is still not a question" $ do
          gs <- staged "Synthetic Communal Relapse" S.fourPlayerGame
          Spec.assertEqWith s "still no question" (asks gs) 0
          Spec.assertEqWith s "and dave really gained his 4" (S.lifeOf S.dave (after gs)) (Just 24)

-- CR 120.3's event read by its RECIPIENT, which no condition could ask for
-- before: every damage arm beside this one watches a permanent DEALING damage.
--
-- Two producers. Ripjaw Raptor, {2}{G}{G} Creature -- Dinosaur 4/5, whose whole
-- text is "Enrage -- Whenever this creature is dealt damage, draw a card", reads
-- no part of the event; Coalhauler Swine, further down, reads its amount. Enrage
-- is an ability word (CR 207.2c) with no rules meaning, so the condition is
-- ordinary and nothing about it reaches Pawl.Types.Keyword.
--
-- What the group has to separate, a board or a pair of boards each:
--
--   * the RECIPIENT, not the damager and not any permanent -- a Hill Giant
--     beside the Raptor, under the same controller, takes the same damage and
--     draws nothing.
--   * the DAMAGE KIND, which this arm does not filter on where its neighbours all
--     do: a noncombat event and a CR 510.2 combat event each draw one.
--   * CR 510.2's simultaneity, which is the reading a naive once-per-batch arm
--     gets wrong: two blockers deal two events and draw TWO cards.
--   * the AMOUNT the event carried, which the Swine's payload reads back as CR
--     120.3's "that much": a pair of boards carrying different numbers, and a
--     second pair separating a per-event read from a per-batch one.
--
-- The observable is asserted BEFORE and AFTER every time -- hand size for the
-- Raptor, life totals for the Swine. "alice holds one card" alone passes on a
-- board she drew for turn on.
enrageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
enrageSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      -- A noncombat event's own path: Damage.applyDamage records the DamageDealt
      -- entries, the settle gathers what they triggered and puts it on the stack
      -- (CR 603.3), and the priority loop resolves it. The narrowest path that
      -- shows the behaviour.
      dealing events gs = resolveAll (settle (S.runPure S.identityAnswer gs (Damage.applyDamage events)))
      -- alice's library is stocked, or CR 104.3c decks her before the assertion
      -- runs.
      stock printing pid n gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      noncombat src target amount = DamageEvent.MkDamageEvent src (Recipient.ToCreature target) amount False False False 0 Nothing DamageKind.Noncombat
      damageOn oid gs = fmap Object.damage (Game.lookupObject oid gs)
   in Spec.describe s "CR 120.3 enrage" $ do
        -- The noncombat half, and the CONTROL as a pair of boards differing in
        -- exactly one thing: which of alice's two creatures the event's recipient
        -- names. bob's Goblin Piker is the source either way, and the Hill Giant's
        -- toughness is 3 so that the control board's creature survives to be
        -- asked about.
        Spec.it s "CR 120.3 a noncombat event on the Raptor draws exactly one card, and one on another creature draws none" $ do
          raptor <- S.printingOf s registry "Ripjaw Raptor"
          giant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, g1) = S.addCreature raptor S.alice (Setup.emptyGame S.bothPlayers)
              (mineId, g2) = S.addCreature giant S.alice g1
              (theirsId, g3) = S.addCreature piker S.bob g2
              gs = stock piker S.alice 5 g3
              atRaptor = dealing [noncombat theirsId raptorId 1] gs
              atGiant = dealing [noncombat theirsId mineId 1] gs
          Spec.assertEqWith s "alice starts with an empty hand" (S.handSize S.alice gs) 0
          Spec.assertEqWith s "the Raptor's own event drew her one" (S.handSize S.alice atRaptor) 1
          Spec.assertEqWith s "and the damage really landed on the Raptor" (damageOn raptorId atRaptor) (Just 1)
          Spec.assertEqWith s "the same event on her Hill Giant drew nothing" (S.handSize S.alice atGiant) 0
          Spec.assertEqWith s "though that damage landed too" (damageOn mineId atGiant) (Just 1)
        -- CR 510.2's combat damage, through the whole declare-block-deal sequence.
        -- ONE blocker, so exactly one event reaches the Raptor -- which is what
        -- proves the arm does not filter on DamageKind, the noncombat case above
        -- being the other half of that pair.
        Spec.it s "CR 510.2 combat damage fires it too: one blocker, one card" $ do
          raptor <- S.printingOf s registry "Ripjaw Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (gs0, mine, _) = S.combatBoardOf [raptor] [piker]
              gs = stock piker S.alice 5 gs0
          case mine of
            [] -> Spec.assertFailure s "fixture should have given alice a Ripjaw Raptor"
            raptorId : _ -> do
              let after = resolveAll (S.fightWith S.aggressiveAnswer gs)
              Spec.assertEqWith s "alice started with an empty hand" (S.handSize S.alice gs) 0
              Spec.assertEqWith s "and drew exactly one" (S.handSize S.alice after) 1
              Spec.assertEqWith s "the one blocker's 2 is marked on the Raptor" (damageOn raptorId after) (Just 2)
        -- TWO blockers, so CR 510.2 deals two events simultaneously and
        -- Pawl.Engine.Damage records one DamageDealt each. Two triggers, two cards
        -- -- which is what the Ripjaw Raptor rulings say and what an arm folding the
        -- batch into one firing gets wrong.
        --
        -- The blockers are DIFFERENT printings, 2/1 and 1/1, so the 3 marked on the
        -- Raptor is a number neither event carries alone: a single fabricated event
        -- of the whole batch's damage would be indistinguishable otherwise.
        Spec.it s "CR 510.2 two simultaneous events draw two cards, not one" $ do
          raptor <- S.printingOf s registry "Ripjaw Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
          let (gs0, mine, _) = S.combatBoardOf [raptor] [piker, sorcerer]
              gs = stock piker S.alice 5 gs0
          case mine of
            [] -> Spec.assertFailure s "fixture should have given alice a Ripjaw Raptor"
            raptorId : _ -> do
              let after = resolveAll (S.fightWith S.aggressiveAnswer gs)
              Spec.assertEqWith s "alice started with an empty hand" (S.handSize S.alice gs) 0
              Spec.assertEqWith s "one card per event, so two" (S.handSize S.alice after) 2
              Spec.assertEqWith s "and both events' damage is marked" (damageOn raptorId after) (Just 3)
        -- CR 603.2's amount, the half of CR 120.3's event no payload could read
        -- before. Coalhauler Swine, {4}{R}{R} Creature -- Boar Beast 4/4, whose
        -- whole text is "Whenever this creature is dealt damage, it deals that
        -- much damage to each player."
        --
        -- A PAIR OF BOARDS differing in exactly one thing -- the number the event
        -- carries -- because "each player lost 3" alone is passed by a constant
        -- binding of 3 as happily as by reading the event.
        --
        -- Neither amount coincides with a characteristic a wrong binding could
        -- reach: 3 and 5 are neither the Swine's power nor its toughness (4) nor
        -- the Goblin Piker damager's power (2), so a binding reading a
        -- characteristic instead of the event fails both boards rather than
        -- passing one by luck.
        --
        -- THREE SEATS, because "each player" over two collapses onto "you and an
        -- opponent"; carol blocks the payload that hit only the combatants.
        --
        -- TOUGHNESS 4 is load-bearing: 3 is not lethal (CR 704.5g), so the Swine
        -- is still on the battlefield when its own trigger resolves and the
        -- assertion says nothing about CR 608.2h. The 5-damage board IS lethal, and
        -- is asserted on life totals only -- the number the event carried, which
        -- the binding captured at CR 603.2 rather than at resolution.
        Spec.it s "CR 120.3 the payload reads the amount the event carried" $ do
          swine <- S.printingOf s registry "Coalhauler Swine"
          piker <- S.printingOf s registry "Goblin Piker"
          brigade <- S.printingOf s registry "Foriysian Brigade"
          let (swineId, g1) = S.addCreature swine S.alice S.threePlayerGame
              (mineId, g2) = S.addCreature brigade S.alice g1
              (theirsId, gs) = S.addCreature piker S.bob g2
              lives g = (S.lifeOf S.alice g, S.lifeOf S.bob g, S.lifeOf S.carol g)
              threeAt = dealing [noncombat theirsId swineId 3] gs
              fiveAt = dealing [noncombat theirsId swineId 5] gs
              atOther = dealing [noncombat theirsId mineId 3] gs
          Spec.assertEqWith s "all three seats start at 20" (lives gs) (Just 20, Just 20, Just 20)
          Spec.assertEqWith s "CR 120.3a: a 3-damage event costs every player 3" (lives threeAt) (Just 17, Just 17, Just 17)
          Spec.assertEqWith s "and the 3 really landed on the Swine (CR 120.3e)" (damageOn swineId threeAt) (Just 3)
          Spec.assertEqWith s "the same board with a 5-damage event costs every player 5" (lives fiveAt) (Just 15, Just 15, Just 15)
          -- The RECIPIENT control, on the same board and the same amount: the
          -- event pointed at alice's Foriysian Brigade instead, so the life totals
          -- above are this trigger and not damage-adjacent bookkeeping. Its
          -- toughness is 4 too, so it survives to be asked about.
          Spec.assertEqWith s "an event on another of her creatures moves no life total" (lives atOther) (Just 20, Just 20, Just 20)
          Spec.assertEqWith s "though that damage landed too (CR 120.3e)" (damageOn mineId atOther) (Just 3)
        -- CR 510.2 again, now on the AMOUNT rather than on the firing count: two
        -- blockers of 2 and 1 deal two events, and each trigger reads its OWN
        -- event, so the players lose 2 and then 1. The readings this separates,
        -- both of which give 3 per firing and so 6 in total, are "the damage
        -- marked on the recipient" (CR 120.3e has recorded all 3 before either
        -- trigger resolves) and "the batch's total".
        --
        -- Two seats suffice here -- the "each player" reach is the board above's
        -- job, and this one is about which number each of two firings reads.
        Spec.it s "CR 510.2 two simultaneous events are read one at a time, not summed" $ do
          swine <- S.printingOf s registry "Coalhauler Swine"
          piker <- S.printingOf s registry "Goblin Piker"
          sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
          let (gs, mine, _) = S.combatBoardOf [swine] [piker, sorcerer]
          case mine of
            [] -> Spec.assertFailure s "fixture should have given alice a Coalhauler Swine"
            swineId : _ -> do
              let after = resolveAll (S.fightWith S.aggressiveAnswer gs)
              Spec.assertEqWith s "both players started at 20" (S.lifeOf S.alice gs, S.lifeOf S.bob gs) (Just 20, Just 20)
              Spec.assertEqWith s "2 then 1, so 3 each -- not 3 twice" (S.lifeOf S.alice after, S.lifeOf S.bob after) (Just 17, Just 17)
              Spec.assertEqWith s "with both blockers' damage marked on the Swine" (damageOn swineId after) (Just 3)

-- CR 120.1's SOURCE of the damage, the half of the same event enrage above does
-- not read: Belltower Sphinx, {4}{U} Creature -- Sphinx 2/5, "Flying. Whenever a
-- source deals damage to this creature, that source's controller mills that many
-- cards." Its payload is PlayerRef.ControllerOfBound over the slot
-- Pawl.Engine.Event.Binding.eventBindings stamps from DamageEvent.source.
--
-- NONCOMBAT events throughout, which is the point: the slot is spelled
-- `combatDamager` for CR 510.2's sake, and a stamp that filtered on DamageKind
-- would leave every board here unmilled.
--
-- THREE SEATS, because "that source's controller" over two collapses onto "the
-- opponent" -- and the pair of boards below is what separates them: the same
-- Sphinx is hit by bob's Piker on one and carol's on the other, so a payload
-- reading a fixed relation mills the wrong seat rather than no seat at all.
--
-- The amounts are 3 and 6, neither of which is the Sphinx's power (2) or
-- toughness (5) nor the Goblin Piker damager's power (2), so a binding reading a
-- characteristic fails both boards rather than passing one by luck. 6 is lethal
-- (CR 704.5g) and the Sphinx dies, which the mill does not care about: CR 603.2
-- captured the amount when the ability triggered.
belltowerSphinxSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
belltowerSphinxSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      dealing events gs = resolveAll (settle (S.runPure S.identityAnswer gs (Damage.applyDamage events)))
      -- Every library is stocked past the largest mill, or the mill runs out of
      -- cards and both readings of the rule produce the same graveyard.
      stock printing pid n gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      noncombat src target amount = DamageEvent.MkDamageEvent src (Recipient.ToCreature target) amount False False False 0 Nothing DamageKind.Noncombat
      graveyardSize pid gs = length (Game.zoneMembers Zone.Graveyard pid gs)
      graves g = (graveyardSize S.alice g, graveyardSize S.bob g, graveyardSize S.carol g)
      damageOn oid gs = fmap Object.damage (Game.lookupObject oid gs)
      -- One board for both cases: alice's Sphinx, and an IDENTICAL Goblin Piker
      -- under each of the two opponents, so the pair below differs in the damager
      -- alone.
      board sphinx piker =
        let (sphinxId, g1) = S.addCreature sphinx S.alice S.threePlayerGame
            (bobsId, g2) = S.addCreature piker S.bob g1
            (carolsId, g3) = S.addCreature piker S.carol g2
         in (sphinxId, bobsId, carolsId, stock piker S.alice 9 (stock piker S.bob 9 (stock piker S.carol 9 g3)))
   in Spec.describe s "CR 120.1 that source's controller" $ do
        Spec.it s "CR 120.1 the damager's controller mills, and the count is the event's amount" $ do
          sphinx <- S.printingOf s registry "Belltower Sphinx"
          piker <- S.printingOf s registry "Goblin Piker"
          let (sphinxId, bobsId, _, gs) = board sphinx piker
              byBobThree = dealing [noncombat bobsId sphinxId 3] gs
              byBobSix = dealing [noncombat bobsId sphinxId 6] gs
          Spec.assertEqWith s "every graveyard starts empty" (graves gs) (0, 0, 0)
          Spec.assertEqWith s "bob's Piker deals 3, so bob mills 3 and nobody else mills" (graves byBobThree) (0, 3, 0)
          -- alice's 1 is the dead Sphinx itself (CR 704.5g), not a mill.
          Spec.assertEqWith s "the same board with a 6-damage event mills 6" (graves byBobSix) (1, 6, 0)
          Spec.assertEqWith s "and the damage really landed on the Sphinx" (damageOn sphinxId byBobThree) (Just 3)
          Spec.assertEqWith s "6 was lethal, so the Sphinx is gone -- CR 603.2 had already captured the amount" (damageOn sphinxId byBobSix) Nothing
        -- The SEAT, as a pair of boards differing in exactly one thing: which
        -- opponent's Piker dealt the damage. A payload reading a fixed relation --
        -- PlayerRef.Relative Opponent, or the bearer's controller -- mills the same
        -- seat on both, which two seats could not have told apart.
        Spec.it s "CR 120.1 the seat is the damager's controller, not a fixed relation" $ do
          sphinx <- S.printingOf s registry "Belltower Sphinx"
          piker <- S.printingOf s registry "Goblin Piker"
          let (sphinxId, bobsId, carolsId, gs) = board sphinx piker
              byBob = dealing [noncombat bobsId sphinxId 3] gs
              byCarol = dealing [noncombat carolsId sphinxId 3] gs
          Spec.assertEqWith s "every graveyard starts empty" (graves gs) (0, 0, 0)
          Spec.assertEqWith s "bob's Piker deals it, so bob mills" (graves byBob) (0, 3, 0)
          Spec.assertEqWith s "carol's identical Piker deals the same 3, and now it is CAROL who mills" (graves byCarol) (0, 0, 3)
          Spec.assertEqWith s "the same damage landed on the Sphinx either way" (fmap (damageOn sphinxId) [byBob, byCarol]) [Just 3, Just 3]

-- The life-GAIN group's mirror: "whenever an opponent loses life", which the
-- rules give no CR 119.9 of its own. What counts as a loss is therefore fixed by
-- the three sites that RECORD one, and this group walks all three:
--
--   * CR 119.3, an effect that causes a player to lose life -- Sign in Blood's
--     "target player draws two cards and loses 2 life".
--   * CR 119.2 / 120.3a, damage dealt to a player by a source without infect --
--     Hill Giant's three.
--   * CR 119.4, life paid as a cost: "in other words, the player loses that much
--     life" -- Greed's "{B}, Pay 2 life: Draw a card".
--
-- Exquisite Blood, {4}{B} Enchantment, "Whenever an opponent loses life, you gain
-- that much life", is the card that proves it, and the first LIFE condition in the
-- pool whose relation is Opponent rather than You (Megrim's discard trigger is the
-- other one) -- so the loser and CR 109.5's "you" are never the same player, and a
-- matcher that ignored the relation would gain alice life off her own losses.
--
-- What makes the group a proof rather than a demonstration:
--
--   * the amounts DIFFER between the damage case (3) and the two others (2), and
--     no life total on any of these boards is 3 or 2 -- so a constant binding
--     fails one case and a total-reading binding fails all of them.
--   * the CR 109.5 control changes only WHO lost the life, on the same card, the
--     same spell and the same amount.
--   * the CR 120.3b control is on the SAME board and the SAME attack as the
--     damage case: Glistener Elf's infect damage gives poison counters INSTEAD of
--     causing life loss, so alice gains the Hill Giant's 3 and not the Elf's 1.
lifeLossTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lifeLossTriggerSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- Sign in Blood's one target slot, answered with `who` rather than left to
      -- identityAnswer's lowest-sorting candidate -- which is alice, and so is
      -- the control case rather than the positive one.
      aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
        _ -> S.identityAnswer p
      -- alice: two Swamps for the {B}{B}, an Exquisite Blood, and Sign in Blood
      -- in hand.
      --
      -- BOTH players get two library cards, not only the one the positive case
      -- aims at, and this is load-bearing rather than tidy: Sign in Blood draws
      -- its target two cards as well as costing them the life, so a target with
      -- an empty library loses the game to CR 104.3c the next time a player would
      -- get priority -- before any trigger could resolve. The CR 109.5 control
      -- below would then be silent for THAT reason instead of the relation's, and
      -- would pass however the matcher read the relation. With a library on each
      -- side the two cases differ in the target and in nothing else.
      signInBloodBoard swamp blood signInBlood =
        let (_, withBlood) = S.addCreature blood S.alice (S.landsInPlay swamp 2)
            stock pid gs =
              let (_, one) = S.addLibraryCard swamp pid gs
                  (_, two) = S.addLibraryCard swamp pid one
               in two
         in S.handOne signInBlood (stock S.bob (stock S.alice withBlood))
   in Spec.describe s "PlayerLosesLife" $ do
        -- The gameplay-level proof, cast to resolution. bob loses 2 (CR 119.3),
        -- Exquisite Blood matches THAT event and gains alice the 2 it carried.
        Spec.it s "CR 119.3 whole cards: Sign in Blood costs bob 2 life and Exquisite Blood gains alice that much" $ do
          swamp <- S.printingOf s registry "Swamp"
          blood <- S.printingOf s registry "Exquisite Blood"
          signInBlood <- S.printingOf s registry "Sign in Blood"
          let (gs, spellId) = signInBloodBoard swamp blood signInBlood
              cast = snd (Engine.runGamePure (aimAt S.bob) gs (S.cast S.alice spellId))
              settled = resolveAll cast
          Spec.assertEqWith s "bob lost exactly 2" (S.lifeOf S.bob settled) (Just 18)
          Spec.assertEqWith s "and alice gained exactly that much" (S.lifeOf S.alice settled) (Just 22)
        -- The control twin, differing in ONE thing: the spell targets ALICE, so
        -- alice is the one who loses. The same card, the same 2 life, the same
        -- GameEvent.LifeLost written -- and "an opponent" is bob, so Exquisite
        -- Blood stays silent.
        --
        -- alice's loss is asserted too, or the case would pass for the wrong
        -- reason: an engine that recorded no loss at all would also show no gain.
        Spec.it s "CR 109.5/603.3a the control: ALICE loses the life, and her own Exquisite Blood stays silent" $ do
          swamp <- S.printingOf s registry "Swamp"
          blood <- S.printingOf s registry "Exquisite Blood"
          signInBlood <- S.printingOf s registry "Sign in Blood"
          let (gs, spellId) = signInBloodBoard swamp blood signInBlood
              cast = snd (Engine.runGamePure (aimAt S.alice) gs (S.cast S.alice spellId))
              settled = resolveAll cast
          Spec.assertEqWith s "alice really lost the 2" (S.lifeOf S.alice settled) (Just 18)
          Spec.assertEqWith s "bob lost nothing" (S.lifeOf S.bob settled) (Just 20)
        -- CR 119.2 / 120.3a: "damage dealt to a player by a source without infect
        -- causes that player to lose that much life". The second producer, and
        -- the one no effect says the words for -- combat did.
        --
        -- ONE board carries the control. Both of alice's creatures connect, and
        -- CR 120.3b sends Glistener Elf's damage to poison counters INSTEAD of a
        -- life loss, so the 3 alice gains is the Hill Giant's alone. An engine
        -- that read "a life total moved" would gain her 4.
        Spec.it s "CR 119.2 damage loses life and CR 120.3b infect does not, on one attack" $ do
          blood <- S.printingOf s registry "Exquisite Blood"
          hillGiant <- S.printingOf s registry "Hill Giant"
          glistenerElf <- S.printingOf s registry "Glistener Elf"
          let (gs0, _, _) = S.combatBoardOf [hillGiant, glistenerElf] []
              (_, gs1) = S.addCreature blood S.alice gs0
              settled = resolveAll (S.fightWith S.aggressiveAnswer gs1)
          Spec.assertEqWith s "bob lost the Giant's 3 and none of the Elf's 1" (S.lifeOf S.bob settled) (Just 17)
          Spec.assertEqWith s "the Elf really connected" (S.playerCounterOf PlayerCounterKind.Poison S.bob settled) 1
          Spec.assertEqWith s "so alice gained 3, not 4" (S.lifeOf S.alice settled) (Just 23)
        -- CR 119.4's "in other words, the player loses that much life". The third
        -- producer, and the only one that happens while paying a COST rather than
        -- while an effect resolves -- so the record is written outside resolution
        -- and the CR 117.5 trigger scan still has to find it.
        Spec.it s "CR 119.4 bob pays 2 life for Greed and Exquisite Blood gains alice that much" $ do
          swamp <- S.printingOf s registry "Swamp"
          blood <- S.printingOf s registry "Exquisite Blood"
          greed <- S.printingOf s registry "Greed"
          case Face.activatedAbilities (S.combinedFace greed) of
            [] -> Spec.assertFailure s "Greed should carry an activated ability"
            ability : _ -> do
              let (_, withBlood) = S.addCreature blood S.alice (Setup.emptyGame S.bothPlayers)
                  (_, withSwamp) = S.addCreature swamp S.bob withBlood
                  (greedId, withGreed) = S.addCreature greed S.bob withSwamp
                  (_, gs1) = S.addLibraryCard swamp S.bob withGreed
                  gs =
                    gs1
                      { GameState.phase = Phase.PrecombatMain,
                        GameState.activePlayer = S.alice,
                        GameState.priority = Just S.alice
                      }
                  activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.bob greedId ability)
                  settled = resolveAll activated
              Spec.assertEqWith s "bob paid exactly 2" (S.lifeOf S.bob settled) (Just 18)
              Spec.assertEqWith s "and alice gained exactly that much" (S.lifeOf S.alice settled) (Just 22)
        -- eventBindings in isolation, so the binding is pinned to the RULE rather
        -- than to one card's payload -- the gain group's last case, mirrored. The
        -- 7 is no life total and no other number in reach, so an arm binding
        -- anything but the event's own amount fails here.
        --
        -- Both slots at once, and as a WHOLE map rather than a lookup: CR 603.2
        -- makes the amount and the player one environment, and an equality on the
        -- whole map is what would catch an arm that bound a third thing.
        Spec.it s "CR 603.2 eventBindings binds the amount the loss event carries and the loser" $
          Spec.assertEqWith
            s
            "thatMuch is the loss and thatPlayer is who lost it"
            (Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice (TriggerCondition.PlayerLosesLife PlayerRelation.Opponent) (GameEvent.LifeLost (LifeChange.MkLifeChange S.bob 7)))
            (Map.fromList [(Binding.eventAmount, Binding.toAmount 7), (Binding.triggerPlayer, Binding.toPlayer S.bob)])
        -- The loser is bound under the OTHER relation too, and that is a claim
        -- about the event rather than about the relation: CR 603.2's environment
        -- is what the event named, and Event.eventBindingSlots answers per
        -- CONDITION with no relation in hand, so a slot it promises has to hold
        -- for every relation the condition admits.
        Spec.it s "CR 603.2 the loser is bound under the You relation as well" $
          Spec.assertEqWith
            s
            "thatPlayer names the loser whichever relation matched"
            (Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice (TriggerCondition.PlayerLosesLife PlayerRelation.You) (GameEvent.LifeLost (LifeChange.MkLifeChange S.alice 3)))
            (Map.fromList [(Binding.eventAmount, Binding.toAmount 3), (Binding.triggerPlayer, Binding.toPlayer S.alice)])

-- CR 603.2's other half of a life-loss event: the PLAYER it named, not only the
-- amount. Mindcrank, {2} Artifact, "Whenever an opponent loses life, that player
-- mills that many cards" (CR 701.17a) -- the pool's first life trigger whose
-- payload acts on the player the event named rather than on CR 109.5's "you",
-- which is what makes `Binding.triggerPlayer` on this condition a slot something
-- reads rather than speculative construction.
--
-- THREE SEATS, and that is the whole design of the fixture. On a two-seat board
-- "that player" and "an opponent" name the same person, so an implementation that
-- milled SOME opponent -- the first one, say -- would pass with the binding
-- wrong. With bob and carol both opponents of alice, the two cases below differ
-- only in WHICH of them Sign in Blood targets, so a binding that answers a fixed
-- opponent fails whichever case is not that opponent, and a binding that answers
-- CR 109.5's "you" fails both. Confirmed by mutating each of the two in turn.
--
-- Every seat is stocked with six cards, which is load-bearing rather than tidy --
-- the lesson `lifeLossTriggerSpec`'s fixture already carries. Sign in Blood draws
-- its target two cards before it costs them the life, so a target whose library
-- ran out would lose to CR 104.3c the next time a player would get priority,
-- before the trigger could resolve, and the case would pass for that reason
-- instead of for the binding's.
mindcrankSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mindcrankSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- Sign in Blood's one target slot, answered with `who` -- as the Exquisite
      -- Blood group's helper does, and for the same reason.
      aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
        _ -> S.identityAnswer p
      sizeOf zone pid gs = length (Game.zoneMembers zone pid gs)
      -- alice: two Swamps for the {B}{B}, a Mindcrank, and Sign in Blood in hand.
      -- bob and carol: nothing but libraries, so neither is distinguishable from
      -- the other by anything except being targeted.
      board swamp mindcrank signInBlood =
        let withMana = List.foldl' (\g _ -> snd (S.addCreature swamp S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
            (_, withCrank) = S.addCreature mindcrank S.alice withMana
            stock pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard swamp pid g)) gs [1 .. (6 :: Int)]
         in S.handOne signInBlood (stock S.carol (stock S.bob (stock S.alice withCrank)))
      cases =
        [ ("bob", S.bob, S.carol),
          ("carol", S.carol, S.bob)
        ]
   in Spec.describe s "Mindcrank names the player who lost the life"
        . Foldable.for_ cases
        $ \(label, loser, bystander) ->
          -- CR 603.2: the targeted player takes the loss, and the SAME player
          -- mills 2. Six cards, less the two Sign in Blood draws them, less the
          -- two milled, leaves two -- while the other opponent's six are
          -- untouched, which is what a binding naming "an opponent" rather than
          -- THE opponent fails.
          Spec.it s ("CR 701.17a the player who lost the life is the player who mills, with " <> label <> " targeted") $ do
            swamp <- S.printingOf s registry "Swamp"
            mindcrank <- S.printingOf s registry "Mindcrank"
            signInBlood <- S.printingOf s registry "Sign in Blood"
            let (gs, spellId) = board swamp mindcrank signInBlood
                cast = snd (Engine.runGamePure (aimAt loser) gs (S.cast S.alice spellId))
                settled = resolveAll cast
            Spec.assertEqWith s "the targeted player really lost the 2" (S.lifeOf loser settled) (Just 18)
            Spec.assertEqWith s "and milled 2 into their own graveyard" (sizeOf Zone.Graveyard loser settled) 2
            Spec.assertEqWith s "leaving 6 - 2 drawn - 2 milled" (sizeOf Zone.Library loser settled) 2
            Spec.assertEqWith s "the OTHER opponent milled nothing" (sizeOf Zone.Graveyard bystander settled) 0
            Spec.assertEqWith s "and their library is whole" (sizeOf Zone.Library bystander settled) 6
            -- alice's graveyard holds Sign in Blood and nothing else: CR 109.5's
            -- "you" is the wrong answer here, and this is what says so.
            Spec.assertEqWith s "alice, who controls Mindcrank, milled nothing" (sizeOf Zone.Graveyard S.alice settled) 1
            Spec.assertEqWith s "and lost no life either" (S.lifeOf S.alice settled) (Just 20)

-- CR 102.1's bare "a player" -- every player in the game, the ability's own
-- controller included. The Master of Lake-town, {1}{B}{B} Legendary Creature --
-- Human Advisor 3/2, "Deathtouch. Whenever a player loses life, that player mills
-- that many cards." Mindcrank's sentence above with the relation widened, and the
-- pool's first printing that neither PlayerRelation.You nor PlayerRelation.Opponent
-- states: the two partition the table, so either is observably narrower than what
-- is printed.
--
-- The card's third ability -- "when The Master of Lake-town dies, draw a card for
-- each graveyard with seven or more cards in it" -- is masterOfLaketownDeathSpec
-- below.
--
-- THREE SEATS, for the reason Mindcrank's fixture gives and one more. On a
-- two-seat board "a player" is "you and your opponent", so the widened relation is
-- indistinguishable from a card that spelled both out; and the seat an "each
-- opponent" misreading DROPS is the controller's own, which only a case aimed at
-- her can catch. So the three cases below aim Magister Sphinx's entry trigger at
-- alice, at bob and at carol in turn, and each asserts all three graveyards.
--
-- The three seats start at 23, 19 and 27 against the Sphinx's 10, so the loss is
-- 13, 9 and 17 -- distinct from each other, from every starting total, and from
-- the number of players, so no two readings of the rule produce the same mill.
-- The boards are otherwise identical: the ONE thing the cases differ in is which
-- seat the trigger names.
--
-- Every library is stocked past the deepest mill, so CR 104.3c never decides a
-- case before its assertion runs.
masterOfLaketownSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
masterOfLaketownSpec s registry =
  let -- Cast, then settle-and-resolve until the stack runs dry: the Sphinx, its CR
      -- 603.6a entry trigger, and the life-loss trigger that one causes. NOT
      -- Engine.priorityLoop, which advances the turn and clears the event log the
      -- second trigger is scanned against.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step g = S.runPure answer (S.runPure answer g Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\g _ -> step g) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- FILTERED out of the offered set rather than built from the seat: CR 115.1's
      -- pool of players offers the recipients, and a slot answered with anything
      -- the offer did not contain is dropped at CR 608.2b with no error to read.
      -- Pinned, too -- an answerer that took whatever was legal would find another
      -- seat after a mutation and keep the case green.
      aimedAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer who) . snd) sets
        _ -> S.identityAnswer p
      graveyardSize pid gs = length (Game.zoneMembers Zone.Graveyard pid gs)
      board master sphinx plains island swamp =
        let withLands = S.landsFor swamp S.alice 1 (S.landsFor island S.alice 1 (S.landsFor plains S.alice 5 S.threePlayerGame))
            (_, withMaster) = S.addCreature master S.alice withLands
            stock pid g = List.foldl' (\b _ -> snd (S.addLibraryCard swamp pid b)) g [1 .. (20 :: Int)]
            stocked = List.foldl' (flip stock) withMaster [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice 23 (at S.bob 19 (at S.carol 27 (GameState.players stocked)))}
         in S.handOne sphinx lifed
      -- (label, the seat the Sphinx names, what that seat loses reaching 10).
      cases :: [(String, PlayerId.PlayerId, Int)]
      cases =
        [ ("alice, who controls the Master", S.alice, 13),
          ("bob", S.bob, 9),
          ("carol", S.carol, 17)
        ]
   in Spec.describe s "The Master of Lake-town watches every player's life total"
        . Foldable.for_ cases
        $ \(label, victim, loss) ->
          Spec.it s ("CR 102.1 \"a player\" admits " <> label <> ", who mills exactly what they lost") $ do
            master <- S.printingOf s registry "The Master of Lake-town"
            sphinx <- S.printingOf s registry "Magister Sphinx"
            plains <- S.printingOf s registry "Plains"
            island <- S.printingOf s registry "Island"
            swamp <- S.printingOf s registry "Swamp"
            let (gs, spellId) = board master sphinx plains island swamp
                after = castAndTrigger (aimedAt victim) spellId gs
            Spec.assertEqWith s "the named seat really came down to 10" (S.lifeOf victim after) (Just 10)
            Spec.assertEqWith s "and milled exactly the life they lost" (graveyardSize victim after) loss
            -- The other two seats, asserted every time rather than only where a
            -- reading would touch them: the trigger fires once per recorded loss,
            -- so a mill anywhere else is a second fire nobody asked for.
            Foldable.for_ (List.filter (/= victim) [S.alice, S.bob, S.carol]) $ \bystander -> do
              Spec.assertEqWith s "an untouched seat lost no life" (S.lifeOf bystander after) (S.lifeOf bystander gs)
              Spec.assertEqWith s "and milled nothing" (graveyardSize bystander after) 0

-- CR 700.4 / 404.1 / 608.2h: The Master of Lake-town's THIRD ability -- "when
-- The Master of Lake-town dies, draw a card for each graveyard with seven or
-- more cards in it". masterOfLaketownSpec above is the same card's CR 102.1
-- clause; this is the clause that folds PLAYERS by a fact about a zone they own,
-- which Filter.CardsInGraveyardAtLeast is the first atom to ask and
-- Pawl.Engine.Count.bakePerspective the site that answers it.
--
-- THREE SEATS holding 7, 6 and 8 AT THE MOMENT OF THE COUNT, so the answer is 2
-- and no other reading of the sentence agrees: "more than seven" and "each
-- opponent's graveyard" each answer 1, "six or more" and "every player" answer
-- 3, a sum of cards answers 21, and an atom left unbaked answers 0. The three
-- sizes are pairwise distinct, so a fold reading the wrong seat's graveyard
-- cannot land on the right number by luck.
--
-- The counted sizes are NOT the stocked ones, which is CR 404.1 doing real work:
-- the Bolt is an instant that finished resolving and the Master is a destroyed
-- permanent, so both are already in alice's graveyard when the trigger resolves.
-- She is stocked 5 and counted 7 -- and 7 exactly is what tells >= from >.
--
-- The second case is the paired negative, differing in exactly two stocking
-- numbers: all three graveyards hold 6 at the count. It asserts the trigger
-- reached the stack and resolved, so a zero cannot mean "nothing happened".
--
-- Every library holds twenty against a maximum draw of two, so CR 104.3c never
-- decides a case before its assertions run.
masterOfLaketownDeathSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
masterOfLaketownDeathSpec s registry =
  let graveyardSize pid gs = length (Game.zoneMembers Zone.Graveyard pid gs)
      librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)
      -- FILTERED out of the offered set, for masterOfLaketownSpec's reason: a
      -- hand-built recipient is dropped at CR 608.2b with no error, and an
      -- answerer that took whatever was legal could aim elsewhere after a
      -- mutation and keep the case green.
      aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimedAt oid p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature oid) . snd) sets
        _ -> S.identityAnswer p
      board master mountain swamp bolt aliceBin carolBin =
        let withLand = S.landsFor mountain S.alice 1 S.threePlayerGame
            (masterId, withMaster) = S.addCreature master S.alice withLand
            stockLib pid g = List.foldl' (\b _ -> snd (S.addLibraryCard swamp pid b)) g [1 .. (20 :: Int)]
            stocked = List.foldl' (flip stockLib) withMaster [S.alice, S.bob, S.carol]
            bin pid n g = List.foldl' (\b _ -> snd (S.addGraveyardCard swamp pid b)) g [1 .. (n :: Int)]
            (gs, spellId) = S.handOne bolt (bin S.carol carolBin (bin S.bob 6 (bin S.alice aliceBin stocked)))
         in (masterId, gs, spellId)
      -- diesTriggerSpec's sequence: cast the Bolt, resolve it onto the 3/2,
      -- settle -- CR 704.5g destroys the Master and the CR 117.5 settle's own
      -- scan gathers the dies trigger -- then resolve that trigger.
      kill (masterId, gs, spellId) =
        let cast = S.runPure (aimedAt masterId) gs (S.cast S.alice spellId)
            damaged = S.runPure (aimedAt masterId) cast Stack.resolveTop
            settled = S.runPure (aimedAt masterId) damaged Engine.settleForPriority
         in (settled, S.runPure (aimedAt masterId) settled Stack.resolveTop)
      printings = do
        master <- S.printingOf s registry "The Master of Lake-town"
        mountain <- S.printingOf s registry "Mountain"
        swamp <- S.printingOf s registry "Swamp"
        bolt <- S.printingOf s registry "Lightning Bolt"
        pure (master, mountain, swamp, bolt)
   in Spec.describe s "The Master of Lake-town counts the graveyards as it dies" $ do
        Spec.it s "CR 404.1 two of the three graveyards reach seven, so alice draws two" $ do
          (master, mountain, swamp, bolt) <- printings
          let (settled, after) = kill (board master mountain swamp bolt 5 8)
          -- The board the count actually sees, pinned so a later edit cannot make
          -- two readings agree by accident.
          Spec.assertEqWith s "alice's graveyard: five stocked, the Bolt, the Master" (graveyardSize S.alice settled) 7
          Spec.assertEqWith s "bob's stays one short" (graveyardSize S.bob settled) 6
          Spec.assertEqWith s "carol's is over" (graveyardSize S.carol settled) 8
          Spec.assertEqWith s "the dies trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "so she drew two: her own seven and carol's eight, not bob's six" (S.handSize S.alice after) 2
          Spec.assertEqWith s "off her own library" (librarySize S.alice after) 18
          Spec.assertEqWith s "everything resolved" (GameState.stack after) []
          -- The clause draws its CONTROLLER cards, however many graveyards it
          -- counted -- "for each graveyard" is the number, not the drawer.
          Spec.assertEqWith s "bob drew nothing" (S.handSize S.bob after) 0
          Spec.assertEqWith s "and carol nothing" (S.handSize S.carol after) 0
          Spec.assertEqWith s "bob's library is whole" (librarySize S.bob after) 20
          Spec.assertEqWith s "and carol's" (librarySize S.carol after) 20
        Spec.it s "CR 404.1 no graveyard reaches seven, so she draws nothing" $ do
          (master, mountain, swamp, bolt) <- printings
          let (settled, after) = kill (board master mountain swamp bolt 4 6)
          Spec.assertEqWith s "alice's graveyard: four stocked, the Bolt, the Master" (graveyardSize S.alice settled) 6
          Spec.assertEqWith s "bob's" (graveyardSize S.bob settled) 6
          Spec.assertEqWith s "carol's" (graveyardSize S.carol settled) 6
          Spec.assertEqWith s "the dies trigger still reached the stack" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and resolved" (GameState.stack after) []
          Spec.assertEqWith s "drawing nothing" (S.handSize S.alice after) 0
          Spec.assertEqWith s "and touching no library" (librarySize S.alice after) 20

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  lifeGainTriggerSpec s registry
  abilitiesWhenTriggeredSpec s registry
  lifeGainAmountSpec s registry
  falseCureSpec s registry
  singularCureSpec s registry
  apnapDelayedSpec s registry
  oneSeatDelayedSpec s registry
  communalVigilSpec s registry
  lifelinkGainEventsSpec s registry
  forthEorlingasSpec s registry
  communalRelapseSpec s registry
  enrageSpec s registry
  belltowerSphinxSpec s registry
  lifeLossTriggerSpec s registry
  mindcrankSpec s registry
  masterOfLaketownSpec s registry
  masterOfLaketownDeathSpec s registry
