{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Replacement over damage rewrites (CR 615.5 through 615.13, CR
-- 614.9 redirection): the amount-capping and counted shields from Divine
-- Deflection on, Spider-Punk's and Selfless Squire's kind, the redirects from
-- Turn the Tables to Lava Burst, and the APNAP order of CR 616.1. Split out of
-- Pawl.ReplacementSpec, which keeps the machinery.
module Pawl.DamageReplacementSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Int as Int
import Pawl.LifeReplacementSpec (aimCreatureAndOrder, castDeflection, castDisaster, deflectionCombat, winTheFlipAndOrder)
import Pawl.PreventionSpec (aimAndChoose, aimCreature, aimPlayer, answersFor, atLife, attackNoBlock, bobAttacks, castAndResolve, chosenSourcesIn, countersOn, onlyCreature, preventAllRows, preventionsRecorded, riderHits, settleDamage, shieldsLeft, theAbility, wasAskedToOrderDamage)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementEntry as ReplacementEntry
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 615.7's shield over SEVERAL recipients at once, whose producer is Divine
-- Deflection ({X}{W} Instant: "Prevent the next X damage that would be dealt to
-- you and/or permanents you control this turn. If damage is prevented this way,
-- Divine Deflection deals that much damage to any target" -- name, cost, type
-- line and Oracle text checked against api.scryfall.com 2026-08-27).
--
-- The card the rest of the prevention pool cannot reach: every other shield in
-- data/cards/ names ONE recipient, so none of them can tell a shared pool from a
-- shield per recipient. This one covers a player AND a described set of
-- permanents, which is what the DISJOINED recipient side of a DamagePattern is
-- for, and CR 615.7's "such effects count only the amount of damage; the number
-- of events or sources dealing it doesn't matter" is what makes the two
-- recipients share one X rather than each getting their own. CR 615.11's
-- shield-per-creature is the other shape, and that rule scopes itself to a card
-- saying "each", which this one does not.
--
-- REAL COMBAT, unlike the hand-built batches the Mending Hands group settles for:
-- CR 510.2 is what makes two hits simultaneous, and simultaneity is the whole
-- question here -- a batch the shield cannot cover is what raises CR 615.7's
-- allocation choice, and sequential damage would never contest it.
--
-- The numbers are all distinct -- shield 3, 5 at alice, 2 at her creature, 5 back
-- at the attacker -- so no two readings of the rule land on the same board.
divineDeflectionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
divineDeflectionSpec s registry = Spec.describe s "Divine Deflection (CR 615.7)" $ do
  -- THE case. One shield of 3 against 5 aimed at alice and 2 aimed at her
  -- creature, in one combat damage batch: a shield per recipient would prevent
  -- all 7 and hand the rider 7 to throw back, where the rule's one pool prevents
  -- exactly 3 and the other 4 are dealt.
  --
  -- The CR 615.7 allocation choice needs no prompt assertion of its own: the two
  -- orderings below leave different boards, which nothing but a raised and
  -- honoured Prompt.OrderDamage can produce.
  Spec.it s "CR 615.7 one shield over you AND your permanents is a single shared pool" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    piker <- S.printingOf s registry "Goblin Piker"
    deflection <- S.printingOf s registry "Divine Deflection"
    let base = S.landsInPlay plains 4
        (mine, g1) = S.addPermanent jedit S.alice base
        (_, g2) = S.addPermanent jedit S.bob g1
        (blocked, g3) = S.addPermanent piker S.bob g2
        (unshielded, spellId) = S.handOne deflection g3
        shielded = castAndResolve (castDeflection 3 S.bob) unshielded spellId
        strike order g = S.runCombat (deflectionCombat mine blocked order) (bobAttacks g)
        aliceFirst = strike [Recipient.ToPlayer S.alice, Recipient.ToCreature mine] shielded
        creatureFirst = strike [Recipient.ToCreature mine, Recipient.ToPlayer S.alice] shielded
        control = strike [] unshielded
    -- The pool ran out on alice's 5, so her creature's 2 is dealt in full. Under
    -- a shield per recipient this reads 0.
    Spec.assertEqWith s "spent on alice, the pool leaves her creature's 2 unprevented" (S.damageOf mine aliceFirst) (Just 2)
    Spec.assertEqWith s "and 3 of the 5 aimed at alice were prevented" (S.lifeOf S.alice aliceFirst) (Just 18)
    -- CR 615.5's rider, and the arithmetic that tells the two readings apart: the
    -- shield prevented 3, so Divine Deflection throws 3 -- not the 7 a shield per
    -- recipient would have prevented.
    Spec.assertEqWith s "so the rider deals exactly the 3 that was prevented" (S.lifeOf S.bob aliceFirst) (Just 17)
    -- The other allocation, which is the ruling's own example: the creature's 2
    -- is prevented whole and the last 1 goes to alice. Same total, different
    -- board.
    Spec.assertEqWith s "spent on the creature instead, its 2 never happens" (S.damageOf mine creatureFirst) (Just 0)
    Spec.assertEqWith s "and only 1 of alice's 5 is prevented" (S.lifeOf S.alice creatureFirst) (Just 16)
    Spec.assertEqWith s "the rider still deals 3 in total" (S.lifeOf S.bob creatureFirst) (Just 17)
    -- CR 615.13's own unit, and the one thing on this board the life totals
    -- cannot settle: the shield spanned TWO recipients here, and the rule counts
    -- one application of one prevention effect however many of the simultaneous
    -- events it was applied to. So the rider runs ONCE with the total rather than
    -- once per recipient, which is a difference in the number of damage events
    -- and not in their sum -- 3 either way, hence the count.
    Spec.assertEqWith s "CR 615.13 the rider throws its 3 back in ONE event, not a 2 and a 1" (riderHits S.bob creatureFirst) [3]
    Spec.assertEqWith s "and the allocation that spent the shield on one recipient throws one lot too" (riderHits S.bob aliceFirst) [3]
    -- The record those triggers read, which the rider count is a consequence of:
    -- ONE prevention carrying both recipients' shares, rather than one per
    -- recipient. After the behaviour it explains, not before it.
    Spec.assertEqWith
      s
      "and ONE prevention was recorded, holding each recipient's share"
      (fmap DamagePrevented.amounts (preventionsRecorded creatureFirst))
      [Map.fromList [(Recipient.ToCreature mine, 2), (Recipient.ToPlayer S.alice, 1)]]
    -- The fences. The shield covers what is dealt TO alice's side, never what she
    -- deals: her blocker's 5 kills the Piker either way. And the unshielded board
    -- differs in exactly the shield.
    Spec.assertBool s (not (S.onBattlefield blocked aliceFirst)) "her blocker's own 5 was not prevented"
    Spec.assertEqWith s "without the shield alice takes all 5" (S.lifeOf S.alice control) (Just 15)
    Spec.assertEqWith s "her creature takes all 2" (S.damageOf mine control) (Just 2)
    Spec.assertEqWith s "and bob is untouched, there being no rider to run" (S.lifeOf S.bob control) (Just 20)
    Spec.assertEqWith s "the shield is spent to 0 and dropped either way (CR 615.7)" (shieldsLeft aliceFirst, shieldsLeft creatureFirst) ([], [])
    -- The structural fence, AFTER the behaviour it explains rather than before
    -- it: the resolution installed one row holding 3, not one per recipient.
    -- Ordered last on purpose, since a row count sitting first absorbs every
    -- mutation of the arm that builds the rows and reports itself instead.
    Spec.assertEqWith s "and the resolution installed ONE shield, holding 3" (shieldsLeft shielded) [3]
  -- The other half of collapsing a batch to one record: a trigger scoped to ONE
  -- of the recipients still reads its own share. Selfless Squire ({3}{W} Creature
  -- -- Human Soldier 1/1, "Whenever damage that would be dealt to you is
  -- prevented, put that many +1/+1 counters on this creature") is that trigger,
  -- and Divine Deflection's shield is the one in data/cards/ that can span a
  -- player and a permanent at once, so this pair is the only board in the pool
  -- where "that many" and "the whole application" differ.
  --
  -- The Squire is put onto the battlefield DIRECTLY, so its own enters trigger
  -- never installs a second shield to confuse the two: the only prevention here
  -- is the Deflection's.
  --
  -- The allocation is the creature's 2 first and alice's last 1 second, so her
  -- share is 1 of the 3 -- distinct from the total, from the 2 the creature took
  -- and from the 5 aimed at her, so no two readings land on the same board.
  Spec.it s "CR 615.13 a trigger scoped to one recipient reads its own share of the application" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    piker <- S.printingOf s registry "Goblin Piker"
    deflection <- S.printingOf s registry "Divine Deflection"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    let base = S.landsInPlay plains 4
        (mine, g1) = S.addPermanent jedit S.alice base
        (squire, g2) = S.addPermanent squirePrinting S.alice g1
        (_, g3) = S.addPermanent jedit S.bob g2
        (blocked, g4) = S.addPermanent piker S.bob g3
        (unshielded, spellId) = S.handOne deflection g4
        shielded = castAndResolve (castDeflection 3 S.bob) unshielded spellId
        after = S.runCombat (deflectionCombat mine blocked [Recipient.ToCreature mine, Recipient.ToPlayer S.alice]) (bobAttacks shielded)
    Spec.assertEqWith s "setup: the Squire is on the battlefield as a 1/1" (S.powerToughnessOf squire shielded) (Just (1, 1))
    -- The board the counters are read against: the shield stopped 2 on her
    -- creature and 1 of the 5 aimed at her.
    Spec.assertEqWith s "the creature's 2 never happens" (S.damageOf mine after) (Just 0)
    Spec.assertEqWith s "and 1 of alice's 5 is prevented" (S.lifeOf S.alice after) (Just 16)
    -- The discriminating assertion: HER share, not the application's total. A
    -- reading that handed the trigger everything the shield stopped would make
    -- this a 4/4.
    Spec.assertEqWith s "so the Squire counts alice's 1, not the whole application's 3" (countersOn CounterKind.PlusOnePlusOne squire after) 1
    Spec.assertEqWith s "leaving the 1/1 a 2/2" (S.powerToughnessOf squire after) (Just (2, 2))
  -- CR 608.2f: one instruction naming objects AND players is ONE action, so its
  -- damage is one batch. Molten Disaster ({X}{R}{R} Sorcery, "Kicker {R}. If this
  -- spell was kicked, it has split second. Molten Disaster deals X damage to each
  -- creature without flying and each player." -- name, cost, type line and Oracle
  -- text checked against api.scryfall.com 2026-08-29) is the sentence, written as
  -- one Effect.DealDamage over two ObjectRefs.
  --
  -- The shield above is the only observer in data/cards/ that can tell one batch
  -- from two: it alone spans a player AND their permanents, so it alone is
  -- contested by a batch holding both. Two sequential batches spend it on
  -- whichever ran first and raise no CR 615.7 question at all.
  --
  -- Numbers all distinct -- shield 3, X of 2, a 5/5 and a 2/1 -- so no two
  -- readings land on the same board.
  Spec.it s "CR 608.2f creatures and players in one sentence are one batch" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    piker <- S.printingOf s registry "Goblin Piker"
    deflection <- S.printingOf s registry "Divine Deflection"
    disaster <- S.printingOf s registry "Molten Disaster"
    let base = S.landsFor mountain S.alice 6 (S.landsInPlay plains 4)
        (mine, g1) = S.addPermanent jedit S.alice base
        (theirs, g2) = S.addPermanent piker S.bob g1
        (deflectionId, g3) = S.addHandCard deflection S.alice g2
        (disasterId, unshielded) = S.addHandCard disaster S.alice g3
        shielded = castAndResolve (castDeflection 3 S.bob) unshielded deflectionId
        blast order g = castAndResolve (castDisaster 2 order) g disasterId
        aliceFirst = blast [Recipient.ToPlayer S.alice, Recipient.ToCreature mine] shielded
        creatureFirst = blast [Recipient.ToCreature mine, Recipient.ToPlayer S.alice] shielded
        control = blast [] unshielded
    -- THE case. Spending the pool on alice's own 2 first leaves 1 for her
    -- creature's 2, so 1 is marked. Two batches cannot produce this board: the
    -- creature half is the sentence's first half and would take the shield whole.
    Spec.assertEqWith s "spent on alice, 1 of her creature's 2 is dealt" (S.damageOf mine aliceFirst) (Just 1)
    Spec.assertEqWith s "and all 2 aimed at alice were prevented" (S.lifeOf S.alice aliceFirst) (Just 20)
    -- The other allocation, which is also what two batches would have produced --
    -- kept so the pair of boards differs in the answer and in nothing else.
    Spec.assertEqWith s "spent on her creature instead, its 2 never happens" (S.damageOf mine creatureFirst) (Just 0)
    Spec.assertEqWith s "and 1 of alice's 2 is dealt" (S.lifeOf S.alice creatureFirst) (Just 19)
    -- The question itself: two batches leave the shield uncontested in each, so
    -- nothing is asked.
    Spec.assertBool
      s
      (wasAskedToOrderDamage (answersFor (castDisaster 2 []) shielded (S.cast S.alice disasterId Monad.>> Stack.resolveTop)))
      "alice was asked which damage the one batch's shield prevents"
    -- The fences. The shield covers alice's side only, so bob and his Piker take
    -- the whole 2 either way -- and CR 615.5's rider throws back the 3 that was
    -- prevented, which is one pool's worth however it was allocated.
    Spec.assertEqWith s "bob takes the disaster's 2 and the rider's 3" (S.lifeOf S.bob aliceFirst, S.lifeOf S.bob creatureFirst) (Just 15, Just 15)
    Spec.assertEqWith s "and his unshielded creature is marked with the whole 2" (S.damageOf theirs aliceFirst, S.damageOf theirs creatureFirst) (Just 2, Just 2)
    Spec.assertEqWith s "and the shield is spent to 0 either way (CR 615.7)" (shieldsLeft aliceFirst, shieldsLeft creatureFirst) ([], [])
    -- The unshielded board, differing in exactly the shield: alice takes all 2 and
    -- so does her creature, and bob takes 2 with no rider behind it.
    Spec.assertEqWith s "without the shield alice takes all 2" (S.lifeOf S.alice control) (Just 18)
    Spec.assertEqWith s "her creature takes all 2" (S.damageOf mine control) (Just 2)
    Spec.assertEqWith s "and bob takes 2, there being no rider to run" (S.lifeOf S.bob control) (Just 18)
  -- CR 608.2f again, over a sentence whose halves deal DIFFERENT amounts. Char
  -- ({2}{R} Instant, "Char deals 4 damage to any target and 2 damage to you" --
  -- name, cost, type line and Oracle text checked against api.scryfall.com
  -- 2026-08-29) is one instruction over two clauses, 4 at the target and 2 at its
  -- caster, where Molten Disaster's two clauses share one amount.
  --
  -- Aimed at alice's OWN creature, so the shield above spans both halves: the
  -- sentence's whole 6 is dealt to her side at once, and 3 of it is prevented.
  --
  -- Written as two instructions -- one amount per instruction, which is what the
  -- opcode could carry before -- the target's 4 runs first and takes the shield
  -- whole, which is exactly the creature-first board below. So the alice-first
  -- board is the one no two-instruction reading can produce.
  --
  -- Numbers all distinct -- shield 3, 4 at the creature, 2 at alice, a 5/5 and a
  -- 2/1 -- so no two readings land on the same board.
  Spec.it s "CR 608.2f one sentence's differing amounts are one batch" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    piker <- S.printingOf s registry "Goblin Piker"
    deflection <- S.printingOf s registry "Divine Deflection"
    char <- S.printingOf s registry "Char"
    let base = S.landsFor mountain S.alice 3 (S.landsInPlay plains 4)
        (mine, g1) = S.addPermanent jedit S.alice base
        (theirs, g2) = S.addPermanent piker S.bob g1
        (deflectionId, g3) = S.addHandCard deflection S.alice g2
        (charId, unshielded) = S.addHandCard char S.alice g3
        shielded = castAndResolve (castDeflection 3 S.bob) unshielded deflectionId
        burn order g = castAndResolve (aimCreatureAndOrder mine order) g charId
        aliceFirst = burn [Recipient.ToPlayer S.alice, Recipient.ToCreature mine] shielded
        creatureFirst = burn [Recipient.ToCreature mine, Recipient.ToPlayer S.alice] shielded
        control = burn [] unshielded
    -- THE case. Spending the pool on alice's own 2 first leaves 1 for her
    -- creature's 4, so 3 is marked. Two instructions cannot produce this board:
    -- the target's 4 is the sentence's first half and would take the shield
    -- whole, leaving 1.
    Spec.assertEqWith s "spent on alice, 3 of her creature's 4 is dealt" (S.damageOf mine aliceFirst) (Just 3)
    Spec.assertEqWith s "and all 2 aimed at alice were prevented" (S.lifeOf S.alice aliceFirst) (Just 20)
    -- The other allocation, which is also what two instructions would have
    -- produced -- kept so the pair of boards differs in the answer and in
    -- nothing else.
    Spec.assertEqWith s "spent on her creature instead, only 1 of its 4 is dealt" (S.damageOf mine creatureFirst) (Just 1)
    Spec.assertEqWith s "and alice takes the whole 2" (S.lifeOf S.alice creatureFirst) (Just 18)
    -- The question itself: two instructions leave the shield uncontested in each,
    -- so nothing is asked.
    Spec.assertBool
      s
      (wasAskedToOrderDamage (answersFor (aimCreatureAndOrder mine []) shielded (S.cast S.alice charId Monad.>> Stack.resolveTop)))
      "alice was asked which damage the one batch's shield prevents"
    -- The fences. Bob's creature is no part of the sentence, and CR 615.5's rider
    -- throws back the 3 that was prevented, which is one pool's worth however it
    -- was allocated.
    Spec.assertEqWith s "the rider deals bob the 3 that was prevented, either way" (S.lifeOf S.bob aliceFirst, S.lifeOf S.bob creatureFirst) (Just 17, Just 17)
    Spec.assertEqWith s "and bob's creature, named by neither half, is untouched" (S.damageOf theirs aliceFirst, S.damageOf theirs creatureFirst) (Just 0, Just 0)
    Spec.assertEqWith s "and the shield is spent to 0 either way (CR 615.7)" (shieldsLeft aliceFirst, shieldsLeft creatureFirst) ([], [])
    -- The unshielded board, differing in exactly the shield: her creature takes
    -- all 4, alice takes all 2, and bob takes nothing at all.
    Spec.assertEqWith s "without the shield her creature takes all 4" (S.damageOf mine control) (Just 4)
    Spec.assertEqWith s "and alice takes all 2" (S.lifeOf S.alice control) (Just 18)
    Spec.assertEqWith s "and bob is untouched, there being no rider to run" (S.lifeOf S.bob control) (Just 20)
  -- The same sentence shape on an ACTIVATED ability, and with the halves' amounts
  -- EQUAL. Brothers of Fire ({1}{R}{R} Creature -- Human Shaman 2/2,
  -- "{1}{R}{R}: This creature deals 1 damage to any target and 1 damage to you" --
  -- name, cost, type line, power/toughness and Oracle text checked against
  -- api.scryfall.com 2026-08-29) was written as two instructions until this
  -- change, and equal amounts fit one instruction as the opcode already stood.
  --
  -- Aimed at alice's own creature again, so the shield spans both halves. The
  -- discriminator is the PAIR: a shield of 1 spent on alice's own 1 leaves her
  -- creature's 1 dealt, where two instructions spend it on the target's 1 first
  -- and leave alice's own dealt instead. Either board marks one damage somewhere,
  -- so neither reading is told apart by one number alone.
  Spec.it s "CR 608.2f an ability's two halves are one batch" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    deflection <- S.printingOf s registry "Divine Deflection"
    brothers <- S.printingOf s registry "Brothers of Fire"
    let base = S.landsFor mountain S.alice 3 (S.landsInPlay plains 2)
        (mine, g1) = S.addPermanent jedit S.alice base
        (dealerId, g2) = S.addPermanent brothers S.alice g1
        (deflectionId, unshielded) = S.addHandCard deflection S.alice g2
        shielded = castAndResolve (castDeflection 1 S.bob) unshielded deflectionId
        burn order g = S.runPure (aimCreatureAndOrder mine order) g (Activate.activateAbility S.alice dealerId (theAbility brothers) Monad.>> Stack.resolveTop)
        aliceFirst = burn [Recipient.ToPlayer S.alice, Recipient.ToCreature mine] shielded
        creatureFirst = burn [Recipient.ToCreature mine, Recipient.ToPlayer S.alice] shielded
        control = burn [] unshielded
    -- THE case, read as a pair: the shield went on alice's own 1, so her
    -- creature's 1 is dealt. Two instructions produce the other board, which the
    -- next assertion is.
    Spec.assertEqWith s "spent on alice, her creature takes the 1 and she takes none" (S.damageOf mine aliceFirst, S.lifeOf S.alice aliceFirst) (Just 1, Just 20)
    Spec.assertEqWith s "spent on her creature instead, alice takes the 1" (S.damageOf mine creatureFirst, S.lifeOf S.alice creatureFirst) (Just 0, Just 19)
    -- The fences: CR 615.5's rider throws back the 1 that was prevented, the
    -- dealer took none of its own damage, and the unshielded board differs in
    -- exactly the shield.
    Spec.assertEqWith s "the rider deals bob the 1 that was prevented, either way" (S.lifeOf S.bob aliceFirst, S.lifeOf S.bob creatureFirst) (Just 19, Just 19)
    Spec.assertEqWith s "and the dealer itself is untouched" (S.damageOf dealerId aliceFirst, S.damageOf dealerId creatureFirst) (Just 0, Just 0)
    Spec.assertEqWith s "without the shield both halves land" (S.damageOf mine control, S.lifeOf S.alice control) (Just 1, Just 19)
    Spec.assertEqWith s "and bob is untouched, there being no rider to run" (S.lifeOf S.bob control) (Just 20)
  -- The mass shape of the same conversion. Winter Sky ({R} Sorcery, "Flip a coin.
  -- If you win the flip, Winter Sky deals 1 damage to each creature and each
  -- player. If you lose the flip, each player draws a card." -- name, cost, type
  -- line and Oracle text checked against api.scryfall.com 2026-08-29) is Molten
  -- Disaster's sentence at a fixed 1, and was written as two instructions until
  -- this change. The flip is PINNED to a win, this case being about the damage
  -- sentence alone.
  Spec.it s "CR 608.2f creatures and players in a won flip are one batch" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    piker <- S.printingOf s registry "Goblin Piker"
    deflection <- S.printingOf s registry "Divine Deflection"
    sky <- S.printingOf s registry "Winter Sky"
    let base = S.landsFor mountain S.alice 1 (S.landsInPlay plains 2)
        (mine, g1) = S.addPermanent jedit S.alice base
        (theirs, g2) = S.addPermanent piker S.bob g1
        (deflectionId, g3) = S.addHandCard deflection S.alice g2
        (skyId, unshielded) = S.addHandCard sky S.alice g3
        shielded = castAndResolve (castDeflection 1 S.bob) unshielded deflectionId
        blast order g = castAndResolve (winTheFlipAndOrder order) g skyId
        aliceFirst = blast [Recipient.ToPlayer S.alice, Recipient.ToCreature mine] shielded
        creatureFirst = blast [Recipient.ToCreature mine, Recipient.ToPlayer S.alice] shielded
        control = blast [] unshielded
    -- THE case, the pair again: alice's shield of 1 covers her own 1 and her
    -- creature's 1, and two instructions would spend it on the creature half the
    -- sentence writes first.
    Spec.assertEqWith s "spent on alice, her creature takes the 1 and she takes none" (S.damageOf mine aliceFirst, S.lifeOf S.alice aliceFirst) (Just 1, Just 20)
    Spec.assertEqWith s "spent on her creature instead, alice takes the 1" (S.damageOf mine creatureFirst, S.lifeOf S.alice creatureFirst) (Just 0, Just 19)
    -- The fences: the shield covers alice's side only, so bob takes the sorcery's
    -- 1 plus CR 615.5's rider, and his creature takes the whole 1.
    Spec.assertEqWith s "bob takes the sorcery's 1 and the rider's 1, either way" (S.lifeOf S.bob aliceFirst, S.lifeOf S.bob creatureFirst) (Just 18, Just 18)
    Spec.assertEqWith s "and his unshielded creature is marked with the whole 1" (S.damageOf theirs aliceFirst, S.damageOf theirs creatureFirst) (Just 1, Just 1)
    Spec.assertEqWith s "without the shield both of alice's halves land" (S.damageOf mine control, S.lifeOf S.alice control) (Just 1, Just 19)
    Spec.assertEqWith s "and bob takes 1, there being no rider to run" (S.lifeOf S.bob control) (Just 19)

-- CR 615.5's additional effect on the UNBOUNDED shield (CR 615.1 / 615.3), which
-- Test of Faith's countdown shield above cannot reach. Brace for Impact ({4}{W}
-- Instant) prints "Prevent all damage that would be dealt to target multicolored
-- creature this turn. For each 1 damage prevented this way, put a +1/+1 counter
-- on that creature."
--
-- "Multicolored" is CR 105.2b -- two or more of the five colors -- written as
-- the ten pairs of Filter.HasColor rather than as an atom of its own, since a
-- composition of existing atoms is not a second spelling of one relation.
--
-- The unbounded shield has no count, so CR 615.5's "the damage prevented this
-- way" is per APPLICATION rather than a running total; the second case is what
-- tells those two readings apart, and they answer 2 and 3.
braceForImpactSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
braceForImpactSpec s registry = Spec.describe s "Brace for Impact (CR 615.5)" $ do
  Spec.it s "an unbounded shield carries CR 615.5's rider" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    brace <- S.printingOf s registry "Brace for Impact"
    let base = S.landsInPlay plains 5
        (victim, g1) = S.addPermanent jedit S.alice base
        (pinger, g2) = S.addPermanent sorcerer S.alice g1
        (g3, spellId) = S.handOne brace g2
        shielded = castAndResolve (aimCreature victim) g3 spellId
        ping g = S.runPure (aimCreature victim) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
        control = ping g3
    Spec.assertEqWith s "setup: the unbounded shield is a floating replacement" (preventAllRows shielded) 1
    Spec.assertEqWith s "the ping's 1 is prevented, so nothing is marked" (S.damageOf victim after) (Just 0)
    Spec.assertEqWith s "and one +1/+1 counter goes on, per damage prevented" (countersOn CounterKind.PlusOnePlusOne victim after) 1
    Spec.assertEqWith s "so the 5/5 is a 6/6" (S.powerToughnessOf victim after) (Just (6, 6))
    Spec.assertEqWith s "and the shield stays: CR 615.7's terminator does not apply" (preventAllRows after) 1
    Spec.assertEqWith s "unshielded that same ping marks 1 and puts none on" (S.damageOf victim control, countersOn CounterKind.PlusOnePlusOne victim control) (Just 1, 0)
  Spec.it s "CR 615.5's amount is per application, not a running total" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    brace <- S.printingOf s registry "Brace for Impact"
    let base = S.landsInPlay plains 5
        (victim, g1) = S.addPermanent jedit S.alice base
        -- TWO Sorcerers, because one taps for its own ability and a running-total
        -- reading can only be told from a per-application one by a SECOND
        -- application.
        (first, g2) = S.addPermanent sorcerer S.alice g1
        (second, g3) = S.addPermanent sorcerer S.alice g2
        (g4, spellId) = S.handOne brace g3
        shielded = castAndResolve (aimCreature victim) g4 spellId
        ping oid g = S.runPure (aimCreature victim) g (Activate.activateAbility S.alice oid (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping second (ping first shielded)
    Spec.assertEqWith s "setup: the unbounded shield is a floating replacement" (preventAllRows shielded) 1
    -- A running-total reading would put 1 on and then 2 on, for 3.
    Spec.assertEqWith s "two applications of 1 put one counter on each" (countersOn CounterKind.PlusOnePlusOne victim after) 2
    Spec.assertEqWith s "with nothing ever marked" (S.damageOf victim after) (Just 0)
    Spec.assertEqWith s "and the shield still installed after both" (preventAllRows after) 1
  -- The POSITIVE half is the pin: the multicolored creature IS in the legal set
  -- on the same board the mono-coloured one is not, so a filter that excluded
  -- everything would fail here rather than pass the negative vacuously.
  Spec.it s "CR 105.2b: only a multicolored creature is a legal target" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    brace <- S.printingOf s registry "Brace for Impact"
    let base = S.landsInPlay plains 5
        (multi, g1) = S.addPermanent jedit S.alice base
        (mono, g2) = S.addPermanent pikerPrinting S.alice g1
        (g3, spellId) = S.handOne brace g2
        atMulti = castAndResolve (onlyCreature multi) g3 spellId
        atMono = castAndResolve (onlyCreature mono) g3 spellId
    Spec.assertEqWith s "the white-and-blue 5/5 is offered, so a shield goes up" (preventAllRows atMulti) 1
    Spec.assertEqWith s "the mono-red 2/1 is not, so none does" (preventAllRows atMono) 0

-- CR 615.5's additional effect over a PLAYER recipient, which neither Test of
-- Faith's nor Brace for Impact's nor Stormwild Capridor's permanent recipient can
-- reach: a player has no Object, so the prevented amount has nowhere to be bound
-- and CR 615.5's "the amount of damage that was prevented" travels on
-- GameState.ambientAmounts instead. Inkshield ({3}{W}{B} Instant) prints "Prevent
-- all combat damage that would be dealt to you this turn. For each 1 damage
-- prevented this way, create a 2/1 white and black Inkling creature token with
-- flying."
--
-- Numbers all distinct: the attacker is a 5/5 and the pinger deals 1, so the
-- readings answer 20 life and 5 tokens (right), 20 and 1 (one token per
-- application), 20 and 0 (the rider never runs), 15 and 0 (no shield at all) and
-- 19 and 0 (the kind refused it). No two coincide.
inkshieldSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
inkshieldSpec s registry = Spec.describe s "Inkshield (CR 615.5)" $ do
  Spec.it s "a shield over a PLAYER runs CR 615.5's rider, scaled by the amount" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    inkshield <- S.printingOf s registry "Inkshield"
    let base = S.landsFor swamp S.alice 3 (S.landsInPlay plains 2)
        (_, g1) = S.addPermanent jedit S.bob base
        (g2, spellId) = S.handOne inkshield g1
        shielded = castAndResolve S.identityAnswer g2 spellId
        after = S.runCombat attackNoBlock (bobAttacks shielded)
        -- The CONTROL is the same board with Inkshield still in hand, so the one
        -- difference between the two is the shield.
        control = S.runCombat attackNoBlock (bobAttacks g2)
    Spec.assertEqWith s "setup: the unbounded shield is a floating replacement" (preventAllRows shielded) 1
    Spec.assertEqWith s "the 5/5's whole combat damage is prevented" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and five Inklings arrive, one per damage prevented" (length (S.tokensOf after)) 5
    Monad.forM_ (S.tokensOf after) $ \oid -> do
      Spec.assertEqWith s "each a 2/1" (S.powerToughnessOf oid after) (Just (2, 1))
      Spec.assertEqWith s "white and black" (Projection.colorsOf oid after) (Set.fromList [Color.White, Color.Black])
      Spec.assertEqWith s "an Inkling" (Projection.subtypesOf oid after) (Set.singleton Subtype.Inkling)
      Spec.assertEqWith s "with flying" (Projection.hasKeyword Keyword.Flying oid after) True
      -- CR 111.4: the name is the subtypes plus the word "Token".
      Spec.assertEqWith s "named Inkling Token (CR 111.4)" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Inkling Token")))
    -- The channel the amount travelled on is put back, which the stamp it
    -- replaced had no test for: a value left behind would shadow every later
    -- reserved-slot read in the game.
    Spec.assertEqWith s "and CR 615.5's amount channel does not outlive the rider" (GameState.ambientAmounts after) Map.empty
    -- The VACUITY guard: unshielded, that same attack really is dealt, so the
    -- prevention above is a prevention rather than an attack that never happened.
    Spec.assertEqWith s "unshielded the same attack takes 5 and makes no token" (S.lifeOf S.alice control, length (S.tokensOf control)) (Just 15, 0)
  Spec.it s "the combat-only shield leaves noncombat damage alone, rider and all (CR 608)" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    inkshield <- S.printingOf s registry "Inkshield"
    let base = S.landsFor swamp S.alice 3 (S.landsInPlay plains 2)
        (pinger, g1) = S.addPermanent sorcerer S.alice base
        (g2, spellId) = S.handOne inkshield g1
        shielded = castAndResolve S.identityAnswer g2 spellId
        ping g = S.runPure (aimPlayer S.alice) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
    Spec.assertEqWith s "setup: the shield is installed" (preventAllRows shielded) 1
    Spec.assertEqWith s "the Sorcerer's noncombat 1 is dealt anyway" (S.lifeOf S.alice after) (Just 19)
    Spec.assertEqWith s "so nothing was prevented and no rider ran" (length (S.tokensOf after)) 0

-- CR 615.5's additional effect on a STATIC prevention ability, which is where
-- Test of Faith's floating shield above cannot reach: Stormwild Capridor ({2}{W}
-- Creature -- Bird Goat 1/3, flying) prints "If noncombat damage would be dealt
-- to this creature, prevent that damage. Put a +1/+1 counter on this creature
-- for each 1 damage prevented this way."
--
-- Three clauses, three cases, and each case is a PAIR of boards differing in one
-- thing:
--
--   * the rider itself -- 3 prevented becomes 3 counters -- against the same
--     Bolt aimed at the Goblin Piker beside it, which the ability does not cover;
--   * CR 615.1's printed recipient, which is why that second board's Piker dies;
--   * the printed KIND, combat damage passing where the same amount of
--     noncombat damage does not.
--
-- Numbers all distinct: the Bolt is 3, the combat hit is 2, the counters are 3,
-- and the shielded creature goes from 1/3 to 4/6. No two readings of the rule
-- meet on one of them -- a "one counter per event" reading would answer 1, an
-- unrun rider 0, and a Vigor-shaped "another creature you control" reading would
-- have saved the Piker instead.
stormwildCapridorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stormwildCapridorSpec s registry = Spec.describe s "Stormwild Capridor (CR 615.5)" $ do
  let hit kind src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing kind
  -- The rider fires from the funnel a RESOLVING spell drains
  -- (Resolve.runPreventionRider), the same seam Test of Faith's shield uses --
  -- so what is new here is only where the rider came from: the permanent's
  -- printed ability rather than a row a resolution installed.
  Spec.it s "CR 615.5 prevented noncombat damage becomes that many +1/+1 counters" $ do
    mountain <- S.printingOf s registry "Mountain"
    capridorPrinting <- S.printingOf s registry "Stormwild Capridor"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.landsInPlay mountain 1
        (capridor, g1) = S.addPermanent capridorPrinting S.alice base
        (piker, g2) = S.addPermanent pikerPrinting S.alice g1
        (g3, spellId) = S.handOne bolt g2
        -- ONE board, two aims: the only difference between these is which
        -- creature the Bolt names.
        atCapridor = castAndResolve (aimCreature capridor) g3 spellId
        atPiker = castAndResolve (aimCreature piker) g3 spellId
    Spec.assertEqWith s "setup: the Capridor is a 1/3" (S.powerToughnessOf capridor g3) (Just (1, 3))
    -- CR 615.6: the prevented event never happened, so nothing is marked.
    Spec.assertEqWith s "no damage is marked on the Capridor" (S.damageOf capridor atCapridor) (Just 0)
    Spec.assertEqWith s "three +1/+1 counters, one per damage prevented" (countersOn CounterKind.PlusOnePlusOne capridor atCapridor) 3
    Spec.assertEqWith s "so it is a 4/6" (S.powerToughnessOf capridor atCapridor) (Just (4, 6))
    -- CR 615.1's printed recipient: the ability covers "this creature" and
    -- nothing else, so the same Bolt lands on the Piker in full. Marked rather
    -- than dead, since nothing has taken priority to run CR 704.3's check.
    Spec.assertEqWith s "the same Bolt marks its whole 3 on the Piker beside it" (S.damageOf piker atPiker) (Just 3)
    Spec.assertEqWith s "and puts no counter on the Capridor" (countersOn CounterKind.PlusOnePlusOne capridor atPiker) 0
  -- The printed KIND, asked through the damage funnel alone: one field of the
  -- event differs between these two boards and nothing else does. Only the
  -- prevention is read here -- Damage.applyDamage queues the rider for a caller
  -- to drain, and the case below is what proves the queue stays empty on the
  -- combat side.
  Spec.it s "CR 615.1 the printed kind admits noncombat damage and refuses combat damage" $ do
    mountain <- S.printingOf s registry "Mountain"
    capridorPrinting <- S.printingOf s registry "Stormwild Capridor"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay mountain 1
        (capridor, g1) = S.addPermanent capridorPrinting S.alice base
        (attacker, g2) = S.addPermanent pikerPrinting S.bob g1
        settle kind = settleDamage S.identityAnswer g2 [hit kind attacker (Recipient.ToCreature capridor) 2]
    Spec.assertEqWith s "noncombat: the 2 is prevented" (S.damageOf capridor (settle DamageKind.Noncombat)) (Just 0)
    Spec.assertEqWith s "combat: the same 2 is marked" (S.damageOf capridor (settle DamageKind.Combat)) (Just 2)
  -- The same refusal driven through a REAL combat phase, which is the funnel
  -- that would run a rider if one fired (Engine's combat damage step drains the
  -- queue): the Capridor blocks, takes 2, and gains nothing.
  Spec.it s "CR 615.5 combat damage puts no counter on, because none of it was prevented" $ do
    plains <- S.printingOf s registry "Plains"
    capridorPrinting <- S.printingOf s registry "Stormwild Capridor"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay plains 1
        (_attacker, g1) = S.addPermanent pikerPrinting S.alice base
        (capridor, g2) = S.addPermanent capridorPrinting S.bob g1
        after =
          S.runCombat S.aggressiveAnswer $
            g2
              { GameState.activePlayer = S.alice,
                GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
                GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]},
                GameState.remaining =
                  Seq.fromList
                    [ Phase.Combat CombatStep.DeclareBlockers,
                      Phase.Combat CombatStep.CombatDamage,
                      Phase.Combat CombatStep.EndOfCombat,
                      Phase.PostcombatMain
                    ]
              }
    Spec.assertBool s (S.onBattlefield capridor after) "the 1/3 blocker survived a 2-power attacker"
    Spec.assertEqWith s "with the attacker's 2 marked on it" (S.damageOf capridor after) (Just 2)
    Spec.assertEqWith s "and no counters, since nothing was prevented" (countersOn CounterKind.PlusOnePlusOne capridor after) 0
    Spec.assertEqWith s "so it is still a 1/3" (S.powerToughnessOf capridor after) (Just (1, 3))

-- CR 615.10's static shield with an amount, which is the shape neither Fog's
-- blanket prevention nor CR 615.7's countdown can reach: Temple Altisaur ({4}{W}
-- Creature -- Dinosaur 3/4) prints "If a source would deal damage to another
-- Dinosaur you control, prevent all but 1 of that damage" (name, cost, type
-- line, P/T and Oracle text checked against api.scryfall.com 2026-08-29). Its
-- whole text is that one ability, so nothing else on the card can be what these
-- assertions read.
--
-- The rewrite is a FLOOR on what survives rather than a ceiling on what is
-- stopped, and each case below moves exactly one thing off one board:
--
--   * the RECIPIENT, across the printed clause's three narrowings -- "another"
--     (the Altisaur's own damage lands whole), "Dinosaur" (the Goblin Piker's
--     does), and "you control" (bob's Dinosaur's does);
--   * the AMOUNT, an event already at the floor passing through untouched;
--   * the BATCH SIZE, CR 615.10's last sentence -- two simultaneous events each
--     keep their own 1, and nobody is asked which the shield covers;
--   * the SOURCE, over CR 615.12's "can't be prevented", which is what proves
--     the rewrite is a PREVENTION effect (CR 615.1a) rather than an
--     instead-amount that happens to shrink the event: an instead-amount would
--     still cut the Excruciator's 3 to 1, where a prevention prevents none of
--     it.
--
-- Numbers all distinct: the floor is 1, the ordinary hit is 5, the unpreventable
-- one is 3, and the shielded Dinosaur is a 4/4. Damage is settled through
-- Damage.applyDamage rather than a resolution, so nothing has taken priority to
-- run CR 704.3's check and a 4/4 with 5 marked on it is marked rather than dead.
templeAltisaurSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
templeAltisaurSpec s registry = Spec.describe s "Temple Altisaur (CR 615.10)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      withBoard act = do
        plains <- S.printingOf s registry "Plains"
        altisaurPrinting <- S.printingOf s registry "Temple Altisaur"
        raptorPrinting <- S.printingOf s registry "Putrid Raptor"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        let base = S.landsInPlay plains 1
            (altisaur, g1) = S.addPermanent altisaurPrinting S.alice base
            (raptor, g2) = S.addPermanent raptorPrinting S.alice g1
            (piker, g3) = S.addPermanent pikerPrinting S.alice g2
            (theirs, g4) = S.addPermanent raptorPrinting S.bob g3
            (source, g5) = S.addPermanent pikerPrinting S.bob g4
        act altisaur raptor piker theirs source g5
  -- The behaviour and the printed clause's three narrowings, off ONE board: the
  -- only difference between the four readings below is which permanent the same
  -- 5 is aimed at.
  Spec.it s "CR 615.10 all but 1 of the 5 is prevented, and only for another Dinosaur alice controls"
    . withBoard
    $ \altisaur raptor piker theirs source board -> do
      let at victim = settleDamage S.identityAnswer board [hit source (Recipient.ToCreature victim) 5]
      Spec.assertEqWith s "setup: the shielded Dinosaur is a 4/4" (S.powerToughnessOf raptor board) (Just (4, 4))
      Spec.assertEqWith s "1 of the 5 is marked on the Dinosaur beside it" (S.damageOf raptor (at raptor)) (Just 1)
      Spec.assertEqWith s "the Altisaur is not \"another\", so its own 5 lands whole" (S.damageOf altisaur (at altisaur)) (Just 5)
      Spec.assertEqWith s "the Goblin Piker is no Dinosaur, so its 5 lands whole" (S.damageOf piker (at piker)) (Just 5)
      Spec.assertEqWith s "and bob's Dinosaur is not one alice controls" (S.damageOf theirs (at theirs)) (Just 5)
  -- The AMOUNT, one field of the event over: an event already at the floor is
  -- handed back untouched rather than shrunk or dropped, which is the half of
  -- the rewrite a "prevent all" reading cannot produce.
  Spec.it s "CR 615.10 an event already at the floor passes through whole"
    . withBoard
    $ \_ raptor _ _ source board -> do
      let after = settleDamage S.identityAnswer board [hit source (Recipient.ToCreature raptor) 1]
      Spec.assertEqWith s "the lone 1 is marked, nothing having been prevented" (S.damageOf raptor after) (Just 1)
  -- CR 615.10's last sentence -- the shield "will apply separately to damage
  -- from other applicable events that would happen at the same time" -- which is
  -- the rule stating the OPPOSITE of CR 615.7 for a static shield: two
  -- simultaneous events each keep their own 1, where a countdown of 1 would have
  -- covered one of them and asked which. Hence no OrderDamage: with no supply to
  -- allocate there is nothing to decide, and Replacement.contestedResource giving
  -- this rewrite one is what that negative catches.
  Spec.it s "CR 615.10 two simultaneous events each keep 1, and nothing is asked"
    . withBoard
    $ \_ raptor _ theirs source board -> do
      let batch = [hit source (Recipient.ToCreature raptor) 5, hit theirs (Recipient.ToCreature raptor) 3]
          after = settleDamage S.identityAnswer board batch
      Spec.assertEqWith s "1 from each event is marked, so 2 in all" (S.damageOf raptor after) (Just 2)
      Spec.assertEqWith s "and both events happened, each at 1" (fmap DamageEvent.amount (S.damageEventsOf after)) [1, 1]
      Spec.assertBool
        s
        (not (wasAskedToOrderDamage (answersFor S.identityAnswer board (Damage.applyDamage batch))))
        "no OrderDamage was raised: a static shield allocates nothing across a batch"
  -- CR 615.12 / 615.1a: the clause says "prevent", so this IS a prevention
  -- effect, and unpreventable damage is dealt in full. An instead-amount of 1
  -- would cut the Excruciator's 3 to 1 here; the pair of legs is the same 3 from
  -- two sources, one of which says its damage can't be prevented.
  Spec.it s "CR 615.12 the Excruciator's 3 lands whole, where an ordinary source's is cut to 1" $ do
    plains <- S.printingOf s registry "Plains"
    altisaurPrinting <- S.printingOf s registry "Temple Altisaur"
    raptorPrinting <- S.printingOf s registry "Putrid Raptor"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    excruciatorPrinting <- S.printingOf s registry "Excruciator"
    let base = S.landsInPlay plains 1
        (_altisaur, g1) = S.addPermanent altisaurPrinting S.alice base
        (raptor, g2) = S.addPermanent raptorPrinting S.alice g1
        (piker, g3) = S.addPermanent pikerPrinting S.bob g2
        (avatar, g4) = S.addPermanent excruciatorPrinting S.bob g3
        from src = settleDamage S.identityAnswer g4 [hit src (Recipient.ToCreature raptor) 3]
    Spec.assertEqWith s "the Excruciator's whole 3 is marked" (S.damageOf raptor (from avatar)) (Just 3)
    Spec.assertEqWith s "where the Piker's same 3 is cut to 1" (S.damageOf raptor (from piker)) (Just 1)

-- Ajani Steadfast {3}{W} Legendary Planeswalker -- Ajani, loyalty 4. "+1: Until
-- end of turn, up to one target creature gets +1/+1 and gains first strike,
-- vigilance, and lifelink. -2: Put a +1/+1 counter on each creature you control
-- and a loyalty counter on each other planeswalker you control. -7: You get an
-- emblem with 'If a source would deal damage to you or a planeswalker you
-- control, prevent all but 1 of that damage.'" (Name, cost, type line, loyalty
-- and all three loyalty abilities checked against api.scryfall.com 2026-08-29;
-- the card is transcribed whole, with nothing omitted.)
--
-- The emblem is the half this group exists for, and it is CR 615.10's shield
-- with the field Temple Altisaur's leaves empty: a DamagePattern whose PRINTED
-- recipient side names an object half ("a planeswalker you control") AND a
-- player half ("you"), which Replacement.matchesPrintedRecipient joins with
-- `or`. A conjunction would admit nothing, CR 120.3's two kinds of recipient
-- being disjoint, so the pair of positive legs below -- alice's life total and
-- alice's other planeswalker, off one row -- is what the disjunction buys.
-- CR 114.1 puts the emblem in the command zone and CR 114.4 is what makes its
-- ability function there.
--
-- Each leg moves exactly one thing off one board: the RECIPIENT across the
-- clause's three narrowings (bob's life total, bob's planeswalker, and alice's
-- CREATURE, which the clause names not at all), and the single ACT of whether
-- the ultimate was activated. Activating it costs Ajani his last loyalty
-- counter and CR 704.5i buries him, which is the game's own consequence rather
-- than a second knob.
--
-- Every number distinct: alice at 9 and bob at 7, alice's other planeswalker on
-- 8 loyalty counters and bob's on 6, Ajani on 7 (his ultimate's exact cost), the
-- hit 5 and the floor 1. So the floored readings are 8 and 7, the unfloored ones
-- 4 and 3, and a "prevent all" reading would leave 9 and 8; no two readings of
-- the rule land on the same number.
ajaniSteadfastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ajaniSteadfastSpec s registry = Spec.describe s "Ajani Steadfast (CR 114.4, CR 615.10)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      -- alice's precombat main phase with an empty stack, the window CR 606.3
      -- gives a loyalty ability. Loyalty counters are handed over directly
      -- rather than by casting, so each planeswalker's count is exactly the
      -- number named here and no CR 306.5b entry replacement is in play.
      withBoard act = do
        ajaniPrinting <- S.printingOf s registry "Ajani Steadfast"
        jacePrinting <- S.printingOf s registry "Jace Beleren"
        karnPrinting <- S.printingOf s registry "Karn Liberated"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        let base = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain}
            (ajani, g1) = S.addPermanent ajaniPrinting S.alice base
            (jace, g2) = S.addPermanent jacePrinting S.alice g1
            (karn, g3) = S.addPermanent karnPrinting S.bob g2
            (piker, g4) = S.addPermanent pikerPrinting S.alice g3
            (source, g5) = S.addPermanent pikerPrinting S.bob g4
            stocked =
              S.addCounter CounterKind.Loyalty 7 ajani
                . S.addCounter CounterKind.Loyalty 8 jace
                . S.addCounter CounterKind.Loyalty 6 karn
                $ g5
        act ajaniPrinting ajani jace karn piker source (atLife S.alice 9 (atLife S.bob 7 stocked))
  Spec.it s "CR 615.10 the emblem floors damage to alice and to her other planeswalker at 1, and reaches nothing else"
    . withBoard
    $ \printing ajani jace karn piker source base -> do
      let armed = loyaltyAbility 2 S.identityAnswer printing ajani base
          at recipient gs = settleDamage S.identityAnswer gs [hit source recipient 5]
      Spec.assertEqWith s "CR 120.3a 1 of the 5 reaches alice's life total: 9 - 1" (S.lifeOf S.alice (at (Recipient.ToPlayer S.alice) armed)) (Just 8)
      Spec.assertEqWith s "CR 120.3c and 1 loyalty counter comes off her other planeswalker: 8 - 1" (countersOn CounterKind.Loyalty jace (at (Recipient.ToPlaneswalker jace) armed)) 7
      Spec.assertEqWith s "CR 109.5 bob is outside the emblem's \"you\", so his 7 takes the whole 5" (S.lifeOf S.bob (at (Recipient.ToPlayer S.bob) armed)) (Just 2)
      Spec.assertEqWith s "and bob's planeswalker is not one alice controls: 6 - 5" (countersOn CounterKind.Loyalty karn (at (Recipient.ToPlaneswalker karn) armed)) 1
      Spec.assertEqWith s "the clause names no creature, so alice's own Piker is marked with the whole 5" (S.damageOf piker (at (Recipient.ToCreature piker) armed)) (Just 5)
      Spec.assertEqWith s "and with the ultimate never activated the same 5 takes alice to 4, so 5 really is unfloored here" (S.lifeOf S.alice (at (Recipient.ToPlayer S.alice) base)) (Just 4)
      Spec.assertEqWith s "taking 5 loyalty counters off her planeswalker: 8 - 5" (countersOn CounterKind.Loyalty jace (at (Recipient.ToPlaneswalker jace) base)) 3
      -- The fixture's own preconditions, after the behaviour so neither can
      -- absorb a mutation aimed at it.
      Spec.assertEqWith s "CR 114.2 one emblem, in the command zone" (Set.size (GameState.command armed)) 1
      Spec.assertEqWith s "and none on the board where the ultimate was never activated" (Set.size (GameState.command base)) 0
      Spec.assertEqWith s "setup: alice's other planeswalker holds 8 before any damage" (countersOn CounterKind.Loyalty jace armed) 8
      Spec.assertBool s (not (S.onBattlefield ajani armed)) "CR 704.5i Ajani paid his last loyalty counter for the ultimate and is buried"
      Spec.assertBool s (S.onBattlefield ajani base) "where the unactivated board still has him, at the seven counters he never spent"
  -- CR 120.4's damage EVENT, whose granularity is one source, one recipient, one
  -- moment -- and a sentence can name one recipient TWICE. Char ({2}{R} Instant,
  -- "Char deals 4 damage to any target and 2 damage to you" -- name, cost, type
  -- line and Oracle text checked against api.scryfall.com 2026-08-30) aimed at
  -- its own caster is that sentence: CR 608.2f makes its two clauses one action
  -- processed simultaneously, so alice's 4 and alice's 2 are one event of 6 and
  -- not two.
  --
  -- Nothing in the CR individuates simultaneous damage more finely than by
  -- source: CR 615.7 puts the allocation question only to damage "by two or more
  -- applicable SOURCES at the same time", CR 120.4a computes excess against
  -- "damage from other SOURCES that would be dealt at the same time", and CR
  -- 120.9 scopes a trigger's "damage dealt" to the sources named. CR 701.14c is
  -- the rule's own worked instance of the collapse -- a creature that fights
  -- ITSELF "deals damage to itself equal to TWICE its power", one blow rather
  -- than its power twice.
  --
  -- The emblem is the observer and CR 615.10 is why: its floor applies to each
  -- applicable event separately, so it is worth exactly the number of events. One
  -- event of 6 leaves alice 1 damage; two events would leave her 1 apiece.
  --
  -- Numbers all distinct -- 4 at the target, 2 at the caster, 6 in one event, the
  -- floor 1, alice at 9 and bob at 20 -- so no two readings land on the same
  -- board: one event reads 8, two events 7, an unfloored 3.
  Spec.it s "CR 120.4 one sentence naming alice twice deals her ONE event, of 6" $ do
    mountain <- S.printingOf s registry "Mountain"
    ajaniPrinting <- S.printingOf s registry "Ajani Steadfast"
    char <- S.printingOf s registry "Char"
    let base = (S.landsFor mountain S.alice 3 (Setup.emptyGame S.bothPlayers)) {GameState.phase = Phase.PrecombatMain}
        (ajani, g1) = S.addPermanent ajaniPrinting S.alice base
        (charId, unarmed) = S.addHandCard char S.alice (atLife S.alice 9 (S.addCounter CounterKind.Loyalty 7 ajani g1))
        armed = loyaltyAbility 2 S.identityAnswer ajaniPrinting ajani unarmed
        burn victim g = castAndResolve (preferTarget [victim]) g charId
        aimedAtAlice = burn (Recipient.ToPlayer S.alice) armed
        aimedAtBob = burn (Recipient.ToPlayer S.bob) armed
        control = burn (Recipient.ToPlayer S.alice) unarmed
    -- THE case, and the gameplay-level assertion: 4 and 2 aimed at one recipient
    -- are 6 in one event, which the emblem floors once. Two events would floor
    -- twice and take alice to 7.
    Spec.assertEqWith s "CR 120.3a the sentence's whole 6 is floored once: 9 - 1" (S.lifeOf S.alice aimedAtAlice) (Just 8)
    Spec.assertEqWith s "and one event was dealt, not two" (fmap DamageEvent.amount (S.damageEventsOf aimedAtAlice)) [1]
    -- The paired board, differing in exactly the target: aimed at bob the two
    -- clauses name two recipients and stay two events, so the merge is keyed to
    -- the RECIPIENT rather than flattening an instruction to one event.
    Spec.assertEqWith s "aimed at bob the clauses name two recipients: bob's 4 is unfloored, alice's 2 is floored to 1" (S.lifeOf S.bob aimedAtBob, S.lifeOf S.alice aimedAtBob) (Just 16, Just 8)
    -- Sorted, since what is asserted is that there are TWO events and what each
    -- came to, not the order the batch happened to be recorded in.
    Spec.assertEqWith s "so that board deals two events" (List.sort (fmap DamageEvent.amount (S.damageEventsOf aimedAtBob))) [1, 4]
    -- The unemblemed board, differing in exactly the emblem: the same one event
    -- of 6 is dealt whole, which is what makes the floor above the only thing
    -- separating 8 from 3.
    Spec.assertEqWith s "without the emblem the whole 6 lands: 9 - 6" (S.lifeOf S.alice control) (Just 3)
    Spec.assertEqWith s "in one event still" (fmap DamageEvent.amount (S.damageEventsOf control)) [6]
    -- The fixture's own preconditions, after the behaviour so neither can absorb
    -- a mutation aimed at it.
    Spec.assertEqWith s "CR 114.2 setup: the ultimate left one emblem in the command zone" (Set.size (GameState.command armed)) 1
    Spec.assertEqWith s "and the unemblemed board has none" (Set.size (GameState.command unarmed)) 0
    Spec.assertEqWith s "setup: alice is at 9 before the burn" (S.lifeOf S.alice armed) (Just 9)
  -- The -2, whose two instructions differ in every part: counter kind, the card
  -- type swept, and whether the source itself is included. "Each other" is
  -- spelled Filter.Not Filter.IsSource -- a SWEEP rather than a target, so the
  -- exclusion is read off the ability's own source as the instruction is
  -- reached -- and it is the only thing keeping Ajani from topping himself up.
  Spec.it s "CR 122.1e the -2 counters alice's creatures and every OTHER planeswalker she controls"
    . withBoard
    $ \printing ajani jace karn piker source base -> do
      let after = loyaltyAbility 1 S.identityAnswer printing ajani base
      Spec.assertEqWith s "alice's Piker takes a +1/+1 counter, so the 2/1 is a 3/2" (S.powerToughnessOf piker after) (Just (3, 2))
      Spec.assertEqWith s "CR 306.5c her other planeswalker gains one loyalty counter: 8 + 1" (countersOn CounterKind.Loyalty jace after) 9
      Spec.assertEqWith s "\"each other\" excludes Ajani, who only pays: 7 - 2" (countersOn CounterKind.Loyalty ajani after) 5
      Spec.assertEqWith s "CR 109.5 bob's planeswalker is untouched" (countersOn CounterKind.Loyalty karn after) 6
      Spec.assertEqWith s "and bob's Piker takes no +1/+1 counter, so it is still a 2/1" (S.powerToughnessOf source after) (Just (2, 1))
  -- The +1, whose four instructions all aim at ONE target slot. The answerer
  -- FILTERS the offered set rather than building a recipient by hand, so a slot
  -- the pool never offered cannot be smuggled past CR 608.2b's re-read.
  Spec.it s "the +1 pumps up to one target creature and hands it three keywords"
    . withBoard
    $ \printing ajani _ _ piker source base -> do
      let after = loyaltyAbility 0 (preferTarget [Recipient.ToCreature piker]) printing ajani base
      Spec.assertEqWith s "CR 613.4c the targeted 2/1 is a 3/2" (S.powerToughnessOf piker after) (Just (3, 2))
      Spec.assertBool s (Projection.hasKeyword Keyword.FirstStrike piker after) "CR 613.1f and it has first strike"
      Spec.assertBool s (Projection.hasKeyword Keyword.Vigilance piker after) "and vigilance"
      Spec.assertBool s (Projection.hasKeyword Keyword.Lifelink piker after) "and lifelink"
      Spec.assertEqWith s "CR 606.4 the cost put a loyalty counter on Ajani: 7 + 1" (countersOn CounterKind.Loyalty ajani after) 8
      Spec.assertEqWith s "bob's untargeted Piker is still a 2/1" (S.powerToughnessOf source after) (Just (2, 1))
      Spec.assertBool s (not (Projection.hasKeyword Keyword.FirstStrike source after)) "and has gained nothing"

-- Activate the nth loyalty ability of `walker` in printed order, resolve it, and
-- settle CR 704's state-based actions -- which is what buries a planeswalker
-- that paid its last loyalty counter (CR 704.5i). alice is the controller
-- throughout.
loyaltyAbility :: Int -> (forall r. Prompt.Prompt r -> r) -> Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
loyaltyAbility n answer printing walker gs =
  S.settleSba
    ( case drop n (Face.activatedAbilities (S.combinedFace printing)) of
        ability : _ -> S.runPure answer gs (do Activate.activateAbility S.alice walker ability; Stack.resolveTop)
        [] -> gs
    )

-- Protean Hydra {X}{G} Creature -- Hydra, printed 0/0: "this creature enters with
-- X +1/+1 counters on it. If damage would be dealt to this creature, prevent that
-- damage and remove that many +1/+1 counters from it."
--
-- Two rules this group is the pool's only producer of. CR 107.3m puts the SPELL's
-- announced X inside the permanent's own CR 614.1c entry replacement, an
-- exception the rule states against rule 107.3i and bounds in the same sentence
-- ("the value of X for that permanent is 0"); and CR 615.5's additional effect on a printed
-- ability names its own permanent by CR 113.7's reserved self slot, which is what
-- Pawl.Types.RemoveCounters needs and Stormwild Capridor's PutCounters (an
-- ObjectRef) does not.
--
-- Numbers all distinct: X is announced at 4, the Bolt is 3, one counter is left,
-- and the Hydra goes from 4/4 to 1/1. A permanent's own X of 0 would answer no
-- counters at all, an unrun rider would leave 4, and a per-event reading of "that
-- many" would leave 3.
proteanHydraSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
proteanHydraSpec s registry = Spec.describe s "Protean Hydra (CR 107.3m, CR 615.5)" $ do
  Spec.it s "CR 107.3m the X announced for the spell is the X its entry replacement reads" $ do
    forest <- S.printingOf s registry "Forest"
    hydraPrinting <- S.printingOf s registry "Protean Hydra"
    let (g1, spellId) = S.handOne hydraPrinting (S.landsInPlay forest 5)
        -- CR 400.7 mints a new object, but under the same id: the permanent the
        -- spell became is `spellId` on the battlefield.
        after = castAndResolve (answerXOf 4) g1 spellId
        -- CR 400.7 with CR 601.2a: casting moved the card to the stack, where it
        -- became a new object, and resolving moved it again -- so the permanent
        -- is neither `spellId` nor any of the five lands. It is what the
        -- battlefield gained.
        hydra = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield g1)) of
          oid : _ -> oid
          [] -> S.noSource
    Spec.assertEqWith s "four +1/+1 counters, the announced X and not the permanent's 0" (countersOn CounterKind.PlusOnePlusOne hydra after) 4
    Spec.assertEqWith s "so the printed 0/0 is a 4/4" (S.powerToughnessOf hydra after) (Just (4, 4))

  -- CR 615.5's rider on the same card, on a board where the Hydra is PLACED
  -- rather than cast: the counters are handed to it directly, so what the Bolt
  -- proves is the removal alone.
  Spec.it s "CR 615.5 the prevented three come off as three +1/+1 counters" $ do
    mountain <- S.printingOf s registry "Mountain"
    hydraPrinting <- S.printingOf s registry "Protean Hydra"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.landsInPlay mountain 1
        (hydra, g1) = S.addPermanent hydraPrinting S.alice base
        g2 = S.addCounter CounterKind.PlusOnePlusOne 4 hydra g1
        (g3, spellId) = S.handOne bolt g2
        after = castAndResolve (aimCreature hydra) g3 spellId
    Spec.assertEqWith s "setup: a 4/4" (S.powerToughnessOf hydra g3) (Just (4, 4))
    Spec.assertEqWith s "4 - 3: the rider found its own permanent through the self slot" (countersOn CounterKind.PlusOnePlusOne hydra after) 1
    Spec.assertEqWith s "CR 615.6: nothing was marked, the damage having been prevented" (S.damageOf hydra after) (Just 0)
    Spec.assertEqWith s "so the 4/4 is a 1/1" (S.powerToughnessOf hydra after) (Just (1, 1))

-- Announces this value of X and answers every other prompt with the identity
-- fallback. Pawl.CastSpec's answerXOf without its target arm, which the Hydra
-- has no use for -- it targets nothing.
answerXOf :: Natural.Natural -> Prompt.Prompt r -> r
answerXOf n p = case p of
  Prompt.ChooseX {} -> n
  _ -> S.identityAnswer p

-- CR 604.2's "as long as" clause on a PRINTED replacement ability, whose producer
-- is Jared Carthalion, True Heir ({R}{G}{W} Legendary Creature -- Human Warrior
-- 3/3): "If damage would be dealt to Jared Carthalion while you're the monarch,
-- prevent that damage and put that many +1/+1 counters on it."
--
-- THREE SEATS, because CR 725.3 makes the monarch a designation exactly one
-- player holds -- on a two-seat board "the monarch" and "your opponent" are the
-- same player, and a gate reading either would pass. Jared is BOB's, the Firebolt
-- is ALICE's, and the seat that is monarch on the negative board is CAROL's: a
-- gate reading the damage's controller, or reading merely that a monarch exists,
-- answers the same on both boards and so fails one of them.
--
-- Numbers distinct: the Firebolt is 2, the counters are 2 because CR 615.5's
-- "that many" says so, and Jared's 3/3 becomes 5/5. A "one counter per event"
-- reading would answer 1 and an unrun rider 0.
jaredCarthalionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
jaredCarthalionSpec s registry = Spec.describe s "Jared Carthalion, True Heir (CR 604.2)" $ do
  -- alice holds `n` Firebolts over one Mountain apiece, bob's Jared is on the
  -- battlefield, and carol is the third seat. Jared is placed rather than cast,
  -- so his own CR 725.1 enters trigger never fires and each case names the
  -- monarch itself.
  let board n = do
        mountain <- S.printingOf s registry "Mountain"
        jaredPrinting <- S.printingOf s registry "Jared Carthalion, True Heir"
        firebolt <- S.printingOf s registry "Firebolt"
        let withLands = S.landsFor mountain S.alice n S.threePlayerGame
            (jared, g1) = S.addPermanent jaredPrinting S.bob withLands
            addOne (ids, g) _ = let (oid, g') = S.addHandCard firebolt S.alice g in (ids <> [oid], g')
            (bolts, g2) = List.foldl' addOne ([], g1) [1 .. n]
        pure
          ( jared,
            bolts,
            g2
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
          )
  -- THE PROVING TEST. One board, two monarchs, and nothing else differs.
  Spec.it s "CR 604.2 the clause is asked of the ability's controller, not of whoever the monarch is" $ do
    (jared, bolts, g) <- board 1
    let cast gs = case bolts of
          [bolt] -> castAndResolve (aimCreature jared) gs bolt
          _ -> gs
        heldByBob = cast (S.withMonarch S.bob g)
        heldByCarol = cast (S.withMonarch S.carol g)
    Spec.assertEqWith s "setup: Jared is a 3/3" (S.powerToughnessOf jared g) (Just (3, 3))
    -- CR 615.6: the prevented event never happened, so nothing is marked.
    Spec.assertEqWith s "bob is the monarch, so no damage is marked" (S.damageOf jared heldByBob) (Just 0)
    Spec.assertEqWith s "and CR 615.5's rider puts that many +1/+1 counters on" (countersOn CounterKind.PlusOnePlusOne jared heldByBob) 2
    Spec.assertEqWith s "so Jared is a 5/5" (S.powerToughnessOf jared heldByBob) (Just (5, 5))
    -- carol holds the crown on the other board: bob is no more Jared's monarch
    -- than alice is, so the ability does not apply at all.
    Spec.assertEqWith s "carol is the monarch, so the same 2 is marked in full" (S.damageOf jared heldByCarol) (Just 2)
    Spec.assertEqWith s "and no counter is put on" (countersOn CounterKind.PlusOnePlusOne jared heldByCarol) 0
    Spec.assertEqWith s "so Jared is still a 3/3" (S.powerToughnessOf jared heldByCarol) (Just (3, 3))
  -- CR 604.1's "simply true", which is what makes the clause a live read rather
  -- than a latch: the crown changes hands with no trigger and no resolution in
  -- between, and the SAME permanent's ability stops applying. A gate snapshotted
  -- when the ability was gathered -- or when the permanent entered -- would
  -- prevent the second Firebolt too.
  Spec.it s "CR 604.1 the clause is re-asked, so losing the crown turns the ability off" $ do
    (jared, bolts, g) <- board 2
    case bolts of
      [first, second] -> do
        let shielded = castAndResolve (aimCreature jared) (S.withMonarch S.bob g) first
            dethroned = castAndResolve (aimCreature jared) (S.withMonarch S.carol shielded) second
        Spec.assertEqWith s "the first Firebolt is prevented while bob wears the crown" (S.damageOf jared shielded) (Just 0)
        Spec.assertEqWith s "leaving two +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne jared shielded) 2
        Spec.assertEqWith s "the second lands in full once carol has it" (S.damageOf jared dethroned) (Just 2)
        Spec.assertEqWith s "and adds no third counter" (countersOn CounterKind.PlusOnePlusOne jared dethroned) 2
      _ -> Spec.assertFailure s "fixture should hold two Firebolts"

-- CR 613.1f's NAMED removal aimed at a printed PREVENTION ability, whose producer
-- is Glittering Lion ({2}{W} Creature -- Cat 2/2, Prophecy): "Prevent all damage
-- that would be dealt to this creature. {3}: Until end of turn, this creature
-- loses 'Prevent all damage that would be dealt to this creature.' Any player may
-- activate this ability."
--
-- The first sentence is the ability being removed, and CR 614.1 / 615.1 make it a
-- static ability's continuous effect rather than an activated or a triggered one
-- -- so the name the second sentence quotes hangs on PrintedReplacement.name,
-- where Gliding Licid's hangs on ActivatedAbility.name. The two are the whole of
-- what Modification.LoseNamedAbility reaches.
--
-- The card's LAST sentence, "Any player may activate this ability", is CR
-- 602.1b's activation instruction and pawl carries it on
-- ActivatedAbility.activator; the case below where bob activates alice's Lion is
-- what proves it at gameplay level, and Pawl.ActivateSpec's "Any player may
-- activate" group is where the offer itself is read.
--
-- Two seats because the damage needs a source that is not the Lion; three lands
-- for each seat because {3} is the whole cost and both seats activate it here; a
-- 2-power source against a 2/2 so one number decides both the prevention and the
-- lethality -- a 1-power source would leave the Lion alive under either
-- implementation.
glitteringLionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
glitteringLionSpec s registry = Spec.describe s "Glittering Lion (CR 613.1f)" $ do
  let board = do
        plains <- S.printingOf s registry "Plains"
        lionPrinting <- S.printingOf s registry "Glittering Lion"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        let base = S.landsFor plains S.bob 3 (S.landsInPlay plains 3)
            (lion, g1) = S.addPermanent lionPrinting S.alice base
            (attacker, g2) = S.addPermanent pikerPrinting S.bob g1
        pure (lion, attacker, g2 {GameState.priority = Just S.alice})
      -- The Piker's 2, as noncombat damage: the printed pattern names no kind, so
      -- either kind would do, and this is the funnel the Capridor cases above use.
      hits attacker lion = [DamageEvent.MkDamageEvent attacker (Recipient.ToCreature lion) 2 False False False 0 Nothing DamageKind.Noncombat]
      -- Damage, then CR 704.3's check -- the zone is what these cases assert, so
      -- the SBAs have to run or a dead Lion still reads as being on the
      -- battlefield.
      dealAndCheck lion attacker gs = S.settleSba (settleDamage S.identityAnswer gs (hits attacker lion))
  -- The CONTROL, and what stops the case below passing vacuously: a Lion that
  -- never had the prevention would die on both boards.
  Spec.it s "CR 614.1 the printed ability prevents the 2 that would otherwise be lethal" $ do
    (lion, attacker, g) <- board
    let after = dealAndCheck lion attacker g
    Spec.assertBool s (S.onBattlefield lion after) "the 2/2 survives 2 damage, because all of it is prevented"
    Spec.assertEqWith s "with nothing marked on it (CR 615.6)" (S.damageOf lion after) (Just 0)
  -- THE PROVING CASE. The same board and the same 2 damage, with the Lion's own
  -- {3} resolved in between. A removal reaching only PC.activatedAbilities leaves
  -- the prevention standing and the Lion alive -- the two implementations
  -- disagree about WHERE THE LION IS, which is gameplay level.
  Spec.it s "CR 613.1f after {3} the Lion loses its own prevention, so the same 2 kills it" $ do
    (lion, attacker, g) <- board
    lionPrinting <- S.printingOf s registry "Glittering Lion"
    case Face.activatedAbilities (S.combinedFace lionPrinting) of
      [] -> Spec.assertFailure s "Glittering Lion should print one activated ability"
      ability : _ -> do
        let activated = S.runPure S.identityAnswer g (Activate.activateAbility S.alice lion ability)
            resolved = S.runPure S.identityAnswer activated Stack.resolveTop
            after = dealAndCheck lion attacker resolved
        -- By NAME and over the whole graveyard, not by the battlefield id: CR
        -- 111.7's new object means the destroyed Lion is not `lion` any more, and
        -- the list rules out a graveyard that gained something else instead.
        Spec.assertEqWith s "CR 704.5g destroys the 2/2, its prevention gone, and the Lion alone is in alice's graveyard" (graveyardNames S.alice after) [CardName.MkCardName (Text.pack "Glittering Lion")]
        Spec.assertBool s (not (S.onBattlefield lion after)) "so it has left the battlefield"
        -- The SCOPE of the removal, after the behaviour: the {3} names one
        -- ability, so the Lion's own activated ability is untouched by it.
        Spec.assertEqWith s "CR 613.1f and the removal took one ability rather than every one" (length (Projection.abilitiesOf lion resolved)) 1
        -- CR 602.2b: the {3} was really paid, so the removal above is the
        -- ability's effect rather than something a free activation produced.
        Spec.assertEqWith s "CR 602.2b: and the three lands paid for it" (S.tappedCount S.alice resolved) 3

  -- THE CASE THE PRINTED CLAUSE EXISTS FOR, and the whole point of the card: the
  -- player whose creature is trying to kill the Lion is the one who turns the
  -- shield off. The activation is taken from bob's own MENU rather than handed to
  -- Activate.activateAbility directly -- an ability CR 602.2's default withheld
  -- is one this case cannot activate at all, and then the Lion survives and the
  -- graveyard assertion below is what says so.
  Spec.it s "CR 602.1b bob activates alice's Lion, and his Piker's 2 then kills it" $ do
    (lion, attacker, g) <- board
    let offered = g {GameState.priority = Just S.bob, GameState.phase = Phase.PrecombatMain}
        offers = [ab | Action.Activate o ab <- Action.legalActions S.bob offered, o == lion]
        step gs ability = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (Activate.activateAbility S.bob lion ability)) Stack.resolveTop
        resolved = List.foldl' step offered offers
        after = dealAndCheck lion attacker resolved
    -- By NAME and over the whole graveyard, as the case above: CR 111.7's new
    -- object means the destroyed Lion is not `lion` any more.
    Spec.assertEqWith s "CR 704.5g the 2 kills the 2/2 whose opponent turned its prevention off" (graveyardNames S.alice after) [CardName.MkCardName (Text.pack "Glittering Lion")]
    Spec.assertBool s (not (S.onBattlefield lion after)) "so it has left the battlefield"
    -- CR 602.1a: the activating player pays. bob's three lands are spent and
    -- alice's three are not, which is also what rules out the offer having been
    -- served to alice under bob's name.
    Spec.assertEqWith s "CR 602.1a bob's three lands paid for it" (S.tappedCount S.bob resolved) 3
    Spec.assertEqWith s "and its controller's are untouched" (S.tappedCount S.alice resolved) 0

-- CR 615.12's damage that "can't be prevented", whose one producer in the pool
-- is Spider-Punk ({1}{R} Legendary Creature -- Spider Human Hero 2/1, Marvel's
-- Spider-Man 92), set against a COUNTDOWN shield, Mending Hands ("Prevent the
-- next 4 damage that would be dealt to any target this turn"). Fog and Selfless
-- Squire install prevention rows too, but CR 615.7's remaining amount is what
-- clause 3 is about, and the countdown producers in data/cards/ are Mending
-- Hands, Healing Grace, Test of Faith and Decorated Griffin -- Mending Hands
-- being the one that narrows nothing else.
--
-- The rule's first and third clauses: the damage is dealt in full though an
-- applicable shield is there, and "existing damage prevention shields won't be
-- reduced by damage that can't be prevented" -- the shield read here is the
-- countdown amount that sentence names, and every case below asserts it
-- untouched, which is what keeps the middle clause's application from
-- over-reaching into it. The MIDDLE clause is shieldCounterSpec's, where CR
-- 122.1c's amount-independent counter removal makes it observable.
--
-- The MIDDLE clause's other half -- CR 615.5's authored rider, which an
-- amount-scaled one (testOfFaithSpec above) cannot show at a prevented amount of
-- 0 -- is phantomTigerSpec's, on this same Spider-Punk pairing.
--
-- EVERY case here has a CONTROL on a board that differs in Spider-Punk and in
-- nothing else, so no assertion can pass because the damage would have got
-- through anyway: the control board's shield genuinely prevents it. The first
-- two cases are the two halves of one comparison, one board each; the third runs
-- literally the same script over both.
--
-- The DAMAGE BATCHES are hand-built and the SPELL is not, for mendingHandsSpec's
-- reason: casting Mending Hands for real is what proves the card, while reaching
-- a real two-attacker combat batch would mean driving a whole combat phase to
-- produce a fixture these assertions read straight off.
spiderPunkSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spiderPunkSpec s registry = Spec.describe s "Spider-Punk (CR 615.12)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- alice's Piker is shielded by a Mending Hands she really casts on it;
      -- bob's TWO Pikers are the sources of the hand-built events. Two of them
      -- rather than one because CR 615.7's choice clause is conditioned on
      -- "damage ... by two or more applicable sources at the same time", which
      -- one source repeated does not satisfy on the rule's letter.
      --
      -- The Spider-Punks ride out as a LIST -- singleton or empty -- so both
      -- boards can be driven by one script, and destroying "the Punks" is a
      -- no-op on the control.
      withBoard act = do
        plains <- S.printingOf s registry "Plains"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        mendingHands <- S.printingOf s registry "Mending Hands"
        punkPrinting <- S.printingOf s registry "Spider-Punk"
        let build withPunk =
              let base = S.landsInPlay plains 1
                  (victim, g1) = S.addPermanent pikerPrinting S.alice base
                  (attacker, g2) = S.addPermanent pikerPrinting S.bob g1
                  (other, g3) = S.addPermanent pikerPrinting S.bob g2
                  (punk, g4) = S.addPermanent punkPrinting S.alice g3
                  (punks, g5) = if withPunk then ([punk], g4) else ([], g3)
                  (g6, spellId) = S.handOne mendingHands g5
               in (victim, attacker, other, punks, castAndResolve (aimCreature victim) g6 spellId)
        act build
  -- THE CONTROL. Without Spider-Punk the shield does its ordinary CR 615.7 job:
  -- the whole 3 is prevented, the event never happens (CR 615.6), and 1 of the
  -- shield's 4 is left. Every refusal below would be true of a board whose
  -- shield had never been there at all if this case did not pass.
  Spec.it s "CR 615.7 without Spider-Punk the shield prevents the whole 3"
    . withBoard
    $ \build -> do
      let (victim, attacker, _, _, shielded) = build False
          after = settleDamage S.identityAnswer shielded [hit attacker (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the shield is a floating replacement" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "nothing is marked on the shielded creature" (S.damageOf victim after) (Just 0)
      Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
      Spec.assertEqWith s "3 of the shield's 4 were spent, so 1 remains" (shieldsLeft after) [1]
  -- CR 615.12's first and third clauses on one board. The shield is applicable
  -- and is still applied -- CR 615.12a gives it exactly one application, which
  -- is why this terminates -- but it prevents none of the damage, and it is not
  -- reduced by damage it could not prevent.
  Spec.it s "CR 615.12 with Spider-Punk the same 3 lands in full, and the shield is not reduced"
    . withBoard
    $ \build -> do
      let (victim, attacker, _, _, shielded) = build True
          after = settleDamage S.identityAnswer shielded [hit attacker (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the same shield is on the same creature" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "the whole 3 is marked on the shielded creature" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and the event happened, at its full amount" (amounts after) [3]
      Spec.assertEqWith s "the shield still holds all 4" (shieldsLeft after) [4]
  -- The third clause made GAMEPLAY-observable, which the row read above is not:
  -- one script over both boards -- take 3, lose the Punks (CR 604.2), take 2 --
  -- and the shield that was never reduced still covers the 2, where the control's
  -- spent shield cannot.
  Spec.it s "CR 615.12 the unreduced shield still covers the next 2 once Spider-Punk is gone"
    . withBoard
    $ \build -> do
      let script (victim, attacker, _, punks, shielded) =
            let first_ = settleDamage S.identityAnswer shielded [hit attacker (Recipient.ToCreature victim) 3]
                gone = S.runPure S.identityAnswer first_ (Event.destroy Regenerability.Regenerable punks)
             in settleDamage S.identityAnswer gone [hit attacker (Recipient.ToCreature victim) 2]
          punkBoard@(punkVictim, _, _, _, _) = build True
          control@(controlVictim, _, _, _, _) = build False
      Spec.assertEqWith s "with Spider-Punk only the first 3 is marked: the 2 is prevented whole" (S.damageOf punkVictim (script punkBoard)) (Just 3)
      Spec.assertEqWith s "and 2 of the shield's untouched 4 are left" (shieldsLeft (script punkBoard)) [2]
      Spec.assertEqWith s "without it the first 3 was prevented, so only 1 of the 2 is" (S.damageOf controlVictim (script control)) (Just 1)
      Spec.assertEqWith s "and that shield is spent to 0 and gone" (shieldsLeft (script control)) []
  -- The ELISION half, and the CR 615.7 prompt's other gate: two applicable
  -- sources deal damage to the shielded creature at the same time, and the rule
  -- gives its controller the choice of which the shield prevents -- but only
  -- when that choice can change the board. It cannot here, since an
  -- unpreventable batch costs THIS shield nothing in any order (CR 615.12's last
  -- sentence), so nothing is asked and the whole 8 lands either way. A CR 122.1c
  -- shield counter is the opposite and is asked about, since the rule's middle
  -- clause spends it either way -- shieldCounterSpec's CR 101.4c pair.
  Spec.it s "CR 615.12 / 615.7 an unpreventable batch asks the shielded creature's controller nothing"
    . withBoard
    $ \build -> do
      let (victim, attacker, other, _, shielded) = build True
          (controlVictim, controlAttacker, controlOther, _, controlShielded) = build False
          batch = [hit attacker (Recipient.ToCreature victim) 5, hit other (Recipient.ToCreature victim) 3]
          controlBatch = [hit controlAttacker (Recipient.ToCreature controlVictim) 5, hit controlOther (Recipient.ToCreature controlVictim) 3]
      Spec.assertBool
        s
        (wasAskedToOrderDamage (answersFor S.identityAnswer controlShielded (Damage.applyDamage controlBatch)))
        "setup: without Spider-Punk 4 cannot cover 5 and 3, so alice is asked"
      Spec.assertBool
        s
        (not (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch))))
        "no OrderDamage was raised: no order of unpreventable damage spends the shield"
      let after = settleDamage S.identityAnswer shielded batch
      Spec.assertEqWith s "both events happened in full" (amounts after) [5, 3]
      Spec.assertEqWith s "the whole 8 is marked" (S.damageOf victim after) (Just 8)
      Spec.assertEqWith s "and the shield is untouched" (shieldsLeft after) [4]

-- CR 615.12's MIDDLE clause on an AUTHORED rider: an inapplicable prevention is
-- still applied, "those effects won't prevent any damage, but any additional
-- effects they have will take place". Phantom Tiger ({2}{G} Creature -- Cat
-- Spirit 1/0: "This creature enters with two +1/+1 counters on it. If damage
-- would be dealt to this creature, prevent that damage. Remove a +1/+1 counter
-- from this creature." -- Oracle text fetched from Scryfall 2026-08-28) is the
-- producer, and the only shape of producer that can be one: its removal is a CR
-- 615.5 rider of the prevention effect itself and is AMOUNT-INDEPENDENT, so it
-- is visible when the amount prevented is 0. shieldCounterSpec below proves the
-- same clause for the rules-MINTED half, CR 122.1c's counter; this is the
-- AUTHORED half, which travels on the candidate rather than on the rewrite.
--
-- An amount-SCALED rider is applied to unpreventable damage just the same and
-- does nothing, CR 615.5's "amount of damage that was prevented" being 0 -- Test
-- of Faith, Stormwild Capridor, Protean Hydra, Brace for Impact, Divine
-- Deflection and Inkshield all read the amount that way. That is why none of
-- them can show this clause and why this card can.
--
-- NOT a CR 615.13 trigger, which is the distinction the whole group turns on:
-- "when damage is prevented this way" triggers only where the application
-- "prevents some or all of that damage", so Phyrexian Vindicator must stay
-- silent on exactly the board that moves the Tiger's counter. That pair is the
-- last case here.
--
-- Set against Spider-Punk, the pool's producer of damage that can't be prevented
-- (spiderPunkSpec). Every case is a PAIR of boards differing in Spider-Punk and
-- in nothing else.
--
-- Numbers all distinct: the Bolt is 3, the Tiger is placed with 4 counters, the
-- rider takes 1, and the printed body is a 1/0. Four is the value that makes the
-- removal decide the Tiger's LIFE -- a 5/4 survives 3 damage and the 4/3 the
-- rider leaves does not -- so no reading of the rule meets another on it: an
-- unrun rider leaves a live 5/4, and a rider run twice would leave a 3/2.
phantomTigerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phantomTigerSpec s registry = Spec.describe s "Phantom Tiger (CR 615.12)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      named = CardName.MkCardName . Text.pack
  -- CR 614.1c's entry replacement first, so the counters the cases below hand
  -- the Tiger directly are the ones its own card puts there.
  Spec.it s "CR 614.1c it enters with two +1/+1 counters, so the printed 1/0 is a 3/2" $ do
    forest <- S.printingOf s registry "Forest"
    tigerPrinting <- S.printingOf s registry "Phantom Tiger"
    let (g1, spellId) = S.handOne tigerPrinting (S.landsInPlay forest 3)
        after = castAndResolve S.identityAnswer g1 spellId
        -- CR 400.7 with CR 601.2a: casting and resolving each minted a new
        -- object, so the permanent is neither `spellId` nor one of the lands. It
        -- is what the battlefield gained (proteanHydraSpec's reading).
        tiger = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield g1)) of
          oid : _ -> oid
          [] -> S.noSource
    Spec.assertEqWith s "two +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne tiger after) 2
    Spec.assertEqWith s "so the printed 1/0 is a 3/2" (S.powerToughnessOf tiger after) (Just (3, 2))
  -- THE CASE. One Lightning Bolt at one Phantom Tiger, on two boards differing
  -- in Spider-Punk alone. With him the 3 cannot be prevented and lands whole,
  -- and the rider still takes the counter that makes it lethal; without him the
  -- same 3 is prevented (CR 615.6) and the same counter still comes off.
  Spec.it s "CR 615.12 the rider takes its counter off damage the prevention could not prevent" $ do
    mountain <- S.printingOf s registry "Mountain"
    tigerPrinting <- S.printingOf s registry "Phantom Tiger"
    punkPrinting <- S.printingOf s registry "Spider-Punk"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let build withPunk =
          let base = S.landsInPlay mountain 1
              (cat, g1) = S.addPermanent tigerPrinting S.alice base
              g2 = S.addCounter CounterKind.PlusOnePlusOne 4 cat g1
              (_, g3) = S.addPermanent punkPrinting S.alice g2
              (g4, spellId) = S.handOne bolt (if withPunk then g3 else g2)
           in (cat, g4, S.runPure (aimCreature cat) g4 (S.cast S.alice spellId >> Stack.resolveTop >> Engine.settleForPriority))
        (tiger, before, after) = build True
        (controlTiger, _, control) = build False
    Spec.assertEqWith s "setup: four +1/+1 counters make the printed 1/0 a 5/4" (S.powerToughnessOf tiger before) (Just (5, 4))
    -- The gameplay assertion, ahead of every proxy: the counter the rider took
    -- is what left the Tiger a 4/3 under 3 damage, so CR 704.5g destroys it. An
    -- application that ran no rider leaves a live 5/4 here.
    Spec.assertEqWith s "the Tiger is destroyed by damage its own prevention could not prevent" (graveyardNames S.alice after) [named "Lightning Bolt", named "Phantom Tiger"]
    Spec.assertBool s (not (S.onBattlefield tiger after)) "so it has left the battlefield"
    -- THE CONTROL, differing in Spider-Punk alone: the prevention does its
    -- ordinary job, and the rider is unchanged by that -- CR 615.5 runs it
    -- either way, which is what stops the case above passing for a board where
    -- riders fire only on unpreventable damage.
    Spec.assertBool s (S.onBattlefield controlTiger control) "without Spider-Punk the same Bolt is prevented and the Tiger lives"
    Spec.assertEqWith s "CR 615.6: with nothing marked on it" (S.damageOf controlTiger control) (Just 0)
    Spec.assertEqWith s "and one counter gone all the same, so a 4/3" (S.powerToughnessOf controlTiger control) (Just (4, 3))
    Spec.assertEqWith s "CR 615.12a: exactly one counter, so the application happened once" (countersOn CounterKind.PlusOnePlusOne controlTiger control) 3
  -- CR 615.1's printed RECIPIENT, on the unpreventable board: the ability covers
  -- "this creature", so a Bolt at the Piker beside it is not an application at
  -- all and no counter moves. Without this, "any unpreventable damage runs every
  -- rider on the board" would pass the case above.
  Spec.it s "CR 615.1 unpreventable damage to something else applies nothing and moves no counter" $ do
    mountain <- S.printingOf s registry "Mountain"
    tigerPrinting <- S.printingOf s registry "Phantom Tiger"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    punkPrinting <- S.printingOf s registry "Spider-Punk"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.landsInPlay mountain 1
        (tiger, g1) = S.addPermanent tigerPrinting S.alice base
        g2 = S.addCounter CounterKind.PlusOnePlusOne 4 tiger g1
        (piker, g3) = S.addPermanent pikerPrinting S.alice g2
        (_, g4) = S.addPermanent punkPrinting S.alice g3
        (g5, spellId) = S.handOne bolt g4
        after = S.runPure (aimCreature piker) g5 (S.cast S.alice spellId >> Stack.resolveTop >> Engine.settleForPriority)
    Spec.assertEqWith s "the Tiger keeps all four counters, so it is still a 5/4" (S.powerToughnessOf tiger after) (Just (5, 4))
    Spec.assertEqWith s "all four counters, none of them spent on somebody else's damage" (countersOn CounterKind.PlusOnePlusOne tiger after) 4
    Spec.assertBool s (not (S.onBattlefield piker after)) "and the Piker the Bolt did name is dead"
  -- CR 615.13's control, and the one that fixes WHICH clause the fix implements.
  -- Phyrexian Vindicator prints the same PreventAll over itself, but its extra
  -- effect is a TRIGGERED ability -- "when damage is prevented this way" -- and
  -- rule 615.13 conditions such a trigger on the application having "prevented
  -- some or all of that damage". Against Spider-Punk it prevents none, so it must
  -- stay silent on the very board where the Tiger's CR 615.5 rider runs. Two
  -- boards differing in Spider-Punk alone; the control fires it.
  Spec.it s "CR 615.13 a 'prevented this way' trigger stays silent where the CR 615.5 rider runs" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    vindicatorPrinting <- S.printingOf s registry "Phyrexian Vindicator"
    punkPrinting <- S.printingOf s registry "Spider-Punk"
    let build withPunk =
          let base = S.landsInPlay plains 1
              (attacker, g1) = S.addPermanent pikerPrinting S.bob base
              (horror, g2) = S.addPermanent vindicatorPrinting S.alice g1
              (_, g3) = S.addPermanent punkPrinting S.alice g2
           in (horror, strikeAndSettleWith (preferTarget [Recipient.ToPlayer S.bob]) (if withPunk then g3 else g2) [hit attacker (Recipient.ToCreature horror) 4])
        (vindicator, (punkDealt, punkAfter)) = build True
        (controlVindicator, (controlDealt, controlAfter)) = build False
    -- The STACK is the observation here, first and deliberately, and it is the
    -- behavioural one rather than a proxy: a CR 615.13 trigger that fired off an
    -- inert application reads a prevented amount of 0, so every payload such a
    -- trigger can print -- the Vindicator's "that much" damage, Selfless Squire's
    -- "that many" counters -- does nothing at all once it resolves. What a
    -- spurious trigger DOES do is exist: it goes on the stack, its controller
    -- owes it a target, and CR 603.3b orders it against everything else the
    -- boundary gathered. The two life-and-damage assertions behind it are the
    -- vacuity guards, and the control below is what shows a real prevention
    -- still fires it and still reaches bob.
    Spec.assertEqWith s "no trigger was gathered: CR 615.13 wants some of the damage prevented, and none was" (length (GameState.stack punkDealt)) 0
    Spec.assertEqWith s "so bob takes nothing" (S.lifeOf S.bob punkAfter) (Just 20)
    Spec.assertEqWith s "and the whole 4 is marked on the Vindicator instead" (S.damageOf vindicator punkAfter) (Just 4)
    -- THE CONTROL, differing in Spider-Punk alone: the same 4 off the same card,
    -- and the trigger fires and lands.
    Spec.assertEqWith s "without Spider-Punk the prevented 4 reaches bob" (S.lifeOf S.bob controlAfter) (Just 16)
    Spec.assertEqWith s "CR 615.6: and none of it is marked on the Vindicator" (S.damageOf controlVindicator controlAfter) (Just 0)
    Spec.assertEqWith s "one trigger was gathered" (length (GameState.stack controlDealt)) 1

-- The players asked to decide something while a damage batch settles, in the
-- order they were asked. Both batch-level questions count: CR 616.1's "which
-- effect applies next" and CR 615.7's "which damage does the shield prevent".
--
-- The prompt STREAM rather than the board, because there is no board that can
-- tell these two apart: the two players choose about DIFFERENT objects -- each
-- orders the effects hitting their own creature -- so neither answer constrains
-- the other and the same permanents end up in the same state whoever was asked
-- first. What CR 616.1's last sentence governs is which player is asked, and
-- under CR 101.4b that is information the later chooser gets to have.
--
-- Not a claim about Magic: an effect reachable from two players' events at once
-- and spendable only once WOULD make this board-visible, and pawl has no such
-- effect to build with today. Every replacement the pool can produce is
-- unlimited (Furnace of Rath), names one recipient (Mending Hands), or describes
-- a set of recipients that is one player's own (Divine Deflection's "you and/or
-- permanents you control") -- and the third is still one chooser's.
choosersAsked :: GameState.GameState -> [DamageEvent.DamageEvent] -> [PlayerId.PlayerId]
choosersAsked gs batch =
  let step :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
      step p = case p of
        Prompt.ChooseReplacement _ pid _ -> do
          State.modify' (<> [pid])
          pure 0
        Prompt.OrderDamage _ pid events -> do
          State.modify' (<> [pid])
          pure (zipWith const [0 ..] events)
        _ -> pure (S.identityAnswer p)
   in State.execState (Engine.runGame step gs (Damage.applyDamage batch)) []

-- A Furnace of Rath under alice, one Goblin Piker each side, and a Mending Hands
-- shield on both creatures -- so every event addressed to either creature has
-- two applicable effects that differ, and both controllers owe a CR 616.1
-- choice. Answers the state, alice's creature and bob's.
doubledAndShielded :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
doubledAndShielded plains piker furnace mendingHands =
  let base = S.landsInPlay plains 2
      (_, g1) = S.addPermanent furnace S.alice base
      (hers, g2) = S.addPermanent piker S.alice g1
      (his, g3) = S.addPermanent piker S.bob g2
      (g4, firstShield) = S.handOne mendingHands g3
      (g5, secondShield) = S.handOne mendingHands g4
   in (castAndResolve (aimCreature his) (castAndResolve (aimCreature hers) g5 firstShield) secondShield, hers, his)

-- Spend a prevention shield on `src`'s hit first (CR 615.7), and take the shield
-- over `furnace` whenever both are offered (CR 616.1). The second half is named
-- as "not the Furnace" because a floating row's `source` is the spell that
-- installed it -- a CR 608.2n object no fixture holds an id for -- while the
-- permanent's row is the Furnace itself. Pinning it is what makes the amounts
-- the rule's rather than an artefact of which candidate is canonical.
--
-- Top-level rather than a `where` binding, for settleDamage's reason and one
-- more: MonoLocalBinds (implied by GADTs, on at the top of this module) declines
-- to generalize a local binding that closes over a local, which `furnace` is.
allocateShield :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
allocateShield furnace src p = case p of
  Prompt.OrderDamage _ _ events ->
    let key e = (DamageEvent.source e /= src, DamageEvent.source e)
     in fmap fst (List.sortOn (key . snd) (zip [0 ..] events))
  Prompt.ChooseReplacement _ _ entries ->
    maybe 0 Int.toNaturalSaturating (List.findIndex ((/= furnace) . ReplacementEntry.source) entries)
  _ -> S.identityAnswer p

-- CR 616.1's last sentence: "If two or more players have to make these choices
-- at the same time, choices are made in APNAP order (see rule 101.4)."
--
-- The board that reaches it needs one batch whose events are addressed to two
-- players' objects, and TWO DISTINGUISHABLE applicable effects per event --
-- otherwise `choose` elides the prompt and nobody is asked anything. Furnace of
-- Rath ({1}{R}{R}{R} Enchantment, "If a source would deal damage to a permanent
-- or player, it deals double that damage to that permanent or player instead")
-- is symmetric, so it supplies one candidate to every event in the batch; a
-- Mending Hands on each creature supplies the second, and a doubler and a shield
-- differ in `effect`, so neither pair is elided.
--
-- Two Furnaces would NOT do it, which is the trap this fixture avoids: two
-- copies carry the same ReplacementEffect.DamageR, `distinguishing` finds them
-- interchangeable and the prompt is correctly elided. The rule needs candidates
-- that differ, not merely candidates that are several.
--
-- The DAMAGE BATCH is hand-built and the SPELLS are not, for mendingHandsSpec's
-- reason -- and here the batch's ORDER is the input under test, which only a
-- hand-built batch can state.
apnapSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
apnapSpec s registry = Spec.describe s "APNAP (CR 616.1)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
  -- Both batch orders, because only the PAIR discriminates: settling the batch
  -- in gather order already answers [alice, bob] when alice's event happens to
  -- come first, and would answer [bob, alice] when it does not.
  Spec.it s "CR 616.1 two players choosing for one batch are asked in APNAP order" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    furnace <- S.printingOf s registry "Furnace of Rath"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let (shielded, hers, his) = doubledAndShielded plains pikerPrinting furnace mendingHands
        toHers = hit his (Recipient.ToCreature hers) 1
        toHis = hit hers (Recipient.ToCreature his) 1
    Spec.assertEqWith s "setup: alice is the active player" (Game.apnapOrder shielded) [S.alice, S.bob]
    Spec.assertEqWith s "setup: both creatures are shielded" (length (GameState.replacements shielded)) 2
    Spec.assertEqWith
      s
      "alice chooses before bob when her event is gathered first"
      (choosersAsked shielded [toHers, toHis])
      [S.alice, S.bob]
    Spec.assertEqWith
      s
      "and still before bob when his event is gathered first"
      (choosersAsked shielded [toHis, toHers])
      [S.alice, S.bob]
  -- The reason CR 615.7 and CR 616.1's APNAP clause do not contend for the same
  -- ordering, stated as a board. They order DIFFERENT LEVELS: APNAP orders the
  -- choosers, CR 615.7 orders one chooser's own events among themselves, and CR
  -- 101.4c is what licenses the second ("If a player would make more than one
  -- choice at the same time, the player makes the choices in the order
  -- specified"). Nothing forces a pick between them.
  --
  -- What makes that structural rather than lucky: a shield names ONE recipient,
  -- so every event a shield contests is addressed to one player's object -- and
  -- that is the same player CR 616.1 asks about those events, since `contested`
  -- and `choose` read the chooser off the recipient through one `chooserOf`. A
  -- CR 615.7 group can therefore never straddle two CR 616.1 choosers, which is
  -- exactly the shape a genuine collision would need.
  --
  -- Two events at alice's creature and one at bob's, with alice's shield too
  -- small for both of hers: she is asked to allocate it (CR 615.7) and asked
  -- twice which effect applies (CR 616.1), and all three of her questions come
  -- before bob's. That her allocation is then HONOURED is mendingHandsSpec's
  -- "the shielded PLAYER chooses which of two simultaneous damages the shield
  -- prevents", which this fixture does not restate.
  Spec.it s "CR 615.7's order sits INSIDE one chooser's APNAP turn, not across choosers" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    furnace <- S.printingOf s registry "Furnace of Rath"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let (shielded, hers, his) = doubledAndShielded plains pikerPrinting furnace mendingHands
        -- 1 and 4 against a shield of 4: their total exceeds it, so CR 615.7 has
        -- something to ask. The small one first is load-bearing -- doubled by the
        -- Furnace it still costs the shield at most 2, so the shield is still
        -- standing for the 4 and alice's SECOND CR 616.1 choice is a real
        -- question rather than a lone candidate `choose` would elide.
        batch = [hit hers (Recipient.ToCreature his) 1, hit his (Recipient.ToCreature hers) 1, hit his (Recipient.ToCreature hers) 4]
    Spec.assertEqWith
      s
      "alice allocates her shield and settles both her events before bob is asked anything"
      (choosersAsked shielded batch)
      [S.alice, S.alice, S.alice, S.bob]
  -- The other half of "they compose": alice's CR 615.7 answer must still land on
  -- the events she was asked about. `contested` reports BATCH POSITIONS and
  -- `askOne` splices by position, so the sort has to happen before the positions
  -- are computed -- sorting afterwards would leave alice permuting whatever now
  -- sits at her old indices, which here includes bob's event.
  --
  -- The assertion is which event SURVIVED rather than how much damage landed,
  -- because the total cannot tell the two answers apart: the shield prevents 4
  -- either way and the Furnace doubles what is left of 5, so alice takes 2
  -- whichever event she spends it on. What differs is WHICH source dealt it (CR
  -- 615.6: a fully prevented event never happens), which is why the two hits at
  -- her creature come from two different creatures of bob's.
  Spec.it s "CR 615.7's allocation lands on the events it was asked about, after the sort" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    furnace <- S.printingOf s registry "Furnace of Rath"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (theFurnace, g1) = S.addPermanent furnace S.alice base
        (hers, g2) = S.addPermanent pikerPrinting S.alice g1
        (small, g3) = S.addPermanent pikerPrinting S.bob g2
        (big, g4) = S.addPermanent pikerPrinting S.bob g3
        (g5, shieldSpell) = S.handOne mendingHands g4
        shielded = castAndResolve (aimCreature hers) g5 shieldSpell
        -- Bob's event FIRST, so alice's two sit at positions 1 and 2 before the
        -- sort and at 0 and 1 after it.
        batch =
          [ hit hers (Recipient.ToCreature small) 1,
            hit small (Recipient.ToCreature hers) 1,
            hit big (Recipient.ToCreature hers) 4
          ]
        survivors gs = fmap DamageEvent.source (S.damageEventsOf gs)
    Spec.assertEqWith s "setup: alice's creature is the shielded one" (length (GameState.replacements shielded)) 1
    -- Shield on the 1: it is prevented whole and never happens, and the 4 keeps
    -- the remaining 3 off, leaving 1 for the Furnace to double.
    Spec.assertEqWith
      s
      "alice spends the shield on the small hit: the big one is what gets through"
      (survivors (settleDamage (allocateShield theFurnace small) shielded batch))
      [big, hers]
    -- Shield on the 4: 4 covers it whole, and the 1 is then unshielded.
    Spec.assertEqWith
      s
      "alice spends it on the big hit instead: the small one gets through"
      (survivors (settleDamage (allocateShield theFurnace big) shielded batch))
      [small, hers]

-- CR 615.12 NARROWED, whose producer is Excruciator ({6}{R}{R} Creature --
-- Avatar 7/7, Ravnica: City of Guilds 121, "Damage that would be dealt by this
-- creature can't be prevented"). Spider-Punk's sentence names no quality of the
-- damage and this one names its SOURCE -- CR 120.1's "an object that deals
-- damage is the source of that damage", which is Pawl.Types.DamagePattern's
-- `whatSource`.
--
-- ONE board carries both directions, and that is the whole point of the group:
-- alice's Goblin Piker is shielded by a Mending Hands she really casts on it,
-- and bob controls Excruciator AND a Goblin Piker. The first two cases send the
-- same 3 from each of them into that one shield -- the Excruciator's lands whole
-- and costs the shield nothing, the Piker's is prevented whole and spends 3 of
-- the shield's 4 -- so nothing but the damage's SOURCE differs between them, and
-- neither can pass because the damage would have got through anyway. The third
-- puts both in one batch.
--
-- The DAMAGE BATCHES are hand-built and the SPELL is not, for spiderPunkSpec's
-- reason.
excruciatorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
excruciatorSpec s registry = Spec.describe s "Excruciator (CR 615.12)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      withBoard act = do
        plains <- S.printingOf s registry "Plains"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        mendingHands <- S.printingOf s registry "Mending Hands"
        excruciator <- S.printingOf s registry "Excruciator"
        let base = S.landsInPlay plains 1
            (victim, g1) = S.addPermanent pikerPrinting S.alice base
            (piker, g2) = S.addPermanent pikerPrinting S.bob g1
            (avatar, g3) = S.addPermanent excruciator S.bob g2
            (g4, spellId) = S.handOne mendingHands g3
        act victim piker avatar (castAndResolve (aimCreature victim) g4 spellId)
  -- THE CONTROL, and it shares its board with the case below rather than
  -- standing on a second one: the shield is applicable to the Piker's damage and
  -- prevents the whole of it, though an Excruciator is on the battlefield the
  -- entire time. A pattern that admitted every source would fail here.
  Spec.it s "CR 615.7 the shield still prevents the Goblin Piker's 3 whole"
    . withBoard
    $ \victim piker _ shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit piker (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the shield is a floating replacement" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "nothing is marked on the shielded creature" (S.damageOf victim after) (Just 0)
      Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
      Spec.assertEqWith s "3 of the shield's 4 were spent, so 1 remains" (shieldsLeft after) [1]
  -- CR 615.12 for the source the clause names: the same shield, on the same
  -- creature, on the same board, prevents none of the Excruciator's 3 and is not
  -- reduced by it.
  Spec.it s "CR 615.12 the Excruciator's 3 lands in full, and the shield is not reduced"
    . withBoard
    $ \victim _ avatar shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit avatar (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the same shield is on the same creature" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "the whole 3 is marked on the shielded creature" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and the event happened, at its full amount" (amounts after) [3]
      Spec.assertEqWith s "the shield still holds all 4" (shieldsLeft after) [4]
  -- Both directions in ONE batch, which is what makes the narrowing a per-EVENT
  -- fact rather than a per-board one: CR 615.12's clause reaches the
  -- Excruciator's event and leaves the Piker's alone, in a batch the engine
  -- settles together.
  --
  -- The two amounts DIFFER, and that is what makes the case discriminate: with
  -- 3 and 3 an engine that had the two events exactly backwards would leave the
  -- same board, and every assertion here would pass on it.
  Spec.it s "CR 615.12 one batch: the Excruciator's 3 lands and the Piker's 2 is prevented"
    . withBoard
    $ \victim piker avatar shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit avatar (Recipient.ToCreature victim) 3, hit piker (Recipient.ToCreature victim) 2]
      Spec.assertEqWith s "only the Excruciator's event happened" (amounts after) [3]
      Spec.assertEqWith s "so only its 3 is marked" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and only the Piker's 2 came off the shield" (shieldsLeft after) [2]

-- CR 615.12 narrowed on TWO axes at once, whose producer is Questing Beast
-- ({2}{G}{G} Legendary Creature -- Beast 4/4, Throne of Eldraine 171, "Combat
-- damage that would be dealt by creatures you control can't be prevented").
-- Excruciator's clause above names ONE object (Filter.IsSource) and says nothing
-- about the kind; this one names a SET by characteristic and pins CR 120.1's
-- source to CR 510.2's combat damage, so it is the pool's one statement of CR
-- 615.12 where both `whichKind` and a characteristic `whatSource` have to be
-- read -- Excruciator's and Malignus' write IsSource and Spider-Punk's writes
-- neither field.
--
-- CR 109.5: the "you" inside whatSource is the ABILITY'S SOURCE's controller,
-- which is the Maybe ObjectId Pawl.Engine.PlayerEffect.unpreventable threads out
-- beside each pattern -- bob's, and not the shielded creature's controller.
--
-- ONE board carries all four cases, excruciatorSpec's design and for its reason:
-- alice's Goblin Piker is shielded by a Mending Hands she really casts on it,
-- alice controls a SECOND Piker, and bob controls a Piker and the Beast. Nothing
-- but the axis under test differs between the cases -- the source's controller
-- in the first two, the damage's KIND in the third -- so neither control can
-- pass because the damage would have got through anyway.
--
-- The DAMAGE BATCHES are hand-built and the SPELL is not, for spiderPunkSpec's
-- reason: that is what lets a case name DamageKind.Combat without driving a
-- whole combat phase to produce a fixture these assertions read straight off.
questingBeastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
questingBeastSpec s registry = Spec.describe s "Questing Beast (CR 615.12)" $ do
  let hit kind src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing kind
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      withBoard act = do
        plains <- S.printingOf s registry "Plains"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        mendingHands <- S.printingOf s registry "Mending Hands"
        questingBeast <- S.printingOf s registry "Questing Beast"
        let base = S.landsInPlay plains 1
            (victim, g1) = S.addPermanent pikerPrinting S.alice base
            (hers, g2) = S.addPermanent pikerPrinting S.alice g1
            (his, g3) = S.addPermanent pikerPrinting S.bob g2
            (_, g4) = S.addPermanent questingBeast S.bob g3
            (g5, spellId) = S.handOne mendingHands g4
        act victim hers his (castAndResolve (aimCreature victim) g5 spellId)
  -- THE whatSource CONTROL: the same shield, on the same board, with the Beast on
  -- the battlefield the whole time, prevents the combat damage of a creature
  -- ALICE controls whole. "Creatures you control" is bob's set, so a pattern that
  -- dropped the ControlledBy atom would fail here.
  Spec.it s "CR 615.7 combat damage from a creature the Beast's controller does NOT control is prevented"
    . withBoard
    $ \victim hers _ shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit DamageKind.Combat hers (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the shield is a floating replacement" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "nothing is marked on the shielded creature" (S.damageOf victim after) (Just 0)
      Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
      Spec.assertEqWith s "3 of the shield's 4 were spent, so 1 remains" (shieldsLeft after) [1]
  -- THE CARD: the same shield, the same creature, the same 3, and the source is
  -- now a creature BOB controls. It lands whole and the shield is not reduced
  -- (CR 615.12's last sentence).
  --
  -- Bob's GOBLIN PIKER and not the Beast itself, which is what separates this
  -- clause from Excruciator's: an engine reading whatSource as Filter.IsSource
  -- would prevent this damage.
  Spec.it s "CR 615.12 combat damage from a creature the Beast's controller DOES control lands in full"
    . withBoard
    $ \victim _ his shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit DamageKind.Combat his (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the same shield is on the same creature" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "the whole 3 is marked on the shielded creature" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and the event happened, at its full amount" (amounts after) [3]
      Spec.assertEqWith s "the shield still holds all 4" (shieldsLeft after) [4]
  -- THE whichKind CONTROL: the SAME source, the SAME shield, the same 3 -- only
  -- CR 510.2's kind differs, and the damage is prevented. This is the assertion
  -- that keeps whichKind from being decoration.
  Spec.it s "CR 615.7 NONcombat damage from that same creature is prevented"
    . withBoard
    $ \victim _ his shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit DamageKind.Noncombat his (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the same shield is on the same creature" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "nothing is marked on the shielded creature" (S.damageOf victim after) (Just 0)
      Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
      Spec.assertEqWith s "3 of the shield's 4 came off it" (shieldsLeft after) [1]
  -- Both directions in ONE batch, which is what makes the narrowing a per-EVENT
  -- fact rather than a per-board one, exactly as excruciatorSpec's third case is.
  --
  -- The two amounts DIFFER, and that is what makes the case discriminate: with 3
  -- and 3 an engine that had the two events exactly backwards would leave the
  -- same board and every assertion here would pass on it.
  Spec.it s "CR 615.12 one batch: bob's Piker's combat 3 lands and alice's Piker's combat 2 is prevented"
    . withBoard
    $ \victim hers his shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit DamageKind.Combat his (Recipient.ToCreature victim) 3, hit DamageKind.Combat hers (Recipient.ToCreature victim) 2]
      Spec.assertEqWith s "only bob's Piker's event happened" (amounts after) [3]
      Spec.assertEqWith s "so only its 3 is marked" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and only alice's Piker's 2 came off the shield" (shieldsLeft after) [2]

-- CR 615.1 / 609.7b: a shield that names its source by CHARACTERISTIC rather than
-- by identity, whose producer is Luminesce ({W} Instant, Tenth Edition 28,
-- "Prevent all damage that black sources and red sources would deal this turn").
-- Fog with a colour filter on the source and nothing else: no recipient, no
-- amount, no choice made at creation, so it isolates that one axis.
--
-- THE VACUITY TRAP is that "prevented" and "prevents everything" leave the same
-- board when every source on it matches, so bob's three creatures are a Bog
-- Wraith (black), a Goblin Piker (red) and a War Mammoth (green), and all three
-- deal damage in ONE batch. The amounts are 4, 2 and 3 -- distinct, so every
-- reading of the card lands alice on a different life total: 20 - 3 = 17 is the
-- card, 11 prevents nothing, 13 and 15 each drop one of the two disjuncts, and 20
-- ignores the filter altogether.
--
-- The DAMAGE BATCH is hand-built and the SPELL is not, for mendingHandsSpec's
-- reason.
luminesceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
luminesceSpec s registry = Spec.describe s "Luminesce (CR 615.1, CR 609.7b)" $ do
  let hit src n = DamageEvent.MkDamageEvent src (Recipient.ToPlayer S.alice) n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      withBoard act = do
        plains <- S.printingOf s registry "Plains"
        wraithPrinting <- S.printingOf s registry "Bog Wraith"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        mammothPrinting <- S.printingOf s registry "War Mammoth"
        luminesce <- S.printingOf s registry "Luminesce"
        let base = S.landsInPlay plains 1
            (wraith, g1) = S.addPermanent wraithPrinting S.bob base
            (piker, g2) = S.addPermanent pikerPrinting S.bob g1
            (mammoth, g3) = S.addPermanent mammothPrinting S.bob g2
            (g4, spellId) = S.handOne luminesce g3
        act wraith piker mammoth (castAndResolve S.identityAnswer g4 spellId)
  Spec.it s "CR 615.1 the green source's 3 lands and the black and red 4 and 2 are prevented"
    . withBoard
    $ \wraith piker mammoth shielded -> do
      let after = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit mammoth 3, hit wraith 4, hit piker 2])
      Spec.assertEqWith s "setup: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
      Spec.assertEqWith s "setup: alice starts on 20" (S.lifeOf S.alice shielded) (Just 20)
      Spec.assertEqWith s "only the War Mammoth's event happened" (amounts after) [3]
      Spec.assertEqWith s "so alice loses 3 and no more" (S.lifeOf S.alice after) (Just 17)
      Spec.assertEqWith s "and it lasts the turn rather than being used up (CR 615.3)" (length (GameState.replacements after)) 1
  -- CR 609.7b's RECHECK, which is what makes this a filter rather than a list of
  -- objects captured when the shield was made: the Piker's damage is prevented
  -- while it is red and dealt in full once it is not. One board, one shield, and
  -- the only thing that differs between the two readings is the source's colour.
  Spec.it s "CR 609.7b the shield rechecks the source: a Piker made green deals its 2"
    . withBoard
    $ \_ piker _ shielded -> do
      let bleached = S.withEffect piker (Modification.SetColor (Set.singleton Color.Green))
          after = S.runPure S.identityAnswer (bleached shielded) (Damage.applyDamage [hit piker 2])
          asPrinted = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit piker 2])
      Spec.assertEqWith s "while red, the Piker's 2 is prevented whole" (S.lifeOf S.alice asPrinted) (Just 20)
      Spec.assertEqWith s "once green, the same 2 is dealt" (S.lifeOf S.alice after) (Just 18)
  -- CR 608.2h's DEPARTED source, the half neither case above reaches: both of
  -- them deal damage from a permanent still on the battlefield, so the recheck
  -- reads a live projection either way. Ghitu Fire-Eater ({2}{R} Creature --
  -- Human Nomad 2/2, "{T}, Sacrifice this creature: It deals damage equal to its
  -- power to any target") pays the source's own departure as a COST, so the id
  -- the damage event carries names nothing by the time the ability resolves --
  -- no response and no prompt stands between the departure and the read.
  --
  -- THE VACUITY TRAP here is the AMOUNT collapsing to 0: were Quantity.Power to
  -- read the dead id's blank view, both readings would leave bob on 20 and the
  -- case would pass whatever the shield did. Pawl.ActivateSpec's "CR 113.7a whole
  -- card" case pins the amount at 2 with the source already gone, which is what
  -- keeps 20 and 18 apart below. That trap is not hypothetical: emptying
  -- GameState.lastKnown outright is NOT a usable mutation here, because the same
  -- store feeds Quantity.Power, the amount collapses to nothing, and the red board
  -- below lands on 20 under both readings. Only the green board catches it.
  --
  -- TWO boards differing in exactly one thing, because one cannot separate the
  -- readings on its own. Pairing (red, green) the shield gives (20, 18) reading
  -- the filed record, (20, 20) reading the printed card -- the print is red --
  -- (20, 20) admitting any departed source unconditionally, and (18, 18) reading
  -- the live projection of a dead id, which is what the engine did before #1844.
  -- The green board rests on the record holding the PROJECTED characteristics
  -- rather than the printed face; Pawl.ActivateSpec's pumped Fire-Eater is the
  -- direct proof of that, and this is the same fact read on the colour axis.
  --
  -- bob is the recipient because a PLAYER puts no toughness, no state-based
  -- action and no marked damage between the divergence and the read. Two seats
  -- and not three: nothing in CR 608.2h, CR 609.7b or Luminesce's text says
  -- "opponent", "that player" or "defending player" -- the shield is
  -- source-scoped and the recipient is whatever the ability targeted.
  let ghituBoard tint act = do
        plains <- S.printingOf s registry "Plains"
        ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
        luminesce <- S.printingOf s registry "Luminesce"
        let base = S.landsInPlay plains 1
            (fireEater, g1) = S.addPermanent ghitu S.alice base
            (g2, spellId) = S.handOne luminesce g1
            shielded = tint fireEater (castAndResolve S.identityAnswer g2 spellId)
        act fireEater shielded (S.runPure (aimPlayer S.bob) shielded (Activate.activateAbility S.alice fireEater (theAbility ghitu) Monad.>> Stack.resolveTop))
  Spec.it s "CR 608.2h a sacrificed red source's damage is still prevented: the shield reads last known information"
    . ghituBoard (\_ gs -> gs)
    $ \fireEater shielded after -> do
      Spec.assertEqWith s "bob takes nothing: the departed source is read as the red creature it last was" (S.lifeOf S.bob after) (Just 20)
      Spec.assertBool s (not (Set.member fireEater (GameState.battlefield after))) "setup: the cost really did remove the source"
      Spec.assertBool s (Maybe.isNothing (Game.lookupObject fireEater after)) "setup: and the id it left behind names nothing"
      Spec.assertEqWith s "setup: the shield was a floating replacement before the activation" (length (GameState.replacements shielded)) 1
      Spec.assertEqWith s "supporting: and it lasts the turn rather than being used up (CR 615.3)" (length (GameState.replacements after)) 1
  Spec.it s "CR 609.7b the recheck reads the RECORD, not the print: a Fire-Eater made green before it left deals its 2"
    . ghituBoard (\oid -> S.withEffect oid (Modification.SetColor (Set.singleton Color.Green)))
    $ \fireEater _ after -> do
      Spec.assertEqWith s "bob takes 2: neither disjunct matches the green source the record filed" (S.lifeOf S.bob after) (Just 18)
      Spec.assertBool s (Maybe.isNothing (Game.lookupObject fireEater after)) "setup: the source departed here too, so the two boards differ only in colour"

-- CR 615.1 / 609.7b again, on two axes at once: a shield that names its source by
-- CHARACTERISTIC and narrows by DAMAGE KIND. Its producer is Moonmist ({1}{G}
-- Instant, "Transform all Humans. Prevent all combat damage that would be dealt
-- this turn by creatures other than Werewolves and Wolves"). Luminesce with a
-- subtype EXCLUSION where Luminesce has a colour list, plus the kind narrowing
-- Luminesce's card has not.
--
-- THE VACUITY TRAP is that "prevented" and "prevents everything" leave the same
-- board when every source matches or none does, so the batch carries THREE
-- sources whose readings all differ: a Wolf (Russet Wolves), a Werewolf (Tovolar,
-- Dire Overlord) and neither (Goblin Piker). The discriminating assertion is on
-- the SURVIVING EVENTS' SOURCE IDS rather than on alice's life, because the Wolf
-- and the Werewolf deal the same 3: a life total cannot tell "Werewolf dropped
-- from the Or" from "Wolf dropped from the Or", and would pass under either.
--
-- Tovolar's front face is a Human Werewolf, so Moonmist's OWN first sentence
-- names him -- and CR 702.145b's third static ability refuses it (Pawl.DaytimeSpec's
-- restrictionSpec is the proof). Nothing here rests on which way that goes: the
-- damage amounts are hand-built rather than read off power, and both of his faces
-- are Werewolves.
--
-- The DAMAGE BATCH is hand-built and the SPELL is not, for mendingHandsSpec's
-- reason.
--
-- The card's `HasCardType Creature` conjunct ("by CREATURES other than ...") is a
-- REGRESSION FENCE here rather than a proved behaviour: deleting it leaves the
-- whole suite green, because CR 510.1a makes attacking and blocking creatures the
-- only assigners of combat damage, so no rules-legal board separates the two
-- readings. It is written because a hand-built batch like this one can name a
-- Forest as a Combat source, and the card file should say what the card says.
moonmistSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
moonmistSpec s registry = Spec.describe s "Moonmist (CR 615.1, CR 609.7b)" $ do
  let hit kind src n = DamageEvent.MkDamageEvent src (Recipient.ToPlayer S.alice) n False False False 0 Nothing kind
      sources gs = fmap DamageEvent.source (S.damageEventsOf gs)
      withBoard act = do
        forest <- S.printingOf s registry "Forest"
        wolfPrinting <- S.printingOf s registry "Russet Wolves"
        werewolfPrinting <- S.printingOf s registry "Tovolar, Dire Overlord"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        moonmist <- S.printingOf s registry "Moonmist"
        let base = S.landsInPlay forest 2
            (wolf, g1) = S.addPermanent wolfPrinting S.bob base
            (werewolf, g2) = S.addPermanent werewolfPrinting S.bob g1
            (piker, g3) = S.addPermanent pikerPrinting S.bob g2
            (g4, spellId) = S.handOne moonmist g3
        act wolf werewolf piker (castAndResolve S.castAnswer g4 spellId)
  Spec.it s "CR 615.1 the Piker's combat 2 is prevented and the Wolf's and the Werewolf's 3s land"
    . withBoard
    $ \wolf werewolf piker shielded -> do
      let batch = [hit DamageKind.Combat wolf 3, hit DamageKind.Combat werewolf 3, hit DamageKind.Combat piker 2]
          after = S.runPure S.identityAnswer shielded (Damage.applyDamage batch)
      -- The behavioural assertion leads, so no proxy below it can absorb a
      -- mutation to the card's filter and report itself instead.
      Spec.assertEqWith s "the Wolf's and the Werewolf's events happened, the Piker's did not" (sources after) [wolf, werewolf]
      Spec.assertEqWith s "so alice loses 3 and 3 and no more" (S.lifeOf S.alice after) (Just 14)
      Spec.assertEqWith s "supporting: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
      Spec.assertEqWith s "supporting: alice started on 20" (S.lifeOf S.alice shielded) (Just 20)
      Spec.assertEqWith s "and it lasts the turn rather than being used up (CR 615.3)" (length (GameState.replacements after)) 1
  -- The KIND half, which luminesceSpec's card cannot reach: the very same Piker
  -- dealing the very same 2 as NONCOMBAT damage is not prevented. A card file that
  -- dropped whichKind passes the case above and fails this one.
  Spec.it s "CR 615.1 the same source's NONCOMBAT 2 is not prevented"
    . withBoard
    $ \_ _ piker shielded -> do
      let after = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit DamageKind.Noncombat piker 2])
      Spec.assertEqWith s "the event happened" (sources after) [piker]
      Spec.assertEqWith s "and alice took it" (S.lifeOf S.alice after) (Just 18)
  -- CR 609.7b's RECHECK, which is what makes this a filter rather than a list of
  -- objects captured when the shield was made: the Piker's damage is prevented
  -- while it is not a Wolf, and dealt in full once it is one. One board, one
  -- shield, and the only thing that differs between the two readings is a subtype
  -- the projection adds after the shield already exists.
  Spec.it s "CR 609.7b the shield rechecks the source: a Piker made a Wolf deals its 2"
    . withBoard
    $ \_ _ piker shielded -> do
      let lupine = S.withEffect piker (Modification.AddCreatureSubtype Subtype.Wolf)
          after = S.runPure S.identityAnswer (lupine shielded) (Damage.applyDamage [hit DamageKind.Combat piker 2])
          asPrinted = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit DamageKind.Combat piker 2])
      Spec.assertEqWith s "while a Goblin Warrior, the 2 is prevented whole" (S.lifeOf S.alice asPrinted) (Just 20)
      Spec.assertEqWith s "once a Wolf, the same 2 is dealt" (S.lifeOf S.alice after) (Just 18)

-- CR 615.13's trigger read BLIND to which prevention applied: Selfless Squire
-- ({3}{W} Creature -- Human Soldier 1/1, Flash, "When this creature enters, prevent all
-- damage that would be dealt to you this turn. Whenever damage that would be
-- dealt to you is prevented, put that many +1/+1 counters on this creature").
--
-- Four properties, and between them they are the rule: a prevention that
-- prevents something fires the ability, the ability is handed HOW MUCH was
-- prevented, one prevention effect across several SIMULTANEOUS events fires it
-- ONCE, and damage prevented to a permanent is not damage prevented to a player.
-- The card's own 2016-11-08 ruling supplies a fifth -- "any effect that uses the
-- word 'prevent' will cause it to trigger" -- which is the Mending Hands case.
--
-- The DAMAGE BATCHES are hand-built and the SPELL is not, for mendingHandsSpec's
-- reason: casting the Squire for real is what proves the card, while reaching a
-- two-attacker combat batch would mean driving a whole combat phase to produce a
-- fixture these assertions read straight off.
selflessSquireSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selflessSquireSpec s registry = Spec.describe s "Selfless Squire (CR 615.13)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- Cast the Squire, let its CR 603.6a enters trigger go on the stack, and
      -- resolve it -- which is what installs the CR 615.1 shield.
      castSquire gs spellId =
        let entered = castAndResolve S.identityAnswer gs spellId
            triggered = S.runPure S.identityAnswer entered Engine.settleForPriority
         in S.runPure S.identityAnswer triggered Stack.resolveTop
      -- Settle one damage batch, then let whatever it triggered go on the stack
      -- and resolve. One trigger per pass, which is all any case here makes.
      strikeAndSettle gs batch =
        let dealt = S.runPure S.identityAnswer gs (Damage.applyDamage batch >> Engine.settleForPriority)
         in (dealt, S.runPure S.identityAnswer dealt Stack.resolveTop)
      squireOf gs = case Set.toList (Set.filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (CardName.MkCardName (Text.pack "Selfless Squire"))) (GameState.battlefield gs)) of
        oid : _ -> Just oid
        [] -> Nothing
  -- THE WHOLE CARD, and the whole of CR 615.13: a prevention effect is applied,
  -- it prevents some damage, and an ability that watches for exactly that fires
  -- carrying the amount. 3 damage aimed at alice is prevented whole (CR 615.6),
  -- and the 1/1 that shielded her becomes a 4/4.
  Spec.it s "CR 615.13 whole card: damage prevented to alice puts that many +1/+1 counters on the Squire" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    let base = S.landsInPlay plains 4
        (attacker, g1) = S.addPermanent pikerPrinting S.bob base
        (g2, spellId) = S.handOne squirePrinting g1
        shielded = castSquire g2 spellId
        squire = squireOf shielded
        (dealt, after) = strikeAndSettle shielded [hit attacker (Recipient.ToPlayer S.alice) 3]
    Spec.assertEqWith s "setup: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
    Spec.assertEqWith s "setup: the Squire is on the battlefield as a 1/1" (squire >>= \oid -> S.powerToughnessOf oid shielded) (Just (1, 1))
    -- CR 615.6: a fully prevented event never happens, so alice loses nothing and
    -- no damage event is recorded.
    Spec.assertEqWith s "alice's life is untouched" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
    -- The discriminating half: the prevention alone would leave the Squire a
    -- 1/1. What makes it a 4/4 is CR 615.13's trigger reading the amount.
    Spec.assertEqWith s "exactly one trigger was gathered" (length (GameState.stack dealt)) 1
    Spec.assertEqWith s "and it put 3 +1/+1 counters on the Squire" (fmap (\oid -> countersOn CounterKind.PlusOnePlusOne oid after) squire) (Just 3)
    Spec.assertEqWith s "so the 1/1 is now a 4/4" (squire >>= \oid -> S.powerToughnessOf oid after) (Just (4, 4))
  -- The BASELINE that makes the case above discriminate: the same board with the
  -- shield never installed. The Squire is put onto the battlefield directly, so
  -- its CR 603.6a enters trigger never fires and nothing prevents anything.
  --
  -- Both halves move: alice takes the damage AND the Squire stays a 1/1. An
  -- implementation that fired the counters off the DAMAGE rather than off the
  -- prevention would pass the case above and fail this one.
  Spec.it s "CR 615.13 no prevention, no trigger: the same 3 damage lands and the Squire stays a 1/1" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    let base = S.landsInPlay plains 4
        (attacker, g1) = S.addPermanent pikerPrinting S.bob base
        (squire, g2) = S.addPermanent squirePrinting S.alice g1
        (dealt, after) = strikeAndSettle g2 [hit attacker (Recipient.ToPlayer S.alice) 3]
    Spec.assertEqWith s "setup: no shield was installed" (length (GameState.replacements g2)) 0
    Spec.assertEqWith s "alice takes all 3" (S.lifeOf S.alice after) (Just 17)
    Spec.assertEqWith s "and the damage event happened" (amounts after) [3]
    Spec.assertEqWith s "nothing triggered" (length (GameState.stack dealt)) 0
    Spec.assertEqWith s "so the Squire is still a 1/1" (S.powerToughnessOf squire after) (Just (1, 1))
  -- CR 615.13's own arithmetic: "each time a prevention effect is applied to ONE
  -- OR MORE SIMULTANEOUS damage events". One shield across a batch of two is one
  -- application, so the ability fires ONCE with the total -- not once per event.
  --
  -- The counter count cannot tell the two readings apart (3 + 2 either way), so
  -- what discriminates is the number of triggers gathered.
  Spec.it s "CR 615.13 one prevention effect across two simultaneous events fires the ability ONCE" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    let base = S.landsInPlay plains 4
        (first, g1) = S.addPermanent pikerPrinting S.bob base
        (second, g2) = S.addPermanent pikerPrinting S.bob g1
        (g3, spellId) = S.handOne squirePrinting g2
        shielded = castSquire g3 spellId
        squire = squireOf shielded
        batch = [hit first (Recipient.ToPlayer S.alice) 3, hit second (Recipient.ToPlayer S.alice) 2]
        (dealt, after) = strikeAndSettle shielded batch
    Spec.assertEqWith s "both events were prevented whole" (amounts after) []
    Spec.assertEqWith s "ONE trigger, not one per event" (length (GameState.stack dealt)) 1
    Spec.assertEqWith s "carrying the TOTAL prevented" (fmap (\oid -> countersOn CounterKind.PlusOnePlusOne oid after) squire) (Just 5)
  -- The recipient half of the condition. Selfless Squire says "damage that would
  -- be dealt to YOU", so a prevention that covers a CREATURE is silence -- and
  -- the 2016-11-08 ruling's half is here too: the prevention doing the work is
  -- MENDING HANDS, a card the Squire has nothing to do with, and the Squire is
  -- put onto the battlefield directly so its own shield is not in play to
  -- confuse the two.
  Spec.it s "CR 615.13 someone else's prevention fires it, but only for damage to a PLAYER" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (victim, g1) = S.addPermanent pikerPrinting S.alice base
        (attacker, g2) = S.addPermanent pikerPrinting S.bob g1
        (squire, g3) = S.addPermanent squirePrinting S.alice g2
        (g4, spellId) = S.handOne mendingHands g3
        onCreature = castAndResolve (aimCreature victim) g4 spellId
        onAlice = castAndResolve (aimPlayer S.alice) g4 spellId
        (creatureDealt, creatureAfter) = strikeAndSettle onCreature [hit attacker (Recipient.ToCreature victim) 3]
        (playerDealt, playerAfter) = strikeAndSettle onAlice [hit attacker (Recipient.ToPlayer S.alice) 3]
    Spec.assertEqWith s "the creature's 3 was prevented" (S.damageOf victim creatureAfter) (Just 0)
    Spec.assertEqWith s "but nothing triggered" (length (GameState.stack creatureDealt)) 0
    Spec.assertEqWith s "so the Squire stays a 1/1" (S.powerToughnessOf squire creatureAfter) (Just (1, 1))
    -- The same shield, aimed one recipient over, is the discriminating twin.
    Spec.assertEqWith s "alice's 3 was prevented too" (S.lifeOf S.alice playerAfter) (Just 20)
    Spec.assertEqWith s "and THAT fired the Squire" (length (GameState.stack playerDealt)) 1
    Spec.assertEqWith s "for 3 counters, off a prevention its own ability had nothing to do with" (S.powerToughnessOf squire playerAfter) (Just (4, 4))
  -- CR 109.5's relation, read alongside CR 615.13's amount: Selfless Squire's
  -- condition says "damage that would be dealt to YOU", and alice is its only
  -- "you" -- so a shield admitting MORE than alice must bind only her share,
  -- never the total it stopped for everyone it covers.
  --
  -- Synthetic Impartial Ward ({1}{W} Instant, "Prevent the next 10 damage that
  -- would be dealt to any player this turn") is the producer: no card in
  -- data/cards/ writes a PreventNextDamage/PreventAllDamage shield whose player
  -- half admits more than one player -- every whoRecipient in the pool is You
  -- (divine-deflection, synthetic-communal-bulwark, synthetic-parting-ward,
  -- ajani-steadfast, pariah), and Scryfall o:"prevent" o:"any player"
  -- o:"this turn" and o:"damage that would be dealt to an opponent",
  -- 2026-09-03, neither hit a shield of this shape (#3079).
  --
  -- Distinct shares -- alice's 2, bob's 4 -- so a sum (6) cannot be mistaken
  -- for her own (2), and the Squire is put on directly so only the Ward's
  -- shield is in play.
  Spec.it s "CR 109.5 a shield spanning two players binds only the relation's own share, not their sum (#3079)" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    ward <- S.printingOf s registry "Synthetic Impartial Ward"
    let base = S.landsInPlay plains 2
        (attacker, g1) = S.addPermanent pikerPrinting S.bob base
        (squire, g2) = S.addPermanent squirePrinting S.alice g1
        (g3, spellId) = S.handOne ward g2
        shielded = castAndResolve S.identityAnswer g3 spellId
        batch = [hit attacker (Recipient.ToPlayer S.alice) 2, hit attacker (Recipient.ToPlayer S.bob) 4]
        (dealt, after) = strikeAndSettle shielded batch
    Spec.assertEqWith s "setup: the Ward installed a floating shield holding 10" (shieldsLeft shielded) [10]
    Spec.assertEqWith s "both shares were prevented whole" (amounts after) []
    -- The record the binding reads: ONE prevention, holding each player's own
    -- share -- not one row per player.
    Spec.assertEqWith
      s
      "one prevention was recorded, holding each player's share"
      (fmap DamagePrevented.amounts (preventionsRecorded after))
      [Map.fromList [(Recipient.ToPlayer S.alice, 2), (Recipient.ToPlayer S.bob, 4)]]
    Spec.assertEqWith s "exactly one trigger was gathered" (length (GameState.stack dealt)) 1
    -- The discriminating assertion: alice's OWN 2, never the application's 6.
    -- An eventBindings that does not re-ask the relation sums the whole record
    -- and puts 6 counters on instead.
    Spec.assertEqWith s "so the Squire counts alice's 2, not alice-and-bob's 6" (countersOn CounterKind.PlusOnePlusOne squire after) 2
    Spec.assertEqWith s "leaving the 1/1 a 3/3" (S.powerToughnessOf squire after) (Just (3, 3))

-- Aim every target slot at the FIRST of `wanted` the offered set actually holds.
-- The offered set is FILTERED rather than a recipient built by hand: CR 608.2b
-- re-reads a spell or ability's targets as it resolves, and a hand-built
-- recipient that is not the one the pool offered is dropped there with no error.
--
-- Order matters at the call site: a reach for a recipient the pool must NOT
-- offer, with a fallback behind it, is how "any OTHER target" is observed --
-- the fallback is chosen precisely because the first was not there.
preferTarget :: [Recipient.Recipient] -> Prompt.Prompt r -> r
preferTarget wanted p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap (\(_, legal) -> maybe Set.empty Set.singleton (List.find (`Set.member` legal) wanted)) sets
  _ -> S.identityAnswer p

-- CR 616.1's choice between the two prevention effects one shielded Phyrexian
-- Vindicator offers: the one its card PRINTS and CR 122.1c's minted
-- shield-counter pair. Both name the same source, so the entries are told apart
-- by the rewrite -- which is also the only thing that differs between them.
raceShield :: Bool -> [Recipient.Recipient] -> Prompt.Prompt r -> r
raceShield wantMinted wanted p = case p of
  Prompt.ChooseReplacement _ _ entries ->
    let minted entry = case ReplacementEntry.effect entry of
          ReplacementEffect.DamageR (DamageR.MkDamageR _ DamageRewrite.PreventRemovingShieldCounter _) -> True
          _ -> False
     in maybe 0 Int.toNaturalSaturating (List.findIndex ((== wantMinted) . minted) entries)
  _ -> preferTarget wanted p

-- CR 616.1's choice between the two prevention effects one Phyrexian Vindicator
-- with protection from artifacts offers: the one its card PRINTS and CR 702.16e's
-- minted one. Both are a PreventAll on the same source, so the only thing left to
-- tell them apart by is the SOURCE half of the pattern -- rule 702.16e writes the
-- protection quality there, and the Vindicator's printed sentence qualifies the
-- damage in no way.
raceProtection :: Bool -> [Recipient.Recipient] -> Prompt.Prompt r -> r
raceProtection wantMinted wanted p = case p of
  Prompt.ChooseReplacement _ _ entries ->
    let minted entry = case ReplacementEntry.effect entry of
          ReplacementEffect.DamageR (DamageR.MkDamageR matching DamageRewrite.PreventAll _) ->
            DamagePattern.whatSource matching == Filter.Type.HasCardType CardType.Artifact
          _ -> False
     in maybe 0 Int.toNaturalSaturating (List.findIndex ((== wantMinted) . minted) entries)
  _ -> preferTarget wanted p

-- CR 616.1's choice between a permanent's own prevention effect and a FLOATING
-- one somebody else installed, told apart by whose object the entry names.
raceIsSelf :: Bool -> ObjectId.ObjectId -> [Recipient.Recipient] -> Prompt.Prompt r -> r
raceIsSelf wantSelf oid wanted p = case p of
  Prompt.ChooseReplacement _ _ entries ->
    maybe 0 Int.toNaturalSaturating (List.findIndex ((== wantSelf) . (== oid) . ReplacementEntry.source) entries)
  _ -> preferTarget wanted p

-- Settle one damage batch, then let whatever it triggered go on the stack and
-- resolve. selflessSquireSpec's local twin, top-level because the answer is
-- rank-2 and these cases hand it a different one per case.
strikeAndSettleWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> [DamageEvent.DamageEvent] -> (GameState.GameState, GameState.GameState)
strikeAndSettleWith answer gs batch =
  let dealt = S.runPure answer gs (Damage.applyDamage batch >> Engine.settleForPriority)
   in (dealt, S.runPure answer dealt Stack.resolveTop)

-- CR 615.13 read the OTHER way -- "prevented THIS WAY". Phyrexian Vindicator
-- ({W}{W}{W}{W} Creature -- Phyrexian Horror 5/5, "Flying / If damage would be
-- dealt to this creature, prevent that damage. When damage is prevented this
-- way, this creature deals that much damage to any other target"), whose trigger
-- fires for its OWN prevention effect where Selfless Squire above fires for
-- anybody's.
--
-- One prevention on the board cannot tell those two readings apart, so the two
-- discriminating cases each put a SECOND one on the same creature -- CR 122.1c's
-- shield-counter pair, and Mending Hands' floating shield. Either stops the same
-- event, addressed to the same recipient, for the same amount, so the CR 616.1
-- choice of which applies is the only thing that differs between the branches;
-- an implementation firing on any prevention at all passes the whole-card case
-- and fails both of those.
phyrexianVindicatorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phyrexianVindicatorSpec s registry = Spec.describe s "Phyrexian Vindicator (CR 615.13)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      onlyBob = [Recipient.ToPlayer S.bob]
  -- THE WHOLE CARD: the Vindicator's own prevention stops 4 (CR 615.6), its
  -- paired trigger reads how much that was, and the 4 lands on bob instead.
  Spec.it s "CR 615.13 whole card: its own prevention fires the trigger, which deals that much to another target" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    vindicatorPrinting <- S.printingOf s registry "Phyrexian Vindicator"
    let base = S.landsInPlay plains 1
        (attacker, g1) = S.addPermanent pikerPrinting S.bob base
        (vindicator, g2) = S.addPermanent vindicatorPrinting S.alice g1
        (dealt, after) = strikeAndSettleWith (preferTarget onlyBob) g2 [hit attacker (Recipient.ToCreature vindicator) 4]
    Spec.assertEqWith s "setup: the 5/5 is on the battlefield undamaged" (S.damageOf vindicator g2) (Just 0)
    -- The gameplay assertion, ahead of every proxy: 4 prevented is 4 dealt to
    -- somebody else, and bob's life is the only place that shows.
    Spec.assertEqWith s "bob took the 4 the Vindicator's own prevention stopped" (S.lifeOf S.bob after) (Just 16)
    Spec.assertEqWith s "and the Vindicator itself took none of it (CR 615.6)" (S.damageOf vindicator after) (Just 0)
    Spec.assertEqWith s "exactly one trigger was gathered" (length (GameState.stack dealt)) 1
  -- THE DISCRIMINATOR. One shielded Vindicator, one 4-damage event, and a CR
  -- 616.1 choice answered both ways. Rule 122.1c's pair is minted onto the
  -- permanent by the rules rather than printed on its card, so the damage it
  -- prevents was not prevented "this way" and the trigger stays silent -- while
  -- the very same board, choosing the printed ability instead, fires it.
  Spec.it s "CR 122.1c a shield counter's prevention is not 'this way', and the same board's printed one is" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    vindicatorPrinting <- S.printingOf s registry "Phyrexian Vindicator"
    let base = S.landsInPlay plains 1
        (attacker, g1) = S.addPermanent pikerPrinting S.bob base
        (vindicator, g2) = S.addPermanent vindicatorPrinting S.alice g1
        shielded = S.addCounter CounterKind.Shield 1 vindicator g2
        batch = [hit attacker (Recipient.ToCreature vindicator) 4]
        (mintedDealt, mintedAfter) = strikeAndSettleWith (raceShield True onlyBob) shielded batch
        (printedDealt, printedAfter) = strikeAndSettleWith (raceShield False onlyBob) shielded batch
    Spec.assertEqWith s "setup: the Vindicator carries one shield counter" (countersOn CounterKind.Shield vindicator shielded) 1
    -- The pair, in the order that makes the point: the same 4 is prevented on
    -- both branches, and only the printed prevention reaches bob.
    Spec.assertEqWith s "the counter's prevention leaves bob untouched" (S.lifeOf S.bob mintedAfter) (Just 20)
    Spec.assertEqWith s "the printed one deals him the same 4" (S.lifeOf S.bob printedAfter) (Just 16)
    Spec.assertEqWith s "both prevented the whole 4 off the Vindicator" (fmap (S.damageOf vindicator) [mintedAfter, printedAfter]) [Just 0, Just 0]
    Spec.assertEqWith s "nothing triggered off the counter's prevention" (length (GameState.stack mintedDealt)) 0
    Spec.assertEqWith s "and the printed one triggered once" (length (GameState.stack printedDealt)) 1
    -- CR 122.1c removes a counter as it prevents, which is what pins WHICH
    -- prevention each branch actually applied.
    Spec.assertEqWith s "the counter was spent on the minted branch" (countersOn CounterKind.Shield vindicator mintedAfter) 0
    Spec.assertEqWith s "and left alone on the printed one" (countersOn CounterKind.Shield vindicator printedAfter) 1
  -- CR 702.16e's prevention, the SAME point one rules-minted prevention over.
  -- "Any damage that would be dealt by sources that have the stated quality to a
  -- permanent or player with protection is prevented" is a prevention rule 702.16
  -- gives the permanent, not a sentence its card prints -- so the damage it stops
  -- was not stopped "this way" either, and a permanent holding both must stay
  -- silent for it.
  --
  -- Rule 122.1c's pair above is told apart from a printed row by its rewrite;
  -- this one is a plain PreventAll, structurally what Stormwild Capridor and the
  -- Vindicator itself print, so nothing about the VALUE separates them. The
  -- discriminator is the same shape as the shield case: one board, one 4-damage
  -- event, and the CR 616.1 choice answered both ways.
  --
  -- Tower of the Magistrate ("{1}, {T}: Target creature gains protection from
  -- artifacts until end of turn.") is the grant and Darksteel Garrison ({2}
  -- Artifact -- Fortification) the artifact source, both checked against Scryfall
  -- on 2026-08-27. The Garrison is bob's, so the trigger's "any OTHER target" and
  -- the damage's source are different objects.
  Spec.it s "CR 702.16e protection's prevention is not 'this way', and the same board's printed one is" $ do
    plains <- S.printingOf s registry "Plains"
    vindicatorPrinting <- S.printingOf s registry "Phyrexian Vindicator"
    towerPrinting <- S.printingOf s registry "Tower of the Magistrate"
    garrisonPrinting <- S.printingOf s registry "Darksteel Garrison"
    let base = S.landsInPlay plains 2
        (vindicator, g1) = S.addPermanent vindicatorPrinting S.alice base
        (tower, g2) = S.addPermanent towerPrinting S.alice g1
        (garrison, g3) = S.addPermanent garrisonPrinting S.bob g2
    case Projection.abilitiesOf tower g3 of
      [_, protect] -> do
        let activated = S.runPure (preferTarget [Recipient.ToCreature vindicator]) g3 (Activate.activateAbility S.alice tower protect)
            protected = S.runPure (preferTarget [Recipient.ToCreature vindicator]) activated Stack.resolveTop
            batch = [hit garrison (Recipient.ToCreature vindicator) 4]
            (mintedDealt, mintedAfter) = strikeAndSettleWith (raceProtection True onlyBob) protected batch
            (printedDealt, printedAfter) = strikeAndSettleWith (raceProtection False onlyBob) protected batch
        Spec.assertBool s (Projection.hasKeyword (Keyword.Protection (Filter.Type.HasCardType CardType.Artifact)) vindicator protected) "setup: the Tower's ability really did grant protection from artifacts"
        -- The gameplay assertion, ahead of every proxy: rule 702.16e's prevention
        -- fires nothing, so bob keeps his life, and the very same board choosing
        -- the printed prevention instead takes 4 off him.
        Spec.assertEqWith s "protection's prevention leaves bob untouched" (S.lifeOf S.bob mintedAfter) (Just 20)
        Spec.assertEqWith s "the printed one deals him the same 4" (S.lifeOf S.bob printedAfter) (Just 16)
        Spec.assertEqWith s "both prevented the whole 4 off the Vindicator" (fmap (S.damageOf vindicator) [mintedAfter, printedAfter]) [Just 0, Just 0]
        Spec.assertEqWith s "nothing triggered off protection's prevention" (length (GameState.stack mintedDealt)) 0
        Spec.assertEqWith s "and the printed one triggered once" (length (GameState.stack printedDealt)) 1
      abilities -> Spec.assertFailure s ("expected exactly two Tower abilities, got " <> show (length abilities))
  -- The FLOATING rival, and the Squire's ruling read from the other end. Mending
  -- Hands ({W} Instant, "Prevent the next 4 damage that would be dealt to any
  -- target this turn") shields the Vindicator for exactly the amount its own
  -- ability would have stopped, so the two candidates race for one event and
  -- neither the recipient nor the amount can tell them apart -- only whose effect
  -- it was. A prevention of somebody else's is not "this way".
  Spec.it s "CR 615.13 another card's prevention of the same 4, on the same creature, is not 'this way'" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    vindicatorPrinting <- S.printingOf s registry "Phyrexian Vindicator"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (attacker, g1) = S.addPermanent pikerPrinting S.bob base
        (vindicator, g2) = S.addPermanent vindicatorPrinting S.alice g1
        (g3, spellId) = S.handOne mendingHands g2
        shielded = castAndResolve (preferTarget [Recipient.ToCreature vindicator]) g3 spellId
        batch = [hit attacker (Recipient.ToCreature vindicator) 4]
        (theirsDealt, theirsAfter) = strikeAndSettleWith (raceIsSelf False vindicator onlyBob) shielded batch
        (oursDealt, oursAfter) = strikeAndSettleWith (raceIsSelf True vindicator onlyBob) shielded batch
    Spec.assertEqWith s "setup: Mending Hands left a floating shield behind" (length (GameState.replacements shielded)) 1
    Spec.assertEqWith s "Mending Hands' prevention leaves bob untouched" (S.lifeOf S.bob theirsAfter) (Just 20)
    Spec.assertEqWith s "the Vindicator's own deals him the same 4" (S.lifeOf S.bob oursAfter) (Just 16)
    Spec.assertEqWith s "both prevented the whole 4 off the Vindicator" (fmap (S.damageOf vindicator) [theirsAfter, oursAfter]) [Just 0, Just 0]
    Spec.assertEqWith s "nothing triggered off Mending Hands' prevention" (length (GameState.stack theirsDealt)) 0
    Spec.assertEqWith s "and the Vindicator's own triggered once" (length (GameState.stack oursDealt)) 1
  -- "Any OTHER target": the answerer reaches for the Vindicator first and takes
  -- bob only because the pool never offers it. Damage the Vindicator dealt
  -- ITSELF would be prevented by its own shield (CR 615.6) and so leave no mark,
  -- which is why bob's life is what this reads.
  Spec.it s "CR 115.4 the trigger's pool excludes its own source: 'any OTHER target'" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    vindicatorPrinting <- S.printingOf s registry "Phyrexian Vindicator"
    let base = S.landsInPlay plains 1
        (attacker, g1) = S.addPermanent pikerPrinting S.bob base
        (vindicator, g2) = S.addPermanent vindicatorPrinting S.alice g1
        (_, after) = strikeAndSettleWith (preferTarget (Recipient.ToCreature vindicator : onlyBob)) g2 [hit attacker (Recipient.ToCreature vindicator) 4]
    Spec.assertEqWith s "the Vindicator was not offered, so the fallback took the 4" (S.lifeOf S.bob after) (Just 16)

-- CR 609.7a's "a source of your choice", answered by id so neither branch below
-- depends on the engine's canonical candidate order.
nameDamageSource :: ObjectId.ObjectId -> Prompt.Prompt r -> r
nameDamageSource wanted p = case p of
  Prompt.ChooseDamageSource _ _ _ offered ->
    if List.elem wanted (NonEmpty.toList offered) then wanted else NonEmpty.head offered
  _ -> S.identityAnswer p

-- Which object a floating prevention row was baked to watch (CR 609.7a), off the
-- one row the boards below install. Nothing where the board has no row at all,
-- which is what makes the setup assertion able to fail.
shieldedSource :: GameState.GameState -> Maybe ObjectId.ObjectId
shieldedSource gs = case GameState.replacements gs of
  [active] -> case ActiveReplacement.effect active of
    ReplacementEffect.DamageR (DamageR.MkDamageR matching _ _) -> DamagePattern.whichSource matching
    _ -> Nothing
  _ -> Nothing

-- CR 615.13's "this way" asked about the DAMAGE'S SOURCE -- Samite Ministration
-- ({1}{W} Instant, "Prevent all damage that would be dealt to you this turn by a
-- source of your choice. Whenever damage from a black or red source is prevented
-- this way this turn, you gain that much life"). Phyrexian Vindicator above
-- pairs the same trigger with no filter at all; this is the reading that looks at
-- what would have dealt the damage, and the shield is a FLOATING row where the
-- Vindicator's is a permanent's -- so this is also what observes
-- Replacement.printedBy's floating arm.
--
-- ONE board and ONE damage batch, run twice, differing only in the answer to CR
-- 609.7a's source choice. bob's black Cabal Evangel and his green Giant Spider
-- strike alice simultaneously; the shield names one of them, so exactly one
-- event is prevented and the other lands whichever way the choice went. Naming
-- the black source gains the life; naming the green one must not, and that is
-- the case an implementation with no filter fails -- it fires on its own
-- prevention whatever dealt the damage, and hands alice the 5 as life.
--
-- Distinct amounts on the two events, so no life total below can be reached two
-- ways: 20 - 5 + 2 is 17, 20 - 2 is 18, and the unfiltered reading's 20 - 2 + 5
-- is 23.
samiteMinistrationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
samiteMinistrationSpec s registry = Spec.describe s "Samite Ministration (CR 615.9 / 615.13)" $ do
  Spec.it s "CR 615.9 / 615.13 samite ministration gains life from a black source it named, and none from a green one" $ do
    plains <- S.printingOf s registry "Plains"
    evangelPrinting <- S.printingOf s registry "Cabal Evangel"
    spiderPrinting <- S.printingOf s registry "Giant Spider"
    ministration <- S.printingOf s registry "Samite Ministration"
    let base = S.landsInPlay plains 2
        (evangel, g1) = S.addPermanent evangelPrinting S.bob base
        (spider, g2) = S.addPermanent spiderPrinting S.bob g1
        (g3, spellId) = S.handOne ministration g2
        batch =
          [ DamageEvent.MkDamageEvent evangel (Recipient.ToPlayer S.alice) 2 False False False 0 Nothing DamageKind.Noncombat,
            DamageEvent.MkDamageEvent spider (Recipient.ToPlayer S.alice) 5 False False False 0 Nothing DamageKind.Noncombat
          ]
        run named =
          let shielded = castAndResolve (nameDamageSource named) g3 spellId
              (dealt, after) = strikeAndSettleWith (nameDamageSource named) shielded batch
           in (shielded, dealt, after)
        (blackShield, blackDealt, blackAfter) = run evangel
        (greenShield, greenDealt, greenAfter) = run spider
    Spec.assertEqWith s "setup: alice is at 20 before either branch" (S.lifeOf S.alice g3) (Just 20)
    Spec.assertEqWith s "setup: each branch installed one shield, naming the source it chose" (fmap shieldedSource [blackShield, greenShield]) [Just evangel, Just spider]
    -- The gameplay assertions, ahead of every proxy: alice's life is the only
    -- place the trigger shows, and the two branches disagree about it.
    Spec.assertEqWith s "naming the black source: the green 5 got through, and the prevented 2 came back as life" (S.lifeOf S.alice blackAfter) (Just 17)
    Spec.assertEqWith s "naming the green source: only the black 2 got through, and no life came back" (S.lifeOf S.alice greenAfter) (Just 18)
    Spec.assertEqWith s "the black branch gathered its trigger" (length (GameState.stack blackDealt)) 1
    Spec.assertEqWith s "and the green branch gathered nothing" (length (GameState.stack greenDealt)) 0
  -- The printed sentence names TWO colours, and a Filter that had dropped either
  -- disjunct would still pass the pair above. bob's red Goblin Piker on a board
  -- of its own, for a third amount again.
  Spec.it s "CR 615.13 the other disjunct: a red source gains the life too" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    ministration <- S.printingOf s registry "Samite Ministration"
    let base = S.landsInPlay plains 2
        (piker, g1) = S.addPermanent pikerPrinting S.bob base
        (g2, spellId) = S.handOne ministration g1
        shielded = castAndResolve (nameDamageSource piker) g2 spellId
        (dealt, after) = strikeAndSettleWith (nameDamageSource piker) shielded [DamageEvent.MkDamageEvent piker (Recipient.ToPlayer S.alice) 3 False False False 0 Nothing DamageKind.Noncombat]
    Spec.assertEqWith s "setup: the shield names the Piker" (shieldedSource shielded) (Just piker)
    Spec.assertEqWith s "the 3 was prevented and came back as life" (S.lifeOf S.alice after) (Just 23)
    Spec.assertEqWith s "one trigger was gathered" (length (GameState.stack dealt)) 1

-- alice is mid-combat attacking with `mine`; bob defends holding `spells` and
-- `lands` untapped Plains that pay for them. Sits at the declare attackers step
-- like every combatBoardOf board, so the ENGINE declares the attack and the
-- combat damage this group observes is CR 510.2's own, never hand-written.
-- CombatSpec.killShotBoard is the same shape one seat over.
tablesBoard :: Printing.Printing -> Int -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId])
tablesBoard plains lands mine spells =
  let (gs0, ours, _) = S.combatBoardOf mine []
      withLands = List.foldl' (\g _ -> snd (S.addPermanent plains S.bob g)) gs0 [1 .. lands]
      withCards = List.foldl' (\g p -> snd (S.addHandCard p S.bob g)) withLands spells
   in (withCards, ours)

-- Cast the named cards in the ORDER GIVEN, aiming every target at `victim`, and
-- attack with everything.
--
-- Order matters in the Kill Shot case, and one answerer cannot express it: both
-- spells declare the same Pool.Creatures/IsAttacking slot, and casting both in
-- one priority round puts Kill Shot on TOP of the stack, so it resolves first
-- and Turn the Tables fizzles under CR 608.2b with no row installed. That case
-- therefore runs one step per spell, naming one card each time.
castInOrder :: [String] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
castInOrder names victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction _ _ actions ->
    let isNamed n a = case a of
          Action.Cast _ cardName _ -> cardName == CardName.MkCardName (Text.pack n)
          _ -> False
     in case Maybe.mapMaybe (\n -> List.find (isNamed n) actions) names of
          h : _ -> h
          [] -> Action.Pass
  -- Blocks are DECLINED, so a case that gives bob a creature of his own still
  -- has the attacker's damage aimed at bob for the redirect to move.
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- Every floating redirection row, as (kind, source side, destination). The three
-- things Resolve bakes, read back off the store: a case that asserted only on
-- where the damage landed could not tell a redirect aimed at the right creature
-- from one that always aims at the first attacker.
redirectRows :: GameState.GameState -> [(Maybe DamageKind.DamageKind, Maybe Recipient.Recipient, Recipient.Recipient)]
redirectRows gs =
  [ (DamagePattern.whichKind pat, DamagePattern.whichRecipient pat, dest)
  | active <- GameState.replacements gs,
    ReplacementEffect.DamageR (DamageR.MkDamageR pat (DamageRewrite.Redirect dest) _) <- [ActiveReplacement.effect active]
  ]

-- CR 614.9: "Some effects replace damage dealt to one battle, creature,
-- planeswalker, or player with the same damage dealt to another ...; such
-- effects are called redirection effects."
--
-- Turn the Tables ({3}{W}{W}, Instant, Darksteel): "All combat damage that would
-- be dealt to you this turn is dealt to target attacking creature instead." bob
-- is the caster, because "you" is the redirect's controller and only the
-- DEFENDING player is being dealt combat damage worth moving.
turnTheTablesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
turnTheTablesSpec s registry = Spec.describe s "Turn the Tables (CR 614.9)" $ do
  let atCombatDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      hit src recipient n = DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      targets gs = fmap DamageEvent.target (S.damageEventsOf gs)
  -- THE WHOLE CARD. alice attacks with a lone Jedit Ojanen (5/5); bob redirects
  -- its combat damage onto Jedit itself, which is lethal to a 5/5 (CR 704.5g).
  Spec.it s "CR 614.9 whole card: the attacker's combat damage is dealt to the attacker instead of to bob" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    tables <- S.printingOf s registry "Turn the Tables"
    case tablesBoard plains 5 [jedit] [tables] of
      (gs, [attacker]) -> do
        let atDamage = atCombatDamage (castInOrder ["Turn the Tables"] attacker) gs
            after = S.runCombat (castInOrder ["Turn the Tables"] attacker) atDamage
        -- Combat-timing vacuity: a fixture that skipped combat would pass the
        -- life assertions below without dealing anything.
        Spec.assertEqWith s "setup: the spell resolved and combat damage has NOT been dealt yet" (GameState.phase atDamage) (Phase.Combat CombatStep.CombatDamage)
        -- The "did the target stick" trap: all three baked fields, not merely
        -- that some redirect exists.
        Spec.assertEqWith s "setup: one row, keyed to COMBAT damage to bob, aimed at the creature bob targeted" (redirectRows atDamage) [(Just DamageKind.Combat, Just (Recipient.ToPlayer S.bob), Recipient.ToCreature attacker)]
        Spec.assertEqWith s "the damage never reached bob" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "it landed on the attacker instead" (targets after) [Recipient.ToCreature attacker]
        -- The assertion that separates a redirect from a prevention: CR 614.9
        -- replaces the RECIPIENT and nothing else, so one event of the same size.
        Spec.assertEqWith s "one event, of the same amount" (amounts after) [5]
        Spec.assertEqWith s "5 marked on a 5/5 is lethal" (S.creaturesInPlay S.alice after) 0
        Spec.assertEqWith s "and nothing splashed onto the attacker's controller" (S.lifeOf S.alice after) (Just 20)
      _ -> Spec.assertFailure s "fixture should have one attacker"
  -- The BASELINE that makes the case above discriminate: the same board, the
  -- spell never cast. Every assertion moves.
  Spec.it s "CR 614.9 no redirect, no move: the same 5 lands on bob and the attacker survives" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    tables <- S.printingOf s registry "Turn the Tables"
    case tablesBoard plains 5 [jedit] [tables] of
      (gs, [attacker]) -> do
        let after = S.runCombat S.aggressiveAnswer gs
        Spec.assertEqWith s "setup: no row was installed" (redirectRows after) []
        Spec.assertEqWith s "bob takes all 5" (S.lifeOf S.bob after) (Just 15)
        Spec.assertEqWith s "addressed to bob" (targets after) [Recipient.ToPlayer S.bob]
        Spec.assertEqWith s "and the attacker is unharmed" (S.damageOf attacker after) (Just 0)
      _ -> Spec.assertFailure s "fixture should have one attacker"
  -- THE KIND NARROWING. "All COMBAT damage", so a noncombat event aimed at bob
  -- is none of the card's business. Without this case the whichKind thread is
  -- unproven, and dropping it would run WEAKER than printed in bob's favour.
  Spec.it s "CR 614.9 the printed narrowing: NONCOMBAT damage to bob is not redirected" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    tables <- S.printingOf s registry "Turn the Tables"
    case tablesBoard plains 5 [jedit] [tables] of
      (gs, [attacker]) -> do
        let atDamage = atCombatDamage (castInOrder ["Turn the Tables"] attacker) gs
            after = settleDamage S.identityAnswer atDamage [hit attacker (Recipient.ToPlayer S.bob) 4]
        Spec.assertEqWith s "setup: the redirect really is installed" (length (redirectRows atDamage)) 1
        Spec.assertEqWith s "bob takes the noncombat 4" (S.lifeOf S.bob after) (Just 16)
        Spec.assertEqWith s "addressed to bob, not moved" (targets after) [Recipient.ToPlayer S.bob]
        Spec.assertEqWith s "and the attacker is untouched" (S.damageOf attacker after) (Just 0)
      _ -> Spec.assertFailure s "fixture should have one attacker"
  -- CR 614.9's GUARD. "If one of those permanents is no longer on the
  -- battlefield when the damage would be redirected ... the effect does
  -- nothing." bob redirects onto the Hill Giant and then kills it with Kill
  -- Shot, so the surviving attacker's damage lands on bob exactly as if the
  -- redirect were not there.
  --
  -- "Does nothing" is not "prevents": the load-bearing assertion is that the
  -- event still HAPPENED. Dropping it instead would run weaker than printed.
  Spec.it s "CR 614.9 guard: a destination that left the battlefield makes the effect do nothing" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    hillGiant <- S.printingOf s registry "Hill Giant"
    tables <- S.printingOf s registry "Turn the Tables"
    killShot <- S.printingOf s registry "Kill Shot"
    case tablesBoard plains 8 [jedit, hillGiant] [tables, killShot] of
      (gs, [big, doomed]) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (castInOrder ["Turn the Tables"] doomed) gs
            atDamage = atCombatDamage (castInOrder ["Kill Shot"] doomed) atBlockers
            after = S.runCombat S.aggressiveAnswer atDamage
        -- The negative-cast trap: bob is at 15 either way if the spell was never
        -- cast at all, so the row's existence is asserted outright.
        Spec.assertEqWith s "setup: the redirect was installed, aimed at the doomed creature" (redirectRows atDamage) [(Just DamageKind.Combat, Just (Recipient.ToPlayer S.bob), Recipient.ToCreature doomed)]
        Spec.assertEqWith s "setup: Kill Shot really destroyed it" (S.creaturesInPlay S.alice atDamage) 1
        Spec.assertEqWith s "so the survivor's 5 lands on bob" (S.lifeOf S.bob after) (Just 15)
        Spec.assertEqWith s "and the event still HAPPENED -- 'does nothing' is not 'prevents'" (amounts after) [5]
        Spec.assertEqWith s "addressed to bob, its original recipient" (targets after) [Recipient.ToPlayer S.bob]
        Spec.assertEqWith s "the big attacker took nothing" (S.damageOf big after) (Just 0)
      _ -> Spec.assertFailure s "fixture should have two attackers"
  -- CR 615.12 is about PREVENTION effects, and CR 614.9's redirection is not one
  -- (CR 615.1a: it never says "prevent"). Spider-Punk's "damage can't be
  -- prevented" therefore has nothing to say to it, and the damage still moves.
  --
  -- This is the case that gives Replacement.prevents' Redirect arm an observer:
  -- classify a redirect as a prevention and `inertPrevention` makes it do
  -- nothing here, though the CR 615.13 trigger route cannot tell the two apart
  -- (a redirect shrinks no event, so preventionBy reports nothing either way).
  Spec.it s "CR 615.12 a redirect is not a prevention: unpreventable damage is still moved" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    spiderPunk <- S.printingOf s registry "Spider-Punk"
    tables <- S.printingOf s registry "Turn the Tables"
    case tablesBoard plains 5 [jedit] [tables] of
      (gs, [attacker]) -> do
        let (punk, withPunk) = S.addPermanent spiderPunk S.bob gs
            after = S.runCombat (castInOrder ["Turn the Tables"] attacker) withPunk
        Spec.assertBool s (Set.member punk (GameState.battlefield withPunk)) "setup: Spider-Punk is out, so no damage can be prevented"
        Spec.assertEqWith s "the damage still left bob" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "and landed on the attacker" (targets after) [Recipient.ToCreature attacker]
        Spec.assertEqWith s "at its full size" (amounts after) [5]
      _ -> Spec.assertFailure s "fixture should have one attacker"

-- The SOURCE each floating redirection row watches (DamagePattern.whichSource),
-- read off the store. redirectRows above reads the other three baked fields; a
-- fourth element there would have moved every Turn the Tables tuple, and this is
-- a proxy for one group rather than a field those cases assert on.
redirectSources :: GameState.GameState -> [Maybe ObjectId.ObjectId]
redirectSources gs =
  [ DamagePattern.whichSource pat
  | active <- GameState.replacements gs,
    ReplacementEffect.DamageR (DamageR.MkDamageR pat (DamageRewrite.Redirect _) _) <- [ActiveReplacement.effect active]
  ]

-- CR 609.7a's chosen source on CR 614.9's REDIRECTION, whose producer is
-- Oracle's Attendants ({3}{W} Creature -- Human Soldier, 1/5: "{T}: All damage
-- that would be dealt to target creature this turn by a source of your choice is
-- dealt to this creature instead").
--
-- Turn the Tables above with CR 609.7a's clause added, which is exactly the
-- difference the rule makes: that redirection moves damage from EVERY source,
-- this one only from the ONE object its controller chose when the effect was
-- created. Auriok Replica carries the same field on CR 615.1's PREVENTION side;
-- this is the same choice on CR 614.1's replacement side.
--
-- The chosen source is deliberately NOT the first candidate the prompt offers:
-- CR 609.7a's pool here holds alice's Plains, the Attendants, the victim and the
-- two Pikers, sorted ascending, and the Plains was placed first -- so an engine
-- that ignored the answer and took the head would watch the Plains and both
-- damage assertions would read the other way round.
--
-- "This creature" is the destination, so the Attendants is BOTH the ability's
-- source and where the damage lands: `to` is EachMatching Filter.IsSource, and
-- the case asserts that sweep resolved to the Attendants alone by reading the
-- damage off it.
oraclesAttendantsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
oraclesAttendantsSpec s registry = Spec.describe s "Oracle's Attendants (CR 614.9, CR 609.7a)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
  Spec.it s "CR 609.7a the redirection moves the chosen source's damage and no other source's" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    attendants <- S.printingOf s registry "Oracle's Attendants"
    let base = S.landsInPlay plains 1
        (attendantsId, g1) = S.addPermanent attendants S.alice base
        (victim, g2) = S.addPermanent pikerPrinting S.alice g1
        (alpha, g3) = S.addPermanent pikerPrinting S.bob g2
        (omega, g4) = S.addPermanent pikerPrinting S.bob g3
        activate = Activate.activateAbility S.alice attendantsId (theAbility attendants) Monad.>> Stack.resolveTop
        aimed = S.runPure (aimAndChoose victim omega) g4 activate
        strike src n g = S.runPure S.identityAnswer g (Damage.applyDamage [hit src (Recipient.ToCreature victim) n])
    -- THE gameplay assertion, and the one this unit exists for: the source alice
    -- did NOT choose is not redirected, so its damage stays on the creature it
    -- was aimed at. Before the chosenSource field this 2 moved to the
    -- Attendants, the row naming no source at all.
    Spec.assertEqWith s "the unchosen source's 2 stays on the victim" (S.damageOf victim (strike alpha 2 aimed)) (Just 2)
    Spec.assertEqWith s "and nothing landed on the Attendants" (S.damageOf attendantsId (strike alpha 2 aimed)) (Just 0)
    -- Its twin, on the same board and differing in one thing: the chosen
    -- source's damage IS moved, so the case cannot pass by installing no row.
    Spec.assertEqWith s "the chosen source's 3 leaves the victim" (S.damageOf victim (strike omega 3 aimed)) (Just 0)
    -- CR 614.9 replaces the RECIPIENT and nothing else, so the whole 3 arrives:
    -- "does nothing" and "prevents" are what this separates the redirect from.
    Spec.assertEqWith s "and lands whole on the Attendants, the ability's own source" (S.damageOf attendantsId (strike omega 3 aimed)) (Just 3)
    -- The proxies, after the behaviour: a row exists, it watches the object
    -- alice named, and she was asked at all.
    Spec.assertEqWith s "setup: one redirection row, from the targeted victim to the Attendants" (redirectRows aimed) [(Nothing, Just (Recipient.ToCreature victim), Recipient.ToCreature attendantsId)]
    Spec.assertEqWith s "and it watches the source alice chose" (redirectSources aimed) [Just omega]
    Spec.assertEqWith s "alice was asked which source, and answered omega" (chosenSourcesIn (answersFor (aimAndChoose victim omega) g4 activate)) [omega]
  -- The discriminating twin, differing from the case above in the ANSWER alone:
  -- CR 609.7a's source is the player's choice, so the same board answering alpha
  -- moves alpha's damage and leaves omega's where it was. Without it the case
  -- above could pass on an engine that baked a fixed candidate -- the last one,
  -- say -- rather than the one alice named.
  Spec.it s "CR 609.7a the same board answering the OTHER source moves that one's damage instead" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    attendants <- S.printingOf s registry "Oracle's Attendants"
    let base = S.landsInPlay plains 1
        (attendantsId, g1) = S.addPermanent attendants S.alice base
        (victim, g2) = S.addPermanent pikerPrinting S.alice g1
        (alpha, g3) = S.addPermanent pikerPrinting S.bob g2
        (omega, g4) = S.addPermanent pikerPrinting S.bob g3
        activate = Activate.activateAbility S.alice attendantsId (theAbility attendants) Monad.>> Stack.resolveTop
        aimed = S.runPure (aimAndChoose victim alpha) g4 activate
        strike src n g = S.runPure S.identityAnswer g (Damage.applyDamage [hit src (Recipient.ToCreature victim) n])
    Spec.assertEqWith s "alpha's 2 moves now that alpha is the chosen source" (S.damageOf attendantsId (strike alpha 2 aimed)) (Just 2)
    Spec.assertEqWith s "and the victim took none of it" (S.damageOf victim (strike alpha 2 aimed)) (Just 0)
    Spec.assertEqWith s "omega's 3 stays on the victim" (S.damageOf victim (strike omega 3 aimed)) (Just 3)
    Spec.assertEqWith s "alice was asked which source, and answered alpha" (chosenSourcesIn (answersFor (aimAndChoose victim alpha) g4 activate)) [alpha]

-- Every floating COUNTED redirection row, as (remaining, source side, destination,
-- watched source): redirectRows' twin for DamageRewrite.RedirectNext, read back
-- off the store so a case can tell a spent row from one never installed.
countedRedirectRows :: GameState.GameState -> [(Natural.Natural, Maybe Recipient.Recipient, Recipient.Recipient, Maybe ObjectId.ObjectId)]
countedRedirectRows gs =
  [ (remaining, DamagePattern.whichRecipient pat, dest, DamagePattern.whichSource pat)
  | active <- GameState.replacements gs,
    ReplacementEffect.DamageR (DamageR.MkDamageR pat (DamageRewrite.RedirectNext remaining dest) _) <- [ActiveReplacement.effect active]
  ]

-- Carom's two slots aimed by NAME, `from` at the victim and `to` at the haven,
-- each filtered out of its offered set rather than built.
aimFromTo :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimFromTo victim haven p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    Map.mapWithKey (\slot (_, legal) -> Set.filter ((==) (Just (if slot == SlotName.MkSlotName (Text.pack "from") then victim else haven)) . Recipient.objectOf) legal) sets
  _ -> S.identityAnswer p

-- Aim Harm's Way at bob and choose `src` as CR 609.7a's source.
aimAtBobChoosing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtBobChoosing src p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) sets
  Prompt.ChooseDamageSource _ _ _ candidates ->
    Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== src) (NonEmpty.toList candidates))
  _ -> S.identityAnswer p

-- Spend a contested countdown on the batch's hits in `wanted` order, by
-- RECIPIENT, deflectionCombat's shape.
orderedBy :: [Recipient.Recipient] -> Prompt.Prompt r -> r
orderedBy wanted p = case p of
  Prompt.OrderDamage _ _ events ->
    let rank e = Maybe.fromMaybe (length wanted) (List.elemIndex (DamageEvent.target e) wanted)
     in fmap fst (List.sortOn (rank . snd) (zip [0 ..] events))
  _ -> S.identityAnswer p

-- CR 614.9's redirection with CR 615.7's countdown, whose producer is Carom
-- ({1}{W} Instant: "The next 1 damage that would be dealt to target creature
-- this turn is dealt to another target creature instead. / Draw a card."; name,
-- cost, type line and Oracle text checked against api.scryfall.com 2026-09-03).
--
-- Turn the Tables above with an AMOUNT: that redirection moves every event for
-- the turn, this one moves 1 damage and is then spent -- so a 3-damage event
-- comes out as TWO events, 1 on the other creature and 2 where it was aimed.
-- The Oracle rulings on Harm's Way, the same shape one card over, say so:
-- "Harm's Way will redirect just 2 of that damage."
--
-- The victim and the haven are both alice's Goblin Pikers, so a redirect aimed
-- the wrong way round moves damage between two things the board tells apart
-- only by id; bob's Piker is the source. alice's library is stocked for the
-- card's second sentence.
caromSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
caromSpec s registry = Spec.describe s "Carom (CR 614.9, CR 615.7)" $ do
  let hit src recipient n = DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = List.sort (fmap DamageEvent.amount (S.damageEventsOf gs))
      targets gs = fmap DamageEvent.target (S.damageEventsOf gs)
      strike src recipient n g = S.runPure S.identityAnswer g (Damage.applyDamage [hit src recipient n])
      board = do
        plains <- S.printingOf s registry "Plains"
        piker <- S.printingOf s registry "Goblin Piker"
        carom <- S.printingOf s registry "Carom"
        let base = S.landsInPlay plains 2
            (victim, g1) = S.addPermanent piker S.alice base
            (haven, g2) = S.addPermanent piker S.alice g1
            (attacker, g3) = S.addPermanent piker S.bob g2
            (caromId, g4) = S.addHandCard carom S.alice g3
            (_, g5) = S.addLibraryCard plains S.alice g4
            ready =
              g5
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            redirected = S.runPure (aimFromTo victim haven) ready (S.cast S.alice caromId Monad.>> Stack.resolveTop)
        pure (victim, haven, attacker, ready, redirected)
  Spec.it s "CR 615.7 the next 1 of a 3-damage event moves and the other 2 stay where they were aimed" $ do
    (victim, haven, attacker, _, redirected) <- board
    let after = strike attacker (Recipient.ToCreature victim) 3 redirected
    -- THE gameplay assertion, and the pair that separates a counted redirect
    -- from Turn the Tables: the victim keeps 2 and the haven takes 1.
    Spec.assertEqWith s "the victim takes 2 of the 3" (S.damageOf victim after) (Just 2)
    Spec.assertEqWith s "and the haven takes the 1 that moved" (S.damageOf haven after) (Just 1)
    -- CR 614.9 moves "the same damage": two events of the same source, and
    -- nothing prevented.
    Spec.assertEqWith s "two events, 1 and 2" (amounts after) [1, 2]
    Spec.assertEqWith s "the row is spent and gone" (countedRedirectRows after) []
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "setup: one counted row of 1, from the victim to the haven" (countedRedirectRows redirected) [(1, Just (Recipient.ToCreature victim), Recipient.ToCreature haven, Nothing)]
  Spec.it s "CR 615.7 once spent, the next event stays whole" $ do
    (victim, haven, attacker, _, redirected) <- board
    let spent = strike attacker (Recipient.ToCreature victim) 3 redirected
        after = strike attacker (Recipient.ToCreature victim) 2 spent
    Spec.assertEqWith s "the second event's 2 all lands on the victim" (S.damageOf victim after) (Just 4)
    Spec.assertEqWith s "and the haven keeps the 1 alone" (S.damageOf haven after) (Just 1)
  Spec.it s "CR 614.9 a 1-damage event moves whole, and the row is spent by it" $ do
    (victim, haven, attacker, _, redirected) <- board
    let after = strike attacker (Recipient.ToCreature victim) 1 redirected
    Spec.assertEqWith s "the victim takes nothing" (S.damageOf victim after) (Just 0)
    Spec.assertEqWith s "the haven takes the 1" (S.damageOf haven after) (Just 1)
    Spec.assertEqWith s "one event, addressed to the haven" (targets after) [Recipient.ToCreature haven]
    Spec.assertEqWith s "the row is spent and gone" (countedRedirectRows after) []
  -- The BASELINE that makes the cases above discriminate: the same board, the
  -- spell never cast.
  Spec.it s "CR 614.9 no redirect, no move: the whole 3 lands on the victim" $ do
    (victim, haven, attacker, ready, _) <- board
    let after = strike attacker (Recipient.ToCreature victim) 3 ready
    Spec.assertEqWith s "the victim takes all 3" (S.damageOf victim after) (Just 3)
    Spec.assertEqWith s "the haven takes nothing" (S.damageOf haven after) (Just 0)
    Spec.assertEqWith s "one event" (amounts after) [3]
  -- The other recipient is not covered: damage aimed at the haven stays there.
  Spec.it s "CR 614.9 the redirection covers the creature the spell named and not the other" $ do
    (victim, haven, attacker, _, redirected) <- board
    let after = strike attacker (Recipient.ToCreature haven) 2 redirected
    Spec.assertEqWith s "the haven takes its own 2" (S.damageOf haven after) (Just 2)
    Spec.assertEqWith s "the victim takes nothing" (S.damageOf victim after) (Just 0)
    Spec.assertEqWith s "and the row is unspent" (fmap (\(n, _, _, _) -> n) (countedRedirectRows after)) [1]
  -- The card's second sentence, so the whole card is exercised.
  Spec.it s "the card draws as it resolves" $ do
    (_, _, _, ready, redirected) <- board
    Spec.assertEqWith s "Carom left the hand and a card arrived" (S.handSize S.alice redirected) (S.handSize S.alice ready)

-- CR 614.9's redirection over a DESCRIBED recipient side with CR 615.7's
-- countdown and CR 609.7a's chosen source, whose producer is Harm's Way ({W}
-- Instant: "The next 2 damage that a source of your choice would deal to you
-- and/or permanents you control this turn is dealt to any target instead.";
-- name, cost, type line and Oracle text checked against api.scryfall.com
-- 2026-09-03).
--
-- Divine Deflection's recipient side on a redirection: ONE row and one
-- countdown over alice and everything she controls, read live at each damage
-- event, rather than a row per recipient. The destination is bob himself, so the
-- moved damage is read off a life total.
--
-- The chosen source is omega, deliberately not the first candidate offered,
-- Oracle's Attendants' reason.
harmsWaySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
harmsWaySpec s registry = Spec.describe s "Harm's Way (CR 614.9, CR 615.7, CR 609.7a)" $ do
  let hit src recipient n = DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      targets gs = fmap DamageEvent.target (S.damageEventsOf gs)
      strike src recipient n g = S.runPure S.identityAnswer g (Damage.applyDamage [hit src recipient n])
      board = do
        plains <- S.printingOf s registry "Plains"
        piker <- S.printingOf s registry "Goblin Piker"
        harmsWay <- S.printingOf s registry "Harm's Way"
        let base = S.landsInPlay plains 1
            (mine, g1) = S.addPermanent piker S.alice base
            (alpha, g2) = S.addPermanent piker S.bob g1
            (omega, g3) = S.addPermanent piker S.bob g2
            (harmsWayId, g4) = S.addHandCard harmsWay S.alice g3
            ready =
              g4
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            redirected = S.runPure (aimAtBobChoosing omega) ready (S.cast S.alice harmsWayId Monad.>> Stack.resolveTop)
        pure (mine, alpha, omega, ready, redirected)
  Spec.it s "CR 615.7 the chosen source's 5 to alice: 2 is dealt to bob and 3 to alice" $ do
    (_, _, omega, _, redirected) <- board
    let after = strike omega (Recipient.ToPlayer S.alice) 5 redirected
    -- THE gameplay assertion: the moved 2 lands on the target, the rest stays.
    Spec.assertEqWith s "bob, the target, takes the 2 that moved" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "alice takes the remaining 3" (S.lifeOf S.alice after) (Just 17)
    Spec.assertEqWith s "two events, the moved one first" (zip (amounts after) (targets after)) [(2, Recipient.ToPlayer S.bob), (3, Recipient.ToPlayer S.alice)]
    Spec.assertEqWith s "the row is spent and gone" (countedRedirectRows after) []
    -- The proxies, after the behaviour: one described row, naming no recipient
    -- by id, aimed at bob and watching omega.
    Spec.assertEqWith s "setup: one counted row of 2 over a described recipient side" (countedRedirectRows redirected) [(2, Nothing, Recipient.ToPlayer S.bob, Just omega)]
  Spec.it s "CR 611.2c a permanent alice controls is covered by the same countdown" $ do
    (mine, _, omega, _, redirected) <- board
    let first = strike omega (Recipient.ToCreature mine) 1 redirected
        second = strike omega (Recipient.ToCreature mine) 3 first
        third = strike omega (Recipient.ToCreature mine) 2 second
    Spec.assertEqWith s "the 1 to alice's creature moves whole" (S.damageOf mine first) (Just 0)
    Spec.assertEqWith s "and lands on bob" (S.lifeOf S.bob first) (Just 19)
    -- CR 615.7's one countdown: 1 is left, so the next 3 splits 1 and 2.
    Spec.assertEqWith s "the next 3 leaves 2 on the creature" (S.damageOf mine second) (Just 2)
    Spec.assertEqWith s "and 1 more on bob" (S.lifeOf S.bob second) (Just 18)
    Spec.assertEqWith s "and once spent, the next 2 stays whole" (S.damageOf mine third) (Just 4)
    Spec.assertEqWith s "bob takes no more" (S.lifeOf S.bob third) (Just 18)
  -- CR 609.7a: the row watches the ONE source alice chose.
  Spec.it s "CR 609.7a the unchosen source's damage to alice stays where it was aimed" $ do
    (_, alpha, _, _, redirected) <- board
    let after = strike alpha (Recipient.ToPlayer S.alice) 3 redirected
    Spec.assertEqWith s "alice takes alpha's whole 3" (S.lifeOf S.alice after) (Just 17)
    Spec.assertEqWith s "and bob takes nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and the row is unspent" (fmap (\(n, _, _, _) -> n) (countedRedirectRows after)) [2]
  -- The description's other edge: a permanent bob controls is not "a permanent
  -- you control".
  Spec.it s "CR 611.2c the chosen source's damage to bob's own creature is not covered" $ do
    (_, alpha, omega, _, redirected) <- board
    let after = strike omega (Recipient.ToCreature alpha) 3 redirected
    Spec.assertEqWith s "bob's creature takes the whole 3" (S.damageOf alpha after) (Just 3)
    Spec.assertEqWith s "and bob takes nothing" (S.lifeOf S.bob after) (Just 20)
  -- CR 615.7's allocation on a redirection: "If the chosen source would
  -- simultaneously deal damage to multiple permanents you control ... Harm's Way
  -- will redirect just 2 of that damage ... You choose which 2 damage is
  -- redirected" (Oracle rulings). One question to alice, and the answer decides
  -- which recipient keeps its damage.
  Spec.it s "CR 615.7 a simultaneous batch contends for the 2, and alice orders it" $ do
    (mine, _, omega, _, redirected) <- board
    let batch = [hit omega (Recipient.ToCreature mine) 3, hit omega (Recipient.ToPlayer S.alice) 3]
        creatureFirst = S.runPure (orderedBy [Recipient.ToCreature mine]) redirected (Damage.applyDamage batch)
        aliceFirst = S.runPure (orderedBy [Recipient.ToPlayer S.alice]) redirected (Damage.applyDamage batch)
    Spec.assertEqWith s "creature first: it keeps 1" (S.damageOf mine creatureFirst) (Just 1)
    Spec.assertEqWith s "and alice keeps her whole 3" (S.lifeOf S.alice creatureFirst) (Just 17)
    Spec.assertEqWith s "alice first: she keeps 1" (S.lifeOf S.alice aliceFirst) (Just 19)
    Spec.assertEqWith s "and the creature keeps its whole 3" (S.damageOf mine aliceFirst) (Just 3)
    Spec.assertEqWith s "either way bob takes exactly the 2" (fmap (`S.lifeOf` creatureFirst) [S.bob] <> fmap (`S.lifeOf` aliceFirst) [S.bob]) [Just 18, Just 18]
    Spec.assertBool s (wasAskedToOrderDamage (answersFor (orderedBy [Recipient.ToPlayer S.alice]) redirected (Damage.applyDamage batch))) "alice was asked to order the batch"
  -- CR 615.12 speaks of PREVENTION effects, and a redirection is not one, so
  -- unpreventable damage still contends for the 2 and is still moved. This is
  -- what gives Replacement.contested's `contends` its observer: filtering the
  -- batch by preventability, as a prevention shield's contest is, would leave
  -- nothing to order here.
  Spec.it s "CR 615.12 unpreventable damage is still moved, and still contended for" $ do
    (mine, _, omega, _, redirected) <- board
    spiderPunk <- S.printingOf s registry "Spider-Punk"
    let (_, withPunk) = S.addPermanent spiderPunk S.bob redirected
        batch = [hit omega (Recipient.ToCreature mine) 3, hit omega (Recipient.ToPlayer S.alice) 3]
        aliceFirst = S.runPure (orderedBy [Recipient.ToPlayer S.alice]) withPunk (Damage.applyDamage batch)
    Spec.assertEqWith s "alice first: she keeps 1" (S.lifeOf S.alice aliceFirst) (Just 19)
    Spec.assertEqWith s "the creature keeps its whole 3" (S.damageOf mine aliceFirst) (Just 3)
    Spec.assertEqWith s "and bob takes the 2" (S.lifeOf S.bob aliceFirst) (Just 18)
    Spec.assertBool s (wasAskedToOrderDamage (answersFor (orderedBy [Recipient.ToPlayer S.alice]) withPunk (Damage.applyDamage batch))) "alice was asked to order the batch"
  -- The BASELINE: the same board, the spell never cast.
  Spec.it s "CR 614.9 no redirect, no move: alice takes the whole 5" $ do
    (_, _, omega, ready, _) <- board
    let after = strike omega (Recipient.ToPlayer S.alice) 5 ready
    Spec.assertEqWith s "alice takes all 5" (S.lifeOf S.alice after) (Just 15)
    Spec.assertEqWith s "bob takes nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "setup: no row" (countedRedirectRows ready) []

-- CR 614.9 with BOTH halves of the sentence printed on a card, which is what
-- separates this group from Turn the Tables and Oracle's Attendants above: those
-- two are resolutions, so the engine bakes their recipient and their destination
-- as ids. Pariah ({2}{W} Enchantment -- Aura, "Enchant creature / All damage
-- that would be dealt to you is dealt to enchanted creature instead"; name,
-- cost, type line and Oracle text checked against api.scryfall.com 2026-08-28,
-- printed on paper from Urza's Saga onward) is a PERMANENT's static ability, so
-- there is no resolution to bake anything: "you" is
-- DamagePattern.whoRecipient and "enchanted creature" is
-- DamageRewrite.RedirectMatching's Filter.IsHostOfSource, read live at the
-- damage event.
--
-- THREE boards differing in one thing each, because a redirect that fired
-- unconditionally is indistinguishable from one that reads its condition:
--
--   * the recipient is alice (the Aura's controller) -- redirected;
--   * the recipient is bob -- CR 109.5's "you" is not him, so it lands on him;
--   * the recipient is an OBJECT, and one alice controls -- a Filter cannot
--     describe a player, so the pattern's player half must not admit it either.
--
-- TWO of alice's creatures, the Oppressive Rays board's trick for the same
-- reason: the destination is a description, so a board with one creature cannot
-- tell "the enchanted creature" from "a creature alice controls" from "the first
-- permanent on the battlefield". Jedit Ojanen (5/5) is the host and survives; the
-- Goblin Piker (2/1) beside it never takes anything.
--
-- Numbers all distinct -- 3 redirected, 4 to bob, 2 to the bystander -- so no two
-- readings meet on one of them.
pariahSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
pariahSpec s registry = Spec.describe s "Pariah (CR 614.9)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      board plains jedit pikerPrinting pariah =
        let base = S.landsInPlay plains 1
            (host, g1) = S.addPermanent jedit S.alice base
            (bystander, g2) = S.addPermanent pikerPrinting S.alice g1
            (source, g3) = S.addPermanent pikerPrinting S.bob g2
            (aura, g4) = S.addPermanent pariah S.alice g3
         in (host, bystander, source, aura, S.attach aura host g4)
  Spec.it s "CR 614.9 damage that would be dealt to you is dealt to the enchanted creature instead" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    pariah <- S.printingOf s registry "Pariah"
    let (host, bystander, source, aura, gs) = board plains jedit pikerPrinting pariah
        atAlice = settleDamage S.identityAnswer gs [hit source (Recipient.ToPlayer S.alice) 3]
    Spec.assertEqWith s "alice loses no life" (S.lifeOf S.alice atAlice) (Just 20)
    -- CR 614.9 is a replacement and not a prevention (CR 615.1a), so the amount
    -- rides across WHOLE: a prevention would have shown 0 here.
    Spec.assertEqWith s "the 3 is marked on the enchanted creature instead" (S.damageOf host atAlice) (Just 3)
    Spec.assertEqWith s "and none of it on the creature beside it" (S.damageOf bystander atAlice) (Just 0)
    -- The proxy, after the behaviour.
    Spec.assertEqWith s "setup: the Aura is attached to the host" (Projection.hostOf aura gs) (Just host)
  -- THE CONTROL on the player half: bob is not CR 109.5's "you", so the same
  -- source's damage reaches him. A redirect that ignored `whoRecipient` would
  -- have moved this onto alice's creature too.
  Spec.it s "CR 109.5 the same Aura moves nothing that was aimed at the opponent" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    pariah <- S.printingOf s registry "Pariah"
    let (host, bystander, source, _aura, gs) = board plains jedit pikerPrinting pariah
        aimedAtBob = settleDamage S.identityAnswer gs [hit source (Recipient.ToPlayer S.bob) 4]
    Spec.assertEqWith s "bob loses the whole 4" (S.lifeOf S.bob aimedAtBob) (Just 16)
    Spec.assertEqWith s "nothing is marked on the enchanted creature" (S.damageOf host aimedAtBob) (Just 0)
    Spec.assertEqWith s "nor on the creature beside it" (S.damageOf bystander aimedAtBob) (Just 0)
  -- THE CONTROL on the object half: CR 120.3's other kind of recipient. Pariah
  -- names a player and no object, so damage aimed at a permanent alice controls
  -- is not its business -- which is what keeps `whoRecipient` from being read as
  -- "anything of yours".
  Spec.it s "CR 120.3 damage aimed at a permanent alice controls is not redirected" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    pariah <- S.printingOf s registry "Pariah"
    let (host, bystander, source, _aura, gs) = board plains jedit pikerPrinting pariah
        atBystander = settleDamage S.identityAnswer gs [hit source (Recipient.ToCreature bystander) 2]
    Spec.assertEqWith s "the 2 stays on the creature it was aimed at" (S.damageOf bystander atBystander) (Just 2)
    Spec.assertEqWith s "and none of it moves onto the enchanted creature" (S.damageOf host atBystander) (Just 0)
    Spec.assertEqWith s "alice still loses no life either" (S.lifeOf S.alice atBystander) (Just 20)

-- Aim every target slot at one object, and answer CR 601.2b's X with `n`.
-- FILTERED and not built, aimAndChoose's posture: the recipient comes out of the
-- set the prompt offered, so a Pool.AnyTarget slot's own tag is what reaches the
-- engine rather than a hand-made Recipient that CR 608.2b would drop.
aimObjectWithX :: Natural.Natural -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimObjectWithX n oid p = case p of
  Prompt.ChooseX {} -> n
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- The same, aimed at a PLAYER instead: CR 115.4's "any target" offers both kinds
-- of recipient, and Lava Burst's clause narrows to one of them.
aimPlayerWithX :: Natural.Natural -> PlayerId.PlayerId -> Prompt.Prompt r -> r
aimPlayerWithX n pid p = case p of
  Prompt.ChooseX {} -> n
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just pid) . Recipient.playerOf) . snd) sets
  _ -> S.identityAnswer p

-- CR 615.12 and CR 614.9 on the CR 611.2c STORED carrier -- a spell speaking
-- about the damage its own resolution is about to deal -- whose producer is Lava
-- Burst ({X}{R} Sorcery, "Lava Burst deals X damage to any target. If Lava Burst
-- would deal damage to a creature, that damage can't be prevented or dealt
-- instead to another permanent or player"; name, cost, type line and Oracle text
-- checked against api.scryfall.com 2026-08-27, printed on paper in Ice Age and
-- Deckmasters).
--
-- Excruciator's clause above says the same thing from the PRINTED carrier, where
-- the source is a permanent on the battlefield to be asked about. This one is
-- the other carrier: Effect.AffectPlayers stores the effect with
-- ActivePlayerEffect.source set to the resolving spell, and Filter.IsSource in
-- the pattern names that spell. Nothing here was observable while
-- Pawl.Engine.PlayerEffect.applying hardcoded Nothing in that position -- the
-- card loaded, round-tripped and silently never fired.
--
-- The DURATION is UntilEndOfTurn where the sentence is about one resolution, and
-- the two are observably the same: CR 400.7 mints a new object as the spell
-- leaves the stack, so the id Filter.IsSource holds matches nothing once Lava
-- Burst is in the graveyard.
--
-- THREE cases, one per limb of the clause. The first and the last each carry a
-- control on their OWN board differing in the SOURCE alone, and the middle one
-- is itself the control for the RECIPIENT limb -- so none can pass on an engine
-- that suppressed the whole class of preventions or redirections.
lavaBurstSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lavaBurstSpec s registry = Spec.describe s "Lava Burst (CR 615.12, CR 614.9)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
  -- CR 615.12 from the stored carrier, with its control on the same board: the
  -- shield prevents the Piker's damage whole and none of Lava Burst's.
  Spec.it s "CR 615.12 the shield prevents none of Lava Burst's 3 and is not reduced by it" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    mendingHands <- S.printingOf s registry "Mending Hands"
    lavaBurst <- S.printingOf s registry "Lava Burst"
    let base = S.landsFor mountain S.alice 4 (S.landsInPlay plains 1)
        (victim, g1) = S.addPermanent jedit S.alice base
        (decoy, g2) = S.addPermanent pikerPrinting S.bob g1
        (g3, mendId) = S.handOne mendingHands g2
        (g4, burstId) = S.handOne lavaBurst g3
        -- The shield is REALLY cast, so its pattern and its stored amount are the
        -- codec's parse of the committed card rather than a hand-built row.
        shielded = S.runPure (aimObjectWithX 0 victim) g4 (S.cast S.alice mendId Monad.>> Stack.resolveTop)
        burnt = S.runPure (aimObjectWithX 3 victim) shielded (S.cast S.alice burstId Monad.>> Stack.resolveTop)
    -- THE gameplay assertion, and the one #844 existed for: the whole 3 is marked
    -- on the shielded creature. With the source dropped on the way out of
    -- `applying`, Filter.IsSource answered False and the shield ate all of it.
    Spec.assertEqWith s "the whole 3 is marked on the shielded creature" (S.damageOf victim burnt) (Just 3)
    -- CR 615.12's last sentence: "existing damage prevention shields won't be
    -- reduced by damage that can't be prevented".
    Spec.assertEqWith s "and the shield still holds all 4" (shieldsLeft burnt) [4]
    -- THE CONTROL, on the same board and differing in the SOURCE alone: the
    -- Piker's 2 is a source Lava Burst's clause does not name, so the same shield
    -- prevents it whole and is spent for it.
    let elsewhere = settleDamage S.identityAnswer burnt [hit decoy (Recipient.ToCreature victim) 2]
    Spec.assertEqWith s "the Piker's 2 is prevented, so nothing more is marked" (S.damageOf victim elsewhere) (Just 3)
    Spec.assertEqWith s "and 2 of the shield's 4 were spent for it" (shieldsLeft elsewhere) [2]
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "setup: the shield is a floating replacement holding 4" (shieldsLeft shielded) [4]
    Spec.assertEqWith s "setup: Lava Burst resolved out of hand" (length (GameState.stack burnt)) 0
  -- The RECIPIENT limb, which is the other half of Lava Burst's condition: the
  -- clause says "if Lava Burst would deal damage to A CREATURE", so its own
  -- damage dealt to a PLAYER is preventable like anybody's. Same card, same
  -- shield amount and same X as the case above; what differs is the KIND of
  -- recipient both are aimed at.
  Spec.it s "CR 615.12 the same shield still prevents Lava Burst's 3 dealt to a player" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    mendingHands <- S.printingOf s registry "Mending Hands"
    lavaBurst <- S.printingOf s registry "Lava Burst"
    let base = S.landsFor mountain S.alice 4 (S.landsInPlay plains 1)
        (g1, mendId) = S.handOne mendingHands base
        (g2, burstId) = S.handOne lavaBurst g1
        shielded = S.runPure (aimPlayerWithX 0 S.bob) g2 (S.cast S.alice mendId Monad.>> Stack.resolveTop)
        burnt = S.runPure (aimPlayerWithX 3 S.bob) shielded (S.cast S.alice burstId Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "bob loses no life, the shield having prevented the lot" (S.lifeOf S.bob burnt) (Just 20)
    Spec.assertEqWith s "and 3 of the shield's 4 were spent for it" (shieldsLeft burnt) [1]
    Spec.assertEqWith s "setup: the shield is a floating replacement holding 4" (shieldsLeft shielded) [4]
  -- CR 614.9, the redirection limb, and #1681's own case. Oracle's Attendants
  -- ({3}{W} Creature -- Human Soldier, 1/5: "{T}: All damage that would be dealt
  -- to target creature this turn by a source of your choice is dealt to this
  -- creature instead") is the pool's redirection of damage aimed at a CREATURE,
  -- which is exactly the class Lava Burst's clause forbids. Turn the Tables
  -- cannot be used here: its redirection moves damage dealt to YOU, which the
  -- clause never reaches, so a board built on it reads the same either way.
  --
  -- TWO Attendants, one watching Lava Burst and one watching bob's Piker, so the
  -- prohibited redirect and the permitted one are on ONE board and differ only in
  -- which source their controller chose.
  Spec.it s "CR 614.9 Lava Burst's damage to a creature is not redirected, and another source's still is" $ do
    mountain <- S.printingOf s registry "Mountain"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    attendants <- S.printingOf s registry "Oracle's Attendants"
    lavaBurst <- S.printingOf s registry "Lava Burst"
    let base = S.landsInPlay mountain 4
        (watchingBurst, g1) = S.addPermanent attendants S.alice base
        (watchingPiker, g2) = S.addPermanent attendants S.alice g1
        (victim, g3) = S.addPermanent jedit S.alice g2
        (decoy, g4) = S.addPermanent pikerPrinting S.bob g3
        (g5, burstId) = S.handOne lavaBurst g4
        -- The SPELL's own id, read off the stack: CR 609.7a's candidate classes
        -- include "a spell on the stack", and CR 400.7 already made the card and
        -- the spell two objects, so the hand id names neither.
        cast_ = S.runPure (aimObjectWithX 3 victim) g5 (S.cast S.alice burstId)
        spellId = case GameState.stack cast_ of
          oid : _ -> oid
          [] -> burstId
        arm oid src gs =
          S.runPure
            (aimAndChoose victim src)
            gs
            (Activate.activateAbility S.alice oid (theAbility attendants) Monad.>> Stack.resolveTop)
        armed = arm watchingPiker decoy (arm watchingBurst spellId cast_)
        resolved = S.runPure S.identityAnswer armed Stack.resolveTop
    -- THE gameplay assertion, and the one #1681 existed for: the redirection is
    -- not applicable, so the damage stays where Lava Burst aimed it. Without the
    -- gate the whole 3 moved onto the Attendants.
    Spec.assertEqWith s "Lava Burst's 3 stays on the creature it was aimed at" (S.damageOf victim resolved) (Just 3)
    Spec.assertEqWith s "and nothing landed on the Attendants watching Lava Burst" (S.damageOf watchingBurst resolved) (Just 0)
    -- THE CONTROL, same board, differing in the SOURCE alone: the Piker is a
    -- source Lava Burst's clause does not name, so the other Attendants' row
    -- still moves its 2. A blanket suppression of redirection would fail here.
    let struck = settleDamage S.identityAnswer resolved [hit decoy (Recipient.ToCreature victim) 2]
    Spec.assertEqWith s "the Piker's 2 leaves the victim" (S.damageOf victim struck) (Just 3)
    Spec.assertEqWith s "and lands whole on the Attendants watching the Piker" (S.damageOf watchingPiker struck) (Just 2)
    -- The proxies, after the behaviour: two rows exist, and alice was asked for
    -- each and answered the two different sources.
    Spec.assertEqWith s "setup: both redirection rows watch what alice chose" (redirectSources armed) [Just decoy, Just spellId]
    Spec.assertEqWith s "setup: Lava Burst resolved out of hand" (length (GameState.stack resolved)) 0

-- CR 122.1h: a finality counter's replacement effect, gameplay-level.
--
-- Queen's Bay Paladin {3}{B}{B} Creature -- Vampire Knight 5/4, "Whenever this
-- creature enters or attacks, return up to one target Vampire card from your
-- graveyard to the battlefield with a finality counter on it. You lose life
-- equal to its mana value." (name, cost, type line, P/T and Oracle text checked
-- against api.scryfall.com 2026-08-25). Its whole text is that one trigger, so
-- nothing else on the card can be what these assertions read.
--
-- The board is built so the readings come apart:
--
--   * EXILED versus DIED. The returned Vampire is killed by CR 704.5g lethal
--     damage -- a real destruction, not a hand-built proposed event -- and both
--     zones are asserted at once, so an engine that never redirected and one
--     that lost the card entirely are different answers.
--   * EXILED versus THE GRAVEYARD EMPTIED. A Goblin Piker is buried beside the
--     Vampire and never leaves, so the graveyard half of that assertion reads
--     [Goblin Piker] rather than [] and the two are told apart.
--   * ITS mana value versus the Paladin's. The Vampire's is 3 and the Paladin's
--     is 5, so a LoseLife reading the trigger's own source rather than the
--     returned permanent would take 5.
queensBayPaladinSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
queensBayPaladinSpec s registry = Spec.describe s "Queen's Bay Paladin (CR 122.1h)" $ do
  Spec.it s "CR 122.1h a permanent with a finality counter is exiled instead of dying" $ do
    paladin <- S.printingOf s registry "Queen's Bay Paladin"
    vampirePrinting <- S.printingOf s registry "Bloodrage Vampire"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (buriedVampire, g1) = S.addGraveyardCard vampirePrinting S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addGraveyardCard pikerPrinting S.alice g1
        (paladinId, entered) = S.entersWithTrigger paladin S.alice g2
        -- Announce the one target the slot allows, then FILTER the offered set
        -- down to the buried Vampire rather than handing back a recipient built
        -- here: CR 608.2b re-reads the declared targets at resolution, and a
        -- hand-built one of a different shape would be dropped with no error.
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.AnnounceTargets _ _ _ offers -> fmap (const 1) offers
          Prompt.ChooseTargets _ _ _ offers -> fmap (Set.filter (== Recipient.ToObject buriedVampire) . snd) offers
          _ -> S.identityAnswer p
        placed = S.runPure answer entered Engine.placePendingTriggers
        resolved = S.runPure answer placed Stack.resolveTop
        -- The CR 400.7 incarnation the return minted: the battlefield permanent
        -- that is not the Paladin.
        returned = case filter (/= paladinId) (Set.toList (GameState.battlefield resolved)) of
          [only] -> Just only
          _ -> Nothing
        named = Just . CardName.MkCardName . Text.pack
        namesIn zone pid gs = List.sort (fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs))
        exiledNames gs = List.sort (fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Set.toList (GameState.exile gs)))
    case returned of
      Nothing -> Spec.assertFailure s "expected the trigger to return exactly one Vampire"
      Just vampireId ->
        let -- CR 704.5g: a 3/1 with one damage marked on it is destroyed by a
            -- state-based action, which is the destruction CR 122.1h's row
            -- replaces. Nothing here proposes the zone change directly.
            killed = S.settleSba (S.markDamage vampireId 1 resolved)
         in do
              Spec.assertEqWith
                s
                "CR 122.1h the Vampire was exiled, and the graveyard holds only the card that never left it"
                (exiledNames killed, namesIn Zone.Graveyard S.alice killed)
                ([named "Bloodrage Vampire"], [named "Goblin Piker"])
              Spec.assertEqWith s "setup: it came back carrying one finality counter" (S.counterOf CounterKind.Finality vampireId resolved) 1
              Spec.assertEqWith s "setup: alice lost life equal to ITS mana value, not the Paladin's" (S.lifeOf S.alice resolved) (Just 17)
              Spec.assertBool s (S.onBattlefield paladinId killed) "and the Paladin itself never moved"

-- CR 122.1d: a stun counter's replacement effect, gameplay-level.
--
-- Cryogenic Stasis {1}{U} Instant, "Tap target creature and put a stun counter
-- on it. / Draw a card." (name, cost, type line and Oracle text checked against
-- api.scryfall.com 2026-08-25). The reminder text in parentheses is rule 122.1d
-- itself and is not a clause of the card, which is why nothing on pawl's card
-- mentions untapping: the RULE creates the effect, and Projection.stunOf mints
-- it.
--
-- The board is built so the readings come apart:
--
--   * STILL TAPPED versus NEVER TAPPED. The Piker is untapped when the spell is
--     cast, so the tap the spell performs is what the untap step then fails to
--     undo -- an engine that tapped nothing and one that untapped it are
--     different answers.
--   * REPLACED versus PROHIBITED. The second untap step untaps it, so a stun
--     counter is told apart from Object.doesNotUntapNext and from an
--     UntapRestriction: those two leave the permanent tapped for one step and
--     for as long as they stand respectively, and neither spends anything.
--   * SPENT versus IGNORED. The counter count is read after each step, so an
--     engine that left the permanent tapped without paying a counter would fail
--     the second step's assertion rather than the first's.
--
-- A REAL untap step throughout (Engine.runTurnBasedActions at CR 502.3), not a
-- direct call to the funnel.
cryogenicStasisSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cryogenicStasisSpec s registry = Spec.describe s "Cryogenic Stasis (CR 122.1d)" $ do
  -- alice is the active player, so CR 502.3's turn-based action is asked about
  -- the permanents she controls.
  let untapStep gs = S.runPure S.identityAnswer (gs {GameState.activePlayer = S.alice}) (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
  Spec.it s "CR 122.1d a stun counter replaces the untap step's untap and is spent doing it" $ do
    island <- S.printingOf s registry "Island"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    stasis <- S.printingOf s registry "Cryogenic Stasis"
    let (piker, g1) = S.addPermanent pikerPrinting S.alice (S.landsInPlay island 2)
        -- CR 104.3c: the spell draws, so alice needs a library to draw from.
        (_, g2) = S.addLibraryCard pikerPrinting S.alice g1
        (g3, stasisId) = S.handOne stasis g2
        cast = S.runPure S.identityAnswer g3 (S.cast S.alice stasisId)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
        first = untapStep resolved
        second = untapStep first
    -- The behaviour, ahead of every proxy: the untap step ran and left the Piker
    -- exactly as tapped as it found it.
    Spec.assertEqWith s "CR 122.1d the stunned Piker is still tapped after its controller's untap step" (tapStateOf piker first) (Just TapState.Tapped)
    Spec.assertEqWith s "CR 122.1d and that untap spent the stun counter" (countersOn CounterKind.Stun piker first) 0
    -- The row is SPENT, not permanent: with no counter left, the next untap step
    -- untaps it.
    Spec.assertEqWith s "CR 122.1d with the counter gone the next untap step untaps it" (tapStateOf piker second) (Just TapState.Untapped)
    Spec.assertEqWith s "setup: the spell tapped the Piker and left one stun counter on it" (tapStateOf piker resolved, countersOn CounterKind.Stun piker resolved) (Just TapState.Tapped, 1)
    Spec.assertEqWith s "setup: and drew alice her card, so the whole spell resolved" (S.handSize S.alice resolved) 1
  -- Rule 701.26b's second sentence, which is `proposeUntap`'s guard: an UNTAPPED
  -- permanent does not become untapped, so CR 122.1d's event is never proposed
  -- for it and no counter is spent. The board differs from the case above in
  -- exactly one thing -- the permanent's tap state -- so the pair is what tells
  -- "the counter is spent by an untap step" from "the counter is spent by an
  -- untap".
  --
  -- The counter is placed by the fixture rather than by Cryogenic Stasis: the
  -- card TAPS what it stuns, so no board it can build reaches this arm.
  Spec.it s "CR 701.26b an untapped permanent never becomes untapped, so its stun counter is not spent" $ do
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (piker, placed) = S.addPermanent pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
        stunned = S.addCounter CounterKind.Stun 1 piker placed
        after = untapStep stunned
    Spec.assertEqWith s "CR 701.26b the untap step spent no stun counter on a permanent that was already upright" (countersOn CounterKind.Stun piker after) 1
    Spec.assertEqWith s "setup: it went into the step untapped" (tapStateOf piker stunned) (Just TapState.Untapped)
    Spec.assertEqWith s "and came out of it untapped" (tapStateOf piker after) (Just TapState.Untapped)

-- The names of the cards in a player's graveyard, sorted.
graveyardNames :: PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
graveyardNames pid gs = List.sort (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers Zone.Graveyard pid gs))

-- The tap state of a permanent, which is what CR 502.3's untap step writes -- and
-- so what a skipped untap step leaves alone.
tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid = fmap Object.tapped . Game.lookupObject oid

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  divineDeflectionSpec s registry
  braceForImpactSpec s registry
  inkshieldSpec s registry
  stormwildCapridorSpec s registry
  templeAltisaurSpec s registry
  ajaniSteadfastSpec s registry
  proteanHydraSpec s registry
  jaredCarthalionSpec s registry
  glitteringLionSpec s registry
  spiderPunkSpec s registry
  phantomTigerSpec s registry
  apnapSpec s registry
  excruciatorSpec s registry
  questingBeastSpec s registry
  luminesceSpec s registry
  moonmistSpec s registry
  selflessSquireSpec s registry
  phyrexianVindicatorSpec s registry
  samiteMinistrationSpec s registry
  turnTheTablesSpec s registry
  oraclesAttendantsSpec s registry
  caromSpec s registry
  harmsWaySpec s registry
  pariahSpec s registry
  lavaBurstSpec s registry
  queensBayPaladinSpec s registry
  cryogenicStasisSpec s registry
