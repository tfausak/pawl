{-# LANGUAGE GADTs #-}

-- Covers: Pawl.Engine.Projection (CR 613 layer 7 -- 7a characteristic-defined P/T, the
-- CR 608.2h freeze that 7b's stored effects owe, 7c modification and 7d P/T
-- switching), Pawl.Engine.Quantity and Pawl.Engine.ManaCount (the counting
-- quantities, and CR 208.2a's substitution of 0 for a number a CDA cannot
-- determine) and the P3b/M5.5 gates (Tarmogoyf, Inner Calm Outer Strength,
-- Twisted Image, Nightmare, Monstrous War-Leech, Omnath Locus of Mana, Serra
-- Avatar), plus CR 604.2's "as long as" gate on a printed static ability (Kird
-- Ape) and CR 613.4c's layer 7c anthem narrowed by a keyword its affected
-- objects have (Hand of the Praetors).
-- Gameplay-level: each card is cast or resolved through the stack and the
-- resulting game state is asserted on.
module Pawl.PowerToughnessSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

-- The battlefield objects whose printed card is Monstrous War-Leech. Found by
-- name rather than tracked by id: CR 400.7 makes an object that changes zones a
-- new object, and pawl gives each one a fresh ObjectId, so the id the cast was
-- handed names nothing on the battlefield (the Pawl.CopySpec precedent).
leechesOnBattlefield :: GameState.GameState -> [ObjectId.ObjectId]
leechesOnBattlefield gs = filter isLeech (Set.toList (GameState.battlefield gs))
  where
    isLeech oid = maybe False (\f -> Face.name f == CardName.MkCardName (Text.pack "Monstrous War-Leech")) (Game.faceOf oid gs)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.PowerToughness" $ do
  Spec.it s "CR 604.3 the seed carries the CDA as QUANTITIES, with the printed star substituted" $ do
    -- CR 707.2a: a copy acquires the ABILITY, so what the seed (and therefore
    -- the copiable value) holds must be unevaluated. Tarmogoyf's printed box is
    -- \*/1+*, so the pair is <count> and 1+<count>.
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    let gs0 = Setup.emptyGame S.bothPlayers
        (goyfId, gs) = S.addCreature tarmogoyf S.alice gs0
        -- CR 208.2a: Tarmogoyf's shape -- distinct card types over every
        -- graveyard.
        count =
          Quantity.Type.Count
            ( Count.Type.MkCount
                (Scope.InZone Zone.Graveyard PlayerRef.EachPlayer)
                (Filter.Type.And [])
                Aggregation.DistinctCardTypes
            )
    Spec.assertEqWith
      s
      "the CDA pair"
      (PC.characteristicPT (Projection.baseCharacteristics goyfId gs))
      (Just (count, Quantity.Type.Plus (Quantity.Type.Literal 1) count))
  Spec.it s "CR 613.4a no P/T value exists before layer 7a applies one" $ do
    -- The seed evaluates the printed Star, which is deliberately Nothing.
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    let gs0 = Setup.emptyGame S.bothPlayers
        (goyfId, gs) = S.addCreature tarmogoyf S.alice gs0
        seeded = Projection.baseCharacteristics goyfId gs
    Spec.assertEqWith s "no seeded power" (PC.power seeded) Nothing
    Spec.assertEqWith s "no seeded toughness" (PC.toughness seeded) Nothing
  Spec.it s "an ordinary card has no CDA" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, gs) = S.addCreature piker S.alice gs0
    Spec.assertEqWith s "none" (PC.characteristicPT (Projection.baseCharacteristics pikerId gs)) Nothing
  Spec.it s "CR 613.4a Tarmogoyf's P/T is recomputed, not fixed at entry" $ do
    -- THE FALSIFIER for evaluating a printed * once, at the seed or at entry:
    -- nothing touches the Goyf, and its P/T moves because a graveyard did.
    -- Empty graveyards -> 0 card types -> 0/1. Fog resolves and is put into
    -- its owner's graveyard (CR 608.2n), adding the Instant type.
    --
    -- Fog, NOT Lightning Bolt: Bolt targets, S.identityAnswer would aim it at
    -- the only creature on the board, and 3 damage would kill the 0/1 Goyf
    -- being measured. Fog has no target and no effect outside combat.
    forest <- S.printingOf s registry "Forest"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    fog <- S.printingOf s registry "Fog"
    let base = S.landsInPlay forest 1
        (goyfId, board) = S.addCreature tarmogoyf S.alice base
        (gs, fogId) = S.handOne fog board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice fogId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "before: no card types in any graveyard, so 0 power" (Projection.powerOf goyfId board) (Just 0)
    Spec.assertEqWith s "before: 0+1 toughness" (Projection.toughnessOf goyfId board) (Just 1)
    Spec.assertEqWith s "after: one card type (Instant), so 1 power" (Projection.powerOf goyfId after) (Just 1)
    Spec.assertEqWith s "after: 1+1 toughness" (Projection.toughnessOf goyfId after) (Just 2)
  Spec.it s "CR 208.2a 2007-10-01 the CDA works in all zones, and a Goyf in a graveyard counts itself" $ do
    -- Gatherer ruling on Tarmogoyf (WotC, 2007-10-01): "The ability that
    -- defines Tarmogoyf's power and toughness works in all zones, not just
    -- the battlefield. If Tarmogoyf is in your graveyard, it will count
    -- itself." CR 604.3 says a CDA functions in all zones, and CR 208.2a
    -- repeats it for P/T. This is the assertion that a gather-based
    -- implementation cannot make: gather only walks the battlefield.
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    let gs0 = Setup.emptyGame S.bothPlayers
        (goyfId, gs) = S.addGraveyardCard tarmogoyf S.alice gs0
    Spec.assertEqWith s "the Goyf in the graveyard is a creature card, so power 1" (Projection.powerOf goyfId gs) (Just 1)
    Spec.assertEqWith s "1+1 toughness" (Projection.toughnessOf goyfId gs) (Just 2)
  Spec.it s "CR 613.4a/613.4c layer 7a runs before 7c, so a counter adds to the CDA" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withCard) = S.addGraveyardCard lightningBolt S.alice gs0
        (goyfId, board) = S.addCreature tarmogoyf S.alice withCard
        gs = S.addCounter CounterKind.PlusOnePlusOne 1 goyfId board
    Spec.assertEqWith s "1 card type + 1 counter" (Projection.powerOf goyfId gs) (Just 2)
    Spec.assertEqWith s "1+1 toughness + 1 counter" (Projection.toughnessOf goyfId gs) (Just 3)
  Spec.it s "CR 604.3 Humility removes the CDA, and a Humility'd Tarmogoyf is 1/1" $ do
    -- NON-DISTINGUISHING BY CONSTRUCTION, and deliberately kept anyway.
    -- Humility is layer 6 (LoseAllAbilities) AND layer 7b (base P/T 1/1), and
    -- 7b overwrites 7a either way -- so this test passes whether or not
    -- LoseAllAbilities clears characteristicPT. It is here because "a
    -- Humility'd Tarmogoyf is 1/1" is a real ruling worth pinning, not because
    -- it proves the clearing.
    --
    -- What WOULD distinguish: a "loses all abilities" card that does not also
    -- set P/T. Attachment itself has landed (Pawl.Types.Object.attachedTo,
    -- Affected.Attached), but the Darksteel Mutation family still needs
    -- layer-4 card-type REPLACEMENT (turning the enchanted creature into a
    -- 0/0), which does not exist yet; Soul Sculptor needs the same
    -- layer-4 REPLACEMENT; Dress Down needs Flash, a beginning-of-end-step
    -- trigger (P4) and Sacrifice. See the P3b spec, section 8.
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    humility <- S.printingOf s registry "Humility"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
        (goyfId, board) = S.addCreature tarmogoyf S.alice withBolt
        gs = S.withHumility humility board
    Spec.assertEqWith s "1 power" (Projection.powerOf goyfId gs) (Just 1)
    Spec.assertEqWith s "1 toughness" (Projection.toughnessOf goyfId gs) (Just 1)
  Spec.it s "CR 604.3 LoseAllAbilities clears the CDA from the projected characteristics" $ do
    -- The clearing itself, asserted directly on the projection rather than
    -- through P/T -- the only channel through which it IS observable today.
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    humility <- S.printingOf s registry "Humility"
    let gs0 = Setup.emptyGame S.bothPlayers
        (goyfId, board) = S.addCreature tarmogoyf S.alice gs0
        gs = S.withHumility humility board
    Spec.assertEqWith s "no CDA survives layer 6" (PC.characteristicPT (Projection.project goyfId gs)) Nothing
  Spec.it s "CR 608.2h a resolved pump is FROZEN and does not shrink with the hand" $ do
    -- THE FALSIFIER for re-evaluating a stored quantity: CR 608.2h says the
    -- answer is determined only once, when the effect is applied. Alice
    -- resolves the pump with two cards left in hand (+2/+2), then casts one of
    -- them -- her hand is now one card, and the pump must NOT follow it down.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    innerCalm <- S.printingOf s registry "Inner Calm, Outer Strength"
    giantGrowth <- S.printingOf s registry "Giant Growth"
    let base = S.landsInPlay forest 4
        (pikerId, board) = S.addCreature piker S.alice base
        -- handOne FIRST (it replaces the hand and sets up the phase), then
        -- addHandCard for the extras.
        (h1, icId) = S.handOne innerCalm board
        (ggId, h2) = S.addHandCard giantGrowth S.alice h1
        (_, gs) = S.addHandCard forest S.alice h2
        -- Casting Inner Calm moves it from hand to the stack, leaving two.
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice icId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- Now the hand shrinks. Giant Growth is only CAST, not resolved, so it
        -- contributes no pump of its own -- the only thing that changed is the
        -- number Inner Calm counted.
        shrunk = snd (Engine.runGamePure S.identityAnswer after (S.cast S.alice ggId))
    Spec.assertEqWith s "two cards left in hand at resolution" (S.handSize S.alice after) 2
    Spec.assertEqWith s "the 2/1 Piker is pumped to 4" (Projection.powerOf pikerId after) (Just 4)
    Spec.assertEqWith s "and to 3 toughness" (Projection.toughnessOf pikerId after) (Just 3)
    Spec.assertEqWith s "the hand is down to one card" (S.handSize S.alice shrunk) 1
    Spec.assertEqWith s "THE FREEZE: still +2, not +1" (Projection.powerOf pikerId shrunk) (Just 4)
    Spec.assertEqWith s "and still +2 toughness" (Projection.toughnessOf pikerId shrunk) (Just 3)
  Spec.it s "CR 611.2 the freeze does NOT reach a static ability's continuous effect" $ do
    -- Opalescence's SetBasePowerToughness carries ManaValue, and CR 611.2 scopes
    -- the freeze to effects created by a spell's RESOLUTION. A static ability's
    -- effect is regenerated from the permanent every projection and evaluated
    -- per AFFECTED object. Were ManaValue frozen against the wrong object, this
    -- would evaluate against Opalescence itself (mana value 4, from {2}{W}{W}),
    -- not Bad Moon's own (mana value 2, from {1}{B}).
    --
    -- The expected 3/3, not the naively-expected 2/2: Opalescence's layer 7b
    -- sets Bad Moon's base to 2/2 (its own mana value), but layer 4 also makes
    -- Bad Moon itself a black creature -- and Bad Moon's own oracle text,
    -- "Black creatures get +1/+1", carries no "other" exclusion (unlike a lord
    -- effect), so its layer 7c ModifyPowerToughness applies to ITSELF too.
    -- Verified directly (not from recall): docs/rules.txt has no CR 613 clause
    -- excluding a source from its own static ability. 3/3 is still nowhere
    -- near the 5/5 a ManaValue-against-Opalescence leak would produce (4/4
    -- base + Bad Moon's own +1/+1), so the falsifier still distinguishes.
    opalescence <- S.printingOf s registry "Opalescence"
    badMoon <- S.printingOf s registry "Bad Moon"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withOpal) = S.addCreature opalescence S.alice gs0
        (moonId, gs) = S.addCreature badMoon S.alice withOpal
    Spec.assertEqWith s "Bad Moon's own mana value (2) plus its own +1/+1, not Opalescence's mana value (4)" (Projection.powerOf moonId gs) (Just 3)
    Spec.assertEqWith s "and its toughness is 3" (Projection.toughnessOf moonId gs) (Just 3)
  Spec.it s "CR 608.2h the count is the CASTER's hand, not the target's controller's" $ do
    -- The second half of the same bug: applyModification used to evaluate a
    -- stored quantity against the AFFECTED object, so a player-scoped count
    -- would read the wrong player. Alice holds two cards after casting; bob
    -- holds none, and it is bob's creature being pumped.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    innerCalm <- S.printingOf s registry "Inner Calm, Outer Strength"
    giantGrowth <- S.printingOf s registry "Giant Growth"
    let base = S.landsInPlay forest 4
        (bobsPiker, board) = S.addCreature piker S.bob base
        (h1, icId) = S.handOne innerCalm board
        (_, h2) = S.addHandCard giantGrowth S.alice h1
        (_, gs) = S.addHandCard forest S.alice h2
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice icId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "bob holds nothing" (S.handSize S.bob after) 0
    Spec.assertEqWith s "the pump is alice's two, not bob's zero" (Projection.powerOf bobsPiker after) (Just 4)
  Spec.it s "CR 613.4d the switch takes the value AFTER layers 7a-7c" $ do
    -- THE ORDERING FALSIFIER, and the reason Tarmogoyf and Twisted Image are in
    -- the same phase: a symmetric fixture (a +1/+1 counter, Giant Growth)
    -- COMMUTES with the switch and proves nothing. Tarmogoyf's N/N+1 is the
    -- pool's only asymmetric P/T, so it is the only thing that can witness the
    -- order. Two card types in graveyards -> a 2/3 -> switched, a 3/2.
    -- Switching before 7a would switch Nothing/Nothing and the CDA would then
    -- write 2/3 straight back over it.
    island <- S.printingOf s registry "Island"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    piker <- S.printingOf s registry "Goblin Piker"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    twistedImage <- S.printingOf s registry "Twisted Image"
    let base = S.landsInPlay island 1
        (_, g1) = S.addGraveyardCard lightningBolt S.alice base
        (_, g2) = S.addGraveyardCard piker S.alice g1
        (goyfId, board) = S.addCreature tarmogoyf S.alice g2
        (gs, tiId) = S.handOne twistedImage board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice tiId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "before: a 2/3" (Projection.powerOf goyfId board) (Just 2)
    Spec.assertEqWith s "before: toughness 3" (Projection.toughnessOf goyfId board) (Just 3)
    Spec.assertEqWith s "after: power is the old toughness" (Projection.powerOf goyfId after) (Just 3)
    Spec.assertEqWith s "after: toughness is the old power" (Projection.toughnessOf goyfId after) (Just 2)
  Spec.it s "CR 613.4d a switched CDA still tracks the graveyards" $ do
    -- After the switch, a THIRD distinct card type lands in a graveyard, and
    -- the Goyf's CDA (still recomputed live every projection, CR 604.3) must
    -- pick it up underneath the still-active switch. Divination (Sorcery), not
    -- Giant Growth: Giant Growth is an Instant, the same type Lightning Bolt
    -- already contributed, so it would add no NEW type and this fixture would
    -- silently test nothing beyond the previous case.
    island <- S.printingOf s registry "Island"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    piker <- S.printingOf s registry "Goblin Piker"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    twistedImage <- S.printingOf s registry "Twisted Image"
    divination <- S.printingOf s registry "Divination"
    let base = S.landsInPlay island 1
        (_, g1) = S.addGraveyardCard lightningBolt S.alice base
        (_, g2) = S.addGraveyardCard piker S.alice g1
        (goyfId, board) = S.addCreature tarmogoyf S.alice g2
        (gs, tiId) = S.handOne twistedImage board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice tiId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        (_, later) = S.addGraveyardCard divination S.bob after
    Spec.assertEqWith s "a third card type: 3/4 switched is 4/3, power" (Projection.powerOf goyfId later) (Just 4)
    Spec.assertEqWith s "and toughness" (Projection.toughnessOf goyfId later) (Just 3)
  Spec.it s "CR 613.4d 2021-03-19 the switch applies last regardless of WHEN it began" $ do
    -- Gatherer ruling on Twisted Image (WotC, 2021-03-19): "Effects that switch
    -- a creature's power and toughness apply after all other effects,
    -- REGARDLESS OF WHEN THOSE EFFECTS BEGAN TO APPLY. For instance, if you
    -- target a 1/2 creature then give it +2/+0 later in the turn, it's a 2/3
    -- creature, not a 4/1 creature."
    --
    -- The switch is installed FIRST (earlier timestamp) and the pump SECOND, so
    -- a timestamp-ordered implementation would switch then pump. Layer order
    -- (CR 613.4c before 613.4d) must beat timestamp order. The pump is +2/+0 --
    -- ASYMMETRIC, per the ruling's own example, because a symmetric one cannot
    -- tell the two orders apart.
    --
    -- Goblin Piker is 2/1. Correct: 7c gives 4/1, 7d switches to 1/4.
    -- Timestamp-ordered: switch gives 1/2, then the pump gives 3/2.
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board) = S.addCreature piker S.alice gs0
        switched = S.withEffect pikerId Modification.SwitchPowerToughness board
        gs = S.withEffect pikerId (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 0)) switched
    Spec.assertEqWith s "power is the pumped toughness" (Projection.powerOf pikerId gs) (Just 1)
    Spec.assertEqWith s "toughness is the pumped power" (Projection.toughnessOf pikerId gs) (Just 4)
  Spec.it s "CR 613.4d 2021-03-19 two switches return the object to normal" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board) = S.addCreature piker S.alice gs0
        once = S.withEffect pikerId Modification.SwitchPowerToughness board
        twice = S.withEffect pikerId Modification.SwitchPowerToughness once
    Spec.assertEqWith s "once: the 2/1 is a 1/2" (Projection.powerOf pikerId once) (Just 1)
    Spec.assertEqWith s "twice: back to 2" (Projection.powerOf pikerId twice) (Just 2)
    Spec.assertEqWith s "twice: back to 1 toughness" (Projection.toughnessOf pikerId twice) (Just 1)
  Spec.it s "CR 704.5g 2021-03-19 nonlethal damage becomes lethal after a switch" $ do
    -- Gatherer ruling on Twisted Image (WotC, 2021-03-19): "Because damage
    -- remains marked on a creature until the damage is removed as the turn
    -- ends, nonlethal damage dealt to a creature may become lethal if you
    -- switch its power and toughness during that turn." Damage marking
    -- (CR 514.2) and the CR 704.5g lethal-damage state-based action have both
    -- existed since M1b; this is the ruling as a scenario.
    --
    -- A 2/3 Tarmogoyf with 2 damage marked survives. Switched to 3/2, the same
    -- 2 damage is lethal.
    island <- S.printingOf s registry "Island"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    piker <- S.printingOf s registry "Goblin Piker"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    forest <- S.printingOf s registry "Forest"
    twistedImage <- S.printingOf s registry "Twisted Image"
    let base = S.landsInPlay island 1
        (_, g1) = S.addGraveyardCard lightningBolt S.alice base
        (_, g2) = S.addGraveyardCard piker S.alice g1
        (goyfId, g3) = S.addCreature tarmogoyf S.alice g2
        -- Twisted Image draws a card, and this is the one test here that runs
        -- settleForPriority (it needs the SBA sweep). Setup.emptyGame leaves
        -- libraries EMPTY, so without this alice would lose to CR 704.5b
        -- mid-assertion rather than the Goyf dying to CR 704.5g.
        (_, g4) = S.addLibraryCard forest S.alice g3
        board = S.markDamage goyfId 2 g4
        (gs, tiId) = S.handOne twistedImage board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice tiId))
        after = snd (Engine.runGamePure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority))
    Spec.assertBool s (Set.member goyfId (GameState.battlefield board)) "the 2/3 with 2 damage was alive"
    Spec.assertBool s (not (Set.member goyfId (GameState.battlefield after))) "the switched 3/2 with 2 damage is dead"
  Spec.it s "CR 208.2a Nightmare counts the Swamps you control" $ do
    swamp <- S.printingOf s registry "Swamp"
    nightmare <- S.printingOf s registry "Nightmare"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, gs1) = S.addCreature swamp S.alice gs0
        (_, gs2) = S.addCreature swamp S.alice gs1
        (_, gs3) = S.addCreature swamp S.bob gs2
        (nightId, gs) = S.addCreature nightmare S.alice gs3
    Spec.assertEqWith s "power" (Projection.powerOf nightId gs) (Just 2)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf nightId gs) (Just 2)
  Spec.it s "CR 613.1d Urborg makes every land a Swamp, and Nightmare counts them" $ do
    -- THE FALSIFIER for a printed-type count. Nothing touches the Nightmare;
    -- its power moves because a layer-4 type-changer entered.
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    nightmare <- S.printingOf s registry "Nightmare"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, gs1) = S.addCreature mountain S.alice gs0
        (_, gs2) = S.addCreature forest S.alice gs1
        (nightId, gs3) = S.addCreature nightmare S.alice gs2
        (_, gs) = S.addCreature urborg S.alice gs3
    Spec.assertEqWith s "both lands are Swamps, plus Urborg itself (IncludesSource): 3" (Projection.powerOf nightId gs) (Just 3)
  Spec.it s "CR 613.5 a Swamp entering or leaving moves the P/T on the next projection" $ do
    -- CR 613.5: continuous application is automatic -- nothing touches
    -- Nightmare when a Swamp enters or leaves, only the next projection's
    -- read of the battlefield. The Swamp leaves through Event.changeZone --
    -- the same zone-change machinery a real destroy, sacrifice, or bounce
    -- runs, which mints the departing permanent a fresh object id in its
    -- new zone (CR 400.7) -- rather than by deleting it out of
    -- GameState.objects directly, a state the engine itself never
    -- produces.
    nightmare <- S.printingOf s registry "Nightmare"
    swamp <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (nightId, g1) = S.addCreature nightmare S.alice gs0
        (_, g2) = S.addCreature swamp S.alice g1
        (swamp2, g3) = S.addCreature swamp S.alice g2
        g4 = snd (Engine.runGamePure S.identityAnswer g3 (Event.changeZone swamp2 Zone.Graveyard))
    Spec.assertEqWith s "one Swamp: power 1" (Projection.powerOf nightId g2) (Just 1)
    Spec.assertEqWith s "a second Swamp enters: power 2" (Projection.powerOf nightId g3) (Just 2)
    Spec.assertEqWith s "it leaves again: power back to 1" (Projection.powerOf nightId g4) (Just 1)
  Spec.it s "CR 613.1d/305.7 Blood Moon strips Urborg's own Swamp-granting ability" $ do
    -- Urborg enters first (earlier timestamp) and swamps itself; Blood Moon
    -- enters second and SETS its subtype to Mountain. CR 305.7: a land whose
    -- subtype is SET to a basic type loses every ability generated by its
    -- rules text -- including Urborg's OWN "every land is a Swamp" ability.
    -- Pawl.Engine.Projection.staticAbilitiesLive/liveGiven excludes that ability
    -- from `gather` entirely (reading BASE characteristics, not a timestamp
    -- race), so Urborg ends up a bare Mountain with nothing left for
    -- Nightmare to count.
    --
    -- THE POSITIVE CONTROL: a basic Swamp sits on the board alongside
    -- Urborg. Blood Moon's own filter is land-and-NOT-basic, so the basic
    -- Swamp is untouched by it and stays a Swamp. A count that never really
    -- matches HasSubtype, or a land-subtype effect that (wrongly)
    -- suppresses every permanent's static abilities, would return 0 here
    -- exactly as a correct implementation returns for Urborg alone -- only
    -- a count that genuinely sees the basic Swamp reaches 1.
    nightmare <- S.printingOf s registry "Nightmare"
    swamp <- S.printingOf s registry "Swamp"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let gs0 = Setup.emptyGame S.bothPlayers
        (nightId, g1) = S.addCreature nightmare S.alice gs0
        (_, g2) = S.addCreature swamp S.alice g1
        (_, g3) = S.addCreature urborg S.alice g2
        (_, gs) = S.addCreature bloodMoon S.alice g3
    Spec.assertEqWith s "Urborg is a bare Mountain now; only the basic Swamp remains" (Projection.powerOf nightId gs) (Just 1)
  Spec.it s "CR 305.7 the outcome does not depend on entry order" $ do
    -- THE FALSIFIER for a naive per-layer TIMESTAMP-ONLY implementation: the
    -- previous test's order reversed -- Blood Moon enters FIRST, Urborg
    -- SECOND. A plain CR 613.7 timestamp fold would apply Urborg's LATER
    -- AddLandSubtype Swamp after Blood Moon's SetLandSubtype Mountain and
    -- get a Swamp back (Mountain AND Swamp). Pawl instead gates Urborg's own
    -- ability on CR 305.7 liveness read against BASE characteristics
    -- (Pawl.Engine.Projection.staticAbilitiesLive), which does not depend on
    -- timestamps at all, so the result matches the previous test regardless
    -- of order.
    --
    -- This pair is NOT actually a CR 613.7 timestamp race: it is CR 613.8
    -- dependency, and the dependency runs only one way, so order was never
    -- going to matter here regardless of how Pawl is built. By CR 613.8a,
    -- Urborg's AddLandSubtype effect DEPENDS ON Blood Moon's SetLandSubtype
    -- effect -- applying Blood Moon changes the EXISTENCE of Urborg's
    -- effect, because CR 305.7 strips every ability generated by a land's
    -- own rules text (Urborg's Swamp-granting ability included) the moment
    -- that land's subtype is SET to a basic type. Blood Moon's effect does
    -- NOT depend on Urborg's: Blood Moon's filter excludes "basic" lands,
    -- and "basic" is a supertype (CR 205.4a), which an AddLandSubtype
    -- effect cannot add or remove (CR 305.7's own text: "doesn't add or
    -- remove any card types ... or supertypes"), so nothing Urborg does can
    -- change what Blood Moon's filter matches. Per CR 613.8b, a dependent
    -- effect waits to apply until just after the effect(s) it depends on
    -- have applied -- so Blood Moon applies first regardless of timestamps,
    -- and Urborg's ability is already dead by the time it would otherwise
    -- run. Note which mechanism carries this pair: the EXISTENCE half of CR
    -- 613.8a clause (b), which lives in the dedicated CR 305.7 liveness gate,
    -- not the applies-to reorder in projectWith. The reorder would not save
    -- it -- by the time it ran, Urborg's ability would have to still exist to
    -- be ordered at all.
    --
    -- THE POSITIVE CONTROL, as in the previous test: a basic Swamp sits
    -- alongside the other two permanents. It is immune to Blood Moon (which
    -- excludes basic lands), so it stays a Swamp and keeps the count off a
    -- vacuous 0.
    nightmare <- S.printingOf s registry "Nightmare"
    swamp <- S.printingOf s registry "Swamp"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    let gs0 = Setup.emptyGame S.bothPlayers
        (nightId, g1) = S.addCreature nightmare S.alice gs0
        (_, g2) = S.addCreature swamp S.alice g1
        (_, g3) = S.addCreature bloodMoon S.alice g2
        (_, gs) = S.addCreature urborg S.alice g3
    Spec.assertEqWith s "still 1: order does not matter" (Projection.powerOf nightId gs) (Just 1)
  Spec.it s "CR 604.3 Humility erases the CDA, and a Humility'd Nightmare is 1/1" $ do
    -- NON-DISTINGUISHING BY CONSTRUCTION, same shape as the Tarmogoyf
    -- Humility test above. Humility is layer 6 (LoseAllAbilities) AND
    -- layer 7b (SetBasePowerToughness 1/1), and 7b would overwrite
    -- whatever the CDA leaves behind regardless of whether 6 actually ran
    -- first -- so the power/toughness assertions below cannot show that
    -- layer 6 ran BEFORE layer 7a; only the CDA-clearing assertion can.
    -- And even that one proves the clearing happened, not an ordering:
    -- `characteristicPT` is populated once from the seed and is untouched
    -- by SetBasePowerToughness (which writes only PC.power/PC.toughness),
    -- so `Nothing` here means LoseAllAbilities actually cleared it, not
    -- that some later step merely hid a CDA that was still there. With
    -- layer 7b setting a P/T either way, no ordering between "cleared" and
    -- "never ran" is directly observable from this fixture -- only the
    -- clearing itself is, and that is all this test claims.
    swamp <- S.printingOf s registry "Swamp"
    nightmare <- S.printingOf s registry "Nightmare"
    humility <- S.printingOf s registry "Humility"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addCreature swamp S.alice gs0
        (_, g2) = S.addCreature swamp S.alice g1
        (nightId, g3) = S.addCreature nightmare S.alice g2
        gs = S.withHumility humility g3
    Spec.assertEqWith s "no CDA survives layer 6" (PC.characteristicPT (Projection.project nightId gs)) Nothing
    Spec.assertEqWith s "1 power" (Projection.powerOf nightId gs) (Just 1)
    Spec.assertEqWith s "1 toughness" (Projection.toughnessOf nightId gs) (Just 1)
  Spec.it s "CR 303.4m: an attached Unholy Strength gives the enchanted creature +2/+1" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    let base = Setup.emptyGame S.bothPlayers
        -- Goblin Piker is a 2/1.
        (creature, withCreature) = S.addCreature piker S.bob base
        (aura, withAura) = S.addCreature unholyStrength S.alice withCreature
        attached = S.attach aura creature withAura
    Spec.assertEqWith s "unattached, the ability names nothing" (Projection.powerOf creature withAura, Projection.toughnessOf creature withAura) (Just 2, Just 1)
    Spec.assertEqWith s "attached, +2/+1" (Projection.powerOf creature attached, Projection.toughnessOf creature attached) (Just 4, Just 2)
  -- CR 208.2a's last sentence: "If the ability needs to use a number that can't
  -- be determined, including inside a calculation, use 0 instead of that
  -- number." Monstrous War-Leech's power and toughness are each the greatest
  -- mana value among cards in YOUR graveyard, and an empty graveyard has no
  -- greatest -- Pawl.Engine.Count.aggregate says so, honestly, with Nothing.
  -- The number cannot be determined, so the CDA uses 0.
  --
  -- Monstrous War-Leech's other two sentences -- its kicker, and the as-enters
  -- mill that kicking turns on -- are not modelled (#610), so the graveyard
  -- these fixtures read is only ever the one they set up.
  Spec.it s "CR 208.2a an undeterminable CDA number is 0: an empty graveyard makes the Leech a 0/0" $ do
    leech <- S.printingOf s registry "Monstrous War-Leech"
    let gs0 = Setup.emptyGame S.bothPlayers
        (leechId, gs) = S.addCreature leech S.alice gs0
    Spec.assertEqWith s "0 power, not absent" (Projection.powerOf leechId gs) (Just 0)
    Spec.assertEqWith s "0 toughness, not absent" (Projection.toughnessOf leechId gs) (Just 0)
  Spec.it s "CR 208.2a only the undeterminable number is replaced: one Lightning Bolt in the graveyard makes it 1/1" $ do
    -- The other side of the same fixture, so the 0/0 above is the RULE and not
    -- a CDA that never computes anything: Lightning Bolt's mana value is 1
    -- (CR 202.3), so the greatest among a one-card graveyard is 1.
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    leech <- S.printingOf s registry "Monstrous War-Leech"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
        (leechId, gs) = S.addCreature leech S.alice withBolt
    Spec.assertEqWith s "1 power" (Projection.powerOf leechId gs) (Just 1)
    Spec.assertEqWith s "1 toughness" (Projection.toughnessOf leechId gs) (Just 1)
  Spec.it s "CR 704.5f the 0/0 Leech dies: cast with an empty graveyard, it never survives entry" $ do
    -- THE PROVING CASE, at gameplay level: alice casts Monstrous War-Leech off
    -- four Swamps with nothing in her graveyard. It resolves, enters as a 0/0
    -- (CR 208.2a), and the settle boundary buries it (CR 704.5f, "if a creature
    -- has toughness 0 or less, it's put into its owner's graveyard"). Without
    -- CR 208.2a's substitution the CDA determines nothing, the Leech has NO
    -- toughness at all, Pawl.Engine.Sba.zeroToughness reads Nothing and the
    -- creature survives -- the board difference this test exists to falsify.
    swamp <- S.printingOf s registry "Swamp"
    leech <- S.printingOf s registry "Monstrous War-Leech"
    let base = S.landsInPlay swamp 4
        (gs, leechId) = S.handOne leech base
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice leechId))
        settled = snd (Engine.runGamePure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority))
    Spec.assertEqWith s "the spell left the stack" (GameState.stack settled) []
    Spec.assertEqWith s "no Leech on the battlefield" (leechesOnBattlefield settled) []
  omnathSpec s registry
  serraAvatarSpec s registry
  kirdApeSpec s registry
  woodElementalSpec s registry
  handOfThePraetorsSpec s registry

-- CR 613.4c layer 7c, narrowed by a KEYWORD the affected object has: Hand of the
-- Praetors, {3}{B} Creature -- Phyrexian Zombie 3/2, "Other creatures you
-- control with infect get +1/+1."
--
-- Three narrowings in one printed line -- "other" (the ability's own source,
-- excluded by Filter.Not Filter.IsSource), "you control" (CR 109.5's "you",
-- which for a static ability is the current controller of the object it is on),
-- and "with infect" (CR 702.90) -- and each case below moves exactly one, so an
-- Affected filter that admitted every creature is distinguishable from one that
-- reads all three.
--
-- Every printed box on the board is a different pair -- the Hand's 3/2, Glistener
-- Elf's 1/1, Goblin Piker's 2/1 -- so no assertion's expected value is reachable
-- by pumping the wrong creature.
--
-- The cast trigger on the same card is Pawl.TriggerSpec's.
handOfThePraetorsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
handOfThePraetorsSpec s registry = Spec.describe s "Hand of the Praetors" $ do
  -- THE case, and the "other" half beside it: the Elf grows and the Hand, which
  -- also has infect and is also a creature alice controls, does not grow itself.
  Spec.it s "CR 613.4c an infect creature you control gets +1/+1, and the Hand does not pump itself" $ do
    hand <- S.printingOf s registry "Hand of the Praetors"
    elf <- S.printingOf s registry "Glistener Elf"
    let (elfId, alone) = S.addCreature elf S.alice (Setup.emptyGame S.bothPlayers)
        (handId, gs) = S.addCreature hand S.alice alone
    Spec.assertEqWith s "the Elf is its printed 1/1 with no Hand out" (S.powerToughnessOf elfId alone) (Just (1, 1))
    Spec.assertEqWith s "and a 2/2 once the Hand is" (S.powerToughnessOf elfId gs) (Just (2, 2))
    Spec.assertEqWith s "'other': the Hand stays its printed 3/2" (S.powerToughnessOf handId gs) (Just (3, 2))
  -- The KEYWORD half, moved on its own: still a creature, still alice's, and
  -- Goblin Piker (2/1, no keywords) is left alone.
  Spec.it s "CR 702.90 a creature you control WITHOUT infect gets nothing" $ do
    hand <- S.printingOf s registry "Hand of the Praetors"
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, withPiker) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        gs = snd (S.addCreature hand S.alice withPiker)
    Spec.assertEqWith s "the Piker is its printed 2/1" (S.powerToughnessOf pikerId gs) (Just (2, 1))
  -- The CONTROLLER half, moved on its own: the same infect creature, on the same
  -- shared battlefield (CR 400.1), under bob.
  Spec.it s "CR 109.5 'you control': an opponent's infect creature is not pumped" $ do
    hand <- S.printingOf s registry "Hand of the Praetors"
    elf <- S.printingOf s registry "Glistener Elf"
    let (bobsElf, withBobs) = S.addCreature elf S.bob (Setup.emptyGame S.bothPlayers)
        (alicesElf, withBoth) = S.addCreature elf S.alice withBobs
        gs = snd (S.addCreature hand S.alice withBoth)
    Spec.assertEqWith s "bob's Elf is its printed 1/1" (S.powerToughnessOf bobsElf gs) (Just (1, 1))
    -- The positive control on the same board: alice's own copy of the very same
    -- printing does grow, so the silence above is the controller and nothing else.
    Spec.assertEqWith s "and alice's own copy is a 2/2" (S.powerToughnessOf alicesElf gs) (Just (2, 2))

-- Wood Elemental ({3}{G} Creature -- Elemental, printed */*), whole text: "As
-- this creature enters, sacrifice any number of untapped Forests." / "Wood
-- Elemental's power and toughness are each equal to the number of Forests
-- sacrificed as it entered." Oracle text verified against Scryfall.
--
-- The first CDA in the pool whose quantity is a Quantity.InSlot. That is the arm
-- whose Nothing means "unanswered" outright: the entry replacement
-- (EntryRewrite.SacrificeAnyNumber) stamps the count on the permanent it made,
-- so an incarnation that never entered has nothing to read and CR 208.2a's last
-- sentence -- "if the ability needs to use a number that can't be determined,
-- including inside a calculation, use 0 instead of that number" -- supplies the
-- 0. Pawl.Engine.Quantity.determine is where that happens; the first test below
-- is what proves it for this arm, as Monstrous War-Leech's empty graveyard above
-- does for an Aggregation.Greatest over no candidates.
--
-- It is also the first card whose Filter reads CR 110.5's tap status
-- (Filter.IsTapped, spelled `Not IsTapped` for "untapped").
--
-- The counter half of the same entry rewrite is Shimatsu the Bloodcloaked's, in
-- Pawl.ReplacementSpec.
woodElementalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
woodElementalSpec s registry = Spec.describe s "Wood Elemental" $ do
  -- CR 604.3: a characteristic-defining ability functions in all zones, so the
  -- card in hand has a power and a toughness to report -- and no number of
  -- Forests sacrificed as it entered, because it has not entered. CR 208.2a
  -- makes that 0 rather than leaving the box blank.
  Spec.it s "CR 208.2a a Wood Elemental that has not entered has an undeterminable count, so it is 0/0" $ do
    forest <- S.printingOf s registry "Forest"
    woodElemental <- S.printingOf s registry "Wood Elemental"
    let (gs, held) = S.handOne woodElemental (S.landsInPlay forest 8)
    Spec.assertEqWith s "the seed is the slot the entry replacement fills" (PC.characteristicPT (Projection.baseCharacteristics held gs)) (Just (sacrificedCount, sacrificedCount))
    Spec.assertEqWith s "0/0" (S.powerToughnessOf held gs) (Just (0, 0))
  -- The proving pair's first half. Eight Forests, four of which pay for the
  -- {3}{G}: the four still untapped are the whole of the offer, a greedy answer
  -- takes all four, and the Elemental is a 4/4 that survives CR 704.5f.
  Spec.it s "CR 208.2a sacrificing four Forests makes it a 4/4 that lives" $ do
    forest <- S.printingOf s registry "Forest"
    woodElemental <- S.printingOf s registry "Wood Elemental"
    let (gs, held) = S.handOne woodElemental (S.landsInPlay forest 8)
        after = S.runPure sacrificesAll gs (S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority)
    case newestNamed "Wood Elemental" after of
      Nothing -> Spec.assertFailure s "Wood Elemental did not reach the battlefield"
      Just elementalId -> do
        Spec.assertEqWith s "4/4" (S.powerToughnessOf elementalId after) (Just (4, 4))
        -- CR 110.5 / 701.21a: only the four UNTAPPED Forests could be chosen, so
        -- the four the mana came from are still there -- and still tapped. An
        -- IsTapped that matched nothing would have emptied the board.
        Spec.assertEqWith s "four Forests survive" (length (forestsOn after)) 4
        Spec.assertBool s (all (\oid -> Game.isTapped oid after) (forestsOn after)) "every surviving Forest is tapped"
  -- The pair's second half, and CR 704.5f's own test. S.identityAnswer answers
  -- the empty set, which "any number" admits: the count is 0, so the CDA makes a
  -- 0/0 and the state-based action buries it. A Wood Elemental that kept a blank
  -- P/T instead would still be standing here.
  Spec.it s "CR 704.5f sacrificing nothing makes a 0/0 that dies" $ do
    forest <- S.printingOf s registry "Forest"
    woodElemental <- S.printingOf s registry "Wood Elemental"
    let (gs, held) = S.handOne woodElemental (S.landsInPlay forest 8)
        after = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority)
    Spec.assertEqWith s "the 0/0 Wood Elemental is gone" (newestNamed "Wood Elemental" after) Nothing
    Spec.assertEqWith s "and every Forest it declined to eat is still there" (length (forestsOn after)) 8

-- Pawl.Engine.Binding.sacrificedCount as a Quantity, which is what Wood
-- Elemental's characteristicPT holds. Spelled out here rather than imported from
-- the engine so the assertion is against the NAME the card data commits to.
sacrificedCount :: Quantity.Type.Quantity
sacrificedCount = Quantity.Type.InSlot (SlotName.MkSlotName (Text.pack "thatMany"))

-- Sacrifice everything the engine offers. What makes the tap filter testable: a
-- greedy answer eats every Forest it is shown, so a Forest left standing is one
-- that was never offered.
sacrificesAll :: Prompt.Prompt r -> r
sacrificesAll p = case p of
  Prompt.ChooseAnyNumberToSacrifice _ _ _ candidates -> Set.fromList candidates
  _ -> S.identityAnswer p

-- The Forests on the battlefield, whatever their tap state.
forestsOn :: GameState.GameState -> [ObjectId.ObjectId]
forestsOn gs = filter isForest (Set.toList (GameState.battlefield gs))
  where
    isForest oid = maybe False (\f -> Face.name f == CardName.MkCardName (Text.pack "Forest")) (Game.faceOf oid gs)

-- The newest battlefield permanent with this printed name -- ids ascend, and CR
-- 400.7 mints a fresh one on every zone change, so the id a cast was handed
-- names nothing on the battlefield.
newestNamed :: String -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack wanted))
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

-- Kird Ape ({R} Creature -- Ape, printed 1/1), whole text: "This creature gets
-- +1/+2 as long as you control a Forest." Oracle text verified against Scryfall.
--
-- CR 613.4c layer 7c, like Omnath's and unlike Serra Avatar's: the printed
-- box is 1/1, so the pump is a modification rather than a characteristic-defining
-- ability. What is new is the "as long as" clause -- the first
-- StaticAbility.condition in the pool.
--
-- CR 604.1 makes a static ability "simply true" and CR 604.2 keeps its effect
-- active while the permanent is on the battlefield and has the ability; the clause
-- narrows that to while a Forest is also there. CR 613.5 is why every test below
-- can assert two different answers on ONE board with nothing but a permanent
-- moving in between: the layer system is "continually and automatically
-- performed", so the modification stops and starts with no trigger, no resolution
-- and no stored effect anywhere.
--
-- Deliberately NOT CR 611.2b's "for as long as" duration, which ends a stored
-- effect once and for good; CR 611.2c's parenthetical ("Note that this works
-- differently than a continuous effect from a static ability") is the rule that
-- keeps the two apart, and the last test here is the observable difference -- the
-- bonus comes BACK.
kirdApeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kirdApeSpec s registry = Spec.describe s "Kird Ape" $ do
  -- Both halves of the clause on one board, with a permanent ENTERING between the
  -- two assertions. Asserting only the second half would pass for an engine that
  -- ignored the condition entirely; asserting only the first would pass for one
  -- that dropped the ability outright.
  Spec.it s "CR 604.2 a 1/1 with no Forest, and a 2/3 the moment one enters" $ do
    kirdApe <- S.printingOf s registry "Kird Ape"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    let (_, withMountain) = S.addCreature mountain S.alice (Setup.emptyGame S.bothPlayers)
        (apeId, noForest) = S.addCreature kirdApe S.alice withMountain
        withForest = snd (S.addCreature forest S.alice noForest)
    Spec.assertEqWith s "a Mountain is not a Forest" (S.powerToughnessOf apeId noForest) (Just (1, 1))
    Spec.assertEqWith s "a Forest entered" (S.powerToughnessOf apeId withForest) (Just (2, 3))
  -- CR 109.5: "you" is the ability's SOURCE's controller. Bob's Forest is on the
  -- same battlefield -- the Count's scope is every player's (CR 400.1 makes the
  -- battlefield shared) -- so an engine that counted Forests rather than Forests
  -- YOU control reads 2/3 here.
  Spec.it s "CR 109.5 an opponent's Forest is not one you control" $ do
    kirdApe <- S.printingOf s registry "Kird Ape"
    forest <- S.printingOf s registry "Forest"
    let (apeId, board) = S.addCreature kirdApe S.alice (Setup.emptyGame S.bothPlayers)
        bobsForest = snd (S.addCreature forest S.bob board)
    Spec.assertEqWith s "bob's Forest does nothing" (S.powerToughnessOf apeId bobsForest) (Just (1, 1))
  -- The clause read against the PROJECTION rather than the printed type line, and
  -- the flip driven by a real cast: Convincing Mirage's CR 614.1c as-enters choice
  -- makes the Forest an Island in layer 4 (CR 613.1d), which is strictly before
  -- the layer 7c the pump lands in, so the Ape shrinks. An engine that asked
  -- whether alice controlled a card PRINTED "Forest" leaves it a 2/3.
  --
  -- Also the CR 611.2c half: the Aura leaves and the +1/+2 comes back, which a
  -- duration could not do.
  Spec.it s "CR 613.1d a Convincing Mirage'd Forest stops being one, and the pump stops with it" $ do
    kirdApe <- S.printingOf s registry "Kird Ape"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    convincingMirage <- S.printingOf s registry "Convincing Mirage"
    let base = S.landsInPlay island 4
        (forestId, withForest) = S.addCreature forest S.alice base
        (apeId, withApe) = S.addCreature kirdApe S.alice withForest
        (withAura, auraSpell) = S.handOne convincingMirage withApe
        cast = S.runPure (mirageOn forestId Subtype.Island) withAura (S.cast S.alice auraSpell)
        enchanted = S.runPure (mirageOn forestId Subtype.Island) cast Stack.resolveTop
        -- CR 704.5m sends an Aura attached to nothing to the graveyard, but this
        -- only needs the Forest back: the Aura is removed from the battlefield
        -- directly, so its layer-4 effect is simply no longer gathered.
        unenchanted = removeFromBattlefield (auraOf enchanted) enchanted
    Spec.assertEqWith s "2/3 while the Forest is a Forest" (S.powerToughnessOf apeId withApe) (Just (2, 3))
    Spec.assertEqWith s "CR 305.7 the land is now only an Island" (Projection.subtypesOf forestId enchanted) (Set.singleton Subtype.Island)
    Spec.assertEqWith s "so the Ape is back to 1/1" (S.powerToughnessOf apeId enchanted) (Just (1, 1))
    Spec.assertEqWith s "and 2/3 again once the Aura is gone" (S.powerToughnessOf apeId unenchanted) (Just (2, 3))
  -- The other side of the same coin, and the one #765 was about: the test above
  -- moves the BOARD under a fixed clause, this one moves the CLAUSE over a fixed
  -- board.
  --
  -- CR 612.1: a text-changing effect "can apply to any words or symbols printed
  -- on that object, but generally affects only that object's rules text (which
  -- appears in its text box)". CR 604.2's "as long as" clause is printed in that
  -- text box exactly as the +1/+2 beside it is, so a Magical Hack naming Forest
  -- rewrites the clause too -- the hacked Ape asks after a Swamp.
  --
  -- CR 612.2 is satisfied on the way in: the clause's Forest is "a land type
  -- word used as a land type", which is the one use the rule lets a swap reach.
  -- That both words are BASIC land types is Magical Hack's own restriction
  -- rather than CR 612.2's.
  --
  -- Forest -> SWAMP rather than Forest -> Island, deliberately: the Island that
  -- pays the Hack's {U} is a land alice controls, so hacking into Island would
  -- read 2/3 for a reason that has nothing to do with the clause being rewritten.
  -- Both halves are asserted, since "the Ape is 1/1" alone passes for an engine
  -- that dropped the ability outright.
  Spec.it s "CR 612.1 a Magical Hack moves which land the 'as long as' clause names" $ do
    kirdApe <- S.printingOf s registry "Kird Ape"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    swamp <- S.printingOf s registry "Swamp"
    magicalHack <- S.printingOf s registry "Magical Hack"
    let base = S.landsInPlay island 1
        (_, withForest) = S.addCreature forest S.alice base
        (apeId, withApe) = S.addCreature kirdApe S.alice withForest
        (withHack, hackSpell) = S.handOne magicalHack withApe
        cast = S.runPure (hackAt apeId Subtype.Forest Subtype.Swamp) withHack (S.cast S.alice hackSpell)
        hacked = S.runPure (hackAt apeId Subtype.Forest Subtype.Swamp) cast Stack.resolveTop
        withSwamp = snd (S.addCreature swamp S.alice hacked)
    Spec.assertEqWith s "2/3 with a Forest, as printed" (S.powerToughnessOf apeId withApe) (Just (2, 3))
    Spec.assertEqWith s "the Forest is still there, but the clause no longer names it" (S.powerToughnessOf apeId hacked) (Just (1, 1))
    Spec.assertEqWith s "and a Swamp is what it names now" (S.powerToughnessOf apeId withSwamp) (Just (2, 3))

-- Magical Hack's two prompts: its target, forced onto the permanent the test
-- cares about (the board offers several), and the basic-land-type pair its own
-- text asks for.
-- Everything else defers to S.identityAnswer. Same shape as CombatSpec's
-- castHackAt, which is local there for the same reason this one is local here.
hackAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
hackAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject oid)) sets
  Prompt.ChooseLandTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- Convincing Mirage's two prompts: its CR 303.4a enchant slot, forced onto the
-- one land the test cares about (the board offers five), and its CR 614.1c
-- as-enters basic land type. Everything else defers to S.identityAnswer.
-- Pawl.AuraSpec has the same answerer for the same card; this one is local
-- because the group needs it and nothing else here does.
mirageOn :: ObjectId.ObjectId -> Subtype.Subtype -> Prompt.Prompt r -> r
mirageOn landId subtype p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject landId)) sets
  Prompt.ChooseBasicLandType {} -> subtype
  _ -> S.identityAnswer p

-- The one Convincing Mirage on the battlefield, by printed name (the
-- leechesOnBattlefield precedent above: the cast's ObjectId names a spell that
-- CR 400.7 has already replaced).
auraOf :: GameState.GameState -> ObjectId.ObjectId
auraOf gs = case filter isMirage (Set.toList (GameState.battlefield gs)) of
  oid : _ -> oid
  [] -> ObjectId.MkObjectId 999
  where
    isMirage oid = maybe False (\f -> Face.name f == CardName.MkCardName (Text.pack "Convincing Mirage")) (Game.faceOf oid gs)

-- Take a permanent off the battlefield without routing it anywhere, so a test can
-- ask what the board looks like once its continuous effect is no longer gathered.
-- Not a zone change: nothing here is a CR 400.7 move, and no test using it reads
-- the graveyard.
removeFromBattlefield :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
removeFromBattlefield oid gs =
  gs
    { GameState.battlefield = Set.delete oid (GameState.battlefield gs),
      GameState.objects = Map.delete oid (GameState.objects gs)
    }

-- Serra Avatar ({4}{W}{W}{W} Creature -- Avatar, printed */*), first line: "Serra
-- Avatar's power and toughness are each equal to your life total." Oracle text
-- verified against Scryfall.
--
-- CR 604.3 layer 7a, like Tarmogoyf's and Nightmare's above and unlike Omnath's:
-- the printed box is a star, so the pair comes from a characteristic-defining
-- ability rather than from a CR 613.4c modification. What is new is WHAT it
-- reads -- Quantity.LifeTotal, CR 119.1's scalar attached to a PLAYER, which is
-- neither a population over a zone (Count) nor a mana pool (ManaCount).
--
-- The card's second line, the CR 603.6 from-anywhere graveyard trigger, is
-- Pawl.TriggerSpec's half.
serraAvatarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
serraAvatarSpec s registry = Spec.describe s "Serra Avatar" $ do
  -- CR 707.2a again, for the reason Tarmogoyf's seed test above states: what the
  -- seed holds must be the unevaluated quantity, not a number frozen at entry.
  Spec.it s "CR 604.3 the seed carries the life total as a QUANTITY, with the printed star substituted" $ do
    avatar <- S.printingOf s registry "Serra Avatar"
    let (avatarId, gs) = S.addCreature avatar S.alice (Setup.emptyGame S.bothPlayers)
        yourLife = Quantity.Type.LifeTotal (PlayerRef.Relative PlayerRelation.You)
    Spec.assertEqWith s "both boxes are the same quantity" (PC.characteristicPT (Projection.baseCharacteristics avatarId gs)) (Just (yourLife, yourLife))
  Spec.it s "CR 119.1 a starting life total of 20 makes it a 20/20" $ do
    avatar <- S.printingOf s registry "Serra Avatar"
    let (avatarId, gs) = S.addCreature avatar S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "alice is at 20" (S.lifeOf S.alice gs) (Just 20)
    Spec.assertEqWith s "power" (Projection.powerOf avatarId gs) (Just 20)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf avatarId gs) (Just 20)
  -- THE LIVENESS FALSIFIER, upward. CR 119.3 adjusts a life total the moment an
  -- effect says to, and nothing touches the Avatar -- only the next projection's
  -- read of alice's life. Renewed Faith is targetless, so no interpreter here has
  -- to choose who gains.
  Spec.it s "CR 119.3 Renewed Faith's 6 life makes the Avatar a 26/26" $ do
    plains <- S.printingOf s registry "Plains"
    renewedFaith <- S.printingOf s registry "Renewed Faith"
    avatar <- S.printingOf s registry "Serra Avatar"
    let (gs0, spellId) = S.handOne renewedFaith (S.landsInPlay plains 3)
        (avatarId, board) = S.addCreature avatar S.alice gs0
        cast = S.runPure S.identityAnswer board (S.cast S.alice spellId)
        after = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "20/20 before" (S.powerToughnessOf avatarId board) (Just (20, 20))
    Spec.assertEqWith s "alice gained 6" (S.lifeOf S.alice after) (Just 26)
    Spec.assertEqWith s "26/26 after" (S.powerToughnessOf avatarId after) (Just (26, 26))
  -- The same falsifier downward, and the one that matters for the board: an
  -- Avatar that stayed 20/20 while its controller fell to 18 would be reading a
  -- number frozen at entry. S.identityAnswer targets the least Recipient and
  -- Sign in Blood's pool is Players, so alice (player 0) is the target without a
  -- bespoke interpreter.
  Spec.it s "CR 119.3 Sign in Blood's 2 life makes the Avatar an 18/18" $ do
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    avatar <- S.printingOf s registry "Serra Avatar"
    let (gs0, spellId) = S.handOne signInBlood (twoCardLibrary swamp (S.landsInPlay swamp 2))
        (avatarId, board) = S.addCreature avatar S.alice gs0
        cast = S.runPure S.identityAnswer board (S.cast S.alice spellId)
        after = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "20/20 before" (S.powerToughnessOf avatarId board) (Just (20, 20))
    Spec.assertEqWith s "alice lost 2" (S.lifeOf S.alice after) (Just 18)
    Spec.assertEqWith s "18/18 after" (S.powerToughnessOf avatarId after) (Just (18, 18))
  -- CR 109.5 / 604.3a(3): "your" in a characteristic-defining ability is the
  -- object's OWN controller. The two players are held at different life totals
  -- and nothing but control moves between the assertions, so an Avatar reading
  -- its owner's life -- or its source's, or the active player's -- gives 18 where
  -- the rule gives 20.
  Spec.it s "CR 109.5 the life total read is the CONTROLLER's, not the owner's" $ do
    swamp <- S.printingOf s registry "Swamp"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    avatar <- S.printingOf s registry "Serra Avatar"
    let (gs0, spellId) = S.handOne signInBlood (twoCardLibrary swamp (S.landsInPlay swamp 2))
        (avatarId, board) = S.addCreature avatar S.alice gs0
        cast = S.runPure S.identityAnswer board (S.cast S.alice spellId)
        drained = S.runPure S.identityAnswer cast Stack.resolveTop
        stolen = S.giveControl avatarId S.bob drained
    Spec.assertEqWith s "alice at 18, bob untouched at 20" (S.lifeOf S.alice drained, S.lifeOf S.bob drained) (Just 18, Just 20)
    Spec.assertEqWith s "under alice: 18/18" (S.powerToughnessOf avatarId drained) (Just (18, 18))
    Spec.assertEqWith s "under bob: 20/20" (S.powerToughnessOf avatarId stolen) (Just (20, 20))

-- Two cards in alice's library, so Sign in Blood's "draws two cards" has
-- something to draw: an empty library would lose her the game to CR 704.5b
-- rather than shrink her Avatar.
twoCardLibrary :: Printing.Printing -> GameState.GameState -> GameState.GameState
twoCardLibrary printing gs = snd (S.addLibraryCard printing S.alice (snd (S.addLibraryCard printing S.alice gs)))

-- Omnath, Locus of Mana ({2}{G} Legendary Creature -- Elemental), second line:
-- "Omnath gets +1/+1 for each unspent green mana you have." Oracle text verified
-- against Scryfall.
--
-- CR 613.4c layer 7c -- a MODIFICATION, not a characteristic-defining ability:
-- the printed box says 1/1, so there is no CR 208.2a star for a CDA to define and
-- the pump rides on the static ability instead. That is why these tests read
-- Projection.powerOf and never PC.characteristicPT, unlike Tarmogoyf's and
-- Nightmare's above.
--
-- The magnitude is a Quantity.ManaCount, which counts a MANA POOL rather than a
-- zone: CR 400.1 lists seven zones and the pool is none of them, and CR 106.4
-- gives it its own existence attached to a player.
omnathSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
omnathSpec s registry = Spec.describe s "Omnath, Locus of Mana" $ do
  Spec.it s "CR 613.4c an empty pool adds nothing, so Omnath is its printed 1/1" $ do
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (omnathId, gs) = S.addCreature omnath S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "power" (Projection.powerOf omnathId gs) (Just 1)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf omnathId gs) (Just 1)
  Spec.it s "CR 613.4c Omnath gets +1/+1 for each unspent green mana you have" $ do
    forest <- S.printingOf s registry "Forest"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (omnathId, g1) = S.addCreature omnath S.alice (Setup.emptyGame S.bothPlayers)
        (first, g2) = S.addCreature forest S.alice g1
        (second, g3) = S.addCreature forest S.alice g2
        floated = tapAll [first, second] g3
    Spec.assertEqWith s "two green floating: 1+2 power" (Projection.powerOf omnathId floated) (Just 3)
    Spec.assertEqWith s "and 1+2 toughness" (Projection.toughnessOf omnathId floated) (Just 3)
  -- THE LIVENESS FALSIFIER, and the reason the count belongs in the projection
  -- rather than in a sampled or stored value. CR 106.4's pool changes whenever a
  -- mana ability resolves, which CR 605.3a lets happen any time its controller
  -- has priority and CR 605.3b has happen immediately, off the stack -- so there
  -- is no state-based action (CR 704.3) and no priority pass between tapping the
  -- Forest and Omnath being bigger. Nothing here calls
  -- Engine.settleForPriority, and the assertions bracket a single tap.
  --
  -- The base characteristics are asserted unchanged alongside, which is what
  -- rules out the pump having been baked into the seed: the printed 1/1 is still
  -- 1/1 at layer 7a while the projection reads 4/4.
  Spec.it s "CR 106.4 the count is live: tapping a Forest grows Omnath with nothing settled in between" $ do
    forest <- S.printingOf s registry "Forest"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (omnathId, g1) = S.addCreature omnath S.alice (Setup.emptyGame S.bothPlayers)
        (first, g2) = S.addCreature forest S.alice g1
        (second, g3) = S.addCreature forest S.alice g2
        (third, board) = S.addCreature forest S.alice g3
        one = tapAll [first] board
        two = tapAll [second] one
        three = tapAll [third] two
    Spec.assertEqWith s "nothing tapped yet" (Projection.powerOf omnathId board) (Just 1)
    Spec.assertEqWith s "one Forest" (Projection.powerOf omnathId one) (Just 2)
    Spec.assertEqWith s "two Forests" (Projection.powerOf omnathId two) (Just 3)
    Spec.assertEqWith s "three Forests" (Projection.powerOf omnathId three) (Just 4)
    Spec.assertEqWith s "the seed still says 1" (PC.power (Projection.baseCharacteristics omnathId three)) (Just 1)
    Spec.assertBool s (null (GameState.continuousEffects three)) "no continuous effect was stored for the pump"
  -- CR 106.1a: only GREEN mana counts. An Island's {U} floats in the same pool
  -- and is invisible to the count -- the falsifier for a ManaCount that ignores
  -- its filter and just measures the pool's size.
  Spec.it s "CR 106.1a blue mana in the same pool does not pump Omnath" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (omnathId, g1) = S.addCreature omnath S.alice (Setup.emptyGame S.bothPlayers)
        (islandId, g2) = S.addCreature island S.alice g1
        (forestId, g3) = S.addCreature forest S.alice g2
        blueOnly = tapAll [islandId] g3
        both = tapAll [forestId] blueOnly
    Spec.assertEqWith s "one blue floating, still 1/1" (Projection.powerOf omnathId blueOnly) (Just 1)
    Spec.assertEqWith s "the green one is what moves it" (Projection.powerOf omnathId both) (Just 2)
  -- CR 109.5: "you" in a static ability's text is the ability's SOURCE's
  -- controller. THE FALSIFIER for reading the affected object's controller (or
  -- no perspective at all): Omnath is the affected object here as well as the
  -- source, so the two coincide -- what separates them is BOB's pool, which a
  -- PlayerRef.EachPlayer fold would add in.
  --
  -- This is the first static ability in the pool whose modification carries a
  -- player-scoped quantity, so it is also the first test of
  -- Projection.applyModification's context (#155).
  Spec.it s "CR 109.5 the count reads Omnath's controller: an opponent's green mana does not pump it" $ do
    forest <- S.printingOf s registry "Forest"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (omnathId, g1) = S.addCreature omnath S.alice (Setup.emptyGame S.bothPlayers)
        (bobsForest, g2) = S.addCreature forest S.bob g1
        (alicesForest, g3) = S.addCreature forest S.alice g2
        bobFloats = tapAll [bobsForest] g3
        bothFloat = tapAll [alicesForest] bobFloats
    Spec.assertEqWith s "bob's green is not alice's" (Projection.powerOf omnathId bobFloats) (Just 1)
    Spec.assertEqWith s "alice's own green is" (Projection.powerOf omnathId bothFloat) (Just 2)

-- Tap each permanent for mana in turn, threading the state. Every source here is
-- a basic land with one mana ability and one colour, so CR 605.3a's activation
-- asks nothing that S.identityAnswer has to choose between.
tapAll :: [ObjectId.ObjectId] -> GameState.GameState -> GameState.GameState
tapAll oids gs = List.foldl' (\g oid -> S.runPure S.identityAnswer g (Mana.tapForMana oid)) gs oids
