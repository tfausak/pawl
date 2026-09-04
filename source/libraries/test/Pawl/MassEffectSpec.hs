{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over the effects with no target that sweep a whole zone:
-- mass destruction and return, reanimation, and the exile-and-play effects.
-- The machinery is Pawl.ResolveSpec.
module Pawl.MassEffectSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Whether a seat is still playing, and if not why it left (CR 800.4a). Nothing
-- for a PlayerId no roster holds.
statusOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe Status.Status
statusOf pid gs = fmap Player.status (Map.lookup pid (GameState.players gs))

-- The one activated ability of a printing that declares exactly one -- Prodigal
-- Sorcerer's {T}, which is all these fixtures reach for. Nothing for any other
-- printing, so a card that grew a second ability fails the case that names it
-- rather than silently picking whichever came first (Pawl.TargetSpec's
-- soleTargetSlot is the same shape for the same reason).
soleActivatedAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
soleActivatedAbility p = case Face.activatedAbilities (S.combinedFace p) of
  [only] -> Just only
  _ -> Nothing

-- Day of Judgment, cast off four Plains from alice's hand and resolved. Every
-- test in the group below goes through the whole card -- cast, pay, resolve --
-- because "Destroy all creatures" has nothing to exercise at the opcode level
-- that the card does not exercise better: it takes no target and prompts for
-- nothing, so a hand-built applyEffect call would differ from a real cast only
-- in the mana.
castDayOfJudgment :: Printing.Printing -> Printing.Printing -> GameState.GameState -> GameState.GameState
castDayOfJudgment plains dayOfJudgment board =
  let (withSpell, spell) = S.handOne dayOfJudgment (List.foldl' (\gs _ -> snd (S.addCreature plains S.alice gs)) board [1 :: Int .. 4])
      afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
   in S.runPure S.identityAnswer afterCast Stack.resolveTop

destroyAllSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
destroyAllSpec s registry = Spec.describe s "DestroyAll" $ do
  -- CR 109.2: "Destroy all creatures" includes no "card" or "spell", so it
  -- means every CREATURE PERMANENT on the battlefield -- both players' and,
  -- pointedly, the caster's own. Nothing else on the battlefield is touched.
  Spec.it s "Day of Judgment destroys every creature, the caster's own included, and leaves noncreature permanents alone" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (hers, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (his, g2) = S.addCreature piker S.bob g1
        (equipment, g3) = S.addCreature bonesplitter S.alice g2
        resolved = castDayOfJudgment plains dayOfJudgment g3
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertBool s (not (S.onBattlefield his resolved)) "bob's creature died"
    Spec.assertBool s (not (S.onBattlefield hers resolved)) "and so did alice's own"
    Spec.assertBool s (S.onBattlefield equipment resolved) "the Equipment is not a creature and stands"
    Spec.assertEqWith s "no creatures left at all" (Set.size (Set.filter (`Projection.isCreatureOf` resolved) (GameState.battlefield resolved))) 0
  -- CR 702.12b: "A permanent with indestructible can't be destroyed." The
  -- mass form goes through Event.destroy exactly as the single-target form
  -- does, so it inherits that gate rather than bypassing it.
  Spec.it s "CR 702.12b an indestructible creature survives Day of Judgment" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (myr, g1) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
        (his, g2) = S.addCreature piker S.bob g1
        resolved = castDayOfJudgment plains dayOfJudgment g2
    Spec.assertBool s (S.onBattlefield myr resolved) "the Myr can't be destroyed"
    Spec.assertBool s (not (S.onBattlefield his resolved)) "the Piker can"
  -- CR 701.19a: a regeneration shield "protects the permanent the next time
  -- it would be destroyed this turn". Day of Judgment says nothing about
  -- regeneration, so it carries Regenerability.Regenerable and the shield
  -- applies -- the creature is instead tapped and stays.
  Spec.it s "CR 701.19a a regeneration shield saves its creature from Day of Judgment" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (shielded, g1) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (bare, g2) = S.addCreature piker S.bob g1
        resolved = castDayOfJudgment plains dayOfJudgment (S.addRegenShield shielded g2)
    Spec.assertBool s (S.onBattlefield shielded resolved) "the shielded creature stands"
    Spec.assertEqWith s "and CR 701.19a taps it" (fmap Object.tapped (Game.lookupObject shielded resolved)) (Just TapState.Tapped)
    Spec.assertBool s (not (S.onBattlefield bare resolved)) "its unshielded twin died"
  -- CR 608.2f: "Some spells and abilities include actions taken on multiple
  -- players and/or objects. In most cases, each such action is processed
  -- simultaneously." So the affected set is fixed once, before the first
  -- creature dies, and a creature that only IS one because of another
  -- creature dies with it rather than being spared.
  --
  -- Opalescence animates March of the Machines (a non-Aura enchantment);
  -- March in turn animates the Bonesplitter (a noncreature artifact). March
  -- is added BEFORE the Bonesplitter on purpose: it therefore has the lower
  -- ObjectId and is swept first, so an implementation that re-derived "is it
  -- a creature?" after each destruction would spare the Bonesplitter. Both
  -- die. Opalescence itself is never a creature ("each OTHER") and stands.
  Spec.it s "CR 608.2f the affected set is fixed before the first destruction: March of the Machines and the Bonesplitter it animates die together" $ do
    plains <- S.printingOf s registry "Plains"
    opalescence <- S.printingOf s registry "Opalescence"
    march <- S.printingOf s registry "March of the Machines"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (opal, g1) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
        (animator, g2) = S.addCreature march S.alice g1
        (equipment, board) = S.addCreature bonesplitter S.alice g2
    Spec.assertBool s (Projection.isCreatureOf animator board) "setup: March is a creature via Opalescence"
    Spec.assertBool s (Projection.isCreatureOf equipment board) "setup: the Bonesplitter is a creature via March"
    Spec.assertBool s (animator < equipment) "setup: March is swept first"
    let resolved = castDayOfJudgment plains dayOfJudgment board
    Spec.assertBool s (not (S.onBattlefield animator resolved)) "March died"
    Spec.assertBool s (not (S.onBattlefield equipment resolved)) "and so did the Bonesplitter it animated"
    Spec.assertBool s (S.onBattlefield opal resolved) "Opalescence animates each OTHER enchantment, so it was never a creature"
  -- CR 608.2f again, on the other half of what "simultaneously" means: not
  -- just WHICH permanents the instruction names, but WHEN each one's CR
  -- 702.12b gate is judged. "A permanent with indestructible can't be
  -- destroyed" is asked of every victim while every other victim is still on
  -- the battlefield -- including the one whose static ability is granting the
  -- indestructible. So the Walls of Ba Sing Se die and what they protect does
  -- not.
  --
  -- The Walls are added FIRST on purpose, so they hold the lower ObjectId and
  -- are swept first. An implementation that judged each victim against the
  -- board the previous ones had already left would find the grant gone by the
  -- time it reached the Piker and kill it too.
  Spec.it s "CR 608.2f every victim's CR 702.12b gate is judged before any of them dies: the Walls of Ba Sing Se die, what they protect stands" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    walls <- S.printingOf s registry "The Walls of Ba Sing Se"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (granter, g1) = S.addCreature walls S.alice (Setup.emptyGame S.bothPlayers)
        (protected, g2) = S.addCreature piker S.alice g1
        (his, board) = S.addCreature piker S.bob g2
    Spec.assertBool s (granter < protected) "setup: the Walls are swept before the creature they protect"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Indestructible granter board)) "setup: the Walls do not benefit from their own grant"
    Spec.assertBool s (Projection.hasKeyword Keyword.Indestructible protected board) "setup: their controller's other creature does"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Indestructible his board)) "setup: the opponent's does not"
    let resolved = castDayOfJudgment plains dayOfJudgment board
    Spec.assertBool s (not (S.onBattlefield granter resolved)) "the Walls are destroyed"
    Spec.assertBool s (S.onBattlefield protected resolved) "the creature they protected stands"
    Spec.assertBool s (not (S.onBattlefield his resolved)) "and the opponent's creature, never protected, died"
  -- The same board with the two permanents added in the other order, so the
  -- Walls are swept LAST. CR 608.2f leaves nothing for the sweep order to
  -- decide here, and that is the claim: the outcome is identical. This is the
  -- arrangement the sequential reading happens to get right, and it is worth
  -- pinning precisely because it is the one that would keep passing.
  Spec.it s "CR 608.2f the outcome does not depend on where the granter falls in the sweep order" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    walls <- S.printingOf s registry "The Walls of Ba Sing Se"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (protected, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (granter, board) = S.addCreature walls S.alice g1
    Spec.assertBool s (protected < granter) "setup: the Walls are swept last this time"
    let resolved = castDayOfJudgment plains dayOfJudgment board
    Spec.assertBool s (not (S.onBattlefield granter resolved)) "the Walls are destroyed"
    Spec.assertBool s (S.onBattlefield protected resolved) "the creature they protected stands"
  -- CR 608.2f a third time, now about the CR 616.1 loop each victim's
  -- put-into-graveyard runs rather than about the CR 702.12b gate above. The
  -- batch is one simultaneous event, so the replacement effects in force for
  -- it are the ones on the battlefield when it began -- including one
  -- belonging to a permanent the batch is itself killing.
  --
  -- Opalescence animates Rest in Peace (a non-Aura enchantment) into a 2/2,
  -- so Day of Judgment sweeps it alongside bob's Piker. Rest in Peace is
  -- added FIRST on purpose: it holds the lower ObjectId and is swept first,
  -- so an implementation that re-collected each victim's candidates from the
  -- live board would find it already gone by the time it reached the Piker
  -- and bury the Piker instead of exiling it.
  Spec.it s "CR 608.2f a Rest in Peace dying in the sweep still exiles the cards the sweep puts into graveyards" $ do
    plains <- S.printingOf s registry "Plains"
    opalescence <- S.printingOf s registry "Opalescence"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (opal, g1) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
        (rip, g2) = S.addCreature restInPeace S.alice g1
        (his, board) = S.addCreature piker S.bob g2
    Spec.assertBool s (Projection.isCreatureOf rip board) "setup: Opalescence animates Rest in Peace"
    Spec.assertBool s (rip < his) "setup: Rest in Peace is swept before the Piker"
    let resolved = castDayOfJudgment plains dayOfJudgment board
    Spec.assertBool s (not (S.onBattlefield rip resolved)) "Rest in Peace died"
    Spec.assertBool s (not (S.onBattlefield his resolved)) "and so did the Piker"
    Spec.assertEqWith s "the Piker was exiled, not buried" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 0
    Spec.assertEqWith s "the Piker's card is in exile" (length (Game.zoneMembers Zone.Exile S.bob resolved)) 1
    Spec.assertEqWith s "and Rest in Peace exiled its own card too" (length (Game.zoneMembers Zone.Exile S.alice resolved)) 1
    Spec.assertBool s (S.onBattlefield opal resolved) "Opalescence animates each OTHER enchantment, so it stands"
  -- CR 115.10a: "Unless that object or player is identified by the word
  -- 'target' ... it's not a target." "All creatures" is not a target, so the
  -- card declares no target slot and the cast never raises a target prompt
  -- -- and CR 608.2b, which is about targets, has nothing to fizzle.
  Spec.it s "CR 115.10a Day of Judgment targets nothing: no target slot and no target prompt" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let card = Printing.card dayOfJudgment
        (his, g1) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (withSpell, spell) = S.handOne dayOfJudgment (List.foldl' (\gs _ -> snd (S.addCreature plains S.alice gs)) g1 [1 :: Int .. 4])
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseTargets {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asked = State.execState (Engine.runGame countingAnswer withSpell (S.cast S.alice spell)) 0
    Spec.assertEqWith s "no target slot anywhere on the card" (Modal.allTargetSlots (Face.spell (Card.combined card))) Map.empty
    Spec.assertEqWith s "and nothing was asked to target" asked 0
    -- The board still resolves the way the first test says it does, from the
    -- same cast -- so "targets nothing" is not "affects nothing".
    Spec.assertBool s (not (S.onBattlefield his (castDayOfJudgment plains dayOfJudgment g1))) "the creature still died"

-- Evacuation ({3}{U}{U} instant, "Return all creatures to their owners'
-- hands"), the pool's producer for a MoveToZone over a SET rather than over a
-- slot. Cast off five Islands from alice's hand and resolved, for the reason
-- castDayOfJudgment gives: the card takes no target and prompts for nothing, so
-- a hand-built applyEffect call would differ from a real cast only in the mana.
returnAllSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
returnAllSpec s registry = Spec.describe s "ReturnAll" $ do
  -- CR 109.2 makes "all creatures" every creature PERMANENT on the battlefield,
  -- and CR 400.3 files each arrival in its OWNER's hand -- so a 2/1 board splits
  -- 2/1 across the two hands rather than piling into the caster's. The land is
  -- what an implementation returning every permanent would trip over, and the
  -- 2/1 asymmetry is what one returning a creature per player would.
  Spec.it s "Evacuation returns every creature to its owner's hand and leaves a land alone" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    evacuation <- S.printingOf s registry "Evacuation"
    let (herFirst, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (herSecond, g2) = S.addCreature piker S.alice g1
        (his, g3) = S.addCreature piker S.bob g2
        (land, board) = S.addCreature forest S.alice g3
        (withSpell, spell) = S.handOne evacuation (List.foldl' (\gs _ -> snd (S.addCreature island S.alice gs)) board [1 :: Int .. 5])
        -- The baseline is taken AFTER the cast, where the Evacuation itself has
        -- already left alice's hand for the stack, so the two deltas below count
        -- returning creatures and nothing else.
        afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
        resolved = S.runPure S.identityAnswer afterCast Stack.resolveTop
        survivors = Set.difference (GameState.battlefield afterCast) (Set.fromList [herFirst, herSecond, his])
    Spec.assertEqWith
      s
      "exactly the three creatures left the battlefield, and each owner's hand grew by their own"
      ( GameState.battlefield resolved,
        Set.member land (GameState.battlefield resolved),
        S.handSize S.alice resolved - S.handSize S.alice afterCast,
        S.handSize S.bob resolved - S.handSize S.bob afterCast
      )
      (survivors, True, 2, 1)

-- CR 109.2a's reading of a description -- "a description of an object that
-- includes the word 'card' and the name of a zone ... means a card matching that
-- description in the stated zone" -- swept as a SET rather than targeted:
-- ObjectRef.EachCardInGraveyard, where returnAllSpec above is CR 109.2's
-- battlefield default.
--
-- Rise of the Dark Realms {7}{B}{B} Sorcery -- "Put all creature cards from all
-- graveyards onto the battlefield under your control." (name, cost, type line and
-- Oracle text checked against api.scryfall.com). Its whole text is that one
-- sentence, so nothing else on the card can be what these assertions read.
--
-- THREE SEATS, and a graveyard per seat, because the board has to tell four
-- readings of "all graveyards" apart:
--
--   * EACH PLAYER'S versus YOUR OWN. bob and carol each bury a creature card of a
--     printing nobody else has, and both must be reanimated.
--   * EACH PLAYER'S versus AN OPPONENT'S. alice buries one too, and a two-seat
--     board would leave "opponents" and "each other player" indistinguishable
--     besides.
--   * CREATURE CARDS versus the whole zone. Every graveyard also holds one
--     non-creature card, and each must stay buried.
--   * A GRAVEYARD versus the battlefield. bob controls a Benalish Hero, which the
--     battlefield reading of the same sentence would hand to alice "under your
--     control"; it stays bob's, and the graveyards empty of creatures instead.
--
-- Cast off nine Swamps and resolved, for the reason castDayOfJudgment gives: the
-- card takes no target and prompts for nothing, so a hand-built applyEffect call
-- would differ from a real cast only in the mana.
riseOfTheDarkRealmsSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
riseOfTheDarkRealmsSpec s registry = Spec.describe s "RiseOfTheDarkRealms" $ do
  Spec.it s "CR 109.2a every creature card in every graveyard is reanimated under the caster's control" $ do
    swamp <- S.printingOf s registry "Swamp"
    rise <- S.printingOf s registry "Rise of the Dark Realms"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    sentry <- S.printingOf s registry "Ogre Sentry"
    hero <- S.printingOf s registry "Benalish Hero"
    murder <- S.printingOf s registry "Murder"
    judgment <- S.printingOf s registry "Day of Judgment"
    forest <- S.printingOf s registry "Forest"
    let mana = List.foldl' (\g _ -> snd (S.addCreature swamp S.alice g)) S.threePlayerGame [1 .. (9 :: Int)]
        (heroId, withHero) = S.addCreature hero S.bob mana
        buried =
          List.foldl'
            (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g))
            withHero
            [ (piker, S.alice),
              (murder, S.alice),
              (maiden, S.bob),
              (judgment, S.bob),
              (sentry, S.carol),
              (forest, S.carol)
            ]
        (withSpell, spell) = S.handOne rise buried
        afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
        after = S.runPure S.identityAnswer afterCast Stack.resolveTop
        -- Every battlefield object that is not one of alice's nine Swamps, by
        -- name and CONTROLLER: CR 400.7 mints a new object at the destination, so
        -- a reanimated card cannot be found by the id it was buried under.
        reanimated gs =
          List.sort
            [ (fmap S.nameOf (Game.cardOf oid gs), Projection.controllerOf oid gs)
            | oid <- Set.toList (GameState.battlefield gs),
              fmap S.nameOf (Game.cardOf oid gs) /= Just (S.nameOf (Printing.card swamp))
            ]
        named = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith
      s
      "all three buried creature cards are on the battlefield under alice's control, and bob keeps the one he already controlled"
      (reanimated after)
      ( List.sort
          [ (named "Benalish Hero", Just S.bob),
            (named "Bird Maiden", Just S.alice),
            (named "Goblin Piker", Just S.alice),
            (named "Ogre Sentry", Just S.alice)
          ]
      )
    Spec.assertEqWith
      s
      "each graveyard keeps its non-creature card, and alice's also takes the spent sorcery (CR 608.2n)"
      ( List.sort (namesIn Zone.Graveyard S.alice after),
        namesIn Zone.Graveyard S.bob after,
        namesIn Zone.Graveyard S.carol after
      )
      ( List.sort [named "Murder", named "Rise of the Dark Realms"],
        [named "Day of Judgment"],
        [named "Forest"]
      )
    Spec.assertBool s (S.onBattlefield heroId after) "bob's creature was never moved, so nothing swept the battlefield"

-- The same CR 109.2a sweep with its scope taken from another SLOT of the same
-- announcement rather than from CR 109.5's perspective: riseOfTheDarkRealmsSpec
-- above names the whole table, this names the one player the trigger targeted;
-- see #1310.
--
-- Angel of Finality {3}{W} Creature -- Angel 3/4 -- "Flying / When this creature
-- enters, exile target player's graveyard." (name, cost, type line, power,
-- toughness and Oracle text checked against api.scryfall.com, 2026-08-20). The
-- whole card is transcribed, and the only clause with a resolution-time effect is
-- the trigger, so nothing else on it can be what these assertions read.
--
-- THREE SEATS, and a graveyard per seat, because the board has to tell four
-- readings of "target player's graveyard" apart:
--
--   * THE TARGETED SEAT versus YOUR OWN. alice controls the Angel and targets
--     bob, so a scope that had stayed CR 109.5's "you" would empty the wrong
--     graveyard.
--   * THE TARGETED SEAT versus EACH PLAYER'S. carol's graveyard is stocked too
--     and must survive -- the reading Rise of the Dark Realms takes.
--   * THE TARGETED SEAT versus OPPONENTS'. carol is alice's opponent as much as
--     bob is (CR 806.1), so her surviving separates those two readings as well;
--     a two-seat board could not.
--   * A GRAVEYARD versus the battlefield. bob controls a Benalish Hero, which
--     stays put: the sweep reads CR 400.1's per-player graveyard, not the seat's
--     permanents.
--
-- Every buried card is of a printing nobody else has, so the graveyard assertion
-- names which seat lost what rather than counting.
angelOfFinalitySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
angelOfFinalitySpec s registry = Spec.describe s "AngelOfFinality" $ do
  Spec.it s "CR 109.2a only the targeted player's graveyard is exiled" $ do
    angel <- S.printingOf s registry "Angel of Finality"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    sentry <- S.printingOf s registry "Ogre Sentry"
    hero <- S.printingOf s registry "Benalish Hero"
    murder <- S.printingOf s registry "Murder"
    judgment <- S.printingOf s registry "Day of Judgment"
    forest <- S.printingOf s registry "Forest"
    let (heroId, withHero) = S.addCreature hero S.bob S.threePlayerGame
        buried =
          List.foldl'
            (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g))
            withHero
            [ (piker, S.alice),
              (murder, S.alice),
              (maiden, S.bob),
              (judgment, S.bob),
              (sentry, S.carol),
              (forest, S.carol)
            ]
        (_, entered) = S.entersWithTrigger angel S.alice buried
        -- The offered set is FILTERED rather than rebuilt, so the answer is a
        -- recipient the engine itself minted; three seats are offered where one
        -- is wanted, so the prompt is a real choice rather than an elision.
        atBob :: Prompt.Prompt r -> r
        atBob p = case p of
          Prompt.ChooseTargets _ _ _ slots -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) slots
          _ -> S.identityAnswer p
        placed = S.runPure atBob entered Engine.placePendingTriggers
        after = S.runPure atBob placed Stack.resolveTop
        named = Just . CardName.MkCardName . Text.pack
        exiled gs = List.sort [fmap S.nameOf (Game.cardOf oid gs) | oid <- Set.toList (GameState.exile gs)]
    Spec.assertEqWith
      s
      "bob's graveyard is empty and the other two keep every card"
      ( namesIn Zone.Graveyard S.alice after,
        namesIn Zone.Graveyard S.bob after,
        namesIn Zone.Graveyard S.carol after
      )
      ( [named "Goblin Piker", named "Murder"],
        [],
        [named "Ogre Sentry", named "Forest"]
      )
    Spec.assertEqWith
      s
      "the two cards that left bob's graveyard are the two now in exile"
      (exiled after)
      (List.sort [named "Bird Maiden", named "Day of Judgment"])
    Spec.assertBool s (S.onBattlefield heroId after) "bob's creature was never moved, so nothing swept the battlefield"

-- CR 109.2a's reading again, over the OTHER per-player zone CR 400.1 gives out
-- and the one CR 400.2 calls hidden: ObjectRef.EachCardInHand, where
-- angelOfFinalitySpec above is the graveyard's.
--
-- Amnesia {3}{U}{U}{U} Sorcery -- "Target player reveals their hand and discards
-- all nonland cards." (name, cost, type line and Oracle text checked against
-- api.scryfall.com, 2026-08-22). Its whole text is that one sentence, so nothing
-- else on the card can be what these assertions read.
--
-- WHY THE HAND MAY BE SWEPT AT ALL: the card prints its own reveal (CR 701.20a),
-- so which cards matched is public before any of them leaves. That is the
-- visibility answer Pawl.Types.EachCardInHand's header states, and it is the
-- card's rather than the engine's.
--
-- NO PROMPT, and that is a rule and not an elision: CR 701.9b's default choice
-- is over WHICH cards, and this card names them, so there is nothing to ask.
--
-- TWO SEATS, with three control legs on the board, one per way the sweep can go
-- wrong:
--
--   * THE FILTER. bob's hand holds a Forest, which "nonland" must leave standing
--     -- a sweep with no filter, or one whose filter answers True on every
--     candidate, empties his hand instead.
--   * THE SCOPE. alice holds a Goblin Piker of her own, which "target player's"
--     must leave standing -- a sweep wired to the resolving controller takes it.
--   * THE FUNNEL. bob's Bartered Cow, {3}{W} 3/3 Creature -- Ox, "When this
--     creature dies and when you discard this card, create a Food token", is the
--     only board-visible witness that CR 701.9a's DISCARD ran rather than a bare
--     hand-to-graveyard move. Writing Amnesia as an Effect.MoveToZone would put
--     every card in the right graveyard and leave this silent, which is what
--     assertion four keeps dead.
--
-- Distinct printings throughout, so each assertion reads identity and not merely
-- a count. Six Islands, cast and resolved for real, and the trigger placed and
-- resolved after -- the Food is only on the battlefield once it has.
amnesiaSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
amnesiaSpec s registry = Spec.describe s "Amnesia" $ do
  Spec.it s "CR 109.2a the nonland cards leave the targeted hand as DISCARDS, and the land and the caster's own hand stand" $ do
    amnesia <- S.printingOf s registry "Amnesia"
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    cow <- S.printingOf s registry "Bartered Cow"
    -- handOne REPLACES alice's hand and sets the phase up for a cast, so it runs
    -- FIRST and every control leg is appended after it.
    let (withSpell, spell) = S.handOne amnesia (S.landsInPlay island 6)
        (_, withHers) = S.addHandCard piker S.alice withSpell
        (_, withCow) = S.addHandCard cow S.bob withHers
        (_, withHis) = S.addHandCard maiden S.bob withCow
        (_, ready) = S.addHandCard forest S.bob withHis
        -- The offered set is FILTERED rather than rebuilt, so the recipient is
        -- one the engine itself minted and a mutation cannot be repaired by an
        -- answerer that goes looking for a legal pick.
        atBob :: Prompt.Prompt r -> r
        atBob p = case p of
          Prompt.ChooseTargets _ _ _ slots -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) slots
          _ -> S.identityAnswer p
        cast = S.runPure atBob ready (S.cast S.alice spell)
        resolved = S.runPure atBob cast Stack.resolveTop
        placed = S.runPure atBob resolved Engine.placePendingTriggers
        after = S.runPure atBob placed Stack.resolveTop
        named = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith
      s
      "alice's own hand is untouched: the sweep read the target slot, not the resolving controller"
      (namesIn Zone.Hand S.alice after)
      [named "Goblin Piker"]
    Spec.assertEqWith
      s
      "bob keeps exactly his land: 'nonland' matched the other two and not this one"
      (namesIn Zone.Hand S.bob after)
      [named "Forest"]
    Spec.assertEqWith
      s
      "and both nonland cards are in bob's own graveyard (CR 400.3)"
      (List.sort (namesIn Zone.Graveyard S.bob after))
      (List.sort [named "Bartered Cow", named "Bird Maiden"])
    Spec.assertEqWith
      s
      "the Cow's CR 701.9a discard trigger fired, so the cards were DISCARDED rather than moved"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Food Token")) S.bob after)
      1

-- CR 401.4's arrangement of two or more simultaneous library arrivals, taken back
-- from the owner by text that states a RANDOM order -- the placement angel above
-- has no reason to state, its destination being exile.
--
-- Endurance {1}{G}{G} Creature -- Elemental Incarnation 3/4, "Flash / Reach /
-- When this creature enters, up to one target player puts all the cards from
-- their graveyard on the bottom of their library in a random order. / Evoke--
-- Exile a green card from your hand." (name, cost, type line, power, toughness
-- and Oracle text checked against api.scryfall.com, 2026-08-20).
--
-- EVOKE IS NOT TRANSCRIBED: pawl has no such keyword, so the card loses an
-- alternative cost and pawl's Endurance is STRICTER than printed -- one fewer way
-- to cast it, never a cast the printing would refuse. CR 702.74's row is in #877.
--
-- THE RANDOMNESS IS THE ANSWERER'S, which is what makes this observable at all:
-- the engine rolls nothing, it asks Prompt.Shuffle, so a fixture that names a
-- permutation names the resulting library. The answer below is built from the
-- object ids rather than from the batch's own order, so it is neither the batch
-- nor its reverse under any sweep order -- an engine that ignored the answer, and
-- one that asked CR 401.4's owner instead, each leave a DIFFERENT library.
--
-- THREE SEATS: alice controls the Endurance and targets bob, and carol's
-- graveyard is stocked too, so "the targeted player's graveyard" is told apart
-- from "yours", "each player's" and "your opponents'" (CR 102.3 read through CR
-- 806.1's free-for-all makes carol an opponent as much as bob). bob's library is
-- stocked with one card the trigger never touches, so the three arrivals are read
-- as the BOTTOM of a library rather than as the whole of one.
enduranceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
enduranceSpec s registry = Spec.describe s "Endurance" $ do
  Spec.it s "CR 401.4 a stated random order puts the batch on the bottom in the order the randomness named" $ do
    endurance <- S.printingOf s registry "Endurance"
    sentry <- S.printingOf s registry "Ogre Sentry"
    maiden <- S.printingOf s registry "Bird Maiden"
    judgment <- S.printingOf s registry "Day of Judgment"
    murder <- S.printingOf s registry "Murder"
    piker <- S.printingOf s registry "Goblin Piker"
    forest <- S.printingOf s registry "Forest"
    let stocked = snd (S.addLibraryCard sentry S.bob S.threePlayerGame)
        (maidenId, g1) = S.addGraveyardCard maiden S.bob stocked
        (judgmentId, g2) = S.addGraveyardCard judgment S.bob g1
        (murderId, g3) = S.addGraveyardCard murder S.bob g2
        elsewhere =
          List.foldl'
            (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g))
            g3
            [(piker, S.alice), (forest, S.carol)]
        (_, entered) = S.entersWithTrigger endurance S.alice elsewhere
        -- The arrangement, from the chosen end INWARD: the Day of Judgment ends
        -- up deepest and the Bird Maiden nearest the top of the three.
        ordering :: Prompt.Prompt r -> r
        ordering p = case p of
          Prompt.ChooseTargets _ _ _ slots -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) slots
          Prompt.Shuffle _ -> [judgmentId, murderId, maidenId]
          _ -> S.identityAnswer p
        placed = S.runPure ordering entered Engine.placePendingTriggers
        after = S.runPure ordering placed Stack.resolveTop
        named = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith
      s
      "bob's library, top first, is the card that was already there and then the three arrivals in the named order"
      (namesIn Zone.Library S.bob after)
      [named "Ogre Sentry", named "Bird Maiden", named "Murder", named "Day of Judgment"]
    Spec.assertEqWith
      s
      "bob's graveyard is empty and the other two seats keep theirs"
      ( namesIn Zone.Graveyard S.bob after,
        namesIn Zone.Graveyard S.alice after,
        namesIn Zone.Graveyard S.carol after
      )
      ([], [named "Goblin Piker"], [named "Forest"])
    Spec.assertEqWith s "and nothing arrived in alice's or carol's library" (namesIn Zone.Library S.alice after, namesIn Zone.Library S.carol after) ([], [])

-- CR 608.2d's choice made WHILE APPLYING an effect, over a graveyard:
-- ObjectRef.ChosenCardInGraveyard, where riseOfTheDarkRealmsSpec above is the
-- same zone swept as a set.
--
-- Port of Karfell -- Land, "This land enters tapped. {T}: Add {U}. {3}{U}{B}{B},
-- {T}, Sacrifice this land: Mill four cards, then return a creature card from
-- your graveyard to the battlefield tapped." (name, type line and Oracle text
-- checked against api.scryfall.com). The whole card is transcribed.
--
-- NOT A TARGET, which is the distinction the arm exists for: the card never says
-- the word, so CR 115.1c leaves the ability untargeted, nothing is announced as
-- it goes on the stack (CR 601.2c) and nothing is re-checked at resolution (CR
-- 608.2b). A graveyard being a public zone (CR 400.2) is what would ALLOW such a
-- card to target -- it is not what makes this one choose.
--
-- THREE SEATS, and a board built so that five readings of "a creature card from
-- your graveyard" are told apart:
--
--   * THE CHOSEN card versus the FIRST matching one. alice buries two creature
--     cards; the answer is pinned to the second, and Replay.defaultAnswer -- what
--     S.identityAnswer falls through to -- picks the first. The two legs below
--     differ in the answerer and in nothing else, so an engine that picked for
--     the player would give the same card twice.
--   * A CREATURE CARD versus the whole zone. alice buries a Murder as well, and
--     the four Swamps her own mill puts there are candidates for no reading.
--   * YOUR graveyard versus each player's, and versus an opponent's. bob and
--     carol each bury a creature card of a printing alice does not have, and
--     both must stay buried.
--   * A GRAVEYARD versus the battlefield. carol controls a Benalish Hero, which
--     a battlefield reading of the same sentence could hand to alice.
--   * A CHOICE versus a sweep. Exactly one card comes back, though two match.
--
-- Ten lands rather than the six the ability costs: the payment taps sources one
-- prompt at a time, and a board with no slack could fail to cover {U}{B}{B} for
-- reasons that have nothing to do with what is under test.
portOfKarfellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
portOfKarfellSpec s registry =
  let -- alice controls five Swamps, five Islands and one untapped Port of
      -- Karfell; `buried` goes into the named graveyards in the order given, and
      -- `stock` into alice's library. Returns the Port's id.
      board port swamp island hero buried stock =
        let mana = S.landsFor island S.alice 5 (S.landsFor swamp S.alice 5 S.threePlayerGame)
            (_, withHero) = S.addCreature hero S.carol mana
            (portId, withPort) = S.addCreature port S.alice withHero
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) withPort buried
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) withGraves stock
         in (portId, withStock {GameState.priority = Just S.alice})
      -- The ability that mills and returns, told from the mana ability by the
      -- sacrifice its cost carries -- never by position in the list, which no
      -- rule fixes.
      returnAbility portId gs =
        filter
          (elem CostComponent.SacrificeThis . Cost.Type.components . ActivatedAbility.cost)
          (Activate.abilitiesFor portId gs)
      -- Activate the ability and resolve it, keeping the RESPONSES beside the
      -- board so the same call answers both "what happened" and "who was asked".
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> Maybe (GameState.GameState, [Response.Response])
      run answer portId gs = case returnAbility portId gs of
        [ability] ->
          let ((_, after), responses) = Replay.record answer gs (Activate.activateAbility S.alice portId ability >> Stack.resolveTop)
           in Just (after, responses)
        _ -> Nothing
      named = Just . CardName.MkCardName . Text.pack
      -- The WHOLE battlefield minus the basic lands: after the ability resolves
      -- that is carol's Benalish Hero, which nothing may move, and whatever came
      -- back -- the Port sacrificed itself to pay for the ability. By NAME, TAP
      -- STATE and CONTROLLER, because CR 400.7 mints a fresh id at the
      -- destination and CR 110.2a is what decides whose the arrival is.
      arrivals gs =
        List.sort
          [ (fmap S.nameOf (Game.cardOf oid gs), fmap Object.tapped (Game.lookupObject oid gs), Projection.controllerOf oid gs)
          | oid <- Set.toList (GameState.battlefield gs),
            notElem (fmap S.nameOf (Game.cardOf oid gs)) [named "Swamp", named "Island"]
          ]
      -- The board with nothing returned: carol's creature and nothing else.
      untouched = [(named "Benalish Hero", Just TapState.Untapped, Just S.carol)]
      wasAsked responses =
        let isChoice r = case r of
              Response.ChoseCardInGraveyard _ -> True
              _ -> False
         in any isChoice responses
   in Spec.describe s "PortOfKarfell" $ do
        -- The headline: the SECOND buried creature card comes back, tapped, and
        -- everything else stays where it was.
        Spec.it s "CR 608.2d the creature card the controller chose returns tapped" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (murder, S.alice), (maiden, S.alice), (sentry, S.bob), (cavalry, S.carol)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
              -- The Bird Maiden's id, which is the SECOND of alice's two
              -- creature cards in ascending order -- graveyardCards' own order,
              -- and the order the prompt offers.
              maidenId = case Game.zoneMembers Zone.Graveyard S.alice gs of
                [_, _, third] -> Just third
                _ -> Nothing
              choosing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
              choosing wanted p = case p of
                Prompt.ChooseCardInGraveyard {} -> wanted
                _ -> S.identityAnswer p
          case (maidenId, maidenId >>= \wanted -> run (choosing wanted) portId gs) of
            (Just _, Just (after, responses)) -> do
              Spec.assertBool s (wasAsked responses) "the controller was asked which card to return"
              Spec.assertEqWith
                s
                "the Bird Maiden is on alice's battlefield, tapped, and nothing else arrived"
                (arrivals after)
                (List.sort ((named "Bird Maiden", Just TapState.Tapped, Just S.alice) : untouched))
              Spec.assertEqWith
                s
                "the unchosen creature card, the noncreature card, the four milled Swamps and the spent land stay in alice's graveyard"
                (List.sort (namesIn Zone.Graveyard S.alice after))
                (List.sort ([named "Goblin Piker", named "Murder", named "Port of Karfell"] <> replicate 4 (named "Swamp")))
              Spec.assertEqWith
                s
                "and neither opponent's graveyard was touched"
                (namesIn Zone.Graveyard S.bob after, namesIn Zone.Graveyard S.carol after)
                ([named "Ogre Sentry"], [named "Benalish Cavalry"])
            _ -> Spec.assertBool s False "expected exactly one returning ability and three cards in alice's graveyard"
        -- The paired control, and the whole reason the board buries TWO creature
        -- cards: the same activation on the same board with the DEFAULT answerer
        -- brings back the other one. If the engine were picking, both legs would
        -- name the same card.
        Spec.it s "CR 608.2d the engine does not pick: another answer returns the other card" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (murder, S.alice), (maiden, S.alice)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
          case run S.identityAnswer portId gs of
            Just (after, _) ->
              Spec.assertEqWith
                s
                "the first candidate comes back instead"
                (arrivals after)
                (List.sort ((named "Goblin Piker", Just TapState.Tapped, Just S.alice) : untouched))
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"
        -- Where the rules leave nothing to ask, don't prompt: one matching card
        -- is the whole of "a creature card in your graveyard". The board differs
        -- from the leg above in the Bird Maiden and nothing else.
        Spec.it s "one candidate elides the prompt and still returns the card" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (murder, S.alice)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
          case run S.identityAnswer portId gs of
            Just (after, responses) -> do
              Spec.assertBool s (not (wasAsked responses)) "no choice was put to the player"
              Spec.assertEqWith
                s
                "the lone candidate came back anyway"
                (arrivals after)
                (List.sort ((named "Goblin Piker", Just TapState.Tapped, Just S.alice) : untouched))
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"
        -- CR 101.3 and CR 609.3: a graveyard with nothing matching makes the
        -- instruction impossible, so it is ignored -- and nobody is asked. The
        -- mill still happens, which is what keeps this from passing because the
        -- ability never resolved at all.
        Spec.it s "CR 101.3 no matching card returns nothing and asks nothing" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(murder, S.alice), (sentry, S.bob)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
          case run S.identityAnswer portId gs of
            Just (after, responses) -> do
              Spec.assertBool s (not (wasAsked responses)) "no choice was put to the player"
              Spec.assertEqWith s "nothing arrived, and carol keeps the creature she controls" (arrivals after) untouched
              Spec.assertEqWith
                s
                "the mill still ran, so the ability really did resolve"
                (List.sort (namesIn Zone.Graveyard S.alice after))
                (List.sort ([named "Murder", named "Port of Karfell"] <> replicate 4 (named "Swamp")))
              Spec.assertEqWith s "and the opponent's creature card is not a candidate" (namesIn Zone.Graveyard S.bob after) [named "Ogre Sentry"]
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"
        -- An EMPTY graveyard and an empty library: the ability resolves, mills
        -- nothing (CR 701.17b), and returns nothing.
        Spec.it s "CR 609.3 an empty graveyard is a no-op rather than a failure" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          hero <- S.printingOf s registry "Benalish Hero"
          let (portId, gs) = board port swamp island hero [] []
          case run S.identityAnswer portId gs of
            Just (after, responses) -> do
              Spec.assertBool s (not (wasAsked responses)) "no choice was put to the player"
              Spec.assertEqWith s "nothing arrived, and carol keeps the creature she controls" (arrivals after) untouched
              Spec.assertEqWith s "only the land that paid for the ability is in the graveyard" (namesIn Zone.Graveyard S.alice after) [named "Port of Karfell"]
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"

-- CR 400.1's WHOSE said by a SLOT for a resolution-time CHOICE: the graveyard
-- the candidates come from is the one this spell's own target names, where
-- portOfKarfellSpec above says "your graveyard" and exhumeSpec below says "each
-- player's".
--
-- Grasping Tentacles {1}{U}{B} Sorcery, "Target opponent mills eight cards. You
-- may put an artifact card from that player's graveyard onto the battlefield
-- under your control." (name, cost, type line and Oracle text checked against
-- api.scryfall.com, 2026-09-01). The whole card is transcribed: "under your
-- control" is CR 110.2a's default for a card the effect's controller puts onto
-- the battlefield, so it states no rider, and the "may" is a CR 608.2d choice
-- scoped to the second clause.
--
-- THE CHOOSER IS NOT THE SCOPE, which is the pair no arm could state before:
-- alice chooses (CR 608.2d) out of a graveyard that is not hers, and the seat
-- whose graveyard it is comes from the slot the FIRST clause targeted.
--
-- NOT A TARGET, portOfKarfellSpec's distinction: the card says "target" of the
-- opponent alone, so CR 115.1a leaves the artifact card unannounced and CR
-- 608.2b has nothing to re-check about it.
--
-- THREE SEATS, with an artifact card in every graveyard a wider reading would
-- reach: alice's own (which "your graveyard" would take), carol's (which "each
-- opponent's" or "each player's" would take) and bob's two. The two legs below
-- pin their answer to the LAST and the FIRST candidate offered, and
-- Resolve.zoneScopePlayers offers them in APNAP order (CR 101.4), so a scope
-- wider than the slot hands back carol's card on one leg and alice's on the
-- other rather than bob's on both.
--
-- Bob's eight milled Swamps are the filter's other half -- cards in the very
-- graveyard the choice reads that "artifact card" must leave standing -- and the
-- witness that both clauses read the SAME slot.
graspingTentaclesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graspingTentaclesSpec s registry =
  let -- alice holds the spell and has six lands for its {1}{U}{B}, a payment
      -- that taps one source at a time having no slack of its own; `buried` goes
      -- into the named graveyards in the order given, and bob's library is
      -- stocked with `stock`. Returns the spell's id.
      board tentacles island swamp buried stock =
        let mana = S.landsFor island S.alice 3 (S.landsFor swamp S.alice 3 S.threePlayerGame)
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) mana buried
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) withGraves stock
            (withSpell, spell) = S.handOne tentacles withStock
         in (spell, withSpell {GameState.priority = Just S.alice})
      -- Cast and resolve, keeping the RESPONSES beside the board so the same call
      -- answers both "what came back" and "was anybody asked".
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [Response.Response])
      run answer spell gs =
        let ((_, after), responses) = Replay.record answer gs (S.cast S.alice spell >> Stack.resolveTop)
         in (after, responses)
      -- The offered set is FILTERED rather than rebuilt, so the target is a
      -- recipient the engine itself minted (CR 608.2b).
      answering :: (NonEmpty.NonEmpty ObjectId.ObjectId -> ObjectId.ObjectId) -> Prompt.Prompt r -> r
      answering pick p = case p of
        Prompt.ChooseTargets _ _ _ slots -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) slots
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseCardInGraveyard _ _ _ offered -> pick offered
        _ -> S.identityAnswer p
      named = Just . CardName.MkCardName . Text.pack
      -- The whole battlefield minus alice's six lands, by NAME and CONTROLLER:
      -- CR 400.7 mints a fresh id at the destination, and CR 110.2a is what
      -- decides whose the arrival is.
      arrivals gs =
        List.sort
          [ (fmap S.nameOf (Game.cardOf oid gs), Projection.controllerOf oid gs)
          | oid <- Set.toList (GameState.battlefield gs),
            notElem (fmap S.nameOf (Game.cardOf oid gs)) [named "Island", named "Swamp"]
          ]
      wasAsked responses =
        let isChoice r = case r of
              Response.ChoseCardInGraveyard _ -> True
              _ -> False
         in any isChoice responses
   in Spec.describe s "GraspingTentacles" $ do
        -- The headline: the LAST candidate is bob's second artifact card, not
        -- carol's, so the scope is the slot rather than the table.
        Spec.it s "CR 608.2d the candidates come from the graveyard the target slot names" $ do
          tentacles <- S.printingOf s registry "Grasping Tentacles"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          medallion <- S.printingOf s registry "Sapphire Medallion"
          meekstone <- S.printingOf s registry "Meekstone"
          heartstone <- S.printingOf s registry "Heartstone"
          crucible <- S.printingOf s registry "Crucible of Worlds"
          let buried = [(medallion, S.alice), (meekstone, S.bob), (heartstone, S.bob), (crucible, S.carol)]
              (spell, gs) = board tentacles island swamp buried (replicate 8 swamp)
              (after, responses) = run (answering NonEmpty.last) spell gs
          Spec.assertEqWith
            s
            "bob's last artifact card is on alice's battlefield: carol's, which a wider scope would have offered last, is not"
            (arrivals after)
            [(named "Heartstone", Just S.alice)]
          Spec.assertEqWith
            s
            "alice's and carol's own artifact cards were never candidates and are still buried"
            (List.sort (namesIn Zone.Graveyard S.alice after), namesIn Zone.Graveyard S.carol after)
            (List.sort [named "Grasping Tentacles", named "Sapphire Medallion"], [named "Crucible of Worlds"])
          Spec.assertEqWith
            s
            "bob's unchosen artifact and his eight milled Swamps stay in his graveyard, and his library is empty"
            (List.sort (namesIn Zone.Graveyard S.bob after), namesIn Zone.Library S.bob after)
            (List.sort (named "Meekstone" : replicate 8 (named "Swamp")), [])
          Spec.assertBool s (wasAsked responses) "alice was asked which card to take"
        -- The paired control, and the whole reason bob buries TWO artifact cards:
        -- the same cast on the same board answered with the FIRST candidate takes
        -- the other one. A wider scope would offer alice's own Medallion first,
        -- so this leg fails on the same reading the one above does -- and if the
        -- engine were picking, both legs would name one card.
        Spec.it s "CR 608.2d the engine does not pick: another answer takes bob's other artifact card" $ do
          tentacles <- S.printingOf s registry "Grasping Tentacles"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          medallion <- S.printingOf s registry "Sapphire Medallion"
          meekstone <- S.printingOf s registry "Meekstone"
          heartstone <- S.printingOf s registry "Heartstone"
          crucible <- S.printingOf s registry "Crucible of Worlds"
          let buried = [(medallion, S.alice), (meekstone, S.bob), (heartstone, S.bob), (crucible, S.carol)]
              (spell, gs) = board tentacles island swamp buried (replicate 8 swamp)
              (after, _) = run (answering NonEmpty.head) spell gs
          Spec.assertEqWith
            s
            "bob's first artifact card comes instead, where alice's own Medallion is what a wider scope would have offered first"
            (arrivals after)
            [(named "Meekstone", Just S.alice)]
        -- The "may", declined: the mill is a clause of its own and stands.
        Spec.it s "CR 608.2d a declined may leaves the mill done and nothing taken" $ do
          tentacles <- S.printingOf s registry "Grasping Tentacles"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          meekstone <- S.printingOf s registry "Meekstone"
          heartstone <- S.printingOf s registry "Heartstone"
          let buried = [(meekstone, S.bob), (heartstone, S.bob)]
              (spell, gs) = board tentacles island swamp buried (replicate 8 swamp)
              declining p = case p of
                Prompt.ChooseTargets _ _ _ slots -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) slots
                _ -> S.identityAnswer p
              (after, responses) = run declining spell gs
          Spec.assertEqWith s "nothing arrived on any battlefield" (arrivals after) []
          Spec.assertEqWith
            s
            "and bob's eight milled Swamps joined the two artifact cards he had buried"
            (List.sort (namesIn Zone.Graveyard S.bob after))
            (List.sort ([named "Heartstone", named "Meekstone"] <> replicate 8 (named "Swamp")))
          Spec.assertBool s (not (wasAsked responses)) "the declined may asked nothing about which card"

-- CR 701.17c's "from among them", which is the group read Filter.IsBound could
-- not do: a slot bound to the WHOLE batch a mill put in the graveyard, named by a
-- later clause's filter over candidates that batch does not exhaust.
--
-- Midnight Tilling {1}{G} Instant, "Mill four cards, then you may return a
-- permanent card from among them to your hand." (name, cost, type line and
-- Oracle text checked against api.scryfall.com, 2026-08-20). The whole card is
-- transcribed; "a permanent card" is CR 110.4a's six card types written out as an
-- Or, there being no atom that says it in one word.
--
-- Corpse Churn is the printing the pool already had that separates the two
-- readings -- "Mill three cards, then you may return a creature card FROM YOUR
-- GRAVEYARD to your hand", the same sentence with the batch swapped for the
-- zone. So the board buries a permanent card BEFORE the mill: every other clause
-- of Midnight Tilling's sentence admits it, and only "from among them" keeps it
-- out. A reading that ignored the slot would offer four candidates where this one
-- offers three, and the answers below are pinned by INDEX into the offer, so the
-- two readings hand back different cards rather than the same one.
--
-- The milled Murder is the type half of the same filter, kept honest by a batch
-- that is not all permanent cards; bob's buried Ogre Sentry is CR 400.1's other
-- graveyard, which "your" excludes.
midnightTillingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
midnightTillingSpec s registry =
  let -- alice: four Forests for the {1}{G}, one permanent card already in her
      -- graveyard, `stock` into her library BOTTOM FIRST (S.addLibraryCard puts
      -- each new card on top), Midnight Tilling in hand. bob buries one card of
      -- his own. Returns the spell's id.
      board forest tilling decoy sentry stock =
        let mana = S.landsFor forest S.alice 4 S.threePlayerGame
            (_, withDecoy) = S.addGraveyardCard decoy S.alice mana
            (_, withTheirs) = S.addGraveyardCard sentry S.bob withDecoy
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) withTheirs stock
            (withSpell, spellId) = S.handOne tilling withStock
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      -- The candidates in the order the prompt offers them, which is
      -- Resolve.graveyardCardsOf's ascending ObjectId -- and, the mill having
      -- minted fresh ids in milling order (CR 400.7), the order the cards were
      -- milled in. Pinned by index: an answerer that went looking for a legal
      -- card would find one again under either reading.
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- Takes the printed "may" -- clause 1, the return; clause 0 is the mandatory
      -- mill -- and answers the graveyard choice with the nth card offered.
      taking :: Int -> Prompt.Prompt r -> r
      taking n p = case p of
        Prompt.ChooseOptional _ _ _ _ clause
          | clause == ClauseIndex.MkClauseIndex 1 -> OptionalDecision.Exercises
        Prompt.ChooseCardInGraveyard _ _ _ offered -> nth n offered
        _ -> S.identityAnswer p
      cast :: (forall r. Prompt.Prompt r -> r) -> (ObjectId.ObjectId, GameState.GameState) -> GameState.GameState
      cast answer (spellId, gs) =
        let announced = S.runPure answer gs (S.cast S.alice spellId)
         in S.runPure answer announced Stack.resolveTop
      setup = do
        forest <- S.printingOf s registry "Forest"
        tilling <- S.printingOf s registry "Midnight Tilling"
        hero <- S.printingOf s registry "Benalish Hero"
        sentry <- S.printingOf s registry "Ogre Sentry"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        maiden <- S.printingOf s registry "Bird Maiden"
        murder <- S.printingOf s registry "Murder"
        piker <- S.printingOf s registry "Goblin Piker"
        -- Bottom to top: the Island is never reached, and the top four are milled
        -- in the order Goblin Piker, Murder, Bird Maiden, Swamp.
        pure (board forest tilling hero sentry [island, swamp, maiden, murder, piker])
      -- What stays behind when nothing is returned: the card buried before the
      -- mill, all four milled cards, and the spell itself (CR 608.2n).
      allBuried = List.sort ([named "Benalish Hero", named "Bird Maiden", named "Goblin Piker", named "Midnight Tilling", named "Murder"] <> [named "Swamp"])
   in Spec.describe s "MidnightTilling" $ do
        -- The headline, and the case the whole unit exists for: the SECOND card
        -- the offer names is the second MILLED permanent card, not the second
        -- permanent card in the graveyard.
        Spec.it s "CR 701.17c the return chooses among the milled cards, not among the graveyard" $ do
          gs <- setup
          let after = cast (taking 1) gs
          Spec.assertEqWith s "the second milled permanent card is the one in alice's hand" (namesIn Zone.Hand S.alice after) [named "Bird Maiden"]
          Spec.assertEqWith
            s
            "the permanent card buried before the mill was never a candidate, and neither was the milled Murder"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.delete (named "Bird Maiden") allBuried)
          Spec.assertEqWith s "and the other graveyard was not looked in" (namesIn Zone.Graveyard S.bob after) [named "Ogre Sentry"]
        -- The paired control: the same board and the same offer, answered at
        -- index 0 instead. If the engine were picking, both legs would name one
        -- card.
        Spec.it s "CR 608.2d the engine does not pick: another answer returns another milled card" $ do
          gs <- setup
          let after = cast (taking 0) gs
          Spec.assertEqWith s "the first milled permanent card comes back instead" (namesIn Zone.Hand S.alice after) [named "Goblin Piker"]
          Spec.assertEqWith
            s
            "and the Bird Maiden stays milled"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.delete (named "Goblin Piker") allBuried)
        -- CR 603.5: the printed "may" is a real choice, and declining leaves the
        -- whole batch where the mill put it. The mill still ran, so this cannot
        -- pass because the spell never resolved.
        Spec.it s "CR 603.5 declining the may leaves every milled card in the graveyard" $ do
          gs <- setup
          let after = cast S.identityAnswer gs
          Spec.assertEqWith s "nothing reached alice's hand" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith s "and the mill still happened" (List.sort (namesIn Zone.Graveyard S.alice after)) allBuried

-- CR 701.20e's "from among them" over a group that never left the LIBRARY, which
-- is the read no zone-keyed ObjectRef can do: ObjectRef.ChosenCardFromAmong.
--
-- Commune with the Gods {1}{G} Sorcery, "Reveal the top five cards of your
-- library. You may put a creature or enchantment card from among them into your
-- hand. Put the rest into your graveyard." (name, cost, type line and Oracle text
-- checked against api.scryfall.com, 2026-08-20). The whole card is transcribed.
--
-- Three clauses, and the middle one is this unit: the reveal binds the five as a
-- group and leaves them where they are (CR 701.20b), the choice picks one of them
-- by the card's own filter, and "the rest" is the SAME slot read by
-- ObjectRef.InSlot -- which finds the chosen card gone, CR 400.7 having minted a
-- new object for it on the way to the hand.
--
-- The library is stocked so that the offer and the group differ: an Island and a
-- Murder sit among the five and match neither card type, so a reading that
-- ignored the filter would offer five cards where this one offers three. The
-- answers below are pinned by INDEX into the offer, so the two readings hand back
-- different cards rather than the same one. A Swamp sits SIXTH, below the five, so
-- a reveal of the wrong depth is visible in the library as well as in the
-- graveyard.
communeWithTheGodsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
communeWithTheGodsSpec s registry =
  let -- alice: two Forests for the {1}{G}, `stock` into her library BOTTOM FIRST
      -- (S.addLibraryCard puts each new card on top), Commune with the Gods in
      -- hand. Returns the spell's id.
      board forest commune stock =
        let mana = S.landsFor forest S.alice 2 S.threePlayerGame
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) mana stock
            (withSpell, spellId) = S.handOne commune withStock
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      -- The candidates in the order the prompt offers them: the group's own mint
      -- order, which for a reveal of the top five is the library's, top first (CR
      -- 401.2). Pinned by index, since an answerer that went looking for a legal
      -- card would find one again under either reading.
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- Takes the printed "may" -- clause 1, the move to hand; clause 0 is the
      -- reveal and clause 2 the rest -- and answers the group choice with the nth
      -- card offered.
      taking :: Int -> Prompt.Prompt r -> r
      taking n p = case p of
        Prompt.ChooseOptional _ _ _ _ clause
          | clause == ClauseIndex.MkClauseIndex 1 -> OptionalDecision.Exercises
        Prompt.ChooseCardFromAmong _ _ _ offered -> nth n offered
        _ -> S.identityAnswer p
      cast :: (forall r. Prompt.Prompt r -> r) -> (ObjectId.ObjectId, GameState.GameState) -> GameState.GameState
      cast answer (spellId, gs) =
        let announced = S.runPure answer gs (S.cast S.alice spellId)
         in S.runPure answer announced Stack.resolveTop
      setup = do
        forest <- S.printingOf s registry "Forest"
        commune <- S.printingOf s registry "Commune with the Gods"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        maiden <- S.printingOf s registry "Bird Maiden"
        moon <- S.printingOf s registry "Bad Moon"
        murder <- S.printingOf s registry "Murder"
        piker <- S.printingOf s registry "Goblin Piker"
        -- Bottom to top: the Swamp is never revealed, and the five above it are
        -- Island, Goblin Piker, Murder, Bad Moon, Bird Maiden -- so the offer is
        -- Goblin Piker, Bad Moon, Bird Maiden.
        pure (board forest commune [swamp, maiden, moon, murder, piker, island])
      -- What the graveyard holds when nothing is taken: all five revealed cards
      -- and the spell itself (CR 608.2n).
      allBuried = List.sort [named "Bad Moon", named "Bird Maiden", named "Commune with the Gods", named "Goblin Piker", named "Island", named "Murder"]
   in Spec.describe s "CommuneWithTheGods" $ do
        -- The headline: the SECOND card the offer names is the second revealed
        -- card matching the filter, not the second revealed card.
        Spec.it s "CR 701.20e the choice ranges over the matching revealed cards, not over all five" $ do
          gs <- setup
          let after = cast (taking 1) gs
          Spec.assertEqWith s "the second matching revealed card is the one in alice's hand" (namesIn Zone.Hand S.alice after) [named "Bad Moon"]
          Spec.assertEqWith
            s
            "and the rest -- the two unmatched cards included -- are in the graveyard"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.delete (named "Bad Moon") allBuried)
          Spec.assertEqWith s "the sixth card was never revealed" (namesIn Zone.Library S.alice after) [named "Swamp"]
        -- The paired control: the same board and the same offer, answered at
        -- index 0 instead. If the engine were picking, both legs would name one
        -- card.
        Spec.it s "CR 608.2d the engine does not pick: another answer takes another revealed card" $ do
          gs <- setup
          let after = cast (taking 0) gs
          Spec.assertEqWith s "the first matching revealed card comes to hand instead" (namesIn Zone.Hand S.alice after) [named "Goblin Piker"]
          Spec.assertEqWith
            s
            "and the Bad Moon is among the rest"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.delete (named "Goblin Piker") allBuried)
        -- CR 603.5: the printed "may" is a real choice, and declining sends the
        -- whole group to the graveyard. The reveal still ran, so this cannot pass
        -- because the spell never resolved.
        Spec.it s "CR 603.5 declining the may buries all five revealed cards" $ do
          gs <- setup
          let after = cast S.identityAnswer gs
          Spec.assertEqWith s "nothing reached alice's hand" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith s "and every revealed card is in the graveyard" (List.sort (namesIn Zone.Graveyard S.alice after)) allBuried
          Spec.assertEqWith s "the sixth card is still the library" (namesIn Zone.Library S.alice after) [named "Swamp"]
        -- CR 609.3 and CR 101.3: a group holding no matching card offers nothing,
        -- so the taken half is skipped and the rest is all of it. The pair with
        -- the case above differs in exactly one thing -- which cards are stocked.
        Spec.it s "CR 609.3 a group with no matching card takes nothing and buries all five" $ do
          forest <- S.printingOf s registry "Forest"
          commune <- S.printingOf s registry "Commune with the Gods"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          murder <- S.printingOf s registry "Murder"
          let gs = board forest commune [swamp, murder, island, murder, island, murder]
              after = cast (taking 0) gs
          Spec.assertEqWith s "nothing reached alice's hand" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith
            s
            "and all five revealed cards are in the graveyard"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.sort [named "Commune with the Gods", named "Island", named "Island", named "Murder", named "Murder", named "Murder"])
        -- A reveal that names exactly ONE card binds the SINGULAR shape rather
        -- than a group, and "from among them" has to see it: the offer is elided
        -- at one candidate (CR 101.3), so taking the printed "may" puts that card
        -- in hand and leaves the rest empty. A read that saw only the group shape
        -- would offer nothing and bury the card instead -- which is what this
        -- fixture did before fromAmongMembers gave the three readers of a slot
        -- one definition.
        Spec.it s "CR 608.2d a one-card library still offers its card from among them" $ do
          forest <- S.printingOf s registry "Forest"
          commune <- S.printingOf s registry "Commune with the Gods"
          maiden <- S.printingOf s registry "Bird Maiden"
          let after = cast (taking 0) (board forest commune [maiden])
          Spec.assertEqWith s "the one revealed card came to alice's hand" (namesIn Zone.Hand S.alice after) [named "Bird Maiden"]
          Spec.assertEqWith s "and only the spell is in her graveyard" (namesIn Zone.Graveyard S.alice after) [named "Commune with the Gods"]
          Spec.assertEqWith s "her library is empty" (namesIn Zone.Library S.alice after) []

-- ObjectRef.ChosenCardFromAmong's COUNT: "from among them" taking more than one
-- card out of one bound group, where communeWithTheGodsSpec above takes one.
--
-- Ancestral Memories {2}{U}{U}{U} Sorcery, "Look at the top seven cards of your
-- library. Put two of them into your hand and the rest into your graveyard."
-- (name, cost, type line and Oracle text checked against api.scryfall.com,
-- 2026-09-01). The whole card is transcribed.
--
-- Three clauses: CR 701.20e's look binds the seven as a group and leaves them in
-- the library (rule 701.20b), the choice takes two of them, and "the rest" is the
-- SAME slot read by ObjectRef.InSlot -- which finds both chosen cards gone, CR
-- 400.7 having minted new objects for them on the way to the hand.
--
-- The two asks are pinned by INDEX through a State-threaded answerer, since a
-- pure one cannot tell the second ask from the first. Both legs below index 5 and
-- 0 of the offers, in the two orders, and the second leg is what proves the
-- EXCLUSION: index 5 of the untouched seven is the Bad Moon, and index 5 of the
-- six the first ask left is the Bird Maiden, so an implementation that re-offered
-- the taken card would name a different pair. An eighth card sits below the seven
-- so the look's own depth stays observable.
ancestralMemoriesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ancestralMemoriesSpec s registry =
  let -- alice: five Islands for the {2}{U}{U}{U}, `stock` into her library BOTTOM
      -- FIRST (S.addLibraryCard puts each new card on top), Ancestral Memories in
      -- hand. Returns the spell's id.
      board island memories stock =
        let mana = S.landsFor island S.alice 5 S.threePlayerGame
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) mana stock
            (withSpell, spellId) = S.handOne memories withStock
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- One index per ask, taken in order, and the SIZE of each offer recorded
      -- beside it -- so a second ask that never happened and a second ask over the
      -- wrong candidates are both visible.
      taking :: Prompt.Prompt r -> State.State ([Int], [Int]) r
      taking p = case p of
        Prompt.ChooseCardFromAmong _ _ _ offered -> do
          (indices, sizes) <- State.get
          State.put (drop 1 indices, sizes <> [length (NonEmpty.toList offered)])
          pure (nth (Maybe.fromMaybe 0 (Maybe.listToMaybe indices)) offered)
        _ -> pure (S.identityAnswer p)
      cast :: [Int] -> (ObjectId.ObjectId, GameState.GameState) -> (GameState.GameState, [Int])
      cast script (spellId, gs) =
        let ((_, announced), afterCast) = State.runState (Engine.runGame taking gs (S.cast S.alice spellId)) (script, [])
            ((_, after), afterResolve) = State.runState (Engine.runGame taking announced Stack.resolveTop) afterCast
         in (after, snd afterResolve)
      setup = do
        island <- S.printingOf s registry "Island"
        memories <- S.printingOf s registry "Ancestral Memories"
        swamp <- S.printingOf s registry "Swamp"
        maiden <- S.printingOf s registry "Bird Maiden"
        moon <- S.printingOf s registry "Bad Moon"
        murder <- S.printingOf s registry "Murder"
        piker <- S.printingOf s registry "Goblin Piker"
        giant <- S.printingOf s registry "Hill Giant"
        forest <- S.printingOf s registry "Forest"
        mountain <- S.printingOf s registry "Mountain"
        -- Bottom to top: the Swamp is never looked at, and the seven above it are
        -- Mountain, Forest, Hill Giant, Goblin Piker, Murder, Bad Moon, Bird
        -- Maiden read top down, which is the order the offer takes.
        pure (board island memories [swamp, maiden, moon, murder, piker, giant, forest, mountain])
      -- The graveyard when nothing is taken: the seven looked-at cards and the
      -- spell itself (CR 608.2n).
      allBuried =
        List.sort
          [ named "Ancestral Memories",
            named "Bad Moon",
            named "Bird Maiden",
            named "Forest",
            named "Goblin Piker",
            named "Hill Giant",
            named "Mountain",
            named "Murder"
          ]
      burying takens = List.sort (List.foldl' (flip List.delete) allBuried takens)
   in Spec.describe s "AncestralMemories" $ do
        -- The headline: TWO cards come out of the one group, and both are the ones
        -- the answers named.
        Spec.it s "CR 608.2d two cards are taken from among the seven, both of them chosen" $ do
          gs <- setup
          let (after, sizes) = cast [5, 0] gs
          Spec.assertEqWith
            s
            "both chosen cards are in alice's hand"
            (List.sort (namesIn Zone.Hand S.alice after))
            (List.sort [named "Bad Moon", named "Mountain"])
          Spec.assertEqWith
            s
            "and the other five are in the graveyard"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (burying [named "Bad Moon", named "Mountain"])
          Spec.assertEqWith s "the eighth card was never looked at" (namesIn Zone.Library S.alice after) [named "Swamp"]
          Spec.assertEqWith s "two asks, the second over one fewer candidate" sizes [7, 6]
        -- The paired control, and the proof that the second ask cannot re-offer the
        -- first ask's card: the SAME two indices in the other order. Index 5 of the
        -- untouched seven is the Bad Moon; index 5 of what the Mountain's removal
        -- leaves is the Bird Maiden.
        Spec.it s "CR 608.2d the second choice is made among the cards the first left" $ do
          gs <- setup
          let (after, _) = cast [0, 5] gs
          Spec.assertEqWith
            s
            "the Mountain and the Bird Maiden are in alice's hand"
            (List.sort (namesIn Zone.Hand S.alice after))
            (List.sort [named "Bird Maiden", named "Mountain"])
          Spec.assertEqWith
            s
            "and the Bad Moon is among the rest"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (burying [named "Bird Maiden", named "Mountain"])
        -- CR 609.3: a group holding fewer cards than the count gives what it has,
        -- and the rest of the instruction is performed on that (CR 101.3). One
        -- candidate elides the ask entirely, so no index is consumed.
        Spec.it s "CR 609.3 a one-card library gives its one card to a count of two" $ do
          island <- S.printingOf s registry "Island"
          memories <- S.printingOf s registry "Ancestral Memories"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (after, sizes) = cast [] (board island memories [maiden])
          Spec.assertEqWith s "the one looked-at card came to alice's hand" (namesIn Zone.Hand S.alice after) [named "Bird Maiden"]
          Spec.assertEqWith s "and only the spell is in her graveyard" (namesIn Zone.Graveyard S.alice after) [named "Ancestral Memories"]
          Spec.assertEqWith s "her library is empty" (namesIn Zone.Library S.alice after) []
          Spec.assertEqWith s "nobody was asked" sizes []

-- ObjectRef.ChosenCardFromAmong's CHOOSER: "from among them" answered by a seat
-- other than the resolving controller, where communeWithTheGodsSpec above is CR
-- 608.2c's default.
--
-- Animal Magnetism {4}{G} Sorcery, "Reveal the top five cards of your library. An
-- opponent chooses a creature card from among them. Put that card onto the
-- battlefield and the rest into your graveyard." (name, cost, type line and
-- Oracle text checked against api.scryfall.com, 2026-09-01). The whole card is
-- transcribed.
--
-- WHICH opponent is itself a CR 608.2d choice the controller announces, so the
-- card writes Effect.ChoosePlayer into a slot and the ref's chooser reads that
-- slot -- Skullwinder's shape, one ObjectRef over.
--
-- THREE seats, and the two legs below differ in exactly one thing: which opponent
-- alice names. The answerer replies by the seat the PROMPT names -- alice would
-- take index 0, bob takes index 1, carol takes index 2 -- so a reading that asked
-- the controller hands back the same card in both legs, and one that asked the
-- other opponent swaps them.
animalMagnetismSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
animalMagnetismSpec s registry =
  let -- alice: five Forests for the {4}{G}, `stock` into her library BOTTOM FIRST,
      -- Animal Magnetism in hand.
      board forest magnetism stock =
        let mana = S.landsFor forest S.alice 5 S.threePlayerGame
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) mana stock
            (withSpell, spellId) = S.handOne magnetism withStock
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- The index each seat would take, so the card that moves names the seat that
      -- was asked. `opponent` is alice's answer to the ChooseOpponent prompt.
      answering :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      answering opponent p = case p of
        Prompt.ChooseOpponent {} -> opponent
        Prompt.ChooseCardFromAmong _ asked _ offered
          | asked == S.alice -> nth 0 offered
          | asked == S.bob -> nth 1 offered
          | otherwise -> nth 2 offered
        _ -> S.identityAnswer p
      cast opponent (spellId, gs) =
        let announced = S.runPure (answering opponent) gs (S.cast S.alice spellId)
         in S.runPure (answering opponent) announced Stack.resolveTop
      setup = do
        forest <- S.printingOf s registry "Forest"
        magnetism <- S.printingOf s registry "Animal Magnetism"
        swamp <- S.printingOf s registry "Swamp"
        maiden <- S.printingOf s registry "Bird Maiden"
        island <- S.printingOf s registry "Island"
        giant <- S.printingOf s registry "Hill Giant"
        murder <- S.printingOf s registry "Murder"
        piker <- S.printingOf s registry "Goblin Piker"
        -- Bottom to top: the Swamp is never revealed, and the five above it are
        -- Goblin Piker, Murder, Hill Giant, Island, Bird Maiden read top down. The
        -- Murder and the Island match no creature filter, so the offer is Goblin
        -- Piker, Hill Giant, Bird Maiden -- three candidates for three seats.
        pure (board forest magnetism [swamp, maiden, island, giant, murder, piker])
      -- Alice's battlefield without the five Forests she cast the spell off.
      arrived after = filter (/= named "Forest") (namesIn Zone.Battlefield S.alice after)
      allBuried =
        List.sort
          [ named "Animal Magnetism",
            named "Bird Maiden",
            named "Goblin Piker",
            named "Hill Giant",
            named "Island",
            named "Murder"
          ]
   in Spec.describe s "AnimalMagnetism" $ do
        -- The headline: BOB's answer, not alice's, decides which creature card
        -- reaches the battlefield.
        Spec.it s "CR 608.2d the opponent alice named picks the creature card" $ do
          gs <- setup
          let after = cast S.bob gs
          Spec.assertEqWith s "bob's index-1 pick is the creature on the battlefield" (arrived after) [named "Hill Giant"]
          Spec.assertEqWith
            s
            "and the other four revealed cards are in alice's graveyard"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.delete (named "Hill Giant") allBuried)
          Spec.assertEqWith s "the sixth card was never revealed" (namesIn Zone.Library S.alice after) [named "Swamp"]
        -- The paired control: the same board with the OTHER opponent named. If the
        -- controller were answering, both legs would put the same card onto the
        -- battlefield.
        Spec.it s "CR 608.2d naming the other opponent takes the other card" $ do
          gs <- setup
          let after = cast S.carol gs
          Spec.assertEqWith s "carol's index-2 pick is the creature on the battlefield" (arrived after) [named "Bird Maiden"]
          Spec.assertEqWith
            s
            "and the Hill Giant is among the rest"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.delete (named "Bird Maiden") allBuried)

-- ObjectRef.TopOfLibraryUntil: a prefix of a library whose LENGTH is what a
-- Filter decides, where ObjectRef.TopOfLibrary's is what a Quantity counts.
--
-- Treasure Hunt {1}{U} Sorcery, "Reveal cards from the top of your library until
-- you reveal a nonland card, then put all cards revealed this way into your
-- hand." Two clauses, both of whose opcodes already existed: CR 701.20a's reveal
-- binding the walked cards as a group, and a move of that group (CR 701.20b left
-- every one of them in the library, so the move is what takes them out).
--
-- Nothing in the CR governs the word "until" -- the stopping condition is the
-- card's own text. What the walk owes the rulebook is CR 401.2's ordered pile
-- read from its head (CR 121.1) and CR 609.3's shortfall where no card matches.
--
-- The board deliberately puts SEVERAL non-matching cards above the match, which
-- is what separates "revealed until the match" from "revealed the top card" and
-- from "revealed the whole library"; the two legs below it pin the ends of the
-- walk.
treasureHuntSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
treasureHuntSpec s registry =
  let -- alice: two Islands for the {1}{U}, `stock` into her library BOTTOM FIRST
      -- (S.addLibraryCard puts each new card on top), Treasure Hunt in hand.
      -- `decoy` goes into BOB's library, which alice's "your library" must not
      -- reach -- a three-seat game, so "you" and "an opponent" cannot collapse.
      board island treasureHunt stock decoy =
        let mana = S.landsFor island S.alice 2 S.threePlayerGame
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) mana stock
            withDecoy = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) withStock decoy
            (withSpell, spellId) = S.handOne treasureHunt withDecoy
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      cast (spellId, gs) =
        let announced = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer announced Stack.resolveTop
   in Spec.describe s "TreasureHunt" $ do
        -- The headline. Top to bottom alice's library is Island, Swamp, Forest,
        -- Murder, Bird Maiden, Mountain: THREE lands sit above the first nonland
        -- card, so the four cards that reach her hand are neither the one an
        -- unwalked read would take nor the six a walk that never stopped would.
        Spec.it s "CR 401.2 the walk takes the top cards down to and including the first match" $ do
          island <- S.printingOf s registry "Island"
          treasureHunt <- S.printingOf s registry "Treasure Hunt"
          swamp <- S.printingOf s registry "Swamp"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          mountain <- S.printingOf s registry "Mountain"
          let after = cast (board island treasureHunt [mountain, maiden, murder, forest, swamp, island] [murder, murder])
          Spec.assertEqWith
            s
            "the three lands and the nonland card that ended the walk are alice's hand"
            (List.sort (namesIn Zone.Hand S.alice after))
            (List.sort [named "Forest", named "Island", named "Murder", named "Swamp"])
          Spec.assertEqWith
            s
            "the two cards under the match are still her library, in that order"
            (namesIn Zone.Library S.alice after)
            [named "Bird Maiden", named "Mountain"]
          Spec.assertEqWith s "and only the spell is in her graveyard (CR 608.2n)" (namesIn Zone.Graveyard S.alice after) [named "Treasure Hunt"]
          -- CR 400.1: "your library" is one player's. bob's holds two cards that
          -- would both have matched, and the walk never looked at them.
          Spec.assertEqWith s "bob's library is untouched" (namesIn Zone.Library S.bob after) [named "Murder", named "Murder"]
          Spec.assertEqWith s "and nothing reached bob's hand" (namesIn Zone.Hand S.bob after) []
        -- The near end of the walk: the top card already matches, so it is the
        -- whole set. Pairs with the case above on one changed thing -- where the
        -- Murder sits in the stock.
        Spec.it s "CR 401.2 a matching top card ends the walk at one card" $ do
          island <- S.printingOf s registry "Island"
          treasureHunt <- S.printingOf s registry "Treasure Hunt"
          swamp <- S.printingOf s registry "Swamp"
          murder <- S.printingOf s registry "Murder"
          let after = cast (board island treasureHunt [swamp, island, murder] [])
          Spec.assertEqWith s "one card came to hand" (namesIn Zone.Hand S.alice after) [named "Murder"]
          Spec.assertEqWith s "and the two under it stayed put" (namesIn Zone.Library S.alice after) [named "Island", named "Swamp"]
        -- The far end: CR 609.3's shortfall. A library with no matching card is
        -- walked to the bottom and given up whole, which is as much as the
        -- instruction can do.
        Spec.it s "CR 609.3 a library with no matching card is walked to the bottom" $ do
          island <- S.printingOf s registry "Island"
          treasureHunt <- S.printingOf s registry "Treasure Hunt"
          swamp <- S.printingOf s registry "Swamp"
          forest <- S.printingOf s registry "Forest"
          let after = cast (board island treasureHunt [forest, swamp, island] [])
          Spec.assertEqWith
            s
            "every card in the library came to hand"
            (List.sort (namesIn Zone.Hand S.alice after))
            (List.sort [named "Forest", named "Island", named "Swamp"])
          Spec.assertEqWith s "and the library is empty" (namesIn Zone.Library S.alice after) []

-- ObjectRef.EachCardFromAmong: the members of a bound group that a Filter
-- matches, where ObjectRef.ChosenCardFromAmong above takes the number a player
-- picks -- so one sentence can send the matching half of a group one way and the
-- remainder another.
--
-- Mulch {1}{G} Sorcery, "Reveal the top four cards of your library. Put all land
-- cards revealed this way into your hand and the rest into your graveyard."
-- (name, cost, type line and Oracle text checked against api.scryfall.com,
-- 2026-08-20). Three clauses: CR 701.20a's reveal binding the four as a group,
-- the matching half moved out of it, and "the rest" as ObjectRef.InSlot over the
-- SAME slot -- which finds the land cards gone, CR 400.7 having minted new
-- objects for them on the way to the hand. That is PR #1958's reading of "the
-- rest" for the singular choice, and this is the plural of it.
--
-- The board INTERLEAVES lands and nonlands in the revealed four, which is what
-- separates "the cards that match" from "the first two" and from "the last two";
-- two cards sit under them so the reveal's own depth is still observable, and a
-- three-seat game with a stocked opponent library keeps "your library" from
-- collapsing onto the table's.
mulchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mulchSpec s registry =
  let -- alice: two Forests for the {1}{G}, `stock` into her library BOTTOM FIRST
      -- (S.addLibraryCard puts each new card on top), Mulch in hand. `decoy` goes
      -- into BOB's library, which alice's "your library" must not reach.
      board forest mulch stock decoy =
        let mana = S.landsFor forest S.alice 2 S.threePlayerGame
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) mana stock
            withDecoy = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) withStock decoy
            (withSpell, spellId) = S.handOne mulch withDecoy
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      cast (spellId, gs) =
        let announced = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer announced Stack.resolveTop
   in Spec.describe s "Mulch" $ do
        -- The headline. Top to bottom alice's library is Swamp, Murder, Island,
        -- Bird Maiden, Mountain, Murder: the revealed four hold two lands and two
        -- nonland cards, ALTERNATING, so neither half is a prefix of the batch.
        Spec.it s "CR 608.2c the matching members go one way and the rest the other" $ do
          forest <- S.printingOf s registry "Forest"
          mulch <- S.printingOf s registry "Mulch"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          mountain <- S.printingOf s registry "Mountain"
          let after = cast (board forest mulch [murder, mountain, maiden, island, murder, swamp] [island, island])
          Spec.assertEqWith
            s
            "the two land cards among the revealed four are alice's hand"
            (List.sort (namesIn Zone.Hand S.alice after))
            (List.sort [named "Island", named "Swamp"])
          Spec.assertEqWith
            s
            "and the rest of the four, with the spell itself (CR 608.2n), are her graveyard"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.sort [named "Bird Maiden", named "Mulch", named "Murder"])
          Spec.assertEqWith
            s
            "the two cards under the revealed four are still her library, in that order"
            (namesIn Zone.Library S.alice after)
            [named "Mountain", named "Murder"]
          -- CR 400.1: "your library" is one player's. bob's holds two cards that
          -- would both have matched, and nothing looked at them.
          Spec.assertEqWith s "bob's library is untouched" (namesIn Zone.Library S.bob after) [named "Island", named "Island"]
          Spec.assertEqWith s "and nothing reached bob's hand" (namesIn Zone.Hand S.bob after) []
        -- CR 609.3 and CR 101.3: a group holding no matching member yields
        -- nothing, so that half of the sentence is skipped and "the rest" is all
        -- four. Pairs with the case above on exactly one changed thing -- which
        -- cards are stocked.
        Spec.it s "CR 609.3 a group with no matching member sends every one of the four to the graveyard" $ do
          forest <- S.printingOf s registry "Forest"
          mulch <- S.printingOf s registry "Mulch"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          mountain <- S.printingOf s registry "Mountain"
          let after = cast (board forest mulch [murder, mountain, maiden, maiden, murder, maiden] [])
          Spec.assertEqWith s "nothing reached alice's hand" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith
            s
            "and all four revealed cards are in the graveyard with the spell"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.sort [named "Bird Maiden", named "Bird Maiden", named "Bird Maiden", named "Mulch", named "Murder"])
          Spec.assertEqWith s "the two under them are still her library" (namesIn Zone.Library S.alice after) [named "Mountain", named "Murder"]
        -- The other end of the same pair: every member matches, so "the rest" is
        -- EMPTY and the InSlot clause moves nothing. CR 400.7 is what makes that
        -- true -- the ids the group still holds resolve to nothing once the cards
        -- have moved -- so a graveyard holding only the spell is the assertion
        -- that a second move did not drag the four back out of the hand.
        Spec.it s "CR 400.7 a group whose members all match leaves nothing for the rest" $ do
          forest <- S.printingOf s registry "Forest"
          mulch <- S.printingOf s registry "Mulch"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          mountain <- S.printingOf s registry "Mountain"
          murder <- S.printingOf s registry "Murder"
          let after = cast (board forest mulch [murder, swamp, island, mountain, swamp] [])
          Spec.assertEqWith
            s
            "all four revealed cards are alice's hand"
            (List.sort (namesIn Zone.Hand S.alice after))
            (List.sort [named "Island", named "Mountain", named "Swamp", named "Swamp"])
          Spec.assertEqWith s "and only the spell is in her graveyard" (namesIn Zone.Graveyard S.alice after) [named "Mulch"]
        -- A library of ONE card binds the slot as a SINGLE object rather than as
        -- a group, the two shapes every binder dispatches between. The ref reads
        -- both, so the one card still matches and still moves; a read that saw
        -- only the group shape would leave it in the library. CR 609.3 shortens
        -- the reveal itself from four cards to one.
        Spec.it s "CR 609.3 a one-card library binds a single object and the ref still matches it" $ do
          forest <- S.printingOf s registry "Forest"
          mulch <- S.printingOf s registry "Mulch"
          swamp <- S.printingOf s registry "Swamp"
          let after = cast (board forest mulch [swamp] [])
          Spec.assertEqWith s "the one card came to hand" (namesIn Zone.Hand S.alice after) [named "Swamp"]
          Spec.assertEqWith s "her library is empty" (namesIn Zone.Library S.alice after) []
          Spec.assertEqWith s "and only the spell is in her graveyard" (namesIn Zone.Graveyard S.alice after) [named "Mulch"]

-- The counted reveal-until walk: ObjectRef.TopOfLibraryUntil's Quantity counting
-- MATCHES, where treasureHuntSpec above pins the same walk at one match and
-- mulchSpec pins the split of what it bound. The three halves of Open the Way's
-- sentence are those two arms plus CR 401.4's random bottoming enduranceSpec
-- pins, and it is the only card in `data/cards/` that writes all three at once.
--
-- Open the Way {X}{G}{G} Sorcery, "X can't be greater than the number of players
-- in the game. / Reveal cards from the top of your library until you reveal X
-- land cards. Put those land cards onto the battlefield tapped and the rest on
-- the bottom of your library in a random order." (name, cost, type line and
-- Oracle text checked against api.scryfall.com, 2026-08-20). The whole card is
-- transcribed: the first sentence is Face.maximumX, and the second is three
-- clauses -- CR 701.20a's reveal binding the walked cards as a group, the
-- matching half moved to the battlefield tapped, and "the rest" as
-- ObjectRef.InSlot over the SAME slot, which finds the lands gone because CR
-- 400.7 minted new objects for them on the battlefield.
--
-- THREE SEATS, and they are load-bearing twice over. CR 101.1's ceiling IS the
-- seat count, so a two-seat board could not tell an X of 3 that the card refuses
-- from one it permits; and "your library" must not collapse onto the table's, so
-- bob's library is stocked with cards that would all have matched.
--
-- THE CEILING IS READ ONCE, at CR 601.2b's announcement, and never again --
-- Face.maximumX has no resolution-time reader at all. The departure pair below
-- is what makes that observable: carol leaving with the spell already on the
-- stack does not shrink the X alice announced, where carol leaving BEFORE the
-- cast refuses that same X.
--
-- THE RANDOMNESS IS THE ANSWERER'S, as it is for Endurance above: the engine
-- rolls nothing, it asks Prompt.Shuffle, so the fixture's permutation names the
-- resulting library. The answer ROTATES the batch, so it is neither the batch's
-- own order nor its reverse -- an engine that ignored the answer, and one that
-- handed CR 401.4's arrangement to the owner, each leave a different library.
openTheWaySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
openTheWaySpec s registry =
  let -- alice: seven Forests, so {4}{G}{G} is as affordable as {3}{G}{G} and
      -- nothing below turns on mana; `stock` into her library BOTTOM FIRST
      -- (S.addLibraryCard puts each new card on top), Open the Way in hand.
      -- `decoy` goes into BOB's library, which alice's "your library" must not
      -- reach.
      board forest openTheWay stock decoy =
        let mana = S.landsFor forest S.alice 7 S.threePlayerGame
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) mana stock
            withDecoy = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) withStock decoy
            (withSpell, spellId) = S.handOne openTheWay withDecoy
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      -- Announces this X, and rotates whatever batch the bottoming offers.
      answering :: Natural -> Prompt.Prompt r -> r
      answering x p = case p of
        Prompt.ChooseX {} -> x
        Prompt.Shuffle batch -> case batch of
          a : rest -> rest <> [a]
          [] -> []
        _ -> S.identityAnswer p
      -- Cast with this X, let anything in `between` happen while the spell sits
      -- on the stack, then resolve it.
      cast x between (spellId, gs) =
        let announced = S.runPure (answering x) gs (S.cast S.alice spellId)
         in S.runPure (answering x) (between announced) Stack.resolveTop
      -- The tap state of every battlefield object alice owns that carries this
      -- name. Empty where nothing of that name is there, which is how a card the
      -- walk revealed but did NOT match is told from one it did.
      tapOf name pid gs =
        [ Object.tapped o
        | oid <- Game.zoneMembers Zone.Battlefield pid gs,
          fmap S.nameOf (Game.cardOf oid gs) == named name,
          o <- Maybe.maybeToList (Game.lookupObject oid gs)
        ]
   in Spec.describe s "Open the Way" $ do
        -- The headline, and the case the counted walk exists for. Top to bottom
        -- alice's library is Island, Murder, Swamp, Bird Maiden, Goblin Piker,
        -- Mountain, Plains, Ogre Sentry, and X is 3: the walk reveals SIX cards,
        -- stopping on the Mountain that completes the count. Every other reading
        -- of the sentence names a different set -- one match (Treasure Hunt's
        -- walk) stops at the Island, three CARDS stops at the Swamp, and a walk
        -- with no count at all takes the whole library.
        Spec.it s "CR 401.2 the walk takes the top cards down to and including the Xth match" $ do
          forest <- S.printingOf s registry "Forest"
          openTheWay <- S.printingOf s registry "Open the Way"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          plains <- S.printingOf s registry "Plains"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          piker <- S.printingOf s registry "Goblin Piker"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let after = cast 3 id (board forest openTheWay [sentry, plains, mountain, piker, maiden, swamp, murder, island] [island, swamp])
          Spec.assertEqWith
            s
            "the three land cards the walk matched are on the battlefield tapped, and the nonland cards it passed are not there at all"
            (tapOf "Island" S.alice after, tapOf "Swamp" S.alice after, tapOf "Mountain" S.alice after, tapOf "Murder" S.alice after, tapOf "Bird Maiden" S.alice after, tapOf "Goblin Piker" S.alice after)
            ([TapState.Tapped], [TapState.Tapped], [TapState.Tapped], [], [], [])
          -- CR 401.4 through LibraryPlacement.RandomOrder: the three nonland
          -- cards go under the two the walk never reached, in the order the
          -- answerer named, from the BOTTOM inward -- so the Bird Maiden the
          -- rotation named first ends up deepest. Neither the batch's own order
          -- (which would deepen the Murder) nor its reverse (which would put the
          -- Bird Maiden second), so an engine that ignored the answer and one
          -- that reversed it are both excluded.
          Spec.assertEqWith
            s
            "her library is the two cards under the walk, then the three the rest clause bottomed in the named order"
            (namesIn Zone.Library S.alice after)
            [named "Plains", named "Ogre Sentry", named "Murder", named "Goblin Piker", named "Bird Maiden"]
          Spec.assertEqWith s "only the spell is in her graveyard (CR 608.2n)" (namesIn Zone.Graveyard S.alice after) [named "Open the Way"]
          Spec.assertEqWith s "and nothing reached her hand" (namesIn Zone.Hand S.alice after) []
          -- CR 400.1: "your library" is one player's. bob's holds two cards that
          -- would both have matched, and the walk never looked at them.
          Spec.assertEqWith s "bob's library is untouched" (namesIn Zone.Library S.bob after) [named "Swamp", named "Island"]
          Spec.assertEqWith s "and nothing arrived on bob's battlefield" (namesIn Zone.Battlefield S.bob after) []
        -- CR 609.3: a library holding fewer than X matches is walked to the
        -- bottom and given up whole. Pairs with the case above on one changed
        -- thing -- the stock holds two lands where it held three.
        Spec.it s "CR 609.3 a library with fewer than X matches is walked to the bottom" $ do
          forest <- S.printingOf s registry "Forest"
          openTheWay <- S.printingOf s registry "Open the Way"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          let after = cast 3 id (board forest openTheWay [maiden, swamp, murder, island] [])
          Spec.assertEqWith
            s
            "both land cards in the library are on the battlefield tapped"
            (tapOf "Island" S.alice after, tapOf "Swamp" S.alice after)
            ([TapState.Tapped], [TapState.Tapped])
          Spec.assertEqWith
            s
            "and the two nonland cards were bottomed into an otherwise empty library"
            (namesIn Zone.Library S.alice after)
            [named "Murder", named "Bird Maiden"]
        -- CR 101.1 read against CR 601.2b, on a board where the ONLY thing that
        -- can refuse the announcement is the card's own sentence: {4}{G}{G} is
        -- affordable off seven Forests, so an X of 4 in a three-player game is
        -- refused for the ceiling and nothing else, and CR 601.2 returns the game
        -- to before the casting was proposed.
        Spec.it s "CR 101.1 an X above the number of players reverses the cast" $ do
          forest <- S.printingOf s registry "Forest"
          openTheWay <- S.printingOf s registry "Open the Way"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          murder <- S.printingOf s registry "Murder"
          let stock = [murder, mountain, murder, swamp, murder, island]
              after = cast 4 id (board forest openTheWay stock [])
          Spec.assertEqWith
            s
            "no land arrived on the battlefield"
            (tapOf "Island" S.alice after, tapOf "Swamp" S.alice after, tapOf "Mountain" S.alice after)
            ([], [], [])
          Spec.assertEqWith
            s
            "the library is exactly as it was stocked"
            (namesIn Zone.Library S.alice after)
            [named "Island", named "Murder", named "Swamp", named "Murder", named "Mountain", named "Murder"]
          Spec.assertEqWith s "and the card is still in alice's hand" (namesIn Zone.Hand S.alice after) [named "Open the Way"]
        -- The CONTROL, and the same board with one thing changed: the answer.
        -- Three IS the number of players, so CR 101.1 permits it and the walk
        -- runs.
        Spec.it s "CR 101.1 an X equal to the number of players is announced and resolved" $ do
          forest <- S.printingOf s registry "Forest"
          openTheWay <- S.printingOf s registry "Open the Way"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          murder <- S.printingOf s registry "Murder"
          let stock = [murder, mountain, murder, swamp, murder, island]
              after = cast 3 id (board forest openTheWay stock [])
          Spec.assertEqWith
            s
            "all three land cards arrived tapped"
            (tapOf "Island" S.alice after, tapOf "Swamp" S.alice after, tapOf "Mountain" S.alice after)
            ([TapState.Tapped], [TapState.Tapped], [TapState.Tapped])
          Spec.assertEqWith s "and the card left alice's hand" (namesIn Zone.Hand S.alice after) []
        -- CR 601.2b's ceiling is read ONCE, at the announcement: carol leaves the
        -- game (CR 800.4a) with the spell already on the stack, and the X alice
        -- announced is still 3, so three lands are still found. An engine that
        -- re-read Face.maximumX at resolution would have two players to count.
        Spec.it s "CR 601.2b a player leaving in response does not shrink an X already announced" $ do
          forest <- S.printingOf s registry "Forest"
          openTheWay <- S.printingOf s registry "Open the Way"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          murder <- S.printingOf s registry "Murder"
          let stock = [murder, mountain, murder, swamp, murder, island]
              after = cast 3 (S.departs Departure.Type.Conceded S.carol) (board forest openTheWay stock [])
          Spec.assertEqWith
            s
            "all three land cards still arrived tapped"
            (tapOf "Island" S.alice after, tapOf "Swamp" S.alice after, tapOf "Mountain" S.alice after)
            ([TapState.Tapped], [TapState.Tapped], [TapState.Tapped])
          Spec.assertEqWith s "carol really did leave, so the ceiling a resolution-time read would have found is 2" (statusOf S.carol after) (Just (Status.Departed Departure.Type.Conceded))
        -- The discriminating twin, differing in exactly one thing: carol leaves
        -- BEFORE the cast rather than after it, so the ceiling she is counted in
        -- is 2 and CR 101.1 refuses the same X of 3 the case above honoured.
        Spec.it s "CR 101.1 the same X is refused where the departure came first" $ do
          forest <- S.printingOf s registry "Forest"
          openTheWay <- S.printingOf s registry "Open the Way"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          murder <- S.printingOf s registry "Murder"
          let stock = [murder, mountain, murder, swamp, murder, island]
              (spellId, gs) = board forest openTheWay stock []
              after = cast 3 id (spellId, S.departs Departure.Type.Conceded S.carol gs)
          Spec.assertEqWith
            s
            "no land arrived on the battlefield"
            (tapOf "Island" S.alice after, tapOf "Swamp" S.alice after, tapOf "Mountain" S.alice after)
            ([], [], [])
          Spec.assertEqWith s "and the card is still in alice's hand" (namesIn Zone.Hand S.alice after) [named "Open the Way"]

-- CR 701.20e's look, CR 701.20a's reveal of ONE card chosen from among what it
-- showed, and CR 401.4's arrangement handed to randomness -- the three halves
-- communeWithTheGodsSpec and enduranceSpec above each carry one of, on the
-- printing that carries all three at once.
--
-- Carth the Lion {2}{B}{G} Legendary Creature -- Human Warrior 3/5, "Whenever
-- Carth enters or a planeswalker you control dies, look at the top seven cards
-- of your library. You may reveal a planeswalker card from among them and put it
-- into your hand. Put the rest on the bottom of your library in a random order. /
-- Planeswalkers' loyalty abilities you activate cost an additional [+1] to
-- activate." (name, cost, type line, power, toughness and Oracle text checked
-- against api.scryfall.com, 2026-08-20). The second sentence is
-- Pawl.PlaneswalkerSpec's, and is transcribed as "abilities of a planeswalker"
-- for the reason recorded there (gap #1698).
--
-- ONE CHOICE, revealed AND moved: the reveal names ObjectRef.ChosenCardFromAmong
-- and binds what it showed to a slot, and the move reads that slot. A second
-- ChosenCardFromAmong under the move would be a second, independent choice, which
-- the printed "and" forbids -- so the reveal event and the card in hand must name
-- the SAME object, which is what the second assertion of each case below checks.
--
-- The look records nothing (CR 701.20e is private, #1412), so exactly one
-- GameEvent.Revealed is the whole of what the trigger shows -- the six cards left
-- over stay unrevealed however public the bottoming makes their destination.
--
-- THE RANDOMNESS IS THE ANSWERER'S, as it is for Endurance above: the fixture
-- names a permutation built from the OBJECT IDS, so the resulting library is
-- neither the batch's order nor its reverse.
carthTheLionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
carthTheLionSpec s registry =
  let named = Just . CardName.MkCardName . Text.pack
      -- alice's library, BOTTOM FIRST -- S.addLibraryCard puts each new card on
      -- top -- so the top seven, top first, are Island, Jace Beleren, Murder,
      -- Goblin Piker, Chandra, Forest, Bird Maiden, and the Swamp beneath them is
      -- never looked at. Two planeswalker cards among seven, five cards matching
      -- nothing, and the offer is therefore [Jace Beleren, Chandra] where the
      -- group is all seven.
      stockNames = ["Swamp", "Bird Maiden", "Forest", "Chandra, Fire Artisan", "Goblin Piker", "Murder", "Jace Beleren", "Island"]
      stock printings gs = List.mapAccumL (\g p -> let (oid, g') = S.addLibraryCard p S.alice g in (g', oid)) gs printings
      -- The stocked card of a given name, by the id S.addLibraryCard minted for
      -- it: the permutation below is built from these rather than from the
      -- batch's own order.
      idOf ids name = Maybe.fromMaybe S.noSource (Maybe.listToMaybe [oid | (n, oid) <- zip stockNames ids, n == name])
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- Takes the printed "may" -- clause 1, the reveal and the move to hand;
      -- clause 0 is the look and clause 2 the rest -- answers the group choice
      -- with the nth card offered, and names `order` as the random order, deepest
      -- card first. A `Nothing` order leaves Pawl.Engine.Game.honourShuffle the
      -- batch it offered.
      answering :: Maybe Int -> Maybe [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      answering mTake order p = case p of
        Prompt.ChooseOptional _ _ _ _ clause
          | clause == ClauseIndex.MkClauseIndex 1 && Maybe.isJust mTake -> OptionalDecision.Exercises
        Prompt.ChooseCardFromAmong _ _ _ offered -> nth (Maybe.fromMaybe 0 mTake) offered
        Prompt.Shuffle offered -> Maybe.fromMaybe offered order
        _ -> S.identityAnswer p
      -- Which objects a CR 701.20a reveal has shown so far. A look shows nobody
      -- anything and appends no event, so this counts the reveal alone.
      revealed gs =
        Maybe.mapMaybe
          ( \event -> case event of
              GameEvent.Revealed (Revealed.MkRevealed _ oid _ _) -> Just oid
              _ -> Nothing
          )
          (S.eventsOf gs)
   in Spec.describe s "CarthTheLion" $ do
        -- The headline: the SECOND planeswalker card among the seven is revealed
        -- and taken, and the six left over reach the bottom in the order the
        -- randomness named.
        Spec.it s "CR 701.20a the enters trigger reveals the chosen card and puts that same object into its controller's hand" $ do
          carth <- S.printingOf s registry "Carth the Lion"
          printings <- Monad.mapM (S.printingOf s registry) stockNames
          let (stocked, ids) = stock printings (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger carth S.alice stocked
              -- Deepest first, and neither the batch's order nor its reverse.
              order = fmap (idOf ids) ["Murder", "Bird Maiden", "Island", "Forest", "Jace Beleren", "Goblin Piker"]
              answer :: Prompt.Prompt r -> r
              answer = answering (Just 1) (Just order)
              placed = S.runPure answer entered Engine.placePendingTriggers
              after = S.runPure answer placed Stack.resolveTop
          Spec.assertEqWith s "the second planeswalker card among the seven is the one in alice's hand" (namesIn Zone.Hand S.alice after) [named "Chandra, Fire Artisan"]
          Spec.assertEqWith s "and it is the one object the trigger revealed -- one reveal, not seven" (revealed after) [idOf ids "Chandra, Fire Artisan"]
          Spec.assertEqWith
            s
            "alice's library, top first, is the card the look never reached and then the six left over in the named order"
            (namesIn Zone.Library S.alice after)
            [named "Swamp", named "Goblin Piker", named "Jace Beleren", named "Forest", named "Island", named "Bird Maiden", named "Murder"]
          Spec.assertEqWith s "nothing was put into a graveyard" (namesIn Zone.Graveyard S.alice after) []
        -- The paired control: the same board and the same offer, answered at
        -- index 0. If the engine were picking, both legs would name one card.
        Spec.it s "CR 608.2d the engine does not pick: another answer reveals and takes the other planeswalker card" $ do
          carth <- S.printingOf s registry "Carth the Lion"
          printings <- Monad.mapM (S.printingOf s registry) stockNames
          let (stocked, ids) = stock printings (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger carth S.alice stocked
              answer :: Prompt.Prompt r -> r
              answer = answering (Just 0) Nothing
              placed = S.runPure answer entered Engine.placePendingTriggers
              after = S.runPure answer placed Stack.resolveTop
          Spec.assertEqWith s "the first planeswalker card comes to hand instead" (namesIn Zone.Hand S.alice after) [named "Jace Beleren"]
          Spec.assertEqWith s "and that is the object revealed" (revealed after) [idOf ids "Jace Beleren"]
        -- CR 603.5: the printed "may" is a real choice. Declining reveals nothing
        -- and sends all seven to the bottom -- the look still ran, so this cannot
        -- pass because the trigger never resolved.
        Spec.it s "CR 603.5 declining the may reveals nothing and bottoms all seven" $ do
          carth <- S.printingOf s registry "Carth the Lion"
          printings <- Monad.mapM (S.printingOf s registry) stockNames
          let (stocked, ids) = stock printings (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger carth S.alice stocked
              order = fmap (idOf ids) ["Chandra, Fire Artisan", "Island", "Bird Maiden", "Jace Beleren", "Murder", "Forest", "Goblin Piker"]
              answer :: Prompt.Prompt r -> r
              answer = answering Nothing (Just order)
              placed = S.runPure answer entered Engine.placePendingTriggers
              after = S.runPure answer placed Stack.resolveTop
          Spec.assertEqWith s "nothing reached alice's hand" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith s "and nothing was revealed" (revealed after) []
          Spec.assertEqWith
            s
            "all seven are on the bottom in the named order"
            (namesIn Zone.Library S.alice after)
            [named "Swamp", named "Goblin Piker", named "Forest", named "Murder", named "Jace Beleren", named "Bird Maiden", named "Island", named "Chandra, Fire Artisan"]
        -- The condition's OTHER disjunct (CR 603.1b read as "any"): a planeswalker
        -- alice controls dying fires the same ability, with Carth long settled and
        -- entering nothing.
        --
        -- Both Jaces are placed with no loyalty counters -- the fixture puts them
        -- there rather than an entry rider -- so CR 704.5i buries both in one
        -- state-based check. That is the pair the case turns on: bob's dies in the
        -- same batch as alice's, and only alice's is a planeswalker SHE controls,
        -- so an ability that read the filter as "a planeswalker" would resolve
        -- TWICE and put two cards in her hand.
        Spec.it s "CR 603.2 a planeswalker its controller controls dying fires the same ability, and an opponent's does not" $ do
          carth <- S.printingOf s registry "Carth the Lion"
          jace <- S.printingOf s registry "Jace Beleren"
          printings <- Monad.mapM (S.printingOf s registry) stockNames
          let (stocked, ids) = stock printings (Setup.emptyGame S.bothPlayers)
              (_, withCarth) = S.addCreature carth S.alice stocked
              (_, withHers) = S.addCreature jace S.alice withCarth
              (_, withHis) = S.addCreature jace S.bob withHers
              buried = S.settleSba withHis
              answer :: Prompt.Prompt r -> r
              answer = answering (Just 1) Nothing
              placed = S.runPure answer buried Engine.placePendingTriggers
              after = S.runPure answer placed (Monad.replicateM_ 2 Stack.resolveTop)
          Spec.assertEqWith s "one trigger resolved, so exactly the chosen planeswalker card is in alice's hand" (namesIn Zone.Hand S.alice after) [named "Chandra, Fire Artisan"]
          Spec.assertEqWith s "and exactly one card was revealed" (revealed after) [idOf ids "Chandra, Fire Artisan"]
          Spec.assertEqWith s "both planeswalkers died" (namesIn Zone.Graveyard S.alice after, namesIn Zone.Graveyard S.bob after) ([named "Jace Beleren"], [named "Jace Beleren"])

-- The same arm reached from a TRIGGER rather than an activated ability, and over
-- LAND cards rather than creature cards -- the two axes portOfKarfellSpec above
-- holds fixed.
--
-- Blossoming Tortoise {2}{G}{G} Creature -- Turtle 3/3, "Whenever this creature
-- enters or attacks, mill three cards, then return a land card from your
-- graveyard to the battlefield tapped. Activated abilities of lands you control
-- cost {1} less to activate. Land creatures you control get +1/+1." (name, cost,
-- type line and Oracle text checked against api.scryfall.com). Only the trigger
-- is read here; the two static abilities are Pawl.ActivateSpec's.
--
-- Two land cards are buried and the answer is pinned to the SECOND, so the
-- assertion cannot be met by taking the first; a creature card is buried beside
-- them and the three cards the trigger's own mill adds are creature cards too, so
-- an arm that ignored the Filter would have five candidates rather than two.
blossomingTortoiseSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
blossomingTortoiseSpec s registry = Spec.describe s "BlossomingTortoise" $ do
  Spec.it s "CR 608.2d the enters trigger returns the land card its controller chose, tapped" $ do
    tortoise <- S.printingOf s registry "Blossoming Tortoise"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    let (tortoiseId, entered) = S.entersWithTrigger tortoise S.alice (Setup.emptyGame S.bothPlayers)
        buried = List.foldl' (\g printing -> snd (S.addGraveyardCard printing S.alice g)) entered [forest, piker, island]
        gs = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) buried [piker, piker, piker]
        named = Just . CardName.MkCardName . Text.pack
        -- The Island, buried last and so the SECOND of the two land cards in the
        -- ascending order the prompt offers.
        wanted = case Game.zoneMembers Zone.Graveyard S.alice gs of
          [_, _, third] -> Just third
          _ -> Nothing
        choosing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
        choosing chosen p = case p of
          Prompt.ChooseCardInGraveyard {} -> chosen
          _ -> S.identityAnswer p
        onBattlefield gs1 =
          List.sort
            [ (fmap S.nameOf (Game.cardOf oid gs1), fmap Object.tapped (Game.lookupObject oid gs1))
            | oid <- Set.toList (GameState.battlefield gs1)
            ]
    case wanted of
      Nothing -> Spec.assertBool s False "expected three cards in alice's graveyard"
      Just chosen ->
        let answer :: Prompt.Prompt r -> r
            answer = choosing chosen
            placed = S.runPure answer gs Engine.placePendingTriggers
            resolved = S.runPure answer placed Stack.resolveTop
         in do
              Spec.assertEqWith
                s
                "the Island is on the battlefield tapped, beside the untapped Tortoise"
                (onBattlefield resolved)
                (List.sort [(named "Blossoming Tortoise", Just TapState.Untapped), (named "Island", Just TapState.Tapped)])
              Spec.assertEqWith
                s
                "the unchosen Forest, the buried creature card and the three milled ones stay put"
                (List.sort (namesIn Zone.Graveyard S.alice resolved))
                (List.sort (named "Forest" : replicate 4 (named "Goblin Piker")))
              Spec.assertBool s (S.onBattlefield tortoiseId resolved) "and the Tortoise itself never moved"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  destroyAllSpec s registry
  returnAllSpec s registry
  riseOfTheDarkRealmsSpec s registry
  angelOfFinalitySpec s registry
  amnesiaSpec s registry
  enduranceSpec s registry
  portOfKarfellSpec s registry
  graspingTentaclesSpec s registry
  midnightTillingSpec s registry
  communeWithTheGodsSpec s registry
  ancestralMemoriesSpec s registry
  animalMagnetismSpec s registry
  treasureHuntSpec s registry
  mulchSpec s registry
  openTheWaySpec s registry
  carthTheLionSpec s registry
  blossomingTortoiseSpec s registry
