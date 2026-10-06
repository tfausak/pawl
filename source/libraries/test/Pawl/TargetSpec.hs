{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Target: CR 115 target legality, and the rule-702 TARGETING
-- RESTRICTIONS that narrow it. Shroud (CR 702.18) is the pool's first, printed
-- by Blurred Mongoose, and hexproof (CR 702.11) is the second, printed by
-- Slippery Bogle. Their cases sit together because the pair is only interesting
-- together: the two rules differ in exactly one thing, whether the restriction
-- reads WHO is targeting. One case covers the other side of the
-- restriction/admission split -- Pawl.Engine.Sba's CR 303.4c re-check, which
-- asks what an enchant slot ADMITS and must not ask a targeting question --
-- a group of them covers rule 702.11's "hexproof from [quality]" variant (CR
-- 702.11d and 702.11e), the only restriction here that reads what the SOURCE is
-- rather than who controls it, ending on Knight of Grace's printing -- and the
-- rest cover CR 115.2's two escape hatches from "only permanents are legal
-- targets": its clause (b) as Cancel and Stifle, its clause (a) as Raise Dead,
-- Withered Wretch, Riftsweeper and Dwell on the Past. (Those letters are prose
-- inside rule 115.2, not subrule numbers; there is no CR 115.2a.)
--
-- Raise Dead and Withered Wretch are the two halves of CR 400.1's per-player
-- axis -- "in your graveyard" against "from a graveyard" -- and the case that
-- reads their pools reads BOTH off one board, so neither is left to pass against
-- a pool that had stopped asking whose graveyard it was reading.
--
-- Riftsweeper is the other side of that same rule: exile is one of the zones CR
-- 400.1 says are "shared by all players", so its pool has no per-player axis at
-- all, and the case that reads it reads Withered Wretch's off the same board so
-- neither off-battlefield pool can swallow the other.
--
-- Dwell on the Past is the third reading of that axis, and the only one no
-- perspective answers: "their graveyard" is whoever the spell's OTHER slot
-- targets, so its case is about CR 601.2c's one announcement over two slots
-- rather than about the pool alone.
--
-- Three synthetics sit with it: Synthetic Exhume the Archive fixes its card count
-- at two, so a board splitting two cards between two graveyards has no coherent
-- announcement at all; Synthetic Recurring Reclamation puts the mode in CR
-- 700.2d's repeat, where the scope has to name its own occurrence's player slot
-- rather than the first occurrence's; and Synthetic Reclamation Engine is that
-- same repeat one object type over, on an ACTIVATED ability, which re-checks CR
-- 608.2b down a path of its own.
--
-- Fall of the Hammer is beside it because it is the other way one slot can
-- depend on another: not the POOL a slot draws from but the FILTER it is
-- narrowed by, which is CR 601.2c's "another" between two slots of one
-- announcement. Its case reads the same union-offer plus joint-check pair
-- Dwell's does, and turns on an announcement the joint check has to reject.
-- Synthetic Hammer Refrain is that card under CR 700.2d's repeat, where the
-- filter's slot name has to follow the occurrence the way the pool's does, and
-- Itzquinth, Firstborn of Gishath is the same pair on the road no spell takes:
-- a CR 603.12 reflexive ability, whose announcement is judged as CR 603.3d puts
-- it on the stack rather than as CR 601.2c casts it.
--
-- Cancel and Stifle's case has a third beside it, on the same pool one rule
-- over: CR 115.5's self-exclusion for an ABILITY, which Adric, Mathematical
-- Genius is the first card in the pool to make reachable. It is a fence rather
-- than a proof -- Pawl.Engine.Target.legalRecipients' own note says why.
--
-- The last case is hexproof's other axis: not who is targeting but WHETHER THE
-- KEYWORD IS THERE AT ALL. Dawnglade Regent grants it through a CR 604.2 "as
-- long as you're the monarch" clause, so the same Doom Blade answers both ways
-- across CR 725.5's no-monarch window.
--
-- The last group is the other side of the split again, and the only one about a
-- slot's own FILTER rather than about a restriction or a pool: Razorfin
-- Abolisher's "target creature with a counter on it" is CR 122.1 asked
-- kind-agnostically, and it is here because what it narrows is admission.
--
-- Gameplay-level: every target slot under test is read out of a committed card rather
-- than hand-built, and the cases that turn on an effect cast and resolve through
-- the stack.
module Pawl.TargetSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural.Type
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- The one target slot a single-slot card or ability declares, read out of the
-- committed printing (S.spellTargetSlot's rationale) but keyed by COUNT rather
-- than by slot name: Cancel calls its slot "spell", not "target".
soleTargetSlot :: Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Maybe TargetSlot.TargetSlot
soleTargetSlot modal = case Map.elems (Modal.allTargetSlots modal) of
  [only] -> Just only
  _ -> Nothing

-- soleTargetSlot, off the one TRIGGERED ability a printing declares rather than
-- off its spell. Same rationale: the slot is read out of the committed card, so
-- the case exercises the codec's parse and never a hand-built TargetSlot.
triggerTargetSlot :: Printing.Printing -> Maybe TargetSlot.TargetSlot
triggerTargetSlot printing = case Face.triggeredAbilities (S.combinedFace printing) of
  [ability] -> soleTargetSlot (TriggeredAbility.modal ability)
  _ -> Nothing

-- The one ACTIVATED ability of a printing that declares exactly one. Nothing for
-- any other printing, so a card that grew a second ability fails the case that
-- names it rather than silently picking whichever came first -- soleTargetSlot
-- above is the same shape for the same reason.
soleActivatedAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
soleActivatedAbility p = case Face.activatedAbilities (S.combinedFace p) of
  [only] -> Just only
  _ -> Nothing

-- The one activated ability of a printing whose cost carries a named component,
-- for a card that declares more than one. By the COST rather than by index, so a
-- reordering of the card file cannot silently aim a case at the wrong ability.
activatedAbilityCosting :: CostComponent.CostComponent Keyword.Keyword -> Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
activatedAbilityCosting component p =
  List.find
    (List.elem component . Cost.Type.components . ActivatedAbility.cost)
    (Face.activatedAbilities (S.combinedFace p))

-- `pid` controls the restricted creature -- a Blurred Mongoose or a Slippery
-- Bogle -- and a Goblin Piker, and nothing else. The Piker is the CONTROL in
-- every rule-702 case below: it is a legal target of everything the restricted
-- creature is not, so "the restricted creature is excluded" cannot pass because
-- the whole legal set is empty.
restrictionBoard :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
restrictionBoard restricted piker pid =
  let gs0 = Setup.emptyGame S.bothPlayers
      (restrictedId, gs1) = S.addPermanent restricted pid gs0
      (pikerId, gs2) = S.addPermanent piker pid gs1
   in (restrictedId, pikerId, gs2)

-- Aims every target slot at one chosen card, tagged the way
-- Pool.CardsInGraveyard tags its candidates (Recipient.ToObject -- the
-- candidates are CARDS, not creatures). S.identityAnswer would answer with
-- Set.lookupMin instead, which is whichever graveyard card happens to have the
-- lowest id -- no way to say "bob's".
aimAtCard :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCard oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- Answers every target slot with the OFFERED recipient for one object, filtered
-- out of the set the engine offered rather than built: a hand-built recipient
-- carrying another tag is dropped by CR 608.2b's re-read at resolution with no
-- error. The boards below offer several creatures, so S.identityAnswer's
-- lowest-id answer would not discriminate.
aimAtOffered :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtOffered oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- aimAtCard, plus a Prompt.Shuffle that REVERSES the library rather than
-- returning it unchanged. CR 701.24a defines a shuffle as randomising an order,
-- and S.identityAnswer's legal-but-inert answer makes a shuffled library
-- indistinguishable from an unshuffled one -- so a reversal is what lets a test
-- say "this library was shuffled" at all (MulliganSpec's own reversing
-- interpreter, for the same reason). Game.honourShuffle accepts it: a reversal
-- is a permutation, so the contents are unchanged.
aimAtCardShuffling :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCardShuffling oid p = case p of
  Prompt.Shuffle ids -> reverse ids
  _ -> aimAtCard oid p

-- CR 601.2c's whole announcement for Dwell on the Past: bob in the player slot
-- and `oids` in the card slot, with as many cards announced as there are.
--
-- PINNED rather than searched, and pinned to a set the offer may not contain:
-- the case's point is a card that WAS offered (the union over both graveyards)
-- and is still an illegal answer beside bob, so an answerer that filtered
-- against `legal` would hand back an empty slot and the announcement would fail
-- on its COUNT instead -- passing for a reason the case is not about.
aimingDwell :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
aimingDwell oids p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const (Natural.length oids)) offers
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot _ ->
          if slot == SlotName.MkSlotName (Text.pack "player")
            then Set.singleton (Recipient.ToPlayer S.bob)
            else Set.fromList (fmap Recipient.ToObject oids)
      )
      asked
  _ -> S.identityAnswer p

-- aimingDwell with aimAtCardShuffling's reversing Prompt.Shuffle, for the same
-- reason: CR 701.24a leaves a shuffle observable only through the order it
-- produces.
aimingDwellShuffling :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
aimingDwellShuffling oids p = case p of
  Prompt.Shuffle ids -> reverse ids
  _ -> aimingDwell oids p

-- CR 601.2c's whole announcement for Fall of the Hammer: `dealerId` in the
-- dealer slot and `victimId` in the victim slot. Both counts are fixed at one,
-- so there is no Prompt.AnnounceTargets to answer.
--
-- FILTERED out of the offered set rather than built, which is the posture
-- Pawl.CopySpec's answerers take: Pool.Creatures offers Recipient.ToCreature,
-- and a hand-built recipient of another shape would be dropped at CR 608.2b's
-- re-read with no error. The case that names the same creature in both slots
-- therefore also depends on that creature being OFFERED for the victim slot,
-- which the union assertion in the same case pins.
aimingHammer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingHammer dealerId victimId p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let wanted = if slot == SlotName.MkSlotName (Text.pack "dealer") then dealerId else victimId
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  _ -> S.identityAnswer p

-- CR 700.2d's whole announcement for Synthetic Hammer Refrain with its damage
-- mode chosen twice: occurrence 0 names `dealer`/`victim`, occurrence 1 names
-- `dealerTwo`/`victimTwo` under Modal.instanceSlot's suffixed names. Pinned per
-- slot name and FILTERED out of the offered set, aimingHammer's shape and for its
-- reason.
aimingRefrain :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingRefrain dealer victim dealerTwo victimTwo p = case p of
  Prompt.ChooseModes {} -> Seq.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 0]
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let named name = slot == SlotName.MkSlotName (Text.pack name)
              wanted
                | named "dealer" = dealer
                | named "victim" = victim
                | named "dealer#1" = dealerTwo
                | otherwise = victimTwo
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  _ -> S.identityAnswer p

-- CR 700.2d's whole announcement for Synthetic Measured Refrain with its
-- destroy mode chosen twice: occurrence 0 fills `gauge` with TWO creatures and
-- `victim` with one, occurrence 1 fills `gauge#1` with one and `victim#1` with
-- one. Pinned per slot name and FILTERED out of the offered set, aimingRefrain's
-- shape and for its reason.
--
-- Occurrence 0's gauge takes two and occurrence 1's takes one, which is the whole
-- of what separates the two bounds: the slot is "up to two", so the two
-- occurrences of one mode measure 2 and 1.
aimingMeasured :: [ObjectId.ObjectId] -> ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingMeasured gauges victim gaugeTwo victimTwo p = case p of
  Prompt.ChooseModes {} -> Seq.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 0]
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let named name = slot == SlotName.MkSlotName (Text.pack name)
              wanted
                | named "gauge" = gauges
                | named "victim" = [victim]
                | named "gauge#1" = [gaugeTwo]
                | otherwise = [victimTwo]
           in Set.filter (maybe False (`elem` wanted) . Recipient.objectOf) offered
      )
      asked
  _ -> S.identityAnswer p

-- CR 601.2c's whole announcement for Bioshift: `giverId` in the `from` slot and
-- `takerId` in the `to` slot, aimingHammer's shape and FILTERED for its reason.
--
-- Its second prompt is CR 122.5's "any number": the whole offered tally crosses,
-- so a case that moves nothing moved nothing because the announcement or the move
-- refused rather than because the answerer declined.
aimingBioshift :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingBioshift giverId takerId p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let wanted = if slot == SlotName.MkSlotName (Text.pack "from") then giverId else takerId
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  Prompt.ChooseMovedCounters _ _ _ _ offered -> offered
  _ -> S.identityAnswer p

-- CR 607.2d: Pentarch Paladin's "{W}{W}, {T}: Destroy target permanent of the
-- chosen color" is linked to its "As this creature enters, choose a color", so
-- the slot reads the colour the Paladin chose (Oracle checked against Scryfall
-- on 2026-10-01). The choice is stamped rather than cast for; the entry road
-- that writes it is Painter's Servant's (Pawl.ColorSpec).
--
-- bob holds a red Goblin Piker and a green Llanowar Elves, alice the Paladin
-- and two Plains, and the answerer PREFERS the Piker, falling back to whatever
-- else was offered. So each board shows what the engine offered by which
-- permanent dies.
pentarchPaladinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
pentarchPaladinSpec s registry = Spec.describe s "Pentarch Paladin" $ do
  let paladinBoard chosen = do
        paladin <- S.printingOf s registry "Pentarch Paladin"
        plains <- S.printingOf s registry "Plains"
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        elves <- S.printingOf s registry "Llanowar Elves"
        murder <- S.printingOf s registry "Murder"
        let (paladinId, g0) = S.addPermanent paladin S.alice (S.landsFor swamp S.bob 3 (S.landsFor plains S.alice 2 S.threePlayerGame))
            (pikerId, g1) = S.addPermanent piker S.bob g0
            (elvesId, g2) = S.addPermanent elves S.bob g1
            (murderId, g3) = S.addHandCard murder S.bob g2
            chose = g3 {GameState.objects = Map.adjust (\o -> o {Object.chosenColors = Set.singleton chosen}) paladinId (GameState.objects g3)}
        case soleActivatedAbility paladin of
          Nothing -> Spec.assertFailure s "Pentarch Paladin should declare one activated ability" >> pure Nothing
          Just ability -> pure (Just (paladinId, pikerId, elvesId, murderId, S.runPure (aimAtCreature pikerId) chose (Activate.activateAbility S.alice paladinId ability)))
      resolve gs = S.runPure S.identityAnswer gs Stack.resolveTop
  -- A PAIR OF BOARDS differing only in the colour chosen: red reaches the Piker,
  -- green does not, so the same preference kills the Elves instead.
  Spec.it s "CR 607.2d the Paladin destroys only a permanent of the colour it chose" $ do
    red <- paladinBoard Color.Red
    green <- paladinBoard Color.Green
    case (red, green) of
      (Just (_, pikerId, elvesId, _, redActivated), Just (_, pikerId', elvesId', _, greenActivated)) -> do
        let redResolved = resolve redActivated
            greenResolved = resolve greenActivated
        Spec.assertBool s (not (S.onBattlefield pikerId redResolved)) "having chosen red, the Paladin destroys the red Piker"
        Spec.assertBool s (S.onBattlefield pikerId' greenResolved) "having chosen green, it cannot aim at the Piker"
        Spec.assertBool s (not (S.onBattlefield elvesId' greenResolved)) "and destroys the green Elves instead"
        Spec.assertBool s (S.onBattlefield elvesId redResolved) "while the red board left the Elves alone"
      _ -> pure ()
  -- CR 608.2b re-asks the slot at resolution, and CR 608.2h with CR 113.7a answer
  -- "the chosen color" for a source that has left from its last known
  -- information. bob Murders the Paladin in response; the ability still
  -- resolves against the red Piker it aimed at.
  Spec.it s "CR 608.2h the Paladin's ability still destroys its target after the Paladin dies in response" $ do
    red <- paladinBoard Color.Red
    case red of
      Just (paladinId, pikerId, _, murderId, activated) -> do
        let murdered = S.runPure (aimAtCreature paladinId) (activated {GameState.priority = Just S.bob}) (S.cast S.bob murderId Monad.>> Stack.resolveTop)
            resolved = resolve murdered
        Spec.assertBool s (not (S.onBattlefield pikerId resolved)) "the Piker is destroyed by the dead Paladin's ability"
        Spec.assertBool s (not (S.onBattlefield paladinId murdered)) "because the Paladin had already left before it resolved"
      Nothing -> pure ()
  -- CR 707.10c with CR 113.7: a copy of the ability keeps the Paladin as its
  -- source, so its new targets are judged against the colour the Paladin chose.
  -- A second red Piker joins bob's side after the Paladin aims at the first, and
  -- Lithoform Engine ({2}, {T}: "Copy target activated or triggered ability you
  -- control. You may choose new targets for the copy.") copies the ability with
  -- an answerer preferring the second.
  Spec.it s "CR 707.10c a Lithoform Engine copy of the Paladin's ability can aim at another permanent of the chosen colour" $ do
    red <- paladinBoard Color.Red
    engine <- S.printingOf s registry "Lithoform Engine"
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    case (red, Face.activatedAbilities (S.combinedFace engine)) of
      (Just (_, firstId, _, _, activated), copying : _) -> do
        let (secondId, g0) = S.addPermanent piker S.bob activated
            (engineId, g1) = S.addPermanent engine S.alice (S.landsFor plains S.alice 2 g0)
            copied = S.runPure S.identityAnswer (g1 {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice engineId copying)
            retargeted = S.runPure (aimAtCreature secondId) copied Stack.resolveTop
            copyResolved = resolve retargeted
            bothResolved = resolve copyResolved
        Spec.assertBool s (not (S.onBattlefield secondId copyResolved)) "the copy, re-aimed, destroys the second red Piker"
        Spec.assertBool s (S.onBattlefield firstId copyResolved) "while the original has not resolved yet"
        Spec.assertBool s (not (S.onBattlefield firstId bothResolved)) "and the original then destroys the first"
      _ -> Spec.assertFailure s "the Paladin board and Lithoform Engine's copying ability"

-- CR 607.2d one road over: From the Rubble's "At the beginning of your end step,
-- return target creature card of the chosen type from your graveyard to the
-- battlefield with a finality counter on it" is a TRIGGERED ability's slot,
-- announced through Engine.placeBorne rather than Pawl.Engine.Activate (Oracle
-- checked against Scryfall on 2026-10-01). The choice is stamped, as for the
-- Paladin above; the entry road that writes a creature type is Obelisk of
-- Urd's (Pawl.ProjectionSpec).
--
-- A PAIR OF BOARDS differing only in the type chosen, alice's graveyard holding
-- a Goblin Piker and a Hill Giant, and the answerer preferring the Piker: each
-- board shows what the engine offered by which card comes back.
fromTheRubbleSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
fromTheRubbleSpec s registry =
  Spec.it s "CR 607.2d From the Rubble returns only a creature card of the type it chose" $ do
    rubble <- S.printingOf s registry "From the Rubble"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    let (rubbleId, g0) = S.addPermanent rubble S.alice (Setup.emptyGame S.bothPlayers)
        (pikerId, g1) = S.addGraveyardCard piker S.alice g0
        (_, g2) = S.addGraveyardCard giant S.alice g1
        endOfTurn chosen =
          let chose = g2 {GameState.objects = Map.adjust (\o -> o {Object.chosenSubtype = Just chosen}) rubbleId (GameState.objects g2), GameState.remaining = Seq.fromList [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]}
              afterMain = S.runPure (aimAtCreature pikerId) chose Engine.runStep
           in S.runPure (aimAtCreature pikerId) afterMain Engine.runStep
        onBattlefield name = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack name)) S.alice
        goblins = endOfTurn Subtype.Goblin
        giants = endOfTurn Subtype.Giant
    Spec.assertEqWith s "having chosen Goblin, the Piker comes back" (onBattlefield "Goblin Piker" goblins, onBattlefield "Hill Giant" goblins) (1, 0)
    Spec.assertEqWith s "having chosen Giant, the Piker is not offered and the Giant comes back" (onBattlefield "Goblin Piker" giants, onBattlefield "Hill Giant" giants) (0, 1)

-- CR 601.2c / 205.3m: Unbury's "return two target creature cards that share a
-- creature type from your graveyard to your hand". Each slot names the other
-- with SharesCreatureTypeWithBound, since the condition binds both targets alike
-- (CR 608.2b re-checks each), and `second` adds Not (IsBound "first"). alice's graveyard
-- holds two Hill Giants, a Goblin Piker and a Woodland Changeling; every board
-- is the same, and the cases differ only in which pair is named.
unburySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
unburySpec s registry = Spec.describe s "Unbury" $ do
  let fixture = do
        swamp <- S.printingOf s registry "Swamp"
        giant <- S.printingOf s registry "Hill Giant"
        piker <- S.printingOf s registry "Goblin Piker"
        changeling <- S.printingOf s registry "Woodland Changeling"
        unbury <- S.printingOf s registry "Unbury"
        let lands = S.landsFor swamp S.alice 2 (Setup.emptyGame S.bothPlayers)
            (giantA, g1) = S.addObjectIn Zone.Graveyard giant S.alice lands
            (giantB, g2) = S.addObjectIn Zone.Graveyard giant S.alice g1
            (pikerId, g3) = S.addObjectIn Zone.Graveyard piker S.alice g2
            (changelingId, g4) = S.addObjectIn Zone.Graveyard changeling S.alice g3
            (board, spellId) = S.handOne unbury g4
        pure (unbury, board, spellId, giantA, giantB, pikerId, changelingId)
  -- The union posture: before either target is chosen, the second slot is
  -- offered every creature card, the Goblin included, and the joint check is
  -- what narrows it.
  Spec.it s "CR 601.2c the second slot's offer is every creature card" $ do
    (unbury, board, _, giantA, giantB, pikerId, changelingId) <- fixture
    let slots = Modal.allTargetSlots (Face.spell (S.combinedFace unbury))
        offered = Target.legalSets (Just S.alice) False Map.empty S.noSource slots board
    Spec.assertEqWith
      s
      "every creature card in alice's graveyard"
      (Set.map Recipient.objectOf (Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack "second")) offered))
      (Set.fromList (fmap Just [giantA, giantB, pikerId, changelingId]))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Target" $ do
  unburySpec s registry
  pentarchPaladinSpec s registry
  fromTheRubbleSpec s registry
  -- CR 702.18a: "Shroud is a static ability. 'Shroud' means 'This permanent or
  -- player can't be the target of spells or abilities.'" Doom Blade is "target
  -- nonblack creature" and the Mongoose is green, so its Filter admits the
  -- Mongoose and only the restriction can remove it.
  Spec.it s "CR 702.18a an opponent's Doom Blade cannot target Blurred Mongoose" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (mongooseId, pikerId, gs) = restrictionBoard mongoose piker S.bob
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) legal) "the Piker beside it is a legal target"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature mongooseId) legal)) "the Mongoose is not"

  -- THE DISCRIMINATOR between shroud and hexproof. CR 702.11b's hexproof says
  -- "spells or abilities YOUR OPPONENTS control"; CR 702.18a's shroud says no
  -- such thing, so the same board with the same Doom Blade answers the same way
  -- when the Mongoose's own controller is the one aiming it. An implementation
  -- that compared controllers would pass the case above and fail this one.
  Spec.it s "CR 702.18a shroud is not hexproof: the Mongoose's own controller cannot target it either" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (mongooseId, pikerId, gs) = restrictionBoard mongoose piker S.alice
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) legal) "alice may target her own Piker"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature mongooseId) legal)) "but not her own Mongoose"

  -- CR 702.18a's OTHER half: "This permanent OR PLAYER can't be the target of
  -- spells or abilities." Ivory Mask ("You have shroud") is the producer, and a
  -- player has no keywords to carry it -- rule 702's keywords live on objects
  -- and go through the CR 613.1-613.7 layers, which compute an object's
  -- characteristics and nothing else. It rides the CR 613.10/613.11 player axis
  -- instead, as PlayerEffect.CantBeTargetedBy.
  --
  -- Lightning Bolt's "any target" is CR 115.4, which is what puts a player in a
  -- target slot's candidate set at all.
  Spec.it s "CR 702.18a Ivory Mask's controller cannot be targeted, by anyone" $ do
    ivoryMask <- S.printingOf s registry "Ivory Mask"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let bare = Setup.emptyGame S.bothPlayers
        (_, masked) = S.addPermanent ivoryMask S.bob bare
    case S.spellTargetSlot bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSlot -> do
        let legalFor who = Target.legalRecipients (Just who) S.noSource theSlot
        -- The control twin first: without the Mask on the board, bob is an
        -- ordinary CR 115.4 candidate, so the Mask is what removes him.
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) (legalFor S.alice bare)) "without the Mask, alice may bolt bob"
        Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.bob) (legalFor S.alice masked))) "with it, she may not"
        -- Shroud, not hexproof: CR 702.18a names no player, so it stops bob too.
        -- This is the assertion that an Opponents-scoped implementation fails.
        Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.bob) (legalFor S.bob masked))) "and bob cannot bolt himself either"
        -- Scoped to its controller: the Mask says "YOU have shroud", so alice is
        -- untouched. A whole-table implementation fails here.
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) (legalFor S.bob masked)) "alice, with no Mask, is still a legal target"

  -- THE DISCRIMINATOR for the player halves, the twin of the Mongoose pair
  -- above. CR 702.11c: "'Hexproof' on a player means 'You can't be the target of
  -- spells or abilities your opponents control.'" Leyline of Sanctity is the
  -- producer, and the ONLY thing that differs from Ivory Mask is the scope --
  -- which is why the two share one constructor carrying a PlayerScope rather
  -- than getting one apiece. An implementation that ignored the payload would
  -- pass one of these two tests and fail the other, whichever way it defaulted.
  Spec.it s "CR 702.11c Leyline of Sanctity stops an opponent, but not its own controller" $ do
    leyline <- S.printingOf s registry "Leyline of Sanctity"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, warded) = S.addPermanent leyline S.bob (Setup.emptyGame S.bothPlayers)
    case S.spellTargetSlot bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSlot -> do
        let legalFor who = Target.legalRecipients (Just who) S.noSource theSlot warded
        Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.bob) (legalFor S.alice))) "alice, his opponent, may not bolt bob"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) (legalFor S.bob)) "but bob may bolt himself -- hexproof names only opponents"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) (legalFor S.bob)) "and alice is targetable as ever"

  -- The GAMEPLAY-level proof for both cards, which the legality reads above are
  -- not: a Lightning Bolt actually cast, with an answerer that aims at bob
  -- whenever the engine offers him. Under the Mask he is never offered, so the
  -- answer falls to alice and SHE takes the 3 -- the second invariant holding,
  -- since the engine is not choosing a target so much as never presenting an
  -- illegal one. The Leyline half is the same cast with the same answerer and the
  -- opposite outcome, because alice IS bob's opponent.
  Spec.it s "CR 702.18a/702.11c a Bolt aimed at a protected player lands elsewhere" $ do
    ivoryMask <- S.printingOf s registry "Ivory Mask"
    leyline <- S.printingOf s registry "Leyline of Sanctity"
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let -- Prefers bob for every slot he is offered in, and falls back to the
        -- lowest candidate otherwise -- so "bob was not offered" is the only way
        -- the damage can land anywhere else.
        prefersBob p = case p of
          Prompt.ChooseTargets _ _ _ sets ->
            S.preferring (== Recipient.ToPlayer S.bob) sets
          _ -> S.identityAnswer p
        castAt guard =
          let base = S.landsInPlay mountain 1
              withGuard = maybe base (\g -> snd (S.addPermanent g S.bob base)) guard
              (ready, boltId) = S.handOne bolt withGuard
              board = ready {GameState.priority = Just S.alice}
           in S.runPure prefersBob board (S.cast S.alice boltId >> Stack.resolveTop)
        unguarded = castAt Nothing
        masked = castAt (Just ivoryMask)
        warded = castAt (Just leyline)
    -- The control: with nothing protecting him, the answerer gets what it asked
    -- for, so the fixture really does aim at bob.
    Spec.assertEqWith s "unguarded, bob takes the Bolt" (S.lifeOf S.bob unguarded) (Just 17)
    Spec.assertEqWith s "and alice is untouched" (S.lifeOf S.alice unguarded) (Just 20)
    -- CR 702.18a: bob was never offered, so the Bolt landed on alice instead.
    Spec.assertEqWith s "under Ivory Mask, bob takes nothing" (S.lifeOf S.bob masked) (Just 20)
    Spec.assertEqWith s "and alice, the only candidate left, takes it" (S.lifeOf S.alice masked) (Just 17)
    -- CR 702.11c: same cast, same answerer, and alice is his opponent.
    Spec.assertEqWith s "under Leyline of Sanctity, bob takes nothing from his opponent" (S.lifeOf S.bob warded) (Just 20)
    Spec.assertEqWith s "and alice takes it instead" (S.lifeOf S.alice warded) (Just 17)

  -- CR 601.2c / 700.2a: with BOTH players masked and no creature on the board,
  -- Lightning Bolt's only slot has no candidate left, so the spell is
  -- UNCASTABLE rather than castable-and-fizzling. Cast.instantSpeed's own
  -- comment used to call this unobservable for Bolt, on the grounds that
  -- AnyTarget always holds a living player; Ivory Mask is what stopped that
  -- being true, so the claim is pinned here rather than left as prose.
  Spec.it s "CR 601.2c a Bolt with every player masked and no creature is uncastable" $ do
    ivoryMask <- S.printingOf s registry "Ivory Mask"
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let base = S.landsInPlay mountain 1
        (_, oneMask) = S.addPermanent ivoryMask S.bob base
        (_, bothMasked) = S.addPermanent ivoryMask S.alice oneMask
        castableIn board =
          let (ready, boltId) = S.handOne bolt board
           in S.castable S.alice boltId (ready {GameState.priority = Just S.alice})
    -- The control twin, one step at a time: with only bob masked, alice is still
    -- a candidate and the Bolt is castable, so it is the SECOND Mask that empties
    -- the slot rather than anything else about the board.
    Spec.assertBool s (castableIn base) "unmasked, the Bolt is castable"
    Spec.assertBool s (castableIn oneMask) "with bob masked, alice is still a candidate"
    Spec.assertBool s (not (castableIn bothMasked)) "with both masked, there is nothing to target and it is uncastable"

  -- CR 702.18a says "spells OR ABILITIES", so the gate cannot live on the cast
  -- path. Prodigal Sorcerer's "{T}: This creature deals 1 damage to any target"
  -- is an activated ability with a Pool.AnyTarget slot (CR 115.4), and the same
  -- legality call serves it.
  Spec.it s "CR 702.18a shroud stops an ability's target too, not only a spell's" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (mongooseId, pikerId, gs) = restrictionBoard mongoose piker S.alice
    case Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace sorcerer)) of
      [theSlot] -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) legal) "the Piker is a legal any-target"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) legal) "so is a player (CR 115.4)"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature mongooseId) legal)) "the Mongoose is not"
      _ -> Spec.assertFailure s "Prodigal Sorcerer should print one ability with one target slot"

  -- The restriction is read off the PROJECTION, not off the printed card, so it
  -- lives in the CR 613 layer system like every other keyword. Humility is
  -- "All creatures lose all abilities and have base power and toughness 1/1"
  -- (CR 613.1f, layer 6), and CR 702.18a's shroud is one of the abilities it
  -- takes: a Humility'd Mongoose is an ordinary green creature and Doom Blade
  -- may target it.
  Spec.it s "CR 613.1f Humility takes shroud away, and the Mongoose becomes targetable" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (mongooseId, _, board) = restrictionBoard mongoose piker S.bob
        humbled = S.withHumility humility board
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature mongooseId) (Target.legalRecipients (Just S.alice) S.noSource theSlot board)))
          "before Humility the Mongoose is untargetable"
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature mongooseId) (Target.legalRecipients (Just S.alice) S.noSource theSlot humbled))
          "under Humility it is a legal target"

  -- CR 115.5: "A spell or ability on the stack is an illegal target for itself."
  -- Cancel's "counter target spell" draws from Pool.Spells with no Filter at
  -- all, so the rule is the ONLY thing that can exclude the Cancel itself --
  -- there is no "another" for a Not IsSource clause to carry.
  --
  -- The second spell is the control: the exclusion has to be the ONE object the
  -- rule names, not an emptied pool.
  Spec.it s "CR 115.5 a Cancel on the stack is not a legal target for itself, though another spell there is" $ do
    island <- S.printingOf s registry "Island"
    cancel <- S.printingOf s registry "Cancel"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    let base = S.landsInPlay island 3
        (victimId, withVictim) = S.spellOnStack mongoose S.bob base
        (cancelId, gs) = S.spellOnStack cancel S.alice withVictim
    case soleTargetSlot (Face.spell (S.combinedFace cancel)) of
      Nothing -> Spec.assertFailure s "Cancel should declare one target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) cancelId theSlot gs
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToObject cancelId) legal))
          "CR 115.5: the Cancel is an illegal target for itself"
        Spec.assertBool
          s
          (Set.member (Recipient.ToObject victimId) legal)
          "and the OTHER spell on the stack is still legal"

  -- The counterweight, and the reason CR 115.5's gate is "is the source on the
  -- STACK" rather than "is the candidate the source": rule 115.5 speaks of the
  -- object ON THE STACK, which for an activated ability is the ability, not the
  -- permanent it was activated from. Prodigal Sorcerer's "{T}: This creature
  -- deals 1 damage to any target" may therefore still name the Sorcerer, which
  -- is the reading Target.legalSets' own note takes for CR 601.2c's "another"
  -- (a slot that excludes its source says so with Not IsSource).
  Spec.it s "CR 115.5 does not stop Prodigal Sorcerer's ability targeting its own source" $ do
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (sorcererId, gs) = S.addPermanent sorcerer S.alice (Setup.emptyGame S.bothPlayers)
    case Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace sorcerer)) of
      [theSlot] ->
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature sorcererId) (Target.legalRecipients (Just S.alice) sorcererId theSlot gs))
          "the Sorcerer is a legal target of its own ability"
      _ -> Spec.assertFailure s "Prodigal Sorcerer should print one ability with one target slot"

  -- CR 608.2b: "If the spell or ability specifies targets, it checks whether the
  -- targets are still legal. ... If all its targets, for every instance of the
  -- word 'target,' are now illegal, the spell or ability doesn't resolve."
  -- Shroud has to be asked at BOTH of CR 115's moments, and this is the second
  -- one -- the target was legal when CR 601.2c chose it.
  --
  -- No card in this pool GRANTS shroud, so the grant is a stored layer-6
  -- continuous effect (S.withEffect), the shape ColorSpec and ProjectionSpec use
  -- for the same reason. Both halves run off one board and one cast, so the
  -- fizzle cannot be a Doom Blade that never worked.
  Spec.it s "CR 608.2b Doom Blade fizzles when its target gains shroud in response" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let base = S.landsInPlay swamp 2
        (pikerId, board) = S.addPermanent piker S.bob base
        (gs, dbId) = S.handOne doomBlade board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dbId))
        resolve g = snd (Engine.runGamePure S.identityAnswer g Stack.resolveTop)
        killed = resolve cast
        fizzled = resolve (S.withEffect pikerId (Modification.GainKeyword Keyword.Shroud) cast)
    Spec.assertEqWith s "untouched, the Piker dies" (S.creaturesInPlay S.bob killed) 0
    Spec.assertEqWith s "shrouded in response, it survives" (S.creaturesInPlay S.bob fizzled) 1
    Spec.assertEqWith s "and Doom Blade is in alice's graveyard either way" (length (Game.zoneMembers Zone.Graveyard S.alice fizzled)) 1

  -- CR 303.4c asks whether an Aura enchants "an illegal object or player as
  -- defined by its enchant ability and other applicable effects" -- which is NOT
  -- a targeting question. Rule 702 says so itself: protection states both halves
  -- separately (CR 702.16b targeting, CR 702.16c "can't be enchanted by Auras
  -- ... put into their owners' graveyards as a state-based action"), while
  -- shroud (CR 702.18) and hexproof (CR 702.11) state only the targeting one.
  --
  -- Setessan Training is the Aura that proves it: "Enchant creature you control"
  -- carries a Filter, so Pawl.Engine.Sba.stillLegalEnchant cannot answer it from
  -- the pre-pass projection and falls through to the general path. Fusing the
  -- restriction into that path buries this Aura.
  Spec.it s "CR 303.4c an Aura stays attached to a creature that gains shroud" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    setessanTraining <- S.printingOf s registry "Setessan Training"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, g1) = S.addPermanent piker S.alice gs0
        (auraId, g2) = S.addPermanent setessanTraining S.alice g1
        attached = S.attach auraId pikerId g2
        shrouded = S.withEffect pikerId (Modification.GainKeyword Keyword.Shroud) attached
    Spec.assertBool s (Set.member auraId (GameState.battlefield (S.settleSba attached))) "the Aura stays put with no shroud around"
    Spec.assertBool s (Set.member auraId (GameState.battlefield (S.settleSba shrouded))) "and stays put once its host has shroud"

  -- CR 702.11b: "'Hexproof' on a permanent means 'This permanent can't be the
  -- target of spells or abilities your opponents control.'" Slippery Bogle's
  -- entire printed rules text is the keyword, so a case that uses this printing
  -- is asking about 702.11b and nothing else.
  --
  -- BOTH HALVES ON ONE BOARD, with one target slot and one Doom Blade, because the
  -- controller axis IS the rule: an implementation that reused shroud's gate
  -- passes the opponent half and fails the controller half, and one that
  -- inverted the comparison does the reverse. Neither can pass this case.
  --
  -- Doom Blade is "target nonblack creature" and the Bogle is green and blue
  -- (CR 202.2: "an object is the color or colors of the mana symbols in its mana
  -- cost"; CR 107.4e: "a hybrid mana symbol is all of its component colors"), so
  -- the Filter admits it and only the restriction can remove it.
  Spec.it s "CR 702.11b an opponent's Doom Blade cannot target Slippery Bogle, but its own controller's can" $ do
    bogle <- S.printingOf s registry "Slippery Bogle"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (bogleId, pikerId, gs) = restrictionBoard bogle piker S.alice
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let mine = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
            theirs = Target.legalRecipients (Just S.bob) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature bogleId) mine) "alice may target her own Bogle"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature bogleId) theirs)) "bob may not, and that is the whole of CR 702.11b"
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) mine) "the Piker beside it is a legal target for alice"
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) theirs) "and for bob"

  -- CR 702.11b says "spells or ABILITIES your opponents control", so the axis is
  -- read for an ability too, and off the ability's own controller: CR 113.8 fixes
  -- that as "the player who activated it", which is the perspective passed here.
  -- Prodigal Sorcerer's "{T}: This creature deals 1 damage to any target" is the
  -- ability, and the same legality call serves it.
  Spec.it s "CR 702.11b the controller axis holds for an ability, not only a spell" $ do
    bogle <- S.printingOf s registry "Slippery Bogle"
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (bogleId, pikerId, gs) = restrictionBoard bogle piker S.alice
    case Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace sorcerer)) of
      [theSlot] -> do
        let mine = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
            theirs = Target.legalRecipients (Just S.bob) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature bogleId) mine) "alice's own ability may point at her Bogle"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature bogleId) theirs)) "bob's may not"
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) theirs) "though bob's may point at the Piker"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) theirs) "and at a player (CR 115.4)"
      _ -> Spec.assertFailure s "Prodigal Sorcerer should print one ability with one target slot"

  -- Hexproof is read off the PROJECTION and not off the printed card, so it
  -- lives in the CR 613 layer system like every other keyword. Humility is "All
  -- creatures lose all abilities and have base power and toughness 1/1" (CR
  -- 613.1f, layer 6), and rule 702.11a's static ability is one of the abilities
  -- it takes: a Humility'd Bogle is an ordinary 1/1 and an opponent's Doom Blade
  -- may target it.
  Spec.it s "CR 613.1f Humility takes hexproof away, and the Bogle becomes targetable by its opponent" $ do
    bogle <- S.printingOf s registry "Slippery Bogle"
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (bogleId, _, board) = restrictionBoard bogle piker S.bob
        humbled = S.withHumility humility board
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature bogleId) (Target.legalRecipients (Just S.alice) S.noSource theSlot board)))
          "before Humility alice cannot target bob's Bogle"
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature bogleId) (Target.legalRecipients (Just S.alice) S.noSource theSlot humbled))
          "under Humility it is a legal target"

  -- CR 702.11d: "'Hexproof from [quality]' on a permanent means 'This permanent
  -- can't be the target of [quality] spells your opponents control or abilities
  -- your opponents control from [quality] sources.'" The variant narrows CR
  -- 702.11b by the SOURCE's characteristics, which no other targeting question in
  -- pawl asks -- Target.legalRecipients holds the source for CR 601.2c's
  -- "another" and never looked at what it is.
  --
  -- SIX ANSWERS OFF ONE BOARD, and the shape is what makes them discriminating:
  -- the same Goblin Piker and the same two committed spells throughout, with only
  -- the QUALITY mutated between the rows. Doom Blade is black and Angelic Edict
  -- is white, so each spell is stopped in exactly one row and legal in the other
  -- two. An implementation that never admitted the Piker at all fails the legal
  -- cells; one that ignored the quality and read the variant as plain hexproof
  -- fails the legal cells of the two Just rows; one that read the CANDIDATE's
  -- colour rather than the source's fails every stopped cell, the Piker being red
  -- (CR 202.2, {2}{R}).
  --
  -- Each spell is a REAL object on the stack, which is what makes the source
  -- readable at all: S.noSource names no object, so its view carries no colour
  -- and every quality would be vacuously unmatched -- the whole case would pass
  -- for the wrong reason.
  --
  -- SYNTHETIC GRANT BY CHOICE, not for want of a printing: Knight of Grace is
  -- committed and gets its own case below. What no printed card can do is what
  -- this case needs -- MUTATE the quality across six rows off one board, the
  -- Piker's colour and both spells held fixed while only the ability moves -- so
  -- the ability arrives as a stored layer-6 continuous effect, exactly as the CR
  -- 608.2b case below grants plain hexproof. The granted shape is real Magic:
  -- Skrelv, Defector Mite and Sungold Sentinel both grant one, and both of them
  -- choose a colour first, which pawl cannot prompt for.
  Spec.it s "CR 702.11d hexproof from black stops an opponent's black spell and admits their white one" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let (pikerId, board) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        withHexproof quality = S.withEffect pikerId (Modification.GainKeyword (Keyword.Hexproof quality)) board
    case (S.spellTargetSlot doomBlade, S.spellTargetSlot angelicEdict) of
      (Just blackSlot, Just whiteSlot) -> do
        -- Doom Blade's pool is Creatures and Angelic Edict's is Permanents, so
        -- the same Piker is tagged differently in the two sets (CR 115).
        let reaches printing theSlot tag gs =
              let (spellId, onStack) = S.spellOnStack printing S.alice gs
               in Set.member (tag pikerId) (Target.legalRecipients (Just S.alice) spellId theSlot onStack)
            blackReaches = reaches doomBlade blackSlot Recipient.ToCreature
            whiteReaches = reaches angelicEdict whiteSlot Recipient.ToObject
            fromBlack = withHexproof (Just (Filter.Type.HasColor Color.Black))
            fromWhite = withHexproof (Just (Filter.Type.HasColor Color.White))
        Spec.assertBool s (blackReaches board) "with no hexproof at all, alice's Doom Blade reaches bob's Piker"
        Spec.assertBool s (whiteReaches board) "and so does her Angelic Edict"
        Spec.assertBool s (not (blackReaches fromBlack)) "hexproof from black stops the black spell"
        Spec.assertBool s (whiteReaches fromBlack) "and leaves the white one alone -- the half a plain-hexproof reading loses"
        Spec.assertBool s (not (whiteReaches fromWhite)) "mutate the quality to white and the white spell is stopped instead"
        Spec.assertBool s (blackReaches fromWhite) "while the black one reaches again"
        -- CR 702.11b is the same constructor with no quality, and stops both.
        Spec.assertBool s (not (blackReaches (withHexproof Nothing))) "unqualified hexproof stops the black spell"
        Spec.assertBool s (not (whiteReaches (withHexproof Nothing))) "and the white one too"
      _ -> Spec.assertFailure s "Doom Blade and Angelic Edict should each declare a target slot"

  -- CR 702.11d's "your opponents control", which the quality does not replace:
  -- the variant narrows WHICH spells are stopped and leaves CR 702.11b's
  -- controller axis exactly where it was. Bob's own black Doom Blade may still
  -- destroy his own creature with hexproof from black.
  --
  -- ONE BOARD, ONE SPELL, TWO PERSPECTIVES, the way the CR 702.11b case above
  -- reads Slippery Bogle: an implementation that dropped the controller test once
  -- the quality matched would make the Piker untargetable by everybody, and one
  -- that dropped the quality test would read the variant as CR 702.11b's plain
  -- hexproof, which is far too strong. Neither passes both assertions.
  --
  -- The grant stays synthetic so this reads off the SAME Piker board as the case
  -- above, with the controller axis as the only thing that moved between them.
  -- Knight of Grace makes the same "your opponents control" assertion off a real
  -- printing below.
  Spec.it s "CR 702.11d hexproof from black does not stop its own controller's black spell" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (pikerId, board) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        guarded = S.withEffect pikerId (Modification.GainKeyword (Keyword.Hexproof (Just (Filter.Type.HasColor Color.Black)))) board
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let (spellId, onStack) = S.spellOnStack doomBlade S.bob guarded
            reaches caster = Set.member (Recipient.ToCreature pikerId) (Target.legalRecipients (Just caster) spellId theSlot onStack)
        Spec.assertBool s (reaches S.bob) "bob may aim his own Doom Blade at his own Piker (CR 702.11d, 'your opponents control')"
        Spec.assertBool s (not (reaches S.alice)) "alice may not aim the same spell at it"

  -- CR 702.11e: "Any effect that causes an object to lose hexproof will cause an
  -- object to lose all 'hexproof from [quality]' abilities."
  --
  -- Humility ("All creatures lose all abilities and have base power and toughness
  -- 1/1", CR 613.1f layer 6) is pawl's only hexproof-remover -- Modification has
  -- GainKeyword and LoseAllAbilities and nothing narrower -- so it is the witness
  -- this rule gets. The rule is really held BY CONSTRUCTION rather than by this
  -- case: the quality rides the Hexproof constructor, so there is no second
  -- keyword for an ability-removing effect to miss, and the CR 613.1f case above
  -- and this one are the same code path with a different payload.
  --
  -- The grant is stamped BEFORE Humility enters, so CR 613.7's timestamp order
  -- puts the removal last within layer 6. Stamped the other way round the grant
  -- would win, which is CR 613.7 working and not this rule failing.
  Spec.it s "CR 702.11e Humility takes hexproof from black away with the rest of the abilities" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (pikerId, board) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        guarded = S.withEffect pikerId (Modification.GainKeyword (Keyword.Hexproof (Just (Filter.Type.HasColor Color.Black)))) board
        humbled = S.withHumility humility guarded
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let reaches gs =
              let (spellId, onStack) = S.spellOnStack doomBlade S.alice gs
               in Set.member (Recipient.ToCreature pikerId) (Target.legalRecipients (Just S.alice) spellId theSlot onStack)
        Spec.assertBool s (not (reaches guarded)) "before Humility alice's Doom Blade cannot reach the Piker"
        Spec.assertBool s (reaches humbled) "under Humility it can, the variant having gone with the rest"

  -- The whole card, through the CAST path rather than as a set membership: CR
  -- 601.2c makes a spell with no legal target for a slot uncastable at all, which
  -- is what Pawl.Engine.Cast.castable asks through Target.fillableModes.
  --
  -- The point this case makes that the set-membership ones cannot: the source
  -- Cast passes is the card IN HAND, not a spell object on the stack, and rule
  -- 702.11d's quality has to be answerable of it. A black card in a hand is black
  -- (CR 202.2), so the two frames agree -- but nothing else here would notice if
  -- they stopped.
  --
  -- Both rows again, so neither answer is a Doom Blade that never worked: the
  -- board differs only in the quality, and the white row goes all the way through
  -- resolution to a dead Piker.
  Spec.it s "CR 601.2c whole card: hexproof from black leaves alice's Doom Blade no legal target, hexproof from white leaves it one" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (pikerId, board) = S.addPermanent piker S.bob (S.landsInPlay swamp 2) -- {1}{B}
        (base, dbId) = S.handOne doomBlade board
        guarded quality = S.withEffect pikerId (Modification.GainKeyword (Keyword.Hexproof (Just (Filter.Type.HasColor quality)))) base
        resolve gs = snd (Engine.runGamePure S.identityAnswer (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dbId))) Stack.resolveTop)
    Spec.assertBool s (S.castable S.alice dbId base) "with no hexproof at all the spell has a legal target"
    Spec.assertBool s (not (S.castable S.alice dbId (guarded Color.Black))) "hexproof from black leaves the black Doom Blade none"
    Spec.assertBool s (S.castable S.alice dbId (guarded Color.White)) "hexproof from white leaves it the same one it had"
    Spec.assertEqWith s "and bob's Piker dies to it" (S.creaturesInPlay S.bob (resolve (guarded Color.White))) 0
    Spec.assertEqWith s "while the black row leaves it alive" (S.creaturesInPlay S.bob (guarded Color.Black)) 1

  -- THE PRINTED CARD, which is what the cases above stand in for: Knight of Grace
  -- ({1}{W} Creature -- Human Knight 2/2, "First strike / Hexproof from black /
  -- This creature gets +1/+0 as long as any player controls a black permanent"),
  -- oracle text verified against Scryfall. Its variant arrives off the card's own
  -- `keywords`, through the codec, with no test-side grant anywhere -- so this is
  -- the case that would notice the wire format and `Keyword.Hexproof`'s payload
  -- disagreeing, which no synthetic grant can.
  --
  -- THREE ANSWERS OFF ONE BOARD, and each is load-bearing:
  --
  --   * alice's black Doom Blade does NOT reach it (CR 702.11d).
  --   * alice's WHITE Angelic Edict does -- without this leg an implementation
  --     that read the variant as CR 702.11b's plain hexproof passes.
  --   * BOB's own black Doom Blade does, bob being the Knight's controller. CR
  --     702.11d stops only what "your opponents control", and reading this off
  --     alice would let "that player" and "an opponent" collapse into each other.
  --
  -- The Knight is itself WHITE (CR 202.2, {1}{W}), which is what makes the first
  -- leg discriminating: an implementation that matched the quality against the
  -- CANDIDATE's colours rather than the source's finds no black on it and admits
  -- the Doom Blade.
  --
  -- Every spell is a REAL object on the stack for the reason the CR 702.11d case
  -- above gives: S.noSource names no object, its view carries no colour, and the
  -- whole group would pass vacuously.
  Spec.it s "CR 702.11d Knight of Grace stops an opponent's black spell, admits their white one, and admits its own controller's black one" $ do
    knight <- S.printingOf s registry "Knight of Grace"
    doomBlade <- S.printingOf s registry "Doom Blade"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    case (S.spellTargetSlot doomBlade, S.spellTargetSlot angelicEdict) of
      (Just blackSlot, Just whiteSlot) -> do
        let (knightId, board) = S.addPermanent knight S.bob (Setup.emptyGame S.bothPlayers)
            -- Doom Blade's pool is Creatures and Angelic Edict's is Permanents,
            -- so the same Knight is tagged differently in the two sets (CR 115).
            reaches printing theSlot tag caster =
              let (spellId, onStack) = S.spellOnStack printing caster board
               in Set.member (tag knightId) (Target.legalRecipients (Just caster) spellId theSlot onStack)
        Spec.assertBool
          s
          (not (reaches doomBlade blackSlot Recipient.ToCreature S.alice))
          "alice's black Doom Blade cannot target bob's Knight of Grace (CR 702.11d)"
        Spec.assertBool
          s
          (reaches angelicEdict whiteSlot Recipient.ToObject S.alice)
          "her white Angelic Edict can -- the half a plain-hexproof reading loses"
        Spec.assertBool
          s
          (reaches doomBlade blackSlot Recipient.ToCreature S.bob)
          "and bob's own black Doom Blade can, CR 702.11d stopping only what his opponents control"
      _ -> Spec.assertFailure s "Doom Blade and Angelic Edict should each declare a target slot"

  -- CR 702.11d read against a spell whose colour the LAYERS gave it, rather than
  -- its printed one. Celestial Dawn ({1}{W}{W} Enchantment, oracle text verified
  -- against Scryfall) says "Nonland permanents you control are white. The same is
  -- true for spells you control and nonland cards you own that aren't on the
  -- battlefield", which the card transcribes as a pair of
  -- Affected.MatchingOffBattlefield sets; the spell half is what this reads, so
  -- alice's GREEN Giant Growth is white while it waits on the stack (CR 105.2)
  -- and the Knight's hexproof then stops it.
  --
  -- Knight of Malice ({1}{B} Creature -- Human Knight 2/2, "First strike /
  -- Hexproof from white / This creature gets +1/+0 as long as any player controls
  -- a white permanent"), oracle text verified against Scryfall. Its variant
  -- arrives off the card's own `keywords` through the codec, and the Knight stays
  -- BLACK on both boards, which is what makes the stopping leg discriminating: an
  -- implementation reading the quality off the candidate finds no white on it and
  -- lets the spell through either way.
  --
  -- Two boards differing in exactly ONE thing, Celestial Dawn's presence, with
  -- both spell-side questions asked on each. The last leg is the falsifier for an
  -- affected set reaching every object instead of the ones its filter names: bob's
  -- Giant Growth is nobody's spell but his, so it stays green.
  --
  -- The BORROWED pair is what separates the sentence's two halves, and why the
  -- card carries two sets rather than one: a spell alice cast off a card BOB owns
  -- is white by "spells you control" and not by "nonland cards you own", CR 405.4's
  -- controller and CR 108.3's owner being different players there. Dire Fleet
  -- Daredevil is the printing that produces that board, driven in Pawl.CastSpec;
  -- here the caster is written onto the stack object, since what these two legs
  -- read off it is one projection and not the cast.
  Spec.it s "CR 105.2/702.11d Celestial Dawn whitens its controller's spell, which Knight of Malice's hexproof then stops" $ do
    knight <- S.printingOf s registry "Knight of Malice"
    dawn <- S.printingOf s registry "Celestial Dawn"
    giantGrowth <- S.printingOf s registry "Giant Growth"
    case S.spellTargetSlot giantGrowth of
      Just slot -> do
        let (bobKnight, board0) = S.addPermanent knight S.bob (Setup.emptyGame S.bothPlayers)
            (aliceKnight, dawnless) = S.addPermanent knight S.alice board0
            dawned = snd (S.addPermanent dawn S.alice dawnless)
            reachesFrom (spellId, onStack) caster victim =
              Set.member (Recipient.ToCreature victim) (Target.legalRecipients (Just caster) spellId slot onStack)
            reaches board caster = reachesFrom (S.spellOnStack giantGrowth caster board) caster
            -- CR 405.4: a spell's controller is the player who cast it, which is
            -- what Object.enteredUnder holds -- Projection.defaultControllerOf
            -- falls back to CR 108.3's owner only in its absence.
            borrowed board victim =
              let (spellId, onStack) = S.spellOnStack giantGrowth S.bob board
                  underAlice = Map.adjust (\o -> o {Object.enteredUnder = Just S.alice}) spellId (GameState.objects onStack)
               in reachesFrom (spellId, onStack {GameState.objects = underAlice}) S.alice victim
        Spec.assertBool
          s
          (reaches dawnless S.alice bobKnight)
          "without Celestial Dawn alice's green Giant Growth reaches bob's Knight of Malice"
        Spec.assertBool
          s
          (not (reaches dawned S.alice bobKnight))
          "with Celestial Dawn on her battlefield her spell is white, and CR 702.11d stops it"
        Spec.assertBool
          s
          (borrowed dawnless bobKnight)
          "without it a Giant Growth alice cast off bob's card reaches his Knight too"
        Spec.assertBool
          s
          (not (borrowed dawned bobKnight))
          "with it that spell is white as well, by the clause naming the spells she CONTROLS"
        Spec.assertBool
          s
          (reaches dawned S.bob aliceKnight)
          "while bob's own Giant Growth is untouched, and reaches alice's Knight"
      _ -> Spec.assertFailure s "Giant Growth should declare a target slot"

  -- CR 702.16b: "A permanent or player with protection can't be targeted by
  -- spells with the stated quality and can't be targeted by abilities from a
  -- source with the stated quality." Apostle of Purifying Light ({1}{W} Creature
  -- -- Human Cleric 2/1, M20, "Protection from black" plus "{2}: Exile target
  -- card from a graveyard"), oracle text verified against Scryfall. The quality
  -- arrives off the card's own `keywords` through the codec, with no test-side
  -- grant anywhere.
  --
  -- THREE ANSWERS OFF ONE BOARD, and the second is the one that tells this rule
  -- from CR 702.11d's:
  --
  --   * alice's black Doom Blade does NOT reach bob's Apostle.
  --   * BOB's own black Doom Blade does not reach it EITHER. Rule 702.16b names
  --     no player where rule 702.11d stops only what "your opponents control", so
  --     this is the leg a hexproof-shaped implementation fails -- and it is the
  --     exact leg the Knight of Grace case above answers the other way, off the
  --     same two spells.
  --   * Her WHITE Angelic Edict does reach it -- without this leg an
  --     implementation that read protection as CR 702.18a's shroud passes.
  --
  -- The Apostle is itself WHITE (CR 202.2, {1}{W}), which is what makes the first
  -- leg discriminating: an implementation matching the quality against the
  -- CANDIDATE's colours rather than the source's finds no black on it and admits
  -- the Doom Blade.
  --
  -- Every spell is a REAL object on the stack for the reason the CR 702.11d cases
  -- above give: S.noSource names no object, its view carries no colour, and the
  -- whole case would pass vacuously.
  Spec.it s "CR 702.16b Apostle of Purifying Light stops a black spell whoever casts it, and admits a white one" $ do
    apostle <- S.printingOf s registry "Apostle of Purifying Light"
    doomBlade <- S.printingOf s registry "Doom Blade"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    case (S.spellTargetSlot doomBlade, S.spellTargetSlot angelicEdict) of
      (Just blackSlot, Just whiteSlot) -> do
        let (apostleId, board) = S.addPermanent apostle S.bob (Setup.emptyGame S.bothPlayers)
            -- Doom Blade's pool is Creatures and Angelic Edict's is Permanents,
            -- so the same Apostle is tagged differently in the two sets (CR 115).
            reaches printing theSlot tag caster =
              let (spellId, onStack) = S.spellOnStack printing caster board
               in Set.member (tag apostleId) (Target.legalRecipients (Just caster) spellId theSlot onStack)
        Spec.assertBool
          s
          (not (reaches doomBlade blackSlot Recipient.ToCreature S.alice))
          "alice's black Doom Blade cannot target bob's Apostle (CR 702.16b)"
        Spec.assertBool
          s
          (not (reaches doomBlade blackSlot Recipient.ToCreature S.bob))
          "and neither can bob's own -- CR 702.16b names no player, unlike CR 702.11d"
        Spec.assertBool
          s
          (reaches angelicEdict whiteSlot Recipient.ToObject S.alice)
          "her white Angelic Edict can -- the half a shroud-shaped reading loses"
      _ -> Spec.assertFailure s "Doom Blade and Angelic Edict should each declare a target slot"

  -- CR 702.16k: "Such a permanent or player can't be targeted by spells or
  -- abilities the specified player controls." True-Name Nemesis, whose quality is
  -- Filter.OfChosenPlayer -- answered off Filter.Context.carrierChosenPlayer,
  -- which Pawl.Engine.Target.targetable fills off the CANDIDATE rather than off
  -- the aiming source, that being where rule 702.16b's ability lives.
  --
  -- THREE SEATS and a pair of boards differing only in WHOM the Nemesis chose:
  -- two would collapse "the chosen player" onto alice, and the refusal would pass
  -- under a hard-coded "an opponent" as happily as under the field. carol never
  -- acts; she is the seat the second row names.
  --
  -- A Hill Giant (3/3, no abilities) stands beside the Nemesis so the refusal
  -- cannot pass for a spell that reaches nothing at all, and Murder's pool is
  -- Creatures unfiltered, so no printed criterion of its own excludes a 3/1
  -- Merfolk.
  --
  -- The choice is stamped rather than cast for; the entry road that writes it is
  -- proved by Pawl.DamageSpec's True-Name Nemesis group.
  Spec.it s "CR 702.16k alice's Murder cannot target a Nemesis that chose her, and can one that chose carol" $ do
    nemesis <- S.printingOf s registry "True-Name Nemesis"
    giant <- S.printingOf s registry "Hill Giant"
    murder <- S.printingOf s registry "Murder"
    case S.spellTargetSlot murder of
      Just slot -> do
        let (nemesisId, board0) = S.addPermanent nemesis S.bob S.threePlayerGame
            (giantId, board) = S.addPermanent giant S.bob board0
            chose who = board {GameState.objects = Map.adjust (\o -> o {Object.chosenPlayer = Just who}) nemesisId (GameState.objects board)}
            reaches gs victim =
              let (spellId, onStack) = S.spellOnStack murder S.alice gs
               in Set.member (Recipient.ToCreature victim) (Target.legalRecipients (Just S.alice) spellId slot onStack)
        Spec.assertBool s (not (reaches (chose S.alice) nemesisId)) "CR 702.16k alice was chosen, so her Murder cannot target the Nemesis"
        Spec.assertBool s (reaches (chose S.carol) nemesisId) "and with carol chosen instead the same Murder reaches it"
        Spec.assertBool s (reaches (chose S.alice) giantId) "while the Hill Giant beside it admits the Murder in the refusing row too"
      Nothing -> Spec.assertFailure s "Murder should declare a target slot"

  -- The same sentence read the other way round: rule 702.16k's targeting clause
  -- names the controller of the SPELL OR ABILITY, not the controller of the
  -- object it comes from, and CR 113.8 with CR 109.5 fix an activated ability's
  -- controller as the player who activated it. So stealing the source in
  -- response does not hand the ability to the thief, and CR 608.2b's re-check
  -- must still judge it against the player who activated it. An implementation
  -- matching the quality against the source OBJECT -- which is what
  -- Filter.OfChosenPlayer reads when no aimer is supplied -- finds the thief on
  -- it and counters the ability instead.
  --
  -- THREE SEATS, the case above's reason and then one more: carol activates, bob
  -- steals and is the seat the Nemesis chose, and alice owns the Nemesis, so no
  -- one player holds two of the roles.
  --
  -- SALTFIELD RECLUSE ({2}{W} Creature -- Human Rebel Cleric 1/2, "{T}: Target
  -- creature gets -2/-0 until end of turn") rather than a pinger: rule 702.16k's
  -- DAMAGE clause names the source's controller, so a ping off the stolen
  -- permanent would be prevented whatever this rule answers and the two readings
  -- could not be told apart. Ray of Command ({3}{U} Instant, "Untap target
  -- creature an opponent controls and gain control of it until end of turn. That
  -- creature gains haste until end of turn") is the steal, cast in RESPONSE so
  -- that CR 115's two moments see two different controllers. Both Oracle texts
  -- checked against Scryfall on 2026-09-15.
  --
  -- A PAIR OF BOARDS differing only in whether the Ray was cast, so the
  -- weakening cannot be an ability that never worked, and a last row where bob
  -- holds the Recluse himself -- the direction rule 702.16k does refuse.
  Spec.it s "CR 702.16k a Saltfield Recluse stolen in response still weakens the Nemesis that chose the thief" $ do
    nemesis <- S.printingOf s registry "True-Name Nemesis"
    recluse <- S.printingOf s registry "Saltfield Recluse"
    rayOfCommand <- S.printingOf s registry "Ray of Command"
    island <- S.printingOf s registry "Island"
    case Face.activatedAbilities (S.combinedFace recluse) of
      [ability] | Just theSlot <- soleTargetSlot (ActivatedAbility.modal ability) -> do
        let (nemesisId, board0) = S.addPermanent nemesis S.alice S.threePlayerGame
            chose = board0 {GameState.objects = Map.adjust (\o -> o {Object.chosenPlayer = Just S.bob}) nemesisId (GameState.objects board0)}
            (recluseId, board1) = S.addPermanent recluse S.carol chose
            (hisRecluseId, board2) = S.addPermanent recluse S.bob board1
            (rayId, board) = S.addHandCard rayOfCommand S.bob (S.landsFor island S.bob 4 board2)
            -- One activation, two futures: only the stolen row casts the Ray in
            -- response, so the boards differ in that and nothing else.
            activated = S.runPure (aimAtOffered nemesisId) (board {GameState.priority = Just S.carol}) (Activate.activateAbility S.carol recluseId ability)
            stolen = S.runPure (aimAtOffered recluseId) (activated {GameState.priority = Just S.bob}) (S.cast S.bob rayId Monad.>> Stack.resolveTop)
            resolve gs = S.runPure S.identityAnswer gs Stack.resolveTop
        Spec.assertEqWith
          s
          "CR 702.16k/113.8 the ability is still carol's, so the stolen Recluse weakens the Nemesis that chose bob"
          (S.powerToughnessOf nemesisId (resolve stolen))
          (Just (1, 1))
        Spec.assertEqWith s "and the Ray really moved the Recluse to bob" (Projection.controllerOf recluseId stolen) (Just S.bob)
        Spec.assertEqWith
          s
          "while with no Ray cast the same ability weakens it too, so the row above is not an ability that never worked"
          (S.powerToughnessOf nemesisId (resolve activated))
          (Just (1, 1))
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature nemesisId) (Target.legalRecipients (Just S.bob) hisRecluseId theSlot board)))
          "and a Recluse bob activates himself may not aim at the Nemesis that chose him"
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature recluseId) (Target.legalRecipients (Just S.bob) hisRecluseId theSlot board))
          "though carol's Recluse beside it is a legal target for his, so that refusal is not an empty slot"
      _ -> Spec.assertFailure s "Saltfield Recluse should print one activated ability with one target slot"

  -- CR 702.16j: "A permanent or player with protection from everything has
  -- protection from each object regardless of that object's characteristic
  -- values." Progenitus ({W}{W}{U}{U}{B}{B}{R}{R}{G}{G} Legendary Creature --
  -- Hydra Avatar 10/10), oracle text verified against Scryfall. The variant needs
  -- no second keyword: the quality is Filter.And [], which every object
  -- satisfies, so all four of rule 702.16's prohibitions read it as they read a
  -- colour.
  --
  -- FOUR ANSWERS OFF ONE BOARD, in two pairs differing only in which of bob's two
  -- creatures the spell is aimed at. A Hill Giant (3/3, no abilities) stands
  -- beside Progenitus so neither negative can pass for a spell that reaches
  -- nothing at all, and the two spells differ in COLOUR -- alice's black Murder
  -- and her white Angelic Edict -- which is what tells this quality from any
  -- single-colour one: an implementation reading Progenitus as protection from
  -- one colour admits the other spell.
  --
  -- MURDER and not the Doom Blade the CR 702.16b case above uses: Progenitus is
  -- itself black (CR 202.2), so "target nonblack creature" could never aim at it
  -- and that leg would pass with no protection anywhere. Murder's pool is
  -- Creatures unfiltered.
  Spec.it s "CR 702.16j Progenitus's protection from everything stops a black spell and a white one alike" $ do
    progenitus <- S.printingOf s registry "Progenitus"
    giant <- S.printingOf s registry "Hill Giant"
    murder <- S.printingOf s registry "Murder"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    case (S.spellTargetSlot murder, S.spellTargetSlot angelicEdict) of
      (Just blackSlot, Just whiteSlot) -> do
        let (progenitusId, board0) = S.addPermanent progenitus S.bob (Setup.emptyGame S.bothPlayers)
            (giantId, board) = S.addPermanent giant S.bob board0
            -- Murder's pool is Creatures and Angelic Edict's is Permanents, so
            -- the same creature is tagged differently in the two sets (CR 115).
            reaches printing theSlot tag victim =
              let (spellId, onStack) = S.spellOnStack printing S.alice board
               in Set.member (tag victim) (Target.legalRecipients (Just S.alice) spellId theSlot onStack)
        Spec.assertBool
          s
          (not (reaches murder blackSlot Recipient.ToCreature progenitusId))
          "CR 702.16j alice's black Murder cannot target bob's Progenitus"
        Spec.assertBool
          s
          (not (reaches angelicEdict whiteSlot Recipient.ToObject progenitusId))
          "and neither can her white Angelic Edict -- the leg a single-colour quality loses"
        Spec.assertBool
          s
          (reaches murder blackSlot Recipient.ToCreature giantId)
          "while the Hill Giant beside it admits the Murder"
        Spec.assertBool
          s
          (reaches angelicEdict whiteSlot Recipient.ToObject giantId)
          "and the Angelic Edict too, so neither refusal above is a spell that reaches nothing"
      _ -> Spec.assertFailure s "Murder and Angelic Edict should each declare a target slot"

  -- CR 602.2a creates an activated ability on the stack BEFORE CR 602.2b routes
  -- the rest of the activation through CR 601.2b-i, so CR 601.2c's targets are
  -- chosen in a state that already holds the ability. Activate.activateAbility
  -- chooses them against the PRE-MINT snapshot instead. The two states differ by
  -- exactly the ability object, and CR 115.5 -- "a spell or ability on the stack
  -- is an illegal target for itself" -- subtracts that object anyway, so no board
  -- tells them apart and this case cannot prove which one pawl reads.
  --
  -- What it does prove is the HALF-correction wrong, and that is why it is here:
  -- legalRecipientsGiven's CR 115.5 gate is `source` being on the stack, and on
  -- this path `source` is the source PERMANENT (see its own note). Moving this
  -- caller to the post-mint state without also handing that function the
  -- ability's own id offers the ability itself, and the answerer below takes it.
  --
  -- Adric, Mathematical Genius' "Ultimate Sacrifice -- {1}{U}, Sacrifice Adric:
  -- Counter target activated or triggered ability" is Stifle's undifferentiated
  -- Pool.Abilities on an ACTIVATED ability, which is what makes the path
  -- reachable at all. Its other ability copies one instead, so the card declares
  -- two and this case names the one it wants by the component that separates them
  -- -- the sacrifice.
  -- "Ultimate Sacrifice" is an ability word (CR 207.2c) and Doctor's companion is
  -- deck construction (CR 903); neither has a rules meaning in play.
  --
  -- The answerer takes the LARGEST recipient, and Recipient's derived Ord orders
  -- ToObject by ObjectId while Game.freshObjectId hands out increasing ones -- so
  -- it names the newest object on the stack the moment it is ever offered one.
  -- bob's Prodigal Sorcerer ability is aimed at ALICE, so whether it was
  -- countered is readable as her life total rather than as a stack length, which
  -- both readings leave empty.
  Spec.it s "CR 602.2a/115.5 Adric's ability is not offered its own object among the abilities it may counter" $ do
    island <- S.printingOf s registry "Island"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    adric <- S.printingOf s registry "Adric, Mathematical Genius"
    case (soleActivatedAbility sorcerer, activatedAbilityCosting CostComponent.SacrificeThis adric) of
      (Just ping, Just ultimateSacrifice) -> do
        let (srcId, withSorcerer) = S.addPermanent sorcerer S.bob (Setup.emptyGame S.bothPlayers)
            -- CR 302.6: the Sorcerer must have settled before its {T} is legal.
            -- Adric needs no such thing -- its cost carries no {T}.
            settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.bob)
            (adricId, withAdric) = S.addPermanent adric S.alice settled
            (_, withIsland) = S.addPermanent island S.alice withAdric
            (_, withIslands) = S.addPermanent island S.alice withIsland
            atAlice :: Prompt.Prompt r -> r
            atAlice p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
              _ -> S.identityAnswer p
            pinging = S.runPure atAlice (withIslands {GameState.priority = Just S.bob}) (Activate.activateAbility S.bob srcId ping)
            takeNewest :: Prompt.Prompt r -> r
            takeNewest p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> maybe Set.empty Set.singleton (Set.lookupMax legal)) sets
              _ -> S.identityAnswer p
            countering = S.runPure takeNewest (pinging {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice adricId ultimateSacrifice)
            after = S.runPure S.identityAnswer (S.runPure S.identityAnswer countering Stack.resolveTop) Stack.resolveTop
        case GameState.stack pinging of
          [pingId] -> do
            -- The discriminating pair, stated first: countering the ping is the
            -- only way alice's life stays 20. An ability that had been offered
            -- itself would have taken itself instead, and the ping would have
            -- resolved.
            Spec.assertEqWith s "alice's life is untouched: Adric's ability countered the ping (CR 701.6a)" (S.lifeOf S.alice after) (Just 20)
            Spec.assertEqWith s "and no damage was ever dealt" (fmap DamageEvent.amount (Maybe.mapMaybe Event.damageOf (S.eventsOf after))) []
            -- Supporting, not discriminating: an ability leaves the stack
            -- whether it resolved (CR 608.2n) or was countered (CR 701.6a), so
            -- this and the two below hold under both readings. They are here to
            -- stop a green that came from the activation never happening.
            Spec.assertEqWith s "the ping is no longer an object at all" (Game.lookupObject pingId after) Nothing
            Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
            Spec.assertEqWith s "Adric is in alice's graveyard: the sacrifice was paid" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
            -- Legibility, and last so it can absorb no mutation the assertions
            -- above should catch: the pool the announcement was made against held
            -- the ping alone.
            case soleTargetSlot (ActivatedAbility.modal ultimateSacrifice) of
              Nothing -> Spec.assertFailure s "Ultimate Sacrifice should declare one target slot"
              Just theSlot -> Spec.assertEqWith s "and the pool held the ping and nothing else" (Target.legalRecipients (Just S.alice) adricId theSlot pinging) (Set.singleton (Recipient.ToObject pingId))
          _ -> Spec.assertFailure s "the fixture should put exactly one ability on the stack"
      _ -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability, and Adric one whose cost sacrifices it"

  -- CR 115.2's OTHER escape hatch, the one Pool.Spells and Pool.Abilities are
  -- not: "only permanents are legal targets for spells and abilities, unless a
  -- spell or ability (a) SPECIFIES THAT IT CAN TARGET AN OBJECT IN ANOTHER ZONE
  -- or a player". Raise Dead's "target creature card in your graveyard" is that
  -- clause, and its pool is Pool.CardsInGraveyard.
  --
  -- Three ways it could go wrong, on ONE board, because each is a different
  -- mistake and any of them alone would pass against a pool that stayed empty:
  --
  --   * CR 400.1's per-player zone -- "each player has their own library, hand,
  --     and graveyard" -- is why the pool carries a ZoneScope at all, and
  --     bob's copy of the very same card is what proves the axis is real rather
  --     than decorative. It cannot be a Filter: CR 108.4 says "a card doesn't
  --     have a controller unless that card represents a permanent or spell", so
  --     ControlledBy is vacuously False for every card in every graveyard.
  --   * the Filter still narrows, so alice's Lightning Bolt is out.
  --   * the pool is DISJOINT from Pool.Creatures, the way Pool.Abilities is from
  --     Pool.Spells: the Piker on the battlefield is offered under neither tag.
  --     CR 109.2's battlefield default does not reach this card, because its text
  --     says the word "card" outright.
  Spec.it s "CR 115.2 clause (a) Raise Dead reaches the creature card in your graveyard and nothing else" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (inPlayId, g1) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (mineId, g2) = S.addGraveyardCard piker S.alice g1
        (myBoltId, g3) = S.addGraveyardCard bolt S.alice g2
        (theirsId, gs) = S.addGraveyardCard piker S.bob g3
    case S.spellTargetSlot raiseDead of
      Nothing -> Spec.assertFailure s "Raise Dead should declare a target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToObject mineId) legal) "the creature card in alice's own graveyard is legal"
        Spec.assertBool s (not (Set.member (Recipient.ToObject theirsId) legal)) "the identical card in bob's graveyard is not (CR 400.1)"
        Spec.assertBool s (not (Set.member (Recipient.ToObject myBoltId) legal)) "nor is the instant card beside it (the Filter narrows)"
        Spec.assertBool s (not (Set.member (Recipient.ToObject inPlayId) legal)) "nor the Piker on the battlefield, under ToObject"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature inPlayId) legal)) "nor under ToCreature (disjoint from Pool.Creatures)"
        Spec.assertEqWith s "and nothing else at all" legal (Set.singleton (Recipient.ToObject mineId))

  -- CR 400.1's OTHER half. Raise Dead above says "in your graveyard"; Withered
  -- Wretch's "{1}: Exile target card from a graveyard" names no player at all, so
  -- every player's copy of the zone is in the pool at once -- a SET of players
  -- rather than a relation to one.
  --
  -- Raise Dead is read on the SAME graveyards and asserted here, because widening
  -- that axis and DELETING it look identical from the Wretch's side alone: a pool
  -- that had stopped asking whose graveyard it was reading would satisfy every
  -- assertion about the Wretch below and quietly hand Raise Dead bob's graveyard
  -- too.
  --
  -- The ABSENT Filter is the second claim. "Target card" carries no card type, so
  -- the Lightning Bolt card is as legal as the Piker card beside it; Raise Dead's
  -- HasCardType Creature is what makes that contrast a real one rather than a
  -- vacuous one. CR 109.2's battlefield default is switched off for both by the
  -- printed word "card", so neither slot reaches the Piker on the battlefield.
  Spec.it s "CR 115.2 clause (a) Withered Wretch reaches every graveyard, and Raise Dead still reaches only alice's" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    wretch <- S.printingOf s registry "Withered Wretch"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (inPlayId, g1) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (mineId, g2) = S.addGraveyardCard piker S.alice g1
        (myBoltId, g3) = S.addGraveyardCard bolt S.alice g2
        (theirsId, gs) = S.addGraveyardCard piker S.bob g3
        wretchSlots = Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace wretch))
    case (wretchSlots, S.spellTargetSlot raiseDead) of
      ([wretchSlot], Just raiseDeadSlot) -> do
        let legal theSlot = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertEqWith
          s
          "the Wretch offers every card in every graveyard, of every card type, and nothing else"
          (legal wretchSlot)
          (Set.fromList (fmap Recipient.ToObject [mineId, myBoltId, theirsId]))
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature inPlayId) (legal wretchSlot)))
          "and not the Piker on the battlefield under ToCreature either (disjoint from Pool.Creatures)"
        Spec.assertEqWith
          s
          "while Raise Dead, on those same graveyards, still reaches only alice's creature card"
          (legal raiseDeadSlot)
          (Set.singleton (Recipient.ToObject mineId))
      _ -> Spec.assertFailure s "Withered Wretch should print one ability with one target slot, and Raise Dead one slot"

  -- The move itself, activated and resolved through the stack, in BOTH
  -- directions off one board: "a graveyard" is a claim about two candidate sets,
  -- and exiling from your own proves only the half Raise Dead already proved.
  --
  -- CR 406.2: "To exile an object is to put it into the exile zone from whatever
  -- zone it's currently in." Exile is SHARED (CR 400.1: "the other zones are
  -- shared by all players"), so Game.zoneMembers reads it per owner rather than
  -- per player's copy -- which is how each assertion below names whose card
  -- moved.
  Spec.it s "CR 406.2 whole card: Withered Wretch exiles a card from alice's graveyard, and from bob's" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    wretch <- S.printingOf s registry "Withered Wretch"
    let (wretchId, g1) = S.addPermanent wretch S.alice (S.landsInPlay swamp 1)
        (mineId, g2) = S.addGraveyardCard piker S.alice g1
        (theirsId, g3) = S.addGraveyardCard piker S.bob g2
        board = g3 {GameState.priority = Just S.alice}
    case Face.activatedAbilities (S.combinedFace wretch) of
      [ability] -> do
        let exiling oid = S.runPure (aimAtCard oid) (S.runPure (aimAtCard oid) board (Activate.activateAbility S.alice wretchId ability)) Stack.resolveTop
            mine = exiling mineId
            theirs = exiling theirsId
        Spec.assertEqWith s "aimed at her own, alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice mine)) 0
        Spec.assertEqWith s "and the exiled card is hers" (length (Game.zoneMembers Zone.Exile S.alice mine)) 1
        Spec.assertEqWith s "with bob's graveyard untouched" (length (Game.zoneMembers Zone.Graveyard S.bob mine)) 1
        Spec.assertEqWith s "aimed at bob's, HIS graveyard is the empty one" (length (Game.zoneMembers Zone.Graveyard S.bob theirs)) 0
        Spec.assertEqWith s "and the exiled card is his" (length (Game.zoneMembers Zone.Exile S.bob theirs)) 1
        Spec.assertEqWith s "with alice's graveyard untouched" (length (Game.zoneMembers Zone.Graveyard S.alice theirs)) 1
      abilities -> Spec.assertFailure s ("expected one activated ability on Withered Wretch, got " <> show (length abilities))

  -- CR 115.2 clause (a)'s SECOND zone. Riftsweeper's "choose target face-up
  -- exiled card" names exile, which CR 400.2 lists among the public zones
  -- ("graveyard, battlefield, stack, exile, ante, and command are public
  -- zones"), so every candidate is visible to the chooser exactly as a
  -- graveyard's is.
  --
  -- Four claims off one board. The set equality is the load-bearing one and
  -- subsumes the second; the others name mistakes it would not, on its own, tell
  -- apart from each other:
  --
  --   * EITHER PLAYER's exiled card is legal. CR 400.1: "the other zones are
  --     shared by all players", so exile has no per-player copy and the pool
  --     carries no scope to select among them. bob's card is what proves that a
  --     player axis has not crept in: a pool that had quietly become
  --     "your exile" would still offer alice's.
  --   * The battlefield Piker is NOT in it, under either tag -- the pool is
  --     disjoint from Pool.Creatures and Pool.Permanents, the relation
  --     Pool.Abilities has to Pool.Spells. CR 109.2's battlefield default is
  --     switched off by the card's own word "card".
  --   * The GRAVEYARD Piker is not in it either. Two off-battlefield pools now
  --     exist and neither may swallow the other.
  --   * Withered Wretch, read on the SAME board, still reaches exactly the
  --     graveyard card and neither exiled one -- the other direction of that
  --     same disjointness, which the Riftsweeper assertions alone cannot see.
  Spec.it s "CR 115.2 clause (a) Riftsweeper reaches every exiled card and nothing else, and Withered Wretch still reaches only the graveyard" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    riftsweeper <- S.printingOf s registry "Riftsweeper"
    wretch <- S.printingOf s registry "Withered Wretch"
    let (inPlayId, g1) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (buriedId, g2) = S.addGraveyardCard piker S.alice g1
        (hersId, g3) = S.addExiledCard piker S.alice g2
        (hisId, gs) = S.addExiledCard bolt S.bob g3
        riftSlots = Maybe.mapMaybe (soleTargetSlot . TriggeredAbility.modal) (Face.triggeredAbilities (S.combinedFace riftsweeper))
        wretchSlots = Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace wretch))
    case (riftSlots, wretchSlots) of
      ([riftSlot], [wretchSlot]) -> do
        let legal theSlot = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertEqWith
          s
          "both exiled cards, hers and his, of both card types, and nothing else"
          (legal riftSlot)
          (Set.fromList (fmap Recipient.ToObject [hersId, hisId]))
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature inPlayId) (legal riftSlot)))
          "not the Piker on the battlefield under ToCreature either (disjoint from Pool.Creatures)"
        Spec.assertEqWith
          s
          "while Withered Wretch, on that same board, still reaches only the graveyard card"
          (legal wretchSlot)
          (Set.singleton (Recipient.ToObject buriedId))
      _ -> Spec.assertFailure s "Riftsweeper should print one triggered ability with one target slot, and Withered Wretch one activated ability with one"

  -- The whole card, through the trigger pipeline and the stack, in BOTH
  -- directions off one board -- because "its owner shuffles it into THEIR
  -- library" is a claim about a player alice does not pick, and aiming only at
  -- her own card would prove nothing about it.
  --
  -- CR 701.24 is the second half of the effect and is asserted by ORDER, under
  -- an interpreter that REVERSES every Prompt.Shuffle. CR 701.24a ("randomize
  -- the cards within it so that no player knows their order") makes a shuffle
  -- unobservable by any other means -- an identity shuffle and no shuffle at all
  -- look the same -- so the reversal is what turns "was it shuffled?" into a
  -- question the test can ask. bob's run is then read out a SECOND time under
  -- the identity interpreter as the control: that pins the arrival at the BOTTOM
  -- (Effect.ShuffleIntoLibrary states no library position, so the move takes
  -- LibraryPosition.defaultValue and Game.insertIntoZone appends), so the
  -- reversed run's leading position cannot be merely where the move put the
  -- card.
  --
  -- alice controls the Riftsweeper in both runs. When the exiled card is bob's,
  -- it is BOB's library that grows and BOB's that is shuffled: Game.insertIntoZone
  -- files a library arrival under Object.owner, and the shuffle is asked of that
  -- same owner -- which is what "its owner shuffles it into THEIR library" means
  -- and what a controller-relative reading would get wrong.
  Spec.it s "CR 701.24 whole card: Riftsweeper shuffles an exiled card into its OWNER's library, hers or his" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    riftsweeper <- S.printingOf s registry "Riftsweeper"
    -- S.addLibraryCard puts each card ON TOP, so the SECOND of each pair is the
    -- one at the head of the library and the first is under it.
    let (_, g1) = S.entersWithTrigger riftsweeper S.alice (Setup.emptyGame S.bothPlayers)
        (herDeeperId, g2) = S.addLibraryCard piker S.alice g1
        (herTopId, g3) = S.addLibraryCard piker S.alice g2
        (hisDeeperId, g4) = S.addLibraryCard bolt S.bob g3
        (hisTopId, g5) = S.addLibraryCard bolt S.bob g4
        (hersId, g6) = S.addExiledCard piker S.alice g5
        (hisId, board) = S.addExiledCard bolt S.bob g6
        runShuffling oid =
          let placed = S.runPure (aimAtCardShuffling oid) board Engine.placePendingTriggers
           in S.runPure (aimAtCardShuffling oid) placed Stack.resolveTop
        runPlain oid =
          let placed = S.runPure (aimAtCard oid) board Engine.placePendingTriggers
           in S.runPure (aimAtCard oid) placed Stack.resolveTop
        shuffledHers = runShuffling hersId
        shuffledHis = runShuffling hisId
        unshuffledHis = runPlain hisId
        -- CR 400.7 mints a fresh id at the destination, so the arrival is
        -- whichever library member was not seeded here.
        arrivalIn pid seeded gs = filter (\oid -> notElem oid seeded) (Game.zoneMembers Zone.Library pid gs)
    Spec.assertEqWith s "aimed at her own, exile holds only bob's card" (Set.size (GameState.exile shuffledHers)) 1
    Spec.assertEqWith s "and ALICE's library grew to three" (length (Game.zoneMembers Zone.Library S.alice shuffledHers)) 3
    Spec.assertEqWith s "with bob's library untouched" (Game.zoneMembers Zone.Library S.bob shuffledHers) [hisTopId, hisDeeperId]
    case arrivalIn S.alice [herTopId, herDeeperId] shuffledHers of
      [arrived] ->
        Spec.assertEqWith
          s
          "CR 701.24a: her library was shuffled, so the reversal shows through"
          (Game.zoneMembers Zone.Library S.alice shuffledHers)
          [arrived, herDeeperId, herTopId]
      _ -> Spec.assertFailure s "exactly one card should have arrived in alice's library"
    Spec.assertEqWith s "aimed at bob's, exile holds only alice's card" (Set.size (GameState.exile shuffledHis)) 1
    Spec.assertEqWith s "and it is BOB's library that grew, not the controller's" (length (Game.zoneMembers Zone.Library S.bob shuffledHis)) 3
    Spec.assertEqWith s "with alice's library untouched" (Game.zoneMembers Zone.Library S.alice shuffledHis) [herTopId, herDeeperId]
    case (arrivalIn S.bob [hisTopId, hisDeeperId] shuffledHis, arrivalIn S.bob [hisTopId, hisDeeperId] unshuffledHis) of
      ([shuffledArrival], [plainArrival]) -> do
        Spec.assertEqWith
          s
          "CR 701.24a: his library was shuffled too"
          (Game.zoneMembers Zone.Library S.bob shuffledHis)
          [shuffledArrival, hisDeeperId, hisTopId]
        Spec.assertEqWith
          s
          "the control: unshuffled, the same card sits at the BOTTOM where the move put it"
          (Game.zoneMembers Zone.Library S.bob unshuffledHis)
          [hisTopId, hisDeeperId, plainArrival]
      _ -> Spec.assertFailure s "exactly one card should have arrived in bob's library in each run"

  -- CR 608.2b for a TRIGGERED ability, over exile: "a target that's no longer in
  -- the zone it was in when it was targeted is illegal. ... If all its targets,
  -- for every instance of the word 'target,' are now illegal, the spell or
  -- ability doesn't resolve."
  --
  -- The response takes the card out of exile to bob's hand, which is a zone
  -- change like any other, so the trigger's one target is gone. Two things must
  -- then be true: nothing arrives in his library, AND his library is not
  -- shuffled either. The second is the one CR 701.24c could be misread into
  -- breaking -- "that library is shuffled even if none of those objects are in
  -- the zone they're expected to be in" is about an effect that IS resolving,
  -- and this ability never resolves at all, so the clause never comes up. The
  -- reversing interpreter is what makes that assertion say anything: the library
  -- is asserted in its ORIGINAL order, which an unconditional shuffle would have
  -- reversed.
  Spec.it s "CR 608.2b Riftsweeper's trigger fizzles when the card leaves exile in response, shuffling nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    riftsweeper <- S.printingOf s registry "Riftsweeper"
    let (_, g1) = S.entersWithTrigger riftsweeper S.alice (Setup.emptyGame S.bothPlayers)
        (hisDeeperId, g2) = S.addLibraryCard bolt S.bob g1
        (hisTopId, g3) = S.addLibraryCard bolt S.bob g2
        (hisId, board) = S.addExiledCard piker S.bob g3
        placed = S.runPure (aimAtCardShuffling hisId) board Engine.placePendingTriggers
        resolve g = S.runPure (aimAtCardShuffling hisId) g Stack.resolveTop
        shuffledIn = resolve placed
        fizzled = resolve (S.runPure S.identityAnswer placed (Event.changeZone hisId Zone.Hand))
    Spec.assertEqWith s "the enters trigger went on the stack" (length (GameState.stack placed)) 1
    Spec.assertEqWith s "untouched, bob's library grew to three" (length (Game.zoneMembers Zone.Library S.bob shuffledIn)) 3
    Spec.assertEqWith s "taken to his hand in response, his library is the two it started with, in their original order -- not even shuffled" (Game.zoneMembers Zone.Library S.bob fizzled) [hisTopId, hisDeeperId]
    Spec.assertEqWith s "with the card itself in his hand" (length (Game.zoneMembers Zone.Hand S.bob fizzled)) 1
    Spec.assertEqWith s "with exile empty when the trigger resolved (the card was shuffled in)" (Set.size (GameState.exile shuffledIn)) 0
    Spec.assertEqWith s "and empty when it fizzled too (the card was taken to hand)" (Set.size (GameState.exile fizzled)) 0
    Spec.assertEqWith s "with the ability off the stack" (length (GameState.stack fizzled)) 0

  -- CR 601.2c: "the player announces their choice of an appropriate object or
  -- player for each target the spell requires." ONE announcement over every
  -- slot, not one slot at a time -- and Dwell on the Past is the first card in
  -- the pool whose slots are not independent. "Target player shuffles up to four
  -- target cards from THEIR graveyard into their library" scopes the card slot's
  -- pool, CR 400.1's per-player graveyard, to whoever the player slot names.
  --
  -- The engine offers the UNION over the player slot's own candidates, which is
  -- what the first assertion pins, and judges the announcement WHOLE
  -- (Target.selectionLegal). That is the rule's "all at once" without inventing
  -- an order between the slots -- and the union is why the second run's card has
  -- to be rejected by the joint check rather than by never having been offered.
  --
  -- THREE SEATS, because two collapse the reading under test: with alice and bob
  -- alone, "bob's graveyard" and "not the caster's graveyard" pick out the same
  -- cards, so a pool that had scoped itself to PlayerScope.Opponents would pass.
  -- carol's card is the one only the slot scoping can exclude.
  --
  -- The three runs differ in EXACTLY ONE thing apiece: which graveyard the
  -- chosen cards sit in, and how many of them. All three name bob in the player
  -- slot and pay the same {G} off the same Forest, off one board.
  --
  -- The two-card run is CR 601.2c's "up to four" reaching the opcode: the slot
  -- is read as an ObjectRef (SlotArity.Many), so a reader that took one would
  -- leave the second card in the graveyard, and CR 701.24's plural is what makes
  -- one shuffle serve both.
  Spec.it s "CR 601.2c Dwell on the Past's card slot is scoped to the player its other slot targets" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    dwell <- S.printingOf s registry "Dwell on the Past"
    let (_, g1) = S.addPermanent forest S.alice S.threePlayerGame
        (hisId, g2) = S.addGraveyardCard piker S.bob g1
        (hisOtherId, g3) = S.addGraveyardCard bolt S.bob g2
        (hersId, g4) = S.addGraveyardCard bolt S.carol g3
        (board, dwellId) = S.handOne dwell g4
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace dwell))
        offered = Target.legalSets (Just S.alice) False Map.empty S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
        run oids =
          let cast = S.runPure (aimingDwell oids) board (S.cast S.alice dwellId)
           in (cast, S.runPure (aimingDwell oids) cast Stack.resolveTop)
        (castAtBob, atBob) = run [hisId]
        (_, atBoth) = run [hisId, hisOtherId]
        (castAtCarol, _) = run [hersId]
    Spec.assertEqWith
      s
      "the card slot is offered the UNION over the player slot: every graveyard"
      (slotNamed "cards")
      (Set.fromList (fmap Recipient.ToObject [hisId, hisOtherId, hersId]))
    Spec.assertEqWith s "and the player slot every player" (slotNamed "player") (Set.fromList (fmap Recipient.ToPlayer [S.alice, S.bob, S.carol]))
    Spec.assertEqWith s "naming bob and a card in BOB's graveyard, the spell is cast" (length (GameState.stack castAtBob)) 1
    Spec.assertBool s (notElem hisId (Game.zoneMembers Zone.Graveyard S.bob atBob)) "and on resolution his card leaves his graveyard"
    Spec.assertEqWith s "arriving in HIS library (CR 400.3), which was empty" (length (Game.zoneMembers Zone.Library S.bob atBob)) 1
    Spec.assertEqWith s "leaving his other card behind, unnamed" (Game.zoneMembers Zone.Graveyard S.bob atBob) [hisOtherId]
    Spec.assertEqWith s "with carol's graveyard untouched" (Game.zoneMembers Zone.Graveyard S.carol atBob) [hersId]
    Spec.assertEqWith s "naming TWO of his cards, both leave the graveyard" (Game.zoneMembers Zone.Graveyard S.bob atBoth) []
    Spec.assertEqWith s "and both arrive in his library" (length (Game.zoneMembers Zone.Library S.bob atBoth)) 2
    Spec.assertEqWith s "naming bob and a card in CAROL's graveyard, the cast is reversed (CR 601.2)" (length (GameState.stack castAtCarol)) 0
    Spec.assertBool s (elem dwellId (Game.zoneMembers Zone.Hand S.alice castAtCarol)) "and the spell is back in alice's hand"

  -- The same narrowing on the FLOOR rather than the ceiling, which is CR 601.2c's
  -- other half: "In some cases, the number of targets will be defined by the
  -- spell's text." A slot whose count is fixed at two has no announcement to
  -- narrow -- the whole spell is illegal to cast when no coherent answer exists,
  -- and CR 601.2e's reversal is a worse prompt than never offering the cast.
  --
  -- Synthetic Exhume the Archive {1}{G} Sorcery (data/cards/synthetic-exhume-the-archive.json):
  -- "Target player shuffles two target cards from their graveyard into their
  -- library." SYNTHETIC because every printing whose target slot draws from
  -- another slot's graveyard prints a minimum of zero -- Scryfall
  -- o:"cards from their graveyard" -o:"up to" -o:"any number", 2026-08-31, no
  -- hit with a targeted card slot; Dwell on the Past, Gaea's Blessing, Krosan
  -- Reclamation, Memory's Journey, Quandrix Command, Rite of Renewal, Stream of
  -- Consciousness and Witness the Future all say "up to", and Loaming Shaman says
  -- "any number". Nothing in the CR forbids the card: rule 601.2c's own "the
  -- number of targets will be defined by the spell's text" is exactly this shape.
  --
  -- TWO BOARDS differing in exactly one thing -- which graveyard the Bolt sits in
  -- -- with the same two Forests paying the same {1}{G} and the Piker in bob's
  -- graveyard both times.
  Spec.it s "CR 601.2c a spell demanding two cards from one graveyard is uncastable when no one graveyard holds two" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    exhume <- S.printingOf s registry "Synthetic Exhume the Archive"
    let (_, g1) = S.addGraveyardCard piker S.bob (S.landsFor forest S.alice 2 S.threePlayerGame)
        boardWith pid = S.handOne exhume (snd (S.addGraveyardCard bolt pid g1))
        (together, togetherId) = boardWith S.bob
        (split, splitId) = boardWith S.carol
    Spec.assertBool s (S.castable S.alice togetherId together) "with both cards in bob's graveyard the spell has a coherent announcement"
    Spec.assertBool s (not (S.castable S.alice splitId split)) "split one apiece it has none, though the union still holds two"

  -- CR 601.2c's other sibling-slot reading, and the one the rule states in its
  -- own words: "The same target can't be chosen multiple times for any one
  -- instance of the word 'target' on the spell. However, if the spell uses the
  -- word 'target' in multiple places, the same object or player can be chosen
  -- once for each instance of the word 'target'." Sharing between two slots is
  -- the rule's DEFAULT, so a card that forbids it says "another", and the
  -- restriction lives in that slot's own Filter rather than in the machinery.
  --
  -- Fall of the Hammer {1}{R} Instant (data/cards/fall-of-the-hammer.json):
  -- "Target creature you control deals damage equal to its power to another
  -- target creature." The victim slot is Filter.Not (Filter.IsBound "dealer"),
  -- which reads what the dealer slot holds -- the first card in the pool whose
  -- slots depend on each other through a FILTER rather than through a pool
  -- (Dwell on the Past above is the pool reading).
  --
  -- Rabid Bite is the same card one word apart: same two Pool.Creatures slots,
  -- same DealDamage off AgainstSlot/Power, and "target creature you don't
  -- control" where this prints "another target creature". So the Wall is the
  -- discriminator that matters: it is ALICE's, and naming it is legal here,
  -- which "another" as a controller test would reject.
  --
  -- The three runs differ in exactly one thing apiece -- which creature fills
  -- the victim slot -- off one board, with the same {1}{R} paid off the same two
  -- Mountains, and the Piker is the dealer in all three.
  --
  -- Damage is read BEFORE state-based actions, so the 2/1 Piker naming itself
  -- would still be readable had the announcement gone through; a self-damage
  -- reading is not hidden behind CR 704.5g.
  Spec.it s "CR 601.2c Fall of the Hammer's victim slot cannot be the creature its dealer slot names" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    wall <- S.printingOf s registry "Wall of Stone"
    rats <- S.printingOf s registry "Typhoid Rats"
    hammer <- S.printingOf s registry "Fall of the Hammer"
    let (dealerId, g1) = S.addPermanent piker S.alice (S.landsInPlay mountain 2)
        (wallId, g2) = S.addPermanent wall S.alice g1
        (ratsId, g3) = S.addPermanent rats S.bob g2
        (board, hammerId) = S.handOne hammer g3
        run victimId =
          let cast = S.runPure (aimingHammer dealerId victimId) board (S.cast S.alice hammerId)
           in (cast, S.runPure (aimingHammer dealerId victimId) cast Stack.resolveTop)
        (castAtSelf, atSelf) = run dealerId
        (_, atRats) = run ratsId
        (_, atWall) = run wallId
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace hammer))
        offered = Target.legalSets (Just S.alice) False Map.empty S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
    -- The behaviour first: naming the Piker in both slots is not an announcement
    -- the rule allows, so CR 601.2e returns the game to before the proposal.
    Spec.assertEqWith s "naming the Piker in BOTH slots, the cast is reversed (CR 601.2e)" (length (GameState.stack castAtSelf)) 0
    Spec.assertEqWith s "so the Piker was never dealt its own two damage" (S.damageOf dealerId atSelf) (Just 0)
    Spec.assertBool s (elem hammerId (Game.zoneMembers Zone.Hand S.alice castAtSelf)) "and the spell is back in alice's hand"
    Spec.assertEqWith s "naming bob's Rats instead, the Piker's two damage is marked on them" (S.damageOf ratsId atRats) (Just 2)
    Spec.assertEqWith s "with the Piker itself unharmed" (S.damageOf dealerId atRats) (Just 0)
    -- CR 601.2c's "another" is about the OBJECT, not its controller: alice's own
    -- Wall is a legal victim, which is the whole difference from Rabid Bite.
    Spec.assertEqWith s "naming alice's own Wall, the same two damage is marked on it" (S.damageOf wallId atWall) (Just 2)
    -- The union posture, last: the victim slot is still OFFERED the Piker at CR
    -- 601.2c, which is what makes the first assertion a joint-check rejection
    -- rather than a slot the fix emptied.
    Spec.assertEqWith
      s
      "the victim slot is offered every creature, the dealer's own candidate included"
      (slotNamed "victim")
      (Set.fromList (fmap Recipient.ToCreature [dealerId, wallId, ratsId]))
    Spec.assertEqWith s "and the dealer slot only alice's two" (slotNamed "dealer") (Set.fromList (fmap Recipient.ToCreature [dealerId, wallId]))

  -- CR 601.2c through CR 700.2a: the case above's card, asked one step earlier.
  -- A mode is fillable when SOME announcement fills every one of its slots, not
  -- when each slot can be filled on its own -- so Fall of the Hammer off a board
  -- holding exactly one creature is a spell with no legal announcement and is
  -- never offered, rather than a cast proposed and reversed at CR 601.2e.
  --
  -- Reversal and unofferability are indistinguishable on the board afterwards --
  -- both leave the spell in hand and the stack empty -- so the observable is
  -- Action.legalActions, which is where CR 601.2e's cost lands.
  --
  -- TWO BOARDS differing in exactly ONE thing: whether bob has a creature. The
  -- same two Mountains are untapped on both, so the refusal is not the mana, and
  -- the same Piker is alice's only creature on both, so the dealer slot is
  -- identical and the victim slot is the whole difference.
  Spec.it s "CR 700.2a Fall of the Hammer is unfillable on a board holding one creature" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    rats <- S.printingOf s registry "Typhoid Rats"
    hammer <- S.printingOf s registry "Fall of the Hammer"
    let (dealerId, g1) = S.addPermanent piker S.alice (S.landsInPlay mountain 2)
        (alone, hammerId) = S.handOne hammer g1
        (_, together) = S.addPermanent rats S.bob alone
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace hammer))
        offered = Target.legalSets (Just S.alice) False Map.empty S.noSource slots alone
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
    Spec.assertBool s (not (any (S.isCastOf hammerId) (Action.legalActions S.alice alone))) "with only the Piker on the board, the cast is not offered at all"
    Spec.assertBool s (any (S.isCastOf hammerId) (Action.legalActions S.alice together)) "and with bob's Rats beside it, the same spell off the same mana is offered"
    -- The union posture, last and for the case above's reason: each slot still
    -- has a candidate ON ITS OWN, so the refusal is the cross-slot search and not
    -- a slot the change emptied.
    Spec.assertEqWith s "the victim slot is offered the Piker by itself" (slotNamed "victim") (Set.singleton (Recipient.ToCreature dealerId))
    Spec.assertEqWith s "and so is the dealer slot" (slotNamed "dealer") (Set.singleton (Recipient.ToCreature dealerId))

  -- The same question on the ACTIVATION road, which CR 602.2b routes through CR
  -- 601.2b-i and CR 700.2a gates the same way it gates a spell's. Fall of the
  -- Hammer above reaches `fillableModesGiven` through Pawl.Engine.Cast; this
  -- reaches it through Pawl.Engine.Activatable.activatableGiven, so the cross-slot
  -- search is proved on both.
  --
  -- Resourceful Defense {2}{W} Enchantment (data/cards/resourceful-defense.json):
  -- "{4}{W}: Move any number of counters from target permanent you control onto
  -- a second target permanent you control." Its `to` slot is
  -- And [ControlledBy You, Not (IsBound "from")], so a controller whose only
  -- permanent is the Defense itself fills each slot alone and no announcement at
  -- all, and CR 700.2a is what keeps the ability off the offer.
  --
  -- FIVE WHITE MANA FLOATING rather than five Plains, and that is the whole
  -- reason this board is built by hand: a land is a permanent its controller
  -- controls, so the mana to activate would fill the second slot by itself and
  -- there would be no unfillable board to build. Both boards carry the same
  -- floating five, so neither answer is about affordability.
  --
  -- TWO BOARDS differing in exactly ONE thing: whether alice also controls a
  -- Piker.
  Spec.it s "CR 700.2a Resourceful Defense's ability is unfillable when it is its controller's only permanent" $ do
    defense <- S.printingOf s registry "Resourceful Defense"
    piker <- S.printingOf s registry "Goblin Piker"
    let white =
          ManaUnit.MkManaUnit
            { ManaUnit.manaType = ManaType.Colored Color.White,
              ManaUnit.tags = Set.empty,
              ManaUnit.retention = ManaRetention.Ordinary,
              ManaUnit.restriction = Nothing,
              ManaUnit.rider = Nothing,
              ManaUnit.spendTrigger = Nothing,
              ManaUnit.sourceChosenSubtype = Nothing,
              ManaUnit.sourceLastExiled = Nothing
            }
        funded = (Setup.emptyGame S.bothPlayers) {GameState.manaPool = Map.singleton S.alice (Mana.Type.MkMana (replicate 5 white)), GameState.priority = Just S.alice}
        (defenseId, alone) = S.addPermanent defense S.alice funded
        (_, together) = S.addPermanent piker S.alice alone
        activates gs = any (\action -> case action of A.Activate oid _ -> oid == defenseId; _ -> False) (Action.legalActions S.alice gs)
    Spec.assertBool s (not (activates alone)) "with the Defense alone on the battlefield, its ability is not offered at all"
    Spec.assertBool s (activates together) "and with a Piker beside it, the same ability off the same floating five is offered"

  -- The three cases above put CR 601.2c's "another" on a card chosen once. CR
  -- 700.2d puts it on a card whose mode may be chosen twice, and then the
  -- filter's slot NAME has to follow the occurrence exactly as the key and the
  -- pool do -- read under its printed name from occurrence 1 it names occurrence
  -- 0's dealer, and the creature occurrence 1 itself named becomes a legal victim
  -- of its own damage. Weaker than printed, in the caster's favour.
  --
  -- Synthetic Hammer Refrain {1}{R} Instant
  -- (data/cards/synthetic-hammer-refrain.json): "Choose two. You may choose the
  -- same mode more than once. -- Target creature you control deals damage equal
  -- to its power to another target creature. -- Draw a card." SYNTHETIC because
  -- the two printed sets do not intersect: Scryfall o:"choose the same mode more
  -- than once", 2026-08-31, returns 22 cards, and no mode of any of them prints
  -- two targets with one restricting the other. Its damage mode is Fall of the
  -- Hammer's above, one instruction added.
  --
  -- TWO RUNS off one board, differing in exactly one thing -- which creature
  -- fills occurrence 1's victim slot -- with the same {1}{R} paid off the same
  -- two Mountains and the same modes chosen. The legal run is what keeps the
  -- rejected one from passing off a spell that never worked.
  --
  -- TWO PIKERS rather than two printings: occurrence 0's dealer and occurrence
  -- 1's are then indistinguishable except by slot, so a filter that admits the
  -- second because it compared against the first is the only reading that
  -- separates the runs.
  Spec.it s "CR 700.2d a repeated mode's filter reads its own occurrence's sibling slot, not the first's" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    wall <- S.printingOf s registry "Wall of Stone"
    refrain <- S.printingOf s registry "Synthetic Hammer Refrain"
    let (firstDealerId, g1) = S.addPermanent piker S.alice (S.landsInPlay mountain 2)
        (secondDealerId, g2) = S.addPermanent piker S.alice g1
        (wallId, g3) = S.addPermanent wall S.bob g2
        (board, refrainId) = S.handOne refrain g3
        run victimTwo =
          let cast = S.runPure (aimingRefrain firstDealerId wallId secondDealerId victimTwo) board (S.cast S.alice refrainId)
           in (cast, S.runPure (aimingRefrain firstDealerId wallId secondDealerId victimTwo) cast Stack.resolveTop)
        (castAtSelf, atSelf) = run secondDealerId
        (_, atWall) = run wallId
        slots = Modal.modesTargetSlots (Seq.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 0]) (Face.spell (S.combinedFace refrain))
        offered = Target.legalSets (Just S.alice) False Map.empty S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
    -- The behaviour first: occurrence 1 naming its own dealer is not an
    -- announcement the rule allows, so CR 601.2e returns the game to before the
    -- proposal and occurrence 0's damage never happens either.
    Spec.assertEqWith s "occurrence 1 naming the creature its OWN dealer slot holds, bob's Wall is unharmed: the cast is reversed (CR 601.2e)" (S.damageOf wallId atSelf) (Just 0)
    Spec.assertEqWith s "and the creature named twice took none of its own two damage" (S.damageOf secondDealerId atSelf) (Just 0)
    Spec.assertEqWith s "naming the Wall for occurrence 1 instead, both Pikers' two damage reaches it" (S.damageOf wallId atWall) (Just 4)
    -- The proxies after: a cast that never happened would leave the Wall
    -- unharmed too.
    Spec.assertEqWith s "the reversed cast left nothing on the stack" (length (GameState.stack castAtSelf)) 0
    Spec.assertBool s (elem refrainId (Game.zoneMembers Zone.Hand S.alice castAtSelf)) "and the spell is back in alice's hand"
    -- The union posture, last and for the case above's reason: occurrence 1's
    -- victim slot is still OFFERED its own dealer at CR 601.2c, so the first
    -- assertion is a joint-check rejection rather than a slot the rename emptied.
    Spec.assertEqWith
      s
      "occurrence 1's victim slot is offered every creature, its own dealer's candidate included"
      (slotNamed "victim#1")
      (Set.fromList (fmap Recipient.ToCreature [firstDealerId, secondDealerId, wallId]))
    Spec.assertEqWith s "and its dealer slot only alice's two" (slotNamed "dealer#1") (Set.fromList (fmap Recipient.ToCreature [firstDealerId, secondDealerId]))

  -- The case above puts CR 601.2c's "another" -- a FILTER's read of a sibling
  -- slot -- on a repeated mode. This is the slot's THIRD read of one, CR 202.3's
  -- computed bound, which has to follow the occurrence exactly as the key, the
  -- pool and the filter do: read under its printed name from occurrence 1 it
  -- measures occurrence 0's slot, and a creature occurrence 1 could not afford
  -- becomes a legal victim. Weaker than printed, in the caster's favour.
  --
  -- Synthetic Measured Refrain {2}{B} Instant
  -- (data/cards/synthetic-measured-refrain.json): "Choose two. You may choose the
  -- same mode more than once. -- Tap up to two target creatures you control, then
  -- destroy target creature with mana value less than or equal to the number of
  -- those creatures. -- Draw a card." SYNTHETIC because the two printed sets do
  -- not intersect: Scryfall o:"choose the same mode more than once", 2026-08-31,
  -- returns 22 cards, and no mode of any of them prints a target slot with a
  -- computed bound.
  --
  -- Pawl.Types.Scope's OverBound fold over a bound slot is the bound, rather than
  -- Quantity.AgainstSlot's power read, because the OFFER hands a dependent slot
  -- the UNION of what the slot it names could take (legalSetsGiven): a fold over
  -- three candidates answers 3, where a read that insists on ONE object
  -- (Binding.onlyOne) answers nothing at all and empties the slot. The union is a
  -- widening and selectionLegal is where the announcement is narrowed.
  --
  -- TWO RUNS off one board, differing in exactly one thing -- which creature fills
  -- occurrence 1's victim slot -- with the same {2}{B} paid off the same three
  -- Swamps, the same modes chosen and the same three Pikers in the two gauge
  -- slots. The legal run is what keeps the rejected one from passing off a spell
  -- that never worked.
  --
  -- THREE Goblin Pikers for alice rather than three printings: occurrence 0's
  -- gauge and occurrence 1's are then indistinguishable except by slot and by how
  -- many each holds, so a bound that measured the first is the only reading that
  -- separates the runs.
  Spec.it s "CR 700.2d a repeated mode's computed bound measures its own occurrence's sibling slot" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    rats <- S.printingOf s registry "Typhoid Rats"
    refrain <- S.printingOf s registry "Synthetic Measured Refrain"
    let (gaugeA, g1) = S.addPermanent piker S.alice (S.landsInPlay swamp 3)
        (gaugeB, g2) = S.addPermanent piker S.alice g1
        (gaugeC, g3) = S.addPermanent piker S.alice g2
        (victimId, g4) = S.addPermanent piker S.bob g3
        (dearId, g5) = S.addPermanent piker S.bob g4
        (cheapId, g6) = S.addPermanent rats S.bob g5
        (board, refrainId) = S.handOne refrain g6
        run victimTwo =
          let cast = S.runPure (aimingMeasured [gaugeA, gaugeB] victimId gaugeC victimTwo) board (S.cast S.alice refrainId)
           in (cast, S.runPure (aimingMeasured [gaugeA, gaugeB] victimId gaugeC victimTwo) cast Stack.resolveTop)
        (castAtDear, atDear) = run dearId
        (_, atCheap) = run cheapId
        onBattlefield gs oid = elem oid (Game.zoneMembers Zone.Battlefield S.bob gs)
        slots = Modal.modesTargetSlots (Seq.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 0]) (Face.spell (S.combinedFace refrain))
        offered = Target.legalSets (Just S.alice) False Map.empty S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
    -- The behaviour first: occurrence 1's gauge slot holds ONE creature, so a
    -- mana value 2 victim is not an announcement the rule allows, CR 601.2e
    -- returns the game to before the proposal, and occurrence 0's destruction
    -- never happens either.
    Spec.assertBool s (onBattlefield atDear dearId) "occurrence 1 naming a mana value 2 creature against its own one-creature gauge, that creature survives: the cast is reversed (CR 601.2e)"
    Spec.assertBool s (onBattlefield atDear victimId) "and occurrence 0's own victim, which its two-creature gauge did afford, survives with it"
    Spec.assertBool s (not (onBattlefield atCheap victimId)) "naming the mana value 1 Rats for occurrence 1 instead, occurrence 0's victim is destroyed"
    Spec.assertBool s (not (onBattlefield atCheap cheapId)) "and the Rats with it"
    -- The proxies after: a cast that never happened would leave both creatures
    -- standing too.
    Spec.assertEqWith s "the reversed cast left nothing on the stack" (length (GameState.stack castAtDear)) 0
    Spec.assertBool s (elem refrainId (Game.zoneMembers Zone.Hand S.alice castAtDear)) "and the spell is back in alice's hand"
    -- The union posture, last and for the case above's reason: occurrence 1's
    -- victim slot is still OFFERED the dearer creature at CR 601.2c, measured
    -- against all three creatures its gauge slot could take, so the first
    -- assertion is a joint-check rejection rather than a slot the rename emptied.
    Spec.assertEqWith
      s
      "occurrence 1's victim slot is offered every creature, its own bound measured against the whole of what its gauge could take"
      (slotNamed "victim#1")
      (Set.fromList (fmap Recipient.ToCreature [gaugeA, gaugeB, gaugeC, victimId, dearId, cheapId]))

  -- CR 601.2c's sibling-slot reading in its POSITIVE form, where Fall of the
  -- Hammer above is the negative one: "another" excludes what a sibling slot
  -- holds, and "with the same controller" demands something of it -- CR 110.2's
  -- controller, which every permanent has.
  --
  -- Bioshift {G/U} Instant (Gatecrash; name, cost, type line and oracle text
  -- checked against Scryfall 2026-08-31), data/cards/bioshift.json:
  --
  --   Move any number of +1/+1 counters from target creature onto another target
  --   creature with the same controller.
  --
  -- Its `to` slot is And [Not (IsBound "from"), SameControllerAsBound "from"], and
  -- the second atom is the one this case exists for: written without it the card
  -- would be WEAKER than printed in the caster's favour, letting counters cross
  -- between two players' creatures.
  --
  -- THREE Walls of Stone, two alice's and one bob's, so the two boards below
  -- differ in exactly one thing -- which creature fills the `to` slot -- with the
  -- same one hybrid mana paid off the same two lands. One printing three times
  -- over, so nothing but the CONTROLLER can separate the candidates: a filter
  -- reading any characteristic would admit or refuse all three alike.
  Spec.it s "CR 601.2c Bioshift's second slot cannot be a creature its first slot's controller does not control" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    wall <- S.printingOf s registry "Wall of Stone"
    bioshift <- S.printingOf s registry "Bioshift"
    let lands = S.landsFor island S.alice 1 (S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers))
        (giverId, g1) = S.addPermanent wall S.alice lands
        (mineId, g2) = S.addPermanent wall S.alice g1
        (theirsId, g3) = S.addPermanent wall S.bob g2
        (board, spellId) = S.handOne bioshift (S.addCounter CounterKind.PlusOnePlusOne 3 giverId g3)
        run takerId =
          let cast = S.runPure (aimingBioshift giverId takerId) board (S.cast S.alice spellId)
           in (cast, S.runPure (aimingBioshift giverId takerId) cast Stack.resolveTop)
        (_, ontoMine) = run mineId
        (castAtTheirs, ontoTheirs) = run theirsId
        counters = S.counterOf CounterKind.PlusOnePlusOne
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace bioshift))
        offered = Target.legalSets (Just S.alice) False Map.empty S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
    Spec.assertEqWith s "alice's first Wall bears the three counters and nothing else does" (fmap (`counters` board) [giverId, mineId, theirsId]) [3, 0, 0]
    -- THE GAMEPLAY-LEVEL ASSERTIONS, ahead of the reversal's: the counters cross
    -- between alice's two Walls and do not cross to bob's.
    Spec.assertEqWith s "naming alice's other Wall, all three counters cross to it" (fmap (`counters` ontoMine) [giverId, mineId]) [0, 3]
    Spec.assertEqWith s "naming bob's Wall, it receives none" (counters theirsId ontoTheirs) 0
    Spec.assertEqWith s "and alice's first Wall still bears all three" (counters giverId ontoTheirs) 3
    -- CR 608.2b: the `from` Wall changes controller in response (a SetController
    -- effect, Act of Treason's layer-2 half). "With the same controller" is the
    -- `to` target's condition, re-checked against `from`'s controller NOW, so
    -- `to` is illegal and nothing crosses -- the constraint needs no mirror on
    -- `from`, whose own text asks nothing of its sibling.
    let (castMine, _) = run mineId
        stolen = S.runPure (aimingBioshift giverId mineId) (S.giveControl giverId S.bob castMine) Stack.resolveTop
    Spec.assertEqWith s "CR 608.2b the giver was stolen in response: no counter crosses" (fmap (`counters` stolen) [giverId, mineId]) [3, 0]
    -- CR 601.2e, behind the behaviour: the announcement was not one the rule
    -- allows, so the game returned to before the spell was proposed.
    Spec.assertEqWith s "the cast is reversed" (length (GameState.stack castAtTheirs)) 0
    Spec.assertBool s (elem spellId (Game.zoneMembers Zone.Hand S.alice castAtTheirs)) "and the spell is back in alice's hand"
    -- The union posture, last, and the trap this atom had to avoid: the offer is
    -- made before either target is chosen, so the `to` slot is still offered every
    -- creature -- bob's included. A narrowing offer would empty the slot and make
    -- the spell uncastable rather than restricted.
    Spec.assertEqWith
      s
      "the second slot is offered every creature, bob's own candidate included"
      (slotNamed "to")
      (Set.fromList (fmap Recipient.ToCreature [giverId, mineId, theirsId]))

  -- CR 702.164a: "Toxic is a static ability. It is written 'toxic N,' where N is
  -- a number." Flensing Raptor's enters trigger reads "another target creature
  -- you control with toxic", which names the ABILITY rather than one written
  -- instance -- Filter.HasKeywordFamily, where every other keyword narrowing in
  -- the pool is Filter.HasKeyword. So a single filter has to reach both the
  -- toxic 1 Raptor beside it and the toxic 2 Branchblight Stalker (#522).
  --
  -- TWO Ns is the whole point of the board: a HasKeyword-shaped implementation
  -- could match the entering Raptor's own toxic 1 and would still fail on the
  -- Stalker, so one toxic creature would not discriminate.
  --
  -- The Piker is the control, as in the rule-702 cases above -- without it,
  -- "the creature without toxic is excluded" could pass on an empty legal set.
  -- Bob's Stalker separates the family question from the CR 109.5 controller one,
  -- and the entering Raptor itself pins CR 601.2c's "another" (Not IsSource).
  Spec.it s "CR 702.164a Flensing Raptor's trigger reaches toxic 1 and toxic 2 alike, and nothing else" $ do
    raptor <- S.printingOf s registry "Flensing Raptor"
    stalker <- S.printingOf s registry "Branchblight Stalker"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (otherRaptorId, gs1) = S.addPermanent raptor S.alice gs0
        (stalkerId, gs2) = S.addPermanent stalker S.alice gs1
        (pikerId, gs3) = S.addPermanent piker S.alice gs2
        (hisStalkerId, gs4) = S.addPermanent stalker S.bob gs3
        (enteringId, board) = S.entersWithTrigger raptor S.alice gs4
    case triggerTargetSlot raptor of
      Nothing -> Spec.assertFailure s "Flensing Raptor's trigger should declare one target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) enteringId theSlot board
        Spec.assertBool s (Set.member (Recipient.ToCreature otherRaptorId) legal) "the Raptor beside it has toxic 1, and is legal"
        Spec.assertBool s (Set.member (Recipient.ToCreature stalkerId) legal) "Branchblight Stalker has toxic 2, and is legal too"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature pikerId) legal)) "Goblin Piker has no toxic at all"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature enteringId) legal)) "CR 601.2c: not the entering Raptor itself"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature hisStalkerId) legal)) "CR 109.5: not bob's toxic creature"

  -- CR 701.24c's FIRST half: "that library is shuffled even if none of those
  -- objects are in the zone they're expected to be in". Dwell on the Past is the
  -- pool's first card that can reach it, because CR 608.2b only fizzles a spell
  -- whose targets are ALL illegal -- its player slot stays legal when both
  -- targeted cards are exiled in response, so the spell resolves with nothing
  -- left to shuffle in and bob's library must be shuffled anyway (#558).
  --
  -- A PAIR OF RUNS off one board differing in exactly one thing: whether the two
  -- cards were exiled between the cast and the resolution. The control run is
  -- what says the shuffle in the other one is not merely the arrival's doing --
  -- it shows the graveyard losing exactly those two cards and the library
  -- gaining exactly two.
  --
  -- ORDER is how a shuffle is observed at all (CR 701.24a makes it unobservable
  -- otherwise), under the interpreter that REVERSES every Prompt.Shuffle. What
  -- ORDER the shuffle leaves is deliberately not asserted anywhere: a real
  -- shuffle has no assertable one. The seeded libraries are two cards each, so a
  -- reversal is visible.
  --
  -- THREE SEATS, and three seeded libraries. alice casts, bob is targeted, carol
  -- is neither -- so "the library the spell NAMES" is told apart both from the
  -- controller's (alice's, untouched) and from everyone's (carol's, untouched).
  -- carol also holds a graveyard card of the same printing as one of bob's,
  -- which is what makes "their graveyard" observable rather than "any".
  Spec.it s "CR 701.24c Dwell on the Past shuffles the targeted player's library even when both targeted cards have left the graveyard" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    dwell <- S.printingOf s registry "Dwell on the Past"
    -- S.addLibraryCard puts each card ON TOP, so the second of each pair heads
    -- the library and the first sits under it.
    let (_, g1) = S.addPermanent forest S.alice S.threePlayerGame
        (hisId, g2) = S.addGraveyardCard piker S.bob g1
        (hisOtherId, g3) = S.addGraveyardCard bolt S.bob g2
        (hersId, g4) = S.addGraveyardCard piker S.carol g3
        (herDeeperId, g5) = S.addLibraryCard bolt S.alice g4
        (herTopId, g6) = S.addLibraryCard piker S.alice g5
        (hisDeeperId, g7) = S.addLibraryCard bolt S.bob g6
        (hisTopId, g8) = S.addLibraryCard piker S.bob g7
        (carolDeeperId, g9) = S.addLibraryCard piker S.carol g8
        (carolTopId, g10) = S.addLibraryCard bolt S.carol g9
        (board, dwellId) = S.handOne dwell g10
        cast = S.runPure (aimingDwellShuffling [hisId, hisOtherId]) board (S.cast S.alice dwellId)
        exiled =
          S.runPure S.identityAnswer (S.runPure S.identityAnswer cast (Event.changeZone hisId Zone.Exile)) $
            Event.changeZone hisOtherId Zone.Exile
        resolvedWithCards = S.runPure (aimingDwellShuffling [hisId, hisOtherId]) cast Stack.resolveTop
        resolvedWithout = S.runPure (aimingDwellShuffling [hisId, hisOtherId]) exiled Stack.resolveTop
        arrivalsIn pid seeded gs = filter (`notElem` seeded) (Game.zoneMembers Zone.Library pid gs)
    Spec.assertEqWith s "the spell is on the stack" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "the control: his graveyard loses exactly the two named cards" (Game.zoneMembers Zone.Graveyard S.bob resolvedWithCards) []
    Spec.assertEqWith s "and his library gains exactly two" (length (arrivalsIn S.bob [hisTopId, hisDeeperId] resolvedWithCards)) 2
    Spec.assertEqWith s "with carol's graveyard card untouched, so it was HIS graveyard the cards came from" (Game.zoneMembers Zone.Graveyard S.carol resolvedWithCards) [hersId]
    Spec.assertEqWith s "exiled in response, both cards are gone from his graveyard before the spell resolves" (Game.zoneMembers Zone.Graveyard S.bob exiled) []
    Spec.assertEqWith s "so nothing arrives in his library" (length (arrivalsIn S.bob [hisTopId, hisDeeperId] resolvedWithout)) 0
    Spec.assertEqWith
      s
      "CR 701.24c: his library is shuffled all the same -- the reversal shows through"
      (Game.zoneMembers Zone.Library S.bob resolvedWithout)
      [hisDeeperId, hisTopId]
    Spec.assertEqWith s "alice's library is not shuffled, so the library is the one the spell NAMED and not the controller's" (Game.zoneMembers Zone.Library S.alice resolvedWithout) [herTopId, herDeeperId]
    Spec.assertEqWith s "and carol's is not either, so it is one library and not every library" (Game.zoneMembers Zone.Library S.carol resolvedWithout) [carolTopId, carolDeeperId]
    Spec.assertEqWith s "with the spell off the stack" (length (GameState.stack resolvedWithout)) 0

  -- CR 122.1: "A counter is a marker placed on an object or player ...". Razorfin
  -- Abolisher's slot asks whether the candidate has one AT ALL, with no kind to
  -- look up -- Filter.HasCountersOfAnyKind, the kind-agnostic sibling of the
  -- HasCounters atom Renegade Krasis writes.
  --
  -- Razorfin Abolisher {2}{U} Creature -- Merfolk Wizard (EVE), "{1}{U}, {T}:
  -- Return target creature with a counter on it to its owner's hand." (name,
  -- cost, type line, P/T and Oracle text checked against api.scryfall.com,
  -- 2026-08-27). Nothing is omitted, so pawl's card is neither stricter nor
  -- weaker than printed.
  --
  -- TWO candidates on bob's side, because one board cannot discriminate the atom
  -- alone:
  --
  --   * a Hill Giant carrying a STUN counter -- the only legal target. The kind is
  --     deliberately not +1/+1: a HasCounters PlusOnePlusOne written by mistake
  --     admits nothing here, so the case would fail rather than pass.
  --   * a Goblin Piker carrying none -- rejected by the atom alone. Without it the
  --     legal set is a singleton whatever the filter says, and "the Giant was
  --     returned" would prove nothing about counters.
  --
  -- Different names and different P/T, so which creature moved is read off the
  -- printed name in bob's hand. The Abolisher settles first: CR 302.6 makes its
  -- {T} illegal otherwise, and it is itself a third counterless creature the
  -- filter must keep out.
  razorfinSpec s registry
  -- CR 202.3's bound read off the BOARD rather than off the card, which no Filter
  -- could state before Pawl.Types.TargetSlot grew its `amount`; see #2538.
  celestineSpec s registry
  -- The same bound read off the ANNOUNCEMENT instead: the amount CR 603.2's own
  -- event stamped, which no board can answer.
  warsingerSpec s registry
  -- And the SPELL's announcement: CR 601.2b's X, named one step before CR 601.2c
  -- chooses against it.
  stirTheGraveSpec s registry
  -- And the same announcement read TWICE, by the offer and by CR 601.2c's joint
  -- check: a slot whose bound reads that X and whose pool reads a sibling slot.
  borrowedExhumationSpec s registry
  -- And the bound at EQUALITY rather than order, which is a different atom and
  -- not a different reading of the one above: "with mana value X"; see #2989.
  -- The joint check on the road no spell takes: CR 603.3d's placement, where an
  -- announcement that fails it is asked again rather than reversed.
  itzquinthSpec s registry
  -- And the conjunct beside it on that road: rule 601.2c's NUMBER, which only an
  -- interpreter ignoring the offer can get wrong.
  ravenousRatsSpec s registry
  -- And CR 115.7d, a spell that is already on the stack given new targets.
  -- And its joint half, over a spell whose second slot reads its first.
  redirectBioshiftSpec s registry
  -- And CR 115.7a's stricter "change the target", over Deflection.
  deflectionSpec s registry

razorfinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
razorfinSpec s registry = Spec.describe s "HasCountersOfAnyKind (CR 122.1)" $ do
  Spec.it s "CR 122.1 Razorfin Abolisher returns the creature with a counter on it" $ do
    built <- razorfinBoard s registry
    giant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    case built of
      Nothing -> Spec.assertFailure s "Razorfin Abolisher should print one activated ability"
      Just (board, ability, giantId, pikerId) -> do
        -- The fixture's own preconditions: the counter really is there, it is
        -- really not a +1/+1 one, and the other creature really has none -- so
        -- nothing below can pass because S.addCounter missed.
        Spec.assertEqWith s "CR 122.1 the Giant carries one stun counter" (S.counterOf CounterKind.Stun giantId board) 1
        Spec.assertEqWith s "and no +1/+1 counter, which is not the kind under test" (S.counterOf CounterKind.PlusOnePlusOne giantId board) 0
        Spec.assertEqWith s "with the Piker carrying none of either" (S.counterOf CounterKind.Stun pikerId board + S.counterOf CounterKind.PlusOnePlusOne pikerId board) 0
        let after = activateAt giantId board (abolisherOf board) ability
        -- THE GAMEPLAY ASSERTION.
        Spec.assertBool s (not (S.onBattlefield giantId after)) "CR 122.1 the creature with a counter on it left the battlefield"
        Spec.assertBool s (elem (S.printingName giant) (namesInHand S.bob after)) "and CR 400.7's new object is in its OWNER's hand"
        Spec.assertBool s (S.onBattlefield pikerId after) "with the counterless creature untouched"
        Spec.assertBool s (notElem (S.printingName piker) (namesInHand S.bob after)) "and nowhere near bob's hand"

  -- WHAT THE ATOM BUYS, asked of the engine's own candidate set: the counterless
  -- Piker is a creature bob controls and Pool.Creatures gathers it, so only CR
  -- 122.1 keeps it out. The answerer asks for it and S.preferring falls back to
  -- the smallest legal recipient when the offer does not hold it -- so a filter
  -- that had admitted the Piker would have returned the Piker instead.
  Spec.it s "CR 122.1 a creature with no counters on it is not a legal target" $ do
    built <- razorfinBoard s registry
    case built of
      Nothing -> Spec.assertFailure s "Razorfin Abolisher should print one activated ability"
      Just (board, ability, giantId, pikerId) -> do
        let after = activateAt pikerId board (abolisherOf board) ability
        -- THE GAMEPLAY ASSERTION, and the one the Piker's admission would change.
        Spec.assertBool s (S.onBattlefield pikerId after) "CR 122.1 the counterless creature was never offered, so it stayed"
        Spec.assertBool s (not (S.onBattlefield giantId after)) "and the countered one was returned in its place"

  -- The admitted SET by identity, and AFTER the two cases above rather than
  -- before them: a membership read is a proxy for what the ability does, and the
  -- board is what this unit exists to move. It is here so that "the Piker stayed"
  -- cannot be read as an activation that never happened.
  Spec.it s "CR 601.2c the slot admits exactly the creature with a counter on it" $ do
    built <- razorfinBoard s registry
    case built of
      Nothing -> Spec.assertFailure s "Razorfin Abolisher should print one activated ability"
      Just (board, ability, giantId, pikerId) -> case soleTargetSlot (ActivatedAbility.modal ability) of
        Nothing -> Spec.assertFailure s "Razorfin Abolisher's ability should declare one target slot"
        Just theSlot -> do
          let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot board
          Spec.assertEqWith s "CR 122.1 exactly the countered creature" legal (Set.singleton (Recipient.ToCreature giantId))
          Spec.assertBool s (not (Set.member (Recipient.ToCreature pikerId) legal)) "the counterless one is not in it"
          Spec.assertBool s (not (Set.member (Recipient.ToCreature (abolisherOf board)) legal)) "nor alice's own counterless Abolisher"

-- Razorfin Abolisher's board: alice's settled Abolisher and two Islands for the
-- {1}{U}, bob's Hill Giant carrying a stun counter and his counterless Goblin
-- Piker. Nothing if the printing stopped declaring exactly one ability.
razorfinBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Maybe (GameState.GameState, ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card), ObjectId.ObjectId, ObjectId.ObjectId))
razorfinBoard s registry = do
  abolisher <- S.printingOf s registry "Razorfin Abolisher"
  island <- S.printingOf s registry "Island"
  giant <- S.printingOf s registry "Hill Giant"
  piker <- S.printingOf s registry "Goblin Piker"
  pure $ case soleActivatedAbility abolisher of
    Nothing -> Nothing
    Just ability ->
      let (_, g1) = S.addPermanent abolisher S.alice (Setup.emptyGame S.bothPlayers)
          -- CR 302.6: the Abolisher's cost carries {T}, so it must have settled.
          settled = S.runPure S.identityAnswer g1 (Engine.settleAll S.alice)
          (_, g2) = S.addPermanent island S.alice settled
          (_, g3) = S.addPermanent island S.alice g2
          (giantId, g4) = S.addPermanent giant S.bob g3
          (pikerId, g5) = S.addPermanent piker S.bob g4
          board = (S.addCounter CounterKind.Stun 1 giantId g5) {GameState.priority = Just S.alice}
       in Just (board, ability, giantId, pikerId)

-- Activate `ability` off `srcId` aimed at `oid`, and resolve it. One answerer
-- serves both halves -- CR 602.2b's announcement and the resolution -- which is
-- the shape the Withered Wretch cases above already have.
activateAt :: ObjectId.ObjectId -> GameState.GameState -> ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> GameState.GameState
activateAt oid board srcId ability =
  S.runPure (aimAtCreature oid) (S.runPure (aimAtCreature oid) board (Activate.activateAbility S.alice srcId ability)) Stack.resolveTop

-- The Abolisher on the battlefield, by printed name. Read off the board rather
-- than returned by the fixture, which keeps its tuple to the two candidates the
-- cases discriminate between. S.noSource when it is not there, which no case
-- reaches -- the fixture put it on the battlefield.
abolisherOf :: GameState.GameState -> ObjectId.ObjectId
abolisherOf gs =
  Maybe.fromMaybe
    S.noSource
    ( List.find
        (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Razorfin Abolisher")))
        (Set.toList (GameState.battlefield gs))
    )

-- The printed names of the cards in `pid`'s hand. CR 400.7 makes the returned
-- permanent a new object, so an assertion about what moved reads the NAME.
namesInHand :: PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
namesInHand pid gs = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers Zone.Hand pid gs)

-- Aim every target slot at one creature, falling back to the smallest legal
-- recipient when the offer does not hold it -- which is what makes "the engine
-- never offered it" observable as a different permanent moving.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just oid) . Recipient.objectOf) sets
  _ -> S.identityAnswer p

-- alice's graveyard holds a Goblin Piker (mana value 2), a Russet Wolves (mana
-- value 4) and a Lightning Bolt (mana value 1), with `gained` life gained this
-- turn planted in the CR 608.2i log Game.lifeGainedThisTurn folds.
--
-- Three DISTINCT mana values, and the Bolt is the one UNDER every bound the cases
-- use: it is kept out by the And's other conjunct alone, so a filter that had
-- stopped narrowing by card type would show up here rather than pass.
celestineGraveyard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Natural.Type.Natural -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
celestineGraveyard piker wolves bolt gained =
  let (pikerId, g1) = S.addGraveyardCard piker S.alice (Setup.emptyGame S.bothPlayers)
      (wolvesId, g2) = S.addGraveyardCard wolves S.alice g1
      (boltId, g3) = S.addGraveyardCard bolt S.alice g2
   in (pikerId, wolvesId, boltId, S.withEvents [GameEvent.LifeGained (LifeChange.MkLifeChange S.alice gained)] g3)

-- Celestine, the Living Saint ({4}{W} Legendary Creature -- Human Warrior 3/4,
-- Oracle text verified against Scryfall): "Flying, lifelink / Healing Tears -- At
-- the beginning of your end step, return target creature card with mana value X
-- or less from your graveyard to the battlefield, where X is the amount of life
-- you gained this turn."
--
-- THE card the computed mana-value bound was waiting for. CR 601.2c chooses the
-- target and CR 608.2b re-reads it, both through
-- Pawl.Engine.Target.admittedGiven, which is the one site that fills
-- Filter.Context.slotAmount -- off the slot's own Quantity.LifeGainedThisTurn.
--
-- The bound is NOT a printed literal, and the two cases below are what say so: a
-- card out of range on one board is in range on another that differs only in how
-- much life was gained, and a board with no gain at all admits nothing.
celestineSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
celestineSpec s registry = Spec.describe s "ManaValueAtMostAmount (CR 202.3)" $ do
  -- THE PROVING CASE, and the MOVING-BOUND control in one: the same graveyard
  -- judged at two life totals. At 2 the Wolves are out of range; at 4 they are in
  -- it, with nothing else about the board changed.
  Spec.it s "CR 202.3 / 601.2c the bound moves with the board, not with the card" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    wolves <- S.printingOf s registry "Russet Wolves"
    bolt <- S.printingOf s registry "Lightning Bolt"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    case triggerTargetSlot celestine of
      Nothing -> Spec.assertFailure s "Celestine should declare one triggered ability with one target slot"
      Just theSlot -> do
        let (pikerId, wolvesId, boltId, atTwo) = celestineGraveyard piker wolves bolt 2
            (_, _, _, atFour) = celestineGraveyard piker wolves bolt 4
            legal = Target.legalRecipients (Just S.alice) S.noSource theSlot
        Spec.assertEqWith s "the fixture planted 2 life gained" (Game.lifeGainedThisTurn atTwo S.alice) 2
        Spec.assertEqWith s "and 4 on the other board" (Game.lifeGainedThisTurn atFour S.alice) 4
        Spec.assertBool s (Set.member (Recipient.ToObject pikerId) (legal atTwo)) "mana value 2 is within a bound of 2"
        Spec.assertBool s (not (Set.member (Recipient.ToObject wolvesId) (legal atTwo))) "mana value 4 is not"
        Spec.assertBool s (Set.member (Recipient.ToObject wolvesId) (legal atFour)) "and at 4 gained it is -- the bound moved"
        Spec.assertBool s (not (Set.member (Recipient.ToObject boltId) (legal atFour))) "the instant card under every bound is still out (the And narrows by card type)"
        Spec.assertEqWith s "so the wider board offers both creature cards and nothing else" (legal atFour) (Set.fromList [Recipient.ToObject pikerId, Recipient.ToObject wolvesId])
  -- THE ZERO control, built as the same board differing in exactly one thing: no
  -- life gained, so nothing in a graveyard of mana values 1, 2 and 4 is in range.
  -- Without it a bound the engine simply ignored would pass the case above.
  Spec.it s "CR 202.3 with no life gained this turn the slot admits nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    wolves <- S.printingOf s registry "Russet Wolves"
    bolt <- S.printingOf s registry "Lightning Bolt"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    case triggerTargetSlot celestine of
      Nothing -> Spec.assertFailure s "Celestine should declare one triggered ability with one target slot"
      Just theSlot -> do
        let (pikerId, wolvesId, _, atZero) = celestineGraveyard piker wolves bolt 0
        Spec.assertEqWith s "the fixture planted no life gained" (Game.lifeGainedThisTurn atZero S.alice) 0
        Spec.assertEqWith s "and the slot admits nothing at all" (Target.legalRecipients (Just S.alice) S.noSource theSlot atZero) Set.empty
        -- The same two cards ARE offered once the bound reaches them, so the empty
        -- set above is the bound talking and not an empty graveyard.
        let (_, _, _, atFour) = celestineGraveyard piker wolves bolt 4
        Spec.assertEqWith
          s
          "while the same graveyard at 4 gained offers both"
          (Target.legalRecipients (Just S.alice) S.noSource theSlot atFour)
          (Set.fromList [Recipient.ToObject pikerId, Recipient.ToObject wolvesId])
  -- CR 202.3's bound read off the ANNOUNCEMENT rather than off the board: a
  -- Quantity naming a SLOT. CR 601.2c matches the slot as the ability is
  -- announced -- CR 603.3d importing that rule for a trigger -- and slotContext
  -- evaluates the bound against CR 113.7's source, a permanent that carries no
  -- announcement binding, so the answer has to come from the environment the
  -- caller hands over (Filter.Context's boundAmounts).
  --
  -- Three boards differing in exactly one thing, the seed on the SAME slot over
  -- the SAME graveyard: an announcement of 2, one of 4, and one binding nothing.
  -- "thatMuch" is Pawl.Engine.Binding.eventAmount, the name a triggered ability's
  -- own CR 603.2 event stamps -- warsingerSpec below is the printed card, driven
  -- through combat.
  Spec.it s "CR 603.2 a bound naming a slot reads the announcement's amount" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    wolves <- S.printingOf s registry "Russet Wolves"
    bolt <- S.printingOf s registry "Lightning Bolt"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    case triggerTargetSlot celestine of
      Nothing -> Spec.assertFailure s "Celestine should declare one triggered ability with one target slot"
      Just theSlot -> do
        -- No life gained, so the printed Quantity this slot replaces would admit
        -- nothing: every offer below is the seeded amount talking.
        let (pikerId, wolvesId, _, noGain) = celestineGraveyard piker wolves bolt 0
            (sourceId, board) = S.addPermanent piker S.alice noGain
            name = SlotName.MkSlotName (Text.pack "target")
            slotted = theSlot {TargetSlot.amount = Just (Quantity.Type.InSlot Binding.eventAmount)}
            offer seed = Map.findWithDefault Set.empty name (Target.legalSets (Just S.alice) False seed sourceId (Map.singleton name slotted) board)
            announcing n = Map.singleton Binding.eventAmount (Binding.toAmount n)
        Spec.assertEqWith s "an announcement of 2 reaches the mana value 2 card alone" (offer (announcing 2)) (Set.singleton (Recipient.ToObject pikerId))
        Spec.assertEqWith s "and one of 4 reaches the mana value 4 card as well" (offer (announcing 4)) (Set.fromList [Recipient.ToObject pikerId, Recipient.ToObject wolvesId])
        Spec.assertEqWith s "while an announcement binding no amount admits nothing" (offer Map.empty) Set.empty
        Spec.assertBool s (not (Set.member (Recipient.ToObject sourceId) (offer (announcing 4)))) "the source itself is on the battlefield, so the graveyard pool leaves it out"

-- Venerable Warsinger ({1}{R}{W} Creature -- Spirit Cleric 3/3, Oracle text
-- verified against Scryfall): "Vigilance, trample / Whenever this creature deals
-- combat damage to a player, you may return target creature card with mana value
-- X or less from your graveyard to the battlefield, where X is the amount of
-- damage this creature dealt to that player."
--
-- THE card the announcement-read bound was waiting for, and the one celestineSpec
-- cannot reach: Celestine's X is a fact about the BOARD, so its slot is
-- answerable against CR 113.7's source alone. This X is a fact about the
-- ANNOUNCEMENT -- the amount CR 603.2's event stamped under
-- Pawl.Engine.Binding.eventAmount -- and CR 603.3d chooses the target before the
-- ability object on the stack carries any binding at all, so nothing on the board
-- can answer it.
--
-- THREE DISTINCT READINGS of one board, so the offered set names one and rejects
-- two. A -1/-1 counter makes the Warsinger a 2/2 before it connects, so the event
-- carries 2 rather than the printed 3, and alice's graveyard holds a creature card
-- at each of mana value 2, 3 and 4:
--
--   * the event's amount (2) admits the Piker alone -- the printed rule;
--   * the source's printed power (3) would admit the Tyrant too;
--   * a bound that went unanswered admits nothing, which is CR 603.3d's removal.
--
-- The answerer PREFERS every card the rule excludes, so a widened bound is
-- observable as a different permanent arriving rather than as nothing happening --
-- a Lightning Bolt UNDER every bound among them, since it is kept out by the And's
-- other conjunct alone and a filter that had stopped narrowing by card type would
-- otherwise be invisible (the fallback takes the smallest legal recipient, which
-- is the Piker either way).
--
-- THREE SEATS, so "your graveyard" (CR 109.5's you, alice) is a different zone
-- from the damaged player's.
warsingerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
warsingerSpec s registry =
  let board = do
        warsinger <- S.printingOf s registry "Venerable Warsinger"
        piker <- S.printingOf s registry "Goblin Piker"
        tyrant <- S.printingOf s registry "Kalakscion, Hunger Tyrant"
        wolves <- S.printingOf s registry "Russet Wolves"
        bolt <- S.printingOf s registry "Lightning Bolt"
        let (gs0, mine, _, _) = S.threePlayerCombat [warsinger] [] []
            (pikerId, g1) = S.addGraveyardCard piker S.alice gs0
            (tyrantId, g2) = S.addGraveyardCard tyrant S.alice g1
            (wolvesId, g3) = S.addGraveyardCard wolves S.alice g2
            (boltId, g4) = S.addGraveyardCard bolt S.alice g3
            shrunk = List.foldl' (flip (S.addCounter CounterKind.MinusOneMinusOne 1)) g4 mine
            -- The same seam questingBeastSpec uses: the declarations run as
            -- steps, the damage is dealt by hand, and settleForPriority places
            -- the trigger -- so `placed` is the state with the ability on the
            -- stack and its target already chosen.
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) (warsingerPlan [tyrantId, wolvesId, boltId]) shrunk
            fought = S.runPure (warsingerPlan [tyrantId, wolvesId, boltId]) atDamage Damage.dealCombatDamage
            placed = S.runPure (warsingerPlan [tyrantId, wolvesId, boltId]) fought Engine.settleForPriority
        pure (mine, pikerId, shrunk, placed, S.runPure (warsingerPlan [tyrantId, wolvesId, boltId]) placed Engine.priorityLoop)
   in Spec.describe s "ManaValueAtMostAmount (CR 202.3)" $ do
        -- The announcement itself, read off the placed ability rather than
        -- inferred from what happened -- so this says what the event stamped and
        -- what the slot ADMITTED against it.
        Spec.it s "CR 603.3d the slot admits only the card the event's amount reaches" $ do
          (_, pikerId, _, placed, _) <- board
          case GameState.stack placed of
            [abilityId] -> do
              let bindings = maybe Map.empty Object.bindings (Game.lookupObject abilityId placed)
              Spec.assertEqWith s "the graveyard card the event's 2 reaches is the one target chosen" (Map.lookup (SlotName.MkSlotName (Text.pack "target")) (Binding.targetsOf bindings)) (Just (Set.singleton (Recipient.ToObject pikerId)))
              Spec.assertEqWith s "and the event stamped 2 under CR 603.2's own slot" (Binding.amountOf Binding.eventAmount bindings) (Just 2)
            _ -> Spec.assertFailure s "fixture should place exactly one trigger"

-- Attacks bob, takes the printed "may", and aims every target slot at the
-- graveyard cards the rule EXCLUDES -- falling back to the smallest legal
-- recipient, which is what makes a widened bound observable as a different
-- permanent arriving.
warsingerPlan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
warsingerPlan bait p = case p of
  Prompt.ChooseDefender {} -> S.bob
  Prompt.ChooseTargets _ _ _ asked -> S.preferring (maybe False (`elem` bait) . Recipient.objectOf) asked
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.aggressiveAnswer p

-- Stir the Grave ({X}{B} Sorcery, BOK, paper, Oracle text fetched from Scryfall
-- this session and transcribed whole): "Return target creature card with mana
-- value X or less from your graveyard to the battlefield."
--
-- The SPELL half of the announcement-read bound, where warsingerSpec above is the
-- trigger half. The number is CR 601.2b's announced X rather than CR 603.2's event
-- amount, and the two roads differ in more than which slot the bound names:
--
--   * CR 601.2b names X one step BEFORE CR 601.2c chooses the target, so the
--     value exists when the offer is computed and Pawl.Engine.Cast.castProposed
--     hands it over as the seed. That is the second case here.
--   * CR 700.2a's fillability gate runs EARLIER STILL -- before the mode choice
--     rule 601.2b lists first, and so before any X exists at all. A bound read
--     there states nothing, because rule 601.2b puts no ceiling on the value the
--     caster may name; the gate refuses the spell for what the announcement
--     cannot change and for nothing else. That is the first case, built as a PAIR
--     of graveyards differing in one card.
--
-- The gameplay case is Warsinger's board one rule over: alice's graveyard holds a
-- creature card at each of mana value 2, 3 and 4 plus a Lightning Bolt UNDER every
-- bound among them, and the answerer PREFERS every card the announced X excludes,
-- so a widened bound is observable as a different permanent arriving rather than as
-- nothing happening. The Bolt is what makes a filter that had stopped narrowing by
-- card type visible, since the fallback takes the smallest legal recipient either
-- way.
stirTheGraveSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
stirTheGraveSpec s registry =
  let boardOf graveyard swamps = do
        stir <- S.printingOf s registry "Stir the Grave"
        swamp <- S.printingOf s registry "Swamp"
        cards <- traverse (S.printingOf s registry) graveyard
        let (gs0, stirId) = S.boltInHand swamp stir swamps Phase.PrecombatMain
            (ids, gs1) = List.foldl' (\(acc, g) c -> let (oid, g2) = S.addGraveyardCard c S.alice g in (acc <> [oid], g2)) ([], gs0) cards
        pure (stirId, ids, gs1)
   in Spec.describe s "ManaValueAtMostAmount (CR 202.3)" $ do
        -- CR 700.2a asked before CR 601.2b exists, as a pair of boards differing in
        -- their one graveyard card: the same three Swamps either way, a mana value
        -- 4 CREATURE card on one and an instant on the other. A gate that read the
        -- unannounced bound as a bound would refuse BOTH -- mana value 4 is not "4
        -- or less" of an X nobody has named -- and a gate that had stopped narrowing
        -- altogether would offer both. The negative half is carried by the slot's
        -- card-type conjunct rather than by the bound, which is the point: the
        -- permissive floor drops the bound and leaves everything else standing.
        Spec.it s "CR 601.2b the castability gate states no bound the announcement has not made" $ do
          (creatureBoard, _, withCreature) <- boardOf ["Russet Wolves"] 3
          (instantBoard, _, withInstant) <- boardOf ["Lightning Bolt"] 3
          Spec.assertBool s (S.castable S.alice creatureBoard withCreature) "the mana value 4 creature card is reachable by some X, so the cast is offered"
          Spec.assertBool s (not (S.castable S.alice instantBoard withInstant)) "and no X reaches an instant, so it is not"

-- Synthetic Borrowed Exhumation ({X}{B} Sorcery,
-- data/cards/synthetic-borrowed-exhumation.json): "Return target creature card
-- with mana value X or less from target player's graveyard to the battlefield."
--
-- SYNTHETIC, and the search that settled it: Scryfall, 2026-08-31, with a
-- User-Agent -- o:"mana value X or less" (95 printings), o:"power X or less"
-- (8), and o:/X or less/ minus those two (7), every one read. Every X-bounded
-- target slot Magic has printed draws from a pool no other slot names and
-- carries a filter naming none either, so no printing pairs the two halves. Stir
-- the Grave above is the bound alone; Dwell on the Past is the sibling-slot pool
-- alone. Nothing in CR 202.3 or 601.2c forbids one card printing both, which is
-- what makes the synthetic legitimate rather than a shape the rules exclude.
--
-- Those two halves on ONE slot are the whole point: the card slot is jointly
-- judged (Target.jointlyJudged, because its pool is CR 400.1's graveyard scoped
-- to whatever the player slot names) AND its CR 202.3 computed bound reads CR
-- 601.2b's announced X. The offer is computed against the seed carrying that X;
-- the joint check re-derives the same slot, and it is handed the same seed. Given
-- the chosen targets alone the bound reads no number, Filter.ManaValueAtMostAmount
-- is vacuously False, the card the caster was OFFERED is not in the re-derived
-- set, and CR 601.2e reverses a casting rule 601.2c allows; see #2676.
--
-- THREE SEATS for Dwell on the Past's reason: with alice and bob alone, "bob's
-- graveyard" and "not the caster's graveyard" pick out the same cards.
--
-- The two runs differ in EXACTLY ONE thing: which graveyard the mana value 2
-- creature card sits in. Both announce X = 2, both name bob in the player slot,
-- both pay the same {2}{B} off the same three Swamps, off one board.
borrowedExhumationSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
borrowedExhumationSpec s registry = Spec.describe s "ManaValueAtMostAmount (CR 202.3)" $ do
  Spec.it s "CR 601.2c the joint check re-derives a jointly judged slot against the announced X" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    wolves <- S.printingOf s registry "Russet Wolves"
    evangel <- S.printingOf s registry "Cabal Evangel"
    exhumation <- S.printingOf s registry "Synthetic Borrowed Exhumation"
    let (hisId, g1) = S.addGraveyardCard piker S.bob (S.landsFor swamp S.alice 3 S.threePlayerGame)
        (hisBigId, g2) = S.addGraveyardCard wolves S.bob g1
        (hersId, g3) = S.addGraveyardCard evangel S.carol g2
        (board, spellId) = S.handOne exhumation g3
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace exhumation))
        -- The OFFER, taken through the same door Pawl.Engine.Cast.castProposed
        -- takes it through and against the same seed: CR 601.2b's X and nothing
        -- else. It is what the two runs below are judged against, and it is
        -- insensitive to the joint check -- which is what lets the pair say the
        -- offer and the re-check agree rather than merely that something was
        -- rejected.
        offered = Target.legalSets (Just S.alice) False (Binding.fromChoices Map.empty (Just 2) mempty) S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
        run oid =
          let cast = S.runPure (aimingExhumation 2 oid) board (S.cast S.alice spellId)
           in (cast, S.runPure (aimingExhumation 2 oid) cast Stack.resolveTop)
        (castAtBob, atBob) = run hisId
        (castAtCarol, _) = run hersId
    Spec.assertEqWith s "CR 601.2c the card slot is offered the UNION over the player slot, narrowed by the announced X" (slotNamed "card") (Set.fromList (fmap Recipient.ToObject [hisId, hersId]))
    Spec.assertEqWith s "and the player slot every player" (slotNamed "player") (Set.fromList (fmap Recipient.ToPlayer [S.alice, S.bob, S.carol]))
    -- THE GAMEPLAY ASSERTION, ahead of every proxy: the card the offer named came
    -- back, so the joint check read the same X the offer did.
    Spec.assertEqWith s "CR 202.3 naming bob and the mana value 2 card in HIS graveyard, that card is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.bob atBob) 1
    Spec.assertEqWith s "CR 601.2h so the casting was not reversed: all three Swamps paid {2}{B}" (S.tappedCount S.alice atBob) 3
    Spec.assertEqWith s "with the mana value 4 card in the same graveyard left behind, unoffered" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Russet Wolves")) S.bob atBob) 0
    Spec.assertEqWith s "and his card gone from his graveyard (CR 400.7: a new object arrived)" (Game.zoneMembers Zone.Graveyard S.bob atBob) [hisBigId]
    Spec.assertEqWith s "CR 601.2e naming bob and a card in CAROL's graveyard, the joint check rejects and the whole cast is reversed" (length (GameState.stack castAtCarol)) 0
    Spec.assertEqWith s "so carol's card stayed in her graveyard" (Game.zoneMembers Zone.Graveyard S.carol castAtCarol) [hersId]
    Spec.assertBool s (elem spellId (Game.zoneMembers Zone.Hand S.alice castAtCarol)) "and the spell is back in alice's hand"
    Spec.assertEqWith s "where naming his own card put it on the stack" (length (GameState.stack castAtBob)) 1

-- CR 601.2c's whole announcement for Synthetic Borrowed Exhumation: bob in the
-- player slot and `oid` in the card slot, with CR 601.2b's X announced first.
--
-- PINNED rather than searched, aimingDwell's reason: the run naming carol's card
-- beside bob has to be rejected by the JOINT check, so an answerer that filtered
-- against the offer would hand back an empty slot and the announcement would
-- fail on its count instead -- passing for a reason the case is not about.
aimingExhumation :: Natural.Type.Natural -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingExhumation x oid p = case p of
  Prompt.ChooseX {} -> x
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const 1) offers
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot _ ->
          if slot == SlotName.MkSlotName (Text.pack "player")
            then Set.singleton (Recipient.ToPlayer S.bob)
            else Set.singleton (Recipient.ToObject oid)
      )
      asked
  _ -> S.identityAnswer p

-- CR 603.3d's announcement for Itzquinth's reflexive ability, threaded through a
-- counter because a pure `Prompt r -> r` cannot tell a re-ask from the first ask
-- (Pawl.CopySpec's countingAnswer shape): call 0 names `dealer` in BOTH slots,
-- and every call after it names `victim` in the victim slot.
--
-- FILTERED out of the offered set rather than built, aimingHammer's reason -- so
-- the refused answer also depends on the victim slot being OFFERED the dealer's
-- own candidate, which is CR 601.2c's union.
answeringItzquinth :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
answeringItzquinth dealer victim p = case p of
  Prompt.ChooseToPay {} -> pure PaymentDecision.Pays
  Prompt.ChooseTargets _ _ _ asked -> do
    asked_ <- State.get
    State.modify' (+ 1)
    let wanted slot = if asked_ == 0 || slot == SlotName.MkSlotName (Text.pack "dealer") then dealer else victim
    pure (Map.mapWithKey (\slot (_, offered) -> Set.filter ((==) (Just (wanted slot)) . Recipient.objectOf) offered) asked)
  _ -> pure (S.identityAnswer p)

-- CR 603.3d: "The remainder of the process for putting a triggered ability on the
-- stack is identical to the process for casting a spell listed in rules
-- 601.2c-d." Rules 601.2c-d and no further, so CR 601.2e's return to before the
-- proposal is not a trigger's remedy: an announcement CR 601.2c refuses is asked
-- again, and the rule's own removal is reserved for the board where "no legal
-- choices can be made" at all.
--
-- Itzquinth, Firstborn of Gishath {R}{G} Legendary Creature -- Dinosaur 2/3
-- (data/cards/itzquinth-firstborn-of-gishath.json): "Haste. When Itzquinth
-- enters, you may pay {2}. When you do, target Dinosaur you control deals damage
-- equal to its power to another target creature." (name, cost, type line, P/T and
-- Oracle text checked against api.scryfall.com, 2026-09-02). Nothing is omitted,
-- so pawl's card is neither stricter nor weaker than printed.
--
-- Fall of the Hammer's mutually dependent pair -- the victim slot is
-- Not (IsBound "dealer") -- hung on a CR 603.12 reflexive ability, so the
-- announcement is made as THAT ability goes on the stack (Engine.placeBorne)
-- rather than as a spell is cast (Cast.castProposed). It is the pool's only card
-- whose jointly judged slots are on a triggered ability.
--
-- TWO BOARDS differing in exactly ONE thing: whether bob has a Wall of Stone. The
-- same two Mountains pay the same {2} on both, and the same Itzquinth enters the
-- same way, so neither answer is about mana or about the payment.
itzquinthSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
itzquinthSpec s registry = Spec.describe s "Announcing a trigger's targets (CR 603.3d)" $ do
  Spec.it s "CR 603.3d a trigger's announcement naming one creature in both slots is asked again" $ do
    mountain <- S.printingOf s registry "Mountain"
    wall <- S.printingOf s registry "Wall of Stone"
    itzquinth <- S.printingOf s registry "Itzquinth, Firstborn of Gishath"
    let (wallId, withWall) = S.addPermanent wall S.bob (S.landsInPlay mountain 2)
        (dinoId, together) = S.entersWithTrigger itzquinth S.alice withWall
        (aloneId, alone) = S.entersWithTrigger itzquinth S.alice (S.landsInPlay mountain 2)
        -- The CR 603.6a enters trigger onto the stack, then its resolution (where
        -- CR 118.12's {2} is paid and CR 603.12's reflexive is armed), then the
        -- settle that puts the reflexive itself on the stack -- which is the
        -- moment CR 603.3d announces its targets.
        place board = S.runPure S.identityAnswer board Engine.settleForPriority
        arm dealer victim board = State.runState (Engine.runGame (answeringItzquinth dealer victim) (place board) (Stack.resolveTop >> Engine.settleForPriority)) 0
        ((_, placed), asks) = arm dinoId wallId together
        after = S.runPure S.identityAnswer placed Stack.resolveTop
        ((_, ceased), _) = arm aloneId aloneId alone
    -- The gameplay-level assertions first: the re-asked announcement is the one
    -- that stands, so Itzquinth's two damage reaches the Wall and not itself.
    -- Without the joint check the first answer stands, the victim slot holds
    -- Itzquinth, and CR 608.2b drops it at resolution -- leaving the Wall at zero.
    Spec.assertEqWith s "the coherent answer stands: the Wall took Itzquinth's two damage" (S.damageOf wallId after) (Just 2)
    Spec.assertEqWith s "and Itzquinth, named in both slots by the refused answer, took none" (S.damageOf dinoId after) (Just 0)
    -- CR 603.3d's own removal, on the board where no coherent announcement
    -- exists: Itzquinth is the only creature, so it fills each slot alone and
    -- neither slot's candidate leaves the other one anything.
    Spec.assertEqWith s "with no second creature, the reflexive ability is removed from the stack (CR 603.3d)" (length (GameState.stack ceased)) 0
    Spec.assertEqWith s "so it never dealt itself its own two damage" (S.damageOf aloneId ceased) (Just 0)
    -- The proxies last. The payment happened on BOTH boards, so the removal above
    -- is the announcement and not a gate that skipped the reflexive entirely.
    Spec.assertEqWith s "alice paid {2} on the board with the Wall" (S.tappedCount S.alice placed) 2
    Spec.assertEqWith s "and on the board without it" (S.tappedCount S.alice ceased) 2
    Spec.assertEqWith s "the reflexive ability really reached the stack when a coherent answer existed" (length (GameState.stack placed)) 1
    Spec.assertEqWith s "and alice was asked twice for it: the incoherent answer was refused and the announcement asked again" asks 2

  -- The same board, and an interpreter that answers OUTSIDE the offer with a value
  -- it has never used before. Recipient is unbounded and Target.chooseTargets does
  -- not validate its answer, so keying the refused announcements on the raw answer
  -- would re-ask forever; Engine.placeBorne keys on the answer narrowed to the
  -- offer, which is drawn from a finite set.
  --
  -- The Wall is still there, so a coherent announcement EXISTS on this board: the
  -- removal below is the decider's refusal to make one and not CR 603.3d's own
  -- "no legal choices can be made", which the case above already covers.
  --
  -- A regression here HANGS rather than fails, Pawl.ReplacementSpec's shape. This
  -- group carries no Tasty.localOption budget, so what fences it is the --timeout
  -- flake.nix's testFlags pass; run bare, the mutation never returns.
  Spec.it s "CR 603.3d an announcement outside the offer, made afresh each time, is refused rather than asked forever" $ do
    mountain <- S.printingOf s registry "Mountain"
    wall <- S.printingOf s registry "Wall of Stone"
    itzquinth <- S.printingOf s registry "Itzquinth, Firstborn of Gishath"
    let (wallId, withWall) = S.addPermanent wall S.bob (S.landsInPlay mountain 2)
        (dinoId, together) = S.entersWithTrigger itzquinth S.alice withWall
        placed = S.runPure S.identityAnswer together Engine.settleForPriority
        ((_, ceased), asks) = State.runState (Engine.runGame (answeringItzquinthAfresh dinoId) placed (Stack.resolveTop >> Engine.settleForPriority)) 0
    -- The gameplay-level assertions first.
    Spec.assertEqWith s "the reflexive ability is removed from the stack rather than announced forever" (length (GameState.stack ceased)) 0
    Spec.assertEqWith s "so the Wall took no damage" (S.damageOf wallId ceased) (Just 0)
    Spec.assertEqWith s "and neither did Itzquinth" (S.damageOf dinoId ceased) (Just 0)
    -- The proxies last. The payment happened, so the removal is the announcement
    -- rather than a gate that skipped the reflexive ability entirely.
    Spec.assertEqWith s "alice paid {2}" (S.tappedCount S.alice ceased) 2
    Spec.assertEqWith s "and was asked exactly twice: the second fresh answer narrows to the same offer as the first" asks 2

-- CR 603.3d's announcement for Itzquinth's reflexive ability from an interpreter
-- that will not make a legal one: the dealer slot is filled out of the offer, and
-- the victim slot is named a FRESH object id -- one this board never held -- on every
-- call, so no two answers are equal.
answeringItzquinthAfresh :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
answeringItzquinthAfresh dealer p = case p of
  Prompt.ChooseToPay {} -> pure PaymentDecision.Pays
  Prompt.ChooseTargets _ _ _ asked -> do
    asked_ <- State.get
    State.modify' (+ 1)
    let fresh = Recipient.ToObject (ObjectId.MkObjectId (1000 + Int.toNaturalSaturating asked_))
        answer slot (_, offered) =
          if slot == SlotName.MkSlotName (Text.pack "dealer")
            then Set.filter ((==) (Just dealer) . Recipient.objectOf) offered
            else Set.singleton fresh
    pure (Map.mapWithKey answer asked)
  _ -> pure (S.identityAnswer p)

-- CR 603.3d imports rules 601.2c-d WHOLE, and rule 601.2c's first act is the
-- NUMBER: "if the spell has a variable number of targets, the player announces
-- how many targets they will choose before they announce those targets. In some
-- cases, the number of targets will be defined by the spell's text." A count the
-- slot's text refuses is therefore an illegal announcement on this road exactly
-- as it is on a cast's, and Engine.placeBorne asks Target.selectionLegal whole
-- rather than its joint conjunct alone.
--
-- Only an interpreter that ignores the offer can produce one: Target.chooseTargets
-- clamps the count it OFFERS into the slot's own range, so a decider answering
-- the prompt as posed never announces a number to refuse. That is what makes this
-- assertion an engine-level one -- no card in data/cards/ can force the
-- divergence, the announcement being the engine's own prompt in every printing.
--
-- Ravenous Rats {1}{B} Creature -- Rat 1/1 (data/cards/ravenous-rats.json): "When
-- this creature enters, target opponent discards a card." (name, cost, type line,
-- P/T and Oracle text checked against api.scryfall.com, 2026-09-13.) Nothing is
-- omitted, so pawl's card is neither stricter nor weaker than printed.
--
-- THREE SEATS, which is what makes two opponents an over-count rather than the
-- one answer the slot allows; each of them holds exactly one card, so the discard
-- is visible as an empty hand and the two seats are told apart.
ravenousRatsSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
ravenousRatsSpec s registry = Spec.describe s "A trigger's announced count (CR 603.3d)" $ do
  Spec.it s "CR 601.2c through CR 603.3d announcing two targets for a one-target slot is asked again" $ do
    rats <- S.printingOf s registry "Ravenous Rats"
    swamp <- S.printingOf s registry "Swamp"
    let (_, bobHolds) = S.addHandCard swamp S.bob S.threePlayerGame
        (_, held) = S.addHandCard swamp S.carol bobHolds
        (_, entered) = S.entersWithTrigger rats S.alice held
        ((_, after), asks) = State.runState (Engine.runGame overCounting entered (Engine.settleForPriority >> Stack.resolveTop)) 0
    -- The gameplay-level assertions first. The re-asked announcement is the one
    -- that stands, and it names bob alone; without the count conjunct the first
    -- answer stands and CAROL discards too, both hands reading zero.
    Spec.assertEqWith s "the re-asked announcement stands: bob, the one target it named, discarded" (S.handSize S.bob after) 0
    Spec.assertEqWith s "and carol, named only by the refused two-target answer, still holds her card" (S.handSize S.carol after) 1
    -- The proxies last. The trigger really resolved, so the hands above are the
    -- announcement and not an ability removed from the stack before it ran.
    Spec.assertEqWith s "the ability resolved rather than being removed from the stack" (GameState.stack after) []
    Spec.assertEqWith s "and alice was asked twice: the over-counted answer was refused and the announcement asked again" asks 2

-- CR 603.3d's announcement for Ravenous Rats from an interpreter that ignores the
-- offered count: the FIRST answer names every opponent the slot offers, which is
-- two for a slot whose text fixes one, and every answer after it takes the
-- announced number instead. Two answers rather than one, so what is proved is the
-- re-ask and not the removal a decider that never answers legally degrades to.
overCounting :: Prompt.Prompt r -> State.State Int r
overCounting p = case p of
  Prompt.ChooseTargets _ _ _ asked -> do
    asked_ <- State.get
    State.modify' (+ 1)
    pure (if asked_ == 0 then fmap snd asked else S.preferring (const True) asked)
  _ -> pure (S.identityAnswer p)

-- CR 115.7d's two halves over a jointly judged slot: Bioshift's `to` must be
-- "another target creature with the same controller" as its `from`. alice casts
-- it from her Goblin Piker to her other Piker; bob's Redirect moves `from` to his
-- Tomakul Honor Guard ("Ward {2}") and leaves `to` where it is. Two boards
-- differing in one thing: whether the second Piker died in response.
--
-- Dead, `to` was ALREADY illegal and may stay unchanged, so the re-target
-- stands and ward fires. Alive, the new `from` is what makes `to` illegal
-- ("must not cause any unchanged targets to become illegal"), so it is refused.
redirectBioshiftSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
redirectBioshiftSpec s registry =
  let from = SlotName.MkSlotName (Text.pack "from")
      to = SlotName.MkSlotName (Text.pack "to")
      run killed = do
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        guard <- S.printingOf s registry "Tomakul Honor Guard"
        piker <- S.printingOf s registry "Goblin Piker"
        bioshift <- S.printingOf s registry "Bioshift"
        redirect <- S.printingOf s registry "Redirect"
        let lands = S.landsFor island S.bob 2 (S.landsFor forest S.alice 1 S.threePlayerGame)
            (withBioshift, bioshiftId) = S.handOne bioshift lands
            (redirectId, g1) = S.addHandCard redirect S.bob withBioshift
            (guardId, g2) = S.addPermanent guard S.bob g1
            (giverId, g3) = S.addPermanent piker S.alice g2
            (takerId, board) = S.addPermanent piker S.alice g3
            aimed = Map.fromList [(from, Recipient.ToCreature giverId), (to, Recipient.ToCreature takerId)]
            cast = S.runPure (aimingSlots S.alice aimed) board (S.cast S.alice bioshiftId)
            responded = if killed then S.runPure S.identityAnswer cast (Event.destroy Regenerability.Regenerable [takerId]) else cast
        case topOfStack cast of
          Nothing -> Spec.assertFailure s "Bioshift never reached the stack"
          Just spell -> do
            let bobCast = S.runPure (aimingAs S.bob (Recipient.ToObject spell)) responded (S.cast S.bob redirectId)
                redirected = S.runPure (aimingSlots S.bob (Map.insert from (Recipient.ToCreature guardId) aimed)) bobCast (Stack.resolveTop >> Engine.settleForPriority)
                targets = maybe Map.empty (flip Map.restrictKeys (Map.keysSet aimed) . Binding.targetsOf . Object.bindings) (Game.lookupObject spell redirected)
            pure (aimed, targets, length (GameState.stack redirected))
   in Spec.describe s "Redirect over Bioshift (CR 115.7d)" $ do
        Spec.it s "CR 115.7d / 702.21a an unchanged target already illegal may stay, and the new one draws ward" $ do
          (aimed, targets, depth) <- run True
          Spec.assertEqWith s "CR 702.21a the Guard became a target, so its ward trigger sits over Bioshift" depth 2
          Spec.assertEqWith s "and `to` still names the dead Piker" (Map.lookup to targets) (fmap Set.singleton (Map.lookup to aimed))
        Spec.it s "CR 115.7d a new target that makes an unchanged target illegal is refused" $ do
          (aimed, targets, depth) <- run False
          Spec.assertEqWith s "no ward trigger: the re-target was refused" depth 1
          Spec.assertEqWith s "and Bioshift keeps both its targets" targets (fmap Set.singleton aimed)

-- CR 115.7a over Giant Growth: alice aims it at her Goblin Piker (2/1), and
-- bob's Deflection changes its target. `others` are the other creatures on the
-- board, each controlled by the seat it names. bob's answerer asks for the Piker
-- back whenever it is offered and for carol's Llanowar Elves otherwise, so a
-- re-aim that offered the current target ("another legal target") would leave
-- Giant Growth where it was.
deflectionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
deflectionSpec s registry =
  let run others = do
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        piker <- S.printingOf s registry "Goblin Piker"
        growth <- S.printingOf s registry "Giant Growth"
        deflection <- S.printingOf s registry "Deflection"
        printings <- traverse (\(who, name) -> fmap ((,) who) (S.printingOf s registry name)) others
        let lands = S.landsFor island S.bob 4 (S.landsFor forest S.alice 1 S.threePlayerGame)
            (withGrowth, growthId) = S.handOne growth lands
            (deflectionId, g1) = S.addHandCard deflection S.bob withGrowth
            (pikerId, g2) = S.addPermanent piker S.alice g1
            place (ids, g) (who, printing) = let (oid, g') = S.addPermanent printing who g in (ids <> [oid], g')
            (otherIds, board) = List.foldl' place ([], g2) printings
            cast = S.runPure (aimingAs S.alice (Recipient.ToCreature pikerId)) board (S.cast S.alice growthId)
        case topOfStack cast of
          Nothing -> Spec.assertFailure s "Giant Growth never reached the stack" >> pure (pikerId, otherIds, cast)
          Just spell -> do
            let bobCast = S.runPure (aimingAs S.bob (Recipient.ToObject spell)) cast (S.cast S.bob deflectionId)
                deflected = S.runPure (wantingBack pikerId otherIds) bobCast Stack.resolveTop
            pure (pikerId, otherIds, S.runPure S.identityAnswer deflected Stack.resolveTop)
   in Spec.describe s "Deflection (CR 115.7a)" $ do
        Spec.it s "CR 115.7a the target changes only to ANOTHER legal target, the controller's choice" $ do
          (pikerId, otherIds, after) <- run [(S.bob, "Hill Giant"), (S.carol, "Llanowar Elves")]
          case otherIds of
            [giantId, elvesId] -> do
              -- THE GAMEPLAY ASSERTION.
              Spec.assertEqWith s "CR 115.7a Giant Growth pumped the Elves bob chose" (S.powerToughnessOf elvesId after) (Just (4, 4))
              Spec.assertEqWith s "and not the Piker it was cast at" (S.powerToughnessOf pikerId after) (Just (2, 1))
              Spec.assertEqWith s "nor the Giant bob did not choose" (S.powerToughnessOf giantId after) (Just (3, 3))
            _ -> Spec.assertFailure s "the board should hold two other creatures"
        -- bob's answerer names neither the Piker nor the Elves here, so a prompt
        -- would be answered with nothing and rejected: the Giant can be reached
        -- only by the change being made without asking.
        Spec.it s "CR 115.7a with one other legal target the change is made, unasked" $ do
          (pikerId, otherIds, after) <- run [(S.bob, "Hill Giant")]
          Spec.assertEqWith s "CR 115.7a Giant Growth pumped the Giant" (traverse (`S.powerToughnessOf` after) otherIds) (Just [(6, 6)])
          Spec.assertEqWith s "and not the Piker" (S.powerToughnessOf pikerId after) (Just (2, 1))
        -- A REGRESSION FENCE rather than a proven line: no mutation of
        -- changeTargetsFor reaches this board, since with nothing offered every
        -- answer is rejected anyway.
        Spec.it s "CR 115.7a with no other legal target the original target is unchanged" $ do
          (pikerId, _, after) <- run []
          Spec.assertEqWith s "CR 115.7a Giant Growth still pumped the Piker" (S.powerToughnessOf pikerId after) (Just (5, 4))
        -- Two boards differing in one thing: whether Twisted Fealty's second
        -- "target" names the same Hill Giant its first does. Deflection's ruling:
        -- "If a spell targets the same player or object multiple times, you can't
        -- target it with Deflection."
        Spec.it s "CR 601.2c a spell targeting one object through two instances of target does not have a single target" $ do
          fealtyCast <- traverse (deflectFealty s registry) [False, True]
          Spec.assertEqWith s "CR 601.2c Deflection reaches the stack over the single-target Fealty only" fealtyCast [2, 1]

-- bob's answer to Deflection's re-aim: the Piker back whenever it is offered,
-- otherwise the SECOND other creature (carol's Elves), filtered out of the offer.
wantingBack :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
wantingBack piker otherIds p = case p of
  Prompt.ChooseTargets _ player _ asked
    | player == S.bob ->
        let back = Set.filter (== Recipient.ToCreature piker) . snd
            elves = Set.filter (\r -> any (\oid -> Recipient.objectOf r == Just oid) (take 1 (drop 1 otherIds))) . snd
         in fmap (\offer -> if Set.null (back offer) then elves offer else back offer) asked
  _ -> S.identityAnswer p

-- alice's announcement of Twisted Fealty at `giantId`: one target in the Role
-- slot as well when `twice`, none otherwise.
aimFealty :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimFealty twice giantId p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> Map.mapWithKey (\slot _ -> if twice || slot == SlotName.MkSlotName (Text.pack "target") then 1 else 0) offers
  Prompt.ChooseTargets _ _ _ asked -> fmap (Set.filter (== Recipient.ToCreature giantId) . snd) asked
  _ -> S.identityAnswer p

-- Twisted Fealty cast by alice at bob's Hill Giant, its up-to-one Role slot left
-- empty or aimed at the same Giant, then bob's Deflection cast at it with the
-- same four Islands either way. Answers the stack depth afterwards.
deflectFealty :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m Int
deflectFealty s registry twice = do
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  giant <- S.printingOf s registry "Hill Giant"
  fealty <- S.printingOf s registry "Twisted Fealty"
  deflection <- S.printingOf s registry "Deflection"
  let lands = S.landsFor island S.bob 4 (S.landsFor mountain S.alice 3 S.threePlayerGame)
      (withFealty, fealtyId) = S.handOne fealty lands
      (deflectionId, g1) = S.addHandCard deflection S.bob withFealty
      (giantId, board) = S.addPermanent giant S.bob g1
      cast = S.runPure (aimFealty twice giantId) board (S.cast S.alice fealtyId)
  case topOfStack cast of
    Nothing -> Spec.assertFailure s "Twisted Fealty never reached the stack" >> pure 0
    Just spell -> pure (length (GameState.stack (S.runPure (aimingAs S.bob (Recipient.ToObject spell)) cast (S.cast S.bob deflectionId))))

-- Answer every target prompt put to `who` by FILTERING each slot's offered set
-- down to the one recipient `aimed` names for it.
aimingSlots :: PlayerId.PlayerId -> Map.Map SlotName.SlotName Recipient.Recipient -> Prompt.Prompt r -> r
aimingSlots who aimed p = case p of
  Prompt.ChooseTargets _ player _ asked | player == who -> Map.mapWithKey (\slot (_, offered) -> Set.filter ((== Map.lookup slot aimed) . Just) offered) asked
  _ -> S.identityAnswer p

-- Answer every target prompt put to `who` by FILTERING the offered set down to
-- `recipient`; a target prompt put to another seat gets the identity answer.
aimingAs :: PlayerId.PlayerId -> Recipient.Recipient -> Prompt.Prompt r -> r
aimingAs who recipient p = case p of
  Prompt.ChooseTargets _ player _ asked | player == who -> fmap (\(_, offered) -> Set.filter (== recipient) offered) asked
  _ -> S.identityAnswer p

topOfStack :: GameState.GameState -> Maybe ObjectId.ObjectId
topOfStack = Maybe.listToMaybe . GameState.stack
