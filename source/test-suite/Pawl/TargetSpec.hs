{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Target: CR 115 target legality, and the rule-702 TARGETING
-- RESTRICTIONS that narrow it. Shroud (CR 702.18) is the pool's first, printed
-- by Blurred Mongoose, and hexproof (CR 702.11) is the second, printed by
-- Slippery Bogle. Their cases sit together because the pair is only interesting
-- together: the two rules differ in exactly one thing, whether the restriction
-- reads WHO is targeting. One case covers the other side of the
-- restriction/admission split -- Pawl.Engine.Sba's CR 303.4c re-check, which
-- asks what an enchant spec ADMITS and must not ask a targeting question -- and
-- the last ten cover
-- CR 115.2's two escape hatches from "only permanents are legal targets": its
-- clause (b) as Cancel and Stifle, its clause (a) as Raise Dead, Withered Wretch
-- and Riftsweeper. (Those letters are prose inside rule 115.2, not subrule
-- numbers; there is no CR 115.2a.)
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
-- Gameplay-level: every spec under test is read out of a committed card rather
-- than hand-built, and the cases that turn on an effect cast and resolve through
-- the stack.
module Pawl.TargetSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
-- The logic module, alongside Pawl.Types.Modal below: unambiguous under one alias
-- because the two export disjoint names (CardSpec's precedent).
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- The one target spec a single-slot card or ability declares, read out of the
-- committed printing (S.spellTargetSpec's rationale) but keyed by COUNT rather
-- than by slot name: Cancel calls its slot "spell", not "target".
soleTargetSpec :: Modal.Modal Card.Type.Card -> Maybe TargetSpec.TargetSpec
soleTargetSpec modal = case Map.elems (Modal.allTargetSpecs modal) of
  [only] -> Just only
  _ -> Nothing

-- `pid` controls the restricted creature -- a Blurred Mongoose or a Slippery
-- Bogle -- and a Goblin Piker, and nothing else. The Piker is the CONTROL in
-- every rule-702 case below: it is a legal target of everything the restricted
-- creature is not, so "the restricted creature is excluded" cannot pass because
-- the whole legal set is empty.
restrictionBoard :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
restrictionBoard restricted piker pid =
  let gs0 = Setup.emptyGame S.bothPlayers
      (restrictedId, gs1) = S.addCreature restricted pid gs0
      (pikerId, gs2) = S.addCreature piker pid gs1
   in (restrictedId, pikerId, gs2)

-- Aims every target slot at one chosen card, tagged the way
-- Pool.CardsInGraveyard tags its candidates (Recipient.ToObject -- the
-- candidates are CARDS, not creatures). S.identityAnswer would answer with
-- Set.lookupMin instead, which is whichever graveyard card happens to have the
-- lowest id -- no way to say "bob's".
aimAtCard :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCard oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject oid)) sets
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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Target" $ do
  -- CR 702.18a: "Shroud is a static ability. 'Shroud' means 'This permanent or
  -- player can't be the target of spells or abilities.'" Doom Blade is "target
  -- nonblack creature" and the Mongoose is green, so its Filter admits the
  -- Mongoose and only the restriction can remove it.
  Spec.it s "CR 702.18a an opponent's Doom Blade cannot target Blurred Mongoose" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (mongooseId, pikerId, gs) = restrictionBoard mongoose piker S.bob
    case S.spellTargetSpec doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSpec -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSpec gs
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
    case S.spellTargetSpec doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSpec -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSpec gs
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
        (_, masked) = S.addCreature ivoryMask S.bob bare
    case S.spellTargetSpec bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSpec -> do
        let legalFor who = Target.legalRecipients (Just who) S.noSource theSpec
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
    let (_, warded) = S.addCreature leyline S.bob (Setup.emptyGame S.bothPlayers)
    case S.spellTargetSpec bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSpec -> do
        let legalFor who = Target.legalRecipients (Just who) S.noSource theSpec warded
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
            let aim candidates =
                  if Set.member (Recipient.ToPlayer S.bob) candidates
                    then Just (Recipient.ToPlayer S.bob)
                    else Set.lookupMin candidates
             in Map.mapMaybe aim sets
          _ -> S.identityAnswer p
        castAt guard =
          let base = S.landsInPlay mountain 1
              withGuard = maybe base (\g -> snd (S.addCreature g S.bob base)) guard
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
        (_, oneMask) = S.addCreature ivoryMask S.bob base
        (_, bothMasked) = S.addCreature ivoryMask S.alice oneMask
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
    case Maybe.mapMaybe (soleTargetSpec . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace sorcerer)) of
      [theSpec] -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSpec gs
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
    case S.spellTargetSpec doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSpec -> do
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature mongooseId) (Target.legalRecipients (Just S.alice) S.noSource theSpec board)))
          "before Humility the Mongoose is untargetable"
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature mongooseId) (Target.legalRecipients (Just S.alice) S.noSource theSpec humbled))
          "under Humility it is a legal target"

  -- CR 115.10a: "Just because an object or player is being affected by a spell
  -- or ability doesn't make that object or player a target of that spell or
  -- ability." Day of Judgment names no target at all, so shroud has nothing to
  -- say about it -- the classic way to get this rule wrong is to make the
  -- restriction a property of being AFFECTED.
  Spec.it s "CR 115.10a Day of Judgment still destroys Blurred Mongoose: shroud restricts targeting, not effects" $ do
    plains <- S.printingOf s registry "Plains"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let base = S.landsInPlay plains 4 -- {2}{W}{W}
        (_, b1) = S.addCreature mongoose S.bob base
        (_, b2) = S.addCreature piker S.bob b1
        (gs, dojId) = S.handOne dayOfJudgment b2
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dojId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- By NAME, not by id: CR 400.7 makes the graveyard incarnation a new
        -- object, and Event.changeZone mints it a fresh ObjectId.
        buried = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid after)) (Game.zoneMembers Zone.Graveyard S.bob after)
    Spec.assertEqWith s "both of bob's creatures are gone" (S.creaturesInPlay S.bob after) 0
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Blurred Mongoose") buried) "the Mongoose itself is in bob's graveyard"
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Goblin Piker") buried) "and so is the Piker beside it"

  -- CR 113.6: "Abilities of an instant or sorcery spell usually function only
  -- while that object is on the stack. Abilities of all other objects usually
  -- function only while that object is on the battlefield." Shroud is printed on
  -- a CREATURE card, so a Blurred Mongoose SPELL has none and Cancel targets it
  -- legally. It is CR 113.6g -- "an object's ability that states it can't be
  -- countered ... functions on the stack" -- that then saves it, which is the
  -- card's other half and a different rule.
  Spec.it s "CR 113.6 Cancel legally targets the Blurred Mongoose spell, and CR 113.6g stops it countering" $ do
    island <- S.printingOf s registry "Island"
    cancel <- S.printingOf s registry "Cancel"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    let base = S.landsInPlay island 3
        (spellId, onStack) = S.spellOnStack mongoose S.bob base
        (gs, cancelId) = S.handOne cancel onStack
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice cancelId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    case soleTargetSpec (Face.spell (S.combinedFace cancel)) of
      Nothing -> Spec.assertFailure s "Cancel should declare one target slot"
      Just theSpec ->
        Spec.assertBool
          s
          (Set.member (Recipient.ToObject spellId) (Target.legalRecipients (Just S.alice) S.noSource theSpec gs))
          "the Mongoose spell is a legal target on the stack"
    Spec.assertBool s (elem spellId (GameState.stack resolved)) "and is still on the stack, uncountered"
    Spec.assertEqWith s "Cancel resolved into alice's graveyard regardless" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1

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
    case soleTargetSpec (Face.spell (S.combinedFace cancel)) of
      Nothing -> Spec.assertFailure s "Cancel should declare one target slot"
      Just theSpec -> do
        let legal = Target.legalRecipients (Just S.alice) cancelId theSpec gs
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
    let (sorcererId, gs) = S.addCreature sorcerer S.alice (Setup.emptyGame S.bothPlayers)
    case Maybe.mapMaybe (soleTargetSpec . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace sorcerer)) of
      [theSpec] ->
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature sorcererId) (Target.legalRecipients (Just S.alice) sorcererId theSpec gs))
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
        (pikerId, board) = S.addCreature piker S.bob base
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
        (pikerId, g1) = S.addCreature piker S.alice gs0
        (auraId, g2) = S.addCreature setessanTraining S.alice g1
        attached = S.attach auraId pikerId g2
        shrouded = S.withEffect pikerId (Modification.GainKeyword Keyword.Shroud) attached
    Spec.assertBool s (Set.member auraId (GameState.battlefield (S.settleSba attached))) "the Aura stays put with no shroud around"
    Spec.assertBool s (Set.member auraId (GameState.battlefield (S.settleSba shrouded))) "and stays put once its host has shroud"

  -- CR 702.11b: "'Hexproof' on a permanent means 'This permanent can't be the
  -- target of spells or abilities your opponents control.'" Slippery Bogle's
  -- entire printed rules text is the keyword, so a case that uses this printing
  -- is asking about 702.11b and nothing else.
  --
  -- BOTH HALVES ON ONE BOARD, with one spec and one Doom Blade, because the
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
    case S.spellTargetSpec doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSpec -> do
        let mine = Target.legalRecipients (Just S.alice) S.noSource theSpec gs
            theirs = Target.legalRecipients (Just S.bob) S.noSource theSpec gs
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
    case Maybe.mapMaybe (soleTargetSpec . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace sorcerer)) of
      [theSpec] -> do
        let mine = Target.legalRecipients (Just S.alice) S.noSource theSpec gs
            theirs = Target.legalRecipients (Just S.bob) S.noSource theSpec gs
        Spec.assertBool s (Set.member (Recipient.ToCreature bogleId) mine) "alice's own ability may point at her Bogle"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature bogleId) theirs)) "bob's may not"
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) theirs) "though bob's may point at the Piker"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) theirs) "and at a player (CR 115.4)"
      _ -> Spec.assertFailure s "Prodigal Sorcerer should print one ability with one target slot"

  -- The whole card, cast and resolved through the stack rather than asked as a
  -- set membership: alice's own Doom Blade destroys her own Slippery Bogle. The
  -- Bogle is the only creature on the battlefield, so CR 601.2c's choice is
  -- forced -- were 702.11b read as 702.18a's shroud, the spell would have no
  -- legal target at all and the Bogle would live.
  Spec.it s "CR 702.11b whole card: alice's Doom Blade destroys her own Slippery Bogle" $ do
    swamp <- S.printingOf s registry "Swamp"
    bogle <- S.printingOf s registry "Slippery Bogle"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let base = S.landsInPlay swamp 2 -- {1}{B}
        (_, board) = S.addCreature bogle S.alice base
        (gs, dbId) = S.handOne doomBlade board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dbId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- By NAME, not by id: CR 400.7 makes the graveyard incarnation a new
        -- object, and Event.changeZone mints it a fresh ObjectId.
        buried = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid after)) (Game.zoneMembers Zone.Graveyard S.alice after)
    Spec.assertEqWith s "Doom Blade went on the stack" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "alice's own creature is gone" (S.creaturesInPlay S.alice after) 0
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Slippery Bogle") buried) "the Bogle itself is in alice's graveyard"

  -- CR 115.10a: "Just because an object or player is being affected by a spell
  -- or ability doesn't make that object or player a target of that spell or
  -- ability." Day of Judgment names no target at all, so hexproof has nothing to
  -- say about it -- even though alice, who casts it, is exactly the opponent
  -- rule 702.11b names. The classic way to get this rule wrong is to make the
  -- restriction a property of being AFFECTED.
  Spec.it s "CR 115.10a Day of Judgment still destroys Slippery Bogle: hexproof restricts targeting, not effects" $ do
    plains <- S.printingOf s registry "Plains"
    bogle <- S.printingOf s registry "Slippery Bogle"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let base = S.landsInPlay plains 4 -- {2}{W}{W}
        (_, b1) = S.addCreature bogle S.bob base
        (_, b2) = S.addCreature piker S.bob b1
        (gs, dojId) = S.handOne dayOfJudgment b2
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dojId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        buried = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid after)) (Game.zoneMembers Zone.Graveyard S.bob after)
    Spec.assertEqWith s "both of bob's creatures are gone" (S.creaturesInPlay S.bob after) 0
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Slippery Bogle") buried) "the Bogle itself is in bob's graveyard"
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Goblin Piker") buried) "and so is the Piker beside it"

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
    case S.spellTargetSpec doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSpec -> do
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature bogleId) (Target.legalRecipients (Just S.alice) S.noSource theSpec board)))
          "before Humility alice cannot target bob's Bogle"
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature bogleId) (Target.legalRecipients (Just S.alice) S.noSource theSpec humbled))
          "under Humility it is a legal target"

  -- CR 608.2b: "If the spell or ability specifies targets, it checks whether the
  -- targets are still legal. ... If all its targets, for every instance of the
  -- word 'target,' are now illegal, the spell or ability doesn't resolve." The
  -- second of CR 115's two moments, and the controller axis has to be read at
  -- both: a Goblin Piker that gains hexproof in response is out of an OPPONENT's
  -- Doom Blade and still squarely in its own controller's.
  --
  -- Four resolutions off two boards that differ only in who controls the Piker,
  -- so neither answer can be a Doom Blade that never worked. No card in this pool
  -- GRANTS hexproof, so the grant is a stored layer-6 continuous effect
  -- (S.withEffect), as the shroud case above does it.
  Spec.it s "CR 608.2b gaining hexproof in response fizzles an opponent's Doom Blade but not its controller's" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let castAt controller =
          let (pikerId, board) = S.addCreature piker controller (S.landsInPlay swamp 2)
              (gs, dbId) = S.handOne doomBlade board
           in (pikerId, snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dbId)))
        resolve g = snd (Engine.runGamePure S.identityAnswer g Stack.resolveTop)
        (theirPiker, atTheirs) = castAt S.bob
        (myPiker, atMine) = castAt S.alice
        hexproofed oid = S.withEffect oid (Modification.GainKeyword (Keyword.Hexproof Nothing))
    Spec.assertEqWith s "untouched, bob's Piker dies" (S.creaturesInPlay S.bob (resolve atTheirs)) 0
    Spec.assertEqWith s "hexproofed in response, it survives alice's Doom Blade" (S.creaturesInPlay S.bob (resolve (hexproofed theirPiker atTheirs))) 1
    Spec.assertEqWith s "untouched, alice's own Piker dies" (S.creaturesInPlay S.alice (resolve atMine)) 0
    Spec.assertEqWith s "and hexproof does not save it from its own controller (CR 702.11b)" (S.creaturesInPlay S.alice (resolve (hexproofed myPiker atMine))) 0

  -- CR 113.9, the whole rule, as two DISJOINT pools: "activated and triggered
  -- abilities on the stack aren't spells, and therefore can't be countered by
  -- anything that counters only spells. Activated and triggered abilities on
  -- the stack can be countered by effects that specifically counter abilities."
  --
  -- One board with one of each on the stack -- alice's Goblin Piker spell, and
  -- her Prodigal Sorcerer's activated ability above it -- so neither exclusion
  -- can pass by the pool simply being empty. Cancel ("counter target spell",
  -- Pool.Spells) and Stifle ("counter target activated or triggered ability",
  -- Pool.Abilities) are read off their committed printings, so this is the
  -- printed text and not a hand-built spec.
  --
  -- CR 115.2 is what admits either at all: "only permanents are legal targets
  -- ... unless a spell or ability ... (b) targets an object that can't exist on
  -- the battlefield, such as a spell or ability."
  Spec.it s "CR 113.9 Stifle's pool holds only the ability and Cancel's only the spell" $ do
    cancel <- S.printingOf s registry "Cancel"
    stifle <- S.printingOf s registry "Stifle"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    piker <- S.printingOf s registry "Goblin Piker"
    case Face.activatedAbilities (S.combinedFace sorcerer) of
      [] -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      ability : _ -> do
        let (srcId, withSorcerer) = S.addCreature sorcerer S.alice (Setup.emptyGame S.bothPlayers)
            -- CR 302.6: the Sorcerer has to have settled before its {T} is legal.
            settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.alice)
            (spellId, onStack) = S.spellOnStack piker S.alice settled
            gs = S.runPure S.identityAnswer (onStack {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice srcId ability)
            abilIds = filter (/= spellId) (GameState.stack gs)
            -- soleTargetSpec, not S.spellTargetSpec: neither card calls its slot
            -- "target" -- Cancel's is "spell" and Stifle's is "ability".
            legalFor printing = fmap (\theSpec -> Target.legalRecipients (Just S.bob) S.noSource theSpec gs) (soleTargetSpec (Face.spell (S.combinedFace printing)))
        case (abilIds, legalFor cancel, legalFor stifle) of
          ([abilId], Just cancelLegal, Just stifleLegal) -> do
            Spec.assertEqWith s "Cancel sees the spell and only the spell" cancelLegal (Set.singleton (Recipient.ToObject spellId))
            Spec.assertEqWith s "Stifle sees the ability and only the ability" stifleLegal (Set.singleton (Recipient.ToObject abilId))
          _ -> Spec.assertFailure s "the fixture should put one ability and one spell on the stack, and both cards should declare a target slot"

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
  --     and graveyard" -- is why the pool carries a PlayerScope at all, and
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
    let (inPlayId, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (mineId, g2) = S.addGraveyardCard piker S.alice g1
        (myBoltId, g3) = S.addGraveyardCard bolt S.alice g2
        (theirsId, gs) = S.addGraveyardCard piker S.bob g3
    case S.spellTargetSpec raiseDead of
      Nothing -> Spec.assertFailure s "Raise Dead should declare a target slot"
      Just theSpec -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSpec gs
        Spec.assertBool s (Set.member (Recipient.ToObject mineId) legal) "the creature card in alice's own graveyard is legal"
        Spec.assertBool s (not (Set.member (Recipient.ToObject theirsId) legal)) "the identical card in bob's graveyard is not (CR 400.1)"
        Spec.assertBool s (not (Set.member (Recipient.ToObject myBoltId) legal)) "nor is the instant card beside it (the Filter narrows)"
        Spec.assertBool s (not (Set.member (Recipient.ToObject inPlayId) legal)) "nor the Piker on the battlefield, under ToObject"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature inPlayId) legal)) "nor under ToCreature (disjoint from Pool.Creatures)"
        Spec.assertEqWith s "and nothing else at all" legal (Set.singleton (Recipient.ToObject mineId))

  -- The move itself, cast and resolved through the stack: CR 400.7 mints a NEW
  -- object in the hand ("an object that moves from one zone to another becomes a
  -- new object"), so the card is asserted by name in the hand and by the absence
  -- of the id it was targeted under from the graveyard. Raise Dead's own trip to
  -- the graveyard is CR 404.1's "as is any instant or sorcery spell that's
  -- finished resolving".
  Spec.it s "CR 115.2 clause (a) whole card: Raise Dead returns the targeted creature card to alice's hand" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (mineId, board) = S.addGraveyardCard piker S.alice (S.landsInPlay swamp 1)
        (gs, rdId) = S.handOne raiseDead board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice rdId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "Raise Dead went on the stack" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "the Piker card is in alice's hand" (S.countByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice resolved) 1
    Spec.assertBool
      s
      (notElem mineId (Game.zoneMembers Zone.Graveyard S.alice resolved))
      "and the object it was targeted under is gone from the graveyard (CR 400.7)"
    Spec.assertEqWith s "that graveyard holds one card, the spent Raise Dead (CR 404.1)" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
    Spec.assertEqWith s "and alice's hand holds one card, the Piker and not the spell" (S.handSize S.alice resolved) 1

  -- CR 608.2b: "A target that's no longer in the zone it was in when it was
  -- targeted is illegal. ... If all its targets ... are now illegal, the spell or
  -- ability doesn't resolve. It's removed from the stack and, if it's a spell,
  -- put into its owner's graveyard."
  --
  -- The response is Event.changeZone rather than a card, because no card in this
  -- pool can be cast in response to a sorcery AND move a card out of a graveyard:
  -- Rest in Peace is the only one that empties a graveyard, and it does it from
  -- an enchantment's enters trigger -- CR 303.1 lets an enchantment be cast only
  -- "during a main phase of their turn when the stack is empty", which is exactly
  -- when Raise Dead is not on it. Both halves run off one board and one cast, so
  -- the fizzle cannot be a Raise Dead that never worked.
  Spec.it s "CR 608.2b Raise Dead fizzles when its target leaves the graveyard in response" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (mineId, board) = S.addGraveyardCard piker S.alice (S.landsInPlay swamp 1)
        (gs, rdId) = S.handOne raiseDead board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice rdId))
        resolve g = snd (Engine.runGamePure S.identityAnswer g Stack.resolveTop)
        returned = resolve cast
        fizzled = resolve (S.runPure S.identityAnswer cast (Event.changeZone mineId Zone.Exile))
    Spec.assertEqWith s "untouched, the Piker card comes back" (S.countByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice returned) 1
    Spec.assertEqWith s "exiled in response, nothing comes back" (S.countByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice fizzled) 0
    Spec.assertEqWith s "and Raise Dead is in alice's graveyard either way" (length (Game.zoneMembers Zone.Graveyard S.alice fizzled)) 1

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
    let (inPlayId, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (mineId, g2) = S.addGraveyardCard piker S.alice g1
        (myBoltId, g3) = S.addGraveyardCard bolt S.alice g2
        (theirsId, gs) = S.addGraveyardCard piker S.bob g3
        wretchSpecs = Maybe.mapMaybe (soleTargetSpec . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace wretch))
    case (wretchSpecs, S.spellTargetSpec raiseDead) of
      ([wretchSpec], Just raiseDeadSpec) -> do
        let legal theSpec = Target.legalRecipients (Just S.alice) S.noSource theSpec gs
        Spec.assertEqWith
          s
          "the Wretch offers every card in every graveyard, of every card type, and nothing else"
          (legal wretchSpec)
          (Set.fromList (fmap Recipient.ToObject [mineId, myBoltId, theirsId]))
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature inPlayId) (legal wretchSpec)))
          "and not the Piker on the battlefield under ToCreature either (disjoint from Pool.Creatures)"
        Spec.assertEqWith
          s
          "while Raise Dead, on those same graveyards, still reaches only alice's creature card"
          (legal raiseDeadSpec)
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
    let (wretchId, g1) = S.addCreature wretch S.alice (S.landsInPlay swamp 1)
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

  -- CR 608.2b for an ABILITY rather than a spell, and against the opponent's
  -- graveyard: "A target that's no longer in the zone it was in when it was
  -- targeted is illegal. ... If all its targets ... are now illegal, the spell or
  -- ability doesn't resolve."
  --
  -- The response moves the card to bob's HAND rather than exiling it, so the two
  -- outcomes are told apart by the exile zone: a fizzle leaves it empty, and an
  -- ability that resolved anyway would put something in it. Both halves run off
  -- one activation, so the fizzle cannot be an activation that never worked.
  Spec.it s "CR 608.2b Withered Wretch's activation fizzles when the card leaves the graveyard in response" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    wretch <- S.printingOf s registry "Withered Wretch"
    let (wretchId, g1) = S.addCreature wretch S.alice (S.landsInPlay swamp 1)
        (theirsId, g2) = S.addGraveyardCard piker S.bob g1
        board = g2 {GameState.priority = Just S.alice}
    case Face.activatedAbilities (S.combinedFace wretch) of
      [ability] -> do
        let activated = S.runPure (aimAtCard theirsId) board (Activate.activateAbility S.alice wretchId ability)
            resolve g = S.runPure (aimAtCard theirsId) g Stack.resolveTop
            exiled = resolve activated
            fizzled = resolve (S.runPure S.identityAnswer activated (Event.changeZone theirsId Zone.Hand))
        Spec.assertEqWith s "the activation put one ability on the stack" (length (GameState.stack activated)) 1
        Spec.assertEqWith s "untouched, bob's card is exiled" (length (Game.zoneMembers Zone.Exile S.bob exiled)) 1
        Spec.assertEqWith s "taken to his hand in response, nothing is exiled at all" (length (Game.zoneMembers Zone.Exile S.bob fizzled)) 0
        Spec.assertEqWith s "and the card is still in his hand" (length (Game.zoneMembers Zone.Hand S.bob fizzled)) 1
        Spec.assertEqWith s "with the ability off the stack either way" (length (GameState.stack fizzled)) 0
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
    let (inPlayId, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (buriedId, g2) = S.addGraveyardCard piker S.alice g1
        (hersId, g3) = S.addExiledCard piker S.alice g2
        (hisId, gs) = S.addExiledCard bolt S.bob g3
        riftSpecs = Maybe.mapMaybe (soleTargetSpec . TriggeredAbility.modal) (Face.triggeredAbilities (S.combinedFace riftsweeper))
        wretchSpecs = Maybe.mapMaybe (soleTargetSpec . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace wretch))
    case (riftSpecs, wretchSpecs) of
      ([riftSpec], [wretchSpec]) -> do
        let legal theSpec = Target.legalRecipients (Just S.alice) S.noSource theSpec gs
        Spec.assertEqWith
          s
          "both exiled cards, hers and his, of both card types, and nothing else"
          (legal riftSpec)
          (Set.fromList (fmap Recipient.ToObject [hersId, hisId]))
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature inPlayId) (legal riftSpec)))
          "not the Piker on the battlefield under ToCreature either (disjoint from Pool.Creatures)"
        Spec.assertEqWith
          s
          "while Withered Wretch, on that same board, still reaches only the graveyard card"
          (legal wretchSpec)
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
  -- (Game.insertIntoZone appends a library arrival), so the reversed run's
  -- leading position cannot be merely where the move put the card.
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
