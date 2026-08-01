-- Covers Pawl.Engine.Target: CR 115 target legality, and the rule-702 TARGETING
-- RESTRICTIONS that narrow it. Shroud (CR 702.18) is the pool's first, and
-- Blurred Mongoose is the card that prints it. One case covers the other side of
-- that split -- Pawl.Engine.Sba's CR 303.4c re-check, which asks what an enchant
-- spec ADMITS and must not ask a targeting question -- and the last four cover
-- CR 115.2's two escape hatches from "only permanents are legal targets": its
-- clause (b) as Cancel and Stifle, its clause (a) as Raise Dead. (Those letters
-- are prose inside rule 115.2, not subrule numbers; there is no CR 115.2a.)
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
import qualified Pawl.Engine.Cast as Cast
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
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone

-- The one target spec a single-slot card or ability declares, read out of the
-- committed printing (S.spellTargetSpec's rationale) but keyed by COUNT rather
-- than by slot name: Cancel calls its slot "spell", not "target".
soleTargetSpec :: Modal.Modal Card.Type.Card -> Maybe TargetSpec.TargetSpec
soleTargetSpec modal = case Map.elems (Modal.allTargetSpecs modal) of
  [only] -> Just only
  _ -> Nothing

-- `pid` controls a Blurred Mongoose and a Goblin Piker, and nothing else. The
-- Piker is the CONTROL in every shroud case below: it is a legal target of
-- everything the Mongoose is not, so "the Mongoose is excluded" cannot pass
-- because the whole legal set is empty.
shroudBoard :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
shroudBoard mongoose piker pid =
  let gs0 = Setup.emptyGame S.bothPlayers
      (mongooseId, gs1) = S.addCreature mongoose pid gs0
      (pikerId, gs2) = S.addCreature piker pid gs1
   in (mongooseId, pikerId, gs2)

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
    let (mongooseId, pikerId, gs) = shroudBoard mongoose piker S.bob
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
    let (mongooseId, pikerId, gs) = shroudBoard mongoose piker S.alice
    case S.spellTargetSpec doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSpec -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSpec gs
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) legal) "alice may target her own Piker"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature mongooseId) legal)) "but not her own Mongoose"

  -- CR 702.18a says "spells OR ABILITIES", so the gate cannot live on the cast
  -- path. Prodigal Sorcerer's "{T}: This creature deals 1 damage to any target"
  -- is an activated ability with a Pool.AnyTarget slot (CR 115.4), and the same
  -- legality call serves it.
  Spec.it s "CR 702.18a shroud stops an ability's target too, not only a spell's" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (mongooseId, pikerId, gs) = shroudBoard mongoose piker S.alice
    case Maybe.mapMaybe (soleTargetSpec . ActivatedAbility.modal) (Card.Type.activatedAbilities (Printing.card sorcerer)) of
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
    let (mongooseId, _, board) = shroudBoard mongoose piker S.bob
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice dojId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- By NAME, not by id: CR 400.7 makes the graveyard incarnation a new
        -- object, and Event.changeZone mints it a fresh ObjectId.
        buried = Maybe.mapMaybe (\oid -> fmap Card.Type.name (Game.cardOf oid after)) (Game.zoneMembers Zone.Graveyard S.bob after)
    Spec.assertEqWith s "both of bob's creatures are gone" (S.creaturesInPlay S.bob after) 0
    Spec.assertBool s (elem (Text.pack "Blurred Mongoose") buried) "the Mongoose itself is in bob's graveyard"
    Spec.assertBool s (elem (Text.pack "Goblin Piker") buried) "and so is the Piker beside it"

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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice cancelId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    case soleTargetSpec (Card.Type.spell (Printing.card cancel)) of
      Nothing -> Spec.assertFailure s "Cancel should declare one target slot"
      Just theSpec ->
        Spec.assertBool
          s
          (Set.member (Recipient.ToObject spellId) (Target.legalRecipients (Just S.alice) S.noSource theSpec gs))
          "the Mongoose spell is a legal target on the stack"
    Spec.assertBool s (elem spellId (GameState.stack resolved)) "and is still on the stack, uncountered"
    Spec.assertEqWith s "Cancel resolved into alice's graveyard regardless" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1

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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice dbId))
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
    case Card.Type.activatedAbilities (Printing.card sorcerer) of
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
            legalFor printing = fmap (\theSpec -> Target.legalRecipients (Just S.bob) S.noSource theSpec gs) (soleTargetSpec (Card.Type.spell (Printing.card printing)))
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
  --     and graveyard" -- is why the pool carries a PlayerRelation at all, and
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice rdId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "Raise Dead went on the stack" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "the Piker card is in alice's hand" (S.countByName (Text.pack "Goblin Piker") S.alice resolved) 1
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice rdId))
        resolve g = snd (Engine.runGamePure S.identityAnswer g Stack.resolveTop)
        returned = resolve cast
        fizzled = resolve (S.runPure S.identityAnswer cast (Event.changeZone mineId Zone.Exile))
    Spec.assertEqWith s "untouched, the Piker card comes back" (S.countByName (Text.pack "Goblin Piker") S.alice returned) 1
    Spec.assertEqWith s "exiled in response, nothing comes back" (S.countByName (Text.pack "Goblin Piker") S.alice fizzled) 0
    Spec.assertEqWith s "and Raise Dead is in alice's graveyard either way" (length (Game.zoneMembers Zone.Graveyard S.alice fizzled)) 1
