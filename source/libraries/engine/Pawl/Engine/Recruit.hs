-- | CR 701.70, recruit: "draw a card, then discard a card", plus the token the
-- rule prints, and the whole of the keyword action.
--
-- Pawl.Engine.Populate's sibling, and standing on the same ground: rule 701 is a
-- keyword-action rule exactly as rule 702 is a keyword rule, so both the
-- procedure and the token CR 701.70a prints live in the engine rather than in
-- card data. The closed\/open invariant forbids the rules core casing on an
-- EFFECT's identity, and nothing here does -- Pawl.Engine.Resolve.Effect's
-- Effect.Recruit arm calls in without saying which effect it is.
--
-- Rule 701.70 has no second clause, so there is no "whenever a player recruits",
-- no GameEvent and no trigger condition to hang one on -- Pawl.Engine.Forage
-- writes GameEvent.Foraged only because CR 701.61b's counterpart exists.
-- Scryfall @o:"recruits"@, 2026-09-18, returns no card; a printing worded that
-- way is what would need one.
module Pawl.Engine.Recruit where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.Layout as Layout
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Zone as Zone

-- | CR 701.70a's token: a 1\/1 white Human Soldier creature. Minted here rather
-- than carried in card data, Pawl.Engine.Amass.armyToken's terms -- its
-- characteristics are printed in the comprehensive rules, not on Long Lake
-- Nuisance.
--
-- CR 111.4 supplies the name, rule 701.70a naming none: the subtypes plus the
-- word "Token", so the token is named "Human Soldier Token".
--
-- White is a colorIndicator (CR 202.2e), since CR 105.2 makes a token with no
-- mana cost colorless without one.
soldierToken :: Card.Card
soldierToken =
  Card.MkCard
    { Card.layout = Layout.Normal,
      Card.faces =
        NonEmpty.singleton
          Face.MkFace
            { Face.name = CardName.MkCardName (Text.pack "Human Soldier Token"),
              Face.manaCost = Nothing,
              Face.typeLine =
                TypeLine.MkTypeLine
                  Set.empty
                  (Set.singleton CardType.Creature)
                  (Set.fromList [Subtype.Human, Subtype.Soldier]),
              Face.power = Just (Power.MkPower (Quantity.Literal 1)),
              Face.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
              Face.loyalty = Nothing,
              Face.defense = Nothing,
              Face.vanguard = Nothing,
              Face.canBeYourCommander = False,
              Face.keywords = Map.empty,
              Face.colorIndicator = Set.singleton Color.White,
              Face.characteristicPT = Nothing,
              Face.staticAbilities = [],
              Face.spell = Face.defaultSpell,
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.rooms = Seq.empty,
              Face.dungeonEntryQuality = Nothing,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.enchant = [],
              Face.counterability = Counterability.Counterable,
              Face.additionalCosts = [],
              Face.modeCosts = Map.empty,
              Face.maximumX = [],
              Face.alternativeCosts = [],
              Face.costReductions = [],
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.blockPermissions = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.sacrificeRestrictions = [],
              Face.untapRestrictions = [],
              Face.attachRestrictions = [],
              Face.counterRestrictions = [],
              Face.crewRestrictions = [],
              Face.activationProhibitions = [],
              Face.entryRestrictions = [],
              Face.attackCosts = [],
              Face.blockCosts = [],
              Face.mulliganActions = [],
              Face.openingHandActions = [],
              Face.specialActions = []
            }
    }

-- | CR 701.70a: this player recruits. The whole keyword action, in the order
-- rule 701.70a fixes -- the draw first, so the card just drawn is among the
-- cards the discard may choose.
--
-- Pawl.Engine.Resolve.Effect.conniveOne's body one clause at a time, CR 701.50d
-- being the same two sentences with N of one and a counter where this has a
-- token:
--
-- 1. The draw goes through CR 121.2a's replacement funnel, so a replacement on
--    the draw gets its opportunity.
-- 2. CR 701.9b: the discarding player chooses which card, and only where they
--    hold more than one -- a one-card hand leaves nothing to ask, and CR 609.3
--    discards nothing out of an empty one.
-- 3. "Nonland" is asked of the card the discard funnel MINTED, through its CR 613
--    projection: the hand incarnation is gone by then, and a double-faced card in
--    a graveyard has only its front face's characteristics (CR 712.8a).
--
-- CR 609.3 rather than CR 608.2d: an empty library or an empty hand makes a
-- step do as much as possible rather than stopping the action, so
-- Pawl.Engine.Resolve.Effect.effectIsImpossible never refuses this arm.
recruit :: PlayerId -> Game ()
recruit pid = do
  -- CR 121.2a: the one-card draw is one replaceable instruction, Effect.Draw's
  -- arm line for line.
  outcome <- Event.applyReplacements (ProposedEvent.WouldDrawCards pid 1)
  Monad.forM_ (outcome >>= Replacement.asDrawCount) $ \(drawer, settled) ->
    Monad.replicateM_ (Natural.toIntSaturating settled) (Event.drawCard drawer)
  drawn <- State.get
  let held = Game.zoneMembers Zone.Hand pid drawn
  chosen <- case held of
    -- CR 609.3: an empty hand discards nothing, doing as much as possible. A
    -- one-card hand discards that card. Neither leaves a choice, so neither
    -- prompts.
    [] -> pure []
    [sole] -> pure [sole]
    first : second : more -> do
      -- CR 701.9b: the discarding player chooses which card. Filtered,
      -- completed and deduplicated, Pawl.Engine.Resolve.Effect.conniveOne's
      -- posture; this branch is reached only when the hand holds more than one,
      -- so every omitted card is one the player could have discarded.
      let offered = first : second : more
      answer <- Game.choose (Prompt.ChooseDiscard (Decide.deciderFor pid drawn) pid offered 1)
      let valid = ListUtils.nubOrd (filter (\c -> List.elem c offered) answer)
          filler = filter (\c -> List.notElem c valid) offered
      pure (take 1 (valid <> filler))
  moved <- fmap (concatMap Foldable.toList) (Monad.mapM (Event.discardReturning DiscardCause.Ordinary pid) chosen)
  after <- State.get
  let nonland c = not (Set.member CardType.Land (Filter.cardTypes (Projection.viewOfObject c after)))
  -- CR 701.70a's second sentence: one token, and only where a nonland card was
  -- discarded this way. A hand that held only lands, or no cards at all, creates
  -- nothing.
  Monad.when (any nonland moved) $
    Monad.void (Event.createTokens pid soldierToken Nothing 1 TapState.Untapped Map.empty Nothing)
