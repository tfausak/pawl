-- CR 303.4 / 301.5 / 101.2 / 613.11: the continuous effects a DESTINATION puts on
-- what may become attached to it. One of the modules on the axis CR 613.11
-- reaches past the layer system (alongside Pawl.Engine.PlayerEffect,
-- Pawl.Engine.BlockRequirement, Pawl.Engine.AttackRequirement,
-- Pawl.Engine.CombatRestriction, Pawl.Engine.AttackCost,
-- Pawl.Engine.SacrificeRestriction and Pawl.Engine.UntapRestriction). None is a
-- layer, and no layer of Pawl.Engine.Projection rewrites them.
--
-- The only reader of Pawl.Types.AttachRestriction. Its callers ask about a PAIR
-- and never learn which card produced the answer.
--
-- THREE places ask, and all three are needed for the reason
-- Pawl.Engine.SacrificeRestriction's header gives about its own two: CR 101.2
-- makes the "can't" beat the "can", so a refused pairing must neither be
-- ATTACHED (Pawl.Engine.Attach.attachmentFor, which every CR 701.3 move and every
-- Filter.CanHostSubject offer goes through) nor be left STANDING once it somehow
-- is. Gating only the first would leave CR 608.3c's Aura spell -- which enters
-- attached to its target whatever the destination says -- permanently enchanting
-- a permanent that refuses it.
--
-- The STANDING half is two askers rather than one because the two outcomes the
-- rulebook states are two state-based actions: an Aura is buried
-- (Pawl.Engine.Sba.fallsOff, CR 704.5m with CR 303.4c and CR 702.16c) and an
-- Equipment merely detaches (Pawl.Engine.Sba.becomesUnattached, CR 704.5n with
-- CR 301.5c and CR 702.16d). Nothing here knows which is which: `removesGiven`
-- answers one Bool about a pair, and Sba's own classification picks the outcome.
--
-- TWO ENTRY POINTS rather than one, and rule 702.16n is the whole reason: an
-- Aura that says "this effect doesn't remove Auras" spares them from the two
-- state-based actions and says nothing about what may become attached, so
-- `refuses` and `removesGiven` differ in their mint and in nothing else.
--
-- TARGETING is not one of the three, and that is the rule rather than an
-- omission: CR 702.5a gives the enchant ability both jobs and this restriction is
-- neither, so an Aura spell may still target a permanent that refuses it (CR
-- 608.2b keeps it legal, CR 608.3c attaches it, CR 704.5m buries it on the next
-- state-based check). Protection is the one quality that also forbids the
-- targeting, in a clause of its own (CR 702.16b), which
-- Pawl.Engine.Target.targetable answers off Keyword.Protection -- and, for a
-- player, off Pawl.Engine.PlayerEffect.protectedFrom.
--
-- TWO SOURCES of rows, where every sibling above reads printed card data alone:
-- a face's own Face.attachRestrictions, and the rows rule 702 MINTS for a
-- permanent holding a keyword (Pawl.Engine.Keyword.mintedAttachRestrictionsOf,
-- and mintedRemovalRestrictionsOf for the standing half).
-- Protection is the pool's minter, rule 702.16c and rule 702.16d being the
-- rulebook's own instances of this type's shape.
--
-- Rule 702.16c's PLAYER half is not here and cannot be: this type's two
-- positions are ObjectIds, and a player has no keywords to mint from. It rides
-- Pawl.Engine.PlayerEffect.protectedFrom, which Attach.attachmentFor and
-- Sba.fallsOff ask beside the two calls above.
module Pawl.Engine.AttachRestriction where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Types.AttachRestriction as AttachRestriction
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.RuleAbilities as RuleAbilities

-- CR 303.4's last sentence and CR 301.5 with CR 101.2: does any effect in force
-- right now say `subject` can't become attached to `host`? Consecrate Land's
-- "can't be enchanted by other Auras" and Goblin Brawler's "can't be equipped".
--
-- A Bool about a PAIR, where Pawl.Engine.SacrificeRestriction answers a set of
-- ids: this question has two object positions, so there is no one set to hand
-- back. Every caller holds the pair already.
--
-- Pawl.Engine.SacrificeRestriction.cantBeSacrificed's body, gate for gate, plus
-- the second position: the same CR 305.7 and CR 613.1f ability losses are asked
-- of the RESTRICTING permanent, the same CR 612.1 word swap is applied -- to the
-- affected set AND to the attachers filter, since both are words on that one
-- card, which is Pawl.Engine.CombatRestriction.cantBeBlockedBy's posture toward
-- its own two halves -- and CR 613.11 is why the affected set is read against the
-- FULL projection rather than a layer-bounded one.
--
-- The filter's frame is the RESTRICTING permanent: its id is Filter.IsSource, so
-- Consecrate Land's "other Auras" is a Not over that atom, and its controller is
-- CR 109.5's "you". The subject is matched through its own projection, so an Aura
-- that has lost the subtype under CR 613's layer 4 is barred by nothing.
--
-- For a MINTED row that frame is the HOST, which is what makes rule 702.16c and
-- rule 702.16d expressible in this shape at all: the restricting permanent is
-- the destination itself, where every printed producer is a third permanent.
refuses :: ObjectId -> ObjectId -> GameState -> Bool
refuses = barredBy Keyword.mintedAttachRestrictionsOf Map.empty

-- CR 704.5m and CR 704.5n's question rather than CR 701.3a's: does anything in
-- force right now REMOVE `subject` from `host`, once it is attached anyway? The
-- same rows as `refuses` for every producer but one -- CR 702.16n's "this effect
-- doesn't remove [those Auras]" parts the two, and Pawl.Engine.Keyword's
-- mintedRemovalRestrictionsOf is where that narrowing lives. Two askers, both in
-- Pawl.Engine.Sba, for the reason this module's header gives.
--
-- Against a pre-computed projection, Pawl.Engine.Projection's own
-- hasKeywordGiven/hasKeyword pairing: Pawl.Engine.Sba asks this once per attached
-- permanent on every state-based pass and already holds the CR 704.3 pre-pass map,
-- so the host's keywords come out of that map rather than out of a fresh gather
-- per attached permanent per pass -- fallsOff's haddock names that cost for its
-- own enchant read. Pawl.Engine.Attach.attachmentFor holds no such map and passes
-- Map.empty to `refuses`, exactly as every other *Of/*Given pair in the tree does.
--
-- Threading one down to it was considered and declined; see #2396. No caller of
-- attachmentFor holds a map either, and attachmentFor projects the same host
-- again beside that call (Projection.isCreatureOf, Projection.cardTypesOf) where
-- Attach.hostsFor projects it a third time (Projection.viewOfObject), so a map
-- there would drop one projection of several. Measured 2026-09-01 by making
-- `refuses` throw: no Pawl.Benchmark scenario reaches it, the Aura pair included,
-- since CR 608.3c attaches an Aura spell without going through attachmentFor.
removesGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
removesGiven = barredBy Keyword.mintedRemovalRestrictionsOf

-- The body both questions share, and the one `refuses`'s haddock above describes
-- gate for gate. The MINT is the only difference: a PRINTED row is words on a
-- card that says nothing about the distinction, so the two questions read the
-- same face data and differ only in the rows rule 702 adds.
barredBy :: (Map Keyword.Type.Keyword Natural -> [AttachRestriction.AttachRestriction]) -> Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
barredBy mint pcs subject host gs =
  let setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- Forced at most once, and only by a board on which some permanent
      -- actually declares a prohibition.
      subjectView = Projection.viewOfObject subject gs
      hostView = Projection.project host gs
      grants = Projection.controlGrants gs
      fromPermanent source = case RuleAbilities.attachRestrictions (Projection.ruleAbilitiesOf source gs) of
        -- Every permanent in almost every game.
        [] -> False
        restrictions ->
          (null setEffs || Projection.liveAfterLayers setEffs source gs)
            && not (removed source)
            && any (fromRestriction source (Projection.textChangesAffecting source gs)) restrictions
      fromRestriction source changes restriction =
        let affected = AttachRestriction.affected restriction
            attachers = AttachRestriction.attachers restriction
            named =
              Projection.affectsUnder
                grants
                source
                host
                (if null changes then affected else Projection.rewriteAffected changes affected)
                hostView
                gs
            barred =
              Filter.matches
                -- CR 702.16k: the carrier is the row's source, which for a row
                -- rule 702.16 minted is the protected host itself -- the same
                -- frame this function's haddock states for the minted case.
                (Filter.contextFor (Game.teams gs) (Projection.controllerOf source gs) (Just source))
                  { Filter.carrierChosenPlayer = Game.lookupObject source gs >>= Object.chosenPlayer
                  }
                subjectView
                (if null changes then attachers else Filter.rewrite changes attachers)
         in named && barred
      -- CR 702: the rows rule 702 gives the HOST for holding a keyword, which
      -- until protection (CR 702.16c, CR 702.16d) nothing produced -- every row
      -- here was printed card data.
      -- The `mint` argument is that mint, beside the one that gives rule 702.16f
      -- its combat restriction: mintedAttachRestrictionsOf for CR 701.3a's move,
      -- mintedRemovalRestrictionsOf for the two state-based actions.
      --
      -- Only the HOST's keywords, where the printed rows walk the whole
      -- battlefield: rule 702.16c and rule 702.16d put the prohibition on the
      -- protected permanent itself, and no keyword in rule 702 says anything
      -- about what may be attached to somebody ELSE. A walk would find the same
      -- rows and then throw all but the host's away, since `named` is
      -- Filter.IsSource.
      --
      -- NO short-circuit of the kind Pawl.Engine.CombatRestriction.inForce takes
      -- before its own minted rows, and the difference is that one host is not a
      -- board: that gate exists to avoid projecting EVERY permanent, and asks a
      -- whole-board walk to decide it, which here would cost more than the one
      -- projection it saves. The `pcs` argument is the affordance instead.
      --
      -- Read off the PROJECTION rather than the printed face, so a granted or
      -- removed keyword is seen (CR 613.1f). That is also why these rows skip the
      -- three gates the printed ones take -- CR 305.7's subtype set, CR 613.1f's
      -- ability removal and CR 612.1's word swap: the projection has already
      -- applied the first two in layer order, and what a keyword MEANS is rule
      -- 702's text, which CR 612.2 does not reach. The empty change list below is
      -- that third point.
      fromKeywords =
        any
          (fromRestriction host [])
          (mint (Projection.keywordsGiven pcs host gs))
   in fromKeywords || any fromPermanent (Set.toList (GameState.battlefield gs))
