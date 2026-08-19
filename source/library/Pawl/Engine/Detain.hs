-- CR 701.35: detain, the keyword action that forbids a permanent three things at
-- once -- attacking, blocking, and having its activated abilities activated --
-- until the next turn of the detaining spell or ability's controller.
--
-- ONE rule and three gates, because pawl has no single "may this permanent act"
-- funnel: Pawl.Engine.CombatRestriction answers the first two (CR 508.1c / CR
-- 509.1b) and the third is split across the two independent activation gates
-- (Pawl.Engine.Activate.activatableGiven for CR 602.2, Pawl.Engine.Cost's
-- manaActivations for CR 605.3a's mana windows, which the first refuses
-- outright). Rule 701.35a's third clause says "its activated abilities", with no
-- carve-out for mana abilities, so both are gated -- unlike CR 702.61b's split
-- second, which exempts them.
--
-- This is CR 613.11's axis rather than the layer system's: it modifies what the
-- rules permit, so Pawl.Engine.Projection never sees it.
--
-- Casing on rule 701.35 is the closed half reading its own rulebook, the standing
-- Pawl.Engine.Keyword has over rule 702: what the readers below learn is that a
-- permanent is detained, never which card detained it.
module Pawl.Engine.Detain where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)

-- CR 701.35a: detain this permanent until `until_`'s next turn. `until_` is the
-- controller of the detaining spell or ability (CR 109.5's "you"), resolved once
-- here, since the sweep that ends this has no resolution left to read it off.
--
-- An ADDITION to whatever is already there rather than an assignment: a permanent
-- two players have detained stays detained until both of their next turns have
-- come, and a second detain by the same player is the same entry again.
--
-- A no-op for an id naming nothing, which is what an ObjectRef that matched a
-- permanent already gone delivers.
detain :: PlayerId -> ObjectId -> GameState.GameState -> GameState.GameState
detain until_ oid gs =
  gs
    { GameState.objects =
        Map.adjust
          (\o -> o {Object.detainedUntil = Set.insert until_ (Object.detainedUntil o)})
          oid
          (GameState.objects gs)
    }

-- Is this permanent detained right now? The one question the three gates ask, and
-- all any of them learns.
detained :: ObjectId -> GameState.GameState -> Bool
detained oid gs = maybe False (not . Set.null . Object.detainedUntil) (Map.lookup oid (GameState.objects gs))
