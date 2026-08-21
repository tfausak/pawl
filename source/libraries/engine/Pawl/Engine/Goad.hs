-- CR 701.15: goad, the keyword action that makes a creature attack, and attack
-- somebody other than the player who goaded it, until that player's next turn.
--
-- ONE rule and no gate of its own. Rule 701.15b states its whole meaning as two
-- CR 508.1d REQUIREMENTS -- "attacks each combat if able and attacks a player
-- other than the controller of the permanent, spell, or ability that caused it
-- to be goaded if able" -- and CR 701.15c confirms the reading by counting them
-- ("doing so creates additional combat requirements"). So neither half is a CR
-- 508.1c restriction, and nothing here forbids a declaration: the whole of goad
-- is instantiated by Pawl.Engine.AttackRequirement and maximized by
-- Pawl.Engine.Combat.attackCeiling, which is what lets CR 508.1's "if able"
-- leave a requirement unmet when the board admits no announcement obeying it.
--
-- This is CR 613.11's axis rather than the layer system's, for
-- Pawl.Engine.Detain's reason: it modifies what the rules require, so
-- Pawl.Engine.Projection never sees it.
--
-- Casing on rule 701.15 is the closed half reading its own rulebook, the
-- standing Pawl.Engine.Keyword has over rule 702 and Pawl.Engine.Detain over
-- rule 701.35: what the reader below learns is WHO goaded a permanent, never
-- which card did it.
module Pawl.Engine.Goad where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)

-- CR 701.15a: goad this permanent until `until_`'s next turn. `until_` is the
-- controller of the goading spell or ability (CR 109.5's "you"), resolved once
-- here, since the sweep that ends this has no resolution left to read it off.
--
-- An ADDITION to whatever is already there, Detain.detain's posture: CR 701.15c
-- lets several players goad one creature, and each entry runs to its own
-- player's next turn. CR 701.15d's "the same player goading it again has no
-- effect" is Set.insert doing nothing.
--
-- A no-op for an id naming nothing, which is what an ObjectRef that matched a
-- permanent already gone delivers.
goad :: PlayerId -> ObjectId -> GameState.GameState -> GameState.GameState
goad until_ oid gs =
  gs
    { GameState.objects =
        Map.adjust
          (\o -> o {Object.goadedBy = Set.insert until_ (Object.goadedBy o)})
          oid
          (GameState.objects gs)
    }

-- Who has goaded this permanent? CR 701.15b needs the SEATS and not merely a
-- yes: the second requirement names "a player other than" each of them, so a
-- creature two players have goaded owes two different exclusions.
goadedBy :: ObjectId -> GameState.GameState -> Set PlayerId
goadedBy oid gs = maybe Set.empty Object.goadedBy (Map.lookup oid (GameState.objects gs))
