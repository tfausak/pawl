module Pawl.Filter where

import qualified Data.Set as Set
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Filter as Filter
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype

-- The characteristics a Filter atom consults. Supplied by the projection on the
-- battlefield/stack, and by the printed card off the battlefield (the two
-- builders live in Pawl.Projection). `power` and `controller` are Nothing off the
-- battlefield -- a card in a library has neither under the rules that matter here
-- -- so PowerAtLeast / ControlledBy are vacuously False there, which no search
-- filter uses.
data View = MkView
  { cardTypes :: Set.Set CardType.CardType,
    supertypes :: Set.Set Supertype.Supertype,
    colors :: Set.Set Color.Color,
    subtypes :: Set.Set Subtype.Subtype,
    power :: Maybe Integer,
    controller :: Maybe PlayerId.PlayerId,
    -- Which object this view is OF. Nothing for a printed card off the
    -- battlefield, which is not an object -- so IsSource is vacuously False
    -- there, the same posture power and controller already take.
    identity :: Maybe ObjectId.ObjectId
  }
  deriving (Eq, Show)

-- The perspective the match is relative to: who counts as "you" (CR 109.5), and
-- which object the surrounding effect comes from. Both are Nothing when no
-- player and no source frame the match (an off-battlefield search).
data Context = MkContext
  { perspective :: Maybe PlayerId.PlayerId,
    source :: Maybe ObjectId.ObjectId
  }
  deriving (Eq, Show)

-- The one generic matcher. A pure fold over the Filter tree; it never inspects
-- which effect produced the Filter. Identity checks like IsSource consult the
-- supplied Context, not information baked into the predicate.
matches :: Context -> View -> Filter.Filter -> Bool
matches context view predicate = case predicate of
  Filter.HasCardType t -> Set.member t (cardTypes view)
  Filter.HasSupertype s -> Set.member s (supertypes view)
  Filter.HasColor c -> Set.member c (colors view)
  Filter.HasSubtype s -> Set.member s (subtypes view)
  Filter.PowerAtLeast n -> case power view of
    Nothing -> False
    Just p -> p >= n
  -- Every other player is an Opponent by construction: CR 806.1 has a
  -- free-for-all's players compete as individuals against each other, and CR
  -- 102.2 says the same for two players -- one predicate, `c /= p`, serves
  -- both. CR 102.3's teams are the ONE reading it is wrong for, and pawl has
  -- none to express (#175). Unlike Pawl.Count.playersFor, which folds a player
  -- SET, this arm tests one candidate `View` at a time, so there is no set
  -- here to get the size of wrong. `controller view == Nothing` off the
  -- battlefield is already covered by View's own haddock (vacuously False,
  -- the same posture PowerAtLeast takes).
  --
  -- Pinned at three seats by ResolveSpec's "CR 806.1 at three seats a
  -- ControlledBy Opponent pool spans BOTH opponents' creatures".
  Filter.ControlledBy relation -> case (controller view, perspective context) of
    (Just c, Just p) -> case relation of
      PlayerRelation.You -> c == p
      PlayerRelation.Opponent -> c /= p
    _ -> False
  Filter.IsSource -> case (identity view, source context) of
    (Just oid, Just src) -> oid == src
    _ -> False
  Filter.And fs -> all (matches context view) fs
  Filter.Or fs -> any (matches context view) fs
  Filter.Not f -> not (matches context view f)
