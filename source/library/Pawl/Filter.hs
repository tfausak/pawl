module Pawl.Filter where

import qualified Data.Set as Set
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Filter as Filter
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
    controller :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Show)

-- The perspective the match is relative to: who counts as "you" (CR 109.5). For a
-- target it is the targeting source's controller; for a continuous effect's set,
-- the effect's controller; Nothing when no player frames the match (an
-- off-battlefield search, whose filters never reference a player).
newtype Context = MkContext
  { perspective :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Show)

-- The one generic matcher. A pure fold over the Filter tree; it never inspects
-- the object's identity, only the View's characteristics.
matches :: Context -> View -> Filter.Filter -> Bool
matches context view predicate = case predicate of
  Filter.HasCardType t -> Set.member t (cardTypes view)
  Filter.HasSupertype s -> Set.member s (supertypes view)
  Filter.HasColor c -> Set.member c (colors view)
  Filter.HasSubtype s -> Set.member s (subtypes view)
  Filter.PowerAtLeast n -> case power view of
    Nothing -> False
    Just p -> p >= n
  Filter.ControlledBy relation -> case (controller view, perspective context) of
    (Just c, Just p) -> case relation of
      PlayerRelation.You -> c == p
      PlayerRelation.Opponent -> c /= p
    _ -> False
  Filter.And fs -> all (matches context view) fs
  Filter.Or fs -> any (matches context view) fs
  Filter.Not f -> not (matches context view f)
