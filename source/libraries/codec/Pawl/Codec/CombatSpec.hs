module Pawl.Codec.CombatSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Codec.Combat as Combat
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | An empty combat, which is what CR 511.3 leaves behind. Every case below
-- starts from this and moves one axis, so a literal never has to restate eight
-- fields to say something about the ninth.
empty :: Combat.Combat
empty =
  Combat.MkCombat
    { Combat.attackers = Map.empty,
      Combat.blockers = Map.empty,
      Combat.struckFirst = Nothing,
      Combat.joinedUnder = Map.empty,
      Combat.attacked = Set.empty,
      Combat.declaredAttacked = Set.empty,
      Combat.declaredAttackedThisStep = Set.empty,
      Combat.blockersDeclared = False,
      Combat.defender = Nothing
    }

-- | 'empty' on the wire, spelled once.
emptyJson :: String
emptyJson =
  "{\"attackers\":{},\"blockers\":{},\"struckFirst\":null,\"joinedUnder\":{}"
    <> ",\"attacked\":[],\"declaredAttacked\":[],\"declaredAttackedThisStep\":[]"
    <> ",\"blockersDeclared\":false,\"defender\":null}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Combat" $ do
  -- CR 511.3's cleared record.
  Spec.it s "a cleared combat" $
    Common.assertCodec s Combat.codec empty (" " <> emptyJson <> " ")
  -- Every axis at once, and every id distinct: the three maps are keyed by an
  -- ObjectId, so a decimal-keyed JSON object is the wire form, and the three
  -- AttackTarget sets carry different announcements (CR 508.1b) so that no two
  -- of them can be swapped silently.
  Spec.it s "a combat in progress" $
    Common.assertCodec
      s
      Combat.codec
      Combat.MkCombat
        { Combat.attackers = Map.singleton (ObjectId.MkObjectId 1) (AttackTarget.OfPlayer (PlayerId.MkPlayerId 2)),
          Combat.blockers = Map.singleton (ObjectId.MkObjectId 3) (Set.singleton (ObjectId.MkObjectId 4)),
          Combat.struckFirst = Just (Set.singleton (ObjectId.MkObjectId 5)),
          Combat.joinedUnder = Map.singleton (ObjectId.MkObjectId 6) (PlayerId.MkPlayerId 7),
          Combat.attacked = Set.singleton (AttackTarget.OfPlayer (PlayerId.MkPlayerId 2)),
          Combat.declaredAttacked = Set.singleton (AttackTarget.OfPlaneswalker (ObjectId.MkObjectId 8)),
          Combat.declaredAttackedThisStep = Set.singleton (AttackTarget.OfBattle (ObjectId.MkObjectId 9)),
          Combat.blockersDeclared = True,
          Combat.defender = Just (PlayerId.MkPlayerId 10)
        }
      ( " {\"attackers\":{\"1\":{\"type\":\"OfPlayer\",\"value\":2}}"
          <> ",\"blockers\":{\"3\":[4]}"
          <> ",\"struckFirst\":[5]"
          <> ",\"joinedUnder\":{\"6\":7}"
          <> ",\"attacked\":[{\"type\":\"OfPlayer\",\"value\":2}]"
          <> ",\"declaredAttacked\":[{\"type\":\"OfPlaneswalker\",\"value\":8}]"
          <> ",\"declaredAttackedThisStep\":[{\"type\":\"OfBattle\",\"value\":9}]"
          <> ",\"blockersDeclared\":true,\"defender\":10} "
      )
  -- CR 510.4's two ABSENT-looking states, which are not the same state. Nothing
  -- means the first combat damage step has not happened; the empty set means it
  -- has and nobody had first or double strike, so the second damage step is
  -- already spent. The two cases differ in exactly this field, and a codec that
  -- folded them together would make one of the two literals wrong.
  Spec.it s "struckFirst is Nothing before the first combat damage step" $
    Common.assertCodec
      s
      Combat.codec
      empty {Combat.struckFirst = Nothing}
      (" " <> emptyJson <> " ")
  Spec.it s "struckFirst is an empty set once that step has run with nobody in it" $
    Common.assertCodec
      s
      Combat.codec
      empty {Combat.struckFirst = Just Set.empty}
      " {\"attackers\":{},\"blockers\":{},\"struckFirst\":[],\"joinedUnder\":{},\"attacked\":[],\"declaredAttacked\":[],\"declaredAttackedThisStep\":[],\"blockersDeclared\":false,\"defender\":null} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Combat.codec
