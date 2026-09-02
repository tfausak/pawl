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
-- starts from this and moves one axis, so a literal never has to restate every
-- other field to say something about one.
empty :: Combat.Combat
empty =
  Combat.MkCombat
    { Combat.attackers = Map.empty,
      Combat.blockers = Map.empty,
      Combat.struckFirst = Nothing,
      Combat.joinedUnder = Map.empty,
      Combat.attackedUnder = Map.empty,
      Combat.attacked = Set.empty,
      Combat.declaredAttacked = Set.empty,
      Combat.declaredAttackedThisStep = Set.empty,
      Combat.declaredAttackers = Set.empty,
      Combat.declaredBlockers = Set.empty,
      Combat.blockersDeclared = False,
      Combat.attackingNothing = Set.empty,
      Combat.defenders = []
    }

-- | 'empty' on the wire, spelled once. Every field but `struckFirst` is
-- 'Fields.defaulted' and so omitted at its default, which is what leaves a
-- cleared combat down to the one key: this literal is where that omission is
-- pinned, and a field switched back to 'Fields.required' reappears in it.
emptyJson :: String
emptyJson = "{\"struckFirst\":null}"

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
          -- CR 802.2a, keyed by the ATTACKER where joinedUnder above is keyed by
          -- the combatant, so distinct ids on both sides of the entry.
          Combat.attackedUnder = Map.singleton (ObjectId.MkObjectId 14) (PlayerId.MkPlayerId 15),
          Combat.attacked = Set.singleton (AttackTarget.OfPlayer (PlayerId.MkPlayerId 2)),
          Combat.declaredAttacked = Set.singleton (AttackTarget.OfPlaneswalker (ObjectId.MkObjectId 8)),
          Combat.declaredAttackedThisStep = Set.singleton (AttackTarget.OfBattle (ObjectId.MkObjectId 9)),
          -- CR 508.1a / 509.1a: keyed by the CREATURE, where the three sets
          -- above are keyed by what was attacked, and distinct ids again so
          -- neither half can be read off the other.
          Combat.declaredAttackers = Set.singleton (ObjectId.MkObjectId 11),
          Combat.declaredBlockers = Set.singleton (ObjectId.MkObjectId 12),
          Combat.blockersDeclared = True,
          -- CR 506.4c, keyed by the ATTACKER, so a distinct id again -- this set
          -- and declaredAttackers above are the two keyed by the creature.
          Combat.attackingNothing = Set.singleton (ObjectId.MkObjectId 13),
          Combat.defenders = [PlayerId.MkPlayerId 10]
        }
      ( " {\"attackers\":{\"1\":{\"type\":\"OfPlayer\",\"value\":2}}"
          <> ",\"blockers\":{\"3\":[4]}"
          <> ",\"struckFirst\":[5]"
          <> ",\"joinedUnder\":{\"6\":7}"
          <> ",\"attackedUnder\":{\"14\":15}"
          <> ",\"attacked\":[{\"type\":\"OfPlayer\",\"value\":2}]"
          <> ",\"declaredAttacked\":[{\"type\":\"OfPlaneswalker\",\"value\":8}]"
          <> ",\"declaredAttackedThisStep\":[{\"type\":\"OfBattle\",\"value\":9}]"
          <> ",\"declaredAttackers\":[11],\"declaredBlockers\":[12]"
          <> ",\"blockersDeclared\":true,\"attackingNothing\":[13]"
          <> ",\"defenders\":[10]} "
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
      " {\"struckFirst\":[]} "
  -- CR 802.4 / CR 802.5: defenders is ORDERED, not a set -- both rules read it
  -- as APNAP order (CR 101.4). Descending ids, so a codec that sorted or
  -- reversed the list round-trips to a different game rather than to the same
  -- one; every other case here has at most one defender and could not tell.
  Spec.it s "several defending players keep their order" $
    Common.assertCodec
      s
      Combat.codec
      empty {Combat.defenders = [PlayerId.MkPlayerId 14, PlayerId.MkPlayerId 13]}
      " {\"struckFirst\":null,\"defenders\":[14,13]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Combat.codec
