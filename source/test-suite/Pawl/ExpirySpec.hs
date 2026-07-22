-- Covers Pawl.Expiry and Pawl.Type.Expiry: the printed Duration -> stored Expiry
-- arming (CR 611.2), the sweeps that end a duration (CR 514.2, 611.2a, 611.2b),
-- and the two gate cards (Master Thief, Hag of Inner Weakness).
module Pawl.ExpirySpec where

import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Expiry as Expiry.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- A stored continuous effect with a chosen expiry, over a stand-in target.
-- Object id 998 is the stand-in source (Support.withEffectAt's posture);
-- nothing here reads the source's characteristics.
effectWith :: Expiry.Type.Expiry -> GameState.GameState -> GameState.GameState
effectWith expiry gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 998,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = expiry,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Flying,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton (ObjectId.MkObjectId 999))
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

armTests :: Tasty.TestTree
armTests =
  Tasty.testGroup
    "Arm"
    [ HU.testCase "CR 514.2 an until-end-of-turn duration arms to AtCleanup" $
        HU.assertEqual "armed" (Just Expiry.Type.AtCleanup) (Expiry.arm Duration.UntilEndOfTurn),
      HU.testCase "CR 611.2a an indefinite duration arms to Never" $
        HU.assertEqual "armed" (Just Expiry.Type.Never) (Expiry.arm Duration.Indefinite)
    ]

cleanupTests :: Cards.Cards -> Tasty.TestTree
cleanupTests cards =
  Tasty.testGroup
    "DropAtCleanup"
    [ HU.testCase "CR 514.2 cleanup drops an AtCleanup continuous effect and keeps a Never one" $
        let gs0 = Setup.emptyGame S.bothPlayers
            gs1 = effectWith Expiry.Type.Never (effectWith Expiry.Type.AtCleanup gs0)
            after = Expiry.dropAtCleanup gs1
         in do
              HU.assertEqual "two stored before" 2 (length (GameState.continuousEffects gs1))
              HU.assertEqual "one survives" [Expiry.Type.Never] (map ContinuousEffect.expiry (GameState.continuousEffects after)),
      HU.testCase "CR 514.2 the same sweep drops an AtCleanup floating replacement" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (oid, gs1) = S.addPiker cards S.alice gs0
            shielded = S.addRegenShield oid gs1
            after = Expiry.dropAtCleanup shielded
         in do
              HU.assertEqual "one shield before" 1 (length (GameState.replacements shielded))
              HU.assertEqual "none after" [] (map ActiveReplacement.expiry (GameState.replacements after))
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.ExpirySpec" [armTests, cleanupTests cards]
