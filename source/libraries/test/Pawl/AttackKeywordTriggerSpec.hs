{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over keyword triggers on attacking and on other creatures
-- (CR 702): frenzy, exalted, mentor, training, decayed, provoke, evolve, and
-- the cards between. Split out of Pawl.KeywordTriggerSpec, which keeps the
-- machinery.
module Pawl.AttackKeywordTriggerSpec where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import Pawl.KeywordTriggerSpec (aimedCast)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- CR 702.68a's frenzy, which rule 702 states as a triggered ability: "'Frenzy N'
-- means 'Whenever this creature attacks and isn't blocked, it gets +N/+0 until
-- end of turn.'" selfAttacksUnblockedSpec above proves CR 509.1h's event; this
-- group proves the keyword Pawl.Engine.Keyword.frenzy mints on top of it, and
-- with it rule 702's first +N/+0 -- bushido's +N/+N with the toughness term at
-- zero, which is why every reading below is a PAIR.
--
-- Frenzy Sliver {1}{B} Creature -- Sliver 1/1 is the card, and it prints frenzy
-- as a GRANT -- "all Sliver creatures have frenzy 1" -- so the ability is minted
-- off the projection's POST-LAYER keyword count rather than off a printed
-- keyword. Venser's Sliver {5} Artifact Creature -- Sliver 3/3, a vanilla, is
-- the Sliver it lands on, and that split is what keeps the numbers apart: the
-- bonus is 1 and the attacker's power is 3, so a payload reading its source's
-- own power would say 6 where the rule says 4, and one reading the granting
-- permanent's would say 4/2.
--
-- THREE SEATS. Rule 702.68a names no defending player, so what the third seat
-- buys here is narrower than afflictSpec's: at two players "the player attacked"
-- and "the attacker's opponent" collapse, and a bonus wrongly scoped to the seat
-- count rather than to the attack would read the same either way.
--
-- Giant Spider 2/4 is the blocker, so the BLOCKED leg carries an observable of
-- its own rather than only an absence: 3 damage leaves it alive where the 4 a
-- wrongly fired frenzy would deal kills it.
--
-- Readings of power and toughness are taken at the COMBAT DAMAGE step, after the
-- trigger has resolved in the declare blockers step and before damage is dealt.
frenzySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
frenzySpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.threePlayerCombat ours yours [])
      -- CR 508.1's declaration narrowed to the named creatures, against `who`.
      -- S.aggressiveAnswer attacks with everything and would take whichever
      -- defender sorts first, so a case that is about WHICH creature and WHICH
      -- seat has to say both.
      plan :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan who attackers p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.ChooseAttackTarget {} -> S.attackTo who p
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      -- The same, with CR 509.1's declaration switched off. The blocked and
      -- unblocked legs below differ in this and nothing else.
      declining :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      declining who attackers p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> plan who attackers p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
   in Spec.describe s "Frenzy" $ do
        -- The proving test and its control, on ONE board differing only in the
        -- answer to Prompt.DeclareBlockers. The Frenzy Sliver stays home in both,
        -- so what is told apart is the bonus and not the presence of the grant.
        Spec.it s "CR 702.68a whole card: an unblocked Venser's Sliver is 4/3, a blocked one is 3/3" $ do
          (gs, mine, theirs, _) <- board ["Frenzy Sliver", "Venser's Sliver"] ["Giant Spider"]
          case (mine, theirs) of
            ([sliver, venser], [spider]) -> do
              Spec.assertEqWith
                s
                "unblocked: +1/+0 on the attacker, and none on the Frenzy Sliver at home"
                (S.powerToughnessOf venser (atDamage (declining S.bob [venser]) gs), S.powerToughnessOf sliver (atDamage (declining S.bob [venser]) gs))
                (Just (4, 3), Just (1, 1))
              Spec.assertEqWith
                s
                "blocked: the attacker is its printed 3/3"
                (S.powerToughnessOf venser (atDamage (plan S.bob [venser]) gs))
                (Just (3, 3))
              Spec.assertEqWith
                s
                "unblocked: bob took 4"
                (S.lifeOf S.bob (S.runCombat (declining S.bob [venser]) gs))
                (Just 16)
              let blocked = S.runCombat (plan S.bob [venser]) gs
              Spec.assertEqWith
                s
                "blocked: bob took nothing and the 2/4 Spider survived 3"
                (S.lifeOf S.bob blocked, S.onBattlefield spider blocked)
                (Just 20, True)
            _ -> Spec.assertFailure s "fixture should give alice two Slivers and bob a Spider"
        -- CR 509.1h's last sentence: "a creature remains blocked even if all the
        -- creatures blocking it are removed from combat." The 3/3 Sliver kills the
        -- 2/1 Piker at CR 510.2 and still never gets the bonus.
        Spec.it s "CR 509.1h losing every blocker does not earn the bonus" $ do
          (gs, mine, theirs, _) <- board ["Frenzy Sliver", "Venser's Sliver"] ["Goblin Piker"]
          case (mine, theirs) of
            ([_, venser], [piker]) -> do
              let after = S.runCombat (plan S.bob [venser]) gs
              Spec.assertBool s (not (S.onBattlefield piker after)) "the 2/1 Piker died to the Sliver's 3"
              Spec.assertEqWith
                s
                "the Sliver is still its printed 3/3 and bob took nothing"
                (S.powerToughnessOf venser after, S.lifeOf S.bob after)
                (Just (3, 3), Just 20)
            _ -> Spec.assertFailure s "fixture should give alice two Slivers and bob a Piker"
        -- Rule 509.1h carries no condition about anyone being ABLE to block, so
        -- the board where nobody can is the bonus' board too.
        Spec.it s "CR 509.1h an attacker nobody could block gets the bonus" $ do
          (gs, mine, _, _) <- board ["Frenzy Sliver", "Venser's Sliver"] []
          case mine of
            [_, venser] -> do
              Spec.assertEqWith s "4/3 at the damage step" (S.powerToughnessOf venser (atDamage (plan S.bob [venser]) gs)) (Just (4, 3))
              Spec.assertEqWith s "and bob took 4" (S.lifeOf S.bob (S.runCombat (plan S.bob [venser]) gs)) (Just 16)
            _ -> Spec.assertFailure s "fixture should give alice two Slivers"
        -- "ALL SLIVER CREATURES", read as the card's own filter: the bearer is in
        -- it and a Hill Giant is not. Both attack unblocked on one board, so the
        -- two halves cannot hide behind each other -- a grant that missed the
        -- bearer and a grant that reached everything both leave bob at 14, and
        -- only the pair of sizes tells them apart.
        Spec.it s "CR 702.68a the grant reaches the bearer and stops at the Slivers" $ do
          (gs, mine, _, _) <- board ["Frenzy Sliver", "Hill Giant"] []
          case mine of
            [sliver, giant] -> do
              Spec.assertEqWith
                s
                "the 1/1 Sliver is 2/1 and the 3/3 Giant is untouched"
                (S.powerToughnessOf sliver (atDamage (plan S.bob [sliver, giant]) gs), S.powerToughnessOf giant (atDamage (plan S.bob [sliver, giant]) gs))
                (Just (2, 1), Just (3, 3))
              Spec.assertEqWith s "bob took 2 and 3" (S.lifeOf S.bob (S.runCombat (plan S.bob [sliver, giant]) gs)) (Just 15)
            _ -> Spec.assertFailure s "fixture should give alice a Frenzy Sliver and a Hill Giant"
        -- The third seat, which CR 508.1b's announcement is what changes: bob
        -- keeps his Spider and cannot block for carol, so the bonus follows the
        -- attack rather than the seat. Rule 702.68a names no defending player,
        -- and that is the point of asserting both life totals.
        Spec.it s "CR 509.1h the bonus follows the attack rather than the seat count" $ do
          (gs, mine, _, _) <- board ["Frenzy Sliver", "Venser's Sliver"] ["Giant Spider"]
          case mine of
            [_, venser] -> do
              let after = S.runCombat (plan S.carol [venser]) gs
              Spec.assertEqWith s "4/3 all the same" (S.powerToughnessOf venser (atDamage (plan S.carol [venser]) gs)) (Just (4, 3))
              Spec.assertEqWith s "carol took the 4 and bob took nothing" (S.lifeOf S.bob after, S.lifeOf S.carol after) (Just 20, Just 16)
            _ -> Spec.assertFailure s "fixture should give alice two Slivers"
        -- CR 702.68b: "if a creature has multiple instances of frenzy, each
        -- triggers separately" -- poisonous' multiplicity, asserted at the MINT
        -- because no board in this pool grants a second instance.
        Spec.it s "CR 702.68b each instance is its own ability" $ do
          Spec.assertEqWith s "frenzy 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Frenzy 1) 2)) [Keyword.frenzy 1, Keyword.frenzy 1]
          Spec.assertEqWith s "and frenzy 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Frenzy 3) 1)) [Keyword.frenzy 3]

-- CR 702.83a's exalted, which rule 702 states as a triggered
-- ability, and with it CR 506.5 -- "attacks alone", the one attack-trigger form
-- that is about the DECLARATION's size rather than about one creature.
--
-- Aven Squire {1}{W} Creature -- Bird Soldier 1/1 is the card: flying and
-- exalted, and flying decides nothing here, since every reading is taken before
-- damage whoever blocked. Hill Giant 3/3 is the creature it pumps, chosen so no
-- reading lands on the same pair -- 3/3 -> 4/4 is not 1/1 -> 2/2, and neither is
-- +2/+2's 5/5.
--
-- Every reading is taken at the COMBAT DAMAGE step, before damage is dealt, so
-- the pump is read directly rather than through what survives combat.
exaltedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exaltedSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to one named creature. The whole point of
      -- the group: S.aggressiveAnswer attacks with EVERYTHING, which is the
      -- not-alone board rather than the alone one.
      only :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      only oid p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== oid) ids
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
   in Spec.describe s "Exalted" $ do
        -- The proving test, and the one that pins WHICH object the payload moves.
        -- alice's Aven Squire stays home while her Hill Giant attacks alone: the
        -- GIANT is 4/4 and the Squire is untouched. A payload written as prowess'
        -- Filter.IsSource would move the Squire's 1/1 and leave the Giant at 3/3,
        -- and a self-scoped condition would not fire at all -- one assertion over
        -- both, so neither can hide behind the other.
        Spec.it s "CR 702.83a whole card: the Squire stays home and the lone attacker is 4/4" $ do
          (gs, mine, _) <- board ["Aven Squire", "Hill Giant"] []
          case mine of
            [squire, giant] ->
              Spec.assertEqWith
                s
                "the attacking Giant took the +1/+1 and the Squire took none"
                (S.powerToughnessOf giant (atDamage (only giant) gs), S.powerToughnessOf squire (atDamage (only giant) gs))
                (Just (4, 4), Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Squire and a Giant"
        -- CR 506.5's "the ONLY creature declared as an attacker", which is the
        -- count on GameEvent.AttackerDeclared. The SAME board as above, differing
        -- only in the declaration: with the Squire attacking too, nobody is alone
        -- and neither creature is pumped.
        Spec.it s "CR 506.5 two attackers is nobody attacking alone" $ do
          (gs, mine, _) <- board ["Aven Squire", "Hill Giant"] []
          case mine of
            [squire, giant] ->
              Spec.assertEqWith
                s
                "both are at their printed sizes"
                (S.powerToughnessOf giant (atDamage S.aggressiveAnswer gs), S.powerToughnessOf squire (atDamage S.aggressiveAnswer gs))
                (Just (3, 3), Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Squire and a Giant"
        -- "A creature you control" reaches the bearer as readily as anything else:
        -- rule 702.83a excludes nothing, so an Aven Squire attacking by itself
        -- pumps itself to 2/2.
        Spec.it s "CR 702.83a the bearer attacking alone pumps itself" $ do
          (gs, mine, _) <- board ["Aven Squire"] []
          case mine of
            [squire] -> Spec.assertEqWith s "1/1 became 2/2" (S.powerToughnessOf squire (atDamage S.aggressiveAnswer gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Squire"
        -- "YOU control", read against CR 109.5's "you" -- the ability's controller
        -- (CR 603.3a). bob's Aven Squire watches alice's Giant attack alone and
        -- stays silent. Same declaration as the proving test, same Giant, and the
        -- only difference is which seat holds the Squire.
        Spec.it s "CR 702.83a an opponent's Squire does not pump the attacker" $ do
          (gs, mine, _) <- board ["Hill Giant"] ["Aven Squire"]
          case mine of
            [giant] -> Spec.assertEqWith s "the Giant is its printed 3/3" (S.powerToughnessOf giant (atDamage (only giant) gs)) (Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Giant"
        -- Two exalted permanents are two abilities and two +1/+1s, which is CR
        -- 603.2 rather than a clause of rule 702.83: unlike CR 702.28c's shadow,
        -- rule 702.83 prints no "multiple instances are redundant" sentence.
        Spec.it s "CR 603.2 two Squires make the lone attacker 5/5" $ do
          (gs, mine, _) <- board ["Aven Squire", "Aven Squire", "Hill Giant"] []
          case mine of
            [_, _, giant] -> Spec.assertEqWith s "3/3 took both pumps" (S.powerToughnessOf giant (atDamage (only giant) gs)) (Just (5, 5))
            _ -> Spec.assertFailure s "fixture should give alice two Squires and a Giant"
        -- CR 508.1a's declaration is a SET, so a broken interpreter naming one
        -- creature twice has still declared one attacker. Combat.declareAttackers
        -- deduplicates before it counts; without that the count would be 2, so
        -- the Giant would not be attacking alone and would go unpumped.
        Spec.it s "CR 508.1a a repeated id is still one attacker" $ do
          (gs, mine, _) <- board ["Aven Squire", "Hill Giant"] []
          case mine of
            [_, giant] -> do
              let twice :: Prompt.Prompt r -> r
                  twice p = case p of
                    Prompt.DeclareAttackers _ _ ids -> concatMap (\i -> if i == giant then [i, i] else []) ids
                    _ -> S.aggressiveAnswer p
              Spec.assertEqWith s "the Giant is 4/4 all the same" (S.powerToughnessOf giant (atDamage twice gs)) (Just (4, 4))
            _ -> Spec.assertFailure s "fixture should give alice a Squire and a Giant"
        -- The same multiplicity one permanent over, asserted of the MINT because
        -- no printing in the pool carries exalted twice -- as flanking's, bushido's
        -- and prowess' instance cases are.
        Spec.it s "CR 603.2 two instances mint two abilities, both CR 506.5" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Exalted 2
              expected =
                TriggerCondition.CreatureAttacksAlone
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith s "each watching CR 506.5, filtered on the attacker's controller" (fmap TriggeredAbility.condition abilities) [expected, expected]

-- CR 702.134a's mentor, which rule 702 states as a triggered ability
-- and the FIRST whose ability TARGETS -- so this is the group that runs a
-- keyword-minted TargetSlot through CR 601.2c's choosing, and with it
-- Filter.PowerLessThanSource, the one atom whose bound is the source's own power
-- rather than a literal.
--
-- Blade Instructor {2}{W} Creature -- Human Soldier 3/1 is the card: mentor and
-- nothing else, so every number below is the keyword's. Its fellow attackers are
-- the pool's vanillas, picked so no two readings land on the same pair -- a
-- mentored Goblin Piker is 3/2, which is neither its printed 2/1 nor the
-- Instructor's 3/1, and a mentored Icehide Golem is 3/3, which is neither.
--
-- Every reading is taken at the COMBAT DAMAGE step, after the trigger has
-- resolved in the declare attackers step and before damage is dealt, so the
-- counter is read directly rather than through what survives combat.
mentorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mentorSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures, and CR 603.3d's
      -- target named outright. S.aggressiveAnswer attacks with everything and
      -- Replay.defaultAnswer would take whichever target sorts first, so a case
      -- that is about WHICH creature has to say both itself.
      plan :: [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      plan attackers target p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature target))) sets
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      -- CR 122.1: what is actually on the permanent, which a +1/+1 EFFECT would
      -- leave empty while reading the same 3/2.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "Mentor" $ do
        -- The proving test. Both attack, the Instructor mentors the smaller
        -- attacker, and the assertion covers all three things at once: the
        -- counter lands on the TARGET, the bearer takes none, and what landed is
        -- a CR 122.1a counter rather than a pump.
        Spec.it s "CR 702.134a whole card: the mentored attacker takes a +1/+1 counter" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker"] []
          case mine of
            [instructor, piker] -> do
              let after = atDamage (plan [instructor, piker] piker) gs
              Spec.assertEqWith
                s
                "the Piker is 3/2 and the Instructor is untouched"
                (S.powerToughnessOf piker after, S.powerToughnessOf instructor after)
                (Just (3, 2), Just (3, 1))
              Spec.assertEqWith s "and it is a counter" (countersOn piker after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice an Instructor and a Piker"
        -- "POWER LESS THAN this creature's power" is strict, so a 3/3 attacking
        -- beside a 3-power Instructor is no legal target -- and neither is the
        -- Instructor itself, which is why nothing at all is mentored here. Same
        -- declaration as the proving test; only the fellow attacker's power
        -- differs. S.aggressiveAnswer rather than `plan`, so that a filter that
        -- admitted the Giant would take the default target and go red.
        Spec.it s "CR 702.134a a creature whose power is not less is no legal target" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Hill Giant"] []
          case mine of
            [instructor, giant] -> do
              let after = atDamage S.aggressiveAnswer gs
              Spec.assertEqWith
                s
                "both are at their printed sizes"
                (S.powerToughnessOf giant after, S.powerToughnessOf instructor after)
                (Just (3, 3), Just (3, 1))
            _ -> Spec.assertFailure s "fixture should give alice an Instructor and a Giant"
        -- CR 508.1k's "attacking": the same Piker, small enough and on the same
        -- side, is no target while it stays home. The answerer aims at it anyway,
        -- so an ability that dropped the IsAttacking conjunct would mentor it.
        Spec.it s "CR 508.1k a creature that stayed home is no legal target" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker"] []
          case mine of
            [instructor, piker] ->
              Spec.assertEqWith
                s
                "the Piker is its printed 2/1"
                (S.powerToughnessOf piker (atDamage (plan [instructor] piker) gs))
                (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice an Instructor and a Piker"
        -- CR 603.3d, which sends a trigger through CR 601.2c-d: with TWO smaller
        -- attackers the rules leave which one open,
        -- so the controller is asked and the answer is honoured. More candidates
        -- than the slot needs, so the prompt cannot be short-circuited away.
        Spec.it s "CR 603.3d the controller picks which smaller attacker is mentored" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker", "Icehide Golem"] []
          case mine of
            [instructor, piker, golem] -> do
              let after = atDamage (plan [instructor, piker, golem] golem) gs
              Spec.assertEqWith
                s
                "the Golem took the counter and the Piker did not"
                (S.powerToughnessOf golem after, S.powerToughnessOf piker after)
                (Just (3, 3), Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice an Instructor, a Piker and a Golem"
        -- The bound is the SOURCE's power and not a number written into the
        -- ability: Hammer Dropper {2}{R}{W} Creature -- Giant Soldier 5/2 is the
        -- pool's other mentor, and the Hill Giant its 3-power sibling could not
        -- touch two cases up is a legal target for it -- same board, same
        -- declaration, only the mentor's power differs.
        Spec.it s "CR 702.134a a 5-power mentor reaches the 3/3 a 3-power one cannot" $ do
          (gs, mine, _) <- board ["Hammer Dropper", "Hill Giant"] []
          case mine of
            [dropper, giant] ->
              Spec.assertEqWith
                s
                "3 < 5, so the Giant is 4/4"
                (S.powerToughnessOf giant (atDamage (plan [dropper, giant] giant) gs))
                (Just (4, 4))
            _ -> Spec.assertFailure s "fixture should give alice a Dropper and a Giant"
        -- CR 608.2b re-checks the slot as the ability resolves, and rule 702.134a's
        -- comparison is part of what it re-checks. Two Instructors both aim at the
        -- 2/1 Piker; the first counter makes it 3/2, and 3 is no longer less than
        -- 3, so the second ability has no legal target and does not resolve. An
        -- engine that only checked at CR 601.2c would leave a 4/3.
        --
        -- That the second ability EXISTS is asserted at the mint below, not here:
        -- this board cannot tell a fizzled second trigger from a missing one.
        Spec.it s "CR 608.2b the second mentor's target is no longer legal" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Blade Instructor", "Goblin Piker"] []
          case mine of
            [first, second, piker] -> do
              let after = atDamage (plan [first, second, piker] piker) gs
              Spec.assertEqWith s "one counter landed" (S.powerToughnessOf piker after) (Just (3, 2))
              Spec.assertEqWith s "one, not two" (countersOn piker after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice two Instructors and a Piker"
        -- The same multiplicity asserted of the MINT, as exalted's and flanking's
        -- instance cases are, and with it the slot the gameplay cases above can
        -- only see through its effects: CR 508.3a's condition, and a target slot whose
        -- filter is the rule's two printed narrowings.
        Spec.it s "CR 702.134b two instances mint two targeting abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Mentor 2
              expectedSlot =
                TargetSlot.required
                  Pool.Creatures
                  (Just (Filter.Type.And [Filter.Type.IsAttacking, Filter.Type.PowerLessThanSource]))
              slotsOf ability = concatMap (Map.elems . Mode.targetSlots) (Modal.modes (TriggeredAbility.modal ability))
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each watching CR 508.3a's declaration"
            (fmap TriggeredAbility.condition abilities)
            [TriggerCondition.SelfAttacks TriggerFrequency.EveryTime, TriggerCondition.SelfAttacks TriggerFrequency.EveryTime]
          Spec.assertEqWith s "each with rule 702.134a's one slot" (concatMap slotsOf abilities) [expectedSlot, expectedSlot]

-- CR 702.134c, the OTHER half of rule 702.134: not mentor's own attack trigger but
-- an ability that watches a mentor ability RESOLVE. "An ability that triggers
-- whenever a creature mentors another creature triggers whenever a mentor ability
-- whose source is the first creature and whose target is the second creature
-- resolves", which is TriggerCondition.AttachedCreatureMentors read off
-- GameEvent.Mentored.
--
-- Aegis of the Legion {R}{W} Artifact -- Equipment is the card and the only printing
-- that reads rule 702.134c: "Equipped creature gets +1/+1 and has mentor. Whenever
-- equipped creature mentors a creature, put a shield counter on that creature. Equip
-- {3}". Every case below equips it by fixture (CR 301.5a's attachment as a state,
-- not the ability that makes it), so what is under test is the trigger rather than
-- CR 702.6a's equip.
--
-- Hill Giant 3/3 wears it, which makes it a 4/4 with mentor -- so no number here is
-- printed on any card in the board: the mentor's 4 is the Equipment's bonus, the
-- mentored Goblin Piker's 3/2 is its printed 2/1 plus rule 702.134a's counter, and 4
-- is not 3 is not 2. The Aegis itself is a fourth reading again, holding no counters
-- at all.
--
-- What separates "a creature MENTORED another" from "a creature WITH MENTOR
-- attacked" is the pair of declarations: the Giant attacking beside the Piker
-- mentors it, and the Giant attacking ALONE triggers rule 702.134a all the same and
-- mentors nothing, rule 702.134a's target having to be an attacking creature. The
-- two boards are the same board; only the attackers differ.
mentorsTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mentorsTriggerSpec s registry =
  let board mine = do
        ours <- mapM (S.printingOf s registry) mine
        pure (S.combatBoardOf ours [])
      plan :: [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      plan attackers target p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature target))) sets
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "CR 702.134c a creature mentoring another" $ do
        -- The proving test, and the whole of rule 702.134c in one board: the mentor
        -- ability resolves, and the ability watching it puts its counter on the
        -- creature that was MENTORED -- rule 702.134c's "second creature", which is
        -- neither the Aegis (the ability's own source) nor the Giant (the first
        -- creature). Rule 702.134a's +1/+1 counter sits beside it on the same
        -- permanent, so the two kinds are told apart rather than counted together.
        Spec.it s "CR 702.134c the mentored creature takes the shield counter" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion"]
          case mine of
            [giant, piker, aegis] -> do
              let after = atDamage (plan [giant, piker] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "the Piker carries rule 702.134a's counter and rule 702.134c's"
                (countersOn piker after)
                (Map.fromList [(CounterKind.PlusOnePlusOne, 1), (CounterKind.Shield, 1)])
              Spec.assertEqWith
                s
                "and neither the mentor nor the Equipment carries either"
                (countersOn giant after, countersOn aegis after)
                (Map.empty, Map.empty)
              Spec.assertEqWith
                s
                "the equipped Giant is a 4/4 and the mentored Piker a 3/2"
                (S.powerToughnessOf giant after, S.powerToughnessOf piker after)
                (Just (4, 4), Just (3, 2))
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker and an Aegis"
        -- The negative, and the case that makes the one above about MENTORING rather
        -- than about attacking: the same board, with the Piker held back. Rule
        -- 702.134a's ability still triggers -- the Giant attacked -- but rule
        -- 702.134a's target must be an attacking creature (CR 508.1k), so the
        -- ability has no legal target, never resolves, and rule 702.134c's event
        -- never happens. The answerer still aims at the Piker, so an engine that
        -- mentored a creature that stayed home would put both counters on it.
        Spec.it s "CR 702.134c attacking is not mentoring: nothing was mentored" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion"]
          case mine of
            [giant, piker, aegis] -> do
              let after = atDamage (plan [giant] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "no counters anywhere"
                (countersOn piker after, countersOn giant after)
                (Map.empty, Map.empty)
              Spec.assertEqWith
                s
                "and the Piker is its printed 2/1"
                (S.powerToughnessOf piker after)
                (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker and an Aegis"
        -- CR 122.6: BOTH counters go on through the placement funnel, so a CR 614.16
        -- replacement reaches them. Doubling Season ({5}{G}, "If an effect would put
        -- one or more counters on a permanent you control, it puts twice that many")
        -- doubles each, and 2 and 2 is a different reading from 1 and 1: rule
        -- 702.134a's counter would not double if the mentor opcode wrote it straight
        -- onto the permanent, and rule 702.134c's would not if the shield counter
        -- did.
        Spec.it s "CR 122.6 Doubling Season doubles both of them" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion", "Doubling Season"]
          case mine of
            [giant, piker, aegis, _] -> do
              let after = atDamage (plan [giant, piker] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "two of each"
                (countersOn piker after)
                (Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.Shield, 2)])
              Spec.assertEqWith s "so the Piker is a 4/3" (S.powerToughnessOf piker after) (Just (4, 3))
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker, an Aegis and a Doubling Season"
        -- The same funnel narrowed to ONE of the two kinds, which is what tells the
        -- readings apart that Doubling Season above leaves symmetrical: Hardened
        -- Scales ({G}, "If one or more +1/+1 counters would be put on a creature you
        -- control, that many plus one are put instead") reaches rule 702.134a's
        -- counter and not rule 702.134c's, so the Piker ends on two +1/+1 counters
        -- and one shield counter -- a pair no other reading of this board produces.
        Spec.it s "CR 614.1 Hardened Scales reaches the +1/+1 counter alone" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion", "Hardened Scales"]
          case mine of
            [giant, piker, aegis, _] -> do
              let after = atDamage (plan [giant, piker] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "two +1/+1 counters, one shield counter"
                (countersOn piker after)
                (Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.Shield, 1)])
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker, an Aegis and a Hardened Scales"
        -- CR 301.5f's "equipped creature", which is the whole of what the condition
        -- narrows by. A mentoring happens -- Blade Instructor's own printed mentor
        -- (CR 702.134a) puts its counter on the Piker -- and the Aegis, worn by an
        -- Icehide Golem that stayed home, is watching the wrong creature, so no
        -- shield counter is put. An engine that read the condition as "a creature
        -- mentors" rather than "equipped creature mentors" would fire here.
        Spec.it s "CR 702.134c another creature's mentoring is not the equipped creature's" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker", "Icehide Golem", "Aegis of the Legion"]
          case mine of
            [instructor, piker, golem, aegis] -> do
              let after = atDamage (plan [instructor, piker] piker) (S.attach aegis golem gs)
              Spec.assertEqWith
                s
                "the Instructor's counter landed and no shield counter did"
                (countersOn piker after)
                (Map.singleton CounterKind.PlusOnePlusOne 1)
              Spec.assertEqWith
                s
                "nor anywhere else"
                (countersOn golem after, countersOn instructor after, countersOn aegis after)
                (Map.empty, Map.empty, Map.empty)
            _ -> Spec.assertFailure s "fixture should give alice an Instructor, a Piker, a Golem and an Aegis"

-- CR 702.149a's training, which rule 702 states as a triggered
-- ability -- and the first whose trigger CONDITION reads the rest of the
-- declaration, through Filter.PowerGreaterThanSource and the source power
-- TriggerCondition.SelfAttacksWithAnother supplies.
--
-- Apprentice Sharpshooter {2}{G} Creature -- Human Archer 1/4 is the card: reach
-- and training, and reach touches nothing here, so every number below is the
-- keyword's. Its 1 power is what the companions are measured against -- Goblin
-- Piker's 2 clears it, a second Sharpshooter's 1 does not.
--
-- Readings are taken at the DECLARE BLOCKERS step, one step after the trigger
-- resolves, so the counter is read before combat damage can move anything.
trainingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trainingSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures. S.aggressiveAnswer
      -- attacks with everything, so a case about WHO attacks has to say it.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      atBlockers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      -- CR 122.1: what is actually on the permanent, which a +1/+1 EFFECT would
      -- leave empty while reading the same 2/5.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "Training" $ do
        -- The proving test. Both attack, the Piker's 2 beats the Sharpshooter's 1,
        -- and the assertion covers all three things at once: the counter lands on
        -- the BEARER, the companion takes none, and what landed is a CR 122.1a
        -- counter rather than a pump.
        Spec.it s "CR 702.149a whole card: attacking beside a bigger creature trains" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Goblin Piker"] []
          case mine of
            [sharpshooter, piker] -> do
              let after = atBlockers (plan [sharpshooter, piker]) gs
              Spec.assertEqWith
                s
                "the Sharpshooter is 2/5 and the Piker is untouched"
                (S.powerToughnessOf sharpshooter after, S.powerToughnessOf piker after)
                (Just (2, 5), Just (2, 1))
              Spec.assertEqWith s "and it is a counter" (countersOn sharpshooter after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice a Sharpshooter and a Piker"
        -- "POWER GREATER THAN this creature's power" is strict, so two 1-power
        -- Sharpshooters attacking together train neither. Same declaration shape
        -- as the proving test; only the companion's power differs.
        Spec.it s "CR 702.149a a companion whose power is only equal trains nobody" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Apprentice Sharpshooter"] []
          case mine of
            [first, second] -> do
              let after = atBlockers (plan [first, second]) gs
              Spec.assertEqWith
                s
                "both are at their printed size"
                (S.powerToughnessOf first after, S.powerToughnessOf second after)
                (Just (1, 4), Just (1, 4))
            _ -> Spec.assertFailure s "fixture should give alice two Sharpshooters"
        -- CR 508.3a's "attack": the Hill Giant is bigger and on the same side, and
        -- it trains nothing while it stays home. The falsifier for a condition that
        -- swept the battlefield instead of the declaration.
        Spec.it s "CR 508.3a a bigger creature that stayed home is no companion" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Hill Giant"] []
          case mine of
            [sharpshooter, _] ->
              Spec.assertEqWith
                s
                "the Sharpshooter is its printed 1/4"
                (S.powerToughnessOf sharpshooter (atBlockers (plan [sharpshooter]) gs))
                (Just (1, 4))
            _ -> Spec.assertFailure s "fixture should give alice a Sharpshooter and a Giant"
        -- Two combat phases in one turn, which is what makes the COMBAT RECORD the
        -- right source and the event log the wrong one: the log keeps the whole
        -- turn's declarations, so a log-fold would find Aurelia in the
        -- second declaration she is not part of and train the Sharpshooter twice.
        -- Aurelia, the Warleader {2}{R}{R}{W}{W} 3/4 is the pool's extra-combat
        -- attacker, and her 3 power clears the Sharpshooter's 1 in the first phase.
        Spec.it s "CR 702.149a the added combat phase counts only its own declaration" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Aurelia, the Warleader"] []
          case mine of
            [sharpshooter, aurelia] -> do
              let first = atBlockers (plan [sharpshooter, aurelia]) gs
                  second = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (plan [sharpshooter]) first
                  after = atBlockers (plan [sharpshooter]) second
              Spec.assertEqWith s "one counter from the first declaration" (S.powerToughnessOf sharpshooter first) (Just (2, 5))
              Spec.assertEqWith s "the second phase really ran a declaration" (GameState.phase second) (Phase.Combat CombatStep.DeclareAttackers)
              Spec.assertEqWith s "and it added no second counter" (countersOn sharpshooter after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice a Sharpshooter and Aurelia"
        -- The same multiplicity asserted of the MINT, as mentor's and flanking's
        -- instance cases are: CR 702.149b says each instance triggers separately,
        -- and no card in the pool prints training twice.
        Spec.it s "CR 702.149b two instances mint two abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Training 2
              expected =
                TriggerCondition.SelfAttacksWithAnother
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.PowerGreaterThanSource])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each watching CR 702.149a's declaration"
            (fmap TriggeredAbility.condition abilities)
            [expected, expected]

-- CR 702.149c's second trigger form: "when this creature trains" means "when a
-- resolving training ability puts one or more +1/+1 counters on this creature".
--
-- Savior of Ollenbock {1}{W}{W} Creature -- Human Soldier 1/2 is the only paper
-- printing, and the whole card is here: training, "whenever this creature trains,
-- exile up to one other target creature from the battlefield or creature card
-- from a graveyard", and "when this creature leaves the battlefield, put the
-- exiled cards onto the battlefield under their owners' control".
--
-- The exile clause is what makes the trigger OBSERVABLE at gameplay level: rule
-- 702.149c's marker is otherwise invisible, the counter it rides being an
-- ordinary +1/+1 counter. So every case below reads the exile rather than the
-- counter, and the counter assertions are there to prove the training half
-- happened at all.
--
-- The pair of boards differs in exactly one thing: the companion's POWER, moved
-- across rule 702.149a's threshold by a continuous effect rather than by swapping
-- the card, so seats, timing, stock and the declaration are identical.
--
-- The other-source case is Battlegrowth's counter, which is the discrimination
-- this whole unit exists for: a +1/+1 counter arriving from anything but a
-- resolving training ability trains nobody.
saviorOfOllenbockSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
saviorOfOllenbockSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures, trainingSpec's.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      -- CR 601.2c's announcement for the trained creature's trigger: one target,
      -- PINNED rather than searched. An answerer that picked a legal option would
      -- find another one after a mutation and repair the assertion; this one hands
      -- back the recipient the case names, tag and all -- ToCreature for the
      -- battlefield half of the pool, ToObject for the graveyard half.
      aimingAt :: Recipient.Recipient -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      aimingAt recipient attackers p = case p of
        Prompt.AnnounceTargets _ _ _ offers -> fmap (const 1) offers
        Prompt.ChooseTargets _ _ _ asked -> fmap (const (Set.singleton recipient)) asked
        _ -> plan attackers p
      atBlockers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      nameOf oid gs = fmap Face.name (Game.faceOf oid gs)
      -- Exile is one shared zone (CR 400.1), so this is everything in it whoever
      -- owns it. By NAME, because CR 400.7 mints the exiled card a fresh id.
      exiledNames gs = List.sort (Maybe.mapMaybe (`nameOf` gs) (Set.toList (GameState.exile gs)))
      controlledNames pid gs =
        List.sort (Maybe.mapMaybe (\oid -> if Projection.controllerOf oid gs == Just pid then nameOf oid gs else Nothing) (Set.toList (GameState.battlefield gs)))
      graveyardNames pid gs = List.sort (Maybe.mapMaybe (`nameOf` gs) (Game.zoneMembers Zone.Graveyard pid gs))
      -- Destroy the Savior (CR 701.8a), settle so the CR 117.5 boundary scans the
      -- departure and places the leaves-the-battlefield trigger, then resolve it --
      -- promiseOfTomorrowReturnSpec's killIt, and BOTH states come back for its
      -- reason: "the ability triggered" is only readable at the placement.
      killIt oid gs =
        let killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [oid])
            placed = S.runPure S.identityAnswer killed Engine.settleForPriority
         in (placed, S.runPure S.identityAnswer placed Stack.resolveTop)
      named = CardName.MkCardName . Text.pack
      -- Rule 702.149a's threshold crossed from below by a continuous effect: the
      -- Piker's 2 power becomes 1, which is the Savior's own, and CR 702.149a's
      -- "greater" is strict.
      shrink oid = S.withEffect oid (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal (-1)) (Quantity.Type.Literal 0)))
   in Spec.describe s "CR 702.149c a trigger on training" $ do
        -- The proving test. The Piker's 2 clears the Savior's 1, the training
        -- ability resolves and puts the counter, and rule 702.149c's trigger then
        -- exiles the creature it targeted.
        Spec.it s "CR 702.149c whole card: training exiles the targeted creature" $ do
          (gs, mine, theirs) <- board ["Savior of Ollenbock", "Goblin Piker"] ["Hill Giant"]
          case (mine, theirs) of
            ([savior, piker], [giant]) -> do
              let after = atBlockers (aimingAt (Recipient.ToCreature giant) [savior, piker]) gs
              Spec.assertEqWith
                s
                "the counter landed on the Savior and not on its companion"
                (countersOn savior after, countersOn piker after)
                (Map.singleton CounterKind.PlusOnePlusOne 1, Map.empty)
              Spec.assertEqWith s "and the trigger exiled the Giant" (exiledNames after) [named "Hill Giant"]
              Spec.assertEqWith s "which bob no longer controls" (controlledNames S.bob after) []
            _ -> Spec.assertFailure s "fixture should give alice a Savior and a Piker, and bob a Giant"
        -- The one-difference control: the same board with the companion's power
        -- one lower, so rule 702.149a's strict "greater" is not met, nothing
        -- trains, and rule 702.149c's trigger never fires.
        Spec.it s "CR 702.149a a companion whose power is only equal exiles nothing" $ do
          (gs, mine, theirs) <- board ["Savior of Ollenbock", "Goblin Piker"] ["Hill Giant"]
          case (mine, theirs) of
            ([savior, piker], [giant]) -> do
              let weakened = shrink piker gs
                  after = atBlockers (aimingAt (Recipient.ToCreature giant) [savior, piker]) weakened
              Spec.assertEqWith s "the companion really is a 1/1 now" (S.powerToughnessOf piker weakened) (Just (1, 1))
              Spec.assertEqWith s "no counter was put" (countersOn savior after) Map.empty
              Spec.assertEqWith s "and nothing was exiled" (exiledNames after) []
              Spec.assertBool s (S.onBattlefield giant after) "the Giant is where it was"
            _ -> Spec.assertFailure s "fixture should give alice a Savior and a Piker, and bob a Giant"
        -- The pool's OTHER half, and the tag that goes with it: a creature card in
        -- a graveyard is ToObject, where the battlefield half is ToCreature. Bob's
        -- graveyard, so "a graveyard" is not read as the controller's own.
        Spec.it s "CR 404.1 whole card: the same slot reaches a creature card in a graveyard" $ do
          (gs, mine, _) <- board ["Savior of Ollenbock", "Goblin Piker"] []
          sentry <- S.printingOf s registry "Ogre Sentry"
          case mine of
            [savior, piker] -> do
              let (card, stocked) = S.addGraveyardCard sentry S.bob gs
                  after = atBlockers (aimingAt (Recipient.ToObject card) [savior, piker]) stocked
              Spec.assertEqWith s "the Savior trained" (countersOn savior after) (Map.singleton CounterKind.PlusOnePlusOne 1)
              Spec.assertEqWith s "and the graveyard card is in exile" (exiledNames after) [named "Ogre Sentry"]
              Spec.assertEqWith s "out of bob's graveyard" (graveyardNames S.bob after) []
            _ -> Spec.assertFailure s "fixture should give alice a Savior and a Piker"
        -- CR 607.2a's linked set read back by the card's third ability. The victim
        -- is BOB's card, so "under their owners' control" is observable: the
        -- ability's controller is alice, and an owner-blind return would hand her
        -- the Sentry.
        Spec.it s "CR 607.2a whole card: the Savior leaving the battlefield returns what it exiled, to its owner" $ do
          (gs, mine, _) <- board ["Savior of Ollenbock", "Goblin Piker"] []
          sentry <- S.printingOf s registry "Ogre Sentry"
          case mine of
            [savior, piker] -> do
              let (card, stocked) = S.addGraveyardCard sentry S.bob gs
                  exiled = atBlockers (aimingAt (Recipient.ToObject card) [savior, piker]) stocked
                  (placed, after) = killIt savior exiled
              Spec.assertEqWith s "the premise: the Sentry is in exile" (exiledNames exiled) [named "Ogre Sentry"]
              Spec.assertEqWith s "the departure placed one trigger" (length (GameState.stack placed)) 1
              Spec.assertEqWith s "exile is empty again" (exiledNames after) []
              Spec.assertEqWith s "and bob controls the Sentry" (controlledNames S.bob after) [named "Ogre Sentry"]
              Spec.assertEqWith s "while alice keeps only her Piker" (controlledNames S.alice after) [named "Goblin Piker"]
            _ -> Spec.assertFailure s "fixture should give alice a Savior and a Piker"
        -- The discrimination rule 702.149c is FOR: a +1/+1 counter arriving from
        -- Battlegrowth ({G} Instant, "put a +1/+1 counter on target creature") is
        -- the same counter the training ability would have put, and it trains
        -- nobody. Nothing attacks, so nothing else could.
        Spec.it s "CR 702.149c a +1/+1 counter from another source is not training" $ do
          savior <- S.printingOf s registry "Savior of Ollenbock"
          giant <- S.printingOf s registry "Hill Giant"
          forest <- S.printingOf s registry "Forest"
          battlegrowth <- S.printingOf s registry "Battlegrowth"
          let (saviorId, withSavior) = S.addPermanent savior S.alice (S.landsInPlay forest 1)
              (giantId, withGiant) = S.addPermanent giant S.bob withSavior
              (handed, spellId) = S.handOne battlegrowth withGiant
              cast = snd (Engine.runGamePure (aimingAt (Recipient.ToCreature saviorId) []) handed (S.cast S.alice spellId))
              resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
              -- The SETTLE is the load-bearing step, not the resolution: CR 603.3
              -- places a trigger when the boundary scans the log, so a state read
              -- straight off the resolution cannot tell "never triggered" from
              -- "triggered and not yet placed". Without this the case passes
              -- against a condition that fires on every counter placement.
              after = S.runPure S.identityAnswer resolved Engine.settleForPriority
          Spec.assertEqWith s "the counter really arrived" (countersOn saviorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "and the settle placed no trigger" (length (GameState.stack after)) 0
          Spec.assertEqWith s "and nothing was exiled" (exiledNames after) []
          Spec.assertBool s (S.onBattlefield giantId after) "the Giant bob controls is untouched"

-- CR 702.147a's decayed: a combat restriction and a triggered ability that arms
-- a CR 603.7 DELAYED one -- the first minted ability to arm anything, and so the
-- first Effect.ArmDelayedTrigger whose name is on no face
-- (Keyword.mintedDelayedAbilities).
--
-- Falcon Abomination {2}{U} Creature -- Zombie Bird 2/2 is the producer: flying,
-- and "when this creature enters, create a 2/2 black Zombie creature token with
-- decayed". Decayed is printed on tokens far more often than on cards, so the
-- keyword arrives here through the card's own Create -- codec-parsed card data,
-- never a hand-built face -- and the Falcon beside it is the control, a creature
-- of the same size and controller with no decayed.
--
-- bob defends with NOTHING, deliberately: a blocked 2/2 token would die to CR
-- 704.5g whether or not rule 702.147a did anything, so the sacrifice assertion
-- would pass for the wrong reason.
decayedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
decayedSpec s registry =
  let settleFor gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- CR 302.6: the token is minted this turn and so is summoning sick, and
      -- nothing in the pool gives a decayed token haste -- so this is the state a
      -- turn later would reach, and the one fixture step below that is not the
      -- card's own doing.
      settled oid gs =
        gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Settled S.alice}) oid (GameState.objects gs)}
      -- alice's Falcon Abomination, entered and its trigger resolved, with the
      -- Zombie token settled beside it.
      board = do
        falcon <- S.printingOf s registry "Falcon Abomination"
        let (gs0, _, _) = S.combatBoardOf [] []
            (bird, gs1) = S.entersWithTrigger falcon S.alice gs0
            made = resolveAll (settleFor gs1)
        pure (bird, made, S.tokensOf made)
      noAttacks :: Prompt.Prompt r -> r
      noAttacks p = case p of
        Prompt.DeclareAttackers {} -> []
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      atMain = S.runToStep Phase.PostcombatMain
   in Spec.describe s "Decayed (CR 702.147)" $ do
        -- CR 509.1b through rule 702.147a's static half, unleash's carrier with
        -- no gate. The Falcon is on the same board with the same controller, so a
        -- restriction that stopped every creature blocking cannot pass.
        Spec.it s "CR 702.147a the token enters with decayed and cannot block" $ do
          (bird, made, tokens) <- board
          case tokens of
            [zombie] -> do
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Decayed zombie made) "the token has decayed"
              Spec.assertBool s (not (Combat.canBlock S.alice zombie made)) "so it cannot block"
              Spec.assertBool s (Combat.canBlock S.alice bird made) "while the Falcon that made it can"
              Spec.assertEqWith s "and only the Falcon is offered" (Combat.legalBlockers S.alice made) [bird]
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 508.3a's declaration puts the minted trigger on the stack, and its
        -- resolution arms rule 702's own delayed ability -- the only armed entry
        -- on the board, since no card here declares one.
        Spec.it s "CR 702.147a attacking arms one delayed ability" $ do
          (_, made, tokens) <- board
          case tokens of
            [zombie] -> do
              let after = atBlockers S.aggressiveAnswer (settled zombie made)
              Spec.assertBool s (elem zombie (S.attackerDeclarationsOf after)) "the token really attacked"
              Spec.assertEqWith s "one delayed ability waiting" (Seq.length (GameState.delayedTriggers after)) 1
              Spec.assertBool s (S.onBattlefield zombie after) "and it is still there at declare blockers"
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 511.2: an ability that triggers "at end of combat" triggers as the
        -- end of combat step begins. The Falcon attacked too and survives, so
        -- "everything alice attacked with died" cannot pass this.
        Spec.it s "CR 511.2 the delayed ability sacrifices it at end of combat" $ do
          (bird, made, tokens) <- board
          case tokens of
            [zombie] -> do
              let after = atMain S.aggressiveAnswer (settled zombie made)
              Spec.assertBool s (not (S.onBattlefield zombie after)) "the token is gone"
              Spec.assertBool s (S.onBattlefield bird after) "while the Falcon that attacked beside it is still there"
              Spec.assertEqWith s "and the delayed ability is spent" (Seq.length (GameState.delayedTriggers after)) 0
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- THE PAIR THAT MAKES THE TRIGGER REAL. Same board, same fixture, and
        -- only the declaration different: rule 702.147a sacrifices a creature
        -- that ATTACKED, so a decayed creature held back survives its own end of
        -- combat. CR 508.8 skips the declare blockers and combat damage steps on
        -- this run, which the end of combat step is not among.
        Spec.it s "CR 702.147a a decayed creature that did not attack is not sacrificed" $ do
          (_, made, tokens) <- board
          case tokens of
            [zombie] -> do
              let after = atMain noAttacks (settled zombie made)
              Spec.assertEqWith s "nothing was declared" (S.attackerDeclarationsOf after) []
              Spec.assertEqWith s "so nothing was armed" (Seq.length (GameState.delayedTriggers after)) 0
              Spec.assertBool s (S.onBattlefield zombie after) "and the token is still there"
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))

-- CR 603.2's "that player" reaching a TARGET SLOT rather than an effect's
-- operand: Trygon Predator's "whenever this creature deals combat damage to a
-- player, you may destroy target artifact or enchantment THAT PLAYER controls".
-- The slot is narrowed by Filter.ControlledByBound, baked to the damaged player
-- by Pawl.Engine.Filter.bakeBound at both of CR 115's moments -- CR 603.3d's
-- choosing and CR 608.2b's re-check.
--
-- THREE SEATS, and every seat holds the same permanent (Bad Moon), so the board
-- differs in exactly one thing: who controls it. A filter that read "an
-- opponent" would admit bob's, and one that dropped the controller conjunct
-- would admit alice's own; the answerer below takes the LOWEST-numbered legal
-- target of each slot, and alice's Moon is added before bob's and bob's before
-- carol's, so either mistake takes a different permanent rather than passing.
trygonPredatorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trygonPredatorSpec s registry =
  let plan :: Prompt.Prompt r -> r
      plan p = case p of
        Prompt.ChooseDefender {} -> S.carol
        Prompt.ChooseAttackTarget {} -> S.attackTo S.carol p
        -- CR 603.5's printed "may", always exercised: a declined clause would
        -- destroy nothing, and this group is about which permanent it reaches.
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseTargets _ _ _ asked -> fmap (maybe Set.empty Set.singleton . Set.lookupMin . snd) asked
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board = do
        trygon <- S.printingOf s registry "Trygon Predator"
        badMoon <- S.printingOf s registry "Bad Moon"
        let (gs0, mine, theirs, others) = S.threePlayerCombat [trygon, badMoon] [badMoon] [badMoon]
            -- S.threePlayerCombat starts at the beginning of combat, so the
            -- declarations are run as steps (which is what fills CR 508.5's
            -- defending player) and only the damage is dealt by hand -- that is
            -- the seam CR 603.3d's placement needs to be observable in, since a
            -- whole step would resolve the trigger as well.
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) plan gs0
            fought = S.runPure plan atDamage Damage.dealCombatDamage
        pure (mine, theirs, others, S.runPure plan fought Engine.settleForPriority)
   in Spec.describe s "TrygonPredator" $ do
        -- THE proving test. CR 603.3d picks the target as the ability is put on
        -- the stack, and the binding it stamps is read back here rather than
        -- inferred from what died -- so this says which permanent was OFFERED,
        -- not merely which one an effect happened to reach.
        Spec.it s "CR 603.3d the slot admits only the damaged player's permanent" $ do
          (mine, theirs, others, placed) <- board
          case (mine, theirs, others, GameState.stack placed) of
            ([_, alices], [bobs], [carols], [abilityId]) -> do
              let bindings = maybe Map.empty Object.bindings (Game.lookupObject abilityId placed)
                  slotOf name = Map.lookup (SlotName.MkSlotName (Text.pack name)) (Binding.targetsOf bindings)
              Spec.assertEqWith
                s
                "carol took the damage, so she is the player the event bound"
                (slotOf "thatPlayer")
                (Just (Set.singleton (Recipient.ToPlayer S.carol)))
              Spec.assertEqWith
                s
                "and carol's Bad Moon is the one target the slot admitted"
                (slotOf "target")
                (Just (Set.singleton (Recipient.ToObject carols)))
              Spec.assertBool s (alices /= bobs && bobs /= carols) "the three Moons are distinct objects"
            _ -> Spec.assertFailure s "fixture should give alice a Predator and a Moon, bob and carol a Moon each, and place one trigger"
        -- The same board run to the end: the ability resolves and destroys the
        -- one permanent, leaving both other seats' untouched. The whole card,
        -- CR 701.8a's destruction included.
        Spec.it s "CR 608.2c whole card: only carol's Bad Moon is destroyed" $ do
          (mine, theirs, others, placed) <- board
          let after = S.runPure plan placed Engine.priorityLoop
          case (mine, theirs, others) of
            ([_, alices], [bobs], [carols]) -> do
              Spec.assertBool s (not (S.onBattlefield carols after)) "carol's Moon is destroyed"
              Spec.assertBool s (S.onBattlefield bobs after) "bob's Moon is untouched"
              Spec.assertBool s (S.onBattlefield alices after) "and so is alice's own"
            _ -> Spec.assertFailure s "fixture should give each seat a Moon"
        -- CR 608.2b at the OTHER moment: the target changes hands after it was
        -- chosen, so it is no longer a permanent that player controls and the
        -- ability's only target is illegal. The pair differs in exactly the
        -- control change -- same board, same answers, same stack -- which is
        -- what makes the survival the rule's and not the fixture's.
        Spec.it s "CR 608.2b a target that changes hands is no longer that player's" $ do
          (_, _, others, placed) <- board
          case others of
            [carols] -> do
              let stolen = S.runPure plan (S.giveControl carols S.bob placed) Engine.priorityLoop
                  kept = S.runPure plan placed Engine.priorityLoop
              Spec.assertBool s (S.onBattlefield carols stolen) "bob controls it now, so the ability fizzles"
              Spec.assertBool s (not (S.onBattlefield carols kept)) "and without the change it is destroyed"
            _ -> Spec.assertFailure s "fixture should give carol a Moon"

-- BOTH halves of one DamageDealt event read by one bearer-scoped trigger:
-- Questing Beast {2}{G}{G} Legendary Creature -- Beast 4/4, "whenever Questing
-- Beast deals combat damage to an opponent, it deals THAT MUCH damage to target
-- planeswalker THAT PLAYER controls". The amount rides
-- Pawl.Engine.Binding.eventAmount and the player Binding.triggerPlayer, and the
-- target slot narrows by Filter.ControlledByBound off the second -- so the two
-- slots the condition stamps are read at once, one as a Quantity and one as a
-- filter.
--
-- THREE SEATS, bob and carol holding the same planeswalker printing, so "that
-- player controls" is a different set from "an opponent controls" and from "a
-- planeswalker" (trygonPredatorSpec above makes the same distinction for the
-- destroy half of the pattern).
--
-- THREE DISTINCT NUMBERS, so the loyalty count names one reading of "that much"
-- and rejects two: a -1/-1 counter makes the Beast a 3/3 before it connects, so
-- the event carries 3 rather than the printed 4, and four +1/+1 counters added
-- AFTER the damage but BEFORE the ability resolves leave it a 7/7, so a payload
-- reading the source's power at resolution would take 7. Both walkers start on 6
-- loyalty counters, which survives 3 and 4 and dies to 7.
--
-- "An opponent" is deliberately not transcribed, and this is a rules equivalence
-- rather than an elision: CR 508.1a lets only the active player's creatures
-- attack, CR 506.2 and CR 506.2a make the defending player one of the attacking
-- player's opponents, CR 510.1b assigns an unblocked creature's combat damage to
-- what it is attacking, and CR 506.4 removes a permanent from combat if its
-- controller changes. A creature can only ever deal combat damage to a player who
-- is its controller's opponent, so the nullary condition admits exactly the
-- printed events.
questingBeastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
questingBeastSpec s registry =
  let plan :: Prompt.Prompt r -> r
      plan p = case p of
        Prompt.ChooseDefender {} -> S.carol
        -- CR 508.1b's choice pinned to the PLAYER: carol's own planeswalker is a
        -- legal attack target too, and attacking it would deal no combat damage
        -- to a player at all.
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer S.carol) (NonEmpty.toList options))
        Prompt.ChooseTargets _ _ _ asked -> fmap (maybe Set.empty Set.singleton . Set.lookupMin . snd) asked
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board = do
        beast <- S.printingOf s registry "Questing Beast"
        karn <- S.printingOf s registry "Karn Liberated"
        let (gs0, mine, theirs, others) = S.threePlayerCombat [beast] [karn] [karn]
            loyal = List.foldl' (flip (S.addCounter CounterKind.Loyalty 6)) gs0 (theirs <> others)
            shrunk = List.foldl' (flip (S.addCounter CounterKind.MinusOneMinusOne 1)) loyal mine
            -- The same seam trygonPredatorSpec uses: the declarations run as
            -- steps, the damage is dealt by hand, and settleForPriority places
            -- the trigger -- which leaves a state where the ability is on the
            -- stack and has not resolved.
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) plan shrunk
            fought = S.runPure plan atDamage Damage.dealCombatDamage
            placed = S.runPure plan fought Engine.settleForPriority
        pure (mine, theirs, others, shrunk, placed)
   in Spec.describe s "QuestingBeast" $ do
        -- THE proving test, at gameplay level: what carol's planeswalker lost.
        Spec.it s "CR 120.3c whole card: that much is the damage the event carried" $ do
          (mine, theirs, others, before, placed) <- board
          case (mine, theirs, others) of
            ([beastId], [bobs], [carols]) -> do
              let bumped = S.addCounter CounterKind.PlusOnePlusOne 4 beastId placed
                  after = S.runPure plan bumped Engine.priorityLoop
              -- The fixture's own preconditions, asserted rather than assumed:
              -- neither can be reddened by the binding under test.
              Spec.assertEqWith s "the -1/-1 counter makes the Beast a 3/3 before it connects" (S.powerToughnessOf beastId before) (Just (3, 3))
              Spec.assertEqWith s "and both walkers start on 6 loyalty" (S.counterOf CounterKind.Loyalty bobs before, S.counterOf CounterKind.Loyalty carols before) (6, 6)
              Spec.assertEqWith s "CR 120.3c: 6 - 3, not 6 - 4 and not dead on 7" (S.counterOf CounterKind.Loyalty carols after) 3
              Spec.assertEqWith s "and bob's planeswalker is untouched" (S.counterOf CounterKind.Loyalty bobs after) 6
              Spec.assertEqWith s "CR 510.1b: carol herself took the Beast's 3" (S.lifeOf S.carol after) (Just 17)
              Spec.assertEqWith s "CR 704.5q: the Beast is a 7/7 by the time the ability resolves" (S.powerToughnessOf beastId after) (Just (7, 7))
            _ -> Spec.assertFailure s "fixture should give alice a Beast and bob and carol a planeswalker each"
        -- The slots themselves, read back off the placed ability rather than
        -- inferred from what happened -- so this says which player the event
        -- named and which permanent the filter OFFERED.
        Spec.it s "CR 603.3d the slot admits only the damaged player's planeswalker" $ do
          (_, theirs, others, _, placed) <- board
          case (theirs, others, GameState.stack placed) of
            ([bobs], [carols], [abilityId]) -> do
              let bindings = maybe Map.empty Object.bindings (Game.lookupObject abilityId placed)
                  slotOf name = Map.lookup (SlotName.MkSlotName (Text.pack name)) (Binding.targetsOf bindings)
              Spec.assertEqWith s "carol took the damage, so she is the player the event bound" (slotOf "thatPlayer") (Just (Set.singleton (Recipient.ToPlayer S.carol)))
              Spec.assertEqWith s "and carol's planeswalker is the one target the slot admitted" (slotOf "target") (Just (Set.singleton (Recipient.ToObject carols)))
              Spec.assertBool s (bobs /= carols) "the two planeswalkers are distinct objects"
            _ -> Spec.assertFailure s "fixture should give bob and carol a planeswalker each and place one trigger"

-- The same "that player", stamped by the FILTERED twin of that condition and read
-- by a BYSTANDER: Larceny {3}{B}{B} Enchantment, "whenever a creature you control
-- deals combat damage to a player, that player discards a card". The whole card is
-- that one clause, so every reading below is the condition's.
--
-- THREE SEATS, each holding three cards, so the damaged player (carol), the
-- damager's controller (alice) and a bystanding opponent (bob) are three
-- different hands -- and three cards apiece makes "discarded once" (two left)
-- distinguishable from "discarded twice" (one) and from "not at all" (three).
larcenySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
larcenySpec s registry =
  let plan :: Prompt.Prompt r -> r
      plan p = case p of
        Prompt.ChooseDefender {} -> S.carol
        -- CR 508.1b / CR 802.3's choice pinned to the PLAYER, and to carol's
        -- seat rather than bob's, whom CR 802.2 also makes a defending player.
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer S.carol) (NonEmpty.toList options))
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board = do
        larceny <- S.printingOf s registry "Larceny"
        piker <- S.printingOf s registry "Goblin Piker"
        let (gs0, _, _, _) = S.threePlayerCombat [larceny, piker] [] []
            stocked = List.foldl' (\g pid -> List.foldl' (\g' _ -> snd (S.addHandCard piker pid g')) g [(), (), ()]) gs0 [S.alice, S.bob, S.carol]
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) plan stocked
            fought = S.runPure plan atDamage Damage.dealCombatDamage
            placed = S.runPure plan fought Engine.settleForPriority
        pure (stocked, placed)
   in Spec.describe s "Larceny" $ do
        -- THE proving test, at gameplay level: whose hand shrank.
        Spec.it s "CR 603.2 whole card: the DAMAGED player discards, not the damager's controller" $ do
          (before, placed) <- board
          let after = S.runPure plan placed Engine.priorityLoop
          Spec.assertEqWith s "all three seats start on three cards" (S.handSize S.alice before, S.handSize S.bob before, S.handSize S.carol before) (3, 3, 3)
          Spec.assertEqWith s "carol was dealt the combat damage, so carol discarded exactly one" (S.handSize S.carol after) 2
          Spec.assertEqWith s "alice, whose creature dealt it, discarded none" (S.handSize S.alice after) 3
          Spec.assertEqWith s "and bob, who was not in the combat, none either" (S.handSize S.bob after) 3
          Spec.assertEqWith s "CR 510.1b: the Piker's 2 reached carol" (S.lifeOf S.carol after) (Just 18)
        -- The slot itself, read off the placed ability.
        Spec.it s "CR 603.2 the damaged player is what the event stamped" $ do
          (_, placed) <- board
          case GameState.stack placed of
            [abilityId] -> do
              let bindings = maybe Map.empty Object.bindings (Game.lookupObject abilityId placed)
              Spec.assertEqWith s "thatPlayer is carol" (Map.lookup (SlotName.MkSlotName (Text.pack "thatPlayer")) (Binding.targetsOf bindings)) (Just (Set.singleton (Recipient.ToPlayer S.carol)))
            other -> Spec.assertFailure s ("expected exactly one placed trigger, got " <> show (length other))

-- CR 702.39a's provoke, which rule 702 states as a triggered
-- ability and the FIRST whose payload creates a CR 509.1c blocking REQUIREMENT
-- -- so this is the group that runs a resolution-created requirement through the
-- declare blockers step, and with it Filter.ControlledByDefendingPlayer.
--
-- Goblin Grappler {R} Creature -- Goblin 1/1 is the card: provoke and nothing
-- else, so every reading below is the keyword's. Its victims are the pool's
-- vanillas.
--
-- The answerer DECLINES to block throughout. That is what makes every positive
-- reading a claim about CR 509.1c: the block that happens is the one the rules
-- force, never one the interpreter asked for.
provokeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
provokeSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- Exercise or decline rule 702.39a's "may", aim its target, and never
      -- block voluntarily. S.aggressiveAnswer would block with everything and
      -- Script.declining would decline the "may", so a case about either has to
      -- say both itself.
      plan :: OptionalDecision.OptionalDecision -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      plan may target p = case p of
        Prompt.ChooseOptional {} -> may
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature target))) sets
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      atBlockers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)
   in Spec.describe s "Provoke" $ do
        -- The proving test, and it covers both halves of rule 702.39a at once:
        -- bob's only creature is TAPPED, so CR 509.1a makes it no candidate at
        -- all until the untap, and the block that follows is CR 509.1c's
        -- requirement overriding an interpreter that declined to block.
        Spec.it s "CR 702.39a whole card: the provoked creature untaps and blocks" $ do
          (gs0, mine, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case (mine, theirs) of
            ([grappler], [piker]) -> do
              let after = atDamage (plan OptionalDecision.Exercises piker) (S.tapObject piker gs0)
              Spec.assertEqWith s "the Piker is blocking the Grappler" (Combat.blockersOf grappler after) (Set.singleton piker)
            _ -> Spec.assertFailure s "fixture should give alice a Grappler and bob a Piker"
        -- CR 603.5 / 608.2e: one printed "may" over one clause, so declining it
        -- withholds BOTH instructions. The same board and the same declining
        -- blocker answer as the proving test; only the answer to the "may"
        -- differs, which is what makes that test's block the keyword's.
        Spec.it s "CR 603.5 declining the may leaves the creature tapped and blocking nothing" $ do
          (gs0, mine, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case (mine, theirs) of
            ([grappler], [piker]) -> do
              let after = atDamage (plan OptionalDecision.Declines piker) (S.tapObject piker gs0)
              Spec.assertEqWith s "nothing blocks" (Combat.blockersOf grappler after) Set.empty
              Spec.assertEqWith s "and it is still tapped" (tapStateOf piker after) (Just TapState.Tapped)
            _ -> Spec.assertFailure s "fixture should give alice a Grappler and bob a Piker"
        -- The REQUIREMENT alone, with the untap taken out of the picture: bob's
        -- creature is already untapped, so it could have blocked or not, and CR
        -- 509.1c is the only thing that makes declining illegal.
        Spec.it s "CR 509.1c an untapped provoked creature must block anyway" $ do
          (gs0, mine, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case (mine, theirs) of
            ([grappler], [piker]) ->
              Spec.assertEqWith
                s
                "the Piker is blocking"
                (Combat.blockersOf grappler (atDamage (plan OptionalDecision.Exercises piker) gs0))
                (Set.singleton piker)
            _ -> Spec.assertFailure s "fixture should give alice a Grappler and bob a Piker"
        -- The control for the case above, and the reason it is not vacuous: the
        -- same board with a provokeless attacker lets the declining answer
        -- stand. Goblin Piker {1}{R} 2/1 is the pool's vanilla, so the ONLY
        -- difference between the two boards is the keyword.
        Spec.it s "CR 509.1 the same board without provoke lets the defender decline" $ do
          (gs0, mine, theirs) <- board ["Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([attacker], [piker]) ->
              Spec.assertEqWith
                s
                "nothing blocks"
                (Combat.blockersOf attacker (atDamage (plan OptionalDecision.Exercises piker) gs0))
                Set.empty
            _ -> Spec.assertFailure s "fixture should give alice and bob a Piker each"
        -- CR 508.5 at THREE seats, where "defending player" and "an opponent"
        -- come apart: alice attacks carol, so bob's creature is an opponent's and
        -- is still no legal target. Asked of the slot itself rather than through
        -- a block, because a wrongly admitted target would be untapped and then
        -- pruned by CR 509.1b anyway -- the illegal CHOICE is the observable.
        --
        -- The slot is read off the MINTED ability rather than restated here, so
        -- this is a claim about what provoke writes and not about the atom alone.
        Spec.it s "CR 508.5 only the defending player's creature is a legal target" $ do
          grappler <- S.printingOf s registry "Goblin Grappler"
          piker <- S.printingOf s registry "Goblin Piker"
          let (gs0, mine, theirs, others) = S.threePlayerCombat [grappler] [piker] [piker]
              minted = concatMap (concatMap (Map.elems . Mode.targetSlots) . Modal.modes . TriggeredAbility.modal) (Keyword.abilitiesFor Keyword.Type.Provoke 1)
          case (mine, theirs, others, minted) of
            ([attacker], [bobs], [carols], [slot]) -> do
              let after = atBlockers (S.attackTo S.carol) gs0
              Spec.assertEqWith
                s
                "carol is the defending player, so only her creature"
                (Target.legalRecipients (Just S.alice) attacker slot after)
                (Set.singleton (Recipient.ToCreature carols))
              Spec.assertBool
                s
                (Set.notMember (Recipient.ToCreature bobs) (Target.legalRecipients (Just S.alice) attacker slot after))
                "bob is an opponent but not the defender"
            _ -> Spec.assertFailure s "fixture should give each seat one creature"
        -- CR 500.5a: "this combat" ends with the combat PHASE, so the stored
        -- requirement is swept before the postcombat main phase. Read off the
        -- store rather than through a block, there being no second declare
        -- blockers step in one combat phase to observe it in.
        Spec.it s "CR 500.5a the stored requirement lasts exactly the combat phase" $ do
          (gs0, _, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case theirs of
            [piker] -> do
              let atBlock = atBlockers (plan OptionalDecision.Exercises piker) gs0
                  -- S.runToStep stops as soon as combat is left, so naming the
                  -- postcombat main phase runs the rest of the combat phase.
                  afterCombat = S.runToStep Phase.PostcombatMain (plan OptionalDecision.Exercises piker) atBlock
              Spec.assertEqWith s "stored while the ability has resolved" (length (GameState.blockRequirements atBlock)) 1
              Spec.assertEqWith s "and gone once the phase ends" (GameState.blockRequirements afterCombat) []
            _ -> Spec.assertFailure s "fixture should give bob a Piker"
        -- CR 702.39b's instances, asserted of the MINT as mentor's are, and with
        -- them the slot the gameplay cases can only see through its effects: CR
        -- 508.3a's condition, and rule 702.39a's one printed narrowing.
        Spec.it s "CR 702.39b two instances mint two targeting abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Provoke 2
              expectedSlot = TargetSlot.required Pool.Creatures (Just Filter.Type.ControlledByDefendingPlayer)
              slotsOf ability = concatMap (Map.elems . Mode.targetSlots) (Modal.modes (TriggeredAbility.modal ability))
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each on CR 508.3a's condition"
            (fmap TriggeredAbility.condition abilities)
            [TriggerCondition.SelfAttacks TriggerFrequency.EveryTime, TriggerCondition.SelfAttacks TriggerFrequency.EveryTime]
          Spec.assertEqWith s "each with rule 702.39a's one slot" (concatMap slotsOf abilities) [expectedSlot, expectedSlot]

-- CR 702.112a's renown, which rule 702 states as a triggered
-- ability -- and the first minted one carrying an intervening "if" (CR 603.4),
-- which is the whole of why it fires once and not once per connection.
--
-- Rhox Maulers {4}{G} Creature -- Rhino Soldier 4/4 is the card: trample and
-- renown 2. The 2 is what separates "N counters" from "a counter"; the trample is
-- why the blocked case needs a blocker that absorbs all four damage (Apprentice
-- Sharpshooter, 1/4), since a smaller one would let renown's own event through.
--
-- Valeron Wardens {2}{G} Creature -- Human Monk 1/3 is the second card, and the
-- only printing that WATCHES the designation: renown 2 plus "whenever a creature
-- you control becomes renowned, draw a card" (CR 702.112b's marker read by
-- something other than renown itself).
--
-- CR 702.112b's "until it leaves the battlefield" is read on Object.newIncarnation
-- directly, below. Pawl.SetupSpec's "no per-incarnation state survives" case does
-- NOT cover it -- that case asks whether the forgetting is idempotent, which is
-- blind to a field it never touches.
-- CR 702.100: evolve, whose rule text IS a triggered
-- ability, and the first whose intervening "if" is about the EVENT's object
-- rather than its bearer -- so this is the group that runs a Condition reading
-- another object through Quantity.AgainstSlot at Binding.became, and the first
-- disjunction (Condition.Any) in the pool.
--
-- Cloudfin Raptor {U} Creature -- Bird Mutant 0/1 is the card: flying and evolve,
-- so every counter below is the keyword's. Its 0/1 body is what makes the two
-- halves of rule 702.100a's "and/or" separable at all, and each entrant is chosen
-- to satisfy exactly one of them:
--
--   * Goblin Piker 2/1 -- power only (2 > 0, and 1 is not > 1).
--   * Llanowar Augur 0/3 -- toughness only (0 is not > 0, and 3 > 1).
--   * Birds of Paradise 0/1 -- neither, which is what makes "greater" strict.
--
-- A test whose entrant beat the Raptor on both axes would pass whichever half
-- were implemented, and would not be a test of the disjunction at all.
--
-- The last two cases are CR 608.2a's re-check read against CR 608.2h, and they
-- are a pair: the same Piker leaves the battlefield before the trigger resolves
-- either way, and the only difference is the numbers the record filed for it --
-- its own 2/1 when damage killed it, 0/0 when a shrink did.
evolveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
evolveSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- CR 122.1: what is on the permanent, which a +1/+1 EFFECT would leave
      -- empty while reading the same size.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- A Raptor under alice, then one creature entering under `pid` with CR
      -- 603.6a's event, so the scan has something to match.
      board raptor printing pid =
        let (raptorId, gs1) = S.addPermanent raptor S.alice (Setup.emptyGame S.bothPlayers)
            (enteringId, gs2) = S.entersWithTrigger printing pid gs1
         in (raptorId, enteringId, gs2)
      evolvesAgainst raptor printing = do
        entrant <- S.printingOf s registry printing
        let (raptorId, _, gs) = board raptor entrant S.alice
        pure (raptorId, resolveAll (settle gs))
   in Spec.describe s "Evolve" $ do
        -- The proving test, and rule 702.100a's POWER half alone: the Piker's 1
        -- toughness does not beat the Raptor's 1, so an implementation that read
        -- only toughness leaves this board untouched.
        Spec.it s "CR 702.100a whole card: a 2/1 entering beats the Raptor's power and evolves it" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          (raptorId, after) <- evolvesAgainst raptor "Goblin Piker"
          Spec.assertEqWith s "one counter" (countersOn raptorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so it is a 1/2" (S.powerToughnessOf raptorId after) (Just (1, 2))
        -- Rule 702.100a's TOUGHNESS half alone, and the mirror of the case above:
        -- the Augur's 0 power does not beat the Raptor's 0, so an implementation
        -- that read only power leaves this board untouched.
        Spec.it s "CR 702.100a a 0/3 entering beats only the toughness, and evolves it all the same" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          (raptorId, after) <- evolvesAgainst raptor "Llanowar Augur"
          Spec.assertEqWith s "one counter" (countersOn raptorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so it is a 1/2" (S.powerToughnessOf raptorId after) (Just (1, 2))
        -- "GREATER" is strict on both axes: a 0/1 entering ties the Raptor twice
        -- and evolves nothing. The falsifier for a comparison written as "at
        -- least".
        Spec.it s "CR 702.100a a 0/1 entering ties both axes and evolves nothing" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          (raptorId, after) <- evolvesAgainst raptor "Birds of Paradise"
          Spec.assertEqWith s "no counters" (countersOn raptorId after) Map.empty
          Spec.assertEqWith s "it is its printed 0/1" (S.powerToughnessOf raptorId after) (Just (0, 1))
        -- "A creature YOU CONTROL": the same Piker that evolves the Raptor from
        -- alice's side does nothing from bob's, and nothing reaches the stack --
        -- CR 603.4 says an ability whose "if" is false does not trigger, but here
        -- it is the CONDITION that rejects the event.
        Spec.it s "CR 702.100a an opponent's creature entering is not a trigger at all" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, _, gs) = board raptor piker S.bob
              settled = settle gs
          Spec.assertEqWith s "nothing on the stack" (GameState.stack settled) []
          Spec.assertEqWith s "and no counters" (countersOn raptorId (resolveAll settled)) Map.empty
        -- CR 608.2a, the case that makes rule 702.100a's "if" an intervening one
        -- rather than part of the event: the trigger is on the stack legitimately,
        -- and a pump on the BEARER in response makes it resolve doing nothing. The
        -- proving test above is the control -- same board, same Piker.
        Spec.it s "CR 608.2a pumping the Raptor in response takes the counter away" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, _, gs) = board raptor piker S.alice
              onStack = settle gs
              responded = S.withEffect raptorId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 2))) onStack
              after = resolveAll responded
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "the Raptor is a 2/3, which the Piker beats on neither axis" (S.powerToughnessOf raptorId responded) (Just (2, 3))
          Spec.assertEqWith s "so no counter on resolution" (countersOn raptorId after) Map.empty
        -- CR 608.2h, the other half of that re-check: the ENTRANT is killed while
        -- the trigger waits, and rule 702.100a's rulings say the comparison is
        -- made against the power and toughness it last had on the battlefield --
        -- not against an object with no characteristics. Only the RESOLUTION check
        -- can observe this: at gather time the entrant has just entered and is
        -- still there by construction, so the read this pins is Stack's alone.
        --
        -- LETHAL DAMAGE rather than a shrink is what kills it, and that is the
        -- whole design of the board: a shrink would change the very numbers under
        -- test, where damage leaves them alone (CR 704.5g destroys the Piker at
        -- the 2/1 the record files). So last known information answers TRUE here
        -- and a blank object answers False, which is what makes the two readings
        -- distinguishable at all.
        Spec.it s "CR 608.2h a Piker killed in response evolves the Raptor from its last known 2/1" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, pikerId, gs) = board raptor piker S.alice
              onStack = settle gs
              dead = settle (S.markDamage pikerId 1 onStack)
              after = resolveAll dead
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "and the Piker is gone before it resolves" (S.onBattlefield pikerId dead) False
          Spec.assertEqWith s "the counter goes on all the same" (countersOn raptorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so it is a 1/2" (S.powerToughnessOf raptorId after) (Just (1, 2))
        -- The case above's paired negative -- same Raptor, same Piker, same
        -- departure before resolution, and the single difference is HOW it left:
        -- a -2/-1 kills it at 0/0 (CR 704.5f) instead, and 0/0 beats the Raptor's
        -- 0/1 on neither axis. So the numbers the record filed are what the
        -- re-check reads, rather than the entrant's PRINTED 2/1 -- which would put
        -- the counter on -- and rather than a departed entrant being waved through
        -- unexamined.
        Spec.it s "CR 608.2h an entrant shrunk to 0/0 as it died evolves nothing" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, pikerId, gs) = board raptor piker S.alice
              onStack = settle gs
              shrunk = S.withEffect pikerId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal (-2)) (Quantity.Type.Literal (-1)))) onStack
              dead = settle shrunk
              after = resolveAll dead
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "the Piker left as a 0/0" (S.powerToughnessOf pikerId shrunk) (Just (0, 0))
          Spec.assertEqWith s "and is gone before the trigger resolves" (S.onBattlefield pikerId dead) False
          Spec.assertEqWith s "no counters" (countersOn raptorId after) Map.empty
          Spec.assertEqWith s "the Raptor is its printed 0/1" (S.powerToughnessOf raptorId after) (Just (0, 1))
        -- CR 702.100d: each instance triggers separately, asserted of the MINT as
        -- prowess' and training's are, no printing carrying evolve twice.
        Spec.it s "CR 702.100d two instances mint two abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Evolve 2
              expected =
                TriggerCondition.PermanentEnters
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each watching CR 603.6a's entry"
            (fmap TriggeredAbility.condition abilities)
            [expected, expected]

-- CR 702.100b: a creature "evolves" when one or more +1/+1 counters are put on it
-- as a result of its evolve ability RESOLVING -- the marker rule 702.100b makes
-- other abilities able to identify. Renegade Krasis {1}{G}{G} 3/2 is the card:
-- evolve, plus "whenever this creature evolves, put a +1/+1 counter on each other
-- creature you control with a +1/+1 counter on it".
--
-- Four permanents, each pinning one conjunct of that sentence:
--
--   * the Krasis itself -- "each OTHER", so its own count must stay at the one
--     its evolve put there.
--   * alice's Goblin Piker and Hill Giant, each seeded with a counter -- two
--     recipients, so "EACH other creature" is more than one object.
--   * alice's Birds of Paradise, with none -- "with a +1/+1 counter on it".
--   * bob's Piker, seeded with one -- "you control".
--
-- The ENTRANT is Llanowar Augur 0/3: it beats the Krasis' 2 toughness and nothing
-- else, so the Krasis evolves. Goblin Piker 2/1 is the entrant that does not,
-- which the self-scope case below turns on.
krasisSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
krasisSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      seeded printing pid gs =
        let (oid, g1) = S.addPermanent printing pid gs
         in (oid, S.addCounter CounterKind.PlusOnePlusOne 1 oid g1)
      boardOn base = do
        krasisPrinting <- S.printingOf s registry "Renegade Krasis"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        birdsPrinting <- S.printingOf s registry "Birds of Paradise"
        giantPrinting <- S.printingOf s registry "Hill Giant"
        let (krasis, g1) = S.addPermanent krasisPrinting S.alice base
            (mine, g2) = seeded pikerPrinting S.alice g1
            (giant, g3) = seeded giantPrinting S.alice g2
            (birds, g4) = S.addPermanent birdsPrinting S.alice g3
            (theirs, g5) = seeded pikerPrinting S.bob g4
        pure (krasis, mine, giant, birds, theirs, g5)
      board = boardOn (Setup.emptyGame S.bothPlayers)
   in Spec.describe s "Renegade Krasis" $ do
        -- The proving test.
        Spec.it s "CR 702.100b whole card: the Krasis evolves and pays out its other counter-bearers" $ do
          augur <- S.printingOf s registry "Llanowar Augur"
          (krasis, mine, giant, birds, theirs, gs) <- board
          let (_, entered) = S.entersWithTrigger augur S.alice gs
              after = resolveAll (settle entered)
          Spec.assertEqWith s "the Krasis keeps only its evolve counter" (plusOnes krasis after) 1
          Spec.assertEqWith s "alice's Piker gains a second" (plusOnes mine after) 2
          Spec.assertEqWith s "and so does her Giant -- EACH other creature" (plusOnes giant after) 2
          Spec.assertEqWith s "the counterless Bird gains none" (plusOnes birds after) 0
          Spec.assertEqWith s "bob's counter-bearer gains none" (plusOnes theirs after) 1
        -- Self-scoped, not filtered: a Cloudfin Raptor evolving beside the Krasis
        -- is another creature alice controls evolving, and the Krasis' ability
        -- says "this creature". The Piker 2/1 beats the Raptor's 0/1 power and
        -- neither of the Krasis' numbers, so exactly one of the two evolves.
        Spec.it s "CR 702.100b another creature evolving is not this creature evolving" $ do
          raptorPrinting <- S.printingOf s registry "Cloudfin Raptor"
          pikerPrinting <- S.printingOf s registry "Goblin Piker"
          (krasis, mine, _, _, _, gs) <- board
          let (raptor, withRaptor) = S.addPermanent raptorPrinting S.alice gs
              (_, entered) = S.entersWithTrigger pikerPrinting S.alice withRaptor
              after = resolveAll (settle entered)
          Spec.assertEqWith s "the Raptor did evolve" (plusOnes raptor after) 1
          Spec.assertEqWith s "the Krasis did not" (plusOnes krasis after) 0
          Spec.assertEqWith s "so its trigger paid out nothing" (plusOnes mine after) 1
        -- CR 702.100b's "as a result of its evolve ability resolving": the same
        -- counter, on the same permanent, from Battlegrowth instead, is not an
        -- evolution. The falsifier for a condition written against
        -- GameEvent.CountersPut.
        Spec.it s "CR 702.100b a +1/+1 counter from anything else is not an evolution" $ do
          forest <- S.printingOf s registry "Forest"
          battlegrowth <- S.printingOf s registry "Battlegrowth"
          (krasis, mine, _, _, _, gs) <- boardOn (S.landsInPlay forest 1)
          let (handed, spellId) = S.handOne battlegrowth gs
              cast = snd (Engine.runGamePure (aimedCast spellId krasis) handed (S.cast S.alice spellId))
              after = resolveAll (settle cast)
          Spec.assertEqWith s "the Krasis took the counter" (plusOnes krasis after) 1
          Spec.assertEqWith s "and nothing evolved, so nothing was paid out" (plusOnes mine after) 1

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  frenzySpec s registry
  exaltedSpec s registry
  mentorSpec s registry
  mentorsTriggerSpec s registry
  trainingSpec s registry
  saviorOfOllenbockSpec s registry
  decayedSpec s registry
  provokeSpec s registry
  trygonPredatorSpec s registry
  questingBeastSpec s registry
  larcenySpec s registry
  evolveSpec s registry
  krasisSpec s registry
