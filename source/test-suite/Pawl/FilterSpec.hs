-- Covers Pawl.Types.Filter, Pawl.Types.PlayerRelation, Pawl.Engine.Filter.
module Pawl.FilterSpec where

import qualified Data.Set as Set
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
-- Aliased Filter.Type, not Type, because the evaluator module Pawl.Engine.Filter
-- already claims the alias Filter (a documented exception to alias-to-last-
-- component, per the M4.5 P9 plan's global constraints).
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Subtype as Subtype
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- A projected black creature controlled by player 0.
blackCreature :: Filter.View
blackCreature =
  Filter.MkView
    { Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.singleton Color.Black,
      Filter.subtypes = Set.singleton Subtype.Zombie,
      Filter.power = Just 2,
      Filter.controller = Just (PlayerId.MkPlayerId 0),
      Filter.identity = Just (ObjectId.MkObjectId 7),
      Filter.playerIdentity = Nothing,
      Filter.attacking = False,
      Filter.blocking = False,
      Filter.attackedThisTurn = False,
      Filter.attachedToCreature = False,
      Filter.attachedToPermanent = False,
      Filter.canHostSubject = False,
      Filter.token = False
    }

-- A colourless (devoid) creature with power 5, no controller recorded.
devoidBigCreature :: Filter.View
devoidBigCreature =
  Filter.MkView
    { Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.empty,
      Filter.subtypes = Set.empty,
      Filter.power = Just 5,
      Filter.controller = Nothing,
      Filter.identity = Nothing,
      Filter.playerIdentity = Nothing,
      Filter.attacking = False,
      Filter.blocking = False,
      Filter.attackedThisTurn = False,
      Filter.attachedToCreature = False,
      Filter.attachedToPermanent = False,
      Filter.canHostSubject = False,
      Filter.token = False
    }

self :: Filter.Context
self = Filter.MkContext (Just (PlayerId.MkPlayerId 0)) Nothing

other :: Filter.Context
other = Filter.MkContext (Just (PlayerId.MkPlayerId 1)) Nothing

noPerspective :: Filter.Context
noPerspective = Filter.MkContext Nothing Nothing

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Engine.Filter"
    [ HU.testCase "HasCardType matches when present" $
        HU.assertBool "creature" (Filter.matches self blackCreature (Filter.Type.HasCardType CardType.Creature)),
      HU.testCase "HasCardType fails when absent" $
        HU.assertBool "not land" (not (Filter.matches self blackCreature (Filter.Type.HasCardType CardType.Land))),
      HU.testCase "HasColor matches Black creature" $
        HU.assertBool "black" (Filter.matches self blackCreature (Filter.Type.HasColor Color.Black)),
      HU.testCase "Not HasColor Black is Doom Blade's narrowing" $ do
        HU.assertBool "black is illegal" (not (Filter.matches self blackCreature (Filter.Type.Not (Filter.Type.HasColor Color.Black))))
        HU.assertBool "devoid is legal" (Filter.matches self devoidBigCreature (Filter.Type.Not (Filter.Type.HasColor Color.Black))),
      HU.testCase "And [] is the trivial predicate (matches everything)" $
        HU.assertBool "trivial" (Filter.matches self blackCreature (Filter.Type.And [])),
      HU.testCase "Terror: And of two negated atoms" $ do
        let terror = Filter.Type.And [Filter.Type.Not (Filter.Type.HasColor Color.Black), Filter.Type.Not (Filter.Type.HasCardType CardType.Artifact)]
        HU.assertBool "black creature fails" (not (Filter.matches self blackCreature terror))
        HU.assertBool "devoid creature passes" (Filter.matches self devoidBigCreature terror),
      HU.testCase "Or matches when either arm matches" $
        HU.assertBool "creature or enchantment" (Filter.matches self blackCreature (Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasCardType CardType.Enchantment])),
      HU.testCase "PowerAtLeast compares projected power" $ do
        HU.assertBool "power 2 < 4" (not (Filter.matches self blackCreature (Filter.Type.PowerAtLeast 4)))
        HU.assertBool "power 5 >= 4" (Filter.matches self devoidBigCreature (Filter.Type.PowerAtLeast 4)),
      HU.testCase "PowerAtLeast is False when power is Nothing" $ do
        let noPower = blackCreature {Filter.power = Nothing}
        HU.assertBool "no power" (not (Filter.matches self noPower (Filter.Type.PowerAtLeast 1))),
      HU.testCase "ControlledBy You holds for own object" $
        HU.assertBool "you" (Filter.matches self blackCreature (Filter.Type.ControlledBy PlayerRelation.You)),
      HU.testCase "ControlledBy You fails from an opponent's perspective" $
        HU.assertBool "not you" (not (Filter.matches other blackCreature (Filter.Type.ControlledBy PlayerRelation.You))),
      HU.testCase "ControlledBy Opponent holds across differing players" $
        HU.assertBool "opponent" (Filter.matches other blackCreature (Filter.Type.ControlledBy PlayerRelation.Opponent)),
      HU.testCase "ControlledBy is False when the object has no controller" $
        HU.assertBool "no controller" (not (Filter.matches self devoidBigCreature (Filter.Type.ControlledBy PlayerRelation.Opponent))),
      HU.testCase "ControlledBy is False when the context has no perspective" $
        HU.assertBool "no perspective" (not (Filter.matches noPerspective blackCreature (Filter.Type.ControlledBy PlayerRelation.You))),
      Tasty.testGroup
        "IsSource"
        [ HU.testCase "matches the context's source"
            . HU.assertBool "is the source"
            $ Filter.matches
              (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7)))
              blackCreature
              Filter.Type.IsSource,
          HU.testCase "does not match a different object"
            . HU.assertBool "not the source"
            . not
            $ Filter.matches
              (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 8)))
              blackCreature
              Filter.Type.IsSource,
          HU.testCase "no source in context is vacuously false"
            . HU.assertBool "no source"
            . not
            $ Filter.matches
              (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) Nothing)
              blackCreature
              Filter.Type.IsSource,
          HU.testCase "no identity in view is vacuously false"
            . HU.assertBool "no identity"
            . not
            $ Filter.matches
              (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7)))
              devoidBigCreature
              Filter.Type.IsSource
        ],
      Tasty.testGroup
        "IsAttacking"
        [ HU.testCase "matches a view whose combat status says so"
            . HU.assertBool "attacking"
            $ Filter.matches self (blackCreature {Filter.attacking = True}) Filter.Type.IsAttacking,
          HU.testCase "does not match a creature that is not attacking"
            . HU.assertBool "not attacking"
            . not
            $ Filter.matches self blackCreature Filter.Type.IsAttacking,
          -- CR 109.3: combat status is not a characteristic, so no other axis of
          -- the view can stand in for it. A 5-power creature is no more attacking
          -- than a 2-power one.
          HU.testCase "is independent of every characteristic axis"
            . HU.assertBool "power does not imply attacking"
            . not
            $ Filter.matches self devoidBigCreature Filter.Type.IsAttacking,
          -- CR 506.3: only a creature can attack, and a player is not one.
          HU.testCase "a player candidate is vacuously false"
            . HU.assertBool "player"
            . not
            $ Filter.matches self (Filter.playerView (PlayerId.MkPlayerId 0)) Filter.Type.IsAttacking
        ],
      Tasty.testGroup
        "IsBlocking"
        [ HU.testCase "matches a view whose combat status says so"
            . HU.assertBool "blocking"
            $ Filter.matches self (blackCreature {Filter.blocking = True}) Filter.Type.IsBlocking,
          HU.testCase "does not match a creature that is not blocking"
            . HU.assertBool "not blocking"
            . not
            $ Filter.matches self blackCreature Filter.Type.IsBlocking,
          -- The two combat roles are independent in BOTH directions, which is
          -- why Labyrinth of Skophos' "attacking or blocking" needs two atoms
          -- rather than one: CR 508.1k confers the first at the attacker
          -- declaration and CR 509.1g the second at the blocker declaration,
          -- and neither says anything about the other.
          HU.testCase "is not implied by attacking"
            . HU.assertBool "attacking does not imply blocking"
            . not
            $ Filter.matches self (blackCreature {Filter.attacking = True}) Filter.Type.IsBlocking,
          HU.testCase "does not imply attacking"
            . HU.assertBool "blocking does not imply attacking"
            . not
            $ Filter.matches self (blackCreature {Filter.blocking = True}) Filter.Type.IsAttacking,
          -- Labyrinth of Skophos' own filter, over each role in turn.
          HU.testCase "Or [IsAttacking, IsBlocking] admits either role and rejects a creature in neither" $ do
            let both = Filter.Type.Or [Filter.Type.IsAttacking, Filter.Type.IsBlocking]
            HU.assertBool "attacker" (Filter.matches self (blackCreature {Filter.attacking = True}) both)
            HU.assertBool "blocker" (Filter.matches self (blackCreature {Filter.blocking = True}) both)
            HU.assertBool "neither" (not (Filter.matches self blackCreature both)),
          -- CR 509.1a: only a creature can be chosen to block, and a player is
          -- not one.
          HU.testCase "a player candidate is vacuously false"
            . HU.assertBool "player"
            . not
            $ Filter.matches self (Filter.playerView (PlayerId.MkPlayerId 0)) Filter.Type.IsBlocking
        ],
      Tasty.testGroup
        "rewrite"
        -- CR 612.1: a text-changing effect applies to "any words or symbols
        -- printed on that object", and HasSubtype is the only atom that can
        -- carry a basic land type. Threaded into effects by Resolve.rewriteEffect.
        [ HU.testCase "swaps the named subtype word" $
            HU.assertEqual
              "Island became Forest"
              (Filter.Type.HasSubtype Subtype.Forest)
              (Filter.rewrite [(Subtype.Island, Subtype.Forest)] (Filter.Type.HasSubtype Subtype.Island)),
          HU.testCase "leaves an unnamed subtype word alone" $
            HU.assertEqual
              "Wall untouched"
              (Filter.Type.HasSubtype Subtype.Wall)
              (Filter.rewrite [(Subtype.Island, Subtype.Forest)] (Filter.Type.HasSubtype Subtype.Wall)),
          HU.testCase "recurses through And, Or and Not" $
            let before = Filter.Type.And [Filter.Type.Not (Filter.Type.HasSubtype Subtype.Island), Filter.Type.Or [Filter.Type.HasSubtype Subtype.Island, Filter.Type.IsAttacking]]
                after = Filter.Type.And [Filter.Type.Not (Filter.Type.HasSubtype Subtype.Forest), Filter.Type.Or [Filter.Type.HasSubtype Subtype.Forest, Filter.Type.IsAttacking]]
             in HU.assertEqual "every occurrence" after (Filter.rewrite [(Subtype.Island, Subtype.Forest)] before),
          -- CR 612.1 changes WORDS, and a card type is not a subtype word.
          HU.testCase "leaves an atom that names no subtype alone" $
            HU.assertEqual
              "card type untouched"
              (Filter.Type.HasCardType CardType.Creature)
              (Filter.rewrite [(Subtype.Island, Subtype.Forest)] (Filter.Type.HasCardType CardType.Creature))
        ],
      Tasty.testGroup
        "AttackedThisTurn"
        [ HU.testCase "matches a view whose history says so"
            . HU.assertBool "attacked"
            $ Filter.matches self (blackCreature {Filter.attackedThisTurn = True}) Filter.Type.AttackedThisTurn,
          HU.testCase "does not match a creature that never attacked"
            . HU.assertBool "did not attack"
            . not
            $ Filter.matches self blackCreature Filter.Type.AttackedThisTurn,
          -- The two axes are independent in BOTH directions, which is the whole
          -- reason this atom exists. A creature attacking right now may not have
          -- been declared this turn (CR 508.4 puts one onto the battlefield
          -- attacking without it ever having attacked), and one that attacked
          -- earlier this turn is no longer attacking once CR 511.3 has removed it
          -- from combat -- which is Relentless Assault's whole case.
          HU.testCase "is not implied by attacking right now"
            . HU.assertBool "attacking does not imply attacked"
            . not
            $ Filter.matches self (blackCreature {Filter.attacking = True}) Filter.Type.AttackedThisTurn,
          HU.testCase "does not imply attacking right now"
            . HU.assertBool "attacked does not imply attacking"
            . not
            $ Filter.matches self (blackCreature {Filter.attackedThisTurn = True}) Filter.Type.IsAttacking,
          -- CR 506.3: only a creature can be declared as an attacker, and a
          -- player is not one.
          HU.testCase "a player candidate is vacuously false"
            . HU.assertBool "player"
            . not
            $ Filter.matches self (Filter.playerView (PlayerId.MkPlayerId 0)) Filter.Type.AttackedThisTurn
        ],
      Tasty.testGroup
        "IsAttachedToCreature"
        [ HU.testCase "matches a view whose attachment says so"
            . HU.assertBool "attached to a creature"
            $ Filter.matches self (blackCreature {Filter.attachedToCreature = True}) Filter.Type.IsAttachedToCreature,
          HU.testCase "does not match a permanent attached to nothing"
            . HU.assertBool "unattached"
            . not
            $ Filter.matches self blackCreature Filter.Type.IsAttachedToCreature,
          -- CR 109.3 names "what an Aura enchants" among the things that are not
          -- characteristics, so no characteristic axis can stand in for it: being
          -- an Aura by subtype says nothing about whether it is on a creature.
          HU.testCase "is independent of every characteristic axis"
            . HU.assertBool "subtype does not imply attachment"
            . not
            $ Filter.matches self (blackCreature {Filter.subtypes = Set.singleton Subtype.Aura}) Filter.Type.IsAttachedToCreature,
          -- CR 303.4b: a player is enchanted BY an Aura, never attached to
          -- anything -- Object.attachedTo is a field of the attached permanent,
          -- and a player is not one.
          HU.testCase "a player candidate is vacuously false"
            . HU.assertBool "player"
            . not
            $ Filter.matches self (Filter.playerView (PlayerId.MkPlayerId 0)) Filter.Type.IsAttachedToCreature
        ],
      Tasty.testGroup
        "IsAttachedToPermanent"
        [ HU.testCase "matches a view whose attachment says so"
            . HU.assertBool "attached to a permanent"
            $ Filter.matches self (blackCreature {Filter.attachedToPermanent = True}) Filter.Type.IsAttachedToPermanent,
          HU.testCase "does not match a permanent attached to nothing"
            . HU.assertBool "unattached"
            . not
            $ Filter.matches self blackCreature Filter.Type.IsAttachedToPermanent,
          -- The pair that makes this a separate atom rather than a synonym: CR
          -- 303.4 attaches an Aura to "an object or player", so being attached to a
          -- permanent is strictly wider than being attached to a creature and the
          -- implication runs one way only. Pawl.Engine.Projection fills both fields off the
          -- same Object.attachedTo, so the views a real board produces never carry
          -- the impossible combination -- but the matcher folds whatever it is
          -- given, and each atom must read its own field.
          HU.testCase "is a wider question than IsAttachedToCreature" $ do
            let onLand = blackCreature {Filter.attachedToPermanent = True}
            HU.assertBool "on a land: attached to a permanent" (Filter.matches self onLand Filter.Type.IsAttachedToPermanent)
            HU.assertBool "but not to a creature" (not (Filter.matches self onLand Filter.Type.IsAttachedToCreature)),
          -- CR 303.4b: a player is enchanted BY an Aura and is not itself attached
          -- to anything, which is the case this atom exists to exclude -- Curse of
          -- Death's Hold is attached to a player, so it is not a legal target for
          -- Aura Graft.
          HU.testCase "a player candidate is vacuously false"
            . HU.assertBool "player"
            . not
            $ Filter.matches self (Filter.playerView (PlayerId.MkPlayerId 0)) Filter.Type.IsAttachedToPermanent
        ],
      Tasty.testGroup
        "CanHostSubject"
        [ HU.testCase "matches a view the caller marked as a legal destination"
            . HU.assertBool "can host"
            $ Filter.matches self (blackCreature {Filter.canHostSubject = True}) Filter.Type.CanHostSubject,
          -- CR 701.3a asks about the SUBJECT, so no fact about the candidate can
          -- settle it: a creature is exactly what an Aura usually enchants and
          -- still answers False until the caller that knows what is moving says
          -- otherwise.
          HU.testCase "is independent of every characteristic axis"
            . HU.assertBool "being a creature does not make it a legal host"
            . not
            $ Filter.matches self blackCreature Filter.Type.CanHostSubject,
          -- Vacuously False wherever no attach frames the match, which is every
          -- view but the ones Pawl.Engine.Resolve's AttachTarget arm builds.
          HU.testCase "a player candidate is vacuously false"
            . HU.assertBool "player"
            . not
            $ Filter.matches self (Filter.playerView (PlayerId.MkPlayerId 0)) Filter.Type.CanHostSubject
        ],
      Tasty.testGroup
        "IsToken"
        [ HU.testCase "matches a view whose object is a token"
            . HU.assertBool "token"
            $ Filter.matches self (blackCreature {Filter.token = True}) Filter.Type.IsToken,
          -- Ashaya's "nontoken creatures you control" is spelled `Not IsToken`, the
          -- way CR 601.2c's "another" is spelled `Not IsSource` (#163).
          HU.testCase "Not IsToken is how 'nontoken' is written" $ do
            HU.assertBool "a card permanent is nontoken" (Filter.matches self blackCreature (Filter.Type.Not Filter.Type.IsToken))
            HU.assertBool "a token is not" (not (Filter.matches self (blackCreature {Filter.token = True}) (Filter.Type.Not Filter.Type.IsToken))),
          -- CR 111.3: a token's characteristics are effect-defined and are
          -- "functionally equivalent" to printed ones, so no characteristic axis
          -- distinguishes a token from the card it copies.
          HU.testCase "is independent of every characteristic axis"
            . HU.assertBool "power does not imply token"
            . not
            $ Filter.matches self devoidBigCreature Filter.Type.IsToken,
          -- CR 111.1: a token is a marker used to represent a PERMANENT; a player
          -- is not one.
          HU.testCase "a player candidate is vacuously false"
            . HU.assertBool "player"
            . not
            $ Filter.matches self (Filter.playerView (PlayerId.MkPlayerId 0)) Filter.Type.IsToken
        ]
    ]
