-- Oracle text a card file can be checked against, for the cards whose whole
-- text is keywords (#9). A card ingested from MTGJSON keeps its Oracle text in
-- a sidecar under data/oracle/, and Pawl.OracleSpec asserts that 'render' of
-- the card file says what the sidecar says, modulo 'normalise'.
--
-- That check sees what a card SAYS, never what it DOES: an ingested flier is
-- right only because Pawl.Engine.Keyword's flying is (docs/design.md section 4).
module Pawl.Oracle where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TypeLine as TypeLine

-- | The rules text of a single-faced card whose face carries nothing but its
-- printed characteristics (a color indicator among them, CR 204) and keywords
-- printed alone, one keyword per line; 'Nothing' for any other card. A card
-- with no keywords renders as no text, which is what a vanilla creature and a
-- basic land print (CR 305.6, CR 207.2a).
render :: Card.Card -> Maybe Text.Text
render card = case Card.faces card of
  face NonEmpty.:| []
    | Card.layout card == Layout.Normal,
      face == bare (Face.name face) (Face.manaCost face) (Face.colorIndicator face) (Face.typeLine face) (Face.power face) (Face.toughness face) (Face.keywords face) ->
        fmap (Text.intercalate (Text.pack "\n") . concat) . traverse (\(k, n) -> fmap (List.genericReplicate n) (printed k)) $ Map.toAscList (Face.keywords face)
  _ -> Nothing

-- | A face with the given characteristics and keywords and nothing else. Every
-- field is spelled out, so a field added to 'Face.Face' fails to compile here
-- rather than being silently ignored by 'render'.
bare ::
  CardName.CardName ->
  Maybe ManaCost.ManaCost ->
  Set.Set Color.Color ->
  TypeLine.TypeLine ->
  Maybe Power.Power ->
  Maybe Toughness.Toughness ->
  Map.Map Keyword.Keyword Natural.Natural ->
  Face.Face Card.Card
bare n c i t p x k =
  Face.MkFace
    { Face.name = n,
      Face.manaCost = c,
      Face.typeLine = t,
      Face.power = p,
      Face.toughness = x,
      Face.loyalty = Nothing,
      Face.defense = Nothing,
      Face.startingIntensity = Nothing,
      Face.vanguard = Nothing,
      Face.canBeYourCommander = False,
      Face.claimsStartingPlayer = False,
      Face.keywords = k,
      Face.colorIndicator = i,
      Face.characteristicPT = Nothing,
      Face.staticAbilities = [],
      Face.spell = Face.defaultSpell,
      Face.activatedAbilities = [],
      Face.replacementEffects = [],
      Face.triggeredAbilities = [],
      Face.delayedAbilities = Map.empty,
      Face.rooms = Seq.empty,
      Face.dungeonEntryQuality = Nothing,
      Face.castingPermissions = [],
      Face.castingRestrictions = [],
      Face.enchant = [],
      Face.counterability = Counterability.Counterable,
      Face.additionalCosts = [],
      Face.additionalCostChoices = [],
      Face.modeCosts = Map.empty,
      Face.maximumX = [],
      Face.minimumX = 0,
      Face.alternativeCosts = [],
      Face.costReductions = [],
      Face.playerAbilities = [],
      Face.blockRequirements = [],
      Face.blockPermissions = [],
      Face.attackRequirements = [],
      Face.combatRestrictions = [],
      Face.sacrificeRestrictions = [],
      Face.untapRestrictions = [],
      Face.crewRestrictions = [],
      Face.attackPermissions = [],
      Face.attachRestrictions = [],
      Face.entryRestrictions = [],
      Face.counterRestrictions = [],
      Face.activationProhibitions = [],
      Face.attackCosts = [],
      Face.blockCosts = [],
      Face.mulliganActions = [],
      Face.openingHandActions = [],
      Face.specialActions = []
    }

-- | How Oracle text prints a keyword standing alone (CR 702), or 'Nothing' for
-- one that carries a parameter. No wildcard arm: a new keyword fails to compile
-- here until it is given its printed form.
printed :: Keyword.Keyword -> Maybe Text.Text
printed keyword = case keyword of
  Keyword.Deathtouch -> Just (Text.pack "Deathtouch")
  Keyword.Defender -> Just (Text.pack "Defender")
  Keyword.DoubleStrike -> Just (Text.pack "Double strike")
  Keyword.Equip {} -> Nothing
  Keyword.EquipPlaneswalker {} -> Nothing
  Keyword.FirstStrike -> Just (Text.pack "First strike")
  Keyword.Flash -> Just (Text.pack "Flash")
  Keyword.Flying -> Just (Text.pack "Flying")
  Keyword.Haste -> Just (Text.pack "Haste")
  Keyword.Hexproof Nothing -> Just (Text.pack "Hexproof")
  Keyword.Hexproof (Just _) -> Nothing
  Keyword.Indestructible -> Just (Text.pack "Indestructible")
  Keyword.Intimidate -> Just (Text.pack "Intimidate")
  Keyword.Landwalk {} -> Nothing
  Keyword.Lifelink -> Just (Text.pack "Lifelink")
  Keyword.LivingMetal -> Just (Text.pack "Living metal")
  Keyword.MoreThanMeetsTheEye {} -> Nothing
  Keyword.Protection {} -> Nothing
  Keyword.Reach -> Just (Text.pack "Reach")
  Keyword.Shroud -> Just (Text.pack "Shroud")
  Keyword.Trample -> Just (Text.pack "Trample")
  Keyword.TrampleOverPlaneswalkers -> Just (Text.pack "Trample over planeswalkers")
  Keyword.Vigilance -> Just (Text.pack "Vigilance")
  Keyword.Ward {} -> Nothing
  Keyword.Banding -> Just (Text.pack "Banding")
  Keyword.Rampage {} -> Nothing
  Keyword.CumulativeUpkeep {} -> Nothing
  Keyword.Flanking -> Just (Text.pack "Flanking")
  Keyword.Phasing -> Just (Text.pack "Phasing")
  Keyword.Buyback {} -> Nothing
  Keyword.Shadow -> Just (Text.pack "Shadow")
  Keyword.Cycling {} -> Nothing
  Keyword.Echo {} -> Nothing
  Keyword.Horsemanship -> Just (Text.pack "Horsemanship")
  Keyword.Fading {} -> Nothing
  Keyword.Kicker {} -> Nothing
  Keyword.Multikicker {} -> Nothing
  Keyword.Flashback {} -> Nothing
  Keyword.Fear -> Just (Text.pack "Fear")
  Keyword.Morph {} -> Nothing
  Keyword.Amplify {} -> Nothing
  Keyword.Provoke -> Just (Text.pack "Provoke")
  Keyword.Storm -> Just (Text.pack "Storm")
  Keyword.Affinity {} -> Nothing
  Keyword.Entwine {} -> Nothing
  Keyword.Modular {} -> Nothing
  Keyword.Sunburst -> Just (Text.pack "Sunburst")
  Keyword.Bushido {} -> Nothing
  Keyword.Soulshift {} -> Nothing
  Keyword.Splice {} -> Nothing
  Keyword.Offering {} -> Nothing
  Keyword.Ninjutsu {} -> Nothing
  Keyword.Epic -> Just (Text.pack "Epic")
  Keyword.Paradigm -> Just (Text.pack "Paradigm")
  Keyword.Convoke -> Just (Text.pack "Convoke")
  Keyword.Dredge {} -> Nothing
  Keyword.Bloodthirst {} -> Nothing
  Keyword.Haunt -> Just (Text.pack "Haunt")
  Keyword.Replicate {} -> Nothing
  Keyword.Graft {} -> Nothing
  Keyword.Recover {} -> Nothing
  Keyword.Ripple {} -> Nothing
  Keyword.SplitSecond -> Just (Text.pack "Split second")
  Keyword.Suspend {} -> Nothing
  Keyword.Vanishing Nothing -> Just (Text.pack "Vanishing")
  Keyword.Vanishing (Just _) -> Nothing
  Keyword.Absorb {} -> Nothing
  Keyword.AuraSwap {} -> Nothing
  Keyword.Delve -> Just (Text.pack "Delve")
  Keyword.Fortify {} -> Nothing
  Keyword.Frenzy {} -> Nothing
  Keyword.Gravestorm -> Just (Text.pack "Gravestorm")
  Keyword.Poisonous {} -> Nothing
  Keyword.Champion {} -> Nothing
  Keyword.Changeling -> Just (Text.pack "Changeling")
  Keyword.Evoke {} -> Nothing
  Keyword.Hideaway {} -> Nothing
  Keyword.Prowl {} -> Nothing
  Keyword.Reinforce {} -> Nothing
  Keyword.Conspire -> Just (Text.pack "Conspire")
  Keyword.Persist -> Just (Text.pack "Persist")
  Keyword.Wither -> Just (Text.pack "Wither")
  Keyword.Devour {} -> Nothing
  Keyword.Exalted -> Just (Text.pack "Exalted")
  Keyword.Unearth {} -> Nothing
  Keyword.Cascade -> Just (Text.pack "Cascade")
  Keyword.Annihilator {} -> Nothing
  Keyword.LevelUp {} -> Nothing
  Keyword.Infect -> Just (Text.pack "Infect")
  Keyword.BattleCry -> Just (Text.pack "Battle cry")
  Keyword.LivingWeapon -> Just (Text.pack "Living weapon")
  Keyword.Undying -> Just (Text.pack "Undying")
  Keyword.Miracle {} -> Nothing
  Keyword.Soulbond -> Just (Text.pack "Soulbond")
  Keyword.Overload {} -> Nothing
  Keyword.Unleash -> Just (Text.pack "Unleash")
  Keyword.Cipher -> Just (Text.pack "Cipher")
  Keyword.Evolve -> Just (Text.pack "Evolve")
  Keyword.Extort -> Just (Text.pack "Extort")
  Keyword.Fuse -> Just (Text.pack "Fuse")
  Keyword.Tribute {} -> Nothing
  Keyword.Dethrone -> Just (Text.pack "Dethrone")
  Keyword.Outlast {} -> Nothing
  Keyword.Prowess -> Just (Text.pack "Prowess")
  Keyword.Dash {} -> Nothing
  Keyword.Exploit -> Just (Text.pack "Exploit")
  Keyword.Menace -> Just (Text.pack "Menace")
  Keyword.Renown {} -> Nothing
  Keyword.Awaken {} -> Nothing
  Keyword.Devoid -> Just (Text.pack "Devoid")
  Keyword.Ingest -> Just (Text.pack "Ingest")
  Keyword.Myriad -> Just (Text.pack "Myriad")
  Keyword.Surge {} -> Nothing
  Keyword.Skulk -> Just (Text.pack "Skulk")
  Keyword.Emerge {} -> Nothing
  Keyword.Escalate {} -> Nothing
  Keyword.Melee -> Just (Text.pack "Melee")
  Keyword.Crew {} -> Nothing
  Keyword.Fabricate {} -> Nothing
  Keyword.Partner -> Just (Text.pack "Partner")
  Keyword.PartnerText {} -> Nothing
  Keyword.PartnerWith {} -> Nothing
  Keyword.ChooseABackground -> Just (Text.pack "Choose a Background")
  Keyword.DoctorsCompanion -> Just (Text.pack "Doctor's companion")
  Keyword.Undaunted -> Just (Text.pack "Undaunted")
  Keyword.Improvise -> Just (Text.pack "Improvise")
  Keyword.Aftermath -> Just (Text.pack "Aftermath")
  Keyword.Embalm {} -> Nothing
  Keyword.Eternalize {} -> Nothing
  Keyword.Afflict {} -> Nothing
  Keyword.Ascend -> Just (Text.pack "Ascend")
  Keyword.Assist -> Just (Text.pack "Assist")
  Keyword.JumpStart -> Just (Text.pack "Jump-start")
  Keyword.Mentor -> Just (Text.pack "Mentor")
  Keyword.Afterlife {} -> Nothing
  Keyword.Riot -> Just (Text.pack "Riot")
  Keyword.Spectacle {} -> Nothing
  Keyword.Escape {} -> Nothing
  Keyword.Companion {} -> Nothing
  Keyword.Foretell {} -> Nothing
  Keyword.Demonstrate -> Just (Text.pack "Demonstrate")
  Keyword.Daybound -> Just (Text.pack "Daybound")
  Keyword.Nightbound -> Just (Text.pack "Nightbound")
  Keyword.Decayed -> Just (Text.pack "Decayed")
  Keyword.Cleave {} -> Nothing
  Keyword.Training -> Just (Text.pack "Training")
  Keyword.Compleated -> Just (Text.pack "Compleated")
  Keyword.Reconfigure {} -> Nothing
  Keyword.Blitz {} -> Nothing
  Keyword.Casualty {} -> Nothing
  Keyword.ReadAhead -> Just (Text.pack "Read ahead")
  Keyword.Ravenous -> Just (Text.pack "Ravenous")
  Keyword.Squad {} -> Nothing
  Keyword.Prototype {} -> Nothing
  Keyword.ForMirrodin -> Just (Text.pack "For Mirrodin!")
  Keyword.Toxic {} -> Nothing
  Keyword.Backup {} -> Nothing
  Keyword.Bargain -> Just (Text.pack "Bargain")
  Keyword.Craft {} -> Nothing
  Keyword.Disguise {} -> Nothing
  Keyword.Plot {} -> Nothing
  Keyword.Saddle {} -> Nothing
  Keyword.Spree -> Just (Text.pack "Spree")
  Keyword.Offspring {} -> Nothing
  Keyword.Gift {} -> Nothing
  Keyword.Freerunning {} -> Nothing
  Keyword.Impending {} -> Nothing
  Keyword.Exhaust -> Just (Text.pack "Exhaust")
  Keyword.Boast -> Just (Text.pack "Boast")
  Keyword.PowerUp -> Just (Text.pack "Power-up")
  Keyword.ClassLevel {} -> Nothing
  Keyword.Forecast -> Just (Text.pack "Forecast")
  Keyword.StartYourEngines -> Just (Text.pack "Start your engines!")
  Keyword.JobSelect -> Just (Text.pack "Job select")
  Keyword.Tiered -> Just (Text.pack "Tiered")
  Keyword.Exert -> Just (Text.pack "Exert")
  Keyword.Enlist -> Just (Text.pack "Enlist")
  Keyword.Bestow {} -> Nothing
  Keyword.Station -> Just (Text.pack "Station")
  Keyword.Mutate {} -> Nothing
  Keyword.UmbraArmor -> Just (Text.pack "Umbra armor")
  Keyword.Retrace -> Just (Text.pack "Retrace")
  Keyword.Mayhem Nothing -> Just (Text.pack "Mayhem")
  Keyword.Mayhem (Just _) -> Nothing
  Keyword.Madness {} -> Nothing
  Keyword.Rebound -> Just (Text.pack "Rebound")
  Keyword.Scavenge {} -> Nothing
  Keyword.Encore {} -> Nothing
  Keyword.Teamwork {} -> Nothing
  Keyword.WebSlinging {} -> Nothing
  Keyword.Sneak {} -> Nothing
  Keyword.Increment -> Just (Text.pack "Increment")
  Keyword.Storied -> Just (Text.pack "Storied")
  Keyword.Mobilize {} -> Nothing
  Keyword.Firebending {} -> Nothing
  Keyword.Transmute {} -> Nothing
  Keyword.Transfigure {} -> Nothing
  Keyword.Warp {} -> Nothing
  Keyword.Disturb {} -> Nothing
  Keyword.Harmonize {} -> Nothing

-- | Every keyword 'printed' answers for: the words an ingested card's text may
-- consist of. Pawl.OracleSpec checks it against the keyword codec's arms.
standalone :: [Keyword.Keyword]
standalone =
  [ Keyword.Deathtouch,
    Keyword.Defender,
    Keyword.DoubleStrike,
    Keyword.FirstStrike,
    Keyword.Flash,
    Keyword.Flying,
    Keyword.Haste,
    Keyword.Hexproof Nothing,
    Keyword.Indestructible,
    Keyword.Intimidate,
    Keyword.Lifelink,
    Keyword.LivingMetal,
    Keyword.Reach,
    Keyword.Shroud,
    Keyword.Trample,
    Keyword.TrampleOverPlaneswalkers,
    Keyword.Vigilance,
    Keyword.Banding,
    Keyword.Flanking,
    Keyword.Phasing,
    Keyword.Shadow,
    Keyword.Horsemanship,
    Keyword.Fear,
    Keyword.Provoke,
    Keyword.Storm,
    Keyword.Sunburst,
    Keyword.Epic,
    Keyword.Paradigm,
    Keyword.Convoke,
    Keyword.Haunt,
    Keyword.SplitSecond,
    Keyword.Vanishing Nothing,
    Keyword.Delve,
    Keyword.Gravestorm,
    Keyword.Changeling,
    Keyword.Conspire,
    Keyword.Persist,
    Keyword.Wither,
    Keyword.Exalted,
    Keyword.Cascade,
    Keyword.Infect,
    Keyword.BattleCry,
    Keyword.LivingWeapon,
    Keyword.Undying,
    Keyword.Soulbond,
    Keyword.Unleash,
    Keyword.Cipher,
    Keyword.Evolve,
    Keyword.Extort,
    Keyword.Fuse,
    Keyword.Dethrone,
    Keyword.Prowess,
    Keyword.Exploit,
    Keyword.Menace,
    Keyword.Devoid,
    Keyword.Ingest,
    Keyword.Myriad,
    Keyword.Skulk,
    Keyword.Melee,
    Keyword.Partner,
    Keyword.ChooseABackground,
    Keyword.DoctorsCompanion,
    Keyword.Undaunted,
    Keyword.Improvise,
    Keyword.Aftermath,
    Keyword.Ascend,
    Keyword.Assist,
    Keyword.JumpStart,
    Keyword.Mentor,
    Keyword.Riot,
    Keyword.Demonstrate,
    Keyword.Daybound,
    Keyword.Nightbound,
    Keyword.Decayed,
    Keyword.Training,
    Keyword.Compleated,
    Keyword.ReadAhead,
    Keyword.Ravenous,
    Keyword.ForMirrodin,
    Keyword.Bargain,
    Keyword.Spree,
    Keyword.Exhaust,
    Keyword.Boast,
    Keyword.PowerUp,
    Keyword.Forecast,
    Keyword.StartYourEngines,
    Keyword.JobSelect,
    Keyword.Tiered,
    Keyword.Exert,
    Keyword.Enlist,
    Keyword.Station,
    Keyword.UmbraArmor,
    Keyword.Retrace,
    Keyword.Rebound,
    Keyword.Increment,
    Keyword.Storied,
    Keyword.Mayhem Nothing
  ]

-- | Oracle text as a multiset of lowercase lines: reminder text dropped (CR
-- 207.2a), and a line of comma-separated keywords split, so "Flying, vigilance"
-- and "Flying\nVigilance" agree.
normalise :: Text.Text -> [Text.Text]
normalise =
  List.sort
    . filter (not . Text.null)
    . fmap (Text.strip . Text.toLower)
    . concatMap (Text.splitOn (Text.pack ", "))
    . Text.lines
    . withoutReminders

-- | The text with every parenthesised span removed, nested ones included.
withoutReminders :: Text.Text -> Text.Text
withoutReminders = Text.pack . go (0 :: Int) . Text.unpack
  where
    go depth string = case string of
      [] -> []
      '(' : rest -> go (depth + 1) rest
      ')' : rest | depth > 0 -> go (depth - 1) rest
      c : rest -> if depth > 0 && c /= '\n' then go depth rest else c : go depth rest
