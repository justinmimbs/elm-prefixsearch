module Types exposing (Search, exampleText, fill, strings, trie, trieset)

import SearchStrings
import SearchTrie
import SearchTrieSet


type alias Search a =
    { name : String
    , empty : a
    , insert : Int -> String -> a -> a
    , search : String -> a -> List Int
    }


strings : Search SearchStrings.Search
strings =
    { name = "Strings"
    , empty = SearchStrings.empty
    , insert = SearchStrings.insert
    , search = SearchStrings.search
    }


trie : Search SearchTrie.Search
trie =
    { name = "Trie"
    , empty = SearchTrie.empty
    , insert = SearchTrie.insert
    , search = SearchTrie.search
    }


trieset : Search SearchTrieSet.Search
trieset =
    { name = "TrieSet"
    , empty = SearchTrieSet.empty
    , insert = SearchTrieSet.insert
    , search = SearchTrieSet.search
    }


fill : List String -> (Int -> String -> a -> a) -> a -> a
fill texts insert empty =
    texts
        |> List.indexedMap (\i text -> ( i + 1, text ))
        |> List.foldl
            (\( id, text ) struct ->
                text
                    |> String.words
                    |> List.foldl (insert id) struct
            )
            empty


exampleText : List String
exampleText =
    [ "dynamic diaphragm tape rabbits rainstorm"
    , "site regard lifestyle difficult mice school basin"
    , "advance long refute grass snow wink"
    , "favourite tray noise business annoyed shape freezing"
    , "publicize buttocks poem guiltless crate"
    , "flipper competent scam embarrassed copy"
    , "deport violation ear jaded frail guttural cooperative"
    , "juice ascetic sweaty airport risk"
    , "create laundry lease invite glow roof abusive"
    , "smile box shop guiltless shock boy"
    , "juice apnea native finicky majestic alarm"
    , "ensure localize inadvertent drink beginner"
    , "hate peasant hunting easy chickens woebegone steadfast"
    , "cinder mum handiwork calm blow"
    , "lead verdict fixture stick demonic aquatic agree"
    , "grow saltwater psychology tray erratic melted"
    , "sweat practical credit prick discovery detail van"
    , "deport steeple cable face stretch building"
    , "literacy promote throat damaged reflect useful"
    , "theme dialect praise meal spill relax bouncy"
    , "bacon coffee mechanism wave toothpaste passenger telling"
    , "science philharmonic leak abrupt mean bolt"
    , "deficiency pierce sustain lunch creature advertisement adventurous"
    , "shock outer rotate fuel phobic fear peep"
    , "comfort work unfettered attractive umbrella"
    , "workman postgraduate comatose invent replace"
    , "study belly pause statuesque spoon jazzy corn"
    , "predominate destroy saltwater wacky silly"
    , "peasant overview violation muscle overflow daffy"
    , "pore geometry outer equable cushion mundane"
    , "penchant romanticism apnea spotty object"
    , "separate smash elite ill calm stamp zany"
    , "opera copper definition cup humdrum jolly calculator"
    , "divorce computer judge rate yell thirsty goofy"
    , "segregate refute hideout cub comfortable"
    , "indirect affect verdict psychotic full adjoining"
    , "drunkenness alert chill bolt spill"
    , "disapproving fab mask furniture flimsy"
    , "port controversial horsepower death obsequious"
    , "peasant mile crash bear obsequious care soggy"
    , "drain credit stimulation command selfish spot"
    , "oppressive damn skewed border respect"
    , "tactic serve preference nappy follow classy effect"
    , "extract abysmal brick unruly expect"
    , "literacy morsel beach lazy idea challenge hideous"
    , "dynamic horsepower damn rhythm measly grease"
    , "howl excuse long beach trite precede"
    , "accompanying amplified curriculum girls rebel cluttered"
    , "duck melted handbook long-term imagine"
    , "meaning leak deprive actually piquant confuse touch"
    , "mock applicability galvanized mouth bitter"
    , "fashionable unfettered smash substantial grate prepare"
    , "pleasure entry distance intelligent aftermath ajar fretful"
    , "concert compartment variety slope special defeated slow"
    , "deed adaptability house awesome plants"
    , "price attract minimize suppose polish cloudy cart"
    , "unusual flimsy applicability childlike pleasure snail"
    , "tactic safe poetry enter wreck electric"
    , "high-resolution maze impressionist massive meddle"
    , "mold specialty fist equable fuzzy achiever"
    , "straddle tailback castle reaction dolls"
    , "drain update crevice mountain annoyed existence silver"
    , "price sweaty plain sable obsequious protest"
    , "segregate inadvertent work flaky knotty mice"
    , "unusual wiring handout team choke"
    , "equal skeleton tank zinc accessible plant burn"
    , "advance regulatory insertion periodic crowded"
    , "candidate brick mile piquant aloof bulb"
    , "taunt grape herald judicious gleaming"
    , "handset homage integration soap guess mailbox"
    , "rousing scam peace friction brainy preach"
    , "opiate embryo credit spring ants"
    , "comfort vault delude married ugly sassy"
    , "opiate chill abysmal replace sun travel"
    , "banana remain general melted small utopian lively"
    , "disorder throat car feeling punish ad hoc deliver"
    , "extract rationale mystery smash owe brief"
    , "scowl north commercialize squeak protest"
    , "iron insertion remain telling pull wobble"
    , "saudi tend dream sour limit"
    , "intimidation seaman box-office childlike fretful"
    , "iron fisherman soldier rapid nice leg sordid"
    , "elaborate wrap golf flower placid x-ray fumbling"
    , "defunct shop safe nerve doubt"
    , "karma blend geometry like eggnog"
    , "crossing loud instal needless afford lethal circle"
    , "mosaic palace lazy linen mitten female twist"
    , "interdependent mystery overview unused scrub"
    , "indirect poetry explosion talented contain battle wing"
    , "mold cable settlement replace road terrify ignorant"
    , "defunct satisfying student prick list absorbed"
    , "coat student affect warlike poke"
    , "pore delude plush crush stir"
    , "wasteful reptile bowl adorable sable"
    , "firing pleasing homage outgoing ground"
    , "smile southwestern bend receptive vigorous"
    , "jurisdiction plain summary vase squeal quartz kiss"
    , "feel tape pierce thundering tug skip"
    , "candidate stimulation layout volleyball insect testy eggnog"
    , "have dish nominate ship imaginary observant porter"
    , "president introduction trace spicy tiny helpful glove"
    , "panel fist anniversary machine ajar grateful torpid"
    , "slavic rivet philharmonic refuse womanly"
    , "arrange herald thoughtful fallacious lumber admit"
    , "slavic comatose decrease agreement card quaint"
    , "mock poem compartment brown glib strengthen"
    , "high dream ascetic downtown respect happen"
    , "interdependent bowl acquired interrupt thin engine"
    , "meaning enlist fisherman position scorch week"
    , "mother bend serve obedient bumpy gamy"
    , "tutelage decrease enlist onerous stereotyped"
    , "jurisdiction jesuit morsel copy enchanted deadpan"
    , "feel thoughtful atmosphere tail marked mouth mere"
    , "misery lamb feminine flat needless cynical nappy"
    , "pasture chief functional snail unadvised shaggy scream"
    , "musician optimal rationale purpose fit"
    , "puff slit foam delicate snatch"
    , "science gallery permanent point addicted smoggy squeeze"
    , "rousing acquired steeple normal profuse"
    , "catch handiwork gallery friendly instinctive helpless"
    , "blended antelope nazi payment optimal"
    , "arrange brother tasty analyse successful mute push"
    , "conscience variety skeleton tempt street premium"
    , "lead handout coffee battle petite monkey"
    , "interrupt waiter dialogue fuel aboard describe icy"
    , "conscience laborer default boy rule absorbing flower"
    , "banana hideout encourage advice idea horn"
    , "handset peace specialty lean wandering"
    , "coat undermine wiring pumped arrogant stare"
    , "addition goblet undermine tomatoes bike"
    , "deficiency skewed lamb careless permissible believe"
    , "separate plush entry efficacious thinkable tangy"
    , "bacon galvanized laborer marked deep miscreant"
    , "accompanying native jesuit faded symptomatic"
    , "buy nazi update ignorant frightened fresh"
    , "helping beach amplification grease responsible"
    , "president frame flimsy ducks flag"
    , "mother weigh sleeve worry knee groan competition"
    , "concert handbook brother magnificent grubby connect"
    , "buy psychology stab crook chase panicky dream"
    , "misery maze attract rural combative sticky"
    , "dungeon strikeout satisfying gorgeous baby"
    , "tutelage mask regulatory snail boiling scrape"
    , "grow curriculum promote gusty authority reply giraffe"
    , "compartment palm row move amusement visit high-pitched"
    , "oppressive castle resolution pleasant lively tested"
    , "obsessed cruel vault request illegal"
    , "fashionable encourage rational wicked tire halting achiever"
    , "catch integration dry metal willing decide fallacious"
    , "shock foam weigh gusty adjoining alluring"
    ]
