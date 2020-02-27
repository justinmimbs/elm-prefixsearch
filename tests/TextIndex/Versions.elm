module TextIndex.Versions exposing (Version, scanList, scanTrie, seekList, seekSet)

import TextIndex
import TextIndex.V1ScanList
import TextIndex.V2ScanTrie
import TextIndex.V3SeekSet


type alias Version a =
    { name : String
    , empty : a
    , insert : Int -> String -> a -> a
    , search : String -> a -> List Int
    , searchAll : List String -> a -> List Int
    }


scanList : Version TextIndex.V1ScanList.TextIndex
scanList =
    { name = "v1 ScanList"
    , empty = TextIndex.V1ScanList.empty
    , insert = TextIndex.V1ScanList.insert
    , search = TextIndex.V1ScanList.search
    , searchAll = TextIndex.V1ScanList.searchAll
    }


scanTrie : Version TextIndex.V2ScanTrie.TextIndex
scanTrie =
    { name = "v2 ScanTrie"
    , empty = TextIndex.V2ScanTrie.empty
    , insert = TextIndex.V2ScanTrie.insert
    , search = TextIndex.V2ScanTrie.search
    , searchAll = TextIndex.V2ScanTrie.searchAll
    }


seekSet : Version TextIndex.V3SeekSet.TextIndex
seekSet =
    { name = "v3 SeekSet"
    , empty = TextIndex.V3SeekSet.empty
    , insert = TextIndex.V3SeekSet.insert
    , search = TextIndex.V3SeekSet.search
    , searchAll = TextIndex.V3SeekSet.searchAll
    }


seekList : Version TextIndex.TextIndex
seekList =
    { name = "v4 SeekList"
    , empty = TextIndex.empty
    , insert = TextIndex.insert
    , search = TextIndex.search
    , searchAll = TextIndex.searchAll
    }
