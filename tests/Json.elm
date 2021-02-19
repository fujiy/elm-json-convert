module Json exposing (..)

import Array
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Json.Convert exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Test"
        [ testConverter "Primitives" primitives fuzzyPrimitives
        , testConverter "Containers" containers fuzzyContainers
        , testConverter "Objects" record fuzzyRecord
        , testConverter "Recursive data types"
            (tree int)
            (fuzzyTree Fuzz.int 10)
        ]


testConverter : String -> Converter a -> Fuzzer a -> Test
testConverter str converter fz =
    describe str
        [ fuzz fz "the encoder and the decoder should be inverses of each other" <|
            \a ->
                let
                    encValue =
                        converter.encoder a

                    decData =
                        decodeValue converter encValue
                in
                Expect.equal (Ok a) decData
        ]


type alias Primitives =
    { string : String
    , int : Int
    , float : Float
    , bool : Bool
    , null : ()
    }


primitives : Converter Primitives
primitives =
    object Primitives <|
        field "string" .string string
            >> field "int" .int int
            >> field "float" .float float
            >> field "bool" .bool bool
            >> field "null" .null (null ())


fuzzyPrimitives : Fuzzer Primitives
fuzzyPrimitives =
    Fuzz.map5 Primitives
        Fuzz.string
        Fuzz.int
        Fuzz.float
        Fuzz.bool
        (Fuzz.constant ())


type alias Containers =
    { list : List String
    , array : Array.Array Int
    , dict : Dict.Dict String Float
    }


containers : Converter Containers
containers =
    object Containers <|
        field "list" .list (list string)
            >> field "array" .array (array int)
            >> field "dict" .dict (dict float)


fuzzyContainers : Fuzzer Containers
fuzzyContainers =
    Fuzz.map3 Containers
        (Fuzz.list Fuzz.string)
        (Fuzz.array Fuzz.int)
        (Fuzz.map Dict.fromList <|
            Fuzz.list <|
                Fuzz.tuple ( Fuzz.string, Fuzz.float )
        )


type alias Record =
    { nested : Primitives
    , option : Maybe String
    , nullable : Maybe Int
    }


record : Converter Record
record =
    object Record <|
        field "nested" .nested primitives
            >> option "option" .option string
            >> field "nullable" .nullable (nullable int)


fuzzyRecord : Fuzzer Record
fuzzyRecord =
    Fuzz.map3 Record
        fuzzyPrimitives
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe Fuzz.int)


type Tree a
    = Tree a (Maybe (Tree a)) (Maybe (Tree a))


tree : Converter a -> Converter (Tree a)
tree c =
    object Tree <|
        field "item" (\(Tree a _ _) -> a) c
            >> option "left" (\(Tree _ l _) -> l) (lazy <| \_ -> tree c)
            >> option "right" (\(Tree _ _ r) -> r) (lazy <| \_ -> tree c)


fuzzyTree : Fuzzer a -> Int -> Fuzzer (Tree a)
fuzzyTree fuzz depth =
    if depth <= 0 then
        Fuzz.map (\a -> Tree a Nothing Nothing) fuzz

    else
        Fuzz.map3 Tree
            fuzz
            (Fuzz.maybe <| fuzzyTree fuzz <| depth - 1)
            (Fuzz.maybe <| fuzzyTree fuzz <| depth - 1)
