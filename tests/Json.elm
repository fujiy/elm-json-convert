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
        , testConverter "Object" object fuzzyObject
        ]


testConverter : String -> Converter a -> Fuzzer a -> Test
testConverter str converter fz =
    describe str
        [ fuzz fz "encoder and decoder should be inverses of each other" <|
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


type alias Containers =
    { list : List String
    , array : Array.Array Int
    , dict : Dict.Dict String Float
    , nullable : Maybe String
    }


type alias Object =
    { nested : Primitives
    , option : Maybe String
    }


primitives : Converter Primitives
primitives =
    field "string" .string string
        >> field "int" .int int
        >> field "float" .float float
        >> field "bool" .bool bool
        >> field "null" .null (null ())
        |> record Primitives


containers : Converter Containers
containers =
    field "list" .list (list string)
        >> field "array" .array (array int)
        >> field "dict" .dict (dict float)
        >> field "nullable" .nullable (nullable string)
        |> record Containers


object : Converter Object
object =
    field "nested" .nested primitives
        >> option "option" .option string
        |> record Object


fuzzyPrimitives : Fuzzer Primitives
fuzzyPrimitives =
    Fuzz.map5 Primitives
        Fuzz.string
        Fuzz.int
        Fuzz.float
        Fuzz.bool
        (Fuzz.constant ())


fuzzyContainers : Fuzzer Containers
fuzzyContainers =
    Fuzz.map4 Containers
        (Fuzz.list Fuzz.string)
        (Fuzz.array Fuzz.int)
        (Fuzz.map Dict.fromList <|
            Fuzz.list <|
                Fuzz.tuple ( Fuzz.string, Fuzz.float )
        )
        (Fuzz.maybe Fuzz.string)


fuzzyObject : Fuzzer Object
fuzzyObject =
    Fuzz.map2 Object
        fuzzyPrimitives
        (Fuzz.maybe Fuzz.string)
