module Json.Convert exposing
    ( Converter
    , Value, encode, decodeValue, decodeString
    , string, int, float, bool, null
    , list, array, dict
    , Field, object, field, option
    , Iso, map, reverse, compose
    , nullable, value, lazy
    )

{-|


# Converter

@docs Converter


# Encoding and Decoding

@docs Value, encode, decodeValue, decodeString


# Primitives

@docs string, int, float, bool, null


# Containers

@docs list, array, dict


# Objects

@docs Field, object, field, option


# Mappings

@docs Iso, map, reverse, compose


# Miscellaneous

@docs nullable, value, lazy

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


{-| Represents a JavaScript value.
-}
type alias Value =
    E.Value


{-| A pair of JSON encoder and decoder.

You can make custom converters using this directly if you need.

-}
type alias Converter a =
    { encoder : a -> Value
    , decoder : Decoder a
    }


{-| Convert data into a prettified string. The second argument specifies the amount of indentation in the resulting string.
-}
encode : Converter a -> Int -> a -> String
encode converter indent a =
    E.encode indent <| converter.encoder a


{-| Decode a JSON `Value`.
-}
decodeValue : Converter a -> Value -> Result D.Error a
decodeValue converter =
    D.decodeValue converter.decoder


{-| Parse the given string into a JSON value and then decode it.
-}
decodeString : Converter a -> String -> Result D.Error a
decodeString converter =
    D.decodeString converter.decoder


{-| Converter for `String`.

    "hello" <== string ==> "\"hello\""

-}
string : Converter String
string =
    Converter E.string D.string


{-| Converter for `Int`.

    42 <== int ==> "42"

-}
int : Converter Int
int =
    Converter E.int D.int


{-| Converter for `Float`.

    3.14 <== float ==> "3.14"

-}
float : Converter Float
float =
    Converter E.float D.float


{-| Convert between an Elm `Bool` and a JSON boolean.

    True <== bool ==> "true"

-}
bool : Converter Bool
bool =
    Converter E.bool D.bool


{-| Convert between some Elm value and a JSON `null` value.

    "Some Value" <== null "Some Value" ==> null

-}
null : a -> Converter a
null a =
    Converter (always E.null) (D.null a)


{-| Do not do anything, just build a bridge between a JSON Value and an Elm `Value`.
-}
value : Converter Value
value =
    Converter identity D.value


{-| Convert between an Elm `List` and a JSON array.

    [ 1, 2, 3 ] <== list int ==> "[1,2,3]"

-}
list : Converter a -> Converter (List a)
list c =
    Converter (E.list c.encoder) (D.list c.decoder)


{-| Convert between an Elm `Array` and a JSON array.

    Array.fromList [ 1, 2, 3 ] <== array int ==> "[1,2,3]"

-}
array : Converter a -> Converter (Array a)
array c =
    Converter (E.array c.encoder) (D.array c.decoder)


{-| Convert between an Elm `Dict` and a JSON object.

    Dict.fromList [ ( "alice", 42 ), ( "bob", 99 ) ] <== dict int ==> "{ \"alice\": 42, \"bob\": 99 }"

-}
dict : Converter a -> Converter (Dict String a)
dict c =
    Converter (E.dict identity c.encoder) (D.dict c.decoder)


{-| Convert between an Elm value and a nullable JSON value.

    Just 42 <== nullable int ==> "42"

    Nothing <== nullable int ==> "null"

-}
nullable : Converter a -> Converter (Maybe a)
nullable c =
    Converter (unwrap E.null c.encoder) (D.nullable c.decoder)


{-| A helper data type for building an object converter.

The following functions have a little tricky types, but you do not need to think about it! Just write like this:

    type alias User =
        { name : String
        , age : Int
        , height : Maybe Float
        }

    converter : Converter User
    converter =
        object User <|
            field "name" .name string
                >> field "age" .age int
                >> option "height" .height float

-}
type Field l r
    = Field (List ( String, r -> Maybe Value )) (Decoder l)


{-| Build an object converter with a constructor. Use it along with `field` or `option`.
-}
object : constr -> (Field constr a -> Field a a) -> Converter a
object constr mf =
    let
        (Field encoders decoder) =
            mf <| Field [] (D.succeed constr)
    in
    Converter
        (\r ->
            E.object <|
                List.filterMap
                    (\( name, f ) -> Maybe.map (Tuple.pair name) <| f r)
                    encoders
        )
        decoder


{-| Represents a field of an object. Use it along with `object`.
-}
field : String -> (r -> a) -> Converter a -> Field (a -> l) r -> Field l r
field name getter { encoder, decoder } (Field encoders dec) =
    Field
        (( name, getter >> encoder >> Just ) :: encoders)
        (D.map2 identity dec <| D.field name decoder)


{-| Optional version of `field`.
-}
option : String -> (r -> Maybe a) -> Converter a -> Field (Maybe a -> l) r -> Field l r
option name getter { encoder, decoder } (Field encoders dec) =
    Field
        (( name, getter >> Maybe.map encoder ) :: encoders)
        (D.map2 identity dec <| D.maybe <| D.field name decoder)


{-| For building a converter of recursive structure.
Use it like `lazy (\_ -> converter)` instead of just `converter` in order for the converter not to expand infinitly deep.

    type Tree a
        = Tree a (Maybe (Tree a)) (Maybe (Tree a))

    tree : Converter a -> Converter (Tree a)
    tree item =
        object Tree <|
            field "item" (\(Tree a _ _) -> a) item
                >> option "left" (\(Tree _ l _) -> l) (lazy <| \_ -> tree item)
                >> option "right" (\(Tree _ _ r) -> r) (lazy <| \_ -> tree item)

-}
lazy : (() -> Converter a) -> Converter a
lazy lc =
    Converter (\a -> (lc ()).encoder a) (D.lazy (lc >> .decoder))


{-| A pair of a function and its inverse.
Same definition for [Monocle.Iso](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest/Monocle-Iso).
-}
type alias Iso a b =
    { get : a -> b
    , reverseGet : b -> a
    }


{-| Create the reversed isomorphism.
-}
reverse : Iso a b -> Iso b a
reverse iso =
    Iso iso.reverseGet iso.get


{-| Compose two isomorphisms.
-}
compose : Iso a b -> Iso b c -> Iso a c
compose f g =
    Iso (f.get >> g.get) (f.reverseGet << g.reverseGet)


{-| Transform a converter. You need `Iso a b` because the decoder requires a function `a -> b` and the encoder requires an inverse function `b -> a` to map.

    string2CharListIso : Iso String (List Char)
    string2CharListIso =
        Iso String.toList String.fromList

    charList : Converter (List Char)
    charList =
        map string2CharListIso string

-}
map : Iso a b -> Converter a -> Converter b
map iso c =
    Converter (iso.reverseGet >> c.encoder) (D.map iso.get c.decoder)


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap b f ma =
    case ma of
        Just a ->
            f a

        Nothing ->
            b
