module Json.Convert exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Maybe.Extra as Maybe


type alias Value =
    E.Value


type alias Converter a =
    { encoder : a -> Value
    , decoder : Decoder a
    }


string : Converter String
string =
    Converter E.string D.string


int : Converter Int
int =
    Converter E.int D.int


float : Converter Float
float =
    Converter E.float D.float


bool : Converter Bool
bool =
    Converter E.bool D.bool


null : a -> Converter a
null a =
    Converter (always E.null) (D.null a)


value : Converter Value
value =
    Converter identity D.value


list : Converter a -> Converter (List a)
list c =
    Converter (E.list c.encoder) (D.list c.decoder)


array : Converter a -> Converter (Array a)
array c =
    Converter (E.array c.encoder) (D.array c.decoder)


dict : Converter a -> Converter (Dict String a)
dict c =
    Converter (E.dict identity c.encoder) (D.dict c.decoder)


nullable : Converter a -> Converter (Maybe a)
nullable c =
    Converter (Maybe.unwrap E.null c.encoder) (D.nullable c.decoder)


type Field l r
    = Field (List ( String, r -> Maybe Value )) (Decoder l)


record : constr -> (Field constr a -> Field a a) -> Converter a
record constr mf =
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


field : String -> (r -> a) -> Converter a -> Field (a -> l) r -> Field l r
field name getter { encoder, decoder } (Field encoders dec) =
    Field
        (( name, getter >> encoder >> Just ) :: encoders)
        (D.map2 identity dec <| D.field name decoder)


option : String -> (r -> Maybe a) -> Converter a -> Field (Maybe a -> l) r -> Field l r
option name getter { encoder, decoder } (Field encoders dec) =
    Field
        (( name, getter >> Maybe.map encoder ) :: encoders)
        (D.map2 identity dec <| D.maybe <| D.field name decoder)
