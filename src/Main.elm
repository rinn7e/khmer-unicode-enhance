module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List.Extra exposing (..)
import Dict


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


type alias Model =
    { input : String
    , translate : String
    , listTest : List (List String)
    , akharakrom : Dict.Dict String String
    }


model : Model
model =
    { input = ""
    , translate = ""
    , listTest = [ [] ]
    , akharakrom =
        Dict.fromList
            [ ( "១", "!" )
            , ( "២", "ៗ" )
            , ( "៣", "ឈ" )
            , ( "៤", "\"" )
            , ( "៥", "%" )
            , ( "៦", "៍" )
            , ( "៧", "័" )
            , ( "៨", "៏" )
            , ( "៩", "(" )
            , ( "០", ")" )
            , ( "ឥ", "៌" )
            , ( "ឲ", "=" )
            , ( "ឆ", "ឈ" )
            , ( "ឹ", "ឺ" )
            , ( "េ", "ែ" )
            , ( "រ", "ឬ" )
            , ( "ត", "ទ" )
            , ( "យ", "ួ" )
            , ( "ុ", "ូ" )
            , ( "ិ", "ី" )
            , ( "ោ", "ៅ" )
            , ( "ផ", "ភ" )
            , ( "ៀ", "ឿ" )
            , ( "ឪ", "ឧ" )
            , ( "ា", "ា" )
            , ( "ស", "ៃ" )
            , ( "ដ", "ឌ" )
            , ( "ថ", "ធ" )
            , ( "ង", "អ" )
            , ( "ហ", "ះ" )
            , ( "្", "ញ" )
            , ( "ក", "គ" )
            , ( "ល", "ឡ" )
            , ( "ើ", "ើ" )
            , ( "់", "៉" )
            , ( "ឋ", "ឍ" )
            , ( "ខ", "ឃ" )
            , ( "ច", "ជ" )
            , ( "វ", "វ" )
            , ( "ប", "ព" )
            , ( "ន", "ណ" )
            , ( "ម", "ំ" )
            , ( "។", "៕" )
            , ( "៊", "?" )
            , ( "\x200B", " " )
            , ( "\n", "\n" )
            , ( "error", "Mouse" )
            ]
    }


type Msg
    = UserInput String



-- getCurrentIndex : Model -> Int
-- getCurrentIndex model =
--   { model | todo = { modelTodo | index } }


update : Msg -> Model -> Model
update msg model =
    let
        akharakroms =
            Dict.fromList [ ( "Tom", "Cat" ), ( "Jerry", "Mouse" ) ]
    in
        case msg of
            UserInput text ->
                let
                    -- array of array of character group by dup so that we can change to its other form
                    listText =
                        text
                            |> String.split ""
                            |> List.Extra.groupWhile (\x y -> x == y)

                    -------------------------------------------------------------------
                    -- BELOW ARE COMPOSITE AND APPLICATION FUNCTION THE SAME THING
                    -----------------------------------------------------------
                    -- splitAndGroup =
                    --     String.split ""
                    --         >> List.Extra.groupWhile (\x y -> x == y)
                    -- splitAndGroup text =
                    --     text
                    --         |> String.split ""
                    --         |> List.Extra.groupWhile (\x y -> x == y)
                    -- listText =
                    --     splitAndGroup text
                    ---------------------------------------------------------------
                    -- replace dup with its other form, turn it into normal array, filter ",", then join to get string
                    translateText =
                        listText
                            |> List.map
                                (\x ->
                                    if (List.length x == 2) then
                                        -- if (Dict.member (Maybe.withDefault "error" (List.head x)) model.akharakrom == True) then
                                        --     Dict.get Maybe.withDefault "error" (List.head x)
                                        --
                                        -- else
                                        --     x
                                        let
                                            char =
                                                Dict.get (Maybe.withDefault "error" (List.head x)) model.akharakrom
                                        in
                                            [ Maybe.withDefault "error" char ]
                                        -- else if (List.length x == 3) then
                                        --     [ "្ញ" ]
                                    else
                                        x
                                )
                            |> List.Extra.intercalate []
                            |> List.filter (\x -> x /= "\x17FC")
                            |> List.filter (\x -> x /= "\x200B")
                            |> String.join ""
                in
                    { model
                        | input = text
                        , listTest = listText
                        , translate = translateText
                    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Khmer Unicode Enhance" ]
          -- , input [ placeholder "Write here", value model.input, onInput UserInput ] []
          -- , p [] [ text (toString model.listTest) ]
        , hr [] []
        , h2 [] [ text "Tutorial" ]
        , div []
            [ h4 [] [ text "No Shift use double letter" ]
            , p [] [ text "គេ   = [k + k] + e" ]
            , p [] [ text "ល្បែង = l + j + b + [e + e] + g" ]
            , h4 [] [ text "to Separate use space" ]
            , p [] [ text "កករ = k + space + k + r" ]
            , h4 [] [ text "to make actual space , type space 2 times" ]
            , p [] [ text "space  = space + space" ]
            , h4 [] [ text "complex example" ]
            , p [] [ text "ខ្ញុំ  = k + j + space + [j + j] + u + [m + m]" ]
            , h4 [] [ text "Change to khmer unicode before typing" ]
            ]
        , hr [] []
        , pre [] [ text model.translate ]
        , hr [] []
        , textarea
            [ value model.input
            , onInput UserInput
            , placeholder "Type Khmer Word here"
            , Html.Attributes.style
                [ ( "height", "90px" )
                , ( "width", "100%" )
                ]
            ]
            []
        ]
