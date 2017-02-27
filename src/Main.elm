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
    , tutorial : Bool
    , listTest : List (List String)
    , akharakrom : Dict.Dict String String
    , akharakrom2 : Dict.Dict String String
    }


model : Model
model =
    { input = ""
    , translate = "អ្វីដែលសរសេរនឹងចេញនៅទីនេះ"
    , tutorial = False
    , listTest = [ [] ]
    , akharakrom =
        Dict.fromList
            [ ( "«", "»" )
            , ( "១", "!" )
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
            , ( "ឮ", "ឭ" )
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
            , ( "error", "Mouse" )
            ]
    , akharakrom2 =
        Dict.fromList
            [ ( "២", "@" )
            , ( "៣", "៑" )
            , ( "៤", "$" )
            , ( "៥", "€" )
            , ( "៦", "៙" )
            , ( "៧", "៚" )
            , ( "៨", "*" )
            , ( "៩", "{" )
            , ( "០", "}" )
            , ( "ឥ", "x" )
            , ( "ឲ", "៎" )
            , ( "ឮ", "\\" )
            , ( "េ", "ឯ" )
            , ( "រ", "ឫ" )
            , ( "ិ", "ឦ" )
            , ( "ោ", "ឱ" )
            , ( "ផ", "ឰ" )
            , ( "ៀ", "ឩ" )
            , ( "ឪ", "ឳ" )
            , ( "ើ", "៖" )
            , ( "់", "ៈ" )
            , ( "។", "." )
            , ( "៊", "/" )
            ]
    }


type Msg
    = UserInput String
    | ToggleTutorial



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
            ToggleTutorial ->
                if model.tutorial then
                    ({ model | tutorial = False })
                else
                    ({ model | tutorial = True })

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
                                            original =
                                                Maybe.withDefault "error" (List.head x)

                                            char =
                                                Dict.get original model.akharakrom
                                        in
                                            [ Maybe.withDefault (original ++ original) char ]
                                        -- else if (List.length x == 3) then
                                        --     [ "្ញ" ]
                                    else if (List.length x == 3) then
                                        let
                                            original =
                                                Maybe.withDefault "error" (List.head x)

                                            char =
                                                Dict.get original model.akharakrom

                                            char2 =
                                                Dict.get original model.akharakrom2
                                        in
                                            [ Maybe.withDefault ((Maybe.withDefault (original ++ original) char) ++ original) char2 ]
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
    div [ class "container" ]
        [ h3 [] [ text "Khmer Unicode Enhance" ]
          -- , input [ placeholder "Write here", value model.input, onInput UserInput ] []
          -- , p [] [ text (toString model.listTest) ]
          -- , hr [] []
        , button [ class "btn btn-primary my-btn", onClick ToggleTutorial ]
            [ text "បង្ហាញរបៀបប្រើប្រាស់" ]
        , div
            [ class
                ("well"
                    ++ (if model.tutorial then
                            ""
                        else
                            " hidden"
                       )
                )
            ]
            [ h3 [] [ text "របៀបប្រើប្រាស់" ]
            , div []
                [ h4 [] [ text "No Shift use double letter" ]
                , p [] [ text "គេ   = [k + k] + e" ]
                , p [] [ text "ល្បែង = l + j + b + [e + e] + g" ]
                , h4 [] [ text "to Separate use space" ]
                , p [] [ text "កករ = k + space + k + r" ]
                , h4 [] [ text "to make actual space , type space 2 times" ]
                , p [] [ text "space  = space + space" ]
                , h4 [] [ text "complex example" ]
                , p [] [ text "ខ្ញុំ  = x + j + space + [j + j] + u + [m + m]" ]
                , h4 [] [ text "Change to khmer unicode before typing" ]
                ]
            ]
        , div [ class "panel panel-primary" ]
            [ div [ class "panel-heading" ]
                [ h3 [ class "panel-title" ] [ text "លទ្ធិផល" ]
                ]
            , div [ class "panel-body output ", id "copy-me" ]
                [ text model.translate
                ]
              -- h4 [] [ text "Look here" ]
              -- , button [ class "btn btn-success copy-button", attribute "data-clipboard-target" "#copy-me" ] [ text "Copy" ]
              -- , p [ class "output", id "copy-me" ] [ text model.translate ]
            ]
        , button [ class "btn btn-primary my-btn copy-button", attribute "data-clipboard-target" "#copy-me" ] [ text "Copy" ]
          -- , hr [] []
          -- , h4 [] [ text "Type Here" ]
        , div [ class "well" ]
            [ textarea
                [ value model.input
                , onInput UserInput
                , placeholder "សរសេរនៅទីនេះ"
                , class "form-control"
                , rows 5
                  -- , Html.Attributes.style
                  --     [ ( "height", "90px" )
                  --     , ( "width", "100%" )
                  --     ]
                ]
                []
            ]
        , hr [] []
        ]
