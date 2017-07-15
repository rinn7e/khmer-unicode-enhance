module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List.Extra exposing (..)
import Dict
import Data


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = (\x -> Sub.none)
        }


type alias Model =
    { input : String
    , translate : String
    , tutorialStatus : Bool
    , listTest : List (List String)
    , akharakrom : Dict.Dict String String
    , akharakrom2 : Dict.Dict String String
    }


model : Model
model =
    { input = ""
    , translate = "អ្វីដែលសរសេរនឹងចេញនៅទីនេះ"
    , tutorialStatus = True
    , listTest = [ [] ]
    , akharakrom = Data.dataDouble
    , akharakrom2 = Data.dataTriple
    }


type Msg
    = UserInput String
    | ToggleTutorial


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        akharakroms =
            Dict.fromList [ ( "Tom", "Cat" ), ( "Jerry", "Mouse" ) ]
    in
        case msg of
            ToggleTutorial ->
                if model.tutorialStatus then
                    ( { model | tutorialStatus = False }, Cmd.none )
                else
                    ( { model | tutorialStatus = True }, Cmd.none )

            UserInput text ->
                let
                    -- array of array of character group by dup so that we can change to its other form
                    listText =
                        text
                            |> String.split ""
                            |> List.Extra.group

                    -- |> List.Extra.groupWhile (\x y -> x == y)
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
                    ( { model
                        | input = text
                        , listTest = listText
                        , translate = translateText
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h3 [ class "en" ] [ text "Khmer Unicode Enhance" ]
          -- , input [ placeholder "Write here", value model.input, onInput UserInput ] []
          -- , p [] [ text (toString model.listTest) ]
          -- , hr [] []
        , button [ class "btn btn-primary my-btn", onClick ToggleTutorial ]
            [ text "បង្ហាញរបៀបប្រើប្រាស់" ]
        , tutorialView model.tutorialStatus
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
        , button
            [ class "btn btn-primary my-btn copy-button en"
            , attribute "data-clipboard-target" "#copy-me"
            ]
            [ text "Copy" ]
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
        , Html.p []
            [ text "មានយោបល់អ្វីអាចទំនាក់ទំនងបានតាម "
            , a [ target "_blank", href "https://www.facebook.com/pg/Elm-Cambodia-1192992554180521/" ] [ text "Elm Cambodia" ]
            ]
        , Html.p []
            [ text "បង្កើតឡើងដោយ "
            , a [ target "_blank", href "https://github.com/chmar77/khmer-unicode-enhance" ] [ text "chmar77" ]
            ]
        ]


tutorialView status =
    let
        instructionList =
            [ { label = "Type key two times instead of using Shift key"
              , exampleList =
                    [ { word = "គេ"
                      , desc = "k + k + e"
                      }
                    , { word = "ល្បែង"
                      , desc = "l + j + b + [e + e] + g"
                      }
                    ]
              }
            , { label = "To two type the same character twice, type Space in between"
              , exampleList =
                    [ { word = "កករ"
                      , desc = "k + space + k + r"
                      }
                    ]
              }
            , { label = "To output Space, type Space 2 times"
              , exampleList =
                    [ { word = "ក ក"
                      , desc = "k + [space + space] + k"
                      }
                    ]
              }
            , { label = "Complex Example"
              , exampleList =
                    [ { word = "ខ្ញុំ"
                      , desc = "x + j + space + [j + j] + u + [m + m]"
                      }
                    ]
              }
            ]
    in
        div
            [ class
                ("well"
                    ++ (if status then
                            ""
                        else
                            " hidden"
                       )
                )
            ]
            [ h3 [] [ strong [] [ text "របៀបប្រើប្រាស់" ] ]
            , p [ class "en" ] [ text "(Change to khmer unicode before typing)" ]
            , hr [] []
            , div [] <|
                List.map
                    (\instruction -> instructionView instruction)
                    instructionList
            ]


instructionView instruction =
    div []
        [ h5 [ class "en" ]
            [ text instruction.label ]
        , div [] <|
            List.map
                (\example ->
                    p []
                        [ text ("- " ++ example.word)
                        , Html.span [ class "en " ] [ text ("= " ++ example.desc) ]
                        ]
                )
                instruction.exampleList
        ]
