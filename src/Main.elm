module Main exposing (..)

import Browser
import Html exposing (Html, button, datalist, div, input, label, option, text)
import Html.Attributes
    exposing
        ( attribute
        , checked
        , class
        , for
        , id
        , list
        , max
        , min
        , name
        , style
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Matrix
import Task
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type DisplayType
    = RealTime
    | Manual


type alias Model =
    { zone : Maybe Time.Zone
    , time : Maybe Time.Posix
    , displayType : DisplayType
    , manualHour : ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zone = Nothing
      , time = Nothing
      , displayType = RealTime
      , manualHour = ( 10, 8 )
      }
    , Task.perform AdjustTimeZone Time.here
    )


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | RealTimeDisplayChecked
    | ManualDisplayChecked
    | ManualValueChanged String


splitHour : String -> ( Int, Int )
splitHour hourMinuteStr =
    case String.split ":" hourMinuteStr of
        [ hourStr, minuteStr ] ->
            let
                hour =
                    Maybe.withDefault 0 <| String.toInt hourStr

                minute =
                    Maybe.withDefault 0 <| String.toInt minuteStr
            in
            ( hour, minute )

        _ ->
            ( 0, 0 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = Just newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = Just newZone }
            , Cmd.none
            )

        RealTimeDisplayChecked ->
            ( { model | displayType = RealTime }, Cmd.none )

        ManualDisplayChecked ->
            ( { model | displayType = Manual }, Cmd.none )

        ManualValueChanged value ->
            ( { model
                | manualHour = splitHour value
                , displayType = Manual
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


viewHour : Time.Zone -> Time.Posix -> Html msg
viewHour zone time =
    div
        []
        [ text <| String.fromInt <| Time.toHour zone time
        , text <| ":"
        , text <| String.fromInt <| Time.toMinute zone time
        ]


radiobutton : String -> msg -> Bool -> Html msg
radiobutton value msg sel =
    label []
        [ input
            [ type_ "radio"
            , name "value"
            , onClick msg
            , checked sel
            ]
            []
        , text value
        ]


displayHourAndMinute : ( Int, Int ) -> String
displayHourAndMinute ( hour, minute ) =
    let
        hourStr =
            String.fromInt hour
                |> String.padLeft 2 '0'

        minuteStr =
            String.fromInt minute
                |> String.padLeft 2 '0'
    in
    hourStr ++ ":" ++ minuteStr


viewOptions : Model -> Html Msg
viewOptions model =
    div
        [ class "options"
        ]
        [ div [ class "option" ] [ radiobutton "Display realtime hour" RealTimeDisplayChecked (model.displayType == RealTime) ]
        , div [ class "option" ] [ radiobutton "Display manual hour" ManualDisplayChecked (model.displayType /= RealTime) ]
        , div [ class "option" ]
            [ input
                [ type_ "time"
                , value <| displayHourAndMinute model.manualHour
                , style "margin-left" "20px"
                , Html.Attributes.min "09:00"
                , Html.Attributes.max "18:00"
                , onInput ManualValueChanged
                ]
                []
            ]
        ]


view : Model -> Html Msg
view model =
    let
        maybeHour =
            case model.displayType of
                RealTime ->
                    case ( model.zone, model.time ) of
                        ( Just zone, Just time ) ->
                            Just { hour = Time.toHour zone time, minute = Time.toMinute zone time }

                        _ ->
                            Nothing

                Manual ->
                    let
                        ( hour, minute ) =
                            model.manualHour
                    in
                    Just { hour = hour, minute = minute }
    in
    div [ class "container" ]
        [ Matrix.viewMatrix maybeHour
        , viewOptions model
        ]
