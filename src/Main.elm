module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (checked, class, max, min, name, style, type_, value)
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
    | Manual String


type alias Model =
    { zone : Maybe Time.Zone
    , time : Maybe Time.Posix
    , displayType : DisplayType
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zone = Nothing
      , time = Nothing
      , displayType = RealTime
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- ( Model Time.utc (Time.millisToPosix 0)
-- , Task.perform AdjustTimeZone Time.here
-- )


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | RealTimeDisplayChecked
    | ManualDisplayChecked
    | ManualValueChanged String


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
            ( { model | displayType = Manual "0" }, Cmd.none )

        ManualValueChanged value ->
            ( { model | displayType = Manual value }, Cmd.none )


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


getHourAndMinute : String -> ( Int, Int )
getHourAndMinute minutesStr =
    let
        minutes =
            Maybe.withDefault 0 <| String.toInt minutesStr

        hour =
            minutes // 60

        minute =
            remainderBy 60 minutes
    in
    ( hour, minute )


displayHourAndMinute : String -> String
displayHourAndMinute minutesStr =
    let
        ( hour, minute ) =
            getHourAndMinute minutesStr
    in
    String.fromInt hour ++ ":" ++ String.fromInt minute


displayManualValue : DisplayType -> Html msg
displayManualValue displayType =
    text <|
        case displayType of
            RealTime ->
                "N/A"

            Manual str ->
                displayHourAndMinute str


viewOptions : Model -> Html Msg
viewOptions model =
    let
        rangeValue =
            case model.displayType of
                RealTime ->
                    "0"

                Manual str ->
                    str
    in
    div
        [ class "slide" ]
        [ div [] [ radiobutton "Display realtime hour" RealTimeDisplayChecked (model.displayType == RealTime) ]
        , div [] [ radiobutton "Display manual hour" ManualDisplayChecked (model.displayType /= RealTime) ]
        , div []
            [ input
                [ type_ "range"
                , style "width" "400px"
                , Html.Attributes.min "0"
                , Html.Attributes.max "1440"
                , value rangeValue
                , onInput ManualValueChanged
                ]
                []
            ]
        , div []
            [ displayManualValue model.displayType ]
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

                Manual value ->
                    let
                        ( hour, minute ) =
                            getHourAndMinute value
                    in
                    Just { hour = hour, minute = minute }
    in
    div [ class "container" ]
        [ Matrix.viewMatrix maybeHour
        , viewOptions model
        ]
