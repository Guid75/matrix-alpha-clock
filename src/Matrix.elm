module Matrix exposing (Hour, viewMatrix)

import Array
import Html exposing (Attribute, Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class, style)


lettersMatrix =
    [ [ 'I', 'L', 'A', 'E', 'S', 'T', 'B', 'D', 'E', 'U', 'X' ]
    , [ 'Q', 'U', 'A', 'T', 'R', 'E', 'D', 'C', 'I', 'N', 'Q' ]
    , [ 'S', 'I', 'X', 'N', 'E', 'U', 'F', 'O', 'N', 'Z', 'E' ]
    , [ 'U', 'N', 'E', 'H', 'S', 'E', 'P', 'T', 'D', 'I', 'X' ]
    , [ 'H', 'U', 'I', 'T', 'F', 'T', 'R', 'O', 'I', 'S', 'Z' ]
    , [ 'U', 'M', 'I', 'D', 'I', 'H', 'E', 'U', 'R', 'E', 'S' ]
    , [ 'M', 'I', 'N', 'U', 'I', 'T', 'S', 'F', 'E', 'T', 'A' ]
    , [ 'M', 'O', 'I', 'N', 'S', 'O', 'V', 'I', 'N', 'G', 'T' ]
    , [ 'D', 'I', 'X', 'T', 'L', 'E', 'S', 'D', 'E', 'M', 'I' ]
    , [ 'Q', 'U', 'A', 'R', 'T', 'U', 'C', 'I', 'N', 'Q', 'O' ]
    , [ 'A', 'L', 'U', 'N', 'D', 'I', 'O', 'C', 'T', 'O', 'B' ]
    ]


type Word
    = Il
    | Est
    | Une
    | Deux
    | Trois
    | Quatre
    | Cinq
    | Six
    | Sept
    | Huit
    | Neuf
    | Dix
    | Onze
    | Midi
    | Heure
    | Heures
    | Minuit
    | Et
    | Moins
    | Vingt
    | DixMinutes
    | Le
    | Demi
    | Quart
    | CinqMinutes


type alias Hour =
    { hour : Int
    , minute : Int
    }


type alias Position =
    { first : Int, last : Int }


wordPosition : Word -> Position
wordPosition word =
    case word of
        Il ->
            { first = 0, last = 1 }

        Est ->
            { first = 3, last = 5 }

        Deux ->
            { first = 7, last = 10 }

        Quatre ->
            { first = 11, last = 16 }

        Cinq ->
            { first = 18, last = 21 }

        Six ->
            { first = 22, last = 24 }

        Neuf ->
            { first = 25, last = 28 }

        Onze ->
            { first = 29, last = 32 }

        Une ->
            { first = 33, last = 35 }

        Sept ->
            { first = 37, last = 40 }

        Dix ->
            { first = 41, last = 43 }

        Huit ->
            { first = 44, last = 47 }

        Trois ->
            { first = 49, last = 53 }

        Midi ->
            { first = 56, last = 59 }

        Heure ->
            { first = 60, last = 64 }

        Heures ->
            { first = 60, last = 65 }

        Minuit ->
            { first = 66, last = 71 }

        Et ->
            { first = 74, last = 75 }

        Moins ->
            { first = 77, last = 81 }

        Vingt ->
            { first = 83, last = 87 }

        DixMinutes ->
            { first = 88, last = 90 }

        Le ->
            { first = 92, last = 93 }

        Demi ->
            { first = 95, last = 98 }

        Quart ->
            { first = 99, last = 103 }

        CinqMinutes ->
            { first = 105, last = 108 }


isCellInPositions : Int -> List Position -> Bool
isCellInPositions cellIndex positions =
    List.any (\position -> cellIndex >= position.first && cellIndex <= position.last) positions


cellStyle : List Position -> Int -> Attribute msg
cellStyle activeWordsPositions cellIndex =
    if isCellInPositions cellIndex activeWordsPositions then
        style "color" "black"

    else
        style "color" "#F0F0F0"


viewCell : List Position -> Int -> Char -> Html msg
viewCell activeWordsPositions cellIndex char =
    td
        []
        [ div
            [ cellStyle activeWordsPositions cellIndex ]
            [ text <| String.fromChar char
            ]
        ]


viewRow : List Position -> Int -> List Char -> Html msg
viewRow activeWordsPositions rowIndex row =
    tr
        []
        (List.indexedMap (\cellIndex char -> viewCell activeWordsPositions (cellIndex + rowIndex * List.length row) char) row)


extractWordsPositions : List Word -> List Position
extractWordsPositions words =
    List.map wordPosition words


computeHourWord : Int -> List Word -> List Word
computeHourWord hour words =
    let
        twelvedHour =
            modBy 12 hour

        word =
            case twelvedHour of
                0 ->
                    if hour == 12 then
                        Midi

                    else
                        Minuit

                1 ->
                    Une

                2 ->
                    Deux

                3 ->
                    Trois

                4 ->
                    Quatre

                5 ->
                    Cinq

                6 ->
                    Six

                7 ->
                    Sept

                8 ->
                    Huit

                9 ->
                    Neuf

                10 ->
                    Dix

                11 ->
                    Onze

                _ ->
                    Il
    in
    if word == Midi || word == Minuit then
        word :: words

    else if twelvedHour == 1 then
        word :: Heure :: words

    else
        word :: Heures :: words


computeMinuteWordsMagnetized : Int -> List Word -> List Word
computeMinuteWordsMagnetized actualMinute words =
    let
        newWords =
            Moins :: words

        minute =
            60 - actualMinute
    in
    if minute <= 5 then
        CinqMinutes :: newWords

    else if minute <= 10 then
        DixMinutes :: newWords

    else if minute <= 15 then
        Le :: Quart :: newWords

    else if minute <= 20 then
        Vingt :: newWords

    else
        Vingt :: CinqMinutes :: newWords


computeMinuteWordsNominal : Int -> List Word -> List Word
computeMinuteWordsNominal minute words =
    if minute >= 30 then
        Et :: Demi :: words

    else if minute >= 25 then
        Vingt :: CinqMinutes :: words

    else if minute >= 20 then
        Vingt :: words

    else if minute >= 15 then
        Et :: Quart :: words

    else if minute >= 10 then
        DixMinutes :: words

    else if minute >= 5 then
        CinqMinutes :: words

    else
        words


computeMinuteWords : Int -> List Word -> List Word
computeMinuteWords minute words =
    if minute > 34 then
        computeMinuteWordsMagnetized minute words

    else
        computeMinuteWordsNominal minute words


computeActiveWords : Int -> Int -> List Position
computeActiveWords hour minute =
    let
        words =
            [ Il, Est ]

        displayedHour =
            if minute > 34 then
                hour + 1

            else
                hour
    in
    words
        |> computeHourWord displayedHour
        |> computeMinuteWords minute
        |> extractWordsPositions


viewMatrix : Maybe Hour -> Html msg
viewMatrix maybeHour =
    let
        activeWordsPositions =
            case Debug.log "maybeHour" maybeHour of
                Just hour ->
                    computeActiveWords hour.hour hour.minute

                Nothing ->
                    []
    in
    table
        [ style "font-family" "\"Courier New\", Courier, monospace"
        , style "font-size" "40px"
        , class "slide"
        , class "clock"
        ]
        (List.indexedMap (\rowIndex row -> viewRow activeWordsPositions rowIndex row) lettersMatrix)
