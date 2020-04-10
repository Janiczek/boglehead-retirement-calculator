port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Lazy
import List.Extra
import SingleComputation exposing (Row)
import Utils exposing (round100)
import VegaLite as V


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port elmToJS : V.Spec -> Cmd msg


type alias Model =
    -- AGE
    { initialAge : Int
    , retirementAge : Int -- deposit end at this age

    -- SALARY: will increase linearly
    , initialSalary : Float
    , retirementSalary : Float

    -- RETURN
    , initialReturnPercent : Float
    , finalReturnPercent : Float -- lowers as you rebalance the portfolio and add more bonds to lower the risk
    , finalReturnAtAge : Int

    -- DEPOSITS AND WITHDRAWALS
    , depositPercent : Float
    , retirementWithdrawal : Float

    -- non-input model
    , computed : List Row
    }


type Msg
    = SetInitialAge String
    | SetRetirementAge String
    | SetInitialSalary String
    | SetRetirementSalary String
    | SetInitialReturnPercent String
    | SetFinalReturnPercent String
    | SetFinalReturnAtAge String
    | SetDepositPercent String
    | SetRetirementWithdrawal String


init : () -> ( Model, Cmd Msg )
init flags =
    let
        modelWithoutComputed : Model
        modelWithoutComputed =
            { initialAge = 27
            , retirementAge = 65
            , initialSalary = 5700 * 20 * 11
            , retirementSalary = 5700 * 20 * 11
            , initialReturnPercent = 6
            , finalReturnPercent = 3
            , finalReturnAtAge = 80
            , depositPercent = 10
            , retirementWithdrawal = 50000 * 12
            , computed = []
            }
    in
    recompute modelWithoutComputed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    recompute <|
        case msg of
            SetInitialAge string ->
                tryInt string model (\n -> { model | initialAge = n })

            SetRetirementAge string ->
                tryInt string model (\n -> { model | retirementAge = n })

            SetInitialSalary string ->
                tryFloat string model (\n -> { model | initialSalary = n })

            SetRetirementSalary string ->
                tryFloat string model (\n -> { model | retirementSalary = n })

            SetInitialReturnPercent string ->
                tryFloat string model (\n -> { model | initialReturnPercent = n })

            SetFinalReturnPercent string ->
                tryFloat string model (\n -> { model | finalReturnPercent = n })

            SetFinalReturnAtAge string ->
                tryInt string model (\n -> { model | finalReturnAtAge = n })

            SetDepositPercent string ->
                tryFloat string model (\n -> { model | depositPercent = n })

            SetRetirementWithdrawal string ->
                tryFloat string model (\n -> { model | retirementWithdrawal = n })


recompute : Model -> ( Model, Cmd Msg )
recompute model =
    let
        computed =
            SingleComputation.compute model

        newModel =
            { model | computed = computed }
    in
    ( newModel
    , elmToJS <| toSpec newModel computed
    )


toSpec : Model -> List Row -> V.Spec
toSpec model rows =
    let
        firstNegativeRowIndex =
            rows
                |> List.indexedMap Tuple.pair
                |> List.Extra.find (\( _, row ) -> row.balance < 0)
                |> Maybe.map Tuple.first
                |> Maybe.withDefault (List.length rows)

        filteredRows =
            {- We want to filter out the negative rows, except for the last one.

               This is purely for visualization purposes. The table still
               contains the negative values.
            -}
            List.take (firstNegativeRowIndex + 1) rows

        data =
            V.dataFromColumns []
                << V.dataColumn "age" (V.nums <| List.map (.age >> toFloat) filteredRows)
                << V.dataColumn "balance" (V.nums <| List.map .balance filteredRows)

        enc =
            V.encoding
                << V.position V.X
                    [ V.pName "age"
                    , V.pOrdinal
                    , V.pAxis [ V.axTitle "Age" ]
                    ]
                << V.position V.Y
                    [ V.pName "balance"
                    , V.pQuant
                    , V.pAxis [ V.axTitle "Balance" ]
                    ]

        retirementAgeData =
            V.dataFromColumns []
                << V.dataColumn "retirement age" (V.nums [ toFloat model.retirementAge ])

        retirementAgeEnc =
            V.encoding
                << V.position V.X [ V.pName "retirement age", V.pOrdinal ]

        retirementAgeRule =
            V.asSpec
                [ V.rule [ V.maStroke "rgba(0,0,0,0.5)" ]
                , retirementAgeData []
                , retirementAgeEnc []
                ]

        retirementAgeLabel =
            V.asSpec
                [ V.textMark
                    [ V.maText <|
                        String.join "\n"
                            [ "Retirement age:"
                            , "deposits stop and"
                            , "withdrawals begin"
                            ]
                    , V.maAlign V.haLeft
                    , V.maXOffset 10
                    , V.maFontSize 14
                    ]
                , retirementAgeData []
                , retirementAgeEnc []
                ]

        savingPhaseLabel =
            V.asSpec
                [ V.textMark
                    [ V.maText <|
                        String.join "\n"
                            [ "Saving phase:"
                            , String.fromFloat model.depositPercent ++ "% of salary gets"
                            , "deposited every year"
                            ]
                    , V.maAlign V.haRight
                    , V.maXOffset -10
                    , V.maFontSize 14
                    ]
                , retirementAgeData []
                , retirementAgeEnc []
                ]
    in
    V.toVegaLite
        [ V.title "Balance by age" []
        , V.widthOfContainer
        , V.heightOfContainer
        , V.layer
            [ V.asSpec
                [ data []
                , enc []
                , V.area []
                ]
            , retirementAgeRule
            , retirementAgeLabel
            , savingPhaseLabel
            ]
        ]


tryInt : String -> Model -> (Int -> Model) -> Model
tryInt string model setter =
    case String.toInt string of
        Nothing ->
            model

        Just int ->
            setter int


tryFloat : String -> Model -> (Float -> Model) -> Model
tryFloat string model setter =
    case String.toFloat string of
        Nothing ->
            model

        Just float ->
            setter float


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Html.Lazy.lazy view_ model


view_ : Model -> Html Msg
view_ model =
    Html.div [ Attrs.class "main" ]
        [ viewNotes
        , viewInputs model
        , Html.div [ Attrs.class "chart" ] []
        , viewTable model.computed
        ]


viewNotes : Html Msg
viewNotes =
    Html.div
        [ Attrs.class "notes" ]
        [ Html.h1 [] [ Html.text "Boglehead retirement calculator" ]
        , Html.a
            [ Attrs.href "https://github.com/Janiczek/boglehead-retirement-calculator"
            , Attrs.target "_blank"
            ]
            [ Html.text "Source" ]
        , Html.div [ Attrs.class "note" ] [ Html.text "All percentages are in range 0-100." ]
        , Html.div [ Attrs.class "note" ] [ Html.text "All payments are done at the end of the period." ]
        ]


viewInputs : Model -> Html Msg
viewInputs model =
    Html.div
        [ Attrs.class "inputs" ]
        [ Html.div [ Attrs.class "input-group" ]
            [ ageInput model.initialAge "Initial age" SetInitialAge
            , ageInput model.retirementAge "Retirement age" SetRetirementAge
            ]
        , Html.div [ Attrs.class "input-group" ]
            [ moneyInput model.initialSalary "Initial salary (p.a.)" SetInitialSalary
            , moneyInput model.retirementSalary "Salary before retirement (p.a.)" SetRetirementSalary
            ]
        , Html.div [ Attrs.class "input-group" ]
            [ percentInput model.initialReturnPercent "Initial return % (p.a.)" SetInitialReturnPercent
            , percentInput model.finalReturnPercent "Final return % (p.a.)" SetFinalReturnPercent
            , ageInput model.finalReturnAtAge "Final return at age" SetFinalReturnAtAge
            ]
        , Html.div [ Attrs.class "input-group" ]
            [ percentInput model.depositPercent "Deposit % of salary" SetDepositPercent
            , moneyInput model.retirementWithdrawal "Retirement withdrawal (p.a.)" SetRetirementWithdrawal
            ]
        ]


ageInput : Int -> String -> (String -> Msg) -> Html Msg
ageInput =
    input 0 SingleComputation.finalAge 1 String.fromInt


percentInput : Float -> String -> (String -> Msg) -> Html Msg
percentInput =
    input 0 100 0.01 String.fromFloat


moneyInput : Float -> String -> (String -> Msg) -> Html Msg
moneyInput =
    input 0 10000000 1 String.fromFloat


input : a -> a -> a -> (a -> String) -> a -> String -> (String -> Msg) -> Html Msg
input min max step toString value label toMsg =
    let
        min_ =
            toString min

        max_ =
            toString max
    in
    Html.div
        [ Attrs.class "input-row" ]
        [ Html.label [] [ Html.text label ]
        , Html.div
            [ Attrs.class "input-wrapper" ]
            [ Html.input
                [ Events.onInput toMsg
                , Attrs.type_ "number"
                , Attrs.min min_
                , Attrs.max max_
                , Attrs.step <| toString step
                , Attrs.placeholder <| min_ ++ "-" ++ max_
                , Attrs.value <| toString value
                ]
                []
            ]
        ]


viewTable : List Row -> Html Msg
viewTable computed =
    Html.div
        [ Attrs.class "table-wrapper" ]
        [ Html.table []
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Age" ]
                    , Html.th [] [ Html.text "Salary" ]
                    , Html.th [] [ Html.text "Deposit" ]
                    , Html.th [] [ Html.text "Withdrawal" ]
                    , Html.th [] [ Html.text "Return %" ]
                    , Html.th [] [ Html.text "Interest Earned" ]
                    , Html.th [] [ Html.text "Balance" ]
                    ]
                ]
            , computed
                |> List.map viewRow
                |> Html.tbody []
            ]
        ]


viewRow : Row -> Html Msg
viewRow row =
    let
        formatMoney_ n =
            let
                formatted =
                    formatMoney n
            in
            Html.td
                [ Attrs.attribute "data-value" formatted ]
                [ Html.text formatted ]
    in
    Html.tr []
        [ Html.td [] [ Html.text <| String.fromInt row.age ]
        , formatMoney_ row.salary
        , formatMoney_ row.deposit
        , formatMoney_ row.withdrawal
        , Html.td [ Attrs.class "percent" ] [ Html.text <| formatPercent row.returnPercent ]
        , formatMoney_ row.interestEarned
        , formatMoney_ row.balance
        ]


thousandSeparator : String
thousandSeparator =
    "\u{2009}"


decimalDot : String
decimalDot =
    "."


formatMoney : Float -> String
formatMoney n =
    let
        stringN =
            String.fromFloat <| round100 n

        ( whole, maybeDecimal ) =
            case String.split decimalDot stringN of
                [ whole_, decimal ] ->
                    ( whole_, Just decimal )

                [ whole_ ] ->
                    ( whole_, Nothing )

                _ ->
                    ( "0", Nothing )

        wholeFormatted =
            whole
                |> String.toList
                |> List.reverse
                |> List.Extra.greedyGroupsOf 3
                |> List.map (List.reverse >> String.fromList)
                |> List.reverse
                |> String.join thousandSeparator

        decimalFormatted =
            maybeDecimal
                |> Maybe.withDefault "00"
                |> String.padRight 2 '0'
    in
    wholeFormatted ++ decimalDot ++ decimalFormatted


formatPercent : Float -> String
formatPercent n =
    let
        stringN =
            String.fromFloat <| round100 n

        ( whole, maybeDecimal ) =
            case String.split decimalDot stringN of
                [ whole_, decimal ] ->
                    ( whole_, Just decimal )

                [ whole_ ] ->
                    ( whole_, Nothing )

                _ ->
                    ( "0", Nothing )

        decimalFormatted =
            maybeDecimal
                |> Maybe.withDefault "00"
                |> String.padRight 2 '0'
    in
    whole ++ decimalDot ++ decimalFormatted
