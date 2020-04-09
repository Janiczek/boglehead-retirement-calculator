port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Lazy
import List.Extra
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


type alias Row =
    { age : Int
    , salary : Float
    , deposit : Float
    , withdrawal : Float
    , returnPercent : Float
    , balance : Float
    }


finalAge : Int
finalAge =
    100


type alias InterpolateOptions =
    { terms : Int
    , currentTerm : Int
    , startValue : Float
    , endValue : Float
    }


interpolate : InterpolateOptions -> Float
interpolate { terms, currentTerm, startValue, endValue } =
    if currentTerm >= terms then
        endValue

    else
        startValue + (toFloat currentTerm / toFloat terms) * (endValue - startValue)


type alias FutureValueOptions =
    { presentValue : Float
    , interestRate : Float
    }


{-| This version advances by one term only.
-}
futureValue : FutureValueOptions -> Float
futureValue { presentValue, interestRate } =
    -- TODO type (end / start)?
    -- https://github.com/LibreOffice/core/blob/3bf3face224a7e12ba95888821a0ac21525af22c/sc/source/core/tool/interpr2.cxx#L1973
    presentValue * (1 + interestRate)


type alias BalanceOptions =
    { lastBalance : Float
    , deposit : Float
    , withdrawal : Float
    , returnPercent : Float
    }


compute : Model -> List Row
compute model =
    let
        initRow : Row
        initRow =
            let
                age =
                    model.initialAge

                salary =
                    computeSalary age model

                deposit =
                    computeDeposit age salary model

                withdrawal =
                    computeWithdrawal age model

                returnPercent =
                    model.initialReturnPercent
            in
            { age = age
            , salary = salary
            , deposit = deposit
            , withdrawal = withdrawal
            , returnPercent = returnPercent
            , balance =
                computeBalance
                    { lastBalance = 0
                    , deposit = deposit
                    , withdrawal = withdrawal
                    , returnPercent = returnPercent
                    }
            }
    in
    computeHelp model initRow []


computeDeposit : Int -> Float -> Model -> Float
computeDeposit age salary model =
    if age >= model.retirementAge then
        0

    else
        model.depositPercent * salary / 100


computeWithdrawal : Int -> Model -> Float
computeWithdrawal age model =
    if age >= model.retirementAge then
        model.retirementWithdrawal

    else
        0


computeCurrentTerm : Int -> Model -> Int
computeCurrentTerm age model =
    age - model.initialAge


computeReturnPercent : Int -> Model -> Float
computeReturnPercent age model =
    interpolate
        { terms = model.finalReturnAtAge - model.initialAge -- TODO off by 1?
        , currentTerm = computeCurrentTerm age model
        , startValue = model.initialReturnPercent
        , endValue = model.finalReturnPercent
        }


computeSalary : Int -> Model -> Float
computeSalary age model =
    interpolate
        { terms = model.retirementAge - model.initialAge -- TODO off by 1?
        , currentTerm = computeCurrentTerm age model
        , startValue = model.initialSalary
        , endValue = model.retirementSalary
        }


computeBalance : BalanceOptions -> Float
computeBalance { lastBalance, deposit, withdrawal, returnPercent } =
    futureValue
        { presentValue = lastBalance + deposit - withdrawal
        , interestRate = returnPercent / 100
        }


computeHelp : Model -> Row -> List Row -> List Row
computeHelp model last rest =
    if last.age == finalAge then
        List.reverse (last :: rest)

    else
        let
            age =
                last.age + 1

            salary =
                computeSalary age model

            deposit =
                computeDeposit age salary model

            withdrawal =
                computeWithdrawal age model

            returnPercent =
                computeReturnPercent age model

            balance =
                computeBalance
                    { lastBalance = last.balance
                    , deposit = deposit
                    , withdrawal = withdrawal
                    , returnPercent = returnPercent
                    }

            new : Row
            new =
                { age = age
                , salary = salary
                , deposit = deposit
                , withdrawal = withdrawal
                , returnPercent = returnPercent
                , balance = balance
                }
        in
        computeHelp model new (last :: rest)


init : () -> ( Model, Cmd Msg )
init flags =
    let
        modelWithoutComputed : Model
        modelWithoutComputed =
            { initialAge = 27
            , retirementAge = 60
            , initialSalary = 5700 * 20 * 11
            , retirementSalary = 7350 * 20 * 11
            , initialReturnPercent = 6
            , finalReturnPercent = 3
            , finalReturnAtAge = 80
            , depositPercent = 10
            , retirementWithdrawal = 50000
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
            compute model

        newModel =
            { model | computed = computed }
    in
    ( newModel
    , elmToJS <| toSpec newModel computed
    )


toSpec : Model -> List Row -> V.Spec
toSpec model rows =
    let
        data =
            V.dataFromColumns []
                << V.dataColumn "age" (V.nums <| List.map (.age >> toFloat) rows)
                << V.dataColumn "balance" (V.nums <| List.map .balance rows)

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
                [ V.textMark [ V.maText "retirement age", V.maAlign V.haLeft, V.maXOffset 10 ]
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
    let
        computed : List Row
        computed =
            compute model
    in
    Html.div [ Attrs.class "main" ]
        [ viewInputs model
        , Html.div [ Attrs.class "chart" ] []
        , viewTable computed
        ]


viewInputs : Model -> Html Msg
viewInputs model =
    Html.div
        [ Attrs.class "inputs" ]
        [ ageInput model.initialAge "Initial age" SetInitialAge
        , ageInput model.retirementAge "Retirement age" SetRetirementAge
        , moneyInput model.initialSalary "Initial yearly salary" SetInitialSalary
        , moneyInput model.retirementSalary "Retirement yearly salary" SetRetirementSalary
        , percentInput model.initialReturnPercent "Initial return %" SetInitialReturnPercent
        , percentInput model.finalReturnPercent "Final return %" SetFinalReturnPercent
        , ageInput model.finalReturnAtAge "Final return at age" SetFinalReturnAtAge
        , percentInput model.depositPercent "Deposit %" SetDepositPercent
        , moneyInput model.retirementWithdrawal "Retirement yearly withdrawal" SetRetirementWithdrawal
        ]


ageInput : Int -> String -> (String -> Msg) -> Html Msg
ageInput =
    input 0 finalAge String.fromInt


percentInput : Float -> String -> (String -> Msg) -> Html Msg
percentInput =
    input 0 100 String.fromFloat


moneyInput : Float -> String -> (String -> Msg) -> Html Msg
moneyInput =
    input 0 10000000 String.fromFloat


input : a -> a -> (a -> String) -> a -> String -> (String -> Msg) -> Html Msg
input min max toString value label toMsg =
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
    Html.tr []
        [ Html.td [] [ Html.text <| String.fromInt row.age ]
        , Html.td [] [ Html.text <| formatMoney row.salary ]
        , Html.td [] [ Html.text <| formatMoney row.deposit ]
        , Html.td [] [ Html.text <| formatMoney row.withdrawal ]
        , Html.td [ Attrs.class "percent" ] [ Html.text <| formatPercent row.returnPercent ]
        , Html.td [] [ Html.text <| formatMoney row.balance ]
        ]


round100 : Float -> Float
round100 n =
    toFloat (round (n * 100)) / 100


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
