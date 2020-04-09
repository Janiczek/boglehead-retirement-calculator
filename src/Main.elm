module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    -- AGE
    { initialAge : Int
    , retirementAge : Int -- contributions end at this age

    -- SALARY: will increase linearly
    , initialSalary : Float
    , retirementSalary : Float

    -- RETURN
    , initialReturnPercent : Float
    , finalReturnPercent : Float -- lowers as you rebalance the portfolio and add more bonds to lower the risk
    , finalReturnAtAge : Int

    -- CONTRIBUTIONS AND WITHDRAWALS
    , contributionPercent : Float
    , retirementWithdrawal : Float
    }


type Msg
    = SetInitialAge String
    | SetRetirementAge String
    | SetInitialSalary String
    | SetRetirementSalary String
    | SetInitialReturnPercent String
    | SetFinalReturnPercent String
    | SetFinalReturnAtAge String
    | SetContributionPercent String
    | SetRetirementWithdrawal String


type alias Row =
    { age : Int
    , salary : Float
    , contribution : Float
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
    presentValue * (1 + interestRate)


compute : Model -> List Row
compute model =
    let
        initRow : Row
        initRow =
            { age = model.initialAge
            , salary = model.initialSalary
            , contribution = 0
            , withdrawal = 0
            , returnPercent = model.initialReturnPercent
            , balance = 0
            }
    in
    computeHelp model initRow []


computeHelp : Model -> Row -> List Row -> List Row
computeHelp model last rest =
    if last.age == finalAge then
        List.reverse (last :: rest)

    else
        let
            age =
                last.age + 1

            currentTerm =
                -- for interpolations
                age - model.initialAge

            salary =
                interpolate
                    { terms = model.retirementAge - model.initialAge -- TODO off by 1?
                    , currentTerm = currentTerm
                    , startValue = model.initialSalary
                    , endValue = model.retirementSalary
                    }

            contribution =
                if age >= model.retirementAge then
                    0

                else
                    model.contributionPercent * salary / 100

            withdrawal =
                if age >= model.retirementAge then
                    model.retirementWithdrawal

                else
                    0

            returnPercent =
                interpolate
                    { terms = model.finalReturnAtAge - model.initialAge -- TODO off by 1?
                    , currentTerm = currentTerm
                    , startValue = model.initialReturnPercent
                    , endValue = model.finalReturnPercent
                    }

            balance =
                futureValue
                    { presentValue = last.balance + contribution - withdrawal
                    , interestRate = returnPercent / 100
                    }

            new : Row
            new =
                { age = age
                , salary = salary
                , contribution = contribution
                , withdrawal = withdrawal
                , returnPercent = returnPercent
                , balance = balance
                }
        in
        computeHelp model new (last :: rest)


init : () -> ( Model, Cmd Msg )
init flags =
    ( { initialAge = 27
      , retirementAge = 60
      , initialSalary = 5700 * 20 * 11
      , retirementSalary = 7350 * 20 * 11
      , initialReturnPercent = 6
      , finalReturnPercent = 3
      , finalReturnAtAge = 80
      , contributionPercent = 10
      , retirementWithdrawal = 50000
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
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

        SetContributionPercent string ->
            tryFloat string model (\n -> { model | contributionPercent = n })

        SetRetirementWithdrawal string ->
            tryFloat string model (\n -> { model | retirementWithdrawal = n })
    , Cmd.none
    )


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


cssStyle : String
cssStyle =
    """
body {
    padding: 1rem;
    font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif;
    color: #212529;
    border-collapse: collapse;
    font-size: 16px;
}
table {
    font-size: 0.875rem;
    border-spacing: 0;
}
th {
    text-align: center;
    border-top: 1px solid #dee2e6;
    border-bottom: 2px solid #dee2e6;
    padding: 0.3rem;
    vertical-align: bottom;
    position: sticky;
    top: 0;
    background: #fff;
}
td {
    text-align: right;
    padding: 0.25rem 0.5rem;
}
tbody tr:nth-of-type(odd) {
    background-color: rgba(0,0,0,0.05);
}
.percent::after {
    content: '%';
}
.inputs {
    width: 300px;
    display: flex;
    flex-direction: column;
    padding-bottom: 1rem;
    font-size: 0.875rem;
}
.input-row {
    flex: 1;
    height: 1rem;
    display: flex;
    align-items: center;
}
.input-row label {
    width: 70%;
}
.input-wrapper {
    width: 30%;
}
.input-wrapper input {
    margin: 0;
    font-family: inherit;
    display: block;
    width: 100%;
    height: calc(1em + .75rem + 2px);
    padding: .125rem .75rem;
    font-size: 0.875rem;
    color: #495057;
    background-color: #fff;
    background-clip: padding-box;
    border: 1px solid #ced4da;
    border-radius: .25rem;
    overflow: visible;
}
"""


view : Model -> Browser.Document Msg
view model =
    { title = "Investment Retirement"
    , body =
        [ Html.node "style" [] [ Html.text cssStyle ]
        , viewInputs model
        , viewTable model
        , viewGraph model
        ]
    }


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
        , percentInput model.contributionPercent "Contribution %" SetContributionPercent
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


viewTable : Model -> Html Msg
viewTable model =
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Age" ]
                , Html.th [] [ Html.text "Salary" ]
                , Html.th [] [ Html.text "Contribution" ]
                , Html.th [] [ Html.text "Withdrawal" ]
                , Html.th [] [ Html.text "Return %" ]
                , Html.th [] [ Html.text "Balance" ]
                ]
            ]
        , compute model
            |> List.map viewRow
            |> Html.tbody []
        ]


viewRow : Row -> Html Msg
viewRow row =
    Html.tr []
        [ Html.td [] [ Html.text <| String.fromInt row.age ]
        , Html.td [] [ Html.text <| formatMoney row.salary ]
        , Html.td [] [ Html.text <| formatMoney row.contribution ]
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


viewGraph : Model -> Html Msg
viewGraph model =
    Html.text "TODO graph"
