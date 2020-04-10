module SingleComputation exposing
    ( Model
    , Row
    , compute
    , finalAge
    , sanityCheck
    )

import List.Extra
import Utils exposing (round100)


finalAge : Int
finalAge =
    100


type alias Row =
    { age : Int
    , salary : Float
    , deposit : Float
    , withdrawal : Float
    , returnPercent : Float
    , interestEarned : Float
    , balance : Float
    }


type alias Model r =
    { r
        | -- AGE
          initialAge : Int
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
    }


compute : Model r -> List Row
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

                interestEarned =
                    computeInterestEarned
                        { lastBalance = 0
                        , deposit = deposit
                        , withdrawal = withdrawal
                        , returnPercent = returnPercent
                        }

                balance =
                    computeBalance
                        { lastBalance = 0
                        , deposit = deposit
                        , withdrawal = withdrawal
                        , interestEarned = interestEarned
                        }
            in
            { age = age
            , salary = salary
            , deposit = deposit
            , withdrawal = withdrawal
            , returnPercent = returnPercent
            , interestEarned = interestEarned
            , balance = balance
            }
    in
    computeHelp model initRow []


computeDeposit : Int -> Float -> Model r -> Float
computeDeposit age salary model =
    if age > model.retirementAge then
        0

    else
        model.depositPercent * salary / 100


computeWithdrawal : Int -> Model r -> Float
computeWithdrawal age model =
    if age > model.retirementAge then
        model.retirementWithdrawal

    else
        0


computeCurrentTerm : Int -> Model r -> Int
computeCurrentTerm age model =
    age - model.initialAge


computeReturnPercent : Int -> Model r -> Float
computeReturnPercent age model =
    interpolate
        { terms = model.finalReturnAtAge - model.initialAge
        , currentTerm = computeCurrentTerm age model
        , startValue = model.initialReturnPercent
        , endValue = model.finalReturnPercent
        , round = False
        }


computeSalary : Int -> Model r -> Float
computeSalary age model =
    if age > model.retirementAge then
        0

    else
        interpolate
            { terms = model.retirementAge - model.initialAge
            , currentTerm = computeCurrentTerm age model
            , startValue = model.initialSalary
            , endValue = model.retirementSalary
            , round = True
            }


type alias InterestEarnedOptions =
    { lastBalance : Float
    , deposit : Float
    , withdrawal : Float
    , returnPercent : Float
    }


computeInterestEarned : InterestEarnedOptions -> Float
computeInterestEarned { lastBalance, deposit, withdrawal, returnPercent } =
    futureValue
        { presentValue = negate (lastBalance - withdrawal)
        , payment = negate deposit
        , interestRate = returnPercent / 100
        , paymentType = DueAtEndOfPeriod
        , terms = 1
        }
        - (lastBalance + deposit - withdrawal)


type alias BalanceOptions =
    { lastBalance : Float
    , deposit : Float
    , withdrawal : Float
    , interestEarned : Float
    }


computeBalance : BalanceOptions -> Float
computeBalance { lastBalance, deposit, withdrawal, interestEarned } =
    lastBalance + deposit + interestEarned - withdrawal


computeHelp : Model r -> Row -> List Row -> List Row
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

            interestEarned =
                computeInterestEarned
                    { lastBalance = last.balance
                    , deposit = deposit
                    , withdrawal = withdrawal
                    , returnPercent = returnPercent
                    }

            balance =
                computeBalance
                    { lastBalance = last.balance
                    , deposit = deposit
                    , withdrawal = withdrawal
                    , interestEarned = interestEarned
                    }

            new : Row
            new =
                { age = age
                , salary = salary
                , deposit = deposit
                , withdrawal = withdrawal
                , returnPercent = returnPercent
                , interestEarned = interestEarned
                , balance = balance
                }
        in
        computeHelp model new (last :: rest)


type alias InterpolateOptions =
    { terms : Int
    , currentTerm : Int
    , startValue : Float
    , endValue : Float
    , round : Bool
    }


interpolate : InterpolateOptions -> Float
interpolate { terms, currentTerm, startValue, endValue, round } =
    if currentTerm >= terms then
        endValue

    else
        (startValue + (toFloat currentTerm / toFloat terms) * (endValue - startValue))
            |> (if round then
                    round100

                else
                    identity
               )


type alias FutureValueOptions =
    { presentValue : Float
    , interestRate : Float
    , terms : Int
    , payment : Float
    , paymentType : PaymentType
    }


type PaymentType
    = DueAtBeginningOfPeriod
    | DueAtEndOfPeriod


futureValue : FutureValueOptions -> Float
futureValue { presentValue, interestRate, terms, payment, paymentType } =
    -- https://github.com/LibreOffice/core/blob/3bf3face224a7e12ba95888821a0ac21525af22c/sc/source/core/tool/interpr2.cxx#L1973
    if interestRate == 0 then
        presentValue + payment * toFloat terms

    else
        negate <|
            let
                compoundedRate =
                    (1 + interestRate) ^ toFloat terms
            in
            case paymentType of
                -- what the heck, LibreOffice devs... ¯\_(ツ)_/¯
                DueAtBeginningOfPeriod ->
                    (presentValue * compoundedRate)
                        + (payment
                            * (1 + interestRate)
                            * (compoundedRate - 1)
                            / interestRate
                          )

                DueAtEndOfPeriod ->
                    (presentValue * compoundedRate)
                        + (payment
                            * (compoundedRate - 1)
                            / interestRate
                          )


sanityCheck : Model r -> Bool
sanityCheck model =
    let
        rows =
            compute model

        lastPositiveAge =
            rows
                |> List.Extra.takeWhile (\{ balance } -> balance >= 0)
                |> List.Extra.last
                |> Maybe.map .age
                |> Maybe.withDefault -1

        lastsUntilFinalAge : Bool
        lastsUntilFinalAge =
            lastPositiveAge >= finalAge

        initialGEFinal : Bool
        initialGEFinal =
            model.initialReturnPercent >= model.finalReturnPercent

        finalGERetirement : Bool
        finalGERetirement =
            model.finalReturnAtAge >= model.retirementAge
    in
    lastsUntilFinalAge
        && initialGEFinal
        && finalGERetirement
