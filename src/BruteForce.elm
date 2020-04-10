module BruteForce exposing (Input, main)

import List.Extra
import Platform
import Random exposing (Generator)
import SingleComputation


type alias Input =
    { retirementAge : Int
    , initialReturnPercent : Float
    , finalReturnPercent : Float
    , finalReturnAtAge : Int
    , depositPercent : Float
    }


toModel : Input -> SingleComputation.Model {}
toModel r =
    { initialAge = 27
    , retirementAge = r.retirementAge
    , initialSalary = 5700 * 20 * 11
    , retirementSalary = 5700 * 20 * 11
    , initialReturnPercent = r.initialReturnPercent
    , finalReturnPercent = r.finalReturnPercent
    , finalReturnAtAge = r.finalReturnAtAge
    , depositPercent = r.depositPercent
    , retirementWithdrawal = 50000 * 12
    }



-- RETIREMENT AGE


raMin =
    27


raMax =
    100


raStep =
    1



-- DEPOSIT PERCENT


dpMin =
    1


dpMax =
    50


dpStep =
    1



-- INITIAL RETURN PERCENT


irpMin =
    5


irpMax =
    10


irpStep =
    0.1



-- FINAL RETURN PERCENT


frpMin =
    3


frpMax =
    6


frpStep =
    0.1



-- FINAL RETURN AT AGE


fraaMin =
    27


fraaMax =
    100


fraaStep =
    1


{-| SingleComputation.lastsUntilFinalAge initModel == True
scalarize initInput == 2.1

Our goal will be to find an input with as low `scalarize` fn result as possible and lastsUntilFinalAge == True.

-}
initInput : Input
initInput =
    { retirementAge = raMax
    , initialReturnPercent = irpMax
    , finalReturnPercent = frpMax
    , finalReturnAtAge = fraaMax
    , depositPercent = dpMax
    }


initModel : SingleComputation.Model {}
initModel =
    toModel initInput


raWeight =
    0.8


dpWeight =
    0.6


irpWeight =
    0.3


frpWeight =
    0.3


fraaWeight =
    0.1


{-| Normalize a number in a range into the 0..1 range.
-}
normalize : Float -> Float -> Float -> Float
normalize min max n =
    (n - min) / (max - min)


scalarize : Input -> Float
scalarize a =
    (raWeight * normalize raMin raMax (toFloat a.retirementAge))
        + (dpWeight * normalize dpMin dpMax a.depositPercent)
        + (irpWeight * normalize irpMin irpMax a.initialReturnPercent)
        + (frpWeight * normalize frpMin frpMax a.finalReturnPercent)
        + (fraaWeight * normalize fraaMin fraaMax (toFloat a.finalReturnAtAge))


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type Msg
    = GeneratedInputs (List Input)


tries : Int
tries =
    10000


init : () -> ( Model, Cmd Msg )
init flags =
    ( ()
    , Random.generate GeneratedInputs (Random.list tries inputGenerator)
    )


inputGenerator : Generator Input
inputGenerator =
    Random.map5 Input
        -- beware the order
        (Random.int raMin raMax)
        (Random.float irpMin irpMax)
        (Random.float frpMin frpMax)
        (Random.int fraaMin fraaMax)
        (Random.float dpMin dpMax)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg () =
    case msg of
        GeneratedInputs inputs ->
            let
                bestInput =
                    inputs
                        |> List.map (\input -> ( input, toModel input, scalarize input ))
                        |> List.filter (\( _, model, _ ) -> SingleComputation.lastsUntilFinalAge model)
                        |> List.Extra.minimumBy (\( _, _, output ) -> output)
                        |> Debug.log "best found input"
            in
            ( (), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
