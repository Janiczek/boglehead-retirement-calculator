module BruteForce exposing (Input, main, scalarize)

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


type alias Model =
    SingleComputation.Model {}


toModel : Input -> Model
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


toInput : Model -> Input
toInput r =
    { retirementAge = r.retirementAge
    , initialReturnPercent = r.initialReturnPercent
    , finalReturnPercent = r.finalReturnPercent
    , finalReturnAtAge = r.finalReturnAtAge
    , depositPercent = r.depositPercent
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



-- WEIGHTS


raWeight =
    2


dpWeight =
    0.3


irpWeight =
    0.1


frpWeight =
    0.1


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


main : Program () () Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = GeneratedInputs (List Input)


tries : Int
tries =
    10000


init : () -> ( (), Cmd Msg )
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


update : Msg -> () -> ( (), Cmd Msg )
update msg () =
    case msg of
        GeneratedInputs inputs ->
            let
                _ =
                    inputs
                        |> List.map toModel
                        |> List.filter SingleComputation.sanityCheck
                        |> List.map (descendAll >> toInput)
                        |> List.map (\input -> ( input, scalarize input ))
                        |> List.Extra.minimumBy Tuple.second
                        |> Debug.log "best random input"
            in
            ( (), Cmd.none )


setRA n model =
    { model | retirementAge = n }


setDP n model =
    { model | depositPercent = n }


setIRP n model =
    { model | initialReturnPercent = n }


setFRP n model =
    { model | finalReturnPercent = n }


setFRAA n model =
    { model | finalReturnAtAge = n }


descend : number -> number -> (Model -> number) -> (number -> Model -> Model) -> Model -> Model
descend min step getter setter model =
    let
        new =
            getter model - step

        newModel =
            setter new model
    in
    if new < min || not (SingleComputation.sanityCheck newModel) then
        model

    else
        descend min step getter setter newModel


descendAll : Model -> Model
descendAll model =
    model
        |> descend raMin raStep .retirementAge setRA
        |> descend dpMin dpStep .depositPercent setDP
        |> descend irpMin irpStep .initialReturnPercent setIRP
        |> descend frpMin frpStep .finalReturnPercent setFRP
        |> descend fraaMin fraaStep .finalReturnAtAge setFRAA


subscriptions : () -> Sub Msg
subscriptions () =
    Sub.none
