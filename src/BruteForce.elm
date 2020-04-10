port module BruteForce exposing (Input, main, scalarize)

import List.Extra
import Platform
import Random exposing (Generator)
import SingleComputation


port elmToJS : Input -> Cmd msg


type alias Input =
    { retirementAge : Int
    , initialReturnPercent : Float
    , finalReturnPercent : Float
    , finalReturnAtAge : Int
    , depositPercent : Float
    }


emptyInput =
    Input 0 0 0 0 0


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


min =
    { ra = 27
    , dp = 1
    , irp = 5
    , frp = 3
    , fraa = 40
    }


max =
    { ra = 100
    , dp = 50
    , irp = 10
    , frp = 6
    , fraa = 100
    }


step =
    { ra = 1
    , dp = 0.1
    , irp = 0.1
    , frp = 0.1
    , fraa = 1
    }


w =
    { ra = 3.25
    , dp = 1
    , irp = 0.2
    , frp = 0.1
    , fraa = 0.15
    }


{-| Normalize a number in a range into the 0..1 range.
-}
normalize : Float -> Float -> Float -> Float
normalize min_ max_ n =
    (n - min_) / (max_ - min_)


scalarize : Input -> Float
scalarize a =
    (w.ra * normalize min.ra max.ra (toFloat a.retirementAge))
        + (w.dp * normalize min.dp max.dp a.depositPercent)
        + (w.irp * normalize min.irp max.irp a.initialReturnPercent)
        + (w.frp * normalize min.frp max.frp a.finalReturnPercent)
        + (w.fraa * normalize min.fraa max.fraa (toFloat a.finalReturnAtAge))


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
    100000


init : () -> ( (), Cmd Msg )
init flags =
    ( ()
    , Random.generate GeneratedInputs (Random.list tries inputGenerator)
    )


inputGenerator : Generator Input
inputGenerator =
    Random.map5 Input
        -- beware the order
        (Random.int min.ra max.ra)
        (Random.float min.irp max.irp)
        (Random.float min.frp max.frp)
        (Random.int min.fraa max.fraa)
        (Random.float min.dp max.dp)


update : Msg -> () -> ( (), Cmd Msg )
update msg () =
    case msg of
        GeneratedInputs inputs ->
            ( ()
            , inputs
                |> List.map toModel
                |> List.filter SingleComputation.sanityCheck
                |> List.map (descendAll >> toInput)
                |> List.map (\input -> ( input, scalarize input ))
                |> List.Extra.minimumBy Tuple.second
                |> Maybe.map Tuple.first
                |> Maybe.withDefault emptyInput
                |> elmToJS
            )


set =
    { ra = \n model -> { model | retirementAge = n }
    , dp = \n model -> { model | depositPercent = n }
    , irp = \n model -> { model | initialReturnPercent = n }
    , frp = \n model -> { model | finalReturnPercent = n }
    , fraa = \n model -> { model | finalReturnAtAge = n }
    }


descend : number -> number -> (Model -> number) -> (number -> Model -> Model) -> Model -> Model
descend min_ step_ getter setter model =
    let
        new =
            getter model - step_

        newModel =
            setter new model
    in
    if new < min_ || not (SingleComputation.sanityCheck newModel) then
        model

    else
        descend min_ step_ getter setter newModel


descendAll : Model -> Model
descendAll model =
    model
        |> descend min.ra step.ra .retirementAge set.ra
        |> descend min.dp step.dp .depositPercent set.dp
        |> descend min.irp step.irp .initialReturnPercent set.irp
        |> descend min.frp step.frp .finalReturnPercent set.frp
        |> descend min.fraa step.fraa .finalReturnAtAge set.fraa


subscriptions : () -> Sub Msg
subscriptions () =
    Sub.none
