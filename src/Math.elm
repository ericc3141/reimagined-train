module Math exposing (Expression(..), Function, add, eval, sub)

import Dict exposing (Dict)


type Expression
    = Number Float
    | Apply Function (List Expression)


type alias Function =
    { name : String
    , eval : List Expression -> Result Error Expression
    , args : Int
    , description : String
    }


type Error
    = TooFewArgs
    | TooManyArgs
    | MathError String
    | EvalError String


eval : Expression -> Result Error Expression
eval expr =
    let
        allOk : List (Result a b) -> Result a (List b)
        allOk results =
            List.foldl (\item result -> Result.map2 (::) item result) (Ok []) results
    in
    case expr of
        Number num ->
            Ok expr

        Apply func exprs ->
            case allOk (List.map eval exprs) of
                Err err ->
                    Err err

                Ok args ->
                    func.eval args


binaryify :
    (Expression -> Expression -> Result Error Expression)
    -> List Expression
    -> Result Error Expression
binaryify evalfunc args =
    case args of
        [] ->
            Err TooFewArgs

        [ _ ] ->
            Err TooFewArgs

        [ a, b ] ->
            evalfunc a b

        _ ->
            Err TooManyArgs


sub : Function
sub =
    let
        sub_ : Expression -> Expression -> Result Error Expression
        sub_ a b =
            case ( a, b ) of
                ( Number n, Number m ) ->
                    Ok <| Number (n - m)

                _ ->
                    Ok <| Apply sub [ a, b ]
    in
    { name = "sub"
    , eval = binaryify sub_
    , args = 2
    , description = "Sub"
    }


add : Function
add =
    let
        add_ a b =
            case ( a, b ) of
                ( Number n, Number m ) ->
                    Ok <| Number (n + m)

                _ ->
                    Ok <| Apply add [ a, b ]
    in
    { name = "add"
    , eval = binaryify add_
    , args = 2
    , description = "Add"
    }


mul : Function
mul =
    let
        mul_ a b =
            case ( a, b ) of
                ( Number n, Number m ) ->
                    Ok <| Number (n * m)

                _ ->
                    Ok <| Apply mul [ a, b ]
    in
    { name = "mul"
    , eval = binaryify mul_
    , args = 2
    , description = "Multiply"
    }


div : Function
div =
    let
        div_ a b =
            case ( a, b ) of
                ( Number n, Number m ) ->
                    if m == 0 then
                        Err <| MathError "Divide by zero"

                    else
                        Ok <| Number (n / m)

                _ ->
                    Ok <| Apply add [ a, b ]
    in
    { name = "add"
    , eval = binaryify div_
    , args = 2
    , description = "Add"
    }
