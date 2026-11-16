module Arithmetic

// Сандарды тексеру
let validate x =
    if System.Double.IsNaN(x) || System.Double.IsInfinity(x) then
        Error "Қате сан!"
    elif x < 0.0 then
        Error "Теріс сан енгізуге болмайды!"
    else
        Ok x

// Қосу
let add a b =
    match validate a, validate b with
    | Ok x, Ok y -> Ok (x + y)
    | Error e, _ | _, Error e -> Error e

// Азайту
let subtract a b =
    match validate a, validate b with
    | Ok x, Ok y -> Ok (x - y)
    | Error e, _ | _, Error e -> Error e

// Көбейту
let multiply a b =
    match validate a, validate b with
    | Ok x, Ok y -> Ok (x * y)
    | Error e, _ | _, Error e -> Error e

// Бөлу
let divide a b =
    match validate a, validate b with
    | Error e, _ | _, Error e -> Error e
    | Ok _, Ok 0.0 -> Error "Нөлге бөлуге болмайды!"
    | Ok x, Ok y -> Ok (x / y)

// Массивтегі қателерді тексеру
let validateArray (arr: string option array) =
    if isNull arr then
        Error "Массив бос!"
    else
        let error =
            arr
            |> Array.tryPick (function
                | None -> Some "Бос мән табылды!"
                | Some value when isNull value ->
                    Some "Null мән табылды!"
                | Some value ->
                    match System.Double.TryParse(value) with
                    | true, number when number < 0.0 ->
                        Some "Теріс сан табылды!"
                    | true, number when
                        System.Double.IsNaN(number) ||
                        System.Double.IsInfinity(number) ->
                        Some "Қате сан!"
                    | true, _ -> None
                    | false, _ -> Some "Әріп немесе қате мән табылды!")

        match error with
        | Some message -> Error message
        | None -> Ok ()