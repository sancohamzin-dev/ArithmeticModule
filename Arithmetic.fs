module Arithmetic

// Қосу функциясы
let add a b =
    if a < 0.0 || b < 0.0 then
        Error "Теріс сан енгізуге болмайды!"
    else
        Ok (a + b)

// Азайту функциясы
let subtract a b =
    if a < 0.0 || b < 0.0 then
        Error "Теріс сан енгізуге болмайды!"
    else
        Ok (a - b)

// Көбейту функциясы
let multiply a b =
    if a < 0.0 || b < 0.0 then
        Error "Теріс сан енгізуге болмайды!"
    else
        Ok (a * b)

// Бөлу функциясы
let divide a b =
    if a < 0.0 || b < 0.0 then
        Error "Теріс сан енгізуге болмайды!"
    elif b = 0.0 then
        Error "Нөлге бөлуге болмайды!"
    else
        Ok (a / b)

// Массивті тексеру функциясы
let validateArray (values: obj array) =
    values
    |> Array.forall (fun value ->
        match value with
        | null -> false
        | :? string -> false
        | :? float as number -> number >= 0.0 && not (System.Double.IsNaN(number) || System.Double.IsInfinity(number))
        | :? int as number -> number >= 0
        | _ -> false)